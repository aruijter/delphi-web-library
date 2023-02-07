unit OAuth2.Handler;

interface

uses
  DWL.OpenID, DWL.Types, DWL.JOSE, DWL.HTTP.Server.Types,
  DWL.HTTP.Server.Handler.DLL.Classes, DWL.OpenSSL, System.Generics.Collections,
  Winapi.WinInet, DWL.Logging, System.Classes;

const
  Param_UserTable = 'usertable'; ParamDef_UserTable = 'dwl_oauth2_users';
  Param_Field_MD5 = 'field_md5';
  Param_Field_GivenName = 'field_givenname';
  Param_Field_FamilyName = 'field_familyname';
  Param_NewUser_Scopes = 'newuser_scopes';
  Param_Assume_Offline_Scope = 'assume_offline_scope'; ParamDef_Assume_Offline_Scope = false;

type
  TOIDC_Provider = class(TdwlOIDC_Client)
  private
    FTechName: string;
    FDisplayname: string;
    FImgData: string;
  end;

  TAuthenticationSession = class
    session_state: string;
    ExpirationTime: TUnixEpoch;
    internal_clientid: integer;
    client_id: string;
    client_secret: string;
    client_scope: string;
    client_state: string;
    client_redirect_uri: string;
    client_code_challenge: string;
    client_code_challenge_method: string;
    authorization_code: string;
    ProviderIndex: integer;
    authenticated_userid: integer;
    provider_session: TdwlOIDC_Client_Session;
    provider_idtoken: IdwlJWT;
    replay_nonce: string;
    procedure UpdateState_CodeRedirection(const State: PdwlHTTPHandlingState);
  end;

  PUserInfo = ^TUserInfo;
  TUserInfo = record
    Subject: string;
    EMail: string;
    GivenName: string;
    FamilyName: string;
  end;

  THandler_OAuth2 = class(TdwlDLLHandling)
  strict private
    const FSign_KeyID = '1';
    class var FSignKey_Priv: IdwlOpenSSLKey;
    class var FCertsInfo: string;
    class var FLoginPage: string;
    class var FAuthenticationSessions: TThreadList<TAuthenticationSession>;
    class var FProviders: TObjectList<TOIDC_Provider>;
    class var FIssuerUri: string;
    class var FUserTable: string;
    class var FField_MD5: string;
    class var FField_GivenName: string;
    class var FField_FamilyName: string;
    class var FNewUser_Scopes: TArray<string>;
    class var FAssume_Offline_Scope: boolean;
    class function GetClientInfoFromHeader(const State: PdwlHTTPHandlingState; var client_id: string): string;
    class function ResolveSQL(const SQL: string): string;
    class function ConvertScope(State: PdwlHTTPHandlingState; UserID: integer; Scopes: TStringList): string;
    class function CreateRefreshtoken(UserID, Refreshtoken_order: integer; const GrantToken: string; const Scope: string): string;
    class function CreateAccesstoken(UserID: integer; const ConvertedScope: string): string;
    class function CreateIDToken(UserID: integer; Client_Id, Replay_Nonce: string; OutputEMail, OutputProfile: boolean): string;
    class function GetAuthenticationSessionByProviderState(const session_state: string): TAuthenticationSession;
    class function PopAuthenticationSessionByAuthorizationCode(const authorization_code: string): TAuthenticationSession;
    class procedure BuildLoginForm;
    class procedure HandlingError(const State: PdwlHTTPHandlingState; const ErrorMessage: string=''; const StatusCode: integer=HTTP_STATUS_BAD_REQUEST);
    class function Post_token_handle_authorization_code(const State: PdwlHTTPHandlingState; var UserID: integer; RequestedScopes: TStringList; var client_id, AuthorizeNonce, GrantToken: string): boolean;
    class function Post_token_handle_refresh_token(const State: PdwlHTTPHandlingState; var UserID, Refreshtoken_Order: integer; RequestedScopes: TStringList; var GrantToken: string): boolean;
    class function Post_token_handle_client_credentials(const State: PdwlHTTPHandlingState; var UserID: integer; RequestedScopes: TStringList): boolean;
    // processrequest procs
    class function Get_certs(const State: PdwlHTTPHandlingState): boolean;
    class function Get_wellknown_openidconfiguration(const State: PdwlHTTPHandlingState): boolean;
    class function Get_OIDC_Return_Path(const State: PdwlHTTPHandlingState): boolean;
    class function Post_authorize_reply(const State: PdwlHTTPHandlingState): boolean;
    class function GetPost_authorize(const State: PdwlHTTPHandlingState): boolean;
    class function GetPost_userinfo(const State: PdwlHTTPHandlingState): boolean;
    class function Post_token(const State: PdwlHTTPHandlingState): boolean;
    class procedure EnrichUserInfo(UserInfo: PUserInfo; FillEmail, FillProfile: boolean);
  private
    class function Subject_From_Authorization(const State: PdwlHTTPHandlingState): string; static;
  public
    class function Authorize(const State: PdwlHTTPHandlingState): boolean; override;
    class procedure Configure(const Params: string); override;
    class procedure WrapUp(const State: PdwlHTTPHandlingState); override;
  end;

implementation

uses
  System.SysUtils, System.NetEncoding, DWL.Params.Consts, DWL.MySQL,
  System.JSON, DWL.HTTP.Consts,
  DWL.Crypt, System.StrUtils, DWL.HTTP.Server.Utils;

const
  OIDC_Return_Path = '/oidc_return';

  jwtclaimGIVEN_NAME = 'given_name';
  jwtclaimFAMILY_NAME = 'family_name';

  AUTHSESSION_DURATION = 5*60; // 5 minutes
  GRANT_DURATION = 90*24*60*60; //90 days
  ACCESS_DURATION = 60*60; // 1 hour
  IDTOKEN_DURATION = 60*60; // 1 hour

{ THandler_OAuth2 }

class function THandler_OAuth2.Authorize(const State: PdwlHTTPHandlingState): boolean;
begin
  // all endpoints of this handler may be accessed by everyone
  Result := true;
end;

class function THandler_OAuth2.PopAuthenticationSessionByAuthorizationCode(const authorization_code: string): TAuthenticationSession;
begin
  Result := nil;
  var ActualEpoch := TUnixEpoch.Now;
  var LockedSessions := FAuthenticationSessions.LockList;
  try
    // first delete expired sessions
    while (LockedSessions.Count>0) and (LockedSessions[0].ExpirationTime<ActualEpoch) do
    begin
      LockedSessions[0].Free;
      LockedSessions.Delete(0);
    end;
    // then try to find result
    for var i := 0 to LockedSessions.Count-1 do
    begin
      if LockedSessions[i].authorization_code=authorization_code then
      begin
        Result := LockedSessions[i];
        // pop: delete on return
        LockedSessions.Delete(i);
        Exit;
      end;
    end;
  finally
    FAuthenticationSessions.UnlockList;
  end;
end;

class function THandler_OAuth2.GetAuthenticationSessionByProviderState(const session_state: string): TAuthenticationSession;
begin
  Result := nil;
  var ActualEpoch := TUnixEpoch.Now;
  var LockedSessions := FAuthenticationSessions.LockList;
  try
    // first delete expired sessions
    while (LockedSessions.Count>0) and (LockedSessions[0].ExpirationTime<ActualEpoch) do
      LockedSessions.Delete(0);
    // then try to find result
    for var i := 0 to LockedSessions.Count-1 do
    begin
      if LockedSessions[i].session_state=session_state then
      begin
        Result := LockedSessions[i];
        Exit;
      end;
    end;
  finally
    FAuthenticationSessions.UnlockList;
  end;
end;

class function THandler_OAuth2.GetClientInfoFromHeader(const State: PdwlHTTPHandlingState; var client_id: string): string;
begin
  Result := '';
  try
    var BasicAuth: string;
    if TryGetHeaderValue(State, 'Authorization', BasicAuth) and SameText(Copy(BasicAuth,1 ,5), 'Basic') then
    begin
      BasicAuth := trim(Copy(BasicAuth, 6, MaxInt));
      BasicAuth := TNetEncoding.Base64.Decode(BasicAuth);
      var P := Pos(':', BasicAuth);
      if P>1 then
      begin
        client_id := Copy(BasicAuth, 1, P-1);
        Result := Copy(BasicAuth, P+1, MaxInt);
      end
      else
        client_id := BasicAuth;
    end;
  except
    client_id := '';
  end;
end;

class procedure THandler_OAuth2.Configure(const Params: string);
const
  SQL_CheckTable_Providers = 'CREATE TABLE IF NOT EXISTS dwl_oauth2_providers (id INT AUTO_INCREMENT, techname VARCHAR(50), displayname VARCHAR(255), imgdata TEXT, uri VARCHAR(255), client_id VARCHAR(255), client_secret VARCHAR(255), PRIMARY KEY (id))';
  SQL_Get_Providers = 'SELECT techname, displayname, imgdata, uri, client_id, client_secret FROM dwl_oauth2_providers';
  SQL_CheckTable_UserScopes = 'CREATE TABLE IF NOT EXISTS dwl_oauth2_userscopes (id INT AUTO_INCREMENT, user_id INT, scope VARCHAR(50), PRIMARY KEY (id), INDEX UserIndex (user_id, scope), INDEX ScopeIndex (scope, user_id))';
  SQL_CheckTable_Scopes = 'CREATE TABLE IF NOT EXISTS dwl_oauth2_scopes (id INT AUTO_INCREMENT, scope VARCHAR(50), umbrella_scope VARCHAR(50), PRIMARY KEY (id))';
  SQL_CheckTable_Clients = 'CREATE TABLE IF NOT EXISTS dwl_oauth2_clients (id INT AUTO_INCREMENT, displayname VARCHAR(50), client_id VARCHAR(50), client_secret VARCHAR(50), redirect_uri VARCHAR(255), user_id INT, PRIMARY KEY (id))';
  SQL_CheckTable_Users = 'CREATE TABLE IF NOT EXISTS $(usertable) (id INT AUTO_INCREMENT, oidc_provider VARCHAR(50), oidc_subject VARCHAR(150), emailaddress VARCHAR(150), name VARCHAR(150), salt VARCHAR(43), pwd VARCHAR(86), PRIMARY KEY (id))';
  SQL_CheckTable_Grants = 'CREATE TABLE IF NOT EXISTS dwl_oauth2_grants (id INT AUTO_INCREMENT, token VARCHAR(50), user_id INT, client_id INT, scope TEXT, expirationtime BIGINT, refreshtoken_order INT, id_token TEXT, PRIMARY KEY (id))';
begin
  inherited Configure(Params);
  FProviders := TObjectList<TOIDC_Provider>.Create;
  FAuthenticationSessions := TThreadList<TAuthenticationSession>.Create;
  // prepare database, errors will go to server log directly
  FUserTable := FConfigParams.StrValue(Param_UserTable, ParamDef_UserTable);
  FField_MD5 := FConfigParams.StrValue(Param_Field_MD5);
  FField_GivenName := FConfigParams.StrValue(Param_Field_GivenName);
  FField_FamilyName := FConfigParams.StrValue(Param_Field_FamilyName);
  FAssume_Offline_Scope := FConfigParams.BoolValue(Param_Assume_Offline_Scope, ParamDef_Assume_Offline_Scope);
  FConfigParams.WriteValue(Param_CreateDatabase, true);
  FConfigParams.WriteValue(Param_TestConnection, true);
  var Session := New_MySQLSession(FConfigParams);
  FConfigParams.ClearKey(Param_CreateDatabase);
  FConfigParams.ClearKey(Param_TestConnection);
  Session.CreateCommand(SQL_CheckTable_Providers).Execute;
  Session.CreateCommand(SQL_CheckTable_Grants).Execute;
  Session.CreateCommand(SQL_CheckTable_Clients).Execute;
  Session.CreateCommand(ResolveSQL(SQL_CheckTable_Users)).Execute;
  Session.CreateCommand(SQL_CheckTable_Scopes).Execute;
  Session.CreateCommand(SQL_CheckTable_UserScopes).Execute;
  // get the Issuer,
  FIssuerUri := FConfigParams.StrValue(Param_Issuer);
  // get new user scopes
  FNewUser_Scopes := FConfigParams.StrValue(Param_NewUser_Scopes).Split(['  ']);
  // Initialze Sign Key
  var Key := FConfigParams.StrValue('signkey_priv');
  Key := TNetencoding.Base64URL.Decode(Key);
  FSignKey_Priv := TdwlOpenSSL.New_PrivateKey_FromPEMStr(Key);
  if FSignKey_Priv=nil then
    raise Exception.Create('No key found');
  var JSON := TJSONObject.Create;
  try
    var Keys := TJSONArray.Create;
    JSON.AddPair('keys', Keys);
    var JKey := TJSONObject.Create;
    Keys.Add(JKey);
    JKey.AddPair(jwt_Key_algoriTmfamilY, algorithmfamily_RSA);
    JKey.AddPair(joseheaderALGORITHM, algorithm_RS256);
    JKey.AddPair(algoparam_modulusN, TNetEncoding.Base64URL.EncodeBytesToString(FSignKey_Priv.Modulus_n));
    JKey.AddPair(joseheaderKEYID, '1');
    JKey.AddPair(algoparam_exponentE, TNetEncoding.Base64URL.EncodeBytesToString(FSignKey_Priv.PublicExponent_e));
    JKey.AddPair(jwt_key_USagE, usage_SIGnature);
    FCertsInfo := JSON.ToJSON;
  finally
    JSON.Free;
  end;
  // initialize OIDC Providers
  var Cmd := Session.CreateCommand(SQL_Get_Providers);
  Cmd.Execute;
  var Reader := Cmd.Reader;
  while Reader.Read do
  begin
    var Provider := TOIDC_Provider.Create(Reader.GetString(3), Reader.GetString(4), '');
    try
      Provider.Scopes.Add('email');
      Provider.Scopes.Add('profile');
      Provider.FTechName := Reader.GetString(0);
      Provider.FDisplayName := Reader.GetString(1, true, Provider.FTechName);
      Provider.FImgData := Reader.GetString(2, true);
      FProviders.Add(Provider);
      TdwlLogger.Log('Successfully Registered external OIDC Provider '+Provider.FTechName, lsTrace);
    except
      on E: Exception do
      begin
        TdwlLogger.Log('Error while registering external OIDC Provider '+Provider.FTechName+':'+E.Message, lsWarning);
        Provider.Free;
      end;
    end;
  end;
  BuildLoginForm;
  RegisterHandling(dwlhttpGET, OIDC_Well_Known_Config_Path, Get_wellknown_openidconfiguration, []);
  RegisterHandling(dwlhttpGET, '/authorize', GetPost_authorize, []);
  RegisterHandling(dwlhttpPOST, '/authorize', GetPost_authorize, []);
  RegisterHandling(dwlhttpGET, OIDC_Return_Path, Get_OIDC_Return_Path, []);
  RegisterHandling(dwlhttpGET, '/certs', Get_certs, []);
  RegisterHandling(dwlhttpPOST, '/authorize_reply', Post_authorize_reply, []);
  RegisterHandling(dwlhttpPOST, '/token', Post_token, []);
  RegisterHandling(dwlhttpGET, '/userinfo', GetPost_userinfo, []);
  RegisterHandling(dwlhttpPOST, '/userinfo', GetPost_userinfo, []);
end;

class function THandler_OAuth2.ConvertScope(State: PdwlHTTPHandlingState; UserID: integer; Scopes: TStringList): string;
const
  SQL_Get_Scopes_for_User_And_UmbrellaScopes='SELECT us.scope, s.umbrella_scope FROM dwl_oauth2_userscopes us LEFT JOIN dwl_oauth2_scopes s on s.scope=us.scope WHERE (us.user_id=?) AND (s.umbrella_scope IN (?';
begin
  Result := '';
  var Q := SQL_Get_Scopes_for_User_And_UmbrellaScopes;
  for var i := 0 to Scopes.Count-2 do
    Q := Q + ', ?';
  Q := Q+'))';
  var Cmd := MySQLCommand(State, Q);
  Cmd.Parameters.SetIntegerDataBinding(0, UserId);
  for var i := 0 to Scopes.Count-1 do
    Cmd.Parameters.SetTextDataBinding(i+1, Scopes[i]);
  Cmd.Execute;
  while Cmd.Reader.Read do
  begin
    var AddScope := Cmd.Reader.GetString(0);
    if Pos(' '+AddScope, Result)<1 then
      Result := Result+' '+AddScope;
    // umbrella scope is also added
    AddScope := Cmd.Reader.GetString(1);
    if Pos(' '+AddScope, Result)<1 then
      Result := Result+' '+AddScope;
  end;
  // This is a workaround for users without a scope at all: Just allow them anyway until I'm back from holidays and return first scope requested
  if Result='' then
    Result := ' '+Scopes[0];
  Result := Copy(Result, 2, MaxInt);
end;

class function THandler_OAuth2.CreateAccesstoken(UserID: integer; const ConvertedScope: string): string;
begin
  var JWT := New_JWT;
  JWT.Header.Values[joseheaderKEYID] := FSign_KeyID;
  JWT.Payload.Values[jwtclaimISSUER] := FIssuerUri;
  JWT.Payload.Values[jwtclaimSUBJECT] := UserID.ToString;
  JWT.Payload.Values[jwt_key_SCOPE] := ConvertedScope;
  JWT.Payload.IntValues[jwtclaimEXPIRATION_TIME] :=  TUnixEpoch.Now+ACCESS_DURATION+5; //seconds 'room';
  Result := JWT.Serialize(FSignKey_Priv);
end;

class function THandler_OAuth2.CreateIDToken(UserID: integer; Client_Id, Replay_Nonce: string; OutputEMail, OutputProfile: boolean): string;
begin
  var UserInfo: TUserInfo;
  FillChar(UserInfo, SizeOf(UserInfo), 0);
  UserInfo.Subject := UserID.ToString;
  if (FField_GivenName='') or (FField_FamilyName='') then
    OutputProfile := false;
  if OutputEmail or OutputProfile then
  begin
    EnrichUserInfo(@UserInfo, OutputEMail, OutputProfile);
  end;
  var JWT := New_JWT;
  JWT.Header.Values[joseheaderKEYID] := FSign_KeyID;
  JWT.Payload.Values[jwtclaimISSUER] := FIssuerUri;
  JWT.Payload.Values[jwtclaimSUBJECT] := UserInfo.Subject;
  JWT.Payload.Values[jwtclaimAUDIENCE] := Client_Id;
  JWT.Payload.IntValues[jwtclaimISSUED_AT] :=  TUnixEpoch.Now; //seconds 'room';
  JWT.Payload.IntValues[jwtclaimEXPIRATION_TIME] :=  TUnixEpoch.Now+IDTOKEN_DURATION+5; //seconds 'room';
  JWT.Payload.Values[jwt_key_NONCE] :=  Replay_Nonce;
  JWT.Payload.Values[jwtclaimEMAIL] := UserInfo.EMail;
  JWT.Payload.Values[jwtclaimNAME] := trim(UserInfo.GivenName+' '+UserInfo.FamilyName);
  JWT.Payload.Values[jwtclaimGIVEN_NAME] := UserInfo.GivenName;
  JWT.Payload.Values[jwtclaimFAMILY_NAME] := UserInfo.FamilyName;
  Result := JWT.Serialize(FSignKey_Priv);
end;

class function THandler_OAuth2.CreateRefreshtoken(UserID, Refreshtoken_order: integer; const GrantToken: string; const Scope: string): string;
begin
  var JWT := New_JWT;
  JWT.Header.Values[joseheaderKEYID] := FSign_KeyID;
  JWT.Payload.Values[jwtclaimISSUER] := FIssuerUri;
  JWT.Payload.IntValues[jwtclaimEXPIRATION_TIME] :=   TUnixEpoch.Now+GRANT_DURATION;
  JWT.Payload.Values[jwt_key_GRANT] := GrantToken;
  JWT.Payload.IntValues[jwt_key_ORDER] := Refreshtoken_order;
  JWT.Payload.Values[jwt_key_SCOPE] := Scope;
  Result := JWT.Serialize(FSignKey_Priv);
end;

class procedure THandler_OAuth2.EnrichUserInfo(UserInfo: PUserInfo; FillEmail, FillProfile: boolean);
const
  SQL_Get_EmailAddress = 'SELECT emailaddress FROM $(usertable) WHERE id=?';
  SQL_Get_FirstLastName = 'SELECT $(field_givenname), $(field_familyname) FROM $(usertable) WHERE id=?';
begin
  var Session := New_MySQLSession(FConfigParams);
  if FillEmail then
  begin
    var Cmd := Session.CreateCommand(ResolveSQL(SQL_Get_EmailAddress));
    Cmd.Parameters.SetTextDataBinding(0, UserInfo.Subject);
    Cmd.Execute;
    if Cmd.Reader.Read then
      UserInfo.EMail := Cmd.Reader.GetString(0, true);
  end;
  if FillProfile and (FField_GivenName<>'') and (FField_FamilyName<>'') then
  begin
    var SQL := ResolveSQL(SQL_Get_FirstLastName);
    SQL := StringReplace(SQL, '$(field_givenname)', FField_GivenName, [rfIgnoreCase]);
    SQL := StringReplace(SQL, '$(field_familyname)', FField_FamilyName, [rfIgnoreCase]);
    var Cmd := Session.CreateCommand(SQL);
    Cmd.Parameters.SetTextDataBinding(0,UserInfo.Subject);
    Cmd.Execute;
    if Cmd.Reader.Read then
    begin
      UserInfo.GivenName := Cmd.Reader.GetString(0, true);
      UserInfo.FamilyName := Cmd.Reader.GetString(1, true);
    end;
  end;
end;

class procedure THandler_OAuth2.BuildLoginForm;
begin
  var Strm := TResourceStream.Create(HInstance, 'LOGINPAGE', 'TEXT');
  var SList := TStringList.Create;
  try
    SList.LoadFromStream(Strm);
    FLoginPage := SList.Text;
  finally
    SList.Free;
    Strm.Free;
  end;
  var ProviderForms := '';
  if FProviders.Count>0 then
  begin
    // Add provider Forms
    ProviderForms := ProviderForms+'<br /><p>OR<br /><br />';
    for var i := 0 to FProviders.Count-1 do
    begin
      var Provider := FProviders[i];
      ProviderForms := ProviderForms+'<form method="post" action="./authorize_reply">'+
        '<input type="hidden" name="p" value="'+i.ToString+'">'+
        '<input type="hidden" name="session_state" value="$(session_state)">'+
        '<button class="item"><img src="'+
        Provider.FImgData+
        '">Continue with '+
        Provider.FDisplayname+
        '</button></form><br />';
    end;
  end;
  FLoginPage := StringReplace(FLoginPage, '$(providerforms)', ProviderForms, [rfIgnoreCase]);
end;

class function THandler_OAuth2.GetPost_authorize(const State: PdwlHTTPHandlingState): boolean;
const
  SQL_Get_Client = 'SELECT id, client_secret, redirect_uri, displayname FROM dwl_oauth2_clients WHERE client_id=?';
begin
  Result := true;
  // OpenID Connect only allows POST with Form Serialization
  if State.Command=dwlhttpPOST then
  begin
    var ContType: string;
    if not TryGetHeaderValue(State, HTTP_HEADER_CONTENT_TYPE, ContType) or
      (ContType<>CONTENT_TYPE_X_WWW_FORM_URLENCODED) then
    begin
      HandlingError(State);
      Exit;
    end;
  end;
  // get all the request params and validate them more or less
  var response_type: string;
  if not TryGetRequestParamStr(State, 'response_type', response_type) then
  begin
    HandlingError(State, 'missing response_type');
    Exit;
  end;
  if not SameText(response_type, 'code') then
  begin
    HandlingError(State, 'unsupported_response_type');
    Exit;
  end;

  var client_scope: string;
  if (not TryGetRequestParamStr(State, 'scope', client_scope)) or (trim(client_scope)='') then
  begin
    HandlingError(State, 'missing scope');
    Exit;
  end;
  var client_id: string;
  if not TryGetRequestParamStr(State, 'client_id', client_id) then
  begin
    HandlingError(State, 'missing client_id');
    Exit;
  end;

  var redirect_uri: string;
  if not TryGetRequestParamStr(State, 'redirect_uri', redirect_uri) then
  begin
    HandlingError(State, 'missing redirect_uri');
    Exit;
  end;

  var client_state: string;
  if not TryGetRequestParamStr(State, 'state', client_state) then
    client_state := '';

  var client_nonce: string;
  if not TryGetRequestParamStr(State, 'nonce', client_nonce) then
    client_nonce := '';
  // If PKCE is used, we need to keep the challenge to check it later
  var code_challenge_method: string;
  if not TryGetRequestParamStr(State, 'code_challenge_method', code_challenge_method) then
    code_challenge_method := ''
  else
  begin
    if (code_challenge_method<>'S256' ) and (code_challenge_method<>'plain') then
    begin
      HandlingError(State, 'unsupported code_challenge_method');
      Exit;
    end;
  end;
  var code_challenge: string;
  if not TryGetRequestParamStr(State, 'code_challenge', code_challenge) then
  begin
    if code_challenge_method<>'' then
    begin
      HandlingError(State, 'missing code_challenge');
      Exit;
    end;
    code_challenge := '';
  end;
  // validate the client requesting
  var MySQLSession := New_MySQLSession(FConfigParams);
  var Cmd := MySQLSession.CreateCommand(SQL_Get_Client);
  Cmd.Parameters.SetTextDataBinding(0, client_id);
  Cmd.Execute;
  if not Cmd.Reader.Read then
  begin
    HandlingError(State, 'invalid client_id');
    Exit;
  end;
  if Cmd.Reader.GetString(2, true)<>redirect_uri then
  begin
    HandlingError(State, 'unsupported redirect_uri');
    Exit;
  end;
  // create the Authorization session
  var AuthSession := TAuthenticationSession.Create;
  AuthSession.session_state := TNetEncoding.Base64URL.EncodeBytesToString(TdwlOpenSSL.RandomBytes(32));
  AuthSession.internal_clientid := Cmd.Reader.GetInteger(0);
  AuthSession.client_id := client_id;
  AuthSession.client_secret := Cmd.Reader.GetString(1, true);
  AuthSession.client_scope := client_scope;
  AuthSession.client_state := client_state;
  AuthSession.client_redirect_uri := redirect_uri;
  AuthSession.client_code_challenge := code_challenge;
  AuthSession.client_code_challenge_method := code_challenge_method;
  AuthSession.ExpirationTime :=  TUnixEpoch.Now+AUTHSESSION_DURATION;
  if not TryGetRequestParamStr(State, 'nonce', AuthSession.replay_nonce) then
    AuthSession.replay_nonce := '';
  FAuthenticationSessions.Add(AuthSession);

  var Page := StringReplace(FLoginPage, '$(displayname)', Cmd.Reader.GetString(3), [rfIgnoreCase]);
  Page := StringReplace(Page, '$(session_state)', AuthSession.session_state, [rfReplaceAll, rfIgnoreCase]);

  State.SetContentText(Page);
end;

class function THandler_OAuth2.GetPost_userinfo(const State: PdwlHTTPHandlingState): boolean;
begin
  Result := true;
  State.StatusCode := HTTP_STATUS_DENIED;
  var UserInfo: TUserInfo;
  UserInfo.Subject := Subject_From_Authorization(State);
  if UserInfo.Subject='' then
    Exit;
  EnrichUserInfo(@Userinfo, true, true);
  var JSON := Response_JSON(State);
  JSON.AddPair(jwtclaimSUBJECT, UserInfo.Subject);
  JSON.AddPair(jwtclaimEMAIL, UserInfo.EMail);
  JSON.AddPair(jwtclaimNAME, trim(UserInfo.GivenName+' '+UserInfo.FamilyName));
  JSON.AddPair(jwtclaimGIVEN_NAME, UserInfo.GivenName);
  JSON.AddPair(jwtclaimFAMILY_NAME, UserInfo.FamilyName);
  // All ok!
  State.StatusCode := HTTP_STATUS_OK;
end;

class function THandler_OAuth2.Post_authorize_reply(const State: PdwlHTTPHandlingState): boolean;
const
  SQL_Get_User_ByEmailAddress = 'SELECT id, salt, pwd FROM $(usertable) WHERE emailaddress=?';
  SQL_Get_User_Md5_ByEmailAddress = 'SELECT $(field_md5) FROM $(usertable) WHERE emailaddress=?';
  SQL_Update_User_Password = 'UPDATE $(usertable) SET salt=?, pwd=? WHERE id=?';
begin
  Result := true;
  // Get needed parameter values
  var SessionState: string;
  if not TryGetRequestParamStr(State, 'session_state', SessionState) then
  begin
    HandlingError(State);
    Exit;
  end;
  // find applicable authorization session
  var AuthSession := GetAuthenticationSessionByProviderState(SessionState);
  if AuthSession=nil then
  begin
    HandlingError(State, 'state mismatch');
    Exit;
  end;
  var DisposeAuthSession := true;
  try
    var P: integer;
    if not TryGetRequestParamInt(State, 'p', P) then
    begin
      HandlingError(State);
      Exit;
    end;
    if P<0 then
    begin  // This is a local login: process here
      var EmailAddress: string;
      if not TryGetRequestParamStr(State, 'emailaddress', EmailAddress) then
      begin
        HandlingError(State);
        Exit;
      end;
      var Password: string;
      if not TryGetRequestParamStr(State, 'password', Password) then
      begin
        HandlingError(State);
        Exit;
      end;
      // Check the provided credentials
      var Session := New_MySQLSession(FConfigParams);
      var Cmd := Session.CreateCommand(ResolveSQL(SQL_Get_User_ByEmailAddress));
      Cmd.Parameters.SetTextDataBinding(0, EmailAddress);
      Cmd.Execute;
      var Reader := Cmd.Reader;
      if not Reader.Read then
      begin
        State.StatusCode := HTTP_STATUS_DENIED;
        Exit;
      end;
      var Salt := Reader.GetString(1, true);
      var CheckPwd := Reader.GetString(2, true);
      if (Password='') or (Salt='') or (CheckPwd='') or (TdwlOpenSSL.DeriveKeyFromPassword(Salt, Password)<>Checkpwd) then
      begin
        if (Salt='') and (CheckPwd='') and (FField_MD5<>'') then // do md5 migration
        begin
          var Cmd_Migrate := Session.CreateCommand(StringReplace(ResolveSQL(SQL_Get_User_Md5_ByEmailAddress), '$(field_md5)', FField_MD5, [rfIgnoreCase]));
          Cmd_Migrate.Parameters.SetTextDataBinding(0, EmailAddress);
          Cmd_Migrate.Execute;
          if Cmd_Migrate.Reader.Read then
          begin
            if not SameText(Cmd_Migrate.Reader.GetString(0, true), TdwlCrypt.MD5(Password)) then
            begin
              State.StatusCode := HTTP_STATUS_DENIED;
              Exit;
            end;
            Salt := TNetEncoding.Base64URL.EncodeBytesToString(TdwlOpenSSL.RandomBytes(32));
            var Cmd2 := Session.CreateCommand(ResolveSQL(SQL_Update_User_Password));
            Cmd2.Parameters.SetTextDataBinding(0, Salt);
            Cmd2.Parameters.SetTextDataBinding(1, TdwlOpenSSL.DeriveKeyFromPassword(Salt, Password));
            Cmd2.Parameters.SetIntegerDataBinding(2, Reader.GetInteger(0));
            Cmd2.Execute;
          end
          else
          begin
            State.StatusCode := HTTP_STATUS_DENIED;
            Exit;
          end;
        end
        else
        begin
          State.StatusCode := HTTP_STATUS_DENIED;
          Exit;
        end;
      end;
      var UserId := Reader.GetInteger(0);
      AuthSession.ProviderIndex := -1;
      AuthSession.authenticated_userid := UserId;
      // and redirect to the client
      AuthSession.UpdateState_CodeRedirection(State);
    end
    else
    begin
      if P>=FProviders.Count then
      begin
        HandlingError(State);
        Exit;
      end;
      var Provider := FProviders[p];
      // Create Provider Session
      AuthSession.ProviderIndex := p;
      AuthSession.provider_session.Init(AuthSession.session_state);
      // fill redirect_uri on first pass (the BaseUri is filled by now)
      if Provider.Redirect_Uri='' then
        Provider.Redirect_Uri := FIssuerUri+OIDC_Return_Path;
      // redirect to the provider
      State.SetHeaderValue('Location', Provider.GetAuthorizeUri(@AuthSession.provider_session));
      State.StatusCode := HTTP_STATUS_REDIRECT;
      // we will see this authorization request coming back via at OIDC_Return_Path
    end;
    DisposeAuthSession := false;
  finally
    if DisposeAuthSession then
    begin
      FAuthenticationSessions.Remove(AuthSession);
      AuthSession.Free;
    end;
  end;
end;

class function THandler_OAuth2.Get_certs(const State: PdwlHTTPHandlingState): boolean;
begin
  State.SetContentText(FCertsInfo, CONTENT_TYPE_JSON);
  Result := true;
end;

class function THandler_OAuth2.Get_OIDC_Return_Path(const State: PdwlHTTPHandlingState): boolean;
begin
  Result := true;
  // Get needed parameter values
  var Code: string;
  if not TryGetRequestParamStr(State, 'code', Code) then
  begin
    HandlingError(State);
    Exit;
  end;
  var ReturnedState: string;
  if not TryGetRequestParamStr(State, 'state', ReturnedState) then
  begin
    HandlingError(State);
    Exit;
  end;
  // find applicable authorization session
  var AuthSession := GetAuthenticationSessionByProviderState(ReturnedState);
  if AuthSession=nil then
  begin
    HandlingError(State, 'authentication state mismatch');
    Exit;
  end;
  var DisposeAuthSession := true;
  try
    var Res := FProviders[AuthSession.ProviderIndex].GetIDTokenFromReceivedCode(code, @AuthSession.provider_session, AuthSession.provider_idtoken);
    if not Res.Success then
    begin
      AuthSession.provider_idtoken := nil;
      HandlingError(State, Res.ErrorMsg);
      Exit;
    end;
    // Now we have oidc_subject validated, it's time to redirect back to the client
    AuthSession.UpdateState_CodeRedirection(State);
    DisposeAuthSession := false;
  finally
    if DisposeAuthSession then
    begin
      FAuthenticationSessions.Remove(AuthSession);
      AuthSession.Free;
    end;
  end;
end;

class function THandler_OAuth2.Get_wellknown_openidconfiguration(const State: PdwlHTTPHandlingState): boolean;
const
  SQL_Get_All_Umbrella_Scopes = 'SELECT DISTINCT umbrella_scope FROM dwl_oauth2_scopes ';
begin
  var JSON := Response_JSON(State);
  JSON.AddPair('issuer', FIssuerUri);
  JSON.AddPair('authorization_endpoint', FIssuerUri+'/authorize');
  JSON.AddPair('token_endpoint', FIssuerUri+'/token');
  JSON.AddPair('userinfo_endpoint', FIssuerUri+'/userinfo');
  JSON.AddPair('jwks_uri', FIssuerUri+'/certs');
  var Arr := TJSONArray.Create;
  JSON.AddPair('response_types_supported', Arr);
  Arr.Add('code');
  Arr := TJSONArray.Create;
  JSON.AddPair('subject_types_supported', Arr);
  Arr.Add('public');
  Arr := TJSONArray.Create;
  JSON.AddPair('scopes_supported', Arr);
  var Cmd :=  New_MySQLSession(FConfigParams).CreateCommand(SQL_Get_All_Umbrella_Scopes);
  Cmd.Execute;
  while Cmd.Reader.Read do
    Arr.Add(Cmd.Reader.GetString(0));
  Arr := TJSONArray.Create;
  JSON.AddPair('id_token_signing_alg_values_supported', Arr);
  Arr.Add('RS256');
  Result := true;
end;

class procedure THandler_OAuth2.HandlingError(const State: PdwlHTTPHandlingState; const ErrorMessage: string=''; const StatusCode: integer=HTTP_STATUS_BAD_REQUEST);
begin
  State.StatusCode := StatusCode;
  if ErrorMessage='' then
    Response_JSON(State).AddPair('error', 'invalid request')
  else
    Response_JSON(State).AddPair('error', ErrorMessage);
end;

class function THandler_OAuth2.Post_token(const State: PdwlHTTPHandlingState): boolean;
const
  grant_type_none = 0;
  grant_type_refresh_token = 1;
  grant_type_authorization_code = 2;
  grant_type_client_credentials = 3;
begin
  Result := true;
  // Get needed parameter values
  var grant_type: string;
  if not TryGetRequestParamStr(State, 'grant_type', grant_type) then
  begin
    HandlingError(State, 'missing grant_type parameter');
    Exit;
  end;
  var GrantType := grant_type_none;
  if grant_type='refresh_token' then
    GrantType := grant_type_refresh_token
  else
  if grant_type='authorization_code' then
    GrantType := grant_type_authorization_code
  else
  if grant_type='client_credentials' then
    GrantType := grant_type_client_credentials;
  if GrantType=grant_type_none then
  begin
    HandlingError(State, 'unsupported grant_type');
    Exit;
  end;
  var UserID := -1;
  var ClientID := '';
  var AuthorizeNonce := '';
  var GrantToken := '';
  var Refreshtoken_Order := 1;
  var Success := false;
  var RequestedScopes := TStringList.Create;
  try
    RequestedScopes.Delimiter := ' ';
    case GrantType of
    grant_type_authorization_code: Success := Post_token_handle_authorization_code(State, UserID, RequestedScopes, ClientID, AuthorizeNonce, GrantToken);
    grant_type_refresh_token: Success := Post_token_handle_refresh_token(State, UserID, Refreshtoken_Order, RequestedScopes, GrantToken);
    grant_type_client_credentials: Success := Post_token_handle_client_credentials(State, UserID, RequestedScopes);
    end;
    if not Success then
      Exit;
    // Scope openid
    var ScopeIndex := RequestedScopes.IndexOf(Scope_openid);
    var OutputIDToken := (GrantType=grant_type_authorization_code) and (ScopeIndex>=0);
    if ScopeIndex>=0 then
      RequestedScopes.Delete(ScopeIndex);
    // scope offline_access
    ScopeIndex := RequestedScopes.IndexOf(Scope_offline_access);
    var OutputRefreshtoken := (GrantToken<>'') and ((GrantType=grant_type_refresh_token) or FAssume_Offline_Scope or (ScopeIndex>=0));
    if ScopeIndex>=0 then
      RequestedScopes.Delete(ScopeIndex);
    // Scope email
    ScopeIndex := RequestedScopes.IndexOf(Scope_email);
    var OutputEmail := OutputIDToken and (ScopeIndex>=0);
    if ScopeIndex>=0 then
      RequestedScopes.Delete(ScopeIndex);
    // scope profile
    ScopeIndex := RequestedScopes.IndexOf(Scope_profile);
    var OutputProfile := OutputIDToken and (ScopeIndex>=0);
    if ScopeIndex>=0 then
      RequestedScopes.Delete(ScopeIndex);
    // Convert to Output Scope
    var Accesstoken_ConvertedScope := ConvertScope(State, UserId, RequestedScopes);
    if (Accesstoken_ConvertedScope='') then
    begin
      State.StatusCode := HTTP_STATUS_DENIED;
      Exit;
    end;
    // all info is gathered, all is checked
    // issue the result
    var JSON := Response_JSON(State);
    if OutputRefreshtoken then
      JSON.AddPair(keyREFRESH_TOKEN, CreateRefreshtoken(UserID, Refreshtoken_Order, GrantToken, RequestedScopes.DelimitedText));
    JSON.AddPair(keyACCESS_TOKEN, CreateAccesstoken(UserID, Accesstoken_ConvertedScope));
    JSON.AddPair(keyEXPIRES_IN, ACCESS_DURATION.ToString);
    JSON.AddPair(keySCOPE, Accesstoken_ConvertedScope);
    JSON.AddPair(keyTOKEN_TYPE, tokentypeBearer);
    if OutputIDToken then
      JSON.AddPair(keyID_TOKEN, CreateIDToken(UserID, ClientID, AuthorizeNonce, OutputEmail, OutputProfile));
  finally
    RequestedScopes.Free;
  end;
end;

class function THandler_OAuth2.Post_token_handle_authorization_code(const State: PdwlHTTPHandlingState; var UserID: integer; RequestedScopes: TStringList; var client_id, AuthorizeNonce, GrantToken: string): boolean;
const
  SQL_Create_Grant = 'INSERT INTO dwl_oauth2_grants (token, user_id, client_id, expirationtime, refreshtoken_order, scope, id_token) VALUES (?, ?, ?, ?, ?, ?, ?)';
  SQL_Get_User_ByProviderAndSubject = 'SELECT id FROM $(usertable) WHERE oidc_provider=? and oidc_subject=?';
  SQL_Create_User = 'INSERT INTO $(usertable) (oidc_provider, oidc_subject, emailaddress, name) VALUES (?, ?, ?, ?)';
  SQL_Create_UserScope = 'INSERT INTO dwl_oauth2_userscopes (user_id, scope) VALUES (?, ?)';
begin
  Result := false;
  var client_secret := '';
  if TryGetRequestParamStr(State, 'client_id', client_id) then
    TryGetRequestParamStr(State, 'client_secret', client_secret)
  else
    client_secret := GetClientInfoFromHeader(State, client_id);
  if client_id='' then
  begin
    HandlingError(State, 'missing client_id parameter');
    Exit;
  end;
  var Code: string;
  if not TryGetRequestParamStr(State, 'code', Code) then
  begin
    HandlingError(State, 'missing code parameter');
    Exit;
  end;
  var AuthSession := PopAuthenticationSessionByAuthorizationCode(Code);
  if AuthSession=nil then
  begin
    HandlingError(State, 'authentication state mismatch');
    Exit;
  end;
  AuthorizeNonce := AuthSession.replay_nonce;
  try
    if client_id<>AuthSession.client_id then
    begin
      HandlingError(State, 'client_id mismatch');
      Exit;
    end;
    // If PKCE is used, we need to check the challenge
    var code_verifier: string;
    if not TryGetRequestParamStr(State, 'code_verifier', code_verifier) then
      code_verifier := '';
    if (code_verifier<>'') or (AuthSession.client_code_challenge<>'') or (AuthSession.client_code_challenge_method<>'') then
    begin
      var code_challenge := code_verifier;
      if AuthSession.client_code_challenge_method='S256' then
        code_challenge := TNetEncoding.Base64URL.EncodeBytesToString(TdwlOpenSSL.Calculate_SHA256(ansistring(code_challenge)));
      if code_challenge<>AuthSession.client_code_challenge then
      begin
        HandlingError(State, 'invalid code_verifier');
        Exit;
      end;
    end;
    if (client_secret='') and (code_verifier='') then
    begin
      HandlingError(State, 'missing code_verifier or client_secret parameter');
      Exit;
    end
    else
    begin
      if client_secret<>AuthSession.client_secret then
      begin
        HandlingError(State, 'client_secret mismatch');
        Exit;
      end;
    end;
    var redirect_uri: string;
    if not TryGetRequestParamStr(State, 'redirect_uri', redirect_uri) then
    begin
      HandlingError(State, 'missing redirect_uri parameter');
      Exit;
    end;
    if redirect_uri<>AuthSession.client_redirect_uri then
    begin
      HandlingError(State, 'redirect_uri mismatch');
      Exit;
    end;
    // Everything seems in order, so start issueing the requested tokens
    if AuthSession.ProviderIndex>=0 then // oidc provider used, get local user
    begin
      var Cmd := MySQLCommand(State, ResolveSQL(SQL_Get_User_ByProviderAndSubject));
      Cmd.Parameters.SetTextDataBinding(0, FProviders[AuthSession.ProviderIndex].FTechName);
      Cmd.Parameters.SetTextDataBinding(1, AuthSession.provider_idtoken.Payload.Values[jwtclaimSUBJECT]);
      Cmd.Execute;
      if not Cmd.Reader.Read then
      begin
        // No user found, create it!
        Cmd := MySQLCommand(State, ResolveSQL(SQL_Create_User));
        if AuthSession.ProviderIndex>=0 then
          Cmd.Parameters.SetTextDataBinding(0, FProviders[AuthSession.ProviderIndex].FTechName)
        else
          Cmd.Parameters.SetTextDataBinding(0, 'local');
        Cmd.Parameters.SetTextDataBinding(1, AuthSession.provider_idtoken.Payload.Values[jwtclaimSUBJECT]);
        Cmd.Parameters.SetTextDataBinding(2, AuthSession.provider_idtoken.Payload.Values[jwtclaimEMAIL]);
        Cmd.Parameters.SetTextDataBinding(3, AuthSession.provider_idtoken.Payload.Values[jwtclaimNAME]);
        Cmd.Execute;
        UserID := Cmd.LastInsertID;
        if High(FNewUser_Scopes)>=0 then
        begin
          Cmd := MySQLCommand(State, SQL_Create_UserScope);
          Cmd.Parameters.SetIntegerDataBinding(0, UserID);
          for var Scope in FNewUser_Scopes do
          begin
            Cmd.Parameters.SetTextDataBinding(1, Scope);
            Cmd.Execute(true);
          end;
        end;
      end
      else
        UserID := Cmd.Reader.GetInteger(0);
    end
    else
      UserID := AuthSession.authenticated_userid;
    RequestedScopes.DelimitedText := AuthSession.client_scope;
    // Now we have the userid, create a grant
    GrantToken := TNetEncoding.Base64URL.EncodeBytesToString(TdwlOpenSSL.RandomBytes(32));
    var Cmd := MySQLCommand(State, SQL_Create_Grant);
    Cmd.Parameters.SetTextDataBinding(0, GrantToken);
    Cmd.Parameters.SetIntegerDataBinding(1, UserID);
    Cmd.Parameters.SetIntegerDataBinding(2, AuthSession.internal_clientid);
    Cmd.Parameters.SetBigIntegerDataBinding(3, TUnixEpoch.Now+GRANT_DURATION);
    Cmd.Parameters.SetIntegerDataBinding(4, 1); // refreshtoken order=1
    Cmd.Parameters.SetTextDataBinding(5, AuthSession.client_scope);
    if AuthSession.provider_idtoken<>nil then
      Cmd.Parameters.SetTextDataBinding(6, AuthSession.provider_idtoken.Payload.ToJSON)
    else
      Cmd.Parameters.SetNullDataBinding(6);
    Cmd.Execute;
  finally
    AuthSession.Free;
  end;
  Result := true;
end;

class function THandler_OAuth2.Post_token_handle_client_credentials(const State: PdwlHTTPHandlingState; var UserID: integer; RequestedScopes: TStringList): boolean;
const
  SQL_Get_User_By_Client_Credentials = 'SELECT user_id FROM dwl_oauth2_clients WHERE client_id=? and client_secret=?';
begin
  Result := false;
  var client_id: string;
  var client_secret: string;
  if TryGetRequestParamStr(State, 'client_id', client_id) then
  begin
    if not TryGetRequestParamStr(State, 'client_secret', client_secret) then
    begin
      HandlingError(State, 'missing client_secret');
      Exit;
    end;
  end
  else
    client_secret := GetClientInfoFromHeader(State, client_id);
  if client_id='' then
  begin
    HandlingError(State, 'missing client_id');
    Exit;
  end;
  var RequestedScope: string;
  if (not TryGetRequestParamStr(State, 'scope', RequestedScope)) or (trim(RequestedScope)='') then
  begin
    HandlingError(State, 'missing scope parameter');
    Exit;
  end;
  RequestedScopes.DelimitedText := RequestedScope;
  var Cmd := MySQLCommand(State, SQL_Get_User_By_Client_Credentials);
  Cmd.Parameters.SetTextDataBinding(0, client_id);
  Cmd.Parameters.SetTextDataBinding(1, client_secret);
  Cmd.Execute;
  if Cmd.Reader.Read then
  begin
    UserID := Cmd.Reader.GetInteger(0, true, -1);
    Result := UserID>=0;
  end;
  if not Result then
    HandlingError(State, 'wrong credentials', HTTP_STATUS_DENIED);
end;

class function THandler_OAuth2.Post_token_handle_refresh_token(const State: PdwlHTTPHandlingState; var UserID, Refreshtoken_Order: integer; RequestedScopes: TStringList; var GrantToken: string): boolean;
const
  SQL_Set_Grant_RefreshTokenOrderAndExpiration = 'UPDATE dwl_oauth2_grants SET refreshtoken_order=?, expirationtime=? WHERE id=?';
  SQL_Get_Grant = 'SELECT g.id, g.user_id, c.client_id, g.expirationtime, g.refreshtoken_order, g.scope FROM dwl_oauth2_grants g LEFT JOIN dwl_oauth2_clients c on c.id=g.client_id WHERE token=?';
  SQL_Del_Grant = 'DELETE FROM dwl_oauth2_grants WHERE id=?';
begin
  Result := false;
  var client_id := '';
  if not TryGetRequestParamStr(State, 'client_id', client_id) then
    GetClientInfoFromHeader(State, client_id);
  if client_id='' then
  begin
    HandlingError(State, 'missing client_id parameter');
    Exit;
  end;
  var refresh_token: string;
  if not TryGetRequestParamStr(State, 'refresh_token', refresh_token) then
  begin
    HandlingError(State, 'missing refresh_token parameter');
    Exit;
  end;
  try
    var JWT := New_JWT_FromSerialization(refresh_token);
    // we now check with the one and only known keys,
    // if we ever start using multiple keys, find the right one here
    // and we check with the private key as it also containts the public key
    if not JWT.CheckSignature(FSignKey_Priv) then
    begin
      HandlingError(State, 'invalid refreshtoken', HTTP_STATUS_DENIED);
      Exit;
    end;
    var ThisMoment: Int64 := TUnixEpoch.Now;
    if JWT.Payload.IntValues[jwtclaimEXPIRATION_TIME]<ThisMoment then
    begin
      HandlingError(State, 'refresh_token expired', HTTP_STATUS_DENIED);
      Exit;
    end;
    GrantToken := JWT.Payload.Values[jwt_key_GRANT];
    var Cmd := MySQLCommand(State, SQL_Get_Grant);
    Cmd.Parameters.SetTextDataBinding(0, GrantToken);
    Cmd.Execute;
    if not Cmd.Reader.Read then
    begin
      HandlingError(State, 'invalid refreshtoken', HTTP_STATUS_DENIED);
      Exit;
    end;
    if Cmd.Reader.GetString(2, true)<>client_id then
    begin
      HandlingError(State, 'invalid client_id parameter');
      Exit;
    end;
    if Cmd.Reader.GetInt64(3)<ThisMoment then
    begin
      HandlingError(State, 'refresh_token expired', HTTP_STATUS_DENIED);
      Exit;
    end;
    RequestedScopes.DelimitedText := JWT.Payload.Values[jwt_key_SCOPE];
    RefreshToken_Order := Cmd.Reader.GetInteger(4);
    var GrantID := Cmd.Reader.GetInteger(0);
    if JWT.Payload.Values[jwt_key_SCOPE]<>Cmd.Reader.GetString(5) then
    begin
      HandlingError(State, 'invalid scope');
      Exit;
    end;
    if JWT.Payload.IntValues[jwt_key_ORDER]<>RefreshToken_Order then
    begin
      // if we for any reason receive a refreshtoken out of sync, the grant is
      // invalidated immediately
      var CmdDel := MySQLCommand(State, SQL_Del_Grant);
      CmdDel.Parameters.SetIntegerDataBinding(0, GrantID);
      CmdDel.Execute;
      HandlingError(State, 'invalid refresh_token', HTTP_STATUS_DENIED);
      Exit;
    end;
    // increment refreshtoken_order and post
    inc(Refreshtoken_order);
    var CmdSet := MySQLCommand(State, SQL_Set_Grant_RefreshTokenOrderAndExpiration);
    CmdSet.Parameters.SetIntegerDataBinding(0, Refreshtoken_Order);
    CmdSet.Parameters.SetBigIntegerDataBinding(1, TUnixEpoch.Now+GRANT_DURATION);
    CmdSet.Parameters.SetIntegerDataBinding(2, GrantID);
    CmdSet.Execute;
    // finally set userid
    UserID := Cmd.Reader.GetInteger(1);
    // all set, the actual tokens are created below
  except
    HandlingError(State, 'invalid refresh_token parameter');
    Exit;
  end;
  Result := true;
end;

class function THandler_OAuth2.ResolveSQL(const SQL: string): string;
begin
  Result := StringReplace(SQL, '$(usertable)', FUserTable, [rfIgnoreCase]);
end;

class function THandler_OAuth2.Subject_From_Authorization(const State: PdwlHTTPHandlingState): string;
begin
  Result := '';
  try
    var AuthStr: string;
    if not TryGetHeaderValue(State, 'Authorization', AuthStr) then
      Exit;
    AuthStr := trim(AuthStr);
    if not SameText(Copy(AuthStr, 1, 7), 'Bearer ') then
      Exit;
    var AccessToken := trim(Copy(AuthStr, 8, MaxInt));
    var JWT := New_JWT_FromSerialization(AccessToken);
    if JWT.Header.Values[joseheaderKEYID] <> FSign_KeyID then
      Exit;
    if not JWT.CheckSignature(FSignKey_Priv) then
      Exit;
    if not  (JWT.Payload.IntValues[jwtclaimEXPIRATION_TIME]>TUnixEpoch.Now) then
      Exit;
    if not SameText(JWT.Payload.Values[jwtclaimISSUER], FIssuerUri) then
      Exit;
    // All ok set result
    Result := JWT.Payload.Values[jwtclaimSUBJECT];
  except
  end;
end;

class procedure THandler_OAuth2.WrapUp(const State: PdwlHTTPHandlingState);
begin
  if State=nil then
  begin
    FProviders.Free;
    var Lst := FAuthenticationSessions.LockList;
    try
      for var Sess in Lst do
        Sess.Free;
    finally
      FAuthenticationSessions.UnlockList;
    end;
    FAuthenticationSessions.Free;
  end;
  inherited WrapUp(State);
end;

{ TAuthenticationSession }

procedure TAuthenticationSession.UpdateState_CodeRedirection(const State: PdwlHTTPHandlingState);
begin
  authorization_code := TNetEncoding.Base64URL.EncodeBytesToString(TdwlOpenSSL.RandomBytes(32));
  State.SetHeaderValue('Location', client_redirect_uri+'?code='+authorization_code+IfThen(client_state<>'', '&state='+client_state));
  State.StatusCode := HTTP_STATUS_REDIRECT;
end;

end.




