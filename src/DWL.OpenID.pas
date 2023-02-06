/// <summary>
///   An OpenID/OAuth2 client implementation
/// </summary>
unit DWL.OpenID;

interface

uses
  DWL.JOSE, System.Classes, System.JSON, DWL.Classes;

const
  OIDC_Well_Known_Config_Path = '/.well-known/openid-configuration';

  keyREFRESH_TOKEN='refresh_token';
  keyACCESS_TOKEN='access_token';
  keyID_TOKEN='id_token';
  keyEXPIRES_IN='expires_in';
  keySCOPE='scope';
  keyTOKEN_TYPE='token_type';

  tokentypeBEARER='Bearer';

  Scope_openid = 'openid';
  Scope_offline_access = 'offline_access';
  Scope_email = 'email';
  Scope_profile = 'profile';

type
  PdwlOIDC_Client_Session = ^TdwlOIDC_Client_Session;
  /// <summary>
  ///   The details of an client session
  /// </summary>
  TdwlOIDC_Client_Session = record
    State: string;
    Nonce: string;
    CodeVerifier: string;
    procedure Init(const AState: string);
  end;

  TdwlOIDC_Client = class
  strict private
    FAuthorization_Endpoint: string;
    FClient_Id: string;
    FIssuer_Uri: string;
    FJWKs: IdwlJWKs;
    FRedirect_Uri: string;
    FScopes: TStringList;
    FToken_Endpoint: string;
    procedure InitFromIssuer;
  protected
    function ExchangeReceivedCode(const Code: string; Session: PdwlOIDC_Client_Session; out JSON_MustBeFreed: TJSONValue): TdwlResult;
  public
    /// <summary>
    ///   The uri of the issuer, needs to be provided, this is the starting
    ///   point of all OIDC actions
    /// </summary>
    property Issuer_Uri: string read FIssuer_Uri;
    /// <summary>
    ///   The redirect_uri, needs to be provided, is part of implementation and
    ///   authorization information
    /// </summary>
    property Redirect_Uri: string read FRedirect_Uri write FRedirect_Uri;
    /// <summary>
    ///   The scopes that are requested by the authentication
    /// </summary>
    property Scopes: TStringList read FScopes;
    /// <summary>
    ///   Create the client and immediately provide the endpiont/authentication
    ///   information: issuer, client_id and redirect_uri
    /// </summary>
    constructor Create(const Issuer_Uri, Client_Id, Redirect_Uri: string);
    destructor Destroy; override;
    /// <summary>
    ///   Checks if an JSON Web tokens originates from this provider
    /// </summary>
    function CheckJWT(JWT: IdwlJWT): TdwlResult;
    /// <summary>
    ///   gets the authorize uri of a session
    /// </summary>
    function GetAuthorizeUri(Session: PdwlOIDC_Client_Session; ForceLoginPrompt: boolean=false): string;
    /// <summary>
    ///   Exchanges a receive code for an ID Token
    /// </summary>
    function GetIDTokenFromReceivedCode(const Code: string; Session: PdwlOIDC_Client_Session; out ID_Token: IdwlJWT): TdwlResult;
    /// <summary>
    ///   Exchanges a received code for a refreshtoken(and accesstoken)
    /// </summary>
    function GetRefreshAndAccessTokenFromReceivedCode(const Code: string; Session: PdwlOIDC_Client_Session; out Refresh_Token, Access_token: string; out Expires_In: integer): TdwlResult;
    /// <summary>
    ///   gets a new accesstoken by providing an accesstoken
    /// </summary>
    function GetAccessTokenFromRefreshToken(var Refresh_Token: string; out Access_token: string; out Expires_In: integer): TdwlResult;
  end;

implementation

uses
  DWL.HTTP.Client, DWL.HTTP.Consts, System.NetEncoding, Winapi.WinInet,
  System.StrUtils, DWL.OpenSSL, System.SysUtils;

{ TOIDC_Client }

function TdwlOIDC_Client.CheckJWT(JWT: IdwlJWT): TdwlResult;
begin
  InitFromIssuer;
  // find applicable key to check signature
  var JWK: IdwlJWK;
  var kid := JWT.Header.Values[joseheaderKEYID];
  if not FJWKs.TryGetKey(kid, JWK) then
    Result.AddErrorMsg('no key available for signature check')
  else
  begin
    // check signature of ID_Token
    if not JWT.CheckSignature(JWK.OpenSSLKey) then
      Result.AddErrorMsg('id_token signature invalid');
  end;
end;

constructor TdwlOIDC_Client.Create(const Issuer_Uri, Client_Id, Redirect_Uri: string);
begin
  inherited Create;
  FScopes := TStringList.Create;
  FScopes.Delimiter := ' ';
  FClient_Id := Client_Id;
  FRedirect_Uri := Redirect_Uri;
  FIssuer_Uri := Issuer_Uri;
end;

destructor TdwlOIDC_Client.Destroy;
begin
  FScopes.Free;
  inherited Destroy;
end;

function TdwlOIDC_Client.ExchangeReceivedCode(const Code: string; Session: PdwlOIDC_Client_Session; out JSON_MustBeFreed: TJSONValue): TdwlResult;
begin
  InitFromIssuer;
  var Request := New_HTTPRequest(FToken_Endpoint);
  Request.Method := HTTP_COMMAND_POST;
  Request.Header['Content-Type'] := CONTENT_TYPE_X_WWW_FORM_URLENCODED;
  Request.WritePostData('code='+TNetEncoding.URL.Encode(Code)+
    '&client_id='+TNetEncoding.URL.Encode(FClient_id)+
    '&redirect_uri='+TNetEncoding.URL.Encode(FRedirect_Uri)+
    '&code_verifier='+Session.CodeVerifier+
    '&grant_type=authorization_code');
  var Response := Request.Execute;
  if Response.StatusCode<>HTTP_STATUS_OK then
  begin
    Result.AddErrorMsg('error fetching tokens from OIDC Provider');
    Exit;
  end;
  JSON_MustBeFreed := TJSONObject.ParseJSONValue(Response.AsString);
end;

function TdwlOIDC_Client.GetAccessTokenFromRefreshToken(var Refresh_Token: string; out Access_token: string; out Expires_In: integer): TdwlResult;
begin
  InitFromIssuer;
  var Request := New_HTTPRequest(FToken_Endpoint);
  Request.Method := HTTP_COMMAND_POST;
  Request.Header['Content-Type'] := CONTENT_TYPE_X_WWW_FORM_URLENCODED;
  Request.WritePostData(keyREFRESH_TOKEN+'='+TNetEncoding.URL.Encode(Refresh_Token)+
    '&client_id='+TNetEncoding.URL.Encode(FClient_id)+
    '&grant_type=refresh_token');
  var Response := Request.Execute;
  if Response.StatusCode<>HTTP_STATUS_OK then
  begin
    if Response.StatusCode=HTTP_STATUS_DENIED then
      Refresh_Token := ''; // we need to re-authenticate
    Result.AddErrorMsg('error fetching tokens from OIDC Provider');
    Exit;
  end;
  var JSON := TJSONObject.ParseJSONValue(Response.AsString);
  try
    Access_Token := JSON.GetValue<string>(keyACCESS_TOKEN);
    Expires_In := JSON.GetValue<integer>(keyEXPIRES_IN);
    var RT := JSON.GetValue<string>(keyREFRESH_TOKEN, '');
    if RT<>'' then
      Refresh_Token := RT;
  finally
    JSON.Free;
  end;
end;

function TdwlOIDC_Client.GetAuthorizeUri(Session: PdwlOIDC_Client_Session; ForceLoginPrompt: boolean=false): string;
begin
  InitFromIssuer;
  Result := FAuthorization_Endpoint+
    '?scope='+TNetEncoding.URL.Encode(Scopes.DelimitedText)+
    '&response_type=code'+
    '&state='+Session.State+
    '&nonce='+Session.Nonce+
    '&redirect_uri='+TNetEncoding.URL.Encode(FRedirect_Uri)+
    '&client_id='+TNetEncoding.URL.Encode(FClient_id)+
    IfThen(ForceLoginPrompt, '&prompt=login', '')+
    '&code_challenge_method=S256'+
    '&code_challenge='+TNetEncoding.Base64URL.EncodeBytesToString(TdwlOpenSSL.Calculate_SHA256(ansistring(Session.CodeVerifier)));
end;

function TdwlOIDC_Client.GetIDTokenFromReceivedCode(const Code: string; Session: PdwlOIDC_Client_Session; out ID_Token: IdwlJWT): TdwlResult;
begin
  var JSONResponse: TJSONValue;
  Result := ExchangeReceivedCode(Code, Session, JSONResponse);
  if not Result.Success then
    Exit;
  try
    try
      ID_Token := New_JWT_FromSerialization(JSONResponse.GetValue<string>('id_token'));
    except
      Result.AddErrorMsg('error decoding id_token');
      Exit;
    end;
  finally
    JSONResponse.Free;
  end;
  // Check Signature
  var Res := CheckJWT(ID_Token);
  if not Res.Success then
  begin
    ID_Token := nil;
    Result.Merge(Res);
    Exit;
  end;
  // check the nonce
  if ID_Token.Payload.Values[jwt_key_NONCE]<>Session.Nonce then
  begin
    ID_Token := nil;
    Result.AddErrorMsg('nonce replay error');
    Exit;
  end;
end;

function TdwlOIDC_Client.GetRefreshAndAccessTokenFromReceivedCode(const Code: string; Session: PdwlOIDC_Client_Session; out Refresh_Token, Access_Token: string; out Expires_In: integer): TdwlResult;
begin
  var JSONResponse: TJSONValue;
  Result := ExchangeReceivedCode(Code, Session, JSONResponse);
  if not Result.Success then
    Exit;
  try
    try
      Access_Token := JSONResponse.GetValue<string>('access_token');
      Refresh_Token := JSONResponse.GetValue<string>('refresh_token');
      Expires_In := JSONResponse.GetValue<integer>('expires_in');
    except
      Access_Token := '';
      Refresh_Token := '';
      Result.AddErrorMsg('error getting tokens');
      Exit;
    end;
  finally
    JSONResponse.Free;
  end;
end;

procedure TdwlOIDC_Client.InitFromIssuer;
begin
  if FAuthorization_Endpoint<>'' then // already initialized
    Exit;
  var Request := New_HTTPRequest(FIssuer_Uri+OIDC_Well_Known_Config_Path);
  var Response := Request.Execute;
  if Response.StatusCode<>HTTP_STATUS_OK then
    raise Exception.Create('Error Discovering Issuer '+FIssuer_Uri);

  var JWKs_Uri: string;
  var JSONResult := TJsonObject.ParseJSONValue(Response.AsString);
  try
    if not JSONResult.TryGetValue<string>('authorization_endpoint', FAuthorization_Endpoint) then
      raise Exception.Create('Error Discovering authorization_endpoint in '+FIssuer_Uri);
    if not JSONResult.TryGetValue<string>('token_endpoint', FToken_Endpoint) then
      raise Exception.Create('Error Discovering token_endpoint in '+FIssuer_Uri);
    if not JSONResult.TryGetValue<string>('jwks_uri', JWKs_Uri) then
      raise Exception.Create('Error Discovering jwk_url in '+FIssuer_Uri);
  finally
    JSONResult.Free;
  end;
  Request := New_HTTPRequest(JWKs_Uri);
  Response := Request.Execute;
  if Response.StatusCode<>HTTP_STATUS_OK then
    raise Exception.Create('Error getting JWKs from '+JWKs_Uri);
  FJWKs := New_JWKs_FromJSONString(Response.AsString)
end;

{ TdwlOIDC_Client_Session }

procedure TdwlOIDC_Client_Session.Init(const AState: string);
begin
  State := AState;
  Nonce := TNetEncoding.Base64URL.EncodeBytesToString(TdwlOpenSSL.RandomBytes(32));
  CodeVerifier := TNetEncoding.Base64URL.EncodeBytesToString (TdwlOpenSSL.RandomBytes(32));
end;

end.
