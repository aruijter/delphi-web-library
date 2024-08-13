unit DWL.HTTP.APIClient.OAuth2;

interface

uses
  DWL.HTTP.APIClient, DWL.JOSE, DWL.Classes, System.SysUtils, DWL.Params;

const
  param_ForceLoginPrompt='forceloginprompt';

type
  IdwlAPIOAuth2Authorizer = interface(IdwlAPIAuthorizer)
    ['{A1BB96A5-7391-4633-B9DC-7BD681FF7731}']
    function CheckJWT(JWT: IdwlJWT): TdwlResult;
    function GetRefreshToken: string;
  end;

/// <summary>
///   Function to create an OAuth2 authorizer.<br/>
///   Client secret can be omitted in most case, because the authorizer uses a CodeVerifier.<br/>
///   DialogTitle is required, if left empty, the authorizer will not try to get a refreshtoken interactivly, but only through the callback procedure. This is meant for server applications.<br/><br/>
///   Params: <br/>param_ForceLoginPrompt; bool; to force a login-prompt and skip SingleSignOn in the browser.
/// </summary>
function New_OAuth2Authorizer(CallbackProc: TdwlAPIAuthorizerCallBackProc; const Issuer_Uri, Client_Id, Redirect_Uri: string; Scopes: array of string; const DialogTitle: string; Params: IdwlParams=nil): IdwlAPIOAuth2Authorizer; overload;
function New_OAuth2Authorizer(const Issuer_Uri, Client_Id, Redirect_Uri, SecretsFileName: string; AESKey: TBytes): IdwlAPIOAuth2Authorizer; overload;


implementation

uses
  DWL.OpenID, Winapi.Windows, DWL.HTTP.APIClient.OAuth2.LoginForm,
  System.NetEncoding, DWL.OpenSSL, System.UITypes, DWL.HTTP.Client,
  System.IOUtils, DWL.Crypt;

type
  TdwlAPIOAuth2Authorizer = class(TdwlAPIAuthorizerWithRefreshToken, IdwlAPIOAuth2Authorizer)
  strict private
    FOIDC_Client: TdwlOIDC_Client;
    FDialogTitle: string;
    FForceLoginPrompt: boolean;
  private
    function CheckJWT(JWT: IdwlJWT): TdwlResult;
  protected
    procedure AcquireAccessToken; override;
    procedure AcquireRefreshtoken; override;
  public
    constructor Create(CallbackProc: TdwlAPIAuthorizerCallBackProc; const Issuer_Uri, Client_Id, Redirect_Uri: string; Scopes: array of string; const DialogTitle: string; Params: IdwlParams=nil);
    destructor Destroy; override;
  end;

  TdwlAPIOAuth2SecretsFileAuthorizer = class(TdwlAPIOAuth2Authorizer, IdwlAPIOAuth2Authorizer)
  strict private
    FSecretsFileName: string;
    FAESKey: TBytes;
    procedure CallBack(var Token: string; Action: TdwlAPIAuthorizerCallBackAction);
  public
    constructor Create(const Issuer_Uri, Client_Id, Redirect_Uri, SecretsFileName: string; AESKey: TBytes);
  end;

function New_OAuth2Authorizer(CallbackProc: TdwlAPIAuthorizerCallBackProc; const Issuer_Uri, Client_Id, Redirect_Uri: string; Scopes: array of string; const DialogTitle: string; Params: IdwlParams=nil): IdwlAPIOAuth2Authorizer;
begin
  Result := TdwlAPIOAuth2Authorizer.Create(CallbackProc, Issuer_Uri, Client_Id, Redirect_Uri, Scopes, DialogTitle, Params);
end;

function New_OAuth2Authorizer(const Issuer_Uri, Client_Id, Redirect_Uri, SecretsFileName: string; AESKey: TBytes): IdwlAPIOAuth2Authorizer; overload;
begin
  Result := TdwlAPIOAuth2SecretsFileAuthorizer.Create(Issuer_Uri, Client_Id, Redirect_Uri, SecretsFileName, AESKey);
end;

{ TdwlAPIOAuth2Authorizer }

procedure TdwlAPIOAuth2Authorizer.AcquireAccessToken;
begin
  var Access_Token: string;
  var Expires_In: integer;
  var Refresh_Token := GetRefreshToken;
  if Refresh_Token='' then
    Exit;
  // maybe an access was already provided when getting a refreshtoken
  if AccessTokenPresent then
    Exit;
  var Res := FOIDC_Client.GetAccessTokenFromRefreshToken(Refresh_Token, Access_Token, Expires_In);
  NewRefreshToken(Refresh_Token); // save new (or invalidated) refreshtoken
  if Res.Success then
    NewAccessToken(Access_Token, GetTickCount64+UInt64(Expires_In-2)*1000)
  else
  begin
    if Refresh_Token='' then  // the before non empty refreshtoken was assigned invalid while retrieving accesstoken, do a retry
      AcquireAccessToken;
  end;
end;

procedure TdwlAPIOAuth2Authorizer.AcquireRefreshtoken;
var
  Session: TdwlOIDC_Client_Session;
begin
  // if there is no title, do not start interactive authorization, just quit
  // this is a signal it's running serverside and something else went wrong
  if FDialogTitle='' then
    Exit;
  // no scopes? No action
  if FOIDC_Client.Scopes.Count=0 then
    Exit;
  var Frm := TdwlOAuth2LoginForm.Create(nil);
  try
    Frm.cxLabel1.Caption := FDialogTitle;
    Frm.cxButton1.Caption := 'Cancel';
    Session.Init(TNetEnCoding.Base64URL.EncodeBytesToString(TdwlOpenSSL.RandomBytes(32)));
    Frm.Redirect_Uri := FOIDC_Client.Redirect_Uri;
    Frm.Browser.Navigate2(FOIDC_Client.GetAuthorizeUri(@Session, FForceLoginPrompt));
    if Frm.ShowModal=mrOk then
    begin
      var Access_Token: string;
      var Refresh_Token: string;
      var Expires_In: integer;
      var Res := FOIDC_Client.GetRefreshAndAccessTokenFromReceivedCode(Frm.Code, @Session, Refresh_Token, Access_Token, Expires_In);
      if Res.Success then
      begin
        NewRefreshToken(Refresh_Token);
        NewAccessToken(Access_Token, GetTickCount64+UInt64(Expires_In-2)*1000);
      end;
    end;
  finally
    Frm.Free;
  end;
end;

function TdwlAPIOAuth2Authorizer.CheckJWT(JWT: IdwlJWT): TdwlResult;
begin
   Result := FOIDC_Client.CheckJWT(JWT);
end;

constructor TdwlAPIOAuth2Authorizer.Create(CallbackProc: TdwlAPIAuthorizerCallBackProc; const Issuer_Uri, Client_Id, Redirect_Uri: string; Scopes: array of string; const DialogTitle: string; Params: IdwlParams=nil);
begin
  inherited Create(CallbackProc);
  PutInternetExplorerBrowserEmulationInRegistry;
  FOIDC_Client := TdwlOIDC_Client.Create(Issuer_Uri, Client_Id, Redirect_Uri);
  for var Scope in Scopes do
    FOIDC_Client.Scopes.Add(Scope);
  FDialogTitle := DialogTitle;
  if Params<>nil then
  begin
    FForceLoginPrompt := Params.BoolValue(param_ForceLoginPrompt);
  end;
end;

destructor TdwlAPIOAuth2Authorizer.Destroy;
begin
  FOIDC_Client.Free;
  inherited Destroy;
end;

{ TdwlAPIOAuth2SecretsFileAuthorizer }

procedure TdwlAPIOAuth2SecretsFileAuthorizer.CallBack(var Token: string; Action: TdwlAPIAuthorizerCallBackAction);
begin
  case Action of
    acaGetRefreshtoken:
      begin
        Token := TFile.ReadAllText(FSecretsFileName, TEncoding.UTF8);
        TdwlCrypt.AES_Decrypt(Token, FAESKey);
      end;
    acaNewRefreshtoken:
      begin
        var SaveToken := Token;
        TdwlCrypt.AES_Encrypt(SaveToken, FAESKey);
        TFile.WriteAllText(FSecretsFileName, SaveToken, TEncoding.UTF8);
      end;
    acaInvalidateAuthorization: TFile.Delete(FSecretsFileName);
  end;
end;

constructor TdwlAPIOAuth2SecretsFileAuthorizer.Create(const Issuer_Uri, Client_Id, Redirect_Uri, SecretsFileName: string; AESKey: TBytes);
begin
  FSecretsFileName := SecretsFileName;
  FAESKey := AESKey;
  inherited Create(CallBack, Issuer_Uri, Client_Id, Redirect_Uri, [], '');
end;

end.
