unit DWL.HTTP.APIClient.Authorizer.OAuth2;

interface

uses
  Delinea.Rest.Session, Delinea.JOSE, dsBase, Delinea.Params, System.SysUtils;

const
  param_ForceLoginPrompt='forceloginprompt';

type
  IfdlRestOAuth2Authorizer = interface(IfdlRestAuthorizer)
    ['{A1BB96A5-7391-4633-B9DC-7BD681FF7731}']
    function CheckJWS(JWS: IdsJWS): TdlResult;
  end;

/// <summary>
///   Function to create an OAuth2 authorizer.<br/>
///   Client secret can be omitted in most case, because the authorizer uses a CodeVerifier.<br/>
///   DialogTitle is required, if left empty, the authorizer will not try to get a refreshtoken interactivly, but only through the callback procedure. This is meant for server applications.<br/><br/>
///   Params: <br/>param_ForceLoginPrompt; bool; to force a login-prompt and skip SingleSignOn in the browser.
/// </summary>
function GetOAuth2Authorizer(CallbackProc: TfdlRestAuthorizerCallBackProc; const Issuer_Uri, Client_Id, Redirect_Uri: string; Scopes: array of string; const DialogTitle: string; Params: IdsParams=nil): IfdlRestOAuth2Authorizer; overload;
function GetOAuth2Authorizer(const Issuer_Uri, Client_Id, Redirect_Uri, SecretsFileName: string; AESKey: TBytes): IfdlRestOAuth2Authorizer; overload;


implementation

uses
  Delinea.Internet.OpenID, Delinea.Rest.Session.Authorizer.OAuth2.LoginForm,
  System.UITypes, dsDialogs, Delinea.Encoding, Delinea.OpenSSL,
  System.Win.Registry, Winapi.Windows, Delinea.Internet,
  dsSysUtils, System.IOUtils, Delinea.Crypt;

type
  TfdlRestOAuth2Authorizer = class(TfdlRestAuthorizer, IfdlRestOAuth2Authorizer)
  strict private
    FOIDC_Client: TOIDC_Client;
    FDialogTitle: string;
    FForceLoginPrompt: boolean;
  private
    function CheckJWS(JWS: IdsJWS): TdlResult;
  protected
    procedure AcquireAccessToken; override;
    procedure AcquireRefreshtoken; override;
  public
    constructor Create(CallbackProc: TfdlRestAuthorizerCallBackProc; const Issuer_Uri, Client_Id, Redirect_Uri: string; Scopes: array of string; const DialogTitle: string; Params: IdsParams=nil);
    destructor Destroy; override;
  end;

  TfdlRestOAuth2SecretsFileAuthorizer = class(TfdlRestOAuth2Authorizer, IfdlRestOAuth2Authorizer)
  strict private
    FSecretsFileName: string;
    FAESKey: TBytes;
    procedure CallBack(var Token: string; Action: TfdlRestAuthorizerCallBackAction);
  public
    constructor Create(const Issuer_Uri, Client_Id, Redirect_Uri, SecretsFileName: string; AESKey: TBytes);
  end;

function GetOAuth2Authorizer(CallbackProc: TfdlRestAuthorizerCallBackProc; const Issuer_Uri, Client_Id, Redirect_Uri: string; Scopes: array of string; const DialogTitle: string; Params: IdsParams=nil): IfdlRestOAuth2Authorizer;
begin
  Result := TfdlRestOAuth2Authorizer.Create(CallbackProc, Issuer_Uri, Client_Id, Redirect_Uri, Scopes, DialogTitle, Params);
end;

function GetOAuth2Authorizer(const Issuer_Uri, Client_Id, Redirect_Uri, SecretsFileName: string; AESKey: TBytes): IfdlRestOAuth2Authorizer; overload;
begin
  Result := TfdlRestOAuth2SecretsFileAuthorizer.Create(Issuer_Uri, Client_Id, Redirect_Uri, SecretsFileName, AESKey);
end;

{ TfdlRestOAuth2Authorizer }

procedure TfdlRestOAuth2Authorizer.AcquireAccessToken;
begin
  var Access_Token: string;
  var Expires_In: integer;
  var Refresh_Token := GetRefreshToken;
  if Refresh_Token='' then
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

procedure TfdlRestOAuth2Authorizer.AcquireRefreshtoken;
var
  Session: TOIDC_Client_Session;
begin
  // if there is no title, do not start interactive authorization, just quit
  // this is a signal it's running serverside and something else went wrong
  if FDialogTitle='' then
    Exit;
  // no scopes? No action
  if FOIDC_Client.Scopes.Count=0 then
    Exit;
  var Frm := TOAuth2LoginForm.Create(nil);
  try
    Frm.cxLabel1.Caption := _(FDialogTitle);
    Frm.cxButton1.Caption := _('Cancel');
    Session.Init(TBase64UrlEncoding.EncodeFromBytes(TopensslHelper.RandomBytes(32)));
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

function TfdlRestOAuth2Authorizer.CheckJWS(JWS: IdsJWS): TdlResult;
begin
   Result := FOIDC_Client.CheckJWS(JWS);
end;

constructor TfdlRestOAuth2Authorizer.Create(CallbackProc: TfdlRestAuthorizerCallBackProc; const Issuer_Uri, Client_Id, Redirect_Uri: string; Scopes: array of string; const DialogTitle: string; Params: IdsParams=nil);
begin
  inherited Create(CallbackProc);
  PutInternetExplorerBrowserEmulationInRegistry;
  FOIDC_Client := TOIDC_Client.Create(Issuer_Uri, Client_Id, Redirect_Uri);
  for var Scope in Scopes do
    FOIDC_Client.Scopes.Add(Scope);
  FDialogTitle := DialogTitle;
  if Params<>nil then
  begin
    FForceLoginPrompt := Params.BoolValue(param_ForceLoginPrompt);
  end;
end;

destructor TfdlRestOAuth2Authorizer.Destroy;
begin
  FOIDC_Client.Free;
  inherited Destroy;
end;

{ TfdlRestOAuth2SecretsFileAuthorizer }

procedure TfdlRestOAuth2SecretsFileAuthorizer.CallBack(var Token: string; Action: TfdlRestAuthorizerCallBackAction);
begin
  case Action of
    racaGetRefreshtoken:
      begin
        Token := TFile.ReadAllText(FSecretsFileName, TEncoding.UTF8);
        AES_Decrypt(Token, FAESKey);
      end;
    racaNewRefreshtoken:
      begin
        var SaveToken := Token;
        AES_Encrypt(SaveToken, FAESKey);
        TFile.WriteAllText(FSecretsFileName, SaveToken, TEncoding.UTF8);
      end;
    racaInvalidateAuthorization: TFile.Delete(FSecretsFileName);
  end;
end;

constructor TfdlRestOAuth2SecretsFileAuthorizer.Create(const Issuer_Uri, Client_Id, Redirect_Uri, SecretsFileName: string; AESKey: TBytes);
begin
  FSecretsFileName := SecretsFileName;
  FAESKey := AESKey;
  inherited Create(CallBack, Issuer_Uri, Client_Id, Redirect_Uri, [], '');
end;

end.
