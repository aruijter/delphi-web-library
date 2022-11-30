unit DWL.HTTP.APIClient;

interface

uses
  DWL.HTTP.Consts, DWL.HTTP.Types, System.SysUtils, DWL.HTTP.Client;

type
  TdwlAPIAuthorizerCallBackAction = (acaGetRefreshtoken, acaNewRefreshtoken, acaNewAccessToken, acaInvalidateAuthorization);
  TdwlAPIAuthorizerCallBackProc = procedure(var Token: string; Action: TdwlAPIAuthorizerCallBackAction) of object;

  IdwlAPIAuthorizer = interface
    ['{B65833A7-8BC6-4237-A99F-A269D2DDA608}']
    function GetAccesstoken(EnforceNewOne: boolean=false): string;
    function GetRefreshtoken: string;
    procedure InvalidateAuthorization;
  end;

  TdwlAPIAuthorizer = class(TInterfacedObject, IdwlAPIAuthorizer)
  strict private
    FAccessToken: string;
    FAccessTokenExpireTick: UInt64;
    FCallBackProc: TdwlAPIAuthorizerCallBackProc;
    FRefreshtoken: string;
  private
  protected
    procedure NewAccessToken(const NewAccessToken: string; NewAccessTokenExpireTick: UInt64);
    procedure NewRefreshToken(const NewRefreshToken: string);
    function GetAccessToken(EnforceNewOne: boolean=false): string;
    function GetRefreshToken: string;
    procedure InvalidateAuthorization;
    procedure AcquireAccessToken; virtual; abstract;
    procedure AcquireRefreshtoken; virtual; abstract;
  public
    constructor Create(CallbackProc: TdwlAPIAuthorizerCallBackProc);
  end;

  TdwlAPISession = class
  strict private
    FAuthorizer: IdwlAPIAuthorizer;
    FApiBaseUrl: string;
    function InternalApiRequest(IsARetry: boolean; const UriPart: string; const Http_Command, URLEncodedParamsOrPostBody: string; PostBodyIsJSON, OmitAccessToken: boolean; AOnProgress: TdwlHTTPProgressEvent): IdwlHTTPResponse;
  protected
    property ApiBaseUrl: string read FApiBaseUrl;
  public
    property Authorizer: IdwlAPIAuthorizer read FAuthorizer;
    constructor Create(const AApiBaseUrl: string; Authorizer: IdwlAPIAuthorizer);
    function DoApiRequest(const UriPart: string; const Http_Command: string=HTTP_COMMAND_GET; const URLEncodedParamsOrPostBody: string=''; PostBodyIsJSON: boolean=true; OmitAccessToken: boolean=false; AOnProgress: TdwlHTTPProgressEvent=nil): IdwlHTTPResponse;
    function PrepareAPIRequest(const UriPart: string; const Http_Command: string=HTTP_COMMAND_GET; OmitAccessToken: boolean=false): IdwlHTTPRequest;
  end;

implementation

uses
  System.StrUtils, Winapi.Windows, Winapi.WinInet;

{ TdwlAPISession }

constructor TdwlAPISession.Create(const AApiBaseUrl: string; Authorizer: IdwlAPIAuthorizer);
begin
  inherited Create;
  FApiBaseUrl := AApiBaseUrl;
  if not FApiBaseUrl.EndsWith('/') then
    FApiBaseUrl := FApiBaseUrl+'/';
  FAuthorizer := Authorizer;
end;

function TdwlAPISession.DoApiRequest(const UriPart: string; const Http_Command: string=HTTP_COMMAND_GET; const URLEncodedParamsOrPostBody: string=''; PostBodyIsJSON: boolean=true; OmitAccessToken: boolean=false; AOnProgress: TdwlHTTPProgressEvent=nil): IdwlHTTPResponse;
begin
  Result := InternalApiRequest(false, UriPart, Http_Command, URLEncodedParamsOrPostBody, PostBodyIsJSON, OmitAccessToken, AOnProgress);
end;

function TdwlAPISession.InternalApiRequest(IsARetry: boolean; const UriPart, Http_Command, URLEncodedParamsOrPostBody: string; PostBodyIsJSON, OmitAccessToken: boolean; AOnProgress: TdwlHTTPProgressEvent): IdwlHTTPResponse;
var
  lRequest: IdwlHTTPRequest;
  lResponse: IdwlHTTPResponse;
begin
  try
    lRequest := New_HTTPRequest(FApiBaseUrl + UriPart);
    lRequest.OnProgress := AOnProgress;
    lRequest.Method := Http_Command;
    if not OmitAccessToken then
    begin
      var AccessToken := FAuthorizer.GetAccesstoken;
      if AccessToken='' then
        Exit;
      lRequest.Header['Authorization'] := 'Bearer '+AccessToken;
    end;
    if URLEncodedParamsOrPostBody<>'' then
    begin
      if (Http_Command=HTTP_COMMAND_POST) or (Http_Command=HTTP_COMMAND_PUT) then
      begin
        lRequest.WritePostData(URLEncodedParamsOrPostBody);
        if PostBodyIsJSON then
          lRequest.Header[HTTP_HEADER_CONTENT_TYPE] := CONTENT_TYPE_JSON
        else
          lRequest.Header[HTTP_HEADER_CONTENT_TYPE] := CONTENT_TYPE_X_WWW_FORM_URLENCODED;
      end
      else
        lRequest.URL := lRequest.URL+'?'+URLEncodedParamsOrPostBody;
    end;
    lResponse := lRequest.Execute;
    case lResponse.StatusCode of
    200: Result := lResponse;
    401:
      begin
        if not IsARetry then
        begin
          if not OmitAccessToken then
            FAuthorizer.InvalidateAuthorization;
          // try again and we will ask for a username password next time
          Result := InternalApiRequest(true, UriPart, Http_Command, URLEncodedParamsOrPostBody, PostBodyIsJSON, OmitAccessToken, AOnProgress);
        end;
      end;
    end;
  except
  end;
end;

function TdwlAPISession.PrepareAPIRequest(const UriPart, Http_Command: string; OmitAccessToken: boolean): IdwlHTTPRequest;
begin
  try
    Result := New_HTTPRequest(FApiBaseUrl + UriPart);
    Result.Method := Http_Command;
    if not OmitAccessToken then
    begin
      var AccessToken := FAuthorizer.GetAccesstoken;
      if AccessToken='' then
        Exit;
      Result.Header['Authorization'] := 'Bearer '+AccessToken;
    end;
  except
    Result := nil;
  end;
end;

{ TdwlAPIAuthorizer }

constructor TdwlAPIAuthorizer.Create(CallbackProc: TdwlAPIAuthorizerCallBackProc);
begin
  inherited Create;
  FCallBackProc := CallbackProc;
end;

function TdwlAPIAuthorizer.GetAccessToken(EnforceNewOne: boolean): string;
begin
  if EnforceNewOne or (FAccessTokenExpireTick<GetTickCount64) then
    FAccessToken := '';
  if FAccessToken='' then
  begin
    // we always need a valid refreshtoken to continue;
    if GetRefreshtoken='' then
      Exit;
    // maybe we also got a new access-token, so check again
    if FAccessToken='' then
      AcquireAccessToken;
  end;
  Result := FAccessToken;
end;

function TdwlAPIAuthorizer.GetRefreshToken: string;
begin
  if (FRefreshtoken='') and Assigned(FCallBackProc) then
    FCallBackProc(FRefreshtoken, acaGetRefreshtoken);
  if FRefreshtoken='' then
    AcquireRefreshToken;
  Result := FRefreshtoken;
end;

procedure TdwlAPIAuthorizer.InvalidateAuthorization;
begin
  FAccessToken := '';
  FRefreshtoken := '';
  if Assigned(FCallBackProc) then
  begin
    var Dummy: string;
    FCallBackProc(Dummy, acaInvalidateAuthorization);
  end;
end;

procedure TdwlAPIAuthorizer.NewAccessToken(const NewAccessToken: string; NewAccessTokenExpireTick: UInt64);
begin
  FAccessToken := NewAccessToken;
  FAccessTokenExpireTick := NewAccessTokenExpireTick;
  if Assigned(FCallBackProc) then
    FCallBackProc(FAccessToken, acaNewAccessToken);
end;

procedure TdwlAPIAuthorizer.NewRefreshToken(const NewRefreshToken: string);
begin
  if NewRefreshToken=FRefreshtoken then
    Exit;
  FRefreshtoken := NewRefreshToken;
  if Assigned(FCallBackProc) then
    FCallBackProc(FRefreshtoken, acaNewRefreshtoken);
end;

end.
