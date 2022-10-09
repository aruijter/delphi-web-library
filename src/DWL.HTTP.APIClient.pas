unit DWL.HTTP.APIClient;

interface

uses
  DWL.HTTP.Consts, DWL.HTTP.Types;

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
  protected
    property ApiBaseUrl: string read FApiBaseUrl;
  public
    property Authorizer: IdwlAPIAuthorizer read FAuthorizer;
    constructor Create(const AApiBaseUrl: string; Authorizer: IdwlAPIAuthorizer);
    function DoApiRequest(out StatusCode: word; const UriPart: string; const Http_Command: string=HTTP_COMMAND_GET; const URLEncodedParamsOrPostBody: string=''; PostBodyIsJSON: boolean=true; OmitAccessToken: boolean=false; IsARetry: boolean=false; AOnProgress: TdwlHTTPProgressEvent=nil): string;
  end;

implementation

uses
  System.StrUtils, Winapi.Windows, Winapi.WinInet, DWL.HTTP.Client;

{ TdwlAPISession }

constructor TdwlAPISession.Create(const AApiBaseUrl: string; Authorizer: IdwlAPIAuthorizer);
begin
  inherited Create;
  FApiBaseUrl := AApiBaseUrl;
  FAuthorizer := Authorizer;
end;

function TdwlAPISession.DoApiRequest(out StatusCode: word; const UriPart: string; const Http_Command: string=HTTP_COMMAND_GET; const URLEncodedParamsOrPostBody: string=''; PostBodyIsJSON: boolean=true; OmitAccessToken: boolean=false; IsARetry: boolean=false; AOnProgress: TdwlHTTPProgressEvent=nil): string;
var
  lRequest: IdwlHTTPRequest;
  lResponse: IdwlHTTPResponse;
begin
  Result := '';
  StatusCode := HTTP_STATUS_BAD_REQUEST;
  try
    lRequest := New_HTTPRequest(FApiBaseUrl + UriPart);
    lRequest.OnProgress := AOnProgress;
    lRequest.Method := Http_Command;
    if not OmitAccessToken then
    begin
      var AccessToken := FAuthorizer.GetAccesstoken;
      if AccessToken='' then
      begin
        StatusCode := HTTP_STATUS_DENIED; // actually this means, you're not authenticated in the right way I cant help you
        Exit;
      end;
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
    StatusCode := lResponse.StatusCode;
    case lResponse.StatusCode of
    200: Result := lResponse.AsString;
    401:
      begin
        if not IsARetry then
        begin
          FAuthorizer.InvalidateAuthorization;
          // try again and we will ask for a username password next time
          Result := DoApiRequest(StatusCode, UriPart, Http_Command, URLEncodedParamsOrPostBody, OmitAccessToken, true);
        end;
      end;
    end;
  except
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
