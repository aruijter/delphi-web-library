unit DWL.HTTP.APIClient;

interface

uses
  DWL.HTTP.Consts, DWL.Server.Types, System.SysUtils, DWL.HTTP.Client,
  System.JSON, DWL.HTTP.Types;

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

  IdwlAPIResponse = interface
    function Data: TJSONObject;
    function Success: boolean;
  end;

  TdwlAPISession = class
  strict private
    FAuthorizer: IdwlAPIAuthorizer;
    FApiBaseUrl: string;
    function InternalApiRequest(IsARetry: boolean; const UriPart: string; const Http_Method, URLEncodedParamsOrPostBody: string; PostBodyIsJSON, OmitAccessToken: boolean; AOnProgress: TdwlHTTPProgressEvent): IdwlHTTPResponse;
  protected
  public
    property ApiBaseUrl: string read FApiBaseUrl;
    property Authorizer: IdwlAPIAuthorizer read FAuthorizer;
    constructor Create(const AApiBaseUrl: string; Authorizer: IdwlAPIAuthorizer);
    function ExecuteApiRequest(const UriPart: string; const Http_Method: string=HTTP_METHOD_GET; const URLEncodedParamsOrPostBody: string=''; PostBodyIsJSON: boolean=true; OmitAccessToken: boolean=false; AOnProgress: TdwlHTTPProgressEvent=nil): IdwlHTTPResponse;
    function ExecuteJSONRequest(const UriPart: string; const Http_Method: string=HTTP_METHOD_GET; const URLEncodedParamsOrPostBody: string=''; OmitAccessToken: boolean=false): IdwlAPIResponse;
    function PrepareAPIRequest(const UriPart: string; const Http_Method: string=HTTP_METHOD_GET; OmitAccessToken: boolean=false): IdwlHTTPRequest;
  end;

implementation

uses
  System.StrUtils, Winapi.Windows, Winapi.WinInet, System.IOUtils;

type
  TdwlAPIResponse = class(TInterfacedObject, IdwlAPIResponse)
  private
    FJSON: TJSONObject;
    FData: TJSONObject;
    function Success: boolean;
  public
    constructor Create(Response: IdwlHTTPResponse);
    destructor Destroy; override;
    function Data: TJSONObject;
  end;

{ TdwlAPIResponse }

constructor TdwlAPIResponse.Create(Response: IdwlHTTPResponse);
begin
  inherited Create;
  if Response.StatusCode=HTTP_STATUS_OK then
  begin
    var JSONResult := TJSONValue.ParseJSONValue(Response.AsString);
    if JSONResult is TJSONObject then
    begin
      FJSON := TJSONObject(JSONResult);
      var IsOk := FJSON.GetValue<boolean>('success', false);
      if IsOk then
      begin
        FData := FJSON.GetValue<TJSONObject>('data');
        IsOk := FData<>nil;
      end;
      if not IsOk then
        FreeAndNil(FJSON);
    end
    else
      JSONResult.Free;
  end;
end;

function TdwlAPIResponse.Data: TJSONObject;
begin
  Result := FData;
end;

destructor TdwlAPIResponse.Destroy;
begin
  FJSON.Free;
  inherited Destroy;
end;

function TdwlAPIResponse.Success: boolean;
begin
  Result := (FJSON<>nil);
end;

{ TdwlAPISession }

constructor TdwlAPISession.Create(const AApiBaseUrl: string; Authorizer: IdwlAPIAuthorizer);
begin
  inherited Create;
  FApiBaseUrl := AApiBaseUrl;
  if not FApiBaseUrl.EndsWith('/') then
    FApiBaseUrl := FApiBaseUrl+'/';
  FAuthorizer := Authorizer;
end;

function TdwlAPISession.ExecuteApiRequest(const UriPart: string; const Http_Method: string=HTTP_METHOD_GET; const URLEncodedParamsOrPostBody: string=''; PostBodyIsJSON: boolean=true; OmitAccessToken: boolean=false; AOnProgress: TdwlHTTPProgressEvent=nil): IdwlHTTPResponse;
begin
  Result := InternalApiRequest(false, UriPart, Http_Method, URLEncodedParamsOrPostBody, PostBodyIsJSON, OmitAccessToken, AOnProgress);
end;

function TdwlAPISession.ExecuteJSONRequest(const UriPart, Http_Method, URLEncodedParamsOrPostBody: string;OmitAccessToken: boolean): IdwlAPIResponse;
begin
  Result := TdwlAPIResponse.Create(InternalApiRequest(false, UriPart, Http_Method, URLEncodedParamsOrPostBody, true, OmitAccessToken, nil));
end;

function TdwlAPISession.InternalApiRequest(IsARetry: boolean; const UriPart, Http_Method, URLEncodedParamsOrPostBody: string; PostBodyIsJSON, OmitAccessToken: boolean; AOnProgress: TdwlHTTPProgressEvent): IdwlHTTPResponse;
var
  lRequest: IdwlHTTPRequest;
begin
  try
    lRequest := New_HTTPRequest(FApiBaseUrl + UriPart);
    lRequest.OnProgress := AOnProgress;
    lRequest.Method := Http_Method;
    if not OmitAccessToken then
    begin
      var AccessToken := FAuthorizer.GetAccesstoken;
      if AccessToken='' then
        Exit(Get_EmptyHTTPResponse(HTTP_STATUS_DENIED));
       lRequest.Header['Authorization'] := 'Bearer '+AccessToken;
    end;
    if URLEncodedParamsOrPostBody<>'' then
    begin
      if (Http_Method=HTTP_METHOD_POST) or (Http_Method=HTTP_METHOD_PUT) then
      begin
        lRequest.WritePostData(URLEncodedParamsOrPostBody);
        if PostBodyIsJSON then
          lRequest.Header[HTTP_FIELD_CONTENT_TYPE] := CONTENT_TYPE_JSON
        else
          lRequest.Header[HTTP_FIELD_CONTENT_TYPE] := CONTENT_TYPE_X_WWW_FORM_URLENCODED;
      end
      else
        lRequest.URL := lRequest.URL+'?'+URLEncodedParamsOrPostBody;
    end;
    Result := lRequest.Execute;
    case Result.StatusCode of
    401:
      begin
        if not IsARetry then
        begin
          if not OmitAccessToken then
            FAuthorizer.InvalidateAuthorization;
          // try again and we will ask for a username password next time
          Result := InternalApiRequest(true, UriPart, Http_Method, URLEncodedParamsOrPostBody, PostBodyIsJSON, OmitAccessToken, AOnProgress);
        end;
      end;
    end;
  except
  end;
end;

function TdwlAPISession.PrepareAPIRequest(const UriPart, Http_Method: string; OmitAccessToken: boolean): IdwlHTTPRequest;
begin
  try
    Result := New_HTTPRequest(FApiBaseUrl + UriPart);
    Result.Method := Http_Method;
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
