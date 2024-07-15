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

  IdwlAPIJSONArray = interface
    function JSON: TJSONArray;
  end;

  IdwlAPIResponse = interface
    function Data: TJSONObject;
    function Data_Array(const APath: string): IdwlAPIJSONArray;
    function HTTPResponse: IdwlHTTPResponse;
    function Success: boolean;
    function Errors: TJSONArray;
  end;

  IdwlAPIRequest = interface
    function JSON: TJSONObject;
    function HTTPRequest: IdwlHTTPRequest;
    function Execute: IdwlAPIResponse;
  end;


  TdwlAPISession = class
  strict private
    FApiBaseUrl: string;
    function InternalApiRequest(IsARetry: boolean; const UriPart: string; const Http_Method, URLEncodedParamsOrPostBody: string; PostBodyIsJSON, OmitAccessToken: boolean; AOnProgress: TdwlHTTPProgressEvent): IdwlHTTPResponse;
  private
    FAuthorizer: IdwlAPIAuthorizer;
  public
    property ApiBaseUrl: string read FApiBaseUrl;
    property Authorizer: IdwlAPIAuthorizer read FAuthorizer;
    constructor Create(const AApiBaseUrl: string; Authorizer: IdwlAPIAuthorizer);
    function ExecuteApiRequest(const UriPart: string; const Http_Method: string=HTTP_METHOD_GET; const URLEncodedParamsOrPostBody: string=''; PostBodyIsJSON: boolean=true; OmitAccessToken: boolean=false; AOnProgress: TdwlHTTPProgressEvent=nil): IdwlHTTPResponse;
    function ExecuteJSONRequest(const UriPart: string; const Http_Method: string=HTTP_METHOD_GET; const URLEncodedParamsOrPostBody: string=''; OmitAccessToken: boolean=false): IdwlAPIResponse;
    function New_APIRequest(const UriPart: string; const Http_Method: string=HTTP_METHOD_GET; OmitAccessToken: boolean=false): IdwlAPIRequest;
  end;

implementation

uses
  System.StrUtils, Winapi.Windows, Winapi.WinInet, System.IOUtils;

type
  TdwlAPIRequest = class(TInterfacedObject, IdwlAPIRequest)
  strict private
    FJSON: TJSonObject;
    FSession: TdwlAPISession;
    FHTTPRequest: IdwlHTTPRequest;
  private
    function Execute: IdwlAPIResponse;
    function HTTPRequest: IdwlHTTPRequest;
    function JSON: TJSONObject;
  public
    constructor Create(Session: TdwlAPISession; const Url: string);
    destructor Destroy; override;
  end;

  TdwlAPIResponse = class(TInterfacedObject, IdwlAPIResponse)
  private
    FHTTPResponse: IdwlHTTPResponse;
    FSuccess: boolean;
    FJSON: TJSONObject;
    FData: TJSONObject;
    FErrors: TJSONArray;
    function Data_Array(const APath: string): IdwlAPIJSONArray;
    function Errors: TJSONArray;
    function HTTPResponse: IdwlHTTPResponse;
    function Success: boolean;
  public
    constructor Create(AHTTPResponse: IdwlHTTPResponse);
    destructor Destroy; override;
    function Data: TJSONObject;
  end;

  TdwlAPIJSONArray = class(TInterfacedObject, IdwlAPIJSONArray)
  strict private
    FResponse: IdwlAPIResponse;
    FJSON: TJSONArray;
  private
    function JSON: TJSONArray;
  public
    constructor Create(Response: IdwlAPIResponse; AJSON: TJSONArray);
  end;

{ TdwlAPIResponse }

constructor TdwlAPIResponse.Create(AHTTPResponse: IdwlHTTPResponse);
begin
  inherited Create;
  FHTTPResponse := AHTTPResponse;
  if (AHTTPResponse.StatusCode=HTTP_STATUS_OK) then
  begin
    try
      var JSONResult := TJSONValue.ParseJSONValue(AHTTPResponse.AsString);
      if JSONResult is TJSONObject then
      begin
        FJSON := TJSONObject(JSONResult);
        FSuccess := FJSON.GetValue<boolean>('success', false);
      end
      else
        JSONResult.Free;
    except
      FJSON := nil;
    end;
  end;
end;

function TdwlAPIResponse.Data: TJSONObject;
begin
  if FData=nil then
    FJSON.TryGetValue<TJSONObject>('data', FData);
  Result := FData;
end;

function TdwlAPIResponse.Data_Array(const APath: string): IdwlAPIJSONArray;
begin
  var JSON: TJSONArray;
  if (Data<>nil) and Data.TryGetValue<TJSONArray>(APath, JSON) then
    Result := TdwlAPIJSONArray.Create(Self, JSON)
  else
    Result := nil;
end;

destructor TdwlAPIResponse.Destroy;
begin
  FJSON.Free;
  inherited Destroy;
end;

function TdwlAPIResponse.Errors: TJSONArray;
begin
  if FErrors=nil then
    FJSON.TryGetValue<TJSONArray>('errors', FErrors);
  Result := FErrors;
end;

function TdwlAPIResponse.HTTPResponse: IdwlHTTPResponse;
begin
  Result := FHTTPResponse;
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

function TdwlAPISession.New_APIRequest(const UriPart: string; const Http_Method: string=HTTP_METHOD_GET; OmitAccessToken: boolean=false): IdwlAPIRequest;
begin
  try
    Result := TdwlAPIRequest.Create(Self, FApiBaseUrl+UriPart);
    Result.HTTPRequest.Method := Http_Method;
    if not OmitAccessToken then
    begin
      var AccessToken := FAuthorizer.GetAccesstoken;
      if AccessToken='' then
        Exit;
      Result.HTTPRequest.Header['Authorization'] := 'Bearer '+AccessToken;
    end;
  except
    // continue un-prepared, this will come out in the execution phase
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

{ TdwlAPIRequest }

constructor TdwlAPIRequest.Create(Session: TdwlAPISession; const Url: string);
begin
  inherited Create;
  FSession := Session;
  FHTTPRequest := New_HTTPRequest(Url)
end;

destructor TdwlAPIRequest.Destroy;
begin
  FJSON.Free;
  inherited Destroy;
end;

function TdwlAPIRequest.Execute: IdwlAPIResponse;
  function InternalExecute(IsARetry: boolean): IdwlHTTPResponse;
  begin
    try
      Result := HTTPRequest.Execute;
      case Result.StatusCode of
      401:
        begin
          if not IsARetry then
          begin
            if Result.Header['Authorization']<>'' then
              FSession.FAuthorizer.InvalidateAuthorization;
            // try again and we will ask for a username password next time
            Result := InternalExecute(true);
          end;
        end;
      end;
    except
      Result := Get_EmptyHTTPResponse(HTTP_STATUS_SERVER_ERROR);
    end;
  end;
begin
  if FJSON<>nil then
  begin
    HTTPRequest.Header[HTTP_FIELD_CONTENT_TYPE] := CONTENT_TYPE_JSON;
    HTTPRequest.WritePostData(FJSON.ToJSON);
  end;
  Result := TdwlAPIResponse.Create(InternalExecute(false));
end;

function TdwlAPIRequest.HTTPRequest: IdwlHTTPRequest;
begin
  Result := FHTTPRequest;
end;

function TdwlAPIRequest.JSON: TJSONObject;
begin
  if FJSON=nil then
    FJSON := TJSONObject.Create;
  Result := FJSON;
end;

{ TdwlAPIJSONArray }

constructor TdwlAPIJSONArray.Create(Response: IdwlAPIResponse; AJSON: TJSONArray);
begin
  FResponse := Response; // just a interface reference to keep reponse alive and be able to keep accessing JSON
  FJSON := AJSON;
end;

function TdwlAPIJSONArray.JSON: TJSONArray;
begin
  Result := FJSON;
end;

end.
