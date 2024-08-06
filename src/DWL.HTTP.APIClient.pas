unit DWL.HTTP.APIClient;

interface

uses
  DWL.HTTP.Consts, DWL.Server.Types, System.SysUtils, DWL.HTTP.Client,
  System.JSON, DWL.HTTP.Types;

type
  TdwlAPIAuthorizerCallBackAction = (acaGetRefreshtoken, acaNewRefreshtoken, acaInvalidateAuthorization);
  TdwlAPIAuthorizerCallBackProc = procedure(var Token: string; Action: TdwlAPIAuthorizerCallBackAction) of object;

  IdwlAPIAuthorizer = interface
    ['{B65833A7-8BC6-4237-A99F-A269D2DDA608}']
    function AuthHeaderLeadText: string;
    function GetAccesstoken(EnforceNewOne: boolean=false): string;
    procedure InvalidateAuthorization;
  end;

  TdwlAPIAuthorizer = class(TInterfacedObject, IdwlAPIAuthorizer)
  strict private
    FAccessToken: string;
    FAccessTokenExpireTick: UInt64;
  private
    function GetAccesstoken(EnforceNewOne: boolean=false): string;
    procedure InvalidateAuthorization; virtual;
  protected
    function AccessTokenPresent: boolean;
    procedure AcquireAccessToken; virtual; abstract;
    function AuthHeaderLeadText: string; virtual;
    procedure NewAccessToken(const NewAccessToken: string; NewAccessTokenExpireTick: UInt64);
  end;

  TdwlAPIAuthorizerWithRefreshToken = class(TdwlAPIAuthorizer)
  strict private
    FCallBackProc: TdwlAPIAuthorizerCallBackProc;
    FRefreshtoken: string;
  private
  protected
    procedure NewRefreshToken(const NewRefreshToken: string);
    function GetRefreshToken: string;
    procedure InvalidateAuthorization; override;
    procedure AcquireRefreshtoken; virtual; abstract;
  public
    constructor Create(CallbackProc: TdwlAPIAuthorizerCallBackProc);
  end;

  IdwlAPIJSONArray = interface
    function JSON: TJSONArray;
  end;

  IdwlAPIResponse = interface
    function JSON: TJSONObject;
    function JSON_Data: TJSONObject;
    function JSON_Data_Array(const APath: string): IdwlAPIJSONArray;
    function HTTPResponse: IdwlHTTPResponse;
    function StatusCode: cardinal;
    function Success: boolean;
    function Errors: TJSONArray;
  end;

  IdwlAPIRequest = interface
    function GetMethod: string;
    procedure SetMethod(const Value: string);
    /// <summary>
    ///   Change of onsult the calling method of the request, default it is
    ///   HTTP_METHOD_GET
    /// </summary>
    property Method: string read GetMethod write SetMethod;
    /// <summary>
    ///   if the postdata is simple text, you can use WritePostData instead of
    ///   the more complicated PostStreadm approach
    /// </summary>
    /// <param name="PostData">
    ///   the text to add as postdata in the body
    /// </param>
    procedure WritePostData(const PostData: string);
    function JSON: TJSONObject;
    function HTTPRequest: IdwlHTTPRequest;
    function Execute: IdwlAPIResponse;
  end;

  IdwlAPISession = interface
    function New_APIRequest(const UriPart: string; const Http_Method: string=HTTP_METHOD_GET; OmitAccessToken: boolean=false): IdwlAPIRequest;
  end;

  TdwlAPISession = class(TInterfacedObject, IdwlAPISession)
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

function New_APISession(const AApiBaseUrl: string; Authorizer: IdwlAPIAuthorizer): IdwlAPISession;

implementation

uses
  System.StrUtils, Winapi.Windows, Winapi.WinInet, System.IOUtils,
  DWL.MediaTypes;

function New_APISession(const AApiBaseUrl: string; Authorizer: IdwlAPIAuthorizer): IdwlAPISession;
begin
  Result := TdwlAPISession.Create(AApiBaseUrl, Authorizer);
end;

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
    function GetMethod: string;
    procedure SetMethod(const Value: string);
    procedure WritePostData(const PostData: string);
  public
    constructor Create(Session: TdwlAPISession; const Url: string);
    destructor Destroy; override;
  end;

  TdwlAPIResponse = class(TInterfacedObject, IdwlAPIResponse)
  private
    FHTTPResponse: IdwlHTTPResponse;
    FSuccess: boolean;
    FJSON: TJSONObject;
    FJSON_Data: TJSONObject;
    FErrors: TJSONArray;
    function JSON: TJSONObject;
    function JSON_Data: TJSONObject;
    function JSON_Data_Array(const APath: string): IdwlAPIJSONArray;
    function Errors: TJSONArray;
    function HTTPResponse: IdwlHTTPResponse;
    function StatusCode: cardinal;
    function Success: boolean;
  public
    constructor Create(AHTTPResponse: IdwlHTTPResponse);
    destructor Destroy; override;
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
  if (AHTTPResponse.StatusCode div 100=2) then
  begin
    if AHTTPResponse.Header[HTTP_FIELD_CONTENT_TYPE].StartsWith(MEDIA_TYPE_JSON, true) then
    begin
      var JSONResult: TJSONValue := nil;
      try
        JSONResult := TJSONValue.ParseJSONValue(AHTTPResponse.AsString);
        if JSONResult is TJSONObject then
        begin
          FJSON := TJSONObject(JSONResult);
          FSuccess := FJSON.GetValue<boolean>('success', false);
        end
        else
          JSONResult.Free;
      except
        FSuccess := false;
        JSONResult.Free;
        FJSON := nil;
      end;
    end
    else
      FSuccess := true;
  end;
end;

function TdwlAPIResponse.JSON: TJSONObject;
begin
  Result := FJSON;
end;

function TdwlAPIResponse.JSON_Data: TJSONObject;
begin
  if FJSON_Data=nil then
    FJSON.TryGetValue<TJSONObject>('data', FJSON_Data);
  Result := FJSON_Data;
end;

function TdwlAPIResponse.JSON_Data_Array(const APath: string): IdwlAPIJSONArray;
begin
  var JSON: TJSONArray;
  if (JSON_Data<>nil) and JSON_Data.TryGetValue<TJSONArray>(APath, JSON) then
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

function TdwlAPIResponse.StatusCode: cardinal;
begin
  Result := HTTPResponse.StatusCode;
end;

function TdwlAPIResponse.Success: boolean;
begin
  Result := FSuccess;
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
       lRequest.Header['Authorization'] := FAuthorizer.AuthHeaderLeadText+AccessToken;
    end;
    if URLEncodedParamsOrPostBody<>'' then
    begin
      if (Http_Method=HTTP_METHOD_POST) or (Http_Method=HTTP_METHOD_PUT) then
      begin
        lRequest.WritePostData(URLEncodedParamsOrPostBody);
        if PostBodyIsJSON then
          lRequest.Header[HTTP_FIELD_CONTENT_TYPE] := MEDIA_TYPE_JSON
        else
          lRequest.Header[HTTP_FIELD_CONTENT_TYPE] := MEDIA_TYPE_X_WWW_FORM_URLENCODED;
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
      Result.HTTPRequest.Header['Authorization'] := FAuthorizer.AuthHeaderLeadText+AccessToken;
    end;
  except
    // continue un-prepared, this will come out in the execution phase
  end;
end;

{ TdwlAPIAuthorizerWithRefreshToken }

constructor TdwlAPIAuthorizerWithRefreshToken.Create(CallbackProc: TdwlAPIAuthorizerCallBackProc);
begin
  inherited Create;
  FCallBackProc := CallbackProc;
end;

function TdwlAPIAuthorizerWithRefreshToken.GetRefreshToken: string;
begin
  if (FRefreshtoken='') and Assigned(FCallBackProc) then
    FCallBackProc(FRefreshtoken, acaGetRefreshtoken);
  if FRefreshtoken='' then
    AcquireRefreshToken;
  Result := FRefreshtoken;
end;

procedure TdwlAPIAuthorizerWithRefreshToken.InvalidateAuthorization;
begin
  inherited InvalidateAuthorization;
  FRefreshtoken := '';
  if Assigned(FCallBackProc) then
  begin
    var Dummy: string;
    FCallBackProc(Dummy, acaInvalidateAuthorization);
  end;
end;

procedure TdwlAPIAuthorizerWithRefreshToken.NewRefreshToken(const NewRefreshToken: string);
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
    HTTPRequest.Header[HTTP_FIELD_CONTENT_TYPE] := MEDIA_TYPE_JSON;
    HTTPRequest.WritePostData(FJSON.ToJSON);
  end;
  Result := TdwlAPIResponse.Create(InternalExecute(false));
end;

function TdwlAPIRequest.GetMethod: string;
begin
  Result := HTTPRequest.Method;
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

procedure TdwlAPIRequest.SetMethod(const Value: string);
begin
  HTTPRequest.Method := Value;
end;

procedure TdwlAPIRequest.WritePostData(const PostData: string);
begin
  HTTPRequest.WritePostData(PostData);
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

{ TdwlAPIAuthorizer }

function TdwlAPIAuthorizer.AccessTokenPresent: boolean;
begin
  Result := FAccessToken<>'';
end;

function TdwlAPIAuthorizer.AuthHeaderLeadText: string;
begin
  Result := HTTP_AUTHORIZATION_BEARER+' ';
end;

function TdwlAPIAuthorizer.GetAccesstoken(EnforceNewOne: boolean): string;
begin
  if EnforceNewOne or (FAccessTokenExpireTick<GetTickCount64) then
    FAccessToken := '';
  if FAccessToken='' then
      AcquireAccessToken;
  Result := FAccessToken;
end;

procedure TdwlAPIAuthorizer.InvalidateAuthorization;
begin
  FAccessToken := '';
end;

procedure TdwlAPIAuthorizer.NewAccessToken(const NewAccessToken: string; NewAccessTokenExpireTick: UInt64);
begin
  FAccessToken := NewAccessToken;
  FAccessTokenExpireTick := NewAccessTokenExpireTick;
end;

end.
