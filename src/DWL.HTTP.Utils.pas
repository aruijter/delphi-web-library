unit DWL.HTTP.Utils;

interface

type
  IdwlHTTPFieldValue = interface
    function MainValue: string;
    function SubValue(const Key: string; const Default: string=''): string;
  end;

  TdwlHTTPUtils = record
    class function StringTodwlhttpMethod(const HttpMethod: string): integer; static;
    class function StatusCodeDescription(StatusCode: integer): string; static;
    class function ParseHTTPFieldValue(const Value: string): IdwlHTTPFieldValue; static;
    class function MIMEnameToCodepage(const Charset: string; Default: cardinal=CP_UTF8): integer; static;
  end;

implementation

uses
  DWL.HTTP.Consts, System.StrUtils, Winapi.WinInet, System.SysUtils,
  System.Classes, DWL.Logging;

type
  TdwlHTTPField = class(TInterfacedObject, IdwlHTTPFieldValue)
  strict private
    FMainValue: string;
    FExtraValues: TStringList;
  private
    function MainValue: string;
    function SubValue(const Key: string; const Default: string=''): string;
  public
    constructor Create(const Value: string);
    destructor Destroy; override;
  end;

{ TdwlHTTPUtils }

class function TdwlHTTPUtils.MIMEnameToCodepage(const Charset: string; Default: cardinal=CP_UTF8): integer;
begin
  for var Combination in CodePageMIMEnameCombinations do
    if SameText(Combination.MIMEname, Charset) then
      Exit(Combination.Codepage);
  Result := CP_UTF8; // The default
end;

class function TdwlHTTPUtils.ParseHTTPFieldValue(const Value: string): IdwlHTTPFieldValue;
begin
  Result := TdwlHTTPField.Create(Value);
end;

class function TdwlHTTPUtils.StatusCodeDescription(StatusCode: integer): string;
begin
  case StatusCode of
    HTTP_STATUS_CONTINUE: Result := 'Continue';
    HTTP_STATUS_SWITCH_PROTOCOLS: Result := 'Switching protocols';
    HTTP_STATUS_OK: Result := 'OK';
    HTTP_STATUS_CREATED: Result := 'Created';
    HTTP_STATUS_ACCEPTED: Result := 'Accepted';
    HTTP_STATUS_PARTIAL: Result := 'Non-Authoritative Information';
    HTTP_STATUS_NO_CONTENT: Result := 'No Content';
    HTTP_STATUS_RESET_CONTENT: Result := 'Reset Content';
    HTTP_STATUS_PARTIAL_CONTENT: Result := 'Partial Content';
    HTTP_STATUS_AMBIGUOUS: Result := 'Multiple Choices';
    HTTP_STATUS_MOVED: Result := 'Moved Permanently';
    HTTP_STATUS_REDIRECT: Result := 'Found';
    HTTP_STATUS_REDIRECT_METHOD: Result := 'See Other';
    HTTP_STATUS_NOT_MODIFIED: Result := 'Not Modified';
    HTTP_STATUS_USE_PROXY: Result := 'Use Proxy';
    HTTP_STATUS_REDIRECT_KEEP_VERB: Result := 'Temporary Redirect';
    HTTP_STATUS_BAD_REQUEST: Result := 'Bad Request';
    HTTP_STATUS_DENIED: Result := 'Unauthorized';
    HTTP_STATUS_PAYMENT_REQ: Result := 'Payment Required';
    HTTP_STATUS_FORBIDDEN: Result := 'Forbidden';
    HTTP_STATUS_NOT_FOUND: Result := 'Not Found';
    HTTP_STATUS_BAD_METHOD: Result := 'Method Not Allowed';
    HTTP_STATUS_NONE_ACCEPTABLE: Result := 'Not Acceptable';
    HTTP_STATUS_PROXY_AUTH_REQ: Result := 'Proxy Authentication Required';
    HTTP_STATUS_REQUEST_TIMEOUT: Result := 'Request Timeout';
    HTTP_STATUS_CONFLICT: Result := 'Conflict';
    HTTP_STATUS_GONE: Result := 'Gone';
    HTTP_STATUS_LENGTH_REQUIRED: Result := 'Length Required';
    HTTP_STATUS_PRECOND_FAILED: Result := 'Precondition Failed';
    HTTP_STATUS_REQUEST_TOO_LARGE: Result := 'Payload Too Large';
    HTTP_STATUS_URI_TOO_LONG: Result := 'URI Too Long';
    HTTP_STATUS_UNSUPPORTED_MEDIA: Result := 'Unsupported Media Type';
    HTTP_STATUS_RETRY_WITH: Result := 'Retry With';
    HTTP_STATUS_SERVER_ERROR: Result := 'Internal Server Error';
    HTTP_STATUS_NOT_SUPPORTED: Result := 'Not Implemented';
    HTTP_STATUS_BAD_GATEWAY: Result := 'Bad Gateway';
    HTTP_STATUS_SERVICE_UNAVAIL: Result := 'Service Unavailable';
    HTTP_STATUS_GATEWAY_TIMEOUT: Result := 'Gateway Timeout';
    HTTP_STATUS_VERSION_NOT_SUP: Result := 'HTTP Version Not Supported';
  else
    Result := 'Status Code '+StatusCode.ToString;
  end;
end;

class function TdwlHTTPUtils.StringTodwlhttpMethod(const HttpMethod: string): integer;
begin
  Result := IndexStr(HttpMethod, dwlhttpMethodToString);
end;

{ TdwlHTTPField }

constructor TdwlHTTPField.Create(const Value: string);
begin
  inherited Create;
  var Parts := Value.Split([';']);
  if High(Parts)<0 then
    Exit;
  FMainValue := Parts[0].Trim;
  if High(Parts)<1 then
    Exit;
  FExtraValues := TStringList.Create;
  for var i := 1 to High(Parts) do
    FExtraValues.Add(Parts[i].Trim);
  inherited Destroy;
end;

destructor TdwlHTTPField.Destroy;
begin
  FExtraValues.Free;
  inherited Destroy;
end;

function TdwlHTTPField.SubValue(const Key: string; const Default: string=''): string;
begin
  if FExtraValues=nil then
    Exit(Default);
  Result := FExtraValues.Values[Key];
  if Result='' then
    Result := Default;
end;

function TdwlHTTPField.MainValue: string;
begin
  Result := FMainValue;
end;

end.
