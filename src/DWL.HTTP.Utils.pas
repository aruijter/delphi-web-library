unit DWL.HTTP.Utils;

interface

type
  TdwlHTTPUtils = record
    class function StringTodwlhttpCommand(const HttpCommand: string): byte; static;
    class function StatusCodeDescription(StatusCode: integer): string; static;
  end;

implementation

uses
  DWL.HTTP.Consts, System.StrUtils, Winapi.WinInet, System.SysUtils;

{ TdwlHTTPUtils }

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

class function TdwlHTTPUtils.StringTodwlhttpCommand(const HttpCommand: string): byte;
begin
  Result := IndexStr(HttpCommand, dwlhttpCommandToString);
end;

end.
