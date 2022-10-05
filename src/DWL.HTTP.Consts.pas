unit DWL.HTTP.Consts;

interface

const
  HTTP_HEADER_CONTENT_TYPE = 'Content-Type';
  HTTP_HEADER_REPLAY_NONCE = 'Replay-Nonce';
  HTTP_HEADER_LOCATION = 'Location';

  HTTP_COMMAND_UNKNOWN = 'UNKNOWN';
  HTTP_COMMAND_HEAD = 'HEAD';
  HTTP_COMMAND_GET ='GET';
  HTTP_COMMAND_POST = 'POST';
  HTTP_COMMAND_DELETE = 'DELETE';
  HTTP_COMMAND_PUT = 'PUT';
  HTTP_COMMAND_TRACE = 'TRACE';
  HTTP_COMMAND_OPTIONS = 'OPTIONS';

  CONTENT_TYPE_X_WWW_FORM_URLENCODED = 'application/x-www-form-urlencoded';
  CONTENT_TYPE_HTML = 'text/html';
  CONTENT_TYPE_JSON = 'application/json';

  CHARSET_UTF8 = 'utf-8';

  Param_BaseURI= 'baseuri';
  Param_Endpoint= 'endpoint';

  EndpointURI_Log = '/log';

implementation

end.
