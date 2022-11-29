unit DWL.HTTP.Consts;

interface

const
  HTTP_HEADER_CONTENT_TYPE = 'Content-Type';
  HTTP_HEADER_CONTENT_DISPOSITION = 'Content-Disposition';
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
  CONTENT_TYPE_OCTET_STREAM = 'application/octet-stream';
  CONTENT_TYPE_IMAGE_PNG = 'image/png';
  CONTENT_TYPE_7Z = 'application/x-7z-compressed';

  CHARSET_UTF8 = 'utf-8';

  Param_BaseURI= 'baseuri';
  Param_Endpoint= 'endpoint';
  Param_Issuer = 'issuer';
  Param_LogSecret = 'logsecret';

  EndpointURI_Log = '/log';
  EndpointURI_Mail = '/mail';
  Default_EndpointURI_OAuth2 = '/oauth2';

  // HTTP Command consts
  dwlhttpUNKNOWN = 0;
  dwlhttpHEAD = 1;
  dwlhttpGET = 2;
  dwlhttpPOST = 3;
  dwlhttpDELETE = 4;
  dwlhttpPUT = 5;
  dwlhttpTRACE = 6;
  dwlhttpOPTIONS = 7;

  dwlhttpCommandToString: array[dwlhttpUNKNOWN..dwlhttpOPTIONS] of string = (HTTP_COMMAND_UNKNOWN,
    HTTP_COMMAND_HEAD,HTTP_COMMAND_GET, HTTP_COMMAND_POST, HTTP_COMMAND_DELETE, HTTP_COMMAND_PUT, HTTP_COMMAND_TRACE, HTTP_COMMAND_OPTIONS);

implementation

end.
