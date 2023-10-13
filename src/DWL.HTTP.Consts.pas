unit DWL.HTTP.Consts;

interface

const
  CHARSET_UTF8 = 'utf-8';

  CONNECTION_CLOSE = 'Close';
  CONNECTION_KEEP_ALIVE = 'Keep-Alive';

  CONTENT_TYPE_7Z = 'application/x-7z-compressed';
  CONTENT_TYPE_HTML = 'text/html';
  CONTENT_TYPE_IMAGE_PNG = 'image/png';
  CONTENT_TYPE_JOSE_JSON = 'application/jose+json';
  CONTENT_TYPE_JSON = 'application/json';
  CONTENT_TYPE_JSONAPI = 'application/vnd.api+json';
  CONTENT_TYPE_OCTET_STREAM = 'application/octet-stream';
  CONTENT_TYPE_PLAIN = 'text/plain';
  CONTENT_TYPE_X_WWW_FORM_URLENCODED = 'application/x-www-form-urlencoded';

  EXPECT_100_CONTINUE = '100-continue';

  HTTP_FIELD_CACHE_CONTROL = 'Cache-Control';
  HTTP_FIELD_CONNECTION = 'Connection';
  HTTP_FIELD_CONTENT_DISPOSITION = 'Content-Disposition';
  HTTP_FIELD_CONTENT_LENGTH = 'Content-Length';
  HTTP_FIELD_CONTENT_TYPE = 'Content-Type';
  HTTP_FIELD_EXPECT = 'Expect';
  HTTP_FIELD_HOST = 'Host';
  HTTP_FIELD_LOCATION = 'Location';
  HTTP_FIELD_REPLAY_NONCE = 'Replay-Nonce';
  HTTP_FIELD_RETRY_AFTER = 'Retry-After';
  HTTP_FIELD_TRANSFER_ENCODING = 'Transfer-Encoding';

  HTTP_METHOD_DELETE = 'DELETE';
  HTTP_METHOD_GET ='GET';
  HTTP_METHOD_HEAD = 'HEAD';
  HTTP_METHOD_OPTIONS = 'OPTIONS';
  HTTP_METHOD_POST = 'POST';
  HTTP_METHOD_PUT = 'PUT';
  HTTP_METHOD_TRACE = 'TRACE';
  HTTP_METHOD_UNKNOWN = 'UNKNOWN';

  HTTP_SUBFIELD_CHARSET = 'charset';

  TRANSFER_ENCODING_CHUNCKED = 'chuncked';
  // HTTP Method consts
  dwlhttpUNKNOWN = 0;
  dwlhttpHEAD = 1;
  dwlhttpGET = 2;
  dwlhttpPOST = 3;
  dwlhttpDELETE = 4;
  dwlhttpPUT = 5;
  dwlhttpTRACE = 6;
  dwlhttpOPTIONS = 7;

  ALPN_HTTP_1_1 = 'http/1.1';
  ALPN_ACME_tls_1 = 'acme-tls/1';

  dwlhttpMethodToString: array[dwlhttpUNKNOWN..dwlhttpOPTIONS] of string = (HTTP_METHOD_UNKNOWN,
    HTTP_METHOD_HEAD,HTTP_METHOD_GET, HTTP_METHOD_POST, HTTP_METHOD_DELETE, HTTP_METHOD_PUT, HTTP_METHOD_TRACE, HTTP_METHOD_OPTIONS);

  CodePageMIMEnameCombinations: array[0..139] of record Codepage: cardinal; MIMEname: string end = (
   (Codepage: 65001; MIMEname: 'utf-8'),
   (Codepage: 20127; MIMEname: 'us-ascii'),
   (Codepage: 1252; MIMEname: 'windows-1252'),
   (Codepage: 28591; MIMEname: 'iso-8859-1'),
   (Codepage: 65000; MIMEname: 'utf-7'),
   (Codepage: 12000; MIMEname: 'utf-32'),
   (Codepage: 28592; MIMEname: 'iso-8859-2'),
   (Codepage: 28593; MIMEname: 'iso-8859-3'),
   (Codepage: 28594; MIMEname: 'iso-8859-4'),
   (Codepage: 28595; MIMEname: 'iso-8859-5'),
   (Codepage: 28596; MIMEname: 'iso-8859-6'),
   (Codepage: 28597; MIMEname: 'iso-8859-7'),
   (Codepage: 28598; MIMEname: 'iso-8859-8'),
   (Codepage: 28599; MIMEname: 'iso-8859-9'),
   (Codepage: 28603; MIMEname: 'iso-8859-13'),
   (Codepage: 28605; MIMEname: 'iso-8859-15'),
   (Codepage: 1250; MIMEname: 'windows-1250'),
   (Codepage: 1251; MIMEname: 'windows-1251'),
   (Codepage: 1253; MIMEname: 'windows-1253'),
   (Codepage: 1254; MIMEname: 'windows-1254'),
   (Codepage: 1255; MIMEname: 'windows-1255'),
   (Codepage: 1256; MIMEname: 'windows-1256'),
   (Codepage: 1257; MIMEname: 'windows-1257'),
   (Codepage: 1258; MIMEname: 'windows-1258'),
   (Codepage: 037; MIMEname: 'IBM037'),
   (Codepage: 437; MIMEname: 'IBM437'),
   (Codepage: 500; MIMEname: 'IBM500'),
   (Codepage: 708; MIMEname: 'ASMO-708'),
   (Codepage: 720; MIMEname: 'DOS-720'),
   (Codepage: 737; MIMEname: 'ibm737'),
   (Codepage: 775; MIMEname: 'ibm775'),
   (Codepage: 850; MIMEname: 'ibm850'),
   (Codepage: 852; MIMEname: 'ibm852'),
   (Codepage: 855; MIMEname: 'IBM855'),
   (Codepage: 857; MIMEname: 'ibm857'),
   (Codepage: 858; MIMEname: 'IBM00858'),
   (Codepage: 860; MIMEname: 'IBM860'),
   (Codepage: 861; MIMEname: 'ibm861'),
   (Codepage: 862; MIMEname: 'DOS-862'),
   (Codepage: 863; MIMEname: 'IBM863'),
   (Codepage: 864; MIMEname: 'IBM864'),
   (Codepage: 865; MIMEname: 'IBM865'),
   (Codepage: 866; MIMEname: 'cp866'),
   (Codepage: 869; MIMEname: 'ibm869'),
   (Codepage: 870; MIMEname: 'IBM870'),
   (Codepage: 874; MIMEname: 'windows-874'),
   (Codepage: 875; MIMEname: 'cp875'),
   (Codepage: 932; MIMEname: 'shift_jis'),
   (Codepage: 936; MIMEname: 'gb2312'),
   (Codepage: 949; MIMEname: 'ks_c_5601-1987'),
   (Codepage: 950; MIMEname: 'big5'),
   (Codepage: 1026; MIMEname: 'IBM1026'),
   (Codepage: 1047; MIMEname: 'IBM01047'),
   (Codepage: 1140; MIMEname: 'IBM01140'),
   (Codepage: 1141; MIMEname: 'IBM01141'),
   (Codepage: 1142; MIMEname: 'IBM01142'),
   (Codepage: 1143; MIMEname: 'IBM01143'),
   (Codepage: 1144; MIMEname: 'IBM01144'),
   (Codepage: 1145; MIMEname: 'IBM01145'),
   (Codepage: 1146; MIMEname: 'IBM01146'),
   (Codepage: 1147; MIMEname: 'IBM01147'),
   (Codepage: 1148; MIMEname: 'IBM01148'),
   (Codepage: 1149; MIMEname: 'IBM01149'),
   (Codepage: 1200; MIMEname: 'utf-16'),
   (Codepage: 1201; MIMEname: 'unicodeFFFE'),
   (Codepage: 1361; MIMEname: 'Johab'),
   (Codepage: 10000; MIMEname: 'macintosh'),
   (Codepage: 10001; MIMEname: 'x-mac-japanese'),
   (Codepage: 10002; MIMEname: 'x-mac-chinesetrad'),
   (Codepage: 10003; MIMEname: 'x-mac-korean'),
   (Codepage: 10004; MIMEname: 'x-mac-arabic'),
   (Codepage: 10005; MIMEname: 'x-mac-hebrew'),
   (Codepage: 10006; MIMEname: 'x-mac-greek'),
   (Codepage: 10007; MIMEname: 'x-mac-cyrillic'),
   (Codepage: 10008; MIMEname: 'x-mac-chinesesimp'),
   (Codepage: 10010; MIMEname: 'x-mac-romanian'),
   (Codepage: 10017; MIMEname: 'x-mac-ukrainian'),
   (Codepage: 10021; MIMEname: 'x-mac-thai'),
   (Codepage: 10029; MIMEname: 'x-mac-ce'),
   (Codepage: 10079; MIMEname: 'x-mac-icelandic'),
   (Codepage: 10081; MIMEname: 'x-mac-turkish'),
   (Codepage: 10082; MIMEname: 'x-mac-croatian'),
   (Codepage: 12001; MIMEname: 'utf-32BE'),
   (Codepage: 20000; MIMEname: 'x-Chinese_CNS'),
   (Codepage: 20001; MIMEname: 'x-cp20001'),
   (Codepage: 20002; MIMEname: 'x_Chinese-Eten'),
   (Codepage: 20003; MIMEname: 'x-cp20003'),
   (Codepage: 20004; MIMEname: 'x-cp20004'),
   (Codepage: 20005; MIMEname: 'x-cp20005'),
   (Codepage: 20105; MIMEname: 'x-IA5'),
   (Codepage: 20106; MIMEname: 'x-IA5-German'),
   (Codepage: 20107; MIMEname: 'x-IA5-Swedish'),
   (Codepage: 20108; MIMEname: 'x-IA5-Norwegian'),
   (Codepage: 20261; MIMEname: 'x-cp20261'),
   (Codepage: 20269; MIMEname: 'x-cp20269'),
   (Codepage: 20273; MIMEname: 'IBM273'),
   (Codepage: 20277; MIMEname: 'IBM277'),
   (Codepage: 20278; MIMEname: 'IBM278'),
   (Codepage: 20280; MIMEname: 'IBM280'),
   (Codepage: 20284; MIMEname: 'IBM284'),
   (Codepage: 20285; MIMEname: 'IBM285'),
   (Codepage: 20290; MIMEname: 'IBM290'),
   (Codepage: 20297; MIMEname: 'IBM297'),
   (Codepage: 20420; MIMEname: 'IBM420'),
   (Codepage: 20423; MIMEname: 'IBM423'),
   (Codepage: 20424; MIMEname: 'IBM424'),
   (Codepage: 20833; MIMEname: 'x-EBCDIC-KoreanExtended'),
   (Codepage: 20838; MIMEname: 'IBM-Thai'),
   (Codepage: 20866; MIMEname: 'koi8-r'),
   (Codepage: 20871; MIMEname: 'IBM871'),
   (Codepage: 20880; MIMEname: 'IBM880'),
   (Codepage: 20905; MIMEname: 'IBM905'),
   (Codepage: 20924; MIMEname: 'IBM00924'),
   (Codepage: 20932; MIMEname: 'EUC-JP'),
   (Codepage: 20936; MIMEname: 'x-cp20936'),
   (Codepage: 20949; MIMEname: 'x-cp20949'),
   (Codepage: 21025; MIMEname: 'cp1025'),
   (Codepage: 21866; MIMEname: 'koi8-u'),
   (Codepage: 29001; MIMEname: 'x-Europa'),
   (Codepage: 38598; MIMEname: 'iso-8859-8-i'),
   (Codepage: 50220; MIMEname: 'iso-2022-jp'),
   (Codepage: 50221; MIMEname: 'csISO2022JP'),
   (Codepage: 50222; MIMEname: 'iso-2022-jp'),
   (Codepage: 50225; MIMEname: 'iso-2022-kr'),
   (Codepage: 50227; MIMEname: 'x-cp50227'),
   (Codepage: 51932; MIMEname: 'euc-jp'),
   (Codepage: 51936; MIMEname: 'EUC-CN'),
   (Codepage: 51949; MIMEname: 'euc-kr'),
   (Codepage: 52936; MIMEname: 'hz-gb-2312'),
   (Codepage: 54936; MIMEname: 'GB18030'),
   (Codepage: 57002; MIMEname: 'x-iscii-de'),
   (Codepage: 57003; MIMEname: 'x-iscii-be'),
   (Codepage: 57004; MIMEname: 'x-iscii-ta'),
   (Codepage: 57005; MIMEname: 'x-iscii-te'),
   (Codepage: 57006; MIMEname: 'x-iscii-as'),
   (Codepage: 57007; MIMEname: 'x-iscii-or'),
   (Codepage: 57008; MIMEname: 'x-iscii-ka'),
   (Codepage: 57009; MIMEname: 'x-iscii-ma'),
   (Codepage: 57010; MIMEname: 'x-iscii-gu'),
   (Codepage: 57011; MIMEname: 'x-iscii-pa')
  );

implementation

end.
