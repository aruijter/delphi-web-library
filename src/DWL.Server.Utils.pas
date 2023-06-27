/// <summary>
///   Utils to centralize common server functionality
/// </summary>
unit DWL.Server.Utils;

interface

uses
  DWL.Server.Types, DWL.HTTP.Consts;

type
  /// <summary>
  ///   State helpers for easy task when handling requests
  /// </summary>
  TdwlHTTPHandlingStateHelper = record helper for TdwlHTTPHandlingState
    function TryGetRequestParamStr(const Key: string; var Value: string): boolean;
    function TryGetHeaderValue(const Key: string; var Value: string): boolean;
    function TryGetResponseHeaderValue(const Key: string; var Value: string): boolean;
    /// <summary>
    ///   an easy way to set the contenttype when handling a request. This
    ///   function uses the server callback function to set the contenttype of
    ///   the request.
    /// </summary>
    procedure SetContentType(const ContentType: string; const CharSet: string='');
    /// <summary>
    ///   an easy way to set the contenttext when handling a request.
    ///   This function uses the server callback function to set the
    ///   contentbuffer/length and contenttype of the request.
    /// </summary>
    procedure SetContentText(const BodyStr: string; const ContentType: string=CONTENT_TYPE_HTML);
    procedure SetHeaderValue(const HeaderKey, Value: string);
  end;

implementation

uses
  System.SysUtils, Winapi.Windows, DWL.Server.Globals;

{ TdwlHTTPHandlingStateHelper }

procedure TdwlHTTPHandlingStateHelper.SetContentType(const ContentType: string; const CharSet: string='');
begin
  var CharsetToWrite := CharSet;
  if CharsetToWrite='' then
  begin
    if SameText(ContentType, CONTENT_TYPE_X_WWW_FORM_URLENCODED) then
      CharsetToWrite := CHARSET_UTF8; // always add charset=utf-8 TNetEncoding.Url does utf8, but default for content type is iso-8859-1
  end;
  var CombinedContentType := ContentType;
  if CharSet<>'' then
    CombinedContentType := CombinedContentType+'; charset='+CharsetToWrite;
  serverProcs.SetHeaderValueProc(@Self, HTTP_FIELD_CONTENT_TYPE, PWideChar(CombinedContentType));
end;

procedure TdwlHTTPHandlingStateHelper.SetHeaderValue(const HeaderKey,Value: string);
begin
  serverProcs.SetHeaderValueProc(@Self, PWideChar(HeaderKey), PWideChar(Value));
end;

function TdwlHTTPHandlingStateHelper.TryGetHeaderValue(const Key: string; var Value: string): boolean;
begin
  var CharCount := 0;
  var Res := serverProcs.GetHeaderValueProc(@Self, PWideChar(Key), nil, CharCount);
  Result := Res=-1;
  if Result then
  begin
    SetLength(Value, CharCount);
    Res := serverProcs.GetHeaderValueProc(@Self, PWideChar(Key), PWideChar(Value), CharCount);
    Result := (Res=1) and (CharCount=Length(Value));
  end;
end;

function TdwlHTTPHandlingStateHelper.TryGetResponseHeaderValue(const Key: string; var Value: string): boolean;
begin
  var CharCount := 0;
  var Res := serverProcs.GetResponseHeaderValueProc(@Self, PWideChar(Key), nil, CharCount);
  Result := Res=-1;
  if Result then
  begin
    SetLength(Value, CharCount);
    Res := serverProcs.GetResponseHeaderValueProc(@Self, PWideChar(Key), PWideChar(Value), CharCount);
    Result := (Res=1) and (CharCount=Length(Value));
  end;
end;

function TdwlHTTPHandlingStateHelper.TryGetRequestParamStr(const Key: string; var Value: string): boolean;
begin
  var CharCount := 0;
  var Res := serverProcs.GetRequestParamProc(@Self, PWideChar(Key), nil, CharCount);
  Result := Res=-1;
  if Result then
  begin
    SetLength(Value, CharCount);
    Res := serverProcs.GetRequestParamProc(@Self, PWideChar(Key), PWideChar(Value), CharCount);
    Result := (Res=1) and (CharCount=Length(Value));
  end;
end;

procedure TdwlHTTPHandlingStateHelper.SetContentText(const BodyStr: string; const ContentType: string=CONTENT_TYPE_HTML);
begin
  if ContentType<>'' then
    Self.SetContentType(ContentType, CHARSET_UTF8);
  var ContentLength := WideCharToMultiByte(CP_UTF8, 0, PWideChar(BodyStr), BodyStr.Length, nil, 0, nil, nil);
  var ContentBuffer := nil;
  serverProcs.ArrangeContentBufferProc(@Self, ContentBuffer, ContentLength);
  WideCharToMultiByte(CP_UTF8, 0, PWideChar(BodyStr), BodyStr.Length, ContentBuffer, ContentLength, nil, nil);
end;

end.
