/// <summary>
///   A simple to use HTTPClient. Constructed around the WinINet library <br />
///   To start a request just call the function New_HTTPCreateRequest();
/// </summary>
unit DWL.HTTP.Client;

{$I DWL.inc}

interface

uses
  System.Classes, System.SysUtils, DWL.HTTP.Types;

const
  WINHTTP_ACCESS_TYPE_AUTOMATIC_PROXY = 4;

type
  IdwlHTTPResponse = interface
    function GetHeader(const HeaderKey: string): string;
    /// <summary>
    ///   The header property can be used to get Header values
    /// </summary>
    /// <param name="HeaderKey">
    ///   The key of the header to set or get
    /// </param>
    /// <value>
    ///   The headervalue that belongs to the given key
    /// </value>
    property Header[const HeaderKey: string]: string read GetHeader;
    /// <summary>
    ///   retrieve the applicatble Error message is the request was not
    ///   successful
    /// </summary>
    function ErrorMsg: string;
    /// <summary>
    ///   The returned HTTP Status code
    /// </summary>
    function StatusCode: cardinal;
    /// <summary>
    ///   Gives the data back as textual data
    /// </summary>
    /// <param name="Encoding">
    ///   Encoding to use when converting bytes to string. No Encoding means
    ///   TEncoding.Default
    /// </param>
    function AsString(Encoding: TEncoding=nil): string;
    /// <summary>
    ///   Gives the data back as TBytes
    /// </summary>
    function AsBytes: TBytes;
    /// <summary>
    ///   a stream to be able to access the body returned
    /// </summary>
    function Stream: TMemoryStream;
  end;

  /// <summary>
  ///   The HTTP Request is an interface to an object handling all asprecrts of
  ///   an HTTP Request through WinINet. Just prepare and execute, a HTTP
  ///   Response interface will be the answer
  /// </summary>
  IdwlHTTPRequest = interface
    function GetHeader(const HeaderKey: string): string;
    function GetMethod: string;
    function GetPassword: string;
    function GetTimeOut: cardinal;
    function GetURL: string;
    function GetUserName: string;
    procedure SetHeader(const HeaderKey, Value: string);
    procedure SetMethod(const Value: string);
    procedure SetPassword(const Value: string);
    procedure SetTimeOut(Value: cardinal);
    procedure SetURL(const Value: string);
    procedure SetUserName(const Value: string);
    function GetOnProgress: TdwlHTTPProgressEvent;
    procedure SetOnProgress(const Value: TdwlHTTPProgressEvent);
    /// <summary>
    ///   The header property can be used to set and get Header values
    /// </summary>
    /// <param name="HeaderKey">
    ///   The key of the header to set or get
    /// </param>
    /// <value>
    ///   The headervalue that belongs to the given key
    /// </value>
    property Header[const HeaderKey: string]: string read GetHeader write SetHeader;
    /// <summary>
    ///   Change of onsult the calling method of the request, default it is
    ///   HTTP_METHOD_GET
    /// </summary>
    property Method: string read GetMethod write SetMethod;
    /// <summary>
    ///   set of get the password to be used with basic authentication
    /// </summary>
    property Password: string read GetPassword write SetPassword;
    /// <summary>
    ///   The applicable timout if the request is executed, default 1000ms
    /// </summary>
    property TimeOut: cardinal read GetTimeOut write SetTimeOut;
    /// <summary>
    ///   set of get the username to be used wiht basic authentication
    /// </summary>
    property UserName: string read GetUserName write SetUserName;
    /// <summary>
    ///   Attach a procedure to get informed about the progress
    /// </summary>
    property OnProgress: TdwlHTTPProgressEvent read GetOnProgress write SetOnProgress;
    /// <summary>
    ///   the URL of the request
    /// </summary>
    property URL: string read GetURL write SetURL;
    /// <summary>
    ///   Executes the request and returns a respons. See IdslHTTPRespons for
    ///   further information
    /// </summary>
    function Execute: IdwlHTTPResponse;
    /// <summary>
    ///   A Stream to write PostData into the request
    /// </summary>
    function PostStream: TMemoryStream;
    /// <summary>
    ///   if the postdata is simple text, you can use WritePostData instead of
    ///   the more complicated PostStreadm approach
    /// </summary>
    /// <param name="PostData">
    ///   the text to add as postdata in the body
    /// </param>
    procedure WritePostData(const PostData: string);
  end;

/// <summary>
///   This function prepares a new request, eventually enrich the request
///   thhrough its properties and call the Execute method
/// </summary>
function New_HTTPRequest(const URL: string=''): IdwlHTTPRequest;
/// <summary>
///   This function is used to create a respons to signal that request failed before actually executing it
/// </summary>
function Get_EmptyHTTPResponse(StatusCode: cardinal): IdwlHTTPResponse;

procedure PutInternetExplorerBrowserEmulationInRegistry;

implementation

uses
  Winapi.WinHTTP, Winapi.Windows, System.Win.Registry, DWL.HTTP.Consts;

var
 hSession: HINTERNET=nil;

type
  TdwlHTTPRequest = class(TInterfacedObject, IdwlHTTPRequest)
  private
    FURL: string;
    FMethod: string;
    FUserName: string;
    FPassword: string;
    FTimeOut: cardinal;
    FOnProgress: TdwlHTTPProgressEvent;
    FPostStream: TMemoryStream;
    FHeaderKeys: TStringList;
    FHeaderValues: TStringList;
    constructor Create;
    destructor Destroy; override;
    function GetHeader(const HeaderKey: string): string;
    function GetMethod: string;
    function GetOnProgress: TdwlHTTPProgressEvent;
    function GetPassword: string;
    function GetTimeOut: cardinal;
    function GetURL: string;
    function GetUserName: string;
    procedure SetHeader(const HeaderKey, Value: string);
    procedure SetMethod(const Value: string);
    procedure SetOnProgress(const Value: TdwlHTTPProgressEvent);
    procedure SetPassword(const Value: string);
    procedure SetTimeOut(Value: cardinal);
    procedure SetURL(const Value: string);
    procedure SetUserName(const Value: string);
    function Execute: IdwlHTTPResponse;
    function PostStream: TMemoryStream;
    procedure WritePostData(const PostData: string);
  end;

  TdwlHTTPResponse = class(TInterfacedObject, IdwlHTTPResponse)
  private
    FErrorMsg: string;
    FHeaderStr: string;
    FHeaderKeys: TStringList;
    FHeaderValues: TStringList;
    FStatusCode: cardinal;
    FStream: TMemoryStream;
    constructor Create;
    destructor Destroy; override;
    function AsString(Encoding: TEncoding=nil): string;
    function AsBytes: TBytes;
    function ErrorMsg: string;
    function GetHeader(const HeaderKey: string): string;
    function StatusCode: cardinal;
    function Stream: TMemoryStream;
  end;

function New_HTTPRequest(const URL: string=''): IdwlHTTPRequest;
begin
  Result := TdwlHTTPRequest.Create;
  Result.URL := URL;
end;

function Get_EmptyHTTPResponse(StatusCode: cardinal): IdwlHTTPResponse;
begin
  var Response := TdwlHTTPResponse.Create;
  Response.FStatusCode := StatusCode;
  Result := Response;
end;

procedure PutInternetExplorerBrowserEmulationInRegistry;
begin
  var Reg := TRegistry.Create(KEY_WRITE);
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey('\Software\Microsoft\Internet Explorer\Main\FeatureControl\FEATURE_BROWSER_EMULATION', true) then
      Reg.WriteInteger(ExtractFileName(ParamStr(0)), 11001);
  finally
    Reg.Free;
  end;
end;

 function CheckhSession: boolean;
begin
  Result := hSession<>nil;
  if not Result then
  begin
    hSession := WinHTTPOpen('Mozilla/5.0 (compatible)', WINHTTP_ACCESS_TYPE_AUTOMATIC_PROXY, nil, nil, 0);
    Result := hSession<>nil;
  end;
end;

{ TdwlHTTPRequest }

constructor TdwlHTTPRequest.Create;
begin
  inherited Create;
  FMethod := HTTP_METHOD_GET;
  FTimeOut := 1000;
  FHeaderKeys := TStringList.Create;
  FHeaderKeys.CaseSensitive := false;
  FHeaderValues := TStringList.Create;
end;

destructor TdwlHTTPRequest.Destroy;
begin
  FPostStream.Free;
  FHeaderKeys.Free;
  FHeaderValues.Free;
  inherited Destroy;
end;

function TdwlHTTPRequest.Execute: IdwlHTTPResponse;
var
  Buffer: array[0..16384] of byte; // Raw Headers can be as big as 16K
begin
  var Response := TdwlHTTPResponse.Create;
  try
    // Initialize internet api
    if not CheckhSession then
    begin
      Response.FStatusCode := HTTP_STATUS_SERVICE_UNAVAIL;
      Response.FErrorMsg := 'Failed to initialize WinHTTP ('+GetLastError.ToString+')';
      Exit;
    end;
    // Crack URL into components
    // Warning: values in URLComps are NOT Null terminated, you really need to use Length for these string values
    var URLComps: TURLComponents;
    FillChar(URLComps, SizeOf(URLComps), 0);
    URLComps.dwStructSize := SizeOf(URLComps);
    URLComps.dwSchemeLength := 1;
    URLComps.dwHostNameLength := 1;
    URLComps.dwURLPathLength := 1;
    if not WinHttpCrackUrl(PWideChar(FURL), 0, 0, URLComps) then
    begin
      Response.FStatusCode := HTTP_STATUS_BAD_REQUEST;
      Response.FErrorMsg := 'WinHttpCrackUrl failed ('+GetLastError.ToString+') (url:'+FURL;
      Exit;
    end;
    // Set internet connection timeout
    if FTimeOut>0 then
    begin
      if not WinHttpSetOption(hSession, WINHTTP_OPTION_CONNECT_TIMEOUT, @FTimeout, 4) then
      begin
        Response.FStatusCode := HTTP_STATUS_SERVICE_UNAVAIL;
        Response.FErrorMsg := 'WinHttpSetOption failed when setting timeout ('+GetLastError.ToString+')';
        Exit;
      end;
    end;
    // Open IP Connection
    var HostName := Copy(URLComps.lpszHostName, 1, URLComps.dwHostNameLength);
    var hConnect := WinHttpConnect(hSession, PWideChar(HostName), URLComps.nPort, 0);
    if hConnect=nil then
    begin
      Response.FStatusCode := HTTP_STATUS_SERVICE_UNAVAIL;
      Response.FErrorMsg := 'WinHttpConnect failed ('+GetLastError.ToString+')';
      Exit;
    end;
    // Open HTTP Request
    try
      var UrlPath := Copy(URLComps.lpszUrlPath, 1, URLComps.dwUrlPathLength);
      var Flags := 0;
      if SameText(Copy(URLComps.lpszScheme, 1, URLComps.dwSchemeLength), 'https') then
        Flags := Flags or WINHTTP_FLAG_SECURE;
      var hRequest := WinHttpOpenRequest(hConnect, PWideChar(FMethod), PWideChar(UrlPath), nil, WINHTTP_NO_REFERER, WINHTTP_DEFAULT_ACCEPT_TYPES, Flags);
      if hRequest=nil then
      begin
        Response.FStatusCode := HTTP_STATUS_BAD_REQUEST;
        Response.FErrorMsg := 'WinHttpOpenRequest failed ('+GetLastError.ToString+')';
        Exit;
      end;
      try
        // if given, set credentials
        if (FUserName<>'') or (FPassword<>'') then
        begin
          if not WinHttpSetCredentials(hRequest, WINHTTP_AUTH_TARGET_SERVER, WINHTTP_AUTH_SCHEME_BASIC, PWideChar(FUsername), pWideChar(FPassword), nil) then
          begin
            Response.FStatusCode := HTTP_STATUS_SERVICE_UNAVAIL;
            Response.FErrorMsg := 'WinHttpSetCredentials failed ('+GetLastError.ToString+')';
            Exit;
          end;
        end;
        // Add headers
        for var i := 0 to FHeaderKeys.Count-1 do
        begin
          var Header := FHeaderKeys[i]+': '+FHeaderValues[i];
          if not WinHttpAddRequestHeaders(hRequest, PWideChar(Header), Header.Length, WINHTTP_ADDREQ_FLAG_ADD) then
          begin
            Response.FStatusCode := HTTP_STATUS_SERVICE_UNAVAIL;
            Response.FErrorMsg := 'WinHttpAddRequestHeaders failed ('+GetLastError.ToString+')';
            Exit;
          end;
        end;
        // Send Http Reqest
        var PostData: pointer;
        var PostDataSize: DWORD;
        if (FPostStream{!}=nil) then
        begin
          PostData := WINHTTP_NO_REQUEST_DATA;
          PostDataSize := 0;
        end
        else
        begin
          PostData := FPostStream.Memory;
          PostDataSize := FPostStream.Size;
        end;
        if not WinHttpSendRequest(hRequest, nil, 0, PostData, PostDataSize, PostDataSize, 0) then
        begin
          Response.FStatusCode := HTTP_STATUS_BAD_REQUEST;
          Response.FErrorMsg := 'WinHttpSendRequest failed ('+GetLastError.ToString+')';
          Exit;
        end;
        if not WinHttpReceiveResponse(hRequest, nil) then
        begin
          Response.FStatusCode := HTTP_STATUS_BAD_REQUEST;
          Response.FErrorMsg := 'WinHttpReceiveResponse failed ('+GetLastError.ToString+')';
          Exit;
        end;
        // check status code
        var BufferLen: DWORD := SizeOf(Buffer);
        var HeaderIdx: DWORD := 0;
        if not WinHttpQueryHeaders(hRequest, WINHTTP_QUERY_STATUS_CODE or WINHTTP_QUERY_FLAG_NUMBER, nil, @Buffer, BufferLen, @HeaderIdx) then
        begin
          Response.FStatusCode := HTTP_STATUS_BAD_REQUEST;
          Response.FErrorMsg := 'WinHttpQueryHeaders failed ('+GetLastError.ToString+')';
          Exit;
        end;
        Response.FStatusCode := PCardinal(@Buffer)^;
        if not (Response.StatusCode in [HTTP_STATUS_OK, HTTP_STATUS_CREATED]) then
          Response.FErrorMsg := 'Status code '+Response.FStatusCode.ToString;
        // get headers
        BufferLen := SizeOf(Buffer);
        HeaderIdx := 0;
        if WinHttpQueryHeaders(hRequest, WINHTTP_QUERY_RAW_HEADERS_CRLF, nil, @Buffer, BufferLen, @HeaderIdx) then
        begin
          SetLength(Response.FHeaderStr, BufferLen div 2);
          Move((@Buffer)^, (@Response.FHeaderStr[1])^, BufferLen);
        end;
        // total size
        BufferLen := SizeOf(Buffer);
        HeaderIdx := 0;
        var TotalSize: cardinal := 0;
        if Assigned(FOnProgress) and WinHttpQueryHeaders(hRequest, WINHTTP_QUERY_CONTENT_LENGTH or WINHTTP_QUERY_FLAG_NUMBER, nil, @Buffer, BufferLen, @HeaderIdx) then
          TotalSize := PCardinal(@Buffer)^;
        FUserName := TotalSize.ToString;
        // Fetch Data
        var BytesRead := 0;
        var CancelReceiving := False;
        repeat
          if not WinHttpReadData(hRequest, Buffer, Sizeof(Buffer), @BufferLen) then
            Break;
           if BufferLen>0 then
           begin
             Response.FStream.WriteBuffer(Buffer[0], BufferLen);
             inc(BytesRead, BufferLen);
             if Assigned(FOnProgress) then
               TThread.Queue(nil,
                 procedure
                 begin
                   FOnProgress(BytesRead, TotalSize, CancelReceiving);
                 end);
             if CancelReceiving then
             begin
               Response.FStatusCode := HTTP_STATUS_BAD_REQUEST;
               Response.FErrorMsg := 'User cancelled...';
               Exit;
             end;
           end;
        until BufferLen = 0;
      finally
        WinHttpCloseHandle(hRequest);
      end;
    finally
      WinHttpCloseHandle(hConnect);
    end;
  finally
    Result := Response;
  end;
end;

function TdwlHTTPRequest.GetHeader(const HeaderKey: string): string;
begin
  var i := FHeaderKeys.IndexOf(HeaderKey);
  if i>=0 then
    Result := FHeaderValues[i]
  else
    Result := '';
end;

function TdwlHTTPRequest.GetMethod: string;
begin
  Result := FMethod;
end;

function TdwlHTTPRequest.GetOnProgress: TdwlHTTPProgressEvent;
begin
   Result := FOnProgress;
end;

function TdwlHTTPRequest.GetPassword: string;
begin
  Result := FPassword;
end;

function TdwlHTTPRequest.GetTimeOut: cardinal;
begin
  Result := FTimeOut;
end;

function TdwlHTTPRequest.GetURL: string;
begin
  Result := FURL;
end;

function TdwlHTTPRequest.GetUserName: string;
begin
  Result := FUserName;
end;

function TdwlHTTPRequest.PostStream: TMemoryStream;
begin
  if FPostStream=nil then
    FPostStream := TMemoryStream.Create;
  Result := FPostStream;
end;

procedure TdwlHTTPRequest.SetHeader(const HeaderKey, Value: string);
begin
  var i := FHeaderKeys.IndexOf(HeaderKey);
  if i>=0 then
    FHeaderValues[i] := Value
  else
  begin
    FHeaderKeys.Add(HeaderKey);
    FHeaderValues.Add(Value);
  end;
end;

procedure TdwlHTTPRequest.SetMethod(const Value: string);
begin
  FMethod := Value;
end;

procedure TdwlHTTPRequest.SetOnProgress(const Value: TdwlHTTPProgressEvent);
begin
  FOnProgress := Value;
end;

procedure TdwlHTTPRequest.SetPassword(const Value: string);
begin
  FPassword := Value;
end;

procedure TdwlHTTPRequest.WritePostData(const PostData: string);
var
  AnsiPostData: ansistring;
begin
  AnsiPostData := ansistring(PostData);
  PostStream.Write(PAnsiChar(AnsiPostData)^, Length(AnsiPostData));
end;

procedure TdwlHTTPRequest.SetTimeOut(Value: cardinal);
begin
  FTimeOut := Value;
end;

procedure TdwlHTTPRequest.SetURL(const Value: string);
begin
  FURL := Value;
end;

procedure TdwlHTTPRequest.SetUserName(const Value: string);
begin
  FUserName := Value;
end;

{ TdwlHTTPResponse }

function TdwlHTTPResponse.AsBytes: TBytes;
begin
  SetLength(Result, FStream.Size);
  FStream.Seek(0, soBeginning);
  FStream.Read(Result, 0, FStream.Size);
end;

function TdwlHTTPResponse.AsString(Encoding: TEncoding): string;
begin
  if Encoding=nil then
    Encoding := TEncoding.Default;
  Result := Encoding.GetString(AsBytes);
end;

constructor TdwlHTTPResponse.Create;
begin
  inherited Create;
  FStream := TMemoryStream.Create;
end;

destructor TdwlHTTPResponse.Destroy;
begin
  FStream.Free;
  FHeaderKeys.Free;
  FHeaderValues.Free;
  inherited Destroy;
end;

function TdwlHTTPResponse.ErrorMsg: string;
begin
  Result := FErrorMsg;
end;

function TdwlHTTPResponse.GetHeader(const HeaderKey: string): string;
  procedure ParseHeaders;
  begin
    if FHeaderKeys<>nil then
      Exit;
    FHeaderKeys := TStringList.Create;
    FHeaderValues := TStringList.Create;
    FHeaderKeys.CaseSensitive := false;
    FHeaderKeys.Text := FHeaderStr;
    if FHeaderKeys.Count>0 then
      FHeaderKeys.Delete(0); // The HTTP version header;
    for var i := 0 to FHeaderKeys.Count-1 do
    begin
      var Header := FHeaderKeys[i];
      var P := Pos(':', Header);
      FHeaderKeys[i] := Copy(Header, 1, P-1);
      FHeaderValues.Add(Copy(Header, P+1, MaxInt).TrimLeft);
    end;
  end;
begin
  ParseHeaders;
  var i := FHeaderKeys.IndexOf(HeaderKey);
  if i>=0 then
    Result := FHeaderValues[i]
  else
    Result := '';
end;

function TdwlHTTPResponse.StatusCode: cardinal;
begin
  Result := FStatusCode;
end;

function TdwlHTTPResponse.Stream: TMemoryStream;
begin
  Result := FStream;
end;

initialization

finalization
  if hSession<>nil then
    WinHttpCloseHandle(hSession);

end.

