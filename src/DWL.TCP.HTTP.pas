unit DWL.TCP.HTTP;

interface

uses
  DWL.TCP.Server, Winapi.Winsock2, System.Generics.Collections, DWL.Params,
  System.Classes, DWL.TCP, Winapi.WinInet;

type
  TdwlHTTPServerConnectionState = (hcsReadRequest, hcsReadHeader, hcsReadRequestBody, hcsProcessing, hcsClosing, hcsError);
  TdwlHTTPProtocol = (HTTP10, HTTP11);

type
  TdwlHTTPSocket=class;
  TdwlHTTPServer_OnLog=procedure(Request: TdwlHTTPSocket) of object;


  TdwlCustomHTTPServer = class(TdwlTCPServer)
  strict private
    FOnLog: TdwlHTTPServer_Onlog;
  protected
    function HandleRequest(Request: TdwlHTTPSocket): boolean; virtual;
  public
    property OnLog: TdwlHTTPServer_Onlog read FOnLog write FOnLog;
    constructor Create;
  end;

  TdwlHTTPSocket = class(TdwlSocket)
  strict private
    FRequestMethod: byte;
    FUri: string;
    FState: TdwlHTTPServerConnectionState;
    FPendingLine: string;
    FRequestParams: TStringList;
    FRequestHeaders: IdwlParams;
    FResponseHeaders: IdwlParams;
    FRequestBodyStream: TMemoryStream;
    FResponseDataStream: TMemoryStream;
    FContentLength: integer;
    FStatusCode: integer;
    FTickStart: UInt64;
    FProtocol: TdwlHTTPProtocol;
    FReadError: string;
    procedure ClearCurrentRequest;
    procedure ReadAddToPendingLine(First, Last: PByte);
    procedure ReadFinishHeader;
    procedure ReadPendingLine;
    procedure ReadProcessRequest;
    procedure ReadProcessURI;
    procedure ReadError(const ErrorText: string; NewStatusCode: integer = HTTP_STATUS_BAD_REQUEST);
  public
    property RequestMethod: byte read FRequestMethod;
    property RequestBodyStream: TMemoryStream read FRequestBodyStream;
    property RequestHeaders: IdwlParams read FRequestHeaders;
    property RequestParams: TStringList read FRequestParams;
    property ResponseHeaders: IdwlParams read FResponseHeaders;
    property StatusCode: integer read FStatusCode write FStatusCode;
    property Uri: string read FUri;
    property ResponseDataStream: TMemoryStream read FResponseDataStream;
    property TickStart: UInt64 read FTickStart;
    constructor Create(AService: TdwlTCPService); override;
    destructor Destroy; override;
    procedure ReadHandlingBuffer(HandlingBuffer: PdwlHandlingBuffer); override;
  end;

implementation

uses
  Winapi.Windows, System.SysUtils, System.NetEncoding,
  System.StrUtils, DWL.HTTP.Utils, DWL.HTTP.Consts, DWL.Logging;

{ TdwlCustomHTTPServer }

constructor TdwlCustomHTTPServer.Create;
begin
  inherited Create(TdwlHTTPSocket);
end;

function TdwlCustomHTTPServer.HandleRequest(Request: TdwlHTTPSocket): boolean;
begin
  Result := false;
end;

{ TdwlHTTPSocket }

procedure TdwlHTTPSocket.ReadAddToPendingLine(First, Last: PByte);
begin
  var Finished := Last^=10;
  if Finished then
  begin
    dec(Last);
    if First>Last then // notthing to process, but check if last of FPendingLine = #13!
    begin
      var len := Length(FPendingLine);
      if (len>0) and (FPendingLine[len]=#13) then
        FPendingLine := Copy(FPendingLine, 1, len-1);
    end
    else
    begin
      if Last^=13 then
        dec(Last);
    end;
  end;
  if First<=Last then
  begin
    var WideStr: string;
    var Len := Last-First+1;
    SetLength(WideStr, Len);
    SetLength(WideStr, MultiByteToWideChar(FService.CodePage_US_ASCII, 0, PAnsiChar(First), Len, @WideStr[1], Len));
    FPendingLine := FPendingLine+WideStr;
  end;
  if Finished then
  begin
    ReadPendingLine;
    FPendingLine := '';
  end;
end;

procedure TdwlHTTPSocket.ReadError(const ErrorText: string; NewStatusCode: integer = HTTP_STATUS_BAD_REQUEST);
begin
  FState := hcsError;
  FStatusCode := NewStatusCode;
  if FReadError<>'' then
    FReadError := FReadError+'<br />' ;
  FReadError := FReadError+ ErrorText;
end;

procedure TdwlHTTPSocket.ClearCurrentRequest;
begin
  FRequestParams.Clear;
  FRequestHeaders.Clear;
  FResponseHeaders.Clear;
  if FRequestBodyStream<>nil then
    FRequestBodyStream.Clear;
  if FResponseDataStream<>nil then
    FResponseDataStream.Clear;
  FState := hcsReadRequest;
  FReadError := '';
  // No need to clear other variables, they're always overwritten during evaluation of next request
end;

constructor TdwlHTTPSocket.Create(AService: TdwlTCPService);
begin
  inherited Create(AService);
  Assert(AService is TdwlCustomHTTPServer);
  FState := hcsReadRequest;
  FRequestHeaders := New_Params;
  FRequestParams := TStringList.Create;
  FResponseHeaders := New_Params;
  FStatusCode := HTTP_STATUS_OK;
end;

destructor TdwlHTTPSocket.Destroy;
begin
  FRequestBodyStream.Free;
  FResponseDataStream.Free;
  FRequestParams.Free;
  inherited Destroy;
end;

procedure TdwlHTTPSocket.ReadProcessURI;
begin
  var P := Pos('?', FUri);
  if P>0 then
  begin
    var Query := Copy(FUri, p+1, MaxInt).Split(['&']);
    FUri := Copy(FUri, 1, P-1);
    for var Param in Query do
    begin
      try
        P := pos('=', Param);
        if P>1 then
          RequestParams.Add(TNetEncoding.URL.Decode(Copy(Param, 1, P-1))+'='+TNetEncoding.URL.Decode(Copy(Param, P+1, MaxInt)))
        else
          RequestParams.Add(TNetEncoding.URL.Decode(Param));
      except
        ReadError('Error Decoding URL Parameter: '+Param);
      end;
    end;
  end;
  var ContTypeHeader := TdwlHTTPUtils.ParseHTTPFieldValue(RequestHeaders.StrValue(HTTP_FIELD_CONTENT_TYPE));
  if SameText(ContTypeHeader.MainValue, CONTENT_TYPE_X_WWW_FORM_URLENCODED) and (FRequestBodyStream<>nil) then
  begin
    var WideStr: string;;
    var CodePage := TdwlHTTPUtils.MIMEnameToCodepage(ContTypeHeader.SubValue(HTTP_SUBFIELD_CHARSET, CHARSET_UTF8));
    var Len := RequestBodyStream.Size;
    SetLength(WideStr, Len);
    Len := MultiByteToWideChar(CodePage, 0, RequestBodyStream.Memory, Len, @WideStr[1], Len);
    SetLength(WideStr, Len);
    var Query := WideStr.Split(['&']);
    for var Param in Query do
    begin
      P := pos('=', Param);
      if P>1 then
        // leave the value undecoded, to be able to use stringlist with all provided string values
        RequestParams.Add(TNetEncoding.URL.Decode(Copy(Param, 1, P-1))+'='+Copy(Param, P+1, MaxInt))
      else
        RequestParams.Add(TNetEncoding.URL.Decode(Param));
    end;
  end;
end;

procedure TdwlHTTPSocket.ReadHandlingBuffer(HandlingBuffer: PdwlHandlingBuffer);
begin
  var Curr: PByte := HandlingBuffer.Buf;
  var Eof: PByte := Curr+HandlingBuffer.NumberOfBytes;
  while Curr<Eof do
  begin
    case FState of
    hcsReadRequest, hcsReadHeader:
      begin
        var Start := Curr;
        while Curr<Eof do
        begin
          if Curr^=10 then // found a line
          begin
            ReadAddToPendingLine(Start, Curr);
            Start := Curr+1;
          end;
          inc(Curr);
          if FState>hcsReadHeader then
            Break;
        end;
        if (FState<=hcsReadHeader) and (Start<Eof) then
        begin
          ReadAddToPendingLine(Start, Eof-1);
          Curr := Eof;
        end;
      end;
    hcsReadRequestBody:
      begin
        FRequestBodyStream.Write(Curr^, Eof-Curr);
        if FRequestBodyStream.Size>=FContentLength then
        begin
          FRequestBodyStream.Seek(0, soFromBeginning);
          FState := hcsProcessing;
        end;
        Curr := Eof;
      end;
    hcsProcessing: ReadProcessRequest;
    hcsError,
    hcsClosing: Exit;
    end;
  end;
  if FState=hcsProcessing then
    ReadProcessRequest;
end;

procedure TdwlHTTPSocket.ReadFinishHeader;
begin
  var TransferEncoding := FRequestHeaders.StrValue(HTTP_FIELD_TRANSFER_ENCODING);
  if SameText(TransferEncoding, TRANSFER_ENCODING_CHUNCKED) then
  begin
    ReadError('Transfer encoding chuncked not supported', HTTP_STATUS_NOT_SUPPORTED);
    Exit;
  end;
  if not FRequestHeaders.TryGetIntValue(HTTP_FIELD_CONTENT_LENGTH, FContentLength) then
    FContentLength := 0;
  if FContentLength>0 then
  begin
    if FRequestBodyStream=nil then
      FRequestBodyStream := TMemoryStream.Create;
    FState := hcsReadRequestBody
  end
  else
    FState := hcsProcessing;
  // Handle 100 request
  if SameText(FRequestHeaders.StrValue(HTTP_FIELD_EXPECT), EXPECT_100_CONTINUE) then
  begin
    WriteLine('HTTP/1.1 100 Continue');
    WriteLine;
    FlushWrites;
  end;
end;

procedure TdwlHTTPSocket.ReadPendingLine;
begin
  case FState of
  hcsReadRequest:
    begin
      var Parts := FPendingLine.Split([' ']);
      if (High(Parts)<>2) then
      begin
        ReadError('First line not containing three elements');
        Exit;
      end;
      if Copy(Parts[2], 1, 8)='HTTP/1.1' then
        FProtocol := HTTP11
      else
      begin
        if Copy(Parts[2], 1, 8)='HTTP/1.0' then
          FProtocol := HTTP10
        else
        begin
          ReadError('Cannot handle protocol '+Parts[2], HTTP_STATUS_VERSION_NOT_SUP);
          Exit;
        end;
      end;
      var Method := TdwlHTTPUtils.StringTodwlhttpMethod(Parts[0]);
      if Method<0 then
      begin
        ReadError('Cannot handle method '+Parts[0], HTTP_STATUS_NOT_SUPPORTED);
        Exit;
      end
      else
        FRequestMethod := Method;
      FUri := Parts[1];
      FState := hcsReadHeader;
    end;
  hcsReadHeader:
    begin
      if FPendingLine='' then
        ReadFinishHeader
      else
      begin
        var p := pos(':', FPendingLine);
        if p<2 then
          ReadError('Missing colon in header line')
        else
        begin
          // RFC2616 describes that headers with same name must be concatenated, not overwritten
          var Key := Copy(FPendingLine, 1, p-1);
          var Value := Copy(FPendingLine, p+2, MaxInt).Trim;
          var CurrentValue := FRequestHeaders.StrValue(Key);
          if CurrentValue='' then
            FRequestHeaders.WriteValue(Key, Value)
          else
            FRequestHeaders.WriteValue(Key, CurrentValue+','+Value);
        end;
      end;
    end;
  end;
end;

procedure TdwlHTTPSocket.ReadProcessRequest;
begin
  var KeepAlive: boolean;
  try
    try
      FTickStart := GetTickCount64;
      KeepAlive := (FState<>hcsError) and (FProtocol<>HTTP10) and SameText(RequestHeaders.StrValue(HTTP_FIELD_CONNECTION), CONNECTION_KEEP_ALIVE);
      if FProtocol<>HTTP10 then
        FResponseHeaders.WriteValue(HTTP_FIELD_CONNECTION, IfThen(KeepAlive, CONNECTION_KEEP_ALIVE, CONNECTION_CLOSE));
      if FResponseDataStream=nil then
        FResponseDataStream := TMemoryStream.Create;
      // extract request parameters
      if FState<>hcsError then
        ReadProcessURI;
      if FState=hcsError then
      begin
        if FReadError<>'' then
        begin
          FResponseHeaders.WriteValue(HTTP_FIELD_CONTENT_TYPE, CONTENT_TYPE_HTML);
          var ErrStr := ansistring('<html><body><h1>'+TNetEncoding.HTML.Encode(FReadError)+'</h1></body></html>');
          FResponseDataStream.WriteBuffer(PAnsiChar(ErrStr)^, Length(ErrStr));
        end;
      end
      else
      begin
        // let the request be processed bij the server implementation
        if not TdwlCustomHTTPServer(FService).HandleRequest(Self) then
          StatusCode := HTTP_STATUS_NOT_FOUND;
      end;
      // Remember to not write Content-Length when method CONNECT is added later
      FResponseHeaders.WriteValue(HTTP_FIELD_CONTENT_LENGTH, FResponseDataStream.Size.ToString);
      // Write HTTP protocol line
      WriteStr('HTTP/1.');
      case FProtocol of
        HTTP10: WriteStr('0');
        HTTP11: WriteStr('1');
      end;
      WriteStr(' ');
      WriteStr(FStatusCode.ToString);
      WriteStr(' ');
      WriteLine(TdwlHTTPUtils.StatusCodeDescription(FStatusCode));
      // write headers
      var HeaderENum := FResponseHeaders.GetEnumerator;
      while HeaderEnum.MoveNext do
        WriteLine(HeaderENum.CurrentKey+': '+HeaderENum.CurrentValue.ToString);
      WriteLine; // end of headers
      // write body
      if FResponseDataStream.Size>0 then
        WriteBuf(PByte(FResponseDataStream.Memory), FResponseDataStream.Size);
      // finalize
      FlushWrites(not KeepAlive);
    finally
      // always try to log the request
      if Assigned(TdwlCustomHTTPServer(FService).OnLog) then
        TdwlCustomHTTPServer(FService).OnLog(Self);
    end;
  except
    on E: Exception do
    begin
      KeepAlive := false;
      TdwlLogger.Log('Error in TdwlHTTPSocket.ReadProcessRequest: '+E.Message,lsError);
    end;
  end;
  if KeepAlive then
    ClearCurrentRequest
  else
    FState := hcsClosing;
end;

end.



