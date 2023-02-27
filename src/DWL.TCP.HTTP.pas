unit DWL.TCP.HTTP;

interface

uses
  DWL.TCP.Server, Winapi.Winsock2, System.Generics.Collections, DWL.Params,
  System.Classes, DWL.TCP;

type
  TdwlHTTPServerConnectionState = (hcsReadRequest, hcsReadHeader, hcsReadRequestBody, hcsProcessing, hcsClosing, hcsError);
  TdwlHTTPProtocol = (HTTP10, HTTP11);

type
  TdwlHTTPSocket=class;

  TdwlCustomHTTPServer = class(TdwlTCPServer)
  protected
    function HandleRequest(Request: TdwlHTTPSocket): boolean;
  public
    constructor Create;
  end;

  TdwlHTTPSocket = class(TdwlSocket)
  strict private
    FCommand: string;
    FUri: string;
    FState: TdwlHTTPServerConnectionState;
    FPendingLine: string;
    FRequestHeaders: IdwlParams;
    FResponseHeaders: IdwlParams;
    FRequestBodyStream: TMemoryStream;
    FResponseDataStream: TMemoryStream;
    FContentLength: integer;
    FStatusCode: integer;
    FProtocol: TdwlHTTPProtocol;
    FReadError: string;
    procedure ClearCurrentRequest;
    procedure ReadAddToPendingLine(First, Last: PByte);
    procedure ReadFinishHeader;
    procedure ReadPendingLine;
    procedure ReadProcessRequest;
  public
    property StatusCode: integer read FStatusCode write FStatusCode;
    constructor Create(AService: TdwlTCPService); override;
    destructor Destroy; override;
    procedure ReadHandlingBuffer(HandlingBuffer: PdwlHandlingBuffer); override;
  end;

implementation

uses
  Winapi.Windows, System.SysUtils, System.NetEncoding, Winapi.WinInet,
  System.StrUtils, Access2000, DWL.HTTP.Utils, DWL.HTTP.Consts;

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

procedure TdwlHTTPSocket.ClearCurrentRequest;
begin
  FRequestHeaders.Clear;
  FResponseHeaders.Clear;
  if FRequestBodyStream<>nil then
    FRequestBodyStream.Clear;
  if FResponseDataStream<>nil then
    FResponseDataStream.Clear;
  FState := hcsReadRequest;
  // No need to clear other variables, they're always overwritten during evaluation of next request
end;

constructor TdwlHTTPSocket.Create(AService: TdwlTCPService);
begin
  inherited Create(AService);
  Assert(AService is TdwlCustomHTTPServer);
  FState := hcsReadRequest;
  FRequestHeaders := New_Params;
  FResponseHeaders := New_Params;
  FStatusCode := HTTP_STATUS_OK;
end;

destructor TdwlHTTPSocket.Destroy;
begin
  FRequestBodyStream.Free;
  FResponseDataStream.Free;
  inherited Destroy;
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
        FRequestBodyStream.Write(Curr^, Eof-Curr)
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
  var TransferEncoding := FRequestHeaders.StrValue(HTTP_HEADER_TRANSFER_ENCODING);
  if SameText(TransferEncoding, TRANSFER_ENCODING_CHUNCKED) then
  begin
    FState := hcsError;
    FStatusCode := HTTP_STATUS_NOT_SUPPORTED;
    FReadError := 'Transfer encoding chuncked not supported';
    Exit;
  end;
  if not FRequestHeaders.TryGetIntValue(HTTP_HEADER_CONTENT_LENGTH, FContentLength) then
    FContentLength := 0;
  if FContentLength>0 then
  begin
    if FRequestBodyStream=nil then
      FRequestBodyStream := TMemoryStream.Create;
    FState := hcsReadRequestBody
  end
  else
    FState := hcsProcessing;
end;

procedure TdwlHTTPSocket.ReadPendingLine;
begin
  case FState of
  hcsReadRequest:
    begin
      var Parts := FPendingLine.Split([' ']);
      if (High(Parts)<>2) then
      begin
        FState := hcsError;
        FStatusCode := HTTP_STATUS_BAD_REQUEST;
        FReadError := 'First line not containing three elements';
        Exit;
      end;
      if Copy(Parts[2], 1, 8)='HTTP/1.1' then
        FProtocol := HTTP11
      else
      begin
        if Copy(Parts[2], 1, 8)='HTTP/1.0' then
          FProtocol := HTTP11
        else
        begin
          FState := hcsError;
          FStatusCode := HTTP_STATUS_VERSION_NOT_SUP;
          FReadError := 'Cannot handle protocol '+Parts[2];
          Exit;
        end;
      end;
      FCommand := Parts[0];
      FUri := TNetEncoding.URL.Decode(Parts[1]);
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
        begin
          FReadError := 'Missing colon in header line';
          FStatusCode := HTTP_STATUS_BAD_REQUEST;
          FState := hcsError;
        end
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
  var KeepAlive := (FProtocol<>HTTP10) and SameText(FRequestHeaders.StrValue(HTTP_HEADER_CONNECTION), CONNECTION_KEEP_ALIVE);
  if FProtocol<>HTTP10 then
    FResponseHeaders.WriteValue(HTTP_HEADER_CONNECTION, IfThen(KeepAlive, CONNECTION_KEEP_ALIVE, CONNECTION_CLOSE));
  if FResponseDataStream=nil then
    FResponseDataStream := TMemoryStream.Create;
  if FState=hcsError then
  begin
    if FReadError<>'' then
    begin
      FResponseHeaders.WriteValue(HTTP_HEADER_CONTENT_TYPE, CONTENT_TYPE_HTML);
      FResponseDataStream.WriteData('<html><body><h1>'+TNetEncoding.HTML.Encode(FReadError)+'</h1></body></html>');
    end;
  end
  else
  begin
    if not TdwlCustomHTTPServer(FService).HandleRequest(Self) then
      StatusCode := HTTP_STATUS_NOT_FOUND;
  end;
  // Remember to not write Content-Length when Command CONNECT is added later
  FResponseHeaders.WriteValue(HTTP_HEADER_CONTENT_LENGTH, FResponseDataStream.Size.ToString);
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
  WriteLine(''); // end of headers
  // write body
  if FResponseDataStream.Size>0 then
    WriteBuf(PByte(FResponseDataStream.Memory), FResponseDataStream.Size);
  // finalize
  FlushWrites(not KeepAlive);
  if KeepAlive then
    ClearCurrentRequest
  else
    FState := hcsClosing;
end;

end.



