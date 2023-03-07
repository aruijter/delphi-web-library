/// <summary>
///   A Base HTTP Server encapsulating an Indy HTTP server. The endpoints that
///   will be served can be extended by writing your own handlers. These
///   handlers can be attached to an endpoint that will be served by this
///   server
/// </summary>
unit DWL.HTTP.Server;

interface

uses
  IdCustomHTTPServer, System.SysUtils, IdContext, System.SyncObjs,
  System.Generics.Collections, IdHTTPServer,
  IdSocketHandle, DWL.Server.Types;

type
  TdwlHTTPServer=class;

  TdwlHTTPServer = class
  strict private
    FServer: TObject;
    FOnlyLocalConnections: boolean;
    FRequestsInProgress: TDictionary<TIdContext, PdwlHTTPHandlingState>;
    FRequestsInProgressAccess: TCriticalSection;
    FHTTPServer: TIdHTTPServer;
    // SSL fields
    FSSL_HostName: string;
    FSSL_CertificateFileName: string;
    FSSL_RootCertificateFileName: string;
    FSSL_PrivateKeyFileName: string;
    function GetBindings: TIdSocketHandles;
    procedure HTTPServerCommand(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
    procedure HTTPServerDisconnect(AContext: TIdContext);
    procedure HTTPServerParseAuthentication(AContext: TIdContext; const AAuthType, AAuthData: String; var VUsername, VPassword: String; var VHandled: Boolean);
    procedure QuerySSLPort(APort: Word; var VUseSSL: boolean);
  public
    /// <summary>
    ///   user Bindings to bind to physical ip/port combinations. This directly
    ///   refers to the Binding property of th Indy HTTP server
    /// </summary>
    property Bindings: TIdSocketHandles read GetBindings;
    /// <summary>
    ///   The minimum loglevel to use when logging requests. See loglevel
    ///   consts for details about the levels
    /// </summary>
    property OnlyLocalConnections: boolean read FOnlyLocalConnections write FOnlyLocalConnections;
    constructor Create(Server: TObject);
    destructor Destroy; override;
    /// <summary>
    ///   <para>
    ///     Adds an hostname by providing certificate details.
    ///   </para>
    ///   <para>
    ///     For now no SNI implemented (because it is not available in
    ///     TIdHTTPServer, so it's quite difficult to implement that). But to
    ///     prepare for the future we defined the by now somewhat too
    ///     promising procedure AddHostName
    ///   </para>
    ///   <para>
    ///     What it actually does, is just overwrite the one and only
    ///     hostname and certificate definition that will be used <br />
    ///   </para>
    /// </summary>
    procedure AddHostName(const HostName, CertificateFileName, RootCertificateFileName, PrivateKeyFileName: string);
    /// <summary>
    ///   Active the server and start processing requests
    /// </summary>
    procedure Open;
  end;

procedure AssignServerProcs;

implementation

uses
  System.Classes, Winapi.Windows, Winapi.WinInet,
  IdGlobal, IdHashSHA,
  System.NetEncoding, IdSSLOpenSSL, DWL.Logging, DWL.HTTP.Consts,
  IdAssignedNumbers, System.StrUtils, DWL.Server.Utils,
  DWL.Server.Globals, DWL.HTTP.Utils, DWL.Classes, DWL.Server;

type
  PServerStructure = ^TServerStructure;
  TServerStructure = record
    State_URI: string;
    FinalHandler: TdwlHTTPHandler;
    Tick: UInt64;
    WebSocketsReceiveProc: TdwlHTTPWebSocket_OnData;
    ContentBuffer: pointer;
    ContentLength: cardinal;
    ContentOwned: boolean;
    Context: TIdContext;
    RequestInfo: TIdHTTPRequestInfo;
    ResponseInfo: TIdHTTPResponseInfo;
  end;

{ TdwlHTTPServer }

procedure TdwlHTTPServer.HTTPServerDisconnect(AContext: TIdContext);
  function GetLogLine(State: PdwlHTTPHandlingState): string;
  begin
    var FinalHandler := PServerStructure(State._InternalServerStructure).FinalHandler;
    if FinalHandler<>nil then
      Result := FinalHandler.URI
    else
      Result := '';
    Result := 'Rq '+AContext.Binding.PeerIP+':'+AContext.Binding.Port.ToString+' '+
      dwlhttpCommandToString[State.Command]+' '+Result+PServerStructure(State._InternalServerStructure).State_URI+' '+
      State.StatusCode.ToString+' ('+(GetTickCount64-PServerStructure(State._InternalServerStructure).Tick).ToString+'ms)';
  end;
begin
  // remove request from FRequestsInProgress, to be able to track while ones are not yet served out
  // FinalizeConnections is using this list to wait for 'processing idle state'
  // Is f.e. used when unloading DLL's
  var State: PdwlHTTPHandlingState;
  FRequestsInProgressAccess.Enter;
  try
    if FRequestsInProgress.TryGetValue(AContext, State) then
      FRequestsInProgress.Remove(AContext)
    else
      State := nil;
  finally
    FRequestsInProgressAccess.Leave;
  end;
  // Do Logging
  if (State<>nil) then
  begin
    if (State.Flags and HTTP_FLAG_NOLOGGING)=0 then
    begin
      if Assigned(PServerStructure(State._InternalServerStructure).WebSocketsReceiveProc) then
        TdwlLogger.Log(GetLogLine(State)+' (websocket closed)', lsTrace)
      else
      begin
        if (State.StatusCode<>HTTP_STATUS_OK) and (TDWLServer(FServer).LogLevel>=httplogLevelFailedRequests) then
          TdwlLogger.Log(GetLogLine(State), lsNotice)
        else
        begin
          if TDWLServer(FServer).Loglevel>=httplogLevelAllRequests then
            TdwlLogger.Log(GetLogLine(State), lsTrace)
        end;
      end;
    end
    else
    begin
      if TDWLServer(FServer).LogLevel>=httplogLevelEverything then
        TdwlLogger.Log(GetLogLine(State), lsTrace);
    end;
    // for safety: feed zero bytes to signal we're finished! (if connection thread was stopped this has never been done in the thread)
    try
      if Assigned(PServerStructure(State._InternalServerStructure).WebSocketsReceiveProc) then
        PServerStructure(State._InternalServerStructure).WebSocketsReceiveProc(State, nil, 0, false);
    except
    end;
    if  PServerStructure(State._InternalServerStructure).ContentOwned then
      FreeMem(PServerStructure(State._InternalServerStructure).ContentBuffer);
    if Assigned(PServerStructure(State._InternalServerStructure).FinalHandler) and
      Assigned(PServerStructure(State._InternalServerStructure).FinalHandler.WrapupProc) then
      PServerStructure(State._InternalServerStructure).FinalHandler.WrapupProc(State);
    PServerStructure(State._InternalServerStructure).State_URI := ''; // dispose string
    Freemem(State._InternalServerStructure);
    FreeMem(State);
  end;
end;

procedure TdwlHTTPServer.HTTPServerParseAuthentication(AContext: TIdContext; const AAuthType, AAuthData: String; var VUsername, VPassword: String; var VHandled: Boolean);
begin
  VHandled := true; // This is to let a bearer token come through without error, this is handled by the server internally
end;

procedure TdwlHTTPServer.AddHostName(const HostName, CertificateFileName, RootCertificateFileName, PrivateKeyFileName: string);
begin
  FSSL_HostName := HostName;
  FSSL_CertificateFileName := CertificateFileName;
  FSSL_RootCertificateFileName := RootCertificateFileName;
  FSSL_PrivateKeyFileName := PrivateKeyFileName;
end;

constructor TdwlHTTPServer.Create(Server: TObject);
begin
  inherited Create;
  FServer := Server;
  FRequestsInProgressAccess := TCriticalSection.Create;
  FRequestsInProgress := TDictionary<TIDContext, PdwlHTTPHandlingState>.Create;
  FHTTPServer := TIdHTTPServer.Create(nil);
  FHTTPServer.OnCommandOther := HTTPServerCommand;
  FHTTPServer.OnCommandGet := HTTPServerCommand;
  FHTTPServer.OnDisconnect := HTTPServerDisconnect;
  FHTTPServer.OnParseAuthentication := HTTPServerParseAuthentication;
end;

destructor TdwlHTTPServer.Destroy;
begin
  FHTTPServer.Free;
  FRequestsInProgress.Free;
  FRequestsInProgressAccess.Free;
  inherited Destroy;
end;

function TdwlHTTPServer.GetBindings: TIdSocketHandles;
begin
  Result := FHTTPServer.Bindings;
end;

procedure State_ArrangeContentBuffer(const State: PdwlHTTPHandlingState; var ContentBuffer: pointer; const ContentLength: cardinal); stdcall;
begin
  Assert(PServerStructure(State._InternalServerStructure).ContentBuffer=nil);
  var Owned := ContentBuffer=nil;
  PServerStructure(State._InternalServerStructure).ContentOwned := Owned;
  if Owned then
    GetMem(ContentBuffer, ContentLength);
  PServerStructure(State._InternalServerStructure).ContentLength := ContentLength;
  PServerStructure(State._InternalServerStructure).ContentBuffer := ContentBuffer;
end;

function State_GetRequestParam(const State: PdwlHTTPHandlingState; const Key: PWideChar; const Value: PWideChar; var  ValueCharCnt: integer): integer; stdcall;
begin
  var ValueFound: boolean := false;;
  var FoundStr: string;
  // Keep in mind do not change Value if Result is false
  var Params := PServerStructure(State._InternalServerStructure).RequestInfo.Params;
  if Copy(Key, Length(Key)-1, 2)='[]' then // handle as array!
  begin
    var Vals := '';
    for var i := 0 to Params.Count-1 do
    begin
      if SameText(Params.Names[i], Key) then
        Vals := Vals+','+Params.ValueFromIndex[i];
    end;
    ValueFound := Vals<>'';
    if ValueFound then
      FoundStr := Copy(Vals, 2, MaxInt);
  end;
  if not ValueFound then
  begin
    ValueFound := Params.IndexOfName(Key)>=0;
    if ValueFound then
      FoundStr := Params.Values[Key];
  end;
  if not ValueFound then
  begin
    ValueFound := SameText(Key, 'remoteip');
    if ValueFound then
      FoundStr := PServerStructure(State._InternalServerStructure).RequestInfo.RemoteIP;
  end;
  if not ValueFound then
  begin
    ValueFound := SameText(Key, 'authusername');
    if ValueFound then
      FoundStr := PServerStructure(State._InternalServerStructure).RequestInfo.AuthUsername;
  end;
  if not ValueFound then
  begin
    ValueFound := SameText(Key, 'authpassword');
    if ValueFound then
      FoundStr := PServerStructure(State._InternalServerStructure).RequestInfo.AuthPassword;
  end;
  if not ValueFound then
  begin
    ValueFound := SameText(Key, 'host');
    if ValueFound then
      FoundStr := PServerStructure(State._InternalServerStructure).RequestInfo.Host;
  end;
  if ValueFound then
  begin
    var FoundStrCharCount := Length(FoundStr);
    if ValueCharCnt>=FoundStrCharCount then
    begin
      if FoundStrCharCount>0 then
      begin
        Move(PWideChar(FoundStr)^, Value^, (FoundStrCharCount+1)*2);
      end;
      Result := 1;
    end
    else
      Result := -1;
    ValueCharCnt := FoundStrCharCount;
  end
  else
    Result := 0;
end;

function State_GetPostDataPtr(State: PdwlHTTPHandlingState; out Data: pointer; out DataSize: Int64): boolean; stdcall;
begin
  var Stream := TMemoryStream(PServerStructure(State._InternalServerStructure).RequestInfo.PostStream);
  Result := (Stream is TMemoryStream) and (Stream.Size>0);
  if not Result then
    Exit;
  Data := Stream.Memory;
  DataSize := Stream.Size;
end;

function State_GetHeaderValue(const State: PdwlHTTPHandlingState; const Key: PWideChar; const Value: PWideChar; var  ValueCharCnt: integer): integer; stdcall;
begin
  var FoundStr := PServerStructure(State._InternalServerStructure).RequestInfo.RawHeaders.Values[Key];
  if FoundStr='' then
    Exit(0);
  var FoundStrCharCount := Length(FoundStr);
  if ValueCharCnt>=FoundStrCharCount then
  begin
    Result := 1;
    Move(PWideChar(FoundStr)^, Value^, (FoundStrCharCount+1)*2);
  end
  else
    Result := -1;
  ValueCharCnt := FoundStrCharCount;
end;

procedure State_SetHeaderValue(const State: PdwlHTTPHandlingState; const HeaderKey, Value: PWideChar); stdcall;
begin
  // Indy writes the contenttype twice if you put it directly in the header,
  // so for contenttype use the ResponseInfo property
  if SameText(HeaderKey, HTTP_HEADER_CONTENT_TYPE) then
    PServerStructure(State._InternalServerStructure).ResponseInfo.ContentType:= Value
  else
    PServerStructure(State._InternalServerStructure).ResponseInfo.CustomHeaders.Values[HeaderKey] := Value;
end;

procedure WriteToWebSocket(State: PdwlHTTPHandlingState; const Data: pointer; DataSize: cardinal; DataIsText: boolean);
begin
  var HeaderByteCount: byte;
  if DataSize<=125 then
    HeaderByteCount := 2 // FIN/opcode and 1 bytes for payload length
  else
  begin
    if DataSize>65535 then
      HeaderByteCount := 10 // FIN/opcode, 127 and 8 bytes for payload length
    else
      HeaderByteCount := 4; // FIN/opcode, 126 and 4 bytes for payload length
  end;
  var DataToSend: TIdBytes;
  SetLength(DataToSend, HeaderByteCount+DataSize);
  if DataSize=0 then // close connection
    DataToSend[0] := $88; // FIN $80 and connection close $8
  if DataIsText then
    DataToSend[0] := $81 // FIN $80 and text frame $1
  else
    DataToSend[0] := $82; // FIN $80 and binary frame $2
  if HeaderByteCount=2 then
    DataToSend[1] := DataSize
  else
  begin
    if HeaderByteCount=4 then
      DataToSend[1] := 126
    else
      DataToSend[1] := 127;
    // stream out payload length in network byte order
    var PayloadLength := DataSize;
    for var i := HeaderByteCount-1 downto 2 do
    begin
      DataToSend[i] := PayloadLength and $FF;
      PayloadLength := PayloadLength shr 8;
    end;
  end;
  if DataSize>0 then
    Move(Data^, DataToSend[HeaderByteCount], DataSize);
  PServerStructure(State._InternalServerStructure).Context.Connection.IOHandler.Write(DataToSend);
end;

function State_ActivateWebSocket(const State: PdwlHTTPHandlingState; ReceiveProc: TdwlHTTPWebSocket_OnData): TdwlHTTPWebSocket_OnData;
begin
  Result := nil;
  var Key: string;
  var Version: string;
  if not (State.TryGetHeaderValue('Sec-WebSocket-Key', Key) and
    State.TryGetHeaderValue('Sec-WebSocket-Version', Version) and
    (Key<>'') and (Version='13')) then
    Exit;
  var Hash := TIdHashSHA1.Create;
  var SignStr: string;
  try
    SignStr := TNetEncoding.Base64.EncodeBytesToString(Hash.HashString(Key+'258EAFA5-E914-47DA-95CA-C5AB0DC85B11'));
  finally
    Hash.Free;
  end;
  PServerStructure(State._InternalServerStructure).ResponseInfo.ResponseNo := HTTP_STATUS_SWITCH_PROTOCOLS;
  PServerStructure(State._InternalServerStructure).ResponseInfo.ResponseText := 'Switching Protocols';
  PServerStructure(State._InternalServerStructure).ResponseInfo.CustomHeaders.AddValue('Upgrade', 'websocket');
  PServerStructure(State._InternalServerStructure).ResponseInfo.CustomHeaders.AddValue('Connection', 'Upgrade');
  PServerStructure(State._InternalServerStructure).ResponseInfo.CustomHeaders.AddValue('Sec-WebSocket-Accept', SignStr);
  PServerStructure(State._InternalServerStructure).ResponseInfo.WriteHeader;
  PServerStructure(State._InternalServerStructure).WebSocketsReceiveProc := ReceiveProc;
  Result := WriteToWebSocket;
  var Binding := PServerStructure(State._InternalServerStructure).Context.Binding;
  TdwlLogger.Log('Rq '+Binding.PeerIP+':'+Binding.Port.ToString+' '+dwlhttpCommandToString[State.Command]+' '+State.URI+' '+State.StatusCode.ToString+' (websocket opened)', lsTrace);
end;

procedure TdwlHTTPServer.HTTPServerCommand(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
begin
  if OnlyLocalConnections then
  begin
    if (AContext.Binding.PeerIP<>'127.0.0.1') then
    begin
      AResponseInfo.ResponseNo := HTTP_STATUS_FORBIDDEN;
      Exit;
    end;
  end;
  var State: PdwlHTTPHandlingState := AllocMem(SizeOf(TdwlHTTPHandlingState));
  FRequestsInProgressAccess.Enter;
  try
    FRequestsInProgress.Add(AContext, State);
  finally
    FRequestsInProgressAccess.Leave;
  end;
  try
    State._InternalServerStructure := AllocMem(SizeOf(TServerStructure));
    State.Command := TdwlHTTPUtils.StringTodwlhttpCommand(ARequestInfo.Command);
    PServerStructure(State._InternalServerStructure).RequestInfo := ARequestInfo;
    PServerStructure(State._InternalServerStructure).ResponseInfo := AResponseInfo;
    PServerStructure(State._InternalServerStructure).Context := AContext;
    PServerStructure(State._InternalServerStructure).State_URI := ARequestInfo.URI;
    TDWLServer(FServer).ProcessRequest(State);
  except
    on E: Exception do
    begin
      TdwlLogger.Log('Exception while handling '+State.URI+': '+E.Message, lsError);
      State.StatusCode := HTTP_STATUS_SERVER_ERROR;
      State.SetContentText('<!DOCTYPE html><html lang=""><head><title>Internal server error</title></head><body>Internal server error</body></html>');
    end;
  end;
  if Assigned(PServerStructure(State._InternalServerStructure).WebSocketsReceiveProc) then
  begin
    while true {ToDo: server is active ?} do
    begin
      var Data: TIdBytes := nil;
      AContext.Connection.IOHandler.ReadBytes(Data, 6, True);
      if (Data = nil) or (Data[0] = $88) then
        Break;
      var DataSize := 0;
      case Data[1] and $7F of
        126: DataSize := AContext.Connection.IOHandler.ReadUInt16;
        127: DataSize := AContext.Connection.IOHandler.ReadUInt64;
      end;
      if DataSize=0 then
        Break;
      Data := nil;
      AContext.Connection.IOHandler.ReadBytes(Data, DataSize);
      PServerStructure(State._InternalServerStructure).WebSocketsReceiveProc(State, Data, DataSize, Data[0]=$81);
    end;
    // feed zero bytes to signal we're finished!
    PServerStructure(State._InternalServerStructure).WebSocketsReceiveProc(State, nil, 0, false);
    WriteToWebSocket(State, nil, 0, false);
  end
  else
  begin
    AResponseInfo.ResponseNo := State.StatusCode;
    AResponseInfo.ContentStream := TdwlReadOnlyBufferStream.Create(PServerStructure(State._InternalServerStructure).ContentBuffer, PServerStructure(State._InternalServerStructure).ContentLength);
  end;
end;

procedure TdwlHTTPServer.Open;
begin
  if FSSL_CertificateFileName<>'' then
  begin
    var HandleSSL := TIdServerIOHandlerSSLOpenSSL.Create(FHTTPServer);
    HandleSSL.SSLOptions.CertFile := FSSL_CertificateFileName;
    HandleSSL.SSLOptions.RootCertFile := FSSL_RootCertificateFileName;
    HandleSSL.SSLOptions.KeyFile := FSSL_PrivateKeyFileName;
    HandleSSL.SSLOptions.SSLVersions := [sslvTLSv1, sslvTLSv1_1, sslvTLSv1_2];
    FHTTPServer.IOHandler := HandleSSL;
  end;
  FHTTPServer.OnQuerySSLPort := QuerySSLPort;
  FHTTPServer.Active := true;
end;

procedure TdwlHTTPServer.QuerySSLPort(APort: Word; var VUseSSL: boolean);
begin
  VUseSSL := FSSL_HostName<>'';
end;

procedure AssignServerProcs;
begin
  serverProcs.ArrangeContentBufferProc := State_ArrangeContentBuffer;
  serverProcs.GetRequestParamProc := State_GetRequestParam;
  serverProcs.GetHeaderValueProc := State_GetHeaderValue;
  serverProcs.GetPayloadPtrProc := State_GetPostDataPtr;
  serverProcs.SetHeaderValueProc := State_SetHeaderValue;
  serverProcs.ActivateWebSocketproc := State_ActivateWebSocket;
end;

end.
