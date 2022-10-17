unit DWL.TCP.Server;
// PLEASE NOTE THIS UNIT IS WIP (WORK IN PROGRESS)
interface

uses
  System.Classes, DWL.SyncObjs, Winapi.Winsock2, System.SysUtils;

type
  TdwlTCPServer=class;

  TdwlTCPServerClientThreadClass = class of TdwlTCPServerClientThread;

  TdwlTCPServerClientThread = class(TdwlThread)
  strict private
    FReadBuffer: PByte;
    FServer: TdwlTCPServer;
    Fm_socket: TSocket;
    FClientAddr: TSockAddrIn;
    FSendRequests: TdwlThreadQueue<TBytes>;
  protected
    procedure Execute; override;
    procedure DoRead; virtual;
    procedure DoWrite; virtual;
  public
    constructor Create(Socket: TSocket; ClientAddr: TSockAddrIn; Server: TdwlTCPServer); virtual;
    destructor Destroy; override;
    procedure AddSendRequest(B: TBytes);
  end;

  TdwlTCPServerThread = class(TdwlThread)
  private
    FServer: TdwlTCPServer;
  protected
    procedure Execute; override;
  public
    constructor Create(Server: TdwlTCPServer);
  end;

  TdwlTCPServer = class
  private
    FServerThread: TdwlTCPServerThread;
    FPort: word;
  protected
    FClientThreads: TThreadList;
  public
    property ClientThreads: TThreadList read FClientThreads;
    property Port: word read FPort write FPort;
    constructor Create;
    destructor Destroy; override;
    procedure StartListening;
    procedure StopListening;
  end;

implementation

uses
  System.RTLConsts, Winapi.Windows, DWL.Logging, System.Win.ScktComp,
  System.Types;

const
  READ_BUFFER_SIZE = 65536;

procedure RaiseWinsockError(WinSockErrorNo: integer; const Op: string);
begin
  raise Exception.CreateResFmt(@sWindowsSocketError,
    [SysErrorMessage(WinSockErrorNo), WinSockErrorNo, Op]);
end;

function CheckSocketResult(ResultCode: Integer; const Op: string): Integer;
begin
  if ResultCode <> 0 then
  begin
    Result := WSAGetLastError;
    if (Result<>integer(WSAEWOULDBLOCK)) and (Result<>integer(WSA_IO_PENDING))
      and (Result<>integer(WSA_IO_INCOMPLETE)) then
      RaiseWinSockError(Result, Op);
  end
  else
    Result := 0;
end;

{ TdwlTCPServerThread }

constructor TdwlTCPServerThread.Create(Server: TdwlTCPServer);
begin
  inherited Create;
  FServer := Server;
end;

procedure TdwlTCPServerThread.Execute;
var
  lEvents: array[0..1] of THandle;
begin
  var clientaddrsize := SizeOf(TSockAddrIn);
  var server_socket := WSASocket(AF_INET, SOCK_STREAM, IPPROTO_TCP, nil, 0, WSA_FLAG_OVERLAPPED);
  try
    if (server_socket = INVALID_SOCKET) then
      CheckSocketResult(1, 'WSASocket');
    // zet adresgegegevens
    var clientservice: TSockAddrIn;
    clientservice.sin_family := AF_INET;
    clientService.sin_addr.s_addr := INADDR_ANY;
    clientService.sin_port := htons(FServer.Port);
    CheckSocketResult(bind(server_socket, TSockAddr(clientservice), SizeOf(clientservice)), 'bind');
    CheckSocketResult(listen(server_socket, SOMAXCONN), 'listen');
    lEvents[0] := FWorkToDoEventHandle;
    lEvents[1] := WSACreateEvent; //event for accepting sockets;
    try
      CheckSocketResult(WSAEventSelect(server_socket, lEvents[1], FD_ACCEPT), 'WSAEventSelect');
      while not Terminated do
      begin
        WaitForMultipleObjectsEx(2, @lEvents[0], false, INFINITE, false);
        var NetEvents: TWSANetworkEvents;
        CheckSocketResult(WSAEnumNetworkEvents(server_socket, lEvents[1], NetEvents), 'WSAEnumNetworkEvents');
        if (NetEvents.lNetworkEvents and FD_ACCEPT)>0 then
        begin
          var clientaddr: TSockAddrIn;
          var conn_socket := accept(server_socket, @clientaddr, @clientaddrsize);
          if conn_socket<>INVALID_SOCKET then
            TdwlTCPServerClientThread.Create(conn_socket, clientaddr, FServer);
        end;
      end;
    finally
      WSACloseEvent(lEvents[1]);
    end;
  finally
    CheckSocketResult(closesocket(server_socket), 'closesocket');
  end;
end;

{ TdwlTCPServer }

constructor TdwlTCPServer.Create;
begin
  inherited Create;
  FClientThreads := TThreadList.Create;
end;

destructor TdwlTCPServer.Destroy;
begin
  StopListening;
  FClientThreads.Free;
  inherited Destroy;
end;

procedure TdwlTCPServer.StartListening;
begin
  if FServerThread=nil then
  FServerThread := TdwlTCPServerThread.Create(Self);
end;

procedure TdwlTCPServer.StopListening;
begin
  FreeAndNil(FServerThread);
end;

{ TdwlTCPServerClientThread }

procedure TdwlTCPServerClientThread.AddSendRequest(B: TBytes);
begin
  FSendRequests.Push(B);
  SetEvent(FWorkToDoEventHandle);
end;

constructor TdwlTCPServerClientThread.Create(Socket: TSocket; ClientAddr: TSockAddrIn; Server: TdwlTCPServer);
begin
  Fm_socket := Socket;
  FClientAddr := ClientAddr;
  FServer := Server;
  GetMem(FReadBuffer, READ_BUFFER_SIZE);
  FSendRequests := TdwlThreadQueue<TBytes>.Create;
  inherited Create;
  FreeOnTerminate := true;
end;

destructor TdwlTCPServerClientThread.Destroy;
begin
  FreeMem(FReadBuffer);
  FSendRequests.Free;
  inherited Destroy;
end;

procedure TdwlTCPServerClientThread.DoRead;
begin
  var ReceivedBytesCount: integer;
  repeat
    var Flags: integer := 0;
    ReceivedBytesCount := recv(Fm_socket, FReadBuffer^, READ_BUFFER_SIZE, Flags);
    if ReceivedBytesCount=SOCKET_ERROR then
    begin
      var Err := GetLastError;
      if Err<>WSAEWOULDBLOCK then
        raise Exception.Create(Err.ToString);
    end;
  until ReceivedBytesCount<=0;
end;

procedure TdwlTCPServerClientThread.DoWrite;
begin
  var BytesToSend: TBytes;
  while FSendRequests.TryPop(BytesToSend) do
  begin
    var BytesToDo := Length(BytesToSend);
    var BytesFinished := 0;
    while BytesToDo>0 do
    begin
      var Res := send(Fm_socket, BytesToSend[BytesFinished], BytesToDo, 0);
      if Res=SOCKET_ERROR then
      begin
        var Err := GetLastError;
        if Err<>WSAEWOULDBLOCK then
          raise Exception.Create(Err.ToString)
      end;
      inc(BytesFinished, Res);
      dec(BytesToDo, Res);
    end;
  end;
end;

procedure TdwlTCPServerClientThread.Execute;
var
  lEvents: array[0..1] of THandle;
begin
  try
    FServer.FClientThreads.Add(Self);
    try
      var SocketEvent := WSACreateEvent;
      if SocketEvent=WSA_INVALID_EVENT then
        CheckSocketResult(1, 'WSACreateEvent')
      else
      try
        CheckSocketResult(WSAEventSelect(Fm_socket, SocketEvent, FD_READ or FD_CLOSE), 'WSAEventSelect');
        lEvents[0] := FWorkToDoEventHandle;
        lEvents[1] := SocketEvent;
        while not Terminated do
        begin
          var NetEvents: TWSANetworkEvents;
          if (NetEvents.lNetworkEvents and FD_READ)>0 then
            DoRead;
          DoWrite;
          var Res:DWORD := WaitForMultipleObjects(2, @lEvents, false, INFINITE); // this call resets the FWorkToDoEvent if applicable
          if (Res>WAIT_OBJECT_0+2) then
            raise Exception.Create('Error on WaitForMultipleEvents: '+Res.ToHexString);
          CheckSocketResult(WSAEnumNetworkEvents(Fm_socket, SocketEvent, NetEvents), 'WSAEnumNetworkEvents'); // This should reset the FSocketEvent
          if (NetEvents.lNetworkEvents and FD_CLOSE)>0 then
            Break;
        end;
        // close
        CheckSocketResult(WSAEventSelect(Fm_socket, 0, 0), 'WSAEventSelect');
        CheckSocketResult(shutdown(Fm_socket, SD_BOTH), 'shutdown');
        CheckSocketResult(closesocket(Fm_socket), 'closesocket');
      finally
        WSACloseEvent(SocketEvent);
      end;
    finally
      FServer.FClientThreads.Remove(Self);
    end;
  except
    on E: Exception do
    begin
      if (E.Message<>'10053') and (E.Message<>'10054') then
        TdwlLogger.Log('TCPThread Exception: '+E.Message+#13#10#13#10+E.StackTrace);
    end;
  end;
end;

initialization
  // startup winsock
  var WSAData: TWSAData;
  var ErrorCode := WSAStartup(WINSOCK_VERSION, WSAData);
  if ErrorCode <> 0 then
    raise ESocketError.CreateResFmt(@sWindowsSocketError,
      [SysErrorMessage(ErrorCode), ErrorCode, 'WSAStartup']);

finalization
  // cleanup winsock
  var ErrorCode := WSACleanup;
  if ErrorCode <> 0 then
    raise ESocketError.CreateResFmt(@sWindowsSocketError,
      [SysErrorMessage(ErrorCode), ErrorCode, 'WSACleanup']);




end.
