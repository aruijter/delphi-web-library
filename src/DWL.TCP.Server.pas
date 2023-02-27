unit DWL.TCP.Server;

interface

uses
  DWL.TCP, Winapi.WinSock, Winsock2, System.Classes, System.Generics.Collections,
  DWL.SyncObjs;

type
  TdwlTCPServer=class;
  TdwlServerBinding=class;
  TdwlServerBindings=class;

  TdwlServerBinding = class
  strict private
    FPort: word;
    FListenSocket: TSocket;
    FListenIndex: byte;
  private
    FBindings: TdwlServerBindings;
    procedure CreateAcceptSocket;
    procedure StartListening(ListenIndex: byte);
    procedure StopListening;
  public
    constructor Create(ABindings: TdwlServerBindings; APort: word);
  end;

  TdwlServerBindings = class
  strict private
  private
    FBindings: TObjectList<TdwlServerBinding>;
    FServer: TdwlTCPServer;
  public
    constructor Create(AServer: TdwlTCPServer);
    destructor Destroy; override;
    function Add(APort: word): TdwlServerBinding;
    procedure StartListening;
    procedure StopListening;
  end;

  TdwlTCPServer = class(TdwlTCPService)
  strict private
    FBindings: TdwlServerBindings;
  private
    FSocketClass: TdwlSocketClass;
    FAcceptIoCompletionPort: THandle;
    FAcceptIoThreads: TdwlThreadList<TThread>;
  protected
    procedure InternalActivate; override;
    procedure InternalDeActivate; override;
    procedure IoCompleted(TransmitBuffer: PdwlTransmitBuffer; NumberOfBytesTransferred: cardinal);
  public
    property Bindings: TdwlServerBindings read FBindings;
    constructor Create(SocketClass: TdwlSocketClass);
    destructor Destroy; override;
  end;

implementation

uses
  System.RTLConsts, DWL.Logging, System.Win.ScktComp,
  System.Types, System.Math, Winapi.Windows, System.SysUtils;

const
  ACCEPT_SOCKET_COUNT = 2;

type
  TAcceptIoThread = class(TThread)
  strict private
    FServer: TdwlTCPServer;
  protected
    procedure Execute; override;
  public
    constructor Create(AServer: TdwlTCPServer);
  end;

{ TdwlTCPServer }

constructor TdwlTCPServer.Create(SocketClass: TdwlSocketClass);
begin
  inherited Create;
  FSocketClass := SocketClass;
  FAcceptIoThreads := TdwlThreadList<TThread>.Create;
  FBindings := TdwlServerBindings.Create(Self);
end;

destructor TdwlTCPServer.Destroy;
begin
  Active := false; // needed for my DeActivate actions
  FBindings.Free;
  FAcceptIoThreads.Free;
  inherited Destroy; // does a deactivate etc, so irst inherited
end;

procedure TdwlTCPServer.InternalActivate;
begin
  inherited InternalActivate;
  // Create IoCompletionPort
	FAcceptIoCompletionPort := CreateIoCompletionPort(INVALID_HANDLE_VALUE, 0, 0, 0);
  TAcceptIOThread.Create(Self); // one is really enough, don't worry, but if really wanted create as much as you want...
  FBindings.StartListening;
end;

procedure TdwlTCPServer.InternalDeActivate;
begin
  FBindings.StopListening;
  // stop running threads and wait for them
  var RunningThreads := FAcceptIoThreads.LockList;
  try
    // stop IoThreads
    for var Thread in RunningThreads do
      Thread.Terminate;  //FreeOnTerminate
    // to be sure to wake up all threads post threadcount queuedcompletionstatus
    for var i := 1 to  RunningThreads.Count do
      PostQueuedCompletionStatus(FAcceptIoCompletionPort, 0, 0, nil);
  finally
    FAcceptIoThreads.UnlockList;
  end;
  // wait untill all IoThreads are finshed
  while FAcceptIoThreads.Count>0 do
    Sleep(100);
  // Close IoCompletionPort
  CloseHandle(FAcceptIoCompletionPort);
  inherited InternalDeActivate;
end;

procedure TdwlTCPServer.IoCompleted(TransmitBuffer: PdwlTransmitBuffer; NumberOfBytesTransferred: cardinal);
begin
  // create a new accept socket
  FBindings.FBindings[TransmitBuffer.CompletionIndicator].CreateAcceptSocket;
  IOHandler.SocketOnAccept(TransmitBuffer.Socket);
  // start receiving on the socket
  TransmitBuffer.Socket.StartReceiving;
  ReleaseTransmitBuffer(TransmitBuffer);
end;

{ TdwlServerBindings }

function TdwlServerBindings.Add(APort: word): TdwlServerBinding;
begin
  Result := TdwlServerBinding.Create(Self, APort);
  FBindings.Add(Result);
end;

constructor TdwlServerBindings.Create;
begin
  inherited Create;
  FBindings := TObjectList<TdwlServerBinding>.Create(true);
  FServer := AServer;
end;

destructor TdwlServerBindings.Destroy;
begin
  FBindings.Free;
  inherited Destroy;
end;

procedure TdwlServerBindings.StartListening;
begin
  for var i := 0 to FBindings.Count-1 do
    FBindings[i].StartListening(i);
end;

procedure TdwlServerBindings.StopListening;
begin
  for var Binding in FBindings do
    Binding.StopListening;
end;

{ TdwlServerBinding }

constructor TdwlServerBinding.Create(ABindings: TdwlServerBindings; APort: word);
begin
  inherited Create;
  FPort := APort;
  FBindings := ABindings;
end;

procedure TdwlServerBinding.CreateAcceptSocket;
begin
  // we use the writetransmitbuffer for the AcceptEx call
  // if writebuffer is not yet used when closing socket
  // it will be freed by the socket self
  var Socket := FBindings.FServer.FSocketClass.Create(FBindings.FServer);
  var TransmitBuffer := Socket.Service.AcquireTransmitBuffer(Socket, FListenIndex);
  var BytesReceived: cardinal;
  // we made the choice not to receive the first part of the data in the AcceptEx call
  CheckWSAResult(AcceptEx(FListenSocket, Socket.SocketHandle, TransmitBuffer.WSABuf.buf, 0,
    SizeOf(sockaddr_storage), SizeOf(sockaddr_storage), BytesReceived, POverlapped(TransmitBuffer)), 'AcceptEx');
end;

procedure TdwlServerBinding.StartListening(ListenIndex: byte);
begin
  FListenIndex := ListenIndex;
  FListenSocket := WSASocket(AF_INET, SOCK_STREAM, IPPROTO_TCP, nil, 0, WSA_FLAG_OVERLAPPED);
	if CreateIoCompletionPort(FListenSocket, FBindings.FServer.FAcceptIoCompletionPort, 0, 0)=0 then
    CheckWSAResult(-1, 'CreateIoCompletionPort');
  if (FListenSocket = INVALID_SOCKET) then
    CheckWSAResult(1, 'WSASocket');
  var sockaddr: TSockAddrIn;
  sockaddr.sin_family := AF_INET;
  sockaddr.sin_addr.s_addr := htonl(INADDR_ANY);
  sockaddr.sin_port := htons(FPort);
  CheckWSAResult(bind(FListenSocket, TSockAddr(sockaddr), SizeOf(sockaddr)), 'bind');
  CheckWSAResult(listen(FListenSocket, SOMAXCONN), 'listen');
  for var i := 1 to ACCEPT_SOCKET_COUNT do
    CreateAcceptSocket;
end;

procedure TdwlServerBinding.StopListening;
begin
  shutdown(FListenSocket, SD_BOTH);
  closesocket(FListenSocket);
end;

{ TAcceptIoThread }

constructor TAcceptIoThread.Create(AServer: TdwlTCPServer);
begin
  FServer := AServer;
  FServer.FAcceptIoThreads.Add(Self);
  FreeOnTerminate := true;
  inherited Create;
end;

procedure TAcceptIoThread.Execute;
begin
  while not Terminated do
  begin
    try
      var NumberOfBytesTransferred: cardinal;
      var CompletionKey: NativeUInt;
      var TransmitBuffer: PdwlTransmitBuffer;
      if not GetQueuedCompletionStatus(FServer.FAcceptIoCompletionPort, NumberOfBytesTransferred, CompletionKey, POverlapped(TransmitBuffer), INFINITE) then
        Continue;
      if TransmitBuffer=nil then
        Continue;
      FServer.IoCompleted(TransmitBuffer, NumberOfBytesTransferred);
    except
      on E: Exception do
        TdwlLogger.Log('TAcceptIoThread.Execute error: '+E.Message, lsError);
    end;
  end;
  FServer.FAcceptIoThreads.Remove(Self);
end;

end.

