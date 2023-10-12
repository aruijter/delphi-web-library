unit DWL.TCP.Server;

interface

uses
  DWL.TCP,  Winapi.WinSock, Winapi.Winsock2, System.Classes,
  System.Generics.Collections;

type
  TdwlTCPServer=class;
  TdwlServerBinding=class;
  TdwlServerBindings=class;

  TdwlServerBinding = class
  strict private
    FIp: string;
    FPort: word;
    FListenSocket: TSocket;
    FListenIndex: byte;
    FIoHandler: IdwlTCPIoHandler;
  private
    FBindings: TdwlServerBindings;
    procedure CreateAcceptSocket;
    procedure StartListening(ListenIndex: byte);
    procedure StopListening;
  public
    property Ip: string read FIp;
    property Port: word read FPort;
    constructor Create(ABindings: TdwlServerBindings; const AIp: string; APort: word; IOHandler: IdwlTCPIoHandler);
  end;

  TdwlServerBindings = class
  strict private
  private
    FBindings: TObjectList<TdwlServerBinding>;
    FServer: TdwlTCPServer;
    function GetBindings(Index: integer): TdwlServerBinding;
  public
    property Bindings[Index: integer]: TdwlServerBinding read GetBindings; default;
    constructor Create(AServer: TdwlTCPServer);
    destructor Destroy; override;
    function Add(const AIP: string; APort: word; IOHandler: IdwlTCPIoHandler): TdwlServerBinding;
    procedure Clear;
    procedure StartListening;
    procedure StopListening;
  end;

  TdwlTCPServer = class(TdwlTCPService)
  strict private
    FBindings: TdwlServerBindings;
  private
    FSocketClass: TdwlSocketClass;
    FAcceptIoCompletionPort: THandle;
    FAcceptIoThread: TThread;
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
  DWL.Logging, Winapi.Windows, System.SysUtils;

const
  ACCEPT_SOCKET_COUNT = 3;

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
  FBindings := TdwlServerBindings.Create(Self);
end;

destructor TdwlTCPServer.Destroy;
begin
  Active := false; // needed for DeActivate actions
  FBindings.Free;
  inherited Destroy; // does a deactivate etc, so irst inherited
end;

procedure TdwlTCPServer.InternalActivate;
begin
  inherited InternalActivate;
  // Create IoCompletionPort
	FAcceptIoCompletionPort := CreateIoCompletionPort(INVALID_HANDLE_VALUE, 0, 0, 0);
  FAcceptIoThread := TAcceptIOThread.Create(Self); // one is really enough, don't worry
  FBindings.StartListening;
end;

procedure TdwlTCPServer.InternalDeActivate;
begin
  FBindings.StopListening;
  // stop running thread
  FAcceptIoThread.Terminate;
  PostQueuedCompletionStatus(FAcceptIoCompletionPort, 0, 0, nil); // to activate the thread
  // wait until IoThread is finshed
  while FAcceptIoThread<>nil do
    Sleep(100);
  // Close IoCompletionPort
  CloseHandle(FAcceptIoCompletionPort);
  inherited InternalDeActivate;
end;

procedure TdwlTCPServer.IoCompleted(TransmitBuffer: PdwlTransmitBuffer; NumberOfBytesTransferred: cardinal);
begin
  // get the address information from the TransmitBuffer
  var LocalAddress: Winapi.WinSock.PSOCKADDR;
  var RemoteAddress: Winapi.WinSock.PSOCKADDR;
  var LocalAddressSize: integer;
  var RemoteAddressSize: integer;
  GetAcceptExSockaddrs(TransmitBuffer.WSABuf.buf, 0, SizeOf(sockaddr_storage), SizeOf(sockaddr_storage),
    LocalAddress, LocalAddressSize, RemoteAddress, RemoteAddressSize);
  var Socket := TransmitBuffer.Socket;
  if not LockSocket(Socket.SocketHandle, Socket) then
    Exit;
  try
    SetSocketAddresses(Socket,
      string(Winapi.Winsock.inet_ntoa(LocalAddress.sin_addr)),
      ntohs(LocalAddress.sin_port),
      string(Winapi.Winsock.inet_ntoa(RemoteAddress.sin_addr)),
      ntohs(RemoteAddress.sin_port));
    // signal the iohandler we accepted the socket
    Socket.IOHandler.SocketOnAccept(Socket);
    // start receiving on the socket
    Socket.StartReceiving;
    // finally release the used buffer
    ReleaseTransmitBuffer(TransmitBuffer);
  finally
    UnLockSocket(Socket);
  end;
end;

{ TdwlServerBindings }

function TdwlServerBindings.Add(const AIP: string; APort: word; IOHandler: IdwlTCPIoHandler): TdwlServerBinding;
begin
  Result := TdwlServerBinding.Create(Self, AIP, APort, IOHandler);
  FBindings.Add(Result);
end;

procedure TdwlServerBindings.Clear;
begin
  FBindings.Clear;
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

function TdwlServerBindings.GetBindings(Index: integer): TdwlServerBinding;
begin
  Result := FBindings[Index];
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

constructor TdwlServerBinding.Create(ABindings: TdwlServerBindings; const AIp: string; APort: word; IOHandler: IdwlTCPIoHandler);
begin
  inherited Create;
  FIp:= AIp;
  FPort := APort;
  FBindings := ABindings;
  FIoHandler := IOHandler;
end;

procedure TdwlServerBinding.CreateAcceptSocket;
begin
  if FListenSocket=INVALID_SOCKET then // not listening, so no accept socket needed
    Exit;
  // we use the writetransmitbuffer for the AcceptEx call
  // if writebuffer is not yet used when closing socket
  // it will be freed by the socket self
  var Socket := FBindings.FServer.FSocketClass.Create(FIoHandler);
  var TransmitBuffer := FIoHandler.Service.AcquireTransmitBuffer(Socket, FListenIndex);
  var BytesReceived: cardinal;
  // we made the choice not to receive the first part of the data in the AcceptEx call
  CheckWSAResult(AcceptEx(FListenSocket, Socket.SocketHandle, TransmitBuffer.WSABuf.buf, 0,
    SizeOf(sockaddr_storage), SizeOf(sockaddr_storage), BytesReceived, POverlapped(TransmitBuffer)), 'AcceptEx');
end;

procedure TdwlServerBinding.StartListening(ListenIndex: byte);
begin
  FListenIndex := ListenIndex;
  FListenSocket := WSASocket(AF_INET, SOCK_STREAM, IPPROTO_TCP, nil, 0, WSA_FLAG_OVERLAPPED);
	if CreateIoCompletionPort(FListenSocket, FBindings.FServer.FAcceptIoCompletionPort, ListenIndex, 0)=0 then
    CheckWSAResult(-1, 'CreateIoCompletionPort');
  if (FListenSocket = INVALID_SOCKET) then
    CheckWSAResult(1, 'WSASocket');
  var sockaddr: TSockAddrIn;
  sockaddr.sin_family := AF_INET;
  if FIp='' then
    sockaddr.sin_addr.s_addr := INADDR_ANY
  else
    sockaddr.sin_addr.s_addr := inet_addr(PAnsiChar(ansistring(FIp)));
  sockaddr.sin_port := htons(FPort);
  CheckWSAResult(bind(FListenSocket, TSockAddr(sockaddr), SizeOf(sockaddr)), 'bind');
  CheckWSAResult(listen(FListenSocket, SOMAXCONN), 'listen');
  for var i := 1 to ACCEPT_SOCKET_COUNT do
    CreateAcceptSocket;
end;

procedure TdwlServerBinding.StopListening;
begin
  // first set listensocket to 0 before closing: otherwise a new accept socket will possibly be created
  var SockToClose := FListenSocket;
  FListenSocket := INVALID_SOCKET;
  closesocket(SockToClose);
end;

{ TAcceptIoThread }

constructor TAcceptIoThread.Create(AServer: TdwlTCPServer);
begin
  FServer := AServer;
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
      var Res := GetQueuedCompletionStatus(FServer.FAcceptIoCompletionPort, NumberOfBytesTransferred, CompletionKey, POverlapped(TransmitBuffer), INFINITE);
      // create a new accept socket
      FServer.Bindings.FBindings[CompletionKey].CreateAcceptSocket;
      if not Res then
        Continue;
      if TransmitBuffer=nil then
        Continue;
      FServer.IoCompleted(TransmitBuffer, NumberOfBytesTransferred);
    except
      on E: Exception do
        TdwlLogger.Log('TAcceptIoThread.Execute error: '+E.Message, lsError);
    end;
  end;
  FServer.FAcceptIoThread := nil;
end;

end.

