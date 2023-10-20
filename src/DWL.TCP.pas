unit DWL.TCP;

interface

uses
  Winapi.Windows, Winapi.Winsock2, System.SyncObjs, System.Generics.Collections,
  DWL.SyncObjs, System.Classes;

const
  COMPLETIONINDICATOR_READ = 1;
  COMPLETIONINDICATOR_WRITE = 2;
  DWL_TCP_BUFFER_SIZE = 16384;

  type
  TdwlTCPService=class;
  TdwlSocket=class;

  PdwlHandlingBuffer = ^TdwlHandlingBuffer;
  TdwlHandlingBuffer = record
    Socket: TdwlSocket;
    case integer of
     0: (NumberOfBytes: cardinal;
         Buf: PByte);
     1: (WSABuf: TWsaBuf);
  end;

  PdwlTransmitBuffer = ^TdwlTransmitBuffer;
  TdwlTransmitBuffer = record {build with HandlingBuffer as a subpart, please leave it this way}
    Overlapped: TOverlapped;  // must be the first of the structure to be able to hand over to winsock functions as POverlapped or LPWSAOVERLAPPED
    CompletionIndicator: byte;
    case integer of
    0: (Socket: TdwlSocket;
        WSABuf: TWsaBuf);
    1: (HandlingBuffer: TdwlHandlingBuffer);
  end;

  IdwlTCPIoHandler = interface
    ['{B4B4EB8D-4905-4E1F-BD64-DC28995E85FF}']
    function Service: TdwlTCPService;
    function SizeOfSocketIoVars: cardinal;
    procedure SocketAfterConstruction(Socket: TdwlSocket);
    procedure SocketBeforeDestruction(Socket: TdwlSocket);
    procedure SocketOnAccept(Socket: TdwlSocket);
    function SocketHandleReceive(var TransmitBuffer: PdwlTransmitBuffer): boolean;
    function SocketHandleWrite(var HandlingBuffer: PdwlHandlingBuffer): boolean;
  end;

  TdwlSocketClass = class of TdwlSocket;
  TdwlSocket = class
  strict private
    FIoHandler: IdwlTCPIoHandler;
    FSocketVars: pointer;
    FSocketHandle: TSocket;
    FWriteBuffer: PdwlHandlingBuffer;
    FWritePos: PByte;
    FWriteBufLeft: cardinal;
    FCloseConnection: boolean;
    FContext_HostName: string;
    FWritesInProgress: cardinal;
    procedure CreateRecvRequest;
    procedure HandleCurrentWriteBuffer;
    function CheckWSAResult_ShutdownOnError(ResultCode: Integer; const LogErrorWithThisString: string=''): integer;
  private
    FService: TdwlTCPService;
    FSocketCS: TCriticalSection;
    FLastIoTick: UInt64;
    FShutdownTick: UInt64;
    FTransmitBuffers: TList<PdwlTransmitBuffer>;
    FHandlingBuffers: TList<PdwlHandlingBuffer>;
    FIp_Local: string;
    FIp_Remote: string;
    FPort_Local: word;
    FPort_Remote: word;
    procedure IoCompleted(TransmitBuffer: PdwlTransmitBuffer; NumberOfBytesTransferred: cardinal);
  protected
    class var FCodePage_US_ASCII: integer;
    procedure CreateWriteBuffer;
  public
    property Ip_Local: string read FIp_Local;
    property Ip_Remote: string read FIp_Remote;
    property Port_Local: word read FPort_Local;
    property Port_Remote: word read FPort_Remote;
    property Service: TdwlTCPService read FService;
    property SocketHandle: TSocket read FSocketHandle;
    property SocketVars: pointer read FSocketVars;
    property Context_HostName: string read FContext_HostName write FContext_HostName;
    property IOHandler: IdwlTCPIoHandler read FIoHandler;
    class constructor Create;
    constructor Create(IOHandler: IdwlTcpIOHandler); virtual;
    destructor Destroy; override;
    procedure ReadHandlingBuffer(HandlingBuffer: PdwlHandlingBuffer); virtual;
    procedure FlushWrites(CloseConnection: boolean=false);
    procedure SendTransmitBuffer(TransmitBuffer: PdwlTransmitBuffer);
    procedure Shutdown;
    procedure ShutdownDetected;
    procedure StartReceiving;
    procedure WriteBuf(Buf: PByte; Size: integer);
    procedure WriteLine(const Str: string='');
    procedure WriteStr(const Str: string);
    procedure WriteUInt8(B: byte);
  end;

  TdwlTCPService = class
  strict private
    FActive: boolean;
    procedure SetActive(const Value: boolean);
  private
    FSocketStorage: TObject;
    FIoCompletionPort: THandle;
    FIoThreads: TdwlThreadList<TThread>;
    FCheckThread: TdwlThread;
  protected
    procedure InternalActivate; virtual;
    procedure InternalDeActivate; virtual;
    procedure SetSocketAddresses(Socket: TdwlSocket; const Ip_Local: string; Port_Local: word; const Ip_Remote: string; Port_Remote: word);
  public
    property Active: boolean read FActive write SetActive;
    constructor Create;
    destructor Destroy; override;
    function AcquireHandlingBuffer(Socket: TdwlSocket): PdwlHandlingBuffer;
    function AcquireTransmitBuffer(Socket: TdwlSocket; Completionindicator: byte): PdwlTransmitBuffer; overload;
    function AcquireTransmitBuffer(HandlingBuffer: PdwlHandlingBuffer; Completionindicator: byte): PdwlTransmitBuffer; overload;
    procedure ReleaseHandlingBuffer(HandlingBuffer: PdwlHandlingBuffer);
    procedure ReleaseTransmitBuffer(TransmitBuffer: PdwlTransmitBuffer);
    function LockSocket(Handle: TSocket; var Socket: TdwlSocket): boolean;
    procedure UnLockSocket(Socket: TdwlSocket);
  end;

  TdwlBaseIoHandler = class(TInterfacedObject)
  strict private
    FService: TdwlTCPService;
  protected
    function Service: TdwlTCPService;
  public
    constructor Create(AService: TdwlTCPService);
  end;

  TdwlPlainIoHandler = class(TdwlBaseIoHandler, IdwlTCPIoHandler)
  strict private
    function SizeOfSocketIoVars: cardinal;
    procedure SocketAfterConstruction(Socket: TdwlSocket);
    procedure SocketBeforeDestruction(Socket: TdwlSocket);
    procedure SocketOnAccept(Socket: TdwlSocket);
    function SocketHandleReceive(var TransmitBuffer: PdwlTransmitBuffer): boolean;
    function SocketHandleWrite(var HandlingBuffer: PdwlHandlingBuffer): boolean;
  end;

function CheckWSAResult(ResultCode: Integer; const LogErrorWithThisString: string=''): Integer; overload;
function CheckWSAResult(ResultBool: boolean; const LogErrorWithThisString: string=''): Integer; overload;
function WSARecv2(s: TSocket; lpBuffers: LPWSABUF; dwBufferCount: DWORD;
  lpNumberOfBytesRecvd: PCardinal; var lpFlags: DWORD; lpOverlapped: LPWSAOVERLAPPED;
  lpCompletionRoutine: LPWSAOVERLAPPED_COMPLETION_ROUTINE): Integer; stdcall;
function WSASend2(s: TSocket; lpBuffers: LPWSABUF; dwBufferCount: DWORD;
  lpNumberOfBytesSent: PCardinal; dwFlags: DWORD; lpOverlapped: LPWSAOVERLAPPED;
  lpCompletionRoutine: LPWSAOVERLAPPED_COMPLETION_ROUTINE): Integer; stdcall;

implementation

uses
  DWL.Logging, DWL.OS, System.Math, System.SysUtils;

  // We have slightly modified the definitions from Winapi.Winsock2: NumberOfBytes now can be passed as nil (pointer instead of var)
function WSARecv2; external 'ws2_32.dll' name 'WSARecv';
function WSASend2; external 'ws2_32.dll' name 'WSASend';

const
  DELETE_DELAY_MSECS = 500;
  TIMEOUT_MSECS = 1000*60*5; // 5 min
  TIMEOUT_CHECK_MSECS = TIMEOUT_MSECS div 3;

function CheckWSAResult(ResultCode: Integer; const LogErrorWithThisString: string=''): Integer;
begin
  if ResultCode<>0 then
  begin
    Result := WSAGetLastError;
    if (LogErrorWithThisString<>'') and (Result<>WSAEWOULDBLOCK) and (Result<>integer(WSA_IO_PENDING)) and (Result<>integer(WSA_IO_INCOMPLETE)) then
      TdwlLogger.Log('Winsock error: '+SysErrorMessage(Result)+' ('+Result.ToString+') in '+LogErrorWithThisString, lsError);
  end
  else
    Result := 0;
end;

function CheckWSAResult(ResultBool: boolean; const LogErrorWithThisString: string): Integer;
begin
  if not ResultBool then
    Result := CheckWSAResult(-1, LogErrorWithThisString)
  else
    Result := 0;
end;

type
  TIoThread = class(TThread)
  strict private
    FService: TdwlTCPService;
  protected
    procedure Execute; override;
  public
    constructor Create(AService: TdwlTCPService);
  end;

  TCheckThread = class(TdwlThread)
  strict private
    FService: TdwlTCPService;
    FNextTimeOutCheck: UInt64;
  protected
    procedure Execute; override;
  public
    constructor Create(AService: TdwlTCPService);
  end;

  TdwlSocketStorage = class
  strict private
    FListAccess: TCriticalSection;
    FSockets: TDictionary<TSocket, TdwlSocket>;
    FSockets2Delete: TList<TdwlSocket>;
  private
    procedure DoDeleteCheck;
    procedure DoTimeOutCheck;
    procedure RegisterNewSocket(Socket: TdwlSocket);
    function LockSocket(Handle: TSocket; var Socket: TdwlSocket): boolean;
    procedure UnLockSocket(Socket: TdwlSocket);
  public
    constructor Create;
    destructor Destroy; override;
  end;

{ TdwlSocket }

procedure TdwlSocket.HandleCurrentWriteBuffer;
begin
  if not FIoHandler.SocketHandleWrite(FWriteBuffer) then
    Shutdown;
  if FWriteBuffer<>nil then
    FService.ReleaseHandlingBuffer(FWriteBuffer);
end;

procedure TdwlSocket.SendTransmitBuffer(TransmitBuffer: PdwlTransmitBuffer);
begin
  var Res := CheckWSAResult_ShutdownOnError(WSASend2(SocketHandle, @TransmitBuffer.WSABuf, 1, nil, 0, LPWSAOVERLAPPED(TransmitBuffer), nil));
  if (Res<>0) and (Res<>WSAECONNRESET) and (Res<>WSAECONNABORTED) and (Res<>WSAEWOULDBLOCK) and (Res<>integer(WSA_IO_PENDING)) and (Res<>integer(WSA_IO_INCOMPLETE)) then
    TdwlLogger.Log('Winsock error: '+SysErrorMessage(Res)+' ('+Res.ToString+') in SendTransmitBuffer', lsError);
  inc(FWritesInProgress);
end;

procedure TdwlSocket.Shutdown;
begin
  Winapi.Winsock2.shutdown(FSocketHandle, SD_BOTH);
end;

procedure TdwlSocket.ShutdownDetected;
begin
  if FShutdownTick=0 then
    FShutdownTick := GetTickCount64;
end;

procedure TdwlSocket.StartReceiving;
begin
  FLastIoTick := GetTickCount64;
  CreateRecvRequest;
end;

procedure TdwlSocket.WriteBuf(Buf: PByte; Size: integer);
begin
  var BytesToWrite := min(Size, FWriteBufLeft);
  while BytesToWrite>0 do
  begin
    Move(Buf^, FWritePos^, BytesToWrite);
    dec(FWriteBufLeft, BytesToWrite);
    if FWriteBufLeft>0 then
    begin
      inc(FWritePos, BytesToWrite);
      Break;
    end;
    // buffer full: send out:
    HandleCurrentWriteBuffer;
    CreateWriteBuffer;
    // Decrease Size and calculate new BytesToWrite
    inc(Buf, BytesToWrite);
    Size := Size - BytesToWrite;
    BytesToWrite := min(Size, FWriteBufLeft);
  end;
end;

procedure TdwlSocket.WriteLine(const Str: string='');
begin
  WriteStr(Str);
  WriteUInt8(13);
  WriteUInt8(10);
end;

procedure TdwlSocket.WriteStr(const Str: string);
begin
  var StrAnsi: ansistring;
  var Len := Str.Length;
  SetLength(StrAnsi, Len);
  SetLength(StrAnsi, WideCharToMultiByte(FCodePage_US_ASCII, 0, PWideChar(Str), Len, PAnsiChar(StrAnsi), Len, nil, nil));
  WriteBuf(PByte(PAnsiChar(StrAnsi)), Len);
end;

procedure TdwlSocket.WriteUInt8(B: byte);
begin
  WriteBuf(@B, 1);
end;

function TdwlSocket.CheckWSAResult_ShutdownOnError(ResultCode: Integer; const LogErrorWithThisString: string=''): integer;
begin
  Result := CheckWSAResult(ResultCode, LogErrorWithThisString);
  if (Result<>0) and (Result<>WSA_IO_PENDING) then
    ShutdownDetected;
end;

constructor TdwlSocket.Create(IOHandler: IdwlTcpIOHandler);
begin
  inherited Create;
  FTransmitBuffers := TList<PdwlTransmitBuffer>.Create;
  FHandlingBuffers := TList<PdwlHandlingBuffer>.Create;
  FIoHandler := IOHandler;
  FService := FIoHandler.Service;
  FSocketCS := TCriticalSection.Create;
  FSocketHandle := WSASocket(AF_INET, SOCK_STREAM, IPPROTO_TCP, nil, 0, WSA_FLAG_OVERLAPPED);
  var SocketVarsSize := FIOHandler.SizeOfSocketIoVars;
  if SocketVarsSize>0 then
    GetMem(FSocketVars, SocketVarsSize);
  FIOHandler.SocketAfterConstruction(Self);
  TdwlSocketStorage(FService.FSocketStorage).RegisterNewSocket(Self);
  //and attach to IoCompletionPort
	if CreateIoCompletionPort(SocketHandle, FService.FIoCompletionPort, SocketHandle, 0)=0 then
    CheckWSAResult_ShutdownOnError(-1, 'CreateIoCompletionPort');
  CreateWriteBuffer;
end;

class constructor TdwlSocket.Create;
begin
  // get the available most basic codepage
  var Dummy: TCPInfo;
  if GetCPInfo(20127, Dummy) then
    FCodePage_US_ASCII := 20127
  else
    FCodePage_US_ASCII := 437;
end;

procedure TdwlSocket.CreateRecvRequest;
begin
  var TransmitBuffer := FService.AcquireTransmitBuffer(Self, COMPLETIONINDICATOR_READ);
  var EmptyFlags: cardinal := 0;
  var Res:= CheckWSAResult_ShutdownOnError(WSARecv2(SocketHandle, @TransmitBuffer.WSABuf, 1, nil, EmptyFlags, LPWSAOVERLAPPED(TransmitBuffer), nil));
  if (Res<>0) and (Res<>WSAECONNRESET) and (Res<>WSAECONNABORTED) and (Res<>WSAEWOULDBLOCK) and (Res<>integer(WSA_IO_PENDING)) and (Res<>integer(WSA_IO_INCOMPLETE)) then
    TdwlLogger.Log('Winsock error: '+SysErrorMessage(Res)+' ('+Res.ToString+') in CreateRecvRequest', lsError);
end;

procedure TdwlSocket.CreateWriteBuffer;
begin
  FWriteBuffer := FService.AcquireHandlingBuffer(Self);
  FWritePos := FWriteBuffer.Buf;
  FWriteBufLeft := FWriteBuffer.NumberOfBytes;
end;

destructor TdwlSocket.Destroy;
begin
  FIOHandler.SocketBeforeDestruction(Self);
  if FSocketVars<>nil then
    FreeMem(FSocketVars);
  closesocket(FSocketHandle);
  // release buffers never sent to or returned from IoCompletion
  while FTransmitBuffers.Count>0 do
    FService.ReleaseTransmitBuffer(FTransmitBuffers[0]);
  FTransmitBuffers.Free;
  while FHandlingBuffers.Count>0  do
    FService.ReleaseHandlingBuffer(FHandlingBuffers[0]);
  FHandlingBuffers.Free;
  FSocketCS.Free;
  inherited Destroy;
end;

procedure TdwlSocket.ReadHandlingBuffer(HandlingBuffer: PdwlHandlingBuffer);
begin
  // base socket does not need to read ;-)
end;

procedure TdwlSocket.FlushWrites(CloseConnection: boolean=false);
begin
  FCloseConnection := CloseConnection;
  if FWriteBufLeft<DWL_TCP_BUFFER_SIZE then
  begin
    FWriteBuffer.NumberOfBytes := DWL_TCP_BUFFER_SIZE-FWriteBufLeft;
    HandleCurrentWriteBuffer;
    if not CloseConnection then
      CreateWriteBuffer
    else
      FWriteBuffer := nil; // do not create a new one, we're finished
  end
  else
  begin
    if CloseConnection then
    begin
      // post a completion status for shutdown of socket (this is needed because order of iocompletion is not always as expected)
      inc(FWritesInProgress);
      var TransmitBuffer := FService.AcquireTransmitBuffer(Self, COMPLETIONINDICATOR_WRITE);
      PostQueuedCompletionStatus(FService.FIoCompletionPort, 0, SocketHandle, POverlapped(TransmitBuffer));
      FWriteBuffer := nil; // do not create a new one, we're finished
    end;
  end;
end;

procedure TdwlSocket.IoCompleted(TransmitBuffer: PdwlTransmitBuffer; NumberOfBytesTransferred: cardinal);
begin
  FLastIoTick := GetTickCount64;
  case TransmitBuffer.CompletionIndicator of
  COMPLETIONINDICATOR_READ:
    begin
      // set WSAbuf.len to actual used length
      TransmitBuffer.WSABuf.len := NumberOfBytesTransferred;
      // handle the finished request;
      if not FIOHandler.SocketHandleReceive(TransmitBuffer) then
        Shutdown;
      CreateRecvRequest; // create a new receive request
    end;
  COMPLETIONINDICATOR_WRITE:
      dec(FWritesInProgress);
  end;
  if FCloseConnection and (FWritesInProgress=0) then
    Shutdown;
end;

{ TdwlTCPService }

function TdwlTCPService.AcquireTransmitBuffer(Socket: TdwlSocket; CompletionIndicator: byte): PdwlTransmitBuffer;
begin
  GetMem(Result, Sizeof(TdwlTransmitBuffer));
  Getmem(Result.WSABuf.buf, DWL_TCP_BUFFER_SIZE);
  Result.Socket := Socket;
  Result.WSABuf.len := DWL_TCP_BUFFER_SIZE;
  ZeroMemory(@Result.Overlapped, SizeOf(TOverlapped));
  Result.CompletionIndicator := CompletionIndicator;
  Socket.FTransmitBuffers.Add(Result);
end;

function TdwlTCPService.AcquireTransmitBuffer(HandlingBuffer: PdwlHandlingBuffer; Completionindicator: byte): PdwlTransmitBuffer;
begin
  GetMem(Result, Sizeof(TdwlTransmitBuffer));
  Move(HandlingBuffer.Socket, Result.Socket, Sizeof(TdwlSocket)+SizeOf(TWsaBuf));
  Result.Socket.FHandlingBuffers.Remove(HandlingBuffer);
  FreeMem(HandlingBuffer);
  ZeroMemory(@Result.Overlapped, SizeOf(TOverlapped));
  Result.CompletionIndicator := CompletionIndicator;
  Result.Socket.FTransmitBuffers.Add(Result);
end;

constructor TdwlTCPService.Create;
begin
  inherited Create;
  FIoThreads := TdwlThreadList<TThread>.Create;
  // startup winsock
  var WSAData: TWSAData;
  CheckWSAResult(WSAStartup(WINSOCK_VERSION, WSAData), 'WSAStartup');
end;

destructor TdwlTCPService.Destroy;
begin
  Active := false;
  // cleanup winsock
  CheckWSAResult(WSACleanup, 'WSACleanup');
  FIoThreads.Free;
  inherited Destroy;
end;

procedure TdwlTCPService.InternalActivate;
begin
  FSocketStorage := TdwlSocketStorage.Create;
  // Create IoCompletionPort
  var NumberOfThreads := Min(3, TdwlOS.NumberOfLogicalProcessors);
	FIoCompletionPort := CreateIoCompletionPort(INVALID_HANDLE_VALUE, 0, 0, NumberOfThreads);
  // Create IoThreads
  for var i := 1 to NumberOfThreads do
    TIOThread.Create(Self);
  FCheckThread := TCheckThread.Create(Self);
  FCheckThread.FreeOnTerminate := true;
end;

procedure TdwlTCPService.InternalDeActivate;
begin
  // stop cleanup thread
  FCheckThread.Terminate;  // It's Free on terminate
  // stop running threads and wait for them
  var RunningThreads := FIoThreads.LockList;
  try
    // stop IoThreads
    for var Thread in RunningThreads do
      Thread.Terminate;  //FreeOnTerminate
    // to be sure to wake up all threads post threadcount queuedcompletionstatus
    for var i := 1 to  RunningThreads.Count do
      PostQueuedCompletionStatus(FIoCompletionPort, 0, 0, nil);
  finally
    FIoThreads.UnlockList;
  end;
  // wait untill all IoThreads are finshed
  while FIoThreads.Count>0 do
    Sleep(100);
  FSocketStorage.Free;
  // Close IoCompletionPort
  CloseHandle(FIoCompletionPort);
end;

function TdwlTCPService.LockSocket(Handle: TSocket; var Socket: TdwlSocket): boolean;
begin
  Result := TdwlSocketStorage(FSocketStorage).LockSocket(Handle, Socket);
end;

procedure TdwlTCPService.ReleaseHandlingBuffer(HandlingBuffer: PdwlHandlingBuffer);
begin
  HandlingBuffer.Socket.FHandlingBuffers.Remove(HandlingBuffer);
  FreeMem(HandlingBuffer.Buf);
  FreeMem(HandlingBuffer);
end;

procedure TdwlTCPService.ReleaseTransmitBuffer(TransmitBuffer: PdwlTransmitBuffer);
begin
  Assert(TransmitBuffer.Socket.FTransmitBuffers.Contains(TransmitBuffer));
  TransmitBuffer.Socket.FTransmitBuffers.Remove(TransmitBuffer);
  FreeMem(TransmitBuffer.WSABuf.buf);
  FreeMem(TransmitBuffer);
end;

function TdwlTCPService.AcquireHandlingBuffer(Socket: TdwlSocket): PdwlHandlingBuffer;
begin
  GetMem(Result, Sizeof(TdwlHandlingBuffer));
  Getmem(Result.Buf, DWL_TCP_BUFFER_SIZE);
  Result.NumberOfBytes := DWL_TCP_BUFFER_SIZE;
  Result.Socket := Socket;
  Socket.FHandlingBuffers.Add(Result);
end;

procedure TdwlTCPService.SetActive(const Value: boolean);
begin
  if FActive=Value then
    Exit;
  if Value then
    InternalActivate
  else
    InternalDeActivate;
  FActive := Value;
end;

procedure TdwlTCPService.SetSocketAddresses(Socket: TdwlSocket; const IP_Local: string; Port_Local: word; const IP_Remote: string; Port_Remote: word);
begin
  Socket.FIp_Local := Ip_Local;
  Socket.FPort_Local := Port_Local;
  Socket.FIp_Remote := Ip_Remote;
  Socket.FPort_Remote := Port_Remote;
end;

procedure TdwlTCPService.UnLockSocket(Socket: TdwlSocket);
begin
  TdwlSocketStorage(FSocketStorage).UnLockSocket(Socket);
end;

{ TIoThread }

constructor TIoThread.Create(AService: TdwlTCPService);
begin
  FService := AService;
  FService.FIoThreads.Add(Self);
  FreeOnTerminate := true;
  inherited Create;
end;

procedure TIoThread.Execute;
begin
  while not Terminated do
  begin
    try
      var NumberOfBytesTransferred: cardinal;
      var SocketHandle: THandle;
      var TransmitBuffer: PdwlTransmitBuffer;
      // GetQueueCompletionsStatus never times out so no need to check result
      if not GetQueuedCompletionStatus(FService.FIoCompletionPort, NumberOfBytesTransferred, SocketHandle, POverlapped(TransmitBuffer), INFINITE) then
        Continue;
      if TransmitBuffer=nil then  // my own post to 'activate' the IOThread on termination
        Continue;
      var Socket: TdwlSocket;
      if not FService.LockSocket(SocketHandle, Socket) then
        Continue;
      try
        Assert(TransmitBuffer.Socket.SocketHandle=SocketHandle);
        if NumberOfBytesTransferred>0 then
          Socket.IoCompleted(TransmitBuffer, NumberOfBytesTransferred)
        else
          Socket.ShutdownDetected;
        FService.ReleaseTransmitBuffer(TransmitBuffer);
      finally
        FService.UnLockSocket(Socket);
      end;
    except
      on E: Exception do
        TdwlLogger.Log(E);
    end;
  end;
  FService.FIoThreads.Remove(Self);
end;

{ TCleanupThread }

constructor TCheckThread.Create(AService: TdwlTCPService);
begin
  FService := AService;
  inherited Create;
end;

procedure TCheckThread.Execute;
begin
  while not Terminated do
  begin
    try
      WaitForSingleObject(FWorkToDoEventHandle, DELETE_DELAY_MSECS);
      TdwlSocketStorage(FService.FSocketStorage).DoDeleteCheck;
      var TickNow := GetTickCount64;
      if TickNow>FNextTimeOutCheck then
      begin
        FNextTimeOutCheck := TickNow+TIMEOUT_CHECK_MSECS;
        TdwlSocketStorage(FService.FSocketStorage).DoTimeOutCheck;
      end;
    except
    end;
  end;
end;

{ TdwlPlainIoHandler }

function TdwlPlainIoHandler.SizeOfSocketIoVars: cardinal;
begin
  Result := 0;
end;

procedure TdwlPlainIoHandler.SocketAfterConstruction(Socket: TdwlSocket);
begin
  // no usage here
end;

procedure TdwlPlainIoHandler.SocketBeforeDestruction(Socket: TdwlSocket);
begin
  // no usage here
end;

procedure TdwlPlainIoHandler.SocketOnAccept(Socket: TdwlSocket);
begin
  // no usage here
end;

function TdwlPlainIoHandler.SocketHandleReceive(var TransmitBuffer: PdwlTransmitBuffer): boolean;
begin
  TransmitBuffer.Socket.ReadHandlingBuffer(@TransmitBuffer.HandlingBuffer);
  Result := true;
end;

function TdwlPlainIoHandler.SocketHandleWrite(var HandlingBuffer: PdwlHandlingBuffer): boolean;
begin
  var TransmitBuffer := HandlingBuffer.Socket.FService.AcquireTransmitBuffer(HandlingBuffer, COMPLETIONINDICATOR_WRITE);
  HandlingBuffer := nil; // to signal we took it
  TransmitBuffer.Socket.SendTransmitBuffer(TransmitBuffer);
  Result := true;
end;

{ TdwlSocketStorage }

constructor TdwlSocketStorage.Create;
begin
  inherited Create;
  FListAccess := TCriticalSection.Create;
  FSockets := TDictionary<TSocket, TdwlSocket>.Create;
  FSockets2Delete := TList<TdwlSocket>.Create;
end;

destructor TdwlSocketStorage.Destroy;
begin
  var SockArray := FSockets.ToArray;
  for var Sock in SockArray do
    Sock.Value.Free;
  for var Sock in FSockets2Delete do
    Sock.Free;
  FSockets.Free;
  FSockets2Delete.Free;
  FListAccess.Free;
  inherited Destroy;
end;

procedure TdwlSocketStorage.DoDeleteCheck;
begin
  var Socket2Free: TdwlSocket;
  var NowTick := GetTickCount64;
  repeat
    FListAccess.Enter;
    try
      if FSockets2Delete.Count=0 then
        Break;
      Socket2Free := FSockets2Delete[0];
      if Socket2Free.FShutdownTick+DELETE_DELAY_MSECS<NowTick then
        FSockets2Delete.Delete(0)
      else
        Break; // not yet (also the later added ones are skipped)
    finally
      FListAccess.Leave;
    end;
    Socket2Free.Free;
  until true;
end;

procedure TdwlSocketStorage.DoTimeOutCheck;
begin
  var Handles2Check: TArray<TSocket>;
  FListAccess.Enter;
  try
    Handles2Check := FSockets.Keys.ToArray;
  finally
    FListAccess.Leave;
  end;
  var NowTick := GetTickCount64;
  for var Handle2Check in Handles2Check do
  begin
    var Socket: TdwlSocket;
    if LockSocket(Handle2Check, Socket) then
    try
      if (Socket.FLastIoTick>0 {not a listen socket}) and (Socket.FLastIoTick+TIMEOUT_MSECS<NowTick) then
        Socket.FShutdownTick := 1; // to signal it must be deleted in the unlock
    finally
      UnLockSocket(Socket);
    end;
  end;
end;

function TdwlSocketStorage.LockSocket(Handle: TSocket; var Socket: TdwlSocket): boolean;
begin
  FListAccess.Enter;
  try
    Result := FSockets.TryGetValue(Handle, Socket);
  finally
    FListAccess.Leave;
  end;
  if Result then
    Socket.FSocketCS.Enter;
end;

procedure TdwlSocketStorage.RegisterNewSocket(Socket: TdwlSocket);
begin
  FListAccess.Enter;
  try
    FSockets.Add(Socket.SocketHandle, Socket);
  finally
    FListAccess.Leave;
  end;
end;

procedure TdwlSocketStorage.UnLockSocket(Socket: TdwlSocket);
begin
  if Socket.FShutdownTick>0 then
  begin
    // socket has shut down, remove it from the list and queue for deletion
    // to simplify Locking/unlocking, we do a delayed delete
    FListAccess.Enter;
    try
      if FSockets.ContainsKey(Socket.SocketHandle) then
      begin
        FSockets.Remove(Socket.SocketHandle);
        Socket.FShutdownTick := GetTickCount64;
        FSockets2Delete.Add(Socket);
      end;
    finally
      FListAccess.Leave;
    end;
  end;
  Socket.FSocketCS.Leave;;
end;

{ TdwlBaseIoHandler }

constructor TdwlBaseIoHandler.Create(AService: TdwlTCPService);
begin
  FService := AService;
end;

function TdwlBaseIoHandler.Service: TdwlTCPService;
begin
  Result := FService;
end;

end.

