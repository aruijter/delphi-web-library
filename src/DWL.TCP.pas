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
    CompletionId: cardinal;
    case integer of
    0: (Socket: TdwlSocket;
        WSABuf: TWsaBuf);
    1: (HandlingBuffer: TdwlHandlingBuffer);
  end;

  TdwlSocketClass = class of TdwlSocket;
  TdwlSocket = class
  strict private
    FSocketVars: pointer;
    FEarlyReads: TList<PdwlTransmitBuffer>;
    FSocketHandle: TSocket;
    FReadExpectedID: cardinal;
    FReadLastID: cardinal;
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
    FSocketCS: TCriticalSection;
    FShutdownTick: UInt64;
    FTransmitBuffers: TdwlThreadList<PdwlTransmitBuffer>;
    FHandlingBuffers: TdwlThreadList<PdwlHandlingBuffer>;
    FIp_Local: string;
    FIp_Remote: string;
    FPort_Local: word;
    FPort_Remote: word;
  protected
    FService: TdwlTCPService;
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
    constructor Create(AService: TdwlTCPService); virtual;
    destructor Destroy; override;
    procedure ReadHandlingBuffer(HandlingBuffer: PdwlHandlingBuffer); virtual;
    procedure FlushWrites(CloseConnection: boolean=false);
    procedure IoCompleted(TransmitBuffer: PdwlTransmitBuffer; NumberOfBytesTransferred: cardinal);
    procedure SendTransmitBuffer(TransmitBuffer: PdwlTransmitBuffer);
    procedure Shutdown;
    procedure ShutdownDetected;
    procedure StartReceiving;
    procedure WriteBuf(Buf: PByte; Size: integer);
    procedure WriteLine(const Str: string='');
    procedure WriteStr(const Str: string);
    procedure WriteUInt8(B: byte);
  end;

  IdwlTCPIoHandler = interface
    function SizeOfSocketIoVars: cardinal;
    procedure SocketAfterConstruction(Socket: TdwlSocket);
    procedure SocketBeforeDestruction(Socket: TdwlSocket);
    procedure SocketOnAccept(Socket: TdwlSocket);
    function SocketHandleReceive(var TransmitBuffer: PdwlTransmitBuffer): boolean;
    function SocketHandleWrite(var HandlingBuffer: PdwlHandlingBuffer): boolean;
  end;

  TdwlTCPService = class
  strict private
    FCodePage_US_ASCII: integer;
    FActive: boolean;
    FIoHandler: IdwlTCPIoHandler;
    procedure SetActive(const Value: boolean);
  private
    FIoCompletionPort: THandle;
    FActiveSockets: TdwlThreadList<TdwlSocket>;
    FInActiveSockets: TdwlThreadList<TdwlSocket>;
    FIoThreads: TdwlThreadList<TThread>;
    FCleanupThread: TdwlThread;
  protected
    procedure InternalActivate; virtual;
    procedure InternalDeActivate; virtual;
    procedure SetSocketAddresses(Socket: TdwlSocket; const Ip_Local: string; Port_Local: word; const Ip_Remote: string; Port_Remote: word);
  public
    property Active: boolean read FActive write SetActive;
    property CodePage_US_ASCII: integer read FCodePage_US_ASCII;
    property IOHandler: IdwlTCPIOHandler read FIOHandler write FIOHandler;
    constructor Create;
    destructor Destroy; override;
    function AcquireHandlingBuffer(Socket: TdwlSocket): PdwlHandlingBuffer;
    function AcquireTransmitBuffer(Socket: TdwlSocket; Completionindicator: byte): PdwlTransmitBuffer; overload;
    function AcquireTransmitBuffer(HandlingBuffer: PdwlHandlingBuffer; Completionindicator: byte): PdwlTransmitBuffer; overload;
    procedure ReleaseHandlingBuffer(HandlingBuffer: PdwlHandlingBuffer);
    procedure ReleaseTransmitBuffer(TransmitBuffer: PdwlTransmitBuffer);
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
  DWL.Logging, System.StrUtils, DWL.OS, System.Math, System.SysUtils;

  // We have slightly modified the definitions from Winapi.Winsock2: NumberOfBytes now can be passed as nil (pointer instead of var)
function WSARecv2; external 'ws2_32.dll' name 'WSARecv';
function WSASend2; external 'ws2_32.dll' name 'WSASend';

const
  RECV_REQUEST_COUNT = 3;
  CLEANUPTHREAD_SLEEP_MSECS = 300;
  CLEANUP_DELAY_MSECS = 750;

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

  TCleanupThread = class(TdwlThread)
  strict private
    FService: TdwlTCPService;
  protected
    procedure Execute; override;
  public
    constructor Create(AService: TdwlTCPService);
  end;

  TPlainIoHandler = class(TInterfacedObject, IdwlTCPIoHandler)
  strict private
    function SizeOfSocketIoVars: cardinal;
    procedure SocketAfterConstruction(Socket: TdwlSocket);
    procedure SocketBeforeDestruction(Socket: TdwlSocket);
    procedure SocketOnAccept(Socket: TdwlSocket);
    function SocketHandleReceive(var TransmitBuffer: PdwlTransmitBuffer): boolean;
    function SocketHandleWrite(var HandlingBuffer: PdwlHandlingBuffer): boolean;
  end;

{ TdwlSocket }

procedure TdwlSocket.HandleCurrentWriteBuffer;
begin
  if not fService.IoHandler.SocketHandleWrite(FWriteBuffer) then
    Shutdown;
  if FWriteBuffer<>nil then
    FService.ReleaseHandlingBuffer(FWriteBuffer);
end;

procedure TdwlSocket.SendTransmitBuffer(TransmitBuffer: PdwlTransmitBuffer);
begin
  var Res := CheckWSAResult_ShutdownOnError(WSASend2(SocketHandle, @TransmitBuffer.WSABuf, 1, nil, 0, LPWSAOVERLAPPED(TransmitBuffer), nil));
  if (Res<>0) and (Res<>WSAECONNRESET) and (Res<>WSAEWOULDBLOCK) and (Res<>integer(WSA_IO_PENDING)) and (Res<>integer(WSA_IO_INCOMPLETE)) then
    TdwlLogger.Log('Winsock error: '+SysErrorMessage(Res)+' ('+Res.ToString+') in SendTransmitBuffer', lsError);
  AtomicIncrement(FWritesInProgress);
end;

procedure TdwlSocket.Shutdown;
begin
  Winapi.Winsock2.shutdown(FSocketHandle, SD_BOTH);
  ShutdownDetected;
end;

procedure TdwlSocket.ShutdownDetected;
begin
  if FShutdownTick<>0 then
    Exit;
  FShutdownTick := GetTickCount64;
  if FService.FActiveSockets.Contains(Self) then
  begin // no need for extra thread protection, this is only done once and already done within SocketCS Lock.
    FService.FActiveSockets.Remove(Self);
    FService.FInActiveSockets.Add(Self);
  end;
end;

procedure TdwlSocket.StartReceiving;
begin
  FSocketCS.Enter;
  try
    FReadLastId := 0;
    FReadExpectedID := 1;
    for var i := 1 to RECV_REQUEST_COUNT do
      CreateRecvRequest;
    // if using getpeername, getsockname, getsockopt, or setsockopt enable the next line
    // setsockopt(Overlapped.Socket.SocketHandle, SOL_SOCKET, SO_UPDATE_ACCEPT_CONTEXT, <>, sizeof(SOCKET));
  finally
    FSocketCS.Leave;
  end;
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
  SetLength(StrAnsi, WideCharToMultiByte(FService.CodePage_US_ASCII, 0, PWideChar(Str), Len, PAnsiChar(StrAnsi), Len, nil, nil));
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
    ShutDown;
end;

constructor TdwlSocket.Create(AService: TdwlTCPService);
begin
  inherited Create;
  FTransmitBuffers := TdwlThreadList<PdwlTransmitBuffer>.Create;
  FHandlingBuffers := TdwlThreadList<PdwlHandlingBuffer>.Create;
  FService := AService;
  FService.FActiveSockets.Add(Self);
  FSocketCS := TCriticalSection.Create;
  FEarlyReads := TList<PdwlTransmitBuffer>.Create;
  var SocketVarsSize := FService.IOHandler.SizeOfSocketIoVars;
  if SocketVarsSize>0 then
    GetMem(FSocketVars, SocketVarsSize);
  FService.IOHandler.SocketAfterConstruction(Self);
  FSocketHandle := WSASocket(AF_INET, SOCK_STREAM, IPPROTO_TCP, nil, 0, WSA_FLAG_OVERLAPPED);
  //and attach to IoCompletionPort
	if CreateIoCompletionPort(SocketHandle, FService.FIoCompletionPort, 0, 0)=0 then
    CheckWSAResult_ShutdownOnError(-1, 'CreateIoCompletionPort');
  CreateWriteBuffer;
end;

procedure TdwlSocket.CreateRecvRequest;
begin
  var TransmitBuffer := FService.AcquireTransmitBuffer(Self, COMPLETIONINDICATOR_READ);
  Inc(FReadLastID);
  TransmitBuffer.CompletionId := FReadLastID;
  var EmptyFlags: cardinal := 0;
  var Res:= CheckWSAResult_ShutdownOnError(WSARecv2(SocketHandle, @TransmitBuffer.WSABuf, 1, nil, EmptyFlags, LPWSAOVERLAPPED(TransmitBuffer), nil));
  if (Res<>0) and (Res<>WSAECONNRESET) and (Res<>WSAEWOULDBLOCK) and (Res<>integer(WSA_IO_PENDING)) and (Res<>integer(WSA_IO_INCOMPLETE)) then
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
  FSocketCS.Enter;
  FService.IOHandler.SocketBeforeDestruction(Self);
  if FSocketVars<>nil then
    FreeMem(FSocketVars);
  closesocket(FSocketHandle);
  FService.FInActiveSockets.Remove(Self);
  FEarlyReads.Free;
  // release buffers never sent to or returned from IoCompletion
  var LeftOverTransmitBuf: PdwlTransmitBuffer;
  while FTransmitBuffers.TryPop(LeftOverTransmitBuf) do
    FService.ReleaseTransmitBuffer(LeftOverTransmitBuf);
  FTransmitBuffers.Free;
  var LeftOverhandlingBuf: PdwlHandlingBuffer;
  while FHandlingBuffers.TryPop(LeftOverhandlingBuf) do
    FService.ReleaseHandlingBuffer(LeftOverhandlingBuf);
  FHandlingBuffers.Free;
  inherited Destroy;
  FSocketCS.Free;
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
      // post a completion status for shutdown of socket (this is needed because order of iocompletion is no always as expected)
      AtomicIncrement(FWritesInProgress);
      var TransmitBuffer := FService.AcquireTransmitBuffer(Self, COMPLETIONINDICATOR_WRITE);
      PostQueuedCompletionStatus(FService.FIoCompletionPort, 0, 0, POverlapped(TransmitBuffer));
      FWriteBuffer := nil; // do not create a new one, we're finished
    end;
  end;
end;

procedure TdwlSocket.IoCompleted(TransmitBuffer: PdwlTransmitBuffer; NumberOfBytesTransferred: cardinal);
  procedure CheckEarlyReads;
  begin
    var NoneFound: boolean;
    repeat
      NoneFound := true;
      for var i := 0 to FEarlyReads.Count-1 do
      begin
        if FEarlyReads[i].CompletionId=FReadExpectedID then
        begin
          NoneFound :=  false;
          var EarlyTransmit := FEarlyReads[i];
          FEarlyReads.Delete(i);
          if not FService.IOHandler.SocketHandleReceive(EarlyTransmit) then
            Shutdown;
          if EarlyTransmit<>nil then // could be set to nil if IOHandler took over, f.e. for a delayed handling
            FService.ReleaseTransmitBuffer(EarlyTransmit);
          inc(FReadExpectedID);
          Break;
        end;
      end;
    until NoneFound;
  end;
begin
  FSocketCS.Enter;
  try
    case TransmitBuffer.CompletionIndicator of
    COMPLETIONINDICATOR_READ:
      begin
        CreateRecvRequest; // create a new receive request
        // set WSAbuf.len to actual used length
        TransmitBuffer.WSABuf.len := NumberOfBytesTransferred;
        // handle the finished request;
        CheckEarlyReads;
        if TransmitBuffer.CompletionId<>FReadExpectedID then
          FEarlyReads.Add(TransmitBuffer) // add current to early reads
        else
        begin
          if not FService.IOHandler.SocketHandleReceive(TransmitBuffer) then
            Shutdown;
          if TransmitBuffer<>nil then // could be set to nil if IOHandler took over, f.e. for a delayed handling
            FService.ReleaseTransmitBuffer(TransmitBuffer);
          inc(FReadExpectedID);
          CheckEarlyReads;
        end;
      end;
    COMPLETIONINDICATOR_WRITE:
      begin
        FService.ReleaseTransmitBuffer(TransmitBuffer);
        AtomicDecrement(FWritesInProgress);
      end;
    end;
  finally
    FSocketCS.Leave;
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
  FActiveSockets := TdwlThreadList<TdwlSocket>.Create;
  FInActiveSockets := TdwlThreadList<TdwlSocket>.Create;
  var Dummy: TCPInfo;
  // get the available most basic codepage
  if GetCPInfo(20127, Dummy) then
    FCodePage_US_ASCII := 20127
  else
    FCodePage_US_ASCII := 437;
end;

destructor TdwlTCPService.Destroy;
begin
  Active := false;
  FActiveSockets.Free;
  FInActiveSockets.Free;
  // cleanup winsock
  CheckWSAResult(WSACleanup, 'WSACleanup');
  FIoThreads.Free;
  inherited Destroy;
end;

procedure TdwlTCPService.InternalActivate;
begin
  if IOHandler=nil then
    IOHandler := TPlainIOHandler.Create;
  // Create IoCompletionPort
	FIoCompletionPort := CreateIoCompletionPort(INVALID_HANDLE_VALUE, 0, 0, 0);
  // Create IoThreads
  for var i := 1 to TdwlOS.NumberOfLogicalProcessors do
    TIOThread.Create(Self);
  FCleanupThread := TCleanupThread.Create(Self);
  FCleanupThread.FreeOnTerminate := true;
end;

procedure TdwlTCPService.InternalDeActivate;
begin
  // stop cleanup thread
  FCleanupThread.Terminate;  // It's Free on terminate
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
  // close all sockets
  var Socket: TdwlSocket;
  while FActiveSockets.TryPop(Socket) do
    Socket.Free;
  while FInActiveSockets.TryPop(Socket) do
    Socket.Free;
  // Close IoCompletionPort
  CloseHandle(FIoCompletionPort);
end;

procedure TdwlTCPService.ReleaseHandlingBuffer(HandlingBuffer: PdwlHandlingBuffer);
begin
  HandlingBuffer.Socket.FHandlingBuffers.Remove(HandlingBuffer);
  FreeMem(HandlingBuffer.Buf);
  FreeMem(HandlingBuffer);
end;

procedure TdwlTCPService.ReleaseTransmitBuffer(TransmitBuffer: PdwlTransmitBuffer);
begin
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
      var CompletionKey: NativeUInt;
      var TransmitBuffer: PdwlTransmitBuffer;
      if not GetQueuedCompletionStatus(FService.FIoCompletionPort, NumberOfBytesTransferred, CompletionKey, POverlapped(TransmitBuffer), INFINITE) then
        Continue;
      if TransmitBuffer=nil then
        Continue;
      if NumberOfBytesTransferred>0 then
        TransmitBuffer.Socket.IoCompleted(TransmitBuffer, NumberOfBytesTransferred)
      else
      begin
        TransmitBuffer.Socket.ShutdownDetected;
        FService.ReleaseTransmitBuffer(TransmitBuffer);
      end;
    except
      on E: Exception do
        TdwlLogger.Log(E);
    end;
  end;
  FService.FIoThreads.Remove(Self);
end;

{ TCleanupThread }

constructor TCleanupThread.Create(AService: TdwlTCPService);
begin
  FService := AService;
  inherited Create;
end;

procedure TCleanupThread.Execute;
begin
  while not Terminated do
  begin
    try
      WaitForSingleObject(FWorkToDoEventHandle, CLEANUPTHREAD_SLEEP_MSECS);
      var Socket: TdwlSocket;
      var DeleteMoment := GetTickCount64-CLEANUP_DELAY_MSECS;
      while FService.FInActiveSockets.TryPop(Socket) do
      begin
        if (Socket.FShutdownTick>0) and (Socket.FShutdownTick<DeleteMoment) then
          Socket.Free
        else
        begin
          FService.FInActiveSockets.Insert(0, Socket); // maybe next time
          Break;
        end;
      end;
    except
    end;
  end;
end;

{ TPlainIoHandler }

function TPlainIoHandler.SizeOfSocketIoVars: cardinal;
begin
  Result := 0;
end;

procedure TPlainIoHandler.SocketAfterConstruction(Socket: TdwlSocket);
begin
  // no usage here
end;

procedure TPlainIoHandler.SocketBeforeDestruction(Socket: TdwlSocket);
begin
  // no usage here
end;

procedure TPlainIoHandler.SocketOnAccept(Socket: TdwlSocket);
begin
  // no usage here
end;

function TPlainIoHandler.SocketHandleReceive(var TransmitBuffer: PdwlTransmitBuffer): boolean;
begin
  TransmitBuffer.Socket.ReadHandlingBuffer(@TransmitBuffer.HandlingBuffer);
  Result := true;
end;

function TPlainIoHandler.SocketHandleWrite(var HandlingBuffer: PdwlHandlingBuffer): boolean;
begin
  var TransmitBuffer := HandlingBuffer.Socket.FService.AcquireTransmitBuffer(HandlingBuffer, COMPLETIONINDICATOR_WRITE);
  HandlingBuffer := nil; // to signal we took it
  TransmitBuffer.Socket.SendTransmitBuffer(TransmitBuffer);
  Result := true;
end;

end.

