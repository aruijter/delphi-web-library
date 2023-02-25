unit DWL.TCP;

interface

uses
  Winapi.Windows, Winapi.Winsock2, System.SyncObjs, System.Generics.Collections,
  DWL.SyncObjs, System.Classes;

type
  TdwlTCPService=class;
  TdwlSocket=class;

  PdwlTransmitBuffer = ^TdwlTransmitBuffer;
  TdwlTransmitBuffer = record
    Overlapped: TOverlapped;  // must be the first to be able to parse to winsock functions as POverlapped or LPWSAOVERLAPPED
    WSABuf: TWsaBuf;
    Socket: TdwlSocket;
    CompletionIndicator: byte;
    CompletionId: cardinal;
  end;

  TEarlyRead = record
    TransmitBuffer: PdwlTransmitBuffer;
    NumberOfBytesTransferred: cardinal;
  end;

  TdwlSocket = class
  strict private
    FEarlyReads: TList<TEarlyRead>;
    FSocketHandle: TSocket;
    FReadExpectedID: cardinal;
    FReadLastID: cardinal;
    FSendPos: PByte;
    FSendBufLeft: cardinal;
    FCloseConnection: boolean;
    FWritesInProgress: cardinal;
    procedure CreateRecvRequest;
    procedure SendCurrentOverlapped;
    procedure Write(Buf: PByte; Size: integer);
    procedure WSA_ShutdownOnError(ResultCode: Integer; const Op: string);
  private
    FSocketCS: TCriticalSection;
    FShutdownTick: UInt64;
    FTransmitBuffers: TdwlThreadList<PdwlTransmitBuffer>;
  protected
    FService: TdwlTCPService;
    FSendTransmitBuffer: PdwlTransmitBuffer;
    procedure CreateSendBuffer;
  public
    property SocketHandle: TSocket read FSocketHandle;
    constructor Create(AService: TdwlTCPService); virtual;
    destructor Destroy; override;
    procedure DoRead(FData: PByte; NumberOfBytes: cardinal); virtual;
    procedure FinishWriting(CloseConnection: boolean);
    procedure IoCompleted(TransmitBuffer: PdwlTransmitBuffer; NumberOfBytesTransferred: cardinal);
    procedure Shutdown;
    procedure ShutdownDetected;
    procedure StartReceiving;
    procedure WriteLine(const Str: string);
    procedure WriteStr(const Str: string);
    procedure WriteUInt8(B: byte);
  end;

  TdwlTCPService = class
  strict private
    FCodePage_US_ASCII: integer;
    FActive: boolean;
    procedure SetActive(const Value: boolean);
  private
    FTransmitBuffers: TdwlThreadList<PdwlTransmitBuffer>;
    FIoCompletionPort: THandle;
    FActiveSockets: TdwlThreadList<TdwlSocket>;
    FInActiveSockets: TdwlThreadList<TdwlSocket>;
    FIoThreads: TdwlThreadList<TThread>;
    FCleanupThread: TdwlThread;
  protected
    procedure InternalActivate; virtual;
    procedure InternalDeActivate; virtual;
  public
    property Active: boolean read FActive write SetActive;
    property CodePage_US_ASCII: integer read FCodePage_US_ASCII;
    constructor Create;
    destructor Destroy; override;
    function AcquireTransmitBuffer(Socket: TdwlSocket; Completionindicator: byte): PdwlTransmitBuffer;
    procedure ReleaseTransmitBuffer(TransmitBuffer: PdwlTransmitBuffer);
  end;

function CheckWSAResult(ResultCode: Integer; const Op: string): Integer; overload;
function CheckWSAResult(ResultBool: boolean; const Op: string): Integer; overload;
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
  COMPLETIONINDICATOR_READ = 1;
  COMPLETIONINDICATOR_WRITE = 2;
  TRANSMIT_BUFFER_SIZE = 16384;
  RECV_REQUEST_COUNT = 3;
  CLEANUPTHREAD_SLEEP_MSECS = 300;
  CLEANUP_DELAY_MSECS = 750;

function CheckWSAResult(ResultCode: Integer; const Op: string): Integer;
begin
  if ResultCode<>0 then
  begin
    Result := WSAGetLastError;
    if (Result<>WSAEWOULDBLOCK) and (Result<>integer(WSA_IO_PENDING)) and (Result<>integer(WSA_IO_INCOMPLETE)) then
      TdwlLogger.Log('Winsock error: '+SysErrorMessage(Result)+' ('+Result.ToString+')'+IfThen(Op<>'',' in '+Op), lsError);
  end
  else
    Result := 0;
end;

function CheckWSAResult(ResultBool: boolean; const Op: string): Integer;
begin
  if not ResultBool then
    Result := CheckWSAResult(-1, Op)
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

{ TdwlSocket }

procedure TdwlSocket.SendCurrentOverlapped;
begin
  AtomicIncrement(FWritesInProgress);
  WSA_ShutdownOnError(WSASend2(SocketHandle, @FSendTransmitBuffer.WSABuf, 1, nil, 0, LPWSAOVERLAPPED(FSendTransmitBuffer), nil), 'WSASend');
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

procedure TdwlSocket.Write(Buf: PByte; Size: integer);
begin
  var BytesToWrite := min(Size, FSendBufLeft);
  while BytesToWrite>0 do
  begin
    Move(Buf^, FSendPos^, BytesToWrite);
    dec(FSendBufLeft, BytesToWrite);
    if FSendBufLeft>0 then
    begin
      inc(FSendPos, BytesToWrite);
      Break;
    end;
    // buffer full: send out:
    SendCurrentOverlapped;
    CreateSendBuffer;
    // Decrease Size and calculate new BytesToWrite
    inc(Buf, BytesToWrite);
    Size := Size - BytesToWrite;
    BytesToWrite := min(Size, FSendBufLeft);
  end;
end;

procedure TdwlSocket.WriteLine(const Str: string);
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
  Write(PByte(PAnsiChar(StrAnsi)), Len);
end;

procedure TdwlSocket.WriteUInt8(B: byte);
begin
  Write(@B, 1);
end;

procedure TdwlSocket.WSA_ShutdownOnError(ResultCode: Integer; const Op: string);
begin
  if ResultCode=0 then
    Exit;
  Resultcode := CheckWSAResult(ResultCode, Op);
  if (ResultCode<>0) and (ResultCode<>WSA_IO_PENDING) then
    ShutDown;
end;

constructor TdwlSocket.Create(AService: TdwlTCPService);
begin
  inherited Create;
  FTransmitBuffers := TdwlThreadList<PdwlTransmitBuffer>.Create;
  FService := AService;
  FService.FActiveSockets.Add(Self);
  FSocketCS := TCriticalSection.Create;
  FEarlyReads := TList<TEarlyRead>.Create;
  FSocketHandle := WSASocket(AF_INET, SOCK_STREAM, IPPROTO_TCP, nil, 0, WSA_FLAG_OVERLAPPED);
  //and attach to IoCompletionPort
	if CreateIoCompletionPort(SocketHandle, FService.FIoCompletionPort, 0, 0)=0 then
    WSA_ShutdownOnError(-1, 'CreateIoCompletionPort');
  CreateSendBuffer;
end;

procedure TdwlSocket.CreateRecvRequest;
begin
  var TransmitBuffer := FService.AcquireTransmitBuffer(Self, COMPLETIONINDICATOR_READ);
  Inc(FReadLastID);
  TransmitBuffer.CompletionId := FReadLastID;
  var EmptyFlags: cardinal := 0;
  WSA_ShutdownOnError(WSARecv2(SocketHandle, @TransmitBuffer.WSABuf, 1, nil, EmptyFlags, LPWSAOVERLAPPED(TransmitBuffer), nil), 'WSARecv');
end;

procedure TdwlSocket.CreateSendBuffer;
begin
  FSendTransmitBuffer := FService.AcquireTransmitBuffer(Self, COMPLETIONINDICATOR_WRITE);
  FSendPos := PByte(FSendTransmitBuffer.WSABuf.buf);
  FSendBufLeft := FSendTransmitBuffer.WSABuf.len;
end;

destructor TdwlSocket.Destroy;
begin
  FSocketCS.Enter;
  closesocket(FSocketHandle);
  FService.FInActiveSockets.Remove(Self);
  FEarlyReads.Free;
  // release buffers never sent to or returned from IoCompletion
  var LeftOverBuf: PdwlTransmitBuffer;
  while FTransmitBuffers.TryPop(LeftOverBuf) do
    FService.ReleaseTransmitBuffer(LeftOverBuf);
  FTransmitBuffers.Free;
  inherited Destroy;
  FSocketCS.Free;
end;

procedure TdwlSocket.DoRead(FData: PByte; NumberOfBytes: cardinal);
begin
  // base socket does not need to read ;-)
end;

procedure TdwlSocket.FinishWriting(CloseConnection: boolean);
begin
  FCloseConnection := CloseConnection;
  if FSendBufLeft<TRANSMIT_BUFFER_SIZE then
  begin
    FSendTransmitBuffer.WSABuf.len := TRANSMIT_BUFFER_SIZE-FSendBufLeft;
    SendCurrentOverlapped;
    if CloseConnection then
      FSendTransmitBuffer := nil // do not create a new one, we're finished
    else
      CreateSendBuffer;
  end
  else
  begin
    if CloseConnection then
    begin
      // post a completion status for shutdown of socket (this is needed because order of iocompletion is no always as expected)
      AtomicIncrement(FWritesInProgress);
      PostQueuedCompletionStatus(FService.FIoCompletionPort, 0, 0, POverlapped(FSendTransmitBuffer));
      FSendTransmitBuffer := nil // do not create a new one, we're finished
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
        if FEarlyReads[i].TransmitBuffer.CompletionId=FReadExpectedID then
        begin
          NoneFound :=  false;
          DoRead(PByte(FEarlyReads[i].TransmitBuffer.WSABuf.buf), FEarlyReads[i].NumberOfBytesTransferred);
          FService.ReleaseTransmitBuffer(FEarlyReads[i].TransmitBuffer);
          inc(FReadExpectedID);
          FEarlyReads.Delete(i);
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
        // handle the finished request;
        CheckEarlyReads;
        if TransmitBuffer.CompletionId<>FReadExpectedID then
        begin // add current to early reads
          var EarlyRead: TEarlyRead;
          EarlyRead.TransmitBuffer := TransmitBuffer;
          EarlyRead.NumberOfBytesTransferred := NumberOfBytesTransferred;
          FEarlyReads.Add(EarlyRead);
        end
        else
        begin
          DoRead(PByte(TransmitBuffer.WSABuf.buf), NumberOfBytesTransferred);
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

constructor TdwlTCPService.Create;
begin
  inherited Create;
  FTransmitBuffers := TdwlThreadList<PdwlTransmitBuffer>.Create;
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
  CheckWSAResult(WSACleanup, 'WSACleanup');;
  // dispose the transmitbuffers
  var BuffersList :=  FTransmitBuffers.LockList;
  try
    for var Buffer in BuffersList do
    begin
      FreeMem(Buffer.WSABuf.buf);
      FreeMem(Buffer);
    end;
  finally
    FTransmitBuffers.UnlockList;
  end;
  FTransmitBuffers.Free;
  FIoThreads.Free;
  inherited Destroy;
end;

procedure TdwlTCPService.InternalActivate;
begin
  // Create IoCompletionPort
	FIoCompletionPort := CreateIoCompletionPort(INVALID_HANDLE_VALUE, 0, 0, 0);
  // Create IoThreads
  for var i := 1 to TdwlOS.NumberOfLogicalProcessors do
    TIOThread.Create(Self);
  FCleanupThread := TCleanupThread.Create(Self);
end;

procedure TdwlTCPService.InternalDeActivate;
begin
  // stop cleanup thread
  FCleanupThread.Terminate;
  FCleanupThread.WaitFor;
  FCleanupThread.Free;
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

procedure TdwlTCPService.ReleaseTransmitBuffer(TransmitBuffer: PdwlTransmitBuffer);
begin
  TransmitBuffer.Socket.FTransmitBuffers.Remove(TransmitBuffer);
  FTransmitBuffers.Add(TransmitBuffer);
end;

function TdwlTCPService.AcquireTransmitBuffer(Socket: TdwlSocket; CompletionIndicator: byte): PdwlTransmitBuffer;
begin
  if not FTransmitBuffers.TryPop(Result) then
  begin
    GetMem(Result, Sizeof(TdwlTransmitBuffer));
    Getmem(Result.WSABuf.buf, TRANSMIT_BUFFER_SIZE);
  end;
  Result.WSABuf.len := TRANSMIT_BUFFER_SIZE; // also for existing buffers, could have been changed while sending
  ZeroMemory(@Result.Overlapped, SizeOf(TOverlapped));
  Result.CompletionIndicator := CompletionIndicator;
  Result.Socket := Socket;
  Socket.FTransmitBuffers.Add(Result);
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
      begin
        TdwlLogger.Log('TIoThread.Execute error: '+E.Message, lsError);
      end;
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

end.
