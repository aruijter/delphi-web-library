unit DWL.TCP.SSL;

interface

uses
  DWL.OpenSSL.Api, System.SyncObjs, System.Generics.Collections, DWL.TCP,
  System.Classes, DWL.HTTP.Consts;

type
  TdwlSslEnvironment=class;

  IdwlSslContext = interface
    function Environment: TdwlSslEnvironment;
    function HostName: string;
    function opSSL_CTX: pSSL_CTX;
    function ProtocolAccepted: string;
  end;

  TdwlSslContext = class(TInterfacedObject, IdwlSslContext)
  strict private
    FHostName: string;
    FEnvironment: TdwlSslEnvironment;
    FopSSL_CTX: pSSL_CTX;
  private
    FProtocolAccepted: string;
    function Environment: TdwlSslEnvironment;
    function HostName: string;
    function opSSL_CTX: pSSL_CTX;
    function ProtocolAccepted: string;
  public
    constructor Create(AEnvironment: TdwlSslEnvironment; const AHostName, Cert, Key, ProtocolAccepted: string);
    destructor Destroy; override;
  end;

  TdwlSslEnvironment = class
  strict private
    FMainContext: IdwlSslContext;
    FMREW: TLightweightMREW;
    FContexts: TList<IdwlSslContext>;
    function InternalGetContext(const HostName, ProtocolAccepted: string): IdwlSslContext;
  public
    property MainContext: IdwlSslContext read FMainContext;
    constructor Create;
    destructor Destroy; override;
    procedure AddContext(const HostName, Cert, Key, ProtocolAccepted: string);
    function ContextCount: cardinal;
    function GetContext(const HostName, ProtocolAccepted: string): IdwlSslContext;
    procedure RemoveContext(const HostName, ProtocolAccepted: string);
  end;

  IdwlSslIoHandler = interface
    ['{A302DD55-C27A-4132-A934-584D33599AA8}']
    function Environment: TdwlSSlEnvironment;
  end;

  TdwlSslIoHandler = class(TdwlBaseIoHandler, IdwlTcpIoHandler, IdwlSslIoHandler)
  strict private
    FEnvironment: TdwlSslEnvironment;
    function Process(Socket: TdwlSocket): boolean;
    function SizeOfSocketIoVars: cardinal;
    procedure SocketAfterConstruction(Socket: TdwlSocket);
    procedure SocketBeforeDestruction(Socket: TdwlSocket);
    procedure SocketOnAccept(Socket: TdwlSocket);
    function SocketHandleReceive(var TransmitBuffer: PdwlTransmitBuffer): boolean;
    function SocketHandleWrite(var HandlingBuffer: PdwlHandlingBuffer): boolean;
    function SslOnError_ShouldRetry(SslError: integer): boolean;
  private
    function Environment: TdwlSSlEnvironment;
  public
    constructor Create(AService: TdwlTCPService);
    destructor Destroy; override;
  end;

implementation

uses
  Winapi.Windows, DWL.OpenSSL, System.SysUtils;

const
  SSL_EX_DATA_SELF_INDEX = 0;

type
  PsslSocketVars = ^TsslSocketVars;
  TsslSocketVars = record
    bioRecv: pBIO;
    bioSend: pBIO;
    SendBuf: PdwlTransmitBuffer;
    ReadBuf: PdwlHandlingBuffer;
    _Context: TdwlSslContext;
    opSSL: pSSL;
    function Context: IdwlSslContext;
    procedure SetContext(AContext: IdwlSslContext; Propogate: boolean=true);
  end;

function client_hello(opSSL: pSSL; al: PInteger; arg: pointer): integer; cdecl;
begin
  // init variables
  Result := SSL_CLIENT_HELLO_ERROR;
  var SocketVars := PsslSocketVars(SSL_get_ex_data(opSSL, SSL_EX_DATA_SELF_INDEX));
  // context initialisation
  var Context2Use: IdwlSslContext := SocketVars.Context;
  if Context2Use.HostName='' then
  begin
    Context2Use := SocketVars.Context.Environment.MainContext;
    if Context2Use.HostName='' then // still no valid context defined, cannot continue
      Exit;
  end;
  // from now on we can proceed with success
  Result := SSL_CLIENT_HELLO_SUCCESS;
  // If needed change context according to hostname
  if SocketVars.Context.Environment.ContextCount>1 then //else switching doesn't make sense
  begin
    var ExtData : PByte;
    var ExtDataLen: SIZE_T;
    if SSL_client_hello_get0_ext(opSSL, TLSEXT_TYPE_server_name, @ExtData, @ExtDataLen)=1 then
    begin
      if (ExtDataLen>2) and (ExtData[0]=0) and (ExtData[1]=(ExtDataLen-2)) then
      begin
        var Len := ExtData[4];
        var AnsiHostName: ansistring;
        SetLength(AnsiHostName, Len);
        Move(ExtData[5], AnsiHostName[1], Len);
        var HostName := string(AnsiHostName);
        // eventually switch context
        var NewContext := SocketVars.Context.Environment.GetContext(HostName, Context2Use.ProtocolAccepted);
        if (NewContext<>nil) then
          Context2Use := NewContext;
      end;
    end;
  end;
  // change to the new context
  SocketVars.SetContext(Context2Use)
end;

function alpn_select(opSSL: pSSL; var _out: pointer; var outlen: integer; input: pointer; inlen: cardinal; arg: pointer): integer; cdecl;
begin
  // start with no acknowledgement result
  Result := SSL_TLSEXT_ERR_NOACK;
  // init variables
  var SocketVars := PsslSocketVars(SSL_get_ex_data(opSSL, SSL_EX_DATA_SELF_INDEX));
  // now check the protocol
  var Ptr := PByte(input);
  var LenLeft: integer := inlen;
  while LenLeft>1 do
  begin
    var Protocol: ansistring;
    var Len := Ptr^;
    LenLeft := LenLeft-Len-1;
    inc(Ptr);
    SetLength(Protocol, Len);
    Move(Ptr^, Protocol[1], Len);
    var NewContext := SocketVars.Context.Environment.GetContext(SocketVars.Context.HostName, string(Protocol));
    if (NewContext<>nil) then
    begin
      SocketVars.SetContext(NewContext);
      _out := Ptr;
      outlen := len;
      Result := SSL_TLSEXT_ERR_OK;
      Break; // first found is ok!
    end;
    inc(Ptr, Len);
  end;
end;

{ TdwlSslContext }

constructor TdwlSslContext.Create(AEnvironment: TdwlSslEnvironment; const AHostName, Cert, Key, ProtocolAccepted: string);
const
  HDR_BEGIN = '-----BEGIN ';
  HDR_END = '-----END ';
begin
  inherited Create;
  FEnvironment := AEnvironment;
  FHostName := AHostName;
  FProtocolAccepted := ProtocolAccepted;
  FopSSL_CTX := SSL_CTX_new(TLS_method);
  if Cert<>'' then
  begin
    var CertBegin := pos(HDR_BEGIN, Cert);
    var MainDone := false;
    while CertBegin>0 do
    begin
      var CertEnd := pos(HDR_END, Cert, CertBegin);
      if CertEnd<0 then
        Break;
      CertEnd := pos(#10, Cert, CertEnd);
      if CertEnd<1 then
        CertEnd := Length(Cert);
      var X509Cert := TdwlOpenSSL.New_Cert_FromPEMStr(Copy(Cert, CertBegin, CertEnd-CertBegin+1));
      if X509Cert=nil then
        Break;
      if MainDone then
      begin
        // as extra certs are added to original, we need to transfer ownership
        // using and extra parameter when getting the X509 pointer from the IdwlX509Cert
        if SSL_CTX_add_extra_chain_cert(FopSSL_CTX, X509Cert.X509(true))=0 then
          raise Exception.Create('Error SSL_CTX_use_certificate');
      end
      else
      begin
        if SSL_CTX_use_certificate(FopSSL_CTX, X509Cert.X509)=0 then
          raise Exception.Create('Error SSL_CTX_use_certificate');
        MainDone := true;
      end;
      CertBegin := pos(HDR_BEGIN, Cert, CertEnd+1);
    end;
  end;
  // Load Private Key
  if Key<>'' then
  begin
    var PrivKey := TdwlOpenSSL.New_PrivateKey_FromPEMStr(Key);
    if SSL_CTX_use_PrivateKey(FopSSL_CTX, PrivKey.key)=0 then
        raise Exception.Create('Error SSL_CTX_use_PrivateKey');
  end;
  SSL_CTX_set_client_hello_cb(opSSL_CTX, @client_hello, nil);
  SSL_CTX_set_alpn_select_cb(opSSL_CTX, @alpn_select, Self);
end;

destructor TdwlSslContext.Destroy;
begin
  SSL_CTX_free(FopSSL_CTX);
  inherited Destroy
end;

function TdwlSslContext.Environment: TdwlSslEnvironment;
begin
  Result := FEnvironment;
end;

function TdwlSslContext.HostName: string;
begin
  Result := FHostName;
end;

function TdwlSslContext.opSSL_CTX: pSSL_CTX;
begin
  Result := FopSSL_CTX;
end;

function TdwlSslContext.ProtocolAccepted: string;
begin
  Result := FProtocolAccepted;
end;

{ TdwlSSLEnvironment }

procedure TdwlSslEnvironment.AddContext(const HostName, Cert, Key, ProtocolAccepted: string);
begin
  FMREW.BeginWrite;
  try
    var NewCtx: IdwlSslContext := TdwlSslContext.Create(Self, HostName, Cert, Key, ProtocolAccepted);
    var DeprCtx := InternalGetContext(HostName, ProtocolAccepted);
    if DeprCtx<>nil then
      FContexts.Remove(DeprCtx);
    FContexts.Add(NewCtx);
    if FContexts.Count=1 then
      FMainContext := NewCtx;
  finally
    FMREW.EndWrite;
  end;
end;

function TdwlSslEnvironment.ContextCount: cardinal;
begin
  FMREW.BeginRead;
  try
    Result := FContexts.Count;
  finally
    FMREW.EndRead;
  end;
end;

constructor TdwlSslEnvironment.Create;
begin
  inherited Create;
  FContexts := TList<IdwlSslContext>.Create;
  FMainContext := TdwlSslContext.Create(Self, '', '', '', ALPN_HTTP_1_1);
end;

destructor TdwlSslEnvironment.Destroy;
begin
  FContexts.Free;
  inherited Destroy;
end;

function TdwlSslEnvironment.GetContext(const HostName, ProtocolAccepted: string): IdwlSslContext;
begin
  FMREW.BeginRead;
  try
    Result := InternalGetContext(HostName, ProtocolAccepted);
  finally
    FMREW.EndRead;
  end;
end;

function TdwlSslEnvironment.InternalGetContext(const HostName, ProtocolAccepted: string): IdwlSslContext;
begin
  Result := nil;
 for var Context in FContexts do
  begin
    if (Context.HostName=HostName) and (Context.ProtocolAccepted=ProtocolAccepted) then
    begin
      Result := Context;
      Exit;
    end;
  end;
end;

procedure TdwlSslEnvironment.RemoveContext(const HostName, ProtocolAccepted: string);
begin
  FMREW.BeginWrite;
  try
    for var Context in FContexts do
    begin
      if (Context.HostName=HostName) and (Context.ProtocolAccepted=ProtocolAccepted) then
      begin
        FContexts.Remove(Context);
        if FMainContext=Context then
        begin
          if FContexts.Count>0 then
            FMainContext := FContexts[0]
          else
            FMainContext := TdwlSslContext.Create(Self, '', '', '', ALPN_HTTP_1_1);
        end;
        Exit;
      end;
    end;
  finally
    FMREW.EndWrite;
  end;
end;

{ TdwlSslIoHandler }

constructor TdwlSslIoHandler.Create(AService: TdwlTCPService);
begin
  inherited Create(AService);
  FEnvironment := TdwlSslEnvironment.Create;
end;

destructor TdwlSslIoHandler.Destroy;
begin
  FEnvironment.Free;
  inherited Destroy;
end;

function TdwlSslIoHandler.Environment: TdwlSSlEnvironment;
begin
  Result := FEnvironment;
end;

function TdwlSslIoHandler.Process(Socket: TdwlSocket): boolean;
begin
  Result := true;
  var SslVars := PsslSocketVars(Socket.SocketVars);
  var BytesRead: integer;
  // Try to deliver bytes to application
  var ReadBuf := sslVars.ReadBuf;
  repeat
    BytesRead := SSL_read(SslVars.opSSL, PByte(ReadBuf.buf), ReadBuf.NumberOfBytes);
    if BytesRead>0 then
    begin
      ReadBuf.NumberOfBytes := BytesRead;
      Socket.ReadHandlingBuffer(ReadBuf);
      ReadBuf.NumberOfBytes := DWL_TCP_BUFFER_SIZE;
    end
    else
      Result := SslOnError_ShouldRetry(SSL_get_error(SslVars.opSSL, BytesRead));
  until BytesRead<=0;
  // Try to send out bytes to winsock
  repeat
    BytesRead := BIO_read(SslVars.bioSend, SslVars.SendBuf.WSABuf.buf, SslVars.SendBuf.WSABuf.len);
    if BytesRead>0 then
    begin
      SslVars.SendBuf.WSABuf.len := BytesRead;
      Socket.SendTransmitBuffer(SslVars.SendBuf);
      SslVars.SendBuf := Socket.Service.AcquireTransmitBuffer(Socket, COMPLETIONINDICATOR_WRITE);
    end
    else
      Result := SslOnError_ShouldRetry(SSL_get_error(SslVars.opSSL, BytesRead));
  until BytesRead<=0;
end;

function TdwlSslIoHandler.SizeOfSocketIoVars: cardinal;
begin
  Result := SizeOf(TsslSocketVars)
end;

procedure TdwlSslIoHandler.SocketAfterConstruction(Socket: TdwlSocket);
begin
  PsslSocketVars(Socket.SocketVars).SendBuf := Socket.Service.AcquireTransmitBuffer(Socket, COMPLETIONINDICATOR_WRITE);
  PsslSocketVars(Socket.SocketVars).ReadBuf := Socket.Service.AcquireHandlingBuffer(Socket);
  // We need to best guess a context to bind to this socket,
  // That's why we link the MainContext.\
  // Depending on client_hello and alpn protocol
  // the context will be changed in a later stage
  PsslSocketVars(Socket.SocketVars)._Context := nil;
  PsslSocketVars(Socket.SocketVars).SetContext(FEnvironment.MainContext, false);
  PsslSocketVars(Socket.SocketVars).opSSL := SSL_new(PsslSocketVars(Socket.SocketVars).Context.opSSL_CTX);
  if PsslSocketVars(Socket.SocketVars).opSSL=nil then
    raise Exception.Create('Error creating OpenSSL Object');
  // add pointer to myself: needed in callback situations
  if SSL_set_ex_data(PsslSocketVars(Socket.SocketVars).opSSL, SSL_EX_DATA_SELF_INDEX, Socket.SocketVars)=0 then
    raise Exception.Create('Error in SSL_set_ex_data');
  // for noW we do server based, maybe later we make diff between server and client
  PsslSocketVars(Socket.SocketVars).bioRecv := BIO_new(BIO_s_mem());
  PsslSocketVars(Socket.SocketVars).bioSend := BIO_new(BIO_s_mem());
  SSL_set_bio(PsslSocketVars(Socket.SocketVars).opSSL, PsslSocketVars(Socket.SocketVars).bioRecv, PsslSocketVars(Socket.SocketVars).bioSend);
end;

procedure TdwlSslIoHandler.SocketBeforeDestruction(Socket: TdwlSocket);
begin
  Socket.Service.ReleaseTransmitBuffer(PsslSocketVars(Socket.SocketVars).SendBuf);
  Socket.Service.ReleaseHandlingBuffer(PsslSocketVars(Socket.SocketVars).ReadBuf);
  PsslSocketVars(Socket.SocketVars).SetContext(nil, false);
  SSL_free(PsslSocketVars(Socket.SocketVars).opSSL);
end;

procedure TdwlSslIoHandler.SocketOnAccept(Socket: TdwlSocket);
begin
  SSL_set_accept_state(PsslSocketVars(Socket.SocketVars).opSSL);
  Process(Socket); // for possible startup of handshaking
end;

function TdwlSslIoHandler.SocketHandleReceive(var TransmitBuffer: PdwlTransmitBuffer): boolean;
begin
  var Socket := TransmitBuffer.Socket;
  var SslVars := PsslSocketVars(Socket.SocketVars);
  var BytesWritten := BIO_write(SslVars.bioRecv, TransmitBuffer.WSABuf.buf, TransmitBuffer.WSaBuf.len);
	Result := cardinal(BytesWritten)=TransmitBuffer.WSABuf.len;
//  if (not Result) and (BIO_should_retry(SslVars.bioRecv)<>0) then
//    raise Exception.Create('Please implement delayed receiving');
  if Result then
    Process(Socket);
end;

function TdwlSslIoHandler.SocketHandleWrite(var HandlingBuffer: PdwlHandlingBuffer): boolean;
begin
  var Socket := HandlingBuffer.Socket;
  var SslVars := PsslSocketVars(Socket.SocketVars);
  var BytesWritten := SSL_write(SslVars.opSSL, HandlingBuffer.WSABuf.buf, HandlingBuffer.WSaBuf.len);
	Result := cardinal(BytesWritten)=HandlingBuffer.WSABuf.len;
  if (not Result) and SslOnError_ShouldRetry(ssl_get_error(SslVars.opSSL, BytesWritten)) then
    raise Exception.Create('Please implement delayed sending');
  if Result then
    Process(Socket);
end;

function TdwlSslIoHandler.SslOnError_ShouldRetry(SslError: integer): boolean;
begin
  Result := SslError in [SSL_ERROR_WANT_READ, SSL_ERROR_WANT_WRITE, SSL_ERROR_WANT_CONNECT, SSL_ERROR_WANT_ACCEPT];
end;

{ TsslSocketVars }

function TsslSocketVars.Context: IdwlSslContext;
begin
  Result := _Context;
end;

procedure TsslSocketVars.SetContext(AContext: IdwlSslContext; Propogate: boolean=true);
begin
  if _Context=TdwlSslContext(AContext) then
    Exit;
  if Propogate and (AContext<>nil) then
    SSL_set_SSL_CTX(opSSL, AContext.opSSL_CTX);
  if _Context<>nil then
    _Context._Release;
  if AContext<>nil then
  begin
    _Context := TdwlSslContext(AContext);
    AContext._AddRef;
    SendBuf.Socket.Context_HostName := AContext.HostName;
  end;
end;

end.

