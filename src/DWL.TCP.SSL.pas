unit DWL.TCP.SSL;

interface

uses
  DWL.OpenSSL.Api, System.SyncObjs, System.Generics.Collections, DWL.TCP,
  System.Classes;

type
  TdwlSslEnvironment=class;

  TdwlSslContext = class
  strict private
    FHostName: string;
    FEnvironment: TdwlSslEnvironment;
    FopSSL_CTX: pSSL_CTX;
  private
    FProtocolsAccepted: TStringList;
    FBindingIP: string;
  public
    property Environment: TdwlSslEnvironment read FEnvironment;
    property HostName: string read FHostName;
    property opSSL_CTX: pSSL_CTX read FopSSL_CTX;
    constructor Create(AEnvironment: TdwlSslEnvironment; const AHostName, Cert, Key: string; const ABindingIP: string; ProtocolsAccepted: TArray<string>);
    destructor Destroy; override;
  end;

  TdwlSslEnvironment = class
  strict private
    FMainContext: TdwlSslContext;
    FMREW: TLightweightMREW;
    FContexts: TDictionary<string, TdwlSslContext>;
    FDeprecatedContexts: TObjectList<TdwlSslContext>;
    function GetMainContext: TdwlSslContext;
    procedure CheckMainContext;
  private
    FEmptyContext: TdwlSslContext;
  public
    property MainContext: TdwlSslContext read GetMainContext;
    constructor Create;
    destructor Destroy; override;
    procedure AddContext(const HostName, Cert, Key: string; ProtocolsAccepted: TArray<string>=[]);
    function ContextCount: cardinal;
    function GetContext(const HostName: string; const BindingIP: string=''): TdwlSslContext;
    procedure RemoveContext(const HostName: string);
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
  Winapi.Windows, DWL.OpenSSL, System.SysUtils, System.StrUtils,
  DWL.HTTP.Consts, DWL.Logging;

const
  SSL_EX_DATA_SELF_INDEX = 0;

type
  PsslSocketVars = ^TsslSocketVars;
  TsslSocketVars = record
    bioRecv: pBIO;
    bioSend: pBIO;
    SendBuf: PdwlTransmitBuffer;
    ReadBuf: PdwlHandlingBuffer;
    Context: TdwlSslContext;
    opSSL: pSSL;
end;

function client_hello(opSSL: pSSL; al: PInteger; arg: pointer): integer; cdecl;
begin
  // init variables
  Result := SSL_CLIENT_HELLO_ERROR;
  var SocketVars := PsslSocketVars(SSL_get_ex_data(opSSL, SSL_EX_DATA_SELF_INDEX));
  // context initialisation
  var Context2Use := SocketVars.Context;
  if SocketVars.Context = SocketVars.Context.Environment.FEmptyContext then
  begin
    if SocketVars.Context.Environment.ContextCount=0 then // still no valid context defined, cannot continue
      Exit;
    Context2Use := SocketVars.Context.Environment.MainContext;
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
        var NewContext := SocketVars.Context.Environment.GetContext(HostName);
        if (NewContext<>nil) then
          Context2Use := NewContext
      end;
    end;
  end;
  // if needed change to the new context
  if SocketVars.Context<>Context2Use then
  begin
    SSL_set_SSL_CTX(opSSL, Context2Use.opSSL_CTX);
    SocketVars.Context := Context2Use;
    SocketVars.SendBuf.Socket.Context_HostName := Context2Use.HostName;
  end;
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
    if SocketVars.Context.FProtocolsAccepted.IndexOf(string(Protocol))>=0 then
    begin
      _out := Ptr;
      outlen := len;
      Result := SSL_TLSEXT_ERR_OK;
      Break; // first found is ok!
    end;
    inc(Ptr, Len);
  end;
end;

{ TdwlSslContext }

constructor TdwlSslContext.Create(AEnvironment: TdwlSslEnvironment; const AHostName, Cert, Key: string; const ABindingIP: string; ProtocolsAccepted: TArray<string>);
const
  HDR_BEGIN = '-----BEGIN ';
  HDR_END = '-----END ';
begin
  inherited Create;
  FBindingIP := ABindingIP;
  FEnvironment := AEnvironment;
  FHostName := AHostName;
  FProtocolsAccepted := TStringList.Create;
  for var Protocol in ProtocolsAccepted do
    FProtocolsAccepted.Add(Protocol);
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
  FProtocolsAccepted.Free;
  inherited Destroy
end;

{ TdwlSSLEnvironment }

procedure TdwlSslEnvironment.AddContext(const HostName, Cert, Key: string; ProtocolsAccepted: TArray<string>=[]);
begin
  FMREW.BeginWrite;
  try
    var NewCtx := TdwlSslContext.Create(Self, HostName, Cert, Key, '', ProtocolsAccepted);
    var DeprCtx: TdwlSslContext;
    if not FContexts.TryGetValue(HostName.ToLower, DeprCtx) then
      DeprCtx := nil;
    if FMainContext=DeprCtx then // initially nil=nil ;-)
      FMainContext := NewCtx;
    if DeprCtx<>nil then
      FContexts.Remove(HostName.ToLower);
    FContexts.Add(HostName, NewCtx);
    if DeprCtx<>nil then
      FDeprecatedContexts.Add(DeprCtx); // keep for now (current connections), will be disposed in destroy of environment
    CheckMainContext;
  finally
    FMREW.EndWrite;
  end;
end;

procedure TdwlSslEnvironment.CheckMainContext;
begin
  if (FMainContext=nil) and (FContexts.Count>0) then
  begin
    var ContextValues := FContexts.Values.ToArray;
    FMainContext := ContextValues[0];
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
  FContexts := TDictionary<string, TdwlSslContext>.Create;
  FDeprecatedContexts := TObjectList<TdwlSslContext>.Create;
end;

destructor TdwlSslEnvironment.Destroy;
begin
  var ENum := FContexts.GetEnumerator;
  try
    while ENum.MoveNext do
      ENum.Current.Value.Free;
  finally
    ENum.Free;
  end;
  FContexts.Free;
  FDeprecatedContexts.Free;
  FEmptyContext.Free;
  inherited Destroy;
end;

function TdwlSslEnvironment.GetContext(const HostName: string; const BindingIP: string=''): TdwlSslContext;
begin
  FMREW.BeginRead;
  try
    if not FContexts.TryGetValue(HostName.ToLower, Result) then
      Result := nil;
  finally
    FMREW.EndRead;
  end;
  // check if only specific binding is allowed
  if (BindingIP<>'') and (Result<>nil) and (Result.FBindingIP<>'') and (Result.FBindingIP<>BindingIP) then
    Result := nil;
end;

function TdwlSslEnvironment.GetMainContext: TdwlSslContext;
begin
  if FMainContext=nil then
  begin
    if FEmptyContext=nil then
      FEmptyContext := TdwlSslContext.Create(Self, '', '', '', '', []);
    Result := FEmptyContext;
  end
  else
    Result := FMainContext;
end;

procedure TdwlSslEnvironment.RemoveContext(const HostName: string);
begin
  FMREW.BeginWrite;
  try
    var CtxToRemove: TdwlSslContext;
    if not FContexts.TryGetValue(HostName.ToLower, CtxToRemove) then
      Exit;
    FContexts.Remove(HostName.ToLower);
    FDeprecatedContexts.Add(CtxToRemove);
    if FMainContext=CtxToRemove then
    begin
      FMainContext := nil;
      CheckMainContext;
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
  PsslSocketVars(Socket.SocketVars).Context := FEnvironment.MainContext;
  Socket.Context_HostName := FEnvironment.MainContext.HostName; // in case client_hello is not executed
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
  PsslSocketVars(Socket.SocketVars).SendBuf := Socket.Service.AcquireTransmitBuffer(Socket, COMPLETIONINDICATOR_WRITE);
  PsslSocketVars(Socket.SocketVars).ReadBuf := Socket.Service.AcquireHandlingBuffer(Socket);
end;

procedure TdwlSslIoHandler.SocketBeforeDestruction(Socket: TdwlSocket);
begin
  Socket.Service.ReleaseTransmitBuffer(PsslSocketVars(Socket.SocketVars).SendBuf);
  Socket.Service.ReleaseHandlingBuffer(PsslSocketVars(Socket.SocketVars).ReadBuf);
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

end.

