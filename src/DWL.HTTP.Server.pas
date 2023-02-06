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
  DWL.HTTP.Server.Types, System.Generics.Collections, IdHTTPServer,
  IdSocketHandle;

type
  TdwlHTTPServer=class;
  TdwlHTTPHandler=class;

  /// <summary>
  ///   <para>
  ///     A HTTP server specifically designed to be able to server content by self
  ///     developed handlers. All basic requirements to be able to quickly
  ///     have a self developed Web or Rest server running are centralized in
  ///     this object.
  ///   </para>
  ///   <para>
  ///     Although it can function completely on its own, the main usage of
  ///     this class is the app dwlserver.
  ///   </para>
  /// </summary>
  TdwlHTTPServer = class
  strict private
    FOnlyLocalConnections: boolean;
    FRequestsInProgress: TDictionary<TIdContext, PdwlHTTPHandlingState>;
    FRequestsInProgressAccess: TCriticalSection;
    FRootHandlerAccess: TMultiReadExclusiveWriteSynchronizer;
    FRootHandler: TdwlHTTPHandler;
    FHTTPServer: TIdHTTPServer;
    FURIAliases: TDictionary<string, string>;
    FLogLevel: byte;
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
    function GetActive: boolean;
  public
    /// <summary>
    ///   Set Active to true to have the server to bootstrap and start
    ///   listening to requests
    /// </summary>
    property Active: boolean read GetActive;
    /// <summary>
    ///   user Bindings to bind to physical ip/port combinations. This directly
    ///   refers to the Binding property of th Indy HTTP server
    /// </summary>
    property Bindings: TIdSocketHandles read GetBindings;
    /// <summary>
    ///   The minimum loglevel to use when logging requests. See loglevel
    ///   consts for details about the levels
    /// </summary>
    property LogLevel: byte read FLogLevel write FLogLevel;
    property OnlyLocalConnections: boolean read FOnlyLocalConnections write FOnlyLocalConnections;
    constructor Create;
    destructor Destroy; override;
    function BaseURI: string;
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
    ///   By adding and URL alias, in the case the alias URI is called by a
    ///   client, the traffice is rerouted to the given URI Handler
    /// </summary>
    procedure AddURIAlias(const Alias, Uri: string);
    /// <summary>
    ///   Removes all currentley know URI aliases
    /// </summary>
    procedure ClearURIAliases;
    /// <summary>
    ///   if an Hostname including certificates is added through AddHostName
    ///   the SSL protocol is activated and this function returns true
    /// </summary>
    function IsSecure: boolean;
    /// <summary>
    ///   Get the handler bound to the given URI
    /// </summary>
    function FindHandler(const URI: string): TdwlHTTPHandler;
    /// <summary>
    ///   register a handler bound to a URI. All requests starting with this
    ///   specific URI will be routed through this handler
    /// </summary>
    procedure RegisterHandler(const URI: string; Handler: TdwlHTTPHandler);
    /// <summary>
    ///   Unregister the handler bound to a specific URI
    /// </summary>
    function UnRegisterHandler(const URI: string): boolean;
    /// <summary>
    ///   This will suspend handling, the server will stay active, but the
    ///   handling of the requests will be delayed until ResumeHandling is
    ///   called. Do not suspend handling for a long time.
    /// </summary>
    procedure SuspendHandling;
    /// <summary>
    ///   Resumes handling after it was delayed by Suspendhandling
    /// </summary>
    procedure ResumeHandling;
    /// <summary>
    ///   Active the server and start processing requests
    /// </summary>
    procedure Open;
    /// <summary>
    ///   FinalizeSessions will wait until all pending requests have been
    ///   processed.
    /// </summary>
    procedure FinalizeSessions;
  end;

  /// <summary>
  ///   The TdwlHTTPHandler class is the base class from which you can derive
  ///   you own handler to process requests
  /// </summary>
  TdwlHTTPHandler = class
  private
    FHTTPServer: TdwlHTTPServer;
    FURI: string;
  protected
    /// <summary>
    ///   If the variable FWrapupProc is assigned, it will be called everything a process has finished
    ///   The handler then can free request specific resources
    ///   When the handler is destroyed, this function will be called with nil parameter
    ///   to indicate that globals resources can be freed
    /// </summary>
    FWrapupProc: TdwlWrapupProc;
    /// <summary>
    ///   The HTTPServer from which this request originated
    /// </summary>
    property HTTPServer: TdwlHTTPServer read FHTTPServer;
    /// <summary>
    ///   Override this abstract function and implement the actual process in
    ///   this function
    /// </summary>
    function ProcessRequest(const State: PdwlHTTPHandlingState): boolean; virtual; abstract;
    function LogDescription: string; virtual;
  public
    destructor Destroy; override;
    /// <summary>
    ///   When authorizing clients, the handler that will actual handle the
    ///   request will first be asked to authorize the user and allow this
    ///   user to execute the functions implemented in this handler
    /// </summary>
    function Authorize(const State: PdwlHTTPHandlingState): boolean; virtual;
  end;

implementation

uses
  System.Classes, Winapi.Windows, Winapi.WinInet,
  IdGlobal, IdHashSHA,
  System.NetEncoding, IdSSLOpenSSL, DWL.Logging, DWL.HTTP.Consts,
  IdAssignedNumbers, System.StrUtils, DWL.HTTP.Server.Utils,
  DWL.HTTP.Server.Globals, DWL.HTTP.Utils, DWL.Classes;

type
  TdwlHTTPHandler_PassThrough = class(TdwlHTTPHandler)
  strict private
    FHandlers: TObjectDictionary<string, TdwlHTTPHandler>;
  private
    function FindHandler(const URI: string): TdwlHTTPHandler;
    procedure RegisterHandler(const URI: string; Handler: TdwlHTTPHandler);
    function UnRegisterHandler(const URI: string): boolean;
  protected
    function ProcessRequest(const State: PdwlHTTPHandlingState): boolean; override;
  public
    constructor Create;
    destructor Destroy; override;
    function Authorize(const State: PdwlHTTPHandlingState): boolean; override;
  end;

  PServerStructure = ^TServerStructure;
  TServerStructure = record
    State_URI: string;
    ContentBuffer: pointer;
    ContentLength: cardinal;
    ContentOwned: boolean;
    HTTPServer: TdwlHTTPServer;
    Context: TIdContext;
    RequestInfo: TIdHTTPRequestInfo;
    ResponseInfo: TIdHTTPResponseInfo;
    Tick: UInt64;
    FinalHandler: TdwlHTTPHandler;
    WebSocketsReceiveProc: TdwlHTTPWebSocket_OnData;
  end;

{ TdwlHTTPServer }

procedure TdwlHTTPServer.HTTPServerDisconnect(AContext: TIdContext);
  function GetLogLine(State: PdwlHTTPHandlingState): string;
  begin
    var FinalHandler := PServerStructure(State._InternalServerStructure).FinalHandler;
    if FinalHandler<>nil then
      Result := FinalHandler.FURI
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
        if (State.StatusCode<>HTTP_STATUS_OK) and (LogLevel>=httplogLevelFailedRequests) then
          TdwlLogger.Log(GetLogLine(State), lsNotice)
        else
        begin
          if LogLevel>=httplogLevelAllRequests then
            TdwlLogger.Log(GetLogLine(State), lsTrace)
        end;
      end;
    end
    else
    begin
      if LogLevel>=httplogLevelEverything then
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
      Assigned(PServerStructure(State._InternalServerStructure).FinalHandler.FWrapupProc) then
      PServerStructure(State._InternalServerStructure).FinalHandler.FWrapupProc(State);
    PServerStructure(State._InternalServerStructure).State_URI := ''; // dispose string
    Freemem(State._InternalServerStructure);
    FreeMem(State);
  end;
end;

procedure TdwlHTTPServer.HTTPServerParseAuthentication(AContext: TIdContext; const AAuthType, AAuthData: String; var VUsername, VPassword: String; var VHandled: Boolean);
begin
  VHandled := true; // This is to let a bearer token come through without error, this is handled by the server internally
end;

function TdwlHTTPServer.IsSecure: boolean;
begin
  Result := FSSL_HostName<>'';
end;

procedure TdwlHTTPServer.SuspendHandling;
begin
  FRootHandlerAccess.BeginWrite;
end;

function TdwlHTTPServer.UnRegisterHandler(const URI: string): boolean;
begin
  FRootHandlerAccess.BeginWrite;
  try
    Result := TdwlHTTPHandler_PassThrough(FRootHandler).UnRegisterHandler(URI);
  finally
    FRootHandlerAccess.EndWrite;
  end;
end;

procedure TdwlHTTPServer.AddHostName(const HostName, CertificateFileName, RootCertificateFileName, PrivateKeyFileName: string);
begin
  FSSL_HostName := HostName;
  FSSL_CertificateFileName := CertificateFileName;
  FSSL_RootCertificateFileName := RootCertificateFileName;
  FSSL_PrivateKeyFileName := PrivateKeyFileName;
end;

procedure TdwlHTTPServer.AddURIAlias(const Alias, Uri: string);
begin
  if FURIAliases=nil then
    FURIAliases := TDictionary<string, string>.Create;
  FURIAliases.Add(Alias, Uri);
end;

function TdwlHTTPServer.BaseURI: string;
begin
  // The base uri is ALWAYS the standard port!!
  // Listenport is not taken into account
  Result := IfThen(IsSecure, 'https', 'http')+'://'+
    IfThen(FSSL_HostName='', 'localhost', FSSL_HostName);
end;

procedure TdwlHTTPServer.ClearURIAliases;
begin
  if FURIAliases<>nil then
    FURIAliases.Clear;
end;

constructor TdwlHTTPServer.Create;
begin
  inherited Create;
  FRootHandlerAccess := TMultiReadExclusiveWriteSynchronizer.Create;
  FRequestsInProgressAccess := TCriticalSection.Create;
  FRequestsInProgress := TDictionary<TIDContext, PdwlHTTPHandlingState>.Create;
  FHTTPServer := TIdHTTPServer.Create(nil);
  FHTTPServer.OnCommandOther := HTTPServerCommand;
  FHTTPServer.OnCommandGet := HTTPServerCommand;
  FHTTPServer.OnDisconnect := HTTPServerDisconnect;
  FHTTPServer.OnParseAuthentication := HTTPServerParseAuthentication;
  FRootHandler := TdwlHTTPHandler_PassThrough.Create;
  FRootHandler.FHTTPServer := Self;
end;

destructor TdwlHTTPServer.Destroy;
begin
  FHTTPServer.Free;
  FRootHandler.Free;
  FRootHandlerAccess.Free;
  FRequestsInProgress.Free;
  FRequestsInProgressAccess.Free;
  FURIAliases.Free;
  inherited Destroy;
end;

procedure TdwlHTTPServer.FinalizeSessions;
  function GetCount: word;
  begin
    FRequestsInProgressAccess.Enter;
    try
      Result := FRequestsInProgress.Count;
    finally
      FRequestsInProgressAccess.Leave;
    end;
  end;
begin
  while GetCount>0 do
  begin
    Sleep(200);
  end;
end;

function TdwlHTTPServer.FindHandler(const URI: string): TdwlHTTPHandler;
begin
  FRootHandlerAccess.BeginRead;
  try
    Result := TdwlHTTPHandler_PassThrough(FRootHandler).FindHandler(URI);
  finally
    FRootHandlerAccess.EndRead;
  end;
end;

function TdwlHTTPServer.GetActive: boolean;
begin
  Result := FHTTPServer.Active;
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
    // all strings that are put in State as PWiderChar should 'live'
    // until State will be freed
    // We arranged this by putting the strings that are referenced in the state
    // are kept in the serverstructure
    State._InternalServerStructure := AllocMem(SizeOf(TServerStructure));
    State.Command := TdwlHTTPUtils.StringTodwlhttpCommand(ARequestInfo.Command);
    if (FURIAliases=nil) or not FURIAliases.TryGetValue(ARequestInfo.URI, PServerStructure(State._InternalServerStructure).State_URI) then
      PServerStructure(State._InternalServerStructure).State_URI := ARequestInfo.URI;
    State.URI := PWideChar(PServerStructure(State._InternalServerStructure).State_URI);
    State.Flags := 0;
    PServerStructure(State._InternalServerStructure).HTTPServer := Self;
    PServerStructure(State._InternalServerStructure).RequestInfo := ARequestInfo;
    PServerStructure(State._InternalServerStructure).ResponseInfo := AResponseInfo;
    PServerStructure(State._InternalServerStructure).Context := AContext;
    if (LogLevel>=httplogLevelFailedRequests) then
      PServerStructure(State._InternalServerStructure).Tick := GetTickCount64;

    AResponseInfo.CustomHeaders.AddValue('Cache-Control', 'no-cache');
    State.StatusCode := HTTP_STATUS_OK;
    FRootHandlerAccess.BeginRead;
    try
      // RootHandler never needs Authentication Control
      // So no need to check here, just process request
      if (not FRootHandler.ProcessRequest(State)) then
      begin
        State.StatusCode := HTTP_STATUS_NOT_FOUND;
        State.SetContentText('<!DOCTYPE html><html lang=""><head><title>Not Found</title></head><body>Not Found</body></html>');
      end;
    finally
      FRootHandlerAccess.EndRead;
    end;
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
  if Active then
    Exit;
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
  VUseSSL := IsSecure;
end;

procedure TdwlHTTPServer.RegisterHandler(const URI: string; Handler: TdwlHTTPHandler);
begin
  FRootHandlerAccess.BeginWrite;
  try
    TdwlHTTPHandler_PassThrough(FRootHandler).RegisterHandler(URI, Handler);
  finally
    FRootHandlerAccess.EndWrite;
  end;
end;

procedure TdwlHTTPServer.ResumeHandling;
begin
  FRootHandlerAccess.EndWrite;
end;

{ TdwlHTTPHandler_PassThrough }

function TdwlHTTPHandler_PassThrough.Authorize(const State: PdwlHTTPHandlingState): boolean;
begin
  // Authorization is always passed through, so for me its ok now!
  Result := true;
end;

constructor TdwlHTTPHandler_PassThrough.Create;
begin
  FHandlers := TObjectDictionary<string, TdwlHTTPHandler>.Create([doOwnsValues]);
end;

destructor TdwlHTTPHandler_PassThrough.Destroy;
begin
  FHandlers.Free;
  inherited Destroy;
end;

function TdwlHTTPHandler_PassThrough.FindHandler(const URI: string): TdwlHTTPHandler;
begin
  Result := nil;
  if Copy(URI, 1, 1)<>'/' then
    Exit;
  var S := Copy(URI, 2, MaxInt);
  var P := Pos('/', S);
  var URISegment: string;
  if P>1 then
    URISegment := Copy(S, 1, P-1)
  else
    URISegment := S;
  var Handler: TdwlHTTPHandler;
  if not FHandlers.TryGetValue(URISegment, Handler) then
    Exit;
  if P<1 then // we reached the end: found
    Result := Handler
  else
  begin
    if (Handler is TdwlHTTPHandler_PassThrough) then
      Result := TdwlHTTPHandler_PassThrough(Handler).FindHandler(Copy(S, P, MaxInt));
  end;
end;

function TdwlHTTPHandler_PassThrough.ProcessRequest(const State: PdwlHTTPHandlingState): boolean;
begin
  Result := false;
  var New_URI := PServerStructure(State._InternalServerStructure).State_URI;
  if Copy(New_URI, 1, 1)<>'/' then
    Exit;
  var S := Copy(New_URI, 2, MaxInt);
  var P := Pos('/', S);
  var P2 := Pos('?' , S);
  if (P2>0) and (P2<P) then
    P := P2;
  if P=0 then
    P := MaxInt;
  var Handler: TdwlHTTPHandler;
  if FHandlers.TryGetValue(Copy(S, 1, P-1), Handler) then
  begin
    New_URI := Copy(S, P, MaxInt);
    PServerStructure(State._InternalServerStructure).State_URI := New_URI;
    State.URI := PWideChar(PServerStructure(State._InternalServerStructure).State_URI);
    // Do a security check
    // Before passing request to the found handler
    if Handler.Authorize(State) then
    begin
      PServerStructure(State._InternalServerStructure).FinalHandler := Handler;
      Result := Handler.ProcessRequest(State)
    end
    else
    begin
      // We issue a not found (instead of a denied), because this doesn't reveal unneeded information
      // and if the url is temporary not available, giving a 401 will result in logging out
      // in some clients which we want to prevent
      State.StatusCode := HTTP_STATUS_NOT_FOUND;
      State.SetContentText('<html>Not found.</html>');
      Result := true;
    end;
  end;
end;

procedure TdwlHTTPHandler_PassThrough.RegisterHandler(const URI: string; Handler: TdwlHTTPHandler);
begin
  if Copy(URI, 1, 1)<>'/' then
    raise Exception.Create('URI does not start with a slash');
  var S := Copy(URI, 2, MaxInt);
  var P := Pos('/', S);
  if P>1 then
  begin
    var URISegment := Copy(S, 1, P-1);
    var PassThroughHandler: TdwlHTTPHandler;
    if FHandlers.TryGetValue(URISegment, PassThroughHandler) then
    begin
      if not (PassThroughHandler is TdwlHTTPHandler_PassThrough) then
        raise Exception.Create('URI '+URI+' is already associated with another handler');
    end
    else
    begin
      PassThroughHandler := TdwlHTTPHandler_PassThrough.Create;
      PassThroughHandler.FHTTPServer := HTTPServer;
      PassThroughHandler.FURI := FURI+URISegment;
      FHandlers.Add(URISegment, PassThroughHandler);
    end;
    TdwlHTTPHandler_PassThrough(PassThroughHandler).RegisterHandler(Copy(S, P, MaxInt), Handler);
  end
  else
  begin
    FHandlers.Add(S, Handler);
    Handler.FHTTPServer := HTTPServer;
    Handler.FURI := FURI+URI;
  end;
end;

function TdwlHTTPHandler_PassThrough.UnRegisterHandler(const URI: string): boolean;
begin
  Result := false;
  if Copy(URI, 1, 1)<>'/' then
    Exit;
  var S := Copy(URI, 2, MaxInt);
  var P := Pos('/', S);
  var URISegment: string;
  if P>1 then
    URISegment := Copy(S, 1, P-1)
  else
    URISegment := S;
  var Handler: TdwlHTTPHandler;
  if not FHandlers.TryGetValue(URISegment, Handler) then
    Exit;
  if P<1 then
  begin  // we reached the end: Do the actual unregister
    FHandlers.Remove(URISegment);
    Result := true;
  end
  else
  begin
    if (Handler is TdwlHTTPHandler_PassThrough) then
      Result := TdwlHTTPHandler_PassThrough(Handler).UnRegisterHandler(Copy(S, P, MaxInt));
  end;
end;

{ TdwlHTTPHandler }

function TdwlHTTPHandler.Authorize(const State: PdwlHTTPHandlingState): boolean;
begin
  Result := false;
end;

destructor TdwlHTTPHandler.Destroy;
begin
  if Assigned(FWrapUpProc) then
  try
    FWrapUpProc(nil);
  except
    On E: Exception do
      TdwlLogger.Log(LogDescription+': exception on wrapup: '+E.Message, lsError)
  end;
  inherited Destroy;
end;

function TdwlHTTPHandler.LogDescription: string;
begin
  Result := Classname;
end;

initialization
  serverProcs.ArrangeContentBufferProc := State_ArrangeContentBuffer;
  serverProcs.GetRequestParamProc := State_GetRequestParam;
  serverProcs.GetHeaderValueProc := State_GetHeaderValue;
  serverProcs.GetPayloadPtrProc := State_GetPostDataPtr;
  serverProcs.SetHeaderValueProc := State_SetHeaderValue;
  serverProcs.ActivateWebSocketproc := State_ActivateWebSocket;

end.
