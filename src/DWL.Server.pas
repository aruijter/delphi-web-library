unit DWL.Server;

{$I DWL.inc}

interface

uses
  System.Threading, DWL.Logging,
  DWL.TCP.HTTP,
  System.Generics.Collections, System.SysUtils,
  DWL.Server.Types;

type
  TDWlServer=class;

  IdwlHTTPHandler = interface
    function Authorize(const State: PdwlHTTPHandlingState): boolean;
    function ProcessRequest(const State: PdwlHTTPHandlingState): boolean;
  end;

  /// <summary>
  ///   The TdwlHTTPHandler class is the base class from which you can derive
  ///   you own handler to process requests
  /// </summary>
  TdwlHTTPHandler = class(TInterfacedObject, IdwlHTTPHandler)
  strict private
    FURI: string;
    FURICutIndex: cardinal;
    procedure SetUri(Value: string);
  private
    function RemoveLeadUri(const URI: string): string;
  protected
    /// <summary>
    ///   If the variable FWrapupProc is assigned, it will be called everytime a request has finished
    ///   The handler then can free request specific resources
    ///   When the handler is destroyed, this function will be called with nil parameter
    ///   to indicate that globals resources can be freed
    /// </summary>
    FWrapupProc: TdwlWrapupProc;
    FServer: TDWLServer;
    function LogDescription: string; virtual;
    /// <summary>
    ///   When authorizing clients, the handler that will actual handle the
    ///   request will first be asked to authorize the user and allow this
    ///   user to execute the functions implemented in this handler
    /// </summary>
    function Authorize(const State: PdwlHTTPHandlingState): boolean; virtual;
    /// <summary>
    ///   Override this abstract function and implement the actual process in
    ///   this function
    /// </summary>
    function ProcessRequest(const State: PdwlHTTPHandlingState): boolean; virtual;
  public
    property URI: string read FURI write SetUri;
    property WrapupProc: TdwlWrapupProc read FWrapupProc;
    destructor Destroy; override;
  end;

  TDWLServer = class(TdwlCustomHTTPServer)
  strict private
    FURIAliases: TDictionary<string, string>;
    FOnlyLocalConnections: boolean;
    FExecutionTask: ITask;
    FHandlerAccess: TMultiReadExclusiveWriteSynchronizer;
    FRootHandler: TdwlHTTPHandler;
    FGlobalIssuer: string;
  private
    function GetIsRunning: boolean;
  protected
    function HandleRequest(Request: TdwlHTTPSocket): boolean; override;
    procedure InternalDeActivate; override;
  public
    property GlobalIssuer: string read FGlobalIssuer write FGlobalIssuer;
    property IsRunning: boolean read GetIsRunning;
    property OnlyLocalConnections: boolean read FOnlyLocalConnections write FOnlyLocalConnections;
    constructor Create;
    destructor Destroy; override;
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
    ///   Get the handler bound to the given URI
    /// </summary>
    function FindHandler(const URI: string): IdwlHTTPHandler;
    /// <summary>
    ///   register a handler bound to a URI. All requests starting with this
    ///   specific URI will be routed through this handler
    /// </summary>
    procedure RegisterHandler(const URI: string; Handler: IdwlHTTPHandler);
    /// <summary>
    ///   Unregister the handler bound to a specific URI
    /// </summary>
    function UnRegisterHandler(const URI: string): boolean;
  end;

procedure AssignServerProcs;

implementation

uses
  System.Classes,
  System.Rtti,

  Winapi.Windows, DWL.Server.Consts,
  DWL.HTTP.Consts, DWL.Mail.Queue, System.Math,
  Winapi.WinInet, DWL.Server.Utils,
  DWL.Server.Globals, System.NetEncoding, IdMessage, DWL.Mail.Utils,
  System.JSON;

type
  TdwlHTTPHandler_PassThrough = class(TdwlHTTPHandler)
  strict private
    FHandlers: TDictionary<string, IdwlHTTPHandler>;
  private
    function FindHandler(const EndpointURI: string): IdwlHTTPHandler;
    procedure RegisterHandler(const EndpointURI: string; Handler: IdwlHTTPHandler);
    function UnRegisterHandler(const EndpointURI: string): boolean;
    procedure UnRegisterAllHandlers;
  public
    constructor Create(AServer: TDWLServer);
    destructor Destroy; override;
  end;

type
  PServerStructure = ^TServerStructure;
  TServerStructure = record
    State_URI: string;
    Handler: IdwlHTTPHandler;
    WebSocketsReceiveProc: TdwlHTTPWebSocket_OnData;
    ContentBuffer: pointer;
    ContentLength: cardinal;
    ContentOwned: boolean;
    Request: TdwlHTTPSocket;
  end;

{ TDWLServer }

procedure TDWLServer.AddURIAlias(const Alias, Uri: string);
begin
  if FURIAliases=nil then
    FURIAliases := TDictionary<string, string>.Create;
  FURIAliases.Add(Alias, Uri);
end;

procedure TDWLServer.ClearURIAliases;
begin
  if FURIAliases<>nil then
    FURIAliases.Clear;
end;

constructor TDWLServer.Create;
begin
  inherited Create;
  FHandlerAccess := TMultiReadExclusiveWriteSynchronizer.Create;
  FRootHandler := TdwlHTTPHandler_PassThrough.Create(Self);
end;

destructor TDWLServer.Destroy;
begin
  Active := false;
  FURIAliases.Free;
  FRootHandler.Free;
  FHandlerAccess.Free;
  inherited Destroy;
end;

function TDWLServer.FindHandler(const URI: string): IdwlHTTPHandler;
begin
  FHandlerAccess.BeginRead;
  try
    Result := TdwlHTTPHandler_PassThrough(FRootHandler).FindHandler(URI);
  finally
    FHandlerAccess.EndRead;
  end;
end;

function TDWLServer.GetIsRunning: boolean;
begin
  Result := (FExecutionTask<>nil) and (FExecutionTask.Status=TTaskStatus.Running);
end;

function TDWLServer.HandleRequest(Request: TdwlHTTPSocket): boolean;
  procedure WriteNotFound;
  const
    S: ansistring = '<!DOCTYPE html><html lang=""><head><title>Not Found</title></head><body>Not Found</body></html>';
  begin
    Request.StatusCode := HTTP_STATUS_NOT_FOUND;
    Request.ResponseDataStream.WriteBuffer(S[1], Length(S));
  end;
  procedure WriteServerError;
  const
    S: ansistring = '<!DOCTYPE html><html lang=""><head><title>Internal server error</title></head><body>Internal server error</body></html>';
  begin
    Request.StatusCode := HTTP_STATUS_SERVER_ERROR;
    Request.ResponseDataStream.WriteBuffer(S[1], Length(S));
  end;
begin
  Result := true;
  try
    if OnlyLocalConnections and (Request.IP_Remote<>'127.0.0.1') then
    begin
      WriteNotFound;
      Exit;
    end;
    var Uri := Request.Uri;
    var NewUri: string;
    if (FURIAliases<>nil) and FURIAliases.TryGetValue(Uri, NewURI) then
      Uri := NewUri;
    var Handler := FindHandler(Uri);
    if Handler=nil then
    begin
      WriteNotFound;
      Exit;
    end;
    var State: PdwlHTTPHandlingState := AllocMem(SizeOf(TdwlHTTPHandlingState));
    State._InternalServerStructure := AllocMem(SizeOf(TServerStructure));
    try
      State.RequestMethod := Request.RequestMethod;
      State.Flags := Request.Flags;
      PServerStructure(State._InternalServerStructure).Handler := Handler;
      PServerStructure(State._InternalServerStructure).State_URI := TdwlHTTPHandler(Handler).RemoveLeadURI(URI);
      PServerStructure(State._InternalServerStructure).Request := Request;
      // all strings that are put in State as PWiderChar should backed by a Pascal String
      // until State will be freed
      // We arranged this by putting the strings that are referenced in the serverstructure
      State.URI := PWideChar(PServerStructure(State._InternalServerStructure).State_URI);
      State.SetHeaderValue(HTTP_FIELD_CACHE_CONTROL, 'no-cache');
      State.StatusCode := HTTP_STATUS_OK;
      if not Handler.Authorize(State) then
        State.StatusCode := HTTP_STATUS_DENIED
      else
      begin
        if Handler.ProcessRequest(State) then
        begin
          Request.Flags := State.Flags;
          Request.StatusCode := State.StatusCode;
          Request.ResponseDataStream.WriteBuffer(PServerStructure(State._InternalServerStructure).ContentBuffer^, PServerStructure(State._InternalServerStructure).ContentLength);
          if Assigned(TdwlHTTPHandler(Handler).WrapupProc) then
            TdwlHTTPHandler(Handler).WrapupProc(State);
        end
        else
          WriteNotFound;
      end;
      if PServerStructure(State._InternalServerStructure).ContentOwned then
        FreeMem(PServerStructure(State._InternalServerStructure).ContentBuffer);
    finally
      // dispose reference counted items
      PServerStructure(State._InternalServerStructure).Handler := nil;
      PServerStructure(State._InternalServerStructure).State_URI := '';
      Freemem(State._InternalServerStructure);
      FreeMem(State);
    end;
  except
    on E: Exception do
    begin
      TdwlLogger.Log('Exception while handling '+Request.URI+': '+E.Message, lsError);
      WriteServerError;
    end;
  end;
end;

procedure TDWLServer.InternalDeActivate;
begin
  TdwlHTTPHandler_PassThrough(FRootHandler).UnRegisterAllHandlers;
  inherited InternalDeActivate;
end;

procedure TDWLServer.RegisterHandler(const URI: string; Handler: IdwlHTTPHandler);
begin
  FHandlerAccess.BeginWrite;
  try
    TdwlHTTPHandler_PassThrough(FRootHandler).RegisterHandler(URI, Handler);
  finally
    FHandlerAccess.EndWrite;
  end;
end;

procedure TDWLServer.ResumeHandling;
begin
  FHandlerAccess.EndWrite;
end;

procedure TDWLServer.SuspendHandling;
begin
  FHandlerAccess.BeginWrite;
end;

function TDWLServer.UnRegisterHandler(const URI: string): boolean;
begin
  FHandlerAccess.BeginWrite;
  try
    Result := TdwlHTTPHandler_PassThrough(FRootHandler).UnRegisterHandler(URI);
  finally
    FHandlerAccess.EndWrite;
  end;
end;

{ TdwlHTTPHandler_PassThrough }

constructor TdwlHTTPHandler_PassThrough.Create(AServer: TDWLServer);
begin
  inherited Create;
  FServer := AServer;
  FHandlers := TDictionary<string, IdwlHTTPHandler>.Create;
end;

destructor TdwlHTTPHandler_PassThrough.Destroy;
begin
  FHandlers.Free;
  inherited Destroy;
end;

function TdwlHTTPHandler_PassThrough.FindHandler(const EndpointURI: string): IdwlHTTPHandler;
begin
  if Copy(EndpointURI, 1, 1)<>'/' then
    Exit;
  var S := Copy(EndpointURI, 2, MaxInt);
  var P := Pos('/', S);
  var URISegment: string;
  if P>1 then
    URISegment := Copy(S, 1, P-1)
  else
    URISegment := S;
  if not FHandlers.TryGetValue(URISegment, Result) then
    Exit(nil);
  if P>1 then
  begin
    if (Result is TdwlHTTPHandler_PassThrough) then
      Result := TdwlHTTPHandler_PassThrough(Result).FindHandler(Copy(S, P, MaxInt));
  end
  else
  begin
    // don't return a passthrough handler
    if Result is TdwlHTTPHandler_PassThrough then
      Result := nil;

  end;
end;

procedure TdwlHTTPHandler_PassThrough.RegisterHandler(const EndpointURI: string; Handler: IdwlHTTPHandler);
begin
  if Copy(EndpointURI, 1, 1)<>'/' then
    raise Exception.Create('URI does not start with a slash');
  var S := Copy(EndpointURI, 2, MaxInt);
  var P := Pos('/', S);
  if P>1 then
  begin
    var URISegment := Copy(S, 1, P-1);
    var PassThroughHandler: IdwlHTTPHandler;
    if FHandlers.TryGetValue(URISegment, PassThroughHandler) then
    begin
      if not (PassThroughHandler is TdwlHTTPHandler_PassThrough) then
        raise Exception.Create('URI '+EndpointURI+' is already associated with another handler');
    end
    else
    begin
      PassThroughHandler := TdwlHTTPHandler_PassThrough.Create(FServer);
      TdwlHTTPHandler_PassThrough(PassThroughHandler).URI := URI+'/'+URISegment;
      FHandlers.Add(URISegment, PassThroughHandler);
    end;
    TdwlHTTPHandler_PassThrough(PassThroughHandler).RegisterHandler(Copy(S, P, MaxInt), Handler);
  end
  else
  begin
    FHandlers.Add(S, Handler);
    TdwlHTTPHandler(Handler).FServer := FServer;
    TdwlHTTPHandler(Handler).URI := URI+EndpointURI;
  end;
end;

procedure TdwlHTTPHandler_PassThrough.UnRegisterAllHandlers;
begin
  FHandlers.Clear;
end;

function TdwlHTTPHandler_PassThrough.UnRegisterHandler(const EndpointURI: string): boolean;
begin
  Result := false;
  if Copy(EndpointURI, 1, 1)<>'/' then
    Exit;
  var S := Copy(EndpointURI, 2, MaxInt);
  var P := Pos('/', S);
  var URISegment: string;
  if P>1 then
    URISegment := Copy(S, 1, P-1)
  else
    URISegment := S;
  var Handler: IdwlHTTPHandler;
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

function TdwlHTTPHandler.ProcessRequest(const State: PdwlHTTPHandlingState): boolean;
begin
  Result := false;
end;

function TdwlHTTPHandler.RemoveLeadUri(const URI: string): string;
begin
  Result := Copy(Uri, FURICutIndex, MaxInt);
end;

procedure TdwlHTTPHandler.SetUri(Value: string);
begin
  FURI := Value;
  FURICutIndex := Length(FUri)+1;
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
  // the values in params are URLEncoded!
  var Params := PServerStructure(State._InternalServerStructure).Request.RequestParams;
  // Keep in mind do not change Value if Result is false
  if Key='*' then
  begin
    FoundStr := Params.Text;
    ValueFound := true;
  end;
  if not ValueFound then
  begin
    if Copy(Key, Length(Key)-1, 2)='[]' then // handle as array!
    begin
      var Vals := '';
      for var i := 0 to Params.Count-1 do
      begin
        if SameText(Params.Names[i], Key) then
          Vals := Vals+','+TNetEncoding.URL.Decode(Params.ValueFromIndex[i]);
      end;
      ValueFound := Vals<>'';
      if ValueFound then
        FoundStr := Copy(Vals, 2, MaxInt);
    end;
  end;
  if not ValueFound then
  begin
    ValueFound := Params.IndexOfName(Key)>=0;
    if ValueFound then
      FoundStr := TNetEncoding.URL.Decode(Params.Values[Key]);
  end;
  if not ValueFound then
  begin
    ValueFound := SameText(Key, SpecialRequestParam_RemoteIP);
    if ValueFound then
      FoundStr := PServerStructure(State._InternalServerStructure).Request.IP_Remote;
  end;
  if not ValueFound then
  begin
    ValueFound := SameText(Key, SpecialRequestParam_Context_Issuer);
    if ValueFound then
    begin
      FoundStr := TdwlHTTPHandler(PServerStructure(State._InternalServerStructure).Handler).FServer.GlobalIssuer;
      if FoundStr='' then
      begin
        var HostName := PServerStructure(State._InternalServerStructure).Request.Context_HostName;
        if HostName='' then // This is a non-ssl localhost connection, for SSl a Context_HosName is altijd non-empty
          FoundStr := 'http://localhost'+EndpointURI_OAuth2
        else
          FoundStr := 'https://'+HostName+EndpointURI_OAuth2
      end;
    end;
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

function State_GetHeaderValue(const State: PdwlHTTPHandlingState; const Key: PWideChar; const Value: PWideChar; var  ValueCharCnt: integer): integer; stdcall;
begin
  var FoundStr := PServerStructure(State._InternalServerStructure).Request.RequestHeaders.StrValue(Key);
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

function State_GetResponseHeaderValue(const State: PdwlHTTPHandlingState; const Key: PWideChar; const Value: PWideChar; var  ValueCharCnt: integer): integer; stdcall;
begin
  var FoundStr := PServerStructure(State._InternalServerStructure).Request.ResponseHeaders.StrValue(Key);
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

function State_GetPostDataPtr(State: PdwlHTTPHandlingState; out Data: pointer; out DataSize: Int64): boolean; stdcall;
begin
  var Stream := PServerStructure(State._InternalServerStructure).Request.RequestBodyStream;
  Result := (Stream is TMemoryStream) and (Stream.Size>0);
  if not Result then
    Exit;
  Data := Stream.Memory;
  DataSize := Stream.Size;
end;

procedure State_SetHeaderValue(const State: PdwlHTTPHandlingState; const HeaderKey, Value: PWideChar); stdcall;
begin
  PServerStructure(State._InternalServerStructure).Request.ResponseHeaders.WriteValue(HeaderKey, Value);
end;

function State_CallService(const State: PdwlHTTPHandlingState; ServiceID: cardinal; const Data: PWideChar): integer; stdcall;
begin
  Result := -1;
  case ServiceID of
  serverservice_SendEMail:
    begin
      var MailMsg := TdwlMailUtils.New_IdMessage;
      try
        TdwlMailUtils.FillIdMessageFromString(MailMsg, Data);
        if TdwlMailQueue.QueueForSending(MailMsg) then
          Result := 1;
      finally
        MailMsg.Free;
      end;
    end;
  serverservice_Log:
    begin
      var JSON := TJSONValue.ParseJSONValue(Data);
      try
        TdwlLogger.Log(
          JSON.GetValue<string>('msg'),
          TdwlLogSeverityLevel(JSON.GetValue<integer>('level')),
          JSON.GetValue<string>('topic'),
          JSON.GetValue<string>('channel'),
          JSON.GetValue<string>('source'));
      finally
        JSON.Free;
      end;
    end;
  end;
end;

procedure AssignServerProcs;
begin
  serverProcs.ArrangeContentBufferProc := State_ArrangeContentBuffer;
  serverProcs.GetRequestParamProc := State_GetRequestParam;
  serverProcs.GetHeaderValueProc := State_GetHeaderValue;
  serverProcs.GetResponseHeaderValueProc := State_GetResponseHeaderValue;
  serverProcs.GetPayloadPtrProc := State_GetPostDataPtr;
  serverProcs.SetHeaderValueProc := State_SetHeaderValue;
//  serverProcs.ActivateWebSocketproc := State_ActivateWebSocket;
  serverProcs.CallServiceProc := State_CallService;
end;

end.
