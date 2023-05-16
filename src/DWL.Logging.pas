/// <summary>
///   The DWL.Logging framework. Simply log through TdwlLogger.Log and via
///   Dispatchers the log can be handled to land whereever you want.
/// </summary>
unit DWL.Logging;

interface

uses
  DWL.Types, System.Sysutils;

type

  /// <summary>
  ///   This enum defines the severity levels with which you can indicate a log
  ///   message
  /// </summary>
  TdwlLogSeverityLevel = (
    /// <summary>
    ///   Severity Not applicable
    /// </summary>
    lsNotSet=0,
    /// <summary>
    ///   Informal messages about details of the processflow to get into the
    ///   inner handling <br />
    /// </summary>
    lsTrace=1,
    /// <summary>
    ///   Informal messages for debugging purposes
    /// </summary>
    lsDebug=2,
    /// <summary>
    ///   mostly Informal messages about porocessflow. No problem indication,
    ///   just progress etc
    /// </summary>
    lsNotice=3,
    /// <summary>
    ///   Not an error but near. Please take note and take action when time
    ///   allows
    /// </summary>
    lsWarning=4,
    /// <summary>
    ///   Errors, like exceptions, unexpected situations, etc
    /// </summary>
    lsError=5,
    /// <summary>
    ///   Very Critical Errors, proces flow is aborted and immediate action is
    ///   required
    /// </summary>
    lsCritical=6,
    /// <summary>
    ///   Fatal Errrors, process flow is aborted
    /// </summary>
    lsFatal=7);

  /// <summary>
  ///   The structure wherin the logitem is stored, normally quite hidden, but
  ///   by using PrepareLogItem() you can modify every detail of the log before
  ///   posting through Log()
  /// </summary>
  PdwlLogItem = ^TdwlLogItem;
  TdwlLogItem = record
    /// <summary>
    ///   Unix timestamp of moment of posting
    /// </summary>
    TimeStamp: TUnixEpoch;
    /// <summary>
    ///   The short main message
    /// </summary>
    Msg: string;
    /// <summary>
    ///   The severitylevel as described in the TdwlLogSeverityLevel
    ///   enumeration
    /// </summary>
    SeverityLevel: TdwlLogSeverityLevel;
    /// <summary>
    ///   The source where the item is born
    /// </summary>
    Source: string;
    /// <summary>
    ///   The Channel on which the message will be delivered, can later on be
    ///   used to f.e. activate trigger or subscribe to
    /// </summary>
    Channel: string;
    /// <summary>
    ///   The Topic that will be used when the message will be delivered, can
    ///   later on be used to f.e. activate trigger or subscribe to
    /// </summary>
    Topic: string;
    /// <summary>
    ///   The MIME contenttype of the additional information
    /// </summary>
    ContentType: string;
    /// <summary>
    ///   Additional content, type described by mimetype
    /// </summary>
    Content: TBytes;
    /// <summary>
    ///   By giveng a specific destination, the item will only be delivered to
    ///   the Dispatchers where Dispatcher.Identifier=LogItem.Destination,
    ///   normally left empty, which will deliver the message to all
    ///   dispatchers
    /// </summary>
    Destination: string; //
  end;

  /// <summary>
  ///   The base class of a dispatcher that listens to log items
  /// </summary>
  IdwlLogDispatcher = interface
    procedure DispatchLog(LogItem: PdwlLogItem);
    function Identifier: string;
  end;

  TdwlLogDispatcher = class(TInterfacedObject, IdwlLogDispatcher)
  private
    FIdentifier: string;
    function Identifier: string;
  protected
    procedure DispatchLog(LogItem: PdwlLogItem); virtual; abstract;
  public
    constructor Create(const Identifier: string);
  end;

  /// <summary>
  ///   Combined record with TdwlLogger functionality
  /// </summary>
  TdwlLogger = record
  private
    class var FHookedRaiseExceptObjProc: TRaiseExceptObjProc;
  public
    /// <summary>
    ///   Default Source to use when LogItem source is not provided, defaults
    ///   to BareModuleName
    /// </summary>
    class function Default_Source: string; static;
    /// <summary>
    ///   When creating a dispatcher, register is here to activate it.
    /// </summary>
    class procedure RegisterDispatcher(Dispatcher: IdwlLogDispatcher); static;
    /// <summary>
    ///   Deactives the given dispatcher
    /// </summary>
    class procedure UnregisterDispatcher(var Dispatcher: IdwlLogDispatcher); static;
    /// <summary>
    ///   When EnableExceptionLogging is called, literally all Exceptions in
    ///   the application will be logged with stack frames.
    /// </summary>
    class procedure EnableExceptionLogging; static;
    /// <summary>
    ///   Posts a log item. Apart from Msg all properties are optional and have
    ///   defaults
    /// </summary>
    class procedure Log(const Msg: string; SeverityLevel: TdwlLogSeverityLevel=lsNotice; const Topic: string=''; Channel: string=''; Source: string=''); overload; static;
    /// <summary>
    ///   Prepares a LogItem structure. After filling you must post is through
    ///   Log(). If not a memory leak will be created
    /// </summary>
    class function PrepareLogitem: PdwlLogItem; static;
    class procedure Log(const LogItem: PdwlLogItem); overload; static;
    /// <summary>
    ///   Posts a prepared logitem obtained before bu PrepareLogItem
    /// </summary>
    class procedure Log(Exc: Exception; SeverityLevel: TdwlLogSeverityLevel=lsFatal; const Topic: string=''; Channel: string=''; Source: string=''); overload; static;
    /// <summary>
    ///   Sets the default Source, Channel and Topic that will be added for
    ///   items when not provided
    ///   These cannot be empty, if an empty string is provided the Source, Channel or Topic
    ///   will stay on the current value
    /// </summary>
    class procedure SetDefaultOrigins(const Source, Channel, Topic: string); static;
    /// <summary>
    ///   Supression of duplicates can be activated. Suppression period can be
    ///   given in ms (one hour default). Duplicity is only recognizes if all
    ///   fields except ContentType/Content are identical
    /// </summary>
    class procedure SetSuppression(DoSuppress: boolean; Duration: UInt64=3600000); static;
    /// <summary>
    ///   class this procedure at the latest moment on application shutdown
    ///   where the application is still 'alive' f.e. API HTTP calls must be
    ///   able to be executed
    /// </summary>
    class procedure FinalizeDispatching; static;
    class function GetSeverityLevelAsString(SeverityLevel: TdwlLogSeverityLevel): string; static;
  end;

  TdwlLogHelper = record
    /// <summary>
    ///   Put a HTML string in the details of a logitem. Contenttype will be
    ///   set. Please make sure that all text is properly escaped (no chars
    ///   &gt;#127)
    /// </summary>
    class procedure AddPureHtmlAsLogDetail(const LogItem: PdwlLogItem; const Html: string); static;
    /// <summary>
    ///   The provided plain text will converted to HTML and added as detail of
    ///   a logitem. Contecttype is taken care of
    /// </summary>
    class procedure AddPlainTextAsHTMLLogDetail(const LogItem: PdwlLogItem; const Text: string; const FontFamily: string='Tahoma'; const FontSize: byte=10); static;
  end;


implementation

uses
  System.Classes, System.Generics.Collections, DWL.SyncObjs, System.NetEncoding,
  {$IFOPT D+}JclDebug,{$ENDIF}
  DWL.Logging.EventLog, System.Hash, DWL.IOUtils;

type
  TLogDispatchThread = class(TThread)
  strict private
    FCleanUpCounter: word;
    function CheckNotSuppressed(LogItem: PdwlLogItem): boolean;
    procedure CheckLogListenerAvailabiity;
  private
    FDispatching: boolean;
    FHashes: TDictionary<integer, UInt64>;
    FSuppressDuplicates: boolean;
    FSuppressDuration: UInt64;
  protected
    procedure Execute; override;
  end;

  TLogEngine = record
  private
    class var
      FLogsQueue: TdwlThreadQueue_Evented<PdwlLogItem>;
      FLogDispatchThread: TLogDispatchThread;
      FLogDispatchers: TdwlThreadList<IdwlLogDispatcher>;
      FDefaultSource: string;
      FDefaultChannel: string;
      FDefaultTopic: string;
    class procedure FinalizeDispatching; static;
  public
    class constructor Create;
    class destructor Destroy;
    class procedure  PostLog(const LogItem: PdwlLogItem); static;
    class function CreateLogItem: PdwlLogItem; static;
  end;

{ TLogEngine }

class constructor TLogEngine.Create;
begin
  FLogsQueue := TdwlThreadQueue_Evented<PdwlLogItem>.Create;
  FLogDispatchThread := TLogDispatchThread.Create;
  FLogDispatchThread.FreeOnTerminate := true;
  FLogDispatchers := TdwlThreadList<IdwlLogDispatcher>.Create;
  FDefaultSource := TdwlFile.ExtractBareName(GetModuleName(hInstance));
  FDefaultChannel := FDefaultSource;
  FDefaultTopic := 'generic';
end;

class function TLogEngine.CreateLogItem: PdwlLogItem;
begin
  New(Result);
  Result.TimeStamp := TUnixEpoch.Now;
end;

class destructor TLogEngine.Destroy;
begin
  // terminate thread
  FLogDispatchThread.Terminate; {it's free on terminate}
  FLogsQueue.Set_Event; {To wake up thread for termination}
  // clean lists
  var Item: PdwlLogItem;
  while FLogsQueue.TryPop(Item) do
    Dispose(Item);
  // free objects
  FLogDispatchers.Free;
  FLogsQueue.Free; {the last one to be quite sure the thread is not using it anymore}
end;

class procedure TLogEngine.FinalizeDispatching;
begin
  // give time to dispatching thread to dispatch all log messages
  for var i := 0 to 30 do
  begin
    if (not FLogDispatchThread.FDispatching) and (FLogsQueue.Count=0) then
      Break;
    Sleep(300);
  end;
end;

class procedure TLogEngine.PostLog(const LogItem: PdwlLogItem);
begin
  if LogItem.Source='' then
    LogItem.Source := FDefaultSource;
  if LogItem.Channel='' then
    LogItem.Channel := FDefaultChannel;
  if LogItem.Topic='' then
    LogItem.Topic := FDefaultTopic;
  FLogsQueue.Push(LogItem);
end;

{ TdwlLogDispatcher }

constructor TdwlLogDispatcher.Create(const Identifier: string);
begin
  inherited Create;
  FIdentifier := Identifier;
end;

function TdwlLogDispatcher.Identifier: string;
begin
  Result := FIdentifier;
end;

{ TdwlLogger }

class procedure TdwlLogger.RegisterDispatcher(Dispatcher: IdwlLogDispatcher);
begin
  TLogEngine.FLogDispatchers.Add(Dispatcher);
end;

class procedure TdwlLogger.UnregisterDispatcher(var Dispatcher: IdwlLogDispatcher);
begin
  TLogEngine.FLogDispatchers.Remove(Dispatcher);
  Dispatcher := nil; // to remove reference
end;

procedure LoggedRaiseExceptObjProc(P: System.PExceptionRecord);
const
  cDelphiException = $0EEDFADE;
  excIsBeingReRaised    = $00000002;
begin
  if Assigned(TdwlLogger.FHookedRaiseExceptObjProc) then
    TdwlLogger.FHookedRaiseExceptObjProc(P);
  if (P.ExceptionCode = cDelphiException) and (P.ExceptObject <> nil) then
    TdwlLogger.Log(Exception(P.ExceptObject));
end;

class function TdwlLogger.Default_Source: string;
begin
  Result := TLogEngine.FDefaultSource;
end;

class procedure TdwlLogger.EnableExceptionLogging;
begin
  FHookedRaiseExceptObjProc := RaiseExceptObjProc;
  RaiseExceptObjProc := @LoggedRaiseExceptObjProc;
end;

class procedure TdwlLogger.FinalizeDispatching;
begin
  TLogEngine.FinalizeDispatching;
end;

class function TdwlLogger.GetSeverityLevelAsString(
  SeverityLevel: TdwlLogSeverityLevel): string;
begin
  case SeverityLevel of
  lsNotSet: Result := 'NOT_SET';
  lsTrace: Result := 'TRACE';
  lsDebug: Result := 'DEBUG';
  lsNotice: Result := 'NOTICE';
  lsWarning: Result := 'WARN';
  lsError: Result := 'ERROR';
  lsCritical: Result := 'CRITICAL';
  lsFatal: Result := 'FATAL';
  else
    Result := '';
  end;
end;

class procedure TdwlLogger.Log(const LogItem: PdwlLogItem);
begin
  TLogEngine.PostLog(LogItem);
end;

class function TdwlLogger.PrepareLogitem: PdwlLogItem;
begin
  Result := TLogEngine.CreateLogItem;
  Result.SeverityLevel := lsNotice;
end;

class procedure TdwlLogger.SetDefaultOrigins(const Source, Channel, Topic: string);
begin
  if Source<>'' then
    TLogEngine.FDefaultSource := Source;
  if Channel<>'' then
    TLogEngine.FDefaultChannel := Channel;
  if Topic<>'' then
    TLogEngine.FDefaultTopic := Topic;
end;

class procedure TdwlLogger.SetSuppression(DoSuppress: boolean; Duration: UInt64=3600000);
begin
  TLogEngine.FLogDispatchThread.FSuppressDuration := Duration;
  TLogEngine.FLogDispatchThread.FSuppressDuplicates := DoSuppress;
end;

class procedure TdwlLogger.Log(const Msg: string; SeverityLevel: TdwlLogSeverityLevel=lsNotice; const Topic: string=''; Channel: string=''; Source: string='');
begin
  var LogItem := TLogEngine.CreateLogItem;
  LogItem.Msg := MSg;
  LogItem.SeverityLevel := SeverityLevel;
  LogItem.Source := Source;
  LogItem.Channel := Channel;
  LogItem.Topic := Topic;
  TLogEngine.PostLog(LogItem);
end;

class procedure TdwlLogger.Log(Exc: Exception; SeverityLevel: TdwlLogSeverityLevel=lsFatal; const Topic: string=''; Channel: string=''; Source: string='');
begin
  var LogItem := TLogEngine.CreateLogItem;
  LogItem.Msg := Exc.Message;
  LogItem.SeverityLevel := SeverityLevel;
  LogItem.Source := Source;
  LogItem.Channel := Channel;
  LogItem.Topic := Topic;
  {$IFOPT D+}
  var S := Exc.StackTrace;
  if S<>'' then
  begin
    LogItem.ContentType := 'plain/text; charset=utf-8';
    LogItem.Content := TEncoding.UTF8.GetBytes(S);
  end;
  {$ENDIF}
  TLogEngine.PostLog(LogItem);
end;

{ TdwlLogHelper }

class procedure TdwlLogHelper.AddPlainTextAsHTMLLogDetail(const LogItem: PdwlLogItem; const Text: string; const FontFamily: string='Tahoma'; const FontSize: byte=10);
begin
  var HTMLTxt := TNetEncoding.HTML.Encode(Text);
  HTMLTxt := StringReplace(HTMLTxt, #13, '', [rfReplaceAll]);
  HTMLTxt := StringReplace(HTMLTxt, #10, '<br />', [rfReplaceAll]);
  AddPureHtmlAsLogDetail(LogItem,
    '<!DOCTYPE html>'#13#10'<html><head><meta charset="utf-8"></head><body style="font-family: '+FontFamily+'; font-size: '+FontSize.ToString+';">'+
    HTMLTxt+'</body></html>');
end;

class procedure TdwlLogHelper.AddPureHtmlAsLogDetail(const LogItem: PdwlLogItem; const Html: string);
begin
  LogItem.ContentType := 'text/html;charset=UTF-8';
  LogItem.Content := TEncoding.UTF8.GetBytes(Html);
end;

{ TLogDispatchThread }

procedure TLogDispatchThread.CheckLogListenerAvailabiity;
begin
  if TLogEngine.FLogDispatchers.Count>0 then
    Exit;
  var FDispatcher := EnableLogDispatchingToWindowsEventLog;
  var LogItem := TdwlLogger.PrepareLogitem;
  try
    LogItem.Msg := 'Logging framework not properly initialized: auto enabled logging to windows event log';
    FDispatcher.DispatchLog(LogItem);
  finally
    Dispose(LogItem);
  end;
end;

function TLogDispatchThread.CheckNotSuppressed(LogItem: PdwlLogItem): boolean;
begin
  Result := not FSuppressDuplicates;
  if Result then
    Exit;
  var Tick := GetTickCount64;
  // remove Old Hashes
  if FCleanUpCounter=0 then
  begin
    var Enum := FHashes.GetEnumerator;
    while ENum.MoveNext do
    begin
      if ENum.Current.Value<Tick then
        FHashes.Remove(ENum.Current.Key);
    end;
    FCleanUpCounter := 1000;
  end;
  dec(FCleanUpCounter);
  var Hash := THashBobJenkins.Create;
  Hash.Update(Logitem.Msg);
  Hash.Update(Logitem.SeverityLevel, SizeOf(LogItem.SeverityLevel));
  Hash.Update(Logitem.Source);
  Hash.Update(Logitem.Channel);
  Hash.Update(Logitem.Topic);
  Hash.Update(Logitem.Destination);
  var LogTick: UInt64;
  Result := not FHashes.TryGetValue(Hash.HashAsInteger, LogTick);
  if not Result then
  begin
    if LogTick<Tick then
    begin
      Result := true;
      FHashes.Remove(Hash.HashAsInteger);
    end;
  end;
  if Result then
    FHashes.Add(Hash.HashAsInteger, Tick+FSuppressDuration);
end;

procedure TLogDispatchThread.Execute;
begin
  FHashes := TDictionary<integer, UInt64>.Create;
  try
    while not Terminated do
    begin
      var Item: PdwlLogItem;
      if TLogEngine.FLogsQueue.WaitForListNotEmpty(INFINITE) then
      begin
        FDispatching := true;
        try
          if not TLogEngine.FLogsQueue.TryPop(Item) then
            Continue;
          if Item.SeverityLevel>=lsWarning then
            CheckLogListenerAvailabiity;
          if CheckNotSuppressed(Item) then
          begin
            var Dispatchers := TLogEngine.FLogDispatchers.LockListRead;
            try
              if Item.Destination='' then
              begin
                for var Dispatcher in Dispatchers do
                  Dispatcher.DispatchLog(Item);
              end
              else
              begin
                for var Dispatcher in Dispatchers do
                begin
                  if Dispatcher.Identifier=Item.Destination then
                    Dispatcher.DispatchLog(Item);
                end;
              end;
            finally
              TLogEngine.FLogDispatchers.UnlockListRead;
            end;
          end;
          Dispose(Item);
        finally
          FDispatching := false;
        end;
      end;
    end;
  finally
    FHashes.Free;
  end;
end;

end.
