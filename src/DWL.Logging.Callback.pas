/// <summary>
///   A Logging Dispatcher to post information to a callback function in the application
/// </summary>
unit DWL.Logging.Callback;

interface

uses
  DWL.Logging;

type
  TdwlLogDispatchObjectProc = procedure(LogItem: PdwlLogItem) of object;
  TdwlLogDispatchRegularProc = procedure(LogItem: PdwlLogItem);

/// <summary>
///   Enabled Dispatching to a callback function.
///   Indicate if the Callback needs to be synchronized within the main thread and provide a callback function
///   and if needed an identifier (to receive specific
///   destination logs) <br />
/// </summary>
function EnableLogDispatchingToCallback(AQueueForMainThread: boolean; ALogDispatchProc: TdwlLogDispatchObjectProc; const Identifier: string=''): IdwlLogDispatcher; overload;
function EnableLogDispatchingToCallback(AQueueForMainThread: boolean; ALogDispatchProc: TdwlLogDispatchRegularProc; const Identifier: string=''): IdwlLogDispatcher; overload;

implementation

uses
  System.Classes;

type
  TdwlLogDispatcher_CallBack_Object = class(TdwlLogDispatcher)
  strict private
    FOnLog: TdwlLogDispatchObjectProc;
    FQueueForMainThread: boolean;
  protected
    procedure DispatchLog(LogItem: PdwlLogItem); override;
  public
    constructor Create(const Identifier: string; AQueueForMainThread: boolean; AOnLog: TdwlLogDispatchObjectProc);
  end;

  TdwlLogDispatcher_CallBack_Regular = class(TdwlLogDispatcher)
  strict private
    FOnLog: TdwlLogDispatchRegularProc;
    FQueueForMainThread: boolean;
  protected
    procedure DispatchLog(LogItem: PdwlLogItem); override;
  public
    constructor Create(const Identifier: string; AQueueForMainThread: boolean; AOnLog: TdwlLogDispatchRegularProc);
  end;


function EnableLogDispatchingToCallback(AQueueForMainThread: boolean; ALogDispatchProc: TdwlLogDispatchObjectProc; const Identifier: string=''): IdwlLogDispatcher; overload;
begin
  Result := TdwlLogDispatcher_CallBack_Object.Create(Identifier, AQueueForMainThread, ALogDispatchProc);
  TdwlLogger.RegisterDispatcher(Result);
end;

function EnableLogDispatchingToCallback(AQueueForMainThread: boolean; ALogDispatchProc: TdwlLogDispatchRegularProc; const Identifier: string=''): IdwlLogDispatcher; overload;
begin
  Result := TdwlLogDispatcher_CallBack_Regular.Create(Identifier, AQueueForMainThread, ALogDispatchProc);
  TdwlLogger.RegisterDispatcher(Result);
end;

{ TdwlLogDispatcher_CallBack_Object }

constructor TdwlLogDispatcher_CallBack_Object.Create(const Identifier: string; AQueueForMainThread: boolean; AOnLog: TdwlLogDispatchObjectProc);
begin
  inherited Create(Identifier);
  FQueueForMainThread := AQueueForMainThread;
  FOnLog := AOnLog;
end;

procedure TdwlLogDispatcher_CallBack_Object.DispatchLog(LogItem: PdwlLogItem);
begin
  if FQueueForMainThread then
  begin
    // create a copy for use in another thread
    var NewItem := TdwlLogger.PrepareLogitem;
    NewItem^ := LogItem^;
    TThread.Queue(nil, procedure
      begin
        FOnLog(NewItem);
        Dispose(NewItem);
      end);
  end
  else
    FOnLog(LogItem);
end;

{ TdwlLogDispatcher_CallBack_Regular }

constructor TdwlLogDispatcher_CallBack_Regular.Create(const Identifier: string; AQueueForMainThread: boolean; AOnLog: TdwlLogDispatchRegularProc);
begin
  inherited Create(Identifier);
  FQueueForMainThread := AQueueForMainThread;
  FOnLog := AOnLog;
end;

procedure TdwlLogDispatcher_CallBack_Regular.DispatchLog(LogItem: PdwlLogItem);
begin
  if FQueueForMainThread then
  begin
    // create a copy for use in another thread
    var NewItem := TdwlLogger.PrepareLogitem;
    NewItem^ := LogItem^;
    TThread.Queue(nil, procedure
      begin
        FOnLog(NewItem);
        Dispose(NewItem);
      end);
  end
  else
    FOnLog(LogItem);
end;

end.
