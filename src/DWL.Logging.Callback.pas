/// <summary>
///   A Logging Dispatcher to post information to a callback function in the application
/// </summary>
unit DWL.Logging.Callback;

interface

uses
  DWL.Logging;

type
  TdwlLogDispatchProc = procedure(LogItem: PdwlLogItem) of object;

/// <summary>
///   Enabled Dispatching to a callback function.
///   Indicate if the Callback needs to be synchronized within the main thread and provide a callback function
///   and if needed an identifier (to receive specific
///   destination logs) <br />
/// </summary>
function EnableLogDispatchingToCallback(AQueueForMainThread: boolean; ALogDispatchProc: TdwlLogDispatchProc; const Identifier: string=''): IdwlLogDispatcher;

implementation

uses
  System.Classes;

type
  TdwlLogDispatcher_CallBack = class(TdwlLogDispatcher)
  strict private
    FOnLog: TdwlLogDispatchProc;
    FQueueForMainThread: boolean;
  protected
    procedure DispatchLog(LogItem: PdwlLogItem); override;
  public
    constructor Create(const Identifier: string; AQueueForMainThread: boolean; AOnLog: TdwlLogDispatchProc);
  end;

function EnableLogDispatchingToCallback(AQueueForMainThread: boolean; ALogDispatchProc: TdwlLogDispatchProc; const Identifier: string=''): IdwlLogDispatcher; overload;
begin
  Result := TdwlLogDispatcher_Callback.Create(Identifier, AQueueForMainThread, ALogDispatchProc);
  TdwlLogger.RegisterDispatcher(Result);
end;

{ TdwlLogDispatcher_CallBack }

constructor TdwlLogDispatcher_CallBack.Create(const Identifier: string; AQueueForMainThread: boolean; AOnLog: TdwlLogDispatchProc);
begin
  inherited Create(Identifier);
  FQueueForMainThread := AQueueForMainThread;
  FOnLog := AOnLog;
end;

procedure TdwlLogDispatcher_CallBack.DispatchLog(LogItem: PdwlLogItem);
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
