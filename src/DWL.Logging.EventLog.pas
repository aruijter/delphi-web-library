/// <summary>
///   A Logging Dispatcher to post information to the windows event log
/// </summary>
unit DWL.Logging.EventLog;

interface

uses
  DWL.Logging;

/// <summary>
///   Enabled Dispatching to the windows event log. Give a minimum
///   severityLevel and if needed an identifier (to receive specific
///   destination logs) <br />
/// </summary>
function EnableLogDispatchingToWindowsEventLog(MinimumSeverityLevel: TdwlLogSeverityLevel=lsWarning; const Identifier: string=''): IdwlLogDispatcher;

implementation

uses
  Winapi.Windows, System.SysUtils;

const
  EVENTLOG_TYPE_ERROR = 1;
  EVENTLOG_TYPE_WARNING = 2;
  EVENTLOG_TYPE_INFORMATION= 4;

type
  TdwlLogDispatcher_EventLog = class(TdwlLogDispatcher)
  strict private
    FHandle: THandle;
    FMinimumSeverityLevel: TdwlLogSeverityLevel;
  protected
    procedure DispatchLog(LogItem: PdwlLogItem); override;
  public
    constructor Create(MinimumSeverityLevel: TdwlLogSeverityLevel; const Identifier: string);
    destructor Destroy; override;
  end;

function EnableLogDispatchingToWindowsEventLog(MinimumSeverityLevel: TdwlLogSeverityLevel=lsWarning; const Identifier: string=''): IdwlLogDispatcher;
begin
  Result := TdwlLogDispatcher_EventLog.Create(MinimumSeverityLevel, Identifier);
  TdwlLogger.RegisterDispatcher(Result);
end;

{ TdwlLogDispatcher_EventLog }

constructor TdwlLogDispatcher_EventLog.Create(MinimumSeverityLevel: TdwlLogSeverityLevel; const Identifier: string);
begin
  inherited Create(Identifier);
  FMinimumSeverityLevel := MinimumSeverityLevel;
  FHandle := RegisterEventSource(nil, PWideChar(ChangeFileExt(ExtractFileName(GetModuleName(hInstance)),'')));
end;

destructor TdwlLogDispatcher_EventLog.Destroy;
begin
  DeregisterEventSource(FHandle);
  inherited Destroy;
end;

procedure TdwlLogDispatcher_EventLog.DispatchLog(LogItem: PdwlLogItem);
begin
  if LogItem.SeverityLevel<FMinimumSeverityLevel then
    Exit;
  var lLogType: integer;
  case LogItem.SeverityLevel of
    lsNotice: lLogType := EVENTLOG_TYPE_INFORMATION;
    lsError,
    lsCritical,
    lsFatal: lLogType := EVENTLOG_TYPE_ERROR;
  else
    lLogType := EVENTLOG_TYPE_WARNING;
  end;
  var MessagePtr := PChar(LogItem.Msg);
  ReportEvent(FHandle, lLogType, 0, 0, nil, 1, 0, @MessagePtr, nil);
end;

end.
