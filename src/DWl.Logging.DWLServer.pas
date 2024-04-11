/// <summary>
///   A Logging Dispatcher to post information directly to the DWLServer
/// </summary>
unit DWl.Logging.DWLServer;

interface

uses
  DWL.Logging, DWL.Server.Types;

/// <summary>
///   Enabled Dispatching directly to the DWLServer
///   The CallServiceProc must be provided
///   and if needed an identifier (to receive specific
///   destination logs) <br />
/// </summary>
function EnableLogDispatchingToDWLServer(const Identifier: string=''): IdwlLogDispatcher;

implementation

uses
  System.JSON, DWL.Server.Globals, DWL.Server.Consts;

type
  TdwlLogDispatcher_DWLServer = class(TdwlLogDispatcher)
  protected
    procedure DispatchLog(LogItem: PdwlLogItem); override;
  end;

function EnableLogDispatchingToDWLServer(const Identifier: string=''): IdwlLogDispatcher;
begin
  Result := TdwlLogDispatcher_DWlServer.Create(Identifier);
  TdwlLogger.RegisterDispatcher(Result);
end;

{ TdwlLogDispatcher_DWLServer }

procedure TdwlLogDispatcher_DWLServer.DispatchLog(LogItem: PdwlLogItem);
begin
  var JSON := TJSONObject.Create;
  try
    JSON.AddPair('msg', LogItem.Msg);
    JSON.AddPair('level', integer(LogItem.SeverityLevel));
    JSON.AddPair('source', LogItem.Source);
    JSON.AddPair('channel', LogItem.Channel);
    JSON.AddPair('topic', LogItem.Topic);
    serverprocs.CallServiceProc(nil, serverservice_Log, pWideChar(JSON.ToJSON));
  finally
    JSON.Free;
  end;
end;

end.
