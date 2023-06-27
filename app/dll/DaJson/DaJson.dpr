library DaJson;

uses
  DWL.Server.Types,
  DWL.Server.Globals,
  System.SysUtils,
  DaJson.Handler in 'units\DaJson.Handler.pas';

{$R *.res}

function Authorize(const State: PdwlHTTPHandlingState): boolean; stdcall;
begin
  Result := THandler_DaJson.Authorize(State);
end;

function Configure(const CallBackProcs: PdwlCallBackProcs; const Params: PWideChar): string; stdcall;
begin
  Result := '';
  serverProcs := CallBackProcs^;
  try
    THandler_DaJson.Configure(Params);
  except
    on E: Exception do
      Result := E.Message;
  end;
end;

function ProcessRequest(const State: PdwlHTTPHandlingState): boolean; stdcall;
begin
  Result := false;
  THandler_DaJson.ProcessRequest(State, Result);
end;

procedure WrapUp(const State: PdwlHTTPHandlingState);
begin
  THandler_DaJson.WrapUp(State);
end;

exports
  Authorize,
  Configure,
  ProcessRequest,
  WrapUp;

begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}
end.

