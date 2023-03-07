library HelloWorld;

uses
  DWL.Server.Globals,
  DWL.Server.Types,
  System.SysUtils,
  HelloWorld.Handler in 'units\HelloWorld.Handler.pas';

{$R *.res}

function Authorize(const State: PdwlHTTPHandlingState): boolean; stdcall;
begin
  Result := THandler_HelloWorld.Authorize(State);
end;

function Configure(const CallBackProcs: PdwlCallBackProcs; const Params: PWideChar): string; stdcall;
begin
  Result := '';
  serverProcs := CallBackProcs^;
  try
    THandler_HelloWorld.Configure(Params);
  except
    on E: Exception do
      Result := E.Message;
  end;
end;

function ProcessRequest(const State: PdwlHTTPHandlingState): boolean; stdcall;
begin
  Result := false;
  THandler_HelloWorld.ProcessRequest(State, Result);
end;

procedure WrapUp(const State: PdwlHTTPHandlingState);
begin
  THandler_HelloWorld.WrapUp(State);
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



