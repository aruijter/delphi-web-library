library ServeFiles;

uses
  DWL.Server.Globals,
  DWL.Server.Types,
  DWL.Logging,
  System.SysUtils,
  ServeFiles.Handler in 'Units\ServeFiles.Handler.pas';

{$R *.res}

function Authorize(const State: PdwlHTTPHandlingState): boolean; stdcall;
begin
  try
    Result := THandler_ServeFiles.Authorize(State);
  except
    on E: Exception do
    begin
      Result := false;
      TdwlLogger.Log(E);
    end;
  end;
end;

procedure Configure(const CallBackProcs: PdwlCallBackProcs; const Params: PWideChar); stdcall;
begin
  try
    serverProcs := CallBackProcs^;
    THandler_ServeFiles.Configure(Params);
  except
    on E: Exception do
      TdwlLogger.Log(E);
  end;
end;

function ProcessRequest(const State: PdwlHTTPHandlingState): boolean; stdcall;
begin
  Result := false;
  try
    THandler_ServeFiles.ProcessRequest(State, Result);
  except
    on E: Exception do
      TdwlLogger.Log(E);
  end;
end;

procedure WrapUp(const State: PdwlHTTPHandlingState); stdcall;
begin
  try
    THandler_ServeFiles.WrapUp(State);
  except
    on E: Exception do
      TdwlLogger.Log(E);
  end;
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



