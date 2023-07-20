library DisCo;

uses
  DWL.Server.Globals,
  DWL.Server.Types,
  DWL.Logging,
  System.SysUtils,
  DisCo.Handler in 'units\DisCo.Handler.pas',
  DWL.DisCo.Consts in '..\..\..\src\DWL.DisCo.Consts.pas';

{$R *.res}

function Authorize(const State: PdwlHTTPHandlingState): boolean; stdcall;
begin
  try
    Result := THandler_DisCo.Authorize(State);
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
    THandler_DisCo.Configure(Params);
  except
    on E: Exception do
      TdwlLogger.Log(E);
  end;
end;

function ProcessRequest(const State: PdwlHTTPHandlingState): boolean; stdcall;
begin
  Result := false;
  try
    THandler_DisCo.ProcessRequest(State, Result);
  except
    on E: Exception do
      TdwlLogger.Log(E);
  end;
end;

procedure WrapUp(const State: PdwlHTTPHandlingState); stdcall;
begin
  try
    THandler_DisCo.WrapUp(State);
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



