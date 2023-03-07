library DisCo;

uses
  DWL.Server.Globals,
  DWL.Server.Types,
  System.SysUtils,
  DisCo.Handler in 'units\DisCo.Handler.pas',
  DWL.DisCo.Consts in '..\..\..\src\DWL.DisCo.Consts.pas';

{$R *.res}

function Authorize(const State: PdwlHTTPHandlingState): boolean; stdcall;
begin
  Result := THandler_DisCo.Authorize(State);
end;

function Configure(const CallBackProcs: PdwlCallBackProcs; const Params: PWideChar): string; stdcall;
begin
  Result := '';
  serverProcs := CallBackProcs^;
  try
    THandler_DisCo.Configure(Params);
  except
    on E: Exception do
      Result := E.Message;
  end;
end;

function ProcessRequest(const State: PdwlHTTPHandlingState): boolean; stdcall;
begin
  Result := false;
  THandler_DisCo.ProcessRequest(State, Result);
end;

procedure WrapUp(const State: PdwlHTTPHandlingState);
begin
  THandler_DisCo.WrapUp(State);
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



