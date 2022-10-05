library HelloWorld;

uses
  SimpleShareMem,
  DWL.HTTP.Server.Globals,
  DWL.HTTP.Server.Types,
  System.SysUtils,
  HelloWorld.Handler in 'Units\HelloWorld.Handler.pas';

{$R *.res}

function Authenticate(const State: PdwlHTTPHandlingState): boolean; stdcall;
begin
  Result := THandler_HelloWorld.Authenticate(State);
end;

function Configure(const CallBackProcs: PdwlCallBackProcs; const Params: string): string; stdcall;
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
  Authenticate,
  Configure,
  ProcessRequest,
  WrapUp;

begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}
end.
