library OAuth2;

{$R *.dres}

uses
  DWL.Server.Types,
  DWL.Server.Globals,
  DWL.Logging,
  System.SysUtils,
  OAuth2.Handler in 'units\OAuth2.Handler.pas';

{$R *.res}

function Authorize(const State: PdwlHTTPHandlingState): boolean; stdcall;
begin
  try
    Result := THandler_OAuth2.Authorize(State);
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
    THandler_OAuth2.Configure(Params);
  except
    on E: Exception do
      TdwlLogger.Log(E);
  end;
end;

function ProcessRequest(const State: PdwlHTTPHandlingState): boolean; stdcall;
begin
  Result := false;
  try
    THandler_OAuth2.ProcessRequest(State, Result);
  except
    on E: Exception do
      TdwlLogger.Log(E);
  end;
end;

procedure WrapUp(const State: PdwlHTTPHandlingState); stdcall;
begin
  try
    THandler_OAuth2.WrapUp(State);
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

