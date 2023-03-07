library OAuth2;

{$R *.dres}

uses
  DWL.Server.Types,
  DWL.Server.Globals,
  System.SysUtils,
  OAuth2.Handler in 'units\OAuth2.Handler.pas';

{$R *.res}

function Authorize(const State: PdwlHTTPHandlingState): boolean; stdcall;
begin
  Result := THandler_OAuth2.Authorize(State);
end;

function Configure(const CallBackProcs: PdwlCallBackProcs; const Params: PWideChar): string; stdcall;
begin
  Result := '';
  serverProcs := CallBackProcs^;
  try
    THandler_OAuth2.Configure(Params);
  except
    on E: Exception do
      Result := E.Message;
  end;
end;

function ProcessRequest(const State: PdwlHTTPHandlingState): boolean; stdcall;
begin
  Result := false;
  THandler_OAuth2.ProcessRequest(State, Result);
end;

procedure WrapUp(const State: PdwlHTTPHandlingState);
begin
  THandler_OAuth2.WrapUp(State);
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

