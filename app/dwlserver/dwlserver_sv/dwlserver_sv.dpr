program dwlserver_sv;

uses
  SvcMgr,
  {$IFOPT D+}
  DWL.Logging,
  {$ENDIF}
  fMain in 'forms\fMain.pas' {DWL_Server: TService},
  DWLServer.Section in '..\units\DWLServer.Section.pas';

{$R *.RES}

begin
  if not Application.DelayInitialize or Application.Installing then
    Application.Initialize;
  {$IFOPT D+}
  TdwlLogger.EnableExceptionLogging;
  {$ENDIF}
  Application.CreateForm(TDWL_Server, DWL_Server);
  Application.Run;
end.

