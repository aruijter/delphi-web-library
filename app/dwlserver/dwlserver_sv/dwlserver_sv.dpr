program dwlserver_sv;

uses
  SvcMgr,
  DWL.Logging,
  fMain in 'forms\fMain.pas' {DWL_Server: TService},
  DWLServer.Section in '..\units\DWLServer.Section.pas';

{$R *.RES}

begin
  {$IFOPT D+}
  TdwlLogger.EnableExceptionLogging;
  {$ENDIF}
  if not Application.DelayInitialize or Application.Installing then
    Application.Initialize;
  Application.CreateForm(TDWL_Server, DWL_Server);
  Application.Run;
end.

