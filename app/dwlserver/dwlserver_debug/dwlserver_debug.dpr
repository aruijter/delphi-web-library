program dwlserver_debug;

uses
  Vcl.Forms,
  {$IFOPT D+}
  DWL.Logging,
  {$ENDIF}
  fMain in 'Forms\fMain.pas' {MainForm},
  DWLServer.Section in '..\units\DWLServer.Section.pas';

{$R *.res}

begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  {$IFOPT D+}
  TdwlLogger.EnableExceptionLogging;
  {$ENDIF}
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
