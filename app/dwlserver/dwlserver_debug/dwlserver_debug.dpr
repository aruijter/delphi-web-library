program dwlserver_debug;

uses
  Vcl.Forms,
  fMain in 'Forms\fMain.pas' {MainForm},
  DWLServer.Section in '..\units\DWLServer.Section.pas';

{$R *.res}

begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
