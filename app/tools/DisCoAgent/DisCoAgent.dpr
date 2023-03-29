program DisCoAgent;

uses
  Vcl.Forms,
  fMain in 'forms\fMain.pas' {MainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
