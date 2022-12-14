program dwlserver_sv;

uses
  SimpleShareMem,
  SvcMgr,
  fMain in 'forms\fMain.pas' {DWL_Server: TService},
  DWLServer.Consts in '..\units\DWLServer.Consts.pas',
  DWLServer.Section in '..\units\DWLServer.Section.pas',
  DWLServer.Server in '..\units\DWLServer.Server.pas';

{$R *.RES}

begin
  // Windows 2003 Server requires StartServiceCtrlDispatcher to be
  // called before CoRegisterClassObject, which can be called indirectly
  // by Application.Initialize. TServiceApplication.DelayInitialize allows
  // Application.Initialize to be called from TService.Main (after
  // StartServiceCtrlDispatcher has been called).
  //
  // Delayed initialization of the Application object may affect
  // events which then occur prior to initialization, such as
  // TService.OnCreate. It is only recommended if the ServiceApplication
  // registers a class object with OLE and is intended for use with
  // Windows 2003 Server.
  //
  // Application.DelayInitialize := True;
  //
  if not Application.DelayInitialize or Application.Installing then
    Application.Initialize;
  Application.CreateForm(TDWL_Server, DWL_Server);
  Application.Run;
end.
