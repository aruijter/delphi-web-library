unit fMain;

interface

uses
  Vcl.SvcMgr, DWLServer.Section;

type
  TDWL_Server = class(TService)
    procedure ServiceAfterInstall(Sender: TService);
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
  strict private
    FDWLServerSection: TDWLServerSection;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function GetServiceController: TServiceController; override;
  end;

var
  DWL_Server: TDWL_Server;

implementation

{$R *.DFM}

uses
  WinApi.Windows, System.Win.Registry, System.SysUtils;

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  DWL_Server.Controller(CtrlCode);
end;

procedure TDWL_Server.AfterConstruction;
begin
  inherited AfterConstruction;
  FDWLServerSection := TDWLServerSection.Create;
end;

procedure TDWL_Server.BeforeDestruction;
begin
  FDWLServerSection.Free;
  inherited BeforeDestruction;
end;

function TDWL_Server.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

procedure TDWL_Server.ServiceAfterInstall(Sender: TService);
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create(KEY_READ or KEY_WRITE);
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKey('\SYSTEM\CurrentControlSet\Services\'+Name, false) then
    begin
      Reg.WriteString('Description', 'DWLServer, serving handlers residing in DLLs');
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

procedure TDWL_Server.ServiceStart(Sender: TService; var Started: Boolean);
begin
  try
    FDWLServerSection.StartServer;
  except
  end;
end;

procedure TDWL_Server.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  try
    FDWLServerSection.StopServer;
  except
  end;
end;

end.
