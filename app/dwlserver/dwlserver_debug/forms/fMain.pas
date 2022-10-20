unit fMain;

interface

uses
  Vcl.Forms, System.Classes, Vcl.Controls, Vcl.StdCtrls, DWLServer.Section,
  Vcl.ExtCtrls, DWL.Logging;

type
  TMainForm = class(TForm)
    Panel1: TPanel;
    Memo1: TMemo;
    btnStart: TButton;
    btnStop: TButton;
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
  strict private
    FDWLServerSection: TDWLServerSection;
    FLogDispatcher: IdwlLogDispatcher;
    procedure DoLog(LogItem: PdwlLogItem);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

var
  MainForm: TMainForm;

implementation

uses
  System.SysUtils, System.DateUtils, DWL.HTTP.Server.Handler.Log,
  DWL.Logging.Callback;

{$R *.dfm}

{ TMainForm }

procedure TMainForm.AfterConstruction;
begin
  inherited AfterConstruction;
  FLogDispatcher := EnableLogDispatchingToCallback(true, DoLog, logdestinationServerConsole);
  FDWLServerSection := TDWLServerSection.Create;
  FDWLServerSection.StartServer;
end;

procedure TMainForm.BeforeDestruction;
begin
  TdwlLogger.UnregisterDispatcher(FLogDispatcher);
  FDWLServerSection.Free;
  inherited BeforeDestruction;
end;

procedure TMainForm.btnStartClick(Sender: TObject);
begin
  FDWLServerSection.StartServer;
end;

procedure TMainForm.btnStopClick(Sender: TObject);
begin
  FDWLServerSection.StopServer;
end;

procedure TMainForm.DoLog(LogItem: PdwlLogItem);
begin
  if not (csDestroying in ComponentState) then
    Memo1.Lines.Add(
      FormatDateTime('dd-mm-yyyy hh:nn:ss', UnixToDateTime(LogItem.TimeStamp, false))+'  '+
      LogItem.Source.PadRight(17)+
      LogItem.Channel.PadRight(17)+
      LogItem.Topic.PadRight(17)+
      LogItem.Msg);
end;

end.
