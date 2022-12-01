unit DWL.DisCo.UIClient.ProgressForm;

interface

uses
  Vcl.Forms, Vcl.StdCtrls, Vcl.Buttons, Vcl.Samples.Gauges, Vcl.Controls,
  System.Classes, Vcl.ExtCtrls;

type
  TDiscoUIClientProgressForm = class(TForm)
    Panel1: TPanel;
    lblInfo: TLabel;
    Gauge: TGauge;
    btnCancel: TBitBtn;
    procedure btnCancelClick(Sender: TObject);
  strict private
    FCancelled: boolean;
  public
    property Cancelled: boolean read FCancelled write FCancelled;
    procedure Info(const S: string);
    procedure Progress(BytesReceived, TotalBytes: cardinal);
  end;

implementation

{$R *.dfm}

{ TDiscoUIClientProgressForm }

procedure TDiscoUIClientProgressForm.Info(const S: string);
begin
  lblInfo.Caption := S;
  lblInfo.Update;
end;

procedure TDiscoUIClientProgressForm.btnCancelClick(Sender: TObject);
begin
  FCancelled := true;
end;

procedure TDiscoUIClientProgressForm.Progress(BytesReceived, TotalBytes: cardinal);
begin
  Gauge.Visible := TotalBytes>0;
  if TotalBytes>0 then
  begin
    Gauge.Progress := trunc(BytesReceived/TotalBytes*100);
    Gauge.Update;
  end;
end;

end.


