unit DWL.UI.Utils;

interface

uses
  DWL.Classes;

type
  TdwlUIUtils = record
  strict private
  public
    class procedure Result_Feedback(Res: TdwlResult); static;
  end;

implementation

uses
  Vcl.Dialogs, System.UITypes, System.SysUtils, System.Math;



{ TdwlUIUtils }

class procedure TdwlUIUtils.Result_Feedback(Res: TdwlResult);
begin
  var Msg := Res.ErrorMsg;
  if Msg.IsEmpty then
    Exit;
  var DlgType := TMsgDlgType.mtWarning;
  if Res.Success then
    DlgType := TMsgDlgType.mtInformation;
  MessageDlg(Msg, DlgType, [TMsgDlgBtn.mbOK], 0);
end;

end.
