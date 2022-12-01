object DiscoUIClientProgressForm: TDiscoUIClientProgressForm
  Left = 756
  Top = 249
  BorderStyle = bsNone
  Caption = '-'
  ClientHeight = 80
  ClientWidth = 320
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg'
  Font.Style = []
  PopupMode = pmAuto
  Position = poMainFormCenter
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 320
    Height = 80
    Align = alClient
    BevelOuter = bvNone
    BevelWidth = 2
    Color = clSkyBlue
    ParentBackground = False
    TabOrder = 0
    object lblInfo: TLabel
      Left = 8
      Top = 6
      Width = 82
      Height = 16
      Caption = 'Initialising...'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'MS Shell Dlg'
      Font.Style = [fsBold]
      ParentFont = False
      Transparent = True
    end
    object Gauge: TGauge
      Left = 16
      Top = 27
      Width = 287
      Height = 18
      Progress = 0
      Visible = False
    end
    object btnCancel: TBitBtn
      Left = 229
      Top = 50
      Width = 75
      Height = 21
      Caption = 'Cancel'
      TabOrder = 0
      OnClick = btnCancelClick
    end
  end
end
