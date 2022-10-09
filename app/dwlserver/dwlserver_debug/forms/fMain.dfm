object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'MainForm'
  ClientHeight = 681
  ClientWidth = 1024
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1024
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 1022
    object btnStart: TButton
      Left = 4
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Start Server'
      TabOrder = 0
      OnClick = btnStartClick
    end
    object btnStop: TButton
      Left = 92
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Stop server'
      TabOrder = 1
      OnClick = btnStopClick
    end
  end
  object Memo1: TMemo
    Left = 0
    Top = 41
    Width = 1024
    Height = 640
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    ExplicitWidth = 1022
    ExplicitHeight = 647
  end
end
