object PublishForm: TPublishForm
  Left = 147
  Top = 108
  BorderStyle = bsDialog
  Caption = 'Publish a Release'
  ClientHeight = 363
  ClientWidth = 497
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Shell Dlg'
  Font.Style = []
  Position = poMainFormCenter
  TextHeight = 16
  object pnlFinish: TPanel
    Left = 0
    Top = 0
    Width = 497
    Height = 363
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 493
    ExplicitHeight = 362
    object Panel6: TPanel
      Left = 0
      Top = 0
      Width = 497
      Height = 2
      Align = alTop
      BevelOuter = bvNone
      Color = clSkyBlue
      ParentBackground = False
      TabOrder = 0
      ExplicitWidth = 493
    end
    object btnRelease: TButton
      Left = 272
      Top = 320
      Width = 115
      Height = 25
      Caption = 'Publish Now!'
      Enabled = False
      TabOrder = 1
      OnClick = btnReleaseClick
    end
    object btnCancel: TButton
      Left = 408
      Top = 320
      Width = 75
      Height = 25
      Caption = 'Cancel'
      TabOrder = 2
      OnClick = btnCancelClick
    end
    object reMsg: TRichEdit
      Left = 10
      Top = 8
      Width = 473
      Height = 289
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Shell Dlg'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
    end
  end
  object ApplicationEvents1: TApplicationEvents
    OnIdle = ApplicationEvents1Idle
    Left = 320
    Top = 32
  end
end
