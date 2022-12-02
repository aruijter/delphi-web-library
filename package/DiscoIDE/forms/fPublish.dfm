object PublishForm: TPublishForm
  Left = 147
  Top = 108
  BorderStyle = bsDialog
  Caption = 'Publish a Release'
  ClientHeight = 385
  ClientWidth = 749
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
    Width = 749
    Height = 385
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 745
    ExplicitHeight = 384
    DesignSize = (
      749
      385)
    object Panel6: TPanel
      Left = 0
      Top = 0
      Width = 749
      Height = 2
      Align = alTop
      BevelOuter = bvNone
      Color = clSkyBlue
      ParentBackground = False
      TabOrder = 0
      ExplicitWidth = 745
    end
    object btnRelease: TButton
      Left = 508
      Top = 342
      Width = 115
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Publish Now!'
      Enabled = False
      TabOrder = 1
      OnClick = btnReleaseClick
      ExplicitLeft = 504
      ExplicitTop = 341
    end
    object btnCancel: TButton
      Left = 644
      Top = 342
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = 'Cancel'
      TabOrder = 2
      OnClick = btnCancelClick
      ExplicitLeft = 640
      ExplicitTop = 341
    end
    object lbMessages: TListBox
      Left = 13
      Top = 8
      Width = 722
      Height = 308
      Style = lbOwnerDrawFixed
      TabOrder = 3
      OnDrawItem = lbMessagesDrawItem
    end
  end
  object ApplicationEvents1: TApplicationEvents
    OnIdle = ApplicationEvents1Idle
    Left = 320
    Top = 32
  end
end
