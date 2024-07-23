object dwlOAuth2LoginForm: TdwlOAuth2LoginForm
  Left = 0
  Top = 0
  BorderStyle = bsNone
  Caption = 'Login'
  ClientHeight = 683
  ClientWidth = 496
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poMainFormCenter
  TextHeight = 20
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 496
    Height = 683
    Align = alClient
    BorderStyle = bsSingle
    Caption = 'Panel1'
    TabOrder = 0
    object Browser: TWebBrowser
      Left = 1
      Top = 52
      Width = 490
      Height = 578
      Align = alClient
      TabOrder = 0
      OnBeforeNavigate2 = BrowserBeforeNavigate2
      OnNavigateComplete2 = BrowserNavigateComplete2
      ExplicitWidth = 440
      ExplicitHeight = 519
      ControlData = {
        4C000000A5320000BD3B00000000000000000000000000000000000000000000
        000000004C000000000000000000000001000000E0D057007335CF11AE690800
        2B2E126208000000000000004C0000000114020000000000C000000000000046
        8000000000000000000000000000000000000000000000000000000000000000
        00000000000000000100000000000000000000000000000000000000}
    end
    object Panel2: TPanel
      Left = 1
      Top = 630
      Width = 490
      Height = 48
      Align = alBottom
      BevelOuter = bvNone
      Color = clWindow
      ParentBackground = False
      TabOrder = 1
      DesignSize = (
        490
        48)
      object cxButton1: TButton
        Left = 320
        Top = 8
        Width = 152
        Height = 30
        Anchors = [akTop, akRight]
        Caption = 'Cancel login'
        ModalResult = 2
        TabOrder = 0
      end
    end
    object Panel3: TPanel
      Left = 1
      Top = 1
      Width = 490
      Height = 51
      Align = alTop
      BevelOuter = bvNone
      Color = clWindow
      Padding.Left = 5
      Padding.Top = 5
      ParentBackground = False
      TabOrder = 2
      object cxLabel1: TLabel
        Left = 5
        Top = 5
        Width = 485
        Height = 46
        Align = alClient
        AutoSize = False
        Caption = 'Please login'
        WordWrap = True
      end
    end
  end
end
