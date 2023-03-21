object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'DisCo'
  ClientHeight = 811
  ClientWidth = 957
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 20
  object Label5: TLabel
    Left = 154
    Top = 65
    Width = 11
    Height = 20
    Caption = 'D'
  end
  object pnlMain: TPanel
    Left = 0
    Top = 0
    Width = 957
    Height = 811
    Align = alClient
    BevelOuter = bvNone
    Color = clWhite
    Ctl3D = True
    DoubleBuffered = False
    ParentBackground = False
    ParentCtl3D = False
    ParentDoubleBuffered = False
    TabOrder = 0
    ExplicitWidth = 953
    ExplicitHeight = 810
    object Splitter1: TSplitter
      Left = 310
      Top = 0
      Width = 5
      Height = 811
      Beveled = True
      ExplicitLeft = 198
    end
    object pnlLeft: TPanel
      Left = 0
      Top = 0
      Width = 310
      Height = 811
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      object pnlLeftTop: TPanel
        Left = 0
        Top = 0
        Width = 310
        Height = 35
        Align = alTop
        BevelOuter = bvNone
        ParentColor = True
        TabOrder = 0
        ExplicitWidth = 198
        object lblPackages: TLabel
          Left = 8
          Top = 8
          Width = 60
          Height = 20
          Caption = 'Packages'
        end
        object Button3: TButton
          Left = 232
          Top = 4
          Width = 72
          Height = 25
          Action = aiSaveCSV
          TabOrder = 0
        end
      end
      object lbPackages: TListBox
        Left = 0
        Top = 35
        Width = 310
        Height = 776
        Align = alClient
        ItemHeight = 20
        TabOrder = 1
        TabWidth = 100
        OnClick = lbPackagesClick
      end
    end
    object pnlRight: TPanel
      Left = 315
      Top = 0
      Width = 642
      Height = 811
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitLeft = 203
      ExplicitWidth = 750
      ExplicitHeight = 810
      object pnlRightTop: TPanel
        Left = 0
        Top = 0
        Width = 642
        Height = 35
        Align = alTop
        BevelOuter = bvNone
        Color = clInfoBk
        ParentBackground = False
        TabOrder = 0
        ExplicitWidth = 750
        object lblPackageName: TLabel
          Left = 6
          Top = 7
          Width = 101
          Height = 20
          Caption = '<no package>'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -15
          Font.Name = 'Segoe UI'
          Font.Style = [fsBold]
          ParentFont = False
        end
      end
      object pnlReleases: TPanel
        Left = 0
        Top = 35
        Width = 642
        Height = 776
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        Visible = False
        ExplicitWidth = 750
        ExplicitHeight = 775
        object Panel1: TPanel
          Left = 0
          Top = 0
          Width = 198
          Height = 776
          Align = alLeft
          BevelOuter = bvNone
          TabOrder = 0
          ExplicitHeight = 775
          object Panel2: TPanel
            Left = 0
            Top = 0
            Width = 198
            Height = 35
            Align = alTop
            BevelOuter = bvNone
            Color = clInfoBk
            ParentBackground = False
            TabOrder = 0
            object Label1: TLabel
              Left = 8
              Top = 8
              Width = 57
              Height = 20
              Caption = 'Releases'
            end
          end
          object lbReleases: TListBox
            Left = 0
            Top = 35
            Width = 198
            Height = 741
            Align = alClient
            ItemHeight = 20
            TabOrder = 1
            OnClick = lbPackagesClick
          end
        end
        object pnlRelease: TPanel
          Left = 198
          Top = 0
          Width = 444
          Height = 776
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 1
          ExplicitWidth = 552
          ExplicitHeight = 775
          object Label2: TLabel
            Left = 18
            Top = 57
            Width = 122
            Height = 20
            Alignment = taRightJustify
            AutoSize = False
            Caption = 'Release moment:'
          end
          object Label3: TLabel
            Left = 18
            Top = 83
            Width = 122
            Height = 20
            Alignment = taRightJustify
            AutoSize = False
            Caption = 'Soort:'
          end
          object Label4: TLabel
            Left = 18
            Top = 109
            Width = 122
            Height = 20
            Alignment = taRightJustify
            AutoSize = False
            Caption = 'Extension:'
          end
          object lblMoment: TLabel
            Left = 146
            Top = 57
            Width = 11
            Height = 20
            Caption = 'D'
          end
          object lblSoort: TLabel
            Left = 146
            Top = 83
            Width = 11
            Height = 20
            Caption = 'D'
          end
          object lblExtension: TLabel
            Left = 146
            Top = 109
            Width = 11
            Height = 20
            Caption = 'D'
          end
          object Button1: TButton
            Left = 146
            Top = 160
            Width = 97
            Height = 25
            Action = aiDownload
            TabOrder = 0
          end
          object Button2: TButton
            Left = 146
            Top = 199
            Width = 97
            Height = 25
            Action = aiUpload
            TabOrder = 1
          end
          object Panel3: TPanel
            Left = 0
            Top = 0
            Width = 444
            Height = 35
            Align = alTop
            BevelOuter = bvNone
            Color = clInfoBk
            ParentBackground = False
            TabOrder = 2
            ExplicitWidth = 552
            object lblRelease: TLabel
              Left = 6
              Top = 9
              Width = 93
              Height = 20
              Caption = '<no release>'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -15
              Font.Name = 'Segoe UI'
              Font.Style = [fsBold]
              ParentFont = False
            end
          end
        end
      end
    end
  end
  object ApplicationEvents: TApplicationEvents
    OnIdle = ApplicationEventsIdle
    Left = 67
    Top = 59
  end
  object ActionList: TActionList
    Left = 72
    Top = 128
    object aiDownload: TAction
      Caption = 'Download'
      OnExecute = aiDownloadExecute
    end
    object aiUpload: TAction
      Caption = 'Upload'
      OnExecute = aiUploadExecute
    end
    object aiSaveCSV: TAction
      Caption = '->CSV'
      OnExecute = aiSaveCSVExecute
    end
  end
  object OpenDialog: TOpenDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 809
    Top = 99
  end
  object SaveDialog: TSaveDialog
    Options = [ofOverwritePrompt, ofHideReadOnly, ofNoDereferenceLinks, ofEnableSizing]
    Left = 809
    Top = 163
  end
end
