object FormMain: TFormMain
  Left = 803
  Top = 227
  Width = 600
  Height = 435
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'Proj.4 Delphi'
  Color = clBtnFace
  Constraints.MaxHeight = 435
  Constraints.MaxWidth = 600
  Constraints.MinHeight = 435
  Constraints.MinWidth = 600
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object pnlSpacer: TPanel
    Left = 0
    Top = 376
    Width = 592
    Height = 32
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object btnQuit: TButton
      Left = 512
      Top = 4
      Width = 75
      Height = 25
      Caption = 'Quit'
      TabOrder = 0
      OnClick = btnQuitClick
    end
  end
  object pgcBody: TPageControl
    Left = 0
    Top = 0
    Width = 592
    Height = 376
    ActivePage = tabWork
    Align = alClient
    TabOrder = 1
    TabPosition = tpBottom
    OnChange = pgcBodyChange
    object tabDemo: TTabSheet
      Caption = 'Demo'
      inline fraPlayMode: TFramePlayMode
        Left = 0
        Top = 0
        Width = 584
        Height = 350
        Align = alClient
        TabOrder = 0
        inherited pnlBody: TPanel
          Width = 584
          Height = 350
        end
      end
    end
    object tabWork: TTabSheet
      Caption = 'Work'
      ImageIndex = 1
      inline fraWorkMode: TFrameWorkMode
        Left = 0
        Top = 0
        Width = 584
        Height = 350
        Align = alClient
        TabOrder = 0
        inherited pnlBody: TPanel
          Width = 584
          Height = 350
        end
      end
    end
  end
end
