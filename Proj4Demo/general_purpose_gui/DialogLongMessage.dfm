object DialogLongMessage: TDialogLongMessage
  Left = 691
  Top = 54
  Width = 552
  Height = 682
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object mmoLongMessage: TMemo
    Left = 10
    Top = 41
    Width = 534
    Height = 565
    Align = alClient
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object pnlBase: TPanel
    Left = 0
    Top = 606
    Width = 544
    Height = 49
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object pnlButtons: TPanel
      Left = 152
      Top = 8
      Width = 193
      Height = 33
      BevelOuter = bvNone
      TabOrder = 0
      object btnOK: TButton
        Left = 8
        Top = 8
        Width = 81
        Height = 25
        Caption = 'OK'
        Default = True
        TabOrder = 0
        OnClick = btnOKClick
      end
      object btnCopy: TButton
        Left = 104
        Top = 8
        Width = 81
        Height = 25
        Caption = 'Copy message'
        TabOrder = 1
        OnClick = btnCopyClick
      end
    end
  end
  object pnlHeader: TPanel
    Left = 0
    Top = 0
    Width = 544
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
  end
  object pnlLeftMargin: TPanel
    Left = 0
    Top = 41
    Width = 10
    Height = 565
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 3
  end
end
