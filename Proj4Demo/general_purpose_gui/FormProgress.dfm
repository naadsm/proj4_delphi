object FormProgress: TFormProgress
  Left = 238
  Top = 28
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Form1'
  ClientHeight = 205
  ClientWidth = 370
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pnlBase: TPanel
    Left = 0
    Top = 168
    Width = 370
    Height = 37
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object pnlCtrlButtons: TPanel
      Left = 72
      Top = 0
      Width = 97
      Height = 25
      BevelOuter = bvNone
      TabOrder = 0
      object btnCancel: TButton
        Left = 8
        Top = 0
        Width = 73
        Height = 25
        Cancel = True
        Caption = 'Cancel'
        Enabled = False
        TabOrder = 0
        OnClick = btnCancelClick
      end
    end
  end
  object pnlMessage: TPanel
    Left = 0
    Top = 0
    Width = 370
    Height = 33
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object lblMessage: TLabel
      Left = 40
      Top = 8
      Width = 53
      Height = 13
      Caption = 'lblMessage'
    end
  end
  object pnlDoubleBar: TPanel
    Left = 200
    Top = 41
    Width = 370
    Height = 142
    BevelOuter = bvNone
    TabOrder = 2
    Visible = False
    object lblPrimary: TLabel
      Left = 32
      Top = 16
      Width = 74
      Height = 13
      Caption = 'Stage progress:'
    end
    object lblSecondary: TLabel
      Left = 32
      Top = 76
      Width = 79
      Height = 13
      Caption = 'Overall progress:'
    end
    object pbrSecondary: TProgressBar
      Left = 32
      Top = 96
      Width = 289
      Height = 25
      TabOrder = 0
    end
    object pbrPrimary: TProgressBar
      Left = 32
      Top = 40
      Width = 289
      Height = 25
      Align = alCustom
      TabOrder = 1
    end
  end
  object pnlCounter: TPanel
    Left = 32
    Top = 56
    Width = 161
    Height = 81
    BevelOuter = bvNone
    TabOrder = 3
    Visible = False
    object lblCounter: TLabel
      Left = 56
      Top = 16
      Width = 71
      Height = 16
      Caption = 'lblCounter'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
end
