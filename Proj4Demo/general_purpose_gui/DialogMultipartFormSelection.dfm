object DialogMultipartFormSelection: TDialogMultipartFormSelection
  Left = 509
  Top = 171
  Width = 416
  Height = 355
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  Scaled = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  inline fraFileSelector: TFrameFileSelector
    Left = 0
    Top = 0
    Width = 408
    Height = 65
    Align = alTop
    TabOrder = 0
    inherited SaveDialog1: TSaveDialog
      Left = 134
    end
  end
  object pnlPartsList: TPanel
    Left = 0
    Top = 65
    Width = 408
    Height = 263
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object gbxItems: TGroupBox
      Left = 16
      Top = 0
      Width = 369
      Height = 217
      Caption = 'Items to _____: '
      TabOrder = 0
    end
    object pnlButtons: TPanel
      Left = 0
      Top = 224
      Width = 408
      Height = 39
      Align = alBottom
      TabOrder = 1
      object btnCancel: TButton
        Left = 320
        Top = 8
        Width = 75
        Height = 25
        Cancel = True
        Caption = 'Cancel'
        TabOrder = 0
        OnClick = btnCancelClick
      end
      object btnOK: TButton
        Left = 232
        Top = 8
        Width = 75
        Height = 25
        Caption = 'OK'
        Default = True
        TabOrder = 1
        OnClick = btnOKClick
      end
    end
  end
end
