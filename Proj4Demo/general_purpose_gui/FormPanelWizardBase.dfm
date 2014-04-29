object FormPanelWizardBase: TFormPanelWizardBase
  Left = 186
  Top = 206
  Width = 1088
  Height = 750
  BorderIcons = []
  Caption = 'FormWizardBase'
  Color = clBtnFace
  Enabled = False
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  Scaled = False
  Position = poMainFormCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object pnlWizardButtons: TPanel
    Left = 0
    Top = 682
    Width = 1080
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      1080
      41)
    object btnCancel: TBitBtn
      Left = 727
      Top = 8
      Width = 75
      Height = 25
      Hint = 
        'Abort current operations, not saving information on the screen a' +
        'nd return to the main menu'
      Anchors = [akTop, akRight]
      Caption = '&Cancel'
      ModalResult = 2
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnClick = wizardButtonClick
    end
    object btnBack: TBitBtn
      Left = 802
      Top = 8
      Width = 75
      Height = 25
      Hint = 'Return to the previous screen, saving this information'
      Anchors = [akTop, akRight]
      Caption = '< &Back'
      ModalResult = 1
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = wizardButtonClick
    end
    object btnNext: TBitBtn
      Left = 877
      Top = 8
      Width = 75
      Height = 25
      Hint = 'Proceed to the next screen, saving this information'
      Anchors = [akTop, akRight]
      Caption = '&Next >'
      ModalResult = 1
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = wizardButtonClick
    end
    object btnFinish: TBitBtn
      Left = 952
      Top = 8
      Width = 75
      Height = 25
      Hint = 'Complete working on the screen by saving the information'
      Anchors = [akTop, akRight]
      Caption = '&Finish'
      ModalResult = 1
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      OnClick = wizardButtonClick
    end
  end
end
