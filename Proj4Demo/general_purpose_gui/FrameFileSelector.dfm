object FrameFileSelector: TFrameFileSelector
  Left = 0
  Top = 0
  Width = 376
  Height = 47
  TabOrder = 0
  object lblFileName: TLabel
    Left = 8
    Top = 8
    Width = 48
    Height = 13
    Caption = 'File name:'
  end
  object leFileName: TEdit
    Left = 7
    Top = 24
    Width = 298
    Height = 21
    TabOrder = 0
  end
  object btnBrowse: TButton
    Left = 311
    Top = 20
    Width = 57
    Height = 25
    Caption = 'Browse...'
    TabOrder = 1
    OnClick = btnBrowseClick
  end
  object SaveDialog1: TSaveDialog
    Filter = '*.*|All files|*.csv|Comma separated values'
	Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 150
    Top = 16
  end
end
