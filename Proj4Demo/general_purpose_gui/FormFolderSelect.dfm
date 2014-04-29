object FormFolderSelect: TFormFolderSelect
  Left = 1204
  Top = 38
  Width = 400
  Height = 450
  BorderIcons = [biSystemMenu]
  Caption = 'Browse for folder'
  Color = clBtnFace
  Constraints.MinWidth = 325
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object treeView: TShellTreeView
    Left = 0
    Top = 41
    Width = 392
    Height = 303
    Hint = 'Select a folder'
    ObjectTypes = [otFolders]
    Root = 'rfDesktop'
    UseShellImages = True
    Align = alClient
    AutoRefresh = False
    Indent = 19
    ParentColor = False
    ParentShowHint = False
    RightClickSelect = True
    ShowHint = True
    ShowRoot = False
    TabOrder = 0
    OnChange = treeViewChange
    OnExpanded = treeViewExpanded
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 344
    Width = 392
    Height = 79
    Align = alBottom
    TabOrder = 1
    object lblSelectedFolder: TLabel
      Left = 1
      Top = 9
      Width = 390
      Height = 13
      Align = alTop
      Caption = 'Selected folder:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblFolder: TLabel
      Left = 1
      Top = 22
      Width = 390
      Height = 26
      Align = alClient
      Caption = 'lblFolder'
    end
    object pnlSpacer: TPanel
      Left = 1
      Top = 1
      Width = 390
      Height = 8
      Align = alTop
      TabOrder = 0
    end
    object pnlButtons: TPanel
      Left = 1
      Top = 48
      Width = 390
      Height = 30
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object pnlButtonsRight: TPanel
        Left = 211
        Top = 0
        Width = 179
        Height = 30
        Align = alRight
        BevelOuter = bvNone
        TabOrder = 0
        object btnOK: TButton
          Left = 16
          Top = 3
          Width = 75
          Height = 25
          Caption = 'OK'
          Default = True
          TabOrder = 0
          OnClick = btnOKClick
        end
        object btnCancel: TButton
          Left = 96
          Top = 3
          Width = 75
          Height = 25
          Cancel = True
          Caption = 'Cancel'
          TabOrder = 1
          OnClick = btnCancelClick
        end
      end
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 392
    Height = 41
    Align = alTop
    TabOrder = 2
    object pnlNewFolder: TPanel
      Left = 266
      Top = 1
      Width = 125
      Height = 39
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object btnNewFolder: TBitBtn
        Left = 88
        Top = 8
        Width = 25
        Height = 25
        Hint = 'Create new folder'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnClick = btnNewFolderClick
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000120B0000120B00000000000000000000FF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FF00000000000000000000000000000000000000000000
          0000000000000000000000000000FF00FFFF00FFFF00FFFF00FF000000C8D0D4
          00FFFFC8D0D400FFFFC8D0D400FFFFC8D0D400FFFFC8D0D400FFFF000000FF00
          FFFF00FFFF00FFFF00FF00000000FFFFC8D0D400FFFFC8D0D400FFFFC8D0D400
          FFFFC8D0D400FFFFC8D0D4000000FF00FFFF00FFFF00FFFF00FF000000C8D0D4
          00FFFFC8D0D400FFFFC8D0D400FFFFC8D0D400FFFFC8D0D400FFFF000000FF00
          FFFF00FFFF00FFFF00FF00000000FFFFC8D0D400FFFFC8D0D400FFFFC8D0D400
          FFFFC8D0D400FFFFC8D0D4000000FF00FFFF00FFFF00FFFF00FF000000C8D0D4
          00FFFFC8D0D400FFFFC8D0D400FFFFC8D0D400FFFFC8D0D400FFFF000000FF00
          FFFF00FF000000FF00FF00000000FFFFC8D0D400FFFFC8D0D400FFFFC8D0D400
          FFFFC8D0D400FFFFC8D0D4000000FF00FF000000FF00FFFF00FF000000C8D0D4
          00FFFFC8D0D400FFFFC8D0D400FFFFC8D0D400FFFFC8D0D400FFFF0000000000
          00FF00FFFF00FFFF00FF00000000000000000000000000000000000000000000
          0000000000000000000000FF00FFFF00FF000000FF00FF000000FF00FF000000
          00FFFFC8D0D400FFFFC8D0D4000000FF00FFFF00FFFF00FF000000FF00FF0000
          00FF00FFFF00FFFF00FFFF00FFFF00FF000000000000000000000000FF00FFFF
          00FFFF00FF000000FF00FF000000FF00FF000000FF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF000000FF00FFFF00FFFF00FFFF00
          FFFF00FF000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FF000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
          FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
          FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
          00FFFF00FFFF00FFFF00FF000000FF00FFFF00FFFF00FFFF00FF}
      end
    end
    object pnlTopContainer: TPanel
      Left = 1
      Top = 1
      Width = 265
      Height = 39
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object lblHeader: TLabel
        Left = 8
        Top = 8
        Width = 158
        Height = 13
        Caption = 'Select a folder from the list below:'
      end
    end
  end
end
