object FrameWorkMode: TFrameWorkMode
  Left = 0
  Top = 0
  Width = 592
  Height = 382
  TabOrder = 0
  object pnlBody: TPanel
    Left = 0
    Top = 0
    Width = 592
    Height = 382
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object lblParamsWork: TLabel
      Left = 16
      Top = 8
      Width = 174
      Height = 13
      Caption = '1) Edit the parameter string for Proj.4:'
    end
    object lblXY: TLabel
      Left = 200
      Top = 120
      Width = 128
      Height = 13
      Caption = 'Projected coordinates (x,y):'
    end
    object lblLonLat: TLabel
      Left = 32
      Top = 120
      Width = 150
      Height = 13
      Caption = 'Longitude/latitude data (lon,lat):'
    end
    object lblProjDirection: TLabel
      Left = 16
      Top = 56
      Width = 190
      Height = 13
      Caption = '2) Select a forward or inverse projection:'
    end
    object lblData: TLabel
      Left = 16
      Top = 100
      Width = 437
      Height = 13
      Caption = 
        '3) Paste CSV-formatted data into the appropriate textbox below, ' +
        'with a header row as shown:'
    end
    object lblProject: TLabel
      Left = 408
      Top = 136
      Width = 128
      Height = 26
      Caption = '4) Click '#39'Project" to perform the projection:'
      WordWrap = True
    end
    object leParams: TEdit
      Left = 32
      Top = 28
      Width = 529
      Height = 21
      TabOrder = 0
      Text = '+proj=utm +zone=13 +ellps=GRS80 +datum=WGS84 +units=km'
    end
    object pnlFwdOrInvWork: TPanel
      Left = 24
      Top = 68
      Width = 457
      Height = 29
      BevelOuter = bvNone
      TabOrder = 1
      object rdoFwd: TRadioButton
        Left = 8
        Top = 8
        Width = 177
        Height = 17
        Caption = 'Forward projection (lon,lat to x,y)'
        Checked = True
        TabOrder = 0
        TabStop = True
      end
      object rdoInv: TRadioButton
        Left = 208
        Top = 8
        Width = 185
        Height = 17
        Caption = 'Inverse projection (x,y to lon,lat)'
        TabOrder = 1
      end
    end
    object btnProjectWork: TButton
      Left = 432
      Top = 172
      Width = 75
      Height = 25
      Caption = '&Project'
      TabOrder = 2
      OnClick = btnProjectWorkClick
    end
    object mmoXY: TMemo
      Left = 200
      Top = 136
      Width = 153
      Height = 185
      ScrollBars = ssBoth
      TabOrder = 3
    end
    object mmoLonLat: TMemo
      Left = 32
      Top = 136
      Width = 153
      Height = 185
      Lines.Strings = (
        'lon, lat'
        '-105.08, 40.41'
        '-108.56, 39.09'
        '-104.5, 37.18'
        '-104.08, 39.14')
      ScrollBars = ssBoth
      TabOrder = 4
    end
    object btnClearXY: TButton
      Left = 480
      Top = 296
      Width = 99
      Height = 25
      Caption = 'Clear x,y data'
      TabOrder = 5
      OnClick = btnClearXYClick
    end
    object btnClearLL: TButton
      Left = 368
      Top = 296
      Width = 99
      Height = 25
      Caption = 'Clear lon,lat data'
      TabOrder = 6
      OnClick = btnClearLLClick
    end
  end
  object MainMenu1: TMainMenu
    Left = 536
    Top = 64
    object File1: TMenuItem
      Caption = '&File'
      object mnuOpenLL: TMenuItem
        Caption = '&Open lon/lat CSV file'
        Enabled = False
        OnClick = mnuOpenLLClick
      end
      object mnuOpenXY: TMenuItem
        Caption = 'Op&en x/y CSV file'
        Enabled = False
        OnClick = mnuOpenXYClick
      end
      object mnuDivider1: TMenuItem
        Caption = '-'
      end
      object mnuSaveLL: TMenuItem
        Caption = '&Save lon/lat CSV file'
        Enabled = False
        OnClick = mnuSaveLLClick
      end
      object mnuSaveXYCsv: TMenuItem
        Caption = 'S&ave x/y CSV file'
        Enabled = False
        OnClick = mnuSaveXYCsvClick
      end
      object mnuDivider2: TMenuItem
        Caption = '-'
      end
      object mnuQuit: TMenuItem
        Caption = '&Quit'
        OnClick = mnuQuitClick
      end
    end
    object mnuEdit: TMenuItem
      Caption = '&Edit'
      object mnuCopyLL: TMenuItem
        Caption = 'Copy lon,lat to clipboard'
        OnClick = mnuCopyLLClick
      end
      object mnuCopyXY: TMenuItem
        Caption = 'Copy x,y to clipboard'
        OnClick = mnuCopyXYClick
      end
      object mnuEditBreak1: TMenuItem
        Caption = '-'
      end
      object mnuPasteLL: TMenuItem
        Caption = 'Paste lon,lat coordinates'
        OnClick = mnuPasteLLClick
      end
      object mnuPasteXY: TMenuItem
        Caption = 'Paste x,y coordinates'
        OnClick = mnuPasteXYClick
      end
    end
    object mnuHelp: TMenuItem
      Caption = '&Help'
      object mnuHelpAbout: TMenuItem
        Caption = '&About GeoProjDelphi...'
        OnClick = mnuHelpAboutClick
      end
    end
  end
end
