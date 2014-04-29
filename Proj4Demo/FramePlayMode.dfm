object FramePlayMode: TFramePlayMode
  Left = 0
  Top = 0
  Width = 716
  Height = 539
  TabOrder = 0
  object pnlBody: TPanel
    Left = 0
    Top = 0
    Width = 716
    Height = 539
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object lblLocation: TLabel
      Left = 16
      Top = 16
      Width = 295
      Height = 26
      Caption = 
        'Select a location, or choose '#39'Custom'#39' to enter your own lat/lon ' +
        'coordinates in the box to the right:'
      WordWrap = True
    end
    object lblProjParams: TLabel
      Left = 16
      Top = 88
      Width = 303
      Height = 26
      Caption = 
        'Select a set of projection system parameters, or choose '#39'Custom ' +
        'parameters'#39' to enter a string of your own in the box below:'
      WordWrap = True
    end
    object lblMemoProjParams: TLabel
      Left = 16
      Top = 160
      Width = 301
      Height = 39
      Caption = 
        'View selected projection system parameters, or choose '#39'Custom pa' +
        'rameters'#39' above to enter your own.  See the Proj.4 manual (inclu' +
        'ded with this application) for more information.'
      WordWrap = True
    end
    object Label1: TLabel
      Left = 16
      Top = 300
      Width = 194
      Height = 26
      Caption = 
        'Click '#39'Project'#39' to project the specified lat/lon to the selected' +
        ' coordinate system:'
      WordWrap = True
    end
    object Button1: TButton
      Left = 8
      Top = 304
      Width = 75
      Height = 25
      Caption = 'Lib loaded?'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 88
      Top = 304
      Width = 97
      Height = 25
      Caption = 'Do some points'
      TabOrder = 1
      OnClick = Button2Click
    end
    object cboLocation: TComboBox
      Left = 16
      Top = 48
      Width = 345
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 2
      OnChange = cboLocationChange
      Items.Strings = (
        'Loveland, CO: 40.41 N, 105.08 W (UTM zone 13N)'
        'Grand Junction, CO: 39.09 N, 108.56 W (UTM zone 12N)'
        'San Francisco, CA: 37.78 N, 121.42 W (UTM zone 10N)'
        'Williamsburg, VA: 37.27 N 76.71 W (UTM zone 18N)'
        'Rio de Janeiro, Brazil: 22.91 S, 43.20 W (UTM zone 22S)'
        'London, England: 51.51 N, 0.13 W (UTM zone 30N)'
        'Tokyo, Japan: 35.67 N, 139.75 E (UTM zone 54N)'
        'Invalid lat/lon parameters'
        'Custom location')
    end
    object gbxLatLon: TGroupBox
      Left = 384
      Top = 16
      Width = 185
      Height = 89
      Caption = 'Original lat/lon: '
      TabOrder = 3
      object lblLat1: TLabel
        Left = 8
        Top = 24
        Width = 18
        Height = 13
        Caption = 'Lat:'
      end
      object lblLon1: TLabel
        Left = 96
        Top = 24
        Width = 21
        Height = 13
        Caption = 'Lon:'
      end
      object rleLat: TREEdit
        Left = 8
        Top = 40
        Width = 81
        Height = 21
        EditAlign = eaLeft
        ReadOnly = True
        TabOrder = 0
      end
      object rleLon: TREEdit
        Left = 96
        Top = 40
        Width = 81
        Height = 21
        EditAlign = eaLeft
        ReadOnly = True
        TabOrder = 1
      end
    end
    object GroupBox1: TGroupBox
      Left = 384
      Top = 128
      Width = 185
      Height = 89
      Caption = 'Projected coordinates: '
      TabOrder = 4
      object lblY: TLabel
        Left = 8
        Top = 24
        Width = 10
        Height = 13
        Caption = 'Y:'
      end
      object lblX: TLabel
        Left = 96
        Top = 24
        Width = 10
        Height = 13
        Caption = 'X:'
      end
      object rleY: TREEdit
        Left = 8
        Top = 40
        Width = 81
        Height = 21
        EditAlign = eaLeft
        ReadOnly = True
        TabOrder = 0
      end
      object rleX: TREEdit
        Left = 96
        Top = 40
        Width = 81
        Height = 21
        EditAlign = eaLeft
        ReadOnly = True
        TabOrder = 1
      end
    end
    object GroupBox2: TGroupBox
      Left = 384
      Top = 240
      Width = 185
      Height = 89
      Caption = 'Inverse projection: '
      TabOrder = 5
      object lblLat2: TLabel
        Left = 8
        Top = 24
        Width = 18
        Height = 13
        Caption = 'Lat:'
      end
      object lblLon2: TLabel
        Left = 96
        Top = 24
        Width = 21
        Height = 13
        Caption = 'Lon:'
      end
      object rleLatProj: TREEdit
        Left = 8
        Top = 40
        Width = 81
        Height = 21
        EditAlign = eaLeft
        ReadOnly = True
        TabOrder = 0
      end
      object rleLonProj: TREEdit
        Left = 96
        Top = 40
        Width = 81
        Height = 21
        EditAlign = eaLeft
        ReadOnly = True
        TabOrder = 1
      end
    end
    object cboProjParams: TComboBox
      Left = 16
      Top = 120
      Width = 345
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 6
      OnChange = cboProjParamsChange
      Items.Strings = (
        'UTM zone 13N'
        'UTM zone 22S'
        'UTM zone 30N'
        'UTM zone 54N'
        'NAADSM defaults'
        'Incomplete parameters'
        'Custom parameters')
    end
    object mmoProjParams: TMemo
      Left = 16
      Top = 208
      Width = 345
      Height = 81
      ReadOnly = True
      TabOrder = 7
    end
    object btnProject: TButton
      Left = 240
      Top = 304
      Width = 59
      Height = 25
      Caption = 'Project'
      TabOrder = 8
      OnClick = btnProjectClick
    end
    object btnAbout: TButton
      Left = 304
      Top = 304
      Width = 59
      Height = 25
      Caption = 'About...'
      TabOrder = 9
      OnClick = btnAboutClick
    end
  end
end
