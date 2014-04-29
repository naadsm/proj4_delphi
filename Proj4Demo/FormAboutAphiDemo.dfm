object FormAboutAphiDemo: TFormAboutAphiDemo
  Left = 449
  Top = 331
  BorderStyle = bsDialog
  Caption = 'About the demo application'
  ClientHeight = 388
  ClientWidth = 448
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
  object lblDescription1: TLabel
    Left = 16
    Top = 9
    Width = 417
    Height = 100
    Caption = 'lblDescription1'
    Constraints.MaxHeight = 100
    Constraints.MaxWidth = 417
    Constraints.MinHeight = 100
    Constraints.MinWidth = 417
    WordWrap = True
  end
  object lblWebsite: TLabel
    Left = 16
    Top = 225
    Width = 235
    Height = 13
    Cursor = crHandPoint
    Caption = 'http://www.naadsm.org/opensource/proj4delphi/'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentFont = False
    OnClick = lblWebsiteClick
  end
  object lblCopyright: TLabel
    Left = 8
    Top = 257
    Width = 433
    Height = 26
    Alignment = taCenter
    Caption = 
      'Copyright '#169' 2009 - 2011 Animal Population Health Institute at Co' +
      'lorado State University'
    WordWrap = True
  end
  object lblLicenseBlurb: TLabel
    Left = 16
    Top = 280
    Width = 417
    Height = 61
    Caption = 
      'This program is free software; you can redistribute it and/or mo' +
      'dify it under the terms of the GNU General Public License as pub' +
      'lished by the Free Software Foundation; either version 2 of the ' +
      'License, or (at your option) any later version.  For complete li' +
      'cense details, please see below.'
    Constraints.MaxHeight = 61
    Constraints.MaxWidth = 417
    Constraints.MinHeight = 61
    Constraints.MinWidth = 417
    WordWrap = True
  end
  object lblProjWebsite: TLabel
    Left = 16
    Top = 121
    Width = 126
    Height = 13
    Cursor = crHandPoint
    Caption = 'http://trac.osgeo.org/proj/'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentFont = False
    OnClick = lblWebsiteClick
  end
  object lblDescription2: TLabel
    Left = 16
    Top = 153
    Width = 417
    Height = 60
    Caption = 'lblDescription2'
    Constraints.MaxHeight = 60
    Constraints.MaxWidth = 417
    Constraints.MinHeight = 60
    Constraints.MinWidth = 417
    WordWrap = True
  end
  object pnlButtons: TPanel
    Left = 0
    Top = 350
    Width = 448
    Height = 38
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object btnLicense: TButton
      Left = 266
      Top = 5
      Width = 89
      Height = 25
      Caption = '&License...'
      TabOrder = 0
      OnClick = btnLicenseClick
    end
    object btnOK: TButton
      Left = 365
      Top = 5
      Width = 75
      Height = 25
      Caption = '&OK'
      Default = True
      TabOrder = 1
      OnClick = btnOKClick
    end
  end
end
