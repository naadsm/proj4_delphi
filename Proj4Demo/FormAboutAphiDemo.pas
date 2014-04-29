unit FormAboutAphiDemo;

(*
FormAboutAphiDemo.pas/dfm
-------------------------
Begin: 2008/12/14
Last revision: $Date: 2011-01-26 22:29:38 $ $Author: areeves $
Version: $Revision: 1.5 $
Project: APHI Delphi Library for Simulation Modeling: Demo application
Website: http://www.naadsm.org/opensource/libaphi/
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2008 - 2011 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

interface

  uses
    // Standard Delphi units
    Windows, 
    Messages, 
    SysUtils, 
    Variants, 
    Classes, 
    Graphics, 
    Controls, 
    Forms,
    Dialogs,
    StdCtrls,
    ExtCtrls
  ;

  type TFormAboutAphiDemo = class(TForm)
    lblDescription1: TLabel;
      lblWebsite: TLabel;
      lblCopyright: TLabel;
      lblLicenseBlurb: TLabel;
      pnlButtons: TPanel;
      btnLicense: TButton;
      btnOK: TButton;
      lblProjWebsite: TLabel;
      lblDescription2: TLabel;

      procedure btnOKClick(Sender: TObject);
      procedure btnLicenseClick(Sender: TObject);
      procedure lblWebsiteClick(Sender: TObject);
      procedure FormCreate(Sender: TObject);

    protected
      // Construction/initialization/destruction
      procedure translateUI();

    public
      // Construction/initialization/destruction
      constructor create( AOwner: TComponent ); override;

    end
  ;

implementation

{$R *.dfm}

  uses
    // Standard Delphi units
    ShellAPI,

    // APHI General-Purpose Delphi Library
    // See http://www.naadsm.org/opensource/generalpurpose
    MyStrUtils,
    DialogLongMessage,
    I88n
  ;

  constructor TFormAboutAphiDemo.create( AOwner: TComponent );
    begin
      inherited create( AOwner );
      translateUI();
    end
  ;


  procedure TFormAboutAphiDemo.FormCreate( Sender: TObject );
    begin
      Assert(not Scaled, 'You should set Scaled property of Form to False!');

      if( Screen.PixelsPerInch <> 96 ) then
        ScaleBy( Screen.PixelsPerInch, 96 )
      ;
    end
  ;


  procedure TFormAboutAphiDemo.translateUI();
    var
      str1, str2a, str2b, str3: string;
    begin
      str1 := ''
        + 'This program is free software; you can redistribute it and/or modify'
        + ' it under the terms of the GNU General Public License as published by the'
        + ' Free Software Foundation; either version 2 of the License, or (at your option)'
        + ' any later version.  For complete license details, please see below.'
      ;

      self.Caption := tr( 'About this application' );
      btnOK.Caption := tr( '&OK' );
      btnLicense.Caption := tr( '&License...' );
      lblCopyright.Caption := tr( 'Copyright © 2009 - 2011 Animal Population Health Institute at Colorado State University' );
      lblLicenseBlurb.Caption := tr( str1 );

      str2a := ''
        + 'This application demonstrates the use of Proj.4 for Delphi, a Delphi wrapper'
        + ' for the Proj.4 Cartographic Projections Library.  Proj.4 is written in C,'
        + ' and is distributed as a dynamically loaded library (DLL) for Windows.  The Delphi'
        + ' class TProj4 makes use of the DLL to provide access to functions to carry out'
        + ' cartographic projection.'
      ;

      str2b := ''
        + 'Proj.4 was developed by Gerald Evenden and Frank Warmerdam.  It is available'
        + ' under the terms of an MIT-style license from the following website:'
      ;

      lblDescription1.Caption := tr( str2a ) + endl + endl + tr( str2b );


      str3 := ''
        + 'This application and the Delphi wrapper for Proj.4 are released under the'
        + ' terms of the GNU General Public license (see below). Source code for the Proj.4'
        + ' Delphi wrapper is available from the following website:'
      ;

      lblDescription2.Caption := tr( str3 );
    end
  ;


  procedure TFormAboutAphiDemo.btnOKClick( Sender: TObject );
    begin
      self.Close();
    end
  ;


  procedure TFormAboutAphiDemo.btnLicenseClick( Sender: TObject );
    var
      frm: TDialogLongMessage;
    begin
      frm := TDialogLongMessage.create( self, tr( 'GNU General Public License' ) );
      frm.mmoLongMessage.Font.Name := 'Courier New';
      frm.setMessage( i88nLicense() );

      self.Hide();

      frm.ShowModal();
      frm.Free();

      self.Show();
    end
  ;


  procedure TFormAboutAphiDemo.lblWebsiteClick(Sender: TObject);
    begin
      if( sender is TLabel ) then
        begin
          ShellExecute(
            Application.Handle,
            PChar( 'open' ),
            PChar( (sender as TLabel).Caption ),
            PChar( 0 ),
            nil,
            SW_NORMAL
          );
        end
      ;
    end
  ;



end.
 