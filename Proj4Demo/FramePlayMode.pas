unit FramePlayMode;

(*
FramePlayMode.pas/dfm
---------------------
Begin: 2009/08/12
Last revision: $Date: 2009-08-28 17:29:03 $ $Author: areeves $
Version: $Revision: 1.1 $
Project: Demo application for the Delphi wrapper for the Proj.4 Cartographic Projections library
Website: http://www.naadsm.org/opensource
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2009 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

interface

  uses
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
    ExtCtrls,

    // APHI Delphi components
    // See http://www.naadsm.org/opensource
    REEdit
  ;

  type TFramePlayMode = class(TFrame)
      pnlBody: TPanel;
      lblLocation: TLabel;
      lblProjParams: TLabel;
      lblMemoProjParams: TLabel;
      Label1: TLabel;
      Button1: TButton;
      Button2: TButton;
      cboLocation: TComboBox;
      gbxLatLon: TGroupBox;
      lblLat1: TLabel;
      lblLon1: TLabel;
      rleLat: TREEdit;
      rleLon: TREEdit;
      GroupBox1: TGroupBox;
      lblY: TLabel;
      lblX: TLabel;
      rleY: TREEdit;
      rleX: TREEdit;
      GroupBox2: TGroupBox;
      lblLat2: TLabel;
      lblLon2: TLabel;
      rleLatProj: TREEdit;
      rleLonProj: TREEdit;
      cboProjParams: TComboBox;
      mmoProjParams: TMemo;
      btnProject: TButton;
      btnAbout: TButton;

      { Sets the lat/lon coordinates to be projected to the coordinates of the selected location. }
      procedure cboLocationChange(Sender: TObject);

      { Sets the projection parameters to the selected set. }
      procedure cboProjParamsChange(Sender: TObject);

      { Executes a projection using an instance of the TProj4 class. }
      procedure btnProjectClick(Sender: TObject);

      { Displays useful information about this application and the libraries that it uses. }
      procedure btnAboutClick(Sender: TObject);

      { Run more extensive but less user- and reader-friendly code for testing the TProj4 class. }
      procedure Button1Click(Sender: TObject);
      procedure Button2Click(Sender: TObject);

    protected
      { Clears projected values (the contents of the line editors) when the user changes any of the options. }
      procedure clearRles();

    public
      // Construction/initialization/destruction
      //----------------------------------------
      constructor create( AOwner: TComponent ); override;
      destructor destroy(); override;

    end
  ;

implementation

{$R *.dfm}

  uses
    // APHI General Purpose Delphi Library
    // See http://www.naadsm.org/opensource/generalpurpose
    DebugWindow,
    Points,
    MyStrUtils,
    RegExpDefs,
    MyDialogs,

    // Proj4 cartographic projection library
    // See http://www.naadsm.org/opensource/proj4wrapper
    Proj4,

    // Application-specific units
    FormAboutAphiDemo
  ;


//-----------------------------------------------------------------------------
// Construction/initialization/destruction
//-----------------------------------------------------------------------------
  constructor TFramePlayMode.create( AOwner: TComponent );
    begin
      inherited create( AOwner );

      rleLat.InputExpression := RE_SIGNED_DECIMAL_INPUT;
      rleLon.InputExpression := RE_SIGNED_DECIMAL_INPUT;

      {$IFDEF DEBUG}
        Button1.Visible := true;
        Button2.Visible := true;
      {$ELSE}
        Button1.Visible := false;
        Button2.Visible := false;
      {$ENDIF}

      cboLocation.ItemIndex := 0;
      cboProjParams.ItemIndex := 0;

      cboLocationChange( nil );
      cboProjParamsChange( nil );
    end
  ;


  destructor TFramePlayMode.destroy();
    begin
      inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// GUI event handlers
//-----------------------------------------------------------------------------
  {*
    Sets the lat/lon coordinates to be projected to the coordinates of the selected location.
    If the user selects the "Custom" option, the user can enter latitude/longitude coordinates
    of his own to be projected.
  }
  procedure TFramePlayMode.cboLocationChange(Sender: TObject);
    begin
      clearRles();

      rleLat.ReadOnly := ( ( cboLocation.Items.Count - 1 ) <> cboLocation.ItemIndex );
      rleLon.ReadOnly := ( ( cboLocation.Items.Count - 1 ) <> cboLocation.ItemIndex );

      case cboLocation.ItemIndex of
        0: // Loveland
          begin
            rleLat.Text := '40.41';
            rleLon.Text := '-105.08';
          end
        ;
        1: // Grand Junction
          begin
            rleLat.Text := '39.09';
            rleLon.Text := '-108.56';
          end
        ;
        2: // SF
          begin
            rleLat.Text := '37.78';
            rleLon.Text := '-121.42';
          end
        ;
        3: // Williamsburg
          begin
           rleLat.Text := '37.27';
           rleLon.Text := '-76.71';
          end
        ;
        4: // Rio
          begin
            rleLat.Text := '-22.91';
            rleLon.Text := '-43.20';
          end
        ;
        5: // London
          begin
            rleLat.Text := '51.51';
            rleLon.Text := '0.13';
          end
        ;
        6: // Tokyo
          begin
            rleLat.Text := '35.67';
            rleLon.Text := '139.75';
          end
        ;
        7: // Bad parameters
          begin
            rleLat.Text := '95.00';
            rleLon.Text := '-200.00';
          end
        ;
        8: // Custom
          begin
            rleLat.Text := '';
            rleLon.Text := '';
          end
        ;
      end;


    end
  ;


  {*
    Sets the projection parameters to the selected set.  If the user selects "Custom parameters",
    he can enter his own string of Proj.4 parameters as described in the Proj.4 manual.
  }
  procedure TFramePlayMode.cboProjParamsChange(Sender: TObject);
    begin
      clearRles();
      mmoProjParams.ReadOnly := ( 6 <> cboProjParams.ItemIndex );

      case cboProjParams.ItemIndex of
        0: mmoProjParams.Text := '+proj=utm +zone=13 +ellps=GRS80 +datum=WGS84 +units=km'; // UTM zone 13N
        1: mmoProjParams.Text := '+proj=utm +zone=22 +south=true +ellps=GRS80 +datum=WGS84 +units=km'; // UTM zone 22S
        2: mmoProjParams.Text := '+proj=utm +zone=30 +ellps=GRS80 +datum=WGS84 +units=km'; // UTM zone 30N
        3: mmoProjParams.Text := '+proj=utm +zone=54 +ellps=GRS80 +datum=WGS84 +units=km'; // UTM zone 54N
        4: mmoProjParams.Text := '(Fill this in)'; // NAADSM default
        5: mmoProjParams.Text := '+proj=utm +zone=17 +units=km'; // Incomplete parameters
        6: { Do nothing }; // Custom parameters
      end;
    end
  ;

  {*
    Attempts to project the specified lat/lon to y/x, and then back again with an
    inverse projection.

    An error message is displayed if the projection system is not properly specified
    or if there is a problem with the projection.
  }
  procedure TFramePlayMode.btnProjectClick(Sender: TObject);
    var
      proj: TProj4;
      llr: RLatLon;
      xyr: RPoint;
      fwdSuccess: boolean;
    begin
      proj := TProj4.create( mmoProjParams.Text );

      // Is the specified projection valid?
      // If not, show the error.
      if( not( proj.isValid ) ) then
        begin
          msgOK(
            'An error was encountered: ' + endl + proj.errorString(),
            'Projection failed',
            IMGCritical,
            self
          );
        end
      else // The specified projection is OK.  Give it a try.
        begin
          llr.lat := uiStrToFloat( rleLat.Text );
          llr.lon := uiStrToFloat( rleLon.Text );

          // Try the forward projection.  Raise an exception if it fails.
          try
            xyr := proj.pjFwdR( llr, true );
            fwdSuccess := true;
          except
            msgOK(
              'An error was encountered: ' + endl + proj.errorString(),
              'Forward projection failed',
              IMGCritical,
              self
            );
            fwdSuccess := false;
          end;


          if( fwdSuccess ) then
            begin
              rleY.Text := uiFloatToStr( xyr.y );
              rleX.Text := uiFloatToStr( xyr.x );

              // Try the inverse projection.  Raise an exception if it fails.
              try
                llr := proj.pjInvR( xyr, true );
              except
                msgOK(
                  'An error was encountered: ' + endl + proj.errorString(),
                  'Forward projection failed',
                  IMGCritical,
                  self
                );
                exit;
              end;

              rleLatProj.Text := uiFloatToStr( llr.lat );
              rleLonProj.Text := uiFloatToStr( llr.lon );
            end
          ;
        end
      ;

      proj.Free();
    end
  ;


  {*
    Displays useful information about this application and the libraries that it uses.
  }
  procedure TFramePlayMode.btnAboutClick(Sender: TObject);
    var
      frm: TFormAboutAphiDemo;
    begin
      frm := TFormAboutAphiDemo.create( self );
      frm.showModal();
      frm.free();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Useful helper functions
//-----------------------------------------------------------------------------
  {*
    Used to clear projected values (the contents of the line editors)
    when the user changes any of the options.
  }
  procedure TFramePlayMode.clearRles();
    begin
      rleY.Text := '';
      rleX.Text := '';
      rleLatProj.Text := '';
      rleLonProj.Text := '';
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// User- and reader-unfriendly functions
// These functions run more extensive code for testing the TProj4 class.
// The buttons are visible only when the DEBUG symbol is defined: see
// Defs.inc.
//-----------------------------------------------------------------------------
  {* Determines whether proj.dll was loaded. }
  procedure TFramePlayMode.Button1Click(Sender: TObject);
    begin
      if( projLibLoaded ) then
        dbcout2( 'Lib loaded' )
      else
        begin
          dbcout2( 'Lib not loaded:' );
          dbcout2( projLibLoadErrors() );
        end
      ;
    end
  ;


  {* Tests all of the various TProj4 functions for forward and inverse projection. }
  procedure TFramePlayMode.Button2Click(Sender: TObject);
    var
      proj: TProj4;
      llr: RLatLon;
      xyr: RPoint;
      llt: TLatLonPoint;
      xyt: T2DPoint;
      lat, lon: double;
      x, y: double;
      params: string;
    begin
      // Clearly an error
      (*
      lat := 493212;
      lon := 4473267;


      // Loveland
      lat := 40.41;
      lon := -105.08;
      *)

      // Rio
      (*
      lat := -22.91;
      lon := -43.20;
      *)

      (*
      // London
      lat := 51.51;
      lon := -0.13;
      *)

      
      // Tokyo
      lat := 35.67;
      lon :=  139.75;

      params := '+proj=utm +zone=54 +ellps=GRS80 +datum=WGS84 +units=km'; // A good parameter string for Tokyo.
      //params := '+proj=utm +zone=22 +ellps=GRS80 +datum=WGS84 +units=km'; // A good parameter string for Rio.
      //params := '+proj=utm +zone=13 +ellps=GRS80 +datum=WGS84 +units=km'; // A good parameter string for CO.
      //params := '+proj=utm +zone=30 +ellps=GRS80 +datum=WGS84 +units=km'; // A good parameter string for London.
      //params := '+proj=utm +zone=17 +units=km'; // This parameter string won't work: pieces are missing.

      proj := TProj4.create( params, true );
      dbcout2( proj.isValid );
      dbcout2( proj.errorString() );
      dbcout2( proj.paramString );

      exit;

      // Forward projections
      //--------------------
      dbcout2( 'Forward projection of lat/lon (' + usFloatToStr( lat ) + ', ' + usFloatToStr( lon ) + '):' );

      llr.lat := lat;
      llr.lon := lon;
      xyr := proj.pjFwdR( llr, true );
      dbcout2( proj.errorString() );
      dbcout2( usFloatToStr( xyr.x ) + ', ' + usFloatToStr( xyr.y ) );
      dbcout2( endl );

      llt := TLatLonPoint.create( lat, lon );
      xyt := T2DPoint.create();
      proj.pjFwdT( llt, xyt );
      dbcout2( proj.errorString() );
      xyt.debug();
      xyt.Free();

      xyt := proj.createPjFwdT( llt );
      dbcout2( proj.errorString() );
      xyt.debug();
      xyt.Free();
      llt.Free();

      xyr := proj.pjFwd( lon, lat ); // pjFwd takes parameters in x,y order!
      dbcout2( proj.errorString() );
      dbcout2( usFloatToStr( xyr.x ) + ', ' + usFloatToStr( xyr.y ) );
      dbcout2( endl );

      // Inverse projections
      //--------------------
      x := xyr.x;
      y := xyr.y;

      dbcout2( 'Inverse projection of x/y (' + usFloatToStr( x ) + ', ' + usFloatToStr( y ) + '):' );
      llr := proj.pjInvR( xyr );
      dbcout2( proj.errorString() );
      dbcout2( usFloatToStr( llr.lat ) + ', ' + usFloatToStr( llr.lon ) );
      dbcout2( endl );

      llr := proj.pjInv( x, y );
      dbcout2( proj.errorString() );
      dbcout2( usFloatToStr( llr.lat ) + ', ' + usFloatToStr( llr.lon ) );

      xyt := T2DPoint.create( x, y );
      llt := TLatLonPoint.create();
      proj.pjInvT( xyt, llt );
      dbcout2( proj.errorString() );
      llt.debug();
      llt.Free();

      llt := proj.createPjInvT( xyt );
      llt.debug();
      llt.Free();

      xyt.Free();

      proj.Free();
    end
  ;
//-----------------------------------------------------------------------------


end.
