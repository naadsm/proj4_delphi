unit FrameWorkMode;

(*
FrameWorkMode.pas/dfm
---------------------
Begin: 2009/08/27
Last revision: $Date: 2009-08-31 22:44:42 $ $Author: areeves $
Version: $Revision: 1.2 $
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
    Menus,
    StdCtrls,
    ExtCtrls,
    ToolWin,
    ComCtrls,

    // APHI General Purpose Library
    // See http://www.naadsm.org/opensource/generalpurpose
    CsvParser,
    Points,

    // Proj.4 for Delphi
    // See http://www.naadsm.org/opensource/proj4wrapper
    Proj4
  ;


  type
    /// Used to specify forward (lon,lat to x,y) or inverse (x,y to lon,lat) cartographic projection
    TProjDirection = (
      PJForward,
      PJInverse
  );


  type
    {*
      A frame with data and commands for carrying out cartographic projection on a batch of coordinates

      TODO:
        - Add support for regional list separators and decimal points (see unit I88n in the
          APHI General Purpose library).
        - Implement remaining menu commands.
    }
    TFrameWorkMode = class(TFrame)
      pnlBody: TPanel;
      leParams: TEdit;
      lblParamsWork: TLabel;
      pnlFwdOrInvWork: TPanel;
      rdoFwd: TRadioButton;
      rdoInv: TRadioButton;
      btnProjectWork: TButton;
      mmoXY: TMemo;
      lblXY: TLabel;
      lblLonLat: TLabel;
      mmoLonLat: TMemo;
      MainMenu1: TMainMenu;
      mnuOpenLL: TMenuItem;
      mnuOpenXY: TMenuItem;
      mnuDivider1: TMenuItem;
      mnuSaveLL: TMenuItem;
      mnuSaveXYCsv: TMenuItem;
      mnuDivider2: TMenuItem;
      mnuQuit: TMenuItem;
      lblProjDirection: TLabel;
      lblData: TLabel;
      lblProject: TLabel;
      btnClearXY: TButton;
      btnClearLL: TButton;
      mnuEdit: TMenuItem;
      mnuCopyLL: TMenuItem;
      mnuCopyXY: TMenuItem;
      mnuEditBreak1: TMenuItem;
      mnuPasteLL: TMenuItem;
      mnuPasteXY: TMenuItem;
      mnuHelp: TMenuItem;
      mnuHelpAbout: TMenuItem;

      //Primary GUI events
      //------------------
      procedure btnProjectWorkClick(Sender: TObject);

      //Other GUI events
      //----------------
      procedure btnClearLLClick(Sender: TObject);
      procedure btnClearXYClick(Sender: TObject);

      // Menu commands
      //--------------
      procedure mnuOpenLLClick(Sender: TObject);
      procedure mnuOpenXYClick(Sender: TObject);
      procedure mnuSaveLLClick(Sender: TObject);
      procedure mnuSaveXYCsvClick(Sender: TObject);
      procedure mnuQuitClick(Sender: TObject);
      procedure mnuCopyLLClick(Sender: TObject);
      procedure mnuCopyXYClick(Sender: TObject);
      procedure mnuPasteLLClick(Sender: TObject);
      procedure mnuPasteXYClick(Sender: TObject);
      procedure mnuHelpAboutClick(Sender: TObject);

    protected
      _myForm: TForm; /// A reference to the application's main form.  Used to display message boxes properly.

      // Functions for doing the projection
      //-----------------------------------
      procedure project( const direction: TProjDirection );

      // Helpers for project()
      function createCsv( str: string ): TCsvContents;
      function createPointList( csv: TCsvContents; const xColName, yColName: string ): T2DPointList;
      function createProjectedPointList( pointsToProject: T2DPointList; const direction: TProjDirection ): T2DPointList;
      procedure writeProjectedVals( const xyList: T2DPointList; const mmo: TMemo );

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
    // Standard Delphi units
    Math,
    StrUtils,
    Clipbrd,

    // APHI General Purpose Library
    // See http://www.naadsm.org/opensource/generalpurpose
    DebugWindow,
    MyDialogs,
    MyStrUtils,

    // This application
    FormAboutAphiDemo
  ;


//-----------------------------------------------------------------------------
// Construction/initialization/destruction
//-----------------------------------------------------------------------------
  constructor TFrameWorkMode.create( AOwner: TComponent );
    begin
      inherited create( AOwner );
      _myForm := Application.MainForm;
    end
  ;


  destructor TFrameWorkMode.destroy();
    begin
      // This function is also pretty simple for now...
      inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Doing the projection
//-----------------------------------------------------------------------------
  {*
    Carries out the specified projection

    If an error occurs during projection, a message will be displayed for the user.

    @param direction Specifies whether forward (lon,lat to x,y) or inverse (x,y to lon,lat) projection should be carried out
  }
  procedure TFrameWorkMode.project( const direction: TProjDirection );
    var
      csv: TCsvContents;
      list1: T2DPointList;
      list2: T2DPointList;
    begin
      csv := nil;
      list1 := nil;
      list2 := nil;

      case direction of
        PJForward: csv := createCsv( mmoLonLat.Text );
        PJInverse: csv := createCsv( mmoXY.Text );
      end;

      if( nil = csv ) then
        msgOK( 'CSV contents could not be parsed.  Check your file format.', 'Parsing failed', IMGCritical, _myForm )
      else
        begin
          case direction of
            PJForward: list1 := createPointList( csv, 'lon', 'lat' );
            PJInverse: list1 := createPointList( csv, 'x', 'y' );
          end;

          //list1.debug();

          if( nil = list1 ) then
            msgOK( 'CSV contents appear to be the wrong format.  Projection failed.', 'Parsing failed', IMGCritical, _myForm )
          else
            begin
              list2 := createProjectedPointList( list1, direction );
              //list2.debug();
              
              if( nil = list2 ) then
                msgOK( 'Projection failed.  Check your projection parameters.', 'Projection failed', IMGCritical, _myForm )
              else
                begin
                  case direction of
                    PJForward: writeProjectedVals( list2, mmoXY );
                    PJInverse: writeProjectedVals( list2, mmoLonLat );
                  end;
                end
              ;
            end
          ;
        end
      ;

      freeAndNil( list2 );
      freeAndNil( list1 );
      freeAndNil( csv );
    end
  ;


  {*
    Parses the CSV-formatted contents of the specified string
    @param str The CSV-formatted string
    @return A new instance of TCsvContents if parsing is successful, otherwise nil
  }
  function TFrameWorkMode.createCsv( str: string ): TCsvContents;
    var
      csv: TCSVContents;
    begin
      csv := TCsvContents.createFromString( str, ',', true );

      if( not( csv.parseSuccess ) ) then
        freeAndNil( csv )
      ;

      result := csv;
    end
  ;


  {*
    Converts the plain-text CSV values into a list of points
    @param csv The instance of TCsvContents that contains text to be converted to numerical points
    @xColName The header of the column that contains the x coordinate or longitude of each point
    @yColName The header of the column that contains the y coordinate or latitude of each point
    @return a new instance of T2DPointList if conversion is successful, otherwise nil
  }
  function TFrameWorkMode.createPointList( csv: TCsvContents; const xColName, yColName: string ): T2DPointList;
    var
      list: T2DPointList;
      i: integer;
      xCol, yCol: integer;
      x, y: double;
      success: boolean;
    begin
      list := nil;

      xCol := -1;
      yCol := -1;

      for i := 0 to csv.columnCount - 1 do
        begin
          if( xColName = leftStr( fixup( csv.header( i ) ), length( xColName ) ) ) then
            xCol := i
          else if(  yColName = leftStr( fixup( csv.header( i ) ), length( yColName ) ) ) then
            yCol := i
          ;
        end
      ;

      if( ( 0 > xCol ) or ( 0 > yCol ) ) then
        begin
          result := nil;
          exit;
        end
      ;

      list := T2DPointList.create();

      success := true;

      for i := 0 to csv.rowCount - 1 do
        begin
          x := usStrToFloat( csv.value( xCol, i ), infinity );
          y := usStrToFloat( csv.value( yCol, i ), infinity );

          if( ( infinity = x ) or ( infinity = y ) ) then
            begin
              success := false;
              break;
            end
          else
            list.append( TLatLonPoint.create( x, y ) )
          ;
        end
      ;

      if( not( success ) ) then
        freeAndNil( list )
      ;

      result := list;
    end
  ;


  {*
    Carries out the cartographic projection
    @param pointsToProject A list of points to be projected
    @param direction Specifies whether forward (lon,lat to x,y) or inverse (x,y to lon,lat) projection should be carried out
    @return A new instance of T2DPointList if successful, otherwise nil
  }
  function TFrameWorkMode.createProjectedPointList( pointsToProject: T2DPointList; const direction: TProjDirection ): T2DPointList;
    var
      list: T2DPointList;
      pt: T2DPoint;
      proj: TProj4;
      i: integer;
      success: boolean;
      pjPt: RPoint;
      ll: RLatLon;
    begin
      dbcout2( 'leParams: ' + leParams.Text );
      proj := TProj4.create( leParams.text );
      proj.debug();
      
      list := T2DPointList.create();

      success := true;

      for i := 0 to pointsToProject.Count - 1 do
        begin
          pt := pointsToProject.at( i );

          case direction of
            PJForward: pjPt := proj.pjFwd( pt.x, pt.y );
            PJInverse: ll := proj.pjInv( pt.x, pt.y );
          end;

          if( proj.error ) then
            begin
              success := false;
              break;
            end
          else
            begin
              case direction of
                PJForward: list.append( T2DPoint.create( pjPt.x, pjPt.y ) );
                PJInverse: list.append( T2DPoint.create( ll.lon, ll.lat ) );
              end;
            end
          ;
        end
      ;

      if( not( success ) ) then
        freeAndNil( list )
      ;

      proj.free();

      result := list;
    end
  ;


  {*
    Converts contents of a list of points to CSV-formatted plain text for display and export
    @param xyList The list of points
    @param mmo The TMemo object that will contain the CSV-formatted plain text
  }
  procedure TFrameWorkMode.writeProjectedVals( const xyList: T2DPointList; const mmo: TMemo );
    var
      i: integer;
    begin
      mmo.Clear;

      if( mmoLonLat = mmo ) then
        mmo.lines.append( 'lon, lat' )
      else
        mmo.lines.append( 'x, y' )
      ;

      // FIX ME: Replace this with T2DPointList.asCsv(), once it's implemented
      for i := 0 to xyList.Count - 1 do
        mmo.Lines.Append( usFloatToStr( xyList.at(i).x ) + ', ' + usFloatToStr( xyList.at(i).y ) )
      ;
    end
  ;


//-----------------------------------------------------------------------------
// Primary GUI events
//-----------------------------------------------------------------------------
  {*
    Does some simple error checking, and if things are superficially OK, attempts to carry out the specified projection
  }
  procedure TFrameWorkMode.btnProjectWorkClick(Sender: TObject);
    var
      proj: TProj4;
      success: boolean;
      errors: string;
    begin
      if( 0 = length( trim( leParams.Text ) ) ) then
        begin
          msgOK( 'Please specify parameters for the projection.', 'No parameters given', IMGWarning, _myForm );
          exit;
        end
      ;

      proj := TProj4.create( leParams.Text );
      success := proj.isValid;
      errors := proj.errorString();
      proj.Free();

      if( not( success ) ) then
        begin
          msgOK( 'Projection parameters are not valid: ' + errors, 'Invalid projection parameters', IMGCritical, _myForm );
          exit;
        end
      ;

      if( rdoFwd.Checked ) then
        begin
          if( 1 >= mmoLonLat.Lines.Count ) then
            msgOK( 'There are no points to project.  Check your direction.', 'Projection failed', IMGWarning, _myForm )
          else
            begin
              mmoXY.Clear();
              project( PJForward );
            end
          ;
        end
      else
        begin
          if( 1 >= mmoXY.Lines.Count ) then
            msgOK( 'There are no points to project.', 'Projection failed', IMGWarning, _myForm )
          else
            begin
              mmoLonLat.Clear();
              project( PJInverse );
            end
          ;
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Other GUI events
//-----------------------------------------------------------------------------
  procedure TFrameWorkMode.btnClearLLClick(Sender: TObject);
    begin
      mmoLonLat.Clear();
    end
  ;


  procedure TFrameWorkMode.btnClearXYClick(Sender: TObject);
    begin
      mmoXY.Clear();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Menu commands
//-----------------------------------------------------------------------------
  {*
    Open a user-selected CSV-formatted plain-text file, and display the contents in mmoLonLat (not yet implemented)
  }
  procedure TFrameWorkMode.mnuOpenLLClick(Sender: TObject);
    begin
      dbcout( 'Do something, eventually', true );
    end
  ;


  {*
    Open a user-selected CSV-formatted plain-text file, and display the contents in mmoXY (not yet implemented)
  }
  procedure TFrameWorkMode.mnuOpenXYClick(Sender: TObject);
    begin
      dbcout( 'Do something, eventually', true );
    end
  ;


  {*
    Save the contents of mmoLonLat in a CSV-formatted plain-text file selected by the user (not yet implemented)
  }
  procedure TFrameWorkMode.mnuSaveLLClick(Sender: TObject);
    begin
      dbcout( 'Do something, eventually', true );
    end
  ;


  {*
    Save the contents of mmoXY in a CSV-formatted plain-text file selected by the user (not yet implemented)
  }
  procedure TFrameWorkMode.mnuSaveXYCsvClick(Sender: TObject);
    begin
      dbcout( 'Do something, eventually', true );
    end
  ;


  {*
    Quit the application
  }
  procedure TFrameWorkMode.mnuQuitClick(Sender: TObject);
    begin
      Application.MainForm.close();
    end
  ;


  {*
    Copies contents of mmoLonLat to the clipboard
  }
  procedure TFrameWorkMode.mnuCopyLLClick(Sender: TObject);
    begin
      if( 0 < mmoLonLat.Lines.Count ) then
        begin
          mmoLonLat.SelectAll();
          mmoLonLat.CopyToClipboard();
        end
      ;
    end
  ;


  {*
    Copies contents of mmoXY to the clipboard
  }
  procedure TFrameWorkMode.mnuCopyXYClick(Sender: TObject);
    begin
      if( 0 < mmoXY.Lines.Count ) then
        begin
          mmoXY.SelectAll();
          mmoXY.CopyToClipboard();
        end
      ;
    end
  ;


  {*
    Pastes the contents of the clipboard to mmoLonLat.  Does not do any detailed format checking.
  }
  procedure TFrameWorkMode.mnuPasteLLClick(Sender: TObject);
    begin
      if Clipboard.HasFormat( CF_TEXT ) then
        mmoLonLat.Text := Clipboard.AsText
      ;
    end
  ;


  {*
    Pastes the contents of the clipboard to mmoXY.  Does not do any detailed format checking.
  }
  procedure TFrameWorkMode.mnuPasteXYClick(Sender: TObject);
    begin
      if Clipboard.HasFormat( CF_TEXT ) then
        mmoXY.Text := Clipboard.AsText
      ;
    end
  ;


  {*
    Displays some information about the application
  }
  procedure TFrameWorkMode.mnuHelpAboutClick(Sender: TObject);
    var
      frm: TFormAboutAphiDemo;
    begin
      frm := TFormAboutAphiDemo.create( self );
      frm.showModal();
      frm.free();
    end
  ;
//-----------------------------------------------------------------------------


end.
