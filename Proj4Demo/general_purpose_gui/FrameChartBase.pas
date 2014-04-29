{*
FrameChartBase.pas/dfm
----------------------
Begin: 2005/08/04
Last revision: $Date: 2010-11-02 18:16:23 $ $Author: rhupalo $
Version number: $Revision: 1.21 $
Project: APHI General Purpose Delphi Libary
Website: http://www.naadsm.org/opensource/delphi/
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2005 - 2010 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
---------------------------------------------------

This unit defines a base class frame used by forms providing charts showing graphical summaries of
various outputs - cost relationships and epicurves, and disease, detection, and control status, etc.
}

(*
  Documentation generation tags begin with {* or ///
  Replacing these with (* or // foils the documentation generator
*)

unit FrameChartBase;

interface

uses
  Windows, 
  Messages,
  Variants,
  Classes, 
  Graphics, 
  Controls, 
  Forms, 
  Dialogs,
  Chart
;

type
/// Base class, inheriting from TFrame, for many of the graphical output summary frames and forms
TFrameChartBase = class( TFrame )
	protected
    /// the chart that the public methods of this class operate on
		_chart: TChart;
    /// whether to throw an exception if an instance of a derived class is created with more than one chart
    _ignoreCharts: boolean;  

    function getChartWidth(): integer; virtual;
    function getChartHeight(): integer; virtual;

  public
  	constructor create( AOwner: TComponent ); override;
  	destructor destroy(); override;

  	function createMetafile(): TMetaFile; virtual;

    function saveChartToFile( fileName: string ): boolean;
    function copyChartToClipboard(): boolean;
    function printChart(): boolean; virtual;

    /// read-only property providing the width of _chart
    property chartWidth: integer read getChartWidth;
    /// read-only property providing the height of _chart
    property chartHeight: integer read getChartHeight;
    /// read-only access to _ignoreCharts
    property ignoreCharts: boolean read _ignoreCharts default false;

  end
;

implementation

{$R *.dfm}

  uses
    SysUtils,
    ClipBrd,
    MyStrUtils,
    DebugWindow
  ;

  const
    DBSHOWMSG: boolean = false; /// Set to true to enable debugging messages for this unit

//-----------------------------------------------------------------------------
// Construction/destruction
//-----------------------------------------------------------------------------

  {*
     Creates the ChartBase object and ensures the frame only contains one
     chart unless ignoreCharts is true.
     @param AOwner the form that owns this instance of the frame
     @throws An exception is raised if ignoreCharts is not true and the
     frame (including child controls contains more than one chart.
  }
	constructor TFrameChartBase.create( AOwner: TComponent );
    var
      chartCount: integer;

      // recursively examines controls of self to count the number of Chart objects
      procedure lookForChart( cntnr: TWinControl );
        var
          i: integer;
        begin
          for i := 0 to cntnr.ControlCount - 1 do
            begin
              if( cntnr.Controls[i] is TChart ) then
                begin
                  inc( chartCount );
                  _chart := cntnr.Controls[i] as TChart;
                end
              ;

              if( cntnr.Controls[i] is TWinControl ) then
                begin
                  if( 0 < (cntnr.Controls[i] as TWinControl).controlCount ) then
                    lookForChart( cntnr.Controls[i] as TWinControl )
                  ;
                end
              ;
            end
          ;
        end
      ;
		begin
			inherited create( AOwner );

      chartCount := 0;
			_chart := nil;

      lookForChart( self );

      if( ( 1 <> chartCount ) AND (not ignoreCharts ) ) then
        begin
          raise exception.Create( 'Wrong number of main charts (' + intToStr(chartCount) + ') in TFrameChartBase' );
          _chart := nil;
        end
      ;

		end
	;


  /// destroys the object and frees memory
	destructor TFrameChartBase.destroy();
		begin
			inherited destroy();
		end
	;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Meta file creation
//-----------------------------------------------------------------------------

  {*
    Helper function that creates a graphical image of _chart formatted as a metafile
    @return graphic of chart as an un-enhanced metafile - a .WMF (Windows 3.1 Metafile, with Aldus header)
    @comment Called by saveChartToFile()
  }
  function TFrameChartBase.createMetafile(): TMetaFile;
    begin
      dbcout( '_chart is nil: ' + uiBoolToText( nil = _chart ), true );
      if( nil  <> _chart ) then
        //rbh20101102 Fix Me - does this work? TeeCreateMetafile is defined in TEEPROCS.pas, which is not on my system...
        result := _chart.TeeCreateMetafile( False, Rect(0, 0, _chart.Width, _chart.Height ) )
      else
        result := nil
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Chart handling
//-----------------------------------------------------------------------------
  {*
     Saves the chart image as a graphic and writes it to filename
     @param fileName full path for image file to be created
     @comment file format is an un-enhanced metafile - a .WMF (Windows 3.1 Metafile, with Aldus header)
  }
  function TFrameChartBase.saveChartToFile( fileName: string ): boolean;
    Var
      m: TMetafile;
    begin
      m := nil;

      try
        try
          m := createMetaFile();
          m.SaveToFile( fileName );
          result := true;
        except
          result := false;
        end;
      finally
        freeAndNil( m );
      end;
    end
  ;


  {*
     Saves an image of _chart to the Windows clipboard
     @return true if successful, else false
  }
  function TFrameChartBase.copyChartToClipboard(): boolean;
    var
      m: TMetafile;
      AFormat: word;
      AData: Cardinal;
      APalette: HPALETTE;
    begin
      m := nil;

      try
        try
          m := createMetaFile();
          m.SaveToClipboardFormat( AFormat, AData, aPalette );
          ClipBoard.SetAsHandle( AFormat, AData );
          result := true;
        except
          result := false;
        end;
      finally
        freeAndNil( m );
      end;
    end
  ;


  {*
     Attempts to print an image of _chart to the default printer in landscape mode
     @return false if an exception occurs, else true
  }
  function TFrameChartBase.printChart(): boolean;
    begin
      try
        try
          Screen.Cursor := crHourGlass;
          _chart.PrintLandscape();
          result := true;
        except
          result := false;
        end;
      finally
        Screen.Cursor := crDefault;
      end;
    end
  ;
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
// Chart properties
//-----------------------------------------------------------------------------
  /// Get function for property chartWidth, returning the width of _chart
  function TFrameChartBase.getChartWidth(): integer;
    begin
      if( nil  <> _chart ) then
        result := _chart.Width
      else
        result := -1
      ;
    end
  ;

  /// Get function for property chartHeight, returning the height of _chart
  function TFrameChartBase.getChartHeight(): integer;
    begin
      if( nil  <> _chart ) then
        result := _chart.height
      else
        result := -1
      ;
    end
  ;
//-----------------------------------------------------------------------------

end.
