unit FrameDensityBase;

(*
FrameDensityBase.pas/dfm
----------------------
Begin: 2008/02/15
Last revision: $Date: 2008-12-10 21:03:31 $ $Author: areeves $
Version number: $Revision: 1.3 $
Project: APHI General Purpose Delphi Libary
Website: http://www.naadsm.org/opensource/delphi/
Author: Shaun Case <Shaun.Case@colostate.edu>
--------------------------------------------------
Copyright (C) 2005 - 2008 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

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
  FrameEpiCurveDensityPlot
;

type TFrameDensityBase = class( TFrame )
	protected
		_graph: TFrameSummaryEpiCurvesDensityPlot;

    function getGraphWidth(): integer;
    function getGraphHeight(): integer;

  public
  	constructor create( AOwner: TComponent ); override;
  	destructor destroy(); override;

  	function createMetafile(): TMetaFile;

    function saveGraphToFile( fileName: string ): boolean;
    function copyGraphToClipboard(): boolean;
    function printGraph(): boolean;

    property graphWidth: integer read getGraphWidth;
    property graphHeight: integer read getGraphHeight;

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
    DBSHOWMSG: boolean = true; // Set to true to enable debugging messages for this unit

//-----------------------------------------------------------------------------
// Construction/destruction
//-----------------------------------------------------------------------------
	constructor TFrameDensityBase.create( AOwner: TComponent );
    var
      chartCount: integer;

      procedure lookForGraph( cntnr: TWinControl );
        var
          i: integer;
        begin
          for i := 0 to cntnr.ControlCount - 1 do
            begin
              if( cntnr.Controls[i] is TFrameSummaryEpiCurvesDensityPlot ) then
                begin
                  inc( chartCount );
                  _graph := cntnr.Controls[i] as TFrameSummaryEpiCurvesDensityPlot;
                end
              ;

              if( cntnr.Controls[i] is TWinControl ) then
                begin
                  if( 0 < (cntnr.Controls[i] as TWinControl).controlCount ) then
                    lookForGraph( cntnr.Controls[i] as TWinControl )
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
			_graph := nil;

      lookForGraph( self );

      if( 1 <> chartCount ) then
        begin
          raise exception.Create( 'Wrong number of main graphs (' + intToStr(chartCount) + ') in TFrameDensityBase' );
          _graph := nil;
        end
      ;
		end
	;


	destructor TFrameDensityBase.destroy();
		begin
			inherited destroy();
		end
	;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Meta file creation
//-----------------------------------------------------------------------------
  function TFrameDensityBase.createMetafile(): TMetaFile;
    begin
      dbcout( '_graph is nil: ' + booltoText( nil = _graph), true );
      if( nil  <> _graph ) then
//        result := _graph.TeeCreateMetafile( False, Rect(0, 0, _chart.Width, _chart.Height ) )
      else
        result := nil
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Chart handling
//-----------------------------------------------------------------------------
  function TFrameDensityBase.saveGraphToFile( fileName: string ): boolean;
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


  function TFrameDensityBase.copyGraphToClipboard(): boolean;
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


  function TFrameDensityBase.printGraph(): boolean;
    begin
      try
        try
          Screen.Cursor := crHourGlass;
//          _graph.PrintLandscape();
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
  function TFrameDensityBase.getGraphWidth(): integer;
    begin
      if( nil  <> _graph ) then
        result := _graph.Width
      else
        result := -1
      ;
    end
  ;

  function TFrameDensityBase.getGraphHeight(): integer;
    begin
      if( nil  <> _graph ) then
        result := _graph.Height
      else
        result := -1
      ;
    end
  ;
//-----------------------------------------------------------------------------

end.
