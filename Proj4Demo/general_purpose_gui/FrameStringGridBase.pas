{*
FrameStringGridBase.pas/dfm
---------------------------
Begin: 2005/10/21
Last revision: $Date: 2011-10-12 19:42:49 $ $Author: rhupalo $
Version number: $Revision: 1.25 $
Project: APHI General Purpose Delphi Libary
Website: http://www.naadsm.org/opensource/delphi/
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2005 - 2010 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
--------------------------------------------------

Frame for tabular output forms to display, print, and manage numeric output in a grid of cells.

}

(*
  Documentation generation tags begin with {* or ///
  Replacing these with (* or // foils the documentation generator
*)

unit FrameStringGridBase;

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
    Grids,
    ExtCtrls,
    
    ARSyncGrid
  ;

  // From a demonstration written by Nilesh N Shah.  
  // See http://www.delphipages.com/tips/thread.cfm?ID=102
  type
  /// Record for printing the contents of a grid
  RRecPrintStrGrid = Record
      PrCanvas : TCanvas;     /// Printer or PaintBox Canvas
      sGrid: TARSyncGrid;     /// StringGrid containing data
      sTitle: String;         /// Title of document
      bPrintFlag : Boolean;   /// Print if True
      ptXYOffset : TPoint;    /// Left and Top margins
      ftTitleFont : TFont;    /// Font for Title
      ftHeadingFont : TFont;  /// Font for Heading row
      ftDataFont : TFont;     /// Font for Data
      bBorderFlag : Boolean;  /// Print border if True
      // variables above are the original and the ones below are needed for Teixeira and Pacheco's approach
      lineHeight: Integer;    /// line height based on text height using the currently rendered font
      amountPrinted: integer; /// keeps track of vertical space in pixels, printed on a page
      tenthsOfInchPixelsY: integer; /// number of pixels in 1/10 of an inch, used for line spacing
    end
  ;


	type
  /// Frame containing the string grid (TARSyncGrid) and methods to manage it.
  TFrameStringGridBase = class( TFrame )
      stgGrid: TARSyncGrid;  /// the grid of cells, holding either captions or data
  	  pnlSpacer: TPanel;     /// panel component for implementing the rightSpace property

      procedure stgGridSelectCell( Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean );
      procedure stgGridEnter(Sender: TObject);

  	protected
      function getOptions(): TGridOptions;
      function getCell( ACol, ARow: integer ): string;

      procedure setCell( ACol, ARow: integer; val: string );

      procedure setColCount( const val: integer );
      procedure setRowCount( const val: integer );
      procedure setColWidth( ACol: longint; val: integer );
      procedure setRowHeight( ARow: longint; val: integer );
      procedure setFixedCols( const val: integer );
      procedure setFixedRows( const val: integer );

      function getColCount(): integer;
      function getRowCount(): integer;
      function getColWidth( ACol: longint ): integer;
      function getRowHeight( ARow: longint ): integer;
      function getFixedCols(): integer;
      function getFixedRows(): integer;

      procedure setRightSpace( const val: integer );
      function getRightSpace(): integer;

  	public
  		constructor create( AOwner: TComponent ); override;
  		destructor destroy(); override;
  		
      procedure printGrid( pageTitle: string = ''; reportHeader: string = '' );
  		procedure printGrid2( pageTitle: string = '' );
  		function csvText(): string; virtual;
      function saveToFile( const fileName: string; header: string = '' ): boolean; 
      
      procedure clearColumn( const i: integer );
      procedure clearGrid( clearFixedCells: boolean = false );

      /// read-only access to the grid option settings
      property options: TGridOptions read getOptions;
      /// Access to the grid cell at column i, row j
      property Cells[ACol, ARow: Integer]: string read getCell write setCell;
      /// Access for reading or setting the number of columns
      property colCount: integer read getColCount write setColCount;
      /// Access for reading or setting the number of rows
      property rowCount: integer read getRowCount write setRowCount;
      /// Access for reading or setting a column width
      property colWidths[ACol: longint]: integer read getColWidth write setColWidth;
      /// Access for reading or setting a row heights
      property rowHeights[ARow: longint]: integer read getRowHeight write setRowHeight;
      /// Access for reading or setting the number of fixed columns
      property fixedCols: integer read getFixedCols write setFixedCols;
      /// Access for reading or setting the number of fixed rows
      property fixedRows: integer read getFixedRows write setFixedRows;
      /// Access to reading or setting the amount of empty space between the right edge of the grid and the right edge of the frame
      property rightSpace: integer read getRightSpace write setRightSpace;
		end
	;


  const
    DBFRAMESTRINGGRIDBASE: boolean = false; /// Set to true to enable debugging message for this unit


implementation

  {$R *.dfm}

  uses
    Math,
    Printers,
    StrUtils,
    
    CStringList,
    MyStrUtils,
    DebugWindow,
    I88n
  ;

  {*
     Creates an empty grid, that is, one without cells
     @param AOwner the form that owns this instance of the frame
  }
  constructor TFrameStringGridBase.create( AOwner: TComponent );
    begin
      inherited create( AOwner );
      dbcout( 'Creating TFrameStringGridBase', DBFRAMESTRINGGRIDBASE );

      stgGrid.SyncGrid := nil;

      dbcout( 'Done creating TFrameStringGridBase', DBFRAMESTRINGGRIDBASE );
    end
   ;


   /// Destroys the frame and frees memory
  destructor TFrameStringGridBase.destroy();
    begin
      dbcout( 'Destroying TFrameStringGridBase', DBFRAMESTRINGGRIDBASE );
      inherited destroy();
    end
  ;

  {*
    Get method for property options, returning the option settings for stgGrid
    @return a set of TGridOption enumerated values
  }
  function TFrameStringGridBase.getOptions(): TGridOptions;
    begin
      result := stgGrid.options;
    end
  ;

  {*
    Get method for property cells, returning the contents of a cell
    @param ACol column index, zero-based and should include any fixed columns
    @param ARow row index, zero-based and should include any fixed rows
    @return the contents of cell ACol,ARow or an empty string if the cell does not exist
  }
  function TFrameStringGridBase.getCell( ACol, ARow: integer ): string;
    begin
      result := stgGrid.Cells[ACol, ARow];
    end
  ;


  {*
    Set method for property cells, setting the contents of a cell
    @param ACol column index, zero-based and should include any fixed columns
    @param ARow row index, zero-based and should include any fixed rows
    @param val the value for cell ACol,ARow
  }
  procedure TFrameStringGridBase.setCell( ACol, ARow: integer; val: string );
    begin
      stgGrid.Cells[ACol, ARow] := val;
    end
  ;

  {*
     Set method for property colCount, setting the number of columns of stgGrid to val
  }
  procedure TFrameStringGridBase.setColCount( const val: integer );
    begin
      stgGrid.ColCount := val;
    end
  ;


  {*
     Set method for property rowCount, setting the number of rows of stgGrid to val
  }
  procedure TFrameStringGridBase.setRowCount( const val: integer );
    begin
      stgGrid.RowCount := val;
    end
  ;


  {*
     Set method for property fixedCols, setting the number of fixed columns of stgGrid to val
     @comment Fixed columns often contain a descriptive term/name for the column, not data.
  }
  procedure TFrameStringGridBase.setFixedCols( const val: integer );
    begin
      stgGrid.FixedCols := val;
    end
  ;


  {*
     Set method for property fixedRows, setting the number of fixed rows of stgGrid to val
     @comment Fixed rows often contain a descriptive term/name for the row, not data.
  }
  procedure TFrameStringGridBase.setFixedRows( const val: integer );
    begin
      stgGrid.FixedRows := val;
    end
  ;


  /// Get method for property fixedCols, returning the number of fixed columns in stgGrid
  function TFrameStringGridBase.getColCount(): integer;
    begin
      result := stgGrid.ColCount;
    end
  ;


  /// Get method for property rowCount, returning the number of rows in stgGrid
  function TFrameStringGridBase.getRowCount(): integer;
    begin
      result := stgGrid.RowCount;
    end
  ;


  /// Get method for property colCount, returning the number of columns in stgGrid
  function TFrameStringGridBase.getFixedCols(): integer;
    begin
      result := stgGrid.FixedCols;
    end
  ;


  /// Get method for property fixedRows, returning the number of fixed rows in stgGrid
  function TFrameStringGridBase.getFixedRows(): integer;
    begin
      result := stgGrid.FixedRows;
    end
  ;


  /// Set method for property colWidths, setting the width of column ACol to val
  procedure TFrameStringGridBase.setColWidth( ACol: longint; val: integer );
    begin
      stgGrid.ColWidths[ACol] := val;
    end
  ;


  /// Get method for property colWidths, returning the width of column ACol
  function TFrameStringGridBase.getColWidth( ACol: longint ): integer;
    begin
      result := stgGrid.ColWidths[ACol];
    end
  ;


  /// Set method for property rowHeights, setting the height of row ARow to val
  procedure TFrameStringGridBase.setRowHeight( ARow: longint; val: integer );
    begin
      stgGrid.RowHeights[ARow] := val;
    end
  ;


  /// Get method for property rowHeights, returning the height of row ARow
  function TFrameStringGridBase.getRowHeight( ARow: longint ): integer;
    begin
      result := stgGrid.RowHeights[ARow];
    end
  ;


  {*
    Deletes any data from the grid and optionally deletes values in the fixed cells
    @param clearFixedCells if true the cell contents of fixed rows and colummns are also cleared
  }
  procedure TFrameStringGridBase.clearGrid( clearFixedCells: boolean = false );
    var
      c, r: integer;
      sc, sr: integer;
    begin
      if( clearFixedCells ) then
        begin
          sc := 0;
          sr := 0;
        end
      else
        begin
          sc := stgGrid.fixedCols;
          sr := stgGrid.FixedRows;
        end
      ;

      for c := sc to stgGrid.colCount - 1 do
        begin
          for r := sr to stgGrid.rowCount - 1 do
            stgGrid.cells[c,r] := ''
          ;
        end
      ;
    end
  ;


  /// Set method for property rightSpace, setting the width of space on the right of the grid to val pixels
  procedure TFrameStringGridBase.setRightSpace( const val: integer );
    begin
      pnlSpacer.Width := val;
    end
  ;


  /// Get method for property rightSpace, returning the width of space on the right of the grid
  function TFrameStringGridBase.getRightSpace(): integer;
    begin
      result := pnlSpacer.Width;
    end
  ;


  /// Helper method for saveToFile(), returning a CSV formatted tabular string of the data and fixed cell values
  function TFrameStringGridBase.csvText(): string;
    var
      s, s2: string;
      c, r: integer;
    begin
      result := '';

      for r := 0 to stgGrid.RowCount - 1 do
        begin
          s := '';

          for c := 0 to stgGrid.ColCount - 1 do
            begin
              s2 := stgGrid.Cells[ c, r ];
              if( not( isNan( uiStrToFloat( s2, NaN ) ) ) ) then
                s2 := ansiReplaceStr( s2, SysUtils.DecimalSeparator, csvDecPt )
              ;
              s := s + s2;

              if( stgGrid.ColCount - 1 > c ) then
                s := s + csvListSep + ' '
              else
                s := s + endl
              ;
            end
          ;

          result := result + s;
        end
      ;
    end
  ;


  {*
    Saves the contents of the grid (data and fixed cell values) to a CVS formatted file
    @param fileName directory path and file name for the file to created
    @param header metadata concerning the file contents, each line should start with ##
    @return true if the file is written to disk, else false
    @comment If a file already exists named fileName it will be overwritten.
  }
  function TFrameStringGridBase.saveToFile( const fileName: string; header: string = '' ): boolean;
    var
      outFile: TextFile;
    begin
      try
        assignFile( outFile, fileName );
        rewrite( outFile );
        writeln( outFile, header );

        writeln( outFile, csvText() );

        closeFile( outFile );
        result := true;
      except
        result := false;
      end;
    end
  ;


  /// Clears the contents of column i, including any fixed cells
  procedure TFrameStringGridBase.clearColumn( const i: integer );
    var
      j: integer;
    begin
      for j := 0 to stgGrid.RowCount -1 do
        stgGrid.cells[i, j] := ''
      ;
    end
  ;


  //rbh20111012: Deprecated, Teixeira and Pacheco's method (printgrid()) seems better
  // Based on a demonstration written by Nilesh N Shah.  
  // See http://www.delphipages.com/tips/thread.cfm?ID=102
  {*
    Outputs the grid contents to the default printer
    @param pageTitle text to print as the page title, optional
  }
  procedure TFrameStringGridBase.printGrid2( pageTitle: string = '' );
    var
      recPrintStrGrid: RRecPrintStrGrid;
      iX1, iX2, iY0, iY1, iY2, iY3, iTmp: integer;
      //iWd: Integer;
      iLoop, jLoop: integer;
      trTextRect: TRect;
      colWidth: array of integer;
      //titleLines: TCStringList;
      //line: string;
    const
      COLSPACER: integer = 100;
      ROWSPACER: integer = 10;
    begin
      // Set up print options
      with recPrintStrGrid do
        begin
          PrCanvas := printer.Canvas;
          sGrid := self.stgGrid;
          sTitle := pageTitle;
          bPrintFlag := true;
          ptXYOffset.X := 10;
          ptXYOffset.Y := 100;

          ftTitleFont := TFont.Create();
          with ftTitleFont do
            begin
              Name := 'Arial';
              Style := [];
              Size := 10;
            end
          ;

          ftHeadingFont := TFont.Create();
          with ftHeadingFont do
            begin
              Name := 'Arial';
              Style := [fsBold];
              Size := 9;
            end
          ;

          ftDataFont := TFont.Create();
          with ftDataFont do
            begin
              Name := 'Arial';
              Style := [];
              Size := 9;
            end
          ;

          bBorderFlag := True;
        end
      ;

      recPrintStrGrid.PrCanvas.Font := recPrintStrGrid.ftDataFont;

      // Determine the maximum width of each column in the grid
      setLength( colWidth, recPrintStrGrid.sGrid.ColCount );

      for iLoop := 0 to recPrintStrGrid.sGrid.ColCount - 1 do
        colWidth[iLoop] := COLSPACER
      ;

      for iLoop := 0 to recPrintStrGrid.sGrid.ColCount - 1 do
        begin
          for jLoop := 0 to recPrintStrGrid.sGrid.RowCount -1 do
            begin
              if( recPrintStrGrid.PrCanvas.textWidth( recPrintStrGrid.sGrid.Cells[iLoop, jLoop] ) > colWidth[iLoop] ) then
                colWidth[iLoop] := recPrintStrGrid.PrCanvas.textWidth( recPrintStrGrid.sGrid.Cells[iLoop, jLoop] ) + COLSPACER
              ;
            end
          ;
        end
      ;

      (*
      // Calculate total width of the grid, based on the widest member of each column
      iWd := 0;
      for iLoop := 0 to recPrintStrGrid.sGrid.ColCount - 1 do
        iWd := iWd + colWidth[iLoop]
      ;
      *)

      with recPrintStrGrid, PrCanvas do
        begin
          //Initialize Printer
          if bPrintFlag then
            begin
              Printer.Title := sTitle;
              Printer.BeginDoc;
            end
          ;

          iY0 := ptXYOffset.Y;

          // FIX ME: title printing doesn't work well at all.
          (*
          //Output Title
          if( '' <> sTitle ) then
            begin
              Pen.Color := clBlack;
              Font := ftTitleFont;
              titleLines := TCStringList.create( sTitle, #10 );
              for iLoop := 0 to titleLines.Count do
                begin
                  TextOut(
                    ptXYOffset.X, // Left-aligned, instead of centered //((iWd Div 2) - (TextWidth(sTitle) Div 2))},
                    iY0,
                    trim( titleLines.at(iLoop) )
                  );
                 iY0 := iY0 + (TextHeight('Ag') + ROWSPACER);
                end
              ;
              titleLines.Free();
            end
          ;
          *)

          //Output Column Data
          for iLoop := 0 to sGrid.ColCount-1 do
            begin
              Font := ftHeadingFont;
              iX1 := ptXYOffset.X;
              iY1 := iY0;
              for iTmp := 0 to (iLoop-1) do
                iX1 := iX1 + colWidth[iTmp]
              ;

              iY1 := iY1 + ((TextHeight('Ag') + ROWSPACER) * 2);

              iX2 := ptXYOffset.X;

              for iTmp := 0 to iLoop do
                ix2 := ix2 + colWidth[iTmp]
              ;

              iY2 := iY1 + TextHeight('Ag');

              trTextRect := Rect(iX1, iY1, iX2, iY2);
              dbcout( 'iX1: ' + intToStr( iX1 ) + ', iX2: ' + intToStr( iX2 ), DBFRAMESTRINGGRIDBASE );
              dbcout( 'ColWidth: ' + intToSTr( colWidth[iLoop] ), DBFRAMESTRINGGRIDBASE );
              TextRect(trTextRect, trTextRect.Left + (COLSPACER div 2), trTextRect.Top+3, sGrid.Cells[iLoop, 0] );
              Brush.Color := clBlack;
              if bBorderFlag then FrameRect(trTextRect);
              Brush.Style := bsClear;


              //Output Row Data
              Font := ftDataFont;
              iY1 := iY2;
              iY3 := TextHeight('Ag') + ROWSPACER;
              for iTmp := 1 to sGrid.RowCount-1 do
                begin
                  iY2 := iY1 + iY3;
                  trTextRect := Rect(iX1, iY1, iX2, iY2);
                  TextRect(trTextRect, trTextRect.Left + 5, trTextRect.Top+3, sGrid.Cells[iLoop, iTmp]);
                  Brush.Color := clBlack;
                  if bBorderFlag then FrameRect(trTextRect);
                  Brush.Style := bsClear;
                  iY1 := iY1 + iY3;
                end
              ;
              
            end
          ;

          if bPrintFlag then Printer.EndDoc;
        end // with ArecPrintStrGrid, prCanvas
      ;

      //clean up
        setLength( colWidth, 0 );
        recPrintStrGrid.ftTitleFont.free();
        recPrintStrGrid.ftHeadingFont.free();
        recPrintStrGrid.ftDataFont.free();
    end
  ;

  
  // Based on Chapter 10 Delphi 5 Developer's Guide (Teixeira and Pacheco, 2000)
  {*
    Outputs the grid contents to the default printer
    @param pageTitle text to print as the page title, optional
    @param reportHeader output from FormOutputStats.textHeader(), optional
    @Comment bBorderFlag is not currently implement, Aaron please see Fix Me below.
  }
  procedure TFrameStringGridBase.printGrid( pageTitle: string = ''; reportHeader: string = '' );

    // inline proc (recPrintStrGrid HAS to be passed in by reference, not value)
    procedure PrintRow(var Items: TStringList; var recPrintStrGrid: RRecPrintStrGrid);
      var
        OutRect: TRect;
        i: integer;
      begin
        // First position the print rect on the print canvas
        OutRect.Left := 0;
        OutRect.Top := recPrintStrGrid.AmountPrinted;
        OutRect.Bottom := OutRect.Top + recPrintStrGrid.lineHeight;

        for i := 0 to Items.Count - 1 do
          begin
            // Determine Right edge
            OutRect.Right := OutRect.Left + longint(Items.Objects[i]);
            // Print the line
            recPrintStrGrid.PrCanvas.TextRect(OutRect, OutRect.Left, OutRect.Top, Items[i]);
            // Adjust right edge
            OutRect.Left := OutRect.Right;

            // The code below prints vertical column lines but does leaves a space between lines of print, ugly
            //recPrintStrGrid.PrCanvas.Brush.Color := clBlack;
            //if recPrintStrGrid.bBorderFlag then recPrintStrGrid.PrCanvas.FrameRect(OutRect);
            //recPrintStrGrid.PrCanvas.Brush.Style := bsClear;
          end
        ;

        //As each line prints, AmountPrinted must increase to reflect how much of a page has been printed
        //recPrintStrGrid.AmountPrinted := recPrintStrGrid.AmountPrinted + (recPrintStrGrid.tenthsOfInchPixelsY * 2);
        recPrintStrGrid.amountPrinted := recPrintStrGrid.amountPrinted + recPrintStrGrid.lineHeight + recPrintStrGrid.tenthsOfInchPixelsY;
      end
    ;

    // inline proc
    procedure PrintTitle(var recPrintStrGrid: RRecPrintStrGrid; title: string);
      begin
        if (title = '') then exit;
        recPrintStrGrid.PrCanvas.Font := recPrintStrGrid.ftTitleFont;

        recPrintStrGrid.PrCanvas.TextOut((Printer.PageWidth div 2) -
          (recPrintStrGrid.PrCanvas.TextWidth(title) div 2),0,title);
          
        recPrintStrGrid.PrCanvas.Font := recPrintStrGrid.ftDataFont;
        // Increment amount printed
        recPrintStrGrid.amountPrinted := recPrintStrGrid.amountPrinted + recPrintStrGrid.lineHeight + recPrintStrGrid.tenthsOfInchPixelsY;
      end
    ;

    // inline proc
    procedure PrintReportHeader(var recPrintStrGrid: RRecPrintStrGrid; header: string);
      var
        headerLines: TCStringList;
        i: integer;
        abbrevFilePath: string;
      begin
        // expecting header to be a endl-delimited string created by TFormOutputStats.textHeader()
        if (header = '') then exit;
        
        headerLines := TCStringList.Create();
        recPrintStrGrid.PrCanvas.Font := recPrintStrGrid.ftDataFont; // no special formatting

        try
          headerLines.explode(header, #13);  // the #10 of endl(#13#10) is trimmed away by explode

          for i := 0 to headerLines.Count-1 do
            begin
              // The path and file name can exceed the printer page width, and the file name would be lost
              if ( pos('## Scenario file:', headerLines.Strings[i]) > 0 ) then
                begin
                  if (100 > length(headerLines.Strings[i])) then  // not sure how many characters to consider is best
                    recPrintStrGrid.PrCanvas.TextOut(0,recPrintStrGrid.amountPrinted,headerLines.Strings[i])
                  else
                    begin
                      abbrevFilePath := leftstr(headerLines.Strings[i], 17) + ' ... ' + rightstr(headerLines.Strings[i],78);
                      recPrintStrGrid.PrCanvas.TextOut(0,recPrintStrGrid.amountPrinted,abbrevFilePath)
                    end
                  ;
                end
              else // some other line in the header other than the scenario file path
                recPrintStrGrid.PrCanvas.TextOut(0,recPrintStrGrid.amountPrinted,headerLines.Strings[i]);

              // Increment amount printed for every line in the header
              recPrintStrGrid.amountPrinted := recPrintStrGrid.amountPrinted + recPrintStrGrid.lineHeight + recPrintStrGrid.tenthsOfInchPixelsY;
            end
          ;
        finally
          headerLines.Free;
        end;
      end
    ;

    // inline proc
    procedure PrintColumnNames(var ColNames: TStringList; var recPrintStrGrid: RRecPrintStrGrid ); 
      begin
        recPrintStrGrid.PrCanvas.Font := recPrintStrGrid.ftHeadingFont;
        PrintRow(ColNames, recPrintStrGrid);
        recPrintStrGrid.PrCanvas.Font := recPrintStrGrid.ftDataFont
      end
    ;

    var
      recPrintStrGrid: RRecPrintStrGrid;
      iLoop, jLoop, width, pageNum: integer;
      colWidth: array of integer; // the number of pixels needed for each column of the grid
      colNames: TStringList; // the header row of column names and column widths
      dataLine: TStringList; // contents of a row cells and their column widths
      FXScale: double; // factor to adjust for the diffences in horizonatal pixel size between screen and printer
      timeStamp, title: string; // info for the print title
      printWidth: integer;  // width of the info being set to the printer

    const
      COLSPACER: integer = 50; // 50 maintains portrait orientation for the summary output statistics grid
    begin

      recPrintStrGrid.amountPrinted := 0;
      colNames := TStringList.Create();
      dataLine := TStringList.Create();

      try
        // Set up print options
        with recPrintStrGrid do
          begin
            PrCanvas := printer.Canvas;
            sGrid := self.stgGrid;
            sTitle := pageTitle;
            bPrintFlag := true;
            ptXYOffset.X := 10;
            ptXYOffset.Y := 100;
            bBorderFlag := True;

            ftTitleFont := TFont.Create();
            with ftTitleFont do
              begin
                Name := 'Arial';
                Style := []; //fsBold
                Size := 8;
              end
            ;

            ftHeadingFont := TFont.Create();
            with ftHeadingFont do
              begin
                Name := 'Arial';
                Style := [fsUnderline];
                Size := 8;
              end
            ;

            ftDataFont := TFont.Create();
            with ftDataFont do
              begin
                Name := 'Arial';
                Style := [];
                Size := 8;
              end
            ;

            // base line height on text height of currently rendered font
            Font := ftDataFont;
            tenthsOfInchPixelsY := GetDeviceCaps(Printer.Handle, LOGPIXELSY) div 10;
            lineHeight := PrCanvas.TextHeight('X') + tenthsOfInchPixelsY;
          end
        ;
        // Account for difference of pixel size between screen (grid cell contents) and printer
        //http://stackoverflow.com/questions/3100895/scaling-pixelvalues-for-printing
        FXScale := (GetDeviceCaps(Printer.Handle, LOGPIXELSX)/96) -
          ((2-(GetDeviceCaps(Printer.Handle, HORZRES)*2) /
          GetDeviceCaps(Printer.Handle, PHYSICALWIDTH)));

        recPrintStrGrid.amountPrinted := recPrintStrGrid.tenthsOfInchPixelsY * 2;
        pageNum := 1;
        timeStamp := FormatDateTime('c', Now);
        
        // Determine the maximum width (as pixels) of each column in the grid
        setLength( colWidth, recPrintStrGrid.sGrid.ColCount );

        for iLoop := 0 to recPrintStrGrid.sGrid.ColCount - 1 do
          colWidth[iLoop] := round(COLSPACER * FXScale);
        ;

        for iLoop := 0 to recPrintStrGrid.sGrid.ColCount - 1 do
          begin
            for jLoop := 0 to recPrintStrGrid.sGrid.RowCount -1 do
              begin
                if( round((recPrintStrGrid.PrCanvas.textWidth( recPrintStrGrid.sGrid.Cells[iLoop, jLoop] )* FXScale)) > colWidth[iLoop] ) then
                  colWidth[iLoop] := round(recPrintStrGrid.PrCanvas.textWidth( recPrintStrGrid.sGrid.Cells[iLoop, jLoop]) * FXScale) + COLSPACER
                ;
              end
            ;
          end
        ;

        // Calculate total width of the grid, based on the widest member of each column
        printWidth := 0;
        for iLoop := 0 to recPrintStrGrid.sGrid.ColCount - 1 do
          printWidth := printWidth + colWidth[iLoop]
        ;

        // If the grid contents print width is greater than the printer page width go to landscape mode
        if recPrintStrGrid.bPrintFlag then
          if ( printWidth > Printer.PageWidth ) then Printer.Orientation := poLandscape;

        // Create a header row of column names, here assuming that the first row of the grid
        // always has a header row (maybe re-visit this assumption)
        for iLoop := 0 to recPrintStrGrid.sGrid.ColCount - 1 do
          begin
            // Store each column header name and the width (pixels) of the column
            width := colWidth[iLoop];
            ColNames.AddObject( recPrintStrGrid.sGrid.Cells[iLoop, 0] , pointer( width ));
          end
        ;

        with recPrintStrGrid, PrCanvas do
          begin
            //Initialize Printer
            if bPrintFlag then
              begin
                Printer.Title := sTitle;
                Printer.BeginDoc;
              end
            ;

            // sTitle may often be an empty string, but it doesn't matter
            title := recPrintStrGrid.sTitle + '  ' + timeStamp + '  ' + 'page ' + intToStr(pageNum);
            if bPrintFlag then
              begin
                if (reportHeader <> '') then // print only at the top of the 1st page
                  PrintReportHeader(recPrintStrGrid, reportHeader)
                else
                  PrintTitle( recPrintStrGrid, title );
              end
            ;

            PrintColumnNames( colNames, recPrintStrGrid ); // always do this once

            for iLoop := 1 to sGrid.RowCount-1 do  // skip header row
              begin

                for jLoop := 0 to sGrid.ColCount-1 do
                  begin
                    width := colWidth[jLoop];
                    dataLine.AddObject(sGrid.Cells[jLoop, iLoop], pointer(width));
                  end
                ;
                PrintRow(dataLine, recPrintStrGrid);
                dataLine.Clear;

                // Force print job to begin a new page if printed output has exceeded page height
                if bPrintFlag then
                  begin
                    if ( (amountPrinted + lineHeight) > Printer.PageHeight ) then
                      begin
                        amountPrinted := tenthsOfInchPixelsY * 2;
                        inc(pageNum);

                        Printer.NewPage;
                        title := sTitle + '  ' + timeStamp + '  ' + 'page ' + intToStr(pageNum);
                        PrintTitle( recPrintStrGrid, title );
                        PrintColumnNames( colNames, recPrintStrGrid ); // do this again at top of each page
                      end
                    ;
                  end
                ;
              end  // for row loop
            ;

            if bPrintFlag then
              begin
                Printer.EndDoc;
                Printer.Orientation := poPortrait;
              end
            ;
          end // with recPrintStrGrid, prCanvas
        ;

      finally
        //clean up
        setLength( colWidth, 0 );
        recPrintStrGrid.ftTitleFont.free();
        recPrintStrGrid.ftHeadingFont.free();
        recPrintStrGrid.ftDataFont.free();
        colNames.Free;
        dataLine.Free;
      end;
    end
  ;
  

  {*
    Debugging method that sends the value of ARow to the debug output window
    @param Sender a grid object referencing this method in OnSelectCell event handler
    @param ACol grid column number
    @param ARow grid row number
    @param CanSelect ?
    @comment It looks like this method is not fully implemented ...
  }
  procedure TFrameStringGridBase.stgGridSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    begin
      dbcout( 'Row selected: ' + intToStr( ARow ), true );
    end
  ;



  {*
    Bug fix for limitation of base component in Delphi 7.
    This avoids a runtime "Grid index out of range." error if the user
    enters the grid by clicking a fixed column and then mouse-wheels.
    @param Sender a grid object referencing this method in OnEnter event handler
  }
  procedure TFrameStringGridBase.stgGridEnter(Sender: TObject);
    begin
      {
        This avoids a runtime "Grid index out of range." error if the user
        enters the grid by clicking a fixed column and then mouse-wheels.
        The problem is that cells in a fixed column can not be selected, so
        the component has focus but Col and Row = -1. Using the mouse wheel
        fires TGrid's current() where a check is done. If the current position is
        -1 it raises a EInvalidGridOperation error. Setting selection avoids the error.
        Using FixedRows and FixedCols counts for the rectangle coordinates selects the upper
        left most data cell and the remaining cells of that row, no matter how many fixed rows or columns are used.
      }
      if (stgGrid.Col = -1) then
        begin
          stgGrid.Selection := TGridRect(Rect(stgGrid.FixedCols,stgGrid.FixedRows,(stgGrid.ColCount - stgGrid.FixedCols),stgGrid.FixedRows));
        end
      ;
    end
  ;

end.