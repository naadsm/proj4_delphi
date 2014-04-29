{*
FrameFileSelector.pas/dfm
-------------------------
Begin: 2005/08/04
Last revision: $Date: 2010-11-03 18:41:47 $ $Author: rhupalo $
Version number: $Revision: 1.14 $
Project: APHI General Purpose Delphi Libary
Website: http://www.naadsm.org/opensource/delphi/
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2005 - 2008 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
---------------------------------------------------

Widget to add to forms where the user needs to create the full path name for
some export file that will be created.
}

(*
  Documentation generation tags begin with {* or ///
  Replacing these with (* or // foils the documentation generator
*)


unit FrameFileSelector;

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
    StdCtrls
  ;

  
  type
  /// FFrame providing components to browse to a folder and to input a file name.
  TFrameFileSelector = class( TFrame )
      leFileName: TEdit;       /// for file name input and editing
      btnBrowse: TButton;      /// for browsing to or creating a folder location
      SaveDialog1: TSaveDialog;/// for obtaining the directory path
      lblFileName: TLabel;
      procedure btnBrowseClick(Sender: TObject);

    protected
      _dir: string;  /// the selected directory path
      _fileNameExtension: string; /// file format extension
      _enabled: boolean;          /// disables or enables the input components

      procedure translateUI();

      procedure setDirectory( dirName: string );
      function getDirectory(): string;

      procedure setFileName( fn: string );
      function getFileName(): string;

      procedure setFilter( flt: string );
      function getFilter(): string;

      function getFileNameExtension(): string;
      procedure setFileNameExtension( val: string );

      function getFrameFileSelectorEnabled(): boolean;
      procedure setFrameFileSelectorEnabled( val: boolean );

    public
      constructor create( AOwner: TComponent ); overload; override;
      constructor create( AOwner: TComponent; pathName: string; filter: string ); reintroduce; overload;

      procedure setFocus(); override;

      /// provides access to _dir
      property directory: string read getDirectory write setDirectory;
      /// provides access to the directory path and filename
      property fileName: string read getFileName write setFileName;
      /// provides access to the file extension filter
      property filter: string read getFilter write setFilter;
      /// provides access to _fileNameExtension
      property fileNameExtension: string read getFileNameExtension write setFileNameExtension;
      /// provides access to _enabled
      property enabled: boolean read getFrameFileSelectorEnabled write setFrameFileSelectorEnabled;
    end
  ;

implementation

{$R *.dfm}

  uses
    DebugWindow,
    MyStrUtils,
    StrUtils,
    I88n
  ;

  {*
    Creates an instance of the frame
    @param AOwner the form that owns this instance of the frame
  }
  constructor TFrameFileSelector.create( AOwner: TComponent );
    begin
      inherited create( AOwner );
      translateUI();

      _enabled := true;
      leFileName.Text := '';
      setFilter( '' );
      _dir := '';
    end
  ;


  {*
    Creates an instance of the frame and itializes some of the file parameters
    @param AOwner the form that owns this instance of the frame
    @param pathName currently not used ...
    @param filter currently not used ...
  }
  constructor TFrameFileSelector.create( AOwner: TComponent; pathName: string; filter: string );
    begin
      // rbh20101102: Fix Me - was it intended that pathName and filter be used to set private members?
      inherited create( AOwner );
      translateUI();

      _enabled := true;

      leFileName.Text := trim( fileName );
      setFilter( '' );

      if( '' <> leFileName.Text ) then
        _dir := MyStrUtils.directory( leFileName.Text )
      else
        _dir := ''
      ;
    end
  ;

  /// Specifies the captions, hints, and other component text phrases for translation
  procedure TFrameFileSelector.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 15:29:38 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/general_purpose_gui/FrameFileSelector.dfm
      // File date: Thu Sep 1 12:33:53 2005

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          lblFileName.Caption := tr( 'File name:' );
          btnBrowse.Caption := tr( 'Browse...' );
          SaveDialog1.Filter := tr( 'All files (*.*)|*.*|Comma separated values (*.csv)|*.csv' );
        end
      ;

    end
  ;


  {*
     Does all the normal things associated with a file browser save dialog
     @params Sender the component instance generating the click event
  }
  procedure TFrameFileSelector.btnBrowseClick(Sender: TObject);
    var
      tmpName: string;
    begin
      saveDialog1.Title := tr( 'Save as' );

      if( length( trim(leFileName.text) ) > 0 ) then
        saveDialog1.fileName := trim( leFileName.text )
      else if( '' <> _dir ) then
        saveDialog1.initialDir := _dir
      ;

      if saveDialog1.Execute() then
        begin
          _dir := MyStrUtils.directory( saveDialog1.FileName );
          tmpName := trim( saveDialog1.fileName );

          if( ansiLowerCase( rightStr( tmpName, length( trim( _fileNameExtension ) ) ) ) <> fixup( _fileNameExtension ) ) then
            tmpName := tmpName + _fileNameExtension
          ;

          leFileName.Text := tmpName;
        end
      ;
    end
  ;


  /// Directs focus to the filename TEdit control
  procedure TFrameFileSelector.setFocus();
    begin
      leFileName.SetFocus();
    end
  ;


  /// Set method for property directory, setting _dir to dirName
  procedure TFrameFileSelector.setDirectory( dirName: string );
    begin
      _dir := dirName;
    end
  ;

  /// Get method for property directory, returning the value of _dir
  function TFrameFileSelector.getDirectory(): string;
    begin
      result := _dir;
    end
  ;


  {*
    Set method for property fileName and parses out a value for _dir
    @param fn full directory path and filename
  }
  procedure TFrameFileSelector.setFileName( fn: string );
    begin
      leFileName.Text := trim( fn );
      _dir := MyStrUtils.directory( leFileName.Text);
    end
  ;

  /// Get method for property fileName, returning the value in leFileName.Text
  function TFrameFileSelector.getFileName(): string;
    begin
      result := trim( leFileName.Text );
    end
  ;


  {*
    Sets the filter for the SaveDialog component, affecting what file types are shown in the file browser
    @param flt filter text string, like: CSV Files (*.csv)|*.csv
  }
  procedure TFrameFileSelector.setFilter( flt: string );
    begin
      if( '' = flt ) then
        SaveDialog1.Filter := tr( 'All Files (*.*)|*.*' )
      else
        begin
          try
            SaveDialog1.Filter := flt;
          except
            SaveDialog1.Filter := tr( 'All Files (*.*)|*.*' );
          end;
        end
      ;
    end
  ;

  /// Get method for property filter, returning the current SaveDialog file browser filter string
  function TFrameFileSelector.getFilter(): string; begin result := SaveDialog1.Filter; end;


  /// Get function for property fileNameExtension
  function TFrameFileSelector.getFileNameExtension(): string;
    begin
      if( 0 < length( fileName ) ) then
        begin
          result := extractFileExt( fileName );
          _fileNameExtension := result;
        end
      else
        setFileNameExtension( result )
      ;
    end
  ;


  {*
     Set method for property fileExtension. Sets a new default file format extension
     in the saveDialog component and edits the file name in the edit control,
     setting the extension to val.
     @param val the new file extension to use
  }
  procedure TFrameFileSelector.setFileNameExtension( val: string );
    begin
      _fileNameExtension := val;

      SaveDialog1.DefaultExt := ansiRightStr( _fileNameExtension, 3 );

      if( 0 < length( fileName ) ) then
        leFileName.Text := changeFileExt( fileName, _fileNameExtension )
      ;
    end
  ;

  /// Get method for property enabled, returning the value of _enabled
  function TFrameFileSelector.getFrameFileSelectorEnabled(): boolean;
    begin
      result := _enabled;
    end
  ;

  {*
    Set method for property enabled and mechanism to disable or enable all the
    components on the frame.
    @param val true enables the components and false disables the components
  }
  procedure TFrameFileSelector.setFrameFileSelectorEnabled( val: boolean );
    begin
      _enabled := val;

      lblFileName.Enabled := val;
      leFileName.Enabled := val;
      btnBrowse.Enabled := val;

      repaint();
    end
  ;

end.
