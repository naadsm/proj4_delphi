unit FormFolderSelect;

(*
FormFolderSelect.pas
--------------------
Begin: 2007/02/02
Last revision: $Date: 2009-09-08 21:49:25 $ $Author: areeves $
Version number: $Revision: 1.2 $
Project: APHI General Purpose Delphi Library
Website: http://www.naadsm.org/opensource/delphi
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2007 - 2008 Animal Population Health Institute, Colorado State University

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
    ComCtrls,
    ExtCtrls,
    Buttons,
    {$WARNINGS OFF}
    ShellCtrls,
    {$WARNINGS ON}
    QLists
  ;

  type TFormFolderSelect = class( TForm )
      treeView: TShellTreeView;
      pnlBottom: TPanel;
      lblSelectedFolder: TLabel;
      lblFolder: TLabel;
      pnlTop: TPanel;
      pnlSpacer: TPanel;
      pnlButtons: TPanel;
      pnlNewFolder: TPanel;
      pnlTopContainer: TPanel;
      lblHeader: TLabel;
      pnlButtonsRight: TPanel;
      btnOK: TButton;
      btnCancel: TButton;
      btnNewFolder: TBitBtn;

      procedure treeViewChange(Sender: TObject; Node: TTreeNode);
      procedure btnNewFolderClick(Sender: TObject);

      procedure btnOKClick(Sender: TObject);
      procedure btnCancelClick(Sender: TObject);
      procedure treeViewExpanded(Sender: TObject; Node: TTreeNode);

    protected
      _selectedDir: string;
      _foldersToHide: TQStringList;

      procedure translateUI();
      procedure translateUIManual();

      procedure fillFoldersToHide();

      function getSelectedDirectory(): string;

    public
      constructor create( AOwner: TComponent ); override;
      destructor destroy(); override;

      property selectedDirectory: string read getSelectedDirectory;
    end
  ;


implementation

{$R *.dfm}

  uses
    StrUtils,
    
    DebugWindow,
    WindowsUtils,
    MyStrUtils,
    MyDialogs,
    I88n
  ;

  constructor TFormFolderSelect.create( AOwner: TComponent );
    var
      i,y: integer;
    begin
      inherited create( AOwner );
      translateUI();

      _selectedDir := '';

      pnlSpacer.BevelOuter := bvNone;
      pnlButtons.BevelOuter := bvNone;

      _foldersToHide := TQStringList.create();
      fillFoldersToHide();

      y := 0;
      for i := 0 to treeView.Items[0].Count - 1 do
        begin
          if( _foldersToHide.contains( treeView.Items[0].Item[y].Text ) ) then
            treeView.Items[0].Item[y].Delete()
          else
            inc(y)
          ;
        end
      ;
    end
  ;


  procedure TFormFolderSelect.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.2.
      // Generation date: Thu Feb 28 14:04:23 2008
      // File name: C:/libs/delphi/general_purpose/forms/test.dfm
      // File date: Thu Feb 28 13:50:22 2008

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Browse for folder' );
          lblSelectedFolder.Caption := tr( 'Selected folder:' );
          lblFolder.Caption := tr( 'lblFolder' );
          btnOK.Caption := tr( 'OK' );
          btnCancel.Caption := tr( 'Cancel' );
          btnNewFolder.Hint := tr( 'Create new folder' );
          lblHeader.Caption := tr( 'Select a folder from the list below:' );
        end
      ;

      // If any phrases are found that could not be automatically extracted by
      // Caption Collector, modify the following function to take care of them.
      // Otherwise, this function will be empty:
      translateUIManual();
    end
  ;


  procedure TFormFolderSelect.translateUIManual();
    begin
    end
  ;


  destructor TFormFolderSelect.destroy();
    begin
      _foldersToHide.free();
      inherited destroy();
    end
  ;


  procedure TFormFolderSelect.fillFoldersToHide();
    begin
      _foldersToHide.append( 'Recycle Bin' );
      _foldersToHide.append( 'Control Panel' );
    end
  ;


  function TFormFolderSelect.getSelectedDirectory(): string;
    begin
      result := _selectedDir;
    end
  ;


  procedure TFormFolderSelect.treeViewChange(Sender: TObject; Node: TTreeNode);
    begin
      if( canWriteToDirectory( treeView.SelectedFolder.PathName ) ) then
        begin
          lblFolder.Caption := abbrevPath( treeView.SelectedFolder.PathName, 60 );
          _selectedDir := treeView.SelectedFolder.PathName;
        end
      else
        begin
          lblFolder.Caption := tr( '(Selected folder is read-only)' );
          _selectedDir := '';
        end
      ;
    end
  ;


  procedure TFormFolderSelect.btnNewFolderClick(Sender: TObject);
    var
      i: integer;
      success: boolean;
      newFolder: string;

      function removeSlashes( str: string ): string;
        begin
          result := str;
          if( ( '\' = leftStr( result, 1 ) ) or ( '/' = leftStr(result, 1 ) ) ) then
            result := removeSlashes( rightStr( result, length( result ) - 1 ) )
          ;
          if( ( '\' = rightStr( result, 1 ) ) or ( '/' = rightStr(result, 1 ) ) ) then
            result := removeSlashes( leftStr( result, length( result ) - 1 ) )
          ;
        end
      ;
    begin
      newFolder := msgInput(
        tr( 'Please enter a name for the new folder:' ),
        '',
        tr( 'New folder name' ),
        IMGQuestion,
        self
      );

      newFolder := removeSlashes( trim( newFolder ) );

      // If the folder already exists, alert the user, select it, and exit.
      if( directoryExists( treeView.SelectedFolder.PathName + '\' + newFolder ) ) then
        begin
          msgOK(
            tr( 'A folder with this name already exists.' ),
            tr( 'Folder creation failed' ),
            IMGInformation,
            self
          );

          for i := 0 to treeView.Selected.Count - 1 do
            begin
              if( newFolder = treeView.Selected.Item[i].Text ) then
                begin
                  treeView.Selected.Item[i].Selected := true;
                  break;
                end
              ;
            end
          ;
          treeView.Selected.Focused := true;
          treeView.SetFocus();
          exit;
        end
      ;

      // Otherwise, try to create the folder.
      try
        success := forceDirectories( treeView.SelectedFolder.PathName + '\' + newFolder );
      except
        success := false;
      end;

      // If the folder was created, select it.
      // If it couldn't be created, display an error message.
      if( success ) then
        begin
          treeView.Refresh( treeView.Selected );
          treeView.Selected.Expand( false );

          for i := 0 to treeView.Selected.Count - 1 do
            begin
              if( newFolder = treeView.Selected.Item[i].Text ) then
                begin
                  treeView.Selected.Item[i].Selected := true;
                  break;
                end
              ;
            end
          ;
          treeView.Selected.Focused := true;
          treeView.SetFocus();
        end
      else
        begin
          msgOK(
            tr( 'The new folder could not be created: you may not have sufficient permission to create it in the selected folder.' ),
            tr( 'Folder creation failed' ),
            IMGWarning,
            self
          );
        end
      ;
    end
  ;


  procedure TFormFolderSelect.btnOKClick(Sender: TObject);
    begin
      // If the selected directory is can't be written to, show an error message.
      // Otherwise, all is good: close the form and move on.
      if( canWriteToDirectory( treeView.SelectedFolder.PathName ) ) then
        begin
          _selectedDir := treeView.SelectedFolder.PathName;
          ModalResult := mrOK;
        end
      else
        begin
          msgOK(
            tr( 'The selected folder cannot be written to: you may not have sufficient permission to write to it.  Please select a different folder.' ),
            tr( 'Folder is read-only' ),
            IMGWarning,
            self
          );
          _selectedDir := '';
        end
      ;
    end
  ;


  procedure TFormFolderSelect.btnCancelClick(Sender: TObject);
    begin
      _selectedDir := '';
      ModalResult := mrCancel;
    end
  ;


  procedure TFormFolderSelect.treeViewExpanded(Sender: TObject; Node: TTreeNode);
    var
      i, y: integer;
    begin
      if( nil <> _foldersToHide ) then
        begin
          y := 0;
          for i := 0 to node.Count - 1 do
            begin
              if( _foldersToHide.contains( node.Item[y].Text ) ) then
                node.Item[y].Delete()
              else
                inc(y)
              ;
            end
          ;
        end
      ;
    end
  ;

end.
