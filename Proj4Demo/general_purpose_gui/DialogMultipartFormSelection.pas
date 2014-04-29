unit DialogMultipartFormSelection;

(*
DialogMultipartFormSelection.pas/dfm
-------------------------------------
Begin: 2005/08/15
Last revision: $Date: 2008-12-10 21:03:31 $ $Author: areeves $
Version number: $Revision: 1.9 $
Project: APHI General Purpose Delphi Libary
Website: http://www.naadsm.org/opensource/delphi/
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
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
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  Contnrs,

  //cArrays,
  //cDictionaries,
  QStringMaps,
  
  FrameFileSelector
  ;

  type TDlgMultipartAction = (
    MULTIPARTNONE,
    MULTIPARTSAVE,
    MULTIPARTCOPY,
    MULTIPARTPRINT
  );

  type TDialogMultipartFormSelection = class( TForm )
      fraFileSelector: TFrameFileSelector;
      pnlPartsList: TPanel;
      gbxItems: TGroupBox;
      pnlButtons: TPanel;
      btnCancel: TButton;
      btnOK: TButton;
      
      procedure FormCreate(Sender: TObject);
      procedure btnOKClick(Sender: TObject);
      procedure btnCancelClick(Sender: TObject);

    protected
      _checkboxes: TObjectList;
      _accepted: boolean;

      procedure translateUI();

      function getFileName(): string;

    public
      constructor create(
        AOwner: TComponent;
        //objDict: TObjectDictionary;
        objDict: TQStringObjectMap;
        activity: TDlgMultipartAction;
        filter: string = ''
      ); reintroduce;

      destructor destroy(); override;

      function execute(): boolean;

      function createSelectedIndexArray(): TIntegerArray;

      property fileName: string read getFileName;
    end
  ;


implementation

{$R *.dfm}
  uses
    myStrUtils,
    MyDialogs,
    I88n
  ;

  constructor TDialogMultipartFormSelection.create(
        AOwner: TComponent;
        //objDict: TObjectDictionary;
        objDict: TQStringObjectMap;
        
        activity: TDlgMultipartAction;
        filter: string = ''
      );
    var
      i: integer;
      cbx: TCheckBox;
    begin
      inherited create( AOwner );
      translateUI();
      
      // Deal with form scaling
      //-----------------------
      Assert(not Scaled, 'You should set Scaled property of Form to False!');

      // Set this value to the PPI AT WHICH THE FORM WAS ORIGINALLY DESIGNED!!
      self.PixelsPerInch := 96;

      // FormCreate() will handle the rest.

      case activity of
        MULTIPARTSAVE:
          begin
            gbxItems.Caption := tr( 'Items to save:' ) + ' ';
            fraFileSelector.Visible := true;
          end
        ;
        MULTIPARTCOPY:
          begin
            gbxItems.Caption := tr( 'Items to copy:' ) + ' ';
            fraFileSelector.Visible := false;
          end
        ;
        else
          begin
            raise exception.Create( 'Unsupported action in TDialogMultipartFormSelection.create()' );
          end
        ;
      end;

      fraFileSelector.filter := filter;

      _checkboxes := TObjectList.create( false );

      for i := 0 to objDict.count - 1 do
        begin
          cbx := TCheckBox.Create( gbxItems );
          cbx.Caption := objDict.GetKeyByIndex( i );
          cbx.Parent := gbxItems;
          cbx.Height := 25;
          cbx.Left := 25;
          cbx.Top := 20 + ( ( 10  + cbx.Height ) * i );
          cbx.Checked := true;
          cbx.Show();
          _checkboxes.Add( cbx );
        end
      ;

    end
  ;


  procedure TDialogMultipartFormSelection.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 15:29:38 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/general_purpose_gui/DialogMultipartFormSelection.dfm
      // File date: Tue Oct 10 08:22:48 2006

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Form1' );
          gbxItems.Caption := tr( 'Items to _____:' );
          btnCancel.Caption := tr( 'Cancel' );
          btnOK.Caption := tr( 'OK' );
        end
      ;

    end
  ;


	procedure TDialogMultipartFormSelection.FormCreate(Sender: TObject);
		begin
      if Screen.PixelsPerInch <> PixelsPerInch then
        ScaleBy( Screen.PixelsPerInch, PixelsPerInch )
      ;
		end
	;
	
  
  destructor TDialogMultipartFormSelection.destroy();
    begin
      _checkboxes.free();
      inherited destroy();
    end
  ;


  function TDialogMultipartFormSelection.createSelectedIndexArray(): TIntegerArray;
    var
      i: integer;
      arr: TIntegerArray;
      cbx: TCheckbox;
    begin
      arr := TIntegerArray.Create();

      for i := 0 to _checkboxes.Count - 1 do
        begin
          cbx := _checkboxes.items[i] as TCheckbox;
          if( cbx.Checked ) then
            arr.AppendItem(i)
          ;
        end
      ;

      result := arr;
    end
  ;


  function TDialogMultipartFormSelection.execute(): boolean;
    begin
      self.ShowModal();
      // The user will do stuff for a while here.

      // When showModal() eventually returns...
      result := _accepted;
    end
  ;


  procedure TDialogMultipartFormSelection.btnOKClick(Sender: TObject);
    begin
      if( fraFileSelector.Visible and ( '' = fileName ) ) then
        begin
          msgOK( tr( 'Please enter a file name.' ), tr( 'Missing file name' ), IMGWarning, self );
          fraFileSelector.SetFocus();
        end
      else
        begin
          _accepted := true;
          close();
        end
      ;
    end
  ;


  procedure TDialogMultipartFormSelection.btnCancelClick(Sender: TObject);
    begin
      _accepted := false;
      close();
    end
  ;

  
  function TDialogMultipartFormSelection.getFileName(): string;
    begin
      result := fraFileSelector.fileName;
    end
  ;

end.
