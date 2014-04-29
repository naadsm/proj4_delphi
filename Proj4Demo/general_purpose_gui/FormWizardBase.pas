unit FormWizardBase;

(*
FormWizardBase.pas/dfm
----------------------
Begin: 2005/06/17
Last revision: $Date: 2008-12-10 21:03:31 $ $Author: areeves $
Version number: $Revision: 1.15 $
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
    Buttons,
    ExtCtrls
  ;


  type TNextForm = ( NFNone, NFBack, NFNext );


  type TFormWizardBase = class( TForm )
      pnlWizardButtons: TPanel;
      btnCancel: TBitBtn;
      btnBack: TBitBtn;
      btnNext: TBitBtn;
      btnFinish: TBitBtn;

			procedure FormCreate( Sender: TObject );
      procedure wizardButtonClick( sender: TObject );
      procedure FormClose( Sender: TObject; var Action: TCloseAction );
      
    protected
      // NextForm is used by Used by TFormMain.showParamForm to determine whether the
      // next or previous form in paramFormList should be shown.  NextForm is set
      // by wizardButtonClick.
      _nextForm: TNextForm; // Has value of NFNone, NFBack, or NFNext.
      
      _okToClose: boolean;

      procedure translateUI();

      // These three functions should almost always be overridden in derived classes.
      procedure initializeFromParams(); virtual;
      procedure updateParams(); virtual;
      function dataIsValid(): boolean; virtual;

    public
      constructor create( AOwner: TComponent ); override;
      destructor destroy(); override;

      procedure setParams(); virtual; abstract;

      // This function may or may not to be overridden in derived classes.
      function showModal( direction: TNextForm ): integer; reintroduce; virtual;

      property nextForm: TNextForm read _nextForm;
    end
  ;


implementation

{$R *.dfm}

  uses
    MyDialogs,
    MyStrUtils,
    I88n
  ;

  constructor TFormWizardBase.create( AOwner: TComponent );
    begin
      dbcout( 'TFormWizardBase.create' );
      inherited create( AOwner );
      
      // Deal with form scaling
      //-----------------------
      Assert(not Scaled, 'You should set Scaled property of Form to False!');
      // Set this value to the PPI AT WHICH THE FORM WAS ORIGINALLY DESIGNED!!
      self.PixelsPerInch := 96;
      // FormCreate() will handle the rest.  
      
      
      self.Visible := false;
      self.Enabled := true;
      _okToClose := false;
    end
  ;


  procedure TFormPanelWizardBase.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 15:29:38 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/general_purpose_gui/FormPanelWizardBase.dfm
      // File date: Tue Oct 10 08:22:48 2006

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'FormWizardBase' );
          btnCancel.Caption := tr( '&Cancel' );
          btnCancel.Hint := tr( 'Abort current operations, not saving information on the screen and return to the main menu' );
          btnBack.Caption := tr( '< &Back' );
          btnBack.Hint := tr( 'Return to the previous screen, saving this information' );
          btnNext.Caption := tr( '&Next >' );
          btnNext.Hint := tr( 'Proceed to the next screen, saving this information' );
          btnFinish.Caption := tr( '&Finish' );
          btnFinish.Hint := tr( 'Complete working on the screen by saving the information' );
        end
      ;

    end
  ;


	procedure TFormWizardBase.FormCreate(Sender: TObject);
		begin
      if Screen.PixelsPerInch <> PixelsPerInch then
        ScaleBy( Screen.PixelsPerInch, PixelsPerInch )
      ;
		end
	;
	

  destructor TFormWizardBase.destroy();
    begin
      inherited destroy();
    end
  ;

  
  // Direction may be used in derived classes.
  // It is not needed here.
	function TFormWizardBase.showModal( direction: TNextForm ): integer;
    var
      frm: TForm;
  	begin
      dbcout( 'TFormWizardBase.showModal' );
      initializeFromParams();
      frm := self as TForm;

      frm.Enabled := true;

    result := frm.showModal();
    end
  ;


  function TFormWizardBase.dataIsValid(): boolean;
    begin
      // Most derived classes will do something more sophisticated in here.
      result := true;
    end
  ;


  procedure TFormWizardBase.initializeFromParams();
    begin
      // Most derived classes will do something in here.
    end
  ;


  procedure TFormWizardBase.updateParams();
    begin
      // Most derived classes will do something in here.
    end
  ;


  procedure TFormWizardBase.wizardButtonClick( sender: TObject );
    var
      clickedButton: TBitBtn;
    begin
      screen.Cursor := crHourGlass;

      clickedButton := TBitBtn( sender );

      _okToClose := true;

      if( btnCancel = clickedButton ) then
      	begin
        	_nextForm := NFNone;
          close();
          exit;
        end
      ;

      if( dataIsValid() ) then
        begin
          updateParams();

          if( btnBack = clickedButton ) then
            _nextForm := NFBack
          else if( btnNext = clickedButton ) then
            _nextForm := NFNext
          else if( btnFinish = clickedButton ) then
            _nextForm := NFNone
          else
            begin
              raise exception.create( self.name + ': illegal button' );
              _nextForm := NFNone;
            end
          ;
          close();
        end
      ;

      screen.Cursor := crDefault;
    end
  ;


  procedure TFormWizardBase.FormClose( Sender: TObject; var Action: TCloseAction );
    begin
      if( not( _okToClose ) ) then
        begin
          msgOK( tr( 'Please make a selection from the buttons below.' ), '', IMGWarning, self );
          action := caNone;
        end
      ;
    end
  ;


initialization
	RegisterClass( TFormWizardBase );

end.
