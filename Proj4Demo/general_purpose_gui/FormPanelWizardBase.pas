unit FormPanelWizardBase;

(*
FormPanelWizardBase.pas/dfm
---------------------------
Begin: 2006/07/18
Last revision: $Date: 2008-12-10 21:03:31 $ $Author: areeves $
Version number: $Revision: 1.5 $
Project: APHI General Purpose Delphi Libary
Website: http://www.naadsm.org/opensource/delphi/
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2006 - 2008 Animal Population Health Institute, Colorado State University

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
    ExtCtrls,

    //cDictionaries,
    QStringMaps,
    
    FrameWizardBase
  ;


  type TNextPanel = ( NPNone, NPBack, NPNext );


  type TFormPanelWizardBase = class( TForm )
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
      _nextPanel: TNextPanel; // Has value of NPNone, NPBack, or NPNext.

      _fraMain: TFrame;

      //_frameList: TObjectDictionary;
      _frameList: TQStringObjectMap;

      _visibleFrame: TFrameWizardBase;
      _currentFrameIndex: integer;
      
      _okToClose: boolean;

      procedure translateUI();

      procedure fillFrameList(); virtual; abstract;

      procedure showPanel( panelName: string ); overload;
      procedure showPanel( idx: integer ); overload;

      // Remember to override these.
      function initializeAndShow(): boolean; virtual; abstract;
      procedure updateParams(); virtual; abstract;
      function dataIsValid(): boolean; virtual; abstract;

    public
      constructor create( AOwner: TComponent ); override;
      destructor destroy(); override;

      property nextPanel: TNextPanel read _nextPanel;
    end
  ;


implementation

{$R *.dfm}

  uses
    MyDialogs,
    MyStrUtils,
    ControlUtilities
  ;

  constructor TFormPanelWizardBase.create( AOwner: TComponent );
    begin
      dbcout( 'TFormPanelWizardBase.create' );
      inherited create( AOwner );
      translateUI();
    	
      // Deal with form scaling
      //-----------------------
      Assert(not Scaled, 'You should set Scaled property of Form to False!');

      // Set this value to the PPI AT WHICH THE FORM WAS ORIGINALLY DESIGNED!!
      self.PixelsPerInch := 96;

      // FormCreate() will handle the rest.      
      
      
      self.Visible := false;
      self.Enabled := true;
      _okToClose := false;

      //_frameList := TObjectDictionary.Create();
      _frameList := TQStringObjectMap.create();
    end
  ;


  procedure TFormPanelWizardBase.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 15:48:33 2008
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


	procedure TFormPanelWizardBase.FormCreate(Sender: TObject);
		begin
      if Screen.PixelsPerInch <> PixelsPerInch then
        ScaleBy( Screen.PixelsPerInch, PixelsPerInch )
      ;
		end
	;


  destructor TFormPanelWizardBase.destroy();
    begin
      _frameList.Free();
      
      inherited destroy();
    end
  ;


  procedure TFormPanelWizardBase.wizardButtonClick( sender: TObject );
    var
      clickedButton: TBitBtn;
    begin
      screen.Cursor := crHourGlass;

      clickedButton := TBitBtn( sender );

      dbcout( '*** Button ' + clickedButton.Name + ' clicked' );

      _okToClose := true;

      if( btnCancel = clickedButton ) then
      	begin
        	_nextPanel := NPNone;
          exit;
        end
      ;

      if( dataIsValid() ) then
        begin
          updateParams();

          if( btnBack = clickedButton ) then
            begin
              _nextPanel := NPBack;
              showPanel( _currentFrameIndex - 1 );
            end
          else if( btnNext = clickedButton ) then
            begin
              _nextPanel := NPNext;
              showPanel( _currentFrameIndex + 1 );
            end
          else if( btnFinish = clickedButton ) then
            begin
              _nextPanel := NPNone;
            end
          else
            begin
              raise exception.create( self.name + ': illegal button' );
              _nextPanel := NPNone;
            end
          ;
        end
      ;

      screen.Cursor := crDefault;
    end
  ;


  procedure TFormPanelWizardBase.showPanel( panelName: string );
    var
      i: integer;
    begin
      dbcout( 'Show panel: ' + panelName );


      // Then show the selected panel
      for i := 0 to _frameList.Count - 1 do
        begin
          dbcout( _frameList.GetKeyByIndex(i) );
          if( panelName = _frameList.GetKeyByIndex(i) ) then
            begin
              showPanel( i );
              break;
            end
          ;
        end
      ;
    end
  ;


  procedure TFormPanelWizardBase.showPanel( idx: integer );
    var
      i: integer;
      frame: TFrameWizardBase;
      pnl1: TPanel;
    begin
      dbcout( 'Showing panel ' + intToStr( idx ) );

       // First, hide all panels
      for i := 0 to _frameList.count - 1 do
        begin
          frame := _frameList.GetItemByIndex(i) as TFrameWizardBase;
          frame.visible := false;
          //_frameList[i].Visible := false;
          frame.Align := alNone;
        end
      ;

      // Then show the selected panel
      _currentFrameIndex := idx;
      _visibleFrame := _fraMain.Controls[idx] as TFrameWizardBase;
      _visibleFrame.Width := self.clientWidth;
      _visibleFrame.Height := self.ClientHeight - pnlWizardButtons.Height - 15;
      _visibleFrame.Align := alClient;
      pnl1 := _visibleFrame.Controls[0] as TPanel;
      horizVertCenterInside( pnl1, _visibleFrame );

      pnl1.BevelInner := bvNone;
      pnl1.BevelOuter := bvNone;
      
      // Finally, deal with the buttons
      btnBack.Enabled := not( 0 = idx );
      btnNext.Enabled := not( _fraMain.ControlCount - 1 = idx );

      dbcout( 'Visible fram is nil: ' + boolToText( _visibleFrame = nil ) );

      if( initializeAndShow() ) then
        _visibleFrame.Visible := true
      else
        begin
          if( NPBack = _nextPanel ) then
            showPanel( _currentFrameIndex - 1 )
          else if( NPNext = _nextPanel ) then
            showPanel( _currentFrameIndex + 1 )
          ;
        end
      ;
    end
  ;


  procedure TFormPanelWizardBase.FormClose( Sender: TObject; var Action: TCloseAction );
    begin
      if( NPNone <> _nextPanel ) then
        action := caNone
      ;
    end
  ;


end.
