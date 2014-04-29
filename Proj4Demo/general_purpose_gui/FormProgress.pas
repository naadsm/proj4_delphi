{*
FormProgress.pas/dfm
---------------------
Begin: 2005/07/14
Last revision: $Date: 2010-11-01 20:40:51 $ $Author: rhupalo $
Version number: $Revision: 1.21 $
Project: APHI General Purpose Delphi Libary
Website: http://www.naadsm.org/opensource/delphi/
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2005 - 2010 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
----------------------------------------------------
Can be used to display a counter or show the progress of a primary and secondary process, and display a status message.
Used by many of the forms where a XML or CVS file import is occurring.

}

(*
  Documentation generation tags begin with {* or ///
  Replacing these with (* or // foils the documentation generator
*)

unit FormProgress;

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
    ComCtrls
  ;
    
      
  type
  /// Enumerations for specifying the type (feature set) of progress form
  TProgressFormType = (
    PRSingleBar,          /// single progress bar
    PRDoubleBar,          /// primary and secondary progress bars
    PRCounter             /// counter (two flavors) without progress bars
  );


  type
  /// Progress form
  TFormProgress = class( TForm )
      pnlBase: TPanel;
      pnlCtrlButtons: TPanel;
      btnCancel: TButton;
      pnlMessage: TPanel;
      lblMessage: TLabel;
      pnlDoubleBar: TPanel;
			lblPrimary: TLabel;
      pbrSecondary: TProgressBar;
			lblSecondary: TLabel;
      pbrPrimary: TProgressBar;
      pnlCounter: TPanel;
      lblCounter: TLabel;

			procedure FormCreate(Sender: TObject);

      procedure FormClose(Sender: TObject; var Action: TCloseAction);
      procedure btnCancelClick(Sender: TObject);

    protected
      _autoClose: boolean;           /// indicates whether the form should close on completion
      _readyToClose: boolean;        /// indicates whether progress is 100% or the user clicked Cancel
      _formType: TProgressFormType;  /// enumerated value to indicate which feature set to show

      _primaryCounter: integer; /// indicates the current value of the primary progress bar

      _myForm: TForm;           /// owner of self if component is a form else nil

      procedure translateUI();

      procedure handleClose();

      procedure stepUp( pbr: TProgressBar; const percent: integer );

    public
      constructor create(
        AOwner: TComponent;
        formType: TProgressFormType;
        autoClose: boolean;
        cpn: string = ''
      ); reintroduce;

      function setPrimary( percent: integer ): boolean;
      function setSecondary( percent: integer ): boolean;
      procedure setMessage( msg: string );
      function setSecondaryAndMessage( percent: integer; msg: string = '' ): boolean;

      function setCounter1( val: integer ): boolean;
      function setCounter2( val, total: integer ): boolean;
    end
  ;

  const
    DBFORMPROGRESS: boolean = false; /// Set to true to enable debugging messages for this unit


implementation

  {$R *.dfm}

  uses
    Math,
    
    ControlUtils,
    MyStrUtils,
    DebugWindow,
    I88n
  ;

// ----------------------------------------------------------------------------
// Creation/initialization/destruction
// ----------------------------------------------------------------------------

  {*
    Creates a progress form of a specifed type.
    @param AOwner owner of the progress form or nil
    formType enumeration for the progress form type (feature set)
    autoClose true if the form should automatically close on completion, else false
    cpn descriptive caption for the form
    @comment The progress form will always be on top when showing.
  }
  constructor TFormProgress.create(
        AOwner: TComponent;
        formType: TProgressFormType;
        autoClose: boolean;
        cpn: string = ''
      );
    begin
      inherited create( AOwner );
      translateUI();

      if( AOwner is TForm ) then
        _myForm := (AOwner as TForm)
      else
        _myForm := nil
      ;

      // Make sure that this form is always on top when its shown
      //---------------------------------------------------------
      SetWindowPos(
        Self.Handle, // handle to window
        HWND_TOPMOST, // placement-order handle {*}
        Self.Left,  // horizontal position
        Self.Top,   // vertical position
        Self.Width,
        Self.Height,
        // window-positioning options
        SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE
      );

      // Deal with form scaling
      //-----------------------
      Assert(not Scaled, 'You should set Scaled property of Form to False!');

      // Set this value to the PPI AT WHICH THE FORM WAS ORIGINALLY DESIGNED!!
      self.PixelsPerInch := 96;

      // FormCreate() will handle the rest.


      _formType := formType;

      lblMessage.left := pbrPrimary.left;

      case _formType of
        PRSingleBar:
          begin
            pnlDoubleBar.Align := alClient;
            pnlDoubleBar.Width := self.ClientWidth;
            pnlDoubleBar.Visible := true;

            pbrPrimary.Position := 0;
            pbrPrimary.Max := 100;
            lblPrimary.Caption := tr( 'Progress:' );
            lblPrimary.Top := lblPrimary.Top + 20;
            pbrPrimary.Top := pbrPrimary.Top + 20;

            _primaryCounter := -1;

            pbrSecondary.Position := 0;
            pbrSecondary.Max := 0;
            pbrSecondary.Visible := false;

            lblPrimary.Left :=  pbrPrimary.Left;
            lblSecondary.Visible := false;
          end
        ;
        PRDoubleBar:
          begin
            pnlDoubleBar.Align := alClient;
            pnlDoubleBar.Width := self.ClientWidth;
            pnlDoubleBar.Visible := true;

            pbrPrimary.Position := 0;
            pbrPrimary.Max := 100;
            _primaryCounter := -1;

            pbrSecondary.Position := 0;
            pbrSecondary.Max := 100;

            lblPrimary.Left :=  pbrPrimary.Left;
            lblSecondary.Left := pbrPrimary.Left;
          end
        ;                   
        PRCounter:
          begin
            pnlCounter.Align := alClient;
            pnlCounter.Width := self.ClientWidth;
            pnlCounter.Visible := true;
            lblCounter.Visible := false;
          end
        ;
      end;

      centerChildren( self, true );

      if( '' = cpn ) then
        caption := tr( 'Please wait...' )
      else
        caption := cpn
      ;

      _autoClose := autoClose;
      _readyToClose := false;
    end
  ;
 

 /// Specifies the captions, hints, and other component text phrases for translation
  procedure TFormProgress.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 15:29:38 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/general_purpose_gui/FormProgress.dfm
      // File date: Thu Oct 12 16:20:40 2006

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Form1' );
          btnCancel.Caption := tr( 'Cancel' );
          lblMessage.Caption := tr( 'lblMessage' );
          lblPrimary.Caption := tr( 'Stage progress:' );
          lblSecondary.Caption := tr( 'Overall progress:' );
          lblCounter.Caption := tr( 'lblCounter' );
        end
      ;

    end
  ;
 

  /// Initializes the form when first created. Applications do not call FormCreate
	procedure TFormProgress.FormCreate(Sender: TObject);
		begin
      if Screen.PixelsPerInch <> 96 then
        ScaleBy( Screen.PixelsPerInch, 96 )
      ;
		end
	;
// ----------------------------------------------------------------------------



// ----------------------------------------------------------------------------
//
// ----------------------------------------------------------------------------

  {*
     Sets the counter value seen by the user when the form is of type PRCounter
     @param val count to display
     @return the current value of _readyToClose
  }
  function TFormProgress.setCounter1( val: integer ): boolean;
    begin
      lblCounter.Visible := true;
      lblCounter.Caption := intToStr( val );
      horizCenterInside( pnlCounter, lblCounter );
      Application.ProcessMessages();
      result := _readyToClose;
    end
  ;


  {*
     Sets the counter values seen by the user when the form is of type PRCounter
     @param val current count to display
     @param total total count to display
     @return the current value of _readyToClose
     @comment The counter message displayed is val of total, e.g. "53 of 2000"
  }
  function TFormProgress.setCounter2( val, total: integer ): boolean;
    begin
      lblCounter.Visible := true;
      lblCounter.Caption := intToStr( val ) + ' ' + tr( 'of' ) + ' ' + intToStr( total );
      horizCenterInside( pnlCounter, lblCounter );
      Application.ProcessMessages();
      result := _readyToClose;
    end
  ;


  {*
    Helper method to update one of the progress bars to position percent in increments of 10
    @param pbr progress bar object to update, could be either the primary or secondary one
    @param percent final percentage to display in this step
  }
  procedure TFormProgress.stepUp( pbr: TProgressBar; const percent: integer );
    var
      startDelay: comp; // Funky type: see Delphi help for an explanation.
      n: integer;
    begin
      while( percent > pbr.Position ) do
        begin
          n := pbr.Position + 10;
          if( n > percent ) then
            pbr.Position := percent
          else
            pbr.Position := n
          ;
          repaint();

          // Delay for a bit for the progress bar to be drawn
          startDelay := timeStampToMSecs( dateTimeToTimeStamp( time() ) );
          while( timeStampToMSecs( dateTimeToTimeStamp( time() ) ) < startDelay + 15 ) do
            Application.ProcessMessages()
          ;
        end
      ;
    end
  ;


  {*
    Increments the primary progress bar to position percentage
    @param percent final percentage to display in this step
    @return the current value of _readyToClose
  }
  function TFormProgress.setPrimary( percent: integer ): boolean;
    begin
      percent := min( percent, 100 );

      //dbcout( 'Primary percent is ' + intToStr( percent ), DBFORMPROGRESS );
      if( 10 < percent - pbrPrimary.Position ) then
        stepUp( pbrPrimary, percent )
      else
        pbrPrimary.Position := percent
      ;
      handleClose();
      Application.ProcessMessages();
      result := _readyToClose;
    end
  ;


  {*
    Increments the secondary progress bar to position percentage
    @param percent final percentage to display in this step
    @return the current value of _readyToClose
  }
  function TFormProgress.setSecondary( percent: integer ): boolean;
    begin
      percent := min( percent, 100 );
      
      //dbcout( 'Scondary percent is ' + intToStr( percent ), DBFORMPROGRESS );
      if( 10 < percent - pbrSecondary.Position ) then
        stepUp( pbrSecondary, percent )
      else
        pbrSecondary.Position := percent
      ;
      handleClose();
      Application.ProcessMessages();
      result := _readyToClose;
    end
  ;


  {*
    Increments the secondary progress bar to position percentage and displays msg
    @param percent final percentage to display in this step
    @param progress status message, optional
    @return the current value of _readyToClose
  }
  function TFormProgress.setSecondaryAndMessage( percent: integer; msg: string = '' ): boolean;
    begin
      if( '' <> msg ) then
        begin
          inc( _primaryCounter );
          pbrPrimary.Position := _primaryCounter;
          setMessage( msg );
        end
      ;
      if( 10 > percent - pbrSecondary.Position ) then
        stepUp( pbrSecondary, percent )
      else
        pbrSecondary.Position := percent
      ;

      result := _readyToClose;
      
      Application.ProcessMessages();
    end
  ;


  {*
    Displays msg at the top of the progress form
    @param msg text to display
  }
  procedure TFormProgress.setMessage( msg: string );
    begin
      lblMessage.Caption := prettyPrint( msg, 40 );
      centerChildren( pnlMessage, false );
      Application.ProcessMessages();
      repaint();
    end
  ;
// ----------------------------------------------------------------------------


{*
  Helper method to manage form behavior when processing progress is completed.
}
procedure TFormProgress.handleClose();
  var
    startDelay: comp; // Funky type: see Delphi help for an explanation.
  begin

    case _formType of
      PRSingleBar:
        begin
          if( pbrPrimary.Max = pbrPrimary.Position ) then
            begin
              if( _autoClose ) then
                begin
                  repaint();

                  // Delay long enough for the completed progress bar to show up
                  startDelay := timeStampToMSecs( dateTimeToTimeStamp( time() ) );
                  while( timeStampToMSecs( dateTimeToTimeStamp( time() ) ) < startDelay + 750 ) do
                    Application.ProcessMessages()
                  ;
                  close();
                end
              else
                begin
                  btnCancel.Caption := tr( 'Close' );
                  btnCancel.Default := true;
                  btnCancel.Enabled := true;
                end
              ;
            end
          ;
        end
      ;
      PRDoubleBar:
        begin
          if
            ( pbrPrimary.Max = pbrPrimary.Position )
          and
            ( pbrSecondary.Max = pbrSecondary.Position )
          then
            begin
              if( _autoClose ) then
                begin
                  repaint();

                  // Delay long enough for the completed progress bar to show up
                  startDelay := timeStampToMSecs( dateTimeToTimeStamp( time() ) );
                  while( timeStampToMSecs( dateTimeToTimeStamp( time() ) ) < startDelay + 750 ) do
                    Application.ProcessMessages()
                  ;
                  close();
                end
              else
                begin
                  btnCancel.Caption := tr( 'Close' );
                  btnCancel.Default := true;
                  btnCancel.Enabled := true;
                end
              ;
            end
          ;
        end
      ;
      PRCounter:
        begin
          if( _autoClose ) then
            begin
              repaint();

              // Delay long enough for the last label to show up
              startDelay := timeStampToMSecs( dateTimeToTimeStamp( time() ) );
              while( timeStampToMSecs( dateTimeToTimeStamp( time() ) ) < startDelay + 250 ) do
                Application.ProcessMessages()
              ;
              close();
            end
          else
            begin
              btnCancel.Caption := tr( 'Close' );
              btnCancel.Default := true;
              btnCancel.Enabled := true;
            end
          ;
        end
      ;
    end;

  end
;

/// Does nothing, upto to creator to call the form's release() method to free resources
procedure TFormProgress.FormClose( Sender: TObject; var Action: TCloseAction );
  begin
    //action := caNone;
  end
;


{*
  Closes the form if progress is completed or sets _readyToClose true if not.
  @comment Processing is never actually canceled. However the return value of subsquent set calls will be false.
  This could be used by the caller to indicate to rollback whatever process is being conducted.
}
procedure TFormProgress.btnCancelClick(Sender: TObject);
  begin
    if
      ( pbrPrimary.Max = pbrPrimary.Position )
    and
      ( pbrSecondary.Max = pbrSecondary.Position )
    then
      self.Close()
    else
      _readyToClose := true
    ;
  end
;


end.
