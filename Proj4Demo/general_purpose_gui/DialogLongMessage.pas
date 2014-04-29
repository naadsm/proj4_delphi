{*
DialogLongMessage.pas/dfm
-------------------------
Begin: 2005/06/20
Last revision: $Date: 2011-09-30 19:12:57 $ $Author: areeves $
Version number: $Revision: 1.19 $
Project: APHI General Purpose Delphi Libary
Website: http://www.naadsm.org/opensource/delphi/
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2005 - 2011 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
---------------------------------------------------

Used by many of the sm_forms units to display errors or warnings and present information to the user.
Long multi-line messages can be displayed and the message contents can be copied to the clipboard.

}

(*
  Documentation generation tags begin with {* or ///
  Replacing these with (* or // foils the documentation generator
*)

unit DialogLongMessage;

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
    ExtCtrls
  ;

  type
  {*
    Unit for displaying messages at runtime, inherits from TForm.
    Long multi-line messages can be displayed with scroll control and the
    message contents can be copied to the clipboard.
  }
  TDialogLongMessage = class( TForm )
      mmoLongMessage: TMemo;
      pnlBase: TPanel;
      pnlButtons: TPanel;
      btnOK: TButton;
      btnCopy: TButton;
      pnlHeader: TPanel;
      pnlLeftMargin: TPanel;

			procedure FormCreate(Sender: TObject);

      { Closes the dialog. }
      procedure btnOKClick(Sender: TObject);

      { Copies contents of the memo control to the clipboard. }
      procedure btnCopyClick(Sender: TObject);

    protected
      procedure initialize();
      procedure translateUI();

    public
      constructor create( AOwner: TComponent ); overload; override;

      { Creates the form with the designated caption, header, and message. }
      constructor create(
        AOwner: TComponent;
        cap: string = '';
        header: string = '';
        msg: string = ''
      ); reintroduce; overload;

      { Sets the text of the long message to display. }
      procedure setMessage( msg: string );
      procedure appendMessage( msg: string );

      { Sets the text of the header. }
      procedure setHeader( header: string );

      { Clears the contents of the form. }
      procedure clear();

      procedure disableFormClose();
      procedure enableFormClose();

      procedure show( const alwaysOnTop: boolean );

      property header: string write setHeader;
      property msg: string write setMessage;
    end
  ;


implementation

{$R *.dfm}

	uses
  	ControlUtils,
    MyStrUtils,
    I88n
  ;

  constructor TDialogLongMessage.create( AOwner: TComponent );
    begin
    	inherited create( AOwner );
      initialize();
    end
  ;

  {*
    Creates the form with the designated form caption, header (in pnlHeader), and message.
    @param AOwner Form that owns the message dialog
    @param cap text to be displayed in the form caption area to identify the subject to the user
    @param header A place to put a longer directive to the user than can be displayed in the caption
    @param msg some multi-line message; perhaps the logged outcome from a process, or the contents of a text file
  }
	constructor TDialogLongMessage.create(
      AOwner: TComponent;
      cap: string = '';
      header: string = '';
      msg: string = ''
    );
  	begin
    	inherited create( AOwner );
      initialize();

      if( 0 <> length( trim( cap ) ) ) then
        self.Caption := cap
      ;

      if( 0 <> length( trim( header ) ) ) then
        setHeader( header )
      ;

      if( 0 <> length( trim( msg ) ) ) then
        setMessage( msg )
      ;
    end
  ;


  procedure TDialogLongMessage.initialize();
    begin
      translateUI();
      horizCenterInside( pnlButtons, pnlBase );
    end
  ;


  /// Specifies the captions, hints, and other component text phrases for translation
  procedure TDialogLongMessage.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 15:29:38 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/general_purpose_gui/DialogLongMessage.dfm
      // File date: Tue Oct 10 08:22:48 2006

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          Caption := tr( 'Form1' );
          btnOK.Caption := tr( 'OK' );
          btnCopy.Caption := tr( 'Copy message' );
        end
      ;

    end
  ;

  /// Initializes the dialog form when first created. Applications do not call FormCreate
	procedure TDialogLongMessage.FormCreate(Sender: TObject);
		begin
      Assert(not Scaled, 'You should set Scaled property of Form to False!');

      if Screen.PixelsPerInch <> 96 then
        begin
          ScaleBy( Screen.PixelsPerInch, 96 );
				  self.width := round( self.width * ( screen.pixelsPerInch / 96 ) );
				  self.height := round( self.height * ( screen.pixelsPerInch / 96 ) );
        end
      ;
		end
	;
	

  /// OnClick event that closes the dialog form.
  procedure TDialogLongMessage.btnOKClick(Sender: TObject);
    begin
      close();
    end
  ;


  /// Prevents the user from closing the form by disabling the OK button and the title bar close button
  procedure TDialogLongMessage.disableFormClose();
    var
      AppSysMenu: THandle;
    begin
      btnOK.Enabled := false;

      // See http://www.greatis.com/delphicb/tips/lib/system-hideclose.html
      AppSysMenu:=GetSystemMenu( Handle, False );
      EnableMenuItem( AppSysMenu, SC_CLOSE, MF_BYCOMMAND or MF_GRAYED );
    end
  ;


  /// Enables closing the form by enabling the OK button and the title bar close button
  procedure TDialogLongMessage.enableFormClose();
    var
      AppSysMenu: THandle;
    begin
      btnOK.Enabled := true;

      // See http://www.greatis.com/delphicb/tips/lib/system-hideclose.html
      AppSysMenu := GetSystemMenu( Handle, False );
      EnableMenuItem( AppSysMenu, SC_CLOSE, MF_BYCOMMAND or MF_ENABLED );
    end
  ;

  {*
    Shows the dialog form in the event it had been hidden.
    @param alwaysOnTop Makes the dialog window stay on top, even when another window has focus
  }
  procedure TDialogLongMessage.show( const alwaysOnTop: boolean );
    begin
      inherited show();

      if( alwaysOnTop ) then
        begin
          //Possible values for placement-order handle:
          //   HWND_BOTTOM: Places the window at the bottom of the Z order.
          //   HWND_NOTOPMOST: Places the window above all non-topmost windows
          //   HWND_TOP: Places the window at the top of the Z order.
          //   HWND_TOPMOST: Places the window above all non-topmost windows.
          //     The window maintains its topmost position even when it is deactivated.
          // See http://www.swissdelphicenter.ch/torry/showcode.php?id=6

          SetWindowPos(
            self.Handle, // handle to window
            HWND_TOPMOST, // placement-order handle
            self.Left,  // horizontal position
            self.Top,   // vertical position
            self.Width,
            self.Height,
            SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE // window-positioning options
          );
        end
      ;
    end
  ;



  /// Copies contents of the memo control to the clipboard.
  procedure TDialogLongMessage.btnCopyClick(Sender: TObject);
    begin
      mmoLongMessage.SelectAll();
      mmoLongMessage.CopyToClipboard();
    end
  ;


  /// Sets the text of the memo control.
  procedure TDialogLongMessage.setMessage( msg: string );
    begin
      mmoLongMessage.Lines.Text := msg;
    end
  ;


  {*
     Appends text to the memo control.
     @param msg text to append to the dialog contents
  }
  procedure TDialogLongMessage.appendMessage( msg: string );
    begin
      mmoLongMessage.Lines.Append( msg );
    end
  ;


  /// Clears the contents of the dialog output text
  procedure TDialogLongMessage.clear();
    begin
      mmoLongMessage.Clear();
    end
  ;



  /// Sets the text of the header ( the panel at the top of the form).
  procedure TDialogLongMessage.setHeader( header: string );
    var
      str: string;
      strList: TStringList;
      lbl: TLabel;
      i: integer;
      vertPos: integer;
    begin
      str := prettyPrint( header, 70 );
      strList := TStringList.Create();
      strList.Text := str;

      vertPos := 10;

      for i := 0 to strList.Count - 1 do
        begin
          lbl := TLabel.Create( pnlHeader );
          lbl.Parent := pnlHeader;
          lbl.Caption := strList.Strings[i];
          lbl.Top := vertPos;
          lbl.Width := self.Canvas.TextWidth( strList.Strings[i] );
          lbl.Height := self.Canvas.TextHeight( 'Ag' );
          lbl.Left := ( pnlHeader.Width - lbl.Width ) div 2;
          lbl.Show();
          
          // lbl will be destroyed with the form

          inc( vertPos, lbl.Height );
        end
      ;

      pnlHeader.Height := ( strList.Count * self.Canvas.TextHeight( 'Ag' ) ) + 20;

      strList.Free();
    end
  ;

end.
