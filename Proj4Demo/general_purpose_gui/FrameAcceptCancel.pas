{*
FrameAcceptCancel.pas/dfm
-------------------------
Begin: 2005/11/10
Last revision: $Date: 2010-11-02 18:16:23 $ $Author: rhupalo $
Version number: $Revision: 1.8 $
Project: APHI General Purpose Delphi Libary
Website: http://www.naadsm.org/opensource/delphi/
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2005 - 2010 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
-----------------------------------------------------
Click events used from the buttons in this frame can be used on a form to invoke user input validation
rules on accept, or ignore and clear edits on cancel for one or a series of user input controls.

}

(*
  Documentation generation tags begin with {* or ///
  Replacing these with (* or // foils the documentation generator
*)

unit FrameAcceptCancel;

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
    Buttons
  ;

  type
  /// Buttons providing click events to process something on container form
  TFrameAcceptCancel = class( TFrame )
      btnAccept: TBitBtn;  /// provides click event to submit some input entry
      btnCancel: TBitBtn;  /// provides click event to cancel input entry

    protected
      procedure translateUI();

    public
      constructor create( AOwner: TComponent ); override;

    end
  ;

implementation

{$R *.dfm}

  uses
    I88n
  ;

  {*
    Creates an instance of the frame
    @param AOwner the form that owns this instance of the frame
  }
  constructor TFrameAcceptCancel.create( AOwner: TComponent );
    begin
      inherited create( AOwner );
      translateUI();
    end
  ;


  /// Specifies the captions, hints, and other component text phrases for translation
  procedure TFrameAcceptCancel.translateUI();
    begin
      // This function was generated automatically by Caption Collector 0.6.0.
      // Generation date: Mon Feb 25 15:29:38 2008
      // File name: C:/Documents and Settings/apreeves/My Documents/NAADSM/Interface-Fremont/general_purpose_gui/FrameAcceptCancel.dfm
      // File date: Thu Nov 10 10:52:48 2005

      // Set Caption, Hint, Text, and Filter properties
      with self do
        begin
          btnAccept.Hint := tr( 'Accept changes' );
          btnCancel.Hint := tr( 'Cancel changes' );
        end
      ;

    end
  ;

end.
