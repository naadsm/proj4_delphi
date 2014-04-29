unit FormMain;

(*
FormMain.pas/dfm
----------------
Begin: 2009/08/12
Last revision: $Date: 2009-08-28 17:29:03 $ $Author: areeves $
Version: $Revision: 1.2 $
Project: Demo application for the Delphi wrapper for the Proj.4 Cartographic Projections library
Website: http://www.naadsm.org/opensource
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2009 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

interface

{$INCLUDE Defs.inc}

  uses
    // Standard Delphi units
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
    Menus,

    // This application
    FramePlayMode,
    FrameWorkMode
  ;


  type TFormMain = class( TForm )
      pnlSpacer: TPanel;
      pgcBody: TPageControl;
      tabDemo: TTabSheet;
      tabWork: TTabSheet;
      fraPlayMode: TFramePlayMode;
      fraWorkMode: TFrameWorkMode;
      btnQuit: TButton;

      procedure pgcBodyChange(Sender: TObject);
      procedure btnQuitClick(Sender: TObject);

    public
      constructor create( AOwner: TComponent ); override;

    end
  ;

  var
    frmMain: TFormMain;


implementation

{$R *.dfm}

  uses
    // APHI General Purpose Library
    // See http://www.naadsm.org/opensource/generalpurpose
    DebugWindow
  ;


  constructor TFormMain.create( AOwner: TComponent );
    begin
      inherited create( AOwner );

      pgcBody.ActivePageIndex := 0;
    end
  ;

  
  procedure TFormMain.pgcBodyChange(Sender: TObject);
    begin
      if( 1 = pgcBody.ActivePageIndex ) then
        self.Menu := fraWorkMode.MainMenu1
      else
        self.Menu := nil
      ;
    end
  ;


  procedure TFormMain.btnQuitClick(Sender: TObject);
    begin
      self.Close();
    end
  ;

end.
