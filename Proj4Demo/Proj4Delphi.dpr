program Proj4Delphi;

(*
Proj4Delphi.dpr
---------------
Begin: 2009/08/12
Last revision: $Date: 2009-08-31 22:44:42 $ $Author: areeves $
Version: $Revision: 1.1 $
Project: Demo application for the Delphi wrapper for the Proj.4 Cartographic Projections library
Website: http://www.naadsm.org/opensource
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2009 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)


{$INCLUDE Defs.inc}


{*
  This application demonstrates the use of Proj.4 for Delphi, a Delphi wrapper
  for the Proj.4 Cartographic Projections Library.  Proj.4 is written in C,
  and is distributed as a dynamically loaded library (DLL) for Windows.  The Delphi
  class TProj4 makes use of the DLL to provide access to functions to carry out
  cartographic projection.

  Proj.4 was developed by Gerald Evenden and Frank Warmerdam.  It is available
  under the terms of an MIT-style license from http://trac.osgeo.org/proj/.

  The Delphi wrapper for Proj.4 was developed by Aaron Reeves.  It is available
  under the terms of the GPL license from http://www.naadsm.org/opensource/proj4wrapper/.
}


uses
  // Standard Delphi units
  Forms,

  // APHI General-Purpose Library
  // See http://www.naadsm.org/opensource/generalpurpose
  DebugWindow,
  CsvParser,
  Points,
  MyDialogs,
  MyStrUtils,

  // Proj.4 for Delphi
  // See http://www.naadsm.org/opensource/proj4wrapper
  Proj4,

  // Application-specific units
  FormMain in 'FormMain.pas' {FormMain},
  FramePlayMode in 'FramePlayMode.pas' {FramePlayMode: TFrame},
  FrameWorkMode in 'FrameWorkMode.pas' {FrameWorkMode: TFrame}, 
  FormAboutAphiDemo in 'FormAboutAphiDemo.pas' {FormAboutAphiDemo},
  DialogLongMessage in 'general_purpose_gui\DialogLongMessage.pas' {DialogLongMessage}
;

{$R *.res}

begin
  {$IFDEF DEBUG}
    setDebugging( true );
  {$ELSE}
    setDebugging( false );
  {$ENDIF}

  Application.Initialize;
  // The Delphi UI sometimes overwrites the following line.
  // It should look like this:
  //Application.CreateForm( TFormMain, frmMain );
  Application.CreateForm( TFormMain, frmMain );
  Application.Run;
end.
