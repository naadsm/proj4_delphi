{*
CustomSaveDialog.pas
--------------------
Begin: 2006/04/01
Last revision: $Date: 2011-10-25 05:05:07 $ $Author: areeves $
Version number: $Revision: 1.7 $
Project: APHI General Purpose Delphi Libary
Website: http://www.naadsm.org/opensource/delphi/
Author: Aaron Reeves <aaron@aaronreeves.com>
----------------------------------------------------
Copyright (C) 2010 Aaron Reeves

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
}

(*
  Documentation generation tags begin with {* or ///
  Replacing these with (* or // foils the documentation generator
*)

unit CustomSaveDialog;

interface

uses
  Dialogs,
  ExtDlgs,
  ExtCtrls
;

type
/// Inherits from the VCL TSaveDialog
TCustomSaveDialog = class( TSaveDialog(*TSavePictureDialog*) )
  public
    procedure setImage( img: TImage );
  end
;


implementation

  {*
    Currently this method implements nothing.
    @param img used to display a graphical image on a form, the original file
    can be of many formats -  a bitmap, icon, metafile, or JPG
  }
  procedure TCustomSaveDialog.setImage( img: TImage );
    begin
      //self.ImageCtrl.Picture.Bitmap := img.Picture.Bitmap;
    end
  ;

end.
