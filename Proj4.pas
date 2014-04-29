unit Proj4;

(*
Proj4.pas
---------
Begin: 2009/08/12
Last revision: $Date: 2009-08-18 20:48:23 $ $Author: areeves $
Version: $Revision: 1.6 $
Project: Delphi wrapper for the Proj.4 Cartographic Projections library
Website: http://www.naadsm.org/opensource
Author: Aaron Reeves <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2009 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

interface

  uses
    // APHI General Purpose Delphi Library
    // See http://www.naadsm.org/opensource/generalpurpose
    Points,
    CStringList
  ;


  {*
    This class is a Delphi a wrapper for cartographic projection capabilities in the
    Proj.4 Cartographic Projections library.  This Delphi unit was written for version
    4.6.1 of Proj.4.  This version of the Proj.4 library for Windows (proj.dll: and its
    source code are included with the Delphi wrapper.

    Proj.4 was developed by Gerald Evenden and Frank Warmerdam, and is released under the
    following terms:

      Copyright (c) 2000 Frank Warmerdam

      Permission is hereby granted, free of charge, to any person obtaining a
      copy of this software and associated documentation files (the "Software"),
      to deal in the Software without restriction, including without limitation
      the rights to use, copy, modify, merge, publish, distribute, sublicense,
      and/or sell copies of the Software, and to permit persons to whom the
      Software is furnished to do so, subject to the following conditions:

      The above copyright notice and this permission notice shall be included
      in all copies or substantial portions of the Software.

      THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
      OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
      FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
      THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
      LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
      FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
      DEALINGS IN THE SOFTWARE.

    See the Proj.4 website at http://trac.osgeo.org/proj/ for more information about
    Proj.4.
  }
  type TProj4 = class
    protected
      _pj: pointer;
      _isValid: boolean;
      _params: TCStringList;

      // Initialization
      //---------------
      { Used during construction }
      procedure initializeC( const raiseExceptionOnError: boolean );

      // Properties
      //-----------
      { Returns the Proj.4 parameter string used by the object. }
      function getParamString(): string;

    public
      // Construction/initialization/destruction
      //----------------------------------------
      constructor create( const projParams: string; const raiseExceptionOnError: boolean = false ); overload;
      constructor create( src: TProj4 ); overload;
      destructor destroy(); override;

      // Forward projections
      //--------------------
      { Projects a lat/lon point.  Note that parameters are given in x/y order! }
      function pjFwd( const lon, lat: double; const raiseExceptionOnError: boolean = false ): RPoint;

      { Projects a lat/lon record }
      function pjFwdR( const latLon: RLatLon; const raiseExceptionOnError: boolean = false ): RPoint;

      { Projects a lat/lon type and fills the specified instance of an x/y point type }
      procedure pjFwdT( const latLon: TLatLonPoint; projectedPoint: T2DPoint; const raiseExceptionOnError: boolean = false );

      { Projects a lat/lon type and creates a new instance of an x/y point type }
      function createPjFwdT( const latLon: TLatLonPoint; const raiseExceptionOnError: boolean = false ): T2DPoint;


      // Inverse projections
      //--------------------
      function pjInv( const x, y: double; const raiseExceptionOnError: boolean = false ): RLatLon;

      { Projects an x/y record }
      function pjInvR( const pp: RPoint; const raiseExceptionOnError: boolean = false ): RLatLon;

      { Projects an x/y type and fills the specified instance of a lat/lon type }
      procedure pjInvT( const pp: T2DPoint; latLon: TLatLonPoint; const raiseExceptionOnError: boolean = false );

      { Projects an x/y type and creates a new instance of a lat/lon type }
      function createPjInvT( const pp: T2DPoint; const raiseExceptionOnError: boolean = false ): TLatLonPoint;


      // Error checking
      //---------------
      { Returns true if an error occurred }
      function error(): boolean;

      { Returns 0 on if the last operation was successful, something else if it failed }
      function errorCode(): integer;

      { Returns the message associated with the last error }
      function errorString(): string;

      { Writes the contents of the object to a debugging console }
      procedure debug();
      
      // Properties
      //-----------
      property isValid: boolean read _isValid;
      property paramString: string read getParamString;
    end
  ;


  // A type used by the Proj4 library
  //---------------------------------
  type RProjPoint = record
      u: double;
      v: double;
    end
  ;


  // Global helper functions: is the DLL loaded?
  //--------------------------------------------
  function projLibLoaded(): boolean;
  function projLibLoadErrors(): string;

implementation

  uses
    // Standard Delphi units
    Windows, // Defines THandle
    SysUtils,

    // APHI General Purpose Delphi Library
    // See http://www.naadsm.org/opensource/generalpurpose
    DebugWindow,
    MyStrUtils
  ;

  const
    DBSHOWMSG: boolean = false; // Set to true to enable debugging messages in this unit
    DEG_TO_RAD = 0.0174532925;
  var
    pj_init: function( nParams: integer; params: TCStringArray ): pointer; cdecl;
    pj_free: procedure( pj: pointer ); cdecl;
    pj_fwd: function( p: RProjPoint; pj: pointer ): RProjPoint; cdecl;
    pj_inv: function( p: RProjPoint; pj: pointer ): RProjPoint; cdecl;
    pj_get_errno_ref: function(): pinteger; cdecl;
    pj_strerrno: function( val: integer ): PAnsiChar; cdecl;

    _dllLoadErrors: string;
    _projLibLoaded: boolean;


//-----------------------------------------------------------------------------
// Is the DLL loaded?
//-----------------------------------------------------------------------------
  function projLibLoadErrors(): string;
    begin
      result := _dllLoadErrors;
    end
  ;


  function projLibLoaded(): boolean;
    begin
      result := _projLibLoaded;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TProj4: Construction/initialization/destruction
//-----------------------------------------------------------------------------
  constructor TProj4.create( const projParams: string; const raiseExceptionOnError: boolean = false );
    begin
      inherited create();

      // Parse the project system parameters
      //------------------------------------
      // FIX ME: This process could be a bit more robust.
      _params := TCStringList.create( projParams, '+' );

      // Initialize pj in C
      //-------------------
      initializeC( raiseExceptionOnError );
    end
  ;


  constructor TProj4.create( src: TProj4 );
    begin
      inherited create();
      _params := TCStringList.create( src._params );
      initializeC( false );
    end
  ;


  procedure TProj4.initializeC( const raiseExceptionOnError: boolean );
    var
      paramsArray: TCStringArray;
      i: integer;
    begin
      if( projLibLoaded() ) then
        begin
          setLength( paramsArray, _params.Count );

          for i := 0 to _params.Count - 1 do
            paramsArray[i] := _params.at(i)
          ;

          _pj := pj_init( length( paramsArray ), paramsArray );

          setLength( paramsArray, 0 );

          _isValid := ( nil <> _pj );
        end
      else
        _isValid := false
      ;

      if( raiseExceptionOnError and not( isValid ) ) then
        raise exception.Create( 'Proj4: ' + errorString() )
      ;
    end
  ;


  destructor TProj4.destroy();
    begin
      _params.Free();

      if( nil <> _pj ) then
        pj_free( _pj )
      ;
      
      inherited destroy();
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// TProj4: Forward projections
//-----------------------------------------------------------------------------
  function TProj4.pjFwd( const lon, lat: double; const raiseExceptionOnError: boolean = false ): RPoint;
    var
      ll: RLatLon;
    begin
      if( isValid ) then
        begin
          ll.lat := lat;
          ll.lon := lon;

          result := pjFwdR( ll );

          if( raiseExceptionOnError and error() ) then
            raise exception.Create( 'Proj4: ' + errorString() )
          ;
        end
      else
        raise exception.Create( 'Proj4: Projection system is not valid.' )
      ;
    end
  ;

  
  { Projects a lat/lon record }
  function TProj4.pjFwdR( const latLon: RLatLon; const raiseExceptionOnError: boolean = false ): RPoint;
    var
      p: RProjPoint;
      res: RPoint;
    begin
      if( isValid ) then
        begin
          p.u := latLon.lon * DEG_TO_RAD;
          p.v := latLon.lat * DEG_TO_RAD;

          p := pj_fwd( p, _pj );

          res.x := p.u;
          res.y := p.v;

          result := res;

          if( raiseExceptionOnError and error() ) then
            raise exception.Create( 'Proj4: ' + errorString() )
          ;
        end
      else
        raise exception.Create( 'Proj4: Projection system is not valid.' )
      ;
    end
  ;


  { Projects a lat/lon type and fills the specified instance of an x/y point type }
  procedure TProj4.pjFwdT( const latLon: TLatLonPoint; projectedPoint: T2DPoint; const raiseExceptionOnError: boolean = false );
    var
      p: RProjPoint;
    begin
      if( isValid ) then
        begin
          p.u := latLon.rLon;
          p.v := latLon.rLat;

          p := pj_fwd( p, _pj );

          projectedPoint.x := p.u;
          projectedPoint.y := p.v;

          if( raiseExceptionOnError and error() ) then
            raise exception.Create( 'Proj4: ' + errorString() )
          ;
        end
      else
        raise exception.Create( 'Proj4: Projection system is not valid.' )
      ;
    end
  ;


  { Projects a lat/lon type and creates a new instance of an x/y point type }
  function TProj4.createPjFwdT( const latLon: TLatLonPoint; const raiseExceptionOnError: boolean = false ): T2DPoint;
    var
      p: RProjPoint;
    begin
      if( isValid ) then
        begin
          p.u := latLon.rLon;
          p.v := latLon.rLat;

          p := pj_fwd( p, _pj );

          result := T2DPoint.create( p.u, p.v );

          if( raiseExceptionOnError and error() ) then
            raise exception.Create( 'Proj4: ' + errorString() )
          ;
        end
      else
        raise exception.Create( 'Proj4: Projection system is not valid.' )
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Inverse projections
//-----------------------------------------------------------------------------
  function TProj4.pjInv( const x, y: double; const raiseExceptionOnError: boolean = false ): RLatLon;
    var
      xy: RPoint;
    begin
      if( isValid ) then
        begin
          xy.x := x;
          xy.y := y;

          result := pjInvR( xy );

          if( raiseExceptionOnError and error() ) then
            raise exception.Create( 'Proj4: ' + errorString() )
          ;
        end
      else
        raise exception.Create( 'Proj4: Projection system is not valid.' )
      ;
    end
  ;


  { Projects an x/y record }
  function TProj4.pjInvR( const pp: RPoint; const raiseExceptionOnError: boolean = false ): RLatLon;
    var
      p: RProjPoint;
      ll: RLatLon;
    begin
      if( isValid ) then
        begin
          p.u := pp.x;
          p.v := pp.y;

          p := pj_inv( p, _pj );

          ll.lat := p.v / DEG_TO_RAD;
          ll.lon := p.u / DEG_TO_RAD;

          result := ll;

          if( raiseExceptionOnError and error() ) then
            raise exception.Create( 'Proj4: ' + errorString() )
          ;
        end
      else
        raise exception.Create( 'Proj4: Projection system is not valid.' )
      ;
    end
  ;

  { Projects an x/y type and fills the specified instance of a lat/lon type }
  procedure TProj4.pjInvT( const pp: T2DPoint; latLon: TLatLonPoint; const raiseExceptionOnError: boolean = false );
    var
      p: RProjPoint;
    begin
      if( isValid ) then
        begin
          p.u := pp.x;
          p.v := pp.y;

          p := pj_inv( p, _pj );

          latLon.lat := p.v / DEG_TO_RAD;
          latLon.lon := p.u / DEG_TO_RAD;

          if( raiseExceptionOnError and error() ) then
            raise exception.Create( 'Proj4: ' + errorString() )
          ;
        end
      else
        raise exception.Create( 'Proj4: Projection system is not valid.' )
      ;
    end
  ;



  { Projects an x/y type and creates a new instance of a lat/lon type }
  function TProj4.createPjInvT( const pp: T2DPoint; const raiseExceptionOnError: boolean = false ): TLatLonPoint;
    var
      p: RProjPoint;
      latLon: TLatLonPoint;
    begin
      if( isValid ) then
        begin
          latLon := TLatLonPoint.create();

          p.u := pp.x;
          p.v := pp.y;

          p := pj_inv( p, _pj );

          latLon.lat := p.v / DEG_TO_RAD;
          latLon.lon := p.u / DEG_TO_RAD;

          result := latLon;

          if( raiseExceptionOnError and error() ) then
            raise exception.Create( 'Proj4: ' + errorString() )
          ;
        end
      else
        raise exception.Create( 'Proj4: Projection system is not valid.' )
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Error checking
//-----------------------------------------------------------------------------
  function TProj4.error(): boolean;
    begin
      result := ( 0 <> errorCode() );
    end
  ;


  function TProj4.errorCode(): integer;
    var
      ptr: pinteger;
    begin
      ptr:= pj_get_errno_ref();
      result := ptr^;
    end
  ;


  function TProj4.errorString(): string;
    begin
      if( nil <> @pj_strerrno ) then
        result := pj_strerrno( errorCode() )
      else
        result := 'functions from proj.dll could not be loaded'
      ;
    end
  ;


  procedure TProj4.debug();
    begin
      dbcout( 'TProj4 object:', true );
      dbcout( '  ' + self.paramString, true );
      if( isValid ) then
        dbcout( '(projection is valid)', true )
      else
        dbcout( '(PROJECTION IS NOT VALID)', true )
      ;
      if( projLibLoaded ) then
        dbcout( 'DLL was loaded', true )
      else
        begin
          dbcout( 'DLL DID NOT LOAD.  The following errors occurred:', true );
          dbcout( projLibLoadErrors, true );
        end
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Properties
//-----------------------------------------------------------------------------
  {*
    Returns the Proj.4 parameter string used by the object.  This string is
    recreated from the TCStringList _params.  If the parameter string is not
    valid, then an error message is returned instead.
  }
  function TProj4.getParamString(): string;
    begin
      if( isValid ) then
        begin
          result := _params.delimitedString( ' +' );
          result := trim( result );
        end
      else
        result := '(Invalid parameters)'
      ;
    end
  ;
//-----------------------------------------------------------------------------



//-----------------------------------------------------------------------------
// Loading the DLL
//-----------------------------------------------------------------------------
  procedure setFunctionPointersToNil();
    begin
      pj_init := nil;
      pj_free := nil;
      pj_fwd := nil;
      pj_inv := nil;
      pj_get_errno_ref := nil;
      pj_strerrno := nil;
    end
  ;


  function loadDynamicDll(): boolean;
    var
      dllHandle: THandle; //Handle used to open the DLL.  Defined in unit Windows.
    begin
      _dllLoadErrors := '';
      setFunctionPointersToNil();

      try
        dllHandle := loadLibrary( 'proj.dll' );
        dbcout( 'loadLibrary successful', DBSHOWMSG );
      except
        dbcout( 'loadLibrary failed', DBSHOWMSG );
        result := false;
        exit;
      end;

      result := true;

      _dllLoadErrors := _dllLoadErrors + intToStr( dllHandle );

      if( dllHandle >= 32 ) then // library was successfully loaded.  Assign function pointers now.
        begin
          dbcout( 'Library was successfully loaded', DBSHOWMSG );

          dbcout( 'Attempting to set function pointers', DBSHOWMSG );

          // Load functions
          //---------------
          pj_init := GetProcAddress( dllHandle, 'pj_init' );
          if( nil = @pj_init ) then
            begin
              _dllLoadErrors := _dllLoadErrors + endl + 'MISSING FUNCTION pj_init';
              result := false;
            end
          ;

          pj_free := GetProcAddress( dllHandle, 'pj_free' );
          if( nil = @pj_free ) then
            begin
              _dllLoadErrors := _dllLoadErrors + endl + 'MISSING FUNCTION pj_free';
              result := false;
            end
          ;

          pj_fwd := GetProcAddress( dllHandle, 'pj_fwd' );
          if( nil = @pj_fwd ) then
            begin
              _dllLoadErrors := _dllLoadErrors + endl + 'MISSING FUNCTION pj_fwd';
              result := false;
            end
          ;

          pj_inv := GetProcAddress( dllHandle, 'pj_inv' );
          if( nil = @pj_inv ) then
            begin
              _dllLoadErrors := _dllLoadErrors + endl + 'MISSING FUNCTION pj_inv';
              result := false;
            end
          ;

          pj_get_errno_ref := GetProcAddress( dllHandle, 'pj_get_errno_ref' );
          if( nil = @pj_get_errno_ref ) then
            begin
              _dllLoadErrors := _dllLoadErrors + endl + 'MISSING FUNCTION pj_get_errno_ref';
              result := false;
            end
          ;

          pj_strerrno := GetProcAddress( dllHandle, 'pj_strerrno' );
          if( nil = @pj_strerrno ) then
            begin
              _dllLoadErrors := _dllLoadErrors + endl + 'MISSING FUNCTION pj_strerrno';
              result := false;
            end
          ;

        end
      ;

      // Did everything work?
      //---------------------
      // If not, clear function pointers
      if( false = result ) then
        setFunctionPointersToNil()
      ;
    end
  ;
//-----------------------------------------------------------------------------



initialization

  _projLibLoaded := loadDynamicDll();


end.
