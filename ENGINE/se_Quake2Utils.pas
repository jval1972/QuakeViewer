//------------------------------------------------------------------------------
//
//  Surfaces Engine (SE) - Gaming engine for Windows based on DirectX & DelphiX
//  Copyright (C) 1999-2004, 2018 by Jim Valavanis
//
//  This program is free software; you can redistribute it and/or
//  modify it under the terms of the GNU General Public License
//  as published by the Free Software Foundation; either version 2
//  of the License, or (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
// DESCRIPTION:
//  id-tech2 & idtec3 map importing utilities
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//------------------------------------------------------------------------------

{$I defs.inc}

unit se_Quake2Utils;

{*
 *      $id:          q2bsp.pas
 *      $description: Quake2 .bsp file fmt
 *      $version:     converted from q2bsp.h
 *
 *}


interface

{$IFNDEF NO_IDSOFTGAMESSUPPORT}

uses
  Classes, SysUtils, Graphics, se_Main, se_DirectX, se_QuakeTypes, se_DXClasses;

{$Z4,A+}

type
  Vector = array[0..2] of single;

function GetQuake2MapData(AScene: TD3DScene;
  MainPak, PPak: TFileName; MapName: string;
  Factor: integer = DEFQUAKEIMPORTFACTOR;
  lFactor: Single = DEFQUAKEIMPORTLFACTOR;
  TessellationLevel: integer = DEFTESSELLATIONLEVEL): integer;

function GetBspVersion(FName: TFilename): integer;

function IsBspVersionSupported(BspVersion: integer): boolean;

function GetQuakeVersion(FName: TFilename): integer; overload;

function GetQuakeVersion(BspVersion: integer): integer; overload;

function GetBspDescription(FName: TFilename): string;

function Quake2ErrorToString(const err: integer): string;

function GetPAKLinkDescription(const PAKFileName: string; Entry: string): string;

function IsPAKLinkInfo(const inf: string): boolean;

function GetPAKLinkInfo(const inf: string; var PAKFileName: string; var Entry: string): boolean;

function IsFakeQuake2TextureName(const s: string): boolean;

{$ENDIF}

implementation

{$IFNDEF NO_IDSOFTGAMESSUPPORT}

uses
  Math, se_DXDUtils, se_D3DUtils, se_MyD3DUtils, se_DXDraws, Windows;

const
  Alpha: Integer = 255; // Alpha Value For Faces' Texture (Default=255)

  Lx = 8; // Internal Lightmap X Size (maximum 16, bigger=slower)
  Ly = 8; // Guess What ?

// Πολ/σμός με αριθμό του Quake3 Vertex
function MulDVertex_v3(const v: DVertex_v3; const x: TD3DValue): DVertex_v3;
begin
  Result.Position[0] := v.Position[0] * x;
  Result.Position[1] := v.Position[1] * x;
  Result.Position[2] := v.Position[2] * x;
  Result.TextureCoord[0] := v.TextureCoord[0] * x;
  Result.TextureCoord[1] := v.TextureCoord[1] * x;
  Result.LightmapCoord[0] := v.LightmapCoord[0] * x;
  Result.LightmapCoord[1] := v.LightmapCoord[1] * x;
  Result.Normal[0] := v.Normal[0] * x;
  Result.Normal[1] := v.Normal[1] * x;
  Result.Normal[2] := v.Normal[2] * x;
  Result.Color[0] := trunc(v.Color[0] * x);
  Result.Color[1] := trunc(v.Color[1] * x);
  Result.Color[2] := trunc(v.Color[2] * x);
  Result.Color[3] := trunc(v.Color[3] * x);
end;

function AddDVertex_v3(const v1, v2: DVertex_v3): DVertex_v3;
begin
  Result.Position[0] := v1.Position[0] + v2.Position[0];
  Result.Position[1] := v1.Position[1] + v2.Position[1];
  Result.Position[2] := v1.Position[2] + v2.Position[2];
  Result.TextureCoord[0] := v1.TextureCoord[0] + v2.TextureCoord[0];
  Result.TextureCoord[1] := v1.TextureCoord[1] + v2.TextureCoord[1];
  Result.LightmapCoord[0] := v1.LightmapCoord[0] + v2.LightmapCoord[0];
  Result.LightmapCoord[1] := v1.LightmapCoord[1] + v2.LightmapCoord[1];
  Result.Normal[0] := v1.Normal[0] + v2.Normal[0];
  Result.Normal[1] := v1.Normal[1] + v2.Normal[1];
  Result.Normal[2] := v1.Normal[2] + v2.Normal[2];
  Result.Color[0] := v1.Color[0] + v2.Color[0];
  Result.Color[1] := v1.Color[1] + v2.Color[1];
  Result.Color[2] := v1.Color[2] + v2.Color[2];
  Result.Color[3] := v1.Color[3] + v2.Color[3];
end;

function Combine3DVertex_v3(const v1, v2, v3: DVertex_v3; const w1, w2, w3: TD3DValue): DVertex_v3;
begin
  Result := AddDVertex_v3(AddDVertex_v3(
              MulDVertex_v3(v1, w1), MulDVertex_v3(v2, w2)), MulDVertex_v3(v3, w3));
end;

function IsFakeQuake2TextureName(const s: string): boolean;
var i: integer;
    tmp: string;
begin
  Result := True;
  if s = '' then Exit;
  tmp := UpperCase(ExtractFileName(AdjustQuake2EntryName(s)));
  // Return False for known fake textures
  if tmp = UpperCase(rsSkyTex) then Exit;
  if tmp = UpperCase(rsHLTrigger1) then Exit; // HalfLife Trigger
  if tmp = UpperCase(rsHLTrigger2) then Exit; // HalfLife Trigger
  if tmp = UpperCase(rsHLHurt1) then Exit;
  if tmp = UpperCase(rsHLHurt2) then Exit;
  if Pos('.', tmp) = 0 then tmp := tmp + UpperCase(rsExtWal);
  if tmp = UpperCase(rsTriggerTex) then Exit; // Quake2 Trigger;
  if tmp = UpperCase(rsrtex412) then Exit; // Hexen2 - Blank
  if tmp = UpperCase(rsrtex114) then Exit; // Hexen2 - Trigger
  for i := 0 to 9 do
  begin
    if tmp = UpperCase(Format(rsFmtSkyTex, [i])) then Exit;
    if tmp = UpperCase(Format(rsFmtSkyTex0, [i])) then Exit;
    if tmp = UpperCase(Format(rsFmtSkyTex00, [i])) then Exit;
  end;
  Result := False;
end;

function IsValidQ3Name(const txname: string): boolean;
var txname2: string;
begin
  txname2 := UpperCase(txname);
  if Pos('TEXTURES\SKIES\', txname2) >  0 then
    Result := False
  else if Pos('TEXTURES\SFX\', txname2) >  0 then
    Result := False
  else
  begin
    txname2 := ExtractFileNameOnly(txname2);
    Result := (txname2 <> 'NOSHADER') and
              (txname2 <> 'NODRAW') and
              (txname2 <> 'NODROP') and
              (txname2 <> 'NULL') and
              (txname2 <> 'CLIP') and
              (txname2 <> 'HINT') and
              (txname2 <> 'FOG_TIMCTF1') and
              (txname2 <> 'FLARESHADER') and
              (txname2 <> 'TRIGGER');
  end;
end;

type
  Wlst=Word;
  TWlst = array[0..0] of Wlst;
  PWlst = ^TWlst;
  TCluster = array[0..0] of Byte;
  PCluster = ^TCluster;
  TClusters = array[0..0] of PCluster;
  PClusters = ^TClusters;
  Tlst = array[0..0] of Integer;
  Plst = ^Tlst;


  TByte = array[0..0]of Byte;
  PByte = ^TByte;

  Poly = record
    Np: Integer;// Number of Vertices
    Tid: Integer; // Texture Id
    Points: Plst; // Verts Indices
    Fid: Integer;// Frame Id
  end;
  Poly_P = ^Poly;

  TPoly = array[0..0] of Poly;
  PPoly = ^TPoly;

  Plane = record
    A, B, C, D: Single;
  end;

  TPlane = array[0..0] of Plane;
  PPlane = ^TPlane;

  Uvrec = record
    U, V: Single;
  end;
  UvrecP = ^Uvrec;

  Tuvrec = array[0..0] of Uvrec;
  Puvrec = ^Tuvrec;

  Uvdata = record
    Np: Integer;
    Size_S, Size_T: Single;
    Min_S, Min_T: Single;
    Uv: Puvrec;
  end;
  UvdataP = ^Uvdata;

  TUvdata = array[0..0] of Uvdata;
  PUvdata = ^TUvdata;

function MakeVector(x, y, z: single): Vector;
begin
  Result[0] := x;
  Result[1] := y;
  Result[2] := z;
end;

{* Calculate Dot Product Of Two Vectors *}
function Fvec_Dotproduct(const V1, V2: Vector): single;
begin
  Result := V1[0] * V2[0] + V1[1] * V2[1] + V1[2] * V2[2];
end;

resourceString
  rsFmtBspDescription = 'BSP Version: %d - Game Version: %d - (%s)';
  rsUnKnownDescription = 'BSP Version: Unsuported - Game Version: Unknown';
  rsUnKnown = 'Unknown';
  rsFmtQuake1 = 'Quake 1';
  rsFmtQuake2 = 'Quake 2';
  rsFmtQuake3 = 'Quake 3';
  rsFmtRTCW = 'RTCW';
  rsFmtHexen2 = 'Hexen 2';
  rsFmtHeretic2 = 'Heretic 2';
  rsFmtDaikatana = 'Daikatana';
  rsFmtHalflife = 'Half-Life';

function GetBspDescription(FName: TFilename): string;
var
  qVer, bVer: integer;
begin
  bVer := GetBspVersion(FName);
  qVer := GetQuakeVersion(bVer);
  if (qVer = 1) then
  begin
    if bVer = BspVersion_29 then
      Result := Format(rsFmtBspDescription, [bVer, qVer, rsFmtQuake1 + ', ' + rsFmtHexen2])
    else if bVer = BspVersion_30 then
      Result := Format(rsFmtBspDescription, [bVer, qVer, rsFmtHalflife])
    else
      Result := Format(rsFmtBspDescription, [bVer, qVer, rsUnknown])
  end
  else if (qVer = 2) then
  begin
    if bVer = BspVersion_38 then
      Result := Format(rsFmtBspDescription, [bVer, qVer, rsFmtQuake2 + ', ' + rsFmtHeretic2])
    else if bVer = BspVersion_41 then
      Result := Format(rsFmtBspDescription, [bVer, qVer, rsFmtDaikatana])
    else
      Result := Format(rsFmtBspDescription, [bVer, qVer, rsUnknown])
  end
  else if (qVer = 3) then
  begin
    if bVer = BspVersion_46 then
      Result := Format(rsFmtBspDescription, [bVer, qVer, rsFmtQuake3])
    else if bVer = BspVersion_47 then
      Result := Format(rsFmtBspDescription, [bVer, qVer, rsFmtRTCW])
    else
      Result := Format(rsFmtBspDescription, [bVer, qVer, rsUnknown])
  end
  else
    Result := rsUnKnownDescription;
end;

function GetQuakeVersion(FName: TFilename): integer;
var
  ret: integer;
begin
  ret := GetBspVersion(FName);
  Result := GetQuakeVersion(ret);
end;

function GetQuakeVersion(BspVersion: integer): integer;
begin
  Result := -1;
  if BspVersion = -1 then
    Exit;
  if BspVersion in [Bspversion_46, Bspversion_47] then
    Result := 3
  else if BspVersion in [Bspversion_38, Bspversion_41] then
    Result := 2
  else if BspVersion in [Bspversion_29, Bspversion_30] then
    Result := 1;
end;

function IsBspVersionSupported(BspVersion: integer): boolean;
begin
  Result := BspVersion in [Bspversion_29, Bspversion_30, Bspversion_38, Bspversion_46, Bspversion_47];
end;

function GetBspVersion(FName: TFilename): integer;
var
  Qdir: TPakDir;
  P: Pakfile;
  Entries: TDXStringList;
  i: integer;

  function GetVer: integer;
  var
    H: Dheader_T;
  begin
    Qdir.Qblockread(P, H, SizeOf(H));
    if (H.Ident <> Idbspheader) and (H.Version_v1 <> Bspversion_29) and (H.Version_v1 <> Bspversion_30) then
    begin
      //Error: Not A Quake1/2 bsp;
      Result := -1;
      Exit;
    end;
    if (H.Version <> Bspversion_38) and (H.Version <> Bspversion_41) and (H.Version <> Bspversion_46) and (H.Version <> Bspversion_47) and
       (H.Version_v1 <> Bspversion_29) and (H.Version_v1 <> Bspversion_30) then
    begin
      //Error: Undetermined Bsp Version;
      Result := -1;
      Exit;
    end;

    if (H.Version = Bspversion_38) or (H.Version = Bspversion_41) or (H.Version = Bspversion_46) or (H.Version = Bspversion_47) then
      Result := H.Version
    else
      Result := H.Version_v1;
  end;

begin
  Result := -1;
  if TrimStr(FName) = EmptyStr then
    Exit;
  Qdir := TPakDir.Create(FName, '', False);
  try
    if UpperCase(ExtractFileExt(FName)) = UpperCase(rsExtPak) then
    begin
      Entries := TDXStringList.Create;
      try
        QDir.GetEntries(Entries);
        for i := 0 to Entries.Count - 1 do
          if UpperCase(ExtractFileExt(Entries.Strings[i])) = UpperCase(rsExtBsp) then
            if QDir.Openfile(P, Entries.Strings[i]) then
            begin
              Result := GetVer;
              QDir.Closefile(P);
              break;
            end;
      finally
        Entries.Free;
      end;
    end

    else if UpperCase(ExtractFileExt(FName)) = UpperCase(rsExtBsp) then
    begin
      if QDir.Openfile(P, FName) then
      begin
        Result := GetVer;
        QDir.Closefile(P);
      end;
    end

    else if UpperCase(ExtractFileExt(FName)) = UpperCase(rsExtPK3) then
    begin
      Entries := TDXStringList.Create;
      try
        QDir.GetEntries(Entries);
        for i := 0 to Entries.Count - 1 do
          if UpperCase(ExtractFileExt(Entries.Strings[i])) = UpperCase(rsExtBsp) then
            if QDir.Openfile(P, Entries.Strings[i]) then
            begin
              Result := GetVer;
              QDir.Closefile(P);
              break;
            end;
      finally
        Entries.Free;
      end;
    end

  finally
    QDir.Free;
  end;
end;

function GetQuake2MapData(AScene: TD3DScene;
  MainPak, PPak: TFileName; MapName: string;
  Factor: integer = DEFQUAKEIMPORTFACTOR; lFactor: Single = DEFQUAKEIMPORTLFACTOR;
  TessellationLevel: integer = DEFTESSELLATIONLEVEL): integer;
var
  QMipTexs_v1: PMiptex_S_v1;
  NMipTexs_v1: integer;
  Qedges: Pdedge_T;
  Nedges: Integer;
  Qledges: Plst;
  Nledges: Integer;
  Qverts: Pdvertex_T;
  Nverts: Integer;
  Qtexinfo: Ptexinfo_T;
  Qtexinfo_v1: Ptexinfo_T_v1;
  Ntexinfo: Integer;
  Qfaces: Pdface_T;
  Nfaces: Integer;
  TextureNames: TDXStringList;

  Qlight: Pbyte;
  Lightsz: Integer;

  Plist: Ppoly;
  Plisti: Poly_P;

  Uvlist: Puvdata;
  Uvi: UvdataP;
  Luvlist: Puvdata;

  Texlist: Pmiptex_S;
  ptex: Miptex_P;

  Qdir: TPakDir;

  inf: TD3DGenericTriangleInfo;

  pv: PD3DLVertex;
  tmpColor: TD3DColor;

  QuakeVersion,
  BspVersion: integer;

// Optimization Variables
  DefSpecular: integer;
  One_sub_lFactor: single;
  lFactor_mul_255: single;

var
  P, TxP: Pakfile;
  H: Dheader_T;

  procedure Loadlump(var H: Dheader_T; Lump: Integer; var Dat: Pointer;
    var Nr: Integer; Sz: Integer);
  var
    Ps:Integer;
  begin
    if QuakeVersion <> 1 then
    begin
      if Frac((H.Lumps[Lump].Filelen) / Sz) <> 0 then
      begin
        //loadlump: funny lump size
        Exit;
      end;
      Nr := H.Lumps[Lump].Filelen div Sz;
      GetMem(Dat, Nr * Sz);
      Ps := Qdir.Qfilepos(P);
      Qdir.Qseek(P, H.Lumps[Lump].Fileofs);
      Qdir.Qblockread(P,Dat^, Nr * Sz);
      Qdir.Qseek(P, Ps);
    end
    else
    begin
      if Frac((H.Lumps_v1[Lump].Filelen) / Sz) <> 0 then
      begin
        //loadlump: funny lump size
        Exit;
      end;
      Nr := H.Lumps_v1[Lump].Filelen div Sz;
      GetMem(Dat, Nr * Sz);
      Ps := Qdir.Qfilepos(P);
      Qdir.Qseek(P, H.Lumps_v1[Lump].Fileofs);
      Qdir.Qblockread(P,Dat^, Nr * Sz);
      Qdir.Qseek(P, Ps);
    end
  end;

  procedure LoadTexture_v1_lump(var H: Dheader_T; Lump: Integer; var Dat: PMiptex_S_v1;
    var Nr: Integer);
  var
    Ps, oldPs:Integer;
    i: integer;
    ofs: integer;
  begin
    Ps := Qdir.Qfilepos(P);
    Qdir.Qseek(P, H.Lumps_v1[Lump].Fileofs);

    Qdir.Qblockread(P, Nr, SizeOf(Nr));
    GetMem(Dat, Nr * SizeOf(Miptex_S_v1));

    for i := 0 to Nr - 1 do
    begin
      Qdir.Qblockread(P, ofs, SizeOf(ofs));
      oldPs := Qdir.Qfilepos(P);

      Qdir.Qseek(P, ofs + H.Lumps_v1[Lump].Fileofs);
      Qdir.Qblockread(P, Dat^[i], SizeOf(Miptex_S_v1));

      Qdir.Qseek(P, oldPs);
    end;
    Qdir.Qseek(P, Ps);
  end;

  // Convert QFaces Into Internal Format
  procedure Convert_F;
  var
    I, J: Integer;
    Pl: array[0..64] of Integer;
    Pl2: array[0..64] of Integer;
    Np2: Integer;
    Np: Integer;

    procedure AddEdge(A, B: Integer);
    begin
      if Np = 0 then
      begin
        Pl[Np]:=A;
        Inc(Np);
        Pl[Np]:=B;
        Inc(Np);
      end
      else
      begin
        if Pl[Np - 1] <> A then
        begin
          Pl[Np] := A;
          Inc(Np);
        end;
        Pl[Np] := B;
        Inc(Np);
      end;
    end;

    // Test If Three Points Are Colinear.
    // (Crappy Method, There Are Other Ways To Do This :)
    function Col(A, B, C: Integer): Boolean;
    var
      V1, V2, V3: Vector;

      // Make A Plane From Three Points
      // 2018 WTF, how many nested functions I put here ?
      procedure Makeplane(var Plan: Plane; C3, C2, C1: Vector);
      var
        Rx1, Ry1, Rz1, Rx2, Ry2, Rz2: Single;
        Len: Single;
      begin
        Rx1 := C2[0] - C1[0];
        Ry1 := C2[1] - C1[1];
        Rz1 := C2[2] - C1[2];
        Rx2 := C3[0] - C1[0];
        Ry2 := C3[1] - C1[1];
        Rz2 := C3[2] - C1[2];
        with Plan do
        begin
          A := Ry1 * Rz2 - Ry2 * Rz1;
          B := Rz1 * Rx2 - Rz2 * Rx1;
          C := Rx1 * Ry2 - Rx2 * Ry1;
          Len := A * A + B * B + C * C;
          if Len = 0 then
          begin
            A := 0;
            B := 0;
            C := 0;
            D := 0;
            Exit;
          end; // Colinear Points
          Len := Sqrt(Len);
          A := A / Len;
          B := B / Len;
          C := C / Len;
          D := A * C2[0] + B * C2[1] + C * C2[2];
        end;
      end;

    var
      Pp:Plane;
      ppoints: SingleArray_3_P;
    begin
      ppoints := @Qverts[A].Point;
      V1[0] := ppoints[0];
      V1[1] :=-ppoints[2];
      V1[2] := ppoints[1];
      ppoints := @Qverts[B].Point;
      V2[0] := ppoints[0];
      V2[1] :=-ppoints[2];
      V2[2] := ppoints[1];
      ppoints := @Qverts[C].Point;
      V3[0] := ppoints[0];
      V3[1] :=-ppoints[2];
      V3[2] := ppoints[1];
      Makeplane(Pp, V3, V2, V1);
      Col := (Pp.A = 0) and (Pp.B = 0) and (Pp.C = 0) and (Pp.D = 0);
    end;


    // Optimize A Face: Remove All The Colinear Vertices
    procedure Optimizepl;
    var
      I: Integer;
    begin
      if Np <= 3 then
        Exit;
      Np := 1;
      Pl[0] := Pl2[0];
      for I := 1 to Np2 - 2 do
        if not Col(Pl[Np - 1], Pl2[I], Pl2[I + 1]) then
        begin
          Pl[Np] := Pl2[I];
          Inc(Np);
        end;
      Pl[Np] := Pl2[Np2 - 1];
      Inc(Np);
    end;

  begin
    GetMem(Plist, Nfaces * SizeOf(Poly));
    Plisti := @Plist[0];
    for I := 0 to Nfaces - 1 do
    begin
      Np := 0;
      for J := Qfaces[I].Firstedge to Qfaces[I].Firstedge + Qfaces[I].Numedges - 1 do
        if Qledges[J] < 0 then
          AddEdge(Qedges[-Qledges[J]].V[1], Qedges[-Qledges[J]].V[0])
        else
          AddEdge(Qedges[Qledges[J]].V[0], Qedges[Qledges[J]].V[1]);

      if Pl[Np - 1] = Pl[0] then
        Dec(Np);
      if Np < 0 then
        Np := 0;
      Move(Pl, Pl2, SizeOf(Pl));
      Np2 := Np;
      Optimizepl;
      if Np < 3 then
      begin
        Move(Pl2, Pl,SizeOf(Pl));
        Np := Np2;
      end;

      Plisti.Np := Np;

      Plisti.Tid := Qfaces[I].Texinfo;
      Plisti.Fid := -1;
      GetMem(Plisti.Points, Np * 4);
      for J := 0 to Np - 1 do
      begin
       Plisti.Points[J] := Pl[J];
      end;
      inc(Plisti);
    end;
  end;

  // Scale UV Coords For A Face. (Doesn't Work As It Should...)
  procedure Scaleuv(var P: Uvdata; Fi: Integer; Tu, Tv: Integer);
  var
    Mxu, Mxv: Single;
    Mu, Mv: Single;
    Temp1, Temp2, I: Integer;
    puvi: UvrecP;
  begin
    if P.Np = 0 then
      Exit;
    puvi := @P.Uv[0];
    Mu := puvi.U;
    Mv := puvi.V;
    Mxu := puvi.U;
    Mxv := puvi.V;
    for I := 1 to P.Np - 1 do
    begin
      inc(puvi);
      if puvi.U < Mu then
        Mu := puvi.U;
      if puvi.V < Mv then
        Mv := puvi.V;
      if puvi.U > Mxu then
        Mxu := puvi.U;
      if puvi.V > Mxv then
        Mxv := puvi.V;
    end;

    Temp1 := Floor(Mu / 16.0);
    Temp2 := Ceil(Mxu / 16.0);
    P.Size_S := (Temp2 - Temp1) shl 4;

    Temp1 := Floor(Mv / 16.0);
    Temp2 := Ceil(Mxv / 16.0);
    P.Size_T := (Temp2 - Temp1) shl 4;

    if Tv > Tu then
      Tu := Tv;

    puvi := @P.Uv[0];
    for I := 0 to P.Np - 1 do
    begin
      Temp1 := Floor(puvi.U / 16.0);
      puvi.U := (Temp1 shl 4) / Tu * 256;
      Temp1 := Floor(puvi.V / 16.0);
      puvi.V := (Temp1 shl 4) / Tu * 256;//*(Tv/Tu);
      inc(puvi);
    end;
  end;

  // Scale Lightmap UV Coords For A Face (Works 99.99%)
  procedure Scaleluv(var Lp, P: Uvdata; Fi: Integer);
  var
    I: Integer;
    Mxu, Mxv: Single;
    Mu, Mv, Vu, Vv: Single;
    Temp1, Temp2: Integer;
    puvi: UvrecP;
    lpuvi: UvrecP;
  begin
    if P.Np = 0 then
      Exit;
    puvi := @P.Uv[0];
    Mu := puvi.U;
    Mv := puvi.V;
    Mxu := puvi.U;
    Mxv := puvi.V;
    for I := 1 to P.Np - 1 do
    begin
      inc(puvi);
      if puvi.U < Mu then Mu := puvi.U;
      if puvi.V < Mv then Mv := puvi.V;
      if puvi.U > Mxu then Mxu := puvi.U;
      if puvi.V > Mxv then Mxv := puvi.V;
    end;

    Temp1 := Floor(Mu / 16.0);
    Temp2 := Ceil(Mxu / 16.0);
    Mu := Temp1 shl 4;
    Lp.Size_S := (Temp2 - Temp1) shl 4;

    Temp1 := Floor(Mv / 16.0);
    Temp2 := Ceil(Mxv / 16.0);
    Mv := Temp1 shl 4;
    Lp.Size_T := (Temp2 - Temp1) shl 4;

    Vu := Lp.Size_S;
    Vv := Lp.Size_T;

    lpuvi := @Lp.Uv[0];
    puvi := @P.Uv[0];

    for I := 0 to P.Np - 1 do
    begin
      if Vu <> 0 then
        lpuvi.U := (puvi.U - Mu) / Vu * 255
      else
        lpuvi.U := 0;
      if Vv <> 0 then
        lpuvi.V := (puvi.V - Mv) / Vv * 255
      else
        lpuvi.V := 0;
      if lpuvi.U < 16 then lpuvi.U := 16;
      if lpuvi.V < 16 then lpuvi.V := 16;
      if lpuvi.U > 240 then lpuvi.U := 240;
      if lpuvi.V > 240 then lpuvi.V := 240;
      inc(lpuvi);
      inc(puvi);
    end;
  end;

  // Compute The UVList and LUVList Variables, From Faces' Vectors
  procedure Makeuv;
  var
    I, J, idx: Integer;
    Tu, Tv: Vector;
    tidx, pidx: integer;
    pvec: SingleArray_2x4_P;
    ppoints: SingleArray_3_P;
    pface: Dface_P;
    puv: Puvrec;
    uvP: UvrecP;
    pNFO: Miptex_T_P;
  begin
    GetMem(Uvlist, Nfaces * SizeOf(Uvdata));
    GetMem(Luvlist, Nfaces * SizeOf(Uvdata));
    Plisti := @Plist[0];
    Uvi := @Uvlist[0];
    for I := 0 to Nfaces - 1 do
    begin
      uvi.Np := Plisti.Np;
      GetMem(uvi.Uv, uvi.Np * SizeOf(Uvrec));
      puv := uvi.Uv;
      pface := @Qfaces[I];

      uvP := @puv[0];
      for J := 0 to uvi.Np - 1 do
      begin
        tidx := pface.Texinfo;

        pvec := @Qtexinfo[tidx].Vecs;

        Tu[0] :=-pvec[0][0];
        Tu[1] := pvec[0][2];
        Tu[2] :=-pvec[0][1];

        pidx := Plisti.Points[J];

        ppoints := @Qverts[pidx].Point;

        uvP.U := Fvec_Dotproduct(
          MakeVector(
           -ppoints[0],
            ppoints[2], // Switch from Quake's Coord. Sys. to Direct3D
           -ppoints[1]),
          Vector(Tu)) + Qtexinfo[pface.Texinfo].Vecs[0][3];

        Tv[0] :=-pvec[1][0];
        Tv[1] := pvec[1][2];
        Tv[2] :=-pvec[1][1];
        uvP.V := Fvec_Dotproduct(
          MakeVector(
           -ppoints[0],
            ppoints[2], // Switch from Quake's Coord. Sys. to Direct3D
           -ppoints[1]),
          Vector(Tv)) + Qtexinfo[pface.Texinfo].Vecs[1][3];
        idx := TextureNames.IndexOf(AdjustQuake2EntryName(Qtexinfo[Plisti.Tid].Texture));
        if idx > -1 then
          pNFO := @Texlist[idx].Nfo
        else
          pNFO := nil;
        if (idx > -1) and (pNFO.Width <> 0) and (pNFO.Height <> 0) then
        begin
          uvP.U := uvP.U / pNFO.Width;
          uvP.V := uvP.V / pNFO.Height;
        end
        else
        begin
          uvP.U := uvP.U / 128;
          uvP.V := uvP.V / 128;
        end;
        inc(uvP);
      end;
      Luvlist[I].Np := Plisti.Np;
      GetMem(Luvlist[I].Uv, Plisti.Np * SizeOf(Uvrec));
      Scaleluv(Luvlist[I], uvi^, I);
      inc(Plisti);
      inc(uvi);
    end;
  end;

  function GetLightValue(i: integer): TD3DCOLOR;
  var
    r, g, b: byte;
    Lightofs: Longint;
  begin
    Lightofs := Qfaces[I].Lightofs;
    if Lightofs = -1 then
      Result := CA_MAKE(RGB($FF, $FF, $FF), 0)
    else
    begin
      r := Qlight[Lightofs];
      g := Qlight[Lightofs + 1];
      b := Qlight[Lightofs + 2];

      r :=  round(lFactor_mul_255 + r * One_sub_lFactor);
      g :=  round(lFactor_mul_255 + g * One_sub_lFactor);
      b :=  round(lFactor_mul_255 + b * One_sub_lFactor);

      Result := CA_MAKE(RGB(r, g, b), 0)
    end;
  end;

  // Creates the tessellation patch!
  procedure AddTessellation(const bqPatch: TBiQuadricPatch; const newTessellation: integer); overload;
  var
    Tessellation: integer;
    Tessellation1: integer;
    rTessellation1: integer;
    uTessellation1: integer;
    px, px1, py, py1: TD3DValue;
    v, u: integer;
    row, point: integer;
    temp: array[0..2] of DVertex_v3;
    vertices: PDVertex_v3;
    dvp: DVertex_v3P;
    indices: PIntegerArray;
    numIndices: integer;
    n0, n1, n2, n3: integer;
    pv: PD3DLVertex;
    pvert: DVertex_v3P;
    pColors: PByteArray;
    pposition: SingleArray_3_P;
  begin
    Tessellation := newTessellation;
    Tessellation1 := Tessellation + 1;
    GetMem(vertices, (Tessellation1) * (Tessellation1) * SizeOf(DVertex_v3));

    numIndices := Tessellation * (Tessellation + 1) * 2;
    GetMem(indices, numIndices * SizeOf(Integer));

    dvp := @vertices[0];
    for v := 0 to Tessellation do
    begin
      px := v / Tessellation;
      px1 := 1.0 - px;

      dvp^ := Combine3DVertex_v3(
        bqPatch.ControlPoints[0],
        bqPatch.ControlPoints[3],
        bqPatch.ControlPoints[6],
        px1 * px1,
        px1 * px * 2,
        px * px
      );

      inc(dvp);
    end;

    uTessellation1 := 0;
    for u := 1 to Tessellation do
    begin
      py := u / Tessellation;
      py1 := 1.0 - py;

      temp[0] := Combine3DVertex_v3(
        bqPatch.ControlPoints[0],
        bqPatch.ControlPoints[1],
        bqPatch.ControlPoints[2],
        py1 * py1,
        py1 * py * 2,
        py * py
      );
      temp[1] := Combine3DVertex_v3(
        bqPatch.ControlPoints[3],
        bqPatch.ControlPoints[4],
        bqPatch.ControlPoints[5],
        py1 * py1,
        py1 * py * 2,
        py * py
      );
      temp[2] := Combine3DVertex_v3(
        bqPatch.ControlPoints[6],
        bqPatch.ControlPoints[7],
        bqPatch.ControlPoints[8],
        py1 * py1,
        py1 * py * 2,
        py * py
      );

      uTessellation1 := uTessellation1 + Tessellation1;
      dvp := @vertices[uTessellation1];
      for v := 0 to Tessellation do
      begin
        px := v / Tessellation;
        px1 := 1.0 - px;

        dvp^ := Combine3DVertex_v3(
          temp[0],
          temp[1],
          temp[2],
          px1 * px1,
          px1 * px * 2,
          px * px);

        inc(dvp);
      end;

    end;

    rTessellation1 := 0;
    for row := 0 to Tessellation - 1 do
    begin
      for point := 0 to Tessellation - 1 do
      begin
        n0 := rTessellation1 + point;
        n1 := n0 + Tessellation1; //(row + 1) * (Tessellation1) + point;
        n2 := n1 + 1;
        n3 := n0 + 1;

        pv := @inf.Vertexes[0];

        pvert := @vertices[n0];
        pposition := @pvert.Position;

        pv.x := -pposition[0] / Factor;
        pv.y :=  pposition[2] / Factor;
        pv.z := -pposition[1] / Factor;

        pColors := @pvert.Color;
        pv.color := CA_MAKE(RGB(
          round(lFactor_mul_255 + pColors[0] * One_sub_lFactor),
          round(lFactor_mul_255 + pColors[1] * One_sub_lFactor),
          round(lFactor_mul_255 + pColors[2] * One_sub_lFactor)), 0);
        pv.Specular := DefSpecular;
        pv.tu := pvert.TextureCoord[0];
        pv.tv := pvert.TextureCoord[1];

        pv := @inf.Vertexes[1];

        pvert := @vertices[n1];
        pposition := @pvert.Position;

        pv.x := -pposition[0] / Factor;
        pv.y :=  pposition[2] / Factor;
        pv.z := -pposition[1] / Factor;

        pColors := @pvert.Color;
        pv.color := CA_MAKE(RGB(
          round(lFactor_mul_255 + pColors[0] * One_sub_lFactor),
          round(lFactor_mul_255 + pColors[1] * One_sub_lFactor),
          round(lFactor_mul_255 + pColors[2] * One_sub_lFactor)), 0);
        pv.Specular := DefSpecular;
        pv.tu := pvert.TextureCoord[0];
        pv.tv := pvert.TextureCoord[1];

        pv := @inf.Vertexes[2];

        pvert := @vertices[n2];
        pposition := @pvert.Position;

        pv.x := -pposition[0] / Factor;
        pv.y :=  pposition[2] / Factor;
        pv.z := -pposition[1] / Factor;

        pColors := @pvert.Color;
        pv.color := CA_MAKE(RGB(
          round(lFactor_mul_255 + pColors[0] * One_sub_lFactor),
          round(lFactor_mul_255 + pColors[1] * One_sub_lFactor),
          round(lFactor_mul_255 + pColors[2] * One_sub_lFactor)), 0);
        pv.Specular := DefSpecular;
        pv.tu := pvert.TextureCoord[0];
        pv.tv := pvert.TextureCoord[1];

        AScene.MergePolygonData(ID3D_GenericTriangle, @inf);

        inf.Vertexes[1] := pv^;

        pvert := @vertices[n3];
        pposition := @pvert.Position;

        pv.x := -pposition[0] / Factor;
        pv.y :=  pposition[2] / Factor;
        pv.z := -pposition[1] / Factor;

        pColors := @pvert.Color;
        pv.color := CA_MAKE(RGB(
          round(lFactor_mul_255 + pColors[0] * One_sub_lFactor),
          round(lFactor_mul_255 + pColors[1] * One_sub_lFactor),
          round(lFactor_mul_255 + pColors[2] * One_sub_lFactor)), 0);
        pv.Specular := DefSpecular;
        pv.tu := pvert.TextureCoord[0];
        pv.tv := pvert.TextureCoord[1];

        AScene.MergePolygonData(ID3D_GenericTriangle, @inf);
      end;
      rTessellation1 := rTessellation1 + Tessellation1;
    end;

    FreeMem(indices, numIndices * SizeOf(Integer));
    FreeMem(vertices, (Tessellation1) * (Tessellation1) * SizeOf(DVertex_v3));
  end;

  {$IFNDEF NO_D3DGENERICRTLPOLOYGONS}
  // Creates the tessellation patch!
  procedure AddTessellation(const bqPatch: TBiQuadricPatch;
    const newTessellation: integer; var polyv: PD3DLvertex); overload;
  var
    Tessellation: integer;
    Tessellation1: integer;
    rTessellation1: integer;
    uTessellation1: integer;
    px, px1, py, py1: TD3DValue;
    v, u: integer;
    row, point: integer;
    temp: array[0..2] of DVertex_v3;
    vertices: PDVertex_v3;
    dvp: DVertex_v3P;
    indices: PIntegerArray;
    numIndices: integer;
    n0, n1, n2, n3: integer;
    pv: PD3DLVertex;
    pvert: DVertex_v3P;
    pColors: PByteArray;
    pposition: SingleArray_3_P;
  begin
    Tessellation := newTessellation;
    Tessellation1 := Tessellation + 1;
    GetMem(vertices, Tessellation1 * Tessellation1 * SizeOf(DVertex_v3));

    numIndices := Tessellation * Tessellation1 * 2;
    GetMem(indices, numIndices * SizeOf(Integer));

    dvp := @vertices[0];
    for v := 0 to Tessellation do
    begin
      px := v / Tessellation;
      px1 := 1.0 - px;

      dvp^ := Combine3DVertex_v3(
        bqPatch.ControlPoints[0],
        bqPatch.ControlPoints[3],
        bqPatch.ControlPoints[6],
        px1 * px1,
        px1 * px * 2,
        px * px
      );

      inc(dvp);
    end;

    uTessellation1 := 0;
    for u := 1 to Tessellation do
    begin
      py := u / Tessellation;
      py1 := 1.0 - py;

      temp[0] := Combine3DVertex_v3(
        bqPatch.ControlPoints[0],
        bqPatch.ControlPoints[1],
        bqPatch.ControlPoints[2],
        py1 * py1,
        py1 * py * 2,
        py * py
      );
      temp[1] := Combine3DVertex_v3(
        bqPatch.ControlPoints[3],
        bqPatch.ControlPoints[4],
        bqPatch.ControlPoints[5],
        py1 * py1,
        py1 * py * 2,
        py * py
      );
      temp[2] := Combine3DVertex_v3(
        bqPatch.ControlPoints[6],
        bqPatch.ControlPoints[7],
        bqPatch.ControlPoints[8],
        py1 * py1,
        py1 * py * 2,
        py * py
      );

      uTessellation1 := uTessellation1 + Tessellation1;
      dvp := @vertices[uTessellation1];
      for v := 0 to Tessellation do
      begin
        px := v / Tessellation;
        px1 := 1.0 - px;

        dvp^ := Combine3DVertex_v3(
          temp[0],
          temp[1],
          temp[2],
          px1 * px1,
          px1 * px * 2,
          px * px
        );

        inc(dvp);
      end;

    end;

    rTessellation1 := 0;
    for row := 0 to Tessellation - 1 do
    begin
      for point := 0 to Tessellation - 1 do
      begin
        n0 := rTessellation1 + point;
        n1 := n0 + Tessellation1; //(row + 1) * (Tessellation1) + point;
        n2 := n1 + 1;
        n3 := n0 + 1;

        pv := @inf.Vertexes[0];

        pvert := @vertices[n0];
        pposition := @pvert.Position;

        pv.x := -pposition[0] / Factor;
        pv.y :=  pposition[2] / Factor;
        pv.z := -pposition[1] / Factor;

        pColors := @pvert.Color;
        pv.color := CA_MAKE(RGB(
          round(lFactor_mul_255 + pColors[0] * One_sub_lFactor),
          round(lFactor_mul_255 + pColors[1] * One_sub_lFactor),
          round(lFactor_mul_255 + pColors[2] * One_sub_lFactor)), 0);
        pv.Specular := DefSpecular;
        pv.tu := pvert.TextureCoord[0];
        pv.tv := pvert.TextureCoord[1];

        pv := @inf.Vertexes[1];

        pvert := @vertices[n1];
        pposition := @pvert.Position;

        pv.x := -pposition[0] / Factor;
        pv.y :=  pposition[2] / Factor;
        pv.z := -pposition[1] / Factor;

        pColors := @pvert.Color;
        pv.color := CA_MAKE(RGB(
          round(lFactor_mul_255 + pColors[0] * One_sub_lFactor),
          round(lFactor_mul_255 + pColors[1] * One_sub_lFactor),
          round(lFactor_mul_255 + pColors[2] * One_sub_lFactor)), 0);
        pv.Specular := DefSpecular;
        pv.tu := pvert.TextureCoord[0];
        pv.tv := pvert.TextureCoord[1];

        pv := @inf.Vertexes[2];

        pvert := @vertices[n2];
        pposition := @pvert.Position;

        pv.x := -pposition[0] / Factor;
        pv.y :=  pposition[2] / Factor;
        pv.z := -pposition[1] / Factor;

        pColors := @pvert.Color;
        pv.color := CA_MAKE(RGB(
          round(lFactor_mul_255 + pColors[0] * One_sub_lFactor),
          round(lFactor_mul_255 + pColors[1] * One_sub_lFactor),
          round(lFactor_mul_255 + pColors[2] * One_sub_lFactor)), 0);
        pv.Specular := DefSpecular;
        pv.tu := pvert.TextureCoord[0];
        pv.tv := pvert.TextureCoord[1];

        polyv^ := inf.Vertexes[0];
        inc(polyv);
        polyv^ := inf.Vertexes[1];
        inc(polyv);
        polyv^ := inf.Vertexes[2];
        inc(polyv);

//        AScene.MergePolygonData(ID3D_GenericTriangle, @inf);

        inf.Vertexes[1] := pv^;

        pvert := @vertices[n3];
        pposition := @pvert.Position;

        pv.x := -pposition[0] / Factor;
        pv.y :=  pposition[2] / Factor;
        pv.z := -pposition[1] / Factor;

        pColors := @pvert.Color;
        pv.color := CA_MAKE(RGB(
          round(lFactor_mul_255 + pColors[0] * One_sub_lFactor),
          round(lFactor_mul_255 + pColors[1] * One_sub_lFactor),
          round(lFactor_mul_255 + pColors[2] * One_sub_lFactor)), 0);
        pv.Specular := DefSpecular;
        pv.tu := pvert.TextureCoord[0];
        pv.tv := pvert.TextureCoord[1];

        polyv^ := inf.Vertexes[0];
        inc(polyv);
        polyv^ := inf.Vertexes[1];
        inc(polyv);
        polyv^ := inf.Vertexes[2];
        inc(polyv);
//        AScene.MergePolygonData(ID3D_GenericTriangle, @inf);
      end;
      rTessellation1 := rTessellation1 + Tessellation1;
    end;

    FreeMem(indices, numIndices * SizeOf(Integer));
    FreeMem(vertices, (Tessellation1) * (Tessellation1) * SizeOf(DVertex_v3));
  end;
{$ENDIF}

  function SearchDeepIndexOfTexture(const TextureName: string;
    const TxNames: TDXStringList): integer;
  var
    txname: string;
    local_i: integer;
  begin
    txname := UpperCase(ExtractFileNameOnly(TextureName));
    for local_i := TxNames.Count - 1 downto 0 do
      if txname = UpperCase(ExtractFileNameOnly(TxNames.Strings[local_i])) then
      begin
        Result := local_i;
        Exit;
      end;
    Result := -1;
  end;

  function ExtractFileNameOnlyUntil_(const FileName: string): string;
  var
    i, j: integer;
    len: integer;
  begin
    Result := FileName;
    len := Length(Result);
    j := 0;
    for i := 1 to len do
      if Result[i] in ['\', '/', ':'] then
        j := i;
    if j > 0 then
    begin
      for i := j + 1 to len do
        Result[i - j] := Result[i];
      SetLength(Result, len - j);
    end;
    Result := ExtractFileName(Result);

    len := Pos('.', Result);
    if len > 0 then
      SetLength(Result, len - 1);

    len := Pos('_', Result);
    if len > 0 then
      SetLength(Result, len - 1);

  end;

  function SearchDeepIndexOfSpecialTexture(const TextureName: string;
    const TxNames: TDXStringList): integer;
  var
    txname: string;
    local_i: integer;
  begin
    txname := UpperCase(ExtractFileNameOnlyUntil_(TextureName));
    for local_i := TxNames.Count - 1 downto 0 do
      if txname = UpperCase(ExtractFileNameOnlyUntil_(TxNames.Strings[local_i])) then
      begin
        Result := local_i;
        Exit;
      end;
    Result := -1;
  end;

  procedure GetQuake3MapData;
  var
    fFaces_v3: PDface_T_v3;
    NFaces_v3: integer;
    fTextures_v3: PTexture_v3;
    NTextures_v3: integer;
    fVertexes_v3 : PDVertex_v3;
    NVertexes_v3: integer;
    fMeshIndeces_v3: PIntegerArray;
    NMeshIndeces_v3: integer;
    i, j: integer;
    // Bezier patches support
    Patch: TBSP3Patch;
    PBiPatch: PBiQuadricPatch;
		numPatchesWide,
		numPatchesHigh: integer;
    x, y, row, point: integer;
    // Optimizers
    pv: PD3DLVertex;
    polyv: PD3DLVertex;
    idx: integer;
    pverts: DVertex_v3P;
    pposition: SingleArray_3_P;
    pface: Dface_P_v3;
    pfidx: integer;
    txname: string;
    pColors: PByteArray;
    pWidth: integer;
    qEntries: TDXStringList;
    tqEntries: TDXStringList; // texture qEntries
    isPK3: boolean;
    txPos: integer;
{$IFNDEF NO_D3DGENERICRTLPOLOYGONS}
    rtlpoly: TD3DGenericRTLPolygon;
{$ENDIF}
  begin
    isPK3 := UpperCase(RightStr(MainPAK, 4)) = UpperCase(rsExtPK3);

    fFaces_v3 := nil;
    fTextures_v3 := nil;
    fVertexes_v3 := nil;
    Loadlump(H, Lump_Faces_v3, Pointer(fFaces_v3), NFaces_v3, SizeOf(Dface_T_v3));
    Loadlump(H, Lump_Textures_v3, Pointer(fTextures_v3), NTextures_v3, SizeOf(DTexture_v3));
    Loadlump(H, Lump_Vertices_v3, Pointer(fVertexes_v3), NVertexes_v3, SizeOf(DVertex_v3));
    LoadLump(H, Lump_MeshVerts_v3, Pointer(fMeshIndeces_v3), NMeshIndeces_v3, SizeOf(integer));
    TextureNames := TDXStringList.Create;

    tqEntries := TDXStringList.Create;
    try
      qEntries := TDXStringList.Create;

      Qdir.GetEntries(qEntries);
      try
        for i := 0 to qEntries.Count - 1 do
        begin
          if (UpperCase(ExtractFileExt(qEntries.Strings[i])) = UpperCase(rsExtJpg)) or
             (UpperCase(ExtractFileExt(qEntries.Strings[i])) = UpperCase(rsExtTga)) then
            tqEntries.Add(qEntries.Strings[i]);
        end;
      finally
        qEntries.Free;
      end;

      // qEntries must not be sorted
      for i := 0 to NTextures_v3 - 1 do
      begin

        txname := UpperCase(AdjustQuake2EntryName(TrimStr(fTextures_v3[i].TextureName)));

        if tqEntries.IndexOfFromLast(txname) >= 0 then
          TextureNames.Add(txname)
        else if tqEntries.IndexOfFromLast(txname + UpperCase(rsExtJpg)) >= 0 then
          TextureNames.Add(txname + UpperCase(rsExtJpg))
        else if tqEntries.IndexOfFromLast(txname + UpperCase(rsExtTGA)) >= 0 then
          TextureNames.Add(txname + UpperCase(rsExtTGA))
        else
        begin
          txpos := SearchDeepIndexOfTexture(txname, tqEntries);
          if txpos < 0 then
            txpos := SearchDeepIndexOfSpecialTexture(txname, tqEntries);
          if txpos >= 0 then
          begin
            txname := tqEntries[txpos];
            TextureNames.Add(txname);
          end
          else if FileExists( // not found tqEntries!!!!
            AdjustQuake2EntryName(
              SafeAddSlash(ExtractFilePath(MainPak)) +
                ExtractFileName(AdjustQuake2EntryName(txname)))) then
            TextureNames.Add(
              AdjustQuake2EntryName(
                SafeAddSlash(ExtractFilePath(MainPak)) +
                  ExtractFileName(AdjustQuake2EntryName(txname))))
          else if FileExists(
            AdjustQuake2EntryName(
              SafeAddSlash(ExtractFilePath(MainPak)) +
                ExtractFileName(AdjustQuake2EntryName(txname + rsExtJpg)))) then
            TextureNames.Add(
              AdjustQuake2EntryName(
                SafeAddSlash(ExtractFilePath(MainPak)) +
                  ExtractFileName(AdjustQuake2EntryName(txname + rsExtJpg))))
          else if FileExists(
            AdjustQuake2EntryName(
              SafeAddSlash(ExtractFilePath(MainPak)) +
                ExtractFileName(AdjustQuake2EntryName(txname + rsExtTGA)))) then
            TextureNames.Add(
              AdjustQuake2EntryName(
                SafeAddSlash(ExtractFilePath(MainPak)) +
                  ExtractFileName(AdjustQuake2EntryName(txname + rsExtTGA))))
          else if FileExists(
            AdjustQuake2EntryName(
              SafeAddSlash(ExtractFilePath(PPak)) +
                ExtractFileName(AdjustQuake2EntryName(txname)))) then
            TextureNames.Add(
              AdjustQuake2EntryName(
                SafeAddSlash(ExtractFilePath(PPak)) +
                  ExtractFileName(AdjustQuake2EntryName(txname))))
          else if FileExists(
            AdjustQuake2EntryName(
              SafeAddSlash(ExtractFilePath(PPak)) +
                ExtractFileName(AdjustQuake2EntryName(txname + rsExtJpg)))) then
            TextureNames.Add(
              AdjustQuake2EntryName(
                SafeAddSlash(ExtractFilePath(PPak)) +
                  ExtractFileName(AdjustQuake2EntryName(txname + rsExtJpg))))
          else if FileExists(
            AdjustQuake2EntryName(
              SafeAddSlash(ExtractFilePath(PPak)) +
                ExtractFileName(AdjustQuake2EntryName(txname + rsExtTGA)))) then
            TextureNames.Add(
              AdjustQuake2EntryName(
                SafeAddSlash(ExtractFilePath(PPak)) +
                  ExtractFileName(AdjustQuake2EntryName(txname + rsExtTGA))))
          else if FileExists(
            AdjustQuake2EntryName(
              AScene.GetExistedFileName(txname))) then
            TextureNames.Add(
              AdjustQuake2EntryName(
                AScene.GetExistedFileName(txname)))
          else if FileExists(
            AdjustQuake2EntryName(
              AScene.GetExistedFileName(txname + rsExtJpg))) then
            TextureNames.Add(
              AdjustQuake2EntryName(
                AScene.GetExistedFileName(txname + rsExtJpg)))
          else if FileExists(
            AdjustQuake2EntryName(
              AScene.GetExistedFileName(txname + rsExtTGA))) then
            TextureNames.Add(
              AdjustQuake2EntryName(
                AScene.GetExistedFileName(txname + rsExtTGA)))
          else
            TextureNames.Add(txname); // Add it anyway...

        end
//          TextureNames.Add(EmptyStr);

      end;

      if isPK3 then
        AScene.CacheIDPAKTextures(MainPAK, TextureNames, QuakeVersion, BspVersion, QDir)
      else
        AScene.AddTexturesToCollection(TextureNames, False);
      AScene.PrepareRTLOptimizer(TextureNames, False, False, D3DCULL_NONE, False);

      for i := 0 to NFaces_v3 - 1 do
      begin
        pface := @fFaces_v3[i];
        pfidx := pface.startVertIndex;

        if IsIntegerInRange(pface.textureID, 0, TextureNames.Count - 1) and
           IsValidQ3Name(TextureNames.Strings[pface.textureID]) then
        begin
          inf.TextureName := TextureNames.Strings[pface.textureID];

          if (pface.Facetype = FACE_POLYGON_v3) then
          begin

            pv := @inf.Vertexes[0];
            idx := pfidx;

            pverts := @fVertexes_v3[idx];
            pposition := @pverts.Position;

            pv.x := -pposition[0] / Factor;
            pv.y :=  pposition[2] / Factor;
            pv.z := -pposition[1] / Factor;

            pColors := @pverts.Color;
            pv.color := CA_MAKE(RGB(
              round(lFactor_mul_255 + pColors[0] * One_sub_lFactor),
              round(lFactor_mul_255 + pColors[1] * One_sub_lFactor),
              round(lFactor_mul_255 + pColors[2] * One_sub_lFactor)), 0);
            pv.Specular := DefSpecular;

            pv.tu := pverts.TextureCoord[0];
            pv.tv := pverts.TextureCoord[1];

            for j := 1 to pface.numOfVerts - 2 do
            begin
              pv := @inf.Vertexes[1];
              idx := pfidx + j;

              pverts := @fVertexes_v3[idx];
              pposition := @pverts.Position;

              pv.x := -pposition[0] / Factor;
              pv.y :=  pposition[2] / Factor;
              pv.z := -pposition[1] / Factor;

              pColors := @pverts.Color;
              pv.color := CA_MAKE(RGB(
                round(lFactor_mul_255 + pColors[0] * One_sub_lFactor),
                round(lFactor_mul_255 + pColors[1] * One_sub_lFactor),
                round(lFactor_mul_255 + pColors[2] * One_sub_lFactor)), 0);
              pv.Specular := DefSpecular;

              pv.tu := pverts.TextureCoord[0];
              pv.tv := pverts.TextureCoord[1];

              inc(pv); // pv := @inf.Vertexes[2];
              inc(idx);
              pverts := @fVertexes_v3[idx]; // inc(pverts);
              pposition := @pverts.Position;

              pv.x := -pposition[0] / Factor;
              pv.y :=  pposition[2] / Factor;
              pv.z := -pposition[1] / Factor;

              pColors := @pverts.Color;
              pv.color := CA_MAKE(RGB(
                round(lFactor_mul_255 + pColors[0] * One_sub_lFactor),
                round(lFactor_mul_255 + pColors[1] * One_sub_lFactor),
                round(lFactor_mul_255 + pColors[2] * One_sub_lFactor)), 0);
              pv.Specular := DefSpecular;

              pv.tu := pverts.TextureCoord[0];
              pv.tv := pverts.TextureCoord[1];

              AScene.MergePolygonData(ID3D_GenericTriangle, @inf);
            end; // for j := 1 to fFaces_v3[i].numOfVerts - 2 do

          end // if fFaces_v3[i].Facetype = FACE_POLYGON_v3 then

          else if (pface.Facetype = FACE_MESH_v3) then
          begin
            j := 0;
            while j < pface.numMeshVerts do
            begin
              x := fMeshIndeces_v3[pface.meshVertIndex + j] + pfidx;

              pverts := @fVertexes_v3[x];
              pposition := @pverts.Position;

              pv := @inf.Vertexes[0];

              pv.x := -pposition[0] / Factor;
              pv.y :=  pposition[2] / Factor;
              pv.z := -pposition[1] / Factor;

              pColors := @pverts.Color;
              pv.color := CA_MAKE(RGB(
                round(lFactor_mul_255 + pColors[0] * One_sub_lFactor),
                round(lFactor_mul_255 + pColors[1] * One_sub_lFactor),
                round(lFactor_mul_255 + pColors[2] * One_sub_lFactor)), 0);
              pv.Specular := DefSpecular;
              pv.tu := pverts.TextureCoord[0];
              pv.tv := pverts.TextureCoord[1];

              x := fMeshIndeces_v3[pface.meshVertIndex + j + 2] + pfidx;

              pverts := @fVertexes_v3[x];
              pposition := @pverts.Position;

              pv := @inf.Vertexes[1];

              pv.x := -pposition[0] / Factor;
              pv.y :=  pposition[2] / Factor;
              pv.z := -pposition[1] / Factor;

              pColors := @pverts.Color;
              pv.color := CA_MAKE(RGB(
                round(lFactor_mul_255 + pColors[0] * One_sub_lFactor),
                round(lFactor_mul_255 + pColors[1] * One_sub_lFactor),
                round(lFactor_mul_255 + pColors[2] * One_sub_lFactor)), 0);
              pv.Specular := DefSpecular;
              pv.tu := pverts.TextureCoord[0];
              pv.tv := pverts.TextureCoord[1];

              x := fMeshIndeces_v3[pface.meshVertIndex + j + 1] + pfidx;

              pverts := @fVertexes_v3[x];
              pposition := @pverts.Position;

              pv := @inf.Vertexes[2];

              pv.x := -pposition[0] / Factor;
              pv.y :=  pposition[2] / Factor;
              pv.z := -pposition[1] / Factor;

              pColors := @pverts.Color;
              pv.color := CA_MAKE(RGB(
                round(lFactor_mul_255 + pColors[0] * One_sub_lFactor),
                round(lFactor_mul_255 + pColors[1] * One_sub_lFactor),
                round(lFactor_mul_255 + pColors[2] * One_sub_lFactor)), 0);
              pv.Specular := DefSpecular;
              pv.tu := pverts.TextureCoord[0];
              pv.tv := pverts.TextureCoord[1];

              AScene.MergePolygonData(ID3D_GenericTriangle, @inf);

              inc(j, 3);

            end;


          end // if (fFaces_v3[i].Facetype = FACE_MESH_v3) then

          else if pface.Facetype = FACE_PATCH_v3 then
          begin
            Patch.TextureIndex := pface.textureID;
            Patch.LightMapIndex := pface.lightmapID;
            pWidth := pface.Size[0];
            Patch.Width := pWidth;
            Patch.Height := pface.Size[1];

            numPatchesWide := (pWidth - 1) div 2;
            numPatchesHigh := (Patch.Height - 1) div 2;

            Patch.numQuadricPatches := numPatchesWide * numPatchesHigh;
            if Patch.numQuadricPatches > 0 then
            begin
{$IFNDEF NO_D3DGENERICRTLPOLOYGONS}
              rtlpoly := TD3DGenericRTLPolygon.Create(AScene.DXDraw,
                TextureNames.Objects[pface.textureID] as TDirect3DTexture2, inf.TextureName);
              rtlpoly.PrimitiveType := D3DPT_TRIANGLELIST;
              rtlpoly.DrawMethod := rpd_Normal;
              rtlpoly.NumIndices := 0;
              rtlpoly.VertexTypeDesc := D3DFVF_LVERTEX;
              rtlpoly.NumVertexes := TessellationLevel * TessellationLevel * 6 * Patch.numQuadricPatches;
              polyv := @rtlpoly.Vertexes[0];
{$ENDIF}
              GetMem(Patch.Patches, Patch.numQuadricPatches * SizeOf(TBiQuadricPatch));

            // fill in the quadratic patches
            	for y := 0 to numPatchesHigh - 1 do
                for x := 0 to numPatchesWide - 1 do
                begin
                  PBiPatch := @Patch.Patches[y * numPatchesWide + x];
                  for row := 0 to 2 do
                    for point := 0 to 2 do
                      PBiPatch.ControlPoints[row * 3 + point] :=
                        fVertexes_v3[
                          (pfidx) +
                          (y * 2 * pWidth + x * 2) +
                          (row * pWidth + point)];
{$IFNDEF NO_D3DGENERICRTLPOLOYGONS}
                  AddTessellation(PBiPatch^, TessellationLevel, polyv);
{$ELSE}
                  AddTessellation(Patch.Patches[y * numPatchesWide + x], TessellationLevel);
{$ENDIF}
                end;

{$IFNDEF NO_D3DGENERICRTLPOLOYGONS}
              AScene.AddSurface(rtlpoly);
{$ENDIF}

              FreeMem(Patch.Patches, Patch.numQuadricPatches * SizeOf(TBiQuadricPatch));
            end;
          end

        end; // IsIntegerInRange

      end; //for i := 0 to NFaces_v3 - 1 do

      AScene.ReleaseUnusedObjects;

    finally
      TextureNames.Free;
      tqEntries.Free;
      ReAllocMem(fFaces_v3, 0);
      ReAllocMem(fTextures_v3, 0);
      ReAllocMem(fVertexes_v3, 0);
      ReAllocMem(fMeshIndeces_v3, 0);
    end;
  end;

// GetQuake2MapData begin
var
  I, J: Integer;
  mipTex: Miptex_T_v1;
  mipTex_m8: Miptex_T_m8;
  m8: boolean;
  ptmppoints: SingleArray_3_P;
{$IFDEF DESIGNER}
  oldCanSaveUndo: boolean;
{$ENDIF}
begin
  AScene.AddSearchPath(ExtractFilePath(MainPak), True);
  AScene.AddSearchPath(PPak, True);
  Qdir := TPakDir.Create(MainPak, PPak);

  if Pos(UpperCase(rsExtBsp), UpperCase(MapName)) = 0 then
    MapName := MapName + rsExtBsp;
  if not Qdir.Openfile(P, MapName) then
  begin
    Result := 1;
    Qdir.Free;
    // Error: not found
    Exit;
  end;

  Qdir.Qblockread(P, H, SizeOf(H));
  if (H.Ident <> Idbspheader) and (H.Version_v1 <> Bspversion_29) and (H.Version_v1 <> Bspversion_30) then
  begin
    // Error: Not A Quake1/2/3 bsp;
    Result := 2;
    Qdir.Free;
    Exit;
  end;
  if (H.Version <> Bspversion_38) and (H.Version <> Bspversion_46) and (H.Version <> Bspversion_47) and
     (H.Version_v1 <> Bspversion_29) and (H.Version_v1 <> Bspversion_30) then
  begin
    // Error: Wrong Bsp Version;
    Result := 3;
    Qdir.Free;
    Exit;
  end;

  if H.Version = Bspversion_38 then
  begin
    QuakeVersion := 2;
    BspVersion := H.Version;
  end
  else if H.Version in [Bspversion_46, Bspversion_47] then
  begin
    QuakeVersion := 3;
    BspVersion := H.Version;
  end
  else
  begin
    QuakeVersion := 1;
    BspVersion := H.Version_v1;
  end;

  One_sub_lFactor := 1 - lFactor;
  lFactor_mul_255 := lFactor * 255;
  DefSpecular := CA_MAKE(0, 0);

  if QuakeVersion = 3 then
  begin
   {$IFDEF DESIGNER}
    AScene.SaveUndo;
    oldCanSaveUndo := AScene.CanSaveUndo;
    AScene.CanSaveUndo := False;
   {$ENDIF}
    GetQuake3MapData;
    QDir.Closefile(P);
    QDir.Free;
    Result := 0;
  {$IFDEF DESIGNER}
    AScene.CanSaveUndo := oldCanSaveUndo;
  {$ENDIF}
    Exit;
  end
  else if QuakeVersion = 2 then
  begin
    Loadlump(H, Lump_Edges, Pointer(Qedges), Nedges, SizeOf(Dedge_T));
    Loadlump(H, Lump_Surfedges, Pointer(Qledges), Nledges, SizeOf(Integer));
    Loadlump(H, Lump_Vertexes, Pointer(Qverts), Nverts, SizeOf(Dvertex_T));
    Loadlump(H, Lump_Texinfo, Pointer(Qtexinfo), Ntexinfo, SizeOf(Texinfo_T));
    Loadlump(H, Lump_Faces, Pointer(Qfaces), Nfaces, SizeOf(Dface_T));
    Loadlump(H, Lump_Lighting, Pointer(Qlight), Lightsz, 1);
  end
  else
  begin
    Loadlump(H, Lump_Edges_v1, Pointer(Qedges), Nedges, SizeOf(Dedge_T));
    Loadlump(H, Lump_Surfedges_v1, Pointer(Qledges), Nledges, SizeOf(Integer));
    Loadlump(H, Lump_Vertexes_v1, Pointer(Qverts), Nverts, SizeOf(Dvertex_T));
    Loadlump(H, Lump_Texinfo_v1, Pointer(Qtexinfo_v1), Ntexinfo, SizeOf(Texinfo_T_v1));
    LoadTexture_v1_lump(H, Lump_Textures_v1, QMipTexs_v1, NMipTexs_v1);
    GetMem(Qtexinfo, Ntexinfo * SizeOf(Texinfo_T));
    for i := 0 to Ntexinfo - 1 do
    begin
      for j := 0 to 3 do
      begin
        Qtexinfo[i].Vecs[0, j] := Qtexinfo_v1[i].Vecs[0, j];
        Qtexinfo[i].Vecs[1, j] := Qtexinfo_v1[i].Vecs[1, j];
      end;

      Qtexinfo[i].Flags := Qtexinfo_v1[i].Flags;
      Qtexinfo[i].Value := 0;

      for j := 0 to 15 do
        Qtexinfo[i].Texture[j] := QMipTexs_v1[Qtexinfo_v1[i].mipTex].Nfo.Name[j];
      for j := 16 to 31 do
        Qtexinfo[i].Texture[j] := #0;

      Qtexinfo[i].Nexttexinfo := -1;

    end;
    FreeMem(Qtexinfo_v1, Ntexinfo * SizeOf(Texinfo_T_v1));
    FreeMem(QMipTexs_v1, NMipTexs_v1 * SizeOf(Miptex_S_v1));

    Loadlump(H, Lump_Faces_v1, Pointer(Qfaces), Nfaces, SizeOf(Dface_T));
    Loadlump(H, Lump_Lighting_v1, Pointer(Qlight), Lightsz, 1);
  end;

  Convert_F;

  TextureNames := TDXStringList.Create;
  try
    Plisti := @PList[0];
    for i := 0 to NFaces - 1 do
    begin
      if not IsFakeQuake2TextureName(Qtexinfo[Plisti.Tid].Texture) then
        if TrimStr(Qtexinfo[Plisti.Tid].Texture) <> EmptyStr then
          if TextureNames.IndexOf(AdjustQuake2EntryName(Qtexinfo[Plisti.Tid].Texture)) < 0 then
            TextureNames.Add(AdjustQuake2EntryName(Qtexinfo[Plisti.Tid].Texture));
      inc(Plisti);
    end;
    AScene.CacheIDPAKTextures(MainPAK, TextureNames, QuakeVersion, BspVersion, QDir);

    GetMem(Texlist, TextureNames.Count * SizeOf(Miptex_S));

    if QuakeVersion = 2 then
    begin
      for i := 0 to TextureNames.Count - 1 do
      begin
        m8 := False;
        ptex := @Texlist[i];
        if QDir.Openfile(TxP, TextureNames.Strings[i]) then
        begin
          QDir.Qblockread(TxP, ptex.Nfo, SizeOf(ptex.Nfo));
          QDir.Closefile(TxP);
        end
        else if QDir.Openfile(TxP, TextureNames.Strings[i] + rsExtWal) then
        begin
          QDir.Qblockread(TxP, ptex.Nfo, SizeOf(ptex.Nfo));
          QDir.Closefile(TxP);
        end
        else if QDir.Openfile(TxP, TextureNames.Strings[i] + rsExtM8) then
        begin
          QDir.Qblockread(TxP, mipTex_m8, SizeOf(mipTex_m8));
          QDir.Closefile(TxP);
          m8 := True;
        end
        else if QDir.Openfile(TxP, rsQuake2TexturesDIR + TextureNames.Strings[i]) then
        begin
          QDir.Qblockread(TxP, ptex.Nfo, SizeOf(ptex.Nfo));
          QDir.Closefile(TxP);
        end
        else if QDir.Openfile(TxP, rsQuake2TexturesDIR + TextureNames.Strings[i] + rsExtWal) then
        begin
          QDir.Qblockread(TxP, ptex.Nfo, SizeOf(ptex.Nfo));
          QDir.Closefile(TxP);
        end
        else if QDir.Openfile(TxP, rsQuake2TexturesDIR + TextureNames.Strings[i] + rsExtM8) then
        begin
          QDir.Qblockread(TxP, mipTex_m8, SizeOf(mipTex_m8));
          QDir.Closefile(TxP);
          m8 := True;
        end
        else
        begin
          FillChar(ptex.Nfo, SizeOf(ptex.Nfo), Chr(0));
          // Default texture dimentions
          ptex.Nfo.Width := 128;
          ptex.Nfo.Height := 128;
        end;
        if m8 then
        begin
          FillChar(ptex.Nfo, SizeOf(ptex.Nfo), Chr(0));
          ptex.Nfo.Width := mipTex_m8.Widths[0];
          ptex.Nfo.Height := mipTex_m8.Heights[0];
          ptex.Nfo.Flags := mipTex_m8.Flags;
          ptex.Nfo.Contents := mipTex_m8.Contents;
          ptex.Nfo.Value := mipTex_m8.Value;
          for j := 0 to 3 do
            ptex.Nfo.Offsets[j] := mipTex_m8.Offsets[j];
          for j := 0 to 31 do
          begin
            ptex.Nfo.Name[j] := mipTex_m8.Name[j];
            ptex.Nfo.Animname[j] := mipTex_m8.Animname[j];
          end;
        end;
      end;
    end
    else if QuakeVersion = 1 then
    begin
      for i := 0 to TextureNames.Count - 1 do
      begin
        ptex := @Texlist[i];
        if QDir.Openfile(TxP, TextureNames.Strings[i]) then
        begin
          QDir.Qblockread(TxP, mipTex, SizeOf(mipTex));
          QDir.Closefile(TxP);
        end
        else if QDir.Openfile(TxP, TextureNames.Strings[i] + rsExtWal) then
        begin
          QDir.Qblockread(TxP, mipTex, SizeOf(mipTex));
          QDir.Closefile(TxP);
        end
        else if QDir.Openfile(TxP, TextureNames.Strings[i] + rsExtM8) then
        begin
          QDir.Qblockread(TxP, mipTex, SizeOf(mipTex));
          QDir.Closefile(TxP);
        end
        else if QDir.Openfile(TxP, rsQuake2TexturesDIR + TextureNames.Strings[i]) then
        begin
          QDir.Qblockread(TxP, mipTex, SizeOf(mipTex));
          QDir.Closefile(TxP);
        end
        else if QDir.Openfile(TxP, rsQuake2TexturesDIR + TextureNames.Strings[i] + rsExtWal) then
        begin
          QDir.Qblockread(TxP, mipTex, SizeOf(mipTex));
          QDir.Closefile(TxP);
        end
        else if QDir.Openfile(TxP, rsQuake2TexturesDIR + TextureNames.Strings[i] + rsExtM8) then
        begin
          QDir.Qblockread(TxP, mipTex, SizeOf(mipTex));
          QDir.Closefile(TxP);
        end
        else
        begin
          FillChar(mipTex, SizeOf(mipTex), Chr(0));
          // Default texture dimentions
          ptex.Nfo.Width := 128;
          ptex.Nfo.Height := 128;
        end;
        FillChar(ptex.Nfo, SizeOf(ptex.Nfo), Chr(0));
        ptex.Nfo.Width := mipTex.Width;
        ptex.Nfo.Height := mipTex.Height;
        for j := 0 to 3 do
          ptex.Nfo.Offsets[j] := mipTex.Offsets[j];
        for j := 0 to 15 do
          ptex.Nfo.Name[j] := mipTex.Name[j];
        for j := 16 to 31 do
          ptex.Nfo.Name[j] := #0;
      end;
    end;

    Makeuv;

  finally
    TextureNames.Free;
  end;

  if Lightsz = 0 then
    for I := 0 to Nfaces - 1 do
      Qfaces[I].Lightofs := -1;


  QDir.Closefile(P);

  FillChar(inf, SizeOf(inf), Chr(0));

  for i := 0 to NVerts - 1 do
    for j := 0 to 2 do
      Qverts[I].Point[j] := Qverts[I].Point[j] / Factor;

{$IFDEF DESIGNER}
   AScene.SaveUndo;
   oldCanSaveUndo := AScene.CanSaveUndo;
   AScene.CanSaveUndo := False;
{$ENDIF}
  Plisti := @PList[0];
  uvi := @UvList[0];
  for i := 0 to Nfaces - 1 do
  begin
    if not IsFakeQuake2TextureName(Qtexinfo[Plisti.Tid].Texture) then
      if Plisti.Np > 2 then
      begin
        if TrimStr(Qtexinfo[Plisti.Tid].Texture) <> EmptyStr then
          inf.TextureName := GetPAKLinkDescription(MainPAK, AdjustQuake2EntryName(Qtexinfo[Plisti.Tid].Texture))
        else
          inf.TextureName := '';

        pv := @inf.Vertexes[0];
        ptmppoints := @Qverts[Plisti.Points[0]].Point;
        pv.x :=-ptmppoints[0];
        pv.y := ptmppoints[2];
        pv.z :=-ptmppoints[1];
        tmpColor := GetLightValue(i);
        pv.color := tmpColor;
        pv.specular := DefSpecular;
        pv.tu := uvi.Uv[0].U;
        pv.tv := uvi.Uv[0].V;
        for J := 1 to Plisti.Np - 2 do
        begin
          pv := @inf.Vertexes[1];
          ptmppoints := @Qverts[Plisti.Points[j]].Point;
          pv.x :=-ptmppoints[0];
          pv.y := ptmppoints[2];
          pv.z :=-ptmppoints[1];
          pv.color := tmpColor; // inf.Vertexes[0].color;
          pv.specular := DefSpecular;
          pv.tu := uvi.Uv[j].U;
          pv.tv := uvi.Uv[j].V;

          inc(pv); // pv := @inf.Vertexes[2];
          ptmppoints := @Qverts[Plisti.Points[j + 1]].Point;
          pv.x :=-ptmppoints[0];
          pv.y := ptmppoints[2];
          pv.z :=-ptmppoints[1];
          pv.color := tmpColor;
          pv.specular := DefSpecular;
          pv.tu := uvi.Uv[j + 1].U;
          pv.tv := uvi.Uv[j + 1].V;

          AScene.MergePolygonData(ID3D_GenericTriangle, @inf);
        end;
      end;
    inc(Plisti);
    inc(uvi);
  end;

{$IFDEF DESIGNER}
  AScene.CanSaveUndo := oldCanSaveUndo;
{$ENDIF}

  Qdir.Free;

  for i := 0 to Nfaces - 1 do
  begin
    ReAllocMem(Luvlist[I].Uv, 0);
    ReAllocMem(Uvlist[I].Uv, 0);
  end;
  ReAllocMem(Uvlist, 0);
  ReAllocMem(Luvlist, 0);

  for i := 0 to Nfaces - 1 do
    ReAllocMem(Plist[I].Points, 0);

  ReAllocMem(Plist, 0);

  ReAllocMem(Texlist, 0);

  ReAllocMem(Qedges, 0);
  ReAllocMem(Qledges, 0);
  ReAllocMem(Qverts, 0);
  ReAllocMem(Qtexinfo, 0);
  ReAllocMem(Qfaces, 0);
  ReAllocMem(Qlight, 0);

  Result := 0;
end;

resourceString
  rsErr1 = 'Map not found';
  rsErr2 = 'Not a Quake1/2/3 bsp';
  rsErr3 = 'Unsupported bsp version';

function Quake2ErrorToString(const err: integer): string;
begin
  case err of
    1: Result := rsErr1;
    2: Result := rsErr2;
    3: Result := rsErr3;
  else
    Result := '';
  end;
end;

resourceString
  rsFmtPAKLINKDESCRIPTION = '->%s::%s';
  rsFmtPAKLINKDESCRIPTION1 = '->';
  rsFmtPAKLINKDESCRIPTION2 = '::';

function GetPAKLinkDescription(const PAKFileName: string; Entry: string): string;
var
  sPAK, sEntry: string;
begin
  if GetPAKLinkInfo(Entry, sPAK, sEntry) then // Entry is already OK
    Result := Format(rsFmtPAKLINKDESCRIPTION, [PAKFileName, sEntry])
  else
    Result := Format(rsFmtPAKLINKDESCRIPTION, [PAKFileName, Entry]);
end;

function IsPAKLinkInfo(const inf: string): boolean;
var
  PAKFileName: string;
  Entry: string;
begin
  Result := GetPAKLinkInfo(inf, PAKFileName, Entry);
  if Result then
    Result := (UpperCase(RightStr(PAKFileName, 3)) = rsPAKExt) or
              (UpperCase(RightStr(PAKFileName, 3)) = rsBSPExt) or
              (UpperCase(RightStr(PAKFileName, 3)) = rsPK3Ext);
end;

function GetPAKLinkInfo(const inf: string; var PAKFileName: string; var Entry: string): boolean;
// Split inf (eg '->C:\PAK1.PAK::WALL1.WAL') to 'C:\PAK1.PAK' & 'WALL1.WAL'
var
  sEntry: string;
  i, j: integer;
begin
  Result := False;
  if Length(inf) >= Length(rsFmtPAKLINKDESCRIPTION1) + Length(rsFmtPAKLINKDESCRIPTION2) + 2 then
  begin
    if inf[1] + inf[2] = rsFmtPAKLINKDESCRIPTION1 then
    begin
      sEntry := '';
      i := Length(inf);
      while (i > Length(rsFmtPAKLINKDESCRIPTION1)) and
            (inf[i] + inf[i - 1] <> rsFmtPAKLINKDESCRIPTION2[Length(rsFmtPAKLINKDESCRIPTION2) - 1] + rsFmtPAKLINKDESCRIPTION2[Length(rsFmtPAKLINKDESCRIPTION2)]) do
      begin
        sEntry := inf[i] + sEntry;
        dec(i);
      end;
      if i > Length(rsFmtPAKLINKDESCRIPTION1) then
      begin
        PAKFileName := '';
        for j := Length(rsFmtPAKLINKDESCRIPTION1) + 1 to i - Length(rsFmtPAKLINKDESCRIPTION2) do
          PAKFileName := PAKFileName + inf[j];
        Entry := sEntry;
        Result := True;
      end;
    end;
  end;
end;

{$ENDIF}
end.

