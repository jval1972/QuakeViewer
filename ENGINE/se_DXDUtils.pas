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
//  Various utility functions
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//------------------------------------------------------------------------------

{$I defs.inc}

unit se_DXDUtils;

interface

uses
  SysUtils, Classes, Windows, Graphics, Controls, se_DirectX, se_D3DUtils,
  se_MyD3DUtils;

const
  MAX_SINGLE = 3.4e38;

  BIGFILESIZE = 1 shl 20; // 1mb

type
  TCPUFrequencyInfo = record
    RawFreq: Cardinal;
    NormFreq: Cardinal;
    InCycles: Cardinal;
    ExTicks: Cardinal;
  end;

  PBooleanArray = ^TBooleanArray;
  TBooleanArray = array[0..$FFFE] of boolean;

  PIntegerArray = ^TIntegerArray;
  TIntegerArray = array[0..$FFFE] of integer;

  PSmallIntArray = ^TSmallIntArray;
  TSmallIntArray = array[0..$FFFE] of SmallInt;

  PBoolArray = ^TBoolArray;
  TBoolArray = array[0..$FFFE] of boolean;

  PSingleArray = ^TSingleArray;
  TSingleArray = array[0..$FFFF] of single;

  PRect = ^TRect;

  TSmallIntPoint = packed record
    x, y: smallInt;
  end;

  PFloatPoint = ^TFloatPoint;
  TFloatPoint = record
    x, y: TD3DValue;
  end;

  PFloatTriangle = ^TFloatTriangle;
  TFloatTriangle = array[0..2] of TFloatPoint;

  PFloatTriangleArray = ^TFloatTriangleArray;
  TFloatTriangleArray = array[0..$FFFF] of TFloatTriangle;

  PD3DFloatTriangle = ^TD3DFloatTriangle;
  TD3DFloatTriangle = array[0..2] of TD3DVector;

  PD3DFloatTriangleArray = ^TD3DFloatTriangleArray;
  TD3DFloatTriangleArray = array[0..$FFFF] of TD3DFloatTriangle;

  PFloatRect = ^TFloatRect;
  TFloatRect = record
    case Integer of
      0: (Left, Top, Right, Bottom: TD3DValue);
      1: (TopLeft, BottomRight: TFloatPoint);
  end;

  PBoundingCube = ^TBoundingCube;
  TBoundingCube = record
    minX, maxX,
    minY, maxY,
    minZ, maxZ: TD3DValue;
  end;

  PFloatPointsArray = ^TFloatPointsArray;
  TFloatPointsArray = array[0..$FFFE] of TFloatPoint;

  PPointsArray = ^TPointsArray;
  TPointsArray = array[0..$FFFE] of TPoint;

  TString2 = packed record
    case integer of
      1: (c1: char;
          c2: char);
      2: (w: word);
  end;

// backups file FileName with extention BackExt
function CreateBackupFile(const FileName: TFileName; const BackExt: string): boolean; overload;

// backups file FileName with default extention
function CreateBackupFile(const FileName: TFileName): boolean; overload;

function LeftStr(const AText: string; const ACount: Integer): string;

function RightStr(const AText: string; const ACount: Integer): string;

function MidStr(const AText: string; const AStart, ACount: Integer): string;

function RemoveSpaces(const AText: string): string;

// eg  C:\DXPLUGIN1.DLL & BMP_1 return '->C:\DXPLUGIN1.DLL::BMP_1'
function GetDllLinkDescription(const DllFileName: string; Entry: string): string;

function IsDLLLinkInfo(const inf: string): boolean;

// split inf (eg '->C:\DXPLUGIN1.DLL::BMP_1') to 'C:\DXPLUGIN1.DLL' & 'BMP_1'
function GetDLLLinkInfo(const inf: string; var DLLFileName: string; var Entry: string): boolean;

function SizeOfFile(const FileName: TFileName): integer;

// Polygon hit test, integer version
function I_PtInPolygon(const rgpts: array of TPoint; wnumpts: word;
  ptTest: TPoint; prbound: PRect = nil): boolean;

function I_PtInPolyRect(const rgpts: array of TPoint; wnumpts: word;
  ptTest: TPoint; prbound: PRect = nil): boolean;

function I_Intersect(const p1, p2, p3, p4: TPoint): boolean;

function I_CCW(const p0, p1, p2: TPoint): integer;

// Polygon hit test, float version
function F_PtInPolygon(const rgpts: array of TFloatPoint; wnumpts: word;
  ptTest: TFloatPoint; prbound: PFloatRect = nil): boolean;

function F_PtInPolyRect(const rgpts: array of TFloatPoint; wnumpts: word;
  ptTest: TFloatPoint; prbound: PFloatRect = nil): boolean;

function F_Intersect(const p1, p2, p3, p4: TFloatPoint): boolean;

function F_CCW(const p0, p1, p2: TFloatPoint): integer;

function IsPointEqual(const p1, p2: TPoint): boolean;

function IsFloatPointEqual(const p1, p2: TFloatPoint): boolean;

function FloatPointDotProduct(const v1, v2: TFloatPoint): TD3DValue;

function TrimStr(const s: string): string; overload;

procedure TrimStr(s: TStringList); overload;

procedure RemoveTrailingLines(s: TStringList); overload;

procedure RemoveTrailingLines(var str: string); overload;

function GetTriangleProjection(const tr: TD3DFloatTriangle;
  const ptTest: TD3DVector): TD3DValue; overload;

function GetTriangleProjection(const tr: TD3DFloatTriangle;
  const ptTest: TFloatPoint): TD3DValue; overload;

// Triangle hit test, TFloatTriangle version
function F_PtInTriangle(const tr: TFloatTriangle;
  const ptTest: TFloatPoint): boolean; overload;

// Triangle near hit test, TFloatTriangle version
function F_PtNearTriangle(const tr: TFloatTriangle;
  const ptTest: TFloatPoint; const Tolerance: TD3DValue): boolean;

// Triangle hit test, TD3DFloatTriangle version
function F_PtInTriangle(const tr: TD3DFloatTriangle;
  const ptTest: TD3DVector): boolean; overload;

function F_PtInTriangle(const tr: TD3DFloatTriangle;
  const ptTest: TFloatPoint): boolean; overload;

{ true αν το v είναι στην γωνία με πλευρά v1, v2 και τιμή angle }
function IsLVertexInAngle(const v, v1, v2: TD3DLVertex; const angle: single): boolean; overload;

function IsFloatPointInAngle(const v, v1, v2, v3: TFloatPoint): boolean;

function MakeFloatPoint(const x, y: TD3DValue): TFloatPoint; overload;

function MakeFloatPoint(const v: TD3DVector): TFloatPoint; overload;

function MakeFloatPoint(const v: TD3DLVertex): TFloatPoint; overload;

function GetProjVertexSqrDistance(const v1, v2: TD3DLVertex): TD3DValue;

function GetValueInRange(Value: TD3DValue; const lowV, hiV: TD3DValue): TD3DValue;

function GetIntegerInRange(Value: integer; const lowV, hiV: integer): integer;

function IsIntegerInRange(const query: integer; const lowV, hiV: integer): boolean;

function IsFloatInRange(query: TD3DValue; const lowV, hiV: TD3DValue): boolean;

function VectorInCube(const v: TD3DVector; const cb: TBoundingCube): boolean;

function ProjectionInCube(const v: TD3DVector; const cb: TBoundingCube): boolean; overload;

function ProjectionInCube(const v: TFloatPoint; const cb: TBoundingCube): boolean; overload;

function VectorInRect(const v: TD3DVector; const r: TFloatRect): boolean;

function FloatPointInRect(const p: TFloatPoint; const r: TFloatRect): boolean;

procedure MakeFloatRect(var r: TFloatRect; const v1, v2: TD3DVector); overload;

procedure MakeFloatRect(var r: TFloatRect; const v1, v2: TFloatPoint); overload;

function MakeFloatRect(const v1, v2: TD3DVector): TFloatRect; overload;

function MakeFloatRect(const v1, v2: TFloatPoint): TFloatRect; overload;

function MakeFloatRect(left, top, right, bottom: TD3DValue): TFloatRect; overload;

function IntersectFloatRect(const r1, r2: TFloatRect): boolean;

procedure MakeEmptyBoundingCube(var BoundingCube: TBoundingCube);

function CombineBoundingCubes(BoundingCube1, BoundingCube2: TBoundingCube): TBoundingCube;

function MakeSlideIntersection(var newPos: TD3DVector; const oldPos: TD3DVector;
  const step1, step2: TFloatPoint; const distance: TD3DValue): boolean;

function MakeStopIntersection(var newPos: TD3DVector; const oldPos: TD3DVector;
  const step1, step2: TFloatPoint; const distance: TD3DValue): boolean;

function MakeReflectIntersection(var newPos: TD3DVector; const oldPos: TD3DVector;
  const step1, step2: TFloatPoint; const distance: TD3DValue; var newRotationY: TD3DValue): boolean;

function MakeStopRoundIntersection(var newPos: TD3DVector; const oldPos: TD3DVector;
  const Center: TD3DVector; const radius: TD3DValue; const distance: TD3DValue): boolean;

function MakeSlideRoundIntersection(var newPos: TD3DVector; const oldPos: TD3DVector;
  const Center: TD3DVector; const radius: TD3DValue; const distance: TD3DValue): boolean;

function MakeSlideInsideCyrcleIntersection(var newPos: TD3DVector; const oldPos: TD3DVector;
  const Center: TD3DVector; const radius: TD3DValue;
  const distance: TD3DValue = 0.0; const doForceInside: boolean = false): boolean;

function GetYDirectionVector(const y: TD3DValue): TD3DVector;

function GetHorizontalProjectionAngle(const v1, v2: TD3DVector): TD3DValue;

function GetHorizontalDistance(const v1, v2: TD3DVector): TD3DValue;

function SafeAddSlash(const s: string): string;

function FirstLine(const s: string): string;

procedure SaveTextFile(const FileName: TFileName; const Source: string);

function LoadTextFile(const FileName: TFileName): string;

function GetCPUSpeed: TCPUFrequencyInfo;

function GetNumCPUs: word;

procedure WIN_DisableAltTab;

procedure WIN_EnableAltTab;

procedure CalcCenterAndRadius(TheVertexes: PD3DLVertexArray; fNumVertexes: integer;
  var fRadius: TD3DValue; var fCenter: TD3DVector); overload;

procedure CalcCenterAndRadius(TheVertexes: PD3DVectorArray; fNumVertexes: integer;
  var fRadius: TD3DValue; var fCenter: TD3DVector); overload;

function CalcBoundingCube(TheVertexes: PD3DLVertexArray; fNumVertexes: integer): TBoundingCube; overload;

function CalcBoundingCube(TheVertexes: PD3DVectorArray; fNumVertexes: integer): TBoundingCube; overload;

// Μετατρέπει τους χαρακτήρες ' σε μία συμβολοσειρά
function GetPascalText(const s: string): string;

function ExtractFileNameOnly(FileName: string): string;

function RemoveExtention(const FileName: string): string;

function GetAngle(const v1, v2: TFloatPoint): TD3DValue; overload;

function GetAngle(const v1, v2, v3: TFloatPoint): TD3DValue; overload;

function GetAngle(const v1, v2, v3: TD3DLVertex): TD3DValue; overload;

function GetNormalizeAngle(a: TD3DValue): TD3DValue;

function MakeStr(const Args: array of const): string;

function IntToStr2(i: integer): string;

function IntToStr3(i: integer): string;

function IntToStr4(i: integer): string;

procedure FreeAndNilSafe(var Obj);

procedure DoForegroundForms(parent: TComponent);

procedure doClearStringList(S: TStringList);

function GetHashCode(const Buffer; Count: Integer): Word; assembler;

procedure MakeBmpAlphaChannel(bmp: TBitmap; Alpha: byte);

function GetPositionInsideBoundingCube(const pCube: PBoundingCube;
  var X, Y, Z: TD3DValue): boolean; overload;

function GetPositionInsideBoundingCube(const pCube: PBoundingCube;
  var V: TD3DVector): boolean; overload;

function GetPositionInsideBoundingCube(const pCube: PBoundingCube;
  const Offset: TD3DValue;
  var X, Y, Z: TD3DValue): boolean; overload;

function GetPositionInsideBoundingCube(const pCube: PBoundingCube;
  const Offset: TD3DValue;
  var V: TD3DVector): boolean; overload;

function FixBackSlash(const s: string): string;

procedure EliminateBlackPixels(b: TBitmap); overload;

procedure EliminateBlackPixels(b: TBitmap; const tolerance: byte); overload;

procedure MakeBlackRangePixels(b: TBitmap); overload;

procedure MakeBlackRangePixels(b: TBitmap; const tolerance: byte); overload;

procedure FlashHandle(h: THandle; sleepmsecs: integer);

procedure WriteStringToStream(strm: TStream; const s: string);

function ReadStringFromStream(strm: TStream): string;

function TryFocusControl(c: TWinControl): boolean;

implementation

uses
  Forms, Dialogs, Math, SyncObjs;

function CreateBackupFile(const FileName: TFileName; const BackExt: string): boolean;
var
  BackFileName: TFileName;
  Ext: string;
  f1, f2: TFileStream;
begin
  Result := false;
  if FileExists(FileName) then
  begin
    if length(BackExt) > 0 then
    begin
      Ext := BackExt;
      if Ext[1] <> '.' then
         Ext := '.' + Ext;
    end
    else
      Ext := '.';
    if UpperCase(ExtractFileExt(FileName)) <> UpperCase(Ext) then
    begin
      BackFileName := ChangeFileExt(FileName, Ext);
      f1 := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
      f2 := TFileStream.Create(BackFileName, fmCreate or fmShareDenyWrite);
      try
        f2.Size := 0;
        f2.CopyFrom(f1, 0);
        Result := true;
      finally
        f1.Free;
        f2.Free;
      end;
    end;
  end;
end;

function CreateBackupFile(const FileName: TFileName): boolean;
var
  Ext: string;
begin
  Ext := ExtractFileExt(FileName);
  if Length(Ext) > 1 then
  begin
    if Ext[1] <> '.' then Ext := '.' + Ext;
    Insert('~', Ext, 2);
    Result := CreateBackupFile(FileName, Ext);
  end
  else
    Result := false;
end;

function LeftStr(const AText: string; const ACount: Integer): string;
begin
  Result := Copy(AText, 1, ACount);
end;

function RightStr(const AText: string; const ACount: Integer): string;
begin
  Result := Copy(AText, Length(AText) + 1 - ACount, ACount);
end;

function MidStr(const AText: string; const AStart, ACount: Integer): string;
begin
  Result := Copy(AText, AStart, ACount);
end;

function RemoveSpaces(const AText: string): string;
var
  i: integer;
begin
  Result := '';
  for i := 1 to Length(AText) do
    if AText[i] <> ' ' then Result := Result + AText[i];
end;

resourceString
  rsDLLExt = 'DLL';
  rsFmtDLLLINKDESCRIPTION = '->%s::%s'; {eg '->C:\DXPLUGIN1.DLL::BMP_1' }
  rsFmtDLLLINKDESCRIPTION1 = '->';
  rsFmtDLLLINKDESCRIPTION2 = '::';

function GetDllLinkDescription(const DllFileName: string; Entry: string): string;
var sDll, sEntry: string;
begin
  if GetDllLinkInfo(Entry, sDll, sEntry) then // Το Entry is already formated
    Result := Format(rsFmtDLLLINKDESCRIPTION, [DllFileName, sEntry])
  else
    Result := Format(rsFmtDLLLINKDESCRIPTION, [DllFileName, Entry]);
end;

function IsDLLLinkInfo(const inf: string): boolean;
var
  DLLFileName: string;
  Entry: string;
begin
  Result := GetDLLLinkInfo(inf, DLLFileName, Entry);
  if Result then
    Result := UpperCase(RightStr(DLLFileName, 3)) = rsDLLExt;
end;

function GetDLLLinkInfo(const inf: string; var DLLFileName: string; var Entry: string): boolean;
var
  sEntry: string;
  i, j: integer;
begin
  Result := false;
  if Length(inf) >= length(rsFmtDLLLINKDESCRIPTION1) + length(rsFmtDLLLINKDESCRIPTION2) + 2 then
  begin
    if inf[1] + inf[2] = rsFmtDLLLINKDESCRIPTION1 then
    begin
      sEntry := '';
      i := Length(inf);
      while (i > length(rsFmtDLLLINKDESCRIPTION1)) and
           (inf[i] + inf[i-1] <> rsFmtDLLLINKDESCRIPTION2[length(rsFmtDLLLINKDESCRIPTION2)-1] + rsFmtDLLLINKDESCRIPTION2[length(rsFmtDLLLINKDESCRIPTION2)]) do
      begin
        sEntry := inf[i] + sEntry;
        dec(i);
      end;
      if i > length(rsFmtDLLLINKDESCRIPTION1) then
      begin
        DllFileName := '';
        for j := length(rsFmtDLLLINKDESCRIPTION1) + 1 to i - length(rsFmtDLLLINKDESCRIPTION2) do DllFileName := DllFileName + inf[j];
        Entry := sEntry;
        Result := true;
      end;
    end;
  end;
end;

function SizeOfFile(const FileName: TFileName): integer;
begin
  Result := 0;
  if FileExists(FileName) and (FileName <> '') then
  begin
    with TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone) do
    try
      Result := Size;
    finally
      Free;
    end;
  end;
end;

{*************************************************************************

 * FUNCTION:   I_PtInPolygon
 *
 * PURPOSE
 * This routine determines if the point passed is in the polygon. It uses

 * the classical polygon hit-testing algorithm: a horizontal ray starting

 * at the point is extended infinitely rightwards and the number of
 * polygon edges that intersect the ray are counted. If the number is odd,

 * the point is inside the polygon.
 *
 * RETURN VALUE
 * (BOOL) TRUE if the point is inside the polygon, FALSE if not.
 *************************************************************************}

function I_PtInPolygon(const rgpts: array of TPoint; wnumpts: word;
  ptTest: TPoint; prbound: PRect = nil): boolean;
var
  r: TRect;
  i: integer;
  pt1, pt2: TPoint;
  wnumintsct: word;
begin
  Result := false;
  if I_PtInPolyRect(rgpts, wnumpts, ptTest, prbound) then
  begin
    wnumintsct := 0;

    pt1 := ptTest;
    pt2 := ptTest;
    pt2.x := r.right + 500;

  // Now go through each of the lines in the polygon and see if it
  // intersects
    for i := 0 to wnumpts - 2 do
      if I_Intersect(ptTest, pt2, rgpts[i], rgpts[i + 1]) then
        inc(wnumintsct);

  // And the last line
    if I_Intersect(ptTest, pt2, rgpts[wnumpts - 1], rgpts[0]) then
      inc(wnumintsct);

    Result := odd(wnumintsct);
  end;
end;

{*************************************************************************

 * FUNCTION:   I_PtInPolyRect
 *
 * PURPOSE
 * This routine determines if a point is within the smallest rectangle
 * that encloses a polygon.
 *
 * RETURN VALUE
 * (BOOL) TRUE or FALSE depending on whether the point is in the rect or

 * not.
 *************************************************************************}

function I_PtInPolyRect(const rgpts: array of TPoint; wnumpts: word;
  ptTest: TPoint; prbound: PRect = nil): boolean;
var
  r: TRect;
  xmin, xmax,
  ymin, ymax: integer;
  i: integer;
begin
  // If a bounding rect has not been passed in, calculate it
  if prbound <> nil then
    r := prbound^
  else
  begin
    xmin := MAXINT;
    ymin := MAXINT;
    xmax := low(integer);
    ymax := low(integer);

    for i := 0 to wnumpts - 1 do
    begin
      if rgpts[i].x < xmin then xmin := rgpts[i].x;
      if rgpts[i].x > xmax then xmax := rgpts[i].x;
      if rgpts[i].y < ymin then ymin := rgpts[i].y;
      if rgpts[i].y > ymax then ymax := rgpts[i].y;
    end;
    SetRect(r, xmin, ymin, xmax, ymax);
  end;
  Result :=  PtInRect(r,ptTest);
end;

{*************************************************************************

 * FUNCTION:   Intersect
 *
 * PURPOSE
 * Given two line segments, determine if they intersect.
 *
 * RETURN VALUE
 * TRUE if they intersect, FALSE if not.
 *************************************************************************}

function I_Intersect(const p1, p2, p3, p4: TPoint): boolean;
begin
  Result := (((I_CCW(p1, p2, p3) * I_CCW(p1, p2, p4)) <= 0) and
             ((I_CCW(p3, p4, p1) * I_CCW(p3, p4, p2)  <= 0)));
end;

{*************************************************************************

 * FUNCTION:   CCW (CounterClockWise)
 *
 * PURPOSE
 * Determines, given three points, if when travelling from the first to
 * the second to the third, we travel in a counterclockwise direction.
 *
 * RETURN VALUE
 * (int) 1 if the movement is in a counterclockwise direction, -1 if
 * not.
 *************************************************************************}

function I_CCW(const p0, p1, p2: TPoint): integer;
var
  dx1, dx2: integer;
  dy1, dy2: integer;
begin
  dx1 := p1.x - p0.x; dx2 := p2.x - p0.x;
  dy1 := p1.y - p0.y; dy2 := p2.y - p0.y;

{ This is basically a slope comparison: we don't do divisions because of
  divide by zero possibilities with pure horizontal and pure vertical lines. }
  if dx1 * dy2 > dy1 * dx2 then
    Result := 1
  else
    Result := -1;
end;

// *******************************
// Polygon hit test, float version
// *******************************
function F_PtInPolygon(const rgpts: array of TFloatPoint; wnumpts: word;
  ptTest: TFloatPoint; prbound: PFloatRect = nil): boolean;
var
  r: TFloatRect;
  i: integer;
  pt1, pt2: TFloatPoint;
  wnumintsct: word;
begin
  Result := false;
  if F_PtInPolyRect(rgpts, wnumpts, ptTest, prbound) then
  begin
    wnumintsct := 0;

    pt1 := ptTest;
    pt2 := ptTest;
    pt2.x := r.right + 500;

  // Now go through each of the lines in the polygon and see if it
  // intersects
    for i := 0 to wnumpts - 2 do
      if F_Intersect(ptTest, pt2, rgpts[i], rgpts[i + 1]) then
        inc(wnumintsct);

  // And the last line
    if F_Intersect(ptTest, pt2, rgpts[wnumpts - 1], rgpts[0]) then
      inc(wnumintsct);

    Result := odd(wnumintsct);
  end;
end;

function F_PtInPolyRect(const rgpts: array of TFloatPoint; wnumpts: word;
  ptTest: TFloatPoint; prbound: PFloatRect = nil): boolean;
var
  r: TFloatRect;
  xmin, xmax,
  ymin, ymax: TD3DValue;
  i: integer;
begin
  // If a bounding rect has not been passed in, calculate it
  if prbound <> nil then
    r := prbound^
  else
  begin
    xmin := MAXINT;
    ymin := MAXINT;
    xmax := low(integer);
    ymax := low(integer);

    for i := 0 to wnumpts - 1 do
    begin
      if rgpts[i].x < xmin then xmin := rgpts[i].x;
      if rgpts[i].x > xmax then xmax := rgpts[i].x;
      if rgpts[i].y < ymin then ymin := rgpts[i].y;
      if rgpts[i].y > ymax then ymax := rgpts[i].y;
    end;
    r.Left := xmin;
    r.Right := xmax;
    r.Top := ymin;
    r.Bottom := ymax;
  end;
  Result := (ptTest.x >= r.Left) and (ptTest.x <= r.Right) and
            (ptTest.y >= r.Top) and (ptTest.x <= r.Bottom);
end;

function F_Intersect(const p1, p2, p3, p4: TFloatPoint): boolean;
begin
  Result := (((F_CCW(p1, p2, p3) * F_CCW(p1, p2, p4)) <= 0) and
             ((F_CCW(p3, p4, p1) * F_CCW(p3, p4, p2)  <= 0)));
end;

function F_CCW(const p0, p1, p2: TFloatPoint): integer;
var
  dx1, dx2: TD3DValue;
  dy1, dy2: TD3DValue;
begin
  dx1 := p1.x - p0.x; dx2 := p2.x - p0.x;
  dy1 := p1.y - p0.y; dy2 := p2.y - p0.y;

{ This is basically a slope comparison: we don't do divisions because of
  divide by zero possibilities with pure horizontal and pure vertical lines. }
  if dx1 * dy2 > dy1 * dx2 then
    Result := 1
  else
    Result := -1;
end;

function IsPointEqual(const p1, p2: TPoint): boolean;
begin
  Result := (p1.x = p2.x) and (p1.y = p2.y);
end;

function IsFloatPointEqual(const p1, p2: TFloatPoint): boolean;
begin
  Result := (p1.x = p2.x) and (p1.y = p2.y);
end;

function FloatPointDotProduct(const v1, v2: TFloatPoint): TD3DValue;
begin
  Result := (v1.x*v2.x) + (v1.y * v2.y);
end;

function TrimStr(const s: string): string;
var
  tmp: string;
  i: integer;
begin
  tmp := Trim(s);
  Result := '';
  for i := 1 to length(tmp) do
  begin
    if tmp[i] = #0 then exit;
    Result := Result + tmp[i];
  end;
end;

procedure TrimStr(s: TStringList);
var
  i: integer;
begin
  s.Text := TrimStr(s.Text);
  for i := 0 to s.Count - 1 do
    s.Strings[i] := TrimStr(s.Strings[i]);
end;

procedure RemoveTrailingLines(s: TStringList);
var
  i: integer;
begin
  for i := s.Count - 1 downto 0 do
    if TrimStr(s.Strings[i]) = '' then
      s.Delete(i)
    else
      Exit;
end;

procedure RemoveTrailingLines(var str: string);
var
  s: TStringList;
begin
  s := TStringList.Create;
  try
    s.Text := str;
    RemoveTrailingLines(s);
    str := s.Text;
  finally
    s.Free;
  end;
end;

// *******************************
// Triangle hit test, float version
// *******************************
function GetAngle(const v1, v2: TFloatPoint): TD3DValue; overload;
begin
  if v1.x = v2.x then
  begin
    if v2.y > v1.y then
      Result := g_PI_DIV_2
    else if v2.y < v1.y then
      Result := -g_PI_DIV_2
    else
      Result := 0.0;
  end
  else
  begin
    Result := ArcTan2S(v2.y - v1.y, v2.x - v1.x);
    while Result < g_NEG_PI do
      Result := Result + g_2_PI;
    while Result > g_PI do
      Result := Result - g_2_PI;
  end;
end;

function GetAngle(const v1, v2, v3: TFloatPoint): TD3DValue; overload;
begin
  Result := GetAngle(v2, v3) - GetAngle(v2, v1);
  while Result < g_NEG_PI do
    Result := Result + g_2_PI;
  while Result > g_PI do
    Result := Result - g_2_PI;
end;

function GetAngle(const v1, v2, v3: TD3DLVertex): TD3DValue; overload;
begin
  Result := GetAngle(
    MakeFloatPoint(v1),
    MakeFloatPoint(v2),
    MakeFloatPoint(v3));
end;

{ Επιστρέφει true αν το v είναι στην γωνία με πλευρά v1, v2 και τιμή angle }
function IsLVertexInAngle(const v, v1, v2: TD3DLVertex; const angle: single): boolean;
const
  TOLERANCE = 0.001;
var
  ang: single;
begin
  ang := GetAngle(v1, v2, v);
  Result := (ang + TOLERANCE < angle) and (ang > TOLERANCE);
end;

{ Επιστρέφει true αν το v είναι στην γωνία που ορίζουν τα v1, v2, v3 }
function IsFloatPointInAngle(const v, v1, v2, v3: TFloatPoint): boolean;
const
  DEFTOLERANCE = 0.0001;
var
  ang1, ang2: Extended;
begin
  ang1 := GetAngle(v1, v2, v);
  ang2 := GetAngle(v1, v2, v3);
  Result := (ang1 + DEFTOLERANCE < ang2) and (ang1 > DEFTOLERANCE);
end;

function MakeFloatPoint(const x, y: TD3DValue): TFloatPoint;
begin
  Result.x := x;
  Result.y := y;
end;

function MakeFloatPoint(const v: TD3DVector): TFloatPoint;
begin
  Result.x := v.x;
  Result.y := v.z;
end;

function MakeFloatPoint(const v: TD3DLVertex): TFloatPoint; overload;
begin
  Result.x := v.x;
  Result.y := v.z;
end;

function GetProjVertexSqrDistance(const v1, v2: TD3DLVertex): TD3DValue;
var
  dx, dz: TD3DValue;
begin
  dx := (v1.x - v2.x);
  dz := (v1.z - v2.z);
  Result := dx * dx + dz * dz;
end;

function F_PtNearTriangle(const tr: TFloatTriangle;
  const ptTest: TFloatPoint; const Tolerance: TD3DValue): boolean;
var
  x, y: TD3DValue;
begin
  x := ptTest.x + Tolerance;
  if (x < tr[0].x) and (x < tr[1].x) and (x < tr[2].x) then
  begin
    Result := false;
    exit;
  end;
  x := ptTest.x - Tolerance;
  if (x > tr[0].x) and (x > tr[1].x) and (x > tr[2].x) then
  begin
    Result := false;
    exit;
  end;

  y := ptTest.y + Tolerance;
  if (y < tr[0].y) and (y < tr[1].y) and (y < tr[2].y) then
  begin
    Result := false;
    exit;
  end;
  y := ptTest.y - Tolerance;
  if (y > tr[0].y) and (y > tr[1].y) and (y > tr[2].y) then
  begin
    Result := false;
    exit;
  end;

  Result := true;
end;

function F_PtInTriangle(const tr: TD3DFloatTriangle;
  const ptTest: TFloatPoint): boolean;
var
  v: TD3DVector;
begin
  v.x := ptTest.x;
  v.y := 0.0;
  v.z := ptTest.y;
  Result := F_PtInTriangle(tr, v);
end;

function F_PtInTriangle(const tr: TFloatTriangle;
  const ptTest: TFloatPoint): boolean;
const
  TOLERANCE = 3.1415926; {= pi}
var
  a, a0, a1, a2: TD3DValue;
// Εξετάζει αν το σημείο ptTest ανήκει στο τρίγωνο tr
begin
  a0 := GetAngle(ptTest, tr[0]); // Γωνία που σχηματίζει το σημείο ελέγχου με το 1ο σημείο του τριγώνου
  a1 := GetAngle(ptTest, tr[1]); // Γωνία που σχηματίζει το σημείο ελέγχου με το 2ο σημείο του τριγώνου
  a2 := GetAngle(ptTest, tr[2]); // Γωνία που σχηματίζει το σημείο ελέγχου με το 3ο σημείο του τριγώνου
// Ταξινομούμε τις γωνίες σε αύξουσα σειρά (βηματική bubblesort σε πίνακα 3 στοιχείων)
  if a2 < a1 then
  begin
    a := a2;
    a2 := a1;
    a1 := a;
  end;
  if a1 < a0 then
  begin
    a := a1;
    a1 := a0;
    a0 := a;
  end;
  if a2 < a1 then
  begin
    a := a2;
    a2 := a1;
    a1 := a;
  end;
// Για να είναι το σημείο μέσα στο τρίγωνο πρέπει να ισχύει:
  Result := ({abs}(a1 - a0) < TOLERANCE) and       { Γωνίες <= 180 μοιρών }
            ({abs}(a2 - a1) < TOLERANCE) and
            ({abs}(a0 - a2 + g_2_PI) < TOLERANCE); { ή (a0 + 2*pi) - a2, προσθέτουμε 2 * pi γιατί a2 > a0 }
end;

function GetHorizontalProjectionAngle(const v1, v2: TD3DVector): TD3DValue;
// Επιστρέφει την γωνία οριζόντιας προβολής των διανυσμάτων v1, v2
begin
  if v1.x = v2.x then
  begin
    if v2.z > v1.z then
      Result := g_PI_DIV_2
    else if v2.z < v1.z then
      Result := -g_PI_DIV_2
    else
      Result := 0.0;
  end
  else
  begin
    Result := ArcTan2S(v2.z - v1.z, v2.x - v1.x);
    while Result < -g_PI do Result := Result + g_2_PI;
    while Result > g_PI do Result := Result - g_2_PI;
  end;
end;

function F_PtInTriangle(const tr: TD3DFloatTriangle;
  const ptTest: TD3DVector): boolean;
const
  TOLERANCE = 3.1415926; {= pi}
var
  a, a0, a1, a2: TD3DValue;
// Εξετάζει αν η προβολή στο επίπεδο x, z του σημείου ptTest ανήκει στο τρίγωνο tr
begin
// Γωνία που σχηματίζει το σημείο ελέγχου με το 1ο σημείο του τριγώνου
  a0 := GetHorizontalProjectionAngle(ptTest, tr[0]);
// Γωνία που σχηματίζει το σημείο ελέγχου με το 2ο σημείο του τριγώνου
  a1 := GetHorizontalProjectionAngle(ptTest, tr[1]);
// Γωνία που σχηματίζει το σημείο ελέγχου με το 3ο σημείο του τριγώνου
  a2 := GetHorizontalProjectionAngle(ptTest, tr[2]);
// Ταξινομούμε τις γωνίες σε αύξουσα σειρά (βηματική bubblesort σε πίνακα 3 στοιχείων)
  if a2 < a1 then
  begin
    a := a2;
    a2 := a1;
    a1 := a;
  end;
  if a1 < a0 then
  begin
    a := a1;
    a1 := a0;
    a0 := a;
  end;
  if a2 < a1 then
  begin
    a := a2;
    a2 := a1;
    a1 := a;
  end;
// Για να είναι το σημείο μέσα στο τρίγωνο πρέπει να ισχύει:
  Result := (abs(a1 - a0) < TOLERANCE) and       { Γωνίες <= 180 μοιρών }
            (abs(a2 - a1) < TOLERANCE) and
            (abs(a0 - a2 + g_2_PI) < TOLERANCE); { ή (a0 + 2*pi) - a2, προσθέτουμε 2 * pi γιατί a2 > a0 }
end;

function GetTriangleProjection(const tr: TD3DFloatTriangle;
  const ptTest: TFloatPoint): TD3DValue;
var
  v: TD3DVector;
begin
  v.x := ptTest.x;
  v.y := 0.0;
  v.z := ptTest.y;
  Result := GetTriangleProjection(tr, v);
end;

function GetTriangleProjection(const tr: TD3DFloatTriangle;
  const ptTest: TD3DVector): TD3DValue;
const
  MAXLOOPS = 8;
  MAXDEPTH = 8;
// Επιστρέφει την προβολή του σημείου ptTest στο επίπεδο x, z πάνω στο τρίγωνο tr
var
  currValue, lastValue, step, check: TD3DValue;
  index, depth: integer;
  V: TD3DVector;

  function GetAnalizeValue(const input: TD3DValue): TD3DValue;
  // Εκτιμήτρια συνάρτηση, επιστροφή του αρθοίσματος των γωνιών του
  // σημείου input. Αν επιστρέψει 0 τότε είμαστε πάνω στο τρίγωνο
  begin
    V := MakeD3DVector(ptTest.x, input, ptTest.z);
    Result := abs(
        ArcCos(VectorAngle( // Calculate cosine of angle
          VectorSub(V, tr[0]),
          VectorSub(tr[1], V))) +
        ArcCos(VectorAngle(
          VectorSub(V, tr[1]),
          VectorSub(tr[2], V))) +
        ArcCos(VectorAngle(
          VectorSub(V, tr[2]),
          VectorSub(tr[0], V))) - g_2_PI);
{      Result := abs(
        VectorAngle(
          VectorSub(tr[0], V),
          VectorSub(V, tr[1])) +
        VectorAngle(
          VectorSub(tr[1], V),
          VectorSub(V, tr[2])) +
        VectorAngle(
          VectorSub(tr[2], V),
          VectorSub(V, tr[0])) - g_2_PI);}

{
      Result := abs(
        VectorAngle(
          VectorSub(tr[0], V),
          VectorSub(V, tr[1])) +
        VectorAngle(
          VectorSub(tr[1], V),
          VectorSub(MakeD3DVector(ptTest.x, input, ptTest.z), tr[2])) +
        VectorAngle(
          VectorSub(tr[2], V),
          VectorSub(V, tr[0])) - g_2_PI);
}
  end;

{    function AnalizeLoop(const maxv, minv: TD3DValue): TD3DValue;
    // Αναδρομική μέθοδος αριθμητικής ανάλυσης υπολογισμού της θέσης
    // Ελέγχει από το σημείο maxv έως το σημείο minv
    var i: integer;
    begin
      step := (maxv - minv);
      // Αν η διαφορά είναι πολύ μικρή τότε πέρνουμε τον μέσο όρο των δύο ορίων
      if abs(step) < g_EPSILON then
        Result := Min(minv, maxv) + step / 2
      else
      begin
      // step: βήμα επόμενου δείγματος
        step := step / MAXLOOPS;
        Result := minv;
        index := 0;
        lastValue := GetAnalizeValue(Result);
      // Βρίσκουμε το σημείο στο οποίο η εκτιμήτρια επιστρέφει την μικρότερη τιμή
        for i := 1 to MAXLOOPS do
        begin
          check := minv + i * step;
          currValue := GetAnalizeValue(check);
          if lastValue > currValue then
          begin
            lastValue := currValue;
            Result := check;
            index := i;
          end;
        end;
      // Αν δεν είμαστε σε τόσο μεγάλο βάθος αναδρομής...
        if depth < MAXDEPTH then
        begin
      // Αυξάνουμε το βάθος αναζήτησης
          inc(depth);
      // Αν είμασταν στο πρώτο σημείο βάζουμε όρια minv, minv + step
          if index = 0 then
            Result := AnalizeLoop(minv, minv + step)
      // Αν είμασταν στο τελευταίο σημείο βάζουμε όρια maxv - step, maxv
          else if index = MAXLOOPS then
            Result := AnalizeLoop(maxv - step, maxv)
          else
            Result := AnalizeLoop(Result - step, Result + step);
        end;
      end;
    end;}

    function AnalizeLoop(const minv, maxv: TD3DValue): TD3DValue;
    // Αναδρομική μέθοδος αριθμητικής ανάλυσης υπολογισμού της θέσης
    // Ελέγχει από το σημείο maxv έως το σημείο minv
    var i: integer;
    begin
      step := (maxv - minv);
      // Αν η διαφορά είναι πολύ μικρή τότε πέρνουμε τον μέσο όρο των δύο ορίων
      if step < g_EPSILON then
        Result := minv + step / 2
      else
      begin
      // step: βήμα επόμενου δείγματος
        step := step / MAXLOOPS;
        Result := minv;
        index := 0;
        lastValue := GetAnalizeValue(Result);
      // Βρίσκουμε το σημείο στο οποίο η εκτιμήτρια επιστρέφει την μικρότερη τιμή
        for i := 1 to MAXLOOPS do
        begin
          check := minv + i * step;
          currValue := GetAnalizeValue(check);
          if lastValue < currValue then
          begin
            lastValue := currValue;
            Result := check;
            index := i;
          end;
        end;
      // Αν δεν είμαστε σε τόσο μεγάλο βάθος αναδρομής...
        if depth < MAXDEPTH then
        begin
      // Αυξάνουμε το βάθος αναζήτησης
          inc(depth);
      // Αν είμασταν στο πρώτο σημείο βάζουμε όρια minv, minv + step
          if index = 0 then
            Result := AnalizeLoop(minv, minv + step)
      // Αν είμασταν στο τελευταίο σημείο βάζουμε όρια maxv - step, maxv
          else if index = MAXLOOPS then
            Result := AnalizeLoop(maxv - step, maxv)
          else
            Result := AnalizeLoop(Result - step, Result + step);
        end;
      end;
    end;

begin
  if sqrt(sqr(ptTest.x - tr[0].x) + sqr(ptTest.z - tr[0].z)) < g_EPSILON then
    Result := tr[0].y
  else if sqrt(sqr(ptTest.x - tr[1].x) + sqr(ptTest.z - tr[1].z)) < g_EPSILON then
    Result := tr[1].y
  else if sqrt(sqr(ptTest.x - tr[2].x) + sqr(ptTest.z - tr[2].z)) < g_EPSILON then
    Result := tr[2].y
  else
  begin
    depth := 0;
    Result := AnalizeLoop(
      Min(tr[0].y, Min(tr[1].y, tr[2].y)),
      Max(tr[0].y, Max(tr[1].y, tr[2].y)));
  end;
end;

function GetIntegerInRange(Value: integer; const lowV, hiV: integer): integer;
begin
  if lowV < hiV then
  begin
    if Value < lowV then
      Result := lowV
    else if Value > hiV then
      Result := hiV
    else
      Result := Value;
  end
  else
  begin
    if Value < hiV then
      Result := hiV
    else if Value > lowV then
      Result := lowV
    else
      Result := Value;
  end
end;

function GetValueInRange(Value: TD3DValue; const lowV, hiV: TD3DValue): TD3DValue;
begin
  if lowV < hiV then
  begin
    if Value < lowV then
      Result := lowV
    else if Value > hiV then
      Result := hiV
    else
      Result := Value;
  end
  else
  begin
    if Value < hiV then
      Result := hiV
    else if Value > lowV then
      Result := lowV
    else
      Result := Value;
  end
end;

function IsFloatInRange(query: TD3DValue; const lowV, hiV: TD3DValue): boolean;
begin
  Result := (query >= lowV) and (query <= hiV);
end;

function IsIntegerInRange(const query: integer; const lowV, hiV: integer): boolean;
begin
  Result := (query >= lowV) and (query <= hiV);
end;

function VectorInCube(const v: TD3DVector; const cb: TBoundingCube): boolean;
begin
  Result :=
    (v.x <= cb.maxX) and (v.x >= cb.minX) and
    (v.y <= cb.maxY) and (v.y >= cb.minY) and
    (v.z <= cb.maxZ) and (v.z >= cb.minZ);
end;

function ProjectionInCube(const v: TD3DVector; const cb: TBoundingCube): boolean;
begin
  Result :=
    (v.x <= cb.maxX) and (v.x >= cb.minX) and
    (v.z <= cb.maxZ) and (v.z >= cb.minZ);
end;

function ProjectionInCube(const v: TFloatPoint; const cb: TBoundingCube): boolean;
begin
  Result :=
    (v.x <= cb.maxX) and (v.x >= cb.minX) and
    (v.y <= cb.maxZ) and (v.y >= cb.minZ);
end;

function VectorInRect(const v: TD3DVector; const r: TFloatRect): boolean;
// True αν η προβολή του v είναι μέσα στο τετράγωνο r
begin
  Result :=
    (v.x <= r.Right) and (v.x >= r.left) and
    (v.z <= r.Top) and (v.z >= r.bottom);
end;

function FloatPointInRect(const p: TFloatPoint; const r: TFloatRect): boolean;
begin
  Result :=
    (p.x <= r.Right) and (p.x >= r.left) and
    (p.y <= r.Top) and (p.y >= r.bottom);
end;

procedure MakeFloatRect(var r: TFloatRect; const v1, v2: TD3DVector);
begin
  r.Left := Min(v1.x, v2.x);
  r.Right := Max(v1.x, v2.x);
  r.Top := Min(v1.z, v2.z);
  r.Bottom := Max(v1.z, v2.z);
end;

procedure MakeFloatRect(var r: TFloatRect; const v1, v2: TFloatPoint);
begin
  r.Left := Min(v1.x, v2.x);
  r.Right := Max(v1.x, v2.x);
  r.Top := Min(v1.y, v2.y);
  r.Bottom := Max(v1.y, v2.y);
end;

function MakeFloatRect(const v1, v2: TD3DVector): TFloatRect;
begin
  MakeFloatRect(Result, v1, v2);
end;

function MakeFloatRect(const v1, v2: TFloatPoint): TFloatRect;
begin
  MakeFloatRect(Result, v1, v2);
end;

function MakeFloatRect(left, top, right, bottom: TD3DValue): TFloatRect;
begin
  Result.Left := left;
  Result.top := top;
  Result.right := right;
  Result.bottom := bottom;
end;

function IntersectFloatRect(const r1, r2: TFloatRect): boolean;
const FACTOR = 10000;
var r, r1i, r2i: TRect;
begin
  SetRect(r1i, round(FACTOR * r1.left), round(FACTOR * r1.top), round(FACTOR * r1.right), round(FACTOR * r1.Bottom));
  SetRect(r2i, round(FACTOR * r2.left), round(FACTOR * r2.top), round(FACTOR * r2.right), round(FACTOR * r2.Bottom));
  IntersectRect(r, r1i, r2i);
  Result := not IsRectEmpty(r);
end;

function CombineBoundingCubes(BoundingCube1, BoundingCube2: TBoundingCube): TBoundingCube;
begin
  if BoundingCube1.minX < BoundingCube2.minX then
    Result.minX := BoundingCube1.minX
  else
    Result.minX := BoundingCube2.minX;

  if BoundingCube1.minY < BoundingCube2.minY then
    Result.minY := BoundingCube1.minY
  else
    Result.minY := BoundingCube2.minY;

  if BoundingCube1.minZ < BoundingCube2.minZ then
    Result.minZ := BoundingCube1.minZ
  else
    Result.minZ := BoundingCube2.minZ;

  if BoundingCube1.maxX > BoundingCube2.maxX then
    Result.maxX := BoundingCube1.maxX
  else
    Result.maxX := BoundingCube2.maxX;

  if BoundingCube1.maxY > BoundingCube2.maxY then
    Result.maxY := BoundingCube1.maxY
  else
    Result.maxY := BoundingCube2.maxY;

  if BoundingCube1.maxZ > BoundingCube2.maxZ then
    Result.maxZ := BoundingCube1.maxZ
  else
    Result.maxZ := BoundingCube2.maxZ;
end;

procedure MakeEmptyBoundingCube(var BoundingCube: TBoundingCube);
begin
// Οι μέγιστες τιμές τίνουν στο μείο άπειρο & οι ελάχιστες στο + άπειρο.
// Έτσι ο έλεγχος για VectorInCube αποτυγχάνει πάντα.
  BoundingCube.minX := g_HUGE;
  BoundingCube.maxX := g_MINHUGE;
  BoundingCube.minY := g_HUGE;
  BoundingCube.maxY := g_MINHUGE;
  BoundingCube.minZ := g_HUGE;
  BoundingCube.maxZ := g_MINHUGE;
end;

function MakeSlideIntersection(var newPos: TD3DVector; const oldPos: TD3DVector;
  const step1, step2: TFloatPoint; const distance: TD3DValue): boolean;
{ Ελέγχει αν η γραμμή που ορίζεται από τα step1, step2 δεν εμποδίζει την
  κίνηση από το σημείο oldPos στο σημείο newPos (Έλεγχος πάνω στο επίπεδο (x,z)
  Επιτρέφει true αν κάνει αλλαγές στη νέα θέση newPos
  Η αλλαγή που κάνει είναι ολίσθηση σε περίπτωσαη collision}
var _newPos,             // Νέα θέση προορισμού που επιτρέπει να μετακινηθούμε η γραμμή
    _oldPos: TFloatPoint;// Παλαιά θέση
    angle: TD3DValue;    // Γωνία
    sina, cosa: TD3DValue;
    tmp: TD3DValue;
    insX: TD3DValue; // True intersection point (πάνω στην γραμμή)
begin
  if VectorEquel(newPos, oldPos) then
  begin
    Result := false;
    exit;
  end;
  _newPos.x := newPos.x - step1.x;
  _newPos.y := newPos.z - step1.y;
  _oldPos.x := oldPos.x - step1.x;
  _oldPos.y := oldPos.z - step1.y;

  angle := GetAngle(step1, step2);
  SinCosS(angle, sina, cosa);

  tmp := _newPos.x * cosa + _newPos.y * sina;
  _newPos.y := _newPos.y * cosa - _newPos.x * sina;
  _newPos.x := tmp;

  tmp := _oldPos.x * cosa + _oldPos.y * sina;
  _oldPos.y := _oldPos.y * cosa - _oldPos.x * sina;
  _oldPos.x := tmp;

// Απόσταση που διανύεται
  tmp := sqrt(sqr(step1.x - step2.x) + sqr(step1.y - step2.y));

  if (_newPos.x < 0) and (_oldPos.x < 0) then
    Result := false
  else if (_newPos.x > tmp) and (_oldPos.x > tmp) then
    Result := false
  else if (_newPos.y * _oldPos.y < 0) or ((abs(_newPos.y) <= distance + g_EPSILON)) then
  begin
    if abs(_newPos.y - _oldPos.y) > g_EPSILON then
    begin
      insX := _oldPos.x - _oldPos.y * (_newPos.x - _oldPos.x) / (_newPos.y - _oldPos.y);  // Σημείο τομής στην ευθεία
      if not IsFloatInRange(abs(insX), - distance, tmp + distance) then
      begin
        Result := false;
        exit;
      end;
    end;

    if _oldPos.y < 0 then
      _newPos.y := - distance - g_EPSILON
    else
      _newPos.y := distance + g_EPSILON;
//    Result := abs(_newPos.x - GetValueInRange(_newPos.x, distance, sqrt(sqr(step1.x - step2.x) + sqr(step1.y - step2.y)) - distance)) < g_EPSILON;

    Result := true;

    tmp := _newPos.x * cosa - _newPos.y * sina;
    _newPos.y := _newPos.y * cosa + _newPos.x * sina;
    _newPos.x := tmp;

    newPos.x := _newPos.x + step1.x;
    newPos.z := _newPos.y + step1.y;
  end
  else
    Result := false;
end;

function MakeSlideRoundIntersection(var newPos: TD3DVector; const oldPos: TD3DVector;
  const Center: TD3DVector; const radius: TD3DValue; const distance: TD3DValue): boolean;
const ITERATIONS = 16;
var _newPos, _oldPos, _Center: TFloatPoint;
    angle: TD3DValue;
    sina, cosa: TD3DValue;
    sini, cosi: TD3DValue;
    tmp: TD3DValue;
    _newDist, _oldDist: TD3DValue;
    _radius: TD3DValue;
    i, iPos, iPos2: integer;
    minAnalyze: TD3DValue;
    pV: TFloatPoint;
begin
  Result := false;

  _radius := radius + distance;
  if (_radius < 0.0) then
  begin
    raise Exception.Create('MakeSlideRoundIntersection(): Invalid Parameters');
    exit;
  end;

  if _radius <= g_EPSILON then
    exit;
  if VectorEquel(newPos, oldPos) then
    exit;

  _newPos.x := 0.0;
  _newPos.y := 0.0;
  _oldPos.x := oldPos.x - newPos.x;
  _oldPos.y := oldPos.z - newPos.z;
  _Center.x := Center.x - newPos.x;
  _Center.y := Center.z - newPos.z;

  angle := GetAngle(_newPos, _oldPos);

  SinCosS(angle, sina, cosa);

  tmp := _oldPos.x * cosa + _oldPos.y * sina;
  _oldPos.y := _oldPos.y * cosa - _oldPos.x * sina;
  _oldPos.x := tmp;

  tmp := _Center.x * cosa + _Center.y * sina;
  _Center.y := _Center.y * cosa - _Center.x * sina;
  _Center.x := tmp;

  _newDist := sqrt(sqr(_Center.x - _newPos.x) + sqr(_Center.y - _newPos.y));
  _oldDist := sqrt(sqr(_Center.x - _oldPos.x) + sqr(_Center.y - _oldPos.y));

   if (_newDist > _radius) and
      (_oldDist > _newDist) and
      ((_Center.x < 0.0) or (_Center.x > _oldPos.x)) then
     exit;

  if abs(_Center.y) > _radius then
    exit;

  if abs(_Center.y) > _radius - g_EPSILON then
  begin
    _newPos.x := _Center.x + g_EPSILON;
    if _Center.y < 0.0 then
      _newPos.y := g_EPSILON
    else if _Center.y > 0.0 then
      _newPos.y := -g_EPSILON
    else
      _newPos.y := 0.0;
  end
  else
  begin
  // First Analize loop
    minAnalyze := MAXSINGLE;
    iPos := 0;
    for i := 0 to ITERATIONS - 1 do
    begin
      SinCosS((i * g_2_PI) / ITERATIONS, sini, cosi);
      pV := MakeFloatPoint(cosi + sini - _oldPos.x, cosi - sini - _oldPos.y);
      tmp := abs(
        FloatPointDotProduct(
          MakeFloatPoint(cosi + sini, cosi - sini), pV));
      if tmp < minAnalyze then
      begin
        if Sign(_Center.y) = Sign((cosi - sini) * radius + _Center.y) then
        begin
          iPos := i;
          minAnalyze := tmp;
        end;
      end;
    end;

  // Second Analize loop
    iPos2 := ITERATIONS div 2;
    for i := 0 to ITERATIONS - 1 do
    begin
      SinCosS(((iPos + (ITERATIONS / 2 - i) / ITERATIONS ) * g_2_PI) / ITERATIONS, sini, cosi);
      pV := MakeFloatPoint(cosi + sini - _oldPos.x, cosi - sini - _oldPos.y);
      tmp := abs(
        FloatPointDotProduct(
          MakeFloatPoint(cosi + sini, cosi - sini), pV));
      if tmp < minAnalyze then
      begin
        if Sign(_Center.y) = Sign((cosi - sini) * radius + _Center.y) then
        begin
          iPos2 := i;
          minAnalyze := tmp;
        end;
      end;
    end;

    SinCosS(((iPos + (ITERATIONS / 2 - iPos2) / ITERATIONS) * g_2_PI) / ITERATIONS, sini, cosi);

    _newPos.x := _oldPos.x - abs(sini * _oldPos.x);
    _newPos.y := - Sign(_Center.y) * cosi * _oldPos.x;

    _newDist := sqrt(sqr(_Center.x - _newPos.x) + sqr(_Center.y - _newPos.y));
    if _newDist < _radius then
    begin
      angle := GetAngle(_Center, _newPos);
      SinCosS(angle, sini, cosi);

      _newPos.x := _Center.x + _radius * cosi;
      _newPos.y := _Center.y + _radius * sini;
    end;

  end;


  tmp := _newPos.x * cosa - _newPos.y * sina;
  _newPos.y := _newPos.y * cosa + _newPos.x * sina;
  _newPos.x := tmp;

  newPos.x := newPos.x + _newPos.x;
  newPos.z := newPos.z + _newPos.y;
  Result := true;

end;

{
// Working Version, μονό loop αριθμιτικής ανάλυσης
function MakeSlideRoundIntersection(var newPos: TD3DVector; const oldPos: TD3DVector;
  const Center: TD3DVector; const radius: TD3DValue; const distance: TD3DValue): boolean;
var _newPos, _oldPos, _Center: TFloatPoint;
    angle: TD3DValue;
    sina, cosa: TD3DValue;
    sini, cosi: TD3DValue;
    tmp: TD3DValue;
    _newDist, _oldDist: TD3DValue;
    _radius: TD3DValue;
    i, iPos: integer;
    minAnalyze: TD3DValue;
    pV: TFloatPoint;
begin
  Result := false;

  if (distance < 0.0) or (radius < 0.0) then
  begin
    raise Exception.Create('MakeSlideRoundIntersection(): Invalid Parameters');
    exit;
  end;

  _radius := radius + distance;
  if _radius <= g_EPSILON then
    exit;
  if VectorEquel(newPos, oldPos) then
    exit;

  _newPos.x := 0.0;
  _newPos.y := 0.0;
  _oldPos.x := oldPos.x - newPos.x;
  _oldPos.y := oldPos.z - newPos.z;
  _Center.x := Center.x - newPos.x;
  _Center.y := Center.z - newPos.z;

  angle := GetAngle(_newPos, _oldPos);

  SinCosS(angle, sina, cosa);

  tmp := _oldPos.x * cosa + _oldPos.y * sina;
  _oldPos.y := _oldPos.y * cosa - _oldPos.x * sina;
  _oldPos.x := tmp;

  tmp := _Center.x * cosa + _Center.y * sina;
  _Center.y := _Center.y * cosa - _Center.x * sina;
  _Center.x := tmp;

  _newDist := sqrt(sqr(_Center.x - _newPos.x) + sqr(_Center.y - _newPos.y));
  _oldDist := sqrt(sqr(_Center.x - _oldPos.x) + sqr(_Center.y - _oldPos.y));

  if (_newDist > _radius) and
     (_oldDist > _newDist) and
     ((_Center.x < 0) or (_Center.x > _oldPos.x)) then
    exit;

  if abs(_Center.y) > _radius then
    exit;

  if abs(_Center.y) > _radius - g_EPSILON then
  begin
    _newPos.x := _Center.x + g_EPSILON;
    if _Center.y < 0.0 then
      _newPos.y := -g_EPSILON
    else if _Center.y > 0.0 then
      _newPos.y := g_EPSILON
    else
      _newPos.y := 0.0;
  end
  else
  begin
    minAnalyze := MAXSINGLE;
    iPos := -1;
    for i := 0 to 359 do
    begin
      SinCosS(i * g_DEGTORAD, sini, cosi);
      pV := MakeFloatPoint(cosi + sini - _oldPos.x, cosi - sini - _oldPos.y);
      tmp := abs(
        FloatPointDotProduct(
          MakeFloatPoint(cosi + sini, cosi - sini), pV));
      if tmp < minAnalyze then
      begin
        iPos := i;
        if Sign(_Center.y) = Sign((cosi - sini) * radius + _Center.y) then
          minAnalyze := tmp;
      end;
    end;

    SinCosS(iPos * g_DEGTORAD, sini, cosi);
    _newPos.x := cosi * _oldPos.x;
    _newPos.y := sini * _oldPos.x;

  end;


  tmp := _newPos.x * cosa - _newPos.y * sina;
  _newPos.y := _newPos.y * cosa + _newPos.x * sina;
  _newPos.x := tmp;

  newPos.x := newPos.x + _newPos.x;
  newPos.z := newPos.z + _newPos.y;
  Result := true;

end;
}
function MakeStopRoundIntersection(var newPos: TD3DVector; const oldPos: TD3DVector;
  const Center: TD3DVector; const radius: TD3DValue; const distance: TD3DValue): boolean;
var _newPos, _oldPos, _Center: TFloatPoint;
    angle: TD3DValue;
    sina, cosa: TD3DValue;
    tmp: TD3DValue;
    _newDist, _oldDist: TD3DValue;
    _radius: TD3DValue;
begin
  Result := false;

  _radius := radius + distance;
  if (_radius < 0.0) then
  begin
    raise Exception.Create('MakeStopRoundIntersection(): Invalid Parameters');
    exit;
  end;

  if _radius <= g_EPSILON then
    exit;
  if VectorEquel(newPos, oldPos) then
    exit;

  _newPos.x := 0.0;
  _newPos.y := 0.0;
  _oldPos.x := oldPos.x - newPos.x;
  _oldPos.y := oldPos.z - newPos.z;
  _Center.x := Center.x - newPos.x;
  _Center.y := Center.z - newPos.z;

  angle := GetAngle(_newPos, _oldPos);

  SinCosS(angle, sina, cosa);

  tmp := _oldPos.x * cosa + _oldPos.y * sina;
  _oldPos.y := _oldPos.y * cosa - _oldPos.x * sina;
  _oldPos.x := tmp;

  tmp := _Center.x * cosa + _Center.y * sina;
  _Center.y := _Center.y * cosa - _Center.x * sina;
  _Center.x := tmp;

  _newDist := sqrt(sqr(_Center.x - _newPos.x) + sqr(_Center.y - _newPos.y));
  _oldDist := sqrt(sqr(_Center.x - _oldPos.x) + sqr(_Center.y - _oldPos.y));

  if (_newDist > _radius) and
     (_oldDist > _newDist) and
     ((_Center.x < 0) or (_Center.x > _oldPos.x)) then
    exit;

  if abs(_Center.y) > _radius then
    exit;

  if abs(_Center.y) > _radius - g_EPSILON then
  begin
    _newPos.x := _Center.x + g_EPSILON;
    if _Center.y < 0.0 then
      _newPos.y := -g_EPSILON
    else if _Center.y > 0.0 then
      _newPos.y := g_EPSILON
    else
      _newPos.y := 0.0;
  end
  else
    _newPos.x := _oldPos.x;

  tmp := _newPos.x * cosa - _newPos.y * sina;
  _newPos.y := _newPos.y * cosa + _newPos.x * sina;
  _newPos.x := tmp;

  newPos.x := newPos.x + _newPos.x;
  newPos.z := newPos.z + _newPos.y;
  Result := true;

end;

// Δεν αφήνει να βγούμε απ'τον κύκλο
function MakeSlideInsideCyrcleIntersection(var newPos: TD3DVector; const oldPos: TD3DVector;
  const Center: TD3DVector; const radius: TD3DValue;
  const distance: TD3DValue = 0.0; const doForceInside: boolean = false): boolean;
var _newPos, _oldPos: TFloatPoint;
    angle: TD3DValue;
    sina, cosa: TD3DValue;
    _newDist, _oldDist: TD3DValue;
    _radius: TD3DValue;
begin
  Result := false;

  _radius := radius + distance;
  if (_radius < 0.0) then
  begin
    raise Exception.Create('MakeSlideInsideCyrcleIntersection(): Invalid Parameters');
    exit;
  end;

  if _radius <= g_EPSILON then
    exit;
  if VectorEquel(newPos, oldPos) then
    exit;

  _newPos.x := newPos.x - Center.x;
  _newPos.y := newPos.z - Center.z;
  _oldPos.x := oldPos.x - Center.x;
  _oldPos.y := oldPos.z - Center.z;

  _newDist := sqrt(sqr(_newPos.x) + sqr(_newPos.y));
  _oldDist := sqrt(sqr(_oldPos.x) + sqr(_oldPos.y));

  if doForceInside then
    Result := (_newDist >= _radius - g_EPSILON)
  else
    Result := (_newDist >= _radius - g_EPSILON) and (_oldDist <= _radius + g_EPSILON);

  if Result then
  begin
    angle := GetAngle(MakeFloatPoint(0.0, 0.0), _newPos);

    SinCosS(angle, sina, cosa);

    _newPos.x := cosa * (_radius - g_EPSILON);
    _newPos.y := sina * (_radius - g_EPSILON);
    newPos.x := _newPos.x + Center.x;
    newPos.z := _newPos.y + Center.z;
  end
end;

{function MakeRoundIntersection(var newPos: TD3DVector; const oldPos: TD3DVector;
  const Center: TD3DVector; const radius: TD3DValue; const distance: TD3DValue): boolean;
var _newPos, SCP: TD3DVector;
    SCPm: TD3DValue;
begin
  if VectorEquel(newPos, oldPos) then
  begin
    Result := false;
    exit;
  end;
  if (abs(Center.y - newPos.y) > radius) then
  begin
    Result := false;
    exit;
  end;
  if radius <= g_EPSILON then
  begin
    Result := false;
    exit;
  end;

  SCP := VectorSub(Center, newPos);
  SCPm := sqrt(sqr(SCP.x) + sqr(SCP.z));
  if SCPm < radius + distance then
  begin

    if SCPm <= g_EPSILON then
    begin
      newPos := oldPos;
      SCP := VectorSub(Center, oldPos);
      SCPm := sqrt(sqr(SCP.x) + sqr(SCP.z));
      if SCPm <= g_EPSILON then
      begin
        Result := false;
        exit;
      end;
    end;

    _newPos := SCP;

    _newPos.x := SCP.x / SCPm * (radius + distance + g_EPSILON);
    _newPos.z := SCP.z / SCPm * (radius + distance + g_EPSILON);

    newPos.x := Center.x + _newPos.x;
    newPos.z := Center.z + _newPos.z;

    Result := true;
  end
  else
    Result := false;
end;}

function MakeStopIntersection(var newPos: TD3DVector; const oldPos: TD3DVector;
  const step1, step2: TFloatPoint; const distance: TD3DValue): boolean;
{ Ελέγχει αν η γραμμή που ορίζεται από τα step1, step2 δεν εμποδίζει την
  κίνηση από το σημείο oldPos στο σημείο newPos (Έλεγχος πάνω στο επίπεδο (x,z)
  Επιτρέφει true αν κάνει αλλαγές στη νέα θέση newPos
  Η αλλαγή που κάνει είναι να σταματάει την κίνηση σε περίπτωση collision}
var _newPos,             // Νέα θέση προορισμού που επιτρέπει να μετακινηθούμε η γραμμή
    _oldPos: TFloatPoint;// Παλαιά θέση
    angle: TD3DValue;    // Γωνία
    sina, cosa: TD3DValue;
    tmp: TD3DValue;
begin
  if VectorEquel(newPos, oldPos) then
  begin
    Result := false;
    exit;
  end;
  _newPos.x := newPos.x - step1.x;
  _newPos.y := newPos.z - step1.y;
  _oldPos.x := oldPos.x - step1.x;
  _oldPos.y := oldPos.z - step1.y;

  angle := GetAngle(step1, step2);
  SinCosS(angle, sina, cosa);

  tmp := _newPos.x * cosa + _newPos.y * sina;
  _newPos.y := _newPos.y * cosa - _newPos.x * sina;
  _newPos.x := tmp;

  tmp := _oldPos.x * cosa + _oldPos.y * sina;
  _oldPos.y := _oldPos.y * cosa - _oldPos.x * sina;
  _oldPos.x := tmp;

  if (_newPos.y * _oldPos.y < 0) or ((abs(_newPos.y) <= distance + g_EPSILON)) then
  begin
    Result := true;

    if sqr(newPos.x - oldPos.x) + sqr(newPos.z - oldPos.z) < g_EPSILON then
      newPos := oldPos
    else
    begin
      newPos.x := (newPos.x + oldPos.x) /2;
      newPos.z := (newPos.z + oldPos.z) /2;
      MakeStopIntersection(newPos, oldPos, step1, step2, distance);
    end;

  end
  else
    Result := false;
end;

function MakeReflectIntersection(var newPos: TD3DVector; const oldPos: TD3DVector;
  const step1, step2: TFloatPoint; const distance: TD3DValue; var newRotationY: TD3DValue): boolean;
{ Ελέγχει αν η γραμμή που ορίζεται από τα step1, step2 δεν εμποδίζει την
  κίνηση από το σημείο oldPos στο σημείο newPos (Έλεγχος πάνω στο επίπεδο (x,z)
  Επιτρέφει true αν κάνει αλλαγές στη νέα θέση newPos
  Η αλλαγή που κάνει είναι αντανάκλαση σε περίπτωση collision}
var _newPos,             // Νέα θέση προορισμού που επιτρέπει να μετακινηθούμε η γραμμή
    _oldPos: TFloatPoint;// Παλαιά θέση
    angle: TD3DValue;    // Γωνία
    sina, cosa: TD3DValue;
    tmp: TD3DValue;
//    tRotationY: TD3DValue;
begin
  if VectorEquel(newPos, oldPos) then
  begin
    Result := false;
    exit;
  end;
  _newPos.x := newPos.x - step1.x;
  _newPos.y := newPos.z - step1.y;
  _oldPos.x := oldPos.x - step1.x;
  _oldPos.y := oldPos.z - step1.y;

  angle := GetAngle(step1, step2);
  SinCosS(angle, sina, cosa);

  tmp := _newPos.x * cosa + _newPos.y * sina;
  _newPos.y := _newPos.y * cosa - _newPos.x * sina;
  _newPos.x := tmp;

  tmp := _oldPos.x * cosa + _oldPos.y * sina;
  _oldPos.y := _oldPos.y * cosa - _oldPos.x * sina;
  _oldPos.x := tmp;

  if (_newPos.y * _oldPos.y < 0) or ((abs(_newPos.y) <= distance + g_EPSILON)) then
  begin
    _newPos.y :=  -_newPos.y;
    if (_oldPos.y < 0) and (_newPos.y > - distance - g_EPSILON) then
      _newPos.y :=  - distance - g_EPSILON
    else if (_oldPos.y > 0) and (_newPos.y < distance + g_EPSILON) then
      _newPos.y :=  distance + g_EPSILON;

{    tRotationY := -newRotationY + pi/2;
    tRotationY := 2 * angle - tRotationY;
    newRotationY := -tRotationY + pi/2;}
    newRotationY := g_PI - 2 * angle - newRotationY;

    Result := abs(_newPos.x - GetValueInRange(_newPos.x, distance, sqrt(sqr(step1.x - step2.x) + sqr(step1.y - step2.y)) - distance)) < g_EPSILON;

    tmp := _newPos.x * cosa - _newPos.y * sina;
    _newPos.y := _newPos.y * cosa + _newPos.x * sina;
    _newPos.x := tmp;

    newPos.x := _newPos.x + step1.x;
    newPos.z := _newPos.y + step1.y;
  end
  else
    Result := false;
end;

(*
Working version with no scrolling on collision.
Recoursively calls itself.
function MakeIntersection(var newPos: TD3DVector; const oldPos: TD3DVector;
  const step1, step2: TFloatPoint; const distance: TD3DValue): boolean;
{ Ελέγχει αν η γραμμή που ορίζεται από τα step1, step2 δεν εμποδίζει την
  κίνηση από το σημείο oldPos στο σημείο newPos (Έλεγχος πάνω στο επίπεδο (x,z)
  Επιτρέφει true αν κάνει αλλαγές στη νέα θέση newPos }
var _newPos,             // Νέα θέση προορισμού που επιτρέπει να μετακινηθούμε η γραμμή
    _oldPos: TFloatPoint;// Παλαιά θέση
    angle: TD3DValue;    // Γωνία
    sina, cosa: TD3DValue;
    tmp: TD3DValue;
begin
  if VectorEquel(newPos, oldPos) then
  begin
    Result := false;
    exit;
  end;
  _newPos.x := newPos.x - step1.x;
  _newPos.y := newPos.z - step1.y;
  _oldPos.x := oldPos.x - step1.x;
  _oldPos.y := oldPos.z - step1.y;

  angle := GetAngle(step1, step2);
  SinCosS(angle, sina, cosa);

  tmp := _newPos.x * cosa - _newPos.y * sina;
  _newPos.y := _newPos.y * cosa - _newPos.x * sina;
  _newPos.x := tmp;

  tmp := _oldPos.x * cosa - _oldPos.y * sina;
  _oldPos.y := _oldPos.y * cosa - _oldPos.x * sina;
  _oldPos.x := tmp;

  if (_newPos.y * _oldPos.y < 0) or ((abs(_newPos.y) <= distance + g_EPSILON)) then
  begin
    if sqr(newPos.x - oldPos.x) + sqr(newPos.z - oldPos.z) < g_EPSILON then
    begin
      newPos := oldPos;
      Result := true
    end
    else
    begin
      newPos.x := (newPos.x + oldPos.x) /2;
      newPos.z := (newPos.z + oldPos.z) /2;
      Result := MakeIntersection(newPos, oldPos, step1, step2, distance);
      // Result := true ????
    end;
  end
  else
    Result := false;
end;
*)

(*
function MakeIntersection(var newPos: TD3DVector; const oldPos: TD3DVector;
  const step1, step2: TFloatPoint; const distance: TD3DValue): boolean;
{ Ελέγχει αν η γραμμή που ορίζεται από τα step1, step2 δεν εμποδίζει την
  κίνηση από το σημείο oldPos στο σημείο newPos (Έλεγχος πάνω στο επίπεδο (x,z)
  Επιτρέφει true αν κάνει αλλαγές στη νέα θέση newPos }
var _newPos,             // Νέα θέση προορισμού που επιτρέπει να μετακινηθούμε η γραμμή
    _oldPos: TFloatPoint;// Παλαιά θέση
    angle: TD3DValue;    // Γωνία
    sina, cosa: TD3DValue;
    tmp: TD3DValue;
    dist: TD3DValue;
begin
  if VectorEquel(newPos, oldPos) then
  begin
    Result := false;
    exit;
  end;
  _newPos.x := newPos.x - step1.x;
  _newPos.y := newPos.z - step1.y;
  _oldPos.x := oldPos.x - step1.x;
  _oldPos.y := oldPos.z - step1.y;

  angle := GetAngle(step1, step2);
  SinCosS(angle, sina, cosa);

  tmp := _newPos.x * cosa - _newPos.y * sina;
  _newPos.y := _newPos.y * cosa - _newPos.x * sina;
  _newPos.x := tmp;

  tmp := _oldPos.x * cosa - _oldPos.y * sina;
  _oldPos.y := _oldPos.y * cosa - _oldPos.x * sina;
  _oldPos.x := tmp;

  if (_newPos.y * _oldPos.y < 0) or ((abs(_newPos.y) <= distance + g_EPSILON)) then
  begin

    if _oldPos.y < 0 then
      _newPos.y := - distance// - g_EPSILON
    else
      _newPos.y := distance;// + g_EPSILON;
    _newPos.x := GetValueInRange(_newPos.x, distance, sqrt(sqr(step1.x-step2.x) + sqr(step1.y - step2.y)) - distance);

//    SinCosS(-angle, sina, cosa);
    _newPos.x := _newPos.x + step1.x;
    _newPos.y := _newPos.y + step1.y;
    tmp := _newPos.x * cosa + _newPos.y * sina;
    _newPos.y := _newPos.y * cosa + _newPos.x * sina;
    _newPos.x := tmp;

    newPos.x := _newPos.x;// + step1.x;
    newPos.z := _newPos.y;// + step1.y;
    Result := true;
    exit; /////////////////////}

    if sqr(newPos.x - oldPos.x) + sqr(newPos.z - oldPos.z) < g_EPSILON then
    begin
      newPos := oldPos;
      Result := true
    end
    else
    begin
{
      dist := sqrt(sqr(newPos.x - oldPos.x) + sqr(newPos.z + oldPos.z));
      SinCosS(GetAngle(MakeFloatPoint(oldPos), MakeFloatPoint(newPos)), sina, cosa);
      newPos.x := oldPos.x + dist * cosa;
      newPos.z := oldPos.z + dist * sina;
{       + (oldPos.x - newPos.x) * cosa
        + (oldPos.z - newPos.z) * sina}

      newPos.x := (newPos.x + oldPos.x) /2;
      newPos.z := (newPos.z + oldPos.z) /2;
      Result := MakeIntersection(newPos, oldPos, step1, step2, distance);
    end;
//    newPos := oldPos;
  end
  else
    Result := false;
end;
*)

(*
function MakeIntersection(var newPos: TD3DVector; const oldPos: TD3DVector;
  const step1, step2: TFloatPoint; const distance: TD3DValue): boolean;
{ Ελέγχει αν η γραμμή που ορίζεται από τα step1, step2 δεν εμποδίζει την
  κίνηση από το σημείο oldPos στο σημείο newPos (Έλεγχος πάνω στο επίπεδο (x,z)
  Επιτρέφει true αν κάνει αλλαγές στη νέα θέση newPos }
const MAXLOOPS = 8;
      MAXDEPTH = 8;
var _Start,              // Αρχικό σημείο της γραμμής που ελέγχουμε
    _End,                // Τελικό σημείο της γραμμής που ελέγχουμε
    _newPos,             // Νέα θέση προορισμού που επιτρέπει να μετακινηθούμε η γραμμή
    _oldPos: TFloatPoint;// Παλαιά θέση
    angle: TD3DValue;    // Γωνία
    sina, cosa: TD3DValue;
    tmp: TD3DValue;
begin
   Result := false;
  _Start.x := 0.0;
  _Start.y := 0.0;
  _End.x := step2.x - step1.x;
  _End.y := step2.y - step1.y;
  _newPos.x := newPos.x - step1.x;
  _newPos.y := newPos.z - step1.y;
  _oldPos.x := oldPos.x - step1.x;
  _oldPos.y := oldPos.z - step1.y;

  angle := GetAngle(step1, step2);
  SinCosS(angle, sina, cosa);
  _End.x := _End.x * cosa - _End.y * sina;
  _End.y := 0.0;

  tmp := _newPos.x * cosa - _newPos.y * sina;
  _newPos.y := _newPos.y * cosa - _newPos.x * sina;
  _newPos.x := tmp;

  tmp := _oldPos.x * cosa - _oldPos.y * sina;
  _oldPos.y := _oldPos.y * cosa - _oldPos.x * sina;
  _oldPos.x := tmp;

  if (_newPos.y * _oldPos.y < 0) or ((abs(_newPos.y) <= distance + g_EPSILON)) then
  begin
    if _oldPos.y < 0 then
      tmp := - distance
    else
      tmp := distance;
    if abs(_oldPos.x - _newPos.x) < g_EPSILON then
      _newPos.x := _oldPos.x
    else
    begin
      _newPos.x := _oldPos.x + (_newPos.x - _oldPos.x) * (tmp - _oldPos.y) /(_newPos.y - _oldPos.y)
    end;
    _newPos.y := tmp;
{    if not IsFloatInRange(_newPos.x, Min(0, _end.x) - distance, Max(0, _end.x) + distance) then
      exit;}
//    _newPos.x := GetValueInRange(_newPos.x, Min(0, _end.x) - distance, Max(0, _end.x) + distance);

    tmp := _newPos.x * cosa + _newPos.y * sina;
    _newPos.y := _newPos.y * cosa + _newPos.x * sina;
    _newPos.x := tmp;

    newPos.x := _newPos.x + step1.x;
    newPos.z := _newPos.y + step1.y;

    newPos := oldPos; //////////////
    ///////////////////////
{    newPos.x := oldPos.x;
    newPos.z := _newPos.y + step1.y;}
    ////////////////////////
    Result := true;
  end;
end;
*)
(*
function MakeIntersection(var newPos: TD3DVector; const oldPos: TD3DVector;
  const step1, step2: TFloatPoint; const distance: TD3DValue): boolean;
{ Ελέγχει αν η γραμμή που ορίζεται από τα step1, step2 δεν εμποδίζει την
  κίνηση από το σημείο oldPos στο σημείο newPos (Έλεγχος πάνω στο επίπεδο (x,z)
  Επιτρέφει true αν κάνει αλλαγές στη νέα θέση newPos }
var _Start,              // Αρχικό σημείο της γραμμής που ελέγχουμε
    _End,                // Τελικό σημείο της γραμμής που ελέγχουμε
    _newPos,             // Νέα θέση προορισμού που επιτρέπει να μετακινηθούμε η γραμμή
    _oldPos: TFloatPoint;// Παλαιά θέση
    angle: TD3DValue;    // Γωνία
    sina, cosa: TD3DValue;
    tmp: TD3DValue;
begin
  _Start.x := 0.0;
  _Start.y := 0.0;
  _End.x := step2.x - step1.x;
  _End.y := step2.y - step1.y;
  _newPos.x := newPos.x - step1.x;
  _newPos.y := newPos.z - step1.y;
  _oldPos.x := oldPos.x - step1.x;
  _oldPos.y := oldPos.z - step1.y;

  angle := GetAngle(step1, step2);
  SinCosS(angle, sina, cosa);
  _End.x := _End.x * cosa - _End.y * sina;
  _End.y := 0.0;

  tmp := _newPos.x * cosa - _newPos.y * sina;
  _newPos.y := _newPos.y * cosa - _newPos.x * sina;
  _newPos.x := tmp;

  tmp := _oldPos.x * cosa - _oldPos.y * sina;
  _oldPos.y := _oldPos.y * cosa - _oldPos.x * sina;
  _oldPos.x := tmp;

  if (_newPos.y * _oldPos.y < 0) or ((abs(_newPos.y) < distance)) then
  begin
    if _oldPos.y < 0 then
      _newPos.y := -distance
    else
      _newPos.y := distance;
    _newPos.y := GetValueInRange(_newPos.x, Min(0, _end.x), Max(0, _end.x));
    sina := -sina;

    tmp := _newPos.x * cosa - _newPos.y * sina;
    _newPos.y := _newPos.y * cosa - _newPos.x * sina;
    _newPos.x := tmp;

    newPos.x := _newPos.x + step1.x;
    newPos.z := _newPos.y + step1.y;
    newPos := oldPos; //////////////
    Result := true;
  end
  else
    Result := false;
end;
*)
{
////
Working version, not speed optimized, if colizion return last position
////
procedure MakeIntersection(var newPos: TD3DVector; const oldPos: TD3DVector;
  const step1, step2: TFloatPoint; const distance: TD3DValue);
var _start, _end: TD3DVector;
    _newPos, _oldPos: TD3DVector;
    angle: TD3DValue;
    sina, cosa: TD3DValue;
    tmp: TD3DValue;
begin
  _Start := MakeD3DVector(0.0, 0.0, 0.0);
  _End := MakeD3DVector(step2.x - step1.x, 0.0, step2.y - step1.y);
  _newPos := MakeD3DVector(newPos.x - step1.x, 0.0, newPos.z - step1.y);
  _oldPos := MakeD3DVector(oldPos.x - step1.x, 0.0, oldPos.z - step1.y);

  angle := GetAngle(step1, step2);
  SinCosS(angle, sina, cosa);
  _End.x := _End.x * cosa - _End.z * sina;
  _End.z := 0.0;

  tmp := _newPos.x * cosa - _newPos.z * sina;
  _newPos.z := _newPos.z * cosa - _newPos.x * sina;
  _newPos.x := tmp;

  tmp := _oldPos.x * cosa - _oldPos.z * sina;
  _oldPos.z := _oldPos.z * cosa - _oldPos.x * sina;
  _oldPos.x := tmp;

  if (_newPos.z * _oldPos.z < 0) or ((abs(_newPos.z) < distance)) then
  begin
    if _oldPos.z < 0 then
      _newPos.z := -distance
    else
      _newPos.z := distance;
    _newPos.x := GetValueInRange(_newPos.x, Min(0, _end.x), Max(0, _end.x));
    sina := -sina;

    tmp := _newPos.x * cosa - _newPos.z * sina;
    _newPos.z := _newPos.z * cosa - _newPos.x * sina;
    _newPos.x := tmp;

    newPos.x := _newPos.x + step1.x;
    newPos.z := _newPos.z + step1.y;
    newPos := oldPos; //////////////
  end
end;}

function GetYDirectionVector(const y: TD3DValue): TD3DVector;
begin
  SinCosS(y, Result.x, Result.z);
  Result.y := 0.0;
end;

function GetHorizontalDistance(const v1, v2: TD3DVector): TD3DValue;
begin
  Result := sqrt(sqr(v1.x - v2.x) + sqr(v1.z - v2.z));
end;

function SafeAddSlash(const s: string): string;
begin
  Result := s;
  if Length(Result) > 0 then
    if Result[Length(Result)] <> '\' then
      Result := Result + '\';
end;

function FirstLine(const s: string): string;
var str: TStringList;
begin
  Result := '';
  str := TStringList.Create;
  try
    str.Text := s;
    if str.Count > 0 then Result := str.Strings[0]
  finally
    str.Free;
  end;
end;

procedure SaveTextFile(const FileName : TFileName; const Source : string);
begin
  with TStringList.Create do
  try
		Text := Source;
		SaveToFile(FileName);
  finally
    Free;
  end;
end;

function LoadTextFile(const FileName : TFileName): string;
begin
  with TStringList.Create do
  try
		LoadFromFile(FileName);
		Result := Text;
  finally
    Free;
  end;
end;

function RoundFrequency(const Frequency: Integer): Integer;
const
  NF: array [0..8] of Integer = (0, 20, 33, 50, 60, 66, 80, 90, 100);
var
  Freq, RF: Integer;
  i: Byte;
  Hi, Lo: Byte;
begin
  RF:=0;
  Freq:=Frequency mod 100;
  for i:=0 to 8 do begin
    if Freq<NF[i] then begin
      Hi:=i;
      Lo:=i-1;
      if (NF[Hi]-Freq)>(Freq-NF[Lo]) then
        RF:=NF[Lo]-Freq
      else
        RF:=NF[Hi]-Freq;
      Break;
    end;
  end;
  Result:=Frequency+RF;
end;

function GetCPUSpeed: TCPUFrequencyInfo;
var
  T0, T1: TULargeInteger;
  CountFreq: TULargeInteger;
  Freq, Freq2, Freq3, Total: Integer;
  TotalCycles, Cycles: Cardinal;
  Stamp0, Stamp1: Cardinal;
  TotalTicks, Ticks: Cardinal;
  Tries, IPriority: Integer;
  hThread: THandle;
  cs: TCriticalSection;
begin
  cs := TCriticalSection.Create;
  try
    cs.Enter;
    Freq:=0;
    Freq2:=0;
    Freq3:=0;
    Tries:=0;
    TotalCycles:=0;
    TotalTicks:=0;
    Total:=0;

    hThread:=GetCurrentThread;
    if not QueryPerformanceFrequency(Int64(CountFreq)) then
    begin
      Result.RawFreq := 0;
      Result.NormFreq := 0;
      Result.InCycles := 0;
      Result.ExTicks := 0;
    end
    else
    begin
      while ((Tries<3) or ((Tries<20) and ((Abs(3*Freq-Total)>3) or
            (Abs(3*Freq2-Total)>3) or (Abs(3*Freq3-Total)>3)))) do
      begin
        Inc(Tries);
        Freq3:=Freq2;
        Freq2:=Freq;
        QueryPerformanceCounter(Int64(T0));
        T1.LowPart:=T0.LowPart;
        T1.HighPart:=T0.HighPart;

        iPriority:=GetThreadPriority(hThread);
        if iPriority<>THREAD_PRIORITY_ERROR_RETURN then
          SetThreadPriority(hThread, THREAD_PRIORITY_TIME_CRITICAL);
        while (T1.LowPart-T0.LowPart)<50 do
        begin
          QueryPerformanceCounter(Int64(T1));
          asm
            PUSH    EAX
            PUSH    EDX
            DB      0Fh             // Read Time
            DB      31h             // Stamp Counter
            MOV     Stamp0, EAX
            POP     EDX
            POP     EAX
          end;
        end;
        T0.LowPart:=T1.LowPart;
        T0.HighPart:=T1.HighPart;

        while (T1.LowPart-T0.LowPart)<1000 do
        begin
          QueryPerformanceCounter(Int64(T1));
          asm
            PUSH    EAX
            PUSH    EDX
            DB      0Fh             // Read Time
            DB      31h             // Stamp Counter
            MOV     Stamp1, EAX
            POP     EDX
            POP     EAX
          end;
        end;

        if iPriority<>THREAD_PRIORITY_ERROR_RETURN then
          SetThreadPriority(hThread, iPriority);

        Cycles:=Stamp1-Stamp0;
        Ticks:=T1.LowPart-T0.LowPart;
        Ticks:=Ticks*100000;
        Ticks:=Round(Ticks/(CountFreq.LowPart/10));
        TotalTicks:=TotalTicks+Ticks;
        TotalCycles:=TotalCycles+Cycles;

        Freq:=Round(Cycles/Ticks);

        Total:=Freq+Freq2+Freq3;
      end;
      Freq3:=Round((TotalCycles*10)/TotalTicks);
      Freq2:=Round((TotalCycles*100)/TotalTicks);

      if Freq2-(Freq3*10)>=6 then
        Inc(Freq3);

      Result.RawFreq:=Round(TotalCycles/TotalTicks);
      Result.NormFreq:=Result.RawFreq;

      Freq:=Result.RawFreq*10;
      if (Freq3-Freq)>=6 then
        Inc(Result.NormFreq);

      Result.ExTicks:=TotalTicks;
      Result.InCycles:=TotalCycles;

      Result.NormFreq:=RoundFrequency(Result.NormFreq);
    end;
  finally
    cs.Release;
    cs.Free;
  end;
end;

function GetNumCPUs: word;
var
  SI :TSystemInfo;
begin
  ZeroMemory(@SI,SizeOf(SI));
  GetSystemInfo(SI);
  Result := SI.dwNumberOfProcessors;
end;

var s_alttab_disabled: boolean;

procedure WIN_DisableAltTab;
var
  old: Boolean;
begin
  if s_alttab_disabled then
    Exit;

  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    if isLibrary then
      RegisterHotKey(0, $C000, MOD_ALT, VK_TAB)
    else
      RegisterHotKey(0, 0, MOD_ALT, VK_TAB)
  end
  else
    SystemParametersInfo(SPI_SCREENSAVERRUNNING, 1, @old, 0);

  s_alttab_disabled := True;
end;

procedure WIN_EnableAltTab;
var
  old: Boolean;
begin
  if s_alttab_disabled then
  begin
    if Win32Platform = VER_PLATFORM_WIN32_NT then
    begin
      if isLibrary then
        UnregisterHotKey(0, $C000)
      else
        UnregisterHotKey(0, 0)
    end
    else
      SystemParametersInfo(SPI_SCREENSAVERRUNNING, 0, @old, 0);

    s_alttab_disabled := False;
  end;
end;

procedure CalcCenterAndRadius(TheVertexes: PD3DLVertexArray; fNumVertexes: integer;
  var fRadius: TD3DValue; var fCenter: TD3DVector);
var
  i: integer;
  tmp: TD3DValue;
begin
  fCenter := NULLVECTOR;
  fRadius := 0.0;
  if fNumVertexes <> 0 then
  begin
    for i := 0 to fNumVertexes - 1 do
      fCenter :=
        VectorAdd(fCenter,
          MakeD3DVector(TheVertexes[i]));
    fCenter.x := fCenter.x / fNumVertexes;
    fCenter.y := fCenter.y / fNumVertexes;
    fCenter.z := fCenter.z / fNumVertexes;

    for i := 0 to fNumVertexes - 1 do
    begin
      tmp := VectorSquareMagnitude(
        VectorSub(fCenter,
          MakeD3DVector(TheVertexes[i])));
      if tmp > fRadius then
        fRadius := tmp;
    end;
    fRadius := sqrt(fradius);
  end;
end;

procedure CalcCenterAndRadius(TheVertexes: PD3DVectorArray; fNumVertexes: integer;
  var fRadius: TD3DValue; var fCenter: TD3DVector); overload;
var
  i: integer;
  tmp: TD3DValue;
begin
  fCenter := NULLVECTOR;
  fRadius := 0.0;
  if fNumVertexes <> 0 then
  begin
    for i := 0 to fNumVertexes - 1 do
      fCenter :=
        VectorAdd(fCenter,
          TheVertexes[i]);
    fCenter.x := fCenter.x / fNumVertexes;
    fCenter.y := fCenter.y / fNumVertexes;
    fCenter.z := fCenter.z / fNumVertexes;

    for i := 0 to fNumVertexes - 1 do
    begin
      tmp := VectorSquareMagnitude(
        VectorSub(fCenter,
          TheVertexes[i]));
      if tmp > fRadius then
        fRadius := tmp;
    end;
    fRadius := sqrt(fRadius);
  end;
end;

function CalcBoundingCube(TheVertexes: PD3DLVertexArray; fNumVertexes: integer): TBoundingCube;
var
  i: integer;
  pV: PD3DLVertex;
  pVx, pVy, pVz: TD3DValue;
begin
  MakeEmptyBoundingCube(Result);
  pV := @TheVertexes[0];
  for i := 0 to fNumVertexes - 1 do
  begin
    pVx := pV.x;
    if pVx < Result.minX then
      Result.minX := pVx;
    if pVx > Result.maxX then
      Result.maxX := pVx;

    pVy := pV.y;
    if pVy < Result.minY then
      Result.minY := pVy;
    if pVy > Result.maxY then
      Result.maxY := pVy;

    pVz := pV.z;
    if pVz < Result.minZ then
      Result.minZ := pVz;
    if pVz > Result.maxY then
      Result.maxZ := pVz;

    inc(pV);
  end;
end;

function CalcBoundingCube(TheVertexes: PD3DVectorArray; fNumVertexes: integer): TBoundingCube;
var
  i: integer;
  pV: PD3DVector;
  pVx, pVy, pVz: TD3DValue;
begin
  MakeEmptyBoundingCube(Result);
  pV := @TheVertexes[0];
  for i := 0 to fNumVertexes - 1 do
  begin
    pVx := pV.x;
    if pVx < Result.minX then
      Result.minX := pVx;
    if pVx > Result.maxX then
      Result.maxX := pVx;

    pVy := pV.y;
    if pVy < Result.minY then
      Result.minY := pVy;
    if pVy > Result.maxY then
      Result.maxY := pVy;

    pVz := pV.z;
    if pVz < Result.minZ then
      Result.minZ := pVz;
    if pVz > Result.maxY then
      Result.maxZ := pVz;

    inc(pV);
  end;
end;

function GetPascalText(const s: string): string;
var
  i: integer;
begin
  Result := '';
  for i := 1 to Length(s) do
  begin
    Result := Result + s[i];
    if s[i] = '''' then Result := Result + s[i];
  end;
end;

function ExtractFileNameOnly(FileName: string): string;
var
  i, j: integer;
  len: integer;
begin
  Result := FileName;
  len := length(Result);
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

{  Result := ExtractFileName(FileName);
  len := Pos('.', Result);
  if len > 0 then
  begin
    SetLength(Result, len - 1);
    len := len - 1;
  end
  else
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
  end;}
end;

function RemoveExtention(const FileName: string): string;
var
  i: integer;
begin
  Result := FileName;
  i := Length(Result);
  while i > 0 do
  begin
    if Result[i] in ['/', '\', ':', '>', '<', '|'] then
      Exit;
    if Result[i] = '.' then
    begin
      SetLength(Result, i - 1);
      Exit;
    end;
    dec(i);
  end;
end;

function GetNormalizeAngle(a: TD3DValue): TD3DValue;
begin
{  if a > g_PI then
    a := a - g_2_PI
  else if a < -g_PI then
    a := a + g_2_PI;
  Result := a;}
  while a > g_PI do
    a := a - g_2_PI;
  while a < g_NEG_PI do
    a := a + g_2_PI;
  Result := a;
end;

{ We can call this function using an open array constructor (see Open array constructors).
  For example, MakeStr(['test', 100, ' ', True, 3.14159, TForm]) returns the string
  test100 True3.14159TForm. }
function MakeStr(const Args: array of const): string;
const
  BoolChars: array[Boolean] of string = ('True', 'False');
var
  i: Integer;
begin
  Result := '';
  for i := 0 to High(Args) do
    with Args[I] do
      case VType of
        vtInteger:    Result := Result + IntToStr(VInteger);
        vtBoolean:    Result := Result + BoolChars[VBoolean];
        vtChar:       Result := Result + VChar;
        vtExtended:   Result := Result + FloatToStr(VExtended^);
        vtString:     Result := Result + VString^;
        vtPChar:      Result := Result + VPChar;
        vtObject:     Result := Result + VObject.ClassName;
        vtClass:      Result := Result + VClass.ClassName;
        vtAnsiString: Result := Result + string(VAnsiString);
        vtCurrency:   Result := Result + CurrToStr(VCurrency^);
        vtVariant:    Result := Result + string(VVariant^);
        vtInt64:      Result := Result + IntToStr(VInt64^);
    end;
end;

function IntToStr2(i: integer): string;
begin
  Result := IntToStr(i);
  if i < 10 then Result := '0' + Result;
end;

function IntToStr3(i: integer): string;
begin
  Result := IntToStr(i);
  if i < 100 then Result := '0' + Result;
  if i < 10 then Result := '0' + Result;
end;

function IntToStr4(i: integer): string;
begin
  Result := IntToStr(i);
  if i < 1000 then Result := '0' + Result;
  if i < 100 then Result := '0' + Result;
  if i < 10 then Result := '0' + Result;
end;

procedure FreeAndNilSafe(var Obj);
begin
  try
    FreeAndNil(obj);
  except
  end;
end;

procedure DoForegroundForms(parent: TComponent);
var
  i, j: integer;
begin
  for i := 0 to parent.ComponentCount - 1 do
  begin
    if parent.Components[i].InheritsFrom(TForm) then
      if fsModal in (parent.Components[i] as TForm).FormState then
      begin
        (parent.Components[i] as TForm).BringToFront;
        for j := 0 to (parent.Components[i] as TForm).ComponentCount - 1 do
          if (parent.Components[i] as TForm).Components[j].InheritsFrom(TCommonDialog) then
          try
            if ((parent.Components[i] as TForm).Components[j] as TCommonDialog).Handle <> 0 then
              SetForegroundWindow(((parent.Components[i] as TForm).Components[j] as TCommonDialog).Handle);
          except
          end;
      end;
    DoForegroundForms(parent.Components[i]);
  end;
end;

procedure doClearStringList(S: TStringList);
var
  i: integer;
begin
  if S <> nil then
  begin
    for i := 0 to S.Count - 1 do
      if S.Objects[i] <> nil then
      try
        S.Objects[i].Free;
      except
        S.Objects[i] := nil;
      end;
    S.Clear;
  end;
end;

function GetHashCode(const Buffer; Count: Integer): Word; assembler;
asm
        MOV     ECX,EDX
        MOV     EDX,EAX
        XOR     EAX,EAX
@@1:    ROL     AX,5
        XOR     AL,[EDX]
        INC     EDX
        DEC     ECX
        JNE     @@1
end;

procedure MakeBmpAlphaChannel(bmp: TBitmap; Alpha: byte);
var
  p, pixel: Pointer;
  count: integer;
  i, numPixels: integer;
begin
  bmp.PixelFormat := pf32bit;
  numPixels := bmp.Width * bmp.Height;
  count := numPixels * 4;
  GetMem(p, count);
  pixel := p;
  inc(integer(pixel), 3);
  GetBitmapBits(bmp.Handle, count, p);
  for i := 1 to numPixels do
  begin
    byte(pixel^) := alpha;
    inc(integer(pixel), 4);
  end;
  SetBitmapBits(bmp.Handle, count, p);
  FreeMem(p, count);
end;

function GetPositionInsideBoundingCube(const pCube: PBoundingCube;
  var X, Y, Z: TD3DValue): boolean;
begin
  Result := false;

  if X < pCube.MinX then
  begin
    X := pCube.MinX;
    Result := true;
  end;
  if X > pCube.MaxX then
  begin
    X := pCube.MaxX;
    Result := true;
  end;

  if Y < pCube.MinY then
  begin
    Y := pCube.MinY;
    Result := true;
  end;
  if Y > pCube.MaxY then
  begin
    Y := pCube.MaxY;
    Result := true;
  end;

  if Z < pCube.MinZ then
  begin
    Z := pCube.MinZ;
    Result := true;
  end;
  if Z > pCube.MaxZ then
  begin
    Z := pCube.MaxZ;
    Result := true;
  end;
end;

function GetPositionInsideBoundingCube(const pCube: PBoundingCube;
  var V: TD3DVector): boolean;
begin
  Result := false;

  if v.X < pCube.MinX then
  begin
    v.X := pCube.MinX;
    Result := true;
  end;
  if v.X > pCube.MaxX then
  begin
    v.X := pCube.MaxX;
    Result := true;
  end;

  if v.Y < pCube.MinY then
  begin
    v.Y := pCube.MinY;
    Result := true;
  end;
  if v.Y > pCube.MaxY then
  begin
    v.Y := pCube.MaxY;
    Result := true;
  end;

  if v.Z < pCube.MinZ then
  begin
    v.Z := pCube.MinZ;
    Result := true;
  end;
  if v.Z > pCube.MaxZ then
  begin
    v.Z := pCube.MaxZ;
    Result := true;
  end;
end;

function GetPositionInsideBoundingCube(const pCube: PBoundingCube;
  const Offset: TD3DValue;
  var X, Y, Z: TD3DValue): boolean; overload;
begin
  Result := false;

  if X < pCube.MinX - OffSet then
  begin
    X := pCube.MinX - OffSet ;
    Result := true;
  end;
  if X > pCube.MaxX + OffSet then
  begin
    X := pCube.MaxX + OffSet;
    Result := true;
  end;

  if Y < pCube.MinY - OffSet then
  begin
    Y := pCube.MinY - OffSet;
    Result := true;
  end;
  if Y > pCube.MaxY + OffSet then
  begin
    Y := pCube.MaxY + OffSet;
    Result := true;
  end;

  if Z < pCube.MinZ - OffSet then
  begin
    Z := pCube.MinZ - OffSet;
    Result := true;
  end;
  if Z > pCube.MaxZ + OffSet then
  begin
    Z := pCube.MaxZ + OffSet;
    Result := true;
  end;
end;

function GetPositionInsideBoundingCube(const pCube: PBoundingCube;
  const Offset: TD3DValue;
  var V: TD3DVector): boolean; overload;
begin
  Result := false;

  if v.X < pCube.MinX - OffSet then
  begin
    v.X := pCube.MinX - OffSet ;
    Result := true;
  end;
  if v.X > pCube.MaxX + OffSet then
  begin
    v.X := pCube.MaxX + OffSet;
    Result := true;
  end;

  if v.Y < pCube.MinY - OffSet then
  begin
    v.Y := pCube.MinY - OffSet;
    Result := true;
  end;
  if v.Y > pCube.MaxY + OffSet then
  begin
    v.Y := pCube.MaxY + OffSet;
    Result := true;
  end;

  if v.Z < pCube.MinZ - OffSet then
  begin
    v.Z := pCube.MinZ - OffSet;
    Result := true;
  end;
  if v.Z > pCube.MaxZ + OffSet then
  begin
    v.Z := pCube.MaxZ + OffSet;
    Result := true;
  end;
end;

function FixBackSlash(const s: string): string;
var
  i: integer;
begin
  Result := '';
  for i := 1 to Length(s) do
    if s[i] = '/' then
      Result := Result + '\'
    else
      Result := Result + s[i];
{    if s[i] = '\' then
      Result := Result + '/'
    else
      Result := Result + s[i];}
end;

procedure EliminateBlackPixels(b: TBitmap);
var
  w, h: integer;
  local_i, local_j: integer;
  PP: PByteArray;
begin
  case b.PixelFormat of
    pf8bit: w := b.Width;
    pf16bit: w := 2 * b.Width;
    pf24bit: w := 3 * b.Width;
    pf32bit: w := 4 * b.Width;
  else
    exit;
  end;
  h := b.Height;
  for local_i := 0 to h - 1 do
  begin
    PP := b.ScanLine[local_i];
    for local_j := 0 to w - 1 do
      if PP[local_j] < 8 then
        PP[local_j] := 8;
  end;
end;

procedure EliminateBlackPixels(b: TBitmap; const tolerance: byte);
var
  w, h: integer;
  local_i, local_j: integer;
  PP: PByteArray;
begin
  case b.PixelFormat of
    pf8bit: w := b.Width;
    pf16bit: w := 2 * b.Width;
    pf24bit: w := 3 * b.Width;
    pf32bit: w := 4 * b.Width;
  else
    exit;
  end;
  h := b.Height;
  for local_i := 0 to h - 1 do
  begin
      PP := b.ScanLine[local_i];
      for local_j := 0 to w - 1 do
        if PP[local_j] < tolerance then
          PP[local_j] := tolerance;
  end;
end;

procedure MakeBlackRangePixels(b: TBitmap); overload;
var
  w, h: integer;
  local_i, local_j: integer;
  PP: PByteArray;
begin
  case b.PixelFormat of
    pf8bit: w := b.Width;
    pf16bit: w := 2 * b.Width;
    pf24bit: w := 3 * b.Width;
    pf32bit: w := 4 * b.Width;
  else
    exit;
  end;
  h := b.Height;
  for local_i := 0 to h - 1 do
  begin
    PP := b.ScanLine[local_i];
    for local_j := 0 to w - 1 do
      if PP[local_j] < 8 then
        PP[local_j] := 0;
  end;
end;

procedure MakeBlackRangePixels(b: TBitmap; const tolerance: byte); overload;
var
  w, h: integer;
  local_i, local_j: integer;
  PP: PByteArray;
begin
  case b.PixelFormat of
    pf8bit: w := b.Width;
    pf16bit: w := 2 * b.Width;
    pf24bit: w := 3 * b.Width;
    pf32bit: w := 4 * b.Width;
  else
    exit;
  end;
  h := b.Height;
  for local_i := 0 to h - 1 do
  begin
    PP := b.ScanLine[local_i];
    for local_j := 0 to w - 1 do
      if PP[local_j] < tolerance then
        PP[local_j] := 0;
  end;
end;

procedure FlashHandle(h: THandle; sleepmsecs: integer);
begin
  SetActiveWindow(0);
  Sleep(sleepmsecs);
  SetActiveWindow(h);
end;

procedure WriteStringToStream(strm: TStream; const s: string);
var
  len: word;
  i: integer;
begin
  len := length(s);
  strm.Write(len, SizeOf(len));
  for i := 1 to len do
    strm.Write(s[i], SizeOf(s[i]));
end;

function ReadStringFromStream(strm: TStream): string;
var
  len: word;
  i: integer;
begin
  strm.Read(len, SizeOf(len));
  SetLength(Result, len);
  for i := 1 to len do
    strm.Read(Result[i], SizeOf(Result[i]));
end;

function TryFocusControl(c: TWinControl): boolean;
var
  c2: TWinControl;
  cnt: integer;
begin
  Result := c.CanFocus;
  if Result then
  begin
    c2 := c;
    cnt := 0;
    while not (c2 is TForm) do
    begin
      if cnt > 100 then
        Break;
      inc(cnt);
      c2 := c2.parent;
    end;
    try
      if c2 is TForm then
      begin
        if c2.Visible then
          c.SetFocus;
      end
      else
        c.SetFocus;
    except
      Result := false;
    end;
  end;
end;

initialization
  s_alttab_disabled := false;

end.
