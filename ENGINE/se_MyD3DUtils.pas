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
//  Vector geometry functions
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//------------------------------------------------------------------------------

{$I defs.inc}

unit se_MyD3DUtils;

interface

uses
  Windows, se_DirectX, se_D3DUtils, Graphics;

const
  NULLVECTOR: TD3DVECTOR = (x:0.0; y:0.0; z:0.0);

type
  PD3Duv = ^TD3Duv;
  TD3Duv = record
    u, v: TD3DValue;
  end;

  PD3DuvArray = ^TD3DuvArray;
  TD3DuvArray = array[0..$FFFE] of TD3Duv;

  PD3DuvArrays = ^TD3DuvArrays;
  TD3DuvArrays = array[0..$FFFE] of PD3DuvArray;

  P2DVector = ^T2DVector;
  T2DVector = record
    x, y: TD3DValue;
  end;

  P2DVectorArray = ^T2DVectorArray;
  T2DVectorArray = array [0..$FFFE] of T2DVector;

// Για σφαίρες κλπ...
  PD3DVectorArray = ^TD3DVectorArray;
  TD3DVectorArray = array[0..$FFFE] of TD3DVector;

  PD3DVectorArrays = ^TD3DVectorArrays;
  TD3DVectorArrays = array[0..$FFFE] of PD3DVectorArray;

  PD3DLVertexArray = ^TD3DLVertexArray;
  TD3DLVertexArray = array[0..$FFFE] of TD3DLVertex;

  PD3DLVertexArrays = ^TD3DLVertexArrays;
  TD3DLVertexArrays = array[0..$FFFE] of PD3DLVertexArray;

  // Simple Vertex
  PD3DSVertex = ^TD3DSVertex;
  TD3DSVertex = record
    x: TD3DValue;             // Homogeneous coordinates
    y: TD3DValue;
    z: TD3DValue;
    tu: TD3DValue;            // Texture coordinates
    tv: TD3DValue;
  end;

  PD3DSVertexArray = ^TD3DSVertexArray;
  TD3DSVertexArray = array[0..$FFFE] of TD3DSVertex;

  PD3DSVertexArrays = ^TD3DSVertexArrays;
  TD3DSVertexArrays = array[0..$FFFE] of PD3DSVertexArray;


type
  PDWORD = ^DWORD;


type
  TIndexesArray = packed array [0..$FFFF] of word;
  PIndexesArray = ^TIndexesArray;

  TD3DVertexArray = array[0..$FFFF] of TD3DVertex;

  PD3DVertexArray = ^TD3DVertexArray;
  PPD3DVertexArray = ^PD3DVertexArray;
  PPIndexesArray = ^PIndexesArray;

  PD3DCull = ^TD3DCull;
  PD3DPrimitiveType = ^TD3DPrimitiveType;

  PD3DValue = ^TD3DValue;

function Make2DVector(x, y: TD3DValue): T2DVector;

function VectorAngle(const V1, V2: TD3DVector): Single; assembler;

procedure SwapVertex(var v1, v2: TD3DVertex);

procedure MulMatrix(var Mout: TD3DMatrix; Min1, Min2: TD3DMatrix); overload;

function MulMatrix(v1: TD3DVector; Mat1: TD3DMatrix): TD3DVector; overload;

function RotateVector(v1: TD3DVector; x, y, z: TD3DValue): TD3DVector; overload;

function RotateVector(v1: TD3DVector; vR: TD3DVector): TD3DVector; overload;

procedure GetRotationMatrix(out mat: TD3DMatrix; x, y, z: TD3DValue; aspect: TD3DValue=1.0);

procedure GetScaleMatrix(out mat: TD3DMatrix; x, y, z: TD3DValue; aspect: TD3DValue=1.0); overload;

procedure GetScaleMatrix(out mat: TD3DMatrix; v: TD3DVector); overload;

procedure GetTranslationMatrix(out mat: TD3DMatrix; x, y, z: TD3DValue; aspect: TD3DValue=1.0);

procedure SinCosS(const Theta: Single; var Sin, Cos: Single); register;

function ArcCosS(X: Single): Single;

function ArcSinS(X: Single): Single;

function ArcTan2S(const y, x : Single): Single;

function RoundS(const X: Single): Integer; register;

function TruncS(const X: Single): Integer; register;

function FracS(const X: Single): Single; register;

function MakeD3DLVertex(const hv: TD3DVector; const cC, cS: TD3DColor; const tu, tv: TD3DValue): TD3DLVertex; overload;

function MakeD3DLVertex(const hv: TD3DVector; const uv: TD3Duv): TD3DLVertex; overload;

function MakeD3DLVertex(const x, y, z: TD3DValue; const cC, cS: TD3DColor; const tu, tv: TD3DValue): TD3DLVertex; overload;

function MakeD3DLVertex(const V1, V2: TD3DLVertex; const w2: TD3DValue): TD3DLVertex; overload;

function MakeD3DLVertex(const V1, V2: TD3DSVertex; const w2: TD3DValue): TD3DLVertex; overload;

function MakeD3DLVertex(const V1, V2: TD3DVector; const w2: TD3DValue; const uv: TD3Duv): TD3DLVertex; overload;

function MakeD3DLVertex(const V1: TD3DSVertex): TD3DLVertex; overload;

function MakeD3DSVertex(const x, y, z: TD3DValue; const tu, tv: TD3DValue): TD3DSVertex;

function CA_MAKE(const c: TColor; const a: Byte): TD3DColor;

function NegativeVector(const v: TD3DVector): TD3DVector;

function Sign(x: TD3DValue): TD3DValue;

procedure D3D_VectorMatrixMultiply_Only_For_Sphere(var xDest, yDest, zDest: TD3DValue;
  const xSrc, ySrc, zSrc: TD3DValue;
  const mat: TD3DMatrix); register;

procedure D3D_VectorMatrixMultiply_Only_For_SphereRotation(var xDest, yDest, zDest: TD3DValue;
  const xSrc, ySrc, zSrc: TD3DValue;
  const mat: TD3DMatrix); register;

implementation

{ Calculates the cosine of the angle between Vector1 and Vector2.<p> }
function VectorAngle(const V1, V2: TD3DVector): Single; assembler;
// Result = DotProduct(V1, V2) / (Length(V1) * Length(V2)) }
// EAX contains address of Vector1
// EDX contains address of Vector2
asm
  FLD DWORD PTR [EAX]     // V1[0]
  FLD ST                  // double V1[0]
  FMUL ST, ST             // V1[0]^2 (prep. for divisor)
  FLD DWORD PTR [EDX]     // V2[0]
  FMUL ST(2), ST          // ST(2):=V1[0] * V2[0]
  FMUL ST, ST             // V2[0]^2 (prep. for divisor)
  FLD DWORD PTR [EAX + 4] // V1[1]
  FLD ST                  // double V1[1]
  FMUL ST, ST             // ST(0):=V1[1]^2
  FADDP ST(3), ST         // ST(2):=V1[0]^2 + V1[1] *  * 2
  FLD DWORD PTR [EDX + 4] // V2[1]
  FMUL ST(1), ST          // ST(1):=V1[1] * V2[1]
  FMUL ST, ST             // ST(0):=V2[1]^2
  FADDP ST(2), ST         // ST(1):=V2[0]^2 + V2[1]^2
  FADDP ST(3), ST         // ST(2):=V1[0] * V2[0] + V1[1] * V2[1]
  FLD DWORD PTR [EAX + 8] // load V2[1]
  FLD ST                  // same calcs go here
  FMUL ST, ST             // (compare above)
  FADDP ST(3), ST
  FLD DWORD PTR [EDX + 8]
  FMUL ST(1), ST
  FMUL ST, ST
  FADDP ST(2), ST
  FADDP ST(3), ST
  FMULP                   // ST(0):=(V1[0]^2 + V1[1]^2 + V1[2]) *
                          //          (V2[0]^2 + V2[1]^2 + V2[2])
  FSQRT                   // sqrt(ST(0))
  FDIVP                   // ST(0):=Result:=ST(1) / ST(0)
  // the result is expected in ST(0), if it's invalid, an error is raised
end;

procedure SwapVertex(var v1, v2: TD3DVertex);
var vertex: TD3DVertex; // for swaping!
begin
  vertex := v1;
  v1 := v2;
  v2 := vertex;
end;

procedure MulMatrix(var Mout: TD3DMatrix; Min1, Min2: TD3DMatrix);
begin
  Mout._11 := Min1._11 * Min2._11 + Min1._12 * Min2._21 + Min1._13 * Min2._31 + Min1._14 * Min2._41;
  Mout._12 := Min1._11 * Min2._12 + Min1._12 * Min2._22 + Min1._13 * Min2._32 + Min1._14 * Min2._42;
  Mout._13 := Min1._11 * Min2._13 + Min1._12 * Min2._23 + Min1._13 * Min2._33 + Min1._14 * Min2._43;
  Mout._14 := Min1._11 * Min2._14 + Min1._12 * Min2._24 + Min1._13 * Min2._34 + Min1._14 * Min2._44;

  Mout._21 := Min1._21 * Min2._11 + Min1._22 * Min2._21 + Min1._23 * Min2._31 + Min1._24 * Min2._41;
  Mout._22 := Min1._21 * Min2._12 + Min1._22 * Min2._22 + Min1._23 * Min2._32 + Min1._24 * Min2._42;
  Mout._23 := Min1._21 * Min2._13 + Min1._22 * Min2._23 + Min1._23 * Min2._33 + Min1._24 * Min2._43;
  Mout._24 := Min1._21 * Min2._14 + Min1._22 * Min2._24 + Min1._23 * Min2._34 + Min1._24 * Min2._44;

  Mout._31 := Min1._31 * Min2._11 + Min1._32 * Min2._21 + Min1._33 * Min2._31 + Min1._34 * Min2._41;
  Mout._32 := Min1._31 * Min2._12 + Min1._32 * Min2._22 + Min1._33 * Min2._32 + Min1._34 * Min2._42;
  Mout._33 := Min1._31 * Min2._13 + Min1._32 * Min2._23 + Min1._33 * Min2._33 + Min1._34 * Min2._43;
  Mout._34 := Min1._31 * Min2._14 + Min1._32 * Min2._24 + Min1._33 * Min2._34 + Min1._34 * Min2._44;

  Mout._41 := Min1._41 * Min2._11 + Min1._42 * Min2._21 + Min1._43 * Min2._31 + Min1._44 * Min2._41;
  Mout._42 := Min1._41 * Min2._12 + Min1._42 * Min2._22 + Min1._43 * Min2._32 + Min1._44 * Min2._42;
  Mout._43 := Min1._41 * Min2._13 + Min1._42 * Min2._23 + Min1._43 * Min2._33 + Min1._44 * Min2._43;
  Mout._44 := Min1._41 * Min2._14 + Min1._42 * Min2._24 + Min1._43 * Min2._34 + Min1._44 * Min2._44;
end;

function MulMatrix(v1: TD3DVector; Mat1: TD3DMatrix): TD3DVector;
begin
  result.x := v1.x * Mat1._11 + v1.y * Mat1._21 + v1.z * Mat1._31;
  result.y := v1.x * Mat1._12 + v1.y * Mat1._22 + v1.z * Mat1._32;
  result.z := v1.x * Mat1._13 + v1.y * Mat1._23 + v1.z * Mat1._33;
end;

function RotateVector(v1: TD3DVector; x, y, z: TD3DValue): TD3DVector;
var Mat: TD3DMatrix;
begin
  GetRotationMatrix(Mat, x, y, z);
  D3DMath_VectorMatrixMultiply(result, v1, Mat);
end;

function RotateVector(v1: TD3DVector; vR: TD3DVector): TD3DVector;
var Mat: TD3DMatrix;
begin
  GetRotationMatrix(Mat, vR.x, vR.y, vR.z);
  D3DMath_VectorMatrixMultiply(result, v1, Mat);
end;

procedure GetRotationMatrix(out mat: TD3DMatrix; x, y, z: TD3DValue; aspect: TD3DValue=1.0);
var matRotateX, matRotateY, matRotateZ: TD3DMatrix;
begin
// Setup the Rotation Matrix (X Axis)
  FillChar(matRotateX, SizeOf(matRotateX), 0);
  matRotateX._11 := aspect; // e.g.: Screen Aspect Ratio
  matRotateX._22 := cos(x);
  matRotateX._23 := sin(x);
  matRotateX._32 := -sin(x);
  matRotateX._33 := cos(x);
  matRotateX._44 := 1.0;

// Setup the Rotation Matrix (Y Axis)
  FillChar(matRotateY, SizeOf(matRotateY), 0);
  matRotateY._11 := cos(-y);
  matRotateY._13 := -sin(-y);
  matRotateY._22 := 1.0;
  matRotateY._31 := sin(-y);
  matRotateY._33 := cos(-y);
  matRotateY._44 := 1.0;

  // Setup the Rotation Matrix (Z Axis)
  FillChar(matRotateZ, SizeOf(matRotateZ), 0);
  matRotateZ._11 := cos(z);
  matRotateZ._12 := sin(z);
  matRotateZ._21 := -sin(z);
  matRotateZ._22 := cos(z);
  matRotateZ._33 := 1.0;
  matRotateZ._44 := 1.0;

// Compine the matrixes
  MulMatrix(mat, matRotateY, matRotateX);
  MulMatrix(mat, matRotateZ, mat);

end;

procedure GetScaleMatrix(out mat: TD3DMatrix; x, y, z: TD3DValue; aspect: TD3DValue=1.0);
begin
  FillChar(mat, SizeOf(mat), Chr(0));
  mat._11 := x;
  mat._22 := y;
  mat._33 := z;
  mat._44 := aspect;
end;

procedure GetScaleMatrix(out mat: TD3DMatrix; v: TD3DVector); overload;
begin
  FillChar(mat, SizeOf(mat), Chr(0));
  mat._11 := v.x;
  mat._22 := v.y;
  mat._33 := v.z;
  mat._44 := 1.0;
end;

procedure GetTranslationMatrix(out mat: TD3DMatrix; x, y, z: TD3DValue; aspect: TD3DValue=1.0);
begin
  D3DUtil_SetIdentityMatrix(mat);
  mat._41 := x;
  mat._42 := y;
  mat._43 := z;
  mat._44 := aspect;
end;

function ArcCosS(X: Single): Single;
begin
  Result := ArcTan2S(Sqrt(1 - X*X), X);
end;

function ArcSinS(X: Single): Single;
begin
  Result := ArcTan2S(X, Sqrt(1 - X*X))
end;

function ArcTan2S(const y, x : Single): Single;
asm
      FLD  Y
      FLD  X
      FPATAN
end;

procedure SinCosS(const Theta: Single; var Sin, Cos: Single); register;
// EAX contains address of Sin
// EDX contains address of Cos
// Theta is passed over the stack
asm
   FLD  Theta
   FSINCOS
   FSTP DWORD PTR [EDX]    // cosine
   FSTP DWORD PTR [EAX]    // sine
end;

function TruncS(const X: Single): Integer; register;
const
  cwChop: Word = $1F3F;
asm
      SUB     ESP,8
      FSTCW   [ESP]
      FLDCW   cwChop
      FLD     x
      FISTP   dword ptr [ESP+4]
      FLDCW   [ESP]
      POP     ECX
      POP     EAX
end;

function FracS(const X: Single): Single; register;
begin
  Result := X - Trunc(X);
end;

function RoundS(const X: Single): Integer; register;
asm
      SUB     ESP,4
      FLD     x
      FISTP   dword ptr [ESP]
      POP     EAX
end;

function MakeD3DLVertex(const hv: TD3DVector; const cC, cS: TD3DColor; const tu, tv: TD3DValue): TD3DLVertex;
begin
  result.x := hv.x;
  result.y := hv.y;
  result.z := hv.z;
  result.dwReserved := 0;
  result.Color := cC;
  result.Specular := cS;
  result.tu := tu;
  result.tv := tv;
end;

function MakeD3DLVertex(const hv: TD3DVector; const uv: TD3Duv): TD3DLVertex;
begin
  result.dwReserved := 0;
  result.color := CA_MAKE($FFFFFF, 0);
  result.specular := 0;

  result.x := hv.x;
  result.y := hv.y;
  result.z := hv.z;
  result.tu := uv.u;
  result.tv := uv.v;
end;

// Α bit faster ???
function MakeD3DLVertex(const x, y, z: TD3DValue; const cC, cS: TD3DColor; const tu, tv: TD3DValue): TD3DLVertex; overload;
begin
  result.x := x;
  result.y := y;
  result.z := z;
  result.dwReserved := 0;
  result.Color := cC;
  result.Specular := cS;
  result.tu := tu;
  result.tv := tv;
end;

// Επιστρέφει ένα Vertex που προέρχεται από τον συνδιασμό των
// V1 & V2 ως [V1 * (1 - w2) + V2 * w2]
// w2: Βάρος της παραμέτρου V2
function MakeD3DLVertex(const V1, V2: TD3DLVertex; const w2: TD3DValue): TD3DLVertex; overload;
var w1: TD3DValue;
    r1, g1, b1, a1: byte;
    r2, g2, b2, a2: byte;
begin
  if w2 <= g_EPSILON then
    result := V1
  else if (w2 + g_EPSILON) >= 1.0 then
    result := V2
  else
  begin

    w1 := 1.0 - w2;
    result.x := V1.x * w1 + V2.x * w2;
    result.y := V1.y * w1 + V2.y * w2;
    result.z := V1.z * w1 + V2.z * w2;
    result.dwReserved := 0;

    if V1.color <> V2.color then
    begin
      r1 := RGBA_GETRED(V1.color);
      g1 := RGBA_GETGREEN(V1.color);
      b1 := RGBA_GETBLUE(V1.color);
      a1 := RGBA_GETALPHA(V1.color);
      r2 := RGBA_GETRED(V2.color);
      g2 := RGBA_GETGREEN(V2.color);
      b2 := RGBA_GETBLUE(V2.color);
      a2 := RGBA_GETALPHA(V2.color);

      result.Color :=
        RGBA_MAKE(
          trunc(r1 * w1 + r2 * w2),
          trunc(g1 * w1 + g2 * w2),
          trunc(b1 * w1 + b2 * w2),
          trunc(a1 * w1 + a2 * w2));
    end
    else
      result.Color := V1.Color;

    if V1.specular <> V2.specular then
    begin
      r1 := RGBA_GETRED(V1.specular);
      g1 := RGBA_GETGREEN(V1.specular);
      b1 := RGBA_GETBLUE(V1.specular);
      a1 := RGBA_GETALPHA(V1.specular);
      r2 := RGBA_GETRED(V2.specular);
      g2 := RGBA_GETGREEN(V2.specular);
      b2 := RGBA_GETBLUE(V2.specular);
      a2 := RGBA_GETALPHA(V2.specular);

      result.specular :=
        RGBA_MAKE(
          trunc(r1 * w1 + r2 * w2),
          trunc(g1 * w1 + g2 * w2),
          trunc(b1 * w1 + b2 * w2),
          trunc(a1 * w1 + a2 * w2));
    end
    else
      result.specular := V1.specular;

    result.tu := V1.tu * w1 + V2.tu * w2;
    result.tv := V1.tv * w1 + V2.tv * w2;
  end;
end;

function MakeD3DLVertex(const V1, V2: TD3DVector; const w2: TD3DValue; const uv: TD3Duv): TD3DLVertex;
var w1: TD3DValue;
begin
  result.dwReserved := 0;
  result.color := CA_MAKE($FFFFFF, 0);
  result.specular := 0;
  result.tu := uv.u;
  result.tv := uv.v;
  if w2 <= g_EPSILON then
  begin
    result.x  := V1.x;
    result.y  := V1.y;
    result.z  := V1.z;
  end
  else if (w2 + g_EPSILON) >= 1.0 then
  begin
    result.x  := V2.x;
    result.y  := V2.y;
    result.z  := V2.z;
  end
  else
  begin
    w1 := 1.0 - w2;

    result.x  := V1.x * w1 + V2.x * w2;
    result.y  := V1.y * w1 + V2.y * w2;
    result.z  := V1.z * w1 + V2.z * w2;
  end;
end;

function MakeD3DLVertex(const V1, V2: TD3DSVertex; const w2: TD3DValue): TD3DLVertex;
var w1: TD3DValue;
begin
  result.dwReserved := 0;
  result.color := CA_MAKE($FFFFFF, 0);
  result.specular := 0;
  if w2 <= g_EPSILON then
  begin
    result.x  := V1.x;
    result.y  := V1.y;
    result.z  := V1.z;
    result.tu := V1.tu;
    result.tv := V1.tv;
  end
  else if (w2 + g_EPSILON) >= 1.0 then
  begin
    result.x  := V2.x;
    result.y  := V2.y;
    result.z  := V2.z;
    result.tu := V2.tu;
    result.tv := V2.tv;
  end
  else
  begin
    w1 := 1.0 - w2;

    result.x  := V1.x * w1 + V2.x * w2;
    result.y  := V1.y * w1 + V2.y * w2;
    result.z  := V1.z * w1 + V2.z * w2;
    result.tu := V1.tu * w1 + V2.tu * w2;
    result.tv := V1.tv * w1 + V2.tv * w2;
  end;
end;

function MakeD3DLVertex(const V1: TD3DSVertex): TD3DLVertex;
begin
  result.dwReserved := 0;
  result.color := CA_MAKE($FFFFFF, 0);
  result.specular := 0;

  result.x  := V1.x;
  result.y  := V1.y;
  result.z  := V1.z;
  result.tu := V1.tu;
  result.tv := V1.tv;
end;

function MakeD3DSVertex(const x, y, z: TD3DValue; const tu, tv: TD3DValue): TD3DSVertex;
begin
  result.x  := x;
  result.y  := y;
  result.z  := z;
  result.tu := tu;
  result.tv := tv;
end;

// Επιστρέφει το χρώμα C με Alpha Channel = a
function CA_MAKE(const c: TColor; const a: Byte): TD3DColor;
begin
  Result := RGBA_MAKE(GetRValue(c), GetGValue(c), GetBValue(c), a);
end;

function NegativeVector(const v: TD3DVector): TD3DVector;
begin
  result.x := -v.x;
  result.y := -v.y;
  result.z := -v.z;
end;

function Sign(x: TD3DValue): TD3DValue;
begin
  if x > g_EPSILON then
    result := 1.0
  else if x < -g_EPSILON then
    result := -1.0
  else
    result := 0.0;
end;

// Avoid usage of stack;
var
  D3D_VectorMatrixMultiply_Only_For_Sphere_x,
  D3D_VectorMatrixMultiply_Only_For_Sphere_y,
  D3D_VectorMatrixMultiply_Only_For_Sphere_z,
  D3D_VectorMatrixMultiply_Only_For_Sphere_w: TD3DValue;

procedure D3D_VectorMatrixMultiply_Only_For_Sphere(var xDest, yDest, zDest: TD3DValue;
  const xSrc, ySrc, zSrc: TD3DValue;
  const mat: TD3DMatrix);
begin
  D3D_VectorMatrixMultiply_Only_For_Sphere_x :=
    xSrc*mat._11 + ySrc*mat._21 + zSrc* mat._31;
  D3D_VectorMatrixMultiply_Only_For_Sphere_y :=
    xSrc*mat._12 + ySrc*mat._22 + zSrc* mat._32;
  D3D_VectorMatrixMultiply_Only_For_Sphere_z :=
    xSrc*mat._13 + ySrc*mat._23 + zSrc* mat._33;
  D3D_VectorMatrixMultiply_Only_For_Sphere_w :=
    xSrc*mat._14 + ySrc*mat._24 + zSrc* mat._34 + mat._44;

  xDest := D3D_VectorMatrixMultiply_Only_For_Sphere_x/D3D_VectorMatrixMultiply_Only_For_Sphere_w;
  yDest := D3D_VectorMatrixMultiply_Only_For_Sphere_y/D3D_VectorMatrixMultiply_Only_For_Sphere_w;
  zDest := D3D_VectorMatrixMultiply_Only_For_Sphere_z/D3D_VectorMatrixMultiply_Only_For_Sphere_w;

end;

procedure D3D_VectorMatrixMultiply_Only_For_SphereRotation(var xDest, yDest, zDest: TD3DValue;
  const xSrc, ySrc, zSrc: TD3DValue;
  const mat: TD3DMatrix); register;
begin
  xDest := (xSrc*mat._11 + ySrc*mat._21 + zSrc* mat._31);///mat._44;
  yDest := (xSrc*mat._12 + ySrc*mat._22 + zSrc* mat._32);///mat._44;
  zDest := (xSrc*mat._13 + ySrc*mat._23 + zSrc* mat._33);///mat._44;
end;

function Make2DVector(x, y: TD3DValue): T2DVector;
begin
  result.x := x;
  result.y := y;
end;

end.
