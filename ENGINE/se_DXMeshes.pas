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
//  Mesh rendering utilities
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//------------------------------------------------------------------------------

{$I defs.inc}

unit se_DXMeshes;

interface

{$IFNDEF NO_DXMESHES}
uses
  Windows, Classes, se_DirectX, se_DXDraws, se_DXDUtils, se_MyD3DUtils, se_DXClasses,
  se_DXTables {$IFNDEF NO_TEXTUREEFFECTS}, se_DXTextureEffects{$ENDIF};

const
  ZERO: integer = 0;
  TRHEE: integer = 3;
  FOUR: integer = 4;
  CULL_NONE: TD3DCull = D3DCULL_NONE;
  CULL_CW: TD3DCull = D3DCULL_CW;
  CULL_CCW: TD3DCull = D3DCULL_CCW;
  PT_INVALID_0: TD3DPrimitiveType = D3DPT_INVALID_0;
  PT_POINTLIST: TD3DPrimitiveType = D3DPT_POINTLIST;
  PT_LINELIST: TD3DPrimitiveType = D3DPT_LINELIST;
  PT_LINESTRIP: TD3DPrimitiveType = D3DPT_LINESTRIP;
  PT_TRIANGLELIST: TD3DPrimitiveType = D3DPT_TRIANGLELIST;
  PT_TRIANGLESTRIP: TD3DPrimitiveType = D3DPT_TRIANGLESTRIP;
  PT_TRIANGLEFAN: TD3DPrimitiveType = D3DPT_TRIANGLEFAN;

type
  PRTLMesh = ^TRTLMesh;
  TRTLMesh = record
    NumVertexes: PInteger;
    Vertexes: PPD3DVertexArray;
    NumIndices: PInteger;
    Indices: PPIndexesArray;
    PrimitiveType: PD3DPrimitiveType;
    VertexTypeDesc: PDWORD;
    Cull: PD3DCull;
    BoundingCube: PBoundingCube;
    Center: PD3DValue;
    Radius: PD3DValue;
{$IFNDEF NO_TEXTUREEFFECTS}
    TexEffect: PTextureEffect;
{$ENDIF}
    Tex1, Tex2: TDirect3DTexture2;
// if not movable then triangles for vis & collision
// are fixed to bounding cubes / spheres
    Movable: boolean; // true if moves or changes
  end;

procedure GetMeshScript(script: TDXStringList; const m: PRTLMesh; const meshname: string; const stex1, stex2: string);

procedure RenderMesh(const dev: IDirect3DDevice7; const m: PRTLMesh);

procedure RenderMeshWithAlphaEffect(const dev: IDirect3DDevice7; const m: PRTLMesh);

procedure RenderMeshWithStageEffect(const dev: IDirect3DDevice7; const m: PRTLMesh);

function IsDXMeshScriptReserverWord(const Token: string): boolean;

type
  TRenderMeshProc = procedure(const dev: IDirect3DDevice7; const m: PRTLMesh);

{$ENDIF}

implementation

{$IFNDEF NO_DXMESHES}

uses
  SysUtils;

const
  CULL_TABLE: array[0..3] of string = (
    ' // D3DCULL_INVALID_0',
    ' // D3DCULL_NONE',
    ' // D3DCULL_CW',
    ' // D3DCULL_CCW' );

  PRIMITIVE_TABLE: array[0..6] of string = (
    ' // D3DPT_INVALID_0',
    ' // D3DPT_POINTLIST',
    ' // D3DPT_LINELIST',
    ' // D3DPT_LINESTRIP',
    ' // D3DPT_TRIANGLELIST',
    ' // D3DPT_TRIANGLESTRIP',
    ' // D3DPT_TRIANGLEFAN' );

  MSHSCRIPTRESERVEDWORDS: array[0..10] of string = (
    'MESH',
    'BEGIN',
    'END',
    'TEXTURE1',
    'TEXTURE2',
    'PRIMITIVETYPE',
    'CULL',
    'VERTEXTYPEDESC',
    'NUMVERTEXES',
    'NUMINDICES',
    'EFFECT' );


function IsDXMeshScriptReserverWord(const Token: string): boolean;
var
  uToken: string;
  i: integer;
begin
  uToken := UpperCase(Token);
  for i := low(MSHSCRIPTRESERVEDWORDS) to high(MSHSCRIPTRESERVEDWORDS) do
    if uToken = MSHSCRIPTRESERVEDWORDS[i] then
    begin
      result := true;
      exit;
    end;
  result := false;
end;

procedure GetMeshScript(script: TDXStringList; const m: PRTLMesh; const meshname: string; const stex1, stex2: string);
var
  s: TDXStringList;
  sTmp: string;
  i: integer;
  oldSep: char;
begin
  oldSep := decimalSeparator;
  decimalSeparator := '.';
  s := TDXStringList.Create;
  try

{
mesh 'area1'
begin
  primitiveType = 4     // D3DPT_TRIANGLELIST
  VertexTypeDesc = 414  // TD3DVertex
  Cull = 0 // D3DCULL_NONE
  numVertexes 10
    (1.2 1.3 1.8 .... )
    (2.4 3.34 12.3 .... )
    (6 4.03 10.1 .... )
    ...
  numIndices 30
    0 1 2 ....
end
}

    s.Add('mesh "' + meshname + '"');
    s.Add('begin');
    s.Add('  texture1 = "' + stex1 + '"');
    if stex2 <> '' then
      s.Add('  texture2 = "' + stex2 + '"');
      s.Add('  effect = "' + TextureTypeLookUpTable[m.TexEffect.TE_ALPHA.ID].EffectName + '"');
    s.Add('  primitiveType = ' + IntToStrTable(Ord(m.PrimitiveType^)) + PRIMITIVE_TABLE[Ord(m.PrimitiveType^)]);
    s.Add('  cull = ' + IntToStrTable(Ord(m.Cull^)) + CULL_TABLE[Ord(m.Cull^)]);

    if m.VertexTypeDesc^ = D3DFVF_LVERTEX then
      sTmp := ' // TD3DLVERTEX'
    else if m.VertexTypeDesc^ = D3DFVF_VERTEX then
      sTmp := ' // TD3DVERTEX'
    else
      sTmp := '';

    s.Add('  vertexTypeDesc = ' + IntToStrTable(m.VertexTypeDesc^) + sTmp);
    s.Add('  numVertexes = ' + IntToStrTable(m.NumVertexes^));

    if m.VertexTypeDesc^ = D3DFVF_LVERTEX then
    begin
      for i := 0 to m.NumVertexes^ - 1 do
      begin
        s.Add('    (' + FloatToStr(m.Vertexes^[i].x) + ' ' + FloatToStr(m.Vertexes^[i].y) + ' ' + FloatToStr(m.Vertexes^[i].z) + ' ' +
                        IntToStr(TD3DLVertex(m.Vertexes^[i]).color) + ' ' + IntToStr(TD3DLVertex(m.Vertexes^[i]).specular) + ' ' +
                        FloatToStr(m.Vertexes^[i].tu) + ' ' + FloatToStr(m.Vertexes^[i].tv) + ')')
      end
    end
    else
    begin
      for i := 0 to m.NumVertexes^ - 1 do
      begin
        s.Add('    (' + FloatToStr(m.Vertexes^[i].x) + ' ' + FloatToStr(m.Vertexes^[i].y) + ' ' + FloatToStr(m.Vertexes^[i].z) + ' ' +
                        FloatToStr(m.Vertexes^[i].nx) + ' ' + FloatToStr(m.Vertexes^[i].ny) + ' ' + FloatToStr(m.Vertexes^[i].nz) + ' ' +
                        FloatToStr(m.Vertexes^[i].tu) + ' ' + FloatToStr(m.Vertexes^[i].tv) + ')')
      end
    end;

    if m.NumIndices^ > 0 then
    begin
      s.Add('  numIndices = ' + IntToStrTable(m.NumIndices^));
      sTmp := '   ';
      i := 0;
      repeat
        sTmp := sTmp + ' ' + IntToStrTable(i);
        inc(i);
        if (i mod 10 = 0) or (i = m.NumIndices^) then
        begin
          s.Add(sTmp);
          sTmp := '   ';
        end;
      until i = m.NumIndices^;
    end;

    s.Add('end');
    s.Add('');

    script.AddStrings(s);
  finally
    s.Free;
    decimalSeparator := oldSep;
  end;
end;

{$IFNDEF NO_TEXTUREEFFECTS}
procedure RenderMeshWithAlphaEffect(const dev: IDirect3DDevice7; const m: PRTLMesh);
begin
  m.TexEffect.TE_ALPHA.Shaders[0].Args.Texture := m.Tex1;
  m.TexEffect.TE_ALPHA.Shaders[1].Args.Texture := m.Tex2;
  if m.NumIndices^ = 0 then
    TE_ALPH_DrawPrimitive7(m.TexEffect.TE_ALPHA,
      dev,  m.PrimitiveType^, m.VertexTypeDesc^,
        m.Vertexes^[0], m.NumVertexes^)
  else
    TE_ALPH_DrawIndexedPrimitive7(m.TexEffect.TE_ALPHA,
      dev,  m.PrimitiveType^, m.VertexTypeDesc^,
        m.Vertexes^[0], m.NumVertexes^, m.Indices^, m.NumIndices^);
end;

procedure RenderMeshWithStageEffect(const dev: IDirect3DDevice7; const m: PRTLMesh);
begin
  m.TexEffect.TE_STAGE.Shaders[0].Args.Texture := m.Tex1;
  m.TexEffect.TE_STAGE.Shaders[1].Args.Texture := m.Tex2;
  if m.NumIndices^ = 0 then
    TE_STG_DrawPrimitive7(m.TexEffect.TE_STAGE,
      dev,  m.PrimitiveType^,
        PD3DLVertexArray(m.Vertexes^), m.NumVertexes^)
  else
    TE_STG_DrawIndexedPrimitive7(m.TexEffect.TE_STAGE,
      dev,  m.PrimitiveType^,
        PD3DLVertexArray(m.Vertexes^), m.NumVertexes^, m.Indices^, m.NumIndices^);
end;
{$ENDIF}

procedure RenderMesh(const dev: IDirect3DDevice7; const m: PRTLMesh);
begin
  if m.Tex1 <> nil then
    dev.SetTexture(0, m.Tex1.Surface.IDDSurface7)
  else
    dev.SetTexture(0, nil);
  if (m.NumIndices^ = 0) then
    dev.DrawPrimitive(m.PrimitiveType^, m.VertexTypeDesc^,
      m.Vertexes^[0], m.NumVertexes^, 0)
  else
    dev.DrawIndexedPrimitive(m.PrimitiveType^, m.VertexTypeDesc^,
      m.Vertexes^[0], m.NumVertexes^, m.Indices^[0], m.NumIndices^, 0);
end;

{$ENDIF}

end.

