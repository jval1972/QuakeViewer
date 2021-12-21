// DelphiX - Modified for SE
unit se_DXTextureEffects;

interface

uses
  Windows, Classes, se_DirectX, se_DXDraws, se_MyD3DUtils;

////////////////////////////////////////////////////////////////////////////////
// ALPHA BLEND EFFECTS
////////////////////////////////////////////////////////////////////////////////
const
  MAXTEXTUREALPHA = 2;

type
  PTextureAlphaEffectArgs = ^TTextureAlphaEffectArgs;
  TTextureAlphaEffectArgs = record
    SrcBlend: DWORD;
    DestBlend: DWORD;
    Texture: TDirect3DTexture2;
  end;

  PTextureAlphaEffectShader = ^TTextureAlphaEffectShader;
  TTextureAlphaEffectShader = record
    Args: TTextureAlphaEffectArgs;
    Enabled: boolean;
  end;

  PTextureAlphaEffect = ^TTextureAlphaEffect;
  TTextureAlphaEffect = record
    ID: integer;
    ColorKeyEnable: boolean;
    AlphaFunc: DWORD;
    AlphaTestEnable: boolean;
    AlphaRef: DWORD;
    Shaders: array[0..MAXTEXTUREALPHA - 1] of TTextureAlphaEffectShader;
  end;

////////////////////////////////////////////////////////////////////////////////
// Initalize the TextureStage of a device (simply disables alpla blending)
function TE_ALPH_InitDeviceDefaults7(const dev: IDirect3DDevice7): HRESULT;


////////////////////////////////////////////////////////////////////////////////
// Initalize the TextureStage of a device as does DXDesigner Application
function TE_ALPH_InitDXDesignerDefaults7(const dev: IDirect3DDevice7): HRESULT;


////////////////////////////////////////////////////////////////////////////////
// Creates a default (empty) alpha effect
procedure TE_ALPH_CreateDeviceDefault(var TE_ALPH: TTextureAlphaEffect);


////////////////////////////////////////////////////////////////////////////////
// Creates a default (empty) alpha effect as described in DXDesigner application
procedure TE_ALPH_CreateDXDesignerDefault(var TE_ALPH: TTextureAlphaEffect);


////////////////////////////////////////////////////////////////////////////////
// Prepares the device for the alpha effect
function TE_ALPH_PrepareDevice7(const TE_ALPH: TTextureAlphaEffect;
  const dev: IDirect3DDevice7): HRESULT;


////////////////////////////////////////////////////////////////////////////////
// Draws vertexes with alpha TextureEffect
function TE_ALPH_DrawPrimitive7(const TE_ALPH: TTextureAlphaEffect;
  const dev: IDirect3DDevice7;
  d3dptPrimitiveType: TD3DPrimitiveType; dwVertexTypeDesc: DWORD;
  const lpvVertices; dwVertexCount: DWORD): HRESULT;


////////////////////////////////////////////////////////////////////////////////
// Draws vertexes with alpha TextureEffect (indexed)
function TE_ALPH_DrawIndexedPrimitive7(const TE_ALPH: TTextureAlphaEffect;
  const dev: IDirect3DDevice7;
  d3dptPrimitiveType: TD3DPrimitiveType; dwVertexTypeDesc: DWORD;
  const lpvVertices; dwVertexCount: DWORD;
  const lpwIndices: PIndexesArray; dwIndexCount: DWORD): HRESULT;


////////////////////////////////////////////////////////////////////////////////
// TEXTUREALPHA PREDEFINED EFFECTS
////////////////////////////////////////////////////////////////////////////////
type
  TAlphaCreateProc = procedure (var TE_ALPH: TTextureAlphaEffect);

procedure TE_ALPH_CreateLightFromFlare(var TE_ALPH: TTextureAlphaEffect);


procedure TE_ALPH_SeeThrouGlass(var TE_ALPH: TTextureAlphaEffect);


procedure TE_ALPH_CreateDoom3Light(var TE_ALPH: TTextureAlphaEffect);


procedure TE_ALPH_CreateDoom3LightCompinated(var TE_ALPH: TTextureAlphaEffect);


////////////////////////////////////////////////////////////////////////////////
// TEXTURESTAGE EFFECTS
////////////////////////////////////////////////////////////////////////////////
const
  MAXTEXTURESTAGES = 3; // Change this for more, maybe unneeded


type
// Arguments for SetTextureStage
  PTextureStageEffectArgs = ^TTextureStageEffectArgs;
  TTextureStageEffectArgs = record
    ColorArg1,          // dev.SetTextureStageState(stage, D3DTSS_COLORARG1, xx)
    ColorOp,            // dev.SetTextureStageState(stage, D3DTSS_COLOROP, xx)
    ColorArg2: integer; // dev.SetTextureStageState(stage, D3DTSS_COLORARG2, xx)
    AlphaArg1,          // dev.SetTextureStageState(stage, D3DTSS_ALPHAARG1, xx)
    AlphaOp,            // dev.SetTextureStageState(stage, D3DTSS_ALPHAOP, xx)
    AlphaArg2: integer; // dev.SetTextureStageState(stage, D3DTSS_ALPHAARG2, xx)
    MinFilter,          // dev.SetTextureStageState(stage, D3DTSS_MINFILTER, xx)
    MagFilter: integer; // dev.SetTextureStageState(stage, D3DTSS_MAGFILTER, xx)
    Texture: TDirect3DTexture2;   // dev.SetTexture(stage, Texture.Surface.IDDSurface7)
  end;

// Shader Effect
// Texture, Arguments, enabled
  PTextureStageEffectShader = ^TTextureStageEffectShader;
  TTextureStageEffectShader = record
    Args: TTextureStageEffectArgs;
    Enabled: boolean;
  end;


// Device Shader Effects
// TextureFactor, Color of TD3DLVertexes, Shaders
  PTextureStageEffect = ^TTextureStageEffect;
  TTextureStageEffect = record
    ID: integer;
    TextureFactor: DWORD;
    DiffuseColor: TD3DColor;
    Shaders: array[0..MAXTEXTURESTAGES - 1] of TTextureStageEffectShader;
  end;

// Texture Shader procedures
  TE_STG_ShaderProc7 = procedure(const dev: IDirect3DDevice7);
  TE_STG_ShaderProc = TE_STG_ShaderProc7;

////////////////////////////////////////////////////////////////////////////////
// Initalize the TextureStage of a device
function TE_STG_InitDeviceDefaults7(const dev: IDirect3DDevice7): HRESULT;


////////////////////////////////////////////////////////////////////////////////
// Initalize the TextureStage of a device as does DXDesigner Application
function TE_STG_InitDXDesignerDefaults7(const dev: IDirect3DDevice7): HRESULT;


////////////////////////////////////////////////////////////////////////////////
// Creates a default (empty) stage effect
procedure TE_STG_CreateDeviceDefault(var TE_STG: TTextureStageEffect);


////////////////////////////////////////////////////////////////////////////////
// Creates a default (empty) stage effect as described in DXDesigner application
procedure TE_STG_CreateDXDesignerDefault(var TE_STG: TTextureStageEffect);


////////////////////////////////////////////////////////////////////////////////
// Disables additional texture stages of a device
function TE_STG_DisableMultiTexturing7(const dev: IDirect3DDevice7): HRESULT;


////////////////////////////////////////////////////////////////////////////////
// Initalize the vertexes to the diffuse color of the TextureStageEffect
procedure TE_STG_PrepareVertexes(const TE_STG: TTextureStageEffect;
  const lpvVertices: PD3DLVertexArray; dwVertexCount: DWORD);


////////////////////////////////////////////////////////////////////////////////
// Prepares the device for the effect
function TE_STG_PrepareDevice7(const TE_STG: TTextureStageEffect;
  const dev: IDirect3DDevice7): HRESULT;


////////////////////////////////////////////////////////////////////////////////
// Draws vertexes with stage TextureEffect
function TE_STG_DrawPrimitive7(const TE_STG: TTextureStageEffect;
  const dev: IDirect3DDevice7; d3dptPrimitiveType: TD3DPrimitiveType;
  const lpvVertices: PD3DLVertexArray; dwVertexCount: DWORD): HRESULT;


////////////////////////////////////////////////////////////////////////////////
// Draws vertexes with stage TextureEffect (indexed)
function TE_STG_DrawIndexedPrimitive7(const TE_STG: TTextureStageEffect;
  const dev: IDirect3DDevice7; d3dptPrimitiveType: TD3DPrimitiveType;
  const lpvVertices: PD3DLVertexArray; dwVertexCount: DWORD;
  const lpwIndices: PIndexesArray; dwIndexCount: DWORD): HRESULT;


////////////////////////////////////////////////////////////////////////////////
// Helper functions
procedure TE_STG_UpdateStageColor(var TE_STG: TTextureStageEffect;
  stage: integer; op, arg1, arg2: integer);

procedure TE_STG_UpdateStageAlpha(var TE_STG: TTextureStageEffect;
  stage: integer; op, arg1, arg2: integer);

procedure TE_STG_UpdateMinMagFilters(var TE_STG: TTextureStageEffect;
  const minf, magf: integer);

////////////////////////////////////////////////////////////////////////////////
// EFFECTS
type
  TStageCreateProc = procedure (var TE_ALPH: TTextureStageEffect);

procedure TE_STG_CreateModulate(var TE_STG: TTextureStageEffect);

procedure TE_STG_CreateModulateAlpha(var TE_STG: TTextureStageEffect);

procedure TE_STG_CreateAdd(var TE_STG: TTextureStageEffect);

procedure TE_STG_CreateDecalAlpha(var TE_STG: TTextureStageEffect);

procedure TE_STG_CreateColoredLightMap(var TE_STG: TTextureStageEffect);

procedure TE_STG_CreateInverseColoredLightMap(var TE_STG: TTextureStageEffect);

procedure TE_STG_CreateSingleChannelLightMap(var TE_STG: TTextureStageEffect);

procedure TE_STG_CreateModulateAndLateAdd(var TE_STG: TTextureStageEffect);

procedure TE_STG_CreateLinearBlendUsingTextureAlpha(var TE_STG: TTextureStageEffect);

procedure TE_STG_CreateLinearBlendUsingDiffuseAlpha(var TE_STG: TTextureStageEffect);

procedure TE_STG_CreateMultiTextureAndSmoothSaturation(var TE_STG: TTextureStageEffect);

procedure TE_STG_CreateMultiTextureSubTract(var TE_STG: TTextureStageEffect);

procedure TE_STG_CreateAddDiffuseToLightAndThenModulate(var TE_STG: TTextureStageEffect);

procedure TE_STG_CreateDetailModulate(var TE_STG: TTextureStageEffect);

procedure TE_STG_CreateDetailAdd(var TE_STG: TTextureStageEffect);

const
  TE_EFFECT_NONE  = 0;
  TE_EFFECT_ALPHA = 1;
  TE_EFFECT_STAGE = 2;

const
  STGModulate = 1;
  STGModulateAlpha = 2;
  STGAdd = 3;
  STGDecalAlpha = 4;
  STGColoredLightMap = 5;
  STGInverseColoredLightMap = 6;
  STGSingleChannelLightMap = 7;
  STGModulateAndLateAdd = 8;
  STGLinearBlendUsingTextureAlpha = 9;
  STGLinearBlendUsingDiffuseAlpha = 10;
  STGMultiTextureAndSmoothSaturation = 11;
  STGMultiTextureSubTract = 12;
  STGAddDiffuseToLightAndThenModulate = 13;
  STGDetailModulate = 14;
  STGDetailAdd = 15;
  ALPHLightFromFlare = 16;
  ALPHSeeThrouGlass = 17;
  ALPHDoom3Light = 18;
  ALPHDoom3LightCompinated = 19;
  MaxEffectID = 20;

type
  TTextureTypeLookUp = record
    EffectType: integer;
    EffectID: integer;
    EffectName: string;
    ALPHA_CREATOR: TAlphaCreateProc;
    STAGE_CREATOR: TStageCreateProc;
  end;

var
  TextureTypeLookUpTable: array[0..MaxEffectID - 1] of TTextureTypeLookUp;


type
  PTextureEffect = ^TTextureEffect;
  TTextureEffect = record
    case EffectType: integer of
      TE_EFFECT_ALPHA:(TE_ALPHA: TTextureAlphaEffect);
      TE_EFFECT_STAGE:(TE_STAGE: TTextureStageEffect);
  end;

procedure CreateEffectFromID(const id: integer; var E: TTextureEffect);

procedure CreateEffectFromName(const name: string; var E: TTextureEffect);

////////////////////////////////////////////////////////////////////////////////
// Streaming rutines
procedure LoadEffectFromStream(s: TStream; var E: TTextureEffect);

procedure SaveEffectToStream(s: TStream; const E: TTextureEffect);

implementation

uses SysUtils;

////////////////////////////////////////////////////////////////////////////////
// ALPHA BLEND EFFECTS
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
// Initalize the TextureStage of a device (simply disables alpla blending)
function TE_ALPH_InitDeviceDefaults7(const dev: IDirect3DDevice7): HRESULT;
var dwNumPasses: DWORD;
begin
  dev.SetRenderState( D3DRENDERSTATE_ALPHABLENDENABLE, 0);
  dev.SetRenderState( D3DRENDERSTATE_ALPHAFUNC, Ord(D3DCMP_ALWAYS) );
  dev.SetRenderState( D3DRENDERSTATE_ALPHATESTENABLE, Ord(false) );
  dev.SetRenderState( D3DRENDERSTATE_COLORKEYENABLE, 1);
  result := dev.ValidateDevice(dwNumPasses);
end;

////////////////////////////////////////////////////////////////////////////////
// Initalize the TextureStage of a device as does DXDesigner Application
function TE_ALPH_InitDXDesignerDefaults7(const dev: IDirect3DDevice7): HRESULT;
var dwNumPasses: DWORD;
begin
  dev.SetRenderState( D3DRENDERSTATE_ALPHABLENDENABLE, 0); // Same as above
  dev.SetRenderState( D3DRENDERSTATE_ALPHAFUNC, Ord(D3DCMP_ALWAYS) );
  dev.SetRenderState( D3DRENDERSTATE_ALPHATESTENABLE, Ord(false) );
  dev.SetRenderState( D3DRENDERSTATE_COLORKEYENABLE, 1);
  result := dev.ValidateDevice(dwNumPasses);
end;

////////////////////////////////////////////////////////////////////////////////
// Creates a default (empty) alpha effect
procedure TE_ALPH_CreateDeviceDefault(var TE_ALPH: TTextureAlphaEffect);
begin
  FillChar(TE_ALPH, SizeOf(TE_ALPH), Chr(0));
  TE_ALPH.Shaders[0].Enabled := false;
  TE_ALPH.Shaders[1].Enabled := false;
end;

////////////////////////////////////////////////////////////////////////////////
// Creates a default (empty) alpha effect as described in DXDesigner application
procedure TE_ALPH_CreateDXDesignerDefault(var TE_ALPH: TTextureAlphaEffect);
begin
  FillChar(TE_ALPH, SizeOf(TE_ALPH), Chr(0));
  TE_ALPH.Shaders[0].Enabled := false;
  TE_ALPH.Shaders[1].Enabled := false;
end;


////////////////////////////////////////////////////////////////////////////////
// Prepares the device for the alpha effect
function TE_ALPH_PrepareDevice7(const TE_ALPH: TTextureAlphaEffect;
  const dev: IDirect3DDevice7): HRESULT;
begin
  result := dev.SetRenderState( D3DRENDERSTATE_ALPHABLENDENABLE, Ord(true) );
end;

////////////////////////////////////////////////////////////////////////////////
// Draws vertexes with alpha TextureEffect
function TE_ALPH_DrawPrimitive7(const TE_ALPH: TTextureAlphaEffect;
  const dev: IDirect3DDevice7;
  d3dptPrimitiveType: TD3DPrimitiveType; dwVertexTypeDesc: DWORD;
  const lpvVertices; dwVertexCount: DWORD): HRESULT;
var i: integer;
    pArgs: PTextureAlphaEffectArgs;
    passed: boolean;
begin
  result := TE_ALPH_PrepareDevice7(TE_ALPH, dev);
  passed := false;
  if result = S_OK then
  begin
    dev.SetRenderState( D3DRENDERSTATE_COLORKEYBLENDENABLE, Ord(TE_ALPH.ColorKeyEnable) );
    dev.SetRenderState( D3DRENDERSTATE_ALPHAFUNC, Ord(TE_ALPH.AlphaFunc) );
    dev.SetRenderState( D3DRENDERSTATE_ALPHATESTENABLE, Ord(TE_ALPH.AlphaTestEnable) );
    dev.SetRenderState( D3DRENDERSTATE_ALPHAREF, Ord(TE_ALPH.AlphaRef) );
    for i := MAXTEXTUREALPHA - 1 downto 0 do // Attention: backward loop
    begin
      if TE_ALPH.Shaders[i].Enabled then
      begin
        pArgs := @TE_ALPH.Shaders[i];
        dev.SetRenderState( D3DRENDERSTATE_SRCBLEND, Ord(pArgs.SrcBlend) );
        dev.SetRenderState( D3DRENDERSTATE_DESTBLEND, Ord(pArgs.DestBlend) );
        if pArgs.Texture <> nil then
          dev.SetTexture( 0, pArgs.Texture.Surface.IDDSurface7 )
        else
          dev.SetTexture( 0, nil );
        dev.DrawPrimitive(d3dptPrimitiveType, dwVertexTypeDesc,
          lpvVertices, dwVertexCount, 0);
        passed := true;
      end;
    end;
    result := TE_ALPH_InitDeviceDefaults7(dev);
  end;
  if not passed then
  begin
    if TE_ALPH.Shaders[0].Args.Texture <> nil then
      dev.SetTexture( 0, TE_ALPH.Shaders[0].Args.Texture.Surface.IDDSurface7 )
    else
      dev.SetTexture( 0, nil );
    result := dev.DrawPrimitive(d3dptPrimitiveType, dwVertexTypeDesc,
                lpvVertices, dwVertexCount, 0);
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// Draws vertexes with alpha TextureEffect (indexed)
function TE_ALPH_DrawIndexedPrimitive7(const TE_ALPH: TTextureAlphaEffect;
  const dev: IDirect3DDevice7;
  d3dptPrimitiveType: TD3DPrimitiveType; dwVertexTypeDesc: DWORD;
  const lpvVertices; dwVertexCount: DWORD;
  const lpwIndices: PIndexesArray; dwIndexCount: DWORD): HRESULT;
var i: integer;
    pArgs: PTextureAlphaEffectArgs;
    passed: boolean;
begin
  result := TE_ALPH_PrepareDevice7(TE_ALPH, dev);
  passed := false;
  if result = S_OK then
  begin
    dev.SetRenderState( D3DRENDERSTATE_COLORKEYBLENDENABLE, Ord(TE_ALPH.ColorKeyEnable) );
    dev.SetRenderState( D3DRENDERSTATE_ALPHAFUNC, Ord(TE_ALPH.AlphaFunc) );
    dev.SetRenderState( D3DRENDERSTATE_ALPHATESTENABLE, Ord(TE_ALPH.AlphaTestEnable) );
    dev.SetRenderState( D3DRENDERSTATE_ALPHAREF, Ord(TE_ALPH.AlphaRef) );
    for i := MAXTEXTUREALPHA - 1 downto 0 do // Attention: backward loop
    begin
      if TE_ALPH.Shaders[i].Enabled then
      begin
        pArgs := @TE_ALPH.Shaders[i];
        dev.SetRenderState( D3DRENDERSTATE_SRCBLEND, Ord(pArgs.SrcBlend) );
        dev.SetRenderState( D3DRENDERSTATE_DESTBLEND, Ord(pArgs.DestBlend) );
        if pArgs.Texture <> nil then
          dev.SetTexture( 0, pArgs.Texture.Surface.IDDSurface7 )
        else
          dev.SetTexture( 0, nil );
        dev.DrawIndexedPrimitive(d3dptPrimitiveType, dwVertexTypeDesc,
         lpvVertices, dwVertexCount, lpwIndices[0], dwIndexCount, 0);
        passed := true;
      end;
    end;
    result := TE_ALPH_InitDeviceDefaults7(dev);
  end;
  if not passed then
  begin
    if TE_ALPH.Shaders[0].Args.Texture <> nil then
      dev.SetTexture( 0, TE_ALPH.Shaders[0].Args.Texture.Surface.IDDSurface7 )
    else
      dev.SetTexture( 0, nil );
    result := dev.DrawIndexedPrimitive(d3dptPrimitiveType, dwVertexTypeDesc,
                lpvVertices, dwVertexCount, lpwIndices[0], dwIndexCount, 0);
  end;
end;


////////////////////////////////////////////////////////////////////////////////
// TEXTUREALPHA PREDEFINED EFFECTS
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
// TE_ALPH_CreateLightFromFlare
//Texturename1: Texture to apply effect
//Texturename2: Flare texture
// Needs a flare for second texture (see DXDSK7)
// Description of flare:
// A black texture rectangle with portions of gray - white
// values which make lighter the first texture
procedure TE_ALPH_CreateLightFromFlare(var TE_ALPH: TTextureAlphaEffect);
begin
  FillChar(TE_ALPH, SizeOf(TE_ALPH), Chr(0));
  TE_ALPH.ID := ALPHLightFromFlare;
  TE_ALPH.ColorKeyEnable := true;
  TE_ALPH.AlphaTestEnable := true;
  TE_ALPH.AlphaFunc := Ord(D3DCMP_ALWAYS);
  TE_ALPH.AlphaRef := $00;
  TE_ALPH.Shaders[0].Enabled := true;
//  TE_ALPH.Shaders[0].Args.Texture := texturetoapplyeffect;
  TE_ALPH.Shaders[0].Args.SrcBlend := Ord(D3DBLEND_INVSRCCOLOR);
  TE_ALPH.Shaders[0].Args.DestBlend := Ord(D3DBLEND_SRCCOLOR);
  TE_ALPH.Shaders[1].Enabled := true;
//  TE_ALPH.Shaders[1].Args.Texture := flare;
  TE_ALPH.Shaders[1].Args.SrcBlend := Ord(D3DBLEND_SRCCOLOR);
  TE_ALPH.Shaders[1].Args.DestBlend := Ord(D3DBLEND_SRCCOLOR);
end;


//Texturename1: rsWhite
//Texturename2: rsGray RGB(192, 192, 192)
// Creates an effect just as looking throu glass and what you are looking
// seems to be more luminous
// Must be rendered at the end of the scene....
procedure TE_ALPH_SeeThrouGlass(var TE_ALPH: TTextureAlphaEffect);
begin
  FillChar(TE_ALPH, SizeOf(TE_ALPH), Chr(0));
  TE_ALPH.ID := ALPHSeeThrouGlass;
  TE_ALPH.ColorKeyEnable := true;
  TE_ALPH.AlphaTestEnable := true;
  TE_ALPH.AlphaFunc := Ord(D3DCMP_ALWAYS);
  TE_ALPH.AlphaRef := $00;
  TE_ALPH.Shaders[0].Enabled := true;
//  TE_ALPH.Shaders[0].Args.Texture := CreateTexture();
  TE_ALPH.Shaders[0].Args.SrcBlend := Ord(D3DBLEND_DESTCOLOR);
  TE_ALPH.Shaders[0].Args.DestBlend := Ord(D3DBLEND_BOTHSRCALPHA);
  TE_ALPH.Shaders[1].Enabled := true;
//  TE_ALPH.Shaders[1].Args.Texture := CreateTexture();
  TE_ALPH.Shaders[1].Args.SrcBlend := Ord(D3DBLEND_SRCALPHA);
  TE_ALPH.Shaders[1].Args.DestBlend := Ord(D3DBLEND_SRCALPHA);
end;


//Texturename1: Doom3Dir\xxx\xxx\lights\*******.tga
//Texturename2:
// Creates a light effect from Doom3 style tga images in lights directory
procedure TE_ALPH_CreateDoom3Light(var TE_ALPH: TTextureAlphaEffect);
begin
  FillChar(TE_ALPH, SizeOf(TE_ALPH), Chr(0));
  TE_ALPH.ID := ALPHDoom3Light;
  TE_ALPH.ColorKeyEnable := true;
  TE_ALPH.AlphaTestEnable := true;
  TE_ALPH.AlphaFunc := Ord(D3DCMP_ALWAYS);
  TE_ALPH.AlphaRef := $00;
  TE_ALPH.Shaders[0].Enabled := true;
//TE_ALPH.Shaders[0].Args.Texture := CreateTexture(Doom3Dir\xxx\xxx\lights\*******.tga);
  TE_ALPH.Shaders[0].Args.SrcBlend := Ord(D3DBLEND_SRCCOLOR);
  TE_ALPH.Shaders[0].Args.DestBlend := Ord(D3DBLEND_ONE);
  TE_ALPH.Shaders[1].Enabled := false;
end;


//Texturename1: Doom3Dir\xxx\xxx\lights\*******.tga
//Texturename2: Doom3Dir\xxx\xxx\lights\*******.tga
// Create a compinated light, much more luminated than TE_ALPH_CreateDoom3Ligh
// Like 2 lights in the same position
procedure TE_ALPH_CreateDoom3LightCompinated(var TE_ALPH: TTextureAlphaEffect);
begin
  FillChar(TE_ALPH, SizeOf(TE_ALPH), Chr(0));
  TE_ALPH.ID := ALPHDoom3LightCompinated;
  TE_ALPH.ColorKeyEnable := true;
  TE_ALPH.AlphaTestEnable := true;
  TE_ALPH.AlphaFunc := Ord(D3DCMP_ALWAYS);
  TE_ALPH.AlphaRef := $00;
  TE_ALPH.Shaders[0].Enabled := true;
//  TE_ALPH.Shaders[0].Args.Texture := CreateTexture(I:\Doom 3 Demo\demo\demo00\lights\baronflash.tga);
  TE_ALPH.Shaders[0].Args.SrcBlend := Ord(D3DBLEND_SRCCOLOR);
  TE_ALPH.Shaders[0].Args.DestBlend := Ord(D3DBLEND_ONE);
  TE_ALPH.Shaders[1].Enabled := true;
//  TE_ALPH.Shaders[1].Args.Texture := CreateTexture(I:\Doom 3 Demo\demo\demo00\lights\desertlight.tga);
  TE_ALPH.Shaders[1].Args.SrcBlend := Ord(D3DBLEND_SRCCOLOR);
  TE_ALPH.Shaders[1].Args.DestBlend := Ord(D3DBLEND_ONE);
end;


////////////////////////////////////////////////////////////////////////////////
// TEXTURESTAGE EFFECTS
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
// Initalize the TextureStage of a device to default values;
function TE_STG_InitDeviceDefaults7(const dev: IDirect3DDevice7): HRESULT;
var dwNumPasses: DWORD;
begin
  dev.SetTextureStageState(0, D3DTSS_COLORARG1, Ord(D3DTA_TEXTURE) );
  dev.SetTextureStageState(0, D3DTSS_COLOROP,   Ord(D3DTOP_SELECTARG1) );
  dev.SetTextureStageState(0, D3DTSS_ALPHAARG1, Ord(D3DTA_TEXTURE) );
  dev.SetTextureStageState(0, D3DTSS_ALPHAOP,   Ord(D3DTOP_SELECTARG1) );
  dev.SetTextureStageState(0, D3DTSS_MINFILTER, Ord(D3DTFN_LINEAR) );
  dev.SetTextureStageState(0, D3DTSS_MAGFILTER, Ord(D3DTFG_LINEAR) );

  TE_STG_DisableMultiTexturing7(dev);

  result := dev.ValidateDevice(dwNumPasses);
end;


////////////////////////////////////////////////////////////////////////////////
// Initalize the TextureStage of a device as does DXDesigner Application
function TE_STG_InitDXDesignerDefaults7(const dev: IDirect3DDevice7): HRESULT;
var dwNumPasses: DWORD;
begin
  dev.SetTextureStageState(0, D3DTSS_COLORARG1, Ord(D3DTA_TEXTURE) );
  dev.SetTextureStageState(0, D3DTSS_COLOROP,   Ord(D3DTOP_MODULATE) );
  dev.SetTextureStageState(0, D3DTSS_COLORARG2, Ord(D3DTA_DIFFUSE) );
  dev.SetTextureStageState(0, D3DTSS_ALPHAARG1, Ord(D3DTA_TEXTURE) );
  dev.SetTextureStageState(0, D3DTSS_ALPHAOP,   Ord(D3DTOP_SELECTARG1) );
  dev.SetTextureStageState(0, D3DTSS_MINFILTER, Ord(D3DTFN_LINEAR) );
  dev.SetTextureStageState(0, D3DTSS_MAGFILTER, Ord(D3DTFG_LINEAR) );

  TE_STG_DisableMultiTexturing7(dev);

  result := dev.ValidateDevice(dwNumPasses);
end;


////////////////////////////////////////////////////////////////////////////////
// Creates a default (empty) stage effect
procedure TE_STG_CreateDeviceDefault(var TE_STG: TTextureStageEffect);
var i: integer;
begin
  FillChar(TE_STG, SizeOf(TE_STG), Chr(0));
  TE_STG.TextureFactor := $FFFFFFFF;
  TE_STG.DiffuseColor := $FFFFFF;
  TE_STG.Shaders[0].Enabled := true;
  TE_STG.Shaders[0].Args.ColorArg1 := Ord(D3DTA_TEXTURE);
  TE_STG.Shaders[0].Args.ColorOp   := Ord(D3DTOP_SELECTARG1);
  TE_STG.Shaders[0].Args.AlphaArg1 := Ord(D3DTA_TEXTURE);
  TE_STG.Shaders[0].Args.AlphaOp   := Ord(D3DTOP_SELECTARG1);
  TE_STG.Shaders[0].Args.MinFilter := Ord(D3DTFN_LINEAR);
  TE_STG.Shaders[0].Args.MagFilter := Ord(D3DTFG_LINEAR);
  for i := 1 to MAXTEXTURESTAGES - 1 do
    TE_STG.Shaders[i].Enabled := false;
end;

////////////////////////////////////////////////////////////////////////////////
// Creates a default (empty) stage effect as described in DXDesigner application
procedure TE_STG_CreateDXDesignerDefault(var TE_STG: TTextureStageEffect);
var i: integer;
begin
  FillChar(TE_STG, SizeOf(TE_STG), Chr(0));
  TE_STG.TextureFactor := $FFFFFFFF;
  TE_STG.DiffuseColor := $FFFFFF;
  TE_STG.Shaders[0].Enabled := true;
  TE_STG.Shaders[0].Args.ColorArg1 := Ord(D3DTA_TEXTURE);
  TE_STG.Shaders[0].Args.ColorOp   := Ord(D3DTOP_MODULATE);
  TE_STG.Shaders[0].Args.ColorArg2 := Ord(D3DTA_DIFFUSE);
  TE_STG.Shaders[0].Args.AlphaArg1 := Ord(D3DTA_TEXTURE);
  TE_STG.Shaders[0].Args.AlphaOp   := Ord(D3DTOP_SELECTARG1);
  TE_STG.Shaders[0].Args.MinFilter := Ord(D3DTFN_LINEAR);
  TE_STG.Shaders[0].Args.MagFilter := Ord(D3DTFG_LINEAR);
  for i := 1 to MAXTEXTURESTAGES - 1 do
    TE_STG.Shaders[i].Enabled := false;
end;


////////////////////////////////////////////////////////////////////////////////
// Disables additional texture stages of a device
function TE_STG_DisableMultiTexturing7(const dev: IDirect3DDevice7): HRESULT;
var i: integer;
begin
  for i := 1 to MAXTEXTURESTAGES - 1 do
  begin
    dev.SetTexture(i, nil);
    dev.SetTextureStageState(i, D3DTSS_COLOROP, Ord(D3DTOP_DISABLE) );
    dev.SetTextureStageState(i, D3DTSS_ALPHAOP, Ord(D3DTOP_DISABLE) );
  end;
  result := S_OK;
end;


////////////////////////////////////////////////////////////////////////////////
// Initalize the vertexes to the diffuse color of the TextureStageEffect
procedure TE_STG_PrepareVertexes(const TE_STG: TTextureStageEffect;
  const lpvVertices: PD3DLVertexArray; dwVertexCount: DWORD);
var i: integer;
begin
  for i := 0 to dwVertexCount - 1 do
    lpvVertices[i].color := TE_STG.DiffuseColor; // ?? Must also use and specular?
end;


////////////////////////////////////////////////////////////////////////////////
// Prepares the device for the effect
function TE_STG_PrepareDevice7(const TE_STG: TTextureStageEffect;
  const dev: IDirect3DDevice7): HRESULT;
var i: integer;
    pArgs: PTextureStageEffectArgs;
    dwNumPasses: DWORD;
begin
  dev.SetRenderState( D3DRENDERSTATE_TEXTUREFACTOR, TE_STG.TextureFactor );

  i := 0;
  while (TE_STG.Shaders[i].Enabled) and (i < MAXTEXTURESTAGES) do
  begin
    pArgs := @TE_STG.Shaders[i].Args;
    if pArgs.Texture <> nil then
      dev.SetTexture( i, pArgs.Texture.Surface.IDDSurface7 )
    else
      dev.SetTexture( i, nil );
    dev.SetTextureStageState( i, D3DTSS_MINFILTER, pArgs.MinFilter );
    dev.SetTextureStageState( i, D3DTSS_MAGFILTER, pArgs.MagFilter );
    dev.SetTextureStageState( i, D3DTSS_COLORARG1, pArgs.ColorArg1 );
    dev.SetTextureStageState( i, D3DTSS_COLOROP,   pArgs.ColorOp );
    dev.SetTextureStageState( i, D3DTSS_COLORARG2, pArgs.ColorArg2 );
    dev.SetTextureStageState( i, D3DTSS_ALPHAARG1, pArgs.AlphaArg1 );
    dev.SetTextureStageState( i, D3DTSS_ALPHAOP,   pArgs.AlphaOp );
    dev.SetTextureStageState( i, D3DTSS_ALPHAARG2, pArgs.AlphaArg2 );
    inc(i);
  end;

  while i < MAXTEXTURESTAGES do
  begin
    dev.SetTexture(i, nil);
    dev.SetTextureStageState(i, D3DTSS_COLOROP, Ord(D3DTOP_DISABLE) );
    dev.SetTextureStageState(i, D3DTSS_ALPHAOP, Ord(D3DTOP_DISABLE) );
    inc(i);
  end;


  result := dev.ValidateDevice(dwNumPasses);
end;

////////////////////////////////////////////////////////////////////////////////
// Draws vertexes with stage TextureEffect
function TE_STG_DrawPrimitive7(const TE_STG: TTextureStageEffect;
  const dev: IDirect3DDevice7; d3dptPrimitiveType: TD3DPrimitiveType;
  const lpvVertices: PD3DLVertexArray; dwVertexCount: DWORD): HRESULT;
begin
  result := TE_STG_PrepareDevice7(TE_STG, dev);
  if result = S_OK then
  begin
    dev.DrawPrimitive(d3dptPrimitiveType, D3DFVF_LVERTEX,
      lpvVertices[0], dwVertexCount, 0);
    result := TE_STG_InitDeviceDefaults7(dev);
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// Draws vertexes with stage TextureEffect (indexed)
function TE_STG_DrawIndexedPrimitive7(const TE_STG: TTextureStageEffect;
  const dev: IDirect3DDevice7; d3dptPrimitiveType: TD3DPrimitiveType;
  const lpvVertices: PD3DLVertexArray; dwVertexCount: DWORD;
  const lpwIndices: PIndexesArray; dwIndexCount: DWORD): HRESULT;
begin
  result := TE_STG_PrepareDevice7(TE_STG, dev);
  if result = S_OK then
  begin
    dev.DrawIndexedPrimitive(d3dptPrimitiveType, D3DFVF_LVERTEX,
      lpvVertices[0], dwVertexCount, lpwIndices[0], dwIndexCount, 0);
    result := TE_STG_InitDeviceDefaults7(dev);
  end;
end;


////////////////////////////////////////////////////////////////////////////////
// Helper functions
procedure TE_STG_UpdateStageColor(var TE_STG: TTextureStageEffect;
  stage: integer; op, arg1, arg2: integer);
begin
  TE_STG.Shaders[stage].Enabled := true;
  TE_STG.Shaders[stage].Args.ColorArg1 := arg1;
  TE_STG.Shaders[stage].Args.ColorOp := op;
  TE_STG.Shaders[stage].Args.ColorArg2 := arg2;
end;

procedure TE_STG_UpdateStageAlpha(var TE_STG: TTextureStageEffect;
  stage: integer; op, arg1, arg2: integer);
begin
  TE_STG.Shaders[stage].Enabled := true;
  TE_STG.Shaders[stage].Args.AlphaArg1 := arg1;
  TE_STG.Shaders[stage].Args.AlphaOp := op;
  TE_STG.Shaders[stage].Args.AlphaArg2 := arg2;
end;

procedure TE_STG_UpdateMinMagFilters(var TE_STG: TTextureStageEffect;
  const minf, magf: integer);
var i: integer;
begin
  for i := 0 to MAXTEXTURESTAGES - 1 do
  begin
    TE_STG.Shaders[i].Args.MinFilter := minf;
    TE_STG.Shaders[i].Args.MagFilter := magf;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// EFFECTS
procedure TE_STG_CreateModulate(var TE_STG: TTextureStageEffect);
begin
  FillChar(TE_STG, SizeOf(TE_STG), Chr(0));
  TE_STG.ID := STGModulate;
  TE_STG.TextureFactor := $80808080;
  TE_STG.DiffuseColor := $FFFFFFFF;
  TE_STG_UpdateMinMagFilters(TE_STG, Ord(D3DTFN_LINEAR), Ord(D3DTFG_LINEAR) );
  TE_STG_UpdateStageColor( TE_STG, 0, Ord(D3DTOP_MODULATE), D3DTA_TEXTURE, D3DTA_DIFFUSE );
  TE_STG_UpdateStageAlpha( TE_STG, 0, Ord(D3DTOP_SELECTARG1), D3DTA_TEXTURE, D3DTA_DIFFUSE );
end;

procedure TE_STG_CreateModulateAlpha(var TE_STG: TTextureStageEffect);
begin
  FillChar(TE_STG, SizeOf(TE_STG), Chr(0));
  TE_STG.ID := STGModulateAlpha;
  TE_STG.TextureFactor := $80808080;
  TE_STG.DiffuseColor := $FFFFFFFF;
  TE_STG_UpdateMinMagFilters(TE_STG, Ord(D3DTFN_LINEAR), Ord(D3DTFG_LINEAR) );
  TE_STG_UpdateStageColor( TE_STG, 0, Ord(D3DTOP_MODULATE), D3DTA_TEXTURE, D3DTA_DIFFUSE );
  TE_STG_UpdateStageAlpha( TE_STG, 0, Ord(D3DTOP_MODULATE), D3DTA_TEXTURE, D3DTA_DIFFUSE );
end;

procedure TE_STG_CreateAdd(var TE_STG: TTextureStageEffect);
begin
  FillChar(TE_STG, SizeOf(TE_STG), Chr(0));
  TE_STG.ID := STGAdd;
  TE_STG.TextureFactor := $80808080;
  TE_STG.DiffuseColor := $FFFFFFFF;
  TE_STG_UpdateMinMagFilters(TE_STG, Ord(D3DTFN_LINEAR), Ord(D3DTFG_LINEAR) );
  TE_STG_UpdateStageColor( TE_STG, 0, Ord(D3DTOP_ADD), D3DTA_TEXTURE, D3DTA_DIFFUSE );
  TE_STG_UpdateStageAlpha( TE_STG, 0, Ord(D3DTOP_SELECTARG2), D3DTA_TEXTURE, D3DTA_DIFFUSE );
end;

procedure TE_STG_CreateDecalAlpha(var TE_STG: TTextureStageEffect);
begin
  FillChar(TE_STG, SizeOf(TE_STG), Chr(0));
  TE_STG.ID := STGDecalAlpha;
  TE_STG.TextureFactor := $80808080;
  TE_STG.DiffuseColor := $FFFFFFFF;
  TE_STG_UpdateMinMagFilters(TE_STG, Ord(D3DTFN_LINEAR), Ord(D3DTFG_LINEAR) );
  TE_STG_UpdateStageColor( TE_STG, 0, Ord(D3DTOP_BLENDTEXTUREALPHA), D3DTA_TEXTURE, D3DTA_DIFFUSE );
  TE_STG_UpdateStageAlpha( TE_STG, 0, Ord(D3DTOP_SELECTARG2), D3DTA_TEXTURE, D3DTA_DIFFUSE );
end;

procedure TE_STG_CreateColoredLightMap(var TE_STG: TTextureStageEffect);
begin
  FillChar(TE_STG, SizeOf(TE_STG), Chr(0));
  TE_STG.ID := STGColoredLightMap;
  TE_STG.TextureFactor := $80808080;
  TE_STG.DiffuseColor := $FFFFFFFF;
  TE_STG_UpdateMinMagFilters(TE_STG, Ord(D3DTFN_LINEAR), Ord(D3DTFG_LINEAR) );
  TE_STG_UpdateStageColor( TE_STG, 0, Ord(D3DTOP_MODULATE), D3DTA_TEXTURE, D3DTA_DIFFUSE );
  TE_STG_UpdateStageAlpha( TE_STG, 0, Ord(D3DTOP_SELECTARG2), D3DTA_TEXTURE, D3DTA_DIFFUSE );
  TE_STG_UpdateStageColor( TE_STG, 1, Ord(D3DTOP_MODULATE), D3DTA_TEXTURE, D3DTA_CURRENT );
  TE_STG_UpdateStageAlpha( TE_STG, 1, Ord(D3DTOP_SELECTARG1), D3DTA_TEXTURE, D3DTA_DIFFUSE );
end;

procedure TE_STG_CreateInverseColoredLightMap(var TE_STG: TTextureStageEffect);
begin
  FillChar(TE_STG, SizeOf(TE_STG), Chr(0));
  TE_STG.ID := STGInverseColoredLightMap;
  TE_STG.TextureFactor := $80808080;
  TE_STG.DiffuseColor := $FFFFFFFF;
  TE_STG_UpdateMinMagFilters(TE_STG, Ord(D3DTFN_LINEAR), Ord(D3DTFG_LINEAR) );
  TE_STG_UpdateStageColor( TE_STG, 0, Ord(D3DTOP_MODULATE), D3DTA_TEXTURE or D3DTA_COMPLEMENT, D3DTA_DIFFUSE );
  TE_STG_UpdateStageAlpha( TE_STG, 0, Ord(D3DTOP_SELECTARG2), D3DTA_TEXTURE, D3DTA_DIFFUSE );
  TE_STG_UpdateStageColor( TE_STG, 1, Ord(D3DTOP_MODULATE), D3DTA_TEXTURE or D3DTA_COMPLEMENT, D3DTA_CURRENT );
  TE_STG_UpdateStageAlpha( TE_STG, 1, Ord(D3DTOP_SELECTARG1), D3DTA_TEXTURE, D3DTA_DIFFUSE );
end;

procedure TE_STG_CreateSingleChannelLightMap(var TE_STG: TTextureStageEffect);
begin
  FillChar(TE_STG, SizeOf(TE_STG), Chr(0));
  TE_STG.ID := STGSingleChannelLightMap;
  TE_STG.TextureFactor := $80808080;
  TE_STG.DiffuseColor := $FFFFFFFF;
  TE_STG_UpdateMinMagFilters(TE_STG, Ord(D3DTFN_LINEAR), Ord(D3DTFG_LINEAR) );
  TE_STG_UpdateStageColor( TE_STG, 0, Ord(D3DTOP_SELECTARG1), D3DTA_TEXTURE, D3DTA_DIFFUSE );
  TE_STG_UpdateStageAlpha( TE_STG, 0, Ord(D3DTOP_SELECTARG1), D3DTA_TEXTURE, D3DTA_DIFFUSE );
  TE_STG_UpdateStageColor( TE_STG, 1, Ord(D3DTOP_MODULATE), D3DTA_TEXTURE or D3DTA_ALPHAREPLICATE, D3DTA_CURRENT );
  TE_STG_UpdateStageAlpha( TE_STG, 1, Ord(D3DTOP_SELECTARG1), D3DTA_TEXTURE, D3DTA_DIFFUSE );
end;

procedure TE_STG_CreateModulateAndLateAdd(var TE_STG: TTextureStageEffect);
begin
  FillChar(TE_STG, SizeOf(TE_STG), Chr(0));
  TE_STG.ID := STGModulateAndLateAdd;
  TE_STG.TextureFactor := $80808080;
  TE_STG.DiffuseColor := $FFFFFFFF;
  TE_STG_UpdateMinMagFilters(TE_STG, Ord(D3DTFN_LINEAR), Ord(D3DTFG_LINEAR) );
  TE_STG_UpdateStageColor( TE_STG, 0, Ord(D3DTOP_MODULATE), D3DTA_TEXTURE, D3DTA_DIFFUSE );
  TE_STG_UpdateStageAlpha( TE_STG, 0, Ord(D3DTOP_SELECTARG1), D3DTA_TEXTURE, D3DTA_DIFFUSE );
  TE_STG_UpdateStageColor( TE_STG, 1, Ord(D3DTOP_ADD), D3DTA_TEXTURE, D3DTA_CURRENT );
  TE_STG_UpdateStageAlpha( TE_STG, 1, Ord(D3DTOP_SELECTARG1), D3DTA_TEXTURE, D3DTA_DIFFUSE );
end;

procedure TE_STG_CreateLinearBlendUsingTextureAlpha(var TE_STG: TTextureStageEffect);
begin
  FillChar(TE_STG, SizeOf(TE_STG), Chr(0));
  TE_STG.ID := STGLinearBlendUsingTextureAlpha;
  TE_STG.TextureFactor := $80808080;
  TE_STG.DiffuseColor := $FFFFFFFF;
  TE_STG_UpdateMinMagFilters(TE_STG, Ord(D3DTFN_LINEAR), Ord(D3DTFG_LINEAR) );
  TE_STG_UpdateStageColor( TE_STG, 0, Ord(D3DTOP_MODULATE), D3DTA_TEXTURE, D3DTA_DIFFUSE );
  TE_STG_UpdateStageAlpha( TE_STG, 0, Ord(D3DTOP_SELECTARG2), D3DTA_TEXTURE, D3DTA_DIFFUSE );
  TE_STG_UpdateStageColor( TE_STG, 1, Ord(D3DTOP_BLENDTEXTUREALPHA), D3DTA_TEXTURE, D3DTA_CURRENT );
  TE_STG_UpdateStageAlpha( TE_STG, 1, Ord(D3DTOP_SELECTARG1), D3DTA_TEXTURE, D3DTA_DIFFUSE );
end;

procedure TE_STG_CreateLinearBlendUsingDiffuseAlpha(var TE_STG: TTextureStageEffect);
begin
  FillChar(TE_STG, SizeOf(TE_STG), Chr(0));
  TE_STG.ID := STGLinearBlendUsingDiffuseAlpha;
  TE_STG.TextureFactor := $80808080;
  TE_STG.DiffuseColor := $FFFFFFFF;
  TE_STG_UpdateMinMagFilters(TE_STG, Ord(D3DTFN_LINEAR), Ord(D3DTFG_LINEAR) );
  TE_STG_UpdateStageColor( TE_STG, 0, Ord(D3DTOP_MODULATE), D3DTA_TEXTURE, D3DTA_DIFFUSE );
  TE_STG_UpdateStageAlpha( TE_STG, 0, Ord(D3DTOP_SELECTARG2), D3DTA_TEXTURE, D3DTA_DIFFUSE );
  TE_STG_UpdateStageColor( TE_STG, 1, Ord(D3DTOP_BLENDDIFFUSEALPHA), D3DTA_TEXTURE, D3DTA_CURRENT );
  TE_STG_UpdateStageAlpha( TE_STG, 1, Ord(D3DTOP_SELECTARG1), D3DTA_TEXTURE, D3DTA_DIFFUSE );
end;

procedure TE_STG_CreateMultiTextureAndSmoothSaturation(var TE_STG: TTextureStageEffect);
begin
  FillChar(TE_STG, SizeOf(TE_STG), Chr(0));
  TE_STG.ID := STGMultiTextureAndSmoothSaturation;
  TE_STG.TextureFactor := $80808080;
  TE_STG.DiffuseColor := $FFFFFFFF;
  TE_STG_UpdateMinMagFilters(TE_STG, Ord(D3DTFN_LINEAR), Ord(D3DTFG_LINEAR) );
  TE_STG_UpdateStageColor( TE_STG, 0, Ord(D3DTOP_MODULATE), D3DTA_TEXTURE, D3DTA_DIFFUSE );
  TE_STG_UpdateStageAlpha( TE_STG, 0, Ord(D3DTOP_SELECTARG2), D3DTA_TEXTURE, D3DTA_DIFFUSE );
  TE_STG_UpdateStageColor( TE_STG, 1, Ord(D3DTOP_ADD), D3DTA_TEXTURE, D3DTA_CURRENT );
  TE_STG_UpdateStageAlpha( TE_STG, 1, Ord(D3DTOP_SELECTARG1), D3DTA_TEXTURE, D3DTA_DIFFUSE );
end;

procedure TE_STG_CreateMultiTextureSubTract(var TE_STG: TTextureStageEffect);
begin
  FillChar(TE_STG, SizeOf(TE_STG), Chr(0));
  TE_STG.ID := STGMultiTextureSubTract;
  TE_STG.TextureFactor := $80808080;
  TE_STG.DiffuseColor := $FFFFFFFF;
  TE_STG_UpdateMinMagFilters(TE_STG, Ord(D3DTFN_LINEAR), Ord(D3DTFG_LINEAR) );
  TE_STG_UpdateStageColor( TE_STG, 0, Ord(D3DTOP_MODULATE), D3DTA_TEXTURE, D3DTA_DIFFUSE );
  TE_STG_UpdateStageAlpha( TE_STG, 0, Ord(D3DTOP_SELECTARG2), D3DTA_TEXTURE, D3DTA_DIFFUSE );
  TE_STG_UpdateStageColor( TE_STG, 1, Ord(D3DTOP_ADD), D3DTA_TEXTURE or D3DTA_COMPLEMENT, D3DTA_CURRENT );
  TE_STG_UpdateStageAlpha( TE_STG, 1, Ord(D3DTOP_SELECTARG1), D3DTA_TEXTURE, D3DTA_DIFFUSE );
end;

procedure TE_STG_CreateAddDiffuseToLightAndThenModulate(var TE_STG: TTextureStageEffect);
begin
  FillChar(TE_STG, SizeOf(TE_STG), Chr(0));
  TE_STG.ID := STGAddDiffuseToLightAndThenModulate;
  TE_STG.TextureFactor := $80808080;
  TE_STG.DiffuseColor := $FFFFFFFF;
  TE_STG_UpdateMinMagFilters(TE_STG, Ord(D3DTFN_LINEAR), Ord(D3DTFG_LINEAR) );
  TE_STG_UpdateStageColor( TE_STG, 0, Ord(D3DTOP_ADD), D3DTA_TEXTURE, D3DTA_DIFFUSE );
  TE_STG_UpdateStageAlpha( TE_STG, 0, Ord(D3DTOP_SELECTARG1), D3DTA_TEXTURE, D3DTA_DIFFUSE );
  TE_STG_UpdateStageColor( TE_STG, 1, Ord(D3DTOP_MODULATE), D3DTA_TEXTURE, D3DTA_CURRENT );
  TE_STG_UpdateStageAlpha( TE_STG, 1, Ord(D3DTOP_SELECTARG1), D3DTA_TEXTURE, D3DTA_DIFFUSE );
  TE_STG_UpdateStageColor( TE_STG, 2, Ord(D3DTOP_MODULATE), D3DTA_DIFFUSE, D3DTA_CURRENT );
  TE_STG_UpdateStageAlpha( TE_STG, 2, Ord(D3DTOP_SELECTARG1), D3DTA_TEXTURE, D3DTA_DIFFUSE );
end;

procedure TE_STG_CreateDetailModulate(var TE_STG: TTextureStageEffect);
begin
  FillChar(TE_STG, SizeOf(TE_STG), Chr(0));
  TE_STG.ID := STGDetailModulate;
  TE_STG.TextureFactor := $80808080;
  TE_STG.DiffuseColor := $FFFFFFFF;
  TE_STG_UpdateMinMagFilters(TE_STG, Ord(D3DTFN_LINEAR), Ord(D3DTFG_LINEAR) );
  TE_STG_UpdateStageColor( TE_STG, 0, Ord(D3DTOP_MODULATE), D3DTA_TEXTURE, D3DTA_DIFFUSE );
  TE_STG_UpdateStageAlpha( TE_STG, 0, Ord(D3DTOP_SELECTARG2), D3DTA_TEXTURE, D3DTA_DIFFUSE );
  TE_STG_UpdateStageColor( TE_STG, 1, Ord(D3DTOP_MODULATE2X), D3DTA_TEXTURE, D3DTA_CURRENT );
  TE_STG_UpdateStageAlpha( TE_STG, 1, Ord(D3DTOP_SELECTARG1), D3DTA_TEXTURE, D3DTA_DIFFUSE );
end;

procedure TE_STG_CreateDetailAdd(var TE_STG: TTextureStageEffect);
begin
  FillChar(TE_STG, SizeOf(TE_STG), Chr(0));
  TE_STG.ID := STGDetailAdd;
  TE_STG.TextureFactor := $80808080;
  TE_STG.DiffuseColor := $FFFFFFFF;
  TE_STG_UpdateMinMagFilters(TE_STG, Ord(D3DTFN_LINEAR), Ord(D3DTFG_LINEAR) );
  TE_STG_UpdateStageColor( TE_STG, 0, Ord(D3DTOP_MODULATE), D3DTA_TEXTURE, D3DTA_DIFFUSE );
  TE_STG_UpdateStageAlpha( TE_STG, 0, Ord(D3DTOP_SELECTARG2), D3DTA_TEXTURE, D3DTA_DIFFUSE );
  TE_STG_UpdateStageColor( TE_STG, 1, Ord(D3DTOP_ADDSIGNED), D3DTA_TEXTURE, D3DTA_CURRENT );
  TE_STG_UpdateStageAlpha( TE_STG, 1, Ord(D3DTOP_SELECTARG1), D3DTA_TEXTURE, D3DTA_DIFFUSE );
end;


procedure CreateEffectFromID(const id: integer; var E: TTextureEffect);
begin
  E.EffectType := TextureTypeLookUpTable[id].EffectType;
  case E.EffectType of
    TE_EFFECT_STAGE: TextureTypeLookUpTable[id].STAGE_CREATOR(E.TE_STAGE);
    TE_EFFECT_ALPHA: TextureTypeLookUpTable[id].ALPHA_CREATOR(E.TE_ALPHA);
  end;
end;

procedure CreateEffectFromName(const name: string; var E: TTextureEffect);
var uName: string;
    i: integer;
begin
  uName := UpperCase(name);
  for i := 0 to MaxEffectID - 1 do
  begin
    if TextureTypeLookUpTable[i].EffectName = uName then
    begin
      CreateEffectFromID(i, E);
      exit;
    end;
  end;
end;


////////////////////////////////////////////////////////////////////////////////
// Streaming rutines
procedure LoadEffectFromStream(s: TStream; var E: TTextureEffect);
var id: integer;
begin
  s.Read(id, SizeOf(id));
  CreateEffectFromID(id, E);
end;

procedure SaveEffectToStream(s: TStream; const E: TTextureEffect);
begin
  case E.EffectType of
    TE_EFFECT_NONE: s.Write(E.EffectType, SizeOf(integer));
    TE_EFFECT_ALPHA: s.Write(E.TE_ALPHA.ID, SizeOf(integer));
    TE_EFFECT_STAGE: s.Write(E.TE_STAGE.ID, SizeOf(integer));
  else
    raise Exception.Create('Unknown EffectType');
  end;
end;

procedure MakeLookUpItem(_type, id: integer; name: string;
  A_CREATOR: TAlphaCreateProc; S_CREATOR: TStageCreateProc);
begin
  TextureTypeLookUpTable[id].EffectType := _type;
  TextureTypeLookUpTable[id].EffectID := id;
  TextureTypeLookUpTable[id].EffectName := UpperCase(name);
  TextureTypeLookUpTable[id].ALPHA_CREATOR := A_CREATOR;
  TextureTypeLookUpTable[id].STAGE_CREATOR := S_CREATOR;
end;

initialization
  MakeLookUpItem(TE_EFFECT_NONE, 0, '(None)', nil, nil);
  MakeLookUpItem(TE_EFFECT_STAGE, STGModulate, 'Modulate', nil, TE_STG_CreateModulate);
  MakeLookUpItem(TE_EFFECT_STAGE, STGModulateAlpha, 'ModulateAlpha', nil, TE_STG_CreateModulateAlpha);
  MakeLookUpItem(TE_EFFECT_STAGE, STGAdd, 'Add', nil, TE_STG_CreateAdd);
  MakeLookUpItem(TE_EFFECT_STAGE, STGDecalAlpha, 'DecalAlpha', nil, TE_STG_CreateDecalAlpha);
  MakeLookUpItem(TE_EFFECT_STAGE, STGColoredLightMap, 'ColoredLightMap', nil, TE_STG_CreateColoredLightMap);
  MakeLookUpItem(TE_EFFECT_STAGE, STGInverseColoredLightMap, 'InverseColoredLightMap', nil, TE_STG_CreateInverseColoredLightMap);
  MakeLookUpItem(TE_EFFECT_STAGE, STGSingleChannelLightMap, 'SingleChannelLightMap', nil, TE_STG_CreateSingleChannelLightMap);
  MakeLookUpItem(TE_EFFECT_STAGE, STGModulateAndLateAdd, 'ModulateAndLateAdd', nil, TE_STG_CreateModulateAndLateAdd);
  MakeLookUpItem(TE_EFFECT_STAGE, STGLinearBlendUsingTextureAlpha, 'LinearBlendUsingTextureAlpha', nil, TE_STG_CreateLinearBlendUsingTextureAlpha);
  MakeLookUpItem(TE_EFFECT_STAGE, STGLinearBlendUsingDiffuseAlpha, 'LinearBlendUsingDiffuseAlpha', nil, TE_STG_CreateLinearBlendUsingDiffuseAlpha);
  MakeLookUpItem(TE_EFFECT_STAGE, STGMultiTextureAndSmoothSaturation, 'MultiTextureAndSmoothSaturation', nil, TE_STG_CreateMultiTextureAndSmoothSaturation);
  MakeLookUpItem(TE_EFFECT_STAGE, STGMultiTextureSubTract, 'MultiTextureSubTract', nil, TE_STG_CreateMultiTextureSubTract);
  MakeLookUpItem(TE_EFFECT_STAGE, STGAddDiffuseToLightAndThenModulate, 'AddDiffuseToLightAndThenModulate', nil, TE_STG_CreateAddDiffuseToLightAndThenModulate);
  MakeLookUpItem(TE_EFFECT_STAGE, STGDetailModulate, 'DetailModulate', nil, TE_STG_CreateDetailModulate);
  MakeLookUpItem(TE_EFFECT_STAGE, STGDetailAdd, 'DetailAdd', nil, TE_STG_CreateDetailAdd);
  MakeLookUpItem(TE_EFFECT_ALPHA, ALPHLightFromFlare, 'LightFromFlare', TE_ALPH_CreateLightFromFlare, nil);
  MakeLookUpItem(TE_EFFECT_ALPHA, ALPHSeeThrouGlass, 'SeeThrouGlass', TE_ALPH_SeeThrouGlass, nil);
  MakeLookUpItem(TE_EFFECT_ALPHA, ALPHDoom3Light, 'Doom3Light', TE_ALPH_CreateDoom3Light, nil);
  MakeLookUpItem(TE_EFFECT_ALPHA, ALPHDoom3LightCompinated, 'Doom3LightCompinated', TE_ALPH_CreateDoom3LightCompinated, nil);

end.
