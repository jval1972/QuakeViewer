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
//  id-tech2 & idtec3 map importing
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//------------------------------------------------------------------------------

{$I defs.inc}

unit se_QuakeTypes;

interface

{$IFNDEF NO_IDSOFTGAMESSUPPORT}

uses SysUtils, Classes, se_DXClasses, se_ZipFile;
  {
  ==============================================================================

    .Wal Texture file Format

  ==============================================================================
   }

const
  rsExtPak = '.pak';
  rsExtBsp = '.bsp';
  rsExtPK3 = '.pk3';
  rsExtWal = '.wal';
  rsExtM8 = '.m8'; // Heretic II
  rsExtMD2 = '.md2';
  rsQuake1ColorMapEntry = 'gfx\palette.lmp';
  rsQuake2TexturesDIR = 'textures\';
  rsQuake2ColorMapEntry = 'pics\colormap.pcx';
  rsFmtPak = 'pak%d.pak';
  rsPAKExt = 'PAK';
  rsBSPExt = 'BSP';
  rsPK3Ext = 'PK3';
  rsSkyTex = 'sky'; // HereticII sky
  rsFmtSkyTex = 'sky%d.wal';
  rsFmtSkyTex0 = 'sky0%d.wal';
  rsFmtSkyTex00 = 'sky00%d.wal';
  rsTriggerTex = 'trigger.wal';
  rsrtex412 = 'rtex412.wal'; // Hexen2 - Blank
  rsrtex114 = 'rtex114.wal'; // Hexen2 - Trigger
  rsExtJpg = '.jpg';
  rsExtTGA = '.tga';
  rsWadMask = '*.wad';
  rsHLTrigger1 = 'aaatrigger';
  rsHLTrigger2 = 'aaa_trigger';
  rsHLHurt1 = 'aaahurt';
  rsHLHurt2 = 'aaa_hurt';


const
  ZIPFILESIGNATURE = $04034b50; // for pk3, pk4 files!!

  Idbspheader: Longint = $50534249; // 'IBSP'
  Miplevels = 4;
  { Four Mip Maps Stored }
  { Next Frame In Animation Chain }

type
  TQuake2Palette = packed array[0..255] of packed record R, G, B: byte; end;

  Miptex_T = record
    Name: array[0..31] of char;
    Width: Longint;
    Height: Longint;
    Offsets: array[0..Miplevels - 1] of Longint;
    Animname: array[0..31] of char;
    Flags: Longint;
    Contents: Longint;
    Value: Longint;
  end;
  Miptex_T_P = ^Miptex_T;

  TMiptex_T = array[0..0] of Miptex_T;
  PMiptex_T = ^TMiptex_T;

  Miptex_S = record
    Nfo: Miptex_T;
    Data: Pointer;
  end;
  Miptex_P = ^Miptex_S;


  TMiptex_S = array[0..0] of Miptex_S;
  PMiptex_S = ^TMiptex_S;

// Quake1 support
  Miptex_T_v1 = record
    Name: array[0..15] of char;
    Width: Longint;
    Height: Longint;
    Offsets: array[0..Miplevels - 1] of Longint;
  end;
  Miptex_P_v1 = ^Miptex_T_v1;

  TMiptex_T_v1 = array[0..0] of Miptex_T_v1;
  PMiptex_T_v1 = ^TMiptex_T_v1;

  Miptex_S_v1 = record
    Nfo: Miptex_T_v1;
    Data: Pointer;
  end;

  TMiptex_S_v1 = array[0..0] of Miptex_S_v1;
  PMiptex_S_v1 = ^TMiptex_S_v1;

// Heretic2 m8 support
  Miptex_T_m8 = record
    Identifier: integer; // hexa: 02 00 00 00
    Name: array[0..31] of char;
    Widths: array[0..15] of Longint;
    Heights: array[0..15] of Longint;
    Offsets: array[0..15] of Longint;
    Animname: array[0..31] of char;
    Palette: TQuake2Palette;
    Flags: Longint;
    Contents: Longint;
    Value: Longint;
  end;

  TMiptex_T_m8 = array[0..0] of Miptex_T_m8;
  PMiptex_T_m8 = ^TMiptex_T_m8;

  Miptex_S_m8 = record
    Nfo: Miptex_T_m8;
    Data: Pointer;
  end;

  TMiptex_S_m8 = array[0..0] of Miptex_S_m8;
  PMiptex_S_m8 = ^TMiptex_S_m8;

  {
  ==============================================================================

    .Bsp file Format

  ==============================================================================
   }

const
  Bspversion_29 = 29; // Quake1, Hexen2
  Bspversion_30 = 30; // Half-Life, Blue-Shift, CounterStrike
  Bspversion_38 = 38; // Quake2, Heretic2
  Bspversion_41 = 41; // Daikatana (unsupported)
  Bspversion_46 = 46; // Quake3
  Bspversion_47 = 47; // RTCW
  { Upper Design Bounds }
  { Leaffaces, Leafbrushes, Planes, And Verts Are Still Bounded By }
  { 16 Bit Short Limits }
  Max_Map_Models = 1024;
  Max_Map_Brushes = 8192;
  Max_Map_Entities = 2048;
  Max_Map_Entstring = $40000;
  Max_Map_Texinfo = 8192;
  Max_Map_Areas = 256;
  Max_Map_Areaportals = 1024;
  Max_Map_Planes = 65536;
  Max_Map_Nodes = 65536;
  Max_Map_Brushsides = 65536;
  Max_Map_Leafs = 65536;
  Max_Map_Verts = 65536;
  Max_Map_Faces = 65536;
  Max_Map_Leaffaces = 65536;
  Max_Map_Leafbrushes = 65536;
  Max_Map_Portals = 65536;
  Max_Map_Edges = 128000;
  Max_Map_Surfedges = 256000;
  Max_Map_Lighting = $200000;
  Max_Map_Visibility = $100000;
  { Key / Value Pair Sizes }
  Max_Key = 32;
  Max_Value = 1024;
  {============================================================================= }

type

  Lump_T = record
    Fileofs: Longint;
    Filelen: Longint;
  end;

  TLump_T = array[0..0] of Lump_T;
  Plump_T = ^Tlump_T;

const
  Lump_Entities = 0;
  Lump_Planes = 1;
  Lump_Vertexes = 2;
  Lump_Visibility = 3;
  Lump_Nodes = 4;
  Lump_Texinfo = 5;
  Lump_Faces = 6;
  Lump_Lighting = 7;
  Lump_Leafs = 8;
  Lump_Leaffaces = 9;
  Lump_Leafbrushes = 10;
  Lump_Edges = 11;
  Lump_Surfedges = 12;
  Lump_Models = 13;
  Lump_Brushes = 14;
  Lump_Brushsides = 15;
  Lump_Pop = 16;
  Lump_Areas = 17;
  Lump_Areaportals = 18;
  Header_Lumps = 19;

// Quake1 support
const
  LUMP_ENTITIES_v1 = 0;
  LUMP_PLANES_v1 = 1;
  LUMP_TEXTURES_v1 = 2;
  LUMP_VERTEXES_v1 = 3;
  LUMP_VISIBILITY_v1 = 4;
  LUMP_NODES_v1	= 5;
  LUMP_TEXINFO_v1	= 6;
  LUMP_FACES_v1	= 7;
  LUMP_LIGHTING_v1 = 8;
  LUMP_CLIPNODES_v1	= 9;
  LUMP_LEAFS_v1	= 10;
  LUMP_MARKSURFACES_v1	= 11;
  LUMP_EDGES_v1	= 12;
  LUMP_SURFEDGES_v1	= 13;
  LUMP_MODELS_v1	= 14;
  HEADER_LUMPS_v1	= 15;

// This is our lumps enumeration for Quake3 bsp
const
  Lump_Entities_v3     = 0;            // Stores player/object positions, etc...
  Lump_Textures_v3     = 1;            // Stores texture information
  Lump_Planes_v3       = 2;            // Stores the splitting planes
  Lump_Nodes_v3        = 3;            // Stores the BSP nodes
  Lump_Leafs_v3        = 4;            // Stores the leafs of the nodes
  Lump_LeafFaces_v3    = 5;            // Stores the leaf's indices into the faces
  Lump_LeafBrushes_v3  = 6;            // Stores the leaf's indices into the brushes
  Lump_Models_v3       = 7;            // Stores the info of world models
  Lump_Brushes_v3      = 8;            // Stores the brushes info (for collision)
  Lump_BrushSides_v3   = 9;            // Stores the brush surfaces info
  Lump_Vertices_v3     = 10;           // Stores the level vertices
  Lump_MeshVerts_v3    = 11;           // Stores the model vertices offsets
  Lump_Shaders_v3      = 12;           // Stores the shader files (blending, anims..)
  Lump_Faces_v3        = 13;           // Stores the faces for the level
  Lump_Lightmaps_v3    = 14;           // Stores the lightmaps for the level
  Lump_LightVolumes_v3 = 15;           // Stores extra world lighting information
  Lump_VisData_v3      = 16;           // Stores PVS and cluster info (visibility)
  Lump_MaxLumps_v3     = 17;           // A constant to store the number of lumps

const
  FACE_POLYGON_v3 = 1;
  FACE_PATCH_v3 = 2;
  FACE_MESH_v3 = 3;
  FACE_BILLBOARD_v3 = 4;

type
  Dheader_T = record
    case integer of
    1: (Version_v1: Longint;
        Lumps_v1: array[0..HEADER_LUMPS_v1 - 1] of Lump_T);
    2: (Ident: Longint;
        Version: Longint;
        Lumps: array[0..Header_Lumps - 1] of Lump_T);
  end;

  TDheader_T = array[0..0] of Dheader_T;
  PDheader_T = ^TDheader_T;

  { for Sounds Or Lights }
  { Submodels Just Draw Faces }
  { Without Walking The Bsp Tree }

  SingleArray_3_T = array[0..2] of single;
  SingleArray_3_P = ^SingleArray_3_T;

  Dmodel_T = record
    Mins: SingleArray_3_T;
    Maxs: SingleArray_3_T;
    Origin: SingleArray_3_T;
    Headnode: Longint;
    Firstface: Longint;
    Numfaces : Longint;
  end;

  TDmodel_T = array[0..0] of Dmodel_T;
  PDmodel_T = ^TDmodel_T;

  Dvertex_T = record
    Point: SingleArray_3_T;
  end;

  Dvertex_P = ^Dvertex_T;

  TDvertex_T = array[0..0] of Dvertex_T;
  PDvertex_T = ^TDvertex_T;
  { 0-2 Are Axial Planes }

  DVertex_v3 = record
    Position      : array[0..2] of single; // (x, y, z) position.
    TextureCoord  : array[0..1] of single; // (u, v) texture coordinate
    LightmapCoord : array[0..1] of single; // (u, v) lightmap coordinate
    Normal        : array[0..2] of single; // (x, y, z) normal vector
    Color         : array[0..3] of byte    // RGBA color for the vertex
  end;

  DVertex_v3P = ^DVertex_v3;

  TDVertex_v3 = array[0..$FFFF] of DVertex_v3;
  PDVertex_v3 = ^TDVertex_v3;

  TMeshFace_v3 = record
    firstVertexIndex: integer;
    numVertices: integer;
    textureIndex: integer;
    lightmapIndex: integer;
    firstMeshIndex: integer;
    numMeshIndices: integer;
  end;

  PBiQuadricPatch = ^TBiQuadricPatch;
  TBiQuadricPatch = record
    ControlPoints: array[0..8] of DVertex_v3;
  end;

  PBiQuadricPatches = ^TBiQuadricPatches;
  TBiQuadricPatches = array[0..$FFFF] of TBiQuadricPatch;

  TBSP3Patch = record
    TextureIndex: integer;
    LightMapIndex: integer;
    Width, Height: integer;
    numQuadricPatches: integer;
    Patches: PBiQuadricPatches;
  end;

const
  Plane_X = 0;
  Plane_Y = 1;
  Plane_Z = 2;
  { 3-5 Are Non-Axial Planes Snapped to The Nearest }
  Plane_Anyx = 3;
  Plane_Anyy = 4;
  Plane_Anyz = 5;
  { Planes (X&~1) And (X&~1)+1 Are Allways Opposites }
  { Plane_X - Plane_Anyz ?Remove? Trivial to Regenerate }

type
  Dplane_T = record
    Normal: array[0..2] of single;
    Dist: single;
    Type_: Longint;
  end;

  TDplane_T = array[0..0] of Dplane_T;
  PDplane_T = ^TDplane_T;
  { Contents Flags Are Seperate Bits }
  { A Given Brush Can Contribute Multiple Content Bits }
  { Multiple Brushes Can Be In A Single Leaf }
  { These Definitions Also Need to Be In Q_Shared.H! }
  { Lower Bits Are Stronger, And Will Eat Weaker Brushes Completely }
  { An Eye Is Never Valid In A Solid }

const
  Contents_Solid = 1;
  { Translucent, But Not Watery }
  Contents_Window = 2;
  Contents_Aux = 4;
  Contents_Lava = 8;
  Contents_Slime = 16;
  Contents_Water = 32;
  Contents_Mist = 64;
  Last_Visible_Contents = 64;
  { Remaining Contents Are Non-Visible, And Don'T Eat Brushes }
  Contents_Areaportal = $8000;
  Contents_Playerclip = $10000;
  Contents_Monsterclip = $20000;
  { Currents Can Be Added to Any Other Contents, And May Be Mixed }
  Contents_Current_0 = $40000;
  Contents_Current_90 = $80000;
  Contents_Current_180 = $100000;
  Contents_Current_270 = $200000;
  Contents_Current_Up = $400000;
  Contents_Current_Down = $800000;
  { Removed Before Bsping An Entity }
  Contents_Origin = $1000000;
  { Should Never Be On A Brush, Only In Game }
  Contents_Monster = $2000000;
  Contents_Deadmonster = $4000000;
  { Brushes to Be Added After Vis Leafs }
  Contents_Detail = $8000000;
  { Auto Set if Any Surface Has Trans }
  Contents_Translucent = $10000000;
  Contents_Ladder = $20000000;
  { Value Will Hold The Light Strength }
  Surf_Light = $1;
  { Effects Game Physics }
  Surf_Slick = $2;
  { Don'T Draw, But Add to Skybox }
  Surf_Sky = $4;
  { Turbulent Water Warp }
  Surf_Warp = $8;
  Surf_Trans33 = $10;
  Surf_Trans66 = $20;
  { Scroll Towards Angle }
  Surf_Flowing = $40;
  { Don'T Bother Referencing The Texture }
  Surf_Nodraw = $80;
  { Make A Primary Bsp Splitter }
  Surf_Hint = $100;
  { Completely Ignore, Allowing Non-Closed Brushes }
  Surf_Skip = $200;
  { Negative Numbers Are -(Leafs+1), Not Nodes }
  { for Frustom Culling }
  { Counting Both Sides }

type
  Dnode_T = record
    Planenum: Longint;
    Children: array[0..1] of Longint;
    Mins: array[0..2] of Smallint;
    Maxs: array[0..2] of Smallint;
    Firstface: word;
    Numfaces: word;
  end;
  Dnode_P = ^Dnode_T;

  TDnode_T = array[0..0] of Dnode_T;
  PDnode_T = ^TDnode_T;

  { [S/T][Xyz Offset] }
  { Miptex Flags + Overrides }
  { Light Emission, Etc }
  { Texture Name (Textures/*.Wal) }
  { for Animations, -1 = end of Chain }

  SingleArray_2x4_T = array[0..1] of array[0..3] of single;
  SingleArray_2x4_P = ^SingleArray_2x4_T;

  Texinfo_T = record
    Vecs: SingleArray_2x4_T;
    Flags: Longint;
    Value: Longint;
    Texture: array[0..31] of char;
    Nexttexinfo: Longint;
  end;
  Texinfo_P = ^Texinfo_T;

  TTexinfo_T = array[0..0] of Texinfo_T;
  PTexinfo_T = ^Ttexinfo_T;

  Texinfo_S = Texinfo_T;
  { Note That Edge 0 Is Never Used, Because Negative Edge Nums Are Used for }
  { Counterclockwise Use of The Edge In A Face }
  { Vertex Numbers }


// Quake1 support

  Texinfo_T_v1 = record
    Vecs: array[0..1] of array[0..3] of single;
    miptex: Longint;
    flags: Longint;
  end;
  Texinfo_P_v1 = ^Texinfo_T_v1;
  TTexinfo_T_v1 = array[0..0] of Texinfo_T_v1;
  PTexinfo_T_v1 = ^Ttexinfo_T_v1;

  Texinfo_S_v1 = Texinfo_T_v1;

  Dedge_T = record
    V: array[0..1] of word;
  end;
  Dedge_P = ^Dedge_T;

  TDedge_T = array[0..0] of Dedge_T;
  PDedge_T = ^Tdedge_T;

const
  Maxlightmaps = 4;
  { We Must Support > 64K Edges }
  { Lighting Info }
  { Start of [Numstyles*Surfsize] Samples }

type
  Dface_T = record
    Planenum: word;
    Side: Smallint;
    Firstedge: Longint;
    Numedges: Smallint;
    Texinfo: Smallint;
    Styles: array[0..(Maxlightmaps)-1] of Byte;
    Lightofs: Longint;
  end;
  Dface_P = ^Dface_T;

  TDface_T = array[0..0] of Dface_T;
  PDface_T = ^Tdface_T;

  Dface_T_v3 = record
    textureID : Integer;                   // The index into the texture array
    effect    : Integer;                   // The index for the effects (or -1 = n/a)
    FaceType  : Integer;                   // 1=polygon, 2=patch, 3=mesh, 4=billboard
    startVertIndex : Integer;              // The starting index into this face's first vertex
    numOfVerts     : Integer;              // The number of vertices for this face
    meshVertIndex  : Integer;              // The index into the first meshvertex
    numMeshVerts   : Integer;              // The number of mesh vertices
    lightmapID     : Integer;              // The texture index for the lightmap
    lMapCorner : array[0..1] of Integer;   // The face's lightmap corner in the image
    lMapSize   : array[0..1] of Integer;   // The size of the lightmap section
    lMapPos  : array[0..2] of single;      // The 3D origin of lightmap.
                                           // The 3D space for s and t unit vectors.
    lMapVecs : array[0..1] of array[0..2] of single;
    vNormal  : array[0..2] of single;      // The face normal.
    Size : array[0..1] of Integer;         // The bezier patch dimensions.
  end;
  Dface_P_v3 = ^Dface_T_v3;

  TDface_T_v3 = array[0..0] of Dface_T_v3;
  PDface_T_v3 = ^TDface_T_v3;

  DTexture_v3 = record
    TextureName : array[0..63] of Char;    // The name of the texture w/o the extension
    flags    : Integer;                    // The surface flags (unknown)
    contents : Integer;                    // The content flags (unknown)
  end;

  TTexture_v3 = array[0..0] of DTexture_v3;
  PTexture_v3 = ^TTexture_v3;

  { Or of All Brushes (Not Needed?) }
  { for Frustum Culling }

  Dleaf_T = record
    Contents : Longint;
    Cluster : Smallint;
    Area : Smallint;
    Mins : array[0..2] of Smallint;
    Maxs : array[0..2] of Smallint;
    Firstleafface : word;
    Numleaffaces : word;
    Firstleafbrush : word;
    Numleafbrushes : word;
  end;

  TDleaf_T = array[0..0] of Dleaf_T;
  PDleaf_T = ^Tdleaf_T;
  { Facing Out of The Leaf }

  Dbrushside_T = record
    Planenum: word;
    Texinfo: Smallint;
  end;

  TDbrushside_T = array[0..0] of Dbrushside_T;
  PDbrushside_T = ^TDbrushside_T;

  Dbrush_T = record
    Firstside: Longint;
    Numsides: Longint;
    Contents: Longint;
  end;

  TDbrush_T = array[0..0] of Dbrush_T;
  PDbrush_T = ^Tdbrush_T;

const
  Angle_Up = -(1);
  Angle_Down = -(2);
  { The Visibility Lump Consists of A Header With A Count, then }
  { Byte Offsets for The Pvs And Phs of Each Cluster, then The Raw }
  { Compressed Bit Vectors }
  Dvis_Pvs = 0;
  Dvis_Phs = 1;
  { Bitofs[Numclusters][2] }

type
  Dvis_T = record
    Numclusters: Longint;
    Bitofs: array[0..7] of array[0..1] of Longint;
  end;
  TDvis_T = array[0..0] of Dvis_T;
  PDvis_T = ^Tdvis_T;
  { Each Area Has A List of Portals That Lead Into Other Areas }
  { When Portals Are Closed, Other Areas May Not Be Visible Or }
  { Hearable Even if The Vis Info Says That It Should Be }

  Dareaportal_T = record
    Portalnum: Longint;
    Otherarea: Longint;
  end;
  TDareaportal_T = array[0..0] of Dareaportal_T;
  PDareaportal_T = ^Tdareaportal_T;

  Darea_T = record
    Numareaportals : Longint;
    Firstareaportal : Longint;
  end;
  TDarea_T = array[0..0] of Darea_T;
  PDarea_T = ^Tdarea_T;

const
  DEFQUAKEIMPORTFACTOR = 32;
  DEFQUAKEIMPORTLFACTOR = 0.5;
  DEFTESSELLATIONLEVEL = 4;

  Pakid: Longint = $4B434150; // 'PACK' In Hex!
  WAD2id: LongInt = 843333975; // 'WAD2' in Hex!
  WAD3id: LongInt = 860111191; // 'WAD3' in Hex!

type
  FPakHead = packed record // A PAK Directory Entry
    Name: packed array[1..56] of char;
    Offs: Longint;
    Fsize: Longint;
  end;

  TFPakHeadArray = packed array[0..$FFFF] of FPakHead;
  PFPakHeadArray = ^TFPakHeadArray;

  FPakHead2 = packed record // Daikatana
    Name: packed array[1..56] of char;
    Offs: Longint;
    Fsize: Longint; // Decompressed size
    FCsize: Longint; // Compressed size
    FCompressed: Longint; // 0 -> uncompressed
  end;

  TFPakHeadArray2 = packed array[0..$FFFF] of FPakHead2;
  PFPakHeadArray2 = ^TFPakHeadArray2;

  FWadHead = packed record // A WAD2/WAD3 Directory Entry
  	Offs: LongInt;
  	disksize: Longint;
  	size: Longint;					// uncompressed
  	_type: char;
  	compression: char;
	  pad1, pad2: char;
  	name: packed array[1..16] of char; // must be null terminated
  end;

  TFWadHeadArray = packed array[0..$FFFF] of FWadHead;
  PFWadHeadArray = ^TFWadHeadArray;

type
  TCompressorCache = class(TObject)
  private
    fZip: TZipFile;
    fID: integer;
    fPosition: integer;
    fSize: integer;
    data: pointer;
  public
    constructor Create(aZip: TZipFile; aID: integer); virtual;
    destructor Destroy; override;
    function Read(var Buf; Sz: Integer): boolean;
    function Seek(pos: integer): boolean;
    property Position: integer read fPosition;
    property Size: integer read fSize;
  end;

  PakEntry = record // A Directory Entry Memory Image
    Pak: string[255];
    Name: string[255];
    SortName: string[32];
    Ofs, Sz: Integer;
    Hash: integer;
    ZIP: TZipFile;
    CSz: Integer;
    specialpak: integer; // 1 -> daikatana
    mem: TMemoryStream;
  end;

  TPakEntry = array[0..$FFFF] of PakEntry;
  PPakEntry = ^TPakEntry;

  Pakfile = record
    Entry: Integer;
    F: file;
    Z: TCompressorCache;
  end;

  TPakDir = class(TObject)
  private
    Qdir, Qmoddir: string;
    Qentries: PPakentry;
    Nentries: Integer;
    Maxentries: Integer;
    PAKS: TDXStringList;
    fSearchBspTextures: boolean;
    procedure AddVer1TextureEntries(Fn: string; Pakn: string);
    function CheckSpecialEntry(const se: integer): boolean;
  public
    constructor Create(Dir, Mdir: string; SearchBspTextures: boolean = true); virtual;
    destructor Destroy; override;
    procedure GetEntries(var s: TDXStringList);
    function Openfile(var F: Pakfile; Name: string): boolean;
    function OpenfileExtSearch(var F: Pakfile; Name: string): boolean;
    function Closefile(var F: Pakfile): boolean;
    function Qblockread(var F: Pakfile;var Buf; Sz: Integer): boolean;
    function Qseek(var F: Pakfile; Pos: Integer): boolean;
    procedure LoadPakFiles(Dir: string);
    procedure AddIfNotExist(var H: FPakHead; Pakn: string);
    procedure AddPak(Fn: string);
    procedure AddEntry(var H: FPakHead; Pakn: string); overload;
    procedure AddEntry(var H: FPakHead2; Pakn: string); overload;
    procedure AddEntry(var HD: FWADhead; Pakn: string); overload;
    procedure AddEntry(ZIPFILE: TZipFile; const ZIPFileName, EntryName: string; const index: integer); overload;
    function Qfilepos(var F: Pakfile): Integer;
    function Qfilesize(var F: Pakfile): Integer;
  end;

function AdjustQuake2EntryName(s: string): string;

{$ENDIF}

implementation

{$IFNDEF NO_IDSOFTGAMESSUPPORT}

uses
  se_DXDUtils;

{******** TCompressorCache ********}
constructor TCompressorCache.Create(aZip: TZipFile; aID: integer);
begin
  Inherited Create;
  fZip := aZip;
  fID := aID;
  fZip.GetZipFileData(fID, data, fSize);
  fPosition := 0;
end;

destructor TCompressorCache.Destroy;
begin
  FreeMem(data, fSize);
  Inherited Destroy;
end;

function TCompressorCache.Read(var Buf; Sz: Integer): boolean;
begin
  if fPosition + Sz > Size then
    result := false
  else
  begin
    System.Move(pointer(integer(data) + fPosition)^, Buf, Sz);
    fPosition := fPosition + Sz;
    result := true;
  end;
end;

function TCompressorCache.Seek(pos: integer): boolean;
begin
  if (pos < 0) or (pos > Size) then
    result := false
  else
  begin
    fPosition := pos;
    result := true;
  end;
end;

{****************}

function AdjustQuake2EntryName(s: string): string;
var i: integer;
begin
  result := s;
  for i := 1 to Length(s) do
    if result[i] = '/' then
      result[i] := '\';
end;

function MkHash(const s: string): integer;
var
  i: integer;
begin
  Result := 0;
  for I := 1 to length(s) do
  begin
    Result := ((Result shl 7) or (Result shr 25)) + ord(s[i]);
  end;
end;

{********* TPackDir *********}
constructor TPakDir.Create(Dir, Mdir: string; SearchBspTextures: boolean = true); // Dir = Q2 Dir, Mdir = Mod Dir.
var
  I: Integer;
  s1, s2: string;
  c: Char;
begin
  Inherited Create;
  fSearchBspTextures := SearchBspTextures;
  PAKS := TDXStringList.Create;
  Qdir := ExtractFilePath(Dir);
  Qmoddir := ExtractFilePath(Mdir);
  for I := 1 to Length(Qdir) do
    if Qdir[I] = '/' then Qdir[I] := '\';
  for I := 1 to Length(Qmoddir) do
    if Qmoddir[I] = '/' then Qmoddir[I] := '\';

  Qentries := nil;
  Nentries := 0;
  Maxentries := 0;
  if Qdir <> '' then
  begin
    if Qdir[Length(Qdir)] <> '\' then
      Qdir := Qdir + '\';
    Loadpakfiles(Qdir);
    s1 := RemoveExtention(Dir);
    s2 := ExtractFileExt(Dir);
    if s1 <> '' then
      if s1[Length(s1)] in ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'] then
      begin
        SetLength(s1, Length(s1) - 1);
        for c := '0' to '9' do
          if c <> s1[Length(s1)] then
            AddPak(s1 + c + s2);
      end;
    // Leave it last to override double files
    AddPak(Dir);
  end;
  if Qmoddir <> '' then
  begin
    if Qmoddir[Length(Qmoddir)] <> '\' then
      Qmoddir := Qmoddir + '\';
    if Pos(':', QmodDir) > 0 then
      Loadpakfiles(QModDir)
    else
      Loadpakfiles(Qdir + QModDir);
    AddPak(MDir);
  end;
  ReAllocMem(Qentries, Nentries * Sizeof(Pakentry)); // No Memory Leaks
  MaxEntries := Nentries;
end;

procedure TPakDir.LoadPakFiles(Dir: string);
const
  FileAttrs: Integer = faReadOnly	+ faHidden + faSysFile + faArchive + faAnyFile;
var
  I: Integer;
  S: string;
  sr: TSearchRec;

  procedure Add1;
  begin
    if (Pos('?', sr.Name) = 0) and (Pos('*', sr.Name) = 0) then
      AddPak(sr.Name)
  end;

begin
  if Dir <> '' then
  begin
    Getdir(0, S);
    {$I-}Chdir(Dir);{$I+}
    if IOResult = 0 then
    begin
      for I := 0 to 9 do
        if FileExists(Format(rsFmtPak, [i])) then
          Addpak(Format(rsFmtPak, [i]));
    end;

    for i := 1 to 3 do
    begin
      {$I-}Chdir('..\');{$I+}
      if IOResult = 0 then
      begin
        // Load WAD2 & WAD3 files
        if FindFirst(rsWadMask, FileAttrs, sr) = 0 then
        begin
          Add1;
          while FindNext(sr) = 0 do Add1;
          SysUtils.FindClose(sr);
        end;
      end;
      {$I-}Chdir('Valve');{$I+}
      if IOResult = 0 then
      begin
        // Load WAD2 & WAD3 files
        if FindFirst(rsWadMask, FileAttrs, sr) = 0 then
        begin
          Add1;
          while FindNext(sr) = 0 do Add1;
          SysUtils.FindClose(sr);
        end;
        {$I-}Chdir('..\');{$I+}
      end;
    end;

    {$I-}Chdir(Dir);{$I+}
    if IOResult = 0 then
    begin
      // Load WAD2 & WAD3 files
      if FindFirst(rsWadMask, FileAttrs, sr) = 0 then
      begin
        Add1;
        while FindNext(sr) = 0 do Add1;
        SysUtils.FindClose(sr);
      end;
    end;
    Chdir(S);
  end;
end;

procedure TPakDir.AddIfNotExist(var H: FPakHead; Pakn: string);
var
  F: PakFile;
begin
  if Openfile(F, H.Name) then
    CloseFile(F)
  else
    AddEntry(H, Pakn);
end;

procedure TPakDir.AddEntry(var H: FPakHead; Pakn: string); // Add A Pak Entry to Memory List
var
  S: string;
  I: Integer;
begin
  Inc(Nentries);
  if Nentries > Maxentries then
  begin
    Maxentries := Maxentries + 500;
    ReallocMem(Qentries, Maxentries * Sizeof(Pakentry));
  end;
  S := EmptyStr;
  for I := 1 to 56 do
    if H.Name[I] = '/' then
      S := S + '\'
    else if H.Name[I] <> #0 then
      S := S + UpCase(H.Name[I])
    else
      break;
{  for I := 1 to Length(S) do
    if S[I]='/' then
      S[I]:='\';}
  with Qentries[Nentries - 1] do
  begin
    Pak := Pakn;
    Name := S;
    SortName := ExtractFileName(Name);
    Hash := MkHash(SortName);
    Ofs := H.Offs;
    Sz := H.Fsize;
    ZIP := nil;
    CSz := 0;
    specialpak := 0;
    mem := nil;
  end;
  if fSearchBspTextures then
    if UpperCase(ExtractFileExt(s)) = UpperCase(rsExtBsp) then
      AddVer1TextureEntries(s, Pakn);
end;

procedure TPakDir.AddEntry(var H: FPakHead2; Pakn: string);
var
  S: string;
  I: Integer;
begin
  Inc(Nentries);
  if Nentries > Maxentries then
  begin
    Maxentries := Maxentries + 500;
    ReallocMem(Qentries, Maxentries * Sizeof(Pakentry));
  end;
  S := EmptyStr;
  for I := 1 to 56 do
    if H.Name[I] = '/' then
      S := S + '\'
    else if H.Name[I] <> #0 then
      S := S + UpCase(H.Name[I])
    else
      break;
{  for I := 1 to Length(S) do
    if S[I]='/' then
      S[I]:='\';}
  with Qentries[Nentries - 1] do
  begin
    Pak := Pakn;
    Name := S;
    SortName := ExtractFileName(Name);
    Hash := MkHash(SortName);
    Ofs := H.Offs;
    Sz := H.Fsize;
    ZIP := nil;
    CSz := H.FCsize;
    if H.FCompressed <> 0 then
      specialpak := 1
    else
      specialpak := 0;
    mem := nil;
  end;
  if fSearchBspTextures then
    if UpperCase(ExtractFileExt(s)) = UpperCase(rsExtBsp) then
      AddVer1TextureEntries(s, Pakn);
end;

procedure TPakDir.AddEntry(var HD: FWADhead; Pakn: string);
var
  S: string;
  I: Integer;
begin
  Inc(Nentries);
  if Nentries > Maxentries then
  begin
    Maxentries := Maxentries + 500;
    ReallocMem(Qentries, Maxentries * Sizeof(Pakentry));
  end;
  S := EmptyStr;
  for I := 1 to 16 do
    if HD.Name[I] = '/' then
      S := S + '\'
    else if HD.Name[I] <> #0 then
      S := S + UpCase(HD.Name[I])
    else
      break;
{  for I := 1 to Length(S) do
    if S[I]='/' then
      S[I]:='\';}
  with Qentries[Nentries - 1] do
  begin
    Pak := Pakn;
    Name := S;
    SortName := ExtractFileName(Name);
    Hash := MkHash(SortName);
    Ofs := HD.Offs;
    Sz := HD.size;
    ZIP := nil;
    CSz := 0;
    specialpak := 0;
    mem := nil;
  end;
  if fSearchBspTextures then
    if UpperCase(ExtractFileExt(s)) = UpperCase(rsExtBsp) then
      AddVer1TextureEntries(s, Pakn);
end;

procedure TPakDir.AddEntry(ZIPFILE: TZipFile; const ZIPFileName, EntryName: string; const index: integer);
begin
  Inc(Nentries);
  if Nentries > Maxentries then
  begin
    Maxentries := Maxentries + 500;
    ReallocMem(Qentries, Maxentries * Sizeof(Pakentry));
  end;
  with Qentries[Nentries - 1] do
  begin
    Pak := ZIPFileName;
    Name := UpperCase(EntryName);
    SortName := ExtractFileName(Name);
    Hash := MkHash(SortName);
    Ofs := index; // offset is the index to ZIP file
    Sz := 0;
    ZIP := ZIPFILE;
    CSz := 0;
    specialpak := 0;
    mem := nil;
  end;
end;

// bsp version 29 & 30 holds textures inside *.bsp file
procedure TPakDir.AddVer1TextureEntries(Fn: string; Pakn: string);
var
  MipTex_v1: Miptex_T_v1;
  NMipTexs_v1: integer;
  P: Pakfile;
  Ps, Ofs, i, j: integer;
  H: Dheader_T;
  eH: FPakHead;
begin
  if Openfile(P, Fn) then
  begin
    Qblockread(P, H, SizeOf(H));
    if (H.Version_v1 <> Bspversion_29) and (H.Version_v1 <> Bspversion_30) then
    begin
      //Error: Not A Quake1 bsp, HalfLife are stub references;
      CloseFile(P);
      Exit;
    end;

    Qseek(P, H.Lumps_v1[Lump_Textures_v1].Fileofs);

    Qblockread(P, NMipTexs_v1, SizeOf(NMipTexs_v1));

    for i := 1 to NMipTexs_v1 do
    begin
      QBlockread(P, Ofs, SizeOf(Ofs));
      Ps := Qfilepos(P);

      QSeek(P, H.Lumps_v1[Lump_Textures_v1].Fileofs + Ofs);
      eH.Offs := FilePos(P.F);
      Qblockread(P, MipTex_v1, SizeOf(MipTex_v1));

      for j := 1 to 16 do
        eH.Name[j] := MipTex_v1.Name[j - 1];
      for j := 17 to 56 do
        eH.Name[j] := #0;

      eH.Fsize := SizeOf(MipTex_v1) + MipTex_v1.Width * MipTex_v1.Height;

      if MipTex_v1.Offsets[0] <> 0 then // Avoid stub HL textures
        AddEntry(eH, Pakn);

      Qseek(P, Ps);
    end;
    CloseFile(P);
  end;
end;

function TPakDir.CheckSpecialEntry(const se: integer): boolean;
var
  fs: TFileStream;
  buf: PByteArray;
  buf64: array[0..63] of byte;
  ofs: byte;
  i, xin: integer;
  x, b: byte;
  e: ^PakEntry;

  function _readbyte: byte;
  begin
    Result := buf[xin];
    inc(xin);
  end;

  procedure _writebyte(const bb: byte);
  begin
    e.mem.Write(bb, SizeOf(bb));
  end;

begin
  if se < 0 then
  begin
    Result := False;
    Exit;
  end;

  e := @Qentries[se];

  if e.specialpak = 0 then
  begin
    Result := False;
    Exit;
  end;

  if e.mem <> nil then
  begin
    Result := True;
    Exit;
  end;

  e.mem := TMemoryStream.Create;
  e.mem.Size := e.Sz;
  e.mem.Position := 0;

  GetMem(buf, e.CSz);
  fs := TFileStream.Create(e.Pak, fmOpenRead or fmShareDenyWrite);
  try
    fs.Position := e.Ofs;
    fs.Read(buf^, e.CSz);
  finally
    fs.Free;
  end;

  xin := 0;
  while xin < e.CSz do
  begin
    x := _readbyte;
    if x < 64 then
    begin
      for i := 0 to x do
        _writebyte(_readbyte);
    end
    else if x < 128 then
    begin
      for i := 1 to x - 62 do
        _writebyte(0);
    end
    else if x < 192 then
    begin
      b := _readbyte;
      for i := 1 to x - 126 do
        _writebyte(b);
    end
    else if x < 254 then
    begin
      ofs := _readbyte;
      e.mem.Position := e.mem.Size - ofs - 2;
      e.mem.Read(buf64, x - 190);
      e.mem.Position := e.mem.Size;
      for i := 0 to x - 191 do
        _writebyte(buf64[i]);
    end
    else if x = 255 then
      Break;
  end;
  FreeMem(buf, e.CSz);

  e.mem.Position := 0;

  Result := True;
end;

procedure TPakDir.Addpak(Fn: string); // Add A Pak file to Memory List
var
  Nr: Integer;
  N, Id, Ofs:Integer;
  F: file;
  P: Pointer;
  I: Integer;
  z: TZipFile;
  pkid: integer;
begin
  Fn := UpperCase(ExpandFileName(Fn));

  if not FileExists(Fn) then
    Exit;

  if PAKS.IndexOf(Fn) > -1  then
    Exit;

  pkid := PAKS.Add(Fn);
  PAKS.Objects[pkid] := nil;

  Filemode := 0;
  Assign(F, Fn);
  Reset(F, 1);
  Filemode := 2;
  Blockread(F, Id, 4, N);
  if N <> 4 then
  begin
    Close(F);
    Exit;
  end;
  if (Id <> Pakid) and (Id <> WAD2Id) and (Id <> WAD3Id) and (id <> ZIPFILESIGNATURE) then
  begin
    if Id <> Bspversion_29 then
      if Id <> Bspversion_30 then
      begin
        Close(F);
        Exit;
      end;

    if fSearchBspTextures then
      if UpperCase(ExtractFileExt(AdjustQuake2EntryName(Fn))) = UpperCase(rsExtBsp) then
        AddVer1TextureEntries(Fn, Fn);

    Close(F);
    exit;
  end;

  if Id = Pakid then // PAK file
  begin
    BlockRead(F, Ofs, 4, N);
    if N <> 4 then
    begin
      Close(F);
      Exit;
    end;
    BlockRead(F, Nr, 4, N);
    if N <> 4 then
    begin
      Close(F);
      Exit;
    end;
    if Nr mod SizeOf(FPakHead) = 0 then
    begin
      Nr := Nr div SizeOf(FPakHead);
      Seek(F, Ofs);
      GetMem(P, Nr * SizeOf(FPakHead));
      Blockread(f, P^, Nr * SizeOf(FPakHead), N);
      for i := 0 to N div SizeOf(FPakHead) - 1 do
        AddEntry(PFPakHeadArray(P)[i], Fn);
      FreeMem(P, Nr * SizeOf(FPakHead));
    end
    else if Nr mod SizeOf(FPakHead2) = 0 then
    begin
      Nr := Nr div SizeOf(FPakHead2);
      Seek(F, Ofs);
      GetMem(P, Nr * SizeOf(FPakHead2));
      Blockread(f, P^, Nr * SizeOf(FPakHead2), N);
      for i := 0 to N div SizeOf(FPakHead2) - 1 do
        AddEntry(PFPakHeadArray2(P)[i], Fn);
      FreeMem(P, Nr * SizeOf(FPakHead2));
    end;
  end
  else if id = ZIPFILESIGNATURE then // pk3, pk4 file
  begin
    z := TZipFile.Create(Fn);
    PAKS.Objects[pkid] := z;
    for i := 0 to z.FileCount - 1 do
      AddEntry(z, Fn, z.Files[i], i);
  end
  else // WAD2 or WAD3
  begin
    BlockRead(F, Nr, 4, N);
    if N <> 4 then
    begin
      Close(F);
      Exit;
    end;
    BlockRead(F, Ofs, 4, N);
    if N <> 4 then
    begin
      Close(F);
      Exit;
    end;
    Seek(F, Ofs);
    GetMem(P, Nr * SizeOf(FWadHead));
    Blockread(f, P^, Nr * SizeOf(FWadHead), N);
    for i := 0 to N div SizeOf(FWadHead) - 1 do
      AddEntry(PFWadHeadArray(P)[i], Fn);
    FreeMem(P, Nr * SizeOf(FWadHead));

  end;
  Close(F);
end;

procedure TPakDir.GetEntries(var s: TDXStringList);
var
  i: integer;
begin
  if s = nil then
    s := TDXStringList.Create;
  for I := Nentries - 1 downto 0 do
    s.Add(Qentries[I].Name);
end;

function TPakDir.Openfile(var F: PakFile; Name: string): boolean;
var
  I: Integer;
  S: string;
  hcode: integer;
begin
  Result := False;
  F.Z := nil;
  Getdir(0, S);
  for I := 1 to Length(Name) do
    if Name[I] = '/' then Name[I] := '\';
  Filemode := 0;//Read-Only
  if Qmoddir <> EmptyStr then
  begin // Mod file Overrides Baseq2 file
    {$I-}
    Chdir(Qmoddir);
    if IOResult <> 0 then Exit;
    Assign(F.F, Name);
    Reset(F.F, 1);
    {$I+}
    if IOResult = 0 then
    begin
      F.Entry := -1;
      Result := True;
      Chdir(S);
      Exit;
    end; // Disk file Overrides Pak file
  end;
  {$I-}
  Chdir(Qdir);
  {$I+}
  if IOResult <> 0 then
  begin
    Result := False;
    Exit;
  end;
  {$I-}
  Assign(F.F, Name);
  Reset(F.F, 1);
  {$I+}
  if IOResult = 0 then
  begin
    F.Entry := -1;
    Result := True;
    Chdir(S);
    Exit;
  end; // Disk file Overrides Pak file
  Name := UpperCase(Name);
  hcode := MkHash(ExtractFileName(Name));
  for I := Nentries - 1 downto 0 do
  begin
    if (hcode = QEntries[i].Hash) and
       (Qentries[I].Name = Name) then
    begin // Found In Pak
      if CheckSpecialEntry(i) then
      else if Qentries[i].ZIP <> nil then
        F.Z := TCompressorCache.Create(Qentries[i].ZIP, Qentries[i].Ofs)
      else
      begin
        Assign(F.F, string(Qentries[I].Pak));
        Reset(F.F, 1);
        Seek(F.F, Qentries[I].Ofs);
      end;
      F.Entry := I;
      Result := True;
      Exit;
    end;
  end;
  Chdir(S);
end;

// Opens a file with extensive search, checks filenames only!!
function TPakDir.OpenfileExtSearch(var F: Pakfile; Name: string): boolean;
var
  I: Integer;
  S: string;
  hcode: integer;
begin
  Result := False;
  F.Z := nil;
  Getdir(0, S);
  for I := 1 to Length(Name) do
    if Name[I] = '/' then Name[I] := '\';
  Filemode := 0;//Read-Only
  if Qmoddir <> EmptyStr then
  begin // Mod file Overrides Baseq2 file
    {$I-}
    Chdir(Qmoddir);
    if IOResult <> 0 then Exit;
    Assign(F.F, Name);
    Reset(F.F, 1);
    {$I+}
    if IOResult = 0 then
    begin
      F.Entry := -1;
      Result := True;
      Chdir(S);
      Exit;
    end; // Disk file Overrides Pak file
  end;
  {$I-}
  Chdir(Qdir);
  {$I+}
  if IOResult <> 0 then
  begin
    Result := False;
    Exit;
  end;
  {$I-}
  Assign(F.F, Name);
  Reset(F.F, 1);
  {$I+}
  if IOResult = 0 then
  begin
    F.Entry := -1;
    Result := True;
    Chdir(S);
    Exit;
  end; // Disk file Overrides Pak file
  Name := UpperCase(ExtractFileName(Name));
  hcode := MkHash(Name);
  for I := Nentries - 1 downto 0 do
  begin
    if (hcode = QEntries[i].Hash) and
       (Qentries[I].SortName = Name) then
    begin // Found In Pak
      if CheckSpecialEntry(i) then
      else if Qentries[i].ZIP <> nil then
        F.Z := TCompressorCache.Create(Qentries[i].ZIP, Qentries[i].Ofs)
      else
      begin
        Assign(F.F, string(Qentries[I].Pak));
        Reset(F.F, 1);
        Seek(F.F, Qentries[I].Ofs);
      end;
      F.Entry := I;
      Result := True;
      Exit;
    end;
  end;
  Chdir(S);
end;

function TPakDir.Closefile(var F: Pakfile): boolean;
begin
//  if CheckSpecialEntry(Qentries[F.Entry]) then
  if (F.Entry >= 0) and (Qentries[F.Entry].mem <> nil) then
  begin
    Qentries[F.Entry].mem.Free;
    Qentries[F.Entry].mem := nil;
    {$I-}
    Close(F.F);
    {$I+}
  end
  else if F.Z <> nil then
  begin
    F.Z.Free;
    F.Z := nil
  end
  else
  begin
    {$I-}
    Close(F.F);
    {$I+}
  end;
  result := IOResult = 0;
end;

function TPakDir.Qblockread(var F: Pakfile; var Buf; Sz: Integer): boolean;
begin
  if CheckSpecialEntry(F.Entry) then
    result := Qentries[F.Entry].mem.Read(Buf, Sz) = Sz
  else if F.Z <> nil then
    result := F.Z.Read(Buf, Sz)
  else
  begin
    {$I-}
    Blockread(F.F, Buf, Sz);
    {$I+}
    result := IOResult = 0;
  end;
end;

function TPakDir.Qseek(var F: Pakfile; Pos: Integer): boolean;
begin
  if CheckSpecialEntry(F.Entry) then
  begin
    Qentries[F.Entry].mem.Position := Pos;
    result := Qentries[F.Entry].mem.Position = Pos;
  end
  else if F.Z <> nil then
    result := F.Z.Seek(pos)
  else
  begin
  {$I-}
    if F.Entry = -1 then
      Seek(F.F, Pos)
    else
      Seek(F.F, Qentries[F.Entry].Ofs + Pos);
    {$I+}
    result := IOResult = 0;
  end;
end;

function TPakDir.Qfilepos(var F: Pakfile): Integer;
begin
  if CheckSpecialEntry(F.Entry) then
    Result := Qentries[F.Entry].mem.Position
  else if F.Z <> nil then
    Result := F.Z.Position
  else
  begin
    Result := FilePos(F.F);
    if F.Entry <> -1 then
      Result := Result - Qentries[F.Entry].Ofs;
  end;
end;

function TPakDir.Qfilesize(var F: Pakfile): Integer;
begin
  if CheckSpecialEntry(F.Entry) then
    result := Qentries[F.Entry].mem.Size
  else if F.Z <> nil then
    result := F.Z.Size
  else if F.Entry <> -1 then
    result := Qentries[F.Entry].Sz
  else
    result := Filesize(F.F);
end;

destructor TPakDir.Destroy;
var
  i: integer;
begin
  for i := 0 to Nentries - 1 do
    if Qentries[i].mem <> nil then
      Qentries[i].mem.Free;
  ReallocMem(Qentries, 0);
  for i := 0 to PAKS.Count - 1 do
    if PAKS.Objects[i] <> nil then
      PAKS.Objects[i].Free;
  PAKS.Free;
  Inherited Destroy;
end;
{********* TPackDir end *********}

{$ENDIF}

end.

