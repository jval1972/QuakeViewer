//------------------------------------------------------------------------------
//
//  QuakeViewer: 3D Viewer for Quake I, II, III, RTCW, Half-Life etc
//  Copyright (C) 2004-2018 by Jim Valavanis
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
//  Main Form
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  New Site: https://sourceforge.net/projects/quakeviewer/
//  Old Site: http://www.geocities.ws/jimmyvalavanis/applications/quakeviewer.html
//------------------------------------------------------------------------------

{$I defs.inc}

unit Unit1;

{
  Έκδοση 1.5.
  1. Qupport for PK3 του Quake3
  2. Υποστήριξη εφέ ομίχλης.
  3. Διόρθωση υποστήριξης εφέ ομίχλης σε κάρτες γραφικών που δεν υποστηρίζουν
     TABLE_FOG
  4. navigation με το ποντίκι σε full screen
  5. Fixed halflife texture transparency

/// ******
Όταν αλλάζει το nearclippingplane αλλάζει και η συμπεριφορά της ομίχλης     

  Έκδοση 1.21.
  1. Safe Mode rendering added
  
  Έκδοση 1.2
  1. Πλήρη υποστήριξη αρχείων bsp του Quake3. (9/6/2004)
  
  Έκδοση 1.1
  1. Δυνατότητα αποθέκευσης σε εικόνα στο δίσκο

-------------------------
  Έκδοση 1.0.

}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  se_DirectX, se_DXClass, se_DXDraws, se_D3DUtils, se_Main, MessageBox, se_MyD3DUtils,
  se_DXInput, se_RTLCompileParams, AppEvnts, Tabs, ExtCtrls, Variants,
  ToolWin, ComCtrls, Menus, XPMenu, Aboutdlg, AnotherReg, Buttons, se_Quake2Utils,
  StdCtrls, ImgList, ShellAPI, BinaryData, ExtDlgs, xM8, xPPM, xTGA, se_DXDUtils,
  jpeg, pngimage;

type
  TQuakeViewerScene = class(TD3DScene)
  public
    procedure DrawLoop; override;
  end;


  TDXViewerForm = class(TDXForm)
    ApplicationEvents1: TApplicationEvents;
    ToolBar1: TToolBar;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    ToolButton1: TToolButton;
    Open2: TSpeedButton;
    ToolButton2: TToolButton;
    DisplayModeBox: TComboBox;
    FullScreen2: TSpeedButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    PrevMap: TSpeedButton;
    ComboBox1: TComboBox;
    NextMap: TSpeedButton;
    Edit1: TMenuItem;
    Copy1: TMenuItem;
    Copy2: TSpeedButton;
    ToolButton5: TToolButton;
    View1: TMenuItem;
    FullScreen1: TMenuItem;
    StatusBar1: TStatusBar;
    ImageList1: TImageList;
    Contactme1: TMenuItem;
    N2: TMenuItem;
    QuickInfo1: TMenuItem;
    Homepage1: TMenuItem;
    SavePictureDialog1: TSavePictureDialog;
    Save2: TSpeedButton;
    SaveAs1: TMenuItem;
    Options1: TMenuItem;
    Hardware1: TMenuItem;
    SafeModerendering1: TMenuItem;
    Information1: TMenuItem;
    Fog1: TMenuItem;
    Texturefiltering1: TMenuItem;
    DXDrawPanel: TPanel;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ApplicationEvents1Activate(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure OpenClick(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure FullScreenClick(Sender: TObject);
    procedure DisplayModeBoxChange(Sender: TObject);
    procedure PrevMapClick(Sender: TObject);
    procedure NextMapClick(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure CopyClick(Sender: TObject);
    procedure ApplicationEvents1Hint(Sender: TObject);
    procedure Contactme1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure QuickInfo1Click(Sender: TObject);
    procedure Homepage1Click(Sender: TObject);
    procedure Save2Click(Sender: TObject);
    procedure Hardware1Click(Sender: TObject);
    procedure Options1Click(Sender: TObject);
    procedure SafeModerendering1Click(Sender: TObject);
    procedure DXDrawClick(Sender: TObject);
    procedure Fog1Click(Sender: TObject);
    procedure Information1Click(Sender: TObject);
    procedure Texturefiltering1Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    DXDraw: TDXDraw;
    DXTimer: TDXTimer;
    DXInput: TDXInput;

    mmXPMenu1: TmmXPMenu;
    AboutDialog1: TAboutDialog;

    regFormRestorer1: TFormRestorer;
    regSceneBitCount: TVariantProfile;
    regSceneWidth: TVariantProfile;
    regSceneHeight: TVariantProfile;
    regSafeMode: TVariantProfile;
    regTextureFiltering: TVariantProfile;
    regUseHardwareAcceleration: TVariantProfile;
    regShowFog: TVariantProfile;
    regShowInfo: TVariantProfile;
    regMaxPolygonVertexes: TVariantProfile;

    ErrorNoMapSelectedMessageBox: TMessageBox;
    ErrorNoFileSelectMessageBox: TMessageBox;
    ClipboardErrorMessageBox: TMessageBox;
    ErrorMessageBox: TMessageBox;
    MessageBox1: TMessageBox;

    Scene: TQuakeViewerScene;
    WalkTime: double;
    OldTime: double; // last render timer

    MapName: string;
    MainPAK, BspFile: TFileName;

    Factor: integer;
    lFactor: single;
    maxpverts: integer;
    TessellationLevel: integer;
    doFiltering: boolean;

    FloorInfo: TD3DQuadrangleInfo;
    bCube: TBoundingCube;
    bCubeSize: TD3DValue;
    TotalSceneTriangles: integer;
    dorecalcculls: boolean;

    serrormessage: string;

    procedure CreateComponents;
    procedure DestroyComponents;

    function GetCullDistance: TD3DValue;

    procedure FrameMovie(const Time: Double);
    procedure DrawTheScene;

    procedure LoadMap;

    procedure SetHardWare(flag: boolean; fStartup: boolean = False);
    procedure SetSafeMode(flag: boolean; fStartup: boolean = False);
    procedure AdjustFullScreen(const rlevel: integer = 0);
    procedure AdjustFog;
  protected
    IsLoading: boolean;
    // Sky support
    skyTexture: string;
    Texture_sky2: TDirect3DTexture2;
    SPHERE_0004_ID_0008_Vertexes: PD3DLVertexArray;

    procedure DXTimerTimer(Sender: TObject; LagCount: Integer);
    procedure DXDrawInitializeSurface(Sender: TObject);
    procedure DXDrawInitialize(Sender: TObject);
    procedure DXDrawFinalize(Sender: TObject);

    procedure AdjustFocus;
    procedure WMSysCommand(var Msg: TMessage); message WM_SysCommand;
  public
    { Public declarations }
    AppConfigKey1: TAppConfigKey;
  end;

// Compiler $DEFINES (for the "surfaces" engine)
{
    NO_SCRIPTS,
    NO_D3DTRIANGLES,
    NO_D3DSTUBOBJECTS,
    NO_D3DEXOBJECTS,
    NO_D3DBILLBOARDS,
    NO_D3DCUBES,
    NO_D3DSPHERES,
    NO_D3DCONES,
    NO_D3DCYLINDERS,
    NO_D3DPLUGINS,
    NO_D3DRINGS,
    NO_D3DSECTORCOLLECTIONS,
    NO_D3DTEXTS,
    NO_D3DPROCEDURALOBJECTS,
    NO_D3DSOUNDS,
    NO_AVI.

NO_D3DTRIANGLES;NO_D3DSTUBOBJECTS;NO_D3DBILLBOARDS;NO_D3DCUBES;NO_D3DSPHERES;
NO_D3DCONES;NO_D3DCYLINDERS;NO_D3DRINGS;NO_D3DPLUGINS;NO_D3DTEXTS;
NO_D3DPROCEDURALOBJECTS;NO_SCRIPTS;NO_AVI;NO_D3DSOUNDS;NO_DOOMTHINGS;
NO_D3DEXOBJECTS;NO_D3DSECTORCOLLECTIONS}

var
  DXViewerForm: TDXViewerForm;

const
  MINTRVERTEXES = 50;
  MAXTRVERTEXES = 1000;
  DEFTRVERTEXES = 100;

implementation

{$R *.DFM}

uses
  SyncObjs, Math, OpenQuakeMapFrm, Clipbrd, QuickInfoFrm, se_DXTables, zBitmap,
  Splash, qv_argv, se_Utils, qv_globals;

type
  TD3DSceneDisplayParam = class(TObject)
    Width: integer;
    Height: integer;
    BitCount: integer;
    constructor Create(aWidth, aHeight, aBitCount: integer); virtual;
  end;

{ *** TD3DSceneDisplayParam *** }
constructor TD3DSceneDisplayParam.Create(aWidth, aHeight, aBitCount: integer);
begin
  Inherited Create;
  Width := aWidth;
  Height := aHeight;
  BitCount := aBitCount;
end;

procedure TQuakeViewerScene.DrawLoop;
var
  C: TDirectDrawSurfaceCanvas;
  skyverts: integer;
begin
  Inherited;
  skyverts := 0;
  if not DXViewerForm.Fog1.Checked then
  begin
    if (DXViewerForm.Texture_sky2 <> nil) and (DXViewerForm.skyTexture <> '') then
    begin
      DXDraw.D3DDevice7.SetRenderState(D3DRENDERSTATE_CULLMODE, Ord(D3DCULL_NONE));
      DXDraw.D3DDevice7.SetTexture(0, DXViewerForm.Texture_sky2.Surface.IDDSurface7);
      DXDraw.D3DDevice7.DrawPrimitive(D3DPT_TRIANGLESTRIP, D3DFVF_LVERTEX, DXViewerForm.SPHERE_0004_ID_0008_Vertexes[0], 320, 0);
      skyverts := (320 - 2) div 3;
    end;
  end;

  if DXViewerForm.Information1.Checked and (DXViewerForm.TotalSceneTriangles > 0) then
  begin
    C := DXDraw.Surface.Canvas;
    C.Brush.Style := bsClear;
    C.Font.Color := clWhite;
    C.TextOut(5, DXDraw.Surface.Height - 48, 'FPS: ' + IntToStr(round(FPS_Smooth)));
    C.TextOut(5, DXDraw.Surface.Height - 32, 'Total triangles: ' + IntToStr(DXViewerForm.TotalSceneTriangles + ((320 - 2) div 3)));
    C.TextOut(5, DXDraw.Surface.Height - 16, 'Drawn triangles: ' + IntToStr(GetNumDrawnTriangles + skyverts));
    C.Release;
  end;
end;

// DirectInput keys
function KeyAssignProc: TKeyAssignList;
begin
  FillChar(Result, SizeOf(Result), 0);

  AssignKey(Result, isUp,      [VK_UP, VK_NUMPAD8, Ord('W')]);
  AssignKey(Result, isDown,    [VK_DOWN, VK_NUMPAD2, Ord('S')]);
  AssignKey(Result, isLeft,    [VK_LEFT, VK_NUMPAD4]);
  AssignKey(Result, isRight,   [VK_RIGHT, VK_NUMPAD6]);
  AssignKey(Result, isButton1, [VK_NUMPAD9]);
  AssignKey(Result, isButton2, [VK_NUMPAD3]);
  AssignKey(Result, isButton3, [VK_ADD, 187]);
  AssignKey(Result, isButton4, [VK_SUBTRACT, 189]);
  AssignKey(Result, isButton5, [VK_SHIFT]); // run!
  AssignKey(Result, isButton6, [Ord('A')]); // strafe left
  AssignKey(Result, isButton7, [Ord('D')]); // strafe right
end;

procedure TDXViewerForm.WMSysCommand(var Msg: TMessage);
begin
  if (Msg.WParam = SC_SCREENSAVE) then
    if doFullScreen in DXDraw.NowOptions then
    begin
      Msg.Result := 0;
      exit;
    end;
  DefWindowProc(Handle, Msg.Msg, Msg.WParam, Msg.LParam);
end;

procedure TDXViewerForm.DXTimerTimer(Sender: TObject; LagCount: Integer);
var
  cs: TCriticalSection;
begin
  cs := TCriticalSection.Create;
  try
    cs.Enter;
    DrawTheScene;
  finally
    cs.Release;
    cs.Free;
  end;
  if serrormessage <> '' then
  begin
    ErrorMessageBox.Execute(serrormessage);
    serrormessage := '';
  end;
end;

procedure TDXViewerForm.DrawTheScene;
begin
  if Scene = nil then Exit;

  if DXDraw.CanDraw and not IsLoading then
  begin
    { Frame Movie }
    FrameMovie(Scene.Time);

    if doFiltering then
    begin
      DXDraw.D3DDevice7.SetTextureStageState(0, D3DTSS_MINFILTER, ord(D3DTFN_LINEAR));
      DXDraw.D3DDevice7.SetTextureStageState(0, D3DTSS_MAGFILTER, ord(D3DTFG_LINEAR));
    end
    else
    begin
      DXDraw.D3DDevice7.SetTextureStageState(0, D3DTSS_MINFILTER, ord(D3DTFN_POINT));
      DXDraw.D3DDevice7.SetTextureStageState(0, D3DTSS_MAGFILTER, ord(D3DTFG_POINT));
    end;

    Scene.DrawWithBackColor(clBlack);

    DXDraw.Flip;
  end;
end;

procedure TDXViewerForm.DXDrawInitializeSurface(Sender: TObject);
var
  vp: TD3DViewport7;
  mtrl: TD3DMaterial7;
  matProj: TD3DMatrix;
begin
  { Viewport }
  FillChar(vp, SizeOf(vp), 0);
  vp.dwX := 0;
  vp.dwY := 0;
  vp.dwWidth := DXDraw.SurfaceWidth;
  vp.dwHeight := DXDraw.SurfaceHeight;
  vp.dvMinZ := 0.0;
  vp.dvMaxZ := 1.0;

  DXDraw.D3DDevice7.SetViewport(vp);

  {  Material  }
  FillChar(mtrl, SizeOf(mtrl), 0);
  mtrl.ambient.r := 1.0;
  mtrl.ambient.g := 1.0;
  mtrl.ambient.b := 1.0;
  mtrl.ambient.a := 1.0;
  mtrl.specular.r := 0.0;
  mtrl.specular.g := 0.0;
  mtrl.specular.b := 0.0;
  mtrl.specular.a := 0.0;
  mtrl.diffuse.r := 1.0;
  mtrl.diffuse.g := 1.0;
  mtrl.diffuse.b := 1.0;
  DXDraw.D3DDevice7.SetMaterial(mtrl);
  DXDraw.D3DDevice7.SetRenderState(D3DRENDERSTATE_AMBIENT, $ffffffff);
  DXDraw.D3DDevice7.SetRenderState(D3DRENDERSTATE_SPECULARENABLE, 1);

// Set the projection matrix.
  FillChar(matProj, SizeOf(matProj), 0);
  matProj._11 :=  1.0;
  matProj._22 :=  1.0;
  matProj._33 :=  1.0;
  matProj._34 :=  1.0;
  matProj._43 := -1.0;
  DXDraw.D3DDevice7.SetTransform(D3DTRANSFORMSTATE_PROJECTION, matProj);

//Note: in DX7, setting D3DRENDERSTATE_LIGHTING to FALSE is needed to
// turn off vertex lighting (and use the color in the D3DLVERTEX instead.)
  DXDraw.D3DDevice7.SetRenderState(D3DRENDERSTATE_LIGHTING, 0);

  DXDraw.D3DDevice7.SetTextureStageState(0, D3DTSS_COLORARG1, ord(D3DTA_TEXTURE));
  DXDraw.D3DDevice7.SetTextureStageState(0, D3DTSS_COLORARG2, ord(D3DTA_DIFFUSE));
  DXDraw.D3DDevice7.SetTextureStageState(0, D3DTSS_COLOROP, ord(D3DTOP_MODULATE));
  DXDraw.D3DDevice7.SetTextureStageState(0, D3DTSS_ALPHAOP, ord(D3DTOP_SELECTARG1));
  DXDraw.D3DDevice7.SetTextureStageState(0, D3DTSS_ALPHAARG1, ord(D3DTA_TEXTURE));
  DXDraw.D3DDevice7.SetTextureStageState(0, D3DTSS_MINFILTER, ord(D3DTFN_LINEAR));
  DXDraw.D3DDevice7.SetTextureStageState(0, D3DTSS_MAGFILTER, ord(D3DTFG_LINEAR));
//  DXDraw.D3DDevice7.SetRenderState(D3DRENDERSTATE_NORMALIZENORMALS, ord(True));
  DXDraw.D3DDevice7.SetRenderState(D3DRENDERSTATE_COLORKEYENABLE, 1);

//  DXDraw.D3DDevice7.SetRenderState(D3DRENDERSTATE_ANTIALIAS, ord(D3DANTIALIAS_SORTINDEPENDENT));

  if Scene <> nil then Scene.ForceRecalc;
  AdjustFog;
end;

resourceString
  rsFmtMode = '%dx%d %dbit';
  rsFmtTitle = 'QuakeViewer - Map: %s - File: %s';
  rsFmtTitle2 = 'QuakeViewer - Map: %s';

const
  DefSceneWidth = 640;
  DefSceneHeight = 480;
  DefSceneBitCount = 16;
  ValidSceneBitCounts = [16, 32];

procedure TDXViewerForm.DXDrawInitialize(Sender: TObject);
var
  matView: TD3DMatrix;
  i: integer;
  aParam: TD3DSceneDisplayParam;
  sW, sH: integer;
begin
  if DisplayModeBox.Items.Count = 0 then
  begin
    for i := 0 to DXDraw.Display.Count - 1 do
      if DXDraw.Display[i].BitCount in ValidSceneBitCounts then
      begin
        aParam := TD3DSceneDisplayParam.Create(
          DXDraw.Display[i].Width,
          DXDraw.Display[i].Height,
          DXDraw.Display[i].BitCount);
        DisplayModeBox.Items.AddObject(Format(rsFmtMode,
          [aParam.Width, aParam.Height, aParam.BitCount]), aParam);
      end;
    if VarIsEmpty(regSceneWidth.Value) then
      regSceneWidth.Value := DefSceneWidth;
    if VarIsEmpty(regSceneHeight.Value) then
      regSceneHeight.Value := DefSceneHeight;
    if VarIsEmpty(regSceneBitCount.Value) then
      regSceneBitCount.Value := DefSceneBitCount;
    DisplayModeBox.ItemIndex := DisplayModeBox.Items.IndexOf(
      Format(rsFmtMode,
        [Integer(regSceneWidth.Value),
         Integer(regSceneHeight.Value),
         Integer(regSceneBitCount.Value)]));
    if DisplayModeBox.ItemIndex = -1 then
      DisplayModeBox.ItemIndex := DisplayModeBox.Items.IndexOf(
        Format(rsFmtMode,[DefSceneWidth, DefSceneHeight, DefSceneBitCount]));
    TryFocusControl(DXDraw);
  end;

  FillChar(matView, SizeOf(matView), 0);
  sW := GetSystemMetrics(SM_CXSCREEN);
  sH := GetSystemMetrics(SM_CYSCREEN);
  if (sW > 0) and (sH > 0) then
    matView._11 := sH / sW
  else
    matView._11 := 0.75; // Screen Aspect ratio
  matView._22 := 1;
  matView._33 := 1;
  DXDraw.D3DDevice7.SetTransform(D3DTRANSFORMSTATE_VIEW, matView);
  DXTimer.Enabled := True;
end;

procedure TDXViewerForm.CreateComponents;
begin
  DXDraw := TDXDraw.Create(self);
  DXDraw.Parent := DXDrawPanel;
  DXDraw.Left := 0;
  DXDraw.Top := 0;
  DXDraw.Width := 640;
  DXDraw.Height := 480;
  DXDraw.AutoInitialize := True;
  DXDraw.AutoSize := True;
  DXDraw.Color := clBlack;
  DXDraw.Display.FixedBitCount := False;
  DXDraw.Display.FixedRatio := True;
  DXDraw.Display.FixedSize := False;
  DXDraw.Options := [doAllowReboot, doWaitVBlank, doAllowPalette256, doStretch, doCenter, doFlip, do3D, doDirectX7Mode, doHardware, doSelectDriver, doZBuffer];
  DXDraw.SurfaceHeight := 480;
  DXDraw.OnFinalize := DXDrawFinalize;
  DXDraw.OnInitialize := DXDrawInitialize;
  DXDraw.OnInitializeSurface := DXDrawInitializeSurface;
  DXDraw.Align := alClient;
  DXDraw.DragMode := dmAutomatic;
  DXDraw.TabOrder := 2;
  DXDraw.Visible := False;
  DXDraw.OnClick := DXDrawClick;

  DXTimer := TDXTimer.Create(self);
  DXTimer.ActiveOnly := True;
  DXTimer.Enabled := False;
  DXTimer.Interval := 1;
  DXTimer.OnTimer := DXTimerTimer;

  DXInput := TDXInput.Create(self);
  DXInput.UseDirectInput := False;
  DXInput.UseDirectInput := True;
  DXInput.ActiveOnly := True;

  DXInput.Joystick.BindInputStates := True;
  DXInput.Joystick.Enabled := True;
  DXInput.Joystick.ForceFeedback := False;
  DXInput.Joystick.AutoCenter := True;
  DXInput.Joystick.DeadZoneX := 50;
  DXInput.Joystick.DeadZoneY := 50;
  DXInput.Joystick.DeadZoneZ := 50;
  DXInput.Joystick.ID := 0;
  DXInput.Joystick.RangeX := 1000;
  DXInput.Joystick.RangeY := 1000;
  DXInput.Joystick.RangeZ := 1000;

  DXInput.Keyboard.BindInputStates := True;
  DXInput.Keyboard.Enabled := True;
  DXInput.Keyboard.ForceFeedback := False;

  DXInput.Mouse.BindInputStates := True;
  DXInput.Mouse.Enabled := True;
  DXInput.Mouse.ForceFeedback := False;

  mmXPMenu1 := TmmXPMenu.Create(self);
  mmXPMenu1.Font.Charset := DEFAULT_CHARSET;
  mmXPMenu1.Font.Color := clMenuText;
  mmXPMenu1.Font.Height := -11;
  mmXPMenu1.Font.Name := 'Tahoma';
  mmXPMenu1.Font.Style := [];
  mmXPMenu1.Color := clBtnFace;
  mmXPMenu1.IconBackColor := clBtnFace;
  mmXPMenu1.MenuBarColor := clBtnFace;
  mmXPMenu1.SelectColor := clHighlight;
  mmXPMenu1.SelectBorderColor := clHighlight;
  mmXPMenu1.SelectFontColor := clMenuText;
  mmXPMenu1.DisabledColor := clInactiveCaption;
  mmXPMenu1.SeparatorColor := clBtnFace;
  mmXPMenu1.CheckedColor := clHighlight;
  mmXPMenu1.IconWidth := 24;
  mmXPMenu1.DrawSelect := True;
  mmXPMenu1.UseSystemColors := True;
  mmXPMenu1.OverrideOwnerDraw := False;
  mmXPMenu1.Gradient := False;
  mmXPMenu1.FlatMenu := False;
  mmXPMenu1.MakeToolbars := False;
  mmXPMenu1.MakeControlBars := False;
  mmXPMenu1.AutoDetect := False;
  mmXPMenu1.Active := True;

  AboutDialog1 := TAboutDialog.Create(self);
  AboutDialog1.ProductName := 'QuakeViewer';
  AboutDialog1.Version := 'Version 1.7';
  AboutDialog1.Copyright := '© 2004-2018, Jim Valavanis, <jimmyvalavanis@yahoo.gr>';
  AboutDialog1.Comments := 'Quake 3D Viewer for Windows';

  AppConfigKey1 := TAppConfigKey.Create(self);
  AppConfigKey1.ApplicationName := 'QuakeViewer';
  AppConfigKey1.ApplicationVersion := '1.7';
  AppConfigKey1.CompanyName := 'Jim Valavanis';
  AppConfigKey1.Name := 'AppConfigKey1';

  regFormRestorer1 := TFormRestorer.Create(self);
  regFormRestorer1.ParentKey := AppConfigKey1;
  regFormRestorer1.Name := 'FormRestorer1';
  regFormRestorer1.Restoring := frSizeAndPosition;
  regFormRestorer1.Restore;

  regSceneBitCount := TVariantProfile.Create(self);
  regSceneBitCount.Key := regFormRestorer1;
  regSceneBitCount.Name := 'SceneBitCount';

  regShowInfo := TVariantProfile.Create(self);
  regShowInfo.Key := regFormRestorer1;
  regShowInfo.Name := 'ShowInfo';

  regMaxPolygonVertexes := TVariantProfile.Create(self);
  regMaxPolygonVertexes.Key := regFormRestorer1;
  regMaxPolygonVertexes.Name := 'MaxPolygonVertexes';

  regSceneWidth := TVariantProfile.Create(self);
  regSceneWidth.Key := regFormRestorer1;
  regSceneWidth.Name := 'SceneWidth';

  regSceneHeight := TVariantProfile.Create(self);
  regSceneHeight.Key := regFormRestorer1;
  regSceneHeight.Name := 'SceneHeight';

  regUseHardwareAcceleration := TVariantProfile.Create(self);
  regUseHardwareAcceleration.Key := regFormRestorer1;
  regUseHardwareAcceleration.Name := 'UseHardwareAcceleration';

  regSafeMode := TVariantProfile.Create(self);
  regSafeMode.Key := regFormRestorer1;
  regSafeMode.Name := 'SafeMode';

  regTextureFiltering := TVariantProfile.Create(self);
  regTextureFiltering.Key := regFormRestorer1;
  regTextureFiltering.Name := 'TextureFiltering';

  regShowFog := TVariantProfile.Create(self);
  regShowFog.Key := regFormRestorer1;
  regShowFog.Name := 'ShowFog';

  ErrorNoMapSelectedMessageBox := TMessageBox.Create(self);
  ErrorNoMapSelectedMessageBox.Caption := 'QuakeViewer';
  ErrorNoMapSelectedMessageBox.Text := 'You did not select a valid map to preview.';
  ErrorNoMapSelectedMessageBox.Buttons := mbxOK;
  ErrorNoMapSelectedMessageBox.Icon := mbxIconError;
  ErrorNoMapSelectedMessageBox.DefaultButton := mbxDefButton1;
  ErrorNoMapSelectedMessageBox.Modality := mbxTaskModal;
  ErrorNoMapSelectedMessageBox.TextAlignment := mbxLeft;

  ErrorNoFileSelectMessageBox := TMessageBox.Create(self);
  ErrorNoFileSelectMessageBox.Caption := 'QuakeViewer';
  ErrorNoFileSelectMessageBox.Text := 'You did not select a valid filename.';
  ErrorNoFileSelectMessageBox.Buttons := mbxOK;
  ErrorNoFileSelectMessageBox.Icon := mbxIconError;
  ErrorNoFileSelectMessageBox.DefaultButton := mbxDefButton1;
  ErrorNoFileSelectMessageBox.Modality := mbxTaskModal;
  ErrorNoFileSelectMessageBox.TextAlignment := mbxLeft;


  ClipboardErrorMessageBox := TMessageBox.Create(self);
  ClipboardErrorMessageBox.Caption := 'QuakeViewer';
  ClipboardErrorMessageBox.Text := 'Can not copy to clipboard an empty bitmap!';
  ClipboardErrorMessageBox.Buttons := mbxOK;
  ClipboardErrorMessageBox.Icon := mbxIconError;
  ClipboardErrorMessageBox.DefaultButton := mbxDefButton1;
  ClipboardErrorMessageBox.Modality := mbxTaskModal;
  ClipboardErrorMessageBox.TextAlignment := mbxLeft;

  ErrorMessageBox := TMessageBox.Create(self);
  ErrorMessageBox.Caption := 'QuakeViewer';
  ErrorMessageBox.Text := 'Write your message here.';
  ErrorMessageBox.Buttons := mbxOK;
  ErrorMessageBox.Icon := mbxIconStop;
  ErrorMessageBox.DefaultButton := mbxDefButton1;
  ErrorMessageBox.Modality := mbxTaskModal;
  ErrorMessageBox.TextAlignment := mbxLeft;

  MessageBox1 := TMessageBox.Create(self);
  MessageBox1.Caption := 'QuakeViewer';
  MessageBox1.Text := 'Can not determine file type!';
  MessageBox1.Buttons := mbxOK;
  MessageBox1.Icon := mbxIconStop;
  MessageBox1.DefaultButton := mbxDefButton1;
  MessageBox1.Modality := mbxTaskModal;
  MessageBox1.TextAlignment := mbxLeft;
end;

procedure TDXViewerForm.DestroyComponents;
begin
  regMaxPolygonVertexes.Value := maxpverts;
  regMaxPolygonVertexes.Free;
  regShowFog.Free;
  regUseHardwareAcceleration.Free;
  regSceneBitCount.Free;
  regSceneWidth.Free;
  regSceneHeight.Free;
  regSafeMode.Free;
  regTextureFiltering.Free;
  regShowInfo.Free;
  regFormRestorer1.Store;
  regFormRestorer1.Free;
  AppConfigKey1.Free;

  mmXPMenu1.Free;
  AboutDialog1.Free;

  ErrorNoMapSelectedMessageBox.Free;
  ErrorNoFileSelectMessageBox.Free;
  ClipboardErrorMessageBox.Free;
  MessageBox1.Free;
  ErrorMessageBox.Free;
end;

procedure TDXViewerForm.FormCreate(Sender: TObject);
var
  i: integer;
begin
  if QV_CheckParam('-nosplash') = 0 then
  begin
    SplashForm := TSplashForm.Create(nil);
    SplashForm.Update;
  end;

  Visible := False;
  Screen.Cursor := crHourglass;

  try
    CreateComponents;

    Color := clBlack;
    DXDrawPanel.Color := clBlack;

    serrormessage := '';
    IsLoading := False;
    skyTexture := '';
    Texture_sky2 := nil;

    FillChar(FloorInfo, SizeOf(FloorInfo), Chr(0));
    FloorInfo.Key := 1;
    FloorInfo.x1 := -10000.0;
    FloorInfo.x2 :=  10000.0;
    FloorInfo.x3 := -10000.0;
    FloorInfo.x4 :=  10000.0;
    FloorInfo.y1 := -1000.0;
    FloorInfo.y2 := -1000.0;
    FloorInfo.y3 := -1000.0;
    FloorInfo.y4 := -1000.0;
    FloorInfo.z1 :=  10000.0;
    FloorInfo.z2 :=  10000.0;
    FloorInfo.z3 := -10000.0;
    FloorInfo.z4 := -10000.0;
    FloorInfo.C1 := RGB(192, 192, 192);
    FloorInfo.C2 := RGB(192, 192, 192);
    FloorInfo.C3 := RGB(192, 192, 192);
    FloorInfo.C4 := RGB(192, 192, 192);
    FloorInfo.flags := flg_DoubleSided;
    FloorInfo.u := 0.0;
    FloorInfo.v := 0.0;
    FloorInfo.Transparent := False;
    FloorInfo.TextureName := '';

    dorecalcculls := False;

    MakeEmptyBoundingCube(bCube);
    bCubeSize := g_HUGE;
    TotalSceneTriangles := 0;
    FullScreen1.Enabled := False;
    FullScreen2.Enabled := False;

    DXDraw.Align := alClient;

    Scene := TQuakeViewerScene.Create(DXDraw{$IFNDEF NO_D3DSOUNDS}, DXSound{$ENDIF});
    // MaxPolygonVertexes indicates how many vertexes a merged polygon will have.
    // We accept values are from 50 to 1000.
    // Smaller values increase rendering performance but loads slow.
    // Higher values decrease rendering performance but loads fast.
    if not VarIsEmpty(regMaxPolygonVertexes.Value) then
    begin
      maxpverts := regMaxPolygonVertexes.Value;
      if (maxpverts < MINTRVERTEXES) or (maxpverts > MAXTRVERTEXES) then
        maxpverts := 100;
      Scene.MaxPolygonVertexes := maxpverts;
    end
    else
      Scene.MaxPolygonVertexes := DEFTRVERTEXES;
    regMaxPolygonVertexes.Value := Scene.MaxPolygonVertexes;
    Scene.NearClippingPlane := 0.25; // Change Near Clipping Plane

    DXDraw.Visible := True;
    DXTimer.Enabled := True;
    WalkTime := 0.0;
    oldTime := GetTickCount / 1000;
    DXInput.Keyboard.Enabled := True;
    DXInput.Keyboard.BindInputStates := True;
    DXInput.Mouse.Enabled := False;
    DXInput.Mouse.BindInputStates := False;
    DXInput.Joystick.Enabled := True;
    DXInput.Joystick.BindInputStates := True;
    DXInput.Keyboard.KeyAssigns := KeyAssignProc;

    if not VarIsEmpty(regUseHardwareAcceleration.Value) then
      SetHardware(regUseHardwareAcceleration.Value, True);

    if not VarIsEmpty(regSafeMode.Value) then
      SetSafeMode(regSafeMode.Value, True)
    else
      SetSafeMode(False, True);

    if not VarIsEmpty(regTextureFiltering.Value) then
      doFiltering := regTextureFiltering.Value
    else
      doFiltering := False;
    Texturefiltering1.Checked := doFiltering;

    if not VarIsEmpty(regShowFog.Value) then
      Fog1.Checked := regShowFog.Value
    else
      Fog1.Checked := False;

    if not VarIsEmpty(regShowInfo.Value) then
      Information1.Checked := regShowInfo.Value
    else
      Information1.Checked := False;

    i := QV_CheckParam('-file');
    if i > 0 then
    begin
      MainPAK := QV_GetParam(i + 1);
      BspFile := MainPAK;
    end
    else
    begin
      MainPAK := '';
      BspFile := '';
    end;

    i := QV_CheckParam('-map');
    if i > 0 then
    begin
      MapName := QV_GetParam(i + 1);
    end
    else
      MapName := '';

    Factor := -1;
    lFactor := -1;

////////////////////////////////////////////////////////////////////////////////
// Sky support
    GetMem(SPHERE_0004_ID_0008_Vertexes, 320 * SizeOf(TD3DLVertex));
    SPHERE_0004_ID_0008_Vertexes[0] := MakeD3DLVERTEX(0.000000000000, 7000.000000000000, 0.000000000000, D3DCOLOR(16777215), D3DCOLOR(0), 0.000000000000, 0.000000000000);
    SPHERE_0004_ID_0008_Vertexes[1] := MakeD3DLVERTEX(0.000000000000, 6755.282714843750, 3090.169921875000, D3DCOLOR(16777215), D3DCOLOR(0), 0.000000000000, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[2] := MakeD3DLVERTEX(0.000000000000, 7000.000000000000, 0.000000000000, D3DCOLOR(16777215), D3DCOLOR(0), -0.133333340287, 0.000000000000);
    SPHERE_0004_ID_0008_Vertexes[3] := MakeD3DLVERTEX(1256.885375976563, 6755.282714843750, 2823.010742187500, D3DCOLOR(16777215), D3DCOLOR(0), -0.133333340287, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[4] := MakeD3DLVERTEX(0.000000000000, 7000.000000000000, 0.000000000000, D3DCOLOR(16777215), D3DCOLOR(0), -0.266666680574, 0.000000000000);
    SPHERE_0004_ID_0008_Vertexes[5] := MakeD3DLVERTEX(2296.444091796875, 6755.282714843750, 2067.727050781250, D3DCOLOR(16777215), D3DCOLOR(0), -0.266666680574, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[6] := MakeD3DLVERTEX(0.000000000000, 7000.000000000000, 0.000000000000, D3DCOLOR(16777215), D3DCOLOR(0), -0.400000005960, 0.000000000000);
    SPHERE_0004_ID_0008_Vertexes[7] := MakeD3DLVERTEX(2938.926513671875, 6755.282714843750, 954.914978027344, D3DCOLOR(16777215), D3DCOLOR(0), -0.400000005960, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[8] := MakeD3DLVERTEX(0.000000000000, 7000.000000000000, 0.000000000000, D3DCOLOR(16777215), D3DCOLOR(0), -0.533333361149, 0.000000000000);
    SPHERE_0004_ID_0008_Vertexes[9] := MakeD3DLVERTEX(3073.241699218750, 6755.282714843750, -323.010864257813, D3DCOLOR(16777215), D3DCOLOR(0), -0.533333361149, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[10] := MakeD3DLVERTEX(0.000000000000, 7000.000000000000, 0.000000000000, D3DCOLOR(16777215), D3DCOLOR(0), -0.666666686535, 0.000000000000);
    SPHERE_0004_ID_0008_Vertexes[11] := MakeD3DLVERTEX(2676.165771484375, 6755.282714843750, -1545.085205078125, D3DCOLOR(16777215), D3DCOLOR(0), -0.666666686535, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[12] := MakeD3DLVERTEX(0.000000000000, 7000.000000000000, 0.000000000000, D3DCOLOR(16777215), D3DCOLOR(0), -0.800000011921, 0.000000000000);
    SPHERE_0004_ID_0008_Vertexes[13] := MakeD3DLVERTEX(1816.356201171875, 6755.282714843750, -2500.000244140625, D3DCOLOR(16777215), D3DCOLOR(0), -0.800000011921, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[14] := MakeD3DLVERTEX(0.000000000000, 7000.000000000000, 0.000000000000, D3DCOLOR(16777215), D3DCOLOR(0), -0.933333337307, 0.000000000000);
    SPHERE_0004_ID_0008_Vertexes[15] := MakeD3DLVERTEX(642.482177734375, 6755.282714843750, -3022.642333984375, D3DCOLOR(16777215), D3DCOLOR(0), -0.933333337307, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[16] := MakeD3DLVERTEX(0.000000000000, 7000.000000000000, 0.000000000000, D3DCOLOR(16777215), D3DCOLOR(0), -1.066666722298, 0.000000000000);
    SPHERE_0004_ID_0008_Vertexes[17] := MakeD3DLVERTEX(-642.482788085938, 6755.282714843750, -3022.642333984375, D3DCOLOR(16777215), D3DCOLOR(0), -1.066666722298, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[18] := MakeD3DLVERTEX(0.000000000000, 7000.000000000000, 0.000000000000, D3DCOLOR(16777215), D3DCOLOR(0), -1.200000047684, 0.000000000000);
    SPHERE_0004_ID_0008_Vertexes[19] := MakeD3DLVERTEX(-1816.356811523438, 6755.282714843750, -2499.999755859375, D3DCOLOR(16777215), D3DCOLOR(0), -1.200000047684, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[20] := MakeD3DLVERTEX(0.000000000000, 7000.000000000000, 0.000000000000, D3DCOLOR(16777215), D3DCOLOR(0), -1.333333373070, 0.000000000000);
    SPHERE_0004_ID_0008_Vertexes[21] := MakeD3DLVERTEX(-2676.166015625000, 6755.282714843750, -1545.084716796875, D3DCOLOR(16777215), D3DCOLOR(0), -1.333333373070, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[22] := MakeD3DLVERTEX(0.000000000000, 7000.000000000000, 0.000000000000, D3DCOLOR(16777215), D3DCOLOR(0), -1.466666698456, 0.000000000000);
    SPHERE_0004_ID_0008_Vertexes[23] := MakeD3DLVERTEX(-3073.241699218750, 6755.282714843750, -323.010345458984, D3DCOLOR(16777215), D3DCOLOR(0), -1.466666698456, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[24] := MakeD3DLVERTEX(0.000000000000, 7000.000000000000, 0.000000000000, D3DCOLOR(16777215), D3DCOLOR(0), -1.600000023842, 0.000000000000);
    SPHERE_0004_ID_0008_Vertexes[25] := MakeD3DLVERTEX(-2938.926269531250, 6755.282714843750, 954.915405273438, D3DCOLOR(16777215), D3DCOLOR(0), -1.600000023842, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[26] := MakeD3DLVERTEX(0.000000000000, 7000.000000000000, 0.000000000000, D3DCOLOR(16777215), D3DCOLOR(0), -1.733333349228, 0.000000000000);
    SPHERE_0004_ID_0008_Vertexes[27] := MakeD3DLVERTEX(-2296.443603515625, 6755.282714843750, 2067.727783203125, D3DCOLOR(16777215), D3DCOLOR(0), -1.733333349228, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[28] := MakeD3DLVERTEX(0.000000000000, 7000.000000000000, 0.000000000000, D3DCOLOR(16777215), D3DCOLOR(0), -1.866666674614, 0.000000000000);
    SPHERE_0004_ID_0008_Vertexes[29] := MakeD3DLVERTEX(-1256.884887695313, 6755.282714843750, 2823.010986328125, D3DCOLOR(16777215), D3DCOLOR(0), -1.866666674614, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[30] := MakeD3DLVERTEX(0.000000000000, 7000.000000000000, 0.000000000000, D3DCOLOR(16777215), D3DCOLOR(0), -2.000000000000, 0.000000000000);
    SPHERE_0004_ID_0008_Vertexes[31] := MakeD3DLVERTEX(0.000540302484, 6755.282714843750, 3090.169921875000, D3DCOLOR(16777215), D3DCOLOR(0), -2.000000000000, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[32] := MakeD3DLVERTEX(0.000000000000, 6755.282714843750, 3090.169921875000, D3DCOLOR(16777215), D3DCOLOR(0), 0.000000000000, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[33] := MakeD3DLVERTEX(0.000000000000, 6045.084960937500, 5877.852539062500, D3DCOLOR(16777215), D3DCOLOR(0), 0.000000000000, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[34] := MakeD3DLVERTEX(1256.885375976563, 6755.282714843750, 2823.010742187500, D3DCOLOR(16777215), D3DCOLOR(0), -0.133333340287, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[35] := MakeD3DLVERTEX(2390.738037109375, 6045.084960937500, 5369.685058593750, D3DCOLOR(16777215), D3DCOLOR(0), -0.133333340287, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[36] := MakeD3DLVERTEX(2296.444091796875, 6755.282714843750, 2067.727050781250, D3DCOLOR(16777215), D3DCOLOR(0), -0.266666680574, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[37] := MakeD3DLVERTEX(4368.096191406250, 6045.084960937500, 3933.050537109375, D3DCOLOR(16777215), D3DCOLOR(0), -0.266666680574, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[38] := MakeD3DLVERTEX(2938.926513671875, 6755.282714843750, 954.914978027344, D3DCOLOR(16777215), D3DCOLOR(0), -0.400000005960, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[39] := MakeD3DLVERTEX(5590.169921875000, 6045.084960937500, 1816.356201171875, D3DCOLOR(16777215), D3DCOLOR(0), -0.400000005960, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[40] := MakeD3DLVERTEX(3073.241699218750, 6755.282714843750, -323.010864257813, D3DCOLOR(16777215), D3DCOLOR(0), -0.533333361149, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[41] := MakeD3DLVERTEX(5845.652832031250, 6045.084960937500, -614.403137207031, D3DCOLOR(16777215), D3DCOLOR(0), -0.533333361149, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[42] := MakeD3DLVERTEX(2676.165771484375, 6755.282714843750, -1545.085205078125, D3DCOLOR(16777215), D3DCOLOR(0), -0.666666686535, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[43] := MakeD3DLVERTEX(5090.369628906250, 6045.084960937500, -2938.926513671875, D3DCOLOR(16777215), D3DCOLOR(0), -0.666666686535, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[44] := MakeD3DLVERTEX(1816.356201171875, 6755.282714843750, -2500.000244140625, D3DCOLOR(16777215), D3DCOLOR(0), -0.800000011921, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[45] := MakeD3DLVERTEX(3454.914794921875, 6045.084960937500, -4755.283203125000, D3DCOLOR(16777215), D3DCOLOR(0), -0.800000011921, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[46] := MakeD3DLVERTEX(642.482177734375, 6755.282714843750, -3022.642333984375, D3DCOLOR(16777215), D3DCOLOR(0), -0.933333337307, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[47] := MakeD3DLVERTEX(1222.073730468750, 6045.084960937500, -5749.407226562500, D3DCOLOR(16777215), D3DCOLOR(0), -0.933333337307, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[48] := MakeD3DLVERTEX(-642.482788085938, 6755.282714843750, -3022.642333984375, D3DCOLOR(16777215), D3DCOLOR(0), -1.066666722298, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[49] := MakeD3DLVERTEX(-1222.074829101563, 6045.084960937500, -5749.406738281250, D3DCOLOR(16777215), D3DCOLOR(0), -1.066666722298, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[50] := MakeD3DLVERTEX(-1816.356811523438, 6755.282714843750, -2499.999755859375, D3DCOLOR(16777215), D3DCOLOR(0), -1.200000047684, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[51] := MakeD3DLVERTEX(-3454.915527343750, 6045.084960937500, -4755.282226562500, D3DCOLOR(16777215), D3DCOLOR(0), -1.200000047684, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[52] := MakeD3DLVERTEX(-2676.166015625000, 6755.282714843750, -1545.084716796875, D3DCOLOR(16777215), D3DCOLOR(0), -1.333333373070, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[53] := MakeD3DLVERTEX(-5090.369628906250, 6045.084960937500, -2938.925537109375, D3DCOLOR(16777215), D3DCOLOR(0), -1.333333373070, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[54] := MakeD3DLVERTEX(-3073.241699218750, 6755.282714843750, -323.010345458984, D3DCOLOR(16777215), D3DCOLOR(0), -1.466666698456, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[55] := MakeD3DLVERTEX(-5845.652832031250, 6045.084960937500, -614.402160644531, D3DCOLOR(16777215), D3DCOLOR(0), -1.466666698456, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[56] := MakeD3DLVERTEX(-2938.926269531250, 6755.282714843750, 954.915405273438, D3DCOLOR(16777215), D3DCOLOR(0), -1.600000023842, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[57] := MakeD3DLVERTEX(-5590.169433593750, 6045.084960937500, 1816.357055664063, D3DCOLOR(16777215), D3DCOLOR(0), -1.600000023842, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[58] := MakeD3DLVERTEX(-2296.443603515625, 6755.282714843750, 2067.727783203125, D3DCOLOR(16777215), D3DCOLOR(0), -1.733333349228, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[59] := MakeD3DLVERTEX(-4368.095214843750, 6045.084960937500, 3933.051757812500, D3DCOLOR(16777215), D3DCOLOR(0), -1.733333349228, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[60] := MakeD3DLVERTEX(-1256.884887695313, 6755.282714843750, 2823.010986328125, D3DCOLOR(16777215), D3DCOLOR(0), -1.866666674614, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[61] := MakeD3DLVERTEX(-2390.737060546875, 6045.084960937500, 5369.686035156250, D3DCOLOR(16777215), D3DCOLOR(0), -1.866666674614, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[62] := MakeD3DLVERTEX(0.000540302484, 6755.282714843750, 3090.169921875000, D3DCOLOR(16777215), D3DCOLOR(0), -2.000000000000, 0.100000001490);
    SPHERE_0004_ID_0008_Vertexes[63] := MakeD3DLVERTEX(0.001027716324, 6045.084960937500, 5877.852539062500, D3DCOLOR(16777215), D3DCOLOR(0), -2.000000000000, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[64] := MakeD3DLVERTEX(0.000000000000, 6045.084960937500, 5877.852539062500, D3DCOLOR(16777215), D3DCOLOR(0), 0.000000000000, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[65] := MakeD3DLVERTEX(0.000000000000, 4938.926269531250, 8090.169921875000, D3DCOLOR(16777215), D3DCOLOR(0), 0.000000000000, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[66] := MakeD3DLVERTEX(2390.738037109375, 6045.084960937500, 5369.685058593750, D3DCOLOR(16777215), D3DCOLOR(0), -0.133333340287, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[67] := MakeD3DLVERTEX(3290.568603515625, 4938.926269531250, 7390.738281250000, D3DCOLOR(16777215), D3DCOLOR(0), -0.133333340287, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[68] := MakeD3DLVERTEX(4368.096191406250, 6045.084960937500, 3933.050537109375, D3DCOLOR(16777215), D3DCOLOR(0), -0.266666680574, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[69] := MakeD3DLVERTEX(6012.168457031250, 4938.926269531250, 5413.380371093750, D3DCOLOR(16777215), D3DCOLOR(0), -0.266666680574, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[70] := MakeD3DLVERTEX(5590.169921875000, 6045.084960937500, 1816.356201171875, D3DCOLOR(16777215), D3DCOLOR(0), -0.400000005960, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[71] := MakeD3DLVERTEX(7694.208984375000, 4938.926269531250, 2499.999755859375, D3DCOLOR(16777215), D3DCOLOR(0), -0.400000005960, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[72] := MakeD3DLVERTEX(5845.652832031250, 6045.084960937500, -614.403137207031, D3DCOLOR(16777215), D3DCOLOR(0), -0.533333361149, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[73] := MakeD3DLVERTEX(8045.851562500000, 4938.926269531250, -845.653442382813, D3DCOLOR(16777215), D3DCOLOR(0), -0.533333361149, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[74] := MakeD3DLVERTEX(5090.369628906250, 6045.084960937500, -2938.926513671875, D3DCOLOR(16777215), D3DCOLOR(0), -0.666666686535, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[75] := MakeD3DLVERTEX(7006.292480468750, 4938.926269531250, -4045.085693359375, D3DCOLOR(16777215), D3DCOLOR(0), -0.666666686535, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[76] := MakeD3DLVERTEX(3454.914794921875, 6045.084960937500, -4755.283203125000, D3DCOLOR(16777215), D3DCOLOR(0), -0.800000011921, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[77] := MakeD3DLVERTEX(4755.282226562500, 4938.926269531250, -6545.085449218750, D3DCOLOR(16777215), D3DCOLOR(0), -0.800000011921, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[78] := MakeD3DLVERTEX(1222.073730468750, 6045.084960937500, -5749.407226562500, D3DCOLOR(16777215), D3DCOLOR(0), -0.933333337307, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[79] := MakeD3DLVERTEX(1682.040283203125, 4938.926269531250, -7913.380859375000, D3DCOLOR(16777215), D3DCOLOR(0), -0.933333337307, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[80] := MakeD3DLVERTEX(-1222.074829101563, 6045.084960937500, -5749.406738281250, D3DCOLOR(16777215), D3DCOLOR(0), -1.066666722298, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[81] := MakeD3DLVERTEX(-1682.041748046875, 4938.926269531250, -7913.380371093750, D3DCOLOR(16777215), D3DCOLOR(0), -1.066666722298, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[82] := MakeD3DLVERTEX(-3454.915527343750, 6045.084960937500, -4755.282226562500, D3DCOLOR(16777215), D3DCOLOR(0), -1.200000047684, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[83] := MakeD3DLVERTEX(-4755.283691406250, 4938.926269531250, -6545.084472656250, D3DCOLOR(16777215), D3DCOLOR(0), -1.200000047684, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[84] := MakeD3DLVERTEX(-5090.369628906250, 6045.084960937500, -2938.925537109375, D3DCOLOR(16777215), D3DCOLOR(0), -1.333333373070, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[85] := MakeD3DLVERTEX(-7006.292968750000, 4938.926269531250, -4045.084472656250, D3DCOLOR(16777215), D3DCOLOR(0), -1.333333373070, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[86] := MakeD3DLVERTEX(-5845.652832031250, 6045.084960937500, -614.402160644531, D3DCOLOR(16777215), D3DCOLOR(0), -1.466666698456, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[87] := MakeD3DLVERTEX(-8045.851562500000, 4938.926269531250, -845.651977539063, D3DCOLOR(16777215), D3DCOLOR(0), -1.466666698456, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[88] := MakeD3DLVERTEX(-5590.169433593750, 6045.084960937500, 1816.357055664063, D3DCOLOR(16777215), D3DCOLOR(0), -1.600000023842, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[89] := MakeD3DLVERTEX(-7694.208496093750, 4938.926269531250, 2500.001220703125, D3DCOLOR(16777215), D3DCOLOR(0), -1.600000023842, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[90] := MakeD3DLVERTEX(-4368.095214843750, 6045.084960937500, 3933.051757812500, D3DCOLOR(16777215), D3DCOLOR(0), -1.733333349228, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[91] := MakeD3DLVERTEX(-6012.167480468750, 4938.926269531250, 5413.381347656250, D3DCOLOR(16777215), D3DCOLOR(0), -1.733333349228, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[92] := MakeD3DLVERTEX(-2390.737060546875, 6045.084960937500, 5369.686035156250, D3DCOLOR(16777215), D3DCOLOR(0), -1.866666674614, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[93] := MakeD3DLVERTEX(-3290.567382812500, 4938.926269531250, 7390.738769531250, D3DCOLOR(16777215), D3DCOLOR(0), -1.866666674614, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[94] := MakeD3DLVERTEX(0.001027716324, 6045.084960937500, 5877.852539062500, D3DCOLOR(16777215), D3DCOLOR(0), -2.000000000000, 0.200000002980);
    SPHERE_0004_ID_0008_Vertexes[95] := MakeD3DLVERTEX(0.001414530328, 4938.926269531250, 8090.169921875000, D3DCOLOR(16777215), D3DCOLOR(0), -2.000000000000, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[96] := MakeD3DLVERTEX(0.000000000000, 4938.926269531250, 8090.169921875000, D3DCOLOR(16777215), D3DCOLOR(0), 0.000000000000, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[97] := MakeD3DLVERTEX(0.000000000000, 3545.084960937500, 9510.565429687500, D3DCOLOR(16777215), D3DCOLOR(0), 0.000000000000, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[98] := MakeD3DLVERTEX(3290.568603515625, 4938.926269531250, 7390.738281250000, D3DCOLOR(16777215), D3DCOLOR(0), -0.133333340287, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[99] := MakeD3DLVERTEX(3868.295654296875, 3545.084960937500, 8688.333984375000, D3DCOLOR(16777215), D3DCOLOR(0), -0.133333340287, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[100] := MakeD3DLVERTEX(6012.168457031250, 4938.926269531250, 5413.380371093750, D3DCOLOR(16777215), D3DCOLOR(0), -0.266666680574, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[101] := MakeD3DLVERTEX(7067.728027343750, 3545.084960937500, 6363.809570312500, D3DCOLOR(16777215), D3DCOLOR(0), -0.266666680574, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[102] := MakeD3DLVERTEX(7694.208984375000, 4938.926269531250, 2499.999755859375, D3DCOLOR(16777215), D3DCOLOR(0), -0.400000005960, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[103] := MakeD3DLVERTEX(9045.084960937500, 3545.084960937500, 2938.926269531250, D3DCOLOR(16777215), D3DCOLOR(0), -0.400000005960, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[104] := MakeD3DLVERTEX(8045.851562500000, 4938.926269531250, -845.653442382813, D3DCOLOR(16777215), D3DCOLOR(0), -0.533333361149, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[105] := MakeD3DLVERTEX(9458.465820312500, 3545.084960937500, -994.125244140625, D3DCOLOR(16777215), D3DCOLOR(0), -0.533333361149, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[106] := MakeD3DLVERTEX(7006.292480468750, 4938.926269531250, -4045.085693359375, D3DCOLOR(16777215), D3DCOLOR(0), -0.666666686535, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[107] := MakeD3DLVERTEX(8236.390625000000, 3545.084960937500, -4755.283203125000, D3DCOLOR(16777215), D3DCOLOR(0), -0.666666686535, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[108] := MakeD3DLVERTEX(4755.282226562500, 4938.926269531250, -6545.085449218750, D3DCOLOR(16777215), D3DCOLOR(0), -0.800000011921, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[109] := MakeD3DLVERTEX(5590.169433593750, 3545.084960937500, -7694.209960937500, D3DCOLOR(16777215), D3DCOLOR(0), -0.800000011921, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[110] := MakeD3DLVERTEX(1682.040283203125, 4938.926269531250, -7913.380859375000, D3DCOLOR(16777215), D3DCOLOR(0), -0.933333337307, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[111] := MakeD3DLVERTEX(1977.356933593750, 3545.084960937500, -9302.737304687500, D3DCOLOR(16777215), D3DCOLOR(0), -0.933333337307, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[112] := MakeD3DLVERTEX(-1682.041748046875, 4938.926269531250, -7913.380371093750, D3DCOLOR(16777215), D3DCOLOR(0), -1.066666722298, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[113] := MakeD3DLVERTEX(-1977.358642578125, 3545.084960937500, -9302.736328125000, D3DCOLOR(16777215), D3DCOLOR(0), -1.066666722298, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[114] := MakeD3DLVERTEX(-4755.283691406250, 4938.926269531250, -6545.084472656250, D3DCOLOR(16777215), D3DCOLOR(0), -1.200000047684, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[115] := MakeD3DLVERTEX(-5590.171386718750, 3545.084960937500, -7694.208496093750, D3DCOLOR(16777215), D3DCOLOR(0), -1.200000047684, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[116] := MakeD3DLVERTEX(-7006.292968750000, 4938.926269531250, -4045.084472656250, D3DCOLOR(16777215), D3DCOLOR(0), -1.333333373070, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[117] := MakeD3DLVERTEX(-8236.391601562500, 3545.084960937500, -4755.281738281250, D3DCOLOR(16777215), D3DCOLOR(0), -1.333333373070, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[118] := MakeD3DLVERTEX(-8045.851562500000, 4938.926269531250, -845.651977539063, D3DCOLOR(16777215), D3DCOLOR(0), -1.466666698456, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[119] := MakeD3DLVERTEX(-9458.465820312500, 3545.084960937500, -994.123596191406, D3DCOLOR(16777215), D3DCOLOR(0), -1.466666698456, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[120] := MakeD3DLVERTEX(-7694.208496093750, 4938.926269531250, 2500.001220703125, D3DCOLOR(16777215), D3DCOLOR(0), -1.600000023842, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[121] := MakeD3DLVERTEX(-9045.084960937500, 3545.084960937500, 2938.927490234375, D3DCOLOR(16777215), D3DCOLOR(0), -1.600000023842, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[122] := MakeD3DLVERTEX(-6012.167480468750, 4938.926269531250, 5413.381347656250, D3DCOLOR(16777215), D3DCOLOR(0), -1.733333349228, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[123] := MakeD3DLVERTEX(-7067.727050781250, 3545.084960937500, 6363.811523437500, D3DCOLOR(16777215), D3DCOLOR(0), -1.733333349228, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[124] := MakeD3DLVERTEX(-3290.567382812500, 4938.926269531250, 7390.738769531250, D3DCOLOR(16777215), D3DCOLOR(0), -1.866666674614, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[125] := MakeD3DLVERTEX(-3868.293945312500, 3545.084960937500, 8688.334960937500, D3DCOLOR(16777215), D3DCOLOR(0), -1.866666674614, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[126] := MakeD3DLVERTEX(0.001414530328, 4938.926269531250, 8090.169921875000, D3DCOLOR(16777215), D3DCOLOR(0), -2.000000000000, 0.300000011921);
    SPHERE_0004_ID_0008_Vertexes[127] := MakeD3DLVERTEX(0.001662880066, 3545.084960937500, 9510.565429687500, D3DCOLOR(16777215), D3DCOLOR(0), -2.000000000000, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[128] := MakeD3DLVERTEX(0.000000000000, 3545.084960937500, 9510.565429687500, D3DCOLOR(16777215), D3DCOLOR(0), 0.000000000000, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[129] := MakeD3DLVERTEX(0.000000000000, 1999.999755859375, 10000.000000000000, D3DCOLOR(16777215), D3DCOLOR(0), 0.000000000000, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[130] := MakeD3DLVERTEX(3868.295654296875, 3545.084960937500, 8688.333984375000, D3DCOLOR(16777215), D3DCOLOR(0), -0.133333340287, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[131] := MakeD3DLVERTEX(4067.366455078125, 1999.999755859375, 9135.454101562500, D3DCOLOR(16777215), D3DCOLOR(0), -0.133333340287, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[132] := MakeD3DLVERTEX(7067.728027343750, 3545.084960937500, 6363.809570312500, D3DCOLOR(16777215), D3DCOLOR(0), -0.266666680574, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[133] := MakeD3DLVERTEX(7431.448730468750, 1999.999755859375, 6691.305664062500, D3DCOLOR(16777215), D3DCOLOR(0), -0.266666680574, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[134] := MakeD3DLVERTEX(9045.084960937500, 3545.084960937500, 2938.926269531250, D3DCOLOR(16777215), D3DCOLOR(0), -0.400000005960, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[135] := MakeD3DLVERTEX(9510.565429687500, 1999.999755859375, 3090.169677734375, D3DCOLOR(16777215), D3DCOLOR(0), -0.400000005960, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[136] := MakeD3DLVERTEX(9458.465820312500, 3545.084960937500, -994.125244140625, D3DCOLOR(16777215), D3DCOLOR(0), -0.533333361149, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[137] := MakeD3DLVERTEX(9945.218750000000, 1999.999755859375, -1045.285034179688, D3DCOLOR(16777215), D3DCOLOR(0), -0.533333361149, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[138] := MakeD3DLVERTEX(8236.390625000000, 3545.084960937500, -4755.283203125000, D3DCOLOR(16777215), D3DCOLOR(0), -0.666666686535, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[139] := MakeD3DLVERTEX(8660.253906250000, 1999.999755859375, -5000.000488281250, D3DCOLOR(16777215), D3DCOLOR(0), -0.666666686535, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[140] := MakeD3DLVERTEX(5590.169433593750, 3545.084960937500, -7694.209960937500, D3DCOLOR(16777215), D3DCOLOR(0), -0.800000011921, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[141] := MakeD3DLVERTEX(5877.852050781250, 1999.999755859375, -8090.170410156250, D3DCOLOR(16777215), D3DCOLOR(0), -0.800000011921, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[142] := MakeD3DLVERTEX(1977.356933593750, 3545.084960937500, -9302.737304687500, D3DCOLOR(16777215), D3DCOLOR(0), -0.933333337307, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[143] := MakeD3DLVERTEX(2079.116210937500, 1999.999755859375, -9781.476562500000, D3DCOLOR(16777215), D3DCOLOR(0), -0.933333337307, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[144] := MakeD3DLVERTEX(-1977.358642578125, 3545.084960937500, -9302.736328125000, D3DCOLOR(16777215), D3DCOLOR(0), -1.066666722298, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[145] := MakeD3DLVERTEX(-2079.117919921875, 1999.999755859375, -9781.475585937500, D3DCOLOR(16777215), D3DCOLOR(0), -1.066666722298, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[146] := MakeD3DLVERTEX(-5590.171386718750, 3545.084960937500, -7694.208496093750, D3DCOLOR(16777215), D3DCOLOR(0), -1.200000047684, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[147] := MakeD3DLVERTEX(-5877.853515625000, 1999.999755859375, -8090.169433593750, D3DCOLOR(16777215), D3DCOLOR(0), -1.200000047684, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[148] := MakeD3DLVERTEX(-8236.391601562500, 3545.084960937500, -4755.281738281250, D3DCOLOR(16777215), D3DCOLOR(0), -1.333333373070, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[149] := MakeD3DLVERTEX(-8660.254882812500, 1999.999755859375, -4999.999023437500, D3DCOLOR(16777215), D3DCOLOR(0), -1.333333373070, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[150] := MakeD3DLVERTEX(-9458.465820312500, 3545.084960937500, -994.123596191406, D3DCOLOR(16777215), D3DCOLOR(0), -1.466666698456, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[151] := MakeD3DLVERTEX(-9945.218750000000, 1999.999755859375, -1045.283325195313, D3DCOLOR(16777215), D3DCOLOR(0), -1.466666698456, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[152] := MakeD3DLVERTEX(-9045.084960937500, 3545.084960937500, 2938.927490234375, D3DCOLOR(16777215), D3DCOLOR(0), -1.600000023842, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[153] := MakeD3DLVERTEX(-9510.564453125000, 1999.999755859375, 3090.171142578125, D3DCOLOR(16777215), D3DCOLOR(0), -1.600000023842, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[154] := MakeD3DLVERTEX(-7067.727050781250, 3545.084960937500, 6363.811523437500, D3DCOLOR(16777215), D3DCOLOR(0), -1.733333349228, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[155] := MakeD3DLVERTEX(-7431.447265625000, 1999.999755859375, 6691.307617187500, D3DCOLOR(16777215), D3DCOLOR(0), -1.733333349228, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[156] := MakeD3DLVERTEX(-3868.293945312500, 3545.084960937500, 8688.334960937500, D3DCOLOR(16777215), D3DCOLOR(0), -1.866666674614, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[157] := MakeD3DLVERTEX(-4067.364990234375, 1999.999755859375, 9135.455078125000, D3DCOLOR(16777215), D3DCOLOR(0), -1.866666674614, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[158] := MakeD3DLVERTEX(0.001662880066, 3545.084960937500, 9510.565429687500, D3DCOLOR(16777215), D3DCOLOR(0), -2.000000000000, 0.400000005960);
    SPHERE_0004_ID_0008_Vertexes[159] := MakeD3DLVERTEX(0.001748455572, 1999.999755859375, 10000.000000000000, D3DCOLOR(16777215), D3DCOLOR(0), -2.000000000000, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[160] := MakeD3DLVERTEX(0.000000000000, 1999.999755859375, 10000.000000000000, D3DCOLOR(16777215), D3DCOLOR(0), 0.000000000000, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[161] := MakeD3DLVERTEX(0.000000000000, 454.914794921875, 9510.564453125000, D3DCOLOR(16777215), D3DCOLOR(0), 0.000000000000, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[162] := MakeD3DLVERTEX(4067.366455078125, 1999.999755859375, 9135.454101562500, D3DCOLOR(16777215), D3DCOLOR(0), -0.133333340287, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[163] := MakeD3DLVERTEX(3868.295166015625, 454.914794921875, 8688.333007812500, D3DCOLOR(16777215), D3DCOLOR(0), -0.133333340287, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[164] := MakeD3DLVERTEX(7431.448730468750, 1999.999755859375, 6691.305664062500, D3DCOLOR(16777215), D3DCOLOR(0), -0.266666680574, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[165] := MakeD3DLVERTEX(7067.727539062500, 454.914794921875, 6363.809570312500, D3DCOLOR(16777215), D3DCOLOR(0), -0.266666680574, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[166] := MakeD3DLVERTEX(9510.565429687500, 1999.999755859375, 3090.169677734375, D3DCOLOR(16777215), D3DCOLOR(0), -0.400000005960, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[167] := MakeD3DLVERTEX(9045.084960937500, 454.914794921875, 2938.926025390625, D3DCOLOR(16777215), D3DCOLOR(0), -0.400000005960, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[168] := MakeD3DLVERTEX(9945.218750000000, 1999.999755859375, -1045.285034179688, D3DCOLOR(16777215), D3DCOLOR(0), -0.533333361149, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[169] := MakeD3DLVERTEX(9458.464843750000, 454.914794921875, -994.125183105469, D3DCOLOR(16777215), D3DCOLOR(0), -0.533333361149, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[170] := MakeD3DLVERTEX(8660.253906250000, 1999.999755859375, -5000.000488281250, D3DCOLOR(16777215), D3DCOLOR(0), -0.666666686535, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[171] := MakeD3DLVERTEX(8236.390625000000, 454.914794921875, -4755.283203125000, D3DCOLOR(16777215), D3DCOLOR(0), -0.666666686535, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[172] := MakeD3DLVERTEX(5877.852050781250, 1999.999755859375, -8090.170410156250, D3DCOLOR(16777215), D3DCOLOR(0), -0.800000011921, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[173] := MakeD3DLVERTEX(5590.168945312500, 454.914794921875, -7694.208984375000, D3DCOLOR(16777215), D3DCOLOR(0), -0.800000011921, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[174] := MakeD3DLVERTEX(2079.116210937500, 1999.999755859375, -9781.476562500000, D3DCOLOR(16777215), D3DCOLOR(0), -0.933333337307, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[175] := MakeD3DLVERTEX(1977.356811523438, 454.914794921875, -9302.736328125000, D3DCOLOR(16777215), D3DCOLOR(0), -0.933333337307, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[176] := MakeD3DLVERTEX(-2079.117919921875, 1999.999755859375, -9781.475585937500, D3DCOLOR(16777215), D3DCOLOR(0), -1.066666722298, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[177] := MakeD3DLVERTEX(-1977.358642578125, 454.914794921875, -9302.736328125000, D3DCOLOR(16777215), D3DCOLOR(0), -1.066666722298, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[178] := MakeD3DLVERTEX(-5877.853515625000, 1999.999755859375, -8090.169433593750, D3DCOLOR(16777215), D3DCOLOR(0), -1.200000047684, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[179] := MakeD3DLVERTEX(-5590.170410156250, 454.914794921875, -7694.208007812500, D3DCOLOR(16777215), D3DCOLOR(0), -1.200000047684, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[180] := MakeD3DLVERTEX(-8660.254882812500, 1999.999755859375, -4999.999023437500, D3DCOLOR(16777215), D3DCOLOR(0), -1.333333373070, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[181] := MakeD3DLVERTEX(-8236.390625000000, 454.914794921875, -4755.281738281250, D3DCOLOR(16777215), D3DCOLOR(0), -1.333333373070, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[182] := MakeD3DLVERTEX(-9945.218750000000, 1999.999755859375, -1045.283325195313, D3DCOLOR(16777215), D3DCOLOR(0), -1.466666698456, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[183] := MakeD3DLVERTEX(-9458.464843750000, 454.914794921875, -994.123535156250, D3DCOLOR(16777215), D3DCOLOR(0), -1.466666698456, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[184] := MakeD3DLVERTEX(-9510.564453125000, 1999.999755859375, 3090.171142578125, D3DCOLOR(16777215), D3DCOLOR(0), -1.600000023842, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[185] := MakeD3DLVERTEX(-9045.083984375000, 454.914794921875, 2938.927490234375, D3DCOLOR(16777215), D3DCOLOR(0), -1.600000023842, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[186] := MakeD3DLVERTEX(-7431.447265625000, 1999.999755859375, 6691.307617187500, D3DCOLOR(16777215), D3DCOLOR(0), -1.733333349228, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[187] := MakeD3DLVERTEX(-7067.726074218750, 454.914794921875, 6363.811523437500, D3DCOLOR(16777215), D3DCOLOR(0), -1.733333349228, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[188] := MakeD3DLVERTEX(-4067.364990234375, 1999.999755859375, 9135.455078125000, D3DCOLOR(16777215), D3DCOLOR(0), -1.866666674614, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[189] := MakeD3DLVERTEX(-3868.293701171875, 454.914794921875, 8688.333984375000, D3DCOLOR(16777215), D3DCOLOR(0), -1.866666674614, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[190] := MakeD3DLVERTEX(0.001748455572, 1999.999755859375, 10000.000000000000, D3DCOLOR(16777215), D3DCOLOR(0), -2.000000000000, 0.500000000000);
    SPHERE_0004_ID_0008_Vertexes[191] := MakeD3DLVERTEX(0.001662879949, 454.914794921875, 9510.564453125000, D3DCOLOR(16777215), D3DCOLOR(0), -2.000000000000, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[192] := MakeD3DLVERTEX(0.000000000000, 454.914794921875, 9510.564453125000, D3DCOLOR(16777215), D3DCOLOR(0), 0.000000000000, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[193] := MakeD3DLVERTEX(0.000000000000, -938.926025390625, 8090.169921875000, D3DCOLOR(16777215), D3DCOLOR(0), 0.000000000000, 0.700000047684);
    SPHERE_0004_ID_0008_Vertexes[194] := MakeD3DLVERTEX(3868.295166015625, 454.914794921875, 8688.333007812500, D3DCOLOR(16777215), D3DCOLOR(0), -0.133333340287, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[195] := MakeD3DLVERTEX(3290.568603515625, -938.926025390625, 7390.738281250000, D3DCOLOR(16777215), D3DCOLOR(0), -0.133333340287, 0.700000047684);
    SPHERE_0004_ID_0008_Vertexes[196] := MakeD3DLVERTEX(7067.727539062500, 454.914794921875, 6363.809570312500, D3DCOLOR(16777215), D3DCOLOR(0), -0.266666680574, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[197] := MakeD3DLVERTEX(6012.168457031250, -938.926025390625, 5413.380371093750, D3DCOLOR(16777215), D3DCOLOR(0), -0.266666680574, 0.700000047684);
    SPHERE_0004_ID_0008_Vertexes[198] := MakeD3DLVERTEX(9045.084960937500, 454.914794921875, 2938.926025390625, D3DCOLOR(16777215), D3DCOLOR(0), -0.400000005960, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[199] := MakeD3DLVERTEX(7694.208984375000, -938.926025390625, 2499.999755859375, D3DCOLOR(16777215), D3DCOLOR(0), -0.400000005960, 0.700000047684);
    SPHERE_0004_ID_0008_Vertexes[200] := MakeD3DLVERTEX(9458.464843750000, 454.914794921875, -994.125183105469, D3DCOLOR(16777215), D3DCOLOR(0), -0.533333361149, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[201] := MakeD3DLVERTEX(8045.851562500000, -938.926025390625, -845.653442382813, D3DCOLOR(16777215), D3DCOLOR(0), -0.533333361149, 0.700000047684);
    SPHERE_0004_ID_0008_Vertexes[202] := MakeD3DLVERTEX(8236.390625000000, 454.914794921875, -4755.283203125000, D3DCOLOR(16777215), D3DCOLOR(0), -0.666666686535, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[203] := MakeD3DLVERTEX(7006.292480468750, -938.926025390625, -4045.085693359375, D3DCOLOR(16777215), D3DCOLOR(0), -0.666666686535, 0.700000047684);
    SPHERE_0004_ID_0008_Vertexes[204] := MakeD3DLVERTEX(5590.168945312500, 454.914794921875, -7694.208984375000, D3DCOLOR(16777215), D3DCOLOR(0), -0.800000011921, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[205] := MakeD3DLVERTEX(4755.282226562500, -938.926025390625, -6545.085449218750, D3DCOLOR(16777215), D3DCOLOR(0), -0.800000011921, 0.700000047684);
    SPHERE_0004_ID_0008_Vertexes[206] := MakeD3DLVERTEX(1977.356811523438, 454.914794921875, -9302.736328125000, D3DCOLOR(16777215), D3DCOLOR(0), -0.933333337307, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[207] := MakeD3DLVERTEX(1682.040283203125, -938.926025390625, -7913.380859375000, D3DCOLOR(16777215), D3DCOLOR(0), -0.933333337307, 0.700000047684);
    SPHERE_0004_ID_0008_Vertexes[208] := MakeD3DLVERTEX(-1977.358642578125, 454.914794921875, -9302.736328125000, D3DCOLOR(16777215), D3DCOLOR(0), -1.066666722298, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[209] := MakeD3DLVERTEX(-1682.041748046875, -938.926025390625, -7913.380371093750, D3DCOLOR(16777215), D3DCOLOR(0), -1.066666722298, 0.700000047684);
    SPHERE_0004_ID_0008_Vertexes[210] := MakeD3DLVERTEX(-5590.170410156250, 454.914794921875, -7694.208007812500, D3DCOLOR(16777215), D3DCOLOR(0), -1.200000047684, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[211] := MakeD3DLVERTEX(-4755.283691406250, -938.926025390625, -6545.084472656250, D3DCOLOR(16777215), D3DCOLOR(0), -1.200000047684, 0.700000047684);
    SPHERE_0004_ID_0008_Vertexes[212] := MakeD3DLVERTEX(-8236.390625000000, 454.914794921875, -4755.281738281250, D3DCOLOR(16777215), D3DCOLOR(0), -1.333333373070, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[213] := MakeD3DLVERTEX(-7006.292968750000, -938.926025390625, -4045.084472656250, D3DCOLOR(16777215), D3DCOLOR(0), -1.333333373070, 0.700000047684);
    SPHERE_0004_ID_0008_Vertexes[214] := MakeD3DLVERTEX(-9458.464843750000, 454.914794921875, -994.123535156250, D3DCOLOR(16777215), D3DCOLOR(0), -1.466666698456, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[215] := MakeD3DLVERTEX(-8045.851562500000, -938.926025390625, -845.651977539063, D3DCOLOR(16777215), D3DCOLOR(0), -1.466666698456, 0.700000047684);
    SPHERE_0004_ID_0008_Vertexes[216] := MakeD3DLVERTEX(-9045.083984375000, 454.914794921875, 2938.927490234375, D3DCOLOR(16777215), D3DCOLOR(0), -1.600000023842, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[217] := MakeD3DLVERTEX(-7694.208496093750, -938.926025390625, 2500.001220703125, D3DCOLOR(16777215), D3DCOLOR(0), -1.600000023842, 0.700000047684);
    SPHERE_0004_ID_0008_Vertexes[218] := MakeD3DLVERTEX(-7067.726074218750, 454.914794921875, 6363.811523437500, D3DCOLOR(16777215), D3DCOLOR(0), -1.733333349228, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[219] := MakeD3DLVERTEX(-6012.167480468750, -938.926025390625, 5413.381347656250, D3DCOLOR(16777215), D3DCOLOR(0), -1.733333349228, 0.700000047684);
    SPHERE_0004_ID_0008_Vertexes[220] := MakeD3DLVERTEX(-3868.293701171875, 454.914794921875, 8688.333984375000, D3DCOLOR(16777215), D3DCOLOR(0), -1.866666674614, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[221] := MakeD3DLVERTEX(-3290.567382812500, -938.926025390625, 7390.738769531250, D3DCOLOR(16777215), D3DCOLOR(0), -1.866666674614, 0.700000047684);
    SPHERE_0004_ID_0008_Vertexes[222] := MakeD3DLVERTEX(0.001662879949, 454.914794921875, 9510.564453125000, D3DCOLOR(16777215), D3DCOLOR(0), -2.000000000000, 0.600000023842);
    SPHERE_0004_ID_0008_Vertexes[223] := MakeD3DLVERTEX(0.001414530328, -938.926025390625, 8090.169921875000, D3DCOLOR(16777215), D3DCOLOR(0), -2.000000000000, 0.700000047684);
    SPHERE_0004_ID_0008_Vertexes[224] := MakeD3DLVERTEX(0.000000000000, -938.926025390625, 8090.169921875000, D3DCOLOR(16777215), D3DCOLOR(0), 0.000000000000, 0.699999988079);
    SPHERE_0004_ID_0008_Vertexes[225] := MakeD3DLVERTEX(0.000000000000, -2045.085205078125, 5877.852050781250, D3DCOLOR(16777215), D3DCOLOR(0), 0.000000000000, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[226] := MakeD3DLVERTEX(3290.568603515625, -938.926025390625, 7390.738281250000, D3DCOLOR(16777215), D3DCOLOR(0), -0.133333340287, 0.699999988079);
    SPHERE_0004_ID_0008_Vertexes[227] := MakeD3DLVERTEX(2390.737792968750, -2045.085205078125, 5369.684570312500, D3DCOLOR(16777215), D3DCOLOR(0), -0.133333340287, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[228] := MakeD3DLVERTEX(6012.168457031250, -938.926025390625, 5413.380371093750, D3DCOLOR(16777215), D3DCOLOR(0), -0.266666680574, 0.699999988079);
    SPHERE_0004_ID_0008_Vertexes[229] := MakeD3DLVERTEX(4368.095214843750, -2045.085205078125, 3933.050292968750, D3DCOLOR(16777215), D3DCOLOR(0), -0.266666680574, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[230] := MakeD3DLVERTEX(7694.208984375000, -938.926025390625, 2499.999755859375, D3DCOLOR(16777215), D3DCOLOR(0), -0.400000005960, 0.699999988079);
    SPHERE_0004_ID_0008_Vertexes[231] := MakeD3DLVERTEX(5590.169433593750, -2045.085205078125, 1816.356079101563, D3DCOLOR(16777215), D3DCOLOR(0), -0.400000005960, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[232] := MakeD3DLVERTEX(8045.851562500000, -938.926025390625, -845.653442382813, D3DCOLOR(16777215), D3DCOLOR(0), -0.533333361149, 0.699999988079);
    SPHERE_0004_ID_0008_Vertexes[233] := MakeD3DLVERTEX(5845.652343750000, -2045.085205078125, -614.403076171875, D3DCOLOR(16777215), D3DCOLOR(0), -0.533333361149, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[234] := MakeD3DLVERTEX(7006.292480468750, -938.926025390625, -4045.085693359375, D3DCOLOR(16777215), D3DCOLOR(0), -0.666666686535, 0.699999988079);
    SPHERE_0004_ID_0008_Vertexes[235] := MakeD3DLVERTEX(5090.369140625000, -2045.085205078125, -2938.926269531250, D3DCOLOR(16777215), D3DCOLOR(0), -0.666666686535, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[236] := MakeD3DLVERTEX(4755.282226562500, -938.926025390625, -6545.085449218750, D3DCOLOR(16777215), D3DCOLOR(0), -0.800000011921, 0.699999988079);
    SPHERE_0004_ID_0008_Vertexes[237] := MakeD3DLVERTEX(3454.914062500000, -2045.085205078125, -4755.282226562500, D3DCOLOR(16777215), D3DCOLOR(0), -0.800000011921, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[238] := MakeD3DLVERTEX(1682.040283203125, -938.926025390625, -7913.380859375000, D3DCOLOR(16777215), D3DCOLOR(0), -0.933333337307, 0.699999988079);
    SPHERE_0004_ID_0008_Vertexes[239] := MakeD3DLVERTEX(1222.073608398438, -2045.085205078125, -5749.406738281250, D3DCOLOR(16777215), D3DCOLOR(0), -0.933333337307, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[240] := MakeD3DLVERTEX(-1682.041748046875, -938.926025390625, -7913.380371093750, D3DCOLOR(16777215), D3DCOLOR(0), -1.066666722298, 0.699999988079);
    SPHERE_0004_ID_0008_Vertexes[241] := MakeD3DLVERTEX(-1222.074707031250, -2045.085205078125, -5749.406250000000, D3DCOLOR(16777215), D3DCOLOR(0), -1.066666722298, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[242] := MakeD3DLVERTEX(-4755.283691406250, -938.926025390625, -6545.084472656250, D3DCOLOR(16777215), D3DCOLOR(0), -1.200000047684, 0.699999988079);
    SPHERE_0004_ID_0008_Vertexes[243] := MakeD3DLVERTEX(-3454.915283203125, -2045.085205078125, -4755.281738281250, D3DCOLOR(16777215), D3DCOLOR(0), -1.200000047684, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[244] := MakeD3DLVERTEX(-7006.292968750000, -938.926025390625, -4045.084472656250, D3DCOLOR(16777215), D3DCOLOR(0), -1.333333373070, 0.699999988079);
    SPHERE_0004_ID_0008_Vertexes[245] := MakeD3DLVERTEX(-5090.369140625000, -2045.085205078125, -2938.925292968750, D3DCOLOR(16777215), D3DCOLOR(0), -1.333333373070, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[246] := MakeD3DLVERTEX(-8045.851562500000, -938.926025390625, -845.651977539063, D3DCOLOR(16777215), D3DCOLOR(0), -1.466666698456, 0.699999988079);
    SPHERE_0004_ID_0008_Vertexes[247] := MakeD3DLVERTEX(-5845.652343750000, -2045.085205078125, -614.402099609375, D3DCOLOR(16777215), D3DCOLOR(0), -1.466666698456, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[248] := MakeD3DLVERTEX(-7694.208496093750, -938.926025390625, 2500.001220703125, D3DCOLOR(16777215), D3DCOLOR(0), -1.600000023842, 0.699999988079);
    SPHERE_0004_ID_0008_Vertexes[249] := MakeD3DLVERTEX(-5590.168945312500, -2045.085205078125, 1816.356933593750, D3DCOLOR(16777215), D3DCOLOR(0), -1.600000023842, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[250] := MakeD3DLVERTEX(-6012.167480468750, -938.926025390625, 5413.381347656250, D3DCOLOR(16777215), D3DCOLOR(0), -1.733333349228, 0.699999988079);
    SPHERE_0004_ID_0008_Vertexes[251] := MakeD3DLVERTEX(-4368.094726562500, -2045.085205078125, 3933.051269531250, D3DCOLOR(16777215), D3DCOLOR(0), -1.733333349228, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[252] := MakeD3DLVERTEX(-3290.567382812500, -938.926025390625, 7390.738769531250, D3DCOLOR(16777215), D3DCOLOR(0), -1.866666674614, 0.699999988079);
    SPHERE_0004_ID_0008_Vertexes[253] := MakeD3DLVERTEX(-2390.736816406250, -2045.085205078125, 5369.685058593750, D3DCOLOR(16777215), D3DCOLOR(0), -1.866666674614, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[254] := MakeD3DLVERTEX(0.001414530328, -938.926025390625, 8090.169921875000, D3DCOLOR(16777215), D3DCOLOR(0), -2.000000000000, 0.699999988079);
    SPHERE_0004_ID_0008_Vertexes[255] := MakeD3DLVERTEX(0.001027716207, -2045.085205078125, 5877.852050781250, D3DCOLOR(16777215), D3DCOLOR(0), -2.000000000000, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[256] := MakeD3DLVERTEX(0.000000000000, -2045.085205078125, 5877.852050781250, D3DCOLOR(16777215), D3DCOLOR(0), 0.000000000000, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[257] := MakeD3DLVERTEX(0.000000000000, -2755.283203125000, 3090.167968750000, D3DCOLOR(16777215), D3DCOLOR(0), 0.000000000000, 0.900000035763);
    SPHERE_0004_ID_0008_Vertexes[258] := MakeD3DLVERTEX(2390.737792968750, -2045.085205078125, 5369.684570312500, D3DCOLOR(16777215), D3DCOLOR(0), -0.133333340287, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[259] := MakeD3DLVERTEX(1256.884521484375, -2755.283203125000, 2823.008789062500, D3DCOLOR(16777215), D3DCOLOR(0), -0.133333340287, 0.900000035763);
    SPHERE_0004_ID_0008_Vertexes[260] := MakeD3DLVERTEX(4368.095214843750, -2045.085205078125, 3933.050292968750, D3DCOLOR(16777215), D3DCOLOR(0), -0.266666680574, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[261] := MakeD3DLVERTEX(2296.442382812500, -2755.283203125000, 2067.725830078125, D3DCOLOR(16777215), D3DCOLOR(0), -0.266666680574, 0.900000035763);
    SPHERE_0004_ID_0008_Vertexes[262] := MakeD3DLVERTEX(5590.169433593750, -2045.085205078125, 1816.356079101563, D3DCOLOR(16777215), D3DCOLOR(0), -0.400000005960, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[263] := MakeD3DLVERTEX(2938.924316406250, -2755.283203125000, 954.914306640625, D3DCOLOR(16777215), D3DCOLOR(0), -0.400000005960, 0.900000035763);
    SPHERE_0004_ID_0008_Vertexes[264] := MakeD3DLVERTEX(5845.652343750000, -2045.085205078125, -614.403076171875, D3DCOLOR(16777215), D3DCOLOR(0), -0.533333361149, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[265] := MakeD3DLVERTEX(3073.239746093750, -2755.283203125000, -323.010650634766, D3DCOLOR(16777215), D3DCOLOR(0), -0.533333361149, 0.900000035763);
    SPHERE_0004_ID_0008_Vertexes[266] := MakeD3DLVERTEX(5090.369140625000, -2045.085205078125, -2938.926269531250, D3DCOLOR(16777215), D3DCOLOR(0), -0.666666686535, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[267] := MakeD3DLVERTEX(2676.163818359375, -2755.283203125000, -1545.084106445313, D3DCOLOR(16777215), D3DCOLOR(0), -0.666666686535, 0.900000035763);
    SPHERE_0004_ID_0008_Vertexes[268] := MakeD3DLVERTEX(3454.914062500000, -2045.085205078125, -4755.282226562500, D3DCOLOR(16777215), D3DCOLOR(0), -0.800000011921, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[269] := MakeD3DLVERTEX(1816.354980468750, -2755.283203125000, -2499.998779296875, D3DCOLOR(16777215), D3DCOLOR(0), -0.800000011921, 0.900000035763);
    SPHERE_0004_ID_0008_Vertexes[270] := MakeD3DLVERTEX(1222.073608398438, -2045.085205078125, -5749.406738281250, D3DCOLOR(16777215), D3DCOLOR(0), -0.933333337307, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[271] := MakeD3DLVERTEX(642.481811523438, -2755.283203125000, -3022.640380859375, D3DCOLOR(16777215), D3DCOLOR(0), -0.933333337307, 0.900000035763);
    SPHERE_0004_ID_0008_Vertexes[272] := MakeD3DLVERTEX(-1222.074707031250, -2045.085205078125, -5749.406250000000, D3DCOLOR(16777215), D3DCOLOR(0), -1.066666722298, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[273] := MakeD3DLVERTEX(-642.482360839844, -2755.283203125000, -3022.640380859375, D3DCOLOR(16777215), D3DCOLOR(0), -1.066666722298, 0.900000035763);
    SPHERE_0004_ID_0008_Vertexes[274] := MakeD3DLVERTEX(-3454.915283203125, -2045.085205078125, -4755.281738281250, D3DCOLOR(16777215), D3DCOLOR(0), -1.200000047684, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[275] := MakeD3DLVERTEX(-1816.355468750000, -2755.283203125000, -2499.998291015625, D3DCOLOR(16777215), D3DCOLOR(0), -1.200000047684, 0.900000035763);
    SPHERE_0004_ID_0008_Vertexes[276] := MakeD3DLVERTEX(-5090.369140625000, -2045.085205078125, -2938.925292968750, D3DCOLOR(16777215), D3DCOLOR(0), -1.333333373070, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[277] := MakeD3DLVERTEX(-2676.164306640625, -2755.283203125000, -1545.083618164063, D3DCOLOR(16777215), D3DCOLOR(0), -1.333333373070, 0.900000035763);
    SPHERE_0004_ID_0008_Vertexes[278] := MakeD3DLVERTEX(-5845.652343750000, -2045.085205078125, -614.402099609375, D3DCOLOR(16777215), D3DCOLOR(0), -1.466666698456, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[279] := MakeD3DLVERTEX(-3073.239746093750, -2755.283203125000, -323.010131835938, D3DCOLOR(16777215), D3DCOLOR(0), -1.466666698456, 0.900000035763);
    SPHERE_0004_ID_0008_Vertexes[280] := MakeD3DLVERTEX(-5590.168945312500, -2045.085205078125, 1816.356933593750, D3DCOLOR(16777215), D3DCOLOR(0), -1.600000023842, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[281] := MakeD3DLVERTEX(-2938.924072265625, -2755.283203125000, 954.914855957031, D3DCOLOR(16777215), D3DCOLOR(0), -1.600000023842, 0.900000035763);
    SPHERE_0004_ID_0008_Vertexes[282] := MakeD3DLVERTEX(-4368.094726562500, -2045.085205078125, 3933.051269531250, D3DCOLOR(16777215), D3DCOLOR(0), -1.733333349228, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[283] := MakeD3DLVERTEX(-2296.442138671875, -2755.283203125000, 2067.726318359375, D3DCOLOR(16777215), D3DCOLOR(0), -1.733333349228, 0.900000035763);
    SPHERE_0004_ID_0008_Vertexes[284] := MakeD3DLVERTEX(-2390.736816406250, -2045.085205078125, 5369.685058593750, D3DCOLOR(16777215), D3DCOLOR(0), -1.866666674614, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[285] := MakeD3DLVERTEX(-1256.884033203125, -2755.283203125000, 2823.009277343750, D3DCOLOR(16777215), D3DCOLOR(0), -1.866666674614, 0.900000035763);
    SPHERE_0004_ID_0008_Vertexes[286] := MakeD3DLVERTEX(0.001027716207, -2045.085205078125, 5877.852050781250, D3DCOLOR(16777215), D3DCOLOR(0), -2.000000000000, 0.800000011921);
    SPHERE_0004_ID_0008_Vertexes[287] := MakeD3DLVERTEX(0.000540302135, -2755.283203125000, 3090.167968750000, D3DCOLOR(16777215), D3DCOLOR(0), -2.000000000000, 0.900000035763);
    SPHERE_0004_ID_0008_Vertexes[288] := MakeD3DLVERTEX(0.000000000000, -2755.283203125000, 3090.167968750000, D3DCOLOR(16777215), D3DCOLOR(0), 0.000000000000, 0.899999976158);
    SPHERE_0004_ID_0008_Vertexes[289] := MakeD3DLVERTEX(0.000000000000, -3000.000000000000, -0.000874227786, D3DCOLOR(16777215), D3DCOLOR(0), 0.000000000000, 1.000000000000);
    SPHERE_0004_ID_0008_Vertexes[290] := MakeD3DLVERTEX(1256.884521484375, -2755.283203125000, 2823.008789062500, D3DCOLOR(16777215), D3DCOLOR(0), -0.133333340287, 0.899999976158);
    SPHERE_0004_ID_0008_Vertexes[291] := MakeD3DLVERTEX(-0.000355580443, -3000.000000000000, -0.000798646768, D3DCOLOR(16777215), D3DCOLOR(0), -0.133333340287, 1.000000000000);
    SPHERE_0004_ID_0008_Vertexes[292] := MakeD3DLVERTEX(2296.442382812500, -2755.283203125000, 2067.725830078125, D3DCOLOR(16777215), D3DCOLOR(0), -0.266666680574, 0.899999976158);
    SPHERE_0004_ID_0008_Vertexes[293] := MakeD3DLVERTEX(-0.000649677881, -3000.000000000000, -0.000584972557, D3DCOLOR(16777215), D3DCOLOR(0), -0.266666680574, 1.000000000000);
    SPHERE_0004_ID_0008_Vertexes[294] := MakeD3DLVERTEX(2938.924316406250, -2755.283203125000, 954.914306640625, D3DCOLOR(16777215), D3DCOLOR(0), -0.400000005960, 0.899999976158);
    SPHERE_0004_ID_0008_Vertexes[295] := MakeD3DLVERTEX(-0.000831440033, -3000.000000000000, -0.000270151213, D3DCOLOR(16777215), D3DCOLOR(0), -0.400000005960, 1.000000000000);
    SPHERE_0004_ID_0008_Vertexes[296] := MakeD3DLVERTEX(3073.239746093750, -2755.283203125000, -323.010650634766, D3DCOLOR(16777215), D3DCOLOR(0), -0.533333361149, 0.899999976158);
    SPHERE_0004_ID_0008_Vertexes[297] := MakeD3DLVERTEX(-0.000869438692, -3000.000000000000, 0.000091381728, D3DCOLOR(16777215), D3DCOLOR(0), -0.533333361149, 1.000000000000);
    SPHERE_0004_ID_0008_Vertexes[298] := MakeD3DLVERTEX(2676.163818359375, -2755.283203125000, -1545.084106445313, D3DCOLOR(16777215), D3DCOLOR(0), -0.666666686535, 0.899999976158);
    SPHERE_0004_ID_0008_Vertexes[299] := MakeD3DLVERTEX(-0.000757103437, -3000.000000000000, 0.000437113922, D3DCOLOR(16777215), D3DCOLOR(0), -0.666666686535, 1.000000000000);
    SPHERE_0004_ID_0008_Vertexes[300] := MakeD3DLVERTEX(1816.354980468750, -2755.283203125000, -2499.998779296875, D3DCOLOR(16777215), D3DCOLOR(0), -0.800000011921, 0.899999976158);
    SPHERE_0004_ID_0008_Vertexes[301] := MakeD3DLVERTEX(-0.000513858104, -3000.000000000000, 0.000707265164, D3DCOLOR(16777215), D3DCOLOR(0), -0.800000011921, 1.000000000000);
    SPHERE_0004_ID_0008_Vertexes[302] := MakeD3DLVERTEX(642.481811523438, -2755.283203125000, -3022.640380859375, D3DCOLOR(16777215), D3DCOLOR(0), -0.933333337307, 0.899999976158);
    SPHERE_0004_ID_0008_Vertexes[303] := MakeD3DLVERTEX(-0.000181762109, -3000.000000000000, 0.000855123857, D3DCOLOR(16777215), D3DCOLOR(0), -0.933333337307, 1.000000000000);
    SPHERE_0004_ID_0008_Vertexes[304] := MakeD3DLVERTEX(-642.482360839844, -2755.283203125000, -3022.640380859375, D3DCOLOR(16777215), D3DCOLOR(0), -1.066666722298, 0.899999976158);
    SPHERE_0004_ID_0008_Vertexes[305] := MakeD3DLVERTEX(0.000181762269, -3000.000000000000, 0.000855123741, D3DCOLOR(16777215), D3DCOLOR(0), -1.066666722298, 1.000000000000);
    SPHERE_0004_ID_0008_Vertexes[306] := MakeD3DLVERTEX(-1816.355468750000, -2755.283203125000, -2499.998291015625, D3DCOLOR(16777215), D3DCOLOR(0), -1.200000047684, 0.899999976158);
    SPHERE_0004_ID_0008_Vertexes[307] := MakeD3DLVERTEX(0.000513858278, -3000.000000000000, 0.000707265048, D3DCOLOR(16777215), D3DCOLOR(0), -1.200000047684, 1.000000000000);
    SPHERE_0004_ID_0008_Vertexes[308] := MakeD3DLVERTEX(-2676.164306640625, -2755.283203125000, -1545.083618164063, D3DCOLOR(16777215), D3DCOLOR(0), -1.333333373070, 0.899999976158);
    SPHERE_0004_ID_0008_Vertexes[309] := MakeD3DLVERTEX(0.000757103437, -3000.000000000000, 0.000437113806, D3DCOLOR(16777215), D3DCOLOR(0), -1.333333373070, 1.000000000000);
    SPHERE_0004_ID_0008_Vertexes[310] := MakeD3DLVERTEX(-3073.239746093750, -2755.283203125000, -323.010131835938, D3DCOLOR(16777215), D3DCOLOR(0), -1.466666698456, 0.899999976158);
    SPHERE_0004_ID_0008_Vertexes[311] := MakeD3DLVERTEX(0.000869438692, -3000.000000000000, 0.000091381575, D3DCOLOR(16777215), D3DCOLOR(0), -1.466666698456, 1.000000000000);
    SPHERE_0004_ID_0008_Vertexes[312] := MakeD3DLVERTEX(-2938.924072265625, -2755.283203125000, 954.914855957031, D3DCOLOR(16777215), D3DCOLOR(0), -1.600000023842, 0.899999976158);
    SPHERE_0004_ID_0008_Vertexes[313] := MakeD3DLVERTEX(0.000831439975, -3000.000000000000, -0.000270151359, D3DCOLOR(16777215), D3DCOLOR(0), -1.600000023842, 1.000000000000);
    SPHERE_0004_ID_0008_Vertexes[314] := MakeD3DLVERTEX(-2296.442138671875, -2755.283203125000, 2067.726318359375, D3DCOLOR(16777215), D3DCOLOR(0), -1.733333349228, 0.899999976158);
    SPHERE_0004_ID_0008_Vertexes[315] := MakeD3DLVERTEX(0.000649677764, -3000.000000000000, -0.000584972673, D3DCOLOR(16777215), D3DCOLOR(0), -1.733333349228, 1.000000000000);
    SPHERE_0004_ID_0008_Vertexes[316] := MakeD3DLVERTEX(-1256.884033203125, -2755.283203125000, 2823.009277343750, D3DCOLOR(16777215), D3DCOLOR(0), -1.866666674614, 0.899999976158);
    SPHERE_0004_ID_0008_Vertexes[317] := MakeD3DLVERTEX(0.000355580356, -3000.000000000000, -0.000798646885, D3DCOLOR(16777215), D3DCOLOR(0), -1.866666674614, 1.000000000000);
    SPHERE_0004_ID_0008_Vertexes[318] := MakeD3DLVERTEX(0.000540302135, -2755.283203125000, 3090.167968750000, D3DCOLOR(16777215), D3DCOLOR(0), -2.000000000000, 0.899999976158);
    SPHERE_0004_ID_0008_Vertexes[319] := MakeD3DLVERTEX(-0.000000000153, -3000.000000000000, -0.000874227786, D3DCOLOR(16777215), D3DCOLOR(0), -2.000000000000, 1.000000000000);
////////////////////////////////////////////////////////////////////////////////

    if (MainPAK <> '') and (MapName <> '') then
      LoadMap;

  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TDXViewerForm.FormDestroy(Sender: TObject);
var
  i: integer;
begin
  DXInput.Keyboard.Enabled := False;
  DXInput.Keyboard.BindInputStates := False;
  DXInput.Mouse.Enabled := False;
  DXInput.Mouse.BindInputStates := False;

  if DisplayModeBox.ItemIndex >= 0 then
  begin
    regSceneWidth.Value :=
      (DisplayModeBox.Items.Objects[DisplayModeBox.ItemIndex] as TD3DSceneDisplayParam).Width;
    regSceneHeight.Value :=
      (DisplayModeBox.Items.Objects[DisplayModeBox.ItemIndex] as TD3DSceneDisplayParam).Height;
    regSceneBitCount.Value :=
      (DisplayModeBox.Items.Objects[DisplayModeBox.ItemIndex] as TD3DSceneDisplayParam).BitCount;
  end;
  for i := 0 to DisplayModeBox.Items.Count - 1 do
    DisplayModeBox.Items.Objects[i].Free;
  DisplayModeBox.Items.Clear;

  regUseHardwareAcceleration.Value := doHardware in DXDraw.Options;
  regSafeMode.Value := Scene.SafeMode;
  regTextureFiltering.Value := doFiltering;
  regShowFog.Value := Fog1.Checked;
  regShowInfo.Value := Information1.Checked;

  Scene.Free;
  Scene := nil;

  DestroyComponents;
////////////////////////////////////////////////////////////////////////////////
// Sky support
  FreeMem(SPHERE_0004_ID_0008_Vertexes, 320 * SizeOf(TD3DLVertex));
////////////////////////////////////////////////////////////////////////////////
end;

function TDXViewerForm.GetCullDistance: TD3DValue;
begin
  if Scene.Position.Y > bCube.maxY then
    Result := bCubeSize * 2
  else if VectorInCube(Scene.Position, bCube) then
  begin
    if Fog1.Checked then
      Result := Min(bCubeSize, 80.0) 
    else
      Result := bCubeSize
  end
  else
  begin
    if Fog1.Checked then
      Result := Min(bCubeSize * 2, 80.0)
    else
      Result := bCubeSize * 2;
  end;
end;

procedure TDXViewerForm.FrameMovie(const Time: Double);
const
  vPos = 6;
  vRot = 20;
var
  vPosition,
  vRotation: single;
  X, Y, Z,
  dX, dY, dZ: single;
  dt: single;
begin

  if (Time <> oldTime) and // ΟΚ, we hope that we get a very very VERY big frame rate!!!!
     Active then
  begin
    { FullScreen movement }
    X := Scene.Position.X;
    Y := Scene.Position.Y;
    Z := Scene.Position.Z;
    dX := Scene.Rotation.X;
    dY := Scene.Rotation.Y;
    dZ := Scene.Rotation.Z;

    DXInput.Update;
    if isButton5 in DXInput.States then
    begin
      vPosition := 2.0 * vPos;
      vRotation := 0.5 * vRot;
    end
    else
    begin
      vPosition := vPos;
      vRotation := vRot;
    end;

    dt := Time - oldTime;

    if isLeft in DXInput.States then
      dY := dY - vPosition * (dt * vRotation * g_DEGTORAD)
    else if isRight in DXInput.States then
      dY := dY + vPosition * (dt * vRotation * g_DEGTORAD);

    if isButton1 in DXInput.States then
      dX := dX - vPosition * (dt * vRotation * g_DEGTORAD)
    else if isButton2 in DXInput.States then
      dX := dX + vPosition * (dt * vRotation * g_DEGTORAD);

    if isUp in DXInput.States then
    begin
      // vPosition -> velocity
      X := X + vPosition * sin(dY) * dt;
      Z := Z + vPosition * cos(dY) * dt;
    end
    else if isDown in DXInput.States then
    begin
      X := X - vPosition * sin(dY) * dt;
      Z := Z - vPosition * cos(dY) * dt;
    end;

    if isButton6 in DXInput.States then
    begin
      X := X - vPosition * cos(dY) * dt;
      Z := Z + vPosition * sin(dY) * dt;
    end
    else if isButton7 in DXInput.States then
    begin
      X := X + vPosition * cos(dY) * dt;
      Z := Z - vPosition * sin(dY) * dt;
    end;

    if isButton3 in DXInput.States then
      Y := Y + vPosition * dt
    else if isButton4 in DXInput.States then
      Y := Y - vPosition * dt;
    Y := Max(FloorInfo.y1 + 1.0, Y);

    if dX < - g_PI_DIV_2 then
      dX := - g_PI_DIV_2
    else if dX > g_PI_DIV_2 then
      dX := g_PI_DIV_2;

    GetPositionInsideBoundingCube(@bCube, bCubeSize / 2.0, X, Y, Z);

    if not VectorEquel(Scene.Position, MakeD3DVECTOR(X, Y, Z)) then
    begin
      dorecalcculls := True;
      Scene.Position := MakeD3DVECTOR(X, Y, Z);
    end;

    if not VectorEquel(Scene.Rotation, MakeD3DVECTOR(dX, dY, dZ)) then
    begin
      dorecalcculls := True;
      Scene.Rotation := MakeD3DVECTOR(dX, dY, dZ);
    end;

    if dorecalcculls then
    begin
      Scene.CullSurfaces(GetCullDistance);
      dorecalcculls := False;
    end;

    // last render time
    OldTime := Time;
  end;
end;

procedure TDXViewerForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
//  Application Close Full Screen
  if Key = VK_ESCAPE then
  begin
    if (doFullScreen in DXDraw.Options) then
      AdjustFullScreen
  end
  else if (Key = Ord('F')) then
  begin
    Fog1Click(Sender);
  end
  else if (Key = Ord('I')) then
  begin
    Information1Click(Sender);
  end;

//  Screen mode change
  if (ssAlt in Shift) and (Key = VK_RETURN) then
    AdjustFullScreen;
end;

procedure TDXViewerForm.ApplicationEvents1Activate(Sender: TObject);
begin
  if Scene = nil then
    Exit;
    
  if DXDraw.Visible then
  try
    DXDraw.Finalize;
    DXDraw.Initialize;
    TryFocusControl(DXDraw);
    Scene.ForceReCalc;
  except
  end;
  DoForegroundForms(self);
end;

procedure TDXViewerForm.DXDrawFinalize(Sender: TObject);
begin
  DXTimer.Enabled := True;
end;

procedure TDXViewerForm.SetHardWare(flag: boolean; fStartup: boolean = False);
begin
  if not fStartup then
    DXDraw.Finalize;
  if flag then
    DXDraw.Options := DXDraw.Options + [doHardWare] - [doSystemMemory]
  else
    DXDraw.Options := DXDraw.Options - [doHardWare] + [doSystemMemory];
  if not fStartup then
  begin
    DXDraw.Initialize;
    Scene.ForceReCalc;
    Scene.ReInitialize;
  end;
end;

procedure TDXViewerForm.SetSafeMode(flag: boolean; fStartup: boolean = False);
begin
  Scene.SafeMode := flag;
  if flag then
  begin
    if doHardWare in DXDraw.Options then
    begin
      SetHardware(False, fStartUp);
      SetHardware(True, fStartUp);
    end;
  end;
end;

procedure TDXViewerForm.AdjustFullScreen(const rlevel: integer = 0);
begin
  if rlevel > 0 then
  begin
    if rlevel > 1 then
      Exit;
    if not (doFullScreen in DXDraw.Options) then
      Exit;
  end;

  DXDraw.Finalize;
  if (doFullScreen in DXDraw.Options) then
  begin
    DXInput.Mouse.Enabled := False;
    DXInput.Mouse.BindInputStates := False;
    RestoreWindow;
    Visible := True;
    DXDraw.Cursor := crDefault;
    Screen.Cursor := crDefault;
    Menu := MainMenu1;
    BorderStyle := bsSizeable;
    DXDraw.Options := DXDraw.Options - [doFullScreen] + [doFlip];
    TryFocusControl(DXDraw);
    ShowHint := True;
    WIN_EnableAltTab;
  end
  else
  begin
    WIN_DisableAltTab;
    if DisplayModeBox.ItemIndex >= 0 then
    begin
      DXDraw.Display.Width :=
        (DisplayModeBox.Items.Objects[DisplayModeBox.ItemIndex] as TD3DSceneDisplayParam).Width;
      DXDraw.Display.Height :=
        (DisplayModeBox.Items.Objects[DisplayModeBox.ItemIndex] as TD3DSceneDisplayParam).Height;
      DXDraw.Display.BitCount :=
        (DisplayModeBox.Items.Objects[DisplayModeBox.ItemIndex] as TD3DSceneDisplayParam).BitCount;
    end
    else
    begin
      DXDraw.Display.Width := DefSceneWidth;
      DXDraw.Display.Height := DefSceneHeight;
      DXDraw.Display.BitCount := DefSceneBitCount;
    end;
    ShowHint := False;
    TryFocusControl(DXDraw);
    StoreWindow;
    Visible := False;
    Menu := nil;
    DXDraw.Cursor := crNone;
    Screen.Cursor := crNone;
    BorderStyle := bsNone;
    DXDraw.Options := DXDraw.Options + [doFullScreen] - [doFlip];
    DXInput.Mouse.Enabled := True;
    DXInput.Mouse.BindInputStates := True;
  end;
  try
    DXDraw.Initialize;
  except
    on E : Exception do
    begin
      serrormessage := E.ClassName + '() : ' + E.Message;
      AdjustFullScreen(rlevel + 1);
      Exit;
    end;
  end;
  Scene.ForceReCalc;
  Scene.ReInitialize;
end;

procedure TDXViewerForm.About1Click(Sender: TObject);
begin
  AboutDialog1.Execute;
end;

var
  SkyTextureBinData: array[0..1339] of Byte = (
    $78, $01, $D5, $9A, $4D, $6B, $5D, $55, $14, $86, $CF, $FE, $17, $FE, $04,
    $87, $8E, $3A, $16, $A7, $D2, $A9, $03, $71, $E6, $54, $0A, $82, $54, $A9,
    $A4, $12, $2C, $58, $69, $A1, $16, $62, $B1, $C4, $46, $FB, $91, $90, $68,
    $9A, $B6, $11, $AB, $45, $0C, $6A, $94, $D2, $52, $0D, $42, $4C, $1A, $07,
    $A9, $F1, $8F, $5C, $D7, $DE, $E7, $DE, $9B, $B3, $BF, $9E, $F7, $DE, $DE,
    $24, $B6, $17, $72, $73, $F6, $FA, $78, $D7, $BB, $D7, $5E, $7B, $ED, $73,
    $4E, $F2, $CA, $F1, $63, $2F, $35, $E1, $73, $CC, $BE, $5F, $B4, $9F, $97,
    $FB, $3F, $AE, $79, $C1, $AE, $EC, $D3, $D7, $B7, $83, $F6, $7B, $EA, $C4,
    $AB, $DD, $61, $72, $DD, $6B, $5C, $22, $49, $86, $AE, $47, $06, $8E, $94,
    $1E, $C9, $BC, $D9, $A4, $D7, $4B, $02, $A6, $C3, $D3, $27, $8E, $BB, $7A,
    $98, $9E, $F2, $4F, $0C, $92, $A1, $67, $88, $08, $B9, $36, $E1, $3B, $34,
    $F0, $F2, $12, $D4, $99, $93, $AF, $A5, $53, $3A, $CA, $B1, $51, $AA, $67,
    $CF, $13, $29, $71, $EE, $12, $3C, $7B, $EA, $75, $5A, $C2, $24, $1D, $5D,
    $CF, $F6, $3A, $E4, $A5, $A9, $41, $F8, $E2, $40, $08, $CF, $3F, $07, $ED,
    $48, $B0, $3E, $CD, $EE, $FC, $D4, $1B, $1D, $EB, $EC, $12, $83, $9B, $75,
    $CF, $91, $05, $54, $66, $1B, $A9, $64, $90, $00, $F2, $F4, $DC, $85, $E9,
    $37, $33, $D2, $1D, $01, $7B, $E7, $FC, $E3, $E0, $25, $7A, $1D, $70, $5B,
    $36, $2E, $9E, $C8, $B6, $3C, $F8, $F4, $DC, $5B, $65, $45, $90, $72, $7B,
    $31, $93, $84, $40, $2F, $AE, $96, $44, $9B, $07, $2A, $E1, $A7, $29, $C8,
    $BD, $F6, $25, $CE, $5D, $9F, $9D, $DE, $1F, $66, $57, $C6, $87, $3B, $5C,
    $42, $38, $03, $B0, $F9, $C4, $7C, $62, $8B, $B0, $BC, $BC, $C6, $42, $BB,
    $BC, $78, $21, $86, $8C, $46, $3A, $81, $71, $01, $0C, $DB, $5D, $8B, $32,
    $AE, $BB, $79, $8D, $85, $60, $F8, $F3, $B3, $1F, $44, $8C, $B3, $01, $73,
    $18, $57, $9B, $2C, $06, $BB, $1B, $17, $61, $E0, $DC, $95, $8B, $6F, $67,
    $94, $8F, $50, $A0, $DA, $BB, $38, $1D, $9C, $FB, $62, $E6, $1D, $A0, $9B,
    $A4, $2B, $B7, $74, $A2, $BC, $B9, $7A, $D1, $39, $04, $8B, $CB, $B3, $10,
    $FF, $DA, $67, $EF, $C2, $0E, $E5, $DD, $E7, $E1, $30, $81, $7E, $F5, $31,
    $05, $3D, $C9, $2F, $A7, $1C, $4B, $16, $66, $DF, $8F, $05, $F1, $08, $A3,
    $9B, $29, $4F, $D0, $F8, $0B, $03, $71, $FA, $C6, $64, $4A, $A3, $A5, $B9,
    $D3, $25, $F1, $40, $A6, $F8, $8B, $F8, $3E, $FD, $04, $01, $4B, $DF, $32,
    $90, $06, $CB, $57, $A7, $07, $5C, $0B, $BF, $C5, $F6, $97, $FD, $41, $D4,
    $97, $D8, $9D, $46, $48, $D5, $57, $B3, $32, $FF, $A1, $59, $15, $A8, $B7,
    $22, $7D, $7F, $55, $75, $ED, $2B, $28, $FD, $14, $B9, $75, $AF, $33, $EB,
    $C3, $AF, $2E, $7E, $A4, $18, $B0, $5E, $F1, $C3, $0D, $EE, $37, $10, $7E,
    $64, $01, $DC, $FD, $EA, $63, $04, $D0, $4A, $AB, $70, $A8, $52, $AE, $7F,
    $89, $2E, $F2, $EF, $9A, $7B, $37, $CF, $01, $88, $9D, $E6, $9C, $81, $E4,
    $B8, $CF, $A1, $38, $BF, $AC, $35, $34, $C1, $BF, $69, $D6, $56, $3F, $C9,
    $83, $0E, $25, $07, $C0, $1F, $EB, $47, $4F, $1F, $DD, $2D, $B7, $6B, $AB,
    $17, $87, $6C, $8B, $17, $B2, $03, $14, $BD, $86, $42, $DF, $00, $EA, $49,
    $04, $D5, $10, $81, $2F, $D6, $BF, $9B, $61, $83, $67, $5C, $7B, $FF, $87,
    $4B, $C0, $D0, $CA, $1F, $B4, $A6, $9A, $38, $81, $39, $7E, $B4, $27, $78,
    $F7, $19, $81, $87, $6B, $97, $81, $E1, $08, $FC, $13, $02, $71, $F4, $6C,
    $FF, $45, $EA, $11, $B6, $A7, $CA, $8F, $DB, $F8, $F9, $73, $E0, $3F, $4A,
    $04, $74, $17, $FD, $3D, $99, $7C, $0E, $25, $0D, $FE, $5C, $FF, $32, $F7,
    $DA, $97, $F8, $D7, $0B, $88, $E1, $7C, $83, $00, $8B, $D0, $FF, $AB, $7A,
    $F6, $35, $1A, $E2, $FE, $B5, $71, $9B, $F7, $AF, $ED, $B3, $3D, $94, $2B,
    $E2, $18, $CE, $3E, $8E, $6A, $EE, $F0, $71, $DB, $0F, $E7, $41, $3D, $92,
    $8A, $08, $B6, $CF, $B3, $D5, $FC, $DB, $FD, $03, $E8, $2C, $BA, $3A, $BE,
    $DD, $CE, $A3, $85, $49, $4E, $7F, $9F, $9D, $3A, $7F, $43, $56, $27, $94,
    $E4, $CF, $29, $74, $3B, $7F, $20, $7F, $7F, $FC, $10, $02, $1E, $8F, $E6,
    $6A, $F5, $8B, $05, $80, $AF, $EF, $42, $60, $0C, $DF, $34, $BB, $9B, $37,
    $89, $DF, $28, $BA, $7A, $FE, $BD, $37, $EF, $6F, $9E, $9D, $B9, $8B, $09,
    $BA, $7F, $B6, $56, $80, $A3, $5F, $7D, $6C, $C1, $A1, $FD, $54, $01, $2C,
    $77, $A1, $7E, $AA, $39, $A4, $3B, $D7, $80, $5A, $F5, $1C, $C4, $DC, $DB,
    $BE, $35, $B8, $2C, $FC, $1E, $85, $BF, $4A, $3F, $D5, $CF, $E4, $CF, $EF,
    $7B, $8F, $6F, $17, $78, $0F, $45, $2A, $FF, $A2, $BC, $0D, $07, $2B, $44,
    $F3, $47, $77, $AB, $8D, $7F, $99, $FF, $70, $22, $4F, $7D, $E1, $33, $50,
    $77, $A6, $C5, $09, $5E, $8A, $FF, $DE, $CE, $1D, $84, $E7, $EA, $08, $FD,
    $B9, $6E, $E2, $91, $99, $80, $68, $9F, $78, $B4, $87, $F9, $ED, $6D, $DF,
    $46, $FE, $F5, $D4, $B5, $E9, $41, $3D, $20, $B7, $7E, $EA, $74, $B0, $02,
    $21, $0C, $D3, $EE, $E2, $FE, $25, $E7, $C0, $40, $1A, $88, $FC, $53, $6D,
    $B5, $01, $30, $82, $6B, $9E, $6C, $AD, $D0, $1C, $79, $F5, $2D, $02, $1B,
    $60, $F6, $3C, $3F, $73, $97, $36, $61, $1E, $B5, $2F, $EE, $FF, $35, $AF,
    $A1, $9C, $0B, $C0, $32, $23, $0C, $0C, $88, $F9, $73, $7E, $EC, $FC, $FD,
    $6B, $B2, $F3, $97, $A3, $FB, $04, $53, $8B, $A1, $A5, $0F, $39, $C2, $EA,
    $F1, $16, $4F, $36, $97, $83, $61, $F9, $4B, $E3, $C7, $FC, $13, $AE, $32,
    $BA, $34, $28, $D3, $EA, $48, $77, $91, $3F, $67, $CF, $60, $78, $82, $B9,
    $36, $01, $CC, $0D, $3A, $D4, $FC, $A5, $34, $F8, $7B, $63, $31, $71, $89,
    $87, $49, $46, $63, $A5, $1A, $85, $3B, $F3, $E8, $78, $48, $E0, $D4, $E3,
    $95, $EC, $FF, $EE, $F1, $23, $7B, $7E, $81, $65, $4C, $02, $2A, $C6, $07,
    $AF, $67, $02, $F6, $FC, $75, $E3, $E0, $63, $1E, $21, $E2, $D6, $83, $C3,
    $E5, $8F, $ED, $47, $F4, $4E, $4B, $43, $DC, $1E, $0A, $79, $D9, $58, $9F,
    $2B, $48, $9F, $1F, $D1, $EF, $3F, $5D, $F9, $5F, $C9, $46, $BB, $BB, $C4,
    $84, $EB, $BF, $79, $F0, $23, $BD, $7F, $2B, $01, $46, $32, $D8, $F9, $91,
    $DD, $53, $0F, $54, $80, $DF, $F0, $FD, $A7, $F2, $A6, $CE, $D5, $A7, $CC,
    $F9, $93, $01, $D4, $C4, $7F, $FD, $DE, $DE, $3F, $03, $0A, $87, $57, $E8,
    $87, $AF, $FF, $E5, $EE, $0C, $9C, $71, $C9, $71, $99, $D3, $61, $03, $40,
    $6E, $A1, $CC, $9D, $6D, $18, $DF, $40, $F8, $EF, $2F, $39, $E1, $67, $4D,
    $72, $6F, $E5, $FC, $24, $94, $38, $3F, $9C, $5B, $1F, $57, $F6, $77, $45,
    $EE, $DB, $AF, $E9, $EF, $8F, $BA, $BB, $C9, $F3, $29, $35, $88, $F6, $DA,
    $C4, $F4, $9B, $6F, $96, $CE, $C2, $F6, $F5, $F5, $09, $EA, $FC, $F6, $3E,
    $62, $E7, $73, $97, $2D, $50, $64, $C1, $AF, $BF, $BC, $BF, $7A, $FF, $76,
    $EB, $C6, $19, $6F, $F6, $DC, $7E, $F8, $FF, $1F, $26, $9F, $56, $96, $FF,
    $08, $52, $6E, $10, $69, $B0, $34, $37, $15, $21, $8E, $3B, $48, $CB, $3B,
    $F6, $F7, $CF, $BF, $B1, $24, $1E, $49, $7A, $B1, $79, $61, $B4, $30, $7B,
    $8A, $0A, $1C, $A3, $1B, $1C, $EB, $A3, $5A, $2F, $04, $6F, $45, $6C, $C5,
    $11, $9A, $EB, $97, $DF, $03, $7F, $CE, $9E, $E7, $0F, $CE, $78, $AE, $0F,
    $66, $63, $F4, $10, $42, $64, $A8, $B9, $7A, $E9, $E4, $00, $AA, $F0, $9B,
    $AB, $D7, $1C, $B8, $00, $58, $5B, $88, $37, $BE, $E8, $3F, $1C, $F8, $B3,
    $0C, $36, $30, $00, $00
  );

procedure TDXViewerForm.LoadMap;
var
  ret: integer;
  cs: TCriticalSection;
  bmz: TZBitmap;
  m: TMemoryStream;
  btm: TD3DValue;
begin
  cs := TCriticalSection.Create;
  Screen.Cursor := crHourglass;
  isLoading := True;
  try
    cs.Enter;
    Scene.New;
    Scene.MaxPolygonVertexes := maxpverts;
    Scene.FileName := MainPak;
    Scene.ForcePosition(MakeD3DVector(0, 0, 0));
    Scene.ForceRotation(MakeD3DVector(0, 0, 0));
    ret := GetQuake2MapData(Scene, MainPak, BspFile, MapName, Factor, lFactor, TessellationLevel);
    if ret <> 0 then
      ErrorMessageBox.Execute(Quake2ErrorToString(ret))
    else
    begin
      if MapName <> MainPak then
        Caption := Format(rsFmtTitle, [MkShortName(MapName), MkShortName(MainPak)])
      else
        Caption := Format(rsFmtTitle2, [MkShortName(MapName)]);
    end;
    Scene.ReduceMemory;
    Scene.ReleaseUnusedTextures;

    if Scene.Surfaces.Count > 0 then
      Scene.ForcePosition((Scene.Surfaces.Objects[0] as TD3DObject).GetLocatePosition(1.0));

    bCube := Scene.BoundingCube;

    bCubeSize :=
      Max(
        Max(bCube.maxX - bCube.minX, bCube.maxY - bCube.minY),
          bCube.maxZ - bCube.minZ);

    dorecalcculls := True;
    Scene.CullSurfaces(GetCullDistance);

    btm := bCube.minY - 1.0 - factor / 32;
    FloorInfo.y1 := btm;
    FloorInfo.y2 := btm;
    FloorInfo.y3 := btm;
    FloorInfo.y4 := btm;

    Scene.AddSurface(ID3D_Quadrangle, @FloorInfo);

    bmz := TZBitmap.Create;
    m := TMemoryStream.Create;
    try
      m.Write(SkyTextureBinData, SizeOf(SkyTextureBinData));
      m.Position := 0;
      bmz.LoadFromStream(m);
      skyTexture := 'SKY';
      Texture_sky2 := Scene.AddTextureToCollection(skyTexture, bmz);
      if Texture_sky2 <> nil then
        Texture_sky2.Surface.TransparentColor := 0;
    finally
      bmz.Free;
      m.Free;
    end;

    AdjustFocus;
  finally
    TotalSceneTriangles := Scene.GetNumTriangles;
    FullScreen1.Enabled := TotalSceneTriangles > 0;
    FullScreen2.Enabled := TotalSceneTriangles > 0;
    isLoading := False;
    cs.Release;
    cs.Free;
    Screen.Cursor := crDefault;
  end;
end;


procedure TDXViewerForm.OpenClick(Sender: TObject);
var
  s: TStringList;
begin
  s := TStringList.Create;
  try
    if QueryImportQuakeMap(self, MainPAK, BspFile, MapName, Factor, lFactor, maxpverts, TessellationLevel, s) then
    begin
      NextMap.Visible := False;
      ComboBox1.Visible := False;
      PrevMap.Visible := False;
      ComboBox1.Items.Clear;
      if MainPAK = '' then
        ErrorNoFileSelectMessageBox.Execute
      else if MapName = '' then
        ErrorNoMapSelectedMessageBox.Execute
      else
      begin
        PrevMap.Visible := True;
        ComboBox1.Visible := True;
        NextMap.Visible := True;
        ComboBox1.Items.AddStrings(s);
        ComboBox1.ItemIndex := ComboBox1.Items.IndexOf(MapName);
        ComboBox1.Enabled := ComboBox1.Items.Count > 0;
        PrevMap.Enabled := ComboBox1.ItemIndex > 0;
        NextMap.Enabled := ComboBox1.ItemIndex <> ComboBox1.Items.Count - 1;
        LoadMap;
      end;
    end;
  finally
    s.Free;
  end;
end;

procedure TDXViewerForm.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TDXViewerForm.FullScreenClick(Sender: TObject);
begin
  AdjustFullScreen;
end;

procedure TDXViewerForm.DisplayModeBoxChange(Sender: TObject);
begin
  AdjustFocus;
end;

procedure TDXViewerForm.PrevMapClick(Sender: TObject);
begin
  if ComboBox1.ItemIndex > 0 then
  begin
    ComboBox1.ItemIndex := ComboBox1.ItemIndex - 1;
    PrevMap.Enabled := ComboBox1.ItemIndex > 0;
    NextMap.Enabled := True;
    ComboBox1.Enabled := True;
    MapName := ComboBox1.Items.Strings[ComboBox1.ItemIndex];
    LoadMap;
  end;
end;

procedure TDXViewerForm.NextMapClick(Sender: TObject);
begin
  if ComboBox1.ItemIndex < ComboBox1.Items.Count - 1 then
  begin
    ComboBox1.ItemIndex := ComboBox1.ItemIndex + 1;
    PrevMap.Enabled := True;
    NextMap.Enabled := ComboBox1.ItemIndex < ComboBox1.Items.Count - 1;
    ComboBox1.Enabled := True;
    MapName := ComboBox1.Items.Strings[ComboBox1.ItemIndex];
    LoadMap;
  end;
end;

procedure TDXViewerForm.ComboBox1Change(Sender: TObject);
begin
  MapName := ComboBox1.Items.Strings[ComboBox1.ItemIndex];
  LoadMap;
  AdjustFocus;
end;

procedure TDXViewerForm.CopyClick(Sender: TObject);
var
  aBitmap: TBitmap;
  r: TRect;
begin
  aBitmap := TBitmap.Create;
  try
    aBitmap.Width := DXDraw.Width;
    aBitmap.Height := DXDraw.Height;
    if aBitmap.Width * aBitmap.Height <> 0 then
    begin
      SetRect(r, 0, 0, aBitmap.Width, aBitmap.Height);
      aBitmap.Canvas.CopyRect(r, DXDraw.Surface.Canvas, r);
      DXDraw.Surface.Canvas.Release;
      Clipboard.Assign(aBitmap);
    end
    else
      ClipboardErrorMessageBox.Execute;
  finally
    aBitmap.Free;
  end;
end;

procedure TDXViewerForm.ApplicationEvents1Hint(Sender: TObject);
begin
  if Trim(Application.Hint) = EmptyStr then
    StatusBar1.Panels[0].Text := ' ' + Application.Title
  else
    StatusBar1.Panels[0].Text := ' ' + Trim(Application.Hint);
end;

resourceString
  rsOpen = 'open';
  rsMailTo = 'mailto';
  rsJimmyValavanis = 'jimmyvalavanis';
  rsProvider = 'yahoo.gr';
  rsSubject = 'subject';
  rsFmtMail = '%s:%s@%s?%s=%s';

procedure TDXViewerForm.Contactme1Click(Sender: TObject);
begin
  ShellExecute(
    handle,
      PChar(rsOpen),
        PChar(Format(rsFmtMail, [rsMailTo, rsJimmyValavanis, rsProvider, rsSubject, Application.Title])),
          nil, nil, SW_SHOWNORMAL);
end;

procedure TDXViewerForm.AdjustFocus;
begin
  if Visible then
  try
    TryFocusControl(DXDraw);
  except
  end;
end;

procedure TDXViewerForm.FormShow(Sender: TObject);
begin
  Timer1.Enabled := True;
  AdjustFocus;
end;

procedure TDXViewerForm.QuickInfo1Click(Sender: TObject);
begin
  with TQuickInfoForm.Create(self) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TDXViewerForm.Homepage1Click(Sender: TObject);
begin
  VisitHtmlPage(handle, rsHomePage);
end;

resourceString
  rsExtPNG = '.png';
  rsExtJPG1 = '.jpg';
  rsExtJPG2 = '.jpeg';
  rsExtBMP = '.bmp';
  rsExtPPM = '.ppm';
  rsExtM8 =  '.m8';
  rsExtTGA = '.tga';

procedure TDXViewerForm.Save2Click(Sender: TObject);
var
  ext: string;
  aBitmap: TBitmap;
  aPNG: TPNGObject;
  aJPG: TJpegImage;
  r: TRect;
begin
  SavePictureDialog1.FileName := '';
  if SavePictureDialog1.Execute then
  begin
    Screen.Cursor := crHourglass;
    try
      ext := ExtractFileExt(SavePictureDialog1.Filename);
      aBitmap := nil;
      if ext = '' then
      begin
        case SavePictureDialog1.FilterIndex of
          1: ext := rsExtPNG;
          2: ext := rsExtJPG1;
          3: ext := rsExtBMP;
          4: ext := rsExtPPM;
          5: ext := rsExtM8;
          6: ext := rsExtTGA;
        else
          begin
            MessageBox1.Execute;
            exit;
          end;
        end;
        SavePictureDialog1.Filename := SavePictureDialog1.Filename + ext;
      end;
      if (UpperCase(ext) = UpperCase(rsExtPNG)) or
         (UpperCase(ext) = UpperCase(rsExtJPG1)) or
         (UpperCase(ext) = UpperCase(rsExtJPG2)) then
      begin
        aBitmap := TTGABitmap.Create;
        if Assigned(aBitmap) then
        begin
          try
            aBitmap.Width := DXDraw.Width;
            aBitmap.Height := DXDraw.Height;
            if aBitmap.Width * aBitmap.Height <> 0 then
            begin
              aBitmap.PixelFormat := pf32bit;
              SetRect(r, 0, 0, aBitmap.Width, aBitmap.Height);
              aBitmap.Canvas.CopyRect(r, DXDraw.Surface.Canvas, r);
              DXDraw.Surface.Canvas.Release;
            end;
            if UpperCase(ext) = UpperCase(rsExtPNG) then
            begin
              aPNG := TPNGObject.Create;
              try
                aPNG.Assign(aBitmap);
                CreateBackupFile(SavePictureDialog1.Filename);
                aPNG.SaveToFile(SavePictureDialog1.Filename);
              finally
                aPNG.Free;
              end;
            end
            else
            begin
              aJPG := TJpegImage.Create;
              try
                aJPG.Assign(aBitmap);
                CreateBackupFile(SavePictureDialog1.Filename);
                aJPG.SaveToFile(SavePictureDialog1.Filename);
              finally
                aJPG.Free;
              end;
            end;
          finally
            aBitmap.Free;
          end;
        end
        else
          MessageBox1.Execute;
      end
      else
      begin
        if UpperCase(ext) = UpperCase(rsExtBMP) then
          aBitmap := TBitmap.Create
        else if UpperCase(ext) = UpperCase(rsExtM8) then
          aBitmap := TM8Bitmap.Create
        else if UpperCase(ext) = UpperCase(rsExtPPM) then
          aBitmap := TPPMBitmap.Create
        else if UpperCase(ext) = UpperCase(rsExtTGA) then
          aBitmap := TTGABitmap.Create;
        if Assigned(aBitmap) then
        begin
          try
            aBitmap.Width := DXDraw.Width;
            aBitmap.Height := DXDraw.Height;
            if aBitmap.Width * aBitmap.Height <> 0 then
            begin
              SetRect(r, 0, 0, aBitmap.Width, aBitmap.Height);
              aBitmap.Canvas.CopyRect(r, DXDraw.Surface.Canvas, r);
              DXDraw.Surface.Canvas.Release;
            end;
            CreateBackupFile(SavePictureDialog1.Filename);
            aBitmap.SaveToFile(SavePictureDialog1.Filename);
          finally
            aBitmap.Free;
          end;
        end
        else
          MessageBox1.Execute;
      end;
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TDXViewerForm.Hardware1Click(Sender: TObject);
begin
  Hardware1.Checked := not Hardware1.Checked;
  SetHardWare(Hardware1.Checked);
end;

procedure TDXViewerForm.Options1Click(Sender: TObject);
begin
  Hardware1.Checked := doHardware in DXDraw.Options;
  SafeModerendering1.Enabled := Scene <> nil;
  if Scene <> nil then
    SafeModerendering1.Checked := Scene.SafeMode;
end;

procedure TDXViewerForm.SafeModerendering1Click(Sender: TObject);
begin
  SafeModerendering1.Checked := not SafeModerendering1.Checked;
  SetSafeMode(SafeModerendering1.Checked);
end;

procedure TDXViewerForm.DXDrawClick(Sender: TObject);
begin
  TryFocusControl(DXDraw);
end;

procedure TDXViewerForm.Fog1Click(Sender: TObject);
begin
  Fog1.Checked := not Fog1.Checked;
  AdjustFog;
end;

procedure TDXViewerForm.AdjustFog;
begin
  if Scene <> nil then
  begin
    if Fog1.Checked then
      Scene.StartFog(0, 255, 20.0, 50.0, 0.01)
    else
      Scene.StopFog;
  end;
  dorecalcculls := True;
end;

procedure TDXViewerForm.Texturefiltering1Click(Sender: TObject);
begin
  Texturefiltering1.Checked := not Texturefiltering1.Checked;
  doFiltering := Texturefiltering1.Checked;
  regTextureFiltering.Value := doFiltering;
end;

procedure TDXViewerForm.Timer1Timer(Sender: TObject);
begin
  if Active then
    DoForegroundForms(self);
end;

procedure TDXViewerForm.FormResize(Sender: TObject);
begin
  if DXDraw <> nil then
    TryFocusControl(DXDraw);
  AdjustFog;
  Repaint;
end;

procedure TDXViewerForm.Information1Click(Sender: TObject);
begin
  Information1.Checked := not Information1.Checked;
  regShowInfo.Value := Information1.Checked;
end;

end.

