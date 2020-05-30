//------------------------------------------------------------------------------
//
//  QuakeViewer: 3D Viewer for the games Quake I, II, III, Half-Life,
//    Counter Strike, Hexen 2, Heretic 2 and RTCW.
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
//  Main Programm
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  New Site: https://sourceforge.net/projects/quakeviewer/
//  Old Site: http://www.geocities.ws/jimmyvalavanis/applications/quakeviewer.html
//------------------------------------------------------------------------------

program QuakeViewer;

uses
  FastMM4 in 'FASTMM\FastMM4.pas',
  FastMM4Messages in 'FASTMM\FastMM4Messages.pas',
  Forms,
  se_DirectX in 'ENGINE\se_DirectX.pas',
  se_WADS in 'ENGINE\se_WADS.pas',
  se_D3DUtils in 'ENGINE\se_D3DUtils.pas',
  se_DXClasses in 'ENGINE\se_DXClasses.pas',
  se_DXDUtils in 'ENGINE\se_DXDUtils.pas',
  se_DXMeshes in 'ENGINE\se_DXMeshes.pas',
  se_DXTables in 'ENGINE\se_DXTables.pas',
  se_DXTextureEffects in 'ENGINE\se_DXTextureEffects.pas',
  se_DXDraws in 'ENGINE\se_DXDraws.pas',
  se_DXClass in 'ENGINE\se_DXClass.pas',
  se_DXConsts in 'ENGINE\se_DXConsts.pas',
  se_DXTexImg in 'ENGINE\se_DXTexImg.pas',
  se_DXRender in 'ENGINE\se_DXRender.pas',
  se_DXInput in 'ENGINE\se_DXInput.pas',
  se_Main in 'ENGINE\se_Main.pas',
  se_MyD3DUtils in 'ENGINE\se_MyD3DUtils.pas',
  se_TempDXDraw in 'ENGINE\se_TempDXDraw.pas' {TempDXDrawForm},
  se_QuakeTypes in 'ENGINE\se_QuakeTypes.pas',
  se_Quake2Utils in 'ENGINE\se_Quake2Utils.pas',
  se_ZipFile in 'ENGINE\se_ZipFile.pas',
  se_Utils in 'ENGINE\se_Utils.pas',
  se_IDSoftData in 'ENGINE\se_IDSoftData.pas',
  se_RTLCompileParams in 'ENGINE\se_RTLCompileParams.pas',
  zBitmap in 'IMAGEFORMATS\zBitmap.pas',
  pcximage in 'IMAGEFORMATS\pcximage.pas',
  pngimage in 'IMAGEFORMATS\pngimage.pas',
  pnglang in 'IMAGEFORMATS\pnglang.pas',
  xGif in 'IMAGEFORMATS\xGIF.pas',
  xM8 in 'IMAGEFORMATS\xM8.pas',
  xPPM in 'IMAGEFORMATS\xPPM.pas',
  xStubGraphic in 'IMAGEFORMATS\xStubGraphic.pas',
  dibimage in 'IMAGEFORMATS\dibimage.pas',
  xTGA in 'IMAGEFORMATS\xTGA.pas',
  xWZ in 'IMAGEFORMATS\xWZ.pas',
  XPMenu in 'LIBRARY\XPMenu.pas',
  About in 'LIBRARY\About.pas' {AboutBox},
  Aboutdlg in 'LIBRARY\Aboutdlg.pas',
  AnotherReg in 'LIBRARY\AnotherReg.pas',
  binarydata in 'LIBRARY\binarydata.pas',
  DropDownButton in 'LIBRARY\DropDownButton.pas',
  MessageBox in 'LIBRARY\MessageBox.pas',
  rmBaseEdit in 'LIBRARY\rmBaseEdit.pas',
  rmBtnEdit in 'LIBRARY\rmBtnEdit.pas',
  rmLibrary in 'LIBRARY\rmLibrary.pas',
  rmSpeedBtns in 'LIBRARY\rmSpeedBtns.pas',
  smoothshow in 'LIBRARY\smoothshow.pas',
  zlibpas in 'ZLIB\zlibpas.pas',
  Unit1 in 'QUAKEVIEWER\Unit1.pas' {DXViewerForm},
  OpenQuakeMapFrm in 'QUAKEVIEWER\OpenQuakeMapFrm.pas' {ImportQuakeMapForm},
  QuickInfoFrm in 'QUAKEVIEWER\QuickInfoFrm.pas' {QuickInfoForm},
  Splash in 'QUAKEVIEWER\Splash.pas' {SplashForm},
  qv_argv in 'QUAKEVIEWER\qv_argv.pas',
  qv_globals in 'QUAKEVIEWER\qv_globals.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'QuakeViewer';
  Application.CreateForm(TDXViewerForm, DXViewerForm);
  Application.Run;
end.
