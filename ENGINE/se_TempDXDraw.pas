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
//  Temporary rendering Form (if the Application does not provide one)
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//------------------------------------------------------------------------------

unit se_TempDXDraw;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  se_DXDraws, se_DXClass;

type
  TTempDXDrawForm = class(TDXForm)
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    DXDraw: TDXDraw;
  end;

implementation

{$R *.DFM}

procedure TTempDXDrawForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TTempDXDrawForm.FormCreate(Sender: TObject);
begin
  DXDraw := TDXDraw.Create(self);
  DXDraw.Parent := self;
  DXDraw.Left := 8;
  DXDraw.Top := 16;
  DXDraw.Width := 320;
  DXDraw.Height := 240;
  DXDraw.AutoInitialize := True;
  DXDraw.AutoSize := True;
  DXDraw.Color := clBlack;
  DXDraw.Display.BitCount := 32;
  DXDraw.Display.FixedBitCount := False;
  DXDraw.Display.FixedRatio := True;
  DXDraw.Display.FixedSize := False;
  DXDraw.Options := [doAllowReboot, doWaitVBlank, doCenter, doFlip, do3D, doDirectX7Mode, doHardware, doSelectDriver, doZBuffer];
  DXDraw.SurfaceHeight := 240;
  DXDraw.SurfaceWidth := 320;
  DXDraw.TabOrder := 0;
end;

end.
