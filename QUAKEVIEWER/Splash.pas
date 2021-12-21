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
//  Splash Form
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  New Site: https://sourceforge.net/projects/quakeviewer/
//  Old Site: http://www.geocities.ws/jimmyvalavanis/applications/quakeviewer.html
//------------------------------------------------------------------------------

{$I defs.inc}

unit Splash;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls;

type
  TSplashForm = class(TForm)
    Panel1: TPanel;
    Image1: TImage;
    VersionLabel: TLabel;
    Label3: TLabel;
    Label1: TLabel;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    counter: integer;
  public
    { Public declarations }
  end;

var
  SplashForm: TSplashForm;

implementation

{$R *.DFM}

procedure TSplashForm.FormCreate(Sender: TObject);
begin
  Update;
end;

procedure TSplashForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TSplashForm.FormShow(Sender: TObject);
begin
  counter := 10;
  Timer1.Enabled := true;
end;

procedure TSplashForm.Timer1Timer(Sender: TObject);
begin
  dec(counter);
  if counter < 0 then
  begin
    Timer1.Enabled := False;
    Close;
  end;
end;

end.
