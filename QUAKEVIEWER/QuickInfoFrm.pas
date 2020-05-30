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
//  Quick Help Form
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  New Site: https://sourceforge.net/projects/quakeviewer/
//  Old Site: http://www.geocities.ws/jimmyvalavanis/applications/quakeviewer.html
//------------------------------------------------------------------------------

{$I defs.inc}

unit QuickInfoFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, AnotherReg;

type
  TQuickInfoForm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    OKBtn: TButton;
    Bevel1: TBevel;
    Panel3: TPanel;
    Memo1: TMemo;
    Panel4: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    procedure Label2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
  private
    { Private declarations }
    FormRestorer1: TFormRestorer;
  public
    { Public declarations }
  end;

var
  QuickInfoForm: TQuickInfoForm;

implementation

{$R *.DFM}

uses
  se_utils, qv_globals, smoothshow, unit1;

procedure TQuickInfoForm.Label2Click(Sender: TObject);
begin
  VisitHtmlPage(handle, rsHomePage);
end;

procedure TQuickInfoForm.FormCreate(Sender: TObject);
begin
  FormRestorer1 := TFormRestorer.Create(self);
  FormRestorer1.ParentKey := DXViewerForm.AppConfigKey1;
  FormRestorer1.Name := 'FormRestorer1';
  FormRestorer1.Restoring := frPositionOnly;
  FormRestorer1.Restore;
end;

procedure TQuickInfoForm.FormDestroy(Sender: TObject);
begin
  FormRestorer1.Store;
  FormRestorer1.Free;
end;

procedure TQuickInfoForm.FormShow(Sender: TObject);
begin
  FormSmoothShow(self, DXViewerForm);
end;

procedure TQuickInfoForm.FormHide(Sender: TObject);
begin
  FormSmoothHide(self, DXViewerForm);
end;

end.
