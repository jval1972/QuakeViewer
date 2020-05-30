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
//  Open Map Form
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  New Site: https://sourceforge.net/projects/quakeviewer/
//  Old Site: http://www.geocities.ws/jimmyvalavanis/applications/quakeviewer.html
//------------------------------------------------------------------------------

{$I defs.inc}

unit OpenQuakeMapFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, Spin, StdCtrls, Buttons, rmBaseEdit, rmBtnEdit, ExtCtrls,
  AnotherReg, MessageBox, Variants;

type
  TImportQuakeMapForm = class(TForm)
    Notebook1: TNotebook;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    PrevMap: TSpeedButton;
    ComboBox1: TComboBox;
    NextMap: TSpeedButton;
    SpinEdit1: TSpinEdit;
    Label4: TLabel;
    Label5: TLabel;
    TrackBar1: TTrackBar;
    Label6: TLabel;
    Label7: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    OKBtn: TButton;
    CancelBtn: TButton;
    OpenDialog1: TOpenDialog;
    OpenDialog2: TOpenDialog;
    TessellationPanel: TPanel;
    Label8: TLabel;
    TrackBar2: TTrackBar;
    Label10: TLabel;
    Label9: TLabel;
    Timer1: TTimer;
    ImagesPanel: TPanel;
    Image2: TImage;
    Image1: TImage;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    TrackBar3: TTrackBar;
    Bevel1: TBevel;
    procedure FileEdit1Btn1Click(Sender: TObject);
    procedure FileEdit2Btn1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure EnableUpdateThreadComponentExecute(Sender: TObject);
    procedure PrevMapClick(Sender: TObject);
    procedure NextMapClick(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FileEdit2Exit(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FileEdit1Change(Sender: TObject);
    procedure FileEdit2Change(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure Label6Click(Sender: TObject);
    procedure Label7Click(Sender: TObject);
    procedure Label12Click(Sender: TObject);
    procedure Label13Click(Sender: TObject);
    procedure Label10Click(Sender: TObject);
    procedure Label9Click(Sender: TObject);
  private
    { Private declarations }
    closing: boolean;
    fQuakeVersion: integer;

    FormRestorer1: TFormRestorer;
    regLightFactor: TVariantProfile;
    regSizeFactor: TVariantProfile;
    regTessellation: TVariantProfile;

    MessageBoxErrVersions: TMessageBox;
    MessageBoxErrUnSupported: TMessageBox;
    MessageBoxErrUndeterminedFileType: TMessageBox;

    FileEdit1: TrmBtnEdit;
    FileEdit2: TrmBtnEdit;

    procedure EnableUpdate;
    procedure PopulateMapEntries;
    procedure CreateComponents;
    procedure DestroyComponents;
  public
    { Public declarations }
  end;

function QueryImportQuakeMap(AOwner: TComponent;
  var MainPAK, BspFile: TFileName; var MapName: string;
  var Factor: integer; var lFactor: single; var maxverts: integer;
  var TessellationLevel: integer; var Maps: TStringList): boolean;

implementation

uses
  se_DXDUtils, se_Quake2Utils, se_QuakeTypes, se_ZipFile, smoothshow, Unit1;

{$R *.DFM}

function QueryImportQuakeMap(AOwner: TComponent;
  var MainPAK, BspFile: TFileName; var MapName: string;
  var Factor: integer; var lFactor: single; var maxverts: integer;
  var TessellationLevel: integer; var Maps: TStringList): boolean;
var
  mvrt: integer;
begin
  Result := False;
  with TImportQuakeMapForm.Create(AOwner) do
  try
    FileEdit1.Text := MainPAK;
    FileEdit2.Text := BspFile;
{    if Factor >= 0 then SpinEdit1.Value := Factor;
    if lFactor >= 0 then TrackBar1.Position := round(lFactor * (TrackBar1.Max - TrackBar1.Min));
    TrackBar2.Position := GetIntegerInRange(TessellationLevel, TrackBar2.Min, TrackBar2.Max);}
    PopulateMapEntries;
    if ComboBox1.Items.Count > 0 then
      ComboBox1.ItemIndex := ComboBox1.Items.IndexOf(MapName);
    TrackBar3.Min := MINTRVERTEXES div 10;
    TrackBar3.Max := MAXTRVERTEXES div 10;
    mvrt := maxverts;
    if (mvrt < MINTRVERTEXES) or (mvrt > MAXTRVERTEXES) then
      mvrt := DEFTRVERTEXES;
    TrackBar3.Position := mvrt div 10;
    ShowModal;
    if ModalResult = mrOK then
    begin
      MainPAK := FileEdit1.Text;
      BSPFile := FileEdit2.Text;
      Factor := SpinEdit1.Value;
      lFactor := TrackBar1.Position / (TrackBar1.Max - TrackBar1.Min);
      TessellationLevel := TrackBar2.Position;
      if ComboBox1.ItemIndex <> -1 then
        MapName := ComboBox1.Items[ComboBox1.ItemIndex];
      if Maps = nil then
        Maps := TStringList.Create
      else
        Maps.Clear;
      Maps.AddStrings(ComboBox1.Items);
      maxverts := TrackBar3.Position * 10;
      Result := True;
    end;
  finally
    Free;
  end;
end;

procedure TImportQuakeMapForm.FileEdit1Btn1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    FileEdit1.Text := OpenDialog1.FileName;
    PopulateMapEntries;
  end;
end;

procedure TImportQuakeMapForm.FileEdit2Btn1Click(Sender: TObject);
begin
  if OpenDialog2.Execute then
  begin
    FileEdit2.Text := OpenDialog2.FileName;
    PopulateMapEntries;
  end;
end;

procedure TImportQuakeMapForm.FormShow(Sender: TObject);
begin
  FormSmoothShow(self, DXViewerForm);
end;

procedure TImportQuakeMapForm.EnableUpdate;
begin
  PrevMap.Enabled := (ComboBox1.Items.Count > 1) and (ComboBox1.ItemIndex > 0);
  NextMap.Enabled := (ComboBox1.Items.Count > 1) and (ComboBox1.ItemIndex < ComboBox1.Items.Count - 1);
  ComboBox1.Enabled := ComboBox1.Items.Count > 0;
  if (ComboBox1.Items.Count > 0) and (ComboBox1.ItemIndex = -1) then
    ComboBox1.ItemIndex := 0;
  OKBtn.Enabled := ComboBox1.Enabled;
  TessellationPanel.Visible := fQuakeVersion = 3;
end;

procedure TImportQuakeMapForm.EnableUpdateThreadComponentExecute(Sender: TObject);
begin
  EnableUpdate;
end;

procedure TImportQuakeMapForm.PrevMapClick(Sender: TObject);
begin
  if (ComboBox1.Items.Count > 1) and (ComboBox1.ItemIndex > 0) then
  begin
    ComboBox1.ItemIndex := ComboBox1.ItemIndex - 1;
    EnableUpdate;
  end;
end;

procedure TImportQuakeMapForm.NextMapClick(Sender: TObject);
begin
  if (ComboBox1.Items.Count > 1) and (ComboBox1.ItemIndex < ComboBox1.Items.Count - 1) then
  begin
    ComboBox1.ItemIndex := ComboBox1.ItemIndex + 1;
    EnableUpdate;
  end;
end;

procedure TImportQuakeMapForm.ComboBox1Change(Sender: TObject);
begin
  EnableUpdate;
end;

resourceString
  rsHint1 = 'Main PK3/PAK/BSP file';
  rsHint2 = 'External PK3/PAK/BSP file';

procedure TImportQuakeMapForm.PopulateMapEntries;
var
  f: TFileStream;
  id: integer;
  Nr: integer; // Number of entries
  Ofs: integer; // Position inside PAK file to read the entries
  i, idx: integer;
  lastMapName: string;
  Entry: FPakHead;
  Entry2: FPakHead2;
  s: string;
  z: TZipFile;
begin
  if closing then
    Exit;

  Screen.Cursor := crHourglass;
  try
    idx := ComboBox1.ItemIndex;
    if idx > -1 then
      lastMapName := ComboBox1.Items[ComboBox1.ItemIndex]
    else
      lastMapName := '';
    ComboBox1.Items.Clear;
    if FileExists(FileEdit1.Text) and (FileEdit1.Text <> '') then
    begin
      if UpperCase(ExtractFileExt(FileEdit1.Text)) = UpperCase(rsExtPak) then
      begin
        f := TFileStream.Create(FileEdit1.Text, fmOpenRead or fmShareDenyWrite);
        try
          f.Read(id, SizeOf(id));
          if id <> Pakid then
            MessageBoxErrUndeterminedFileType.Execute
          else if f.Size < (12 + SizeOf(FPakHead)) then
            MessageBoxErrUndeterminedFileType.Execute
          else
          begin
            f.Read(Ofs, SizeOf(Ofs));
            f.Read(Nr, SizeOf(Nr));
            if Nr mod SizeOf(FPakHead) = 0 then
            begin
              Nr := Nr div SizeOf(FPakHead);
              f.Seek(ofs, soFromBeginning);
              for i := 0 to Nr - 1 do
              begin
                f.Read(Entry, SizeOf(FPakHead));
                s := TrimStr(Entry.Name);
                if Length(s) > Length(rsExtBsp) then
                  if UpperCase(RightStr(s, Length(rsExtBsp))) = UpperCase(rsExtBsp) then
                    ComboBox1.Items.Add(AdjustQuake2EntryName(s));
              end;
            end
            else if Nr mod SizeOf(FPakHead2) = 0 then
            begin
              Nr := Nr div SizeOf(FPakHead2);
              f.Seek(ofs, soFromBeginning);
              for i := 0 to Nr - 1 do
              begin
                f.Read(Entry2, SizeOf(FPakHead2));
                s := TrimStr(Entry2.Name);
                if Length(s) > Length(rsExtBsp) then
                  if UpperCase(RightStr(s, Length(rsExtBsp))) = UpperCase(rsExtBsp) then
                    ComboBox1.Items.Add(AdjustQuake2EntryName(s));
              end;
            end
            else
              MessageBoxErrUndeterminedFileType.Execute;
          end;
        finally
          f.Free;
        end;
      end
      else if UpperCase(ExtractFileExt(FileEdit1.Text)) = UpperCase(rsExtBsp) then
        ComboBox1.Items.Add(AdjustQuake2EntryName(FileEdit1.Text))
      else if UpperCase(ExtractFileExt(FileEdit1.Text)) = UpperCase(rsExtPK3) then
      begin
        z := TZipFile.Create(FileEdit1.Text);
        try
          for i := 0 to z.FileCount - 1 do
          begin
            s := z.Files[i];
            if Length(s) > Length(rsExtBsp) then
              if UpperCase(RightStr(s, Length(rsExtBsp))) = UpperCase(rsExtBsp) then
                ComboBox1.Items.Add(AdjustQuake2EntryName(s));
          end;
        finally
          z.Free;
        end;
      end
      else
        MessageBoxErrUndeterminedFileType.Execute
    end;

    if FileExists(FileEdit2.Text) and (FileEdit2.Text <> '') then
    begin
      if UpperCase(ExtractFileExt(FileEdit2.Text)) = UpperCase(rsExtPak) then
      begin
        f := TFileStream.Create(FileEdit2.Text, fmOpenRead or fmShareDenyWrite);
        try
          f.Read(id, SizeOf(id));
          if id <> Pakid then
            MessageBoxErrUndeterminedFileType.Execute
          else if f.Size < (12 + SizeOf(FPakHead)) then
            MessageBoxErrUndeterminedFileType.Execute
          else
          begin
            f.Read(Ofs, SizeOf(Ofs));
            f.Read(Nr, SizeOf(Nr));
            if Nr mod SizeOf(FPakHead) = 0 then
            begin
              Nr := Nr div SizeOf(FPakHead);
              f.Seek(ofs, soFromBeginning);
              for i := 0 to Nr - 1 do
              begin
                f.Read(Entry, SizeOf(FPakHead));
                s := TrimStr(Entry.Name);
                if Length(s) > Length(rsExtBsp) then
                  if UpperCase(RightStr(s, Length(rsExtBsp))) = UpperCase(rsExtBsp) then
                    if ComboBox1.Items.IndexOf(s) = -1 then
                      ComboBox1.Items.Add(AdjustQuake2EntryName(s));
              end;
            end
            else if Nr mod SizeOf(FPakHead2) = 0 then
            begin
              Nr := Nr div SizeOf(FPakHead2);
              f.Seek(ofs, soFromBeginning);
              for i := 0 to Nr - 1 do
              begin
                f.Read(Entry2, SizeOf(FPakHead2));
                s := TrimStr(Entry2.Name);
                if Length(s) > Length(rsExtBsp) then
                  if UpperCase(RightStr(s, Length(rsExtBsp))) = UpperCase(rsExtBsp) then
                    if ComboBox1.Items.IndexOf(s) = -1 then
                      ComboBox1.Items.Add(AdjustQuake2EntryName(s));
              end;
            end
            else
              MessageBoxErrUndeterminedFileType.Execute
          end;
        finally
          f.Free;
        end;
      end
      else if UpperCase(ExtractFileExt(FileEdit2.Text)) = UpperCase(rsExtBsp) then
      begin
        if ComboBox1.Items.IndexOf(FileEdit2.Text) = -1 then
          ComboBox1.Items.Add(AdjustQuake2EntryName(FileEdit2.Text))
      end
      else if UpperCase(ExtractFileExt(FileEdit1.Text)) = UpperCase(rsExtPK3) then
      begin
        z := TZipFile.Create(FileEdit1.Text);
        try
          for i := 0 to z.FileCount - 1 do
          begin
            s := z.Files[i];
            if Length(s) > Length(rsExtBsp) then
              if UpperCase(RightStr(s, Length(rsExtBsp))) = UpperCase(rsExtBsp) then
                ComboBox1.Items.Add(AdjustQuake2EntryName(s));
          end;
        finally
          z.Free;
        end;
      end
      else
        MessageBoxErrUndeterminedFileType.Execute;
    end;

    if lastMapName <> '' then
      ComboBox1.ItemIndex := ComboBox1.Items.IndexOf(lastMapName);

    if FileEdit1.Text <> '' then
      FileEdit1.Hint := GetBspDescription(FileEdit1.Text)
    else
      FileEdit1.Hint := rsHint1;

    if FileEdit2.Text <> '' then
      FileEdit2.Hint := GetBspDescription(FileEdit2.Text)
    else
      FileEdit2.Hint := rsHint2;

    if FileEdit1.Text <> '' then
      fQuakeVersion := GetQuakeVersion(FileEdit1.Text)
    else if FileEdit2.Text <> '' then
      fQuakeVersion := GetQuakeVersion(FileEdit2.Text)
    else
      fQuakeVersion := 0;

    EnableUpdate;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TImportQuakeMapForm.CreateComponents;
begin
  FormRestorer1 := TFormRestorer.Create(self);
  FormRestorer1.ParentKey := DXViewerForm.AppConfigKey1;
  FormRestorer1.Name := 'FormRestorer1';
  FormRestorer1.Restoring := frPositionOnly;
  FormRestorer1.Restore;

  regLightFactor := TVariantProfile.Create(self);
  regLightFactor.Key := FormRestorer1;
  regLightFactor.Name := 'LightFactor';

  regSizeFactor := TVariantProfile.Create(self);
  regSizeFactor.Key := FormRestorer1;
  regSizeFactor.Name := 'SizeFactor';

  regTessellation := TVariantProfile.Create(self);
  regTessellation.Key := FormRestorer1;
  regTessellation.Name := 'Tessellation';


  MessageBoxErrUndeterminedFileType := TMessageBox.Create(self);
  MessageBoxErrUndeterminedFileType.Caption := 'QuakeViewer';
  MessageBoxErrUndeterminedFileType.Text :=
      'Can not determine file type. Supported BSP versions are: '#13#10 +
      '29): Quake1, Hexen2)'#13#10 +
      '30): Half-Life, Blue-Shift, Counter Strike'#13#10 +
      '38): Quake2, Heretic2'#13#10 +
      '46): Quake3'#13#10 +
      '47): RTCW';
  MessageBoxErrUndeterminedFileType.Buttons := mbxOK;
  MessageBoxErrUndeterminedFileType.Icon := mbxIconError;
  MessageBoxErrUndeterminedFileType.DefaultButton := mbxDefButton1;
  MessageBoxErrUndeterminedFileType.Modality := mbxTaskModal;
  MessageBoxErrUndeterminedFileType.TextAlignment := mbxLeft;

  MessageBoxErrVersions := TMessageBox.Create(self);
  MessageBoxErrVersions.Caption := 'QuakeViewer';
  MessageBoxErrVersions.Text := 'Basic and external PAK/BSP files must be from the same game!';
  MessageBoxErrVersions.Buttons := mbxOK;
  MessageBoxErrVersions.Icon := mbxIconError;
  MessageBoxErrVersions.DefaultButton := mbxDefButton1;
  MessageBoxErrVersions.Modality := mbxTaskModal;
  MessageBoxErrVersions.TextAlignment := mbxLeft;

  MessageBoxErrUnSupported := TMessageBox.Create(self);
  MessageBoxErrUnSupported.Caption := 'QuakeViewer';
  MessageBoxErrUnSupported.Text := 'Unsupported BSP version!';
  MessageBoxErrUnSupported.Buttons := mbxOK;
  MessageBoxErrUnSupported.Icon := mbxIconError;
  MessageBoxErrUnSupported.DefaultButton := mbxDefButton1;
  MessageBoxErrUnSupported.Modality := mbxTaskModal;
  MessageBoxErrUnSupported.TextAlignment := mbxLeft;

  FileEdit1 := TrmBtnEdit.Create(self);
  FileEdit1.Parent := NoteBook1;
  FileEdit1.Left := 152;
  FileEdit1.Top := 14;
  FileEdit1.Width := 361;
  FileEdit1.Height := 21;
  FileEdit1.Hint := 'Basic PAK/BSP file';
  FileEdit1.BtnWidth := 22;
  FileEdit1.Btn1Glyph := Image1.Picture.Bitmap;
  FileEdit1.Btn1NumGlyphs := 1;
  FileEdit1.Btn2Glyph := Image2.Picture.Bitmap;
  FileEdit1.Btn2NumGlyphs := 1;
  FileEdit1.TabOrder := 0;
  FileEdit1.OnChange := FileEdit1Change;
  FileEdit1.OnBtn1Click := FileEdit1Btn1Click;

  FileEdit2 := TrmBtnEdit.Create(self);
  FileEdit2.Parent := NoteBook1;
  FileEdit2.Left := 152;
  FileEdit2.Top := 38;
  FileEdit2.Width := 361;
  FileEdit2.Height := 21;
  FileEdit2.Hint := 'External map BSP/PAK file';
  FileEdit2.BtnWidth := 22;
  FileEdit2.Btn1Glyph := Image1.Picture.Bitmap;
  FileEdit2.Btn1NumGlyphs := 1;
  FileEdit2.Btn2Glyph := Image2.Picture.Bitmap;
  FileEdit2.Btn2NumGlyphs := 1;
  FileEdit2.TabOrder := 1;
  FileEdit2.OnChange := FileEdit2Change;
  FileEdit2.OnExit := FileEdit2Exit;
  FileEdit2.OnBtn1Click := FileEdit2Btn1Click;
end;

procedure TImportQuakeMapForm.DestroyComponents;
begin
  regLightFactor.Value := TrackBar1.Position;
  regLightFactor.Free;

  regSizeFactor.Value := SpinEdit1.Value;
  regSizeFactor.Free;

  regTessellation.Value := TrackBar2.Position;
  regTessellation.Free;

  FormRestorer1.Store;
  FormRestorer1.Free;

  MessageBoxErrVersions.Free;
  MessageBoxErrUnSupported.Free;
  MessageBoxErrUndeterminedFileType.Free;
end;

procedure TImportQuakeMapForm.FormCreate(Sender: TObject);
begin
  closing := False;
  fQuakeVersion := 0;
  CreateComponents;
  if not VarIsEmpty(regSizeFactor.Value) then
    if IsIntegerInRange(regSizeFactor.Value, SpinEdit1.MinValue, SpinEdit1.MaxValue) then
      SpinEdit1.Value := regSizeFactor.Value;
  if not VarIsEmpty(regLightFactor.Value) then
    if IsIntegerInRange(regLightFactor.Value, TrackBar1.Min, TrackBar1.Max) then
      TrackBar1.Position := regLightFactor.Value;
  if not VarIsEmpty(regTessellation.Value) then
    if IsIntegerInRange(regTessellation.Value, TrackBar2.Min, TrackBar2.Max) then
      TrackBar2.Position := regTessellation.Value;
end;

procedure TImportQuakeMapForm.FormDestroy(Sender: TObject);
begin
  DestroyComponents;
end;

resourceString
  rsFmtUnsupported = 'Unsupported BSP version (%d)!';

procedure TImportQuakeMapForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
  v1, v2: integer;
begin
  if ModalResult = mrOK then
  begin
    if FileEdit1.Text = '' then
      if FileEdit2.Text <> '' then
        FileEdit1.Text := FileEdit2.Text;

    CanClose := FileExists(FileEdit1.Text) and (FileEdit1.Text <> '') and
                (FileExists(FileEdit2.Text) or (FileEdit2.Text = '')) and
                (ComboBox1.ItemIndex > -1);
    if CanClose and (FileEdit2.Text <> '') then
    begin
      v1 := GetBspVersion(FileEdit1.Text);
      v2 := GetBspVersion(FileEdit2.Text);
      if v1 <> v2 then
        if v1 <> -1 then
        begin
          MessageBoxErrVersions.Execute;
          CanClose := False;
          Exit;
        end;
      if not IsBspVersionSupported(v1) then
        begin
          MessageBoxErrUnSupported.Text := Format(rsFmtUnsupported, [v1]);
          MessageBoxErrUnSupported.Execute;
          CanClose := False;
          Exit;
        end;
    end
    else if CanClose and (FileEdit2.Text = '') then
    begin
      v1 := GetBspVersion(FileEdit1.Text);
      if not IsBspVersionSupported(v1) then
        begin
          MessageBoxErrUnSupported.Text := Format(rsFmtUnsupported, [v1]);
          MessageBoxErrUnSupported.Execute;
          CanClose := False;
          Exit;
        end;
    end;
  end
  else
    CanClose := True;
  if not CanClose then
    MessageBeep(MB_ICONHAND);
end;

procedure TImportQuakeMapForm.FileEdit2Exit(Sender: TObject);
begin
//  FileEdit2.Text := Trim(FileEdit2.Text);
//  PopulateMapEntries;
end;

procedure TImportQuakeMapForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  closing := True;
end;

procedure TImportQuakeMapForm.FileEdit1Change(Sender: TObject);
begin
//  FileEdit1.Text := Trim(FileEdit1.Text);
//  PopulateMapEntries;
end;

procedure TImportQuakeMapForm.FileEdit2Change(Sender: TObject);
begin
//  FileEdit2.Text := Trim(FileEdit2.Text);
//  PopulateMapEntries;
end;

procedure TImportQuakeMapForm.Timer1Timer(Sender: TObject);
begin
  EnableUpdate;
end;

procedure TImportQuakeMapForm.FormHide(Sender: TObject);
begin
  FormSmoothHide(self, DXViewerForm);
end;

procedure TImportQuakeMapForm.Label6Click(Sender: TObject);
begin
  if TrackBar1.Position > TrackBar1.Min then
    TrackBar1.Position := TrackBar1.Position - 1;
end;

procedure TImportQuakeMapForm.Label7Click(Sender: TObject);
begin
  if TrackBar1.Position < TrackBar1.Max then
    TrackBar1.Position := TrackBar1.Position + 1;
end;

procedure TImportQuakeMapForm.Label12Click(Sender: TObject);
begin
  if TrackBar3.Position > TrackBar3.Min then
    TrackBar3.Position := TrackBar3.Position - 1;
end;

procedure TImportQuakeMapForm.Label13Click(Sender: TObject);
begin
  if TrackBar3.Position < TrackBar3.Max then
    TrackBar3.Position := TrackBar3.Position + 1;
end;

procedure TImportQuakeMapForm.Label10Click(Sender: TObject);
begin
  if TrackBar2.Position > TrackBar2.Min then
    TrackBar2.Position := TrackBar2.Position - 1;
end;

procedure TImportQuakeMapForm.Label9Click(Sender: TObject);
begin
  if TrackBar2.Position < TrackBar2.Max then
    TrackBar2.Position := TrackBar2.Position + 1;
end;

end.
