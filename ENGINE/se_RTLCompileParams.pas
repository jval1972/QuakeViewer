{$I defs.inc}

unit se_RTLCompileParams;

interface

uses
  Classes, Graphics, se_DirectX;

type
  PRTLCompileParams = ^TRTLCompileParams;
  TRTLCompileParams = record
    xMin, xMax,             // Όρια άξονα Χ
    yMin, yMax,             // Όρια άξονα Υ
    zMin, zMax: TD3DValue;  // Όρια άξονα Ζ
    vPosition,              // Ταχύτητα κίνησης
    vRotation: TD3DValue;   // Ταχύτητα περιστροφής
    fltMin, fltMag: TD3DTextureMinFilter;
    UseAntializing: boolean;
    BitCount: byte;
    Width,
    Height: integer;
    BackColor: TColor;
    AppTitle: string;
    MapName: string;
  end;

procedure MakeDefaultCompilerParams(var c: TRTLCompileParams);

procedure ReadRTLCompilerParams(s: TStream; var c: TRTLCompileParams);

{$IFDEF DESIGNER}
procedure WriteRTLCompilerParams(s: TStream; c: TRTLCompileParams);
{$ENDIF}

implementation

resourceString
  rsDefAppTitle = 'Direct 3D World Viewer';
  rsDefMapName = 'MAP01.D3D';

procedure MakeDefaultCompilerParams(var c: TRTLCompileParams);
begin
  with c do
  begin
    xMin := -1000.0;
    xMax :=  1000.0;
    yMin := -1000.0;
    yMax :=  1000.0;
    zMin := -1000.0;
    zMax :=  1000.0;
    vPosition := 10.0;
    vRotation := 15.0;
    fltMin := D3DTFN_LINEAR;
    fltMag := D3DTFN_LINEAR;
    UseAntializing := true;
    BitCount := 32;
    Width := 1024;
    Height := 768;
    BackColor := clBlack;
    AppTitle := rsDefAppTitle;
    MapName := rsDefMapName;
  end;
end;

procedure ReadRTLCompilerParams(s: TStream; var c: TRTLCompileParams);
var l: smallInt;
    i: integer;
begin
  with c do
  begin
    s.Read(xMin, SizeOf(xMin));
    s.Read(xMax, SizeOf(xMax));
    s.Read(yMin, SizeOf(yMin));
    s.Read(yMax, SizeOf(yMax));
    s.Read(zMin, SizeOf(zMin));
    s.Read(zMax, SizeOf(zMax));
    s.Read(vPosition, SizeOf(vPosition));
    s.Read(vRotation, SizeOf(vRotation));
    s.Read(fltMin, SizeOf(fltMin));
    s.Read(fltMag, SizeOf(fltMag));
    s.Read(UseAntializing, SizeOf(UseAntializing));
    s.Read(BitCount, SizeOf(BitCount));
    s.Read(Width, SizeOf(Width));
    s.Read(Height, SizeOf(Height));
    s.Read(BackColor, SizeOf(BackColor));

    s.Read(l, SizeOf(l));
    SetLength(AppTitle, l);
    for i := 1 to l do
      s.Read(AppTitle[i], SizeOf(AppTitle[i]));

    s.Read(l, SizeOf(l));
    SetLength(MapName, l);
    for i := 1 to l do
      s.Read(MapName[i], SizeOf(MapName[i]));
  end;
end;

{$IFDEF DESIGNER}
procedure WriteRTLCompilerParams(s: TStream; c: TRTLCompileParams);
var l: smallInt;
    i: integer;
begin
  with c do
  begin
    s.Write(xMin, SizeOf(xMin));
    s.Write(xMax, SizeOf(xMax));
    s.Write(yMin, SizeOf(yMin));
    s.Write(yMax, SizeOf(yMax));
    s.Write(zMin, SizeOf(zMin));
    s.Write(zMax, SizeOf(zMax));
    s.Write(vPosition, SizeOf(vPosition));
    s.Write(vRotation, SizeOf(vRotation));
    s.Write(fltMin, SizeOf(fltMin));
    s.Write(fltMag, SizeOf(fltMag));
    s.Write(UseAntializing, SizeOf(UseAntializing));
    s.Write(BitCount, SizeOf(BitCount));
    s.Write(Width, SizeOf(Width));
    s.Write(Height, SizeOf(Height));
    s.Write(BackColor, SizeOf(BackColor));

    l := Length(AppTitle);
    s.Write(l, SizeOf(l));
    for i := 1 to l do
      s.Write(AppTitle[i], SizeOf(AppTitle[i]));

    l := Length(MapName);
    s.Write(l, SizeOf(l));
    for i := 1 to l do
      s.Write(MapName[i], SizeOf(MapName[i]));
  end;
end;
{$ENDIF}

end.
