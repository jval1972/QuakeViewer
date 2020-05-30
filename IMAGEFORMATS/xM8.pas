// The Heretic II mipmap graphic format is a raster image format introduced by Heretic II game. 
// This graphic format is a MipMap, that means it have the full size picture and some other 
// pictures each one one half the size of the previous picture.File extenstion for this graphic 
// format is m8 (*.m8). Supported bits per pixel for this graphic format is 8bpp. It uses a 256 
// color palette, just as 8bpp Windows bitmaps.
///////////////////////////////////////////////////
// Author: Jim Valavanis, 
// E-Mail: jimmyvalavanis@yahoo.gr
// Site  : http://www.geocities.com/jimmyvalavanis/
///////////////////////////////////////////////////

// 2018:
//  Original version can still be downloaded at my old geocities site at:
//  http://www.geocities.ws/jimmyvalavanis/programming/gformats/m8.html

unit xM8;

{$P+,S-,W-,R-,T-,X+,H+}
{$C PRELOAD}

interface

uses
  Windows, Forms, SysUtils, Classes, Graphics;

type
  TM8Bitmap = class(TBitmap)
  private
    procedure WriteM8StreamData(Stream: TStream);
    procedure ReadM8StreamData(Stream: TStream);
  protected
    procedure WriteData(Stream: TStream); override;
    procedure ReadData(Stream: TStream); override;
  public
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromStream(Stream: TStream); override;
  end;

resourceString
  rsPPMError = 'Error reading M8 file: Wrong file type.';

implementation

{ TM8Bitmap }

type
  TQuake2Palette = packed array[0..255] of packed record R, G, B: byte; end;

  // Heretic2 m8 header
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

procedure TM8Bitmap.WriteData(Stream: TStream);
begin
  WriteM8StreamData(Stream);
end;

procedure TM8Bitmap.SaveToStream(Stream: TStream);
begin
  WriteM8StreamData(Stream);
end;

procedure TM8Bitmap.LoadFromStream(Stream: TStream);
begin
  ReadM8StreamData(Stream);
end;

procedure TM8Bitmap.ReadData(Stream: TStream);
begin
  ReadM8StreamData(Stream);
end;

procedure TM8Bitmap.ReadM8StreamData(Stream: TStream);
var
  aBitmap: TBitmap;
  Header: Miptex_T_m8;
  i, j: integer;
  P1: PByteArray;
  lpal: PLogPalette;
  hpal: HPALETTE;
  nearBlack: integer;
  pos: integer;
begin
  pos := Stream.Position;
  Stream.Read(Header, SizeOf(Header));
  aBitmap := TBitmap.Create;
  GetMem(lpal, SizeOf(TLogPalette) + SizeOf(TPaletteEntry) * 255);
  hpal := 0;
  try
    aBitmap.Width := Header.Widths[0];
    aBitmap.Height := Header.Heights[0];
    aBitmap.PixelFormat := pf8Bit;

    lpal.palVersion := $300;
    lpal.palNumEntries := 256;
    lpal.palPalEntry[0].peRed := 0;
    lpal.palPalEntry[0].peGreen := 0;
    lpal.palPalEntry[0].peBlue := 0;
    for j := 1 to 255 do
    begin
      lpal.palPalEntry[j].peRed := Header.Palette[j].r;
      lpal.palPalEntry[j].peGreen := Header.Palette[j].g;
      lpal.palPalEntry[j].peBlue := Header.Palette[j].b;
    end;
    nearBlack := 1;

    for j := 255 downto 1 do
    begin
      if ((lpal.palPalEntry[j].peRed +
           lpal.palPalEntry[j].peGreen +
           lpal.palPalEntry[j].peBlue) <
          (lpal.palPalEntry[nearBlack].peRed +
           lpal.palPalEntry[nearBlack].peGreen +
           lpal.palPalEntry[nearBlack].peBlue)) and
         ((lpal.palPalEntry[j].peRed +
           lpal.palPalEntry[j].peGreen +
           lpal.palPalEntry[j].peBlue) <> 0) then nearBlack := j;
    end;

    for j := 255 downto 1 do
    begin
      if lpal.palPalEntry[j].peRed + lpal.palPalEntry[j].peGreen + lpal.palPalEntry[j].peBlue = 0 then
        lpal.palPalEntry[j] := lpal.palPalEntry[nearBlack];
    end;

    hpal := CreatePalette(lpal^);

    if hpal <> 0 then
      aBitmap.Palette := hpal;

    stream.Position := pos + Header.Offsets[0];
    for i := 0 to aBitmap.Height - 1 do
    begin
      P1 := aBitmap.Scanline[i];
      Stream.Read(P1^, aBitmap.width);
      for j := 0 to aBitmap.Width -  1 do
        if P1[j] = 0 then P1[j] := nearBlack;
    end;
    Assign(aBitmap);
  finally
    aBitmap.Free;
    FreeMem(lpal, SizeOf(TLogPalette) + SizeOf(TPaletteEntry) * 255);
    if hPal <> 0 then
      DeleteObject(hPal);
  end;
end;

type
  TColorAppearence = record
    color: TColor;
    num: integer;
  end;

  TColorAppearences = array[0..$FFFF] of TColorAppearence;
  PColorAppearences = ^TColorAppearences;

function AlmostEqualColors(c1, c2: TColor): boolean;
begin
  result := sqr(integer(GetRValue(c1)) - integer(GetRValue(c2))) +
            sqr(integer(GetGValue(c1)) - integer(GetGValue(c2))) +
            sqr(integer(GetBValue(c1)) - integer(GetBValue(c2))) <= 256;
end;

procedure ForceBitmapTo8bpp(bmp: TBitmap);
// Converts a bitmap to 8 bits per pixel, returns false
// if bitmap has more than 256 unique colors
var 
  CC: PColorAppearences;
  i, j, k: integer;
  b: PByteArray;
  numC: integer;
  c: TColor;
  found: boolean;
  lpal: PLogPalette;
  hpal: HPALETTE;
  newBMPData: PByteArray;
  dist, dist1: integer;
  index: integer;
begin
  if bmp.PixelFormat in [pf1bit, pf4bit] then
    bmp.PixelFormat := pf8bit
  else if bmp.PixelFormat <> pf8bit then
  begin
    bmp.PixelFormat := pf24bit;
    numC := 16;
    CC := nil;
    ReAllocMem(CC, SizeOf(TColorAppearence));
    // Default Windows Palette
    CC[0].color := RGB(0, 0, 0);
    CC[0].num := 0;

    GetMem(newBMPData, bmp.Width * bmp.Height);
    for i := 0 to bmp.Height - 1 do
    begin
      b := bmp.ScanLine[i];
      for j := 0 to bmp.Width - 1 do
      begin
        c := RGB(b[3 * j + 2], b[3 * j + 1], b[3 * j]);
        found := false;
        for k := 0 to numC - 1 do
        begin
        // Color already exists in the palette, increase appearences counter
          if AlmostEqualColors(CC[k].color, c) then
//          if CC[k].color = c then
          begin
            found := true;
            CC[k].num := CC[k].num + 1;
            newBMPData[i * bmp.Width + j] := k;
            break;
          end;
        end;
        // New color, add it to the palette
        if not found then
        begin
          if numC < 256 then  // Less than 256 colors
          begin
            inc(numC);
            ReAllocMem(CC, numC * SizeOf(TColorAppearence));
            newBMPData[i * bmp.Width + j] := numC - 1;
            CC[numC - 1].color := c;
            CC[numC - 1].num := 1;
          end
          else
          begin
          // Find the closest color
            dist := MAXINT;
            index := 0;
            for k := 0 to 255 do
            begin
              dist1 := abs(integer(GetRValue(CC[k].color)) - integer(GetRValue(c))) *
                       abs(integer(GetGValue(CC[k].color)) - integer(GetGValue(c))) *
                       abs(integer(GetBValue(CC[k].color)) - integer(GetBValue(c)));
              if dist1 < dist then
              begin
                dist := dist1;
                index := k;
              end;
            end;
            newBMPData[i * bmp.Width + j] := index;
          end;
        end;
      end;
    end;
    GetMem(lpal, SizeOf(TLogPalette) + SizeOf(TPaletteEntry) * 255);
    lpal.palVersion := $300;
    lpal.palNumEntries := 256;
    for i := 0 to numC - 1 do
    begin
      lpal.palPalEntry[i].peRed := GetRValue(CC[i].color);
      lpal.palPalEntry[i].peGreen := GetGValue(CC[i].color);
      lpal.palPalEntry[i].peBlue := GetBValue(CC[i].color);
    end;
    for i := numC to 255 do
    begin
      lpal.palPalEntry[i].peRed := 0;
      lpal.palPalEntry[i].peGreen := 0;
      lpal.palPalEntry[i].peBlue := 0;
    end;
    bmp.PixelFormat := pf8bit;
    hpal := CreatePalette(lpal^);
    if hpal <> 0 then bmp.Palette := hpal;
    for i := 0 to bmp.Height - 1 do
    begin
      b := bmp.ScanLine[i];
      for j := 0 to bmp.Width - 1 do
        b[j] := newBMPData[i * bmp.Width + j];
    end;
    FreeMem(lpal, SizeOf(TLogPalette) + SizeOf(TPaletteEntry) * 255);
//    DeleteObject(hpal);

    FreeMem(newBMPData, bmp.Width * bmp.Height);
    ReAllocMem(CC, 0);
  end;
end;

procedure TM8Bitmap.WriteM8StreamData(Stream: TStream);
var
  aBitmap: TBitmap;
  i: integer;
  P1: PByteArray;
  Header: Miptex_T_m8;
  Entries: array[0..255] of TPaletteEntry;
begin
  aBitmap := TBitmap.Create;
  try
    aBitmap.Assign(self);
    if aBitmap.PixelFormat <> pf8bit then
      ForceBitmapTo8bpp(aBitmap);

    FillChar(Header, SizeOf(Header), Chr(0));
    GetPaletteEntries(aBitmap.Palette, 0, 255, Entries);
    for i := 0 to 255 do
    begin
      Header.Palette[i].R := Entries[i].peRed;
      Header.Palette[i].G := Entries[i].peGreen;
      Header.Palette[i].B := Entries[i].peBlue;
    end;
    Header.Identifier := 2; // Default Heretic2 id

    for i := 0 to 15 do
    begin
      Header.Widths[i] := aBitmap.Width;
      Header.Heights[i] := aBitmap.Height;
      Header.Offsets[i] := SizeOf(Header);
    end;
    Stream.Write(Header, SizeOf(Header));
    for i := 0 to aBitmap.Height - 1 do
    begin
      P1 := aBitmap.ScanLine[i];
      Stream.Write(P1^, aBitmap.Width);
    end;
  finally
    aBitmap.Free;
  end;
end;

initialization
  { Register the TM8Bitmap as a new graphic file format
    now all the TPicture storage stuff can access our new
    M8 graphic format !
  }
  TPicture.RegisterFileFormat('M8','M8 (Heretic 2) Mipmap bitmap', TM8Bitmap);

finalization
  TPicture.UnregisterGraphicClass(TM8Bitmap);

end.
