// Portable Pixelmap is a UNIX raster format for exchanging images in color. Supported bit per pixel 
// depth is 24bpp(true color). Both binary and ascii formats are supported.
///////////////////////////////////////////////////
// Author: Jim Valavanis, 
// E-Mail: jimmyvalavanis@yahoo.gr
// Site  : http://www.geocities.com/jimmyvalavanis/
///////////////////////////////////////////////////

// 2018:
//  Original version can still be downloaded at my old geocities site at:
//  http://www.geocities.ws/jimmyvalavanis/programming/gformats/ppm.html

unit xPPM;

{$P+,S-,W-,R-,T-,X+,H+}
{$C PRELOAD}

interface

uses
  Windows, Forms, SysUtils, Classes, Graphics;

type
  TPPMBitmapType = (ppmBinary, ppmAscii);

  TPPMBitmap = class(TBitmap)
  private
    procedure WritePPMStreamData(Stream: TStream);
    procedure ReadPPMStreamData(Stream: TStream);
  protected
    procedure WriteData(Stream: TStream); override;
    procedure ReadData(Stream: TStream); override;
  public
    Copyright: string;
    ppmType: TPPMBitmapType;
    constructor Create; override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromStream(Stream: TStream); override;
  end;

resourceString
  rsCopyrightVJ = '# TPPMBitmap Delphi Component, Copyright 2002, Jim Valavanis';
  rsPPMHeaderBinary = 'P6';
  rsPPMHeaderAscii = 'P3';
  rsPPMError = 'Error reading PPM file: Wrong file type.';

implementation

{ TPPMBitmap }

constructor TPPMBitmap.Create;
begin
  Inherited;
  Copyright := rsCopyrightVJ;
  ppmType := ppmBinary;
end;

procedure TPPMBitmap.WriteData(Stream: TStream);
begin
  WritePPMStreamData(Stream);
end;

procedure TPPMBitmap.SaveToStream(Stream: TStream);
begin
  WritePPMStreamData(Stream);
end;

procedure TPPMBitmap.LoadFromStream(Stream: TStream);
begin
  ReadPPMStreamData(Stream);
end;

procedure TPPMBitmap.ReadData(Stream: TStream);
begin
  ReadPPMStreamData(Stream);
end;

function FirstWord(s: string): string;
var i: integer;
begin
  result := '';
  i := 1;
  while (s[i] = ' ') and (i < length(s)) do inc(i);
  repeat
    if s[i] <> ' ' then result := result + s[i];
    inc(i);
  until (i - 1 = length(s)) or (s[i - 1] = ' ');
end;

function SecondWord(s: string): string;
var i: integer;
begin
  result := '';
  i := 1;
  while (s[i] = ' ') and (i < length(s)) do inc(i);
  while (s[i] <> ' ') and (i < length(s)) do inc(i);
  while (s[i] = ' ') and (i < length(s)) do inc(i);
  repeat
    if s[i] <> ' ' then result := result + s[i];
    inc(i);
  until (i - 1 = length(s)) or (s[i - 1] = ' ');
end;

function ThirdWord(s: string): string;
var i: integer;
begin
  result := '';
  i := 1;
  while (s[i] = ' ') and (i < length(s)) do inc(i);
  while (s[i] <> ' ') and (i < length(s)) do inc(i);
  while (s[i] = ' ') and (i < length(s)) do inc(i);
  while (s[i] <> ' ') and (i < length(s)) do inc(i);
  while (s[i] = ' ') and (i < length(s)) do inc(i);
  if i < length(s) then
  repeat
    if s[i] <> ' ' then result := result + s[i];
    inc(i);
  until (i - 1 = length(s)) or (s[i - 1] = ' ');
end;

type
  TDelimeters = set of char;
const
  PPMDelimeters: TDelimeters = [#10, ' '];

function NextWord(Stream: TStream; Delimeters: TDelimeters): string;
var c: char;
  function NextCh: char;
  begin
    Stream.Read(c, SizeOf(c));
    result := c;
  end;
begin
  result := '';
  while (Stream.Position < Stream.Size) and (NextCh in Delimeters) do;
  if Stream.Position = Stream.size then
    exit
  else
    result := c;
  while (Stream.Position < Stream.Size) and not (NextCh in Delimeters) do
    result := result + c;
end;

procedure TPPMBitmap.ReadPPMStreamData(Stream: TStream);
var
  aBitmap : TBitmap;
  buf : Array [0..8191] of byte;
  Header: string;
  s: string;
  c: char;
  i,j: integer;
  P1: PByteArray;
begin
  Stream.Read(c, SizeOf(c));
  Header := c;
  Stream.Read(c, SizeOf(c));
  Header := Header + c;
  if Header = rsPPMHeaderBinary then
    ppmType := ppmBinary
  else if Header = rsPPMHeaderAscii then
    ppmType := ppmAscii
  else
  begin
    raise Exception.Create(rsPPMError);
    exit;
  end;
  Stream.Read(c, SizeOf(c));
  if not (c in PPMDelimeters) then
  begin
    raise Exception.Create(rsPPMError);
    exit;
  end;
  repeat
    s := NextWord(Stream, [#10]);
  until s[1] <> '#'; // End comment
  aBitmap := TBitmap.Create;
  aBitmap.Width := StrToInt(FirstWord(s));
  aBitmap.Height := StrToInt(SecondWord(s));
  if ThirdWord(s) = '' then
    NextWord(Stream, PPMDelimeters); // Next line (?Bitcount?) not implement, use 255 for writer
  aBitmap.PixelFormat := pf24bit;
  if ppmType = ppmBinary then for i := 0 to aBitmap.Height - 1 do
  begin
    Stream.Read(buf, aBitmap.Width * 3);
    P1 := aBitmap.Scanline[i];
    for j := 0 to (aBitmap.Width - 1) do
    begin
      P1[j*3]   := buf[j*3+2];
      P1[j*3+1] := buf[j*3+1];
      P1[j*3+2] := buf[j*3];
    end;
  end
  else
    for i := 0 to aBitmap.Height - 1 do
    begin
      P1 := aBitmap.Scanline[i];
      for j := 0 to (aBitmap.Width - 1) do
      begin
        P1[j*3+2] := StrToInt(NextWord(Stream, PPMDelimeters));
        P1[j*3+1] := StrToInt(NextWord(Stream, PPMDelimeters));
        P1[j*3]   := StrToInt(NextWord(Stream, PPMDelimeters));
      end;
    end;
  Assign(aBitmap);
  aBitmap.Free;
end;

procedure TPPMBitmap.WritePPMStreamData(Stream: TStream);
var
  aBitmap: TBitmap;
  buf : Array [0..8191] of byte;
  i, j: integer;
  P1: PByteArray;
  sizeInfo,s: string;
  c: char;
begin
  aBitmap := TBitmap.Create;
  try
    aBitmap.Assign(self);
    aBitmap.PixelFormat := pf24bit;
    c := Chr(10);
    if ppmType = ppmBinary then
      Stream.Write(PChar(rsPPMHeaderBinary)^, Length(rsPPMHeaderBinary))
    else
      Stream.Write(PChar(rsPPMHeaderAscii)^, Length(rsPPMHeaderAscii));
    Stream.Write(c, SizeOf(c)); // write delimeter
    Stream.Write(PChar(rsCopyrightVJ)^, Length(rsCopyrightVJ));
    Stream.Write(c, SizeOf(c)); // write delimeter
    sizeInfo := IntToStr(aBitmap.Width) + ' ' + IntToStr(aBitmap.Height);
    Stream.Write(PChar(sizeInfo)^, Length(sizeInfo));
    Stream.Write(c, SizeOf(c)); // write delimeter
    s := IntToStr(255);
    Stream.Write(PChar(s)^, Length(s));
    Stream.Write(c, SizeOf(c)); // write delimeter
    if ppmType = ppmBinary then for i := 0 to aBitmap.Height - 1 do
    begin
      P1 := aBitmap.ScanLine[i];
      for j := 0 to (aBitmap.Width - 1) do
      begin
        buf[j*3] := P1[j*3+2];
        buf[j*3+1] := P1[j*3+1];
        buf[j*3+2] := P1[j*3];
      end;
      Stream.Write(buf, aBitmap.Width * 3);
    end
    else
      for i := 0 to aBitmap.Height - 1 do
      begin
        P1 := aBitmap.ScanLine[i];
        for j := 0 to (aBitmap.Width - 1) do
        begin
          s := IntToStr(P1[j*3+2]) + ' ';
          Stream.Write(PChar(s)^, Length(s));
          s := IntToStr(P1[j*3+1]) + ' ';
          Stream.Write(PChar(s)^, Length(s));
          s := IntToStr(P1[j*3]) + ' ';
          Stream.Write(PChar(s)^, Length(s));
        end;
      end;
  finally
    aBitmap.Free;
  end;
end;

initialization
  { Register the TPPMBitmap as a new graphic file format
    now all the TPicture storage stuff can access our new
    PPM graphic format !
  }
  TPicture.RegisterFileFormat('PPM','Portable Pixelmap', TPPMBitmap);

finalization
  TPicture.UnregisterGraphicClass(TPPMBitmap);

end.
