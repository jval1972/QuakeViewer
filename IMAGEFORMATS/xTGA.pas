// Truevision Targa is a raster image format that it is most often used to store
// high-color images. It supports one alpha channel per image. The following source
// code supports only 24bpp and 32bpp uncompressed TGA images.
///////////////////////////////////////////////////
// Author: Jim Valavanis,
// E-Mail: jimmyvalavanis@yahoo.gr
// Site  : http://www.geocities.com/jimmyvalavanis/
///////////////////////////////////////////////////

// 2018:
//  Original version can still be downloaded at my old geocities site at:
//  http://www.geocities.ws/jimmyvalavanis/programming/gformats/tga.html

unit xTGA;

{$P+,S-,W-,R-,T-,X+,H+}
{$C PRELOAD}

interface

uses
  Windows, Forms, SysUtils, Classes, Graphics;

type
  TTGABitmap = class(TBitmap)
  private
    fAllowExceptions: boolean;
    procedure WriteTGAStreamData(Stream: TStream);
    procedure ReadTGAStreamData(Stream: TStream);
  protected
    procedure WriteData(Stream: TStream); override;
    procedure ReadData(Stream: TStream); override;
  public
    constructor Create; override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromStream(Stream: TStream); override;
    property AllowExceptions: boolean read fAllowExceptions write fAllowExceptions;
  end;

resourceString
  rsTGAError = 'Error reading TGA file: Wrong file type.';
  rsErrUnsupported1 = 'Couldn''t load TGA Image. Only 24 and 32bit TGA Images supported.';
  rsErrUnsupported2 = 'Couldn''t load TGA Image. Colormapped TGA images not supported.';
  rsErrUnsupported3 = 'Couldn''t load TGA Image. Only standard 24, 32 bit TGA Images supported.';

implementation

{ TTGABitmap }

type
  TTGAHeader = packed record   // Header type for TGA images
    FileType     : Byte;                // Offset 1
    ColorMapType : Byte;                // Offset 2
    ImageType    : Byte;                // Offset 3
    ColorMapSpec : array[0..4] of Byte; // Offset 4
    OriginX: array [0..1] of Byte;      // Offset 9
    OriginY: array [0..1] of Byte;      // Offset 11
    Width  : array [0..1] of Byte;      // Offset 13
    Height : array [0..1] of Byte;      // Offset 15
    BPP    : Byte;                      // Offset 17
    ImageInfo : Byte;                   // Offset 18
  end;

//OrigX and OrigY show the origin of the image
//TOPLEFT
//BOTTOMLEFT
//BOTTOMRIGHT
//TOPRIGHT

constructor TTGABitmap.Create;
begin
  Inherited Create;
  fAllowExceptions := true;
end;

procedure TTGABitmap.WriteData(Stream: TStream);
begin
  WriteTGAStreamData(Stream);
end;

procedure TTGABitmap.SaveToStream(Stream: TStream);
begin
  WriteTGAStreamData(Stream);
end;

procedure TTGABitmap.LoadFromStream(Stream: TStream);
begin
  ReadTGAStreamData(Stream);
end;

procedure TTGABitmap.ReadData(Stream: TStream);
begin
  ReadTGAStreamData(Stream);
end;

procedure TTGABitmap.ReadTGAStreamData(Stream: TStream);
var
  TGAHeader: TTGAHeader;
  i: integer;
  P1: PByteArray;
  h, w: integer;
//  ox, oy: integer;
  fImage: pointer;
  cImage : pointer;
  ImageSize: integer;
  pImage: pointer;
  ColorDepth: integer;
  BufferIndex : Integer;
  currentByte : Integer;
  CurrentPixel : Integer;
  numPixels: integer;
  Front: PByte;

  procedure MakeStub;
  begin
    pixelFormat := pf8bit;
    Width := 16;
    Height := 16;
    with Canvas do
    begin
      Pen.Width := 1;
      Pen.Color := clGray;
      Brush.Color := clWhite;
      Rectangle(0, 0, 16, 16);
    end;
  end;

begin
  Stream.Read(TGAHeader, SizeOf(TGAHeader));

  if TGAHeader.ColorMapType <> 0 then
  begin
    if fAllowExceptions then
      raise Exception.Create(rsErrUnsupported2)
    else
      MakeStub;
    exit;
  end;

  if not (TGAHeader.BPP in [24, 32]) then
  begin
    if fAllowExceptions then
      raise Exception.Create(rsErrUnsupported1)
    else
      MakeStub;
    exit;
  end;

  // Only support 24, 32 bit images
  if not (TGAHeader.ImageType in [2, 10]) then   // Standard or compressed 24, 32 bit TGA file supported
  begin
    if fAllowExceptions then
      raise Exception.Create(rsErrUnsupported3)
    else
      MakeStub;
    exit;
  end;

  w  := TGAHeader.Width[0]  + TGAHeader.Width[1] * 256;
  h := TGAHeader.Height[0] + TGAHeader.Height[1] * 256;

  if TGAHeader.ImageType = 2 then // Uncompressed
  begin
    Width := w;
    Height := h;
    if TGAHeader.BPP = 24 then
    begin
      PixelFormat := pf24bit;
      w := w * 3;
      for i := h - 1 downto 0 do
      begin
        P1 := Scanline[i];
        Stream.Read(P1^, w);
      end;
    end
    else
    begin
      PixelFormat := pf32bit;
      w := w * 4;
      for i := h - 1 downto 0 do
      begin
        P1 := Scanline[i];
        Stream.Read(P1^, w);
      end;
    end;

  end // Compressed
  else
  begin
    CurrentByte := 0;
    CurrentPixel := 0;
    BufferIndex := 0;
    ColorDepth := TGAHeader.BPP div 8;
    numPixels := w * h;
    ImageSize := numPixels * ColorDepth;
    GetMem(fImage, ImageSize + 1);
    GetMem(cImage, ImageSize + 1);
    Stream.Read(cImage^, ImageSize);

    // Extract pixel information from compressed data
    repeat
      Front := Pointer(Integer(cImage) + BufferIndex);
      Inc(BufferIndex);
      if Front^ < 128 then
      begin
        for i := 0 to Front^ do
        begin
          PInteger(Integer(fImage) + CurrentByte)^ := PInteger(Integer(cImage) + BufferIndex + I * ColorDepth)^;
          CurrentByte := CurrentByte + ColorDepth;
          inc(CurrentPixel);
          if CurrentPixel = numPixels then
            break;
        end;
        BufferIndex := BufferIndex + (Front^ + 1) * ColorDepth
      end
      else
      begin
        for i := 0 to Front^ -128 do
        begin
          PInteger(Integer(fImage) + CurrentByte)^ := PInteger(Integer(cImage) + BufferIndex)^;
          CurrentByte := CurrentByte + ColorDepth;
          inc(CurrentPixel);
          if CurrentPixel = numPixels then
            break;
        end;
        BufferIndex := BufferIndex + ColorDepth
      end;
    until CurrentPixel >= numPixels;

    FreeMem(cImage, ImageSize + 1);

    Width := w;
    Height := h;
    if TGAHeader.BPP = 24 then
    begin
      PixelFormat := pf24bit;
      w := w * 3;
    end
    else
    begin
      PixelFormat := pf32bit;
      w := w * 4;
    end;

    pImage := fImage;
    for i := h - 1 downto 0 do
    begin
      P1 := ScanLine[i];
      Move(pImage^, P1^, w);
      pImage := Pointer(Integer(pImage) + w);
    end;

    FreeMem(fImage, ImageSize + 1);
  end;

{  ox := TGAHeader.OriginX[0]  + TGAHeader.OriginX[1] * 256;
  oy := TGAHeader.OriginY[0] + TGAHeader.OriginY[1] * 256;

  if (ox = 0) and (oy = 0) then
    exit;

  if (ox = 0) and (oy <> 0) then
    Canvas.StretchDraw(Rect(0, Height - 1, Width - 1, 0), self)
  else if (ox <> 0) and (oy = 0) then
    Canvas.StretchDraw(Rect(Width - 1, 0, 0, Height - 1), self)
  else
    Canvas.StretchDraw(Rect(Width - 1, Height - 1, 0, 0), self);}

end;

procedure TTGABitmap.WriteTGAStreamData(Stream: TStream);
var
  aBitmap: TBitmap;
  TGAHeader: TTGAHeader;
  i{,j}: integer;
  P1: PByteArray;
//  b: byte;
begin
  FillChar(TGAHeader, SizeOf(TGAHeader), Chr(0));
  TGAHeader.ImageType := 2;

  aBitmap := TBitmap.Create;
  try
    aBitmap.Assign(self);
    if not (aBitmap.PixelFormat in [pf24bit, pf32bit]) then
      aBitmap.PixelFormat := pf24bit;
    if aBitmap.PixelFormat = pf24bit then
      TGAHeader.BPP := 24
    else
      TGAHeader.BPP := 32;
    TGAHeader.Width[0] := byte(aBitmap.Width);
    TGAHeader.Width[1] := aBitmap.Width shr 8;
    TGAHeader.Height[0] := byte(aBitmap.Height);
    TGAHeader.Height[1] := aBitmap.Height shr 8;
    Stream.Write(TGAHeader, SizeOf(TGAHeader));

    for i := aBitmap.Height - 1 downto 0 do
    begin
      P1 := aBitmap.ScanLine[i];
      if TGAHeader.BPP = 24 then
      begin
{        for j := 0 to aBitmap.Width - 1 do
        begin
          b := P1[j * 3];
          P1[j * 3] := P1[j * 3 + 2];
          P1[j * 3 + 2] := b;
        end;                 }
        Stream.Write(P1^, aBitmap.Width * 3);
      end
      else if TGAHeader.BPP = 32 then
      begin
        // TGAs are stored BGR and not RGB, so swap the R and B bytes.
{        for j := aBitmap.Width - 1 downto 0 do
        begin
          b := P1[j * 4];
          P1[j * 4] := P1[j * 4 + 2];
          P1[j * 4 + 2] := b;
        end;}
        Stream.Write(P1^, aBitmap.Width * 4);
      end;
    end;

  finally
    aBitmap.Free;
  end;
end;

initialization
  { Register the TTGABitmap as a new graphic file format
    now all the TPicture storage stuff can access our new
    TGA graphic format !
  }
  TPicture.RegisterFileFormat('TGA','TGA Image', TTGABitmap);

finalization
  TPicture.UnregisterGraphicClass(TTGABitmap);

end.
