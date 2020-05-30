unit pcximage;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls;

type
  pRGBArray = ^TRGBArray;
  TRGBArray = array[0..$FFFF] of TRGBTriple;

type
  palent = array[0..0] of TPaletteEntry;

  TPCXImage = class(TGraphic)
  private
    FBitmap: TBitmap;
    buf: array[0..128] of byte;
    ibuf2: pRGBArray;
    ibuf: array[0..$FFFF] of byte;
    ibuftmp: PByteArray;
    rpal: TMaxLogPalette;
    wpal: array[0..255] of PALETTEENTRY;
    function RLECompress(Stream: TStream): TStream;
    function RLEDecompress(Stream: TStream): TStream;
  public
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    constructor Create; override;
    destructor Destroy; override;
    procedure Draw(ACanvas: TCanvas; const ARect: TRect); override;
    function Equals(Graphic: TGraphic): Boolean; override;
    function GetBitmap: TBitmap;
    function GetEmpty: Boolean; override;
    function GetHeight: Integer; override;
    function GetPalette: HPALETTE; override;
    function GetPixelFormat: TPixelFormat;
    function GetTransparent: Boolean; override;
    function GetWidth: Integer; override;
    function HasBitmap: boolean;
    procedure LoadFromClipboardFormat(AFormat: Word; AData: THandle; APalette: HPALETTE); override;
    procedure LoadFromFile(const FileName: string); override;
    procedure LoadFromResourceID(Instance: THandle; ResID: Integer; ResType: PChar);
    procedure LoadFromResourceName(Instance: THandle; const ResName: string; ResType: PChar);
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToClipboardFormat(var AFormat: Word; var AData: THandle; var APalette: HPALETTE); override;
    procedure SaveToFile(const FileName: string); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure SetHeight(Value: integer); override;
    procedure SetWidth(Value: integer); override;
    procedure WriteData(Stream: TStream); override;
  end;

  EPcxError = class(Exception)
  end;

implementation

resourcestring
  RC_PcxUnknowFormat = 'PCX : unknow format';
  RC_PcxPaletteProblem = 'PCX : Unable to retrieve palette !';
  RC_PcxInvalid = 'PCX : This isn''t a valid PCX image';

  RC_PcxExtension = 'pcx';
  RC_PcxFilterName = 'PCX Image';

procedure TPCXImage.Assign(Source: TPersistent);
var
  ts: TStream;
begin
  if Source = nil then
  begin
    if FBitmap <> nil then
      FBitmap.free;
  end
  else if (Source is TPCXImage) and (Source <> Self) then
  begin
    if FBitmap <> nil then
      FBitmap.free;
    FBitmap := TBitmap.Create;

    ts := TMemoryStream.Create;
    TPCXImage(Source).SaveToStream(ts);
    ts.Position := 0;
    LoadFromStream(ts);
  end
  else if Source is TBitmap then
  begin
    if FBitmap <> nil then
      FBitmap.free;
    FBitmap := TBitmap.Create;
    FBitmap.Assign(TBitmap(Source));
  end
  else
    inherited Assign(Source);
end;

procedure TPCXImage.AssignTo(Dest: TPersistent);
begin
  if (Dest is TPCXImage) then
    Dest.Assign(Self)
  else if (Dest is TGraphic) then
  begin
    if Empty then
      Dest.assign(nil)
    else
      (Dest as TGraphic).Assign(FBitmap)
  end
  else
    inherited AssignTo(Dest);
end;

constructor TPCXImage.Create;
begin
  inherited Create;
  FBitmap := TBitmap.Create;
  FBitmap.width := 0;
  FBitmap.height := 0;
  FBitmap.Palette := 0;
end;

destructor TPCXImage.Destroy;
begin
  if FBitmap <> nil then
  begin
    if FBitmap.Palette <> 0 then
      DeleteObject(FBitmap.Palette);
    FBitmap.free;
  end;
  inherited Destroy;
end;

procedure TPCXImage.Draw(ACanvas: TCanvas; const ARect: TRect);
begin
  if FBitmap <> nil then
    ACanvas.StretchDraw(ARect, FBitmap);
end;

function TPCXImage.Equals(Graphic: TGraphic): Boolean;
begin
  if FBitmap = nil then
    result := Graphic = nil
  else
    Result := (Graphic is TPCXImage) and (FBitmap = TPCXImage(TPCXImage).FBitmap)
end;

function TPCXImage.GetBitmap: TBitmap;
begin
  Result := FBitmap;
end;

function TPCXImage.GetEmpty: Boolean;
begin
  result := FBitmap = nil;
end;

function TPCXImage.GetHeight: Integer;
begin
  if FBitmap <> nil then
    result := FBitmap.height
  else
    result := -1;
end;

function TPCXImage.GetPalette: HPALETTE;
begin
  if FBitmap <> nil then
    result := FBitmap.Palette
  else
    result := 0;
end;

function TPCXImage.GetPixelFormat: TPixelFormat;
begin
  if FBitmap <> nil then
    result := FBitmap.PixelFormat
  else
    result := pfCustom;
end;

function TPCXImage.GetTransparent: Boolean;
begin
  if FBitmap <> nil then
    result := FBitmap.transparent
  else
    result := false;
end;

function TPCXImage.GetWidth: Integer;
begin
  if FBitmap <> nil then
    result := FBitmap.width
  else
    result := -1;
end;

function TPCXImage.HasBitmap: boolean;
begin
  result := FBItmap <> nil;
end;

procedure TPCXImage.LoadFromClipboardFormat(AFormat: Word; AData: THandle; APalette: HPALETTE);
begin
  if FBitmap <> nil then
    FBitmap.LoadFromClipboardFormat(AFormat, AData, APalette);
end;

procedure TPCXImage.LoadFromFile(const FileName: string);
var
  t: TFileStream;
begin
  t := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  self.LoadFromStream(t);
  t.free;
end;

procedure TPCXImage.LoadFromResourceID(Instance: THandle; ResID: Integer; ResType: PChar);
var
  Stream: TStream;
begin
  Stream := TResourceStream.CreateFromID(Instance, ResId, ResType);
  Self.LoadFromStream(Stream);
  Stream.free;
end;

procedure TPCXImage.LoadFromResourceName(Instance: THandle;
  const ResName: string; ResType: PChar);
var
  Stream: TStream;
begin
  Stream := TResourceStream.Create(Instance, ResName, ResType);
  Self.LoadFromStream(Stream);
  Stream.free;
end;

function TPCXImage.RLECompress(Stream: TStream): TStream;
var
  count, count2, count3, i: integer;
  buf: array[0..1024] of byte;
  buf2: array[0..64000] of byte;
  b: byte;
begin
  result := TMemoryStream.Create;

  count := 1024;
  while count = 1024 do
  begin
    count := Stream.read(buf, 1024);
    count2 := 0;
    i := 0;
    while i < count do
    begin
      b := buf[i];
      count3 := 0;
      while (buf[i] = b) and (i < count) and (count3 < $30) do
      begin
        inc(i);
        inc(count3);
      end;
      if (i = count) and (count3 in [2..$2F]) and (count = 1024) then
        Stream.position := Stream.position - count3
      else
      begin
        if count3 = 1 then
        begin
          if (b and $C0) = $C0 then
          begin
            buf2[count2] := $C1;
            buf2[count2 + 1] := b;
            inc(count2, 2);
          end
          else
          begin
            buf2[count2] := b;
            inc(count2);
          end;
        end
        else
        begin
          buf2[count2] := count3 or $C0;
          buf2[count2 + 1] := b;
          inc(count2, 2);
        end;
      end;
    end;
    result.Write(buf2, count2);
  end;

  result.position := 0;
end;

function TPCXImage.RLEDecompress(Stream: TStream): TStream;
var
  count, count2, count3, i: integer;
  buf: array[0..1024] of byte;
  buf2: array[0..64000] of byte;
  b: byte;
begin
  result := TMemoryStream.Create;

  count := 1024;
  while count = 1024 do
  begin
    count := Stream.Read(buf, 1024);
    count2 := 0;
    i := 0;
    while i < count do
    begin
      if (buf[i] and $C0) = $C0 then
      begin
        if (i = count - 1) then
          Stream.Position := Stream.Position - 1
        else
        begin
          b := buf[i] and $3F;
          inc(i);
          for count3 := count2 to count2 + b - 1 do
            buf2[count3] := buf[i];
          count2 := count2 + b;
        end;
      end
      else
      begin
        buf2[count2] := buf[i];
        inc(count2);
      end;
      inc(i);
    end;
    result.write(buf2, count2);
  end;

  result.Position := 0;
end;

procedure TPCXImage.LoadFromStream(Stream: TStream);
var
  i, j, k, l, m: Integer;
  w1, w2, w3, w4: word;
  BytesPerLine: Integer;
  bpp, planes: Integer;
  decompressed: TStream;
begin
  if FBitmap <> nil then
    FBitmap := TBitmap.Create;

  i := Stream.Read(buf, 128);

  FBitmap.width := 0;
  FBitmap.height := 0;
  if i = 128 then
  begin
    if buf[0] = 10 then
    begin
      w1 := buf[4] + buf[5] * 256;
      w2 := buf[6] + buf[7] * 256;
      w3 := buf[8] + buf[9] * 256;
      w4 := buf[10] + buf[11] * 256;
      FBitmap.Width := (w3 - w1) + 1;
      FBitmap.Height := (w4 - w2) + 1;
      FBItmap.Canvas.FloodFill(0, 0, clWhite, fsSurface);
      BytesPerLine := (Buf[66] + Buf[67] * 256) * buf[65];
      l := FBitmap.width * 2;
      m := FBitmap.width;
      bpp := buf[3];
      planes := buf[65];
      case bpp of
        1: case planes of
            1:
              begin
                FBitmap.PixelFormat := pf1bit;
                FBitmap.Monochrome := true;
                FBitmap.IgnorePalette := true;
                FBitmap.Palette := 0;
              end;
            4:
              begin
                FBitmap.PixelFormat := pf4bit;
                FBitmap.Monochrome := false;
                FBitmap.IgnorePalette := false;
                FBitmap.Palette := 0;
              end;
          end;
        8: case planes of
            1:
              begin //256 colors
                FBitmap.PixelFormat := pf8bit;
                Fbitmap.Monochrome := false;
                Fbitmap.IgnorePalette := false;
                FBitmap.Palette := 0;
              end;
            3:
              begin //16 millions colors
                FBitmap.PixelFormat := pf24bit;
                Fbitmap.Monochrome := false;
                Fbitmap.IgnorePalette := true;
                FBitmap.Palette := 0;
              end;
          else
            raise EPcxError.Create(RC_PcxUnknowFormat);
          end;
      else
        raise EPcxError.Create(RC_PcxUnknowFormat);
      end;

      decompressed := TMemoryStream.Create;
      decompressed.CopyFrom(stream, stream.Size - stream.position);
      decompressed.Position := 0;
      decompressed := RLEDecompress(decompressed);

      if (bpp = 1) and (planes = 1) then
      begin
        //monochrome okey
        for i := 0 to FBitmap.height - 1 do
        begin
          ibuftmp := Fbitmap.ScanLine[i];
          decompressed.read(ibuf, BytesPerLine);
          CopyMemory(ibuftmp, @ibuf, BytesPerLine);
        end;
      end
      else if (bpp = 1) and (planes = 4) then
      begin
        //16 color palette
        stream.position := 16;
        if stream.read(ibuf, 48) <> 48 then
          raise EPcxError.Create(RC_PcxPaletteProblem)
        else
        begin
          rpal.palVersion := $300;
          rpal.palNumEntries := 16;
          for m := 0 to 15 do
          begin
            i := m * 3;
            rpal.palPalEntry[m].peRed := ibuf[i];
            rpal.palPalEntry[m].peGreen := ibuf[i + 1];
            rpal.palPalEntry[m].peBlue := ibuf[i + 2];
            rpal.palPalEntry[m].peFlags := 0;
          end;
          FBitmap.Palette := CreatePalette(tagLogPalette((@rpal)^));
          PaletteModified := True;
        end;

        //Reading data
        for i := 0 to FBitmap.height - 1 do
        begin
          decompressed.read(ibuf, BytesPerLine);
          ibuftmp := FBitmap.scanline[i];
          fillChar(ibuftmp^, (FBitmap.Width div 2) + 1, 0);

          l := 0;
          for k := 0 to FBitmap.Width - 1 do
            if ((ibuf[l + k div 8]) and (1 shl (7 - (k mod 8)))) <> 0 then
              if k mod 2 <> 0 then
                ibuftmp^[k div 2] := ibuftmp^[k div 2] + $01
              else
                ibuftmp^[k div 2] := ibuftmp^[k div 2] + $10;

          l := BytesPerLine div 4;
          for k := 0 to FBitmap.Width - 1 do
            if ((ibuf[l + k div 8]) and (1 shl (7 - (k mod 8)))) <> 0 then
              if k mod 2 <> 0 then
                ibuftmp^[k div 2] := ibuftmp^[k div 2] + $02
              else
                ibuftmp^[k div 2] := ibuftmp^[k div 2] + $20;

          l := (BytesPerLine div 4) * 2;
          for k := 0 to FBitmap.Width - 1 do
            if ((ibuf[l + k div 8]) and (1 shl (7 - (k mod 8)))) <> 0 then
              if k mod 2 <> 0 then
                ibuftmp^[k div 2] := ibuftmp^[k div 2] + $04
              else
                ibuftmp^[k div 2] := ibuftmp^[k div 2] + $40;

          l := (BytesPerLine div 4) * 3;
          for k := 0 to FBitmap.Width - 1 do
            if ((ibuf[l + k div 8]) and (1 shl (7 - (k mod 8)))) <> 0 then
              if k mod 2 <> 0 then
                ibuftmp^[k div 2] := ibuftmp^[k div 2] + $08
              else
                ibuftmp^[k div 2] := ibuftmp^[k div 2] + $80;
        end;
      end
      else if (bpp = 8) and (Planes = 1) then
      begin
        //256 Colors. Okey
        stream.Position := stream.Size - (256 * 3 + 1);
        stream.Read(ibuf, 1000);
        if ibuf[0] = 12 then
        begin
          rpal.palVersion := $300;
          rpal.palNumEntries := 256;
          for m := 0 to 255 do
          begin
            i := m * 3 + 1;
            rpal.palPalEntry[m].peRed := ibuf[i];
            rpal.palPalEntry[m].peGreen := ibuf[i + 1];
            rpal.palPalEntry[m].peBlue := ibuf[i + 2];
            rpal.palPalEntry[m].peFlags := 0;
          end;
          FBitmap.Palette := CreatePalette(tagLogPalette((@rpal)^));
          PaletteModified := true;
          FBitmap.PaletteModified := true;
          Changed(self);
        end;
        for i := 0 to FBitmap.height - 1 do
        begin
          ibuftmp := Fbitmap.ScanLine[i];
          decompressed.read(ibuf, BytesPerLine);
          CopyMemory(ibuftmp, @ibuf, BytesPerLine);
        end;
      end
      else if (bpp = 8) and (Planes = 3) then
      begin
        //24 bit. Okey
        for i := 0 to FBitmap.height - 1 do
        begin
          ibuf2 := Fbitmap.ScanLine[i];
          decompressed.read(ibuf, BytesPerLine);
          for j := 0 to FBitmap.width - 1 do
          begin
            ibuf2[j].rgbtRed := ibuf[j];
            ibuf2[j].rgbtGreen := ibuf[j + m];
            ibuf2[j].rgbtBlue := ibuf[j + l];
          end;
        end;
      end;
      decompressed.free;
    end
    else
      raise EPcxError.Create(RC_PcxInvalid);
  end
  else
    raise EPcxError.Create(RC_PcxInvalid);

  PaletteModified := true;
  Changed(self);
end;

procedure TPCXImage.SaveToClipboardFormat(var AFormat: Word;
  var AData: THandle; var APalette: HPALETTE);
begin
  if FBitmap <> nil then
    Fbitmap.SaveToClipboardFormat(AFormat, Adata, APalette);
end;

procedure TPCXImage.SaveToFile(const FileName: string);
var
  stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate or fmShareExclusive);
  SaveToStream(Stream);
  Stream.Free;
end;

procedure TPCXImage.SaveToStream(Stream: TStream);
var
  i, j, k, l: Integer;
  bytesperline: word;
  st: TStream;
  b: byte;
begin
  if FBitmap = nil then
    Exit;
  if (FBitmap.PixelFormat <> pf1Bit) and (FBitmap.PixelFormat <> pf4Bit) and
     (FBitmap.PixelFormat <> pf8Bit) and (FBitmap.PixelFormat <> pf24Bit) then
    FBItmap.pixelformat := pf24bit;

  //St -> temp stream to put the data
  st := TMemoryStream.Create;

  // header
  FillChar(ibuf, 128, 0);
  ibuf[0] := $A;
  ibuf[1] := 5;
  ibuf[2] := 1;
  ibuf[8] := lo(FBitmap.Width) - 1;
  ibuf[9] := hi(FBitmap.width);
  ibuf[10] := lo(FBitmap.height) - 1;
  ibuf[11] := hi(FBitmap.height);
  ibuf[12] := 1;
  ibuf[13] := 300 - 256;
  ibuf[14] := 1;
  ibuf[15] := 300 - 256;
  ibuf[69] := 1;
  ibuf[70] := hi(screen.height);
  ibuf[71] := lo(screen.height);
  ibuf[72] := hi(screen.Width);
  ibuf[73] := lo(screen.Width);

  case FBitmap.PixelFormat of
    pf1bit:
      begin
        ibuf[3] := 1;
        ibuf[65] := 1;
        BytesPerLine := FBitmap.width div 8;
        if (FBitmap.Width mod 8 <> 0) then
          inc(BytesPerLine);
        ibuf[66] := lo(BytesPerLine);
        ibuf[67] := hi(BytesPerLine);
        stream.Write(ibuf, 128);

        //Write data
        for i := 0 to FBitmap.height - 1 do
        begin
          ibuftmp := Fbitmap.ScanLine[i];
          CopyMemory(@ibuf, ibuftmp, BytesPerLine);
          st.write(ibuf, BytesPerLine);
        end;
      end;
    pf4bit:
      begin
        ibuf[3] := 1;
        ibuf[65] := 4;
        BytesPerLine := FBitmap.width div 8;
        if (FBitmap.Width mod 8 <> 0) then
          inc(BytesPerLine);
        i := BytesPerLine;
        BytesPerLine := BytesPerLine * 4;
        ibuf[66] := lo(i);
        ibuf[67] := hi(i);

        //Write palette
        if (FBitmap.Palette <> 0) then
        begin
          GetPaletteEntries(FBitmap.Palette, 0, 16, wpal);
          for i := 0 to 15 do
          begin
            ibuf[16 + i * 3] := wpal[i].peRed;
            ibuf[16 + i * 3 + 1] := wpal[i].peGreen;
            ibuf[16 + i * 3 + 2] := wpal[i].peBlue;
          end;
        end;
        stream.Write(ibuf, 128);

        //Write data
        for i := 0 to FBitmap.height - 1 do
        begin
          ibuftmp := FBitmap.Scanline[i];
          fillchar(ibuf, BytesPerLine, 0);

          //Red
          l := 0;
          for j := 0 to FBitmap.Width - 1 do
            if (j mod 2) = 0 then
            begin
              if (ibuftmp^[j div 2] and $10) <> 0 then
                ibuf[l + j div 8] := ibuf[l + j div 8] + (1 shl (7 - (j mod 8)));
            end
            else
            begin
              if (ibuftmp^[j div 2] and $01) <> 0 then
                ibuf[l + j div 8] := ibuf[l + j div 8] + (1 shl (7 - (j mod 8)));
            end;

          //Green
          l := BytesPerLine div 4;
          for j := 0 to FBitmap.Width - 1 do
            if (j mod 2) = 0 then
            begin
              if (ibuftmp^[j div 2] and $20) <> 0 then
                ibuf[l + j div 8] := ibuf[l + j div 8] + (1 shl (7 - (j mod 8)));
            end
            else
            begin
              if (ibuftmp^[j div 2] and $02) <> 0 then
                ibuf[l + j div 8] := ibuf[l + j div 8] + (1 shl (7 - (j mod 8)));
            end;

          //Blue
          l := BytesPerLine div 2;
          for j := 0 to FBitmap.Width - 1 do
            if (j mod 2) = 0 then
            begin
              if (ibuftmp^[j div 2] and $40) <> 0 then
                ibuf[l + j div 8] := ibuf[l + j div 8] + (1 shl (7 - (j mod 8)));
            end
            else
            begin
              if (ibuftmp^[j div 2] and $04) <> 0 then
                ibuf[l + j div 8] := ibuf[l + j div 8] + (1 shl (7 - (j mod 8)));
            end;

          //Intensity
          l := (BytesPerLine div 4) * 3;
          for j := 0 to FBitmap.Width - 1 do
            if (j mod 2) = 0 then
            begin
              if (ibuftmp^[j div 2] and $80) <> 0 then
                ibuf[l + j div 8] := ibuf[l + j div 8] + (1 shl (7 - (j mod 8)));
            end
            else
            begin
              if (ibuftmp^[j div 2] and $08) <> 0 then
                ibuf[l + j div 8] := ibuf[l + j div 8] + (1 shl (7 - (j mod 8)));
            end;

          st.Write(ibuf, BytesPerLine);
        end;
      end;
    pf8bit:
      begin
        ibuf[3] := 8;
        ibuf[65] := 1;
        BytesPerLine := Fbitmap.width;
        ibuf[66] := lo(BytesPerLine);
        ibuf[67] := hi(BytesPerLine);
        stream.Write(ibuf, 128);

        //Write Data
        for i := 0 to FBitmap.height - 1 do
        begin
          ibuftmp := Fbitmap.ScanLine[i];
          CopyMemory(@ibuf, ibuftmp, BytesPerLine);
          st.write(ibuf, BytesPerLine);
        end;
      end;
    pf24bit:
      begin
        ibuf[3] := 8;
        ibuf[65] := 3;
        BytesPerLine := FBitmap.width * 3;
        i := FBitmap.width;
        ibuf[66] := lo(i);
        ibuf[67] := hi(i);
        stream.Write(ibuf, 128);

        //Write data
        for i := 0 to FBitmap.height - 1 do
        begin
          ibuf2 := FBitmap.ScanLine[i];

          for j := 0 to FBitmap.Width - 1 do
            ibuf[j] := ibuf2[j].rgbtRed;

          k := FBitmap.Width;
          for j := 0 to FBitmap.Width - 1 do
            ibuf[j + k] := ibuf2[j].rgbtGreen;

          k := FBitmap.Width * 2;
          for j := 0 to FBitmap.Width - 1 do
            ibuf[j + k] := ibuf2[j].rgbtBlue;

          st.write(ibuf, BytesPerLine);
        end;
      end;
  end;

  //RLE Compress temporary stream
  st.Position := 0;
  st := RLECompress(st);
  //Copy temporary stream to final stream
  st.Position := 0;
  stream.CopyFrom(st, st.size);

  //Write palette if mode 256 color.
  if (FBitmap.PixelFormat = pf8bit) and (FBitmap.Palette <> 0) then
  begin
    GetPaletteEntries(FBitmap.Palette, 0, 256, wpal);
    for i := 0 to 255 do
    begin
      ibuf[i * 3] := wpal[i].peRed;
      ibuf[i * 3 + 1] := wpal[i].peGreen;
      ibuf[i * 3 + 2] := wpal[i].peBlue;
    end;
    b := 12;
    stream.write(b, 1);
    stream.write(ibuf, 256 * 3);
  end;

  st.free;
end;

procedure TPCXImage.SetHeight(Value: integer);
begin
  if FBitmap <> nil then
  begin
    FBitmap.Height := Value;
    Changed(self);
  end;
end;

procedure TPCXImage.SetWidth(Value: integer);
begin
  if FBitmap <> nil then
  begin
    FBitmap.Width := Value;
    Changed(self);
  end;
end;

procedure TPCXImage.WriteData(Stream: TStream);
begin
  SaveToStream(Stream);
end;

initialization
  Registerclass(TPCXImage);
  TPicture.RegisterFileFormat(RC_PcxExtension, RC_PcxFilterName, TPCXImage);

finalization
  TPicture.UnRegisterGraphicclass(TPCXImage);

end.

