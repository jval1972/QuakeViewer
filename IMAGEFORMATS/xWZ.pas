unit xWZ;

{$P+,S-,W-,R-,T-,X+,H+}
{$C PRELOAD}

interface

uses
  Windows, Forms, SysUtils, Classes, Graphics;

type
  TWZ1Bitmap = class(TBitmap)
  private
    procedure WriteStreamData(Stream: TStream);
    procedure ReadStreamData(Stream: TStream);
  protected
    procedure WriteData(Stream: TStream); override;
    procedure ReadData(Stream: TStream); override;
  public
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromStream(Stream: TStream); override;
  end;

  TWZ2Bitmap = class(TBitmap)
  private
    procedure WriteStreamData(Stream: TStream);
    procedure ReadStreamData(Stream: TStream);
  protected
    procedure WriteData(Stream: TStream); override;
    procedure ReadData(Stream: TStream); override;
  public
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromStream(Stream: TStream); override;
  end;

type
  EWZException = class(Exception)
  end;

implementation

const
  { error constants }
  geNoError         =  0;          { no errors found }
  geNoFile          =  1;          { gif file not found }
  geNotWZ           =  2;          { file is not a WZ? file }
  geUnExpectedEOF   =  3;          { unexpected EOF }

  ErrName: Array[1..3] of string = (
	'WZ file not found',
	'Not a WZ file',
 	'Unexpected EOF');

type
  WZStruct=packed record
    count : byte;
    index : byte;
  end;

  PWZBuffer = ^TWZBuffer;
  TWZBuffer = array[1..$FFFF] of WZStruct;

  TEntry = packed array[0..255] of TPaletteEntry;	{ the color table }

type
  GraphicLine      = packed array [0..2048] of byte;
  PBmLine         = ^TBmpLinesStruct;
  TBmpLinesStruct = packed record
    LineData  : GraphicLine;
    LineNo    : Integer;
  end;
  TImageSize = packed record
    X,Y: SmallInt;
  end;

  TLogPal = record
    lpal : TLogPalette;
    Entries: TEntry;
  end;

type
  { This is the actual gif object }
  PWZ = ^TWZ;
  TWZ = class(TPersistent)
  private
    FStream             : TStream;       { The file stream for the WZ file }
    Entry               : TEntry;	       { Color table }
    LineBuffer          : GraphicLine;   { array for buffer line output }
    ImageSize           : TImageSize;
    {Conversion Routine Vars}
    BmHeader : TBitmapInfoHeader; {File Header for bitmap file}
    ImageLines: TList; {Image data}
    {Member Functions}
    procedure CreateLine(var CurrentY: integer);
    procedure Error(ErrCode: integer);
    procedure CreateBMHeader;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SaveToStream(Stream: TStream);
  end;

  PWZ1 = ^TWZ1;
  TWZ1 = class(TWZ)
  public
    procedure LoadFromStream(Stream: TStream);
  end;

  PWZ2 = ^TWZ2;
  TWZ2 = class(TWZ)
  public
    procedure LoadFromStream(Stream: TStream);
  end;

{ TWZBitmap }

procedure TWZ1Bitmap.WriteData(Stream: TStream);
begin
  WriteStreamData(Stream);
end;

procedure TWZ1Bitmap.SaveToStream(Stream: TStream);
begin
  WriteStreamData(Stream);
end;

procedure TWZ1Bitmap.LoadFromStream(Stream: TStream);
begin
  ReadStreamData(Stream);
end;

procedure TWZ1Bitmap.ReadData(Stream: TStream);
begin
  ReadStreamData(Stream);
end;

procedure TWZ1Bitmap.ReadStreamData(Stream: TStream);
var
  aBitmap : TBitmap;
  aWZ     : TWZ1;
  aStream : TMemoryStream;
begin
  aWZ := TWZ1.Create;
  try
    aWZ.LoadFromStream(Stream);
    aStream:=TMemoryStream.Create;
    aBitmap:=TBitmap.Create;
    try
      aWZ.SaveToStream(aStream);
      aBitmap.LoadFromStream(aStream);
      Assign(aBitmap);
      PixelFormat := pf8bit;
    finally
      aStream.Free;
      aBitmap.Free;
    end;
  finally
    aWZ.Free;
  end;
end;

procedure TWZ1Bitmap.WriteStreamData(Stream: TStream);
var
  aBitmap: TBitmap;
  i,j: integer;
  Token: WZStruct;
  Pal: TLogPal;
  Size: TImageSize;
  pLine: PByteArray;
  f: TMemoryStream;
begin
  aBitmap := TBitmap.Create;
  f := TMemoryStream.Create;
  try
    aBitmap.Assign(self);
    if aBitmap.PixelFormat <> pf8Bit then aBitmap.PixelFormat := pf8Bit;
    Pal.lPal.palVersion:=$300;
    Pal.lPal.palNumEntries:=256;
    GetPaletteEntries(aBitmap.Palette,0,256,Pal.Entries);
    Size.X := aBitmap.Width;
    Size.Y := aBitmap.Height;
    f.Write(Size,SizeOf(TImageSize));
    f.Write(Pal.Entries,SizeOf(TEntry));
    for j:=0 to Size.Y-1 do
    begin
      i := 0;
      pLine := aBitmap.ScanLine[j];
      while i < Size.X do
      begin
        Token.Count := 0;
        Token.Index := pLine[i];
        repeat
          inc(Token.Count);
          inc(i);
        until (i>=Size.X-1) or (Token.Count=255) or (pLine[i]<>Token.Index);
        f.Write(Token,SizeOf(Token));
      end;
    end;
    Stream.CopyFrom(f, 0);
  finally
    aBitmap.Free;
    f.Free;
  end;
end;

procedure TWZ2Bitmap.WriteData(Stream: TStream);
begin
  WriteStreamData(Stream);
end;

procedure TWZ2Bitmap.SaveToStream(Stream: TStream);
begin
  WriteStreamData(Stream);
end;

procedure TWZ2Bitmap.LoadFromStream(Stream: TStream);
begin
  ReadStreamData(Stream);
end;

procedure TWZ2Bitmap.ReadData(Stream: TStream);
begin
  ReadStreamData(Stream);
end;

procedure TWZ2Bitmap.ReadStreamData(Stream: TStream);
var
  aBitmap : TBitmap;
  aWZ     : TWZ2;
  aStream : TMemoryStream;
begin
  aWZ := TWZ2.Create;
  try
    aWZ.LoadFromStream(Stream);
    aStream:=TMemoryStream.Create;
    aBitmap:=TBitmap.Create;
    try
      aWZ.SaveToStream(aStream);
      aBitmap.LoadFromStream(aStream);
      Assign(aBitmap);
      PixelFormat := pf8bit;
    finally
      aStream.Free;
      aBitmap.Free;
    end;
  finally
    aWZ.Free;
  end;
end;

procedure TWZ2Bitmap.WriteStreamData(Stream: TStream);
var
  aBitmap: TBitmap;
  i,j: integer;
  Token: WZStruct;
  Pal: TLogPal;
  Size: TImageSize;
  pLine: PByteArray;
  flags: array[1..3] of boolean;
  f: TMemoryStream;
begin
  aBitmap := TBitmap.Create;
  f := TMemoryStream.Create;
  try
    aBitmap.Assign(self);
    if aBitmap.PixelFormat <> pf8Bit then aBitmap.PixelFormat := pf8Bit;
    Pal.lPal.palVersion:=$300;
    Pal.lPal.palNumEntries:=256;
    GetPaletteEntries(aBitmap.Palette,0,256,Pal.Entries);
    Size.X := aBitmap.Width;
    Size.Y := aBitmap.Height;
    f.Write(Size,SizeOf(TImageSize));
    f.Write(Pal.Entries,SizeOf(TEntry));
    pLine := aBitmap.ScanLine[0];
    Token.Count := 0;
    Token.Index := pLine[0];
    j := 0;
    while j < Size.Y do
    begin
      i := 0;
      while i < Size.X do
      begin
        repeat
          inc(Token.Count);
          inc(i);
        until (pLine[i]<>Token.Index) or (i=Size.X-1) or (Token.Count=255);
        flags[1] := pLine[i]<>Token.Index;
        flags[2] := i=Size.X-1;
        flags[3] := Token.Count=255;
        if flags[1] or flags[3] then
        begin
          f.Write(Token,SizeOf(Token));
          Token.Count := 0;
          Token.Index := pLine[i];
        end;
        if flags[2] and (j<Size.Y-1) then
        begin
          i := 0;
          inc(j);
          pLine := aBitmap.ScanLine[j];
        end;
      end;
      inc(j);
    end;
    Stream.CopyFrom(f, 0);
  finally
    aBitmap.Free;
    f.Free;
  end;
end;

{ TWZ, TWZ1, TWZ2 }

constructor TWZ.Create;
begin
  FStream := nil;
  ImageLines := TList.Create;
end;

destructor TWZ.Destroy;
begin
  ImageLines.Free;
  inherited Destroy;
end;

function Min(x,y: longint): longint;
begin
  if x < y then Min := x else Min := y;
end;

procedure TWZ.CreateBMHeader;
begin
  with BmHeader do
  begin
    biSize           := Sizeof(TBitmapInfoHeader);
    biWidth          := ImageSize.X;
    biHeight         := ImageSize.Y;
    biPlanes         := 1;            {Arcane and rarely used}
    biBitCount       := 8;            {Hmmm Should this be hardcoded ?}
    biCompression    := BI_RGB;       {Sorry Did not implement compression in this version}
    biSizeImage      := 0;            {Valid since we are not compressing the image}
    biXPelsPerMeter  :=143;           {Rarely used very arcane field}
    biYPelsPerMeter  :=143;           {Ditto}
    biClrUsed        := 0;            {all colors are used}
    biClrImportant   := 0;            {all colors are important}
  end;
end;

procedure TWZ1.LoadFromStream(Stream: TStream);
var
  Buf : TWZBuffer;
  i,j: word;
  NumRead: longint;
  X,Y: integer;
begin
  FStream:=Stream;
  FStream.Read(ImageSize,SizeOf(ImageSize));
  CreateBMHeader;
  ImageLines.Clear;
  ImageLines.Capacity := ImageSize.Y;
  FStream.Read(Entry,SizeOf(Entry));
  if (FStream = nil) then
    Error(geNoFile);
  X := 0;
  Y := 0;
  while FStream.Position < FStream.Size {?} do
  begin
    if (FStream.Size - FStream.Position) >= SizeOf(TWZBuffer) then
      NumRead := SizeOf(TWZBuffer)
    else
      NumRead := (FStream.Size - FStream.Position);
    if NumRead <> 0 then FStream.ReadBuffer(Buf, NumRead);
    for i:=1 to NumRead div 2 do
    begin
      for j := X to Min(X+Buf[i].Count,ImageSize.X) do
        LineBuffer[j] := Buf[i].index;
      if X + Buf[i].Count > ImageSize.X - 1 then
      begin
        CreateLine(Y);
        X := 0
      end
      else
        X := X + Buf[i].Count
    end;
  end;
end;

procedure TWZ2.LoadFromStream(Stream: TStream);
var
  Buf : PWZBuffer;
  i,j: word;
  NumRead: longint;
  X,Y: integer;
begin
  FStream:=Stream;
  if (FStream = nil) then Error(geNoFile);
  FStream.Read(ImageSize,SizeOf(ImageSize));
  CreateBMHeader;
  ImageLines.Clear;
  ImageLines.Capacity := ImageSize.Y;
  FStream.Read(Entry,SizeOf(Entry));
  X := 0;
  Y := 0;
  new(Buf);
  while FStream.Position < FStream.Size {?} do
  begin
    if (FStream.Size - FStream.Position) >= SizeOf(TWZBuffer) then
      NumRead := SizeOf(TWZBuffer)
    else
      NumRead := (FStream.Size - FStream.Position);
    if NumRead <> 0 then FStream.ReadBuffer(Buf^, NumRead);
    for i:=1 to NumRead div 2 do
    begin
      repeat
        for j := X to Min(X+Buf[i].Count,ImageSize.X) do
          LineBuffer[j] := Buf[i].index;
        if X + Buf[i].Count > ImageSize.X -1 then
        begin
          if Y <= ImageSize.Y then
            CreateLine(Y)
          else
          begin
          {$I-}
            dispose(buf);
          {$I+}
            exit
          end;
          Buf[i].Count := Buf[i].Count - ImageSize.X + X + 1;
          X := 0
        end
        else
        begin
          X := X + Buf[i].Count;
          Buf[i].Count := 0
        end
      until Buf[i].Count <= 0;
    end;
  end;
  dispose(Buf);
end;

procedure TWZ.CreateLine(var CurrentY: integer);
var
  p: PBmLine;
begin
  Application.ProcessMessages;
  {Create a new bmp line}
  New(p);
  {Fill in the data}
  p^.LineData := LineBuffer;
  p^.LineNo := CurrentY;
  {Add it to the list of lines}
  ImageLines.Add(p);
  Inc(CurrentY);
end;

{Raise exception with a message}
procedure TWZ.Error(ErrCode: integer);
begin
  raise EWZException.Create(ErrName[ErrCode]);
end;

procedure TWZ.SaveToStream(Stream: TStream);
var
  BitFile: TBitmapFileHeader;
  i: integer;
  Line: integer;
  ch: char;
  p: PBmLine;
  x: integer;
begin
  with BitFile do begin
     bfSize := (3*255) + Sizeof(TBitmapFileHeader) +  {Color map info}
	                 Sizeof(TBitmapInfoHeader) +
  	(ImageSize.X*ImageSize.Y);
     bfReserved1 := 0; {not currently used}
     bfReserved2 := 0; {not currently used}
     bfOffBits := (4*256)+ Sizeof(TBitmapFileHeader)+
                           Sizeof(TBitmapInfoHeader);
  end;
  {Write the file header}
  with Stream do begin
    Position:=0;
    ch:='B';
    Write(ch,1);
    ch:='M';
    Write(ch,1);
    Write(BitFile.bfSize,sizeof(BitFile.bfSize));
    Write(BitFile.bfReserved1,sizeof(BitFile.bfReserved1));
    Write(BitFile.bfReserved2,sizeof(BitFile.bfReserved2));
    Write(BitFile.bfOffBits,sizeof(BitFile.bfOffBits));
    {Write the bitmap image header info}
    Write(BmHeader,sizeof(BmHeader));
    {Write the BGR palete inforamtion to this file}
    for i:= 0 to 255 do
    begin
      Write(Entry[i].peBlue,1);
      Write(Entry[i].peGreen,1);
      Write(Entry[i].peRed,1);
      Write(ch,1);
    end;
    {Init the Line Counter}
    Line := ImageSize.Y;
    {Write out File lines in reverse order}
    while Line >= 0 do
    begin
      {Go through the line list in reverse order looking for the
       current Line. Use reverse order since non interlaced gifs are
       stored top to bottom.  Bmp file need to be written bottom to
       top}
      for i := (ImageLines.Count - 1) downto 0  do
      begin
        p := ImageLines.Items[i];
        if p^.LineNo = Line then
        begin
          x := ImageSize.X;
          Write(p^.LineData, x);
          ch := chr(0);
          while (x and 3) <> 0 do { Pad up to 4-byte boundary with zeroes }
          begin
            Inc(x);
            Write(ch, 1);
          end;
          break;
        end;
      end;
      Dec(Line);
    end;
    Position:=0; { reset mewmory stream}
  end;
end;

{------------------------------------------------------------------------------}

initialization
  { Register the TWZBitmap as a new graphic file format
    now all the TPicture storage stuff can access our new
    WZ graphic format !
  }
  TPicture.RegisterFileFormat('WZ2','WZ2-Format', TWZ2Bitmap);
  TPicture.RegisterFileFormat('WZ1','WZ1-Format', TWZ1Bitmap);

finalization
  TPicture.UnregisterGraphicClass(TWZ2Bitmap);
  TPicture.UnregisterGraphicClass(TWZ1Bitmap);

end.

