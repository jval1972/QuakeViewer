unit xStubGraphic;

{$P+,S-,W-,R-,T-,X+,H+}
{$C PRELOAD}

interface

uses
  Windows, Forms, SysUtils, Classes, Graphics;

type

  TStubBitmap = class(TBitmap)
  private
    procedure WriteStubStreamData(Stream: TStream);
    procedure ReadStubStreamData(Stream: TStream);
    procedure CreateStubGraphic;
  protected
    procedure WriteData(Stream: TStream); override;
    procedure ReadData(Stream: TStream); override;
  public
    constructor Create; override;
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromStream(Stream: TStream); override;
  end;

  TStubPAKBitmap = class(TStubBitmap);
  TStubWADBitmap = class(TStubBitmap);
  TStubBSPBitmap = class(TStubBitmap);
  TStubGRPBitmap = class(TStubBitmap);
  TStubPK3Bitmap = class(TStubBitmap);

implementation

{ TStubBitmap }

constructor TStubBitmap.Create;
begin
  Inherited;
  CreateStubGraphic;
end;

procedure TStubBitmap.WriteData(Stream: TStream);
begin
  WriteStubStreamData(Stream);
end;

procedure TStubBitmap.SaveToStream(Stream: TStream);
begin
  WriteStubStreamData(Stream);
end;

procedure TStubBitmap.LoadFromStream(Stream: TStream);
begin
  ReadStubStreamData(Stream);
end;

procedure TStubBitmap.ReadData(Stream: TStream);
begin
  ReadStubStreamData(Stream);
end;

procedure TStubBitmap.ReadStubStreamData(Stream: TStream);
begin
  CreateStubGraphic;
end;

procedure TStubBitmap.CreateStubGraphic;
var
  aBitmap : TBitmap;
begin
  aBitmap := TBitmap.Create;
  aBitmap.Width := 16;
  aBitmap.Height := 16;
  aBitmap.PixelFormat := pf4bit;
  aBitmap.Canvas.Pen.Width := 1;
  aBitmap.Canvas.Pen.Color := clGray;
  aBitmap.Canvas.Brush.Color := clWhite;
  aBitmap.Canvas.Rectangle(0, 0, 16, 16);
  Assign(aBitmap);
  aBitmap.Free;
end;

procedure TStubBitmap.WriteStubStreamData(Stream: TStream);
begin
end;

initialization
  { Register the TStubBitmap as a new graphic file format
    now all the TPicture storage stuff can access our new
    Stub graphic format !
  }
  TPicture.RegisterFileFormat('pak','Stub PAK Graphic', TStubPAKBitmap);
  TPicture.RegisterFileFormat('wad','Stub WAD Graphic', TStubWADBitmap);
  TPicture.RegisterFileFormat('bsp','Stub BSP Graphic', TStubBSPBitmap);
  TPicture.RegisterFileFormat('grp','Stub GRP Graphic', TStubGRPBitmap);
  TPicture.RegisterFileFormat('pk3','Stub PK3 Graphic', TStubPK3Bitmap);

finalization
  TPicture.UnregisterGraphicClass(TStubPAKBitmap);
  TPicture.UnregisterGraphicClass(TStubWADBitmap);
  TPicture.UnregisterGraphicClass(TStubBSPBitmap);
  TPicture.UnregisterGraphicClass(TStubGRPBitmap);
  TPicture.UnregisterGraphicClass(TStubPK3Bitmap);

end.
