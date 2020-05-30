unit DropDownButton;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Graphics, StdCtrls,
  ExtCtrls, CommCtrl, Buttons, Menus;

type
  TDropDownAnchor = (ddAnchorDown, ddAnchorUp, ddAnchorRight);

  TDropChangingEvent = procedure(Sender: TObject; var Allowed: Boolean) of
    object;

  TCustomDropDownButton = class(TGraphicControl)
  protected
    FDown: Boolean;
    FDropDownWidth: Integer;
    FGroupIndex: Integer;
    FGlyph: Pointer;
    FDragging: Boolean;
    FAllowAllUp: Boolean;
    FLayout: TButtonLayout;
    FSpacing: Integer;
    FMargin: Integer;
    FFlat: Boolean;
    FMouseInControl: Boolean;
    FTransparent: Boolean;
    FDropAnchor: TDropDownAnchor;

    FDropDownArrowColor: TColor;
    FDropDownZone: Boolean;
    FDroppedDown: Boolean;
    FState: TButtonState;

    FOnChange,
      FOnDefaultSelect,
      FOnDropChanged: TNotifyEvent;
    FOnDropChanging: TDropChangingEvent;
    procedure GlyphChanged(Sender: TObject);
    procedure UpdateExclusive;
    function GetGlyph: TBitmap;
    procedure SetDropDownArrowColor(Value: TColor);
    procedure SetDropDownWidth(Value: integer);
    procedure SetGlyph(Value: TBitmap);
    function GetNumGlyphs: TNumGlyphs;
    procedure SetNumGlyphs(Value: TNumGlyphs);
    procedure SetDown(Value: Boolean);
    procedure SetFlat(Value: Boolean);
    procedure SetAllowAllUp(Value: Boolean);
    procedure SetGroupIndex(Value: Integer);
    procedure SetLayout(Value: TButtonLayout);
    procedure SetSpacing(Value: Integer);
    procedure SetMargin(Value: Integer);
    procedure SetDropAnchor(Value: TDropDownAnchor);
    procedure UpdateTracking;
    procedure CMEnabledChanged(var Message: TMessage); message
      CM_ENABLEDCHANGED;
    procedure CMButtonPressed(var Message: TMessage); message CM_BUTTONPRESSED;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMSysColorChange(var Message: TMessage); message
      CM_SYSCOLORCHANGE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMLButtonDblClk(var Message: TWMLButtonDown); message
      WM_LBUTTONDBLCLK;

    procedure DrawButtonSeperatorUp(Canvas: TCanvas);
    procedure DrawButtonSeperatorDown(Canvas: TCanvas);
    procedure DrawTriangle(Canvas: TCanvas; Top, Left, Width: Integer);
    procedure SetTransparent(const Value: Boolean);
    function MouseInButton: boolean; virtual;
    procedure SetDroppedDown(const Value: Boolean); virtual;
    procedure DoDefaultEvent; virtual;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    function GetPalette: HPALETTE; override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    procedure Paint; override;
    property DropAnchor: TDropDownAnchor read FDropAnchor write SetDropAnchor
      default ddAnchorDown;
    property Action;
    property AllowAllUp: Boolean read FAllowAllUp write SetAllowAllUp default
      False;
    property Anchors;
    property BiDiMode;
    property Caption;
    property Constraints;
    property Down: Boolean read FDown write SetDown default False;
    property DropDownArrowColor: TColor read FDropDownArrowColor write
      SetDropDownArrowColor default clBlack;
    property DropDownWidth: integer read FDropDownWidth write SetDropDownWidth
      default 14;
    property Enabled;
    property Flat: Boolean read FFlat write SetFlat default False;
    property Font;
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property Layout: TButtonLayout read FLayout write SetLayout default
      blGlyphLeft;
    property Margin: Integer read FMargin write SetMargin default -1;
    property NumGlyphs: TNumGlyphs read GetNumGlyphs write SetNumGlyphs default
      1;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    property Spacing: Integer read FSpacing write SetSpacing default 4;
    property Transparent: Boolean read FTransparent write SetTransparent default
      True;
    property Visible;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClick;
    property OnDblClick;
    property OnDefaultSelect: TNotifyEvent read FOnDefaultSelect write
      FOnDefaultSelect;
    property OnDropChanged: TNotifyEvent read FOnDropChanged write
      FOnDropChanged;
    property OnDropChanging: TDropChangingEvent read FOnDropChanging write
      FOnDropChanging;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Click; override;

    property DroppedDown: Boolean read FDroppedDown write SetDroppedDown;
  end;

  TDropDownButton = class(TCustomDropDownButton)
  published
    property Action;
    property AllowAllUp;
    property Anchors;
    property BiDiMode;
    property Caption;
    property Constraints;
    property Down;
    property DropAnchor;
    property DropDownArrowColor;
    property DropDownWidth;
    property Enabled;
    property Flat;
    property Font;
    property Glyph;
    property GroupIndex;
    property Layout;
    property Margin;
    property NumGlyphs;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    property Spacing;
    property Transparent;
    property Visible;

    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDefaultSelect;
    property OnDropChanged;
    property OnDropChanging;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

  TAllowPopupNotifyEvent =
    procedure(Sender: TObject; var AllowPopup: boolean) of object;

  TCustomMenuButton = class(TCustomDropDownButton)
  private
    FOnBeforePopup: TAllowPopupNotifyEvent;
    FOnAfterPopup: TNotifyEvent;
  protected
    FMenu: TPopupMenu;
    procedure SetDroppedDown(const Value: Boolean); override;
    property Menu: TPopupMenu read FMenu write FMenu;
    property OnAfterPopup: TNotifyEvent read FOnAfterPopup write FOnAfterPopup;
    property OnBeforePopup: TAllowPopupNotifyEvent read FOnBeforePopup write
      FOnBeforePopup;
  end;

  TMenuButton = class(TCustomMenuButton)
  published
    property Menu: TPopupMenu read FMenu write FMenu;
    property Action;
    property AllowAllUp;
    property Anchors;
    property BiDiMode;
    property Caption;
    property Constraints;
    property Down;
    property DropAnchor;
    property DropDownArrowColor;
    property DropDownWidth;
    property Enabled;
    property Flat;
    property Font;
    property Glyph;
    property GroupIndex;
    property Layout;
    property Margin;
    property NumGlyphs;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    property Spacing;
    property Transparent;
    property Visible;

    property OnAfterPopup;
    property OnBeforePopup;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDefaultSelect;
    property OnDropChanged;
    property OnDropChanging;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

  TMenuTool = class(TCustomMenuButton)
  protected
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    property DropDownWidth: integer read FDropDownWidth write SetDropDownWidth
      default -15;
  public
    property Down: Boolean read FDown write SetDown default true;
    constructor Create(AOwner: TComponent); override;
    property Font;
  published
    property Menu;
    property AllowAllUp: Boolean read FAllowAllUp write SetAllowAllUp default
      true;
    property Action;
    property Anchors;
    property BiDiMode;
    property Caption nodefault;
    property Constraints;
    property DropAnchor;
    property Enabled;
    property Flat: Boolean read FFlat write SetFlat default true;
    property Glyph;
    property GroupIndex;
    property Layout;
    property Margin: Integer read FMargin write SetMargin default 4;
    property NumGlyphs;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    property Transparent;
    property Visible;
    property Width default 80;

    property OnAfterPopup;
    property OnBeforePopup;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDefaultSelect;
    property OnDropChanged;
    property OnDropChanging;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;

procedure Register;

implementation

uses
  ActnList, ImgList;

const
  DRAW_BUTTON_UP = 8208;
  DRAW_BUTTON_DOWN = 8720;

var
  DropDownButtonComponents: TStringList;

type
  TGlyphList = class(TImageList)
  private
    FUsed: TBits;
    FCount: Integer;
    function AllocateIndex: Integer;
  public
    constructor CreateSize(AWidth, AHeight: Integer);
    destructor Destroy; override;

    function AddMasked(Image: TBitmap; MaskColor: TColor): Integer;
    procedure Delete(Index: Integer);
    property Count: Integer read FCount;
  end;

  TGlyphCache = class
  private
    FGlyphLists: TList;
  public
    constructor Create;
    destructor Destroy; override;

    function GetList(AWidth, AHeight: Integer): TGlyphList;
    procedure ReturnList(List: TGlyphList);
    function Empty: Boolean;
  end;

  TButtonGlyph = class
  private
    FOriginal: TBitmap;
    FGlyphList: TGlyphList;
    FIndexes: array[TButtonState] of Integer;
    FTransparentColor: TColor;
    FNumGlyphs: TNumGlyphs;
    FOnChange: TNotifyEvent;
    procedure GlyphChanged(Sender: TObject);
    procedure SetGlyph(Value: TBitmap);
    procedure SetNumGlyphs(Value: TNumGlyphs);
    procedure Invalidate;
    function CreateButtonGlyph(State: TButtonState): Integer;
    procedure DrawButtonGlyph(Canvas: TCanvas; const GlyphPos: TPoint;
      State: TButtonState; Transparent: Boolean);
    procedure DrawButtonText(Canvas: TCanvas; const Caption: string;
      TextBounds: TRect; State: TButtonState; BiDiFlags: Longint);
    procedure CalcButtonLayout(Canvas: TCanvas; const Client: TRect;
      const Offset: TPoint; const Caption: string; Layout: TButtonLayout;
      Margin, Spacing: Integer; var GlyphPos: TPoint; var TextBounds: TRect;
      const DropDownWidth: Integer; BiDiFlags: Longint);
  public
    constructor Create;
    destructor Destroy; override;

    function Draw(Canvas: TCanvas; const Client: TRect; const Offset: TPoint;
      const Caption: string; Layout: TButtonLayout; Margin, Spacing: Integer;
      State: TButtonState; Transparent: Boolean;
      const DropDownWidth: Integer; BiDiFlags: Longint): TRect;

    property Glyph: TBitmap read FOriginal write SetGlyph;
    property NumGlyphs: TNumGlyphs read FNumGlyphs write SetNumGlyphs;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

var
  GlyphCache: TGlyphCache;
  ButtonCount: Integer;

  //----------------- TGlyphList ------------------------------------------------

constructor TGlyphList.CreateSize(AWidth, AHeight: Integer);
begin
  inherited CreateSize(AWidth, AHeight);
  FUsed := TBits.Create;
end;

destructor TGlyphList.Destroy;
begin
  FUsed.Free;
  inherited Destroy;
end;

function TGlyphList.AllocateIndex: Integer;
begin
  Result := FUsed.OpenBit;
  if Result >= FUsed.Size then
  begin
    Result := inherited Add(nil, nil);
    FUsed.Size := Result + 1;
  end;
  FUsed[Result] := True;
end;

function TGlyphList.AddMasked(Image: TBitmap; MaskColor: TColor): Integer;
begin
  Result := AllocateIndex;
  ReplaceMasked(Result, Image, MaskColor);
  Inc(FCount);
end;

procedure TGlyphList.Delete(Index: Integer);
begin
  if FUsed[Index] then
  begin
    Dec(FCount);
    FUsed[Index] := False;
  end;
end;

//----------------- TGlyphCache -----------------------------------------------

constructor TGlyphCache.Create;
begin
  inherited Create;
  FGlyphLists := TList.Create;
end;

destructor TGlyphCache.Destroy;
begin
  FGlyphLists.Free;
  inherited Destroy;
end;

function TGlyphCache.GetList(AWidth, AHeight: Integer): TGlyphList;
var
  I: Integer;
begin
  for I := FGlyphLists.Count - 1 downto 0 do
  begin
    Result := FGlyphLists[I];
    with Result do
      if (AWidth = Width) and (AHeight = Height) then
        Exit;
  end;
  Result := TGlyphList.CreateSize(AWidth, AHeight);
  FGlyphLists.Add(Result);
end;

procedure TGlyphCache.ReturnList(List: TGlyphList);
begin
  if List = nil then
    Exit;
  if List.Count = 0 then
  begin
    FGlyphLists.Remove(List);
    List.Free;
  end;
end;

function TGlyphCache.Empty: Boolean;
begin
  Result := FGlyphLists.Count = 0;
end;

//----------------- TButtonGlyph ----------------------------------------------

constructor TButtonGlyph.Create;
var
  I: TButtonState;
begin
  inherited Create;
  FOriginal := TBitmap.Create;
  FOriginal.OnChange := GlyphChanged;
  FTransparentColor := clOlive;
  FNumGlyphs := 1;
  for I := Low(I) to High(I) do
    FIndexes[I] := -1;
  if GlyphCache = nil then
    GlyphCache := TGlyphCache.Create;
end;

destructor TButtonGlyph.Destroy;
begin
  FOriginal.Free;
  Invalidate;
  if Assigned(GlyphCache) and GlyphCache.Empty then
  begin
    GlyphCache.Free;
    GlyphCache := nil;
  end;
  inherited Destroy;
end;

procedure TButtonGlyph.Invalidate;
var
  I: TButtonState;
begin
  for I := Low(I) to High(I) do
  begin
    if FIndexes[I] <> -1 then
      FGlyphList.Delete(FIndexes[I]);
    FIndexes[I] := -1;
  end;
  GlyphCache.ReturnList(FGlyphList);
  FGlyphList := nil;
end;

procedure TButtonGlyph.GlyphChanged(Sender: TObject);
begin
  if Sender = FOriginal then
  begin
    FTransparentColor := FOriginal.TransparentColor;
    Invalidate;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TButtonGlyph.SetGlyph(Value: TBitmap);
var
  Glyphs: Integer;
begin
  Invalidate;
  FOriginal.Assign(Value);
  if (Value <> nil) and (Value.Height > 0) then
  begin
    FTransparentColor := Value.TransparentColor;
    if Value.Width mod Value.Height = 0 then
    begin
      Glyphs := Value.Width div Value.Height;
      if Glyphs > 4 then
        Glyphs := 1;
      SetNumGlyphs(Glyphs);
    end;
  end;
end;

procedure TButtonGlyph.SetNumGlyphs(Value: TNumGlyphs);
begin
  if (Value <> FNumGlyphs) and (Value > 0) then
  begin
    Invalidate;
    FNumGlyphs := Value;
    GlyphChanged(Glyph);
  end;
end;

function TButtonGlyph.CreateButtonGlyph(State: TButtonState): Integer;
const
  ROP_DSPDxax = $00E20746;
var
  TmpImage, DDB, MonoBmp: TBitmap;
  IWidth, IHeight: Integer;
  IRect, ORect: TRect;
  I: TButtonState;
  DestDC: HDC;
begin
  if (State = bsDown) and (NumGlyphs < 3) then
    State := bsUp;
  Result := FIndexes[State];
  if Result <> -1 then
    Exit;
  if (FOriginal.Width or FOriginal.Height) = 0 then
    Exit;

  IWidth := FOriginal.Width div FNumGlyphs;
  IHeight := FOriginal.Height;
  if FGlyphList = nil then
  begin
    if GlyphCache = nil then
      GlyphCache := TGlyphCache.Create;
    FGlyphList := GlyphCache.GetList(IWidth, IHeight);
  end;
  TmpImage := TBitmap.Create;
  try
    TmpImage.Width := IWidth;
    TmpImage.Height := IHeight;
    IRect := Rect(0, 0, IWidth, IHeight);
    TmpImage.Canvas.Brush.Color := clBtnFace;
    TmpImage.Palette := CopyPalette(FOriginal.Palette);
    I := State;
    if Ord(I) >= NumGlyphs then
      I := bsUp;
    ORect := Rect(Ord(I) * IWidth, 0, (Ord(I) + 1) * IWidth, IHeight);
    case State of
      bsUp, bsDown,
        bsExclusive:
        begin
          TmpImage.Canvas.CopyRect(IRect, FOriginal.Canvas, ORect);
          if FOriginal.TransparentMode = tmFixed then
            FIndexes[State] := FGlyphList.AddMasked(TmpImage, FTransparentColor)
          else
            FIndexes[State] := FGlyphList.AddMasked(TmpImage, clDefault);
        end;
      bsDisabled:
        begin
          MonoBmp := nil;
          DDB := nil;
          try
            MonoBmp := TBitmap.Create;
            DDB := TBitmap.Create;
            DDB.Assign(FOriginal);
            DDB.HandleType := bmDDB;
            if NumGlyphs > 1 then
              with TmpImage.Canvas do
              begin
                // Change white & gray to clBtnHighlight and clBtnShadow
                CopyRect(IRect, DDB.Canvas, ORect);
                MonoBmp.Monochrome := True;
                MonoBmp.Width := IWidth;
                MonoBmp.Height := IHeight;

                // Convert white to clBtnHighlight
                DDB.Canvas.Brush.Color := clWhite;
                MonoBmp.Canvas.CopyRect(IRect, DDB.Canvas, ORect);
                Brush.Color := clBtnHighlight;
                DestDC := Handle;
                SetTextColor(DestDC, clBlack);
                SetBkColor(DestDC, clWhite);
                BitBlt(DestDC, 0, 0, IWidth, IHeight, MonoBmp.Canvas.Handle, 0,
                  0, ROP_DSPDxax);

                // Convert gray to clBtnShadow
                DDB.Canvas.Brush.Color := clGray;
                MonoBmp.Canvas.CopyRect(IRect, DDB.Canvas, ORect);
                Brush.Color := clBtnShadow;
                DestDC := Handle;
                SetTextColor(DestDC, clBlack);
                SetBkColor(DestDC, clWhite);
                BitBlt(DestDC, 0, 0, IWidth, IHeight, MonoBmp.Canvas.Handle, 0,
                  0, ROP_DSPDxax);

                // Convert transparent color to clBtnFace
                DDB.Canvas.Brush.Color := ColorToRGB(FTransparentColor);
                MonoBmp.Canvas.CopyRect(IRect, DDB.Canvas, ORect);
                Brush.Color := clBtnFace;
                DestDC := Handle;
                SetTextColor(DestDC, clBlack);
                SetBkColor(DestDC, clWhite);
                BitBlt(DestDC, 0, 0, IWidth, IHeight, MonoBmp.Canvas.Handle, 0,
                  0, ROP_DSPDxax);
              end
            else
            begin
              // Create a disabled version
              with MonoBmp do
              begin
                Assign(FOriginal);
                HandleType := bmDDB;
                Canvas.Brush.Color := clBlack;
                Width := IWidth;
                if Monochrome then
                begin
                  Canvas.Font.Color := clWhite;
                  Monochrome := False;
                  Canvas.Brush.Color := clWhite;
                end;
                Monochrome := True;
              end;

              with TmpImage.Canvas do
              begin
                Brush.Color := clBtnFace;
                FillRect(IRect);
                Brush.Color := clBtnHighlight;
                SetTextColor(Handle, clBlack);
                SetBkColor(Handle, clWhite);
                BitBlt(Handle, 1, 1, IWidth, IHeight, MonoBmp.Canvas.Handle, 0,
                  0, ROP_DSPDxax);
                Brush.Color := clBtnShadow;
                SetTextColor(Handle, clBlack);
                SetBkColor(Handle, clWhite);
                BitBlt(Handle, 0, 0, IWidth, IHeight, MonoBmp.Canvas.Handle, 0,
                  0, ROP_DSPDxax);
              end;
            end;
          finally
            DDB.Free;
            MonoBmp.Free;
          end;
          FIndexes[State] := FGlyphList.AddMasked(TmpImage, clDefault);
        end;
    end;
  finally
    TmpImage.Free;
  end;
  Result := FIndexes[State];
  FOriginal.Dormant;
end;

procedure TButtonGlyph.DrawButtonGlyph(Canvas: TCanvas; const GlyphPos: TPoint;
  State: TButtonState; Transparent: Boolean);
var
  Index: Integer;
begin
  if Assigned(FOriginal) then
  begin
    if (FOriginal.Width = 0) or (FOriginal.Height = 0) then
      Exit;

    Index := CreateButtonGlyph(State);

    with GlyphPos do
      if Transparent or (State = bsExclusive) then
        ImageList_DrawEx(FGlyphList.Handle, Index, Canvas.Handle, X, Y, 0, 0,
          clNone, clNone, ILD_Transparent)
      else
        ImageList_DrawEx(FGlyphList.Handle, Index, Canvas.Handle, X, Y, 0, 0,
          ColorToRGB(clBtnFace), clNone, ILD_Normal);
  end;
end;

procedure TButtonGlyph.DrawButtonText(Canvas: TCanvas; const Caption: string;
  TextBounds: TRect; State: TButtonState;
  BiDiFlags: Longint);
begin
  with Canvas do
  begin
    Brush.Style := bsClear;
    if State = bsDisabled then
    begin
      OffsetRect(TextBounds, 1, 1);
      Font.Color := clBtnHighlight;
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds, DT_CENTER or
        DT_VCENTER or BiDiFlags);
      OffsetRect(TextBounds, -1, -1);
      Font.Color := clBtnShadow;
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds, DT_CENTER or
        DT_VCENTER or BiDiFlags);
    end
    else
      DrawText(Handle, PChar(Caption), Length(Caption), TextBounds, DT_CENTER or
        DT_VCENTER or BiDiFlags);
  end;
end;

procedure TButtonGlyph.CalcButtonLayout(Canvas: TCanvas; const Client: TRect;
  const Offset: TPoint; const Caption: string; Layout: TButtonLayout; Margin,
  Spacing: Integer; var GlyphPos: TPoint; var TextBounds: TRect;
  const DropDownWidth: Integer; BiDiFlags: Longint);
var
  TextPos: TPoint;
  ClientSize,
  GlyphSize,
  TextSize: TPoint;
  TotalSize: TPoint;
begin
  if (BiDiFlags and DT_RIGHT) = DT_RIGHT then
    if Layout = blGlyphLeft then
      Layout := blGlyphRight
    else if Layout = blGlyphRight then
      Layout := blGlyphLeft;

  // calculate the item sizes
  ClientSize := Point(Client.Right - Client.Left - DropDownWidth, Client.Bottom
    - Client.Top);

  if FOriginal <> nil then
    GlyphSize := Point(FOriginal.Width div FNumGlyphs, FOriginal.Height)
  else
    GlyphSize := Point(0, 0);

  if Length(Caption) > 0 then
  begin
    TextBounds := Rect(0, 0, Client.Right - Client.Left, 0);
    DrawText(Canvas.Handle, PChar(Caption), Length(Caption), TextBounds,
      DT_CALCRECT or BiDiFlags);
    TextSize := Point(TextBounds.Right - TextBounds.Left, TextBounds.Bottom -
      TextBounds.Top);
  end
  else
  begin
    TextBounds := Rect(0, 0, 0, 0);
    TextSize := Point(0, 0);
  end;

  // If the layout has the glyph on the right or the left, then both the
  // text and the glyph are centered vertically.  If the glyph is on the top
  // or the bottom, then both the text and the glyph are centered horizontally.
  if Layout in [blGlyphLeft, blGlyphRight] then
  begin
    GlyphPos.Y := (ClientSize.Y - GlyphSize.Y + 1) div 2;
    TextPos.Y := (ClientSize.Y - TextSize.Y + 1) div 2;
  end
  else
  begin
    GlyphPos.X := (ClientSize.X - GlyphSize.X + 1) div 2;
    TextPos.X := (ClientSize.X - TextSize.X + 1) div 2;
  end;

  // if there is no text or no bitmap, then Spacing is irrelevant
  if (TextSize.X = 0) or (GlyphSize.X = 0) then
    Spacing := 0;

  // adjust Margin and Spacing
  if Margin = -1 then
  begin
    if Spacing = -1 then
    begin
      TotalSize := Point(GlyphSize.X + TextSize.X, GlyphSize.Y + TextSize.Y);
      if Layout in [blGlyphLeft, blGlyphRight] then
        Margin := (ClientSize.X - TotalSize.X) div 3
      else
        Margin := (ClientSize.Y - TotalSize.Y) div 3;
      Spacing := Margin;
    end
    else
    begin
      TotalSize := Point(GlyphSize.X + Spacing + TextSize.X, GlyphSize.Y +
        Spacing + TextSize.Y);
      if Layout in [blGlyphLeft, blGlyphRight] then
        Margin := (ClientSize.X - TotalSize.X + 1) div 2
      else
        Margin := (ClientSize.Y - TotalSize.Y + 1) div 2;
    end;
  end
  else
  begin
    if Spacing = -1 then
    begin
      TotalSize := Point(ClientSize.X - (Margin + GlyphSize.X), ClientSize.Y -
        (Margin + GlyphSize.Y));
      if Layout in [blGlyphLeft, blGlyphRight] then
        Spacing := (TotalSize.X - TextSize.X) div 2
      else
        Spacing := (TotalSize.Y - TextSize.Y) div 2;
    end;
  end;

  case Layout of
    blGlyphLeft:
      begin
        GlyphPos.X := Margin;
        TextPos.X := GlyphPos.X + GlyphSize.X + Spacing;
      end;
    blGlyphRight:
      begin
        GlyphPos.X := ClientSize.X - Margin - GlyphSize.X;
        TextPos.X := GlyphPos.X - Spacing - TextSize.X;
      end;
    blGlyphTop:
      begin
        GlyphPos.Y := Margin;
        TextPos.Y := GlyphPos.Y + GlyphSize.Y + Spacing;
      end;
    blGlyphBottom:
      begin
        GlyphPos.Y := ClientSize.Y - Margin - GlyphSize.Y;
        TextPos.Y := GlyphPos.Y - Spacing - TextSize.Y;
      end;
  end;

  // fixup the result variables
  with GlyphPos do
  begin
    Inc(X, Client.Left + Offset.X);
    Inc(Y, Client.Top + Offset.Y);
  end;
  OffsetRect(TextBounds, TextPos.X + Client.Left + Offset.X, TextPos.Y +
    Client.Top + Offset.X);
end;

function TButtonGlyph.Draw(Canvas: TCanvas; const Client: TRect;
  const Offset: TPoint; const Caption: string; Layout: TButtonLayout;
  Margin, Spacing: Integer; State: TButtonState; Transparent: Boolean;
  const DropDownWidth: Integer; BiDiFlags: Longint): TRect;
var
  GlyphPos: TPoint;
  R: TRect;
begin
  CalcButtonLayout(Canvas, Client, Offset, Caption, Layout, Margin, Spacing,
    GlyphPos, R, DropDownWidth, BidiFlags);
  DrawButtonGlyph(Canvas, GlyphPos, State, Transparent);
  DrawButtonText(Canvas, Caption, R, State, BiDiFlags);

  // return a rectangle wherein the color indicator can be drawn
  if Caption = '' then
  begin
    Result := Client;
    Dec(Result.Right, DropDownWidth + 2);
    InflateRect(Result, -2, -2);

    // consider glyph if no text is to be painted (else it is already taken into account)
    if Assigned(FOriginal) and (FOriginal.Width > 0) and (FOriginal.Height > 0)
      then
      case Layout of
        blGlyphLeft:
          begin
            Result.Left := GlyphPos.X + FOriginal.Width + 4;
            Result.Top := GlyphPos.Y;
            Result.Bottom := GlyphPos.Y + FOriginal.Height;
          end;
        blGlyphRight:
          begin
            Result.Right := GlyphPos.X - 4;
            Result.Top := GlyphPos.Y;
            Result.Bottom := GlyphPos.Y + FOriginal.Height;
          end;
        blGlyphTop:
          Result.Top := GlyphPos.Y + FOriginal.Height + 4;
        blGlyphBottom:
          Result.Bottom := GlyphPos.Y - 4;
      end;
  end
  else
  begin
    // consider caption
    Result := Rect(R.Left, R.Bottom, R.Right, R.Bottom + 6);
    if (Result.Bottom + 2) > Client.Bottom then
      Result.Bottom := Client.Bottom - 2;
  end;
end;

//----------------- TDropDownButton ------------------------------------------

constructor TCustomDropDownButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGlyph := TButtonGlyph.Create;
  TButtonGlyph(FGlyph).OnChange := GlyphChanged;
  SetBounds(0, 0, 37, 22);
  FDropAnchor := ddAnchorDown;
  FDropDownWidth := 14;
  ControlStyle := [csCaptureMouse, csDoubleClicks];
  ParentFont := True;
  Color := clBtnFace;
  FSpacing := 4;
  FMargin := -1;
  FLayout := blGlyphLeft;
  FTransparent := True;
  Inc(ButtonCount);
  DropDownButtonComponents.AddObject(Name, self);
end;

//-----------------------------------------------------------------------------

destructor TCustomDropDownButton.Destroy;
var
  i: integer;
begin
  i := 0;
  repeat
    if TCustomDropDownButton(DropDownButtonComponents.Objects[i]) = self then
    begin
      DropDownButtonComponents.Delete(i);
      i := DropDownButtonComponents.Count;
    end
    else
      inc(i);
  until i = DropDownButtonComponents.Count;
  Dec(ButtonCount);
  TButtonGlyph(FGlyph).Free;
  inherited Destroy;
end;

//-----------------------------------------------------------------------------

procedure TCustomDropDownButton.SetDropDownArrowColor(Value: TColor);
begin
  if not (FDropDownArrowColor = Value) then
    ;
  begin
    FDropDownArrowColor := Value;
    Invalidate;
  end;
end;

//-----------------------------------------------------------------------------

procedure TCustomDropDownButton.SetDropDownWidth(Value: integer);
begin
  if not (FDropDownWidth = Value) then
    ;
  begin
    FDropDownWidth := Value;
    Invalidate;
  end;
end;

//-----------------------------------------------------------------------------

procedure TCustomDropDownButton.Paint;
const
  MAX_WIDTH = 5;
  DownStyles: array[Boolean] of Integer = (BDR_RAISEDINNER, BDR_SUNKENOUTER);
  FillStyles: array[Boolean] of Integer = (BF_MIDDLE, 0);
var
  PaintRect: TRect;
  ExtraRect: TRect;
  DrawFlags: Integer;
  Offset: TPoint;
  LeftPos: Integer;
begin
  if not Enabled then
  begin
    FState := bsDisabled;
    FDragging := False;
  end
  else if (FState = bsDisabled) then
  begin
    if FDown and (GroupIndex <> 0) then
      FState := bsExclusive
    else
      FState := bsUp;
  end;

  Canvas.Font := Self.Font;

  // Creates a rectangle that represent the button and the drop down area,
  // determines also the position to draw the arrow...
  PaintRect := Rect(0, 0, Width, Height);
  ExtraRect := Rect(Width - FDropDownWidth, 0, Width, Height);
  LeftPos := (Width - FDropDownWidth) + ((FDropDownWidth + MAX_WIDTH) div 2) -
    MAX_WIDTH - 1;

  // Determines if the button is a flat or normal button... each uses
  // different painting methods
  if not FFlat then
  begin
    DrawFlags := DFCS_BUTTONPUSH or DFCS_ADJUSTRECT;

    if FState in [bsDown, bsExclusive] then
      DrawFlags := DrawFlags or DFCS_PUSHED;

    // Check if the mouse is in the drop down zone. If it is we then check
    // the state of the button to determine the drawing sequence
    if FDropDownZone then
    begin
      if FDroppedDown then
      begin
        // paint pressed Drop Down Button
        DrawFrameControl(Canvas.Handle, PaintRect, DFC_BUTTON, DRAW_BUTTON_UP);
        DrawFrameControl(Canvas.Handle, ExtraRect, DFC_BUTTON,
          DRAW_BUTTON_DOWN);
      end
      else
      begin
        // paint depressed Drop Down Button
        DrawFrameControl(Canvas.Handle, PaintRect, DFC_BUTTON, DRAW_BUTTON_UP);
        DrawFrameControl(Canvas.Handle, ExtraRect, DFC_BUTTON, DRAW_BUTTON_UP);
        DrawButtonSeperatorUp(Canvas);
      end;
    end
    else
    begin
      DrawFrameControl(Canvas.Handle, PaintRect, DFC_BUTTON, DrawFlags);

      // Determine the type of drop down seperator...
      if (FState in [bsDown, bsExclusive]) then
        DrawButtonSeperatorDown(Canvas)
      else
        DrawButtonSeperatorUp(Canvas);
    end;
  end
  else
  begin
    if (FState in [bsDown, bsExclusive]) or
      (FMouseInControl and (FState <> bsDisabled) and MouseInButton and
        (GetCapture = 0) {!!! bad programming (GetCapture=nil)}) or
      (csDesigning in ComponentState) then
    begin
      // Check if the mouse is in the drop down zone. If it is we then check
      // the state of the button to determine the drawing sequence
      if FDropDownZone then
      begin
        if FDroppedDown then
        begin
          // Paint pressed Drop Down Button
          DrawEdge(Canvas.Handle, PaintRect, DownStyles[False],
            FillStyles[FTransparent] or BF_RECT);
          DrawEdge(Canvas.Handle, ExtraRect, DownStyles[True],
            FillStyles[FTransparent] or BF_RECT);
        end
        else
        begin
          // Paint depressed Drop Down Button
          DrawEdge(Canvas.Handle, PaintRect, DownStyles[False],
            FillStyles[FTransparent] or BF_RECT);
          DrawEdge(Canvas.Handle, ExtraRect, DownStyles[False],
            FillStyles[FTransparent] or BF_RECT);
          DrawButtonSeperatorUp(Canvas);
        end;
      end
      else
      begin
        DrawEdge(Canvas.Handle, PaintRect, DownStyles[FState in [bsDown,
          bsExclusive]], FillStyles[FTransparent] or BF_RECT);

        if (FState in [bsDown, bsExclusive]) then
          DrawButtonSeperatorDown(Canvas)
        else
          DrawButtonSeperatorUp(Canvas);
      end;
    end
    else if not FTransparent then
    begin
      Canvas.Brush.Style := bsSolid;
      Canvas.Brush.Color := Color;
      Canvas.FillRect(PaintRect);
    end;
    InflateRect(PaintRect, -1, -1);
  end;

  if (FState in [bsDown, bsExclusive]) and not (FDropDownZone) then
  begin
    if (FState = bsExclusive) and (not FFlat or not FMouseInControl) then
    begin
      Canvas.Brush.Bitmap := AllocPatternBitmap(clBtnFace, clBtnHighlight);
      Canvas.FillRect(PaintRect);
    end;
    Offset.X := 1;
    Offset.Y := 1;
  end
  else
  begin
    Offset.X := 0;
    Offset.Y := 0;
  end;

  PaintRect := TButtonGlyph(FGlyph).Draw(Canvas, PaintRect, Offset, Caption,
    FLayout, FMargin,
    FSpacing, FState, FTransparent, FDropDownWidth, DrawTextBiDiModeFlags(0));

  // Draws the arrow for the correct state
  if FState = bsDisabled then
  begin
    Canvas.Pen.Style := psClear;
    Canvas.Brush.Color := clBtnShadow;
  end
  else
  begin
    Canvas.Pen.Color := FDropDownArrowColor;
    Canvas.Brush.Color := FDropDownArrowColor;
  end;

  if (FDropDownZone and FDroppedDown) or ((FState = bsDown) and not
    (FDropDownZone)) then
    DrawTriangle(Canvas, (Height div 2), LeftPos + 1, MAX_WIDTH)
  else
    DrawTriangle(Canvas, (Height div 2) - 1, LeftPos, MAX_WIDTH);
end;

//-----------------------------------------------------------------------------

procedure TCustomDropDownButton.UpdateTracking;
var
  P: TPoint;
begin
  if FFlat then
  begin
    if Enabled then
    begin
      GetCursorPos(P);
      FMouseInControl := not (FindDragTarget(P, True) = Self);
      if FMouseInControl then
        Perform(CM_MOUSELEAVE, 0, 0)
      else
        Perform(CM_MOUSEENTER, 0, 0);
    end;
  end;
end;

//-----------------------------------------------------------------------------

procedure TCustomDropDownButton.Loaded;
var
  State: TButtonState;
begin
  inherited Loaded;
  if Enabled then
    State := bsUp
  else
    State := bsDisabled;
  TButtonGlyph(FGlyph).CreateButtonGlyph(State);
end;

//-----------------------------------------------------------------------------

procedure TCustomDropDownButton.MouseDown(Button: TMouseButton; Shift:
  TShiftState; X, Y: Integer);
var
  i: integer;
begin
  inherited MouseDown(Button, Shift, X, Y);
  for i := 0 to DropDownButtonComponents.Count - 1 do
    if TCustomDropDownButton(DropDownButtonComponents.Objects[i]) <> self then
      if
        TCustomDropDownButton(DropDownButtonComponents.Objects[i]).FMouseInControl
        then
      begin
        TCustomDropDownButton(DropDownButtonComponents.Objects[i]).FMouseInControl := false;
        TCustomDropDownButton(DropDownButtonComponents.Objects[i]).UpDate;
      end;

  if (Button = mbLeft) and Enabled then
  begin
    // Determine if mouse is currently in the drop down section...
    FDropDownZone := (X > Width - FDropDownWidth);
    // If so display the button in the proper state and display the menu
    if FDropDownZone then
    begin
      Update;
      DroppedDown := not DroppedDown;
      // Setting this flag to false is very important, we want the dsUp state to
      // be used to display the button properly the next time the mouse moves in
      FDragging := False;
    end
    else
    begin
      if not FDown then
      begin
        FState := bsDown;
        Invalidate;
      end;
      FDragging := True;
    end;
  end;
end;

//-----------------------------------------------------------------------------

procedure TCustomDropDownButton.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewState: TButtonState;
begin
  inherited MouseMove(Shift, X, Y);
  if FDragging then
  begin
    if not FDown then
      NewState := bsUp
    else
      NewState := bsExclusive;
    if (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight) then
      if FDown then
        NewState := bsExclusive
      else
        NewState := bsDown;
    if NewState <> FState then
    begin
      FState := NewState;
      Invalidate;
    end;
  end;
end;

//-----------------------------------------------------------------------------

procedure TCustomDropDownButton.MouseUp(Button: TMouseButton; Shift:
  TShiftState; X, Y: Integer);
var
  DoClick: Boolean;
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FDragging then
  begin
    FDragging := False;
    DoClick := (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <=
      ClientHeight);
    if FGroupIndex = 0 then
    begin
      // Redraw face in case mouse is captured
      FState := bsUp;
      FMouseInControl := False;
      if DoClick and not (FState in [bsExclusive, bsDown]) then
        Invalidate;
    end
    else if DoClick then
    begin
      SetDown(not FDown);
      if FDown then
        Repaint;
    end
    else
    begin
      if FDown then
        FState := bsExclusive;
      Repaint;
    end;
    if DoClick then
      Click;
    UpdateTracking;
    DroppedDown := false;
  end;
end;

//-----------------------------------------------------------------------------

procedure TCustomDropDownButton.Click;
begin
  inherited;
end;

//-----------------------------------------------------------------------------

procedure TCustomDropDownButton.DoDefaultEvent;
begin
  if Assigned(FOnDefaultSelect) then
    FOnDefaultSelect(Self);
end;

//-----------------------------------------------------------------------------

function TCustomDropDownButton.GetPalette: HPALETTE;
begin
  Result := Glyph.Palette;
end;

//-----------------------------------------------------------------------------

function TCustomDropDownButton.GetGlyph: TBitmap;
begin
  Result := TButtonGlyph(FGlyph).Glyph;
end;

//-----------------------------------------------------------------------------

procedure TCustomDropDownButton.SetGlyph(Value: TBitmap);
begin
  TButtonGlyph(FGlyph).Glyph := Value;
  Invalidate;
end;

//-----------------------------------------------------------------------------

function TCustomDropDownButton.GetNumGlyphs: TNumGlyphs;
begin
  Result := TButtonGlyph(FGlyph).NumGlyphs;
end;

//-----------------------------------------------------------------------------

procedure TCustomDropDownButton.DrawButtonSeperatorUp(Canvas: TCanvas);
begin
  with Canvas do
  begin
    Pen.Style := psSolid;
    Brush.Style := bsClear;
    Pen.Color := clBtnHighlight;
    Rectangle(Width - DropDownWidth, 1, Width - DropDownWidth + 1, Height - 1);
    Pen.Color := clBtnShadow;
    Rectangle(Width - DropDownWidth - 1, 1, Width - DropDownWidth, Height - 1);
  end;
end;

//-----------------------------------------------------------------------------

procedure TCustomDropDownButton.DrawButtonSeperatorDown(Canvas: TCanvas);
begin
  with Canvas do
  begin
    Pen.Style := psSolid;
    Brush.Style := bsClear;
    Pen.Color := clBtnHighlight;
    Rectangle(Width - DropDownWidth + 1, 2, Width - DropDownWidth + 2, Height -
      2);
    Pen.Color := clBtnShadow;
    Rectangle(Width - DropDownWidth, 2, Width - DropDownWidth + 1, Height - 2);
  end;
end;

//-----------------------------------------------------------------------------

procedure TCustomDropDownButton.DrawTriangle(Canvas: TCanvas; Top, Left, Width:
  Integer);
begin
  if Odd(Width) then
    Inc(Width);
  if FDropAnchor = ddAnchorDown then
    Canvas.Polygon([Point(Left, Top - 1),
      Point(Left + Width, Top - 1),
        Point(Left + Width div 2, Top + Width div 2 - 1)])
  else if FDropAnchor = ddAnchorUp then
    Canvas.Polygon([Point(Left, Top + Width div 2 - 1),
      Point(Left + Width, Top + Width div 2 - 1),
        Point(Left + Width div 2, Top - 1)])
  else if FDropAnchor = ddAnchorRight then
    Canvas.Polygon([Point(Left, Top - Width div 2),
      Point(Left, Top + Width div 2),
        Point(Left + Width div 2, Top)])
end;

//-----------------------------------------------------------------------------

procedure TCustomDropDownButton.SetNumGlyphs(Value: TNumGlyphs);
begin
  if Value < 0 then
    Value := 1
  else if Value > 4 then
    Value := 4;
  if Value <> TButtonGlyph(FGlyph).NumGlyphs then
  begin
    TButtonGlyph(FGlyph).NumGlyphs := Value;
    Invalidate;
  end;
end;

//-----------------------------------------------------------------------------

procedure TCustomDropDownButton.GlyphChanged(Sender: TObject);
begin
  Invalidate;
end;

//-----------------------------------------------------------------------------

procedure TCustomDropDownButton.UpdateExclusive;
var
  Msg: TMessage;
begin
  if (FGroupIndex <> 0) and (Parent <> nil) then
  begin
    Msg.Msg := CM_BUTTONPRESSED;
    Msg.WParam := FGroupIndex;
    Msg.LParam := Longint(Self);
    Msg.Result := 0;
    Parent.Broadcast(Msg);
  end;
end;

//-----------------------------------------------------------------------------

procedure TCustomDropDownButton.SetDown(Value: Boolean);
begin
  if FGroupIndex = 0 then
    Value := False;
  if Value <> FDown then
  begin
    if FDown and (not FAllowAllUp) then
      Exit;
    FDown := Value;
    if Value then
    begin
      if FState = bsUp then
        Invalidate;
      FState := bsExclusive;
    end
    else
    begin
      FState := bsUp;
      Repaint;
    end;
    if Value then
      UpdateExclusive;
  end;
end;

//-----------------------------------------------------------------------------

procedure TCustomDropDownButton.SetFlat(Value: Boolean);
begin
  if Value <> FFlat then
  begin
    FFlat := Value;
    if Value then
      ControlStyle := ControlStyle - [csOpaque]
    else
      ControlStyle := ControlStyle + [csOpaque];
    Invalidate;
  end;
end;

//-----------------------------------------------------------------------------

procedure TCustomDropDownButton.SetGroupIndex(Value: Integer);
begin
  if FGroupIndex <> Value then
  begin
    FGroupIndex := Value;
    UpdateExclusive;
  end;
end;

//-----------------------------------------------------------------------------

procedure TCustomDropDownButton.SetLayout(Value: TButtonLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Invalidate;
  end;
end;

//-----------------------------------------------------------------------------

procedure TCustomDropDownButton.SetDropAnchor(Value: TDropDownAnchor);
begin
  if (Value <> FDropAnchor) then
  begin
    FDropAnchor := Value;
    Invalidate
  end;
end;

//-----------------------------------------------------------------------------

procedure TCustomDropDownButton.SetMargin(Value: Integer);
begin
  if (Value <> FMargin) and (Value >= -1) then
  begin
    FMargin := Value;
    Invalidate;
  end;
end;

//-----------------------------------------------------------------------------

procedure TCustomDropDownButton.SetSpacing(Value: Integer);
begin
  if Value <> FSpacing then
  begin
    FSpacing := Value;
    Invalidate;
  end;
end;

//-----------------------------------------------------------------------------

procedure TCustomDropDownButton.SetAllowAllUp(Value: Boolean);
begin
  if FAllowAllUp <> Value then
  begin
    FAllowAllUp := Value;
    UpdateExclusive;
  end;
end;

//-----------------------------------------------------------------------------

procedure TCustomDropDownButton.WMLButtonDblClk(var Message: TWMLButtonDown);
begin
  inherited;
  if FDown then
    DblClick;
end;

//-----------------------------------------------------------------------------

procedure TCustomDropDownButton.CMEnabledChanged(var Message: TMessage);
const
  NewState: array[Boolean] of TButtonState = (bsDisabled, bsUp);
begin
  TButtonGlyph(FGlyph).CreateButtonGlyph(NewState[Enabled]);
  UpdateTracking;
  Repaint;
end;

//-----------------------------------------------------------------------------

procedure TCustomDropDownButton.CMButtonPressed(var Message: TMessage);
var
  Sender: TDropDownButton;
begin
  if Message.WParam = FGroupIndex then
  begin
    Sender := TDropDownButton(Message.LParam);
    if Sender <> Self then
    begin
      if Sender.Down and FDown then
      begin
        FDown := False;
        FState := bsUp;
      end;
      FAllowAllUp := Sender.AllowAllUp;
    end;
  end;
  Invalidate;
end;

//-----------------------------------------------------------------------------

procedure TCustomDropDownButton.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, Caption) and
      Enabled and
      Visible and
      Assigned(Parent) and
      Parent.Showing then
    begin
      Click;
      Result := 1;
    end
    else
      inherited;
end;

//-----------------------------------------------------------------------------

procedure TCustomDropDownButton.CMFontChanged(var Message: TMessage);
begin
  Invalidate;
end;

//-----------------------------------------------------------------------------

procedure TCustomDropDownButton.CMTextChanged(var Message: TMessage);
begin
  Invalidate;
end;

//-----------------------------------------------------------------------------

procedure TCustomDropDownButton.CMSysColorChange(var Message: TMessage);
begin
  with TButtonGlyph(FGlyph) do
  begin
    Invalidate;
    CreateButtonGlyph(FState);
  end;
end;

//-----------------------------------------------------------------------------

procedure TCustomDropDownButton.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if FFlat and not FMouseInControl and Enabled then
  begin
    FMouseInControl := True;
    Repaint;
  end
end;

//-----------------------------------------------------------------------------

procedure TCustomDropDownButton.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FFlat and FMouseInControl and Enabled and not FDragging then
  begin
    FMouseInControl := False;
    Invalidate;
  end;
end;

//-----------------------------------------------------------------------------

function TCustomDropDownButton.MouseInButton: boolean;
var
  p: TPoint;
  r: TRect;
begin
  GetCursorPos(p);
  SetRect(r, Left, Top, Left + Width, Top + Height);
  MapWindowPoints(Parent.Handle, 0, r, 2);
  result := PtInRect(r, p);
end;

//-----------------------------------------------------------------------------

procedure TCustomDropDownButton.SetDroppedDown(const Value: Boolean);
var
  Allowed: Boolean;
begin
  if FDroppedDown <> Value then
  begin
    Allowed := True;
    if Assigned(FOnDropChanging) then
      FOnDropChanging(Self, Allowed);
    if Allowed then
    begin
      FDroppedDown := Value;
      if FDroppedDown then
        FState := bsDown
      else
        FState := bsUp;
      Invalidate;
      if Assigned(FOnDropChanged) then
        FOnDropChanged(Self);
    end;
  end;
end;

//-----------------------------------------------------------------------------

procedure TCustomDropDownButton.SetTransparent(const Value: Boolean);
begin
  if Value <> FTransparent then
  begin
    FTransparent := Value;
    if Value then
      ControlStyle := ControlStyle - [csOpaque]
    else
      ControlStyle := ControlStyle + [csOpaque];
    Invalidate;
  end;
end;

//-----------------------------------------------------------------------------

procedure TCustomDropDownButton.ActionChange(Sender: TObject; CheckDefaults:
  Boolean);

//--------------- local functions -----------------------

  procedure CopyImage(ImageList: TCustomImageList; Index: Integer);
  begin
    with Glyph do
    begin
      Width := ImageList.Width;
      Height := ImageList.Height;
      Canvas.Brush.Color := clFuchsia; //! for lack of a better color
      Canvas.FillRect(Rect(0, 0, Width, Height));
      ImageList.Draw(Canvas, 0, 0, Index);
    end;
  end;

  //--------------- end local functions -------------------

begin
  inherited ActionChange(Sender, CheckDefaults);
  if Sender is TCustomAction then
    with TCustomAction(Sender) do
    begin
      // Copy image from action's imagelist
      if Glyph.Empty and
        Assigned(ActionList) and
        Assigned(ActionList.Images) and
        (ImageIndex >= 0) and
        (ImageIndex < ActionList.Images.Count) then
        CopyImage(ActionList.Images, ImageIndex);
    end;
end;

//-----------------------------------------------------------------------------

procedure TCustomMenuButton.SetDroppedDown(const Value: Boolean);
var
  p: TPoint;
  allow: boolean;
begin
  inherited;
  if DroppedDown and Assigned(FMenu) then
  begin
    p := ClientOrigin;
    if FDropAnchor = ddAnchorDown then
      p.y := p.y + Height
    else if FDropAnchor = ddAnchorRight then
      p.x := p.x + Width;
    allow := true;
    if Assigned(FOnBeforePopup) then
      FOnBeforePopup(self, allow);
    if allow then
      FMenu.Popup(p.x, p.y);
    if Assigned(FOnAfterPopup) then
      FOnAfterPopup(self);
    DroppedDown := false;
  end;
end;

constructor TMenuTool.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AllowAllUp := true;
  Caption := 'MenuTool';
  Flat := true;
  Height := GetSystemMetrics(SM_CYMENU);
  Width := 80;
  Margin := 4;
  FDropDownWidth := -15;
  Font := Screen.MenuFont;
end;

procedure TMenuTool.Paint;
begin
  Self.Font.Assign(Screen.MenuFont);
  inherited;
end;

procedure TMenuTool.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:
  Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Enabled then
  begin
    if (Button = mbLeft) then
    begin
      FMouseInControl := FDroppedDown;
      UpDate;
      DroppedDown := not DroppedDown;
      FDown := FDroppedDown;
      FDragging := not FDragging;
    end
    else
    begin
      FMouseInControl := true;
      Invalidate;
    end;
  end;
end;

procedure Register;
begin
  RegisterComponents('TombViewer Components', [TDropDownButton, TMenuButton,
    TMenuTool]);
end;

initialization
  DropDownButtonComponents := TStringList.Create;

finalization
  DropDownButtonComponents.Free;

end.

