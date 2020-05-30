{$B-}  // Complete Boolean Evaluation
{$T-}  // Typed @ operator
{$X+}  // Extended syntax
{$P+}  // Open string params
{$J+}  // Writeable structured consts
{$H+}  // Use long strings by default
{$O-}  // Optimization
{$R-}  // Range-Checking
{$V-}  // Var-String Checking

unit smoothshow;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls;

type

{ TTRVSmoothShow }

  TMinLoc = (mlCenter, mlTopLeft, mlTopRight, mlBottomLeft, mlBottomRight, mlParentCenter, mlCustom);

  TTRVSmoothShow = class(TComponent)
  private
    FDelay: Word;
    FColor: TColor;
    FBorderWidth: TBorderWidth;
    FEnabled: Boolean;
    FReverse: Boolean;
    FMinLocation: TMinLoc;
    FMinLeft: Integer;
    FMinTop: Integer;
    FMinWidth: Word;
    FMinHeight: Word;
    FOnFinishing: TNotifyEvent;
    FOnFinish: TNotifyEvent;
    Window: TCustomForm;
    OldRgn: HRgn;
    Timer: TTimer;
    Shadow: TForm;
    ThisStep: Integer;
    FBusy: Boolean;
    StartRect: TRect;
    StopRect: TRect;
    DiffRect: TRect;
    FParentPoint: TPoint;
    function GetCustomBounds: TRect;
    procedure SetCustomBounds(Value: TRect);
    function MinimizedRect: TRect;
    function CurrentRect(sStep: Word): TRect;
    procedure TimerFired(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Execute(const frm: TForm; const ParentControl: TControl; const reverse: boolean);
    property Busy: Boolean read FBusy;
    property CustomBounds: TRect read GetCustomBounds write SetCustomBounds;
  published
    property Color: TColor read FColor write FColor default clBlack;
    property BorderWidth: TBorderWidth read FBorderWidth write FBorderWidth default 2;
    property Delay: Word read FDelay write FDelay default 25;
    property Enabled: Boolean read FEnabled write FEnabled default True;
    property MinLocation: TMinLoc read FMinLocation write FMinLocation default mlParentCenter;
    property MinLeft: Integer read FMinLeft write FMinLeft default 0;
    property MinTop: Integer read FMinTop write FMinTop default 0;
    property MinWidth: Word read FMinWidth write FMinWidth default 27;
    property MinHeight: Word read FMinHeight write FMinHeight default 27;
    property OnFinishing: TNotifyEvent read FOnFinishing write FOnFinishing;
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
  end;

procedure FormSmoothShow(const frm: TForm; const fprnt: TControl);

procedure FormSmoothHide(const frm: TForm; const fprnt: TControl);

implementation

{ TTRVSmoothShow }

constructor TTRVSmoothShow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FParentPoint.X := Screen.DesktopLeft + Screen.DesktopWidth div 2;
  FParentPoint.Y := Screen.DesktopTop + Screen.DesktopHeight div 2;
  FDelay := 25;
  FColor := clBlack;
  FEnabled := True;
  FReverse := False;
  FMinLocation := mlParentCenter;
  FBorderWidth := 2;
  CustomBounds := Rect(0, 0, 27, 27);
  FBusy := False;
end;

function TTRVSmoothShow.GetCustomBounds: TRect;
begin
  SetRect(Result, MinLeft, MinTop, MinLeft + MinWidth, MinTop + MinHeight);
end;

procedure TTRVSmoothShow.SetCustomBounds(Value: TRect);
begin
  With Value do
  begin
    FMinLeft := Left;
    FMinTop := Top;
    FMinWidth := Right - Left;
    FMinHeight := Bottom - Top;
  end;
end;

function TTRVSmoothShow.MinimizedRect: TRect;
begin
  case MinLocation of
    mlParentCenter:
      SetRect(Result, FParentPoint.X, FParentPoint.Y, 0, 0);
    mlCenter:
      SetRect(Result, Screen.DesktopLeft + Screen.DesktopWidth div 2,
        Screen.DesktopTop + Screen.DesktopHeight div 2, 0, 0);
    mlTopLeft:
      SetRect(Result, Screen.DesktopLeft, Screen.DesktopTop, 0, 0);
    mlTopRight:
      SetRect(Result, Screen.DesktopLeft + Screen.DesktopWidth,
        Screen.DesktopTop, 0, 0);
    mlBottomLeft:
      SetRect(Result, Screen.DesktopLeft, Screen.DesktopTop +
        Screen.DesktopHeight, 0, 0);
    mlBottomRight:
      SetRect(Result, Screen.DesktopLeft + Screen.DesktopWidth,
        Screen.DesktopTop + Screen.DesktopHeight, 0, 0);
  else
    SetRect(Result, MinLeft, MinTop, MinWidth, MinHeight);
  end;
end;

function TTRVSmoothShow.CurrentRect(sStep: Word): TRect;
begin
  Result.Left := StartRect.Left + (sStep * DiffRect.Left) div 100;
  Result.Top := StartRect.Top + (sStep * DiffRect.Top) div 100;
  Result.Right := StartRect.Right + (sStep * DiffRect.Right) div 100;
  Result.Bottom := StartRect.Bottom + (sStep * DiffRect.Bottom) div 100;
end;

procedure TTRVSmoothShow.Execute(const frm: TForm; const ParentControl: TControl; const reverse: boolean);
var
  Rgn: HRgn;
begin
  freverse := reverse;

  if ParentControl <> nil then
    FParentPoint := ParentControl.ClientToScreen(
      Point(
        ParentControl.Width div 2,
        ParentControl.Height div 2)
      )
  else
  begin
    FParentPoint.X := Screen.DesktopLeft + Screen.DesktopWidth div 2;
    FParentPoint.Y := Screen.DesktopTop + Screen.DesktopHeight div 2;
  end;

  Window := TCustomForm(frm);
  if not Assigned(Window) then
    Exit;

  if not FBusy and (Enabled or (csDesigning in ComponentState)) then
  begin
    FBusy := True;
    if freverse then
    begin
      SetRect(StartRect, Window.Left, Window.Top, Window.Width, Window.Height);
      StopRect := MinimizedRect;
    end
    else
    begin
      StartRect := MinimizedRect;
      SetRect(StopRect, Window.Left, Window.Top, Window.Width, Window.Height);
    end;
    DiffRect.Left := StopRect.Left - StartRect.Left;
    DiffRect.Top := StopRect.Top - StartRect.Top;
    DiffRect.Right := StopRect.Right - StartRect.Right;
    DiffRect.Bottom := StopRect.Bottom - StartRect.Bottom;
    Shadow := TForm.Create(Application);
    Shadow.BorderStyle := bsNone;
    Shadow.Color := Color;
    OldRgn := 0;
    GetWindowRgn(Window.Handle, OldRgn);
    Rgn := CreateRectRgn(0, 0, 0, 0);
    SetWindowRgn(Window.Handle, Rgn, True);
    ThisStep := 0;
    Timer := TTimer.Create(Self);
    Timer.Interval := Delay;
    Timer.OnTimer := TimerFired;
    TimerFired(Self);
  end;
end;

procedure TTRVSmoothShow.TimerFired(Sender: TObject);
var
  Rgn1, Rgn2: HRgn;
begin
  with CurrentRect(ThisStep) do Shadow.SetBounds(Left, Top, Right, Bottom);
  Rgn1 := CreateRectRgn(0, 0, Shadow.Width, Shadow.Height);
  Rgn2 := CreateRectRgn(BorderWidth, BorderWidth, Shadow.Width - BorderWidth, Shadow.Height - BorderWidth);
  CombineRgn(Rgn1, Rgn1, Rgn2, RGN_DIFF);
  DeleteObject(Rgn2);
  SetWindowRgn(Shadow.Handle, Rgn1, True);
  if not Shadow.Visible then Shadow.Show;
  Shadow.Update;
  if ThisStep >= 100 then
  begin
    Timer.Free;
    if not (csDesigning in ComponentState) and Assigned(OnFinishing) then
      OnFinishing(Self);
    Shadow.Free;
    if not FReverse then
    begin
      SetWindowRgn(Window.Handle, OldRgn, True);
      Window.Update;
    end;
    FBusy := False;
    if not (csDesigning in ComponentState) and Assigned(OnFinish) then
      OnFinish(Self);
  end;
  ThisStep := (3 * ThisStep div 2) + 2;
  if ThisStep > 100 then ThisStep := 100;
end;

var
  fsmooth: TTRVSmoothShow;

procedure FormSmoothShow(const frm: TForm; const fprnt: TControl);
begin
  fsmooth.Execute(frm, fprnt, false);
end;

procedure FormSmoothHide(const frm: TForm; const fprnt: TControl);
begin
  fsmooth.Execute(frm, fprnt, true);
end;

initialization
  fsmooth := TTRVSmoothShow.Create(nil);

finalization
  fsmooth.Free;

end.

