// 2018:
// Component to create easily file menu history
// (c) 200? - 2018, Jim Valavanis
//  Original version can still be downloaded at my old geocities site at:
//  http://www.geocities.ws/jimmyvalavanis/programming/delphi/index.html

unit FileMenuHistory;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus;

type
  TOnOpenEvent = procedure (Sender: TObject; const FileName: TFileName) of object;

  TFileMenuHistory = class(TComponent)
  private
    { Private declarations }
    fMenuItems: array[0..9] of TMenuItem;
    fOnOpen: TOnOpenEvent;
    fPaths: TStringList;
  protected
    { Protected declarations }
    procedure DoFileOpen(Sender: TObject);
    procedure SetMenuItems0(Value: TMenuItem); virtual;
    procedure SetMenuItems1(Value: TMenuItem); virtual;
    procedure SetMenuItems2(Value: TMenuItem); virtual;
    procedure SetMenuItems3(Value: TMenuItem); virtual;
    procedure SetMenuItems4(Value: TMenuItem); virtual;
    procedure SetMenuItems5(Value: TMenuItem); virtual;
    procedure SetMenuItems6(Value: TMenuItem); virtual;
    procedure SetMenuItems7(Value: TMenuItem); virtual;
    procedure SetMenuItems8(Value: TMenuItem); virtual;
    procedure SetMenuItems9(Value: TMenuItem); virtual;
    procedure SetMenuItems(index: integer; Value: TMenuItem); virtual;
    procedure SetPaths(Value: TStringList); virtual;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function MenuItem(index: integer): TMenuItem;
    procedure AddPath(const FileName: TFileName); virtual;
    procedure RefreshMenuItems; virtual;
  published
    { Published declarations }
    property MenuItem0: TMenuItem read fMenuItems[0] write SetMenuItems0;
    property MenuItem1: TMenuItem read fMenuItems[1] write SetMenuItems1;
    property MenuItem2: TMenuItem read fMenuItems[2] write SetMenuItems2;
    property MenuItem3: TMenuItem read fMenuItems[3] write SetMenuItems3;
    property MenuItem4: TMenuItem read fMenuItems[4] write SetMenuItems4;
    property MenuItem5: TMenuItem read fMenuItems[5] write SetMenuItems5;
    property MenuItem6: TMenuItem read fMenuItems[6] write SetMenuItems6;
    property MenuItem7: TMenuItem read fMenuItems[7] write SetMenuItems7;
    property MenuItem8: TMenuItem read fMenuItems[8] write SetMenuItems8;
    property MenuItem9: TMenuItem read fMenuItems[9] write SetMenuItems9;
    property Paths: TStringList read fPaths write SetPaths;
    property OnOpen:TOnOpenEvent read fOnOpen write fOnOpen;
  end;

procedure Register;

implementation

uses Math;

resourceString
  rsRangeCheckError = 'Index out of range.';
  rsFmtMenuCaption = '&%d. %s';

procedure Register;
begin
  RegisterComponents('TombViewer Components', [TFileMenuHistory]);
end;

procedure TFileMenuHistory.SetMenuItems0(Value: TMenuItem);
begin
  SetMenuItems(0, Value);
end;

procedure TFileMenuHistory.SetMenuItems1(Value: TMenuItem);
begin
  SetMenuItems(1, Value);
end;

procedure TFileMenuHistory.SetMenuItems2(Value: TMenuItem);
begin
  SetMenuItems(2, Value);
end;

procedure TFileMenuHistory.SetMenuItems3(Value: TMenuItem);
begin
  SetMenuItems(3, Value);
end;

procedure TFileMenuHistory.SetMenuItems4(Value: TMenuItem);
begin
  SetMenuItems(4, Value);
end;

procedure TFileMenuHistory.SetMenuItems5(Value: TMenuItem);
begin
  SetMenuItems(5, Value);
end;

procedure TFileMenuHistory.SetMenuItems6(Value: TMenuItem);
begin
  SetMenuItems(6, Value);
end;

procedure TFileMenuHistory.SetMenuItems7(Value: TMenuItem);
begin
  SetMenuItems(7, Value);
end;

procedure TFileMenuHistory.SetMenuItems8(Value: TMenuItem);
begin
  SetMenuItems(8, Value);
end;

procedure TFileMenuHistory.SetMenuItems9(Value: TMenuItem);
begin
  SetMenuItems(9, Value);
end;

procedure TFileMenuHistory.SetMenuItems(index: integer; Value: TMenuItem);
begin
  if (index >= low(fMenuItems)) and (index <= high(fMenuItems)) then
  begin
    fMenuItems[index] := Value;
    fMenuItems[index].OnClick := DoFileOpen;
  end
  else
    raise Exception.Create(rsRangeCheckError);
end;

function TFileMenuHistory.MenuItem(index: integer): TMenuItem;
begin
  if (index >= low(fMenuItems)) and (index <= high(fMenuItems)) then
    result := fMenuItems[index]
  else
    result := nil;
end;

constructor TFileMenuHistory.Create(AOwner: TComponent);
var i: integer;
begin
  Inherited;
  for i := low(fMenuItems) to high(fMenuItems) do fMenuItems[i] := nil;
  fPaths := TStringList.Create;
end;

destructor TFileMenuHistory.Destroy;
begin
  fPaths.Clear;
  fPaths.Free;
  Inherited;
end;

procedure TFileMenuHistory.DoFileOpen(Sender: TObject);
var i: integer;
begin
  if Sender <> nil then
  begin
    for i := low(fMenuItems) to high(fMenuItems) do
    begin
      if Sender = fMenuItems[i] then
      begin
        if fPaths.Count > i then
          if Assigned(fOnOpen) then fOnOpen(Sender, fPaths[i]);
        exit;
      end;
    end;
  end;
end;

function MkShortName(const fname: string): string;
const
  MAXDISPFNAME = 30;
var
  i: integer;
begin
  if Length(fname) < MAXDISPFNAME then
  begin
    Result := fname;
    exit;
  end;
  Result := '';
  for i := Length(fname) downto Length(fname) - (MAXDISPFNAME - 6) do
    Result := fname[i] + Result;
  Result := '...' + Result;
  for i := 3 downto 1 do
    Result := fname[i] + Result;
end;

procedure TFileMenuHistory.SetPaths(Value: TStringList);
var i, count: integer;
begin
  if Value.Text <> Paths.Text then
  begin
    fPaths.Clear;
    for i := 0 to Value.Count do
      if Trim(Value.Strings[i]) <> '' then fPaths.Add(Value.Strings[i]);
  end;
  for i := low(fMenuItems) to high(fMenuItems) do
    if Assigned(fMenuItems[i]) then
      fMenuItems[i].Visible := false;
  count := 0;
  for i := low(fMenuItems) to min(high(fMenuItems), fPaths.Count - 1) do
    if Assigned(fMenuItems[i]) then
    begin
      inc(count);
      fMenuItems[i].Visible := true;
      fMenuItems[i].Caption := Format(rsFmtMenuCaption, [count, MkShortName(fPaths.Strings[i])]);
    end;
end;

procedure TFileMenuHistory.AddPath(const FileName: TFileName);
var i: integer;
begin
  if fPaths.IndexOf(FileName) = -1 then
  begin
    fPaths.Insert(0, FileName);
    if fPaths.Count > high(fMenuItems) then
      for i := fPaths.Count - 1 downto high(fMenuItems) do fPaths.Delete(i);
  end;
  SetPaths(fPaths);
end;

procedure TFileMenuHistory.RefreshMenuItems;
begin
  SetPaths(fPaths);
end;

end.

