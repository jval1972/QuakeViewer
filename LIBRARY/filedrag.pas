{*******************************************************************************
*
*  TFileDrag Component - Adds support for dropping files from explorer onto a
*                        a Delphi form.
*
*  Copyright (c) 1996 - Erik C. Nielsen ( 72233.1314@compuserve.com )
*  All Rights Reserved
*
*******************************************************************************}

unit filedrag;

interface

{$R FILEDRAGBTN.DCR}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ShellApi;

type
  TFileDrag = class(TComponent)
  private
    FNameWithPath: TStrings;
    FNameOnly: TStrings;
    FExtension: TStrings;
    FNumDropped: Integer;
    FEnabled: Boolean;
    FWndHandle: HWND;
    FDefProc: Pointer;
    FWndProcInstance: Pointer;
    FOnDrop: TNotifyEvent;

    procedure DropFiles(hDropHandle: HDrop);
    procedure SetEnabled(Value: Boolean);
    procedure WndProc(var Msg: TMessage);
    procedure InitControl;
    procedure DestroyControl;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property NameWithPath: TStrings read FNameWithPath;
    property NameOnly: TStrings read FNameOnly;
    property Extension: TStrings read FExtension;
    property FileCount: Integer read FNumDropped;
    property EnableDrop: Boolean read FEnabled write SetEnabled default True;
    property OnDrop: TNotifyEvent read FOnDrop write FOnDrop;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TombViewer Components', [TFileDrag]);
end;

constructor TFileDrag.Create( AOwner: TComponent );
begin
  inherited Create(AOwner);
  FNumDropped := 0;
  FNameWithPath := TStringList.Create;
  FNameOnly := TStringList.Create;
  FExtension := TStringList.Create;
  FWndHandle := 0;

  InitControl;
  SetEnabled(FEnabled);
end;

destructor TFileDrag.Destroy;
begin
  DestroyControl;
  SetEnabled(True);
  FNameWithPath.Free;
  FNameOnly.Free;
  FExtension.Free;
  inherited Destroy;
end;

procedure TFileDrag.InitControl;
var
  WinCtl: TWinControl;
begin
  if Owner is TWinControl then
  begin
    { Subclass the owner so this control can capture the WM_DROPFILES message }
    WinCtl := TWinControl(Owner);
    FWndHandle := WinCtl.Handle;
    FWndProcInstance := Classes.MakeObjectInstance(WndProc);
    FDefProc := Pointer(GetWindowLong( FWndHandle, GWL_WNDPROC));
    SetWindowLong(FWndHandle, GWL_WNDPROC, Longint(FWndProcInstance));
  end
  else
    FEnabled := False;
end;

procedure TFileDrag.DestroyControl;
begin
  if FWndHandle <> 0 then
  begin
     { Restore the original window procedure }
    SetWindowLong(FWndHandle, GWL_WNDPROC, Longint(FDefProc));
    Classes.FreeObjectInstance(FWndProcInstance);
  end
end;

procedure TFileDrag.SetEnabled( Value: Boolean );
begin
  FEnabled := Value;
  { Call Win32 API to register the owner as being able to accept dropped files }
  DragAcceptFiles(FWndHandle, FEnabled);
end;

procedure TFileDrag.DropFiles(hDropHandle: HDrop);
var
  pszFileWithPath, pszFile, pszExt: PChar;
  iFile, iPos, iStrLen, iTempLen: Integer;
begin
  iStrLen := 128;
  pszFile := '';
  pszExt := '';
  pszFileWithPath := StrAlloc(iStrLen);
  iFile := 0;

  { Clear any existing strings from the string lists }
  FNameWithPath.Clear;
  FNameOnly.Clear;
  FExtension.Clear;

  { Retrieve the number of files being dropped }
  FNumDropped := DragQueryFile(hDropHandle, $FFFFFFFF, pszFile, iStrLen);

  { Retrieve each file being dropped }
  while iFile < FNumDropped do
  begin
   { Get the length of this file name }
   iTempLen := DragQueryFile(hDropHandle, iFile, nil, 0) + 1;
   { If file length > current PChar, delete and allocate one large enough }
   if iTempLen > iStrLen then
     begin
       iStrLen := iTempLen;
       StrDispose(pszFileWithPath);
       pszFileWithPath := StrAlloc(iStrLen);
     end;
   { Get the fully qualified file name }
   DragQueryFile(hDropHandle, iFile, pszFileWithPath, iStrLen);
   { Get the extension and name parts }
   iPos := StrLen(pszFileWithPath);
   while iPos > 0 do
    begin
     Dec(iPos);
     case pszFileWithPath[iPos] of
       '.': pszExt := @pszFileWithPath[iPos + 1];
       '\': begin
             pszFile := @pszFileWithPath[iPos + 1];
             iPos := 0;
            end
     end;
    end;
    { Add the file names to appropriate lists }
    FNameWithPath.Add(StrPas(pszFileWithPath));
    FNameOnly.Add( StrPas(pszFile));
    FExtension.Add(StrPas(pszExt));
    Inc(iFile);
  end;

  StrDispose(pszFileWithPath);

  { This will result in the OnDrop method being called, if it is defined }
  if Assigned(FOnDrop) then
    FOnDrop(Self);
end;

procedure TFileDrag.WndProc(var Msg: TMessage);
begin
  with Msg do
  begin
    { If message is drop files, process, otherwise call the original window procedure }
    if Msg = WM_DROPFILES then
      DropFiles(HDrop(wParam))
    else
      Result := CallWindowProc(FDefProc, FWndHandle, Msg, WParam, LParam);
  end;
end;

end.

