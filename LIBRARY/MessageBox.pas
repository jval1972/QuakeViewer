// 2018:
// Component that ecapsulates a Windows MessageBox
// (c) 200? - 2018, Jim Valavanis
//  Original version can still be downloaded at my old geocities site at:
//  http://www.geocities.ws/jimmyvalavanis/programming/delphi/index.html

unit MessageBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  TMessageBoxButtons = (mbxAbortRetryIgnore, mbxOK, mbxOKCancel, mbxRetryCancel, mbxYesNo, mbxYesNoCancel);
  TMessageBoxIcon = (mbxIconExclamation, mbxIconWarning, mbxIconInformation, mbxIconAsterisk, mbxIconQuestion,
                      mbxIconStop, mbxIconError,mbxIconHand,mbxNoIcon);
  TMessageBoxDefButton = (mbxDefButton1, mbxDefButton2, mbxDefButton3, mbxNoDefButton);
  TMessageBoxModality = (mbxApplModal, mbxSystemModal, mbxTaskModal, mbxDefModality, mbxNoModality);
  TMessageBoxTextAlignment = (mbxLeft, mbxRight);

  TOnMessageBoxBeforeExecuteEvent = procedure(Sender: TObject; var AllowExec: boolean) of object;

  TOnMessageBoxAfterExecuteEvent = procedure(Sender: TObject) of object;

  TMessageBox = class(TComponent)
  private
    FCaption: string;
    FText: string;
    FButtons: TMessageBoxButtons;
    FIcon: TMessageBoxIcon;
    FDefaultButton: TMessageBoxDefButton;
    FModality: TMessageBoxModality;
    FTextAlignment: TMessageBoxTextAlignment;
    FOnTop: boolean;
    FReturnValue: integer;
    FOnAfterExecute: TOnMessageBoxAfterExecuteEvent;
    FOnBeforeExecute: TOnMessageBoxBeforeExecuteEvent;
  protected
    { Protected declarations }
  public
    { Public declarations }
    property ReturnValue: integer read FReturnValue;
    constructor Create(AOWner: TComponent); override;
    function Execute: integer; overload; virtual;
    function Execute(aText: string): integer; overload; virtual;
    function ExecuteFmt(const Args: array of const): integer; virtual;
  published
    property Caption: string read FCaption write FCaption;
    property Text: string read FText write FText;
    property Buttons: TMessageBoxButtons read FButtons write FButtons;
    property Icon: TMessageBoxIcon read FIcon write FIcon;
    property DefaultButton: TMessageBoxDefButton read FDefaultButton write FDefaultButton;
    property Modality: TMessageBoxModality read FModality write FModality;
    property TextAlignment: TMessageBoxTextAlignment read FTextAlignment write FTextAlignment;
    property OnTop: boolean read FOnTop write FOnTop default false;
    property OnAfterExecute: TOnMessageBoxAfterExecuteEvent read FOnAfterExecute write FOnAfterExecute;
    property OnBeforeExecute: TOnMessageBoxBeforeExecuteEvent read FOnBeforeExecute write FOnBeforeExecute;
  end;

procedure Register;

implementation

resourceString
  rsText = 'Write your message here.';

constructor TMessageBox.Create(AOWner: TComponent);
begin
  Inherited Create(AOwner);
  FCaption := Application.Title;
  FText := rsText;
  FButtons := mbxOKCancel;
  FIcon := mbxNoIcon;
  FDefaultButton := mbxNoDefButton;
  FModality := mbxDefModality;
  FTextAlignment := mbxLeft;
  FOnTop := false;
  FReturnValue := -1;
end;

function TMessageBox.ExecuteFmt(const Args: array of const): integer;
begin
  Result := Execute(Format(FText, Args));
end;

function TMessageBox.Execute(aText: string): integer;
var FText1: string;
begin
  FText1 := FText;
  FText := aText;
  Result := Execute;
  FText := FText1;
end;

function TMessageBox.Execute: integer;
var
  msgCaption, msgText: PChar;
  flags: integer;
  hWnd: THandle;
  AllowExec: boolean;
begin
  AllowExec := true;
  if Assigned(FOnBeforeExecute) then
    FOnBeforeExecute(self, AllowExec);
  if not AllowExec then
  begin
    FReturnValue := -1;
    Execute := FReturnValue;
    exit;
  end;

  flags := 0;

  case FButtons of
    mbxAbortRetryIgnore:
      flags := flags or MB_ABORTRETRYIGNORE;
    mbxOK:
      flags := flags or MB_OK;
    mbxOKCancel:
      flags := flags or MB_OKCANCEL;
    mbxRetryCancel:
      flags := flags or MB_RETRYCANCEL;
    mbxYesNo:
      flags := flags or MB_YESNO;
    mbxYesNoCancel:
      flags := flags or MB_YESNOCANCEL;
  end;

  case FIcon of
    mbxIconExclamation:
      flags := flags or MB_ICONEXCLAMATION;
    mbxIconWarning:
      flags := flags or MB_ICONWARNING;
    mbxIconInformation:
      flags := flags or MB_ICONINFORMATION;
    mbxIconAsterisk:
      flags := flags or MB_ICONASTERISK;
    mbxIconQuestion:
      flags := flags or MB_ICONQUESTION;
    mbxIconStop:
      flags := flags or MB_ICONSTOP;
    mbxIconError:
      flags := flags or MB_ICONERROR;
    mbxIconHand:
      flags := flags or MB_ICONHAND;
    mbxNoIcon: {do nothing}
  end;

  case FDefaultButton of
    mbxDefButton1:
      flags := flags or MB_DEFBUTTON1;
    mbxDefButton2:
      flags := flags or MB_DEFBUTTON2;
    mbxDefButton3:
      flags := flags or MB_DEFBUTTON3;
    mbxNoDefButton:
  end;

  case FModality of
    mbxApplModal, mbxDefModality:
      begin
        hWnd := GetFocus;
        flags := flags or MB_APPLMODAL;
      end;
    mbxSystemModal:
      begin
        hWnd := GetFocus;
        flags := flags or MB_SYSTEMMODAL;
      end;
    mbxTaskModal:
      begin
        hWnd := 0;
        flags := flags or MB_TASKMODAL;
      end;
    mbxNoModality: hWnd := GetDesktopWindow;
    else
      hWnd := GetFocus;
  end;

  if FTextAlignment = mbxRight then
    flags := flags or MB_RIGHT;

  if FOnTop then
    flags := flags or MB_TOPMOST;

  GetMem(msgCaption, Length(FCaption) + 1);
  GetMem(msgText, Length(FText) + 1);
  StrPLCopy(msgCaption, FCaption, Length(FCaption));
  StrPLCopy(msgText, FText, Length(FText));

  FReturnValue := Windows.MessageBox(hWnd, msgText, msgCaption, flags);

  FreeMem(msgText);
  FreeMem(msgCaption);

  Execute := FReturnValue;

  if Assigned(FOnAfterExecute) then
    FOnAfterExecute(self);
end;

procedure Register;
begin
  RegisterComponents('TombViewer Components', [TMessageBox]);
end;

end.

