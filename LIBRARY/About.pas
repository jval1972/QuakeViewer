unit About;

{ This is a dialog wrapped in the ABOUTDLG unit
  not intended to be used as a standalone unit.

  Do not install this unit, install ABOUTDLG.
  }

interface

uses WinTypes, WinProcs, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, AppEvnts;

type
  TAboutBox = class(TForm)
    Button1: TButton;
    WinType: TLabel;
    WinVersion: TLabel;
    Label5: TLabel;
    FreeMemory: TLabel;
    Label6: TLabel;
    FreeResources: TLabel;
    UserLabel: TLabel;
    UserName: TLabel;
    CompanyLabel: TLabel;
    CompanyName: TLabel;
    DiskFreeSpaceLabel: TLabel;
    FreeDisk: TLabel;
    Panel1: TPanel;
    ProgramIcon: TImage;
    ProductName: TLabel;
    Version: TLabel;
    Copyright: TLabel;
    Image1: TImage;
    PlusVersion: TLabel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Comments: TLabel;
    ApplicationEvents1: TApplicationEvents;
    procedure ApplicationEvents1Activate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

procedure TAboutBox.ApplicationEvents1Activate(Sender: TObject);
begin
  if Visible then BringToFront
end;

end.

