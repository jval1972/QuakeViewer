unit Aboutdlg;

interface

uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, About, Registry;

type
  TAboutDialog = class(TComponent)
  private
    FProductName: string;
    FVersion: string;
    FCopyright: string;
    FComments: string;
  public
    function Execute: Boolean;
  published
    property ProductName: string read FProductName write FProductName;
    property Version: string read FVersion write FVersion;
    property Copyright: string read FCopyright write FCopyright;
    property Comments: string read FComments write FComments;
  end;

procedure Register;

implementation

{$R *.dcr}

procedure Register;
begin
  RegisterComponents('TombViewer Components', [TAboutDialog]);
end;

resourceString
  rsFmtDiskFreeMB = '%d MB (%s:\)';
  rsFmtDiskFreeGB = '%d GB (%s:\)';
  rsWinkey = '\SOFTWARE\Microsoft\Windows\CurrentVersion';
  rsWinNTkey = '\SOFTWARE\Microsoft\Windows NT\CurrentVersion';
  rsProductName = 'ProductName';
  rsCurrentVersion = 'CurrentVersion';
  rsCurrentBuildNumber = 'CurrentBuildNumber';
  rsRegisteredOwner = 'RegisteredOwner';
  rsRegisteredOrganization = 'RegisteredOrganization';
  rsVersion = 'Version';
  rsVersionNumber = 'VersionNumber';
  rsPlusVersionNumber = 'Plus! VersionNumber';
  rsFmtVersion = 'Version: %s';
  rsFmtBuild = 'Build: %s';
  rsDiskFreeSpaceLabel = 'Disk free space: ';
  rsCompanyLabel = 'Company: ';
  rsUserLabel = 'User: ';

function TAboutDialog.Execute: Boolean;
var
  reg: TRegistry;
  s: string;
  AboutBox: TAboutBox;
  dsnum: int64;
begin
  AboutBox := TAboutBox.Create(Application.MainForm);
  try
    AboutBox.ProductName.Caption := ProductName;
    AboutBox.Version.Caption := Version;
    AboutBox.Copyright.Caption := Copyright;
    AboutBox.Comments.Caption := Comments;
    AboutBox.Caption := ProductName;

    AboutBox.DiskFreeSpaceLabel.Caption := rsDiskFreeSpaceLabel;
    AboutBox.CompanyLabel.Caption := rsCompanyLabel;
    AboutBox.UserLabel.Caption := rsUserLabel;

    reg := TRegistry.Create;
    try
      reg.RootKey := HKey_Local_Machine;
      if reg.OpenKey(rsWinNTkey,false) and (reg.ReadString(rsProductName)<>'') then
      begin
        AboutBox.WinType.Caption := reg.ReadString(rsProductName);
        AboutBox.WinVersion.Caption := Format(rsFmtVersion, [reg.ReadString(rsCurrentVersion)]);
        AboutBox.PlusVersion.Caption := Format(rsFmtBuild, [reg.ReadString(rsCurrentBuildNumber)]);
        AboutBox.UserName.Caption := reg.ReadString(rsRegisteredOwner);
        AboutBox.CompanyName.Caption := reg.ReadString(rsRegisteredOrganization);
      end
      else if reg.OpenKey(rsWinkey,false) then
      begin
        AboutBox.WinType.Caption := reg.ReadString(rsVersion);
        AboutBox.WinVersion.Caption := reg.ReadString(rsVersionNumber);
        AboutBox.PlusVersion.Caption := reg.ReadString(rsPlusVersionNumber);
        AboutBox.UserName.Caption := reg.ReadString(rsRegisteredOwner);
        AboutBox.CompanyName.Caption := reg.ReadString(rsRegisteredOrganization);
      end;
    finally
      reg.Free;
    end;

    GetDir(0, s); // Current drive's directory   

    dsnum := DiskFree(0) div 1000000;
    if dsnum > 100000 then
    begin
      dsnum := dsnum div 1000;
      AboutBox.FreeDisk.Caption := Format(rsFmtDiskFreeGB, [dsnum, s[1]]);
    end
    else
      AboutBox.FreeDisk.Caption := Format(rsFmtDiskFreeMB, [dsnum, s[1]]);

    AboutBox.ProgramIcon.Picture.Graphic := Application.Icon;
    AboutBox.ShowModal;
    Result := (AboutBox.ModalResult = mrOK);
  finally
    AboutBox.Free;
  end;
end;

end.
