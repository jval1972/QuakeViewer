//------------------------------------------------------------------------------
//
//  Surfaces Engine (SE) - Gaming engine for Windows based on DirectX & DelphiX
//  Copyright (C) 1999-2004, 2018 by Jim Valavanis
//
//  This program is free software; you can redistribute it and/or
//  modify it under the terms of the GNU General Public License
//  as published by the Free Software Foundation; either version 2
//  of the License, or (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
// DESCRIPTION:
//  various utilities functions
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//------------------------------------------------------------------------------

unit se_Utils;

interface

function CopyFile(const sname, dname: string): boolean;

procedure BackupFile(const fname: string);

function MkShortName(const fname: string): string;

procedure VisitHtmlPage(const handle: integer; const pg: string);

implementation

uses
  Windows, SysUtils, Classes, ShellApi;

function CopyFile(const sname, dname: string): boolean;
var
  FromF, ToF: file;
  NumRead, NumWritten: Integer;
  Buf: array[1..8192] of Char;
begin
  if FileExists(sname) then
  begin
    AssignFile(FromF, sname);
    Reset(FromF, 1);
    AssignFile(ToF, dname);
    Rewrite(ToF, 1);
    repeat
      BlockRead(FromF, Buf, SizeOf(Buf), NumRead);
      BlockWrite(ToF, Buf, NumRead, NumWritten);
    until (NumRead = 0) or (NumWritten <> NumRead);
    CloseFile(FromF);
    CloseFile(ToF);
    Result := True;
  end
  else
    Result := False;
end;

procedure BackupFile(const fname: string);
var
  fbck: string;
begin
  if not FileExists(fname) then
    exit;
  fbck := fname + '_bak';
  CopyFile(fname, fbck);
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

procedure VisitHtmlPage(const handle: integer; const pg: string);
begin
  ShellExecute(
    handle,
      'open',
        PChar(pg),
          nil, nil, SW_SHOWNORMAL);
end;

end.
