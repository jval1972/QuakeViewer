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
//  Various utility classes
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//------------------------------------------------------------------------------

{$I defs.inc}
unit se_DXClasses;

interface

uses
  SysUtils, Classes, Graphics;

type
  TSubStream = class(TStream)
  protected
    fParent: TStream;
    fSubPos,
    fSubSize: LongInt;
    fPosition: LongInt;
  public
    constructor Create(aParent: TStream; aSubPos, aSubSize: LongInt); virtual;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function AbsolutPosition: LongInt; virtual;
  end;

  ESSCreateError = class(EStreamError);

  TCachedFileStream = class(TFileStream)
  private
    fBufSize: integer;
    fBuffer: pointer;
    fPosition: integer;
    fBufferStart: integer;
    fBufferEnd: integer;
    fSize: integer;
    fInitialized: boolean;
  protected
    procedure SetSize(NewSize: Longint); override;
    procedure ResetBuffer; virtual;
  public
    constructor Create(const FileName: string; mode: word; ABufSize: integer = $FFFF); virtual;
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function ReadNoBuffer(var Buffer; Count: Longint): Longint; virtual;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
  end;

  TDXStringList = class(TStringList)
  protected
    fLastIndex: integer;
    procedure Put(Index: Integer; const S: string); override;
  public
    constructor Create; virtual;
    procedure Insert(Index: Integer; const S: string); override;
    function IndexOfFromLast(const S: string): Integer; virtual;
  end;

{
  TDXBitmap = class(TBitmap)
  public
    procedure SetDimentions(aWidth, aHeight: Integer); virtual;
  end;
}
implementation

uses
  Windows;

resourceString
  rsSSCreateError = 'Cannot create TSubStream class with nil parent';

constructor TSubStream.Create(aParent: TStream; aSubPos, aSubSize: LongInt);
begin
  if Assigned(aParent) then
  begin
    Inherited Create;
    fParent := aParent;
    fSubPos := aSubPos;
    fSubSize := aSubSize;
    fPosition := 0;
  end
  else
    raise ESSCreateError.Create(rsSSCreateError);
end;

function TSubStream.Read(var Buffer; Count: Longint): Longint;
var fParentPosition: LongInt;
begin
  fParentPosition := fParent.Position;
  fParent.Seek(fPosition + fSubPos, soFromBeginning);
  Result := fParent.Read(Buffer, Count);
  inc(fPosition, Count);
  fParent.Seek(fParentPosition, soFromBeginning);
end;

function TSubStream.Write(const Buffer; Count: Longint): Longint;
var fParentPosition: LongInt;
begin
  fParentPosition := fParent.Position;
  fParent.Seek(fPosition + fSubPos, soFromBeginning);
  Result := fParent.Write(Buffer, Count);
  inc(fPosition, Count);
  fParent.Seek(fParentPosition, soFromBeginning);
end;

function TSubStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  case Origin of
    soFromBeginning: fPosition := Offset;
    soFromCurrent: Inc(fPosition, Offset);
    soFromEnd: fPosition := fSubSize + Offset;
  end;
  Result := fPosition;
end;

function TSubStream.AbsolutPosition: LongInt;
begin
  Result := fPosition + fSubPos;
  if fParent.InheritsFrom(TSubStream) then
    Result := Result + (fParent as TSubStream).AbsolutPosition;
end;

////////////////////////////////////////////////////////////////////////////////

constructor TCachedFileStream.Create(const FileName: string; mode: word; ABufSize: integer = $FFFF);
begin
  fInitialized := false;
  Inherited Create(FileName, mode);
  fBufSize := ABufSize;
  GetMem(fBuffer, fBufSize);
  fPosition := 0;
  ResetBuffer;
  fSize := Inherited Size;
  fInitialized := true;
end;

procedure TCachedFileStream.ResetBuffer;
begin
  fBufferStart := -1;
  fBufferEnd := -1;
end;

destructor TCachedFileStream.Destroy;
begin
  FreeMem(fBuffer, fBufSize);
  Inherited;
end;

function TCachedFileStream.Read(var Buffer; Count: Longint): Longint;
var
  x: Longint;
begin
// Buffer hit
  if (fPosition >= fBufferStart) and (fPosition + Count <= fBufferEnd) then
  begin
    x := LongInt(fBuffer) + fPosition - fBufferStart;
    Move(Pointer(x)^, Buffer, Count);
    fPosition := fPosition + Count;
    Result := Count;
  end
// Non Buffer hit, cache buffer
  else if Count <= fBufSize then
  begin
    fPosition := Inherited Seek(fPosition, soFromBeginning);
    x := Inherited Read(fBuffer^, fBufSize);
    if x < Count then
      Result := x
    else
      Result := Count;
    Move(fBuffer^, Buffer, Count);
    fBufferStart := fPosition;
    fBufferEnd := fPosition + x;
    fPosition := fPosition + Result;
  end
// Keep old buffer
  else
  begin
    fPosition := Inherited Seek(fPosition, soFromBeginning);
    Result := Inherited Read(Buffer, Count);
    fPosition := fPosition + Result;
  end;
end;

function TCachedFileStream.ReadNoBuffer(var Buffer; Count: Longint): Longint;
begin
  fPosition := Inherited Seek(fPosition, soFromBeginning);
  Result := Inherited Read(Buffer, Count);
  fPosition := fPosition + Result;
end;

function TCachedFileStream.Write(const Buffer; Count: Longint): Longint;
begin
  fPosition := Inherited Seek(fPosition, soFromBeginning);
  Result := Inherited Write(Buffer, Count);
  fPosition := fPosition + Result;
  if fSize < fPosition then
    fSize := fPosition;
end;

function TCachedFileStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  if fInitialized then
  begin
    case Origin of
      soFromBeginning: fPosition := Offset;
      soFromCurrent: Inc(fPosition, Offset);
      soFromEnd: fPosition := fSize + Offset;
    end;
    Result := fPosition;
  end
  else
    Result := Inherited Seek(Offset, Origin);
end;

procedure TCachedFileStream.SetSize(NewSize: Longint);
begin
  Inherited;
  fSize := NewSize;
end;

////////////////////////////////////////////////////////////////////////////////
constructor TDXStringList.Create;
begin
  fLastIndex := -1;
  Inherited Create;
end;

procedure TDXStringList.Put(Index: Integer; const S: string);
begin
  if Sorted then
    Sorted := false;
  Inherited;
end;

procedure TDXStringList.Insert(Index: Integer; const S: string);
begin
  if Sorted then
    Sorted := false;
  Inherited;
end;

function TDXStringList.IndexOfFromLast(const S: string): Integer;
begin
  if (fLastIndex >=0) and (fLastIndex < Count) then
  begin
    Result := fLastIndex;
    if AnsiCompareText(Get(Result), S) = 0 then Exit;
  end;
  for Result := Count - 1 downto 0 do
    if AnsiCompareText(Get(Result), S) = 0 then
    begin
      fLastIndex := Result;
      Exit;
    end;
  Result := -1;
end;

////////////////////////////////////////////////////////////////////////////////
{
procedure TDXBitmap.SetDimentions(aWidth, aHeight: Integer);
var
  DIB: TDIBSection;
begin
  with FImage do
    if (FDIB.dsbm.bmWidth <> aWidth) or
       (FDIB.dsbm.bmHeight <> aHeight) then
    begin
      HandleNeeded;
      DIB := FDIB;
      DIB.dsbm.bmWidth := aWidth;
      DIB.dsbmih.biWidth := aWidth;
      DIB.dsbm.bmHeight := aHeight;
      DIB.dsbmih.biHeight := aHeight;
      CopyImage(FHandle, FPalette, DIB);
      Changed(Self);
    end;
end;
}

end.
