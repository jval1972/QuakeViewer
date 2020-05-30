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
//  WAD container file format (Custom - not to be confused with the Doom game file)
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//------------------------------------------------------------------------------

{$I defs.inc}

unit se_WADS;

interface

uses Windows, SysUtils, Classes, Forms, zLib, binarydata;

type
  PWadInfo = ^TWadInfo;
  TWadInfo = record
    Start,
    Size : LongInt;
    Entry : string[255];
  end;

  TDir = array[0..$FFFF-1] of PWadInfo;

  TWadCreateMode = (wcmNone, wcmFile, wcmStream, wcmEntry);

procedure SaveWadFile(FileName: TFileName; nodes: TStrings; NumIgnore: integer = 0;
  cmp: TCompressionLevel = clDefault; ProgressProc: TZProgressEvent=nil);

procedure AppendWadFile(FileName: TFileName; nodes: TStrings; NumIgnore: integer = 0;
  cmp: TCompressionLevel = clDefault; ProgressProc: TZProgressEvent=nil);

procedure UpdateWadFile(FileName: TFileName; node: string; Stream: TStream; NumIgnore: integer = 0;
  cmp: TCompressionLevel = clDefault; ProgressProc: TZProgressEvent=nil);

procedure WadFileEntryCopyToStream(WadFileName: TFileName; Entry: string;
  stream: TStream; ProgressProc: TZProgressEvent=nil);

procedure GetWadEntries(FileName: TFileName; nodes: TStrings);

type
  TWad = class
  private
    fStream: TStream;
    DIR: TDir;
    NumEntries: integer;
    mode: TWadCreateMode;
    function Search(Entry: string): integer;
    procedure InitStream;
  public
    constructor Create; virtual;
    constructor CreateFromFile(FileName: TFileName); virtual;
    constructor CreateFromStream(Stream: TStream); virtual;
    constructor CreateFromEntry(Wad: TWad; Entry: string); virtual;
    function CopyToStream(Entry: string; s: TStream): boolean; virtual;
    function EntryExist(Entry: string): boolean; virtual;
    procedure GetEntries(Entries: TStrings);
    destructor Destroy; override;
    property Stream: TStream read fStream;
  end;

function IgnoreCharacters(s: string; NumChars: integer): string;

implementation

uses
  se_DXClasses;

resourceString
  rsWAD = 'PWAD';
  rsNotWADFile = 'Invalid file type';
  rsEntryNotFound = 'Entry not found inside WAD file';

function IgnoreCharacters(s: string; NumChars: integer): string;
begin
  Result := Copy(s, NumChars + 1, Length(s) - NumChars);
end;

procedure QuickSort(var d: TDir; entries: integer);
var tw: TWadInfo;

  procedure DoQuickSort(left, right: integer); register;
  var l, r: integer;
  begin
    if right > left then
    begin
      l := left;
      r := right;
      while l < r do
      begin
        while UpperCase(d[r].Entry) > UpperCase(d[left].Entry) do dec(r);
        while (UpperCase(d[l].Entry) <= UpperCase(d[left].Entry)) and (l < r) do inc(l);
        if l < r then
        begin
          tw := d[l]^;
          d[l]^ := d[r]^;
          d[r]^ := tw;
        end;
      end;
      tw := d[left]^;
      d[left]^ := d[r]^;
      d[r]^ := tw;
      DoQuickSort(left, r - 1);
      DoQuickSort(r + 1, right)
    end
  end;

begin
  DoQuickSort(0, entries - 1);
end;

{ Δημιουργεί ένα WAD file στο αρχείο FileName με entries αρχείων
  στο nodes }
procedure SaveWadFile(FileName: TFileName; nodes: TStrings; NumIgnore: integer = 0;
  cmp: TCompressionLevel = clDefault; ProgressProc: TZProgressEvent=nil);
var BinaryData1: TBinaryData;
    f: TFileStream;
    i,l,j: integer;
    s: string;
    m: TMemoryStream;
    DIR: array[0..$FFFF-1] of PWadInfo;
begin
  f := TFileStream.Create(FileName, fmCreate);
  try
    s := rsWAD;
    for i := 1 to Length(s) do
      f.Write(s[i], Length(s[i]));
    l := 0;
    f.Write(l, SizeOf(l)); // Start of Directory
    l := nodes.Count;
    f.Write(l, SizeOf(l)); // Number of Entries
    for i := 0 to nodes.Count - 1 do
    begin
      new(DIR[i]);
      FillChar(DIR[i]^, SizeOf(TWadInfo), Chr(0));
      BinaryData1 := TBinaryData.Create;
      BinaryData1.OnProgress := ProgressProc;
      m := TMemoryStream.Create;
      try
        BinaryData1.LoadFromFile(nodes[i]);
        BinaryData1.CompressionLevel := cmp;
        BinaryData1.SaveToStream(m, true);
        m.Position := 0;
        Dir[i]^.Start := f.Position;
        Dir[i]^.Size := m.Size;
        Dir[i]^.Entry := IgnoreCharacters(nodes[i], NumIgnore);
        f.CopyFrom(m, m.Size);
      finally
        BinaryData1.Free;
        m.Free;
      end;
    end;
    l := f.Position;
    BinaryData1 := TBinaryData.Create;
    BinaryData1.OnProgress := ProgressProc;
    m := TMemoryStream.Create;
    try
      m.Size := nodes.Count * SizeOf(TWadInfo);
      m.Seek(0, soFromBeginning);
      for j := 0 to nodes.Count - 1 do m.Write(DIR[j]^, SizeOf(TWadInfo));
      m.Seek(0, soFromBeginning);
      BinaryData1.LoadFromStream(m, false);
      BinaryData1.CompressionLevel := cmp;
      m.Clear;
      BinaryData1.SaveToStream(m, true);
      m.Seek(0, soFromBeginning);
      f.CopyFrom(m, m.Size);
    finally
      BinaryData1.Free;
      m.Free;
    end;

    f.Seek(Length(rsWAD), soFromBeginning);
    f.Write(l, SizeOf(l)); // Start of Directory
    for i := 0 to nodes.Count - 1 do dispose(DIR[i]);
  finally
    f.Free;
  end;
end;

// Προσθέτει στοιχεία σε ένα αρχείο WAD
// Προσοχή δεν ενημερώνει!!!!
procedure AppendWadFile(FileName: TFileName; nodes: TStrings; NumIgnore: integer = 0;
  cmp: TCompressionLevel = clDefault; ProgressProc: TZProgressEvent=nil);
var
  BinaryData1: TBinaryData;
  f: TFileStream;
  i, l, e, j: integer;
  s: string;
  m: TMemoryStream;
  DIR: array[0..$FFFF-1] of PWadInfo;
  exist: boolean;
  c: char;
begin
  f := TFileStream.Create(FileName, fmOpenReadWrite);
  try
    s := '';
    for i := 1 to Length(rsWad) do
    begin
      f.Read(c, SizeOf(c));
      s := s + c;
    end;
    if s <> rsWad then
      raise Exception.Create(rsNotWADFile)
    else
    begin
      f.Read(l, SizeOf(l)); // Start of Directory
      f.Read(e, SizeOf(e)); // Number of Entries
      // Διαβάζουμε το Directory
      f.Seek(l, soFromBeginning);
      m := TMemoryStream.Create;
      try
        BinaryData1 := TBinaryData.Create;
        BinaryData1.OnProgress := ProgressProc;
        try
          m.Size := f.Size - f.Position;
          m.Seek(0, soFromBeginning);
          m.CopyFrom(f, f.Size - f.Position);
          m.Seek(0, soFromBeginning);
          BinaryData1.LoadFromStream(m, true);
          m.Clear;
          BinaryData1.SaveToStream(m, false); // Αποσυμπίεση καταλόγου
        finally
          BinaryData1.Free;
        end;
        m.Seek(0, soFromBeginning);
        for i := 0 to e - 1 do
        begin
          new(DIR[i]);
          m.Read(DIR[i]^, SizeOf(DIR[i]^));
        end;
      finally
        m.Free;
      end;
      // Ξαναγυρνάμε στην αρχή του Directory
      f.Size := l;
      f.Seek(l, soFromBeginning);
      for i := 0 to nodes.Count - 1 do
      begin
        exist := false;
        for j := 0 to e - 1 do
          if DIR[j].Entry = nodes[i] then
            exist := true;
        if not exist then // Ενημερώνουμε το WAD μόνο όταν δεν υπάρχει το Entry
        begin
          inc(e);
          new(DIR[e-1]);
          FillChar(DIR[e-1]^, SizeOf(TWadInfo), Chr(0));
          BinaryData1 := TBinaryData.Create;
          BinaryData1.OnProgress := ProgressProc;
          m := TMemoryStream.Create;
          try
            BinaryData1.LoadFromFile(nodes[i]);
            BinaryData1.CompressionLevel := cmp;
            BinaryData1.SaveToStream(m, true);
            m.Position := 0;
            Dir[e-1]^.Start := f.Position;
            Dir[e-1]^.Size := m.Size;
            Dir[e-1]^.Entry := IgnoreCharacters(nodes[i], NumIgnore);
            f.CopyFrom(m, m.Size);
          finally
            BinaryData1.Free;
            m.Free;
          end;
        end;
      end;
      l := f.Position;
      BinaryData1 := TBinaryData.Create;
      BinaryData1.OnProgress := ProgressProc;
      m := TMemoryStream.Create;
      try
        m.Size := e * SizeOf(TWadInfo);
        m.Seek(0, soFromBeginning);
        for j := 0 to e - 1 do m.Write(DIR[j]^, SizeOf(TWadInfo));
        m.Seek(0, soFromBeginning);
        BinaryData1.LoadFromStream(m, false);
        BinaryData1.CompressionLevel := cmp;
        m.Clear;
        BinaryData1.SaveToStream(m, true);
        m.Seek(0, soFromBeginning);
        f.CopyFrom(m, m.Size);
      finally
        BinaryData1.Free;
        m.Free;
      end;

      f.Seek(Length(rsWAD), soFromBeginning);
      f.Write(l, SizeOf(l)); // Start of Directory
      f.Write(e, SizeOf(e)); // Number of Entries
      for i := 0 to e - 1 do dispose(DIR[i]);
    end;
  finally
    f.Free;
  end;
end;

// Κάνει update το WAD file κατά ένα entry (node)
// Τα στοιχεία βρίσκονται στο stream );
// Αν δεν υπάρχει το αρχείο, το δημιουργεί
procedure UpdateWadFile(FileName: TFileName; node: string; Stream: TStream; NumIgnore: integer = 0;
  cmp: TCompressionLevel = clDefault; ProgressProc: TZProgressEvent=nil);
var
  BinaryData1: TBinaryData;
  f: TFileStream;
  i, l, e, j: integer;
  s: string;
  m: TMemoryStream;
  DIR: array[0..$FFFF-1] of PWadInfo;
  exist: boolean;
  c: char;
  sL: TStringList;
begin
  if not FileExists(FileName) then
  begin
    sL := TStringList.Create;
    try
      SaveWadFile(FileName, sL);
    finally
      sL.Free;
    end;
  end;
  f := TFileStream.Create(FileName, fmOpenReadWrite);
  try
    s := '';
    for i := 1 to Length(rsWad) do
    begin
      f.Read(c, SizeOf(c));
      s := s + c;
    end;
    if s <> rsWad then
      raise Exception.Create(rsNotWADFile)
    else
    begin
      f.Read(l, SizeOf(l)); // Start of Directory
      f.Read(e, SizeOf(e)); // Number of Entries
      // Διαβάζουμε το Directory
      f.Seek(l, soFromBeginning);
      m := TMemoryStream.Create;
      try
        BinaryData1 := TBinaryData.Create;
        BinaryData1.OnProgress := ProgressProc;
        try
          m.Size := f.Size - f.Position;
          m.Seek(0, soFromBeginning);
          m.CopyFrom(f, f.Size - f.Position);
          m.Seek(0, soFromBeginning);
          BinaryData1.LoadFromStream(m, true);
          m.Clear;
          BinaryData1.SaveToStream(m, false); // Αποσυμπίεση καταλόγου
        finally
          BinaryData1.Free;
        end;
        m.Seek(0, soFromBeginning);
        for i := 0 to e - 1 do
        begin
          new(DIR[i]);
          m.Read(DIR[i]^, SizeOf(DIR[i]^));
        end;
      finally
        m.Free;
      end;
      // Ξαναγυρνάμε στην αρχή του Directory
      f.Size := l;
      f.Seek(l, soFromBeginning);

      exist := false;
      i := e; // Σε περίπτωση που δεν βρεθεί έχουμε την τιμή του i = e (στο τέλος του DIR)
      for j := 0 to e - 1 do
        if DIR[j].Entry = node then
        begin
          i := j; // Αν βρεθεί κρατάμε στο i το σημείο που βρέθηκε
          exist := true;
        end;

      if not exist then
      begin
        inc(e);
        new(DIR[e-1]);
        FillChar(DIR[e-1]^, SizeOf(TWadInfo), Chr(0));
      end;

      BinaryData1 := TBinaryData.Create;
      BinaryData1.OnProgress := ProgressProc;
      m := TMemoryStream.Create;
      try
        Stream.Position := 0;
        BinaryData1.LoadFromStream(Stream, false);
        BinaryData1.CompressionLevel := cmp;
        BinaryData1.SaveToStream(m, true);
        m.Position := 0;
        Dir[i]^.Start := f.Position; // Η νέα καταχώρηση στην αρχή του Directory
        Dir[i]^.Size := m.Size;
        Dir[i]^.Entry := IgnoreCharacters(node, NumIgnore);
        f.CopyFrom(m, m.Size);
      finally
        BinaryData1.Free;
        m.Free;
      end;

      l := f.Position;
      BinaryData1 := TBinaryData.Create;
      BinaryData1.OnProgress := ProgressProc;
      m := TMemoryStream.Create;
      try
        m.Size := e * SizeOf(TWadInfo);
        m.Seek(0, soFromBeginning);
        for j := 0 to e - 1 do m.Write(DIR[j]^, SizeOf(TWadInfo));
        m.Seek(0, soFromBeginning);
        BinaryData1.LoadFromStream(m, false);
        BinaryData1.CompressionLevel := cmp;
        m.Clear;
        BinaryData1.SaveToStream(m, true);
        m.Seek(0, soFromBeginning);
        f.CopyFrom(m, m.Size);
      finally
        BinaryData1.Free;
        m.Free;
      end;

      f.Seek(Length(rsWAD), soFromBeginning);
      f.Write(l, SizeOf(l)); // Start of Directory
      f.Write(e, SizeOf(e)); // Number of Entries
      for i := 0 to e - 1 do dispose(DIR[i]);
    end;
  finally
    f.Free;
  end;
end;

procedure WadFileEntryCopyToStream(WadFileName: TFileName; Entry: string;
  stream: TStream; ProgressProc: TZProgressEvent=nil);
var
  BinaryData1: TBinaryData;
  f: TFileStream;
  s: string;
  c: Char;
  l, e, i: integer;
  Dir: TWadInfo;
  m: TStream;
begin
  f := TFileStream.Create(WadFileName, fmOpenRead or fmShareDenyWrite);
  s := '';
  for i := 1 to Length(rsWad) do
  begin
    f.Read(c, SizeOf(c));
    s := s + c;
  end;
  if s <> rsWad then
    raise Exception.Create(rsNotWADFile)
  // not a WAD file
  else
  begin
    f.Read(l, SizeOf(l));  // Start of Directory
    f.Read(e, SizeOf(e));  // Number of Entries
    f.Seek(l, soFromBeginning);

    BinaryData1 := TBinaryData.Create;
    BinaryData1.OnProgress := ProgressProc;
    m := TMemoryStream.Create;
    m.Size := f.Size - f.Position;
    m.Seek(0, soFromBeginning);
    m.CopyFrom(f, f.Size - f.Position);
    m.Seek(0, soFromBeginning);
    BinaryData1.LoadFromStream(m, true);
    try
      (m as TMemoryStream).Clear;
      BinaryData1.SaveToStream(m, false);
    finally
      BinaryData1.Free;
    end;
    m.Seek(0, soFromBeginning);
    FillChar(Dir, SizeOf(Dir), Chr(0));
    i := 0;
    repeat
      m.Read(Dir, SizeOf(Dir));
      inc(i);
    until (i = e) or (UpperCase(Dir.Entry) = UpperCase(Entry));
    m.Free;
    if UpperCase(Dir.Entry) = UpperCase(Entry) then
    begin
      f.Seek(Dir.Start, soFromBeginning);
      m := TSubStream.Create(f, Dir.Start, Dir.Size);
      BinaryData1 := TBinaryData.Create;
      try
        BinaryData1.OnProgress := ProgressProc;
        BinaryData1.LoadFromStream(m, true);
        BinaryData1.SaveToStream(stream, false);
      finally
        BinaryData1.Free;
        m.Free;
      end;
{      m := TMemoryStream.Create;
      m.CopyFrom(f, Dir.Size);
      BinaryData1 := TBinaryData.Create(nil);
      BinaryData1.OnProgress := ProgressProc;
      m.Seek(0, soFromBeginning);
      BinaryData1.LoadFromStream(m, true);
      BinaryData1.SaveToStream(stream, false);
      BinaryData1.Free;
      m.Free;}
    end
    else
    // Entry not found
      raise Exception.Create(rsEntryNotFound + ':' + Chr(13) + Chr(10) + Entry);
  end;
end;

{ *** TWad *** }

constructor TWad.Create;
var
  s: string;
  i: integer;
begin
  fStream := TMemoryStream.Create;
  fStream.Size := Length(rsWAD) + 2 * SizeOf(i);
  fStream.Seek(0, soFromBeginning);
  NumEntries := 0;
  mode := wcmNone;
  s := rsWAD;
  for i := 1 to Length(s) do
    fStream.Write(s[i], Length(s[i]));
  i := 0;
  fStream.Write(i, SizeOf(i)); // Start of Directory
  fStream.Write(i, SizeOf(i)); // Number of Entriesl;
end;

constructor TWad.CreateFromFile(FileName: TFileName);
begin
  fStream := TFileStream.Create(FileName, fmOpenRead	or fmShareDenyWrite);
  mode := wcmFile;
  InitStream;
end;

constructor TWad.CreateFromStream(Stream: TStream);
begin
  fStream := Stream;
  mode := wcmStream;
  InitStream;
end;

constructor TWad.CreateFromEntry(Wad: TWad; Entry: string);
begin
  fStream := TMemoryStream.Create;
  Wad.CopyToStream(Entry, fStream);
  mode := wcmEntry;
  InitStream;
end;

procedure TWad.InitStream;
var
  s: string;
  i: integer;
  c: char;
  start: integer;
  m: TMemoryStream;
  BinaryData1: TBinaryData;
begin
  s := '';
  for i := 1 to Length(rsWad) do
  begin
    fStream.Read(c, SizeOf(c));
    s := s + c;
  end;
  // not a WAD file
  if s <> rsWad then
  begin
    raise Exception.Create(rsNotWADFile);
    NumEntries := 0;
  end
  else
  begin
    fStream.Read(start, SizeOf(start));
    fStream.Read(NumEntries, SizeOf(NumEntries));
    fStream.Seek(start, soFromBeginning);

    m := TMemoryStream.Create;
    try
      m.Size := fStream.Size - fStream.Position;
      m.Seek(0, soFromBeginning);
      m.CopyFrom(fStream, fStream.Size - fStream.Position);
      m.Seek(0, soFromBeginning);

      BinaryData1 := TBinaryData.Create;
      try
        BinaryData1.LoadFromStream(m, true);
        m.Clear;
        BinaryData1.SaveToStream(m, false);
      finally
        BinaryData1.Free;
      end;

      m.Seek(0, soFromBeginning);
      for i := 0 to NumEntries - 1 do
      begin
        new(DIR[i]);
        m.Read(DIR[i]^,SizeOf(DIR[i]^));
      end;
      QuickSort(DIR, NumEntries);
    finally
      m.Free;
    end;
  end;
end;

function TWad.Search(Entry: string): integer;
// Κάνει διαδική αναζήτηση στα ταξινομημένα Entries του DIR
var
  tmp: string;

  function DoSearch(first, last: integer): integer;
  begin
    tmp := UpperCase(DIR[(first + last) div 2].Entry);
    if tmp = UpperCase(Entry) then
      Result := (first + last) div 2
    else if first >= last then Result := -1
    else if tmp < UpperCase(Entry) then
      Result := DoSearch((first + last) div 2 + 1, last)
    else
      Result := DoSearch(first, (first + last) div 2 - 1)
  end;

begin
  Result := DoSearch(0, NumEntries - 1);
end;

function TWad.CopyToStream(Entry: string; s: TStream): boolean;
{ Αντιγράφει το Entry στο Stream }
var
  i: integer;
  m: TSubStream;
  BinaryData1: TBinaryData;
  W: TWad;
begin
  s.Seek(0, soFromBeginning);
  s.Size := 0;
  if Pos('|', Entry) = 0 then
  begin
    i := Search(Entry);
    if i <> -1 then
    begin

      fStream.Seek(DIR[i].Start, soFromBeginning);

      m := TSubStream.Create(fStream, DIR[i].Start, DIR[i].Size);
      BinaryData1 := TBinaryData.Create;
      try
        BinaryData1.LoadFromStream(m, true);
        BinaryData1.SaveToStream(s, false);
        s.Seek(0, soFromBeginning);
        Result := true;
      finally
        BinaryData1.Free;
        m.Free;
      end;
    end
    else
    begin
      // Entry not found
      raise Exception.Create(rsEntryNotFound + ':' + Chr(13) + Chr(10) + Entry);
      Result := false;
    end;
  end
  else // Sub WAD (Αρχείο WAD μέσα σε WAD)
  begin
    W := TWad.CreateFromEntry(self, Copy(Entry, 1, Pos('|', Entry) - 1));
    try
      Result := W.CopyToStream(Copy(Entry, Pos('|', Entry) + 1, Length(Entry) - Pos('|', Entry)), s);
    finally
      W.Free;
    end;
  end;
end;

procedure TWad.GetEntries(Entries: TStrings);
var
  i: integer;
begin
  Entries.Clear;
  for i := 0 to NumEntries - 1 do Entries.Add(DIR[i].Entry)
end;

function TWad.EntryExist(Entry: string): boolean;
begin
  Result := Search(Entry) <> -1;
end;

destructor TWad.Destroy;
var
  i: integer;
begin
  if mode in [wcmNone, wcmFile, wcmEntry] then fStream.Free;
  for i := 0 to NumEntries - 1 do Dispose(DIR[i]);
  inherited;
end;

procedure GetWadEntries(FileName: TFileName; nodes: TStrings);
var
  wad: TWad;
begin
  wad := TWad.CreateFromFile(FileName);
  try
    wad.GetEntries(nodes);
  finally
    wad.Free;
  end;
end;

end.

