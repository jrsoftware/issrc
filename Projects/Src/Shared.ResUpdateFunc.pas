unit Shared.ResUpdateFunc;

{
  Inno Setup
  Copyright (C) 1997-2016 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Resource update functions used by both Setup and the compiler
}

interface

uses
  Windows, SysUtils, Shared.FileClass;

function ReadSignatureAndChecksumFields(const F: TCustomFile;
  var ASignatureAddress, ASignatureSize, AChecksum: DWORD): Boolean;
function ReadSignatureAndChecksumFields64(const F: TCustomFile;
  var ASignatureAddress, ASignatureSize, AChecksum: DWORD): Boolean;
function SeekToResourceData(const F: TCustomFile; const ResType, ResId: Cardinal): Cardinal;
function UpdateSignatureAndChecksumFields(const F: TCustomFile;
  const ASignatureAddress, ASignatureSize, AChecksum: DWORD): Boolean;
procedure UpdateManifestRequestedExecutionLevel(const F: TCustomFile;
  const RequireAdministrator: Boolean);

implementation

uses
  Shared.Int64Em;

const
  IMAGE_NT_SIGNATURE = $00004550;
  IMAGE_NT_OPTIONAL_HDR32_MAGIC = $10b;
  IMAGE_NT_OPTIONAL_HDR64_MAGIC = $20b;
  IMAGE_NUMBEROF_DIRECTORY_ENTRIES = 16;
  IMAGE_SIZEOF_SHORT_NAME = 8;
  IMAGE_DIRECTORY_ENTRY_RESOURCE = 2;
  IMAGE_DIRECTORY_ENTRY_SECURITY = 4;

type
  PImageFileHeader = ^TImageFileHeader;
  TImageFileHeader = packed record
    Machine: Word;
    NumberOfSections: Word;
    TimeDateStamp: DWORD;
    PointerToSymbolTable: DWORD;
    NumberOfSymbols: DWORD;
    SizeOfOptionalHeader: Word;
    Characteristics: Word;
  end;
  PImageDataDirectory = ^TImageDataDirectory;
  TImageDataDirectory = record
    VirtualAddress: DWORD;
    Size: DWORD;
  end;
  PImageOptionalHeader = ^TImageOptionalHeader;
  TImageOptionalHeader = packed record
    { Standard fields. }
    Magic: Word;
    MajorLinkerVersion: Byte;
    MinorLinkerVersion: Byte;
    SizeOfCode: DWORD;
    SizeOfInitializedData: DWORD;
    SizeOfUninitializedData: DWORD;
    AddressOfEntryPoint: DWORD;
    BaseOfCode: DWORD;
    BaseOfData: DWORD;
    { NT additional fields. }
    ImageBase: DWORD;
    SectionAlignment: DWORD;
    FileAlignment: DWORD;
    MajorOperatingSystemVersion: Word;
    MinorOperatingSystemVersion: Word;
    MajorImageVersion: Word;
    MinorImageVersion: Word;
    MajorSubsystemVersion: Word;
    MinorSubsystemVersion: Word;
    Win32VersionValue: DWORD;
    SizeOfImage: DWORD;
    SizeOfHeaders: DWORD;
    CheckSum: DWORD;
    Subsystem: Word;
    DllCharacteristics: Word;
    SizeOfStackReserve: DWORD;
    SizeOfStackCommit: DWORD;
    SizeOfHeapReserve: DWORD;
    SizeOfHeapCommit: DWORD;
    LoaderFlags: DWORD;
    NumberOfRvaAndSizes: DWORD;
    DataDirectory: packed array[0..IMAGE_NUMBEROF_DIRECTORY_ENTRIES-1] of TImageDataDirectory;
  end;
  PImageOptionalHeader64 = ^TImageOptionalHeader64;
  TImageOptionalHeader64 = packed record
    { Standard fields. }
    Magic: Word;
    MajorLinkerVersion: Byte;
    MinorLinkerVersion: Byte;
    SizeOfCode: DWORD;
    SizeOfInitializedData: DWORD;
    SizeOfUninitializedData: DWORD;
    AddressOfEntryPoint: DWORD;
    BaseOfCode: DWORD;
    { NT additional fields. }
    ImageBase: Integer64;
    SectionAlignment: DWORD;
    FileAlignment: DWORD;
    MajorOperatingSystemVersion: Word;
    MinorOperatingSystemVersion: Word;
    MajorImageVersion: Word;
    MinorImageVersion: Word;
    MajorSubsystemVersion: Word;
    MinorSubsystemVersion: Word;
    Win32VersionValue: DWORD;
    SizeOfImage: DWORD;
    SizeOfHeaders: DWORD;
    CheckSum: DWORD;
    Subsystem: Word;
    DllCharacteristics: Word;
    SizeOfStackReserve: Integer64;
    SizeOfStackCommit: Integer64;
    SizeOfHeapReserve: Integer64;
    SizeOfHeapCommit: Integer64;
    LoaderFlags: DWORD;
    NumberOfRvaAndSizes: DWORD;
    DataDirectory: packed array[0..IMAGE_NUMBEROF_DIRECTORY_ENTRIES-1] of TImageDataDirectory;
  end;
  TISHMisc = packed record
    case Integer of
      0: (PhysicalAddress: DWORD);
      1: (VirtualSize: DWORD);
  end;
  PImageSectionHeader = ^TImageSectionHeader;
  TImageSectionHeader = packed record
    Name: packed array[0..IMAGE_SIZEOF_SHORT_NAME-1] of Byte;
    Misc: TISHMisc;
    VirtualAddress: DWORD;
    SizeOfRawData: DWORD;
    PointerToRawData: DWORD;
    PointerToRelocations: DWORD;
    PointerToLinenumbers: DWORD;
    NumberOfRelocations: Word;
    NumberOfLinenumbers: Word;
    Characteristics: DWORD;
  end;
  TImageResourceDirectory = packed record
    Characteristics: DWORD;
    TimeDateStamp: DWORD;
    MajorVersion: Word;
    MinorVersion: Word;
    NumberOfNamedEntries: Word;
    NumberOfIdEntries: Word;
  end;
  TImageResourceDirectoryEntry = packed record
    Id: DWORD;
    Offset: DWORD;
  end;
  TImageResourceDataEntry = packed record
    OffsetToData: DWORD;
    Size: DWORD;
    CodePage: DWORD;
    Reserved: DWORD;
  end;

procedure Error(const Msg: String);
begin
  raise Exception.Create('Resource update error: ' + Msg);
end;

function SeekToPEHeader(const F: TCustomFile): Boolean;
var
  DosHeader: packed record
    Sig: array[0..1] of AnsiChar;
    Other: array[0..57] of Byte;
    PEHeaderOffset: LongWord;
  end;
  Sig: DWORD;
begin
  Result := False;
  F.Seek(0);
  if F.Read(DosHeader, SizeOf(DosHeader)) = SizeOf(DosHeader) then begin
    if (DosHeader.Sig[0] = 'M') and (DosHeader.Sig[1] = 'Z') and
       (DosHeader.PEHeaderOffset <> 0) then begin
      F.Seek(DosHeader.PEHeaderOffset);
      if F.Read(Sig, SizeOf(Sig)) = SizeOf(Sig) then
        if Sig = IMAGE_NT_SIGNATURE then
          Result := True;
    end;
  end;
end;

function SeekToAndReadPEOptionalHeader(const F: TCustomFile;
  var OptHeader: TImageOptionalHeader; var OptHeaderOffset: Integer64): Boolean;
var
  Header: TImageFileHeader;
begin
  Result := False;
  if SeekToPEHeader(F) then begin
    if (F.Read(Header, SizeOf(Header)) = SizeOf(Header)) and
       (Header.SizeOfOptionalHeader = SizeOf(OptHeader)) then begin
      OptHeaderOffset := F.Position;
      if F.Read(OptHeader, SizeOf(OptHeader)) = SizeOf(OptHeader) then
        if OptHeader.Magic = IMAGE_NT_OPTIONAL_HDR32_MAGIC then
          Result := True;
    end;
  end;
end;

function SeekToAndReadPEOptionalHeader64(const F: TCustomFile;
  var OptHeader: TImageOptionalHeader64; var OptHeaderOffset: Integer64): Boolean;
var
  Header: TImageFileHeader;
begin
  Result := False;
  if SeekToPEHeader(F) then begin
    if (F.Read(Header, SizeOf(Header)) = SizeOf(Header)) and
       (Header.SizeOfOptionalHeader = SizeOf(OptHeader)) then begin
      OptHeaderOffset := F.Position;
      if F.Read(OptHeader, SizeOf(OptHeader)) = SizeOf(OptHeader) then
        if OptHeader.Magic = IMAGE_NT_OPTIONAL_HDR64_MAGIC then
          Result := True;
    end;
  end;
end;

procedure FindResourceSection(const F: TCustomFile;
  var SectionVirtualAddr, SectionPhysOffset, SectionPhysSize: Cardinal);
var
  EXESig: Word;
  PEHeaderOffset, PESig: Cardinal;
  PEHeader: TImageFileHeader;
  PEOptHeader: TImageOptionalHeader;
  PESectionHeader: TImageSectionHeader;
  I: Integer;
begin
  { Read DOS header }
  F.Seek(0);
  F.ReadBuffer(EXESig, SizeOf(EXESig));
  if EXESig <> $5A4D {'MZ'} then
    Error('File isn''t an EXE file (1)');
  F.Seek($3C);
  F.ReadBuffer(PEHeaderOffset, SizeOf(PEHeaderOffset));
  if PEHeaderOffset = 0 then
    Error('File isn''t a PE file (1)');

  { Read PE header & optional header }
  F.Seek(PEHeaderOffset);
  F.ReadBuffer(PESig, SizeOf(PESig));
  if PESig <> $00004550 {'PE'#0#0} then
    Error('File isn''t a PE file (2)');
  F.ReadBuffer(PEHeader, SizeOf(PEHeader));
  if PEHeader.SizeOfOptionalHeader <> SizeOf(PEOptHeader) then
    Error('File isn''t a PE file (3)');
  F.ReadBuffer(PEOptHeader, SizeOf(PEOptHeader));
  if PEOptHeader.Magic <> IMAGE_NT_OPTIONAL_HDR32_MAGIC then
    Error('File isn''t a PE file (4)');

  { Scan section headers for resource section }
  if (PEOptHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_RESOURCE].VirtualAddress = 0) or
     (PEOptHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_RESOURCE].Size = 0) then
    Error('No resources (1)');
  SectionVirtualAddr := PEOptHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_RESOURCE].VirtualAddress;
  SectionPhysOffset := 0;
  for I := 0 to PEHeader.NumberOfSections-1 do begin
    F.ReadBuffer(PESectionHeader, SizeOf(PESectionHeader));
    if (PESectionHeader.VirtualAddress = SectionVirtualAddr) and
       (PESectionHeader.SizeOfRawData <> 0) then begin
      SectionPhysOffset := PESectionHeader.PointerToRawData;
      SectionPhysSize := PESectionHeader.SizeOfRawData;
      Break;
    end;
  end;
  if SectionPhysOffset = 0 then
    Error('No resources (2)');
end;

function FindResOffset(const F: TCustomFile; const AnyId: Boolean;
  const Id: Cardinal; const FindSubdir: Boolean; var Offset: Cardinal): Boolean;
var
  Dir: TImageResourceDirectory;
  Entry: TImageResourceDirectoryEntry;
  I: Integer;
begin
  F.ReadBuffer(Dir, SizeOf(Dir));
  { Skip over named entries }
  for I := 0 to Dir.NumberOfNamedEntries-1 do
    F.ReadBuffer(Entry, SizeOf(Entry));
  { Now process ID entries }
  Result := False;
  for I := 0 to Dir.NumberOfIdEntries-1 do begin
    F.ReadBuffer(Entry, SizeOf(Entry));
    if (AnyId or (Entry.Id = Id)) and
       ((Entry.Offset and $80000000 <> 0) = FindSubdir) then begin
      Offset := Entry.Offset and $7FFFFFFF;
      Result := True;
      Break;
    end;
  end;
end;

function SeekToResourceData(const F: TCustomFile; const ResType, ResId: Cardinal): Cardinal;
{ Seeks to the specified resource's data, and returns its size. Raises an
  exception if the resource cannot be found. }
var
  SectionVirtualAddr, SectionPhysOffset, SectionPhysSize, Ofs: Cardinal;
  DataEntry: TImageResourceDataEntry;
begin
  FindResourceSection(F, SectionVirtualAddr, SectionPhysOffset, SectionPhysSize);

  { Scan the resource directory }
  F.Seek(SectionPhysOffset);
  if not FindResOffset(F, False, ResType, True, Ofs) then
    Error('Can''t find resource (1)');
  F.Seek(SectionPhysOffset + Ofs);
  if not FindResOffset(F, False, ResId, True, Ofs) then
    Error('Can''t find resource (2)');
  F.Seek(SectionPhysOffset + Ofs);
  if not FindResOffset(F, True, 0, False, Ofs) then
    Error('Can''t find resource (3).');
  F.Seek(SectionPhysOffset + Ofs);
  F.ReadBuffer(DataEntry, SizeOf(DataEntry));

  { Sanity check: DataEntry.OffsetToData is an RVA. It's technically possible
    for the RVA to point to a different section, but we don't support that. }
  if Cardinal(DataEntry.OffsetToData) < SectionVirtualAddr then
    Error('Invalid resource (1)');
  if Cardinal(DataEntry.OffsetToData - SectionVirtualAddr + DataEntry.Size) > SectionPhysSize then
    Error('Invalid resource (2)');

  { Seek to the resource }
  F.Seek(SectionPhysOffset + (DataEntry.OffsetToData - SectionVirtualAddr));
  Result := DataEntry.Size;
end;

procedure UpdateManifestRequestedExecutionLevel(const F: TCustomFile;
  const RequireAdministrator: Boolean);
const
  ElementText: AnsiString = '<requestedExecutionLevel level="';
  Levels: array[Boolean] of AnsiString = (
    'highestAvailable"    ',
    'requireAdministrator"');
var
  S: AnsiString;
  Offset: Integer64;
  P: Integer;
begin
  { Read the manifest resource into a string }
  SetString(S, nil, SeekToResourceData(F, 24, 1));
  Offset := F.Position;
  F.ReadBuffer(S[1], Length(S));

  { Locate and update the requestedExecutionLevel element }
  P := Pos(ElementText, S);
  if P = 0 then
    Error('Element not found');
  Inc(P, Length(ElementText));
  if Copy(S, P+21, 10) <> ' uiAccess=' then
    Error('Level too short');
  Inc64(Offset, P-1);
  F.Seek64(Offset);
  F.WriteAnsiString(Levels[RequireAdministrator]);
end;

function ReadSignatureAndChecksumFields(const F: TCustomFile;
  var ASignatureAddress, ASignatureSize, AChecksum: DWORD): Boolean;
{ Reads the signature and checksum fields in the specified file's header.
  If the file is not a valid PE32 executable, False is returned. }
var
  OptHeader: TImageOptionalHeader;
  OptHeaderOffset: Integer64;
begin
  Result := SeekToAndReadPEOptionalHeader(F, OptHeader, OptHeaderOffset);
  if Result then begin
    ASignatureAddress := OptHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_SECURITY].VirtualAddress;
    ASignatureSize := OptHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_SECURITY].Size;
    AChecksum := OptHeader.CheckSum;
  end;
end;

function ReadSignatureAndChecksumFields64(const F: TCustomFile;
  var ASignatureAddress, ASignatureSize, AChecksum: DWORD): Boolean;
{ Reads the signature and checksum fields in the specified file's header.
  If the file is not a valid PE32+ executable, False is returned. }
var
  OptHeader: TImageOptionalHeader64;
  OptHeaderOffset: Integer64;
begin
  Result := SeekToAndReadPEOptionalHeader64(F, OptHeader, OptHeaderOffset);
  if Result then begin
    ASignatureAddress := OptHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_SECURITY].VirtualAddress;
    ASignatureSize := OptHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_SECURITY].Size;
    AChecksum := OptHeader.CheckSum;
  end;
end;

function UpdateSignatureAndChecksumFields(const F: TCustomFile;
  const ASignatureAddress, ASignatureSize, AChecksum: DWORD): Boolean;
{ Sets the signature and checksum fields in the specified file's header.
  If the file is not a valid PE32 executable, False is returned. }
var
  OptHeader: TImageOptionalHeader;
  OptHeaderOffset: Integer64;
begin
  Result := SeekToAndReadPEOptionalHeader(F, OptHeader, OptHeaderOffset);
  if Result then begin
    OptHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_SECURITY].VirtualAddress := ASignatureAddress;
    OptHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_SECURITY].Size := ASignatureSize;
    OptHeader.CheckSum := AChecksum;
    F.Seek64(OptHeaderOffset);
    F.WriteBuffer(OptHeader, SizeOf(OptHeader));
  end;
end;

end.
