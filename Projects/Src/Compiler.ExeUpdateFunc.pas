unit Compiler.ExeUpdateFunc;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  PE header and resource update functions used by the compiler only
}

interface

uses
  Windows, SysUtils, Shared.FileClass, Shared.VerInfoFunc, Shared.Struct;

type
  TUpdateIconsAndStyleFile = (uisfSetupE32, uisfSetupCustomStyleE32, uisfSetupLdrE32);
  TUpdateIconsAndStyleOperation = (uisoIcoFileName, uisoWizardDarkStyle, uisoStyleFileName, uisoStyleFileNameDynamicDark, uisoDone);
  TOnUpdateIconsAndStyle = procedure(const Operation: TUpdateIconsAndStyleOperation) of object;

function ReadSignatureAndChecksumFields(const F: TCustomFile;
  var ASignatureAddress, ASignatureSize, AChecksum: DWORD): Boolean;
function ReadSignatureAndChecksumFields64(const F: TCustomFile;
  var ASignatureAddress, ASignatureSize, AChecksum: DWORD): Boolean;
function SeekToResourceData(const F: TCustomFile; const ResType, ResId: Cardinal): Cardinal;
function UpdateSignatureAndChecksumFields(const F: TCustomFile;
  const ASignatureAddress, ASignatureSize, AChecksum: DWORD): Boolean;
  
procedure UpdateSetupPEHeaderFields(const F: TCustomFile;
  const IsTSAware, IsDEPCompatible, IsASLRCompatible: Boolean);
procedure UpdateIconsAndStyle(const FileName: String; const Uisf: TUpdateIconsAndStyleFile; const IcoFileName: String;
  const WizardDarkStyle: TSetupWizardDarkStyle; const StyleFileName, StyleFileNameDynamicDark: String;
  const OnUpdateIconsAndStyle: TOnUpdateIconsAndStyle);
procedure UpdateVersionInfo(const F: TCustomFile;
  const NewBinaryFileVersion, NewBinaryProductVersion: TFileVersionNumbers;
  const NewCompanyName, NewFileDescription, NewTextFileVersion, NewLegalCopyright,
  NewProductName, NewTextProductVersion, NewOriginalFileName: String;
  const SetFileVersionAndDescription: Boolean);
procedure PreventCOMCTL32Sideloading(const F: TCustomFile);

implementation

uses
  Math;

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
    ImageBase: Int64;
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
    SizeOfStackReserve: Int64;
    SizeOfStackCommit: Int64;
    SizeOfHeapReserve: Int64;
    SizeOfHeapCommit: Int64;
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
  var OptHeader: TImageOptionalHeader; var OptHeaderOffset: Int64): Boolean;
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
  var OptHeader: TImageOptionalHeader64; var OptHeaderOffset: Int64): Boolean;
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

function ReadSignatureAndChecksumFields(const F: TCustomFile;
  var ASignatureAddress, ASignatureSize, AChecksum: DWORD): Boolean;
{ Reads the signature and checksum fields in the specified file's header.
  If the file is not a valid PE32 executable, False is returned. }
var
  OptHeader: TImageOptionalHeader;
  OptHeaderOffset: Int64;
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
  OptHeaderOffset: Int64;
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
  OptHeaderOffset: Int64;
begin
  Result := SeekToAndReadPEOptionalHeader(F, OptHeader, OptHeaderOffset);
  if Result then begin
    OptHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_SECURITY].VirtualAddress := ASignatureAddress;
    OptHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_SECURITY].Size := ASignatureSize;
    OptHeader.CheckSum := AChecksum;
    F.Seek(OptHeaderOffset);
    F.WriteBuffer(OptHeader, SizeOf(OptHeader));
  end;
end;

procedure UpdateSetupPEHeaderFields(const F: TCustomFile;
  const IsTSAware, IsDEPCompatible, IsASLRCompatible: Boolean);
const
  IMAGE_DLLCHARACTERISTICS_DYNAMIC_BASE = $0040;
  IMAGE_DLLCHARACTERISTICS_NX_COMPAT = $0100;
  IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE = $8000;
  OffsetOfDllCharacteristics = $46;
var
  Header: TImageFileHeader;
  OptMagic, DllChars, OrigDllChars: Word;
begin
  if SeekToPEHeader(F) then begin
    if (F.Read(Header, SizeOf(Header)) = SizeOf(Header)) and
       (Header.SizeOfOptionalHeader = SizeOf(TImageOptionalHeader)) then begin
      const Ofs = F.Position;
      if (F.Read(OptMagic, SizeOf(OptMagic)) = SizeOf(OptMagic)) and
         (OptMagic = IMAGE_NT_OPTIONAL_HDR32_MAGIC) then begin
        { Update DllCharacteristics }
        F.Seek(Ofs + OffsetOfDllCharacteristics);
        if F.Read(DllChars, SizeOf(DllChars)) = SizeOf(DllChars) then begin
          OrigDllChars := DllChars;
          if IsTSAware then
            DllChars := DllChars or IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE
          else
            DllChars := DllChars and not IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE;
          if IsDEPCompatible then
            DllChars := DllChars or IMAGE_DLLCHARACTERISTICS_NX_COMPAT
          else
            DllChars := DllChars and not IMAGE_DLLCHARACTERISTICS_NX_COMPAT;
          if IsASLRCompatible then
            DllChars := DllChars or IMAGE_DLLCHARACTERISTICS_DYNAMIC_BASE
          else
            DllChars := DllChars and not IMAGE_DLLCHARACTERISTICS_DYNAMIC_BASE;
          if DllChars <> OrigDllChars then begin
            F.Seek(Ofs + OffsetOfDllCharacteristics);
            F.WriteBuffer(DllChars, SizeOf(DllChars));
          end;
          Exit;
        end;
      end;
    end;
  end;
  raise Exception.Create('UpdateSetupPEHeaderFields failed');
end;

procedure ResUpdateError(const Msg: String; const ResourceName: String = '');
begin
  if ResourceName <> '' then
    raise Exception.CreateFmt('Resource %s update error: %s', [ResourceName, Msg])
  else
    raise Exception.CreateFmt('Resource update error: %s', [Msg]);
end;

procedure ResUpdateErrorWithLastError(const Msg: String; const ResourceName: String = '');
begin
  ResUpdateError(Msg + ' (' + IntToStr(GetLastError) + ')', ResourceName);
end;

procedure UpdateVersionInfo(const F: TCustomFile;
  const NewBinaryFileVersion, NewBinaryProductVersion: TFileVersionNumbers;
  const NewCompanyName, NewFileDescription, NewTextFileVersion, NewLegalCopyright,
  NewProductName, NewTextProductVersion, NewOriginalFileName: String;
  const SetFileVersionAndDescription: Boolean);

  function WideStrsEqual(P1, P2: PWideChar): Boolean;

    function WideUpCase(C: WideChar): WideChar;
    begin
      Result := C;
      if (Result >= 'a') and (Result <= 'z') then
        Dec(Result, Ord('a') - Ord('A'));
    end;

  begin
    while True do begin
      if WideUpCase(P1^) <> WideUpCase(P2^) then begin
        Result := False;
        Exit;
      end;
      if P1^ = #0 then
        Break;
      Inc(P1);
      Inc(P2);
    end;
    Result := True;
  end;

  procedure BumpToDWordBoundary(var P: Pointer);
  begin
    if Cardinal(P) and 3 <> 0 then
      Cardinal(P) := (Cardinal(P) or 3) + 1;
  end;

  function QueryValue(P: Pointer; Path: PWideChar; var Buf: Pointer;
    var BufLen: Cardinal): Boolean;
  var
    EndP: Pointer;
    ValueLength: Cardinal;
  begin
    Result := False;
    Cardinal(EndP) := Cardinal(P) + PWord(P)^;
    Inc(PWord(P));
    ValueLength := PWord(P)^;
    Inc(PWord(P));
    Inc(PWord(P));
    if WideStrsEqual(PWideChar(P), Path) then begin
      Inc(PWideChar(P), lstrlenW(P) + 1);
      BumpToDWordBoundary(P);
      Inc(Path, lstrlenW(Path) + 1);
      if Path^ = #0 then begin
        { Found the requested value }
        Buf := P;
        BufLen := ValueLength;
        Result := True;
      end
      else begin
        { Handle children.
          Note: Like VerQueryValue, we always treat ValueLength as a byte count
          when looking for child nodes. Many resource compilers, including
          Borland's, wrongly set ValueLength to a *character* count on string
          nodes. But since we never try to query for a child of a string node,
          that doesn't matter here. }
        Inc(Cardinal(P), ValueLength);
        BumpToDWordBoundary(P);
        while Cardinal(P) < Cardinal(EndP) do begin
          Result := QueryValue(P, Path, Buf, BufLen);
          if Result then
            Exit;
          Inc(Cardinal(P), PWord(P)^);
          BumpToDWordBoundary(P);
        end;
      end;
    end;
  end;

  procedure ReplaceWithRealCopyrightSymbols(const Value: PWideChar);
  var
    Len, I, J: Integer;
  begin
    Len := lstrlenW(Value);
    for I := 0 to Len-3 do begin
      if (Value[I] = '(') and (Value[I+1] = 'C') and (Value[I+2] = ')') then begin
        Value[I] := WideChar($00A9);
        { Shift back two characters }
        for J := I+1 to Len-3 do
          Value[J] := Value[J+2];
        Value[Len-2] := ' ';
        Value[Len-1] := ' ';
      end;
    end;
  end;

  procedure UpdateStringValue(P: Pointer; const Path: PWideChar; NewValue: String);
  var
    Value: PWideChar;
    ValueLen: Cardinal;
  begin
    if not QueryValue(P, Path, Pointer(Value), ValueLen) then
      ResUpdateError('Unexpected version resource format (1)');
    Move(Pointer(NewValue)^, Value^, (Min(Length(NewValue), lstrlenW(Value)))*SizeOf(Char));
    ReplaceWithRealCopyrightSymbols(Value);
  end;

  procedure UpdateFixedFileInfo(P: Pointer; const Path: PWideChar;
    const NewFileVersion, NewProductVersion: TFileVersionNumbers;
    const SetFileVersion: Boolean);
  var
    FixedFileInfo: PVSFixedFileInfo;
    ValueLen: Cardinal;
  begin
    if not QueryValue(P, Path, Pointer(FixedFileInfo), ValueLen) then
      ResUpdateError('Unexpected version resource format (2)');
    if FixedFileInfo.dwSignature <> $FEEF04BD then
      ResUpdateError('Unexpected version resource format (3)');
    if SetFileVersion then begin
      FixedFileInfo.dwFileVersionLS := NewFileVersion.LS;
      FixedFileInfo.dwFileVersionMS := NewFileVersion.MS;
    end;
    FixedFileInfo.dwProductVersionLS := NewProductVersion.LS;
    FixedFileInfo.dwProductVersionMS := NewProductVersion.MS;
  end;

var
  ResSize: Cardinal;
  VersRes: Pointer;
begin
  { Locate the resource }
  ResSize := SeekToResourceData(F, Cardinal(RT_VERSION), 1);
  const ResOffset = F.Position;

  GetMem(VersRes, ResSize);
  try
    { Read the resource }
    F.ReadBuffer(VersRes^, ResSize);

    { Update the resource }
    UpdateStringValue(VersRes, 'VS_VERSION_INFO'#0'StringFileInfo'#0'000004b0'#0'CompanyName'#0, NewCompanyName);
    if SetFileVersionAndDescription then begin
      UpdateStringValue(VersRes, 'VS_VERSION_INFO'#0'StringFileInfo'#0'000004b0'#0'FileDescription'#0, NewFileDescription);
      UpdateStringValue(VersRes, 'VS_VERSION_INFO'#0'StringFileInfo'#0'000004b0'#0'FileVersion'#0, NewTextFileVersion);
    end;
    UpdateStringValue(VersRes, 'VS_VERSION_INFO'#0'StringFileInfo'#0'000004b0'#0'LegalCopyright'#0, NewLegalCopyright);
    UpdateStringValue(VersRes, 'VS_VERSION_INFO'#0'StringFileInfo'#0'000004b0'#0'ProductName'#0, NewProductName);
    UpdateStringValue(VersRes, 'VS_VERSION_INFO'#0'StringFileInfo'#0'000004b0'#0'OriginalFileName'#0, NewOriginalFileName);
    UpdateStringValue(VersRes, 'VS_VERSION_INFO'#0'StringFileInfo'#0'000004b0'#0'ProductVersion'#0, NewTextProductVersion);
    UpdateFixedFileInfo(VersRes, 'VS_VERSION_INFO'#0, NewBinaryFileVersion, NewBinaryProductVersion, SetFileVersionAndDescription);

    { Write the updated resource }
    F.Seek(ResOffset);
    F.WriteBuffer(VersRes^, ResSize);
  finally
    FreeMem(VersRes);
  end;
end;

function EnumLangsFunc(hModule: Cardinal; lpType, lpName: PAnsiChar; wLanguage: Word; lParam: Integer): BOOL; stdcall;
begin
  PWord(lParam)^ := wLanguage;
  Result := False;
end;

function GetResourceLanguage(hModule: Cardinal; lpType, lpName: PChar; var wLanguage: Word): Boolean;
begin
  wLanguage := 0;
  EnumResourceLanguages(hModule, lpType, lpName, @EnumLangsFunc, Integer(@wLanguage));
  Result := True;
end;

procedure UpdateIconsAndStyle(const FileName: String; const Uisf: TUpdateIconsAndStyleFile; const IcoFileName: String;
  const WizardDarkStyle: TSetupWizardDarkStyle; const StyleFileName, StyleFileNameDynamicDark: String;
  const OnUpdateIconsAndStyle: TOnUpdateIconsAndStyle);
type
  PIcoItemHeader = ^TIcoItemHeader;
  TIcoItemHeader = packed record
    Width: Byte;
    Height: Byte;
    Colors: Byte;
    Reserved: Byte;
    Planes: Word;
    BitCount: Word;
    ImageSize: DWORD;
  end;
  PIcoItem = ^TIcoItem;
  TIcoItem = packed record
    Header: TIcoItemHeader;
    Offset: DWORD;
  end;
  PIcoHeader = ^TIcoHeader;
  TIcoHeader = packed record
    Reserved: Word;
    Typ: Word;
    ItemCount: Word;
    Items: array [0..MaxInt shr 4 - 1] of TIcoItem;
  end;
  PGroupIconDirItem = ^TGroupIconDirItem;
  TGroupIconDirItem = packed record
    Header: TIcoItemHeader;
    Id: Word;
  end;
  PGroupIconDir = ^TGroupIconDir;
  TGroupIconDir = packed record
    Reserved: Word;
    Typ: Word;
    ItemCount: Word;
    Items: array [0..MaxInt shr 4 - 1] of TGroupIconDirItem;
  end;

  procedure TriggerOnUpdateIconsAndStyle(const Operation: TUpdateIconsAndStyleOperation);
  begin
    if Assigned(OnUpdateIconsAndStyle) then
      OnUpdateIconsAndStyle(Operation);
  end;

  function LoadFileIntoMemory(const FileName: String; var P: Pointer): Cardinal;
  begin
    const F = TFile.Create(FileName, fdOpenExisting, faRead, fsRead);
    try
      const N = F.CappedSize;
      if Cardinal(N) > Cardinal($100000) then  { sanity check }
        ResUpdateError('File is too large');
      GetMem(P, N);
      F.ReadBuffer(P^, N);
      Result := N;
    finally
      F.Free;
    end;
  end;

  function IsValidIcon(P: Pointer; Size: Cardinal): Boolean;
  var
    ItemCount: Cardinal;
  begin
    Result := False;
    if Size < Cardinal(SizeOf(Word) * 3) then
      Exit;
    if (PChar(P)[0] = 'M') and (PChar(P)[1] = 'Z') then
      Exit;
    ItemCount := PIcoHeader(P).ItemCount;
    if Size < Cardinal((SizeOf(Word) * 3) + (ItemCount * SizeOf(TIcoItem))) then
      Exit;
    P := @PIcoHeader(P).Items;
    while ItemCount > Cardinal(0) do begin
      if (Cardinal(PIcoItem(P).Offset + PIcoItem(P).Header.ImageSize) < Cardinal(PIcoItem(P).Offset)) or
         (Cardinal(PIcoItem(P).Offset + PIcoItem(P).Header.ImageSize) > Cardinal(Size)) then
        Exit;
      Inc(PIcoItem(P));
      Dec(ItemCount);
    end;
    Result := True;
  end;

  function LoadResourcePointer(const M: HMODULE; const ResourceType, ResourceName: PChar;
    const WantSize: Boolean; out Size: DWORD): Pointer; overload;
  begin
    var R := FindResource(M, ResourceName, ResourceType);
    if R = 0 then
      ResUpdateErrorWithLastError('FindResource failed (1)', ResourceName);
    if WantSize then begin
      Size := SizeofResource(M, R);
      if Size = 0 then
        ResUpdateErrorWithLastError('SizeofResource failed (1)', ResourceName);
    end;
    var Res := LoadResource(M, R);
    if Res = 0 then
      ResUpdateErrorWithLastError('LoadResource failed (1)', ResourceName);
    Result := LockResource(Res);
    if Result = nil then
      ResUpdateErrorWithLastError('LockResource failed (1)', ResourceName);
  end;

  function LoadResourcePointer(const M: HMODULE; const ResourceType, ResourceName: PChar): Pointer; overload;
  begin
    var Dummy: DWORD;
    Result := LoadResourcePointer(M, ResourceType, ResourceName, False, Dummy);
  end;

  procedure DeleteResource(const H: THandle; const M: HMODULE; const ResourceType, ResourceName: PChar);
  var
    wLanguage: Word;
  begin
    if not GetResourceLanguage(M, ResourceType, ResourceName, wLanguage) then
      ResUpdateError('GetResourceLanguage failed (1)', ResourceName);
    if not UpdateResource(H, ResourceType, ResourceName, wLanguage, nil, 0) then
      ResUpdateErrorWithLastError('UpdateResource failed (1)', ResourceName);
  end;

  procedure RenameResource(const H: THandle; const M: HMODULE; const ResourceType, OldResourceName, NewResourceName: PChar);
  var
    Size: DWORD;
    P: Pointer;
    wLanguage: Word;
  begin
    { Load the resource }
    P := LoadResourcePointer(M, ResourceType, OldResourceName, True, Size);

    { Create a copy resource with the new name }
    if not GetResourceLanguage(M, ResourceType, OldResourceName, wLanguage) then
      ResUpdateError('GetResourceLanguage failed (2)', OldResourceName);
    if not UpdateResource(H, ResourceType, NewResourceName, wLanguage, P, Size) then
      ResUpdateErrorWithLastError('UpdateResource failed (2)', NewResourceName);

    { Delete the old resource }
    if not UpdateResource(H, ResourceType, OldResourceName, wLanguage, nil, 0) then
      ResUpdateErrorWithLastError('UpdateResource failed (3)', OldResourceName);
 end;

  function DeleteIcon(const H: THandle; const M: HMODULE; const ResourceName: PChar): PGroupIconDir;
  var
    GroupIconDir: PGroupIconDir;
    I: Integer;
    wLanguage: Word;
  begin
    { Load the group icon resource }
    GroupIconDir := LoadResourcePointer(M, RT_GROUP_ICON, ResourceName);

    { Delete the group icon resource }
    DeleteResource(H, M, RT_GROUP_ICON, ResourceName);

    { Delete the icon resources that belonged to the group }
    for I := 0 to GroupIconDir.ItemCount-1 do begin
      if not GetResourceLanguage(M, RT_ICON, MakeIntResource(GroupIconDir.Items[I].Id), wLanguage) then
        ResUpdateError('GetResourceLanguage failed (3)', ResourceName);
      if not UpdateResource(H, RT_ICON, MakeIntResource(GroupIconDir.Items[I].Id), wLanguage, nil, 0) then
        ResUpdateErrorWithLastError('UpdateResource failed (4)', ResourceName);
    end;

    Result := GroupIconDir;
  end;

  function DeleteIconIfExists(const H: THandle; const M: HMODULE; const ResourceName: PChar): PGroupIconDir;
  begin
    if FindResource(M, ResourceName, RT_GROUP_ICON) <> 0 then
      Result := DeleteIcon(H, M, ResourceName)
    else
      Result := nil;
  end;

  function RenameIconWithOverwrite(const H: THandle; const M: HMODULE; const OldResourceName, NewResourceName: PChar): PGroupIconDir;
  var
    GroupIconDir: PGroupIconDir;
    GroupIconDirSize: DWORD;
    wLanguage: Word;
  begin
    DeleteIconIfExists(H, M, NewResourceName);

    { Load the group icon resource }
    GroupIconDir := LoadResourcePointer(M, RT_GROUP_ICON, OldResourceName);

    GroupIconDirSize := Sizeof(Word)*3 + GroupIconDir.ItemCount*Sizeof(TGroupIconDirItem);

    { Create a copy group icon resource with the new name - existing icon resources will belong to
      it automatically }
    if not GetResourceLanguage(M, RT_GROUP_ICON, OldResourceName, wLanguage) then
      ResUpdateError('GetResourceLanguage failed (4)', OldResourceName);
    if not UpdateResource(H, RT_GROUP_ICON, NewResourceName, wLanguage, GroupIconDir, GroupIconDirSize) then
      ResUpdateErrorWithLastError('UpdateResource failed (5)', NewResourceName);

    { Delete the old group icon resource }
    if not UpdateResource(H, RT_GROUP_ICON, OldResourceName, wLanguage, nil, 0) then
      ResUpdateErrorWithLastError('UpdateResource failed (6)', OldResourceName);

    Result := GroupIconDir;
  end;

  function HandleBuiltinStyle(const M: HMODULE; const StyleFileName: String; var Vsf: Pointer; var VsfSize: Cardinal; const Dark: Boolean): Boolean;
  begin
    { Also see DeleteResource calls below }
    var StyleName: PChar := nil;
    if SameText(StyleFileName, 'builtin:polar') then begin
      if Dark then
        StyleName := 'POLAR_DARK'
      else
        StyleName := 'POLAR_LIGHT';
    end else if SameText(StyleFileName, 'builtin:windows11') then begin
      if Dark then
        StyleName := 'WINDOWS11_DARK'
      else
        StyleName := 'WINDOWS11_LIGHT';
    end else if SameText(StyleFileName, 'builtin:slate') then
      StyleName := 'SLATECLASSICO'
    else if SameText(StyleFileName, 'builtin:zircon') then
      StyleName := 'ZIRCON';
    Result := StyleName <> nil;
    if Result then
      Vsf := LoadResourcePointer(M, 'VCLSTYLE', StyleName, True, VsfSize)
  end;

var
  H: THandle;
  M: HMODULE;
  OldGroupIconDir, NewGroupIconDir: PGroupIconDir;
  I: Integer;
  NewGroupIconDirSize: LongInt;
begin
  var Ico: PIcoHeader := nil;
  var Vsf := nil;
  var ShouldFreeVsf := False;
  var VsfDynamicDark := nil;
  var ShouldFreeVsfDynamicDark := False;

  try
    if IcoFileName <> '' then begin
      TriggerOnUpdateIconsAndStyle(uisoIcoFileName);

      { Load the icons }
      var P: Pointer;
      const IcoSize = LoadFileIntoMemory(IcoFileName, P);
      Ico := P;

      { Ensure the icon is valid }
      if not IsValidIcon(Ico, IcoSize) then
        ResUpdateError('Icon file is invalid');
    end;

    { Update the resources }
    var ChangedMainIcon := False;
    H := BeginUpdateResource(PChar(FileName), False);
    if H = 0 then
      ResUpdateErrorWithLastError('BeginUpdateResource failed (1)');
    try
      M := LoadLibraryEx(PChar(FileName), 0, LOAD_LIBRARY_AS_DATAFILE);
      if M = 0 then
        ResUpdateErrorWithLastError('LoadLibraryEx failed (1)');
      try
        { Load the styles. Could be checked using TStyleManager.IsValidStyle but that requires using VCL units. }
        var VsfSize: Cardinal := 0;
        if StyleFileName <> '' then begin
          TriggerOnUpdateIconsAndStyle(uisoStyleFileName);
          if not HandleBuiltinStyle(M, StyleFileName, Vsf, VsfSize, WizardDarkStyle = wdsDark) then begin
            VsfSize := LoadFileIntoMemory(StyleFileName, Vsf);
            ShouldFreeVsf := True;
          end;
        end;

        var VsfSizeDynamicDark: Cardinal := 0;
        if StyleFileNameDynamicDark <> '' then begin
          TriggerOnUpdateIconsAndStyle(uisoStyleFileNameDynamicDark);
          if not HandleBuiltinStyle(M, StyleFileNameDynamicDark, VsfDynamicDark, VsfSizeDynamicDark, True) then begin
            VsfSizeDynamicDark := LoadFileIntoMemory(StyleFileNameDynamicDark, VsfDynamicDark);
            ShouldFreeVsfDynamicDark := True;
          end;
        end;

        { All of the following changes must be independent because updates are not immediate. For
          example, if you call DeleteIcon followed by FindResource then resource will still be found,
          until you call EndUpdateResource *and* reload the file using LoadLibrary }

        if IcoFileName <> '' then begin
          TriggerOnUpdateIconsAndStyle(uisoIcoFileName);

          const ResourceName = 'MAINICON';

          { Delete default icons }
          OldGroupIconDir := DeleteIcon(H, M, PChar(ResourceName));
          DeleteIconIfExists(H, M, PChar(ResourceName + '_DARK'));

          { Build the new group icon resource }
          NewGroupIconDirSize := 3*SizeOf(Word)+Ico.ItemCount*SizeOf(TGroupIconDirItem);
          GetMem(NewGroupIconDir, NewGroupIconDirSize);
          try
            { Build the new group icon resource }
            NewGroupIconDir.Reserved := OldGroupIconDir.Reserved;
            NewGroupIconDir.Typ := OldGroupIconDir.Typ;
            NewGroupIconDir.ItemCount := Ico.ItemCount;
            for I := 0 to NewGroupIconDir.ItemCount-1 do begin
              NewGroupIconDir.Items[I].Header := Ico.Items[I].Header;
              NewGroupIconDir.Items[I].Id := I+100; //start at 100 to avoid overwriting other icons that may exist
            end;

            { Update 'MAINICON' }
            for I := 0 to NewGroupIconDir.ItemCount-1 do
              if not UpdateResource(H, RT_ICON, MakeIntResource(NewGroupIconDir.Items[I].Id), 1033, Pointer(DWORD(Ico) + Ico.Items[I].Offset), Ico.Items[I].Header.ImageSize) then
                ResUpdateErrorWithLastError('UpdateResource failed (7)', ResourceName);

            { Update the icons }
            if not UpdateResource(H, RT_GROUP_ICON, 'MAINICON', 1033, NewGroupIconDir, NewGroupIconDirSize) then
              ResUpdateErrorWithLastError('UpdateResource failed (8)', ResourceName);

            ChangedMainIcon := True;
          finally
            FreeMem(NewGroupIconDir);
          end;
        end else begin
          if WizardDarkStyle <> wdsDynamic then begin
            TriggerOnUpdateIconsAndStyle(uisoWizardDarkStyle);
            if WizardDarkStyle = wdsLight then begin
              { Forced light: remove dark main icon }
              DeleteIconIfExists(H, M, 'MAINICON_DARK')
            end else begin
              { Forced dark: rename dark main icon to be the regular main icon }
              RenameIconWithOverwrite(H, M, 'MAINICON_DARK', 'MAINICON');
              ChangedMainIcon := True;
            end;
          end; { Else keep both main icons }
        end;

        if Uisf in [uisfSetupE32, uisfSetupCustomStyleE32] then begin
          const DeleteUninstallIcon = IcoFileName <> '';
          if DeleteUninstallIcon then begin
            TriggerOnUpdateIconsAndStyle(uisoIcoFileName);
            { Make UninstallProgressForm use the custom icon }
            DeleteIcon(H, M, 'Z_UNINSTALLICON');
            DeleteIconIfExists(H, M, 'Z_UNINSTALLICON_DARK');
          end;
          if WizardDarkStyle <> wdsDynamic then begin
            TriggerOnUpdateIconsAndStyle(uisoWizardDarkStyle);
            { Unlike for MAINICON (for which we don't have the choice) here it always uses DeleteIcon
              instead of also using RenameIcon, to avoid issues with Windows' icon cache }
            var Postfix := '';
            if WizardDarkStyle = wdsLight then
              Postfix := '_DARK';
            { Delete the icons we don't need: either the light ones or the dark ones }
            DeleteIconIfExists(H, M, PChar('Z_GROUPICON' + Postfix));
            if not DeleteUninstallIcon then
              DeleteIconIfExists(H, M, PChar('Z_UNINSTALLICON' + Postfix));
          end;

          if Uisf = uisfSetupCustomStyleE32 then begin
            if Vsf <> nil then begin
              TriggerOnUpdateIconsAndStyle(uisoStyleFileName);
              { Add the regular custom style, used by forced light, forced dark and dynamic light }
              if not UpdateResource(H, 'VCLSTYLE', 'MYSTYLE1', 1033, Vsf, VsfSize) then
                ResUpdateErrorWithLastError('UpdateResource failed (9)', 'MYSTYLE1');
            end;

            if VsfDynamicDark <> nil then begin
              TriggerOnUpdateIconsAndStyle(uisoStyleFileNameDynamicDark);
              { Add the dark custom style, used by dynamic dark only }
              if not UpdateResource(H, 'VCLSTYLE', 'MYSTYLE1_DARK', 1033, VsfDynamicDark, VsfSizeDynamicDark) then
                ResUpdateErrorWithLastError('UpdateResource failed (10)', 'MYSTYLE1_DARK');
            end;

            { See if we need to keep the built-in dark style }
            if (Vsf = nil) and (WizardDarkStyle = wdsDark) then begin
              TriggerOnUpdateIconsAndStyle(uisoWizardDarkStyle);
              { Forced dark without a custom style: make the built-in dark style the regular one }
              RenameResource(H, M, 'VCLSTYLE', 'WINDOWS11_DARK', 'MYSTYLE1');
            end else if (VsfDynamicDark = nil) and (WizardDarkStyle = wdsDynamic) then begin
              TriggerOnUpdateIconsAndStyle(uisoWizardDarkStyle);
              { Dynamic without a custom dark style: make the built-in dark style the dark one }
              RenameResource(H, M, 'VCLSTYLE', 'WINDOWS11_DARK', 'MYSTYLE1_DARK');
            end else begin
              TriggerOnUpdateIconsAndStyle(uisoWizardDarkStyle);
              { Forced dark with a custom style: delete the built-in dark style
                Or, dynamic with a custom dark style: same
                Or, forced light with or without a custom style: same
                Note: forced light without a custom style doesn't actually use SetupCustomStyle.e32 at the moment so won't get here }
              DeleteResource(H, M, 'VCLSTYLE', 'WINDOWS11_DARK');
            end;

            { Delete additional styles - they are handled above }
            DeleteResource(H, M, 'VCLSTYLE', 'WINDOWS11_LIGHT');
            DeleteResource(H, M, 'VCLSTYLE', 'POLAR_LIGHT');
            DeleteResource(H, M, 'VCLSTYLE', 'POLAR_DARK');
            DeleteResource(H, M, 'VCLSTYLE', 'SLATECLASSICO');
            DeleteResource(H, M, 'VCLSTYLE', 'ZIRCON');
          end;
        end;

        TriggerOnUpdateIconsAndStyle(uisoDone);
      finally
        FreeLibrary(M);
      end;
    except
      EndUpdateResource(H, True);  { discard changes }
      raise;
    end;
    if not EndUpdateResource(H, False) then
      if ChangedMainIcon then { Only allow errors (likely from faulty AV software) if the update actually is important }
        ResUpdateErrorWithLastError('EndUpdateResource failed, try excluding the Output folder from your antivirus software');
  finally
    if ShouldFreeVsfDynamicDark then
      FreeMem(VsfDynamicDark);
    if ShouldFreeVsf then
      FreeMem(Vsf);
    if Ico <> nil then
      FreeMem(Ico);
  end;
end;

{ Replaces the entire comctl32 dependency section of the manifest with spaces, then inserts a
  comctl32 file entry before the other entries. Intended for SetupLdr only. Note: The exact number
  of spaces is calculated to allow seamless in-place editing. }
procedure PreventCOMCTL32Sideloading(const F: TCustomFile);
const
  DependencyStartTag: AnsiString = '<dependency>';
  DependencyEndTag: AnsiString = '</dependency>';
  FileStartTag: AnsiString = '<file name="';
  COMCTL32Entry: AnsiString = '<file name="comctl32.dll" loadFrom="%SystemRoot%\system32\" />'#13#10;
var
  S: AnsiString;
  P,Q,R: Integer;
begin
  { Read the manifest resource into a string }
  SetString(S, nil, SeekToResourceData(F, 24, 1));
  var Offset := F.Position;
  F.ReadBuffer(S[1], Length(S));

  { Locate and update the <dependency> tag }
  P := Pos(DependencyStartTag, S);
  if P = 0 then
    ResUpdateError('<dependency> tag not found');
  Q := Pos(DependencyEndTag, S);
  if Q <= P then
    ResUpdateError('<dependency> end tag not found');
  Q := Q+Length(DependencyEndTag);
  if Length(COMCTL32Entry) > Q-P then
    ResUpdateError('<dependency> tag shorter than replacement');
  R := Pos(FileStartTag, S);
  if R <= Q then
    ResUpdateError('<dependency> end tag after <file>?');

  Inc(Offset, P-1);
  F.Seek(Offset);
  F.WriteAnsiString(AnsiString(Format('%*s', [Q-P-Length(COMCTL32Entry), ' '])));
  F.WriteAnsiString(AnsiString(Copy(S, Q, R-Q)));
  F.WriteAnsiString(COMCTL32Entry);
end;

end.
