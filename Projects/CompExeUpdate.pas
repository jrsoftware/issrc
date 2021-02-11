unit CompExeUpdate;

{
  Inno Setup
  Copyright (C) 1997-2020 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  PE header and resource update functions used by the compiler only
}

interface

uses
  Windows, SysUtils, FileClass, VerInfo;

{$I VERSION.INC}

procedure UpdateSetupPEHeaderFields(const F: TCustomFile;
  const IsVistaCompatible, IsTSAware, IsDEPCompatible, IsASLRCompatible: Boolean);
procedure UpdateIcons(const FileName, IcoFileName: String);
procedure UpdateVersionInfo(const F: TCustomFile;
  const NewBinaryFileVersion, NewBinaryProductVersion: TFileVersionNumbers;
  const NewCompanyName, NewFileDescription, NewTextFileVersion, NewLegalCopyright,
  NewProductName, NewTextProductVersion, NewOriginalFileName: String;
  const SetFileVersionAndDescription: Boolean);
procedure RemoveManifestDllHijackProtection(const F: TCustomFile; const TestBlockOnly: Boolean);

implementation

uses
  ResUpdate{$IFDEF UNICODE}, Math{$ENDIF}, Int64Em;

procedure UpdateSetupPEHeaderFields(const F: TCustomFile;
  const IsVistaCompatible, IsTSAware, IsDEPCompatible, IsASLRCompatible: Boolean);

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

const
  IMAGE_DLLCHARACTERISTICS_DYNAMIC_BASE = $0040;
  IMAGE_DLLCHARACTERISTICS_NX_COMPAT = $0100;
  IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE = $8000;
  OffsetOfOperatingSystemVersion = $28;
  OffsetOfImageVersion = $2C;
  OffsetOfSubsystemVersion = $30;
  OffsetOfDllCharacteristics = $46;
var
  Header: TImageFileHeader;
  Ofs: Cardinal;
  OptMagic, DllChars, OrigDllChars: Word;
  VersionRecord: packed record
    Major, Minor: Word;
  end;
begin
  if SeekToPEHeader(F) then begin
    if (F.Read(Header, SizeOf(Header)) = SizeOf(Header)) and
       (Header.SizeOfOptionalHeader = 224) then begin
      Ofs := F.Position.Lo;
      if (F.Read(OptMagic, SizeOf(OptMagic)) = SizeOf(OptMagic)) and
         (OptMagic = IMAGE_NT_OPTIONAL_HDR32_MAGIC) then begin
        if IsVistaCompatible then begin
          { Update OS/Subsystem version }
          VersionRecord.Major := 6;
          VersionRecord.Minor := 0;
          F.Seek(Ofs + OffsetOfOperatingSystemVersion);
          F.WriteBuffer(VersionRecord, SizeOf(VersionRecord));
          F.Seek(Ofs + OffsetOfSubsystemVersion);
          F.WriteBuffer(VersionRecord, SizeOf(VersionRecord));
        end;

        { Update MajorImageVersion and MinorImageVersion to 6.0.
          Works around apparent bug in Vista (still present in Vista SP1;
          not reproducible on Server 2008): When UAC is turned off,
          launching an uninstaller (as admin) from ARP and answering No at the
          ConfirmUninstall message box causes a "This program might not have
          uninstalled correctly" dialog to be displayed, even if the EXE
          has a proper "Vista-aware" manifest. I discovered that if the EXE's
          image version is set to 6.0, like the EXEs that ship with Vista
          (notepad.exe), the dialog does not appear. (This is reproducible
          with notepad.exe too if its image version is changed to anything
          other than 6.0 exactly.) }
        VersionRecord.Major := 6;
        VersionRecord.Minor := 0;
        F.Seek(Ofs + OffsetOfImageVersion);
        F.WriteBuffer(VersionRecord, SizeOf(VersionRecord));

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
          { Note: because we stripped relocations from Setup(Ldr).e32 during
            compilation IMAGE_DLLCHARACTERISTICS_DYNAMIC_BASE won't actually
            enable ASLR, but allow setting it anyway to make checkers happy. }
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

procedure ResUpdateError(const Msg: String);
begin
  raise Exception.Create('Resource update error: ' + Msg);
end;

procedure ResUpdateErrorWithLastError(const Msg: String);
begin
  ResUpdateError(Msg + ' (' + IntToStr(GetLastError) + ')');
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
{$IFDEF UNICODE}
    Move(Pointer(NewValue)^, Value^, (Min(Length(NewValue), lstrlenW(Value)))*SizeOf(Char));
{$ELSE}
    MultiByteToWideChar(CP_ACP, 0, PChar(NewValue), Length(NewValue), Value, lstrlenW(Value));
{$ENDIF}
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
  ResOffset, ResSize: Cardinal;
  VersRes: Pointer;
begin
  { Locate the resource }
  ResSize := SeekToResourceData(F, Cardinal(RT_VERSION), 1);
  ResOffset := F.Position.Lo;

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

procedure UpdateIcons(const FileName, IcoFileName: String);
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

var
  H: THandle;
  M: HMODULE;
  R: HRSRC;
  Res: HGLOBAL;
  GroupIconDir, NewGroupIconDir: PGroupIconDir;
  I: Integer;
  wLanguage: Word;
  F: TFile;
  Ico: PIcoHeader;
  N: Cardinal;
  NewGroupIconDirSize: LongInt;
begin
  if Win32Platform <> VER_PLATFORM_WIN32_NT then
    ResUpdateError('Only supported on Windows NT and above');

  Ico := nil;

  try
    { Load the icons }
    F := TFile.Create(IcoFileName, fdOpenExisting, faRead, fsRead);
    try
      N := F.CappedSize;
      if Cardinal(N) > Cardinal($100000) then  { sanity check }
        ResUpdateError('Icon file is too large');
      GetMem(Ico, N);
      F.ReadBuffer(Ico^, N);
    finally
      F.Free;
    end;

    { Ensure the icon is valid }
    if not IsValidIcon(Ico, N) then
      ResUpdateError('Icon file is invalid');

    { Update the resources }
    H := BeginUpdateResource(PChar(FileName), False);
    if H = 0 then
      ResUpdateErrorWithLastError('BeginUpdateResource failed (1)');
    try
      M := LoadLibraryEx(PChar(FileName), 0, LOAD_LIBRARY_AS_DATAFILE);
      if M = 0 then
        ResUpdateErrorWithLastError('LoadLibraryEx failed (1)');
      try
        { Load the 'MAINICON' group icon resource }
        R := FindResource(M, 'MAINICON', RT_GROUP_ICON);
        if R = 0 then
          ResUpdateErrorWithLastError('FindResource failed (1)');
        Res := LoadResource(M, R);
        if Res = 0 then
          ResUpdateErrorWithLastError('LoadResource failed (1)');
        GroupIconDir := LockResource(Res);
        if GroupIconDir = nil then
          ResUpdateErrorWithLastError('LockResource failed (1)');

        { Delete 'MAINICON' }
        if not GetResourceLanguage(M, RT_GROUP_ICON, 'MAINICON', wLanguage) then
          ResUpdateError('GetResourceLanguage failed (1)');
        if not UpdateResource(H, RT_GROUP_ICON, 'MAINICON', wLanguage, nil, 0) then
          ResUpdateErrorWithLastError('UpdateResource failed (1)');

        { Delete the RT_ICON icon resources that belonged to 'MAINICON' }
        for I := 0 to GroupIconDir.ItemCount-1 do begin
          if not GetResourceLanguage(M, RT_ICON, MakeIntResource(GroupIconDir.Items[I].Id), wLanguage) then
            ResUpdateError('GetResourceLanguage failed (2)');
          if not UpdateResource(H, RT_ICON, MakeIntResource(GroupIconDir.Items[I].Id), wLanguage, nil, 0) then
            ResUpdateErrorWithLastError('UpdateResource failed (2)');
        end;

        { Build the new group icon resource }
        NewGroupIconDirSize := 3*SizeOf(Word)+Ico.ItemCount*SizeOf(TGroupIconDirItem);
        GetMem(NewGroupIconDir, NewGroupIconDirSize);
        try
          { Build the new group icon resource }
          NewGroupIconDir.Reserved := GroupIconDir.Reserved;
          NewGroupIconDir.Typ := GroupIconDir.Typ;
          NewGroupIconDir.ItemCount := Ico.ItemCount;
          for I := 0 to NewGroupIconDir.ItemCount-1 do begin
            NewGroupIconDir.Items[I].Header := Ico.Items[I].Header;
            NewGroupIconDir.Items[I].Id := I+1; //assumes that there aren't any icons left
          end;

          { Update 'MAINICON' }
          for I := 0 to NewGroupIconDir.ItemCount-1 do
            if not UpdateResource(H, RT_ICON, MakeIntResource(NewGroupIconDir.Items[I].Id), 1033, Pointer(DWORD(Ico) + Ico.Items[I].Offset), Ico.Items[I].Header.ImageSize) then
              ResUpdateErrorWithLastError('UpdateResource failed (3)');

          { Update the icons }
          if not UpdateResource(H, RT_GROUP_ICON, 'MAINICON', 1033, NewGroupIconDir, NewGroupIconDirSize) then
            ResUpdateErrorWithLastError('UpdateResource failed (4)');
        finally
          FreeMem(NewGroupIconDir);
        end;
      finally
        FreeLibrary(M);
      end;
    except
      EndUpdateResource(H, True);  { discard changes }
      raise;
    end;
    if not EndUpdateResource(H, False) then
      ResUpdateErrorWithLastError('EndUpdateResource failed');
  finally
    FreeMem(Ico);
  end;
end;

procedure RemoveManifestDllHijackProtection(const F: TCustomFile; const TestBlockOnly: Boolean);
const
  BlockStartText: AnsiString = '<file name="';
  BlockLength = 250;
var
  S: AnsiString;
  Offset: Integer64;
  P: Integer;
begin
  { Read the manifest resource into a string }
  SetString(S, nil, SeekToResourceData(F, 24, 1));
  Offset := F.Position;
  F.ReadBuffer(S[1], Length(S));

  { Locate and update the block with file elements }
  P := Pos(BlockStartText, S);
  if P = 0 then
    ResUpdateError('Block not found');
  if Copy(S, P+BlockLength, 11) <> '</assembly>' then
    ResUpdateError('Block too short');

  if TestBlockOnly then
    Exit;

  Inc64(Offset, P-1);
  F.Seek64(Offset);
  F.WriteAnsiString(AnsiString(Format('%*s', [BlockLength, ' '])));
end;

end.
