unit CompResUpdate;

{
  Inno Setup
  Copyright (C) 1997-2010 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Resource update functions used by the compiler only

  $jrsoftware: issrc/Projects/CompResUpdate.pas,v 1.25 2010/04/05 20:53:41 jr Exp $
}

interface

uses
  Windows, SysUtils, FileClass, VerInfo;

{$I VERSION.INC}

procedure UpdateIcons(const FileName, IcoFileName: String);
procedure UpdateVersionInfo(const F: TFile;
  const NewBinaryFileVersion, NewBinaryProductVersion: TFileVersionNumbers;
  const NewCompanyName, NewFileDescription, NewTextFileVersion, NewLegalCopyright,
  NewProductName, NewTextProductVersion: String);

implementation

uses
  ResUpdate{$IFDEF UNICODE}, Math{$ENDIF};

procedure Error(const Msg: String);
begin
  raise Exception.Create('Resource update error: ' + Msg);
end;

procedure ErrorWithLastError(const Msg: String);
begin
  Error(Msg + ' (' + IntToStr(GetLastError) + ')');
end;

procedure UpdateVersionInfo(const F: TFile;
  const NewBinaryFileVersion, NewBinaryProductVersion: TFileVersionNumbers;
  const NewCompanyName, NewFileDescription, NewTextFileVersion, NewLegalCopyright,
  NewProductName, NewTextProductVersion: String);

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
      Error('Unexpected version resource format (1)');
{$IFDEF UNICODE}
    Move(Pointer(NewValue)^, Value^, (Min(Length(NewValue), lstrlenW(Value)))*SizeOf(Char));
{$ELSE}
    MultiByteToWideChar(CP_ACP, 0, PChar(NewValue), Length(NewValue), Value, lstrlenW(Value));
{$ENDIF}
    ReplaceWithRealCopyrightSymbols(Value);
  end;

  procedure UpdateFixedFileInfo(P: Pointer; const Path: PWideChar; NewFileVersion, NewProductVersion: TFileVersionNumbers);
  var
    FixedFileInfo: PVSFixedFileInfo;
    ValueLen: Cardinal;
  begin
    if not QueryValue(P, Path, Pointer(FixedFileInfo), ValueLen) then
      Error('Unexpected version resource format (2)');
    if FixedFileInfo.dwSignature <> $FEEF04BD then
      Error('Unexpected version resource format (3)');
    FixedFileInfo.dwFileVersionLS := NewFileVersion.LS;
    FixedFileInfo.dwFileVersionMS := NewFileVersion.MS;
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
    UpdateStringValue(VersRes, 'VS_VERSION_INFO'#0'StringFileInfo'#0'000004b0'#0'FileDescription'#0, NewFileDescription);
    UpdateStringValue(VersRes, 'VS_VERSION_INFO'#0'StringFileInfo'#0'000004b0'#0'FileVersion'#0, NewTextFileVersion);
    UpdateStringValue(VersRes, 'VS_VERSION_INFO'#0'StringFileInfo'#0'000004b0'#0'LegalCopyright'#0, NewLegalCopyright);
    UpdateStringValue(VersRes, 'VS_VERSION_INFO'#0'StringFileInfo'#0'000004b0'#0'ProductName'#0, NewProductName);
    UpdateStringValue(VersRes, 'VS_VERSION_INFO'#0'StringFileInfo'#0'000004b0'#0'ProductVersion'#0, NewTextProductVersion);
    UpdateFixedFileInfo(VersRes, 'VS_VERSION_INFO'#0, NewBinaryFileVersion, NewBinaryProductVersion);

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
    Error('Only supported on Windows NT and above');

  Ico := nil;

  try
    { Load the icons }
    F := TFile.Create(IcoFileName, fdOpenExisting, faRead, fsRead);
    try
      N := F.CappedSize;
      if Cardinal(N) > Cardinal($100000) then  { sanity check }
        Error('Icon file is too large');
      GetMem(Ico, N);
      F.ReadBuffer(Ico^, N);
    finally
      F.Free;
    end;

    { Ensure the icon is valid }
    if not IsValidIcon(Ico, N) then
      Error('Icon file is invalid');

    { Update the resources }
    H := BeginUpdateResource(PChar(FileName), False);
    if H = 0 then
      ErrorWithLastError('BeginUpdateResource failed (1)');
    try
      M := LoadLibraryEx(PChar(FileName), 0, LOAD_LIBRARY_AS_DATAFILE);
      if M = 0 then
        ErrorWithLastError('LoadLibraryEx failed (1)');
      try
      	{ Load the 'MAINICON' group icon resource }
        R := FindResource(M, 'MAINICON', RT_GROUP_ICON);
        if R = 0 then
          ErrorWithLastError('FindResource failed (1)');
        Res := LoadResource(M, R);
        if Res = 0 then
          ErrorWithLastError('LoadResource failed (1)');
        GroupIconDir := LockResource(Res);
        if GroupIconDir = nil then
          ErrorWithLastError('LockResource failed (1)');

        { Delete 'MAINICON' }
        if not GetResourceLanguage(M, RT_GROUP_ICON, 'MAINICON', wLanguage) then
          Error('GetResourceLanguage failed (1)');
        if not UpdateResource(H, RT_GROUP_ICON, 'MAINICON', wLanguage, nil, 0) then
          ErrorWithLastError('UpdateResource failed (1)');

        { Delete the RT_ICON icon resources that belonged to 'MAINICON' }
        for I := 0 to GroupIconDir.ItemCount-1 do begin
          if not GetResourceLanguage(M, RT_ICON, MakeIntResource(GroupIconDir.Items[I].Id), wLanguage) then
            Error('GetResourceLanguage failed (2)');
          if not UpdateResource(H, RT_ICON, MakeIntResource(GroupIconDir.Items[I].Id), wLanguage, nil, 0) then
            ErrorWithLastError('UpdateResource failed (2)');
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
              ErrorWithLastError('UpdateResource failed (3)');

          { Update the icons }
          if not UpdateResource(H, RT_GROUP_ICON, 'MAINICON', 1033, NewGroupIconDir, NewGroupIconDirSize) then
            ErrorWithLastError('UpdateResource failed (4)');
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
      ErrorWithLastError('EndUpdateResource failed');
  finally
    FreeMem(Ico);
  end;
end;

end.
