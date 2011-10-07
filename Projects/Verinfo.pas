unit VerInfo;

{
  Inno Setup
  Copyright (C) 1997-2007 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Version info functions

  $jrsoftware: issrc/Projects/VerInfo.pas,v 1.10 2007/02/03 20:06:44 jr Exp $
}

interface

uses
  Windows, SysUtils, Int64Em;

{$I VERSION.INC}

type
  TFileVersionNumbers = record
    MS, LS: LongWord;
  end;

function GetVersionInfo(const Filename: String;
  var VersionInfo: TVSFixedFileInfo): Boolean;
function GetVersionNumbers(const Filename: String;
  var VersionNumbers: TFileVersionNumbers): Boolean;

implementation

uses
  CmnFunc2, FileClass;

function GetVXDVersionInfo(const Filename: String;
  var VersionInfo: TVSFixedFileInfo): Boolean;
{ Gets the version info from a VXD file. Returns True if successful.
  Note: The code in this function is based on the code in the MS KB article
  Q201685.
  Known issue: This function fails if the version resource has a name
  (TVXDVersionResource.cName <> $FF). It's rare, but such VXDs do exist --
  see Windows 98's MSGAME.VXD for example. Given that as of 2007 Windows 9x
  is mostly obsolete, I don't plan on fixing this. }
const
  IMAGE_DOS_SIGNATURE = $5A4D;  { MZ }
  IMAGE_VXD_SIGNATURE = $454C;  { LE }
type
  PImageVXDHeader = ^TImageVXDHeader;
  TImageVXDHeader = packed record
    e32_magic: Word;                    // Magic number
    e32_border: Byte;                   // The byte ordering for the VXD
    e32_worder: Byte;                   // The word ordering for the VXD
    e32_level: DWORD;                   // The EXE format level for now = 0
    e32_cpu: Word;                      // The CPU type
    e32_os: Word;                       // The OS type
    e32_ver: DWORD;                     // Module version
    e32_mflags: DWORD;                  // Module flags
    e32_mpages: DWORD;                  // Module # pages
    e32_startobj: DWORD;                // Object # for instruction pointer
    e32_eip: DWORD;                     // Extended instruction pointer
    e32_stackobj: DWORD;                // Object # for stack pointer
    e32_esp: DWORD;                     // Extended stack pointer
    e32_pagesize: DWORD;                // VXD page size
    e32_lastpagesize: DWORD;            // Last page size in VXD
    e32_fixupsize: DWORD;               // Fixup section size
    e32_fixupsum: DWORD;                // Fixup section checksum
    e32_ldrsize: DWORD;                 // Loader section size
    e32_ldrsum: DWORD;                  // Loader section checksum
    e32_objtab: DWORD;                  // Object table offset
    e32_objcnt: DWORD;                  // Number of objects in module
    e32_objmap: DWORD;                  // Object page map offset
    e32_itermap: DWORD;                 // Object iterated data map offset
    e32_rsrctab: DWORD;                 // Offset of Resource Table
    e32_rsrccnt: DWORD;                 // Number of resource entries
    e32_restab: DWORD;                  // Offset of resident name table
    e32_enttab: DWORD;                  // Offset of Entry Table
    e32_dirtab: DWORD;                  // Offset of Module Directive Table
    e32_dircnt: DWORD;                  // Number of module directives
    e32_fpagetab: DWORD;                // Offset of Fixup Page Table
    e32_frectab: DWORD;                 // Offset of Fixup Record Table
    e32_impmod: DWORD;                  // Offset of Import Module Name Table
    e32_impmodcnt: DWORD;               // Number of entries in Import Module Name Table
    e32_impproc: DWORD;                 // Offset of Import Procedure Name Table
    e32_pagesum: DWORD;                 // Offset of Per-Page Checksum Table
    e32_datapage: DWORD;                // Offset of Enumerated Data Pages
    e32_preload: DWORD;                 // Number of preload pages
    e32_nrestab: DWORD;                 // Offset of Non-resident Names Table
    e32_cbnrestab: DWORD;               // Size of Non-resident Name Table
    e32_nressum: DWORD;                 // Non-resident Name Table Checksum
    e32_autodata: DWORD;                // Object # for automatic data object
    e32_debuginfo: DWORD;               // Offset of the debugging information
    e32_debuglen: DWORD;                // The length of the debugging info. in bytes
    e32_instpreload: DWORD;             // Number of instance pages in preload section of VXD file
    e32_instdemand: DWORD;              // Number of instance pages in demand load section of VXD file
    e32_heapsize: DWORD;                // Size of heap - for 16-bit apps
    e32_res3: array[0..11] of Byte;     // Reserved words
    e32_winresoff: DWORD;
    e32_winreslen: DWORD;
    e32_devid: Word;                    // Device ID for VxD
    e32_ddkver: Word;                   // DDK version for VxD
  end;
  PVXDVersionResource = ^TVXDVersionResource;
  TVXDVersionResource = packed record
    cType: Byte;
    wID: Word;
    cName: Byte;
    wOrdinal: Word;
    wFlags: Word;
    dwResSize: DWORD;
  end;
var
  F: TFile;
  DosHeader: packed record
    Sig: Word;
    Other: array[0..57] of Byte;
    VXDHeaderOffset: DWORD;
  end;
  VXDHeader: TImageVXDHeader;
  VXDVersionRes: TVXDVersionResource;
  RootNode: packed record
    cbNode: Word;
    cbData: Word;
    szKey: array[0..15] of AnsiChar;  { should always be 'VS_VERSION_INFO'#0 }
    Value: TVSFixedFileInfo;
  end;
begin
  Result := False;
  try
    if not NewFileExists(Filename) then
      Exit;
    F := TFile.Create(Filename, fdOpenExisting, faRead, fsRead);
    try
      { DOS header }
      if (F.Read(DosHeader, SizeOf(DosHeader)) <> SizeOf(DosHeader)) or
         (DosHeader.Sig <> IMAGE_DOS_SIGNATURE) or
         (DosHeader.VXDHeaderOffset = 0) then
        Exit;

      { VXD header }
      F.Seek(DosHeader.VXDHeaderOffset);
      if (F.Read(VXDHeader, SizeOf(VXDHeader)) <> SizeOf(VXDHeader)) or
         (DWORD(Pointer(@VXDHeader)^) <> IMAGE_VXD_SIGNATURE) then
        Exit;
      if Cardinal(VXDHeader.e32_winreslen) <= Cardinal(SizeOf(VXDVersionRes)) then
        Exit;  { sanity check }

      { Resource }
      F.Seek(VXDHeader.e32_winresoff);
      F.ReadBuffer(VXDVersionRes, SizeOf(VXDVersionRes));
      if (VXDVersionRes.cType <> $FF) or
         (VXDVersionRes.wID <> 16) or  { RT_VERSION? }
         (VXDVersionRes.cName <> $FF) then
        Exit;  { we don't support non-ordinal types/names (see "Known issue") }
      if Cardinal(VXDVersionRes.dwResSize) < Cardinal(SizeOf(RootNode)) then
        Exit;  { sanity check }
      F.ReadBuffer(RootNode, SizeOf(RootNode));
      if (RootNode.cbNode >= SizeOf(RootNode)) and
         (RootNode.cbData >= SizeOf(RootNode.Value)) and
         (RootNode.Value.dwSignature = VS_FFI_SIGNATURE) then begin
        VersionInfo := RootNode.Value;
        Result := True;
      end;
    finally
      F.Free;
    end;
  except
    { don't propogate exceptions }
  end;
end;

function GetVersionInfo(const Filename: String;
  var VersionInfo: TVSFixedFileInfo): Boolean;
var
  VersionSize: Integer;
  VersionHandle: DWORD;
  VersionBuf: PChar;
  VerInfo: PVSFixedFileInfo;
  VerInfoSize: UINT;
begin
  Result := False;

  VersionSize := GetFileVersionInfoSize(PChar(Filename), VersionHandle);
  if VersionSize > 0 then begin
    GetMem(VersionBuf, VersionSize);
    try
      if GetFileVersionInfo(PChar(Filename), VersionHandle, VersionSize, VersionBuf) then begin
        if VerQueryValue(VersionBuf, '\', Pointer(VerInfo), VerInfoSize) then begin
          VersionInfo := VerInfo^;
          Result := True;
        end;
      end;
    finally
      FreeMem(VersionBuf);
    end;
  end
  else if Win32Platform <> VER_PLATFORM_WIN32_WINDOWS then begin
    { NT's version APIs don't support VXDs, so use our own code to handle them }
    Result := GetVXDVersionInfo(Filename, VersionInfo);
  end;
end;

function GetVersionNumbers(const Filename: String;
  var VersionNumbers: TFileVersionNumbers): Boolean;
var
  VerInfo: TVSFixedFileInfo;
begin
  Result := GetVersionInfo(Filename, VerInfo);
  if Result then begin
    VersionNumbers.MS := VerInfo.dwFileVersionMS;
    VersionNumbers.LS := VerInfo.dwFileVersionLS;
  end;
end;

end.
