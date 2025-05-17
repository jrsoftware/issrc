unit Compression.SevenZipDllDecoder.Interfaces;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Minimal extraction interfaces from 7z(x)(a).dll
}

interface

uses
  Winapi.ActiveX;

const
  { From Guid.txt }
  CLSID_HandlerZip: TGUID = '{23170F69-40C1-278A-1000-000110010000}';
  CLSID_HandlerBZip2: TGUID = '{23170F69-40C1-278A-1000-000110020000}';
  CLSID_HandlerRar: TGUID = '{23170F69-40C1-278A-1000-000110030000}';
  CLSID_HandlerArj: TGUID = '{23170F69-40C1-278A-1000-000110040000}';
  CLSID_HandlerZ: TGUID = '{23170F69-40C1-278A-1000-000110050000}';
  CLSID_HandlerLzh: TGUID = '{23170F69-40C1-278A-1000-000110060000}';
  CLSID_Handler7z: TGUID = '{23170F69-40C1-278A-1000-000110070000}';
  CLSID_HandlerCab: TGUID = '{23170F69-40C1-278A-1000-000110080000}';
  CLSID_HandlerNsis: TGUID = '{23170F69-40C1-278A-1000-000110090000}';
  CLSID_HandlerLzma: TGUID = '{23170F69-40C1-278A-1000-0001100A0000}';
  CLSID_HandlerLzma86: TGUID = '{23170F69-40C1-278A-1000-0001100B0000}';
  CLSID_HandlerXz: TGUID = '{23170F69-40C1-278A-1000-0001100C0000}';
  CLSID_HandlerPpmd: TGUID = '{23170F69-40C1-278A-1000-0001100D0000}';
  CLSID_HandlerZstd: TGUID = '{23170F69-40C1-278A-1000-0001100E0000}';
  CLSID_HandlerLVM: TGUID = '{23170F69-40C1-278A-1000-000110BF0000}';
  CLSID_HandlerAVB: TGUID = '{23170F69-40C1-278A-1000-000110C00000}';
  CLSID_HandlerLP: TGUID = '{23170F69-40C1-278A-1000-000110C10000}';
  CLSID_HandlerSparse: TGUID = '{23170F69-40C1-278A-1000-000110C20000}';
  CLSID_HandlerAPFS: TGUID = '{23170F69-40C1-278A-1000-000110C30000}';
  CLSID_HandlerVhdx: TGUID = '{23170F69-40C1-278A-1000-000110C40000}';
  CLSID_HandlerBase64: TGUID = '{23170F69-40C1-278A-1000-000110C50000}';
  CLSID_HandlerCOFF: TGUID = '{23170F69-40C1-278A-1000-000110C60000}';
  CLSID_HandlerExt: TGUID = '{23170F69-40C1-278A-1000-000110C70000}';
  CLSID_HandlerVMDK: TGUID = '{23170F69-40C1-278A-1000-000110C80000}';
  CLSID_HandlerVDI: TGUID = '{23170F69-40C1-278A-1000-000110C90000}';
  CLSID_HandlerQcow: TGUID = '{23170F69-40C1-278A-1000-000110CA0000}';
  CLSID_HandlerGPT: TGUID = '{23170F69-40C1-278A-1000-000110CB0000}';
  CLSID_HandlerRar5: TGUID = '{23170F69-40C1-278A-1000-000110CC0000}';
  CLSID_HandlerIHex: TGUID = '{23170F69-40C1-278A-1000-000110CD0000}';
  CLSID_HandlerHxs: TGUID = '{23170F69-40C1-278A-1000-000110CE0000}';
  CLSID_HandlerTE: TGUID = '{23170F69-40C1-278A-1000-000110CF0000}';
  CLSID_HandlerUEFIc: TGUID = '{23170F69-40C1-278A-1000-000110D00000}';
  CLSID_HandlerUEFIs: TGUID = '{23170F69-40C1-278A-1000-000110D10000}';
  CLSID_HandlerSquashFS: TGUID = '{23170F69-40C1-278A-1000-000110D20000}';
  CLSID_HandlerCramFS: TGUID = '{23170F69-40C1-278A-1000-000110D30000}';
  CLSID_HandlerAPM: TGUID = '{23170F69-40C1-278A-1000-000110D40000}';
  CLSID_HandlerMslz: TGUID = '{23170F69-40C1-278A-1000-000110D50000}';
  CLSID_HandlerFlv: TGUID = '{23170F69-40C1-278A-1000-000110D60000}';
  CLSID_HandlerSwf: TGUID = '{23170F69-40C1-278A-1000-000110D70000}';
  CLSID_HandlerSwfc: TGUID = '{23170F69-40C1-278A-1000-000110D80000}';
  CLSID_HandlerNtfs: TGUID = '{23170F69-40C1-278A-1000-000110D90000}';
  CLSID_HandlerFat: TGUID = '{23170F69-40C1-278A-1000-000110DA0000}';
  CLSID_HandlerMbr: TGUID = '{23170F69-40C1-278A-1000-000110DB0000}';
  CLSID_HandlerVhd: TGUID = '{23170F69-40C1-278A-1000-000110DC0000}';
  CLSID_HandlerPe: TGUID = '{23170F69-40C1-278A-1000-000110DD0000}';
  CLSID_HandlerElf: TGUID = '{23170F69-40C1-278A-1000-000110DE0000}';
  CLSID_HandlerMachO: TGUID = '{23170F69-40C1-278A-1000-000110DF0000}';
  CLSID_HandlerUdf: TGUID = '{23170F69-40C1-278A-1000-000110E00000}';
  CLSID_HandlerXar: TGUID = '{23170F69-40C1-278A-1000-000110E10000}';
  CLSID_HandlerMub: TGUID = '{23170F69-40C1-278A-1000-000110E20000}';
  CLSID_HandlerHfs: TGUID = '{23170F69-40C1-278A-1000-000110E30000}';
  CLSID_HandlerDmg: TGUID = '{23170F69-40C1-278A-1000-000110E40000}';
  CLSID_HandlerCompound: TGUID = '{23170F69-40C1-278A-1000-000110E50000}';
  CLSID_HandlerWim: TGUID = '{23170F69-40C1-278A-1000-000110E60000}';
  CLSID_HandlerIso: TGUID = '{23170F69-40C1-278A-1000-000110E70000}';
  CLSID_HandlerChm: TGUID = '{23170F69-40C1-278A-1000-000110E90000}';
  CLSID_HandlerSplit: TGUID = '{23170F69-40C1-278A-1000-000110EA0000}';
  CLSID_HandlerRpm: TGUID = '{23170F69-40C1-278A-1000-000110EB0000}';
  CLSID_HandlerDeb: TGUID = '{23170F69-40C1-278A-1000-000110EC0000}';
  CLSID_HandlerCpio: TGUID = '{23170F69-40C1-278A-1000-000110ED0000}';
  CLSID_HandlerTar: TGUID = '{23170F69-40C1-278A-1000-000110EE0000}';
  CLSID_HandlerGZip: TGUID = '{23170F69-40C1-278A-1000-000110EF0000}';

  { From PropID.h}
  kpidPath = 3;
  kpidIsDir = 6;

  { From IArchive.h}
  kExtract = 0;

type
  { From IStream.h }
  ISequentialInStream = interface(IUnknown)
  ['{23170F69-40C1-278A-0000-000300010000}'] { From Guid.txt }
    function Read(data: Pointer; size: UInt32; processedSize: PUInt32): HRESULT; stdcall;
  end;

  ISequentialOutStream = interface(IUnknown)
  ['{23170F69-40C1-278A-0000-000300020000}']
    function Write(data: Pointer; size: UInt32; processedSize: PUint32): HRESULT; stdcall;
  end;

  IInStream = interface(ISequentialInStream)
  ['{23170F69-40C1-278A-0000-000300030000}']
    function Seek(offset: Int64; seekOrigin: UInt32; newPosition: PUInt64): HRESULT; stdcall;
  end;

  { From IProgress.h }
  IProgress = interface(IUnknown)
  ['{23170F69-40C1-278A-0000-000000050000}']
    function SetTotal(total: UInt64): HRESULT; stdcall;
    function SetCompleted(completeValue: PUInt64): HRESULT; stdcall;
  end;

  { From IArchive.h }
  IArchiveOpenCallback = interface
  ['{23170F69-40C1-278A-0000-000600100000}']
    function SetTotal(files, bytes: PUInt64): HRESULT; stdcall;
    function SetCompleted(files, bytes: PUInt64): HRESULT; stdcall;
  end;

  IArchiveExtractCallback = interface(IProgress)
  ['{23170F69-40C1-278A-0000-000600200000}']
    function GetStream(index: UInt32; out outStream: ISequentialOutStream;
      askExtractMode: Int32): HRESULT; stdcall;
    function PrepareOperation(askExtractMode: Int32): HRESULT; stdcall;
    function SetOperationResult(opRes: Int32): HRESULT; stdcall;
  end;

  IInArchive = interface
  ['{23170F69-40C1-278A-0000-000600600000}']
    function Open(stream: IInStream; const maxCheckStartPosition: PInt64;
      openCallback: IUnknown): HRESULT; stdcall;
    procedure Dummy1;
    procedure Dummy2;
    function GetProperty(index: UInt32; propID: PROPID; out value: OleVariant): HRESULT; stdcall;
    function Extract(indices: Pointer; numItems: UInt32; testMode: Integer;
      extractCallback: IArchiveExtractCallback): HRESULT; stdcall;
  end;

  { From IPassword.h }
  ICryptoGetTextPassword = interface(IUnknown)
  ['{23170F69-40C1-278A-0000-000500100000}']
    function CryptoGetTextPassword(out password: TBStr): HRESULT; stdcall;
  end;

implementation

end.
