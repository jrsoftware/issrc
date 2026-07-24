unit Compression.CompressorProps;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Customization for compression properties. 
}

interface

uses
  SysUtils, Compression.Base;

type
  TLZMACompressorProps = class(TCompressorProps)
  public
    Algorithm: Integer;
    BlockSize: Integer;
    BTMode: Integer;
    DictionarySize: Cardinal;
    NumBlockThreads: Integer;
    NumFastBytes: Integer;
    NumThreads: Integer;
    NumThreadGroups: Integer;
    WorkerProcessCheckTrust: Boolean;
    WorkerProcessOnCheckedTrust: TProc<Boolean>;
    WorkerProcessFilename: String;
    UseSolidCompression: Boolean;
    constructor Create;
  end;

implementation

{ TLZMACompressorProps }

constructor TLZMACompressorProps.Create;
begin
  inherited;
  Algorithm := -1;
  BTMode := -1;
end;

end.
