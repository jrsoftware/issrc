unit Shared.FileClass;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  TFile class
  Better than File and TFileStream in that does more extensive error checking
  and uses descriptive, localized system error messages.

  TTextFileReader and TTextFileWriter support ANSI and UTF8 textfiles.
}

interface

uses
  Windows, SysUtils, Shared.Int64Em;

type
  TFileCreateDisposition = (fdCreateAlways, fdCreateNew, fdOpenExisting,
    fdOpenAlways, fdTruncateExisting);
  TFileAccess = (faRead, faWrite, faReadWrite);
  TFileSharing = (fsNone, fsRead, fsWrite, fsReadWrite);

  TCustomFile = class
  private
    function GetCappedSize: Cardinal;
  protected
    function GetPosition: Integer64; virtual; abstract;
    function GetSize: Integer64; virtual; abstract;
  public
    class procedure RaiseError(ErrorCode: DWORD);
    class procedure RaiseLastError;
    function Read(var Buffer; Count: Cardinal): Cardinal; virtual; abstract;
    procedure ReadBuffer(var Buffer; Count: Cardinal);
    procedure Seek(Offset: Cardinal);
    procedure Seek64(Offset: Integer64); virtual; abstract;
    procedure WriteAnsiString(const S: AnsiString);
    procedure WriteBuffer(const Buffer; Count: Cardinal); virtual; abstract;
    property CappedSize: Cardinal read GetCappedSize;
    property Position: Integer64 read GetPosition;
    property Size: Integer64 read GetSize;
  end;

  TFile = class(TCustomFile)
  private
    FHandle: THandle;
    FHandleCreated: Boolean;
  protected
    function CreateHandle(const AFilename: String;
      ACreateDisposition: TFileCreateDisposition; AAccess: TFileAccess;
      ASharing: TFileSharing): THandle; virtual;
    function GetPosition: Integer64; override;
    function GetSize: Integer64; override;
  public
    constructor Create(const AFilename: String;
      ACreateDisposition: TFileCreateDisposition; AAccess: TFileAccess;
      ASharing: TFileSharing);
    constructor CreateWithExistingHandle(const AHandle: THandle);
    destructor Destroy; override;
    function Read(var Buffer; Count: Cardinal): Cardinal; override;
    procedure Seek64(Offset: Integer64); override;
    procedure SeekToEnd;
    procedure Truncate;
    procedure WriteBuffer(const Buffer; Count: Cardinal); override;
    property Handle: THandle read FHandle;
  end;

  TMemoryFile = class(TCustomFile)
  private
    FMemory: Pointer;
    FSize: Integer64;
    FPosition: Integer64;
    function ClipCount(DesiredCount: Cardinal): Cardinal;
  protected
    procedure AllocMemory(const ASize: Cardinal);
    function GetPosition: Integer64; override;
    function GetSize: Integer64; override;
  public
    constructor Create(const AFilename: String);
    constructor CreateFromMemory(const ASource; const ASize: Cardinal);
    constructor CreateFromZero(const ASize: Cardinal);
    destructor Destroy; override;
    function Read(var Buffer; Count: Cardinal): Cardinal; override;
    procedure Seek64(Offset: Integer64); override;
    procedure WriteBuffer(const Buffer; Count: Cardinal); override;
    property Memory: Pointer read FMemory;
  end;

  TTextFileReader = class(TFile)
  private
    FBufferOffset, FBufferSize: Cardinal;
    FEof: Boolean;
    FBuffer: array[0..4095] of AnsiChar;
    FSawFirstLine: Boolean;
    FCodePage: Cardinal;
    function DoReadLine(const UTF8: Boolean): AnsiString;
    function GetEof: Boolean;
    procedure FillBuffer;
  public
    function ReadLine: String;
    function ReadAnsiLine: AnsiString;
    property CodePage: Cardinal write FCodePage;
    property Eof: Boolean read GetEof;
  end;

  TTextFileWriter = class(TFile)
  private
    FSeekedToEnd: Boolean;
    FUTF8WithoutBOM: Boolean;
    procedure DoWrite(const S: AnsiString; const UTF8: Boolean);
  protected
    function CreateHandle(const AFilename: String;
      ACreateDisposition: TFileCreateDisposition; AAccess: TFileAccess;
      ASharing: TFileSharing): THandle; override;
  public
    property UTF8WithoutBOM: Boolean read FUTF8WithoutBOM write FUTF8WithoutBOM;
    procedure Write(const S: String);
    procedure WriteLine(const S: String);
    procedure WriteAnsi(const S: AnsiString);
    procedure WriteAnsiLine(const S: AnsiString);
  end;

  TFileMapping = class
  private
    FMemory: Pointer;
    FMapSize: Cardinal;
    FMappingHandle: THandle;
  public
    constructor Create(AFile: TFile; AWritable: Boolean);
    destructor Destroy; override;
    procedure Commit;
    procedure ReraiseInPageErrorAsFileException;
    property MapSize: Cardinal read FMapSize;
    property Memory: Pointer read FMemory;
  end;

  EFileError = class(Exception)
  private
    FErrorCode: DWORD;
  public
    property ErrorCode: DWORD read FErrorCode;
  end;

implementation

uses
  WideStrUtils,
  Shared.CommonFunc;

const
  SGenericIOError = 'File I/O error %d';

{ TCustomFile }

function TCustomFile.GetCappedSize: Cardinal;
{ Like GetSize, but capped at $7FFFFFFF }
var
  S: Integer64;
begin
  S := GetSize;
  if (S.Hi = 0) and (S.Lo and $80000000 = 0) then
    Result := S.Lo
  else
    Result := $7FFFFFFF;
end;

class procedure TCustomFile.RaiseError(ErrorCode: DWORD);
var
  S: String;
  E: EFileError;
begin
  S := Win32ErrorString(ErrorCode);
  if S = '' then begin
    { In case there was no text for the error code. Shouldn't get here under
      normal circumstances. }
    S := Format(SGenericIOError, [ErrorCode]);
  end;
  E := EFileError.Create(S);
  E.FErrorCode := ErrorCode;
  raise E;
end;

class procedure TCustomFile.RaiseLastError;
begin
  RaiseError(GetLastError);
end;

procedure TCustomFile.ReadBuffer(var Buffer; Count: Cardinal);
begin
  if Read(Buffer, Count) <> Count then begin
    { Raise localized "Reached end of file" error }
    RaiseError(ERROR_HANDLE_EOF);
  end;
end;

procedure TCustomFile.Seek(Offset: Cardinal);
var
  I: Integer64;
begin
  I.Hi := 0;
  I.Lo := Offset;
  Seek64(I);
end;

procedure TCustomFile.WriteAnsiString(const S: AnsiString);
begin
  WriteBuffer(S[1], Length(S));
end;

{ TFile }

constructor TFile.Create(const AFilename: String;
  ACreateDisposition: TFileCreateDisposition; AAccess: TFileAccess;
  ASharing: TFileSharing);
begin
  inherited Create;
  FHandle := CreateHandle(AFilename, ACreateDisposition, AAccess, ASharing);
  if (FHandle = 0) or (FHandle = INVALID_HANDLE_VALUE) then
    RaiseLastError;
  FHandleCreated := True;
end;

constructor TFile.CreateWithExistingHandle(const AHandle: THandle);
begin
  inherited Create;
  FHandle := AHandle;
end;

destructor TFile.Destroy;
begin
  if FHandleCreated then
    CloseHandle(FHandle);
  inherited;
end;

function TFile.CreateHandle(const AFilename: String;
  ACreateDisposition: TFileCreateDisposition; AAccess: TFileAccess;
  ASharing: TFileSharing): THandle;
const
  AccessFlags: array[TFileAccess] of DWORD =
    (GENERIC_READ, GENERIC_WRITE, GENERIC_READ or GENERIC_WRITE);
  SharingFlags: array[TFileSharing] of DWORD =
    (0, FILE_SHARE_READ, FILE_SHARE_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE);
  Disps: array[TFileCreateDisposition] of DWORD =
    (CREATE_ALWAYS, CREATE_NEW, OPEN_EXISTING, OPEN_ALWAYS, TRUNCATE_EXISTING);
begin
  Result := CreateFile(PChar(AFilename), AccessFlags[AAccess],
    SharingFlags[ASharing], nil, Disps[ACreateDisposition],
    FILE_ATTRIBUTE_NORMAL, 0);
end;

function TFile.GetPosition: Integer64;
begin
  Result.Hi := 0;
  Result.Lo := SetFilePointer(FHandle, 0, @Result.Hi, FILE_CURRENT);
  if (Result.Lo = $FFFFFFFF) and (GetLastError <> 0) then
    RaiseLastError;
end;

function TFile.GetSize: Integer64;
begin
  Result.Lo := GetFileSize(FHandle, @Result.Hi);
  if (Result.Lo = $FFFFFFFF) and (GetLastError <> 0) then
    RaiseLastError;
end;

function TFile.Read(var Buffer; Count: Cardinal): Cardinal;
begin
  if not ReadFile(FHandle, Buffer, Count, DWORD(Result), nil) then
    if FHandleCreated or (GetLastError <> ERROR_BROKEN_PIPE) then
      RaiseLastError;
end;

procedure TFile.Seek64(Offset: Integer64);
begin
  if (SetFilePointer(FHandle, Integer(Offset.Lo), @Offset.Hi,
      FILE_BEGIN) = $FFFFFFFF) and (GetLastError <> 0) then
    RaiseLastError;
end;

procedure TFile.SeekToEnd;
var
  DistanceHigh: Integer;
begin
  DistanceHigh := 0;
  if (SetFilePointer(FHandle, 0, @DistanceHigh, FILE_END) = $FFFFFFFF) and
     (GetLastError <> 0) then
    RaiseLastError;
end;

procedure TFile.Truncate;
begin
  if not SetEndOfFile(FHandle) then
    RaiseLastError;
end;

procedure TFile.WriteBuffer(const Buffer; Count: Cardinal);
var
  BytesWritten: DWORD;
begin
  if not WriteFile(FHandle, Buffer, Count, BytesWritten, nil) then
    RaiseLastError;
  if BytesWritten <> Count then begin
    { I'm not aware of any case where WriteFile will return True but a short
      BytesWritten count. (An out-of-disk-space condition causes False to be
      returned.) But if that does happen, raise a generic-sounding localized
      "The system cannot write to the specified device" error. }
    RaiseError(ERROR_WRITE_FAULT);
  end;
end;

{ TMemoryFile }

constructor TMemoryFile.Create(const AFilename: String);
var
  F: TFile;
begin
  inherited Create;
  F := TFile.Create(AFilename, fdOpenExisting, faRead, fsRead);
  try
    AllocMemory(F.CappedSize);
    F.ReadBuffer(FMemory^, FSize.Lo);
  finally
    F.Free;
  end;
end;

constructor TMemoryFile.CreateFromMemory(const ASource; const ASize: Cardinal);
begin
  inherited Create;
  AllocMemory(ASize);
  Move(ASource, FMemory^, FSize.Lo);
end;

constructor TMemoryFile.CreateFromZero(const ASize: Cardinal);
begin
  inherited Create;
  AllocMemory(ASize);
  FillChar(FMemory^, FSize.Lo, 0);
end;

destructor TMemoryFile.Destroy;
begin
  if Assigned(FMemory) then
    LocalFree(HLOCAL(FMemory));
  inherited;
end;

procedure TMemoryFile.AllocMemory(const ASize: Cardinal);
begin
  FMemory := Pointer(LocalAlloc(LMEM_FIXED, ASize));
  if FMemory = nil then
    OutOfMemoryError;
  FSize.Lo := ASize;
end;

function TMemoryFile.ClipCount(DesiredCount: Cardinal): Cardinal;
var
  BytesLeft: Integer64;
begin
  { First check if FPosition is already past FSize, so the Dec6464 call below
    won't underflow }
  if Compare64(FPosition, FSize) >= 0 then begin
    Result := 0;
    Exit;
  end;
  BytesLeft := FSize;
  Dec6464(BytesLeft, FPosition);
  if (BytesLeft.Hi = 0) and (BytesLeft.Lo < DesiredCount) then
    Result := BytesLeft.Lo
  else
    Result := DesiredCount;
end;

function TMemoryFile.GetPosition: Integer64;
begin
  Result := FPosition;
end;

function TMemoryFile.GetSize: Integer64;
begin
  Result := FSize;
end;

function TMemoryFile.Read(var Buffer; Count: Cardinal): Cardinal;
begin
  Result := ClipCount(Count);
  if Result <> 0 then begin
    Move(Pointer(Cardinal(FMemory) + FPosition.Lo)^, Buffer, Result);
    Inc64(FPosition, Result);
  end;
end;

procedure TMemoryFile.Seek64(Offset: Integer64);
begin
  if Offset.Hi and $80000000 <> 0 then
    RaiseError(ERROR_NEGATIVE_SEEK);
  FPosition := Offset;
end;

procedure TMemoryFile.WriteBuffer(const Buffer; Count: Cardinal);
begin
  if ClipCount(Count) <> Count then
    RaiseError(ERROR_HANDLE_EOF);
  if Count <> 0 then begin
    Move(Buffer, Pointer(Cardinal(FMemory) + FPosition.Lo)^, Count);
    Inc64(FPosition, Count);
  end;
end;

{ TTextFileReader }

procedure TTextFileReader.FillBuffer;
begin
  if (FBufferOffset < FBufferSize) or FEof then
    Exit;
  FBufferSize := Read(FBuffer, SizeOf(FBuffer));
  FBufferOffset := 0;
  if FBufferSize = 0 then
    FEof := True;
end;

function TTextFileReader.GetEof: Boolean;
begin
  FillBuffer;
  Result := FEof;
end;

function TTextFileReader.ReadLine: String;
var
 S: RawByteString;
begin
 S := DoReadLine(True);
 if FCodePage <> 0 then
   SetCodePage(S, FCodePage, False);
 Result := String(S);
end; 

function TTextFileReader.ReadAnsiLine: AnsiString;
begin
  Result := DoReadLine(False);
end;

function TTextFileReader.DoReadLine(const UTF8: Boolean): AnsiString;
var
  I, L: Cardinal;
  S: AnsiString;
begin
  while True do begin
    FillBuffer;
    if FEof then begin
      { End of file reached }
      if S = '' then begin
        { If nothing was read (i.e. we were already at EOF), raise localized
          "Reached end of file" error }
        RaiseError(ERROR_HANDLE_EOF);
      end;
      Break;
    end;

    I := FBufferOffset;
    while I < FBufferSize do begin
      if FBuffer[I] in [#10, #13] then
        Break;
      Inc(I);
    end;
    L := Length(S);
    if Integer(L + (I - FBufferOffset)) < 0 then
      OutOfMemoryError;
    SetLength(S, L + (I - FBufferOffset));
    Move(FBuffer[FBufferOffset], S[L+1], I - FBufferOffset);
    FBufferOffset := I;

    if FBufferOffset < FBufferSize then begin
      { End of line reached }
      Inc(FBufferOffset);
      if FBuffer[FBufferOffset-1] = #13 then begin
        { Skip #10 if it follows #13 }
        FillBuffer;
        if (FBufferOffset < FBufferSize) and (FBuffer[FBufferOffset] = #10) then
          Inc(FBufferOffset);
      end;
      Break;
    end;
  end;

  if not FSawFirstLine then begin
    if UTF8 then begin
      { Handle UTF8 as requested: check for a BOM at the start and if not found then check entire file }
      if (Length(S) > 2) and (S[1] = #$EF) and (S[2] = #$BB) and (S[3] = #$BF) then begin
        Delete(S, 1, 3);
        FCodePage := CP_UTF8;
      end else begin
        var OldPosition := GetPosition;
        try
          var CappedSize := GetCappedSize; //can't be 0
          Seek(0);
          var S2: AnsiString;
          SetLength(S2, CappedSize);
          SetLength(S2, Read(S2[1], CappedSize));
          if DetectUTF8Encoding(S2) in [etUSASCII, etUTF8] then
            FCodePage := CP_UTF8;
        finally
          Seek64(OldPosition);
        end;
      end;
    end;
    FSawFirstLine := True;
  end;

  Result := S;
end;

{ TTextFileWriter }

function TTextFileWriter.CreateHandle(const AFilename: String;
  ACreateDisposition: TFileCreateDisposition; AAccess: TFileAccess;
  ASharing: TFileSharing): THandle;
begin
  { faWrite access isn't enough; we need faReadWrite access since the Write
    method may read. No, we don't have to do this automatically, but it helps
    keep it from being a 'leaky abstraction'. }
  if AAccess = faWrite then
    AAccess := faReadWrite;
  Result := inherited CreateHandle(AFilename, ACreateDisposition, AAccess,
    ASharing);
end;

procedure TTextFileWriter.DoWrite(const S: AnsiString; const UTF8: Boolean);
{ Writes a string to the file, seeking to the end first if necessary }
const
  CRLF: array[0..1] of AnsiChar = (#13, #10);
  UTF8BOM: array[0..2] of AnsiChar = (#$EF, #$BB, #$BF);
var
  I: Integer64;
  C: AnsiChar;
begin
  if not FSeekedToEnd then begin
    I := GetSize;
    if (I.Lo <> 0) or (I.Hi <> 0) then begin
      { File is not empty. Figure out if we have to append a line break. }
      Dec64(I, SizeOf(C));
      Seek64(I);
      ReadBuffer(C, SizeOf(C));
      case C of
        #10: ;  { do nothing - file ends in LF or CRLF }
        #13: begin
            { If the file ends in CR, make it into CRLF }
            C := #10;
            WriteBuffer(C, SizeOf(C));
          end;
      else
        { Otherwise, append CRLF }
        WriteBuffer(CRLF, SizeOf(CRLF));
      end;
    end else if UTF8 and not FUTF8WithoutBOM then
      WriteBuffer(UTF8BOM, SizeOf(UTF8BOM));
    FSeekedToEnd := True;
  end;
  WriteBuffer(Pointer(S)^, Length(S));
end;

procedure TTextFileWriter.Write(const S: String);
begin
  DoWrite(Utf8Encode(S), True);
end;

procedure TTextFileWriter.WriteLine(const S: String);
begin
  Write(S + #13#10);
end;

procedure TTextFileWriter.WriteAnsi(const S: AnsiString);
begin
  DoWrite(S, False);
end;

procedure TTextFileWriter.WriteAnsiLine(const S: AnsiString);
begin
  WriteAnsi(S + #13#10);
end;

{ TFileMapping }

type
  NTSTATUS = Longint;
var
  _RtlNtStatusToDosError: function(Status: NTSTATUS): ULONG; stdcall;

constructor TFileMapping.Create(AFile: TFile; AWritable: Boolean);
const
  Protect: array[Boolean] of DWORD = (PAGE_READONLY, PAGE_READWRITE);
  DesiredAccess: array[Boolean] of DWORD = (FILE_MAP_READ, FILE_MAP_WRITE);
begin
  inherited Create;

  if not Assigned(_RtlNtStatusToDosError) then
    _RtlNtStatusToDosError := GetProcAddress(GetModuleHandle('ntdll.dll'),
      'RtlNtStatusToDosError');

  FMapSize := AFile.CappedSize;

  FMappingHandle := CreateFileMapping(AFile.Handle, nil, Protect[AWritable], 0,
    FMapSize, nil);
  if FMappingHandle = 0 then
    TFile.RaiseLastError;

  FMemory := MapViewOfFile(FMappingHandle, DesiredAccess[AWritable], 0, 0,
    FMapSize);
  if FMemory = nil then
    TFile.RaiseLastError;
end;

destructor TFileMapping.Destroy;
begin
  if Assigned(FMemory) then
    UnmapViewOfFile(FMemory);
  if FMappingHandle <> 0 then
    CloseHandle(FMappingHandle);
  inherited;
end;

procedure TFileMapping.Commit;
{ Flushes modified pages to disk. To avoid silent data loss, this should
  always be called prior to destroying a writable TFileMapping instance -- but
  _not_ from a 'finally' section, as this method will raise an exception on
  failure. }
begin
  if not FlushViewOfFile(FMemory, 0) then
    TFile.RaiseLastError;
end;

procedure TFileMapping.ReraiseInPageErrorAsFileException;
{ In Delphi, when an I/O error occurs while accessing a memory-mapped file --
  known as an "inpage error" -- the user will see an exception message of
  "External exception C0000006" by default.
  This method examines the current exception to see if it's an inpage error
  that occurred while accessing our mapped view, and if so, it raises a new
  exception of type EFileError with a more friendly and useful message, like
  you'd see when doing non-memory-mapped I/O with TFile. }
var
  E: TObject;
begin
  E := ExceptObject;
  if (E is EExternalException) and
     (EExternalException(E).ExceptionRecord.ExceptionCode = EXCEPTION_IN_PAGE_ERROR) and
     (Cardinal(EExternalException(E).ExceptionRecord.NumberParameters) >= Cardinal(2)) and
     (Cardinal(EExternalException(E).ExceptionRecord.ExceptionInformation[1]) >= Cardinal(FMemory)) and
     (Cardinal(EExternalException(E).ExceptionRecord.ExceptionInformation[1]) < Cardinal(Cardinal(FMemory) + FMapSize)) then begin
    { There should be a third parameter containing the NT status code of the error
      condition that caused the exception. Convert that into a Win32 error code
      and use it to generate our error message. }
    if (Cardinal(EExternalException(E).ExceptionRecord.NumberParameters) >= Cardinal(3)) and
       Assigned(_RtlNtStatusToDosError) then
      TFile.RaiseError(_RtlNtStatusToDosError(EExternalException(E).ExceptionRecord.ExceptionInformation[2]))
    else begin
      { Use generic "The system cannot [read|write] to the specified device" errors }
      if EExternalException(E).ExceptionRecord.ExceptionInformation[0] = 0 then
        TFile.RaiseError(ERROR_READ_FAULT)
      else
        TFile.RaiseError(ERROR_WRITE_FAULT);
    end;
  end;
end;

end.
