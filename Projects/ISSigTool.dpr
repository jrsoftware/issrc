program ISSigTool;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  "issigtool" utility
}

uses
  SafeDLLPath in '..\Components\SafeDLLPath.pas',
  SysUtils,
  Classes,
  PathFunc in '..\Components\PathFunc.pas',
  SHA256 in '..\Components\SHA256.pas',
  ECDSA in '..\Components\ECDSA.pas',
  StringScanner in '..\Components\StringScanner.pas',
  ISSigFunc in '..\Components\ISSigFunc.pas',
  Shared.CommonFunc in 'Src\Shared.CommonFunc.pas',
  Shared.FileClass in 'Src\Shared.FileClass.pas',
  Shared.Int64Em in 'Src\Shared.Int64Em.pas';

{$APPTYPE CONSOLE}
{$SETPEOSVERSION 6.1}
{$SETPESUBSYSVERSION 6.1}
{$WEAKLINKRTTI ON}

{$R Res\ISSigTool.manifest.res}
{$R Res\ISSigTool.versionandicon.res}

var
  Options: record
    KeyFile: String;
    Quiet: Boolean;
  end;

procedure RaiseFatalError(const Msg: String);
begin
  raise Exception.Create(Msg);
end;

procedure RaiseFatalErrorFmt(const Msg: String; const Args: array of const);
begin
  raise Exception.CreateFmt(Msg, Args);
end;

procedure Print(const S: String; const IncludeNewLine: Boolean = True);
begin
  if IncludeNewLine then
    Writeln(S)
  else
    Write(S);
end;

procedure PrintUnlessQuiet(const S: String;
  const IncludeNewLine: Boolean = True);
begin
  if not Options.Quiet then
    Print(S, IncludeNewLine);
end;

procedure PrintFmtUnlessQuiet(const S: String; const Args: array of const;
  const IncludeNewLine: Boolean = True);
begin
  if not Options.Quiet then
    Print(Format(S, Args), IncludeNewLine);
end;

function CalcFileHash(const AFile: TFile): TSHA256Digest;
var
  Buf: array[0..$FFFF] of Byte;
begin
  var Context: TSHA256Context;
  SHA256Init(Context);
  while True do begin
    const BytesRead = AFile.Read(Buf, SizeOf(Buf));
    if BytesRead = 0 then
      Break;
    SHA256Update(Context, Buf, BytesRead);
  end;
  Result := SHA256Final(Context);
end;

procedure ImportKey(const AKey: TECDSAKey; const ANeedPrivateKey: Boolean);
begin
  const ImportResult = ISSigImportKeyText(AKey,
    ISSigLoadTextFromFile(Options.KeyFile), ANeedPrivateKey);
  if ImportResult <> ikrSuccess then begin
    case ImportResult of
      ikrMalformed:
        RaiseFatalError('Key file is malformed');
      ikrNotPrivateKey:
        RaiseFatalError('Key file must be a private key when signing');
    end;
    RaiseFatalError('Unknown import key result');
  end;
end;

procedure CommandExportPublicKey(const AFilename: String);
begin
  const Key = TECDSAKey.Create;
  try
    ImportKey(Key, False);

    var PublicKeyText: String;
    ISSigExportPublicKeyText(Key, PublicKeyText);
    ISSigSaveTextToFile(AFilename, PublicKeyText);

    PrintFmtUnlessQuiet('Public key file created: "%s"', [AFilename]);
  finally
    Key.Free;
  end;
end;

procedure CommandGeneratePrivateKey;
begin
  if NewFileExists(Options.KeyFile) then
    RaiseFatalError('Key file already exists');

  const Key = TECDSAKey.Create;
  try
    Key.GenerateKeyPair;

    var PrivateKeyText: String;
    ISSigExportPrivateKeyText(Key, PrivateKeyText);
    ISSigSaveTextToFile(Options.KeyFile, PrivateKeyText);

    PrintFmtUnlessQuiet('Private key file created: "%s"', [Options.KeyFile]);
  finally
    Key.Free;
  end;
end;

procedure SignSingleFile(const AKey: TECDSAKey; const AFilename: String);
begin
  PrintFmtUnlessQuiet('%s: ', [AFilename], False);

  var FileSize: Int64;
  var FileHash: TSHA256Digest;
  const F = TFile.Create(AFilename, fdOpenExisting, faRead, fsRead);
  try
    FileSize := Int64(F.Size);
    FileHash := CalcFileHash(F);
  finally
    F.Free;
  end;

  const SigFilename = AFilename + '.issig';

  { ECDSA signature output is non-deterministic: signing the same hash with
    the same key produces a totally different signature each time. To avoid
    unnecessary alterations to the "sig-r" and "sig-s" values when a file is
    being re-signed but its contents haven't changed, we attempt to load and
    verify the existing .issig file. If the key, file size, and file hash are
    all up to date, then we skip creation of a new .issig file. }
  if NewFileExists(SigFilename) then begin
    const ExistingSigText = ISSigLoadTextFromFile(SigFilename);
    var ExistingFileSizeValue: Int64;
    var ExistingFileHashValue: TSHA256Digest;
    if ISSigVerifySignatureText([AKey], ExistingSigText, ExistingFileSizeValue,
       ExistingFileHashValue) = vsrSuccess then begin
      if (FileSize = ExistingFileSizeValue) and
         SHA256DigestsEqual(FileHash, ExistingFileHashValue) then begin
        PrintUnlessQuiet('signature unchanged');
        Exit;
      end;
    end;
  end;

  const SigText = ISSigCreateSignatureText(AKey, FileSize, FileHash);
  ISSigSaveTextToFile(SigFilename, SigText);
  PrintUnlessQuiet('signature written');
end;

procedure CommandSign(const AFilenames: TStringList);
begin
  const Key = TECDSAKey.Create;
  try
    ImportKey(Key, True);

    for var CurFilename in AFilenames do
      SignSingleFile(Key, CurFilename);
  finally
    Key.Free;
  end;
end;

function VerifySingleFile(const AKey: TECDSAKey; const AFilename: String): Boolean;
begin
  Result := False;
  PrintFmtUnlessQuiet('%s: ', [AFilename], False);

  if not NewFileExists(AFilename) then begin
    PrintUnlessQuiet('MISSINGFILE (File does not exist)');
    Exit;
  end;

  const SigFilename = AFilename + '.issig';
  if not NewFileExists(SigFilename) then begin
    PrintUnlessQuiet('MISSINGSIGFILE (Signature file does not exist)');
    Exit;
  end;

  const SigText = ISSigLoadTextFromFile(SigFilename);
  var ExpectedFileSize: Int64;
  var ExpectedFileHash: TSHA256Digest;
  const VerifyResult = ISSigVerifySignatureText([AKey], SigText,
    ExpectedFileSize, ExpectedFileHash);
  if VerifyResult <> vsrSuccess then begin
    case VerifyResult of
      vsrMalformed, vsrBadSignature:
        PrintUnlessQuiet('BADSIGFILE (Signature file is not valid)');
      vsrKeyNotFound:
        PrintUnlessQuiet('UNKNOWNKEY (Incorrect key ID)');
    else
      RaiseFatalError('Unknown verify result');
    end;
    Exit;
  end;

  const F = TFile.Create(AFilename, fdOpenExisting, faRead, fsRead);
  try
    if Int64(F.Size) <> ExpectedFileSize then begin
      PrintUnlessQuiet('WRONGSIZE (File size is incorrect)');
      Exit;
    end;
    const ActualFileHash = CalcFileHash(F);
    if not SHA256DigestsEqual(ActualFileHash, ExpectedFileHash) then begin
      PrintUnlessQuiet('WRONGHASH (File hash is incorrect)');
      Exit;
    end;
  finally
    F.Free;
  end;

  PrintUnlessQuiet('OK');
  Result := True;
end;

function CommandVerify(const AFilenames: TStringList): Boolean;
begin
  const Key = TECDSAKey.Create;
  try
    ImportKey(Key, False);

    Result := True;
    for var CurFilename in AFilenames do
      if not VerifySingleFile(Key, CurFilename) then
        Result := False;
  finally
    Key.Free;
  end;
end;

procedure ShowUsage;
begin
  Writeln(ErrOutput, 'Inno Setup Signature Tool');
  Writeln(ErrOutput, 'Copyright (C) 1997-2025 Jordan Russell. All rights reserved.');
  Writeln(ErrOutput, 'Portions Copyright (C) 2000-2025 Martijn Laan. All rights reserved.');
  Writeln(ErrOutput, 'https://www.innosetup.com');
  Writeln(ErrOutput, '');
  Writeln(ErrOutput, 'Usage:  issigtool [options] sign <filenames>');
  Writeln(ErrOutput, 'or to verify:  issigtool [options] verify <filenames>');
  Writeln(ErrOutput, 'or to export the public key:  issigtool [options] export-public-key <filename>');
  Writeln(ErrOutput, 'or to generate a new private key:  issigtool [options] generate-private-key');
  Writeln(ErrOutput, 'Options:');
  Writeln(ErrOutput, '  --key-file=<filename> Specifies the private key filename (overrides ISSIGTOOL_KEY_FILE environment variable)');
  Writeln(ErrOutput, '  --quiet, -q           Suppresses status messages that are normally printed to standard output');
  Writeln(ErrOutput, '  --help, -?            Prints this information');
  Writeln(ErrOutput, '');
end;

procedure Go;
begin
  const ArgList = TStringList.Create;
  try
    for var I := 1 to NewParamCount do
      ArgList.Add(NewParamStr(I));

    const InitialArgListCount = ArgList.Count;
    var J := 0;
    while J < ArgList.Count do begin
      const S = ArgList[J];
      if S.StartsWith('-') then begin
        if (S = '--help') or (S = '-?') then begin
          ShowUsage;
          if InitialArgListCount <> 1 then
            RaiseFatalErrorFmt('"%s" option cannot be combined with other arguments', [S]);
          Exit;
        end else if (S = '--quiet') or (S = '-q') then begin
          Options.Quiet := True;
        end else if S.StartsWith('--key-file=') then begin
          Options.KeyFile := S.Substring(Length('--key-file='));
        end else
          RaiseFatalErrorFmt('Unknown option "%s".', [S]);
        ArgList.Delete(J);
      end else begin
        if S = '' then
          RaiseFatalError('Empty arguments not allowed');
        Inc(J);
      end;
    end;

    if ArgList.Count = 0 then begin
      ShowUsage;
      RaiseFatalError('Missing command argument');
    end;
    const Command = ArgList[0];
    ArgList.Delete(0);

    if Options.KeyFile = '' then begin
      Options.KeyFile := GetEnv('ISSIGTOOL_KEY_FILE');
      if Options.KeyFile = '' then
        RaiseFatalError('"--key-file=" option must be specified, ' +
          'or set the ISSIGTOOL_KEY_FILE environment variable');
    end;

    if Command = 'export-public-key' then begin
      if ArgList.Count = 0 then
        RaiseFatalError('Missing filename argument')
      else if ArgList.Count <> 1 then
        RaiseFatalError('Too many arguments');
      CommandExportPublicKey(ArgList[0]);
    end else if Command = 'generate-private-key' then begin
      if ArgList.Count <> 0 then
        RaiseFatalError('Too many arguments');
      CommandGeneratePrivateKey;
    end else if Command = 'sign' then begin
      if ArgList.Count = 0 then
        RaiseFatalError('Missing filename argument(s)');
      CommandSign(ArgList);
    end else if Command = 'verify' then begin
      if ArgList.Count = 0 then
        RaiseFatalError('Missing filename argument(s)');
      if not CommandVerify(ArgList) then
        Halt(1);
    end else
      RaiseFatalErrorFmt('Unknown command "%s"', [Command]);
  finally
    ArgList.Free;
  end;
end;

begin
  try
    Go;
  except
    Writeln(ErrOutput, 'issigtool fatal error: ', GetExceptMessage);
    Halt(2);
  end;
end.
