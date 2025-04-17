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
  KeyFilename: String;

procedure RaiseFatalError(const Msg: String);
begin
  raise Exception.Create(Msg);
end;

procedure RaiseFatalErrorFmt(const Msg: String; const Args: array of const);
begin
  raise Exception.CreateFmt(Msg, Args);
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

procedure CheckImportKeyResult(const AResult: TISSigImportKeyResult);
begin
  case AResult of
    ikrSuccess:
      Exit;
    ikrMalformed:
      RaiseFatalError('Key file is malformed');
    ikrNotPrivateKey:
      RaiseFatalError('Key file must be a private key when signing');
  end;
  RaiseFatalError('Unknown import key result');
end;

procedure CommandGeneratePrivateKey;
begin
  if NewFileExists(KeyFilename) then
    RaiseFatalError('Key file already exists');

  const Key = TECDSAKey.Create;
  try
    Key.GenerateKeyPair;

    var PrivateKeyText: String;
    ISSigExportPrivateKeyText(Key, PrivateKeyText);
    ISSigSaveTextToFile(KeyFilename, PrivateKeyText);

    Writeln(KeyFilename, ': OK');
  finally
    Key.Free;
  end;
end;

procedure SignSingleFile(const AKey: TECDSAKey; const AFilename: String);
begin
  var FileSize: Int64;
  var FileHash: TSHA256Digest;
  const F = TFile.Create(AFilename, fdOpenExisting, faRead, fsRead);
  try
    FileSize := Int64(F.Size);
    FileHash := CalcFileHash(F);
  finally
    F.Free;
  end;

  const SigText = ISSigCreateSignatureText(AKey, FileSize, FileHash);
  const ISSigFilename = AFilename + '.issig';
  ISSigSaveTextToFile(ISSigFilename, SigText);

   Writeln(ISSigFilename, ': OK')
end;

procedure CommandSign(const AFilenames: TStringList);
begin
  const Key = TECDSAKey.Create;
  try
    CheckImportKeyResult(ISSigImportKeyText(Key,
      ISSigLoadTextFromFile(KeyFilename), True));

    for var CurFilename in AFilenames do
      SignSingleFile(Key, CurFilename);
  finally
    Key.Free;
  end;
end;

function VerifySingleFile(const AKey: TECDSAKey; const AFilename: String): Boolean;
begin
  Result := False;
  Write(AFilename, ': ');

  if not NewFileExists(AFilename) then begin
    Writeln('MISSINGFILE (File does not exist)');
    Exit;
  end;

  const SigFilename = AFilename + '.issig';
  if not NewFileExists(SigFilename) then begin
    Writeln('MISSINGSIGFILE (Signature file does not exist)');
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
        Writeln('BADSIGFILE (Signature file is not valid)');
      vsrKeyNotFound:
        Writeln('UNKNOWNKEY (Incorrect key ID)');
    else
      RaiseFatalError('Unknown verify result');
    end;
    Exit;
  end;

  const F = TFile.Create(AFilename, fdOpenExisting, faRead, fsRead);
  try
    if Int64(F.Size) <> ExpectedFileSize then begin
      Writeln('WRONGSIZE (File size is incorrect)');
      Exit;
    end;
    const ActualFileHash = CalcFileHash(F);
    if not SHA256DigestsEqual(ActualFileHash, ExpectedFileHash) then begin
      Writeln('WRONGHASH (File hash is incorrect)');
      Exit;
    end;
  finally
    F.Free;
  end;

  Writeln('OK');
  Result := True;
end;

function CommandVerify(const AFilenames: TStringList): Boolean;
begin
  const Key = TECDSAKey.Create;
  try
    CheckImportKeyResult(ISSigImportKeyText(Key,
      ISSigLoadTextFromFile(KeyFilename), False));

    Result := True;
    for var CurFilename in AFilenames do
      if not VerifySingleFile(Key, CurFilename) then
        Result := False;
  finally
    Key.Free;
  end;
end;

procedure ShowBanner;
begin
  Writeln('Inno Setup Command-Line Signature Tool');
  Writeln('Copyright (C) 1997-2025 Jordan Russell. All rights reserved.');
  Writeln('Portions Copyright (C) 2000-2025 Martijn Laan. All rights reserved.');
  Writeln('https://www.innosetup.com');
  Writeln('');
end;

procedure ShowUsage;
begin
  Writeln(ErrOutput, 'Usage:  issigtool [options] sign <filenames>');
  Writeln(ErrOutput, 'or to verify:  issigtool [options] verify <filenames>');
  Writeln(ErrOutput, 'or to generate a private key:  issigtool [options] generate-private-key');
  Writeln(ErrOutput, 'Options:');
  Writeln(ErrOutput, '  --key-file=<filename> Specifies a key filename (overrides ISSIGTOOL_KEY_FILE environment variable)');
  Writeln(ErrOutput, '');
end;

procedure Go;
begin
  const ArgList = TStringList.Create;
  try
    for var I := 1 to NewParamCount do
      ArgList.Add(NewParamStr(I));

    var J := 0;
    while J < ArgList.Count do begin
      const S = ArgList[J];
      if S.StartsWith('--key-file=') then begin
        KeyFilename := S.Substring(Length('--key-file='));
        ArgList.Delete(J);
      end else begin
        if S.StartsWith('-') then
          RaiseFatalErrorFmt('Unknown option "%s"', [S]);
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

    if KeyFilename = '' then begin
      KeyFilename := GetEnv('ISSIGTOOL_KEY_FILE');
      if KeyFilename = '' then
        RaiseFatalError('"--key-file=" option must be specified, ' +
          'or set the ISSIGTOOL_KEY_FILE environment variable');
    end;

    if Command = 'generate-private-key' then begin
      if ArgList.Count <> 0 then
        RaiseFatalError('Too many arguments');
      CommandGeneratePrivateKey;
    end else if Command = 'sign' then begin
      if ArgList.Count = 0 then
        RaiseFatalError('Missing filename argument');
      CommandSign(ArgList);
    end else if Command = 'verify' then begin
      if ArgList.Count = 0 then
        RaiseFatalError('Missing filename argument');
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
    ShowBanner;
    Go;
  except
    Writeln(ErrOutput, 'issigtool fatal error: ', GetExceptMessage);
    Halt(2);
  end;
end.
