unit Setup.ISSigVerifyFunc;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Installation procedures: DoISSigVerify
}

interface

uses
  Classes, SHA256, Shared.FileClass, Shared.SetupTypes, Shared.Struct;

const
  VerificationSuccessfulLogMessage = 'Verification successful.';

function NoVerification: TSetupFileVerification;

procedure VerificationError(const AError: TVerificationError;
  const ASigFilename: String = '');

procedure DoISSigVerify(const SourceF: TFile; const SourceFS: TFileStream;
  const SourceFilename: String; const VerifySourceFilename: Boolean; const ISSigAllowedKeys: AnsiString;
  out ExpectedFileHash: TSHA256Digest);

implementation

uses
  SysUtils,
  ISSigFunc, PathFunc,
  SetupLdrAndSetup.Messages,
  Shared.CommonFunc, Shared.SetupMessageIDs,
  Setup.InstFunc, Setup.LoggingFunc, Setup.MainFunc;

function NoVerification: TSetupFileVerification;
begin
  Result := Default(TSetupFileVerification);
  Result.Typ := fvNone;
end;

procedure VerificationError(const AError: TVerificationError;
  const ASigFilename: String);
const
  LogMessages: array[TVerificationError] of String =
    ('Signature file does not exist', 'Signature is malformed', 'No matching key found',
     'Signature is bad', 'File name is incorrect', 'File size is incorrect', 'File hash is incorrect');
  SetupMessageIDs: array[TVerificationError] of TSetupMessageID =
    (msgVerificationSignatureDoesntExist, msgVerificationSignatureInvalid, msgVerificationKeyNotFound,
     msgVerificationSignatureInvalid, msgVerificationFileNameIncorrect, msgVerificationFileSizeIncorrect,
     msgVerificationFileHashIncorrect);
begin
  { Also see Compiler.SetupCompiler for a similar function }
  Log('Verification error: ' + AddPeriod(LogMessages[AError]));
  raise Exception.Create(FmtSetupMessage1(msgSourceVerificationFailed,
    FmtSetupMessage1(SetupMessageIDs[AError], PathExtractName(ASigFilename)))); { Not all messages actually have a %1 parameter but that's OK }
end;

procedure DoISSigVerify(const SourceF: TFile; const SourceFS: TFileStream;
  const SourceFilename: String; const VerifySourceFilename: Boolean; const ISSigAllowedKeys: AnsiString;
  out ExpectedFileHash: TSHA256Digest);
{ Does not disable FS redirection. Either SourceF or SourceFS must be set, which
  may be opened for writing instead of reading.  }
begin
  if ((SourceF = nil) and (SourceFS = nil)) or ((SourceF <> nil) and (SourceFS <> nil)) then
    InternalError('DoISSigVerify: Invalid SourceF / SourceFS combination');

  var ExpectedFileName: String;
  var ExpectedFileSize: Int64;
  if not ISSigVerifySignature(SourceFilename,
    GetISSigAllowedKeys(ISSigAvailableKeys, ISSigAllowedKeys),
    ExpectedFileName, ExpectedFileSize, ExpectedFileHash,
    nil,
    procedure(const Filename, SigFilename: String)
    begin
      VerificationError(veSignatureMissing, SigFilename);
    end,
    procedure(const Filename, SigFilename: String; const VerifyResult: TISSigVerifySignatureResult)
    begin
      case VerifyResult of
        vsrMalformed:  VerificationError(veSignatureMalformed, SigFilename);
        vsrBad: VerificationError(veSignatureBad, SigFilename);
        vsrKeyNotFound: VerificationError(veKeyNotFound, SigFilename);
      else
        InternalError('Unknown ISSigVerifySignature result');
      end;
    end
  ) then
    InternalError('Unexpected ISSigVerifySignature result');
  if VerifySourceFilename and (ExpectedFileName <> '') and not PathSame(PathExtractName(SourceFilename), ExpectedFileName) then
    VerificationError(veFileNameIncorrect);
  var FileSize: Int64;
  if SourceF <> nil then
    FileSize := SourceF.Size
  else
    FileSize := SourceFS.Size;
  if FileSize <> ExpectedFileSize then
    VerificationError(veFileSizeIncorrect);
  { Caller must check ExpectedFileHash }
end;

end.
