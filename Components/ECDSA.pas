unit ECDSA;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  ECDSA-P256 signing, verification, and key generation, based on CNG (BCrypt)
}

interface

uses
  Windows, SysUtils;

type
  TECDSAInt256 = array[0..31] of Byte;
  TECDSAPublicKey = packed record
    Public_X: TECDSAInt256;
    Public_Y: TECDSAInt256;
  end;
  TECDSAPrivateKey = packed record
    PublicKey: TECDSAPublicKey;
    Private_d: TECDSAInt256;
  end;
  TECDSASignature = packed record
    Sig_r: TECDSAInt256;
    Sig_s: TECDSAInt256;
  end;

  TECDSAKey = class
  private
    FAlgorithmHandle: THandle;  { BCRYPT_ALG_HANDLE }
    FKeyHandle: THandle;        { BCRYPT_KEY_HANDLE }
    class procedure CheckStatus(const AFunctionName: String;
      const AStatus: NTSTATUS); static;
    procedure KeyHandleRequired;
  public
    constructor Create;
    destructor Destroy; override;
    procedure DestroyKey;
    procedure ExportPrivateKey(out APrivateKey: TECDSAPrivateKey);
    procedure GenerateKeyPair;
    procedure ImportPrivateKey([ref] const APrivateKey: TECDSAPrivateKey);
    procedure ImportPublicKey([ref] const APublicKey: TECDSAPublicKey);
    procedure SignHash(const AHash: array of Byte;
      out ASignature: TECDSASignature);
    function VerifySignature(const AHash: array of Byte;
      const ASignature: TECDSASignature): Boolean;
  end;

  EECDSAError = class(Exception);

implementation

type
  BCRYPT_ALG_HANDLE = type THandle;
  BCRYPT_KEY_HANDLE = type THandle;

  BCRYPT_ECCKEY_BLOB = record
    dwMagic: ULONG;
    cbKey: ULONG;
  end;

const
  BCRYPT_ECDSA_P256_ALGORITHM = 'ECDSA_P256';
  BCRYPT_ECCPRIVATE_BLOB = 'ECCPRIVATEBLOB';
  BCRYPT_ECCPUBLIC_BLOB = 'ECCPUBLICBLOB';
  BCRYPT_ECDSA_PRIVATE_P256_MAGIC = $32534345;
  BCRYPT_ECDSA_PUBLIC_P256_MAGIC = $31534345;

  STATUS_INVALID_SIGNATURE = NTSTATUS($C000A000);

  bcrypt = 'bcrypt.dll';

function BCryptCloseAlgorithmProvider(hAlgorithm: BCRYPT_ALG_HANDLE;
  dwFlags: ULONG): NTSTATUS;
  stdcall; external bcrypt;

function BCryptDestroyKey(hKey: BCRYPT_KEY_HANDLE): NTSTATUS;
  stdcall; external bcrypt;

function BCryptExportKey(hKey: BCRYPT_KEY_HANDLE; hExportKey: BCRYPT_KEY_HANDLE;
  pszBlobType: LPCWSTR; var pbOutput; cbOutput: ULONG; out pcbResult: ULONG;
  dwFlags: ULONG): NTSTATUS;
  stdcall; external bcrypt;

function BCryptFinalizeKeyPair(hKey: BCRYPT_KEY_HANDLE; dwFlags: ULONG): NTSTATUS;
  stdcall; external bcrypt;

function BCryptGenerateKeyPair(hAlgorithm: BCRYPT_ALG_HANDLE;
  out phKey: BCRYPT_KEY_HANDLE; dwLength: ULONG; dwFlags: ULONG): NTSTATUS;
  stdcall; external bcrypt;

function BCryptImportKeyPair(hAlgorithm: BCRYPT_ALG_HANDLE;
  hImportKey: BCRYPT_KEY_HANDLE; pszBlobType: LPCWSTR;
  out phKey: BCRYPT_KEY_HANDLE; const pbInput; cbInput: ULONG;
  dwFlags: ULONG): NTSTATUS;
  stdcall; external bcrypt;

function BCryptOpenAlgorithmProvider(out phAlgorithm: BCRYPT_ALG_HANDLE;
  pszAlgId: LPCWSTR; pszImplementation: LPCWSTR; dwFlags: ULONG): NTSTATUS;
  stdcall; external bcrypt;

function BCryptSignHash(hKey: BCRYPT_KEY_HANDLE; pPaddingInfo: Pointer;
  const pbInput; cbInput: ULONG; var pbOutput; cbOutput: ULONG;
  out pcbResult: ULONG; dwFlags: ULONG): NTSTATUS;
  stdcall; external bcrypt;

function BCryptVerifySignature(hKey: BCRYPT_KEY_HANDLE; pPaddingInfo: Pointer;
  const pbHash; cbHash: ULONG; const pbSignature; cbSignature: ULONG;
  dwFlags: ULONG): NTSTATUS;
  stdcall; external bcrypt;

type
  { ECDSA-P256 key blob formats specific to BCrypt }
  TBCryptPrivateKeyBlob = record
    Header: BCRYPT_ECCKEY_BLOB;
    Public_X: TECDSAInt256;
    Public_Y: TECDSAInt256;
    Private_d: TECDSAInt256;
  end;
  TBCryptPublicKeyBlob = record
    Header: BCRYPT_ECCKEY_BLOB;
    Public_X: TECDSAInt256;
    Public_Y: TECDSAInt256;
  end;

{ TECDSAKey }

constructor TECDSAKey.Create;
begin
  inherited;
  var LAlgorithmHandle: BCRYPT_ALG_HANDLE;
  CheckStatus('BCryptOpenAlgorithmProvider',
    BCryptOpenAlgorithmProvider(LAlgorithmHandle, BCRYPT_ECDSA_P256_ALGORITHM,
      nil, 0));
  FAlgorithmHandle := LAlgorithmHandle;  { assign only on success }
end;

destructor TECDSAKey.Destroy;
begin
  DestroyKey;
  if FAlgorithmHandle <> 0 then
    BCryptCloseAlgorithmProvider(FAlgorithmHandle, 0);
  inherited;
end;

class procedure TECDSAKey.CheckStatus(const AFunctionName: String;
  const AStatus: NTSTATUS);
begin
  if AStatus <> 0 then
    raise EECDSAError.CreateFmt('%s failed with error code 0x%x',
      [AFunctionName, AStatus]);
end;

procedure TECDSAKey.DestroyKey;
begin
  const H = FKeyHandle;
  if H <> 0 then begin
    FKeyHandle := 0;
    BCryptDestroyKey(H);
  end;
end;

procedure TECDSAKey.ExportPrivateKey(out APrivateKey: TECDSAPrivateKey);
begin
  KeyHandleRequired;

  var KeyBlob: TBCryptPrivateKeyBlob;
  { Initially clear KeyBlob just to make it easier to verify that
    BCryptExportKey overwrites the entire record }
  FillChar(KeyBlob, SizeOf(KeyBlob), 0);
  try
    var ResultSize: ULONG;
    CheckStatus('BCryptExportKey',
      BCryptExportKey(FKeyHandle, 0, BCRYPT_ECCPRIVATE_BLOB, KeyBlob,
        SizeOf(KeyBlob), ResultSize, 0));

    if ResultSize <> SizeOf(KeyBlob) then
      raise EECDSAError.Create('BCryptExportKey result invalid (1)');
    if KeyBlob.Header.dwMagic <> BCRYPT_ECDSA_PRIVATE_P256_MAGIC then
      raise EECDSAError.Create('BCryptExportKey result invalid (2)');
    if KeyBlob.Header.cbKey <> 32 then
      raise EECDSAError.Create('BCryptExportKey result invalid (3)');

    APrivateKey.PublicKey.Public_X := KeyBlob.Public_X;
    APrivateKey.PublicKey.Public_Y := KeyBlob.Public_Y;
    APrivateKey.Private_d := KeyBlob.Private_d;
  finally
    { Security: don't leave copy of private key on the stack }
    FillChar(KeyBlob, SizeOf(KeyBlob), 0);
  end;
end;

procedure TECDSAKey.GenerateKeyPair;
begin
  DestroyKey;

  var LKeyHandle: BCRYPT_KEY_HANDLE;
  CheckStatus('BCryptGenerateKeyPair',
    BCryptGenerateKeyPair(FAlgorithmHandle, LKeyHandle, 256, 0));
  try
    CheckStatus('BCryptFinalizeKeyPair',
      BCryptFinalizeKeyPair(LKeyHandle, 0));
  except
    BCryptDestroyKey(LKeyHandle);
    raise;
  end;
  FKeyHandle := LKeyHandle;  { assign only on success }
end;

procedure TECDSAKey.ImportPrivateKey([ref] const APrivateKey: TECDSAPrivateKey);
begin
  DestroyKey;

  var KeyBlob: TBCryptPrivateKeyBlob;
  try
    KeyBlob.Header.dwMagic := BCRYPT_ECDSA_PRIVATE_P256_MAGIC;
    KeyBlob.Header.cbKey := 32;
    KeyBlob.Public_X := APrivateKey.PublicKey.Public_X;
    KeyBlob.Public_Y := APrivateKey.PublicKey.Public_Y;
    KeyBlob.Private_d := APrivateKey.Private_d;

    var LKeyHandle: BCRYPT_KEY_HANDLE;
    CheckStatus('BCryptImportKeyPair',
      BCryptImportKeyPair(FAlgorithmHandle, 0, BCRYPT_ECCPRIVATE_BLOB,
        LKeyHandle, KeyBlob, SizeOf(KeyBlob), 0));
    FKeyHandle := LKeyHandle;  { assign only on success }
  finally
    { Security: don't leave copy of private key on the stack }
    FillChar(KeyBlob, SizeOf(KeyBlob), 0);
  end;
end;

procedure TECDSAKey.ImportPublicKey([ref] const APublicKey: TECDSAPublicKey);
begin
  DestroyKey;

  var KeyBlob: TBCryptPublicKeyBlob;
  try
    KeyBlob.Header.dwMagic := BCRYPT_ECDSA_PUBLIC_P256_MAGIC;
    KeyBlob.Header.cbKey := 32;
    KeyBlob.Public_X := APublicKey.Public_X;
    KeyBlob.Public_Y := APublicKey.Public_Y;

    var LKeyHandle: BCRYPT_KEY_HANDLE;
    CheckStatus('BCryptImportKeyPair',
      BCryptImportKeyPair(FAlgorithmHandle, 0, BCRYPT_ECCPUBLIC_BLOB,
        LKeyHandle, KeyBlob, SizeOf(KeyBlob), 0));
    FKeyHandle := LKeyHandle;  { assign only on success }
  finally
    { There's no private key, but clear anyway for consistency }
    FillChar(KeyBlob, SizeOf(KeyBlob), 0);
  end;
end;

procedure TECDSAKey.KeyHandleRequired;
begin
  if FKeyHandle = 0 then
    raise EECDSAError.Create('No key has been assigned');
end;

procedure TECDSAKey.SignHash(const AHash: array of Byte;
  out ASignature: TECDSASignature);
begin
  KeyHandleRequired;

  { Initially clear ASignature just to make it easier to verify that
    BCryptSignHash overwrites the entire record }
  FillChar(ASignature, SizeOf(ASignature), 0);

  var ResultSize: ULONG;
  CheckStatus('BCryptSignHash',
    BCryptSignHash(FKeyHandle, nil, AHash[0], Length(AHash), ASignature,
      SizeOf(ASignature), ResultSize, 0));

  if ResultSize <> SizeOf(ASignature) then
    raise EECDSAError.Create('BCryptSignHash result size invalid');
end;

function TECDSAKey.VerifySignature(const AHash: array of Byte;
  const ASignature: TECDSASignature): Boolean;
begin
  KeyHandleRequired;

  const Status = BCryptVerifySignature(FKeyHandle, nil, AHash[0],
    Length(AHash), ASignature, SizeOf(ASignature), 0);
  if Status = STATUS_INVALID_SIGNATURE then
    Result := False
  else begin
    CheckStatus('BCryptVerifySignature', Status);
    Result := True;
  end;
end;

end.
