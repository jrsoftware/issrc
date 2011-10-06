unit ArcFour;

{
  Inno Setup
  Copyright (C) 1997-2004 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Interface to ISCrypt.dll (ARCFOUR encryption/decryption)

  $jrsoftware: issrc/Projects/ArcFour.pas,v 1.2 2004/04/26 19:11:23 jr Exp $
}

interface

uses
  Windows;

type
  TArcFourContext = record
    state: array[0..255] of Byte;
    x, y: Byte;
  end;

function ArcFourInitFunctions(Module: HMODULE): Boolean;
procedure ArcFourInit(var Context: TArcFourContext; const Key;
  KeyLength: Cardinal);
procedure ArcFourCrypt(var Context: TArcFourContext; const InBuffer;
  var OutBuffer; Length: Cardinal);
procedure ArcFourDiscard(var Context: TArcFourContext; Bytes: Cardinal);

implementation

var
  _ISCryptGetVersion: function: Integer; stdcall;
  _ArcFourInit: procedure(var context: TArcFourContext; const key;
    key_length: Cardinal); stdcall;
  _ArcFourCrypt: procedure(var context: TArcFourContext; const in_buffer;
    var out_buffer; length: Cardinal); stdcall;

function ArcFourInitFunctions(Module: HMODULE): Boolean;
begin
  _ISCryptGetVersion := GetProcAddress(Module, 'ISCryptGetVersion');
  _ArcFourInit := GetProcAddress(Module, 'ArcFourInit');
  _ArcFourCrypt := GetProcAddress(Module, 'ArcFourCrypt');
  if Assigned(_ISCryptGetVersion) and Assigned(_ArcFourInit) and
     Assigned(_ArcFourCrypt) then begin
    { Verify that the DLL's version is what we expect }
    Result := (_ISCryptGetVersion = 1);
  end
  else begin
    Result := False;
    _ISCryptGetVersion := nil;
    _ArcFourInit := nil;
    _ArcFourCrypt := nil;
  end
end;

procedure ArcFourInit(var Context: TArcFourContext; const Key;
  KeyLength: Cardinal);
begin
  _ArcFourInit(Context, Key, KeyLength);
end;

procedure ArcFourCrypt(var Context: TArcFourContext; const InBuffer;
  var OutBuffer; Length: Cardinal);
begin
  _ArcFourCrypt(Context, InBuffer, OutBuffer, Length);
end;

procedure ArcFourDiscard(var Context: TArcFourContext; Bytes: Cardinal);
begin
  _ArcFourCrypt(Context, Pointer(nil)^, Pointer(nil)^, Bytes);
end;

end.
