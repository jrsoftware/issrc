{ Compiletime Classes support }
unit uPSC_classes;

{$I PascalScript.inc}
interface
uses
  uPSCompiler, uPSUtils;

{
  Will register files from:
    Classes (exception TPersistent and TComponent)

  Register STD first

}

procedure SIRegister_Classes_TypesAndConsts(Cl: TPSPascalCompiler);

procedure SIRegisterTStrings(cl: TPSPascalCompiler; Streams: Boolean);
procedure SIRegisterTStringList(cl: TPSPascalCompiler);
procedure SIRegisterTSTREAM(Cl: TPSPascalCompiler);
procedure SIRegisterTHANDLESTREAM(Cl: TPSPascalCompiler);
procedure SIRegisterTFILESTREAM(Cl: TPSPascalCompiler);
procedure SIRegisterTSTRINGSTREAM(Cl: TPSPascalCompiler);

procedure SIRegister_Classes(Cl: TPSPascalCompiler; Streams: Boolean);

implementation

procedure SIRegisterTStrings(cl: TPSPascalCompiler; Streams: Boolean); // requires TPersistent
begin
  with Cl.AddClassN(cl.FindClass('TPersistent'), 'TStrings') do
  begin
    IsAbstract := True;
    RegisterMethod('constructor Create;');
    RegisterMethod('function Add(S: string): Integer;');
    RegisterMethod('procedure Append(S: string);');
    RegisterMethod('procedure AddStrings(Strings: TStrings);');
    RegisterMethod('procedure Clear;');
    RegisterMethod('procedure Delete(Index: Integer);');
    RegisterMethod('function IndexOf(const S: string): Integer; ');
    RegisterMethod('procedure Insert(Index: Integer; S: string); ');
    RegisterProperty('Capacity', 'Integer', iptRW);
    RegisterProperty('Delimiter', 'Char', iptRW);
    RegisterProperty('StrictDelimiter', 'Boolean', iptRW);
    RegisterProperty('DelimitedText', 'string', iptrw);
    RegisterProperty('NameValueSeparator', 'Char', iptRW);
    RegisterProperty('QuoteChar', 'Char', iptRW);
    RegisterProperty('Count', 'Integer', iptR);
    RegisterProperty('Text', 'string', iptrw);
    RegisterProperty('CommaText', 'string', iptrw);
    if Streams then
    begin
      RegisterMethod('procedure LoadFromFile(FileName: string); ');
      RegisterMethod('procedure SaveToFile(FileName: string); ');
    end;
    RegisterProperty('Strings', 'string Integer', iptRW);
    SetDefaultPropery('Strings');
    RegisterProperty('Objects', 'TObject Integer', iptRW);
  end;
end;

procedure SIRegisterTSTRINGLIST(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TStrings'), 'TStringList') do
  begin
    RegisterMethod('constructor Create;');
    RegisterMethod('function Find(S: string; var Index: Integer): Boolean');
    RegisterMethod('procedure Sort');
    RegisterProperty('CaseSensitive', 'Boolean', iptrw);
    RegisterProperty('Duplicates', 'TDuplicates', iptrw);
    RegisterProperty('Sorted', 'Boolean', iptrw);
    RegisterProperty('OnChange', 'TNotifyEvent', iptrw);
    RegisterProperty('OnChanging', 'TNotifyEvent', iptrw);
  end;
end;

procedure SIRegisterTSTREAM(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TObject'), 'TStream') do
  begin
    RegisterMethod('function Read(Buffer: AnyString; Count: LongInt): LongInt');
    RegisterMethod('function Write(Buffer: AnyString; Count: LongInt): LongInt');
    {$IFNDEF PS_NOINT64}
    RegisterMethod('function Seek(Offset: Int64; Origin: Word): Int64');
    {$ENDIF}
    RegisterMethod('procedure ReadBuffer(Buffer: AnyString; Count: LongInt)');
    RegisterMethod('procedure WriteBuffer(Buffer: AnyString; Count: LongInt)');
    {$IFNDEF PS_NOINT64}
    RegisterMethod('function CopyFrom(Source: TStream; Count: Int64; BufferSize: Integer): Int64');
    {$ENDIF}
    RegisterProperty('Position', 'LongInt', iptrw);
    RegisterProperty('Size', 'LongInt', iptrw);
  end;
end;

procedure SIRegisterTHANDLESTREAM(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TStream'), 'THandleStream') do
  begin
    RegisterMethod('constructor Create(AHandle: THandle)');
    RegisterProperty('Handle', 'THandle', iptr);
  end;
end;

procedure SIRegisterTFILESTREAM(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('THandleStream'), 'TFileStream') do
  begin
    RegisterMethod('constructor Create(FileName: string; Mode: Word)');
  end;
end;

procedure SIRegisterTSTRINGSTREAM(Cl: TPSPascalCompiler);
begin
  with Cl.AddClassN(cl.FindClass('TStream'), 'TStringStream') do
  begin
    RegisterMethod('constructor Create(AString: string)');
  end;
end;

procedure SIRegister_Classes_TypesAndConsts(Cl: TPSPascalCompiler);
begin
  cl.AddConstantN('soFromBeginning', 'LongInt').Value.ts32 := 0;
  cl.AddConstantN('soFromCurrent', 'LongInt').Value.ts32 := 1;
  cl.AddConstantN('soFromEnd', 'LongInt').Value.ts32 := 2;
  cl.AddConstantN('toEOF', 'Char').SetString(#0);
  cl.AddConstantN('toSymbol', 'Char').SetString(#1);
  cl.AddConstantN('toString', 'Char').SetString(#2);
  cl.AddConstantN('ToInteger', 'Char').SetString(#3);
  cl.AddConstantN('toFloat', 'Char').SetString(#4);
  cl.AddConstantN('fmCreate', 'LongInt').Value.ts32 := $FFFF;
  cl.AddConstantN('fmOpenRead', 'LongInt').Value.ts32 := 0;
  cl.AddConstantN('fmOpenWrite', 'LongInt').Value.ts32 := 1;
  cl.AddConstantN('fmOpenReadWrite', 'LongInt').Value.ts32 := 2;
  cl.AddConstantN('fmShareCompat', 'LongInt').Value.ts32 := 0;
  cl.AddConstantN('fmShareExclusive', 'LongInt').Value.ts32 := $10;
  cl.AddConstantN('fmShareDenyWrite', 'LongInt').Value.ts32 := $20;
  cl.AddConstantN('fmShareDenyRead', 'LongInt').Value.ts32 := $30;
  cl.AddConstantN('fmShareDenyNone', 'LongInt').Value.ts32 := $40;
  cl.AddConstantN('SecsPerDay', 'LongInt').Value.ts32 := 86400;
  cl.AddConstantN('MSecPerDay', 'LongInt').Value.ts32 := 86400000;
  cl.AddConstantN('DateDelta', 'LongInt').Value.ts32 := 693594;
  cl.AddTypeS('TAlignment', '(taLeftJustify, taRightJustify, taCenter)');
  cl.AddTypeS('THelpEvent', 'function (Command: Word; Data: NativeInt; var CallHelp: Boolean): Boolean');
  cl.AddTypeS('TGetStrProc', 'procedure(const S: string)');
  cl.AddTypeS('TDuplicates', '(dupIgnore, dupAccept, dupError)');
  cl.AddTypeS('TOperation', '(opInsert, opRemove)');
  if cl.FindType('THandle') = nil then
    cl.AddTypeS('THandle', 'NativeUInt');

  cl.AddTypeS('TNotifyEvent', 'procedure (Sender: TObject)');
end;

procedure SIRegister_Classes(Cl: TPSPascalCompiler; Streams: Boolean);
begin
  SIRegister_Classes_TypesAndConsts(Cl);
  if Streams then
    SIRegisterTSTREAM(Cl);
  SIRegisterTStrings(cl, Streams);
  SIRegisterTStringList(cl);
  if Streams then
  begin
    SIRegisterTHANDLESTREAM(Cl);
    SIRegisterTFILESTREAM(Cl);
    SIRegisterTSTRINGSTREAM(Cl);
  end;
end;

end.
