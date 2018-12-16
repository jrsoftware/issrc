unit ASMInline;

interface

{ASM Inliner
 Nicholas Sherlock

 This is incomplete, I've only implemented enough to support InnoCallback.

 Instructions are stored in a TMemoryStream internally

 Instructions usually accept some combination of Registers, Immediates and
 Memory References. Memory References can either be of the simple form [EAX]
 (Where [EAX] is really a Delphi set), or the user can build the address with
 the TASMInline.addr() function. It'd be nice to have a function which builds
 the address from a string, too, allowing the more intuitive '[EAX+EBX*4+1000]'
 style.

 The generation of instruction-relative addresses generates Relocations, using
 SaveToMemory() automatically rebases using the relocations to make these correct
 in the final block of memory.

  !!!! Not all special cases have been implemented in WriteRegRef().
}

uses Sysutils, windows, classes, contnrs;

type
  TModMode = (mmNaked, mmDeref, mmDisp8, mmDisp32);

  TRegister32 = (EAX, EBX, ECX, EDX, ESP, EBP, ESI, EDI);
  TRegister32Set = set of TRegister32;
  TRegister16 = (AX, BX, CX, DX, SP, BP, SI, DI);
  TRegister16Set = set of TRegister16;
  TRegister8 = (AH, AL, BH, BL, CH, CL, DH, DL);

  TCLRegister = CL..CL;

  TMemSize = (ms8, ms16, ms32, ms64);

  TMemoryAddress = record
    size: TMemSize;
    usebase: boolean;
    base, index: TRegister32;
    offset: integer;
    scale: byte;
  end;

  EOperandSizeMismatch = class(exception)
  public
    constructor create;
  end;

  TRelocType = (rt32Bit);

  TReloc = class
  public
    position: longword;
    relocType: TRelocType;
  end;

  TLabelRef = class
  public
    labelname: string;
    position: longword;
    delta: integer;
    labelType: TRelocType;
  end;

  TLabel = class
  public
    name: string;
    position: longword;
  end;

  TLabelList = class
  private
    flabels: TStringList;
  public
    constructor create;
    destructor Destroy; override;
    function GetLabel(const name: string): TLabel;
    function AddLabel(const name: string; position: longword): Boolean;
    procedure Clear;
  end;

  TASMInline = class
  private
    fbuffer: TMemoryStream;
    frelocs, flabelrefs: TObjectList;
    flabels: TLabelList;
    fbase: longword;
    procedure ResolveLabels;

    procedure AddRelocation(position: longword; relocType: TRelocType);
    procedure AddLabelRef(position: longword; delta: integer; relocType: TRelocType; const labelname: string);

    function GetReloc(index: integer): TReloc;
    function RelocCount: integer;
    property Relocs[index: integer]: TReloc read GetReloc;

    procedure WriteByte(b: byte);
    procedure WriteWord(w: word);
    procedure WriteInteger(i: integer);
    procedure WriteLongWord(l: longword);
    procedure WriteOpSizeOverride;
    procedure WriteRegRef(reg: byte; base: TRegister32; deref: boolean; index: TRegister32; Offset: integer; Scale: byte; usebase: boolean); overload;
    procedure WriteRegRef(mem: TMemoryAddress; reg: TRegister8); overload;
    procedure WriteRegRef(mem: TMemoryAddress; reg: TRegister16); overload;
    procedure WriteRegRef(mem: TMemoryAddress; reg: TRegister32); overload;
    procedure WriteRegRef(mem: TMemoryAddress; opcodeext: byte); overload;
    procedure WriteRegRef(reg1: TRegister8; opcodeext: byte); overload;
    procedure WriteRegRef(reg1: TRegister16; opcodeext: byte); overload;
    procedure WriteRegRef(reg1: TRegister32; opcodeext: byte); overload;
    procedure WriteRegRef(reg1: TRegister8; reg2: TRegister8); overload;
    procedure WriteRegRef(reg1: TRegister16; reg2: TRegister16); overload;
    procedure WriteRegRef(reg1: TRegister32; reg2: TRegister32); overload;
    procedure WriteRegRef(reg: TRegister32; base: TRegister32; deref: boolean; index: TRegister32 = EAX; Offset: integer = 0; Scale: byte = 0; usebase: boolean = true); overload;
  public
    function Size: integer;

    procedure Clear;

    procedure Execute;

    procedure Relocate(base: pointer);

    function SaveAsMemory: pointer;
    procedure SaveToMemory(target: pointer);

    constructor create;
    destructor Destroy; override;

    function Addr(base: TRegister32; index: TRegister32; scale: Byte = 1; offset: integer = 0; size: TMemSize = ms32): TMemoryAddress; overload;
    function Addr(base: TRegister32; size: TMemSize = ms32): TMemoryAddress; overload;
    function Addr(base: TRegister32; offset: Integer; size: TMemSize = ms32): TMemoryAddress; overload;
    function Addr(offset: integer; index: TRegister32; scale: Byte = 1; size: TMemSize = ms32): TMemoryAddress; overload;
    function Addr(offset: Integer; size: TMemSize = ms32): TMemoryAddress; overload;

    //RET
    procedure Ret; overload;
    //RET imm8
    procedure Ret(w: Word); overload;

    //PUSH imm
    procedure Push(lw: longword); overload;
    //PUSH reg
    procedure Push(reg: TRegister16); overload;
    procedure Push(reg: TRegister32); overload;
    //PUSH [reg]
    procedure Push(mem: TRegister32Set); overload;
    //PUSH mem
    procedure Push(mem: TMemoryAddress); overload;

    //POP reg
    procedure Pop(reg: TRegister32);

    procedure doLabel(const name: string);

    //DB imm8
    procedure db(b: byte);
    //DW imm16
    procedure dw(w: word);
    //DD imm32
    procedure dd(dw: longword);

    //CALL [reg]
    procedure Call(reg: TRegister32); overload;
    //CALL rel32
    procedure Call(target: pointer); overload;

    //JUMP rel32
    procedure Jmp(target: pointer); overload;
    //JUMP label
    procedure Jmp(const labelname: string); overload;

    //MOV reg, imm
    procedure Mov(reg: TRegister8; b: byte); overload;
    procedure Mov(reg: TRegister16; b: word); overload;
    procedure Mov(reg: TRegister32; b: longword); overload;
    //MOV reg, reg
    procedure Mov(reg1: TRegister8; reg2: TRegister8); overload;
    procedure Mov(reg1: TRegister16; reg2: TRegister16); overload;
    procedure Mov(reg1: TRegister32; reg2: TRegister32); overload;
    //MOV reg, [reg] and MOV [reg], reg
    procedure Mov(reg1: TRegister32; reg2: TRegister32Set); overload;
    procedure Mov(reg1: TRegister32Set; reg2: TRegister32); overload;
    //MOV [reg], imm
    procedure Mov(reg1: TRegister32Set; i: longword); overload;
    //MOV mem, imm
    procedure Mov(mem: TMemoryAddress; i: longword); overload;
    //MOV reg, mem and MOV mem, reg
    procedure Mov(mem: TMemoryAddress; reg: TRegister32); overload;
    procedure Mov(mem: TMemoryAddress; reg: TRegister16); overload;
    procedure Mov(mem: TMemoryAddress; reg: TRegister8); overload;
    procedure Mov(reg: TRegister32; mem: TMemoryAddress); overload;
    procedure Mov(reg: TRegister16; mem: TMemoryAddress); overload;
    procedure Mov(reg: TRegister8; mem: TMemoryAddress); overload;

    procedure Nop;

    //SHL reg, imm
    procedure doSHL(reg: TRegister8; amount: byte); overload;
    procedure doSHL(reg: TRegister16; amount: byte); overload;
    procedure doSHL(reg: TRegister32; amount: byte); overload;
    //SHL reg, CL
    procedure doSHL(reg: TRegister8; amount: TCLRegister); overload;
    procedure doSHL(reg: TRegister16; amount: TCLRegister); overload;
    procedure doSHL(reg: TRegister32; amount: TCLRegister); overload;
    //SHL mem, imm and SHL mem, CL
    procedure doSHL(mem: TMemoryAddress; amount: byte); overload;
    procedure doSHL(mem: TMemoryAddress; amount: TCLRegister); overload;

    //SAR reg, CL
    procedure SAR(reg: TRegister8; amount: byte); overload;
    procedure SAR(reg: TRegister16; amount: byte); overload;
    procedure SAR(reg: TRegister32; amount: byte); overload;
    //SAR reg, imm
    procedure SAR(reg: TRegister8; amount: TCLRegister); overload;
    procedure SAR(reg: TRegister16; amount: TCLRegister); overload;
    procedure SAR(reg: TRegister32; amount: TCLRegister); overload;
    //SAR mem, imm and SHR mem, CL
    procedure SAR(mem: TMemoryAddress; amount: byte); overload;
    procedure SAR(mem: TMemoryAddress; amount: TCLRegister); overload;

    //SHR reg, imm
    procedure doSHR(reg: TRegister8; amount: byte); overload;
    procedure doSHR(reg: TRegister16; amount: byte); overload;
    procedure doSHR(reg: TRegister32; amount: byte); overload;
    //SHR reg, CL
    procedure doSHR(reg: TRegister8; amount: TCLRegister); overload;
    procedure doSHR(reg: TRegister16; amount: TCLRegister); overload;
    procedure doSHR(reg: TRegister32; amount: TCLRegister); overload;
    //SHR mem, imm and SHR mem, CL
    procedure doSHR(mem: TMemoryAddress; amount: byte); overload;
    procedure doSHR(mem: TMemoryAddress; amount: TCLRegister); overload;

    //NOT reg
    procedure doNot(reg: TRegister8); overload;
    procedure doNot(reg: TRegister32); overload;
    procedure doNot(reg: TRegister16); overload;
    //NOT mem
    procedure doNot(mem: TMemoryAddress); overload;
  end;

implementation

constructor TLabelList.create;
begin
  flabels := TStringList.create;
  flabels.Sorted := true;
end;

destructor TLabelList.destroy;
begin
  clear;
  flabels.free;
end;

function TLabelList.GetLabel(const name: string): TLabel;
var i: integer;
begin
  i := flabels.IndexOf(name);
  if i = -1 then
    result := nil else
    result := TLabel(flabels.objects[i]);
end;

function TLabelList.AddLabel(const name: string; position: longword): Boolean;
var alabel: TLabel;
begin
  result := flabels.indexof(name) = -1;
  if result then begin //success
    alabel := TLabel.create;
    alabel.name := name;
    alabel.position := position;
    flabels.AddObject(name, alabel);
  end;
end;

procedure TLabelList.Clear;
var t1: integer;
begin
  for t1 := 0 to flabels.count - 1 do
    flabels.Objects[t1].free;
  flabels.Clear;
end;

constructor EOperandSizeMismatch.create;
begin
  inherited create('Operand size mismatch');
end;

{Throw an exception if test<>match. Poor man's assert().
 Could overload to add other sorts of tests}

procedure require(test: TMemSize; match: TMemSize);
begin
  if test <> match then
    raise EOperandSizeMismatch.create;
end;

{Check that the set has exactly one member. If it has one member, return that
 member, otherwise throw an exception}

function SingleMember(regset: TRegister32Set): TRegister32;
var r: TRegister32;
  found: boolean;
begin
  found := false;
  result:=EAX;
  for r := low(r) to high(r) do
    if r in regset then
      if found then //there is more than one member in this set
        raise exception.create('Invalid register operand') else
      begin
        found := true;
        result := r;
      end;
  if not found then begin
    raise exception.create('Invalid register operand');
    end;
end;

function regnum(reg: TRegister16): byte; overload;
begin
  case reg of
    AX: result := 0;
    BX: result := 3;
    CX: result := 1;
    DX: result := 2;
  else raise exception.create('Unknown register...');
  end;
end;

function regnum(reg: TRegister32): byte; overload;
begin
  case reg of
    EAX: result := 0;
    EBX: result := 3;
    ECX: result := 1;
    EDX: result := 2;
    ESP: result := 4;
    EBP: result := 5;
    ESI: result := 6;
    EDI: result := 7;
  else raise exception.create('Unknown register...');
  end;
end;

function regnum(reg: TRegister8): byte; overload;
begin
  case reg of
    AL: result := 0;
    BL: result := 3;
    CL: result := 1;
    DL: result := 2;
    AH: result := 4;
    BH: result := 7;
    CH: result := 5;
    DH: result := 6;
  else raise exception.create('Unknown register...');
  end;
end;

function ModModeNum(m: TModMode): byte;
begin
  case m of
    mmDeref: result := 0;
    mmDisp8: result := 1;
    mmDisp32: result := 2;
    mmNaked: result := 3;
  else raise exception.create('Invalid mod mode: ' + inttostr(ord(m)));
  end;
end;

function EncodeSIB(scale, index, base: byte): byte;
begin
  result := base or (index shl 3) or (scale shl 6);
end;

function EncodeModRM(aMod, aReg, aRM: byte): byte; overload;
begin
  result := (aMod shl 6) or (areg shl 3) or aRM;
end;

function EncodeModRM(aregister: TRegister32; reg: Byte): byte; overload;
begin
  result := EncodeModRM(3, reg, regnum(aregister));
end;

function EncodeModRM(aregister: TRegister16; reg: Byte): byte; overload;
begin
  result := EncodeModRM(3, reg, regnum(aregister));
end;

function EncodeModRM(aregister: TRegister8; reg: Byte): byte; overload;
begin
  result := EncodeModRM(3, reg, regnum(aregister));
end;

procedure TASMInline.Execute;
var codeBuf: pointer;
begin
  codeBuf := SaveAsMemory;
  try
    tprocedure(codeBuf);
  finally
    FreeMem(codeBuf);
  end;
end;

{$IFOPT R+}
{$DEFINE RESTORER}
{$R-}
{$ENDIF}
{$IFOPT Q+}
{$DEFINE RESTOREQ}
{$Q-}
{$ENDIF}

{Resolve any unresolved references to label names into actual relative or
 absolute addresses}

procedure TASMInline.ResolveLabels;
var t1: integer;
  labelref: TLabelRef;
  alabel: TLabel;
  lw: Longword;
begin
  for t1 := 0 to flabelrefs.count - 1 do begin
    labelref := TLabelRef(flabelrefs[t1]);
    alabel := flabels.GetLabel(labelref.labelname);
    if alabel = nil then
      raise Exception.create('Unknown label ''' + labelref.labelname + '''');

    fbuffer.seek(labelref.position, soFromBeginning);
    lw := alabel.position + labelref.delta;
    writelongword(lw);

//    AddRelocation(labelref.position,labelref.labelType);
  end;

  flabelrefs.Clear; //we have resolved all these now
end;

procedure TASMInline.Relocate(base: pointer);
var oldpos, diff, orig: integer;
  i: integer;
  reloc: TReloc;
begin
  oldpos := fbuffer.Position;
  try

    diff := -(longword(base) - fbase);

    for i := 0 to RelocCount - 1 do begin
      reloc := Relocs[i];
      case reloc.relocType of
        rt32Bit: begin
            fbuffer.Seek(reloc.position, soFromBeginning);
            fbuffer.Read(orig, sizeof(orig));
            fbuffer.seek(-sizeof(orig), soFromCurrent);
            orig := LongWord(orig + diff);
            fbuffer.write(orig, sizeof(orig));
          end;
      end;
    end;
    fbase := longword(base);
  finally
    fbuffer.position := oldpos;
  end;
end;
{$IFDEF RESTORER}
{$R+}
{$ENDIF}
{$IFDEF RESTOREQ}
 {Q+}
{$ENDIF}

function TASMInline.GetReloc(index: integer): TReloc;
begin
  result := TReloc(frelocs[index]);
end;

function TASMInline.RelocCount: integer;
begin
  result := frelocs.Count;
end;

procedure TASMInline.AddLabelRef(position: longword; delta: integer; relocType: TRelocType; const labelname: string);
var labelref: TLabelRef;
begin
  labelref := TLabelRef.create;

  labelref.labelname := labelname;
  labelref.position := position;
  labelref.delta := delta;
  labelref.labelType := relocType;

  fLabelRefs.add(labelref);
end;

procedure TASMInline.AddRelocation(position: longword; relocType: TRelocType);
var reloc: TReloc;
begin
  reloc := TReloc.Create;
  reloc.position := position;
  reloc.relocType := relocType;
  frelocs.add(reloc);
end;

procedure TASMInline.Clear;
begin
  fbuffer.Clear;
  frelocs.Clear;
end;

function TASMInline.SaveAsMemory: pointer;
var buf: pointer;
  oldprotect: Cardinal;
begin
  GetMem(buf, size);
  VirtualProtect(buf, Size, PAGE_EXECUTE_READWRITE, oldprotect);
  SaveToMemory(buf);
  result := buf;
end;

procedure TASMInline.SaveToMemory(target: pointer);
begin
  ResolveLabels;
  Relocate(target);
  Move(fbuffer.memory^, target^, size);
end;

function TASMInline.Addr(base: TRegister32; size: TMemSize = ms32): TMemoryAddress;
begin
  result.base := base;
  result.usebase := true;
  result.size := size;
  result.offset := 0;
  result.scale := 0; //don't use index
end;

function TASMInline.Addr(offset: integer; index: TRegister32; scale: Byte = 1; size: TMemSize = ms32): TMemoryAddress;
begin
  result.offset := offset;
  result.index := index;
  result.scale := scale;
  result.size := size;
  result.usebase := false;
end;

function TASMInline.Addr(base: TRegister32; offset: Integer; size: TMemSize = ms32): TMemoryAddress;
begin
  result.base := base;
  result.scale := 0; //don't use Index
  result.offset := offset;
  result.size := size;
  result.usebase := true;
end;

function TASMInline.Addr(offset: Integer; size: TMemSize = ms32): TMemoryAddress;
begin
  result.offset := offset;
  result.size := size;
  result.scale := 0; //dont use Index
  result.usebase := true;
end;

function TASMInline.Addr(base: TRegister32; index: TRegister32; scale: Byte = 1; offset: integer = 0; size: TMemSize = ms32): TMemoryAddress;
begin
  result.base := base;
  result.index := index;
  result.offset := offset;
  result.scale := scale;
  result.size := size;
  result.usebase := true;
end;

function TASMInline.Size: integer;
begin
  result := fbuffer.size;
end;

procedure TASMInline.WriteInteger(i: integer);
begin
  fbuffer.write(i, 4);
end;

procedure TASMInline.writelongword(l: longword);
begin
  fbuffer.write(l, 4);
end;

procedure TASMInline.writeword(w: word);
begin
  fbuffer.write(w, 2);
end;

procedure TASMInline.writebyte(b: byte);
begin
  fbuffer.write(b, 1);
end;

procedure TASMInline.WriteOpSizeOverride;
begin
  writebyte($66);
end;

procedure TASMInline.Ret;
begin
  writebyte($C3);
end;

procedure TASMInline.Ret(w: Word);
begin
  if w = 0 then
    ret() else begin
    writebyte($C2);
    writeword(w);
  end;
end;

procedure TASMInline.doSHL(mem: TMemoryAddress; amount: byte);
begin
  case mem.size of
    ms16, ms32: begin
        if mem.size = ms16 then WriteOpSizeOverride();

        if amount = 1 then begin
          writebyte($D1);
          WriteRegRef(mem, 4);
        end else begin
          writebyte($C1);
          WriteRegRef(mem, 4);
          writebyte(amount);
        end;
      end;
    ms8: begin
        if amount = 1 then begin
          writebyte($D0);
          WriteRegRef(mem, 4);
        end else begin
          writebyte($C0);
          WriteRegRef(mem, 4);
          writebyte(amount);
        end;
      end;
  else raise EOperandSizeMismatch.create();
  end;
end;

procedure TASMInline.doSHL(mem: TMemoryAddress; amount: TCLRegister);
begin
  case mem.size of
    ms16, ms32: begin
        if mem.size = ms16 then
          WriteOpSizeOverride;
        writebyte($D3);
        writeregref(mem, 4);
      end;
    ms8: begin
        writebyte($D2);
        writeregref(mem, 4);
      end;
  else raise EOperandSizeMismatch.create();
  end;
end;

procedure TASMInline.doSHL(reg: TRegister8; amount: byte);
begin
  if amount = 1 then begin
    writebyte($D0);
    WriteRegRef(reg, 4);
  end else begin
    writebyte($C0);
    WriteRegRef(reg, 4);
    writebyte(amount);
  end;
end;

procedure TASMInline.doSHL(reg: TRegister8; amount: TCLRegister);
begin
  writebyte($D2);
  writeregref(reg, 4);
end;

procedure TASMInline.doSHL(reg: TRegister32; amount: byte);
begin
  if amount = 1 then begin
    writebyte($D1);
    WriteRegRef(reg, 4);
  end else begin
    writebyte($C1);
    WriteRegRef(reg, 4);
    writebyte(amount);
  end;
end;

procedure TASMInline.doSHL(reg: TRegister32; amount: TCLRegister);
begin
  writebyte($D3);
  writeregref(reg, 4);
end;

procedure TASMInline.doSHL(reg: TRegister16; amount: byte);
begin
  WriteOpSizeOverride;
  if amount = 1 then begin
    writebyte($D1);
    WriteRegRef(reg, 4);
  end else begin
    writebyte($C1);
    WriteRegRef(reg, 4);
    writebyte(amount);
  end;
end;

procedure TASMInline.doSHL(reg: TRegister16; amount: TCLRegister);
begin
  WriteOpSizeOverride;
  writebyte($D3);
  writeregref(reg, 4);
end;

procedure TASMInline.doSHR(mem: TMemoryAddress; amount: byte);
begin
  case mem.size of
    ms16, ms32: begin
        if mem.size = ms16 then
          WriteOpSizeOverride;

        if amount = 1 then begin
          writebyte($D1);
          WriteRegRef(mem, 5);
        end else begin
          writebyte($C1);
          WriteRegRef(mem, 5);
          writebyte(amount);
        end;
      end;
    ms8: begin
        if amount = 1 then begin
          writebyte($D0);
          WriteRegRef(mem, 5);
        end else begin
          writebyte($C0);
          WriteRegRef(mem, 5);
          writebyte(amount);
        end;
      end;
  else raise EOperandSizeMismatch.create();
  end;
end;

procedure TASMInline.doSHR(mem: TMemoryAddress; amount: TCLRegister);
begin
  case mem.size of
    ms32: begin
        writebyte($D3);
        WriteRegRef(mem, 5);
      end;
    ms16: begin
        WriteOpSizeOverride;
        writebyte($D3);
        WriteRegRef(mem, 5);
      end;
    ms8: begin
        writebyte($D2);
        writeregref(mem, 5);
      end;
  end;
end;

procedure TASMInline.doSHR(reg: TRegister8; amount: byte);
begin
  if amount = 1 then begin
    writebyte($D0);
    WriteRegRef(reg, 5);
  end else begin
    writebyte($C0);
    WriteRegRef(reg, 5);
    writebyte(amount);
  end;
end;

procedure TASMInline.doSHR(reg: TRegister8; amount: TCLRegister);
begin
  writebyte($D2);
  writeregref(reg, 5);
end;

procedure TASMInline.doSHR(reg: TRegister32; amount: byte);
begin
  if amount = 1 then begin
    writebyte($D1);
    WriteRegRef(reg, 5);
  end else begin
    writebyte($C1);
    WriteRegRef(reg, 5);
    writebyte(amount);
  end;
end;

procedure TASMInline.doSHR(reg: TRegister32; amount: TCLRegister);
begin
  writebyte($D3);
  writeregref(reg, 5);
end;

procedure TASMInline.doSHR(reg: TRegister16; amount: byte);
begin
  WriteOpSizeOverride;
  if amount = 1 then begin
    writebyte($D1);
    WriteRegRef(reg, 5);
  end else begin
    writebyte($C1);
    WriteRegRef(reg, 5);
    writebyte(amount);
  end;
end;

procedure TASMInline.doSHR(reg: TRegister16; amount: TCLRegister);
begin
  WriteOpSizeOverride;
  writebyte($D3);
  writeregref(reg, 5);
end;

procedure TASMInline.SAR(reg: TRegister8; amount: byte);
begin
  if amount = 1 then begin
    writebyte($D0);
    WriteRegRef(reg, 7);
  end else begin
    writebyte($C0);
    WriteRegRef(reg, 7);
    writebyte(amount);
  end;
end;

procedure TASMInline.SAR(reg: TRegister8; amount: TCLRegister);
begin
  writebyte($D2);
  writeregref(reg, 7);
end;

procedure TASMInline.SAR(mem: TMemoryAddress; amount: byte);
begin
  case mem.size of
    ms32, ms16: begin
        if mem.size = ms16 then WriteOpSizeOverride;

        if amount = 1 then begin
          writebyte($D1);
          WriteRegRef(mem, 7);
        end else begin
          writebyte($C1);
          WriteRegRef(mem, 7);
          writebyte(amount);
        end;
      end;
    ms8: begin
        if amount = 1 then begin
          writebyte($D0);
          WriteRegRef(mem, 7);
        end else begin
          writebyte($C0);
          WriteRegRef(mem, 7);
          writebyte(amount);
        end;
      end;
  else raise EOperandSizeMismatch.create();
  end;
end;

procedure TASMInline.SAR(mem: TMemoryAddress; amount: TCLRegister);
begin
  case mem.size of
    ms16, ms32: begin
        if mem.size = ms16 then
          WriteOpSizeOverride;
        writebyte($D3);
        writeregref(mem, 7);
      end;
    ms8: begin
        writebyte($D2);
        writeregref(mem, 7);
      end;
  else raise EOperandSizeMismatch.create();
  end;
end;

procedure TASMInline.SAR(reg: TRegister32; amount: byte);
begin
  if amount = 1 then begin
    writebyte($D1);
    WriteRegRef(reg, 7);
  end else begin
    writebyte($C1);
    WriteRegRef(reg, 7);
    writebyte(amount);
  end;
end;

procedure TASMInline.SAR(reg: TRegister32; amount: TCLRegister);
begin
  writebyte($D3);
  writeregref(reg, 7);
end;

procedure TASMInline.SAR(reg: TRegister16; amount: byte);
begin
  WriteOpSizeOverride;
  if amount = 1 then begin
    writebyte($D1);
    WriteRegRef(reg, 7);
  end else begin
    writebyte($C1);
    WriteRegRef(reg, 7);
    writebyte(amount);
  end;
end;

procedure TASMInline.SAR(reg: TRegister16; amount: TCLRegister);
begin
  WriteOpSizeOverride;
  writebyte($D3);
  writeregref(reg, 7);
end;

procedure TASMInline.doNot(mem: TMemoryAddress);
begin
  case mem.size of
    ms32, ms16: begin
        if mem.size = ms16 then
          WriteOpSizeOverride;
        Writebyte($F7);
        WriteRegRef(mem, 2);
      end;
    ms8: begin
        writebyte($F6);
        WriteRegRef(mem, 2);
      end;
  else raise EOperandSizeMismatch.create;
  end;
end;

procedure TASMInline.doNot(reg: TRegister32);
begin
  Writebyte($F7);
  WriteRegRef(reg, 2);
end;

procedure TASMInline.doNot(reg: TRegister8);
begin
  writebyte($F6);
  WriteRegRef(reg, 2);
end;

procedure TASMInline.doNot(reg: TRegister16);
begin
  WriteOpSizeOverride;
  writebyte($F7);
  WriteRegRef(reg, 2);
end;

procedure TASMInline.Pop(reg: TRegister32);
begin
  writebyte($58 + regnum(reg));
end;

procedure TASMInline.Jmp(const labelname: string);
begin
  WriteByte($E9);
  AddLabelRef(fbuffer.position, -(fbuffer.position + 4), rt32bit, labelname);
  WriteLongword(0); //dummy space for the label target
end;

procedure TASMInline.Jmp(target: pointer);
begin
  writebyte($E9);
  AddRelocation(fbuffer.position, rt32bit);
  WriteInteger(integer(target) - (integer(fBase) + fbuffer.Position + 4));
end;

procedure TASMInline.doLabel(const name: string);
begin
  if not flabels.AddLabel(name, fbuffer.Position) then
    raise exception.create('Duplicate label identifier ''' + name + '''');
end;

procedure TASMInline.db(b: byte);
begin
  WriteByte(b);
end;

procedure TASMInline.dw(w: word);
begin
  WriteWord(w);
end;

procedure TASMInline.dd(dw: longword);
begin
  WriteLongWord(dw);
end;

procedure TASMInline.Call(target: pointer);
begin
  writebyte($E8);
  AddRelocation(fbuffer.position, rt32Bit);
  WriteInteger(integer(target) - (integer(fBase) + fbuffer.Position + 4));
end;

procedure TASMInline.Call(reg: TRegister32);
begin
  writebyte($FF);
  WriteRegRef(reg, 2);
end;

procedure TASMInline.Push(mem: TRegister32Set);
begin
  push(addr(SingleMember(mem)));
end;

procedure TASMInline.Push(mem: TMemoryAddress);
begin
  writebyte($FF);
  WriteRegRef(mem, 6);
end;

procedure TASMInline.Push(lw: longword);
begin
  {bytes get sign extended. Only push as byte if it won't end up being
  interpreted as negative..}
  if lw < 128 then begin
    writebyte($6A);
    writebyte(lw and $FF);
  end else begin //write a longword
    writebyte($68);
    writelongword(lw);
  end;
end;

procedure TASMInline.Push(reg: TRegister16);
begin
  WriteOpSizeOverride;
  writebyte($50 + regnum(reg));
end;

procedure TASMInline.Push(reg: TRegister32);
begin
  writebyte($50 + regnum(reg));
end;

procedure TASMInline.WriteRegRef(reg1: TRegister8; opcodeext: byte);
begin
  writebyte(EncodeModRM(3, opcodeext, regnum(reg1)));
end;

procedure TASMInline.WriteRegRef(reg1: TRegister16; opcodeext: byte);
begin
  writebyte(EncodeModRM(3, opcodeext, regnum(reg1)));
end;

procedure TASMInline.WriteRegRef(reg1: TRegister32; opcodeext: byte);
begin
  writebyte(EncodeModRM(3, opcodeext, regnum(reg1)));
end;

procedure TASMInline.WriteRegRef(reg1: TRegister32; reg2: TRegister32);
begin
  writebyte(EncodeModRM(ModModeNum(mmNaked), regnum(reg2), regnum(reg1)));
end;

procedure TASMInline.WriteRegRef(reg1: TRegister16; reg2: TRegister16);
begin
  WriteByte(EncodeModRM(ModModeNum(mmNaked), regnum(reg2), regnum(reg1)));
end;

procedure TASMInline.WriteRegRef(reg1: TRegister8; reg2: TRegister8);
begin
  WriteByte(EncodeModRM(ModModeNum(mmNaked), regnum(reg2), regnum(reg1)));
end;

procedure TASMInline.WriteRegRef(mem: TMemoryAddress; opcodeext: byte);
begin
  writeregref(opcodeext, mem.base, true, mem.index, mem.offset, mem.scale, mem.usebase);
end;

procedure TASMInline.WriteRegRef(mem: TMemoryAddress; reg: TRegister8);
begin
  writeregref(regnum(reg), mem.base, true, mem.index, mem.offset, mem.scale, mem.usebase);
end;

procedure TASMInline.WriteRegRef(mem: TMemoryAddress; reg: TRegister16);
begin
  writeregref(regnum(reg), mem.base, true, mem.index, mem.offset, mem.scale, mem.usebase);
end;

procedure TASMInline.WriteRegRef(mem: TMemoryAddress; reg: TRegister32);
begin
  writeregref(reg, mem.base, true, mem.index, mem.offset, mem.scale, mem.usebase);
end;

//Write the MODR/M and SIB byte for the given register or memory reference

procedure TASMInline.WriteRegRef(reg: TRegister32; base: TRegister32; deref: boolean; index: TRegister32 = EAX; Offset: integer = 0; Scale: byte = 0; usebase: boolean = true);
begin
  WriteRegRef(regnum(reg), base, deref, index, Offset, scale, usebase);
end;

procedure TASMInline.WriteRegRef(reg: byte; base: TRegister32; deref: boolean; index: TRegister32; Offset: integer; Scale: byte; usebase: boolean);
type TOffSize = (osNone, os8, os32);
var mode: TModMode;
  offsize: TOffSize;
  useSIB: boolean;
  areg, arm: Byte;
begin
  if not deref then begin
    mode := mmNaked;
    offsize := osNone;
  end else
    if usebase = false then begin
      offsize := os32;
      mode := mmDeref;
      base := EBP; //the "no base" value
    end else
      if Offset = 0 then begin
        mode := mmDeref;
        offsize := osNone;
      end else
        if (offset >= -128) and (offset < 128) then begin //signed byte
          mode := mmDisp8;
          offsize := os8;
        end else begin
          mode := mmDisp32;
          offsize := os32;
        end;

  if (mode <> mmnaked) then begin
    usesib := (Scale > 0) or (base = ESP);
  end else usesib := false;

  if useSIB then begin //calculate scale, easiest just to use a case statement..
    case scale of
      0: begin //dont want an index value
          index := ESP; //"none" value
        end;
      1: scale := 0;
      2: scale := 1;
      4: scale := 2;
      8: scale := 3;
    else raise exception.create('Invalid scale, valid values are 1,2,4,8.');
    end;
  end;

  if (not useSIB) and (mode = mmDeref) and (base = EBP) then begin
  //not available, use [EBP+0] instead
    mode := mmDisp8;
    offsize := os8;
    Offset := 0;
  end;

  arm := regnum(base);
  areg := reg;

  if usesib then
    WriteByte(EncodeModRM(ModModeNum(mode), areg, 4)) else
    WriteByte(EncodeModRM(ModModeNum(mode), areg, arm));

  if usesib then begin
    WriteByte(EncodeSIB(Scale, regnum(index), regnum(base)));
  end;

    //Do we have to append an offset?
  case offsize of
    os8: WriteByte(byte(offset)); //ignore sign..
    os32: writelongword(longword(offset));
  end;
end;

procedure TASMInline.Nop;
begin
  WriteByte($90);
end;

procedure TASMInline.Mov(reg1: TRegister32Set; i: longword);
begin
  mov(addr(singlemember(reg1)), i);
end;

procedure TASMInline.Mov(mem: TMemoryAddress; i: longword);
begin
  case mem.size of
    ms8: begin
        if i > high(byte) then
          raise EOperandSizeMismatch.create;
        writebyte($C6);
        WriteRegRef(mem, 0);
        writebyte(i);
      end;
    ms16: begin
        if i > high(word) then
          raise EOperandSizeMismatch.create;
        WriteOpSizeOverride();
        writebyte($C7);
        WriteRegRef(mem, 0);
        writeword(i);
      end;
    ms32: begin
        writebyte($C7);
        WriteRegRef(mem, 0);
        writelongword(i);
      end;
  else raise EOperandSizeMismatch.create;
  end;
end;

procedure TASMInline.Mov(mem: TMemoryAddress; reg: TRegister32);
begin
  require(mem.size, ms32);
  WriteByte($89);
  WriteRegRef(mem, reg);
end;

procedure TASMInline.Mov(mem: TMemoryAddress; reg: TRegister16);
begin
  require(mem.size, ms16);
  WriteOpSizeOverride;
  WriteByte($89);
  WriteRegRef(mem, reg);
end;

procedure TASMInline.Mov(mem: TMemoryAddress; reg: TRegister8);
begin
  require(mem.size, ms8);
  WriteByte($88);
  WriteRegRef(mem, reg);
end;

procedure TASMInline.Mov(reg: TRegister32; mem: TMemoryAddress);
begin
  require(mem.size, ms32);
  WriteByte($8B);
  WriteRegRef(mem, reg);
end;

procedure TASMInline.Mov(reg: TRegister16; mem: TMemoryAddress);
begin
  require(mem.size, ms16);
  WriteOpSizeOverride;
  WriteByte($8B);
  WriteRegRef(mem, reg);
end;

procedure TASMInline.Mov(reg: TRegister8; mem: TMemoryAddress);
begin
  require(mem.size, ms8);
  WriteByte($8A);
  WriteRegRef(mem, reg);
end;

procedure TASMInline.Mov(reg1: TRegister32Set; reg2: TRegister32);
begin
  Mov(addr(singlemember(reg1)), reg2);
end;

procedure TASMInline.Mov(reg1: TRegister32; reg2: TRegister32Set);
begin
  mov(reg1, addr(singlemember(reg2)));
end;

procedure TASMInline.Mov(reg1: TRegister8; reg2: TRegister8);
begin
  WriteByte($88);
  WriteRegRef(reg1, reg2);
end;

procedure TASMInline.Mov(reg1: TRegister16; reg2: TRegister16);
begin
  WriteOpSizeOverride;
  writebyte($89);
  WriteRegRef(reg1, reg2);
end;

procedure TASMInline.Mov(reg1: TRegister32; reg2: TRegister32);
begin
  writebyte($89);
  WriteRegRef(reg1, reg2);
end;

procedure TASMInline.Mov(reg: TRegister8; b: byte);
begin
  writebyte($B0 + regnum(reg));
  writebyte(b);
end;

procedure TASMInline.Mov(reg: TRegister16; b: word);
begin
  WriteOpSizeOverride;
  writebyte($B8 + regnum(reg));
  writeword(b);
end;

procedure TASMInline.Mov(reg: TRegister32; b: longword);
begin
  writebyte($B8 + regnum(reg));
  writelongword(b);
end;

constructor TASMInline.create;
begin
  fbuffer := tmemorystream.create;
  frelocs := tobjectlist.create;
  flabels := TLabelList.create;
  flabelrefs := TObjectlist.create;
end;

destructor TASMInline.destroy;
begin
  fbuffer.free;
  frelocs.free;
  flabels.free;
  flabelrefs.free;
  inherited;
end;

end.

