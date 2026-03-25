unit uPSUtils;
{$I PascalScript.inc}

interface
uses
  Classes, SysUtils {$IFDEF VER130}, Windows {$ENDIF};

const

  PSMainProcName = '!MAIN';

  PSMainProcNameOrg = 'Main Proc';

  PSLowBuildSupport = 12;

  PSCurrentBuildNo = 23;

  PSCurrentversion = '1.31';

  PSValidHeader = 1397769801;

  PSAddrStackStart = 1610612736;

  PSAddrNegativeStackStart = 1073741824;
type
  TbtString = {$IFDEF DELPHI2009UP}AnsiString{$ELSE}String{$ENDIF};

  TPSBaseType = Byte;

  TPSVariableType = (ivtGlobal, ivtParam, ivtVariable);

const

  btReturnAddress   = 0;

  btU8              = 1;

  btS8              = 2;

  btU16             = 3;

  btS16             = 4;

  btU32             = 5;

  btS32             = 6;

  btSingle          = 7;

  btDouble          = 8;

  btExtended        = 9;

  btString          = 10;

  btRecord          = 11;

  btArray           = 12;

  btPointer         = 13;

  btPChar           = 14;

  btResourcePointer = 15;

  btVariant         = 16;

{$IFNDEF PS_NOINT64}
  btS64             = 17;
{$ENDIF}

  btChar            = 18;

{$IFNDEF PS_NOWIDESTRING}
  btWideString      = 19;

  btWideChar        = 20;
{$ENDIF}

  btProcPtr         = 21;

  btStaticArray     = 22;

  btSet             = 23;

  btCurrency        = 24;

  btClass           = 25;

  btInterface       = 26;

  btNotificationVariant = 27;

  btUnicodeString = 28;

  btType = 130;

  btEnum = 129;

  btExtClass = 131;

function MakeHash(const s:  TbtString): Longint;

const
{ Script internal command: Assign command<br>
    Command: TPSCommand;<br>
    VarDest, // no data<br>
    VarSrc: TPSVariable;<br>
}
  CM_A = 0;
{ Script internal command: Calculate Command<br>
    Command: TPSCommand; <br>
    CalcType: Byte;<br>
    <i><br>
      0 = +<br>
      1 = -<br>
      2 = *<br>
      3 = /<br>
      4 = MOD<br>
      5 = SHL<br>
      6 = SHR<br>
      7 = AND<br>
      8 = OR<br>
      9 = XOR<br>
    </i><br>
    VarDest, // no data<br>
    VarSrc: TPSVariable;<br>
<br>
}
  CM_CA = 1;
{ Script internal command: Push<br>
    Command: TPSCommand; <br>
    Var: TPSVariable;<br>
}
  CM_P = 2;
{ Script internal command: Push Var<br>
    Command: TPSCommand; <br>
    Var: TPSVariable;<br>
}
  CM_PV = 3;
{ Script internal command: Pop<br>
    Command: TPSCommand; <br>
}
  CM_PO = 4;
{ Script internal command: Call<br>
    Command: TPSCommand; <br>
    ProcNo: Longword;<br>
}
  Cm_C = 5;
{ Script internal command: Goto<br>
    Command: TPSCommand; <br>
    NewPosition: Longint; //relative to end of this instruction<br>
}
  Cm_G = 6;
{ Script internal command: Conditional Goto<br>
    Command: TPSCommand; <br>
    NewPosition: LongWord; //relative to end of this instruction<br>
    Var: TPSVariable; // no data<br>
}
  Cm_CG = 7;
{ Script internal command: Conditional NOT Goto<br>
    Command: TPSCommand; <br>
    NewPosition: LongWord; // relative to end of this instruction<br>
    Var: TPSVariable; // no data<br>
}
  Cm_CNG = 8;
{ Script internal command: Ret<br>
    Command: TPSCommand; <br>
}
  Cm_R = 9;
{ Script internal command: Set Stack Type<br>
    Command: TPSCommand; <br>
    NewType: LongWord;<br>
    OffsetFromBase: LongWord;<br>
}
  Cm_ST = 10;
{ Script internal command: Push Type<br>
    Command: TPSCommand; <br>
    FType: LongWord;<br>
}
  Cm_Pt = 11;
{ Script internal command: Compare<br>
    Command: TPSCommand; <br>
    CompareType: Byte;<br>
    <i><br>
     0 = &gt;=<br>
     1 = &lt;=<br>
     2 = &gt;<br>
     3 = &lt;<br>
     4 = &lt;&gt<br>
     5 = =<br>
    <i><br>
    IntoVar: TPSAssignment;<br>
    Compare1, Compare2: TPSAssigment;<br>
}
  CM_CO = 12;
{ Script internal command: Call Var<br>
    Command: TPSCommand; <br>
    Var: TPSVariable;<br>
}
  Cm_cv = 13;
{ Script internal command: Set Pointer<br>
    Command: TPSCommand; <br>
    VarDest: TPSVariable;<br>
    VarSrc: TPSVariable;<br>
}
  cm_sp = 14;
{ Script internal command: Boolean NOT<br>
    Command: TPSCommand; <br>
    Var: TPSVariable;<br>
}
  cm_bn = 15;
{ Script internal command: Var Minus<br>
    Command: TPSCommand; <br>
    Var: TPSVariable;
}
  cm_vm = 16;
{ Script internal command: Set Flag<br>
    Command: TPSCommand; <br>
    Var: TPSVariable;<br>
    DoNot: Boolean;<br>
}
  cm_sf = 17;
{ Script internal command: Flag Goto<br>
    Command: TPSCommand; <br>
    Where: Cardinal;<br>
}
  cm_fg = 18;
{ Script internal command: Push Exception Handler<br>
    Command: TPSCommand; <br>
    FinallyOffset,<br>
    ExceptionOffset, // FinallyOffset or ExceptionOffset need to be set.<br>
    Finally2Offset,<br>
    EndOfBlock: Cardinal;<br>
}
  cm_puexh = 19;
{ Script internal command: Pop Exception Handler<br>
    Command:TPSCommand; <br>
    Position: Byte;<br>
    <i> 0 = end of try/finally/exception block;<br>
      1 = end of first finally<br>
      2 = end of except<br>
      3 = end of second finally<br>
    </i><br>
}
  cm_poexh = 20;
{ Script internal command: Integer NOT<br>
    Command: TPSCommand; <br>
    Where: Cardinal;<br>
}
  cm_in = 21;
  {Script internal command: Set Stack Pointer To Copy<br>
      Command: TPSCommand; <br>
    Where: Cardinal;<br>
}
  cm_spc = 22;
  {Script internal command: Inc<br>
    Command: TPSCommand; <br>
    Var: TPSVariable;<br>
  }
  cm_inc = 23;
  {Script internal command: Dec<br>
      Command: TPSCommand; <br>
    Var: TPSVariable;<br>
  }
  cm_dec = 24;
  {Script internal command: nop<br>
      Command: TPSCommand; <br>}
  cm_nop = 255;
{ Script internal command: Pop and Goto<br>
    Command: TPSCommand; <br>
    NewPosition: Longint; //relative to end of this instruction<br>
}
  Cm_PG = 25;
{ Script internal command: Pop*2 and Goto<br>
    Command: TPSCommand; <br>
    NewPosition: Longint; //relative to end of this instruction<br>
}
  Cm_P2G = 26;


type

  TbtU8 = Byte;

  TbtS8 = ShortInt;

  TbtU16 = Word;

  TbtS16 = SmallInt;

  TbtU32 = Cardinal;

  TbtS32 = Longint;

  TbtSingle = Single;

  TbtDouble = double;

  TbtExtended = Extended;

  tbtCurrency = Currency;

{$IFNDEF PS_NOINT64}

  tbts64 = int64;
{$ENDIF}

  tbtchar = {$IFDEF DELPHI4UP}AnsiChar{$ELSE}CHAR{$ENDIF};
{$IFNDEF PS_NOWIDESTRING}

  tbtwidestring = widestring;
  tbtunicodestring = {$IFDEF DELPHI2009UP}UnicodeString{$ELSE}widestring{$ENDIF};

  tbtwidechar = widechar;
  tbtNativeString = {$IFDEF DELPHI2009UP}tbtUnicodeString{$ELSE}tbtString{$ENDIF};
{$ENDIF}
{$IFDEF FPC}
  IPointer = PtrUInt;
{$ELSE}
  {$IFDEF CPUX64}
  IPointer = IntPtr;
  {$ELSE}
  {$IFDEF CPU64} IPointer = LongWord;{$ELSE}  IPointer = Cardinal;{$ENDIF}{$ENDIF}
{$ENDIF}
  TPSCallingConvention = (cdRegister, cdPascal, cdCdecl, cdStdCall, cdSafeCall);


const

  PointerSize = IPointer({$IFDEF CPU64}8{$ELSE}4{$ENDIF});
  PointerSize2 = IPointer(2*PointerSize);
  MaxListSize = Maxint div 16;

type

  PPointerList = ^TPointerList;

  TPointerList = array[0..MaxListSize - 1] of Pointer;


  TPSList = class(TObject)
  protected

    FData: PPointerList;

    FCapacity: Cardinal;

    FCount: Cardinal;

    FCheckCount: Cardinal;
  private
    function GetItem(Nr: Cardinal): Pointer;
    procedure SetItem(Nr: Cardinal; P: Pointer);
  public
    {$IFNDEF PS_NOSMARTLIST}

    procedure Recreate;
    {$ENDIF}

    property Data: PPointerList read FData;

    constructor Create;

    function IndexOf(P: Pointer): Longint;

    destructor Destroy; override;

    property Count: Cardinal read FCount;

    property Items[nr: Cardinal]: Pointer read GetItem write SetItem; default;

    function Add(P: Pointer): Longint;

    procedure AddBlock(List: PPointerList; Count: Longint);

    procedure Remove(P: Pointer);

    procedure Delete(Nr: Cardinal);

    procedure DeleteLast;

    procedure Clear; virtual;
  end;
  TIFList = TPSList;

  TPSStringList = class(TObject)
  private
    List: TPSList;
    function GetItem(Nr: LongInt): TbtString;
    procedure SetItem(Nr: LongInt; const s: TbtString);
  public

    function Count: LongInt;

    property Items[Nr: Longint]: TbtString read GetItem write SetItem; default;


    procedure Add(const P: TbtString);

    procedure Delete(NR: LongInt);

    procedure Clear;

    constructor Create;

    destructor Destroy; override;
  end;
  TIFStringList = TPSStringList;

  TPSUnitList = class;

  TPSUnit = class(TObject)
  private
    fList     : TPSUnitList;
    fUnits    : TPSList;
    fUnitName : TbtString;
    procedure SetUnitName(const Value: TbtString);
  public
    constructor Create(List: TPSUnitList);

    destructor Destroy; override;

    procedure AddUses(pUnitName: TbtString);

    function HasUses(pUnitName: TbtString): Boolean;

    {$WARNINGS OFF}
    property UnitName: TbtString read fUnitName write SetUnitName;
    {$WARNINGS ON}
  end;

  TPSUnitList = class
  private
    fList: TPSList;
    function Add: TPSUnit;

  public
    constructor Create;

    function GetUnit(UnitName: TbtString): TPSUnit;

    destructor Destroy; override;
  end;



type

  TPSPasToken = (
    CSTI_EOF,

    CSTIINT_Comment,
    CSTIINT_WhiteSpace,

    CSTI_Identifier,
    CSTI_SemiColon,
    CSTI_Comma,
    CSTI_Period,
    CSTI_Colon,
    CSTI_OpenRound,
    CSTI_CloseRound,
    CSTI_OpenBlock,
    CSTI_CloseBlock,
    CSTI_Assignment,
    CSTI_Equal,
    CSTI_NotEqual,
    CSTI_Greater,
    CSTI_GreaterEqual,
    CSTI_Less,
    CSTI_LessEqual,
    CSTI_Plus,
    CSTI_Minus,
    CSTI_Divide,
    CSTI_Multiply,
    CSTI_Integer,
    CSTI_Real,
    CSTI_String,
    CSTI_Char,
    CSTI_HexInt,
    CSTI_AddressOf,
    CSTI_Dereference,
    CSTI_TwoDots,

    CSTII_and,
    CSTII_array,
    CSTII_begin,
    CSTII_case,
    CSTII_const,
    CSTII_div,
    CSTII_do,
    CSTII_downto,
    CSTII_else,
    CSTII_end,
    CSTII_for,
    CSTII_function,
    CSTII_if,
    CSTII_in,
    CSTII_mod,
    CSTII_not,
    CSTII_of,
    CSTII_or,
    CSTII_procedure,
    CSTII_program,
    CSTII_repeat,
    CSTII_record,
    CSTII_set,
    CSTII_shl,
    CSTII_shr,
    CSTII_then,
    CSTII_to,
    CSTII_type,
    CSTII_until,
    CSTII_uses,
    CSTII_var,
    CSTII_while,
    CSTII_with,
    CSTII_xor,
    CSTII_exit,
    CSTII_class,
    CSTII_constructor,
    CSTII_destructor,
    CSTII_inherited,
    CSTII_private,
    CSTII_public,
    CSTII_published,
    CSTII_protected,
    CSTII_property,
    CSTII_virtual,
    CSTII_override,
    //CSTII_default, //Birb
    CSTII_As,
    CSTII_Is,
    CSTII_Unit,
    CSTII_Try,
    CSTII_Except,
    CSTII_Finally,
    CSTII_External,
    CSTII_Forward,
    CSTII_Export,
    CSTII_Label,
    CSTII_Goto,
    CSTII_Chr,
    CSTII_Ord,
    CSTII_Interface,
    CSTII_Implementation,
    CSTII_initialization,            //* Nvds
    CSTII_finalization,              //* Nvds
    CSTII_out,
    CSTII_nil
    );

  TPSParserErrorKind = (iNoError
  , iCommentError
  , iStringError
  , iCharError
  , iSyntaxError
  );
  TPSParserErrorEvent = procedure (Parser: TObject; Kind: TPSParserErrorKind) of object;


  TPSPascalParser = class(TObject)
  protected
    FData: TbtString;
    FText: {$IFDEF DELPHI4UP}PAnsiChar{$ELSE}PChar{$ENDIF};
    FLastEnterPos, FRow, FRealPosition, FTokenLength: Cardinal;
    FTokenId: TPSPasToken;
    FToken: TbtString;
    FOriginalToken: TbtString;
    FParserError: TPSParserErrorEvent;
    FEnableComments: Boolean;
    FEnableWhitespaces: Boolean;
    function GetCol: Cardinal;
    // only applicable when Token in [CSTI_Identifier, CSTI_Integer, CSTI_Real, CSTI_String, CSTI_Char, CSTI_HexInt]
  public

    property EnableComments: Boolean read FEnableComments write FEnableComments;

    property EnableWhitespaces: Boolean read FEnableWhitespaces write FEnableWhitespaces;

    procedure Next; virtual;

    property GetToken: TbtString read FToken;

    property OriginalToken: TbtString read FOriginalToken;

    property CurrTokenPos: Cardinal read FRealPosition;

    property CurrTokenID: TPSPasToken read FTokenId;

    property Row: Cardinal read FRow;

    property Col: Cardinal read GetCol;

    procedure SetText(const Data: TbtString); virtual;

    property OnParserError: TPSParserErrorEvent read FParserError write FParserError;
  end;

function FloatToStr(E: Extended): TbtString;

function FastLowerCase(const s: TbtString): TbtString;

function Fw(const S: TbtString): TbtString;

function IntToStr(I: LongInt): TbtString;

function StrToIntDef(const S: TbtString; Def: LongInt): LongInt;

function StrToInt(const S: TbtString): LongInt;
function StrToFloat(const s: TbtString): Extended;

function FastUpperCase(const s: TbtString): TbtString;

function GRFW(var s: TbtString): TbtString;
function GRLW(var s: TbtString): TbtString;

const

  FCapacityInc = 32;
{$IFNDEF PS_NOSMARTLIST}

  FMaxCheckCount = (FCapacityInc div 4) * 64;
{$ENDIF}

{$IFDEF VER130}
function WideUpperCase(const S: WideString): WideString;
function WideLowerCase(const S: WideString): WideString;
{$ENDIF}
implementation

{$IFDEF DELPHI3UP }
resourceString
{$ELSE }
const
{$ENDIF }
  RPS_InvalidFloat = 'Invalid float';

{$IFDEF VER130}

function WideUpperCase(const S: WideString): WideString;
var
  Len: Integer;
begin
  // CharUpperBuffW is stubbed out on Win9x platofmrs
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    Len := Length(S);
    SetString(Result, PWideChar(S), Len);
    if Len > 0 then CharUpperBuffW(Pointer(Result), Len);
  end
  else
    Result := AnsiUpperCase(S);
end;

function WideLowerCase(const S: WideString): WideString;
var
  Len: Integer;
begin
  // CharLowerBuffW is stubbed out on Win9x platofmrs
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    Len := Length(S);
    SetString(Result, PWideChar(S), Len);
    if Len > 0 then CharLowerBuffW(Pointer(Result), Len);
  end
  else
    Result := AnsiLowerCase(S);
end;

{$ENDIF}


function MakeHash(const s: TbtString): Longint;
{small hash maker}
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(s) do
    Result := ((Result shl 7) or (Result shr 25)) + Ord(s[I]);
end;

function GRFW(var s: TbtString): TbtString;
var
  l: Longint;
begin
  l := 1;
  while l <= Length(s) do
  begin
    if s[l] = ' ' then
    begin
      Result := copy(s, 1, l - 1);
      Delete(s, 1, l);
      exit;
    end;
    l := l + 1;
  end;
  Result := s;
  s := '';
end;

function GRLW(var s: TbtString): TbtString;
var
  l: Longint;
begin
  l := Length(s);
  while l >= 1 do
  begin
    if s[l] = ' ' then
    begin
      Result := copy(s, l+1, MaxInt);
      Delete(s, l, MaxInt);
      exit;
    end;
    Dec(l);
  end;
  Result := s;
  s := '';
end;

function StrToFloat(const s: TbtString): Extended;
var
  i: longint;
begin
  Val(string(s), Result, i);
  if i <> 0 then raise Exception.Create(RPS_InvalidFloat);
end;
//-------------------------------------------------------------------

function IntToStr(I: LongInt): TbtString;
var
  s: tbtstring;
begin
  Str(i, s);
  IntToStr := s;
end;
//-------------------------------------------------------------------

function FloatToStr(E: Extended): TbtString;
var
  s: tbtstring;
begin
  Str(e:0:12, s);
  result := s;
end;

function StrToInt(const S: TbtString): LongInt;
var
  e: Integer;
  Res: LongInt;
begin
  Val(string(S), Res, e);
  if e <> 0 then
    StrToInt := -1
  else
    StrToInt := Res;
end;
//-------------------------------------------------------------------

function StrToIntDef(const S: TbtString; Def: LongInt): LongInt;
var
  e: Integer;
  Res: LongInt;
begin
  Val(string(S), Res, e);
  if e <> 0 then
    StrToIntDef := Def
  else
    StrToIntDef := Res;
end;
//-------------------------------------------------------------------

constructor TPSList.Create;
begin
  inherited Create;
  FCount := 0;
  FCapacity := 16;
  {$IFNDEF PS_NOSMARTLIST}
  FCheckCount := 0;
  {$ENDIF}
  GetMem(FData, FCapacity * PointerSize);
end;


function MM(i1,i2: Integer): Integer;
begin
  if ((i1 div i2) * i2) < i1 then
    mm := (i1 div i2 + 1) * i2
  else
    mm := (i1 div i2) * i2;
end;

{$IFNDEF PS_NOSMARTLIST}
procedure TPSList.Recreate;
var
  NewData: PPointerList;
  NewCapacity: Cardinal;
  I: Longint;

begin

  FCheckCount := 0;
  NewCapacity := mm(FCount, FCapacityInc);
  if NewCapacity < 64 then NewCapacity := 64;
  GetMem(NewData, NewCapacity * PointerSize);
  for I := 0 to Longint(FCount) -1 do
  begin
    NewData^[i] := FData^[I];
  end;
  FreeMem(FData, FCapacity * PointerSize);
  FData := NewData;
  FCapacity := NewCapacity;
end;
{$ENDIF}

//-------------------------------------------------------------------

function TPSList.Add(P: Pointer): Longint;
begin
  if FCount >= FCapacity then
  begin
    Inc(FCapacity, FCapacityInc);// := FCount + 1;
    ReAllocMem(FData, FCapacity * PointerSize);
  end;
  FData[FCount] := P; // Instead of SetItem
  Result := FCount;
  Inc(FCount);
{$IFNDEF PS_NOSMARTLIST}
  Inc(FCheckCount);
  if FCheckCount > FMaxCheckCount then Recreate;
{$ENDIF}
end;

procedure TPSList.AddBlock(List: PPointerList; Count: Longint);
var
  L: Longint;

begin
  if Longint(FCount) + Count > Longint(FCapacity) then
  begin
    Inc(FCapacity, mm(Count, FCapacityInc));
    ReAllocMem(FData, FCapacity *PointerSize);
  end;
  for L := 0 to Count -1 do
  begin
    FData^[FCount] := List^[L];
    Inc(FCount);
  end;
{$IFNDEF PS_NOSMARTLIST}
  Inc(FCheckCount);
  if FCheckCount > FMaxCheckCount then Recreate;
{$ENDIF}
end;


//-------------------------------------------------------------------

procedure TPSList.DeleteLast;
begin
  if FCount = 0 then Exit;
  Dec(FCount);
{$IFNDEF PS_NOSMARTLIST}
    Inc(FCheckCount);
    if FCheckCount > FMaxCheckCount then Recreate;
{$ENDIF}
end;



procedure TPSList.Delete(Nr: Cardinal);
begin
  if FCount = 0 then Exit;
  if Nr < FCount then
  begin
    {dec count first, so we move one element less in the move below}
    Dec(FCount);
    {only move if we aren't deleting the last element}
    if Nr < FCount then
      Move(FData[Nr + 1], FData[Nr], (FCount - Nr) * PointerSize);
{$IFNDEF PS_NOSMARTLIST}
    Inc(FCheckCount);
    if FCheckCount > FMaxCheckCount then Recreate;
{$ENDIF}
  end;
end;
//-------------------------------------------------------------------

procedure TPSList.Remove(P: Pointer);
var
  I: Cardinal;
begin
  if FCount = 0 then Exit;
  I := 0;
  while I < FCount do
  begin
    if FData[I] = P then
    begin
      Delete(I);
      Exit;
    end;
    Inc(I);
  end;
end;
//-------------------------------------------------------------------

procedure TPSList.Clear;
begin
  FCount := 0;
{$IFNDEF PS_NOSMARTLIST}
  Recreate;
{$ENDIF}
end;
//-------------------------------------------------------------------

destructor TPSList.Destroy;
begin
  FreeMem(FData, FCapacity * PointerSize);
  inherited Destroy;
end;
//-------------------------------------------------------------------

procedure TPSList.SetItem(Nr: Cardinal; P: Pointer);
begin
  if (FCount = 0) or (Nr >= FCount) then
    Exit;
  FData[Nr] := P;
end;
//-------------------------------------------------------------------

function TPSList.GetItem(Nr: Cardinal): Pointer;  {12}
begin
  if Nr < FCount then
     GetItem := FData[Nr]
  else
    GetItem := nil;
end;



//-------------------------------------------------------------------

function TPSStringList.Count: LongInt;
begin
  count := List.count;
end;
type pStr = ^TbtString;

//-------------------------------------------------------------------

function TPSStringList.GetItem(Nr: LongInt): TbtString;
var
  S: PStr;
begin
  s := List.GetItem(Nr);
  if s = nil then
    Result := ''
  else

    Result := s^;
end;
//-------------------------------------------------------------------


procedure TPSStringList.SetItem(Nr: LongInt; const s: TbtString);
var
  p: PStr;
begin
  p := List.GetItem(Nr);
  if p = nil
    then
    Exit;
  p^ := s;
end;
//-------------------------------------------------------------------

procedure TPSStringList.Add(const P: TbtString);
var
  w: PStr;
begin
  new(w);
  w^ := p;
  List.Add(w);
end;
//-------------------------------------------------------------------

procedure TPSStringList.Delete(NR: LongInt);
var
  W: PStr;
begin
  W := list.getitem(nr);
  if w<>nil then
  begin
    dispose(w);
  end;
  list.Delete(Nr);
end;

procedure TPSStringList.Clear;
begin
  while List.Count > 0 do Delete(Pred(List.Count));
end;


constructor TPSStringList.Create;
begin
  inherited Create;
  List := TPSList.Create;
end;

destructor TPSStringList.Destroy;
begin
  while List.Count > 0 do
    Delete(0);
  List.Destroy;
  inherited Destroy;
end;

//-------------------------------------------------------------------


function Fw(const S: TbtString): TbtString; //  First word
var
  x: integer;
begin
  x := pos(tbtstring(' '), s);
  if x > 0
    then Fw := Copy(S, 1, x - 1)
  else Fw := S;
end;
//-------------------------------------------------------------------
function FastUpperCase(const s: TbtString): TbtString;
{Fast uppercase}
var
  I: Integer;
  C: tbtChar;
begin
  Result := S;
  I := Length(Result);
  while I > 0 do
  begin
    C := Result[I];
    if c in [#97..#122] then
      Result[I] := tbtchar(Ord(Result[I]) -32);
    Dec(I);
  end;
end;
function FastLowerCase(const s: TbtString): TbtString;
{Fast lowercase}
var
  I: Integer;
  C: tbtChar;
begin
  Result := S;
  I := Length(Result);
  while I > 0 do
  begin
    C := Result[I];
    if C in [#65..#90] then
      Result[I] := tbtchar(Ord(Result[I]) + 32);
    Dec(I);
  end;
end;
//-------------------------------------------------------------------

type
  TRTab = record
    name: TbtString;
    c: TPSPasToken;
  end;


const
  KEYWORD_COUNT = 65;  //*NVDS
  LookupTable: array[0..KEYWORD_COUNT - 1] of TRTab = (
      (name: 'AND'; c: CSTII_and),
      (name: 'ARRAY'; c: CSTII_array),
      (name: 'AS'; c: CSTII_as),
      (name: 'BEGIN'; c: CSTII_begin),
      (name: 'CASE'; c: CSTII_case),
      (name: 'CHR'; c: CSTII_chr),
      (name: 'CLASS'; c: CSTII_class),
      (name: 'CONST'; c: CSTII_const),
      (name: 'CONSTRUCTOR'; c: CSTII_constructor),
      (name: 'DESTRUCTOR'; c: CSTII_destructor),
      (name: 'DIV'; c: CSTII_div),
      (name: 'DO'; c: CSTII_do),
      (name: 'DOWNTO'; c: CSTII_downto),
      (name: 'ELSE'; c: CSTII_else),
      (name: 'END'; c: CSTII_end),
      (name: 'EXCEPT'; c: CSTII_except),
      (name: 'EXIT'; c: CSTII_exit),
      (name: 'EXPORT'; c: CSTII_Export),
      (name: 'EXTERNAL'; c: CSTII_External),
      (Name: 'FINALIZATION'; c : CSTII_finalization),//* Nvds
      (name: 'FINALLY'; c: CSTII_finally),
      (name: 'FOR'; c: CSTII_for),
      (name: 'FORWARD'; c: CSTII_Forward),
      (name: 'FUNCTION'; c: CSTII_function),
      (name: 'GOTO'; c: CSTII_Goto),
      (name: 'IF'; c: CSTII_if),
      (name: 'IMPLEMENTATION'; c: CSTII_Implementation),
      (name: 'IN'; c: CSTII_in),
      (name: 'INHERITED'; c: CSTII_inherited),
      (Name: 'INITIALIZATION'; c: CSTII_initialization), //* Nvds
      (name: 'INTERFACE'; c: CSTII_Interface),
      (name: 'IS'; c: CSTII_is),
      (name: 'LABEL'; c: CSTII_Label),
      (name: 'MOD'; c: CSTII_mod),
      (name: 'NIL'; c: CSTII_nil),
      (name: 'NOT'; c: CSTII_not),
      (name: 'OF'; c: CSTII_of),
      (name: 'OR'; c: CSTII_or),
      (name: 'ORD'; c: CSTII_ord),
      (name: 'OUT'; c: CSTII_Out),
      (name: 'OVERRIDE'; c: CSTII_override),
      //(name: 'DEFAULT'; c: CSTII_default), //Birb (if added, don't forget to increase KEYWORD_COUNT)
      (name: 'PRIVATE'; c: CSTII_private),
      (name: 'PROCEDURE'; c: CSTII_procedure),
      (name: 'PROGRAM'; c: CSTII_program),
      (name: 'PROPERTY'; c: CSTII_property),
      (name: 'PROTECTED'; c: CSTII_protected),
      (name: 'PUBLIC'; c: CSTII_public),
      (name: 'PUBLISHED'; c: CSTII_published),
      (name: 'RECORD'; c: CSTII_record),
      (name: 'REPEAT'; c: CSTII_repeat),
      (name: 'SET'; c: CSTII_set),
      (name: 'SHL'; c: CSTII_shl),
      (name: 'SHR'; c: CSTII_shr),
      (name: 'THEN'; c: CSTII_then),
      (name: 'TO'; c: CSTII_to),
      (name: 'TRY'; c: CSTII_try),
      (name: 'TYPE'; c: CSTII_type),
      (name: 'UNIT'; c: CSTII_Unit),
      (name: 'UNTIL'; c: CSTII_until),
      (name: 'USES'; c: CSTII_uses),
      (name: 'VAR'; c: CSTII_var),
      (name: 'VIRTUAL'; c: CSTII_virtual),
      (name: 'WHILE'; c: CSTII_while),
      (name: 'WITH'; c: CSTII_with),
      (name: 'XOR'; c: CSTII_xor));

function TPSPascalParser.GetCol: Cardinal;
begin
  Result := FRealPosition - FLastEnterPos + 1;
end;

procedure TPSPascalParser.Next;
var
  Err: TPSParserErrorKind;
  FLastUpToken: TbtString;
  function CheckReserved(Const S: ShortString; var CurrTokenId: TPSPasToken): Boolean;
  var
    L, H, I: LongInt;
    J: tbtChar;
    SName: ShortString;
  begin
    L := 0;
    J := S[0];
    H := KEYWORD_COUNT-1;
    while L <= H do
    begin
      I := (L + H) shr 1;
      SName := LookupTable[i].Name;
      if J = SName[0] then
      begin
        if S = SName then
        begin
          CheckReserved := True;
          CurrTokenId := LookupTable[I].c;
          Exit;
        end;
        if S > SName then
          L := I + 1
        else
          H := I - 1;
      end else
        if S > SName then
          L := I + 1
        else
          H := I - 1;
    end;
    CheckReserved := False;
  end;
  //-------------------------------------------------------------------

  function _GetToken(CurrTokenPos, CurrTokenLen: Cardinal): TbtString;
  var
    s: tbtString;
  begin
    SetLength(s, CurrTokenLen);
    Move(FText[CurrTokenPos], S[1], CurrtokenLen);
    Result := s;
  end;

  function ParseToken(var CurrTokenPos, CurrTokenLen: Cardinal; var CurrTokenId: TPSPasToken): TPSParserErrorKind;
  {Parse the token}
  var
    ct, ci: Cardinal;
    hs: Boolean;
    p: {$IFDEF DELPHI4UP}PAnsiChar{$ELSE}PChar{$ENDIF};
  begin
    ParseToken := iNoError;
    ct := CurrTokenPos;
    case FText[ct] of
      #0:
        begin
          CurrTokenId := CSTI_EOF;
          CurrTokenLen := 0;
        end;
      'A'..'Z', 'a'..'z', '_':
        begin
          ci := ct + 1;
          while (FText[ci] in ['_', '0'..'9', 'a'..'z', 'A'..'Z']) do begin
            Inc(ci);
          end;
          CurrTokenLen := ci - ct;

          FLastUpToken := _GetToken(CurrTokenPos, CurrtokenLen);
          p := {$IFDEF DELPHI4UP}PAnsiChar{$ELSE}pchar{$ENDIF}(FLastUpToken);
          while p^<>#0 do
          begin
            if p^ in [#97..#122] then
              Dec(Byte(p^), 32);
            inc(p);
          end;
          if not CheckReserved(FLastUpToken, CurrTokenId) then
          begin
            CurrTokenId := CSTI_Identifier;
          end;
        end;
      '$':
        begin
          ci := ct + 1;

          while (FText[ci] in ['0'..'9', 'a'..'f', 'A'..'F'])
            do Inc(ci);

          CurrTokenId := CSTI_HexInt;
          CurrTokenLen := ci - ct;
        end;

      '0'..'9':
        begin
          hs := False;
          ci := ct;
          while (FText[ci] in ['0'..'9']) do
          begin
            Inc(ci);
            if (FText[ci] = '.') and (not hs) then
            begin
              if FText[ci+1] = '.' then break;
              hs := True;
              Inc(ci);
            end;
          end;
          if (FText[ci] in ['E','e']) and ((FText[ci+1] in ['0'..'9'])
            or ((FText[ci+1] in ['+','-']) and (FText[ci+2] in ['0'..'9']))) then
          begin
            hs := True;
            Inc(ci);
            if FText[ci] in ['+','-'] then
              Inc(ci);
            repeat
              Inc(ci);
            until not (FText[ci] in ['0'..'9']);
          end;

          if hs
            then CurrTokenId := CSTI_Real
          else CurrTokenId := CSTI_Integer;

          CurrTokenLen := ci - ct;
        end;


      #39:
        begin
          ci := ct + 1;
          while true do
          begin
            if (FText[ci] = #0) or (FText[ci] = #13) or (FText[ci] = #10) then Break;
            if (FText[ci] = #39) then
            begin
              if FText[ci+1] = #39 then
                Inc(ci)
              else
                Break;
            end;
            Inc(ci);
          end;
          if FText[ci] = #39 then
            CurrTokenId := CSTI_String
          else
          begin
            CurrTokenId := CSTI_String;
            ParseToken := iStringError;
          end;
          CurrTokenLen := ci - ct + 1;
        end;
      '#':
        begin
          ci := ct + 1;
          if FText[ci] = '$' then
          begin
            inc(ci);
            while (FText[ci] in ['A'..'F', 'a'..'f', '0'..'9']) do begin
              Inc(ci);
            end;
            CurrTokenId := CSTI_Char;
            CurrTokenLen := ci - ct;
          end else
          begin
            while (FText[ci] in ['0'..'9']) do begin
              Inc(ci);
            end;
            if FText[ci] in ['A'..'Z', 'a'..'z', '_'] then
            begin
              ParseToken := iCharError;
              CurrTokenId := CSTI_Char;
            end else
              CurrTokenId := CSTI_Char;
            CurrTokenLen := ci - ct;
          end;
        end;
      '=':
        begin
          CurrTokenId := CSTI_Equal;
          CurrTokenLen := 1;
        end;
      '>':
        begin
          if FText[ct + 1] = '=' then
          begin
            CurrTokenid := CSTI_GreaterEqual;
            CurrTokenLen := 2;
          end else
          begin
            CurrTokenid := CSTI_Greater;
            CurrTokenLen := 1;
          end;
        end;
      '<':
        begin
          if FText[ct + 1] = '=' then
          begin
            CurrTokenId := CSTI_LessEqual;
            CurrTokenLen := 2;
          end else
            if FText[ct + 1] = '>' then
            begin
              CurrTokenId := CSTI_NotEqual;
              CurrTokenLen := 2;
            end else
            begin
              CurrTokenId := CSTI_Less;
              CurrTokenLen := 1;
            end;
        end;
      ')':
        begin
          CurrTokenId := CSTI_CloseRound;
          CurrTokenLen := 1;
        end;
      '(':
        begin
          if FText[ct + 1] = '*' then
          begin
            ci := ct + 1;
            while (FText[ci] <> #0) do begin
              if (FText[ci] = '*') and (FText[ci + 1] = ')') then
                Break;
              if FText[ci] = #13 then
              begin
                inc(FRow);
                if FText[ci+1] = #10 then
                  inc(ci);
                FLastEnterPos := ci +1;
              end else if FText[ci] = #10 then
              begin
                inc(FRow);
                FLastEnterPos := ci +1;
              end;
              Inc(ci);
            end;
            if (FText[ci] = #0) then
            begin
              CurrTokenId := CSTIINT_Comment;
              ParseToken := iCommentError;
            end else
            begin
              CurrTokenId := CSTIINT_Comment;
              Inc(ci, 2);
            end;
            CurrTokenLen := ci - ct;
          end
          else
          begin
            CurrTokenId := CSTI_OpenRound;
            CurrTokenLen := 1;
          end;
        end;
      '[':
        begin
          CurrTokenId := CSTI_OpenBlock;
          CurrTokenLen := 1;
        end;
      ']':
        begin
          CurrTokenId := CSTI_CloseBlock;
          CurrTokenLen := 1;
        end;
      ',':
        begin
          CurrTokenId := CSTI_Comma;
          CurrTokenLen := 1;
        end;
      '.':
        begin
          if FText[ct + 1] = '.' then
          begin
            CurrTokenLen := 2;
            CurrTokenId := CSTI_TwoDots;
          end else
          begin
            CurrTokenId := CSTI_Period;
            CurrTokenLen := 1;
          end;
        end;
      '@':
        begin
          CurrTokenId := CSTI_AddressOf;
          CurrTokenLen := 1;
        end;
      '^':
        begin
          CurrTokenId := CSTI_Dereference;
          CurrTokenLen := 1;
        end;
      ';':
        begin
          CurrTokenId := CSTI_Semicolon;
          CurrTokenLen := 1;
        end;
      ':':
        begin
          if FText[ct + 1] = '=' then
          begin
            CurrTokenId := CSTI_Assignment;
            CurrTokenLen := 2;
          end else
          begin
            CurrTokenId := CSTI_Colon;
            CurrTokenLen := 1;
          end;
        end;
      '+':
        begin
          CurrTokenId := CSTI_Plus;
          CurrTokenLen := 1;
        end;
      '-':
        begin
          CurrTokenId := CSTI_Minus;
          CurrTokenLen := 1;
        end;
      '*':
        begin
          CurrTokenId := CSTI_Multiply;
          CurrTokenLen := 1;
        end;
      '/':
        begin
          if FText[ct + 1] = '/' then
          begin
            ci := ct + 1;
            while (FText[ci] <> #0) and (FText[ci] <> #13) and
              (FText[ci] <> #10) do begin
              Inc(ci);
            end;
            if (FText[ci] = #0) then
            begin
              CurrTokenId := CSTIINT_Comment;
            end else
            begin
              CurrTokenId := CSTIINT_Comment;
            end;
            CurrTokenLen := ci - ct;
          end else
          begin
            CurrTokenId := CSTI_Divide;
            CurrTokenLen := 1;
          end;
        end;
      #32, #9, #13, #10:
        begin
          ci := ct;
          while (FText[ci] in [#32, #9, #13, #10]) do
          begin
            if FText[ci] = #13 then
            begin
              inc(FRow);
              if FText[ci+1] = #10 then
                inc(ci);
              FLastEnterPos := ci +1;
            end else if FText[ci] = #10 then
            begin
              inc(FRow);
              FLastEnterPos := ci +1;
            end;
            Inc(ci);
          end;
          CurrTokenId := CSTIINT_WhiteSpace;
          CurrTokenLen := ci - ct;
        end;
      '{':
        begin
          ci := ct + 1;
          while (FText[ci] <> #0) and (FText[ci] <> '}') do begin
            if FText[ci] = #13 then
            begin
              inc(FRow);
              if FText[ci+1] = #10 then
                inc(ci);
              FLastEnterPos := ci + 1;
            end else if FText[ci] = #10 then
            begin
              inc(FRow);
              FLastEnterPos := ci + 1;
            end;
            Inc(ci);
          end;
          if (FText[ci] = #0) then
          begin
            CurrTokenId := CSTIINT_Comment;
            ParseToken := iCommentError;
          end else
            CurrTokenId := CSTIINT_Comment;
          CurrTokenLen := ci - ct + 1;
        end;
    else
      begin
        ParseToken := iSyntaxError;
        CurrTokenId := CSTIINT_Comment;
        CurrTokenLen := 1;
      end;
    end;
  end;
  //-------------------------------------------------------------------
begin
  if FText = nil then
  begin
    FTokenLength := 0;
    FRealPosition := 0;
    FTokenId := CSTI_EOF;
    Exit;
  end;
  repeat
    FRealPosition := FRealPosition + Cardinal(FTokenLength);
    Err := ParseToken(FRealPosition, Cardinal(FTokenLength), FTokenID);
    if Err <> iNoError then
    begin
      FTokenLength := 0;
      FTokenId := CSTI_EOF;
      FToken := '';
      FOriginalToken := '';
      if @FParserError <> nil then FParserError(Self, Err);
      exit;
    end;

    case FTokenID of
      CSTIINT_Comment: if not FEnableComments then Continue else
        begin
          SetLength(FOriginalToken, FTokenLength);
          Move(FText[CurrTokenPos], FOriginalToken[1], FTokenLength);
          FToken := FOriginalToken;
        end;
      CSTIINT_WhiteSpace: if not FEnableWhitespaces then Continue else
        begin
          SetLength(FOriginalToken, FTokenLength);
          Move(FText[CurrTokenPos], FOriginalToken[1], FTokenLength);
          FToken := FOriginalToken;
        end;
      CSTI_Integer, CSTI_Real, CSTI_String, CSTI_Char, CSTI_HexInt:
        begin
          SetLength(FOriginalToken, FTokenLength);
          Move(FText[CurrTokenPos], FOriginalToken[1], FTokenLength);
          FToken := FOriginalToken;
        end;
      CSTI_Identifier:
        begin
          SetLength(FOriginalToken, FTokenLength);
          Move(FText[CurrTokenPos], FOriginalToken[1], FTokenLength);
          FToken := FLastUpToken;
        end;
    else
      begin
        FOriginalToken := '';
        FToken := '';
      end;
    end;
    Break;
  until False;
end;

procedure TPSPascalParser.SetText(const Data: TbtString);
begin
  FData := Data;
  FText := Pointer(FData);
  FTokenLength := 0;
  FRealPosition := 0;
  FTokenId := CSTI_EOF;
  FLastEnterPos := 0;
  FRow := 1;
  Next;
end;

function TPSList.IndexOf(P: Pointer): Longint;
var
  i: Integer;
begin
  for i := FCount -1 downto 0 do
  begin
    if FData[i] = p then
    begin
      result := i;
      exit;
    end;
  end;
  result := -1;
end;

{ TPSUnitList }

function TPSUnitList.Add: TPSUnit;
begin
  result:=TPSUnit.Create(Self);

  fList.Add(result);
end;

constructor TPSUnitList.Create;
begin
  fList:=TPSList.Create;
end;

destructor TPSUnitList.Destroy;
var
  Dummy: Integer;
begin
  for Dummy:=0 to fList.Count-1 do
    TObject(fList[Dummy]).Free;

  FreeAndNil(fList);
  
  inherited;
end;

function TPSUnitList.GetUnit(UnitName: TbtString): TPSUnit;
var
  Dummy: Integer;
begin
  UnitName:=FastUpperCase(UnitName);
  for Dummy:=0 to fList.Count-1 do
  begin
    if TPSUnit(fList[Dummy]).UnitName=UnitName then
    begin
      result:=TPSUnit(fList[Dummy]);
      exit;
    end;
  end;

  result:=Add;

  result.UnitName:=UnitName;
end;

{ TPSUnit }

procedure TPSUnit.AddUses(pUnitName: TbtString);
var
  UsesUnit: TPSUnit;
begin
  UsesUnit:=fList.GetUnit(pUnitName);
  fUnits.Add(UsesUnit);
end;

constructor TPSUnit.Create(List: TPSUnitList);
begin
  fUnits:=TPSList.Create;

  fList:=List;
end;

destructor TPSUnit.Destroy;
begin
  FreeAndNIl(fUnits);
  inherited;
end;

function TPSUnit.HasUses(pUnitName: TbtString): Boolean;
var
  Dummy: Integer;
begin
  pUnitName:=FastUpperCase(pUnitName);

  if fUnitName=pUnitName then
  begin
    result:=true;
    exit;
  end;

  result:=false;

  for Dummy:=0 to fUnits.Count-1 do
  begin
    result:=TPSUnit(fUnits[Dummy]).HasUses(pUnitName);

    if result then
      exit;
  end;
end;

procedure TPSUnit.SetUnitName(const Value: TbtString);
begin
  fUnitName := FastUpperCase(Value);
end;


end.


