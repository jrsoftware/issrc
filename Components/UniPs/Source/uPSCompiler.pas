unit uPSCompiler;
{$I PascalScript.inc}
interface
uses
  {$IFNDEF DELPHI3UP}{$IFNDEF PS_NOINTERFACES}{$IFNDEF LINUX}Windows, Ole2,{$ENDIF}
  {$ENDIF}{$ENDIF}SysUtils, uPSUtils;


type
{$IFNDEF PS_NOINTERFACES}
  TPSInterface = class;
{$ENDIF}

  TPSParameterMode = (pmIn, pmOut, pmInOut);
  TPSPascalCompiler = class;
  TPSType = class;
  TPSValue = class;
  TPSParameters = class;

  TPSSubOptType = (tMainBegin, tProcBegin, tSubBegin, tOneLiner, tifOneliner, tRepeat, tTry, tTryEnd
    {$IFDEF PS_USESSUPPORT},tUnitInit, tUnitFinish {$ENDIF}); //nvds


  {TPSExternalClass is used when external classes need to be called}
  TPSCompileTimeClass = class;
  TPSAttributes = class;
  TPSAttribute = class;

  EPSCompilerException = class(Exception) end;

  TPSParameterDecl = class(TObject)
  private
    FName: tbtString;
    FOrgName: tbtString;
    FMode: TPSParameterMode;
    FType: TPSType;
    {$IFDEF PS_USESSUPPORT}
    FDeclareUnit: tbtString;
    {$ENDIF}
    FDeclarePos: Cardinal;
    FDeclareRow: Cardinal;
    FDeclareCol: Cardinal;
    procedure SetName(const s: tbtString);
  public

    property Name: tbtString read FName;

    property OrgName: tbtString read FOrgName write SetName;

    property aType: TPSType read FType write FType;

    property Mode: TPSParameterMode read FMode write FMode;

    {$IFDEF PS_USESSUPPORT}
    property DeclareUnit: tbtString read FDeclareUnit write FDeclareUnit;
    {$ENDIF}

    property DeclarePos: Cardinal read FDeclarePos write FDeclarePos;

    property DeclareRow: Cardinal read FDeclareRow write FDeclareRow;

    property DeclareCol: Cardinal read FDeclareCol write FDeclareCol;

  end;


  TPSParametersDecl = class(TObject)
  private
    FParams: TPSList;
    FResult: TPSType;
    function GetParam(I: Longint): TPSParameterDecl;
    function GetParamCount: Longint;
  public

    property Params[I: Longint]: TPSParameterDecl read GetParam;

    property ParamCount: Longint read GetParamCount;


    function AddParam: TPSParameterDecl;

    procedure DeleteParam(I: Longint);


    property Result : TPSType read FResult write FResult;


    procedure Assign(Params: TPSParametersDecl);


    function Same(d: TPSParametersDecl): boolean;


    constructor Create;

    destructor Destroy; override;
  end;


  TPSRegProc = class(TObject)
  private
    FNameHash: Longint;
    FName: tbtString;
    FDecl: TPSParametersDecl;
    FExportName: Boolean;
    FImportDecl: tbtString;
    FOrgName: tbtString;
    procedure SetName(const Value: tbtString);
  public

    property OrgName: tbtString read FOrgName write FOrgName;
	
    property Name: tbtString read FName write SetName;

    property NameHash: Longint read FNameHash;
	
    property Decl: TPSParametersDecl read FDecl;
	
    property ExportName: Boolean read FExportName write FExportName;
	
    property ImportDecl: tbtString read FImportDecl write FImportDecl;


    constructor Create;

    destructor Destroy; override;
  end;

  PIFPSRegProc = TPSRegProc;

  PIfRVariant = ^TIfRVariant;

  TIfRVariant = record

    FType: TPSType;
    case Byte of
      1: (tu8: TbtU8);
      2: (tS8: TbtS8);
      3: (tu16: TbtU16);
      4: (ts16: TbtS16);
      5: (tu32: TbtU32);
      6: (ts32: TbtS32);
      7: (tsingle: TbtSingle);
      8: (tdouble: TbtDouble);
      9: (textended: TbtExtended);
      11: (tcurrency: tbtCurrency);
      10: (tstring: Pointer);
      {$IFNDEF PS_NOINT64}
      17: (ts64: Tbts64);
      {$ENDIF}
      19: (tchar: tbtChar);
      {$IFNDEF PS_NOWIDESTRING}
      18: (twidestring: Pointer);
      20: (twidechar: tbtwidechar);
      {$ENDIF}
      21: (ttype: TPSType);
      22: (tunistring: Pointer);
  end;

  TPSRecordFieldTypeDef = class(TObject)
  private
    FFieldOrgName: tbtString;
    FFieldName: tbtString;
    FFieldNameHash: Longint;
    FType: TPSType;
    procedure SetFieldOrgName(const Value: tbtString);
  public

    property FieldOrgName: tbtString read FFieldOrgName write SetFieldOrgName;

    property FieldName: tbtString read FFieldName;

    property FieldNameHash: Longint read FFieldNameHash;

    property aType: TPSType read FType write FType;
  end;

  PIFPSRecordFieldTypeDef = TPSRecordFieldTypeDef;

  TPSType = class(TObject)
  private
    FNameHash: Longint;
    FName: tbtString;
    FBaseType: TPSBaseType;
    {$IFDEF PS_USESSUPPORT}
    FDeclareUnit: tbtString;
    {$ENDIF}
    FDeclarePos: Cardinal;
    FDeclareRow: Cardinal;
    FDeclareCol: Cardinal;
    FUsed: Boolean;
    FExportName: Boolean;
    FOriginalName: tbtString;
    FAttributes: TPSAttributes;
    FFinalTypeNo: cardinal;
    procedure SetName(const Value: tbtString);
  public

    constructor Create;

    destructor Destroy; override;

    property Attributes: TPSAttributes read FAttributes;


    property FinalTypeNo: cardinal read FFinalTypeNo;


    property OriginalName: tbtString read FOriginalName write FOriginalName;

    property Name: tbtString read FName write SetName;

    property NameHash: Longint read FNameHash;

    property BaseType: TPSBaseType read FBaseType write FBaseType;

    {$IFDEF PS_USESSUPPORT}
    property DeclareUnit: tbtString read FDeclareUnit write FDeclareUnit;
    {$ENDIF}

    property DeclarePos: Cardinal read FDeclarePos write FDeclarePos;

    property DeclareRow: Cardinal read FDeclareRow write FDeclareRow;

    property DeclareCol: Cardinal read FDeclareCol write FDeclareCol;

    property Used: Boolean read FUsed;

    property ExportName: Boolean read FExportName write FExportName;

    procedure Use;
  end;


  PIFPSType = TPSType;

  TPSVariantType = class(TPSType)
  private
  public
    function GetDynInvokeProcNo(Owner: TPSPascalCompiler; const Name: tbtString; Params: TPSParameters): Cardinal; virtual;
    function GetDynIvokeSelfType(Owner: TPSPascalCompiler): TPSType; virtual;
    function GetDynInvokeParamType(Owner: TPSPascalCompiler): TPSType; virtual;
    function GetDynIvokeResulType(Owner: TPSPascalCompiler): TPSType; virtual;
  end;


  TPSRecordType = class(TPSType)
  private
    FRecordSubVals: TPSList;
  public

    constructor Create;

    destructor Destroy; override;

    function RecValCount: Longint;

    function RecVal(I: Longint): PIFPSRecordFieldTypeDef;

    function AddRecVal: PIFPSRecordFieldTypeDef;
  end;

  TPSClassType = class(TPSType)
  private
    FCL: TPSCompiletimeClass;
  public

    property Cl: TPSCompileTimeClass read FCL write FCL;
  end;
  TPSExternalClass = class;
  TPSUndefinedClassType = class(TPSType)
  private
    FExtClass: TPSExternalClass;
  public
    property ExtClass: TPSExternalClass read FExtClass write FExtClass;
  end;
{$IFNDEF PS_NOINTERFACES}

  TPSInterfaceType = class(TPSType)
  private
    FIntf: TPSInterface;
  public

    property Intf: TPSInterface read FIntf write FIntf;
  end;
{$ENDIF}


  TPSProceduralType = class(TPSType)
  private
    FProcDef: TPSParametersDecl;
  public

    property ProcDef: TPSParametersDecl read FProcDef;

    constructor Create;

    destructor Destroy; override;
  end;

  TPSArrayType = class(TPSType)
  private
    FArrayTypeNo: TPSType;
  public

    property ArrayTypeNo: TPSType read FArrayTypeNo write FArrayTypeNo;
  end;

  TPSStaticArrayType = class(TPSArrayType)
  private
    FStartOffset: Longint;
    FLength: Cardinal;
  public

    property StartOffset: Longint read FStartOffset write FStartOffset;

    property Length: Cardinal read FLength write FLength;
  end;

  TPSSetType = class(TPSType)
  private
    FSetType: TPSType;
    function GetByteSize: Longint;
    function GetBitSize: Longint;
  public

    property SetType: TPSType read FSetType write FSetType;

    property ByteSize: Longint read GetByteSize;

    property BitSize: Longint read GetBitSize;
  end;

  TPSTypeLink = class(TPSType)
  private
    FLinkTypeNo: TPSType;
  public

    property LinkTypeNo: TPSType read FLinkTypeNo write FLinkTypeNo;
  end;

  TPSEnumType = class(TPSType)
  private
    FHighValue: Cardinal;
  public

    property HighValue: Cardinal read FHighValue write FHighValue;
  end;


  TPSProcedure = class(TObject)
  private
    FAttributes: TPSAttributes;
  public

    property Attributes: TPSAttributes read FAttributes;


    constructor Create;

    destructor Destroy; override;
  end;

  TPSAttributeType = class;

  TPSAttributeTypeField = class(TObject)
  private
    FOwner: TPSAttributeType;
    FFieldOrgName: tbtString;
    FFieldName: tbtString;
    FFieldNameHash: Longint;
    FFieldType: TPSType;
    FHidden: Boolean;
    procedure SetFieldOrgName(const Value: tbtString);
  public

    constructor Create(AOwner: TPSAttributeType);

    property Owner: TPSAttributeType read FOwner;

    property FieldOrgName: tbtString read FFieldOrgName write SetFieldOrgName;

    property FieldName: tbtString read FFieldName;

    property FieldNameHash: Longint read FFieldNameHash;

    property FieldType: TPSType read FFieldType write FFieldType;

    property Hidden: Boolean read FHidden write FHidden;
  end;

  TPSApplyAttributeToType = function (Sender: TPSPascalCompiler; aType: TPSType; Attr: TPSAttribute): Boolean;

  TPSApplyAttributeToProc = function (Sender: TPSPascalCompiler; aProc: TPSProcedure; Attr: TPSAttribute): Boolean;
  { An attribute type }
  TPSAttributeType = class(TPSType)
  private
    FFields: TPSList;
    FName: tbtString;
    FOrgname: tbtString;
    FNameHash: Longint;
    FAAProc: TPSApplyAttributeToProc;
    FAAType: TPSApplyAttributeToType;
    function GetField(I: Longint): TPSAttributeTypeField;
    function GetFieldCount: Longint;
    procedure SetName(const s: tbtString);
  public

    property OnApplyAttributeToType: TPSApplyAttributeToType read FAAType write FAAType;

    property OnApplyAttributeToProc: TPSApplyAttributeToProc read FAAProc write FAAProc;

    property Fields[i: Longint]: TPSAttributeTypeField read GetField;

    property FieldCount: Longint read GetFieldCount;

    procedure DeleteField(I: Longint);

    function AddField: TPSAttributeTypeField;

    property Name: tbtString read FName;

    property OrgName: tbtString read FOrgName write SetName;

    property NameHash: Longint read FNameHash;

    constructor Create;

    destructor Destroy; override;
  end;

  TPSAttribute = class(TObject)
  private
    FAttribType: TPSAttributeType;
    FValues: TPSList;
    {$IFDEF PS_USESSUPPORT}
    FDeclareUnit: tbtString;
    {$ENDIF}
    FDeclarePos: Cardinal;
    FDeclareRow: Cardinal;
    FDeclareCol: Cardinal;
    function GetValueCount: Longint;
    function GetValue(I: Longint): PIfRVariant;
  public

    constructor Create(AttribType: TPSAttributeType);

    procedure Assign(Item: TPSAttribute);

    property AType: TPSAttributeType read FAttribType;

    property Count: Longint read GetValueCount;

    property Values[i: Longint]: PIfRVariant read GetValue; default;

    {$IFDEF PS_USESSUPPORT}
    property DeclareUnit: tbtString read FDeclareUnit write FDeclareUnit;
    {$ENDIF}

    property DeclarePos: Cardinal read FDeclarePos write FDeclarePos;

    property DeclareRow: Cardinal read FDeclareRow write FDeclareRow;

    property DeclareCol: Cardinal read FDeclareCol write FDeclareCol;

    procedure DeleteValue(i: Longint);

    function AddValue(v: PIFRVariant): Longint;

    destructor Destroy; override;
  end;


  TPSAttributes = class(TObject)
  private
    FItems: TPSList;
    function GetCount: Longint;
    function GetItem(I: Longint): TPSAttribute;
  public

    procedure Assign(attr: TPSAttributes; Move: Boolean);

    property Count: Longint read GetCount;

    property Items[i: Longint]: TPSAttribute read GetItem; default;

    procedure Delete(i: Longint);

    function Add(AttribType: TPSAttributeType): TPSAttribute;

    function FindAttribute(const Name: tbtString): TPSAttribute;

    constructor Create;

    destructor Destroy; override;
  end;


  TPSProcVar = class(TObject)
  private
    FNameHash: Longint;
    FName: tbtString;
    FOrgName: tbtString;
    FType: TPSType;
    FUsed: Boolean;
    {$IFDEF PS_USESSUPPORT}
    FDeclareUnit: tbtString;
    {$ENDIF}
    FDeclarePos, FDeclareRow, FDeclareCol: Cardinal;
    procedure SetName(const Value: tbtString);
  public

    property OrgName: tbtString read FOrgName write FOrgname;

    property NameHash: Longint read FNameHash;

    property Name: tbtString read FName write SetName;

    property AType: TPSType read FType write FType;

    property Used: Boolean read FUsed;

    {$IFDEF PS_USESSUPPORT}
    property DeclareUnit: tbtString read FDeclareUnit write FDeclareUnit;
    {$ENDIF}

    property DeclarePos: Cardinal read FDeclarePos write FDeclarePos;

    property DeclareRow: Cardinal read FDeclareRow write FDeclareRow;

    property DeclareCol: Cardinal read FDeclareCol write FDeclareCol;

    procedure Use;
  end;

  PIFPSProcVar = TPSProcVar;

  TPSExternalProcedure = class(TPSProcedure)
  private
    FRegProc: TPSRegProc;
  public

    property RegProc: TPSRegProc read FRegProc write FRegProc;
  end;


  TPSInternalProcedure = class(TPSProcedure)
  private
    FForwarded: Boolean;
    FData: tbtString;
    FNameHash: Longint;
    FName: tbtString;
    FDecl: TPSParametersDecl;
    FProcVars: TPSList;
    FUsed: Boolean;
    FOutputDeclPosition: Cardinal;
    FResultUsed: Boolean;
    FLabels: TIfStringList;
    FGotos: TIfStringList;
    FDeclareRow: Cardinal;
    {$IFDEF PS_USESSUPPORT}
    FDeclareUnit: tbtString;
    {$ENDIF}
    FDeclarePos: Cardinal;
    FDeclareCol: Cardinal;
    FOriginalName: tbtString;
    procedure SetName(const Value: tbtString);
  public

    constructor Create;

    destructor Destroy; override;
    {Attributes}


    property Forwarded: Boolean read FForwarded write FForwarded;

    property Data: tbtString read FData write FData;

    property Decl: TPSParametersDecl read FDecl;

    property OriginalName: tbtString read FOriginalName write FOriginalName;

    property Name: tbtString read FName write SetName;

    property NameHash: Longint read FNameHash;

    property ProcVars: TPSList read FProcVars;

    property Used: Boolean read FUsed;

    {$IFDEF PS_USESSUPPORT}
    property DeclareUnit: tbtString read FDeclareUnit write FDeclareUnit;
    {$ENDIF}

    property DeclarePos: Cardinal read FDeclarePos write FDeclarePos;

    property DeclareRow: Cardinal read FDeclareRow write FDeclareRow;

    property DeclareCol: Cardinal read FDeclareCol write FDeclareCol;

    property OutputDeclPosition: Cardinal read FOutputDeclPosition write FOutputDeclPosition;

    property ResultUsed: Boolean read FResultUsed;


    property Labels: TIfStringList read FLabels;

    property Gotos: TIfStringList read FGotos;

    procedure Use;

    procedure ResultUse;
  end;

  TPSVar = class(TObject)
  private
    FNameHash: Longint;
    FOrgName: tbtString;
    FName: tbtString;
    FType: TPSType;
    FUsed: Boolean;
    FExportName: tbtString;
    FDeclareRow: Cardinal;
    {$IFDEF PS_USESSUPPORT}
    FDeclareUnit: tbtString;
    {$ENDIF}
    FDeclarePos: Cardinal;
    FDeclareCol: Cardinal;
    FSaveAsPointer: Boolean;
    procedure SetName(const Value: tbtString);
  public

    property SaveAsPointer: Boolean read FSaveAsPointer write FSaveAsPointer;

    property ExportName: tbtString read FExportName write FExportName;

    property Used: Boolean read FUsed;

    property aType: TPSType read FType write FType;

    property OrgName: tbtString read FOrgName write FOrgName;

    property Name: tbtString read FName write SetName;

    property NameHash: Longint read FNameHash;

    {$IFDEF PS_USESSUPPORT}
    property DeclareUnit: tbtString read FDeclareUnit write FDeclareUnit;
    {$ENDIF}

    property DeclarePos: Cardinal read FDeclarePos write FDeclarePos;

    property DeclareRow: Cardinal read FDeclareRow write FDeclareRow;

    property DeclareCol: Cardinal read FDeclareCol write FDeclareCol;

    procedure Use;
  end;

  PIFPSVar = TPSVar;

  TPSConstant = class(TObject)
  private

    FOrgName: tbtString;

    FNameHash: Longint;

    FName: tbtString;

    FDeclareRow: Cardinal;
    {$IFDEF PS_USESSUPPORT}
    FDeclareUnit: tbtString;
    {$ENDIF}
    FDeclarePos: Cardinal;
    FDeclareCol: Cardinal;

    FValue: PIfRVariant;
    procedure SetName(const Value: tbtString);
  public

    property OrgName: tbtString read FOrgName write FOrgName;

    property Name: tbtString read FName write SetName;

    property NameHash: Longint read FNameHash;

    property Value: PIfRVariant read FValue write FValue;

    {$IFDEF PS_USESSUPPORT}
    property DeclareUnit: tbtString read FDeclareUnit write FDeclareUnit;
    {$ENDIF}

    property DeclarePos: Cardinal read FDeclarePos write FDeclarePos;

    property DeclareRow: Cardinal read FDeclareRow write FDeclareRow;

    property DeclareCol: Cardinal read FDeclareCol write FDeclareCol;


    procedure SetSet(const val);


    procedure SetInt(const Val: Longint);

    procedure SetUInt(const Val: Cardinal);
    {$IFNDEF PS_NOINT64}

    procedure SetInt64(const Val: Int64);
    {$ENDIF}

    procedure SetString(const Val: tbtString);

    procedure SetChar(c: tbtChar);
    {$IFNDEF PS_NOWIDESTRING}

    procedure SetWideChar(const val: WideChar);

    procedure SetWideString(const val: tbtwidestring);
    procedure SetUnicodeString(const val: tbtunicodestring);
    {$ENDIF}

    procedure SetExtended(const Val: Extended);


    destructor Destroy; override;
  end;

  PIFPSConstant = TPSConstant;

  TPSPascalCompilerErrorType = (
    ecUnknownIdentifier,
    ecIdentifierExpected,
    ecCommentError,
    ecStringError,
    ecCharError,
    ecSyntaxError,
    ecUnexpectedEndOfFile,
    ecSemicolonExpected,
    ecBeginExpected,
    ecPeriodExpected,
    ecDuplicateIdentifier,
    ecColonExpected,
    ecUnknownType,
    ecCloseRoundExpected,
    ecTypeMismatch,
    ecInternalError,
    ecAssignmentExpected,
    ecThenExpected,
    ecDoExpected,
    ecNoResult,
    ecOpenRoundExpected,
    ecCommaExpected,
    ecToExpected,
    ecIsExpected,
    ecOfExpected,
    ecCloseBlockExpected,
    ecVariableExpected,
    ecStringExpected,
    ecEndExpected,
    ecUnSetLabel,
    ecNotInLoop,
    ecInvalidJump,
    ecOpenBlockExpected,
    ecWriteOnlyProperty,
    ecReadOnlyProperty,
    ecClassTypeExpected,
    ecCustomError,
    ecDivideByZero,
    ecMathError,
    ecUnsatisfiedForward,
    ecForwardParameterMismatch,
    ecInvalidnumberOfParameters
    {$IFDEF PS_USESSUPPORT}
    , ecNotAllowed,
    ecUnitNotFoundOrContainsErrors,
    ecCrossReference
    {$ENDIF}
    , ecUnClosedAttributes
    );

  TPSPascalCompilerHintType = (
    ehVariableNotUsed,
    ehFunctionNotUsed,
    ehCustomHint
    );

  TPSPascalCompilerWarningType = (
    ewCalculationAlwaysEvaluatesTo,
    ewIsNotNeeded,
    ewAbstractClass,
    ewCustomWarning
  );

  TPSPascalCompilerMessage = class(TObject)
  protected

    FRow: Cardinal;

    FCol: Cardinal;

    FModuleName: tbtString;

    FParam: tbtString;

    FPosition: Cardinal;

    procedure SetParserPos(Parser: TPSPascalParser);
  public

    property ModuleName: tbtString read FModuleName write FModuleName;

    property Param: tbtString read FParam write FParam;

    property Pos: Cardinal read FPosition write FPosition;

    property Row: Cardinal read FRow write FRow;

    property Col: Cardinal read FCol write FCol;

    function ErrorType: tbtString; virtual; abstract;

    procedure SetCustomPos(Pos, Row, Col: Cardinal);

    function MessageToString: tbtString; virtual;

    function ShortMessageToString: tbtString; virtual; abstract;
  end;

  TPSPascalCompilerError = class(TPSPascalCompilerMessage)
  protected

    FError: TPSPascalCompilerErrorType;
  public

    property Error: TPSPascalCompilerErrorType read FError;

    function ErrorType: tbtString; override;
    function ShortMessageToString: tbtString; override;
  end;

  TPSPascalCompilerHint = class(TPSPascalCompilerMessage)
  protected

    FHint: TPSPascalCompilerHintType;
  public

    property Hint: TPSPascalCompilerHintType read FHint;

    function ErrorType: tbtString; override;
    function ShortMessageToString: tbtString; override;
  end;

  TPSPascalCompilerWarning = class(TPSPascalCompilerMessage)
  protected

    FWarning: TPSPascalCompilerWarningType;
  public

    property Warning: TPSPascalCompilerWarningType read FWarning;

    function ErrorType: tbtString; override;
    function ShortMessageToString: tbtString; override;
  end;
  TPSDuplicCheck = set of (dcTypes, dcProcs, dcVars, dcConsts);

  TPSBlockInfo = class(TObject)
  private
    FOwner: TPSBlockInfo;
    FWithList: TPSList;
    FProcNo: Cardinal;
    FProc: TPSInternalProcedure;
    FSubType: TPSSubOptType;
  public

    property WithList: TPSList read FWithList;

    property ProcNo: Cardinal read FProcNo write FProcNo;

    property Proc: TPSInternalProcedure read FProc write FProc;

    property SubType: TPSSubOptType read FSubType write FSubType;

    procedure Clear;

    constructor Create(Owner: TPSBlockInfo);

    destructor Destroy; override;
  end;



  TPSBinOperatorType = (otAdd, otSub, otMul, otDiv, otMod, otShl, otShr, otAnd, otOr, otXor, otAs, otIntDiv,
                          otGreaterEqual, otLessEqual, otGreater, otLess, otEqual,
                          otNotEqual, otIs, otIn);

  TPSUnOperatorType = (otNot, otMinus, otCast);

  TPSOnUseVariable = procedure (Sender: TPSPascalCompiler; VarType: TPSVariableType; VarNo: Longint; ProcNo, Position: Cardinal; const PropData: tbtString);
  
  TPSOnUseRegProc = procedure (Sender: TPSPascalCompiler; Position: Cardinal; const Name: tbtString);
  
  TPSOnUses = function(Sender: TPSPascalCompiler; const Name: tbtString): Boolean;

  TPSOnExportCheck = function(Sender: TPSPascalCompiler; Proc: TPSInternalProcedure; const ProcDecl: tbtString): Boolean;

  {$IFNDEF PS_USESSUPPORT}
  TPSOnWriteLineEvent = function (Sender: TPSPascalCompiler; Position: Cardinal): Boolean;
  TPSOnWriteLine2Event = function (Sender: TPSPascalCompiler; Position: Cardinal; IsProcExit: Boolean): Boolean;
  {$ELSE}
  TPSOnWriteLineEvent = function (Sender: TPSPascalCompiler; FileName: tbtString; Position: Cardinal): Boolean;
  TPSOnWriteLine2Event = function (Sender: TPSPascalCompiler; FileName: tbtString; Position: Cardinal; IsProcExit: Boolean): Boolean;
  {$ENDIF}

  TPSOnExternalProc = function (Sender: TPSPascalCompiler; Decl: TPSParametersDecl; const Name, FExternal: tbtString): TPSRegProc;

  TPSOnTranslateLineInfoProc = procedure (Sender: TPSPascalCompiler; var Pos, Row, Col: Cardinal; var Name: tbtString);
  TPSOnNotify = function (Sender: TPSPascalCompiler): Boolean;

  TPSOnFunction = procedure(name: tbtString; Pos, Row, Col: Integer) of object;


  TPSPascalCompiler = class
  protected
    FAnyString: TPSType;
    FAnyMethod: TPSType;
    FUnitName: tbtString;
    FID: Pointer;
    FOnExportCheck: TPSOnExportCheck;
    FDefaultBoolType: TPSType;
    FRegProcs: TPSList;
    FConstants: TPSList;
    FProcs: TPSList;
    FTypes: TPSList;
    FAttributeTypes: TPSList;
    FVars: TPSList;
    FOutput: tbtString;
    FParser: TPSPascalParser;
    FParserHadError: Boolean;
    FMessages: TPSList;
    FOnUses: TPSOnUses;
    FUtf8Decode: Boolean;
    FIsUnit: Boolean;
    FAllowNoBegin: Boolean;
    FAllowNoEnd: Boolean;
    FAllowUnit: Boolean;
    FAllowDuplicateRegister : Boolean;
    FBooleanShortCircuit: Boolean;
    FDebugOutput: tbtString;
    FOnExternalProc: TPSOnExternalProc;
    FOnUseVariable: TPSOnUseVariable;
    FOnUseRegProc: TPSOnUseRegProc;
    FOnBeforeOutput: TPSOnNotify;
    FOnBeforeCleanup: TPSOnNotify;
    FOnWriteLine: TPSOnWriteLineEvent;
    FOnWriteLine2: TPSOnWriteLine2Event;
    FContinueOffsets, FBreakOffsets: TPSList;
    FOnTranslateLineInfo: TPSOnTranslateLineInfoProc;
    FAutoFreeList: TPSList;
    FClasses: TPSList;
    FOnFunctionStart: TPSOnFunction;
    FOnFunctionEnd: TPSOnFunction;
    FAttributesOpenTokenID, FAttributesCloseTokenID: TPsPasToken;

		FWithCount: Integer;
		FTryCount: Integer;
    FExceptFinallyCount: Integer;


    {$IFDEF PS_USESSUPPORT}
    FUnitInits : TPSList; //nvds
    FUnitFinits: TPSList; //nvds
    FUses      : TPSStringList;
    fUnits     : TPSUnitList;
    fUnit      : TPSUnit;
    fModule    : tbtString;
    {$ENDIF}
    fInCompile : Integer;
{$IFNDEF PS_NOINTERFACES}
    FInterfaces: TPSList;
{$ENDIF}

    FCurrUsedTypeNo: Cardinal;
    FGlobalBlock: TPSBlockInfo;

    function IsBoolean(aType: TPSType): Boolean;
    {$IFNDEF PS_NOWIDESTRING}

    function GetWideString(Src: PIfRVariant; var s: Boolean): tbtwidestring;
    function GetUnicodeString(Src: PIfRVariant; var s: Boolean): tbtunicodestring;
    {$ENDIF}
    function PreCalc(FUseUsedTypes: Boolean; Var1Mod: Byte; var1: PIFRVariant; Var2Mod: Byte;
      Var2: PIfRVariant; Cmd: TPSBinOperatorType; Pos, Row, Col: Cardinal): Boolean;

    function FindBaseType(BaseType: TPSBaseType): TPSType;

    function IsIntBoolType(aType: TPSType): Boolean;
    function GetTypeCopyLink(p: TPSType): TPSType;

    function at2ut(p: TPSType): TPSType;
    procedure UseProc(procdecl: TPSParametersDecl);


    function GetMsgCount: Longint;

    function GetMsg(l: Longint): TPSPascalCompilerMessage;


    function MakeExportDecl(decl: TPSParametersDecl): tbtString;


    procedure DefineStandardTypes;

    procedure DefineStandardProcedures;

    function ReadReal(const s: tbtString): PIfRVariant;
    function ReadString: PIfRVariant;
    function ReadInteger(const s: tbtString): PIfRVariant;
    function ReadAttributes(Dest: TPSAttributes): Boolean;
    function ReadConstant(FParser: TPSPascalParser; StopOn: TPSPasToken): PIfRVariant;

    function ApplyAttribsToFunction(func: TPSProcedure): boolean;
    function ProcessFunction(AlwaysForward: Boolean; Att: TPSAttributes): Boolean;
    function ValidateParameters(BlockInfo: TPSBlockInfo; Params: TPSParameters; ParamTypes: TPSParametersDecl): boolean;

    function IsVarInCompatible(ft1, ft2: TPSType): Boolean;
    function GetTypeNo(BlockInfo: TPSBlockInfo; p: TPSValue): TPSType;
    function DoVarBlock(proc: TPSInternalProcedure): Boolean;
    function DoTypeBlock(FParser: TPSPascalParser): Boolean;
    function ReadType(const Name: tbtString; FParser: TPSPascalParser): TPSType;
    function ProcessLabel(Proc: TPSInternalProcedure): Boolean;
    function ProcessSub(BlockInfo: TPSBlockInfo): Boolean;
    function ProcessLabelForwards(Proc: TPSInternalProcedure): Boolean;

    procedure WriteDebugData(const s: tbtString);

    procedure Debug_SavePosition(ProcNo: Cardinal; Proc: TPSInternalProcedure);

    procedure Debug_WriteParams(ProcNo: Cardinal; Proc: TPSInternalProcedure);

    procedure Debug_WriteLine(BlockInfo: TPSBlockInfo);
    procedure Debug_WriteLine2(BlockInfo: TPSBlockInfo; IsProcExit: Boolean);

    function IsCompatibleType(p1, p2: TPSType; Cast: Boolean): Boolean;

    function IsDuplicate(const s: tbtString; const check: TPSDuplicCheck): Boolean;
    {$IFDEF PS_USESSUPPORT}
    function IsInLocalUnitList(s: tbtString): Boolean;
	{$ENDIF}

    function NewProc(const OriginalName, Name: tbtString): TPSInternalProcedure;

    function AddUsedFunction(var Proc: TPSInternalProcedure): Cardinal;

    function AddUsedFunction2(var Proc: TPSExternalProcedure): Cardinal;


    function CheckCompatProc(P: TPSType; ProcNo: Cardinal): Boolean;


    procedure ParserError(Parser: TObject; Kind: TPSParserErrorKind);

    function ReadTypeAddProcedure(const Name: tbtString; FParser: TPSPascalParser): TPSType;

    function VarIsDuplicate(Proc: TPSInternalProcedure; const VarNames, s: tbtString): Boolean;

    function IsProcDuplicLabel(Proc: TPSInternalProcedure; const s: tbtString): Boolean;

    procedure CheckForUnusedVars(Func: TPSInternalProcedure);
    function ProcIsDuplic(Decl: TPSParametersDecl; const FunctionName, FunctionParamNames: tbtString; const s: tbtString; Func: TPSInternalProcedure): Boolean;
   public
     function GetConstant(const Name: tbtString): TPSConstant;

     function UseExternalProc(const Name: tbtString): TPSParametersDecl;

    function FindProc(const aName: tbtString): Cardinal;

    function GetTypeCount: Longint;

    function GetType(I: Longint): TPSType;

    function GetVarCount: Longint;

    function GetVar(I: Longint): TPSVar;

    function GetProcCount: Longint;

    function GetProc(I: Longint): TPSProcedure;

    function GetConstCount: Longint;

    function GetConst(I: Longint): TPSConstant;

    function GetRegProcCount: Longint;

    function GetRegProc(I: Longint): TPSRegProc;

    function AddAttributeType: TPSAttributeType;
    function FindAttributeType(const Name: tbtString): TPSAttributeType;

    procedure AddToFreeList(Obj: TObject);

    property ID: Pointer read FID write FID;

    function MakeError(const Module: tbtString; E: TPSPascalCompilerErrorType; const
      Param: tbtString): TPSPascalCompilerMessage;

    function MakeWarning(const Module: tbtString; E: TPSPascalCompilerWarningType;
      const Param: tbtString): TPSPascalCompilerMessage;

    function MakeHint(const Module: tbtString; E: TPSPascalCompilerHintType;
      const Param: tbtString): TPSPascalCompilerMessage;

{$IFNDEF PS_NOINTERFACES}

    function AddInterface(InheritedFrom: TPSInterface; Guid: TGuid; const Name: tbtString): TPSInterface;

    function FindInterface(const Name: tbtString): TPSInterface;

{$ENDIF}
    function AddClass(InheritsFrom: TPSCompileTimeClass; aClass: TClass): TPSCompileTimeClass;

    function AddClassN(InheritsFrom: TPSCompileTimeClass; const aClass: tbtString): TPSCompileTimeClass;


    function FindClass(const aClass: tbtString): TPSCompileTimeClass;

    function AddFunction(const Header: tbtString): TPSRegProc;

    function AddDelphiFunction(const Decl: tbtString): TPSRegProc;

    function AddType(const Name: tbtString; const BaseType: TPSBaseType): TPSType;

    function AddTypeS(const Name, Decl: tbtString): TPSType;

    function AddTypeCopy(const Name: tbtString; TypeNo: TPSType): TPSType;

    function AddTypeCopyN(const Name, FType: tbtString): TPSType;

    function AddConstant(const Name: tbtString; FType: TPSType): TPSConstant;

    function AddConstantN(const Name, FType: tbtString): TPSConstant;

    function AddVariable(const Name: tbtString; FType: TPSType): TPSVar;

    function AddVariableN(const Name, FType: tbtString): TPSVar;

    function AddUsedVariable(const Name: tbtString; FType: TPSType): TPSVar;

    function AddUsedVariableN(const Name, FType: tbtString): TPSVar;

    function AddUsedPtrVariable(const Name: tbtString; FType: TPSType): TPSVar;

    function AddUsedPtrVariableN(const Name, FType: tbtString): TPSVar;

    function FindType(const Name: tbtString): TPSType;

    function MakeDecl(decl: TPSParametersDecl): tbtString;

    function Compile(const s: tbtString): Boolean;

    function GetOutput(var s: tbtString): Boolean;
	
    function GetDebugOutput(var s: tbtString): Boolean;

    procedure Clear;

    constructor Create;
	
    destructor Destroy; override;

    property MsgCount: Longint read GetMsgCount;
	
    property Msg[l: Longint]: TPSPascalCompilerMessage read GetMsg;

    property OnTranslateLineInfo: TPSOnTranslateLineInfoProc read FOnTranslateLineInfo write FOnTranslateLineInfo;

    property OnUses: TPSOnUses read FOnUses write FOnUses;
	
    property OnExportCheck: TPSOnExportCheck read FOnExportCheck write FOnExportCheck;
	
    property OnWriteLine: TPSOnWriteLineEvent read FOnWriteLine write FOnWriteLine;

    property OnWriteLine2: TPSOnWriteLine2Event read FOnWriteLine2 write FOnWriteLine2;

    property OnExternalProc: TPSOnExternalProc read FOnExternalProc write FOnExternalProc;
	
    property OnUseVariable: TPSOnUseVariable read FOnUseVariable write FOnUseVariable;

    property OnUseRegProc: TPSOnUseRegProc read FOnUseRegProc write FOnUseRegProc;

    property OnBeforeOutput: TPSOnNotify read FOnBeforeOutput write FOnBeforeOutput;

    property OnBeforeCleanup: TPSOnNotify read FOnBeforeCleanup write FOnBeforeCleanup;

    property OnFunctionStart: TPSOnFunction read FOnFunctionStart write FOnFunctionStart;

    property OnFunctionEnd: TPSOnFunction read FOnFunctionEnd write FOnFunctionEnd;
	
    property IsUnit: Boolean read FIsUnit;
	
    property AllowNoBegin: Boolean read FAllowNoBegin write FAllowNoBegin;
	
    property AllowUnit: Boolean read FAllowUnit write FAllowUnit;
	
    property AllowNoEnd: Boolean read FAllowNoEnd write FAllowNoEnd;

    property AllowDuplicateRegister : Boolean read FAllowDuplicateRegister write FAllowDuplicateRegister;

    property BooleanShortCircuit: Boolean read FBooleanShortCircuit write FBooleanShortCircuit;

    property UTF8Decode: Boolean read FUtf8Decode write FUtf8Decode;

    property AttributesOpenTokenID: TPSPasToken read FAttributesOpenTokenID write FAttributesOpenTokenID;

    property AttributesCloseTokenID: TPSPasToken read FAttributesCloseTokenID write FAttributesCloseTokenID;

    {$WARNINGS OFF}
    property UnitName: tbtString read FUnitName;
    {$WARNINGS ON}
  end;
  TIFPSPascalCompiler = TPSPascalCompiler;

  TPSValue = class(TObject)
  private
    FPos, FRow, FCol: Cardinal;
  public

    property Pos: Cardinal read FPos write FPos;

    property Row: Cardinal read FRow write FRow;

    property Col: Cardinal read FCol write FCol;

    procedure SetParserPos(P: TPSPascalParser);

  end;

  TPSParameter = class(TObject)
  private
    FValue: TPSValue;
    FTempVar: TPSValue;
    FParamMode: TPSParameterMode;
    FExpectedType: TPSType;
  public

    property Val: TPSValue read FValue write FValue;

    property ExpectedType: TPSType read FExpectedType write FExpectedType;

    property TempVar: TPSValue read FTempVar write FTempVar;

    property ParamMode: TPSParameterMode read FParamMode write FParamMode;

    destructor Destroy; override;
  end;

  TPSParameters = class(TObject)
  private
    FItems: TPSList;
    function GetCount: Cardinal;
    function GetItem(I: Longint): TPSParameter;
  public

    constructor Create;

    destructor Destroy; override;

    property Count: Cardinal read GetCount;

    property Item[I: Longint]: TPSParameter read GetItem; default;

    procedure Delete(I: Cardinal);

    function Add: TPSParameter;
  end;

  TPSSubItem = class(TObject)
  private
    FType: TPSType;
  public

    property aType: TPSType read FType write FType;
  end;

  TPSSubNumber = class(TPSSubItem)
  private
    FSubNo: Cardinal;
  public

    property SubNo: Cardinal read FSubNo write FSubNo;
  end;

  TPSSubValue = class(TPSSubItem)
  private
    FSubNo: TPSValue;
  public

    property SubNo: TPSValue read FSubNo write FSubNo;

    destructor Destroy; override;
  end;

  TPSValueVar = class(TPSValue)
  private
    FRecItems: TPSList;
    function GetRecCount: Cardinal;
    function GetRecItem(I: Cardinal): TPSSubItem;
  public
    constructor Create;
    destructor Destroy; override;

    function RecAdd(Val: TPSSubItem): Cardinal;

    procedure RecDelete(I: Cardinal);

    property RecItem[I: Cardinal]: TPSSubItem read GetRecItem;

    property RecCount: Cardinal read GetRecCount;
  end;

  TPSValueGlobalVar = class(TPSValueVar)
  private
    FAddress: Cardinal;
  public

    property GlobalVarNo: Cardinal read FAddress write FAddress;
  end;


  TPSValueLocalVar = class(TPSValueVar)
  private
    FLocalVarNo: Longint;
  public

    property LocalVarNo: Longint read FLocalVarNo write FLocalVarNo;
  end;

  TPSValueParamVar = class(TPSValueVar)
  private
    FParamNo: Longint;
  public

    property ParamNo: Longint read FParamNo write FParamNo;
  end;

  TPSValueAllocatedStackVar = class(TPSValueLocalVar)
  private
    FProc: TPSInternalProcedure;
  public

    property Proc: TPSInternalProcedure read FProc write FProc;
    destructor Destroy; override;
  end;

  TPSValueData = class(TPSValue)
  private
    FData: PIfRVariant;
  public

    property Data: PIfRVariant read FData write FData;
    destructor Destroy; override;
  end;

  TPSValueReplace = class(TPSValue)
  private
    FPreWriteAllocated: Boolean;
    FFreeOldValue: Boolean;
    FFreeNewValue: Boolean;
    FOldValue: TPSValue;
    FNewValue: TPSValue;
    FReplaceTimes: Longint;
  public

    property OldValue: TPSValue read FOldValue write FOldValue;

    property NewValue: TPSValue read FNewValue write FNewValue;
    {Should it free the old value when destroyed?}
    property FreeOldValue: Boolean read FFreeOldValue write FFreeOldValue;
    property FreeNewValue: Boolean read FFreeNewValue write FFreeNewValue;
    property PreWriteAllocated: Boolean read FPreWriteAllocated write FPreWriteAllocated;

    property ReplaceTimes: Longint read FReplaceTimes write FReplaceTimes;

    constructor Create;
    destructor Destroy; override;
  end;


  TPSUnValueOp = class(TPSValue)
  private
    FVal1: TPSValue;
    FOperator: TPSUnOperatorType;
    FType: TPSType;
  public

    property Val1: TPSValue read FVal1 write FVal1;
    {The operator}
    property Operator: TPSUnOperatorType read FOperator write FOperator;

    property aType: TPSType read FType write FType;
    destructor Destroy; override;
  end;

  TPSBinValueOp = class(TPSValue)
  private
    FVal1,
    FVal2: TPSValue;
    FOperator: TPSBinOperatorType;
    FType: TPSType;
  public

    property Val1: TPSValue read FVal1 write FVal1;

    property Val2: TPSValue read FVal2 write FVal2;
    {The operator for this value}
    property Operator: TPSBinOperatorType read FOperator write FOperator;

    property aType: TPSType read FType write FType;

    destructor Destroy; override;
  end;

  TPSValueNil = class(TPSValue)
  end;

  TPSValueProcPtr = class(TPSValue)
  private
    FProcNo: Cardinal;
  public

    property ProcPtr: Cardinal read FProcNo write FProcNo;
  end;

  TPSValueProc = class(TPSValue)
  private
    FSelfPtr: TPSValue;
    FParameters: TPSParameters;
    FResultType: TPSType;
  public
    property ResultType: TPSType read FResultType write FResultType;

    property SelfPtr: TPSValue read FSelfPtr write FSelfPtr;

    property Parameters: TPSParameters read FParameters write FParameters;
    destructor Destroy; override;
  end;

  TPSValueProcNo = class(TPSValueProc)
  private
    FProcNo: Cardinal;
  public

    property ProcNo: Cardinal read FProcNo write FProcNo;
  end;

  TPSValueProcVal = class(TPSValueProc)
  private
    FProcNo: TPSValue;
  public

    property ProcNo: TPSValue read FProcNo write FProcNo;

    destructor Destroy; override;
  end;

  TPSValueArray = class(TPSValue)
  private
    FItems: TPSList;
    function GetCount: Cardinal;
    function GetItem(I: Cardinal): TPSValue;
  public
    function Add(Item: TPSValue): Cardinal;
    procedure Delete(I: Cardinal);
    property Item[I: Cardinal]: TPSValue read GetItem;
    property Count: Cardinal read GetCount;

    constructor Create;
    destructor Destroy; override;
  end;

  TPSDelphiClassItem = class;

  TPSPropType = (iptRW, iptR, iptW);

  TPSCompileTimeClass = class
  private
    FInheritsFrom: TPSCompileTimeClass;
    FClass: TClass;
    FClassName: tbtString;
    FClassNameHash: Longint;
    FClassItems: TPSList;
    FDefaultProperty: Cardinal;
    FIsAbstract: Boolean;
    FCastProc,
    FNilProc: Cardinal;
    FType: TPSType;

    FOwner: TPSPascalCompiler;
    function GetCount: Longint;
    function GetItem(i: Longint): TPSDelphiClassItem;
  public

    property aType: TPSType read FType;

    property Items[i: Longint]: TPSDelphiClassItem read GetItem;

    property Count: Longint read GetCount;

    property IsAbstract: Boolean read FIsAbstract write FIsAbstract;


    property ClassInheritsFrom: TPSCompileTimeClass read FInheritsFrom write FInheritsFrom;

    function RegisterMethod(const Decl: tbtString): Boolean;

    procedure RegisterProperty(const PropertyName, PropertyType: tbtString; PropAC: TPSPropType);

    procedure RegisterPublishedProperties;

    function RegisterPublishedProperty(const Name: tbtString): Boolean;

    procedure SetDefaultPropery(const Name: tbtString);

    constructor Create(ClassName: tbtString; aOwner: TPSPascalCompiler; aType: TPSType);

    class function CreateC(FClass: TClass; aOwner: TPSPascalCompiler; aType: TPSType): TPSCompileTimeClass;


    destructor Destroy; override;


    function IsCompatibleWith(aType: TPSType): Boolean;

    function SetNil(var ProcNo: Cardinal): Boolean;

    function CastToType(IntoType: TPSType; var ProcNo: Cardinal): Boolean;


    function Property_Find(const Name: tbtString; var Index: TPSDelphiClassItem): Boolean;

    function Property_Get(Index: TPSDelphiClassItem; var ProcNo: Cardinal): Boolean;

    function Property_Set(Index: TPSDelphiClassItem; var ProcNo: Cardinal): Boolean;

    function Property_GetHeader(Index: TPSDelphiClassItem; Dest: TPSParametersDecl): Boolean;


    function Func_Find(const Name: tbtString; var Index: TPSDelphiClassItem): Boolean;

    function Func_Call(Index: TPSDelphiClassItem; var ProcNo: Cardinal): Boolean;


    function ClassFunc_Find(const Name: tbtString; var Index: IPointer): Boolean;

    function ClassFunc_Call(Index: IPointer; var ProcNo: Cardinal): Boolean;
  end;

  TPSDelphiClassItem = class(TObject)
  private
    FOwner: TPSCompileTimeClass;
    FOrgName: tbtString;
    FName: tbtString;
    FNameHash: Longint;
    FDecl: TPSParametersDecl;
    procedure SetName(const s: tbtString);
  public

    constructor Create(Owner: TPSCompileTimeClass);

    destructor Destroy; override;

    property Decl: TPSParametersDecl read FDecl;

    property Name: tbtString read FName;

    property OrgName: tbtString read FOrgName write SetName;

    property NameHash: Longint read FNameHash;

    property Owner: TPSCompileTimeClass read FOwner;
  end;

  TPSDelphiClassItemMethod = class(TPSDelphiClassItem)
  private
    FMethodNo: Cardinal;
  public

    property MethodNo: Cardinal read FMethodNo write FMethodNo;
  end;

  TPSDelphiClassItemProperty = class(TPSDelphiClassItem)
  private
    FReadProcNo: Cardinal;
    FWriteProcNo: Cardinal;
    FAccessType: TPSPropType;
  public

    property AccessType: TPSPropType read FAccessType write FAccessType;

    property ReadProcNo: Cardinal read FReadProcNo write FReadProcNo;

    property WriteProcNo: Cardinal read FWriteProcNo write FWriteProcNo;
  end;


  TPSDelphiClassItemConstructor = class(TPSDelphiClassItemMethod)
  end;

{$IFNDEF PS_NOINTERFACES}

  TPSInterfaceMethod = class;

  TPSInterface = class(TObject)
  private
    FOwner: TPSPascalCompiler;
    FType: TPSType;
    FInheritedFrom: TPSInterface;
    FGuid: TGuid;
    FCastProc,
    FNilProc: Cardinal;
    FItems: TPSList;
    FName: tbtString;
    FNameHash: Longint;
    procedure SetInheritedFrom(p: TPSInterface);
  public

    constructor Create(Owner: TPSPascalCompiler; InheritedFrom: TPSInterface; Guid: TGuid; const Name: tbtString; aType: TPSType);

    destructor Destroy; override;

    property aType: TPSType read FType;

    property InheritedFrom: TPSInterface read FInheritedFrom write SetInheritedFrom;

    property Guid: TGuid read FGuid write FGuid;

    property Name: tbtString read FName write FName;

    property NameHash: Longint read FNameHash;


    function RegisterMethod(const Declaration: tbtString; const cc: TPSCallingConvention): Boolean;

    function RegisterMethodEx(const Declaration: tbtString; const cc: TPSCallingConvention; const CustomParser: TPSPascalParser): Boolean;

    procedure RegisterDummyMethod;

    function IsCompatibleWith(aType: TPSType): Boolean;

    function SetNil(var ProcNo: Cardinal): Boolean;

    function CastToType(IntoType: TPSType; var ProcNo: Cardinal): Boolean;

    function Func_Find(const Name: tbtString; var Index: TPSInterfaceMethod): Boolean;

    function Func_Call(Index: TPSInterfaceMethod; var ProcNo: Cardinal): Boolean;
  end;


  TPSInterfaceMethod = class(TObject)
  private
    FName: tbtString;
    FDecl: TPSParametersDecl;
    FNameHash: Longint;
    FCC: TPSCallingConvention;
    FScriptProcNo: Cardinal;
    FOrgName: tbtString;
    FOwner: TPSInterface;
    FOffsetCache: Cardinal;
    function GetAbsoluteProcOffset: Cardinal;
  public

    property AbsoluteProcOffset: Cardinal read GetAbsoluteProcOffset;

    property ScriptProcNo: Cardinal read FScriptProcNo;

    property OrgName: tbtString read FOrgName;

    property Name: tbtString read FName;

    property NameHash: Longint read FNameHash;

    property Decl: TPSParametersDecl read FDecl;

    property CC: TPSCallingConvention read FCC;


    constructor Create(Owner: TPSInterface);

    destructor Destroy; override;
  end;
{$ENDIF}


  TPSExternalClass = class(TObject)
  protected

    SE: TPSPascalCompiler;

    FTypeNo: TPSType;
  public

    function SelfType: TPSType; virtual;
	
    constructor Create(Se: TPSPascalCompiler; TypeNo: TPSType);

    function ClassFunc_Find(const Name: tbtString; var Index: Cardinal): Boolean; virtual;
	
    function ClassFunc_Call(Index: Cardinal; var ProcNo: Cardinal): Boolean; virtual;

    function Func_Find(const Name: tbtString; var Index: Cardinal): Boolean; virtual;

    function Func_Call(Index: Cardinal; var ProcNo: Cardinal): Boolean; virtual;

    function IsCompatibleWith(Cl: TPSExternalClass): Boolean; virtual;

    function SetNil(var ProcNo: Cardinal): Boolean; virtual;

    function CastToType(IntoType: TPSType; var ProcNo: Cardinal): Boolean; virtual;

    function CompareClass(OtherTypeNo: TPSType; var ProcNo: Cardinal): Boolean; virtual;
  end;


function ExportCheck(Sender: TPSPascalCompiler; Proc: TPSInternalProcedure;
  Types: array of TPSBaseType; Modes: array of TPSParameterMode): Boolean;


procedure SetVarExportName(P: TPSVar; const ExpName: tbtString);

function AddImportedClassVariable(Sender: TPSPascalCompiler; const VarName, VarType: tbtString): Boolean;

const
  {Invalid value, this is returned by most functions of pascal script that return a cardinal, when they fail}
  InvalidVal = Cardinal(-1);

type
  TIFPSCompileTimeClass = TPSCompileTimeClass;
  TIFPSInternalProcedure = TPSInternalProcedure;
  TIFPSPascalCompilerError = TPSPascalCompilerError;

  TPMFuncType = (mftProc
  , mftConstructor
  );


function PS_mi2s(i: Cardinal): tbtString;

function ParseMethod(Owner: TPSPascalCompiler; const FClassName: tbtString; Decl: tbtString; var OrgName: tbtString; DestDecl: TPSParametersDecl; var Func: TPMFuncType): Boolean;
function ParseMethodEx(Owner: TPSPascalCompiler; const FClassName: tbtString; Decl: tbtString; var OrgName: tbtString; DestDecl: TPSParametersDecl; var Func: TPMFuncType; CustomParser: TPSPascalParser): Boolean;

function DeclToBits(const Decl: TPSParametersDecl): tbtString;

function NewVariant(FType: TPSType): PIfRVariant;
function GetString(Src: PIfRVariant; var s: Boolean): tbtString;
procedure DisposeVariant(p: PIfRVariant);

implementation

uses {$IFDEF DELPHI5}ComObj, {$ENDIF}{$IFDEF PS_FPC_HAS_COM}ComObj, {$ENDIF}Classes, typInfo;

{$IFDEF DELPHI3UP}
resourceString
{$ELSE}
const
{$ENDIF}

  RPS_OnUseEventOnly = 'This function can only be called from within the OnUses event';
  RPS_UnableToRegisterFunction = 'Unable to register function %s';
  RPS_UnableToRegisterConst = 'Unable to register constant %s';
  RPS_InvalidTypeForVar = 'Invalid type for variable %s';
  RPS_InvalidType = 'Invalid Type';
  RPS_UnableToRegisterType = 'Unable to register type %s';
  RPS_UnknownInterface = 'Unknown interface: %s';
  RPS_ConstantValueMismatch = 'Constant Value Type Mismatch';
  RPS_ConstantValueNotAssigned = 'Constant Value is not assigned';

  RPS_Error = 'Error';
  RPS_UnknownIdentifier = 'Unknown identifier ''%s''';
  RPS_IdentifierExpected = 'Identifier expected';
  RPS_CommentError = 'Comment error';
  RPS_StringError = 'String error';
  RPS_CharError = 'Char error';
  RPS_SyntaxError = 'Syntax error';
  RPS_EOF = 'Unexpected end of file';
  RPS_SemiColonExpected = 'Semicolon ('';'') expected';
  RPS_BeginExpected = '''BEGIN'' expected';
  RPS_PeriodExpected = 'period (''.'') expected';
  RPS_DuplicateIdent = 'Duplicate identifier ''%s''';
  RPS_ColonExpected = 'colon ('':'') expected';
  RPS_UnknownType = 'Unknown type ''%s''';
  RPS_CloseRoundExpected = 'Closing parenthesis expected';
  RPS_TypeMismatch = 'Type mismatch';
  RPS_InternalError = 'Internal error (%s)';
  RPS_AssignmentExpected = 'Assignment expected';
  RPS_ThenExpected = '''THEN'' expected';
  RPS_DoExpected = '''DO'' expected';
  RPS_NoResult = 'No result';
  RPS_OpenRoundExpected = 'opening parenthesis (''('')expected';
  RPS_CommaExpected = 'comma ('','') expected';
  RPS_ToExpected = '''TO'' expected';
  RPS_IsExpected = 'is (''='') expected';
  RPS_OfExpected = '''OF'' expected';
  RPS_CloseBlockExpected = 'Closing square bracket ('']'') expected';
  RPS_VariableExpected = 'Variable Expected';
  RPS_StringExpected = 'String Expected';
  RPS_EndExpected = '''END'' expected';
  RPS_UnSetLabel = 'Label ''%s'' not set';
  RPS_NotInLoop = 'Not in a loop';
  RPS_InvalidJump = 'Invalid jump';
  RPS_OpenBlockExpected = 'Opening square brackets (''['') expected';
  RPS_WriteOnlyProperty = 'Write-only property';
  RPS_ReadOnlyProperty = 'Read-only property';
  RPS_ClassTypeExpected = 'Class type expected';
  RPS_DivideByZero = 'Divide by Zero';
  RPS_MathError = 'Math Error';
  RPS_UnsatisfiedForward = 'Unsatisfied Forward %s';
  RPS_ForwardParameterMismatch = 'Forward Parameter Mismatch';
  RPS_InvalidNumberOfParameter = 'Invalid number of parameters';
  RPS_UnknownError = 'Unknown error';
  {$IFDEF PS_USESSUPPORT}
  RPS_NotAllowed = '%s is not allowed at this position';
  RPS_UnitNotFound = 'Unit ''%s'' not found or contains errors';
  RPS_CrossReference = 'Cross-Reference error of ''%s''';
  {$ENDIF}
  RPS_UnClosedAttributes = 'Attributes not closed';

  RPS_Hint = 'Hint';
  RPS_VariableNotUsed = 'Variable ''%s'' never used';
  RPS_FunctionNotUsed = 'Function ''%s'' never used';
  RPS_UnknownHint = 'Unknown hint';


  RPS_Warning = 'Warning';
  RPS_CalculationAlwaysEvaluatesTo = 'Calculation always evaluates to %s';
  RPS_IsNotNeeded =  '%s is not needed';
  RPS_AbstractClass = 'Abstract Class Construction';
  RPS_UnknownWarning = 'Unknown warning';

  {$IFDEF DEBUG }
  RPS_UnableToRegister = 'Unable to register %s';
  {$ENDIF}

  RPS_NotArrayProperty = 'Not an array property : ''%s''';
  RPS_NotProperty = 'Not a property : ''%s''';
  RPS_UnknownProperty = 'Unknown Property : ''%s''';

function DeclToBits(const Decl: TPSParametersDecl): tbtString;
var
  i: longint;
begin
  Result := '';
  if Decl.Result = nil then
  begin
    Result := Result + #0;
  end else
    Result := Result + #1;
  for i := 0 to Decl.ParamCount -1 do
  begin
    if Decl.Params[i].Mode <> pmIn then
      Result := Result + #1
    else
      Result := Result + #0;
  end;
end;


procedure BlockWriteByte(BlockInfo: TPSBlockInfo; b: Byte);
begin
  BlockInfo.Proc.Data := BlockInfo.Proc.Data + tbtChar(b);
end;

procedure BlockWriteData(BlockInfo: TPSBlockInfo; const Data; Len: Longint);
begin
  SetLength(BlockInfo.Proc.FData, Length(BlockInfo.Proc.FData) + Len);
  Move(Data, BlockInfo.Proc.FData[Length(BlockInfo.Proc.FData) - Len + 1], Len);
end;

procedure BlockWriteLong(BlockInfo: TPSBlockInfo; l: Cardinal);
begin
  BlockWriteData(BlockInfo, l, 4);
end;

procedure BlockWriteVariant(BlockInfo: TPSBlockInfo; p: PIfRVariant);
var
  du8: tbtu8;
  du16: tbtu16;
begin
  BlockWriteLong(BlockInfo, p^.FType.FinalTypeNo);
  case p.FType.BaseType of
  btType: BlockWriteData(BlockInfo, p^.ttype.FinalTypeno, 4);
  {$IFNDEF PS_NOWIDESTRING}
  btWideString:
    begin
      BlockWriteLong(BlockInfo, Length(tbtWideString(p^.twidestring)));
      BlockWriteData(BlockInfo, tbtwidestring(p^.twidestring)[1], 2*Length(tbtWideString(p^.twidestring)));
    end;
  btUnicodeString:
    begin
      BlockWriteLong(BlockInfo, Length(tbtUnicodeString(p^.twidestring)));
      BlockWriteData(BlockInfo, tbtUnicodeString(p^.twidestring)[1], 2*Length(tbtUnicodeString(p^.twidestring)));
    end;
  btWideChar: BlockWriteData(BlockInfo, p^.twidechar, 2);
  {$ENDIF}
  btSingle: BlockWriteData(BlockInfo, p^.tsingle, sizeof(tbtSingle));
  btDouble: BlockWriteData(BlockInfo, p^.tdouble, sizeof(tbtDouble));
  btExtended: BlockWriteData(BlockInfo, p^.textended, sizeof(tbtExtended));
  btCurrency: BlockWriteData(BlockInfo, p^.tcurrency, sizeof(tbtCurrency));
  btChar: BlockWriteData(BlockInfo, p^.tchar, 1);
  btSet:
    begin
      BlockWriteData(BlockInfo, tbtString(p^.tstring)[1], Length(tbtString(p^.tstring)));
    end;
  btString:
    begin
      BlockWriteLong(BlockInfo, Length(tbtString(p^.tstring)));
      BlockWriteData(BlockInfo, tbtString(p^.tstring)[1], Length(tbtString(p^.tstring)));
    end;
     btenum:
     begin
       if TPSEnumType(p^.FType).HighValue <=256 then
      begin
        du8 := tbtu8(p^.tu32);
        BlockWriteData(BlockInfo, du8, 1)
      end
       else if TPSEnumType(p^.FType).HighValue <=65536 then
      begin
        du16 := tbtu16(p^.tu32);
        BlockWriteData(BlockInfo, du16, 2)
      end;
	end;

  bts8,btu8: BlockWriteData(BlockInfo, p^.tu8, 1);
  bts16,btu16: BlockWriteData(BlockInfo, p^.tu16, 2);
  bts32,btu32: BlockWriteData(BlockInfo, p^.tu32, 4);
  {$IFNDEF PS_NOINT64}
  bts64: BlockWriteData(BlockInfo, p^.ts64, 8);
  {$ENDIF}
  btProcPtr: BlockWriteData(BlockInfo, p^.tu32, 4);
  {$IFDEF DEBUG}
  {$IFNDEF FPC}
  else
      asm int 3; end;
  {$ENDIF}
  {$ENDIF}
  end;
end;



function ExportCheck(Sender: TPSPascalCompiler; Proc: TPSInternalProcedure; Types: array of TPSBaseType; Modes: array of TPSParameterMode): Boolean;
var
  i: Longint;
  ttype: TPSType;
begin
  if High(Types) <> High(Modes)+1 then
  begin
    Result := False;
    exit;
  end;
  if High(Types) <> Proc.Decl.ParamCount then
  begin
    Result := False;
    exit;
  end;
  TType := Proc.Decl.Result;
  if TType = nil then
  begin
    if Types[0] <> btReturnAddress then
    begin
      Result := False;
      exit;
    end;
  end else
  begin
    if TType.BaseType <> Types[0] then
    begin
      Result := False;
      exit;
    end;
  end;
  for i := 0 to High(Modes) do
  begin
    TType := Proc.Decl.Params[i].aType;
    if Modes[i] <> Proc.Decl.Params[i].Mode then
    begin
      Result := False;
      exit;
    end;
    if TType.BaseType <> Types[i+1] then
    begin
      Result := False;
      exit;
    end;
  end;
  Result := True;
end;

procedure SetVarExportName(P: TPSVar; const ExpName: tbtString);
begin
  if p <> nil then
    p.exportname := ExpName;
end;

function FindAndAddType(Owner: TPSPascalCompiler; const Name, Decl: tbtString): TPSType;
var
  tt: TPSType;
begin
  Result := Owner.FindType(Name);
  if Result = nil then
  begin
    tt := Owner.AddTypeS(Name, Decl);
    Result := tt;
  end;
end;

function ParseMethod(Owner: TPSPascalCompiler; const FClassName: tbtString; Decl: tbtString; var OrgName: tbtString; DestDecl: TPSParametersDecl; var Func: TPMFuncType): Boolean;
begin
  Result := ParseMethodEx(Owner, FClassName, Decl, OrgName, DestDecl, Func, nil);
end;

function ParseMethodEx(Owner: TPSPascalCompiler; const FClassName: tbtString; Decl: tbtString; var OrgName: tbtString; DestDecl: TPSParametersDecl; var Func: TPMFuncType; CustomParser: TPSPascalParser): Boolean;
var
  Parser: TPSPascalParser;
  FuncType: Byte;
  VNames: tbtString;
  modifier: TPSParameterMode;
  VCType: TPSType;
  ERow, EPos, ECol: Integer;

begin
  if CustomParser = nil then begin
    Parser := TPSPascalParser.Create;
    Parser.SetText(Decl);
  end else
    Parser := CustomParser;
  if Parser.CurrTokenId = CSTII_Function then
    FuncType:= 0
  else if Parser.CurrTokenId = CSTII_Procedure then
    FuncType := 1
  else if (Parser.CurrTokenId = CSTII_Constructor) and (FClassName <> '') then
    FuncType := 2
  else
  begin
    if Parser <> CustomParser then
      Parser.Free;
    Result := False;
    exit;
  end;
  Parser.Next;
  if Parser.CurrTokenId <> CSTI_Identifier then
  begin
    if Parser <> CustomParser then
      Parser.Free
    else
      Owner.MakeError('', ecIdentifierExpected, '');
    Result := False;
    exit;
  end; {if}
  OrgName := Parser.OriginalToken;
  Parser.Next;
  if Parser.CurrTokenId = CSTI_OpenRound then
  begin
    Parser.Next;
    if Parser.CurrTokenId <> CSTI_CloseRound then
    begin
      while True do
      begin
        if Parser.CurrTokenId = CSTII_Const then
        begin
          modifier := pmIn;
          Parser.Next;
        end
        else
        if Parser.CurrTokenId = CSTII_Var then
        begin
          modifier := pmInOut;
          Parser.Next;
        end
        else
        if Parser.CurrTokenId = CSTII_Out then
        begin
          modifier := pmOut;
          Parser.Next;
        end
        else
          modifier := pmIn;
        if Parser.CurrTokenId <> CSTI_Identifier then
        begin
          if Parser <> CustomParser then
            Parser.Free
          else
            Owner.MakeError('', ecIdentifierExpected, '');
          Result := False;
          exit;
        end;
        EPos:=Parser.CurrTokenPos;
        ERow:=Parser.Row;
        ECol:=Parser.Col;

        VNames := Parser.OriginalToken + '|';
        Parser.Next;
        while Parser.CurrTokenId = CSTI_Comma do
        begin
          Parser.Next;
          if Parser.CurrTokenId <> CSTI_Identifier then
          begin
            if Parser <> CustomParser then
              Parser.Free
            else
              Owner.MakeError('', ecIdentifierExpected, '');
            Result := False;
            exit;
          end;
          VNames := VNames + Parser.OriginalToken + '|';
          Parser.Next;
        end;
        if Parser.CurrTokenId <> CSTI_Colon then
        begin
          if Parser <> CustomParser then
            Parser.Free
          else
            Owner.MakeError('', ecColonExpected, '');
          Result := False;
          exit;
        end;
        Parser.Next;
        if Parser.CurrTokenID = CSTII_Array then
        begin
          Parser.nExt;
          if Parser.CurrTokenId <> CSTII_Of then
          begin
            if Parser <> CustomParser then
              Parser.Free
            else
              Owner.MakeError('', ecOfExpected, '');
            Result := False;
            exit;
          end;
          Parser.Next;
          if Parser.CurrTokenId = CSTII_Const then
          begin
            VCType := FindAndAddType(Owner, '!OPENARRAYOFCONST', 'array of ___Pointer')
          end
          else begin
            VCType := Owner.GetTypeCopyLink(Owner.FindType(Parser.GetToken));
            if VCType = nil then
            begin
              if Parser <> CustomParser then
                Parser.Free
              else
                Owner.MakeError('', ecUnknownType, Parser.GetToken);
              Result := False;
              exit;
            end;
            case VCType.BaseType of
              btU8: VCType := FindAndAddType(Owner, '!OPENARRAYOFU8', 'array of Byte');
              btS8: VCType := FindAndAddType(Owner, '!OPENARRAYOFS8', 'array of ShortInt');
              btU16: VCType := FindAndAddType(Owner, '!OPENARRAYOFU16', 'array of SmallInt');
              btS16: VCType := FindAndAddType(Owner, '!OPENARRAYOFS16', 'array of Word');
              btU32: VCType := FindAndAddType(Owner, '!OPENARRAYOFU32', 'array of Cardinal');
              btS32: VCType := FindAndAddType(Owner, '!OPENARRAYOFS32', 'array of LongInt');
              btSingle: VCType := FindAndAddType(Owner, '!OPENARRAYOFSINGLE', 'array of Single');
              btDouble: VCType := FindAndAddType(Owner, '!OPENARRAYOFDOUBLE', 'array of Double');
              btExtended: VCType := FindAndAddType(Owner, '!OPENARRAYOFEXTENDED', 'array of Extended');
              btString: VCType := FindAndAddType(Owner, '!OPENARRAYOFSTRING', 'array of string');
              btPChar: VCType := FindAndAddType(Owner, '!OPENARRAYOFPCHAR', {$IFDEF PS_PANSICHAR}'array of PAnsiChar'{$ELSE}'array of PChar'{$ENDIF});
              btNotificationVariant, btVariant: VCType := FindAndAddType(Owner, '!OPENARRAYOFVARIANT', 'array of Variant');
            {$IFNDEF PS_NOINT64}btS64:  VCType := FindAndAddType(Owner, '!OPENARRAYOFS64', 'array of Int64');{$ENDIF}
              btChar: VCType := FindAndAddType(Owner, '!OPENARRAYOFCHAR', 'array of Char');
            {$IFNDEF PS_NOWIDESTRING}
              btWideString: VCType := FindAndAddType(Owner, '!OPENARRAYOFWIDESTRING', 'array of WideString');
              btUnicodeString: VCType := FindAndAddType(Owner, '!OPENARRAYOFUNICODESTRING', 'array of UnicodeString');
              btWideChar: VCType := FindAndAddType(Owner, '!OPENARRAYOFWIDECHAR', 'array of WideChar');
            {$ENDIF}
              btClass: VCType := FindAndAddType(Owner, '!OPENARRAYOFTOBJECT', 'array of TObject');
              btRecord: VCType := FindAndAddType(Owner, '!OPENARRAYOFRECORD_'+FastUpperCase(Parser.OriginalToken), 'array of ' +FastUpperCase(Parser.OriginalToken));
              btEnum: VCType := FindAndAddType(Owner, '!OPENARRAYOFENUM_' + FastUpperCase(Parser.OriginalToken), 'array of ' + FastUpperCase(Parser.OriginalToken));
            else
              begin
                if Parser <> CustomParser then
                  Parser.Free;
                Result := False;
                exit;
              end;
            end;
          end;
        end else if Parser.CurrTokenID = CSTII_Const then
          VCType := nil // any type
        else begin
          VCType := Owner.FindType(Parser.GetToken);
          if VCType = nil then
          begin
            if Parser <> CustomParser then
              Parser.Free
            else
              Owner.MakeError('', ecUnknownType, Parser.GetToken);
            Result := False;
            exit;
          end;
        end;
        while Pos(tbtchar('|'), VNames) > 0 do
        begin
          with DestDecl.AddParam do
          begin
            {$IFDEF PS_USESSUPPORT}
            DeclareUnit:=Owner.fModule;
            {$ENDIF}
            DeclarePos := EPos;
            DeclareRow := ERow;
            DeclareCol := ECol;
            Mode := modifier;
            OrgName := copy(VNames, 1, Pos(tbtchar('|'), VNames) - 1);
            aType := VCType;
          end;
          Delete(VNames, 1, Pos(tbtchar('|'), VNames));
        end;
        Parser.Next;
        if Parser.CurrTokenId = CSTI_CloseRound then
          break;
        if Parser.CurrTokenId <> CSTI_Semicolon then
        begin
          if Parser <> CustomParser then
            Parser.Free
          else
            Owner.MakeError('', ecSemiColonExpected, '');
          Result := False;
          exit;
        end;
        Parser.Next;
      end; {while}
    end; {if}
    Parser.Next;
  end; {if}
  if FuncType = 0 then
  begin
    if Parser.CurrTokenId <> CSTI_Colon then
    begin
      if Parser <> CustomParser then
        Parser.Free
      else
        Owner.MakeError('', ecColonExpected, '');
      Result := False;
      exit;
    end;

    Parser.Next;
    VCType := Owner.FindType(Parser.GetToken);
    if VCType = nil then
    begin
      if Parser <> CustomParser then
        Parser.Free
      else
        Owner.MakeError('', ecUnknownType, Parser.GetToken);
      Result := False;
      exit;
    end;
    Parser.Next;
  end
  else if FuncType = 2 then {constructor}
  begin
    VCType := Owner.FindType(FClassName)
  end else
    VCType := nil;
  DestDecl.Result := VCType;
  if Parser <> CustomParser then
    Parser.Free;
  if FuncType = 2 then
    Func := mftConstructor
  else
    Func := mftProc;
  Result := True;
end;



function TPSPascalCompiler.FindProc(const aName: tbtString): Cardinal;
var
  l, h: Longint;
  x: TPSProcedure;
  xr: TPSRegProc;
  name: tbtString;

begin
  name := FastUpperCase(aName);
  h := MakeHash(Name);
  if FProcs = nil then
  begin
    result := InvalidVal;
    Exit;
  end;

  for l := FProcs.Count - 1 downto 0 do
  begin
    x := FProcs.Data^[l];
    if x.ClassType = TPSInternalProcedure then
    begin
      if (TPSInternalProcedure(x).NameHash = h) and
        (TPSInternalProcedure(x).Name = Name) then
      begin
        Result := l;
        exit;
      end;
    end
    else
    begin
      if (TPSExternalProcedure(x).RegProc.NameHash = h) and
        (TPSExternalProcedure(x).RegProc.Name = Name)then
      begin
        Result := l;
        exit;
      end;
    end;
  end;
  for l := FRegProcs.Count - 1 downto 0 do
  begin
    xr := FRegProcs[l];
    if (xr.NameHash = h) and (xr.Name = Name) then
    begin
      x := TPSExternalProcedure.Create;
      TPSExternalProcedure(x).RegProc := xr;
      FProcs.Add(x);
      if @FOnUseRegProc <> nil then
        FOnUseRegProc(Self, FParser.CurrTokenPos, Name);
      Result := FProcs.Count - 1;
      exit;
    end;
  end;
  Result := InvalidVal;
end; {findfunc}

function TPSPascalCompiler.UseExternalProc(const Name: tbtString): TPSParametersDecl;
var
  ProcNo: cardinal;
  proc: TPSProcedure;
begin
  ProcNo := FindProc(FastUppercase(Name));
  if ProcNo = InvalidVal then Result := nil
  else
  begin
    proc := TPSProcedure(FProcs[ProcNo]);
    if Proc is TPSExternalProcedure then
    begin
      Result := TPSExternalProcedure(Proc).RegProc.Decl;
    end else result := nil;
  end;
end;



function TPSPascalCompiler.FindBaseType(BaseType: TPSBaseType): TPSType;
var
  l: Longint;
  x: TPSType;
begin
  for l := 0 to FTypes.Count -1 do
  begin
    X := FTypes[l];
    if (x.BaseType = BaseType) and (x.ClassType = TPSType)  then
    begin
      Result := at2ut(x);
      exit;
    end;
  end;
  X := TPSType.Create;
  x.Name := '';
  x.BaseType := BaseType;
  {$IFDEF PS_USESSUPPORT}
  x.DeclareUnit:=fModule;
  {$ENDIF}
  x.DeclarePos := InvalidVal;
  x.DeclareCol := 0;
  x.DeclareRow := 0;
  FTypes.Add(x);
  Result := at2ut(x);
end;

function TPSPascalCompiler.MakeDecl(decl: TPSParametersDecl): tbtString;
var
  i: Longint;
begin
  if Decl.Result = nil then result := '0' else
  result := Decl.Result.Name;

  for i := 0 to decl.ParamCount -1 do
  begin
    if decl.GetParam(i).Mode = pmIn then
      Result := Result + ' @'
    else
      Result := Result + ' !';
    Result := Result + decl.GetParam(i).aType.Name;
  end;
end;


{ TPSPascalCompiler }

const
  BtTypeCopy = 255;


type
  TFuncType = (ftProc, ftFunc);

function PS_mi2s(i: Cardinal): tbtString;
begin
  SetLength(Result, 4);
  Cardinal((@Result[1])^) := i;
end;




function TPSPascalCompiler.AddType(const Name: tbtString; const BaseType: TPSBaseType): TPSType;
begin
  if FProcs = nil then
  begin
    raise EPSCompilerException.Create(RPS_OnUseEventOnly);
  end;

  if not(AllowDuplicateRegister) and IsDuplicate(FastUpperCase(Name),[dcTypes, dcProcs, dcVars]) then
      Raise EPSCompilerException.CreateFmt(RPS_DuplicateIdent, [Name]);

  case BaseType of
    btProcPtr: Result := TPSProceduralType.Create;
    BtTypeCopy: Result := TPSTypeLink.Create;
    btRecord: Result := TPSRecordType.Create;
    btArray: Result := TPSArrayType.Create;
    btStaticArray: Result := TPSStaticArrayType.Create;
    btEnum: Result := TPSEnumType.Create;
    btClass: Result := TPSClassType.Create;
    btExtClass: REsult := TPSUndefinedClassType.Create;
    btNotificationVariant, btVariant: Result := TPSVariantType.Create;
{$IFNDEF PS_NOINTERFACES}
    btInterface: Result := TPSInterfaceType.Create;
{$ENDIF}
  else
    Result := TPSType.Create;
  end;
  Result.Name := FastUppercase(Name);
  Result.OriginalName := Name;
  Result.BaseType := BaseType;
  {$IFDEF PS_USESSUPPORT}
  Result.DeclareUnit:=fModule;
  {$ENDIF}
  Result.DeclarePos := InvalidVal;
  Result.DeclareCol := 0;
  Result.DeclareRow := 0;
  FTypes.Add(Result);
end;


function TPSPascalCompiler.AddFunction(const Header: tbtString): TPSRegProc;
var
  Parser: TPSPascalParser;
  i: Integer;
  IsFunction: Boolean;
  VNames, Name: tbtString;
  Decl: TPSParametersDecl;
  modifier: TPSParameterMode;
  VCType: TPSType;
  x: TPSRegProc;
begin
  if FProcs = nil then
    raise EPSCompilerException.Create(RPS_OnUseEventOnly);

  Parser := TPSPascalParser.Create;
  Parser.SetText(Header);
  Decl := TPSParametersDecl.Create;
{$IFNDEF DELPHI_TOKYO_UP}
  x := nil;
{$ENDIF}
  try
    if Parser.CurrTokenId = CSTII_Function then
      IsFunction := True
    else if Parser.CurrTokenId = CSTII_Procedure then
      IsFunction := False
    else
      Raise EPSCompilerException.CreateFmt(RPS_UnableToRegisterFunction, ['']);
    Parser.Next;
    if Parser.CurrTokenId <> CSTI_Identifier then
      Raise EPSCompilerException.CreateFmt(RPS_UnableToRegisterFunction, ['']);
    Name := Parser.OriginalToken;
    Parser.Next;
    if Parser.CurrTokenId = CSTI_OpenRound then
    begin
      Parser.Next;
      if Parser.CurrTokenId <> CSTI_CloseRound then
      begin
        while True do
        begin
          if Parser.CurrTokenId = CSTII_Out then
          begin
            Modifier := pmOut;
            Parser.Next;
          end else
          if Parser.CurrTokenId = CSTII_Const then
          begin
            Modifier := pmIn;
            Parser.Next;
          end else
          if Parser.CurrTokenId = CSTII_Var then
          begin
            modifier := pmInOut;
            Parser.Next;
          end
          else
            modifier := pmIn;
          if Parser.CurrTokenId <> CSTI_Identifier then
            raise EPSCompilerException.CreateFmt(RPS_UnableToRegisterFunction, [Name]);
          VNames := Parser.OriginalToken + '|';
          Parser.Next;
          while Parser.CurrTokenId = CSTI_Comma do
          begin
            Parser.Next;
            if Parser.CurrTokenId <> CSTI_Identifier then
              Raise EPSCompilerException.CreateFmt(RPS_UnableToRegisterFunction, [Name]);
            VNames := VNames + Parser.OriginalToken + '|';
            Parser.Next;
          end;
          if Parser.CurrTokenId <> CSTI_Colon then
          begin
            Parser.Free;
            Raise EPSCompilerException.CreateFmt(RPS_UnableToRegisterFunction, [Name]);
          end;
          Parser.Next;
          VCType := FindType(Parser.GetToken);
          if VCType = nil then
            Raise EPSCompilerException.CreateFmt(RPS_UnableToRegisterFunction, [Name]);
          while Pos(tbtchar('|'), VNames) > 0 do
          begin
            with Decl.AddParam do
            begin
              Mode := modifier;
              OrgName := copy(VNames, 1, Pos(tbtchar('|'), VNames) - 1);
              aType := VCType;
            end;
            Delete(VNames, 1, Pos(tbtchar('|'), VNames));
          end;
          Parser.Next;
          if Parser.CurrTokenId = CSTI_CloseRound then
            break;
          if Parser.CurrTokenId <> CSTI_Semicolon then
            Raise EPSCompilerException.CreateFmt(RPS_UnableToRegisterFunction, [Name]);
          Parser.Next;
        end; {while}
      end; {if}
      Parser.Next;
    end; {if}
    if IsFunction then
    begin
      if Parser.CurrTokenId <> CSTI_Colon then
        Raise EPSCompilerException.CreateFmt(RPS_UnableToRegisterFunction, [Name]);

      Parser.Next;
      VCType := FindType(Parser.GetToken);
      if VCType = nil then
        Raise EPSCompilerException.CreateFmt(RPS_UnableToRegisterFunction, [Name]);
    end
    else
      VCType := nil;
    Decl.Result := VCType;
    X := TPSRegProc.Create;
    x.OrgName := Name;
    x.Name := FastUpperCase(Name);
    x.ExportName := True;
    x.Decl.Assign(decl);
    if Decl.Result = nil then
    begin
      x.ImportDecl := x.ImportDecl + #0;
    end else
      x.ImportDecl := x.ImportDecl + #1;
    for i := 0 to Decl.ParamCount -1 do
    begin
      if Decl.Params[i].Mode <> pmIn then
        x.ImportDecl := x.ImportDecl + #1
      else
        x.ImportDecl := x.ImportDecl + #0;
    end;

    FRegProcs.Add(x);
  finally
    Decl.Free;
    Parser.Free;
  end;
  Result := x;
end;

function TPSPascalCompiler.MakeHint(const Module: tbtString; E: TPSPascalCompilerHintType; const Param: tbtString): TPSPascalCompilerMessage;
var
  n: TPSPascalCompilerHint;
begin
  N := TPSPascalCompilerHint.Create;
  n.FHint := e;
  n.SetParserPos(FParser);
  n.FModuleName := Module;
  n.FParam := Param;
  FMessages.Add(n);
  Result := n;
end;

function TPSPascalCompiler.MakeError(const Module: tbtString; E:
  TPSPascalCompilerErrorType; const Param: tbtString): TPSPascalCompilerMessage;
var
  n: TPSPascalCompilerError;
begin
  N := TPSPascalCompilerError.Create;
  n.FError := e;
  n.SetParserPos(FParser);
  {$IFNDEF PS_USESSUPPORT}
  n.FModuleName := Module;
  {$ELSE}
  if Module <> '' then
    n.FModuleName := Module
  else
    n.FModuleName := fModule;
  {$ENDIF}
  n.FParam := Param;
  FMessages.Add(n);
  Result := n;
end;

function TPSPascalCompiler.MakeWarning(const Module: tbtString; E:
  TPSPascalCompilerWarningType; const Param: tbtString): TPSPascalCompilerMessage;
var
  n: TPSPascalCompilerWarning;
begin
  N := TPSPascalCompilerWarning.Create;
  n.FWarning := e;
  n.SetParserPos(FParser);
  n.FModuleName := Module;
  n.FParam := Param;
  FMessages.Add(n);
  Result := n;
end;

procedure TPSPascalCompiler.Clear;
var
  l: Longint;
begin
  FDebugOutput := '';
  FOutput := '';
  for l := 0 to FMessages.Count - 1 do
    TPSPascalCompilerMessage(FMessages[l]).Free;
  FMessages.Clear;
  for L := FAutoFreeList.Count -1 downto 0 do
  begin
    TObject(FAutoFreeList[l]).Free;
  end;
  FAutoFreeList.Clear;
end;

procedure CopyVariantContents(Src, Dest: PIfRVariant);
begin
  case src.FType.BaseType of
    btu8, bts8: dest^.tu8 := src^.tu8;
    btu16, bts16: dest^.tu16 := src^.tu16;
    btenum, btu32, bts32: dest^.tu32 := src^.tu32;
    btsingle: Dest^.tsingle := src^.tsingle;
    btdouble: Dest^.tdouble := src^.tdouble;
    btextended: Dest^.textended := src^.textended;
    btCurrency: Dest^.tcurrency := Src^.tcurrency;
    btchar: Dest^.tchar := src^.tchar;
    {$IFNDEF PS_NOINT64}bts64: dest^.ts64 := src^.ts64;{$ENDIF}
    btset, btstring: tbtstring(dest^.tstring) := tbtstring(src^.tstring);
    {$IFNDEF PS_NOWIDESTRING}
    btunicodestring: tbtunicodestring(dest^.tunistring) := tbtunicodestring(src^.tunistring);
    btwidestring: tbtwidestring(dest^.twidestring) := tbtwidestring(src^.twidestring);
    btwidechar: Dest^.twidechar := src^.twidechar;
    {$ENDIF}
  end;
end;

function DuplicateVariant(Src: PIfRVariant): PIfRVariant;
begin
  New(Result);
  FillChar(Result^, SizeOf(TIfRVariant), 0);
  CopyVariantContents(Src, Result);
end;


procedure InitializeVariant(Vari: PIfRVariant; FType: TPSType);
begin
  FillChar(vari^, SizeOf(TIfRVariant), 0);
  if FType.BaseType = btSet then
  begin
    SetLength(tbtstring(vari^.tstring), TPSSetType(FType).ByteSize);
    fillchar(tbtstring(vari^.tstring)[1], length(tbtstring(vari^.tstring)), 0);
  end;
  vari^.FType := FType;
end;

function NewVariant(FType: TPSType): PIfRVariant;
begin
  New(Result);
  InitializeVariant(Result, FType);
end;

procedure FinalizeA(var s: tbtString); overload; begin s := ''; end;
{$IFNDEF PS_NOWIDESTRING}
procedure FinalizeW(var s: tbtwidestring); overload; begin s := ''; end;
procedure FinalizeU(var s: tbtunicodestring); overload; begin s := ''; end;
{$ENDIF}
procedure FinalizeVariant(var p: TIfRVariant);
begin
  if (p.FType.BaseType = btString) or (p.FType.basetype = btSet) then
    finalizeA(tbtstring(p.tstring))
  {$IFNDEF PS_NOWIDESTRING}
  else if p.FType.BaseType = btWideString then
    finalizeW(tbtWideString(p.twidestring)) // tbtwidestring
  else if p.FType.BaseType = btUnicodeString then
    finalizeU(tbtUnicodeString(p.tunistring)); // tbtwidestring
  {$ENDIF}
end;

procedure DisposeVariant(p: PIfRVariant);
begin
  if p <> nil then
  begin
    FinalizeVariant(p^);
    Dispose(p);
  end;
end;



function TPSPascalCompiler.GetTypeCopyLink(p: TPSType): TPSType;
begin
  if p = nil then
    Result := nil
  else
  if p.BaseType = BtTypeCopy then
  begin
    Result := TPSTypeLink(p).LinkTypeNo;
  end else Result := p;
end;

function IsIntType(b: TPSBaseType): Boolean;
begin
  case b of
    btU8, btS8, btU16, btS16, btU32, btS32{$IFNDEF PS_NOINT64}, btS64{$ENDIF}: Result := True;
  else
    Result := False;
  end;
end;

function IsCharType(b: TPSBaseType): Boolean;
begin
  case b of
    btChar{$IFNDEF PS_NOWIDESTRING}, btWideChar{$ENDIF}: Result := True;
  else
    Result := False;
  end;
end;

function IsStringType(b: TPSBaseType): Boolean;
begin
  case b of
    btString{$IFNDEF PS_NOWIDESTRING}, btWideString, btUnicodeString{$ENDIF}: Result := True;
  else
    Result := False;
  end;
end;

function IsStringOrCharType(b: TPSBaseType): Boolean;
begin
  Result := IsStringType(b) or IsCharType(b);
end;

function IsRealType(b: TPSBaseType): Boolean;
begin
  case b of
    btSingle, btDouble, btCurrency, btExtended: Result := True;
  else
    Result := False;
  end;
end;

function IsIntRealType(b: TPSBaseType): Boolean;
begin
  case b of
    btSingle, btDouble, btCurrency, btExtended, btU8, btS8, btU16, btS16, btU32, btS32{$IFNDEF PS_NOINT64}, btS64{$ENDIF}:
      Result := True;
  else
    Result := False;
  end;

end;

function DiffRec(p1, p2: TPSSubItem): Boolean;
begin
  if p1.ClassType = p2.ClassType then
  begin
    if P1.ClassType = TPSSubNumber then
      Result := TPSSubNumber(p1).SubNo <> TPSSubNumber(p2).SubNo
    else if P1.ClassType = TPSSubValue then
      Result := TPSSubValue(p1).SubNo <> TPSSubValue(p2).SubNo
    else
      Result := False;
  end else Result := True;
end;

function SameReg(x1, x2: TPSValue): Boolean;
var
  I: Longint;
begin
  if (x1.ClassType = x2.ClassType) and (X1 is TPSValueVar) then
  begin
    if
    ((x1.ClassType = TPSValueGlobalVar) and (TPSValueGlobalVar(x1).GlobalVarNo = TPSValueGlobalVar(x2).GlobalVarNo)) or
    ((x1.ClassType = TPSValueLocalVar) and (TPSValueLocalVar(x1).LocalVarNo = TPSValueLocalVar(x2).LocalVarNo)) or
    ((x1.ClassType = TPSValueParamVar) and (TPSValueParamVar(x1).ParamNo = TPSValueParamVar(x2).ParamNo)) or
    ((x1.ClassType = TPSValueAllocatedStackVar) and (TPSValueAllocatedStackVar(x1).LocalVarNo = TPSValueAllocatedStackVar(x2).LocalVarNo)) then
    begin
      if TPSValueVar(x1).GetRecCount <> TPSValueVar(x2).GetRecCount then
      begin
        Result := False;
        exit;
      end;
      for i := 0 to TPSValueVar(x1).GetRecCount -1 do
      begin
        if DiffRec(TPSValueVar(x1).RecItem[i], TPSValueVar(x2).RecItem[i]) then
        begin
          Result := False;
          exit;
        end;
      end;
      Result := True;
    end else Result := False;
  end
  else
    Result := False;
end;

function GetUInt(Src: PIfRVariant; var s: Boolean): Cardinal;
begin
  case Src.FType.BaseType of
    btU8: Result := Src^.tu8;
    btS8: Result := Src^.ts8;
    btU16: Result := Src^.tu16;
    btS16: Result := Src^.ts16;
    btU32: Result := Src^.tu32;
    btS32: Result := Src^.ts32;
    {$IFNDEF PS_NOINT64}
    bts64: Result := src^.ts64;
    {$ENDIF}
    btChar: Result := ord(Src^.tchar);
    {$IFNDEF PS_NOWIDESTRING}
    btWideChar: Result := ord(tbtwidechar(src^.twidechar));
    {$ENDIF}
    btEnum: Result := src^.tu32;
  else
    begin
      s := False;
      Result := 0;
    end;
  end;
end;

function GetInt(Src: PIfRVariant; var s: Boolean): Longint;
begin
  case Src.FType.BaseType of
    btU8: Result := Src^.tu8;
    btS8: Result := Src^.ts8;
    btU16: Result := Src^.tu16;
    btS16: Result := Src^.ts16;
    btU32: Result := Src^.tu32;
    btS32: Result := Src^.ts32;
    {$IFNDEF PS_NOINT64}
    bts64: Result := src^.ts64;
    {$ENDIF}
    btChar: Result := ord(Src^.tchar);
    {$IFNDEF PS_NOWIDESTRING}
    btWideChar: Result := ord(tbtwidechar(src^.twidechar));
    {$ENDIF}
    btEnum: Result := src^.tu32;
  else
    begin
      s := False;
      Result := 0;
    end;
  end;
end;
{$IFNDEF PS_NOINT64}
function GetInt64(Src: PIfRVariant; var s: Boolean): Int64;
begin
  case Src.FType.BaseType of
    btU8: Result := Src^.tu8;
    btS8: Result := Src^.ts8;
    btU16: Result := Src^.tu16;
    btS16: Result := Src^.ts16;
    btU32: Result := Src^.tu32;
    btS32: Result := Src^.ts32;
    bts64: Result := src^.ts64;
    btChar: Result := ord(Src^.tchar);
    {$IFNDEF PS_NOWIDESTRING}
    btWideChar: Result := ord(tbtwidechar(src^.twidechar));
    {$ENDIF}
    btEnum: Result := src^.tu32;
  else
    begin
      s := False;
      Result := 0;
    end;
  end;
end;
{$ENDIF}

function GetReal(Src: PIfRVariant; var s: Boolean): Extended;
begin
  case Src.FType.BaseType of
    btU8: Result := Src^.tu8;
    btS8: Result := Src^.ts8;
    btU16: Result := Src^.tu16;
    btS16: Result := Src^.ts16;
    btU32: Result := Src^.tu32;
    btS32: Result := Src^.ts32;
    {$IFNDEF PS_NOINT64}
    bts64: Result := src^.ts64;
    {$ENDIF}
    btChar: Result := ord(Src^.tchar);
    {$IFNDEF PS_NOWIDESTRING}
    btWideChar: Result := ord(tbtwidechar(src^.twidechar));
    {$ENDIF}
    btSingle: Result := Src^.tsingle;
    btDouble: Result := Src^.tdouble;
    btCurrency: Result := SRc^.tcurrency;
    btExtended: Result := Src^.textended;
  else
    begin
      s := False;
      Result := 0;
    end;
  end;
end;

function GetString(Src: PIfRVariant; var s: Boolean): tbtString;
begin
  case Src.FType.BaseType of
    btChar: Result := Src^.tchar;
    btString: Result := tbtstring(src^.tstring);
    {$IFNDEF PS_NOWIDESTRING}
    btWideChar: Result := tbtstring(src^.twidechar);
    btWideString: Result := tbtstring(tbtWideString(src^.twidestring));
    btUnicodeString: Result := tbtstring(tbtUnicodeString(src^.tunistring));
    {$ENDIF}
  else
    begin
      s := False;
      Result := '';
    end;
  end;
end;

{$IFNDEF PS_NOWIDESTRING}
function TPSPascalCompiler.GetWideString(Src: PIfRVariant; var s: Boolean): tbtwidestring;
begin
  case Src.FType.BaseType of
    btChar: Result := tbtWidestring(Src^.tchar);
    btString: Result := tbtWidestring(tbtstring(src^.tstring));
    btWideChar: Result := src^.twidechar;
    btWideString: Result := tbtWideString(src^.twidestring);
    btUnicodeString: result := tbtUnicodeString(src^.tunistring);
  else
    begin
      s := False;
      Result := '';
    end;
  end;
end;
function TPSPascalCompiler.GetUnicodeString(Src: PIfRVariant; var s: Boolean): tbtunicodestring;
begin
  case Src.FType.BaseType of
    btChar: Result := tbtunicodestring(Src^.tchar);
    btString: Result := tbtunicodestring(tbtstring(src^.tstring));
    btWideChar: Result := src^.twidechar;
    btWideString: Result := tbtWideString(src^.twidestring);
    btUnicodeString: result := tbtUnicodeString(src^.tunistring);
  else
    begin
      s := False;
      Result := '';
    end;
  end;
end;
{$ENDIF}

function ab(b: Longint): Longint;
begin
  ab := Longint(b = 0);
end;

procedure Set_Union(Dest, Src: PByteArray; ByteSize: Integer);
var
  i: Longint;
begin
  for i := ByteSize -1 downto 0 do
    Dest^[i] := Dest^[i] or Src^[i];
end;

procedure Set_Diff(Dest, Src: PByteArray; ByteSize: Integer);
var
  i: Longint;
begin
  for i := ByteSize -1 downto 0 do
    Dest^[i] := Dest^[i] and not Src^[i];
end;

procedure Set_Intersect(Dest, Src: PByteArray; ByteSize: Integer);
var
  i: Longint;
begin
  for i := ByteSize -1 downto 0 do
    Dest^[i] := Dest^[i] and Src^[i];
end;

procedure Set_Subset(Dest, Src: PByteArray; ByteSize: Integer; var Val: Boolean);
var
  i: Integer;
begin
  for i := ByteSize -1 downto 0 do
  begin
    if not (Src^[i] and Dest^[i] = Dest^[i]) then
    begin
      Val := False;
      exit;
    end;
  end;
  Val := True;
end;

procedure Set_Equal(Dest, Src: PByteArray; ByteSize: Integer; var Val: Boolean);
var
  i: Longint;
begin
  for i := ByteSize -1 downto 0 do
  begin
    if Dest^[i] <> Src^[i] then
    begin
      Val := False;
      exit;
    end;
  end;
  val := True;
end;

procedure Set_membership(Item: Longint; Src: PByteArray; var Val: Boolean);
begin
  Val := (Src^[Item shr 3] and (1 shl (Item and 7))) <> 0;
end;

procedure Set_MakeMember(Item: Longint; Src: PByteArray);
begin
  Src^[Item shr 3] := Src^[Item shr 3] or (1 shl (Item and 7));
end;

procedure ConvertToBoolean(SE: TPSPascalCompiler; FUseUsedTypes: Boolean; var1: PIFRVariant; b: Boolean);
begin
  FinalizeVariant(var1^);
  if FUseUsedTypes then
    Var1^.FType := se.at2ut(se.FDefaultBoolType)
  else
    Var1^.FType := Se.FDefaultBoolType;
  var1^.tu32 := Ord(b);
end;

procedure ConvertToString(SE: TPSPascalCompiler; FUseUsedTypes: Boolean; var1: PIFRVariant; const s: tbtString);
var
  atype: TPSType;
begin
  FinalizeVariant(var1^);
  atype := se.FindBaseType(btString);
  if FUseUsedTypes then
    InitializeVariant(var1, se.at2ut(atype))
  else
    InitializeVariant(var1, atype);
  tbtstring(var1^.tstring) := s;
end;
{$IFNDEF PS_NOWIDESTRING}
procedure ConvertToUnicodeString(SE: TPSPascalCompiler; FUseUsedTypes: Boolean; var1: PIFRVariant; const s: tbtunicodestring);
var
  atype: TPSType;
begin
  FinalizeVariant(var1^);
  atype := se.FindBaseType(btUnicodeString);
  if FUseUsedTypes then
    InitializeVariant(var1, se.at2ut(atype))
  else
    InitializeVariant(var1, atype);
  tbtunicodestring(var1^.tunistring) := s;
end;
{$ENDIF}
procedure ConvertToFloat(SE: TPSPascalCompiler; FUseUsedTypes: Boolean; var1: PIfRVariant; NewType: TPSType);
var
  vartemp: PIfRVariant;
  b: Boolean;
begin
  New(vartemp);
  b := false;
  if FUseUsedTypes then
    NewType := se.at2ut(NewType);
  InitializeVariant(vartemp, var1.FType);
  CopyVariantContents(var1, vartemp);
  FinalizeVariant(var1^);
  InitializeVariant(var1, newtype);
  case var1.ftype.basetype of
    btSingle:
      begin
        if (vartemp.ftype.BaseType = btu8) or (vartemp.ftype.BaseType = btu16) or (vartemp.ftype.BaseType = btu32) then
          var1^.tsingle := GetUInt(vartemp, b)
        else
          var1^.tsingle := GetInt(vartemp, b)
      end;
    btDouble:
      begin
        if (vartemp.ftype.BaseType = btu8) or (vartemp.ftype.BaseType = btu16) or (vartemp.ftype.BaseType = btu32) then
          var1^.tdouble := GetUInt(vartemp, b)
        else
          var1^.tdouble := GetInt(vartemp, b)
      end;
    btExtended:
      begin
        if (vartemp.ftype.BaseType = btu8) or (vartemp.ftype.BaseType = btu16) or (vartemp.ftype.BaseType = btu32) then
          var1^.textended:= GetUInt(vartemp, b)
        else
          var1^.textended:= GetInt(vartemp, b)
      end;
    btCurrency:
      begin
        if (vartemp.ftype.BaseType = btu8) or (vartemp.ftype.BaseType = btu16) or (vartemp.ftype.BaseType = btu32) then
          var1^.tcurrency:= GetUInt(vartemp, b)
        else
          var1^.tcurrency:= GetInt(vartemp, b)
      end;
  end;
  DisposeVariant(vartemp);
end;


function TPSPascalCompiler.IsCompatibleType(p1, p2: TPSType; Cast: Boolean): Boolean;
begin
  if
    ((p1.BaseType = btProcPtr) and (p2 = p1)) or
    (p1.BaseType = btPointer) or
    (p2.BaseType = btPointer) or
    ((p1.BaseType = btNotificationVariant) or (p1.BaseType = btVariant)) or
    ((p2.BaseType = btNotificationVariant) or (p2.BaseType = btVariant))  or
    (IsIntType(p1.BaseType) and IsIntType(p2.BaseType)) or
    (IsRealType(p1.BaseType) and IsIntRealType(p2.BaseType)) or
    (((p1.basetype = btPchar) or (p1.BaseType = btString)) and ((p2.BaseType = btString) or (p2.BaseType = btPchar))) or
    (((p1.basetype = btPchar) or (p1.BaseType = btString)) and (p2.BaseType = btChar)) or
    (((p1.BaseType = btArray) or (p1.BaseType = btStaticArray)) and (
    (p2.BaseType = btArray) or (p2.BaseType = btStaticArray)) and IsCompatibleType(TPSArrayType(p1).ArrayTypeNo, TPSArrayType(p2).ArrayTypeNo, False)) or
    ((p1.BaseType = btChar) and (p2.BaseType = btChar)) or
    ((p1.BaseType = btSet) and (p2.BaseType = btSet)) or
    {$IFNDEF PS_NOWIDESTRING}
    ((p1.BaseType = btChar) and (p2.BaseType = btWideChar)) or 
    ((p1.BaseType = btWideChar) and (p2.BaseType = btChar)) or
    ((p1.BaseType = btWideChar) and (p2.BaseType = btWideChar)) or
    ((p1.BaseType = btWidestring) and (p2.BaseType = btChar)) or
    ((p1.BaseType = btWidestring) and (p2.BaseType = btWideChar)) or
    ((p1.BaseType = btWidestring) and ((p2.BaseType = btString) or (p2.BaseType = btPchar) or (p2.BaseType = btUnicodeString))) or
    ((p1.BaseType = btWidestring) and ((p2.BaseType = btWidestring))) or
    ((p1.BaseType = btUnicodeString) and (p2.BaseType = btChar)) or
    ((p1.BaseType = btUnicodeString) and (p2.BaseType = btWideChar)) or
    ((p1.BaseType = btUnicodeString) and ((p2.BaseType = btString) or (p2.BaseType = btPchar) or (p2.BaseType = btUnicodeString))) or
    ((p1.BaseType = btUnicodeString) and (p2.BaseType = btWidestring)) or
    (((p1.basetype = btPchar) or (p1.BaseType = btString)) and (p2.BaseType = btWideString)or (p2.BaseType = btUnicodeString)) or
    (((p1.basetype = btPchar) or (p1.BaseType = btString)) and (p2.BaseType = btWidechar)) or
    (((p1.basetype = btPchar) or (p1.BaseType = btString)) and (p2.BaseType = btchar)) or
    {$ENDIF}
    ((p1.BaseType = btRecord) and (p2.BaseType = btrecord) and (not IsVarInCompatible(p1, p2))) or
    ((p1.BaseType = btEnum) and (p2.BaseType = btEnum)) or
    (Cast and IsIntType(P1.BaseType) and (p2.baseType = btEnum)) or
    (Cast and (p1.baseType = btEnum) and IsIntType(P2.BaseType))
    then
    Result := True
  // nx change start - allow casting class -> integer and vice versa
  else if p1.BaseType = btclass then
    Result := TPSClassType(p1).cl.IsCompatibleWith(p2) or (p2.BaseType in [btU32, btS32])
  else if (p1.BaseType in [btU32, btS32]) then
    Result := (p2.BaseType = btClass)
  // nx change end
{$IFNDEF PS_NOINTERFACES}
  else if p1.BaseType = btInterface then
    Result := TPSInterfaceType(p1).Intf.IsCompatibleWith(p2)
{$ENDIF}
  else if ((p1.BaseType = btExtClass) and (p2.BaseType = btExtClass)) then
  begin
    Result := TPSUndefinedClassType(p1).ExtClass.IsCompatibleWith(TPSUndefinedClassType(p2).ExtClass);
  end
  else
    Result := False;
end;


function TPSPascalCompiler.PreCalc(FUseUsedTypes: Boolean; Var1Mod: Byte; var1: PIFRVariant; Var2Mod: Byte; Var2: PIfRVariant; Cmd: TPSBinOperatorType; Pos, Row, Col: Cardinal): Boolean;
  { var1=dest, var2=src }
var
  b: Boolean;

begin
  Result := True;
  try
    if (IsRealType(var2.FType.BaseType) and IsIntType(var1.FType.BaseType)) then
      ConvertToFloat(Self, FUseUsedTypes, var1, var2^.FType);
    case Cmd of
      otAdd:
        begin { + }
          case var1.FType.BaseType of
            btU8: var1^.tu8 := var1^.tu8 + GetUint(Var2, Result);
            btS8: var1^.ts8 := var1^.ts8 + GetInt(Var2, Result);
            btU16: var1^.tu16 := var1^.tu16 + GetUint(Var2, Result);
            btS16: var1^.ts16 := var1^.ts16 + Getint(Var2, Result);
            btEnum, btU32: var1^.tu32 := var1^.tu32 + GetUint(Var2, Result);
            btS32: var1^.ts32 := var1^.ts32 + Getint(Var2, Result);
            {$IFNDEF PS_NOINT64}btS64: var1^.ts64 := var1^.ts64 + GetInt64(Var2, Result); {$ENDIF}
            btSingle: var1^.tsingle := var1^.tsingle + GetReal( Var2, Result);
            btDouble: var1^.tdouble := var1^.tdouble + GetReal( Var2, Result);
            btExtended: var1^.textended := var1^.textended + GetReal( Var2, Result);
            btCurrency: var1^.tcurrency := var1^.tcurrency + GetReal( Var2, Result);
            btSet:
              begin
                if (var1.FType = var2.FType) then
                begin
                  Set_Union(var1.tstring, var2.tstring, TPSSetType(var1.FType).ByteSize);
                end else Result := False;
              end;
            btChar:
              begin
                ConvertToString(Self, FUseUsedTypes, var1, getstring(Var1, b)+getstring(Var2, b));
              end;
            btString: tbtstring(var1^.tstring) := tbtstring(var1^.tstring) + GetString(Var2, Result);
            {$IFNDEF PS_NOWIDESTRING}
            btwideString: tbtwidestring(var1^.twidestring) := tbtwidestring(var1^.twidestring) + GetWideString(Var2, Result);
            btUnicodeString: tbtunicodestring(var1^.tunistring) := tbtunicodestring(var1^.tunistring) + GetUnicodeString(Var2, Result);
            btWidechar:
              begin
                ConvertToUnicodeString(Self, FUseUsedTypes, var1, GetUnicodeString(Var1, b)+GetUnicodeString(Var2, b));
              end;
            {$ENDIF}
            else Result := False;
          end;
        end;
      otSub:
        begin { - }
          case Var1.FType.BaseType of
            btU8: var1^.tu8 := var1^.tu8 - GetUint(Var2, Result);
            btS8: var1^.ts8 := var1^.ts8 - Getint(Var2, Result);
            btU16: var1^.tu16 := var1^.tu16 - GetUint(Var2, Result);
            btS16: var1^.ts16 := var1^.ts16 - Getint(Var2, Result);
            btEnum, btU32: var1^.tu32 := var1^.tu32 - GetUint(Var2, Result);
            btS32: var1^.ts32 := var1^.ts32 - Getint(Var2, Result);
            {$IFNDEF PS_NOINT64}btS64: var1^.ts64 := var1^.ts64 - GetInt64(Var2, Result); {$ENDIF}
            btSingle: var1^.tsingle := var1^.tsingle - GetReal( Var2, Result);
            btDouble: var1^.tdouble := var1^.tdouble - GetReal(Var2, Result);
            btExtended: var1^.textended := var1^.textended - GetReal(Var2, Result);
            btCurrency: var1^.tcurrency := var1^.tcurrency - GetReal( Var2, Result);
            btSet:
              begin
                if (var1.FType = var2.FType) then
                begin
                  Set_Diff(var1.tstring, var2.tstring, TPSSetType(var1.FType).ByteSize);
                end else Result := False;
              end;
            else Result := False;
          end;
        end;
      otMul:
        begin { * }
          case Var1.FType.BaseType of
            btU8: var1^.tu8 := var1^.tu8 * GetUint(Var2, Result);
            btS8: var1^.ts8 := var1^.ts8 * Getint(Var2, Result);
            btU16: var1^.tu16 := var1^.tu16 * GetUint(Var2, Result);
            btS16: var1^.ts16 := var1^.ts16 * Getint(Var2, Result);
            btU32: var1^.tu32 := var1^.tu32 * GetUint(Var2, Result);
            btS32: var1^.ts32 := var1^.ts32 * Getint(Var2, Result);
            {$IFNDEF PS_NOINT64}btS64: var1^.ts64 := var1^.ts64 * GetInt64(Var2, Result); {$ENDIF}
            btSingle: var1^.tsingle := var1^.tsingle * GetReal(Var2, Result);
            btDouble: var1^.tdouble := var1^.tdouble * GetReal(Var2, Result);
            btExtended: var1^.textended := var1^.textended * GetReal( Var2, Result);
            btCurrency: var1^.tcurrency := var1^.tcurrency * GetReal( Var2, Result);
            btSet:
              begin
                if (var1.FType = var2.FType) then
                begin
                  Set_Intersect(var1.tstring, var2.tstring, TPSSetType(var1.FType).ByteSize);
                end else Result := False;
              end;
            else Result := False;
          end;
        end;
{$IFDEF PS_DELPHIDIV}
      otDiv:
        begin { / }
          if IsIntType(var1.FType.BaseType) then
            ConvertToFloat(self, FUseUsedTypes, var1, Self.FindType('EXTENDED'));
          case Var1.FType.BaseType of
            btSingle: var1^.tsingle := var1^.tsingle / GetReal( Var2, Result);
            btDouble: var1^.tdouble := var1^.tdouble / GetReal( Var2, Result);
            btExtended: var1^.textended := var1^.textended / GetReal( Var2, Result);
            btCurrency: var1^.tcurrency := var1^.tcurrency / GetReal( Var2, Result);
            else Result := False;
          end;
        end;
      otIntDiv:
        begin { / }
          case Var1.FType.BaseType of
            btU8: var1^.tu8 := var1^.tu8 div GetUint(Var2, Result);
            btS8: var1^.ts8 := var1^.ts8 div Getint(Var2, Result);
            btU16: var1^.tu16 := var1^.tu16 div GetUint(Var2, Result);
            btS16: var1^.ts16 := var1^.ts16 div Getint(Var2, Result);
            btU32: var1^.tu32 := var1^.tu32 div GetUint(Var2, Result);
            btS32: var1^.ts32 := var1^.ts32 div Getint(Var2, Result);
            {$IFNDEF PS_NOINT64}btS64: var1^.ts64 := var1^.ts64 div GetInt64(Var2, Result); {$ENDIF}
            else Result := False;
          end;
        end;
{$ELSE}
      otDiv, otIntDiv:
        begin { / }
          case Var1.FType.BaseType of
            btU8: var1^.tu8 := var1^.tu8 div GetUint(Var2, Result);
            btS8: var1^.ts8 := var1^.ts8 div Getint(Var2, Result);
            btU16: var1^.tu16 := var1^.tu16 div GetUint(Var2, Result);
            btS16: var1^.ts16 := var1^.ts16 div Getint(Var2, Result);
            btU32: var1^.tu32 := var1^.tu32 div GetUint(Var2, Result);
            btS32: var1^.ts32 := var1^.ts32 div Getint(Var2, Result);
            {$IFNDEF PS_NOINT64}btS64: var1^.ts64 := var1^.ts64 div GetInt64(Var2, Result); {$ENDIF}
            btSingle: var1^.tsingle := var1^.tsingle / GetReal( Var2, Result);
            btDouble: var1^.tdouble := var1^.tdouble / GetReal( Var2, Result);
            btExtended: var1^.textended := var1^.textended / GetReal( Var2, Result);
            btCurrency: var1^.tcurrency := var1^.tcurrency / GetReal( Var2, Result);
            else Result := False;
          end;
        end;
{$ENDIF}
      otMod:
        begin { MOD }
          case Var1.FType.BaseType of
            btU8: var1^.tu8 := var1^.tu8 mod GetUint(Var2, Result);
            btS8: var1^.ts8 := var1^.ts8 mod Getint(Var2, Result);
            btU16: var1^.tu16 := var1^.tu16 mod GetUint(Var2, Result);
            btS16: var1^.ts16 := var1^.ts16 mod Getint(Var2, Result);
            btU32: var1^.tu32 := var1^.tu32 mod GetUint(Var2, Result);
            btS32: var1^.ts32 := var1^.ts32 mod Getint(Var2, Result);
            {$IFNDEF PS_NOINT64}btS64: var1^.ts64 := var1^.ts64 mod GetInt64(Var2, Result); {$ENDIF}
            else Result := False;
          end;
        end;
      otshl:
        begin { SHL }
          case Var1.FType.BaseType of
            btU8: var1^.tu8 := var1^.tu8 shl GetUint(Var2, Result);
            btS8: var1^.ts8 := var1^.ts8 shl Getint(Var2, Result);
            btU16: var1^.tu16 := var1^.tu16 shl GetUint(Var2, Result);
            btS16: var1^.ts16 := var1^.ts16 shl Getint(Var2, Result);
            btU32: var1^.tu32 := var1^.tu32 shl GetUint(Var2, Result);
            btS32: var1^.ts32 := var1^.ts32 shl Getint(Var2, Result);
            {$IFNDEF PS_NOINT64}btS64: var1^.ts64 := var1^.ts64 shl GetInt64(Var2, Result); {$ENDIF}
            else Result := False;
          end;
        end;
      otshr:
        begin { SHR }
          case Var1.FType.BaseType of
            btU8: var1^.tu8 := var1^.tu8 shr GetUint(Var2, Result);
            btS8: var1^.ts8 := var1^.ts8 shr Getint(Var2, Result);
            btU16: var1^.tu16 := var1^.tu16 shr GetUint(Var2, Result);
            btS16: var1^.ts16 := var1^.ts16 shr Getint(Var2, Result);
            btU32: var1^.tu32 := var1^.tu32 shr GetUint(Var2, Result);
            btS32: var1^.ts32 := var1^.ts32 shr Getint(Var2, Result);
            {$IFNDEF PS_NOINT64}btS64: var1^.ts64 := var1^.ts64 shr GetInt64( Var2, Result); {$ENDIF}
            else Result := False;
          end;
        end;
      otAnd:
        begin { AND }
          case Var1.FType.BaseType of
            btU8: var1^.tu8 := var1^.tu8 and GetUint(Var2, Result);
            btS8: var1^.ts8 := var1^.ts8 and Getint(Var2, Result);
            btU16: var1^.tu16 := var1^.tu16 and GetUint(Var2, Result);
            btS16: var1^.ts16 := var1^.ts16 and Getint(Var2, Result);
            btU32: var1^.tu32 := var1^.tu32 and GetUint(Var2, Result);
            btS32: var1^.ts32 := var1^.ts32 and Getint(Var2, Result);
            btEnum: var1^.ts32 := var1^.ts32 and Getint(Var2, Result);
            {$IFNDEF PS_NOINT64}btS64: var1^.ts64 := var1^.ts64 and GetInt64(Var2, Result); {$ENDIF}
            else Result := False;
          end;
        end;
      otor:
        begin { OR }
          case Var1.FType.BaseType of
            btU8: var1^.tu8 := var1^.tu8 or GetUint(Var2, Result);
            btS8: var1^.ts8 := var1^.ts8 or Getint(Var2, Result);
            btU16: var1^.tu16 := var1^.tu16 or GetUint(Var2, Result);
            btS16: var1^.ts16 := var1^.ts16 or Getint(Var2, Result);
            btU32: var1^.tu32 := var1^.tu32 or GetUint(Var2, Result);
            btS32: var1^.ts32 := var1^.ts32 or Getint(Var2, Result);
            {$IFNDEF PS_NOINT64}btS64: var1^.ts64 := var1^.ts64 or GetInt64(Var2, Result); {$ENDIF}
            btEnum: var1^.ts32 := var1^.ts32 or Getint(Var2, Result);
            else Result := False;
          end;
        end;
      otxor:
        begin { XOR }
          case Var1.FType.BaseType of
            btU8: var1^.tu8 := var1^.tu8 xor GetUint(Var2, Result);
            btS8: var1^.ts8 := var1^.ts8 xor Getint(Var2, Result);
            btU16: var1^.tu16 := var1^.tu16 xor GetUint(Var2, Result);
            btS16: var1^.ts16 := var1^.ts16 xor Getint(Var2, Result);
            btU32: var1^.tu32 := var1^.tu32 xor GetUint(Var2, Result);
            btS32: var1^.ts32 := var1^.ts32 xor Getint(Var2, Result);
            {$IFNDEF PS_NOINT64}btS64: var1^.ts64 := var1^.ts64 xor GetInt64(Var2, Result); {$ENDIF}
            btEnum: var1^.ts32 := var1^.ts32 xor Getint(Var2, Result);
            else Result := False;
          end;
        end;
      otGreaterEqual:
        begin { >= }
          case Var1.FType.BaseType of
            btU8: b := var1^.tu8 >= GetUint(Var2, Result);
            btS8: b := var1^.ts8 >= Getint(Var2, Result);
            btU16: b := var1^.tu16 >= GetUint(Var2, Result);
            btS16: b := var1^.ts16 >= Getint(Var2, Result);
            btU32: b := var1^.tu32 >= GetUint(Var2, Result);
            btS32: b := var1^.ts32 >= Getint(Var2, Result);
            {$IFNDEF PS_NOINT64}btS64: b := var1^.ts64 >= GetInt64(Var2, Result); {$ENDIF}
            btSingle: b := var1^.tsingle >= GetReal( Var2, Result);
            btDouble: b := var1^.tdouble >= GetReal( Var2, Result);
            btExtended: b := var1^.textended >= GetReal( Var2, Result);
            btCurrency: b := var1^.tcurrency >= GetReal( Var2, Result);
            btSet:
              begin
                if (var1.FType = var2.FType) then
                begin
                  Set_Subset(var2.tstring, var1.tstring, TPSSetType(var1.FType).ByteSize, b);
                end else Result := False;
              end;
          else
            Result := False;
          end;
          ConvertToBoolean(Self, FUseUsedTypes, Var1, b);
        end;
      otLessEqual:
        begin { <= }
          case Var1.FType.BaseType of
            btU8: b := var1^.tu8 <= GetUint(Var2, Result);
            btS8: b := var1^.ts8 <= Getint(Var2, Result);
            btU16: b := var1^.tu16 <= GetUint(Var2, Result);
            btS16: b := var1^.ts16 <= Getint(Var2, Result);
            btU32: b := var1^.tu32 <= GetUint(Var2, Result);
            btS32: b := var1^.ts32 <= Getint(Var2, Result);
            {$IFNDEF PS_NOINT64}btS64: b := var1^.ts64 <= GetInt64(Var2, Result); {$ENDIF}
            btSingle: b := var1^.tsingle <= GetReal( Var2, Result);
            btDouble: b := var1^.tdouble <= GetReal( Var2, Result);
            btExtended: b := var1^.textended <= GetReal( Var2, Result);
            btCurrency: b := var1^.tcurrency <= GetReal( Var2, Result);
            btSet:
              begin
                if (var1.FType = var2.FType) then
                begin
                  Set_Subset(var1.tstring, var2.tstring, TPSSetType(var1.FType).ByteSize, b);
                end else Result := False;
              end;
          else
            Result := False;
          end;
          ConvertToBoolean(Self, FUseUsedTypes, Var1, b);
        end;
      otGreater:
        begin { > }
          case Var1.FType.BaseType of
            btU8: b := var1^.tu8 > GetUint(Var2, Result);
            btS8: b := var1^.ts8 > Getint(Var2, Result);
            btU16: b := var1^.tu16 > GetUint(Var2, Result);
            btS16: b := var1^.ts16 > Getint(Var2, Result);
            btU32: b := var1^.tu32 > GetUint(Var2, Result);
            btS32: b := var1^.ts32 > Getint(Var2, Result);
            {$IFNDEF PS_NOINT64}btS64: b := var1^.ts64 > GetInt64(Var2, Result); {$ENDIF}
            btSingle: b := var1^.tsingle > GetReal( Var2, Result);
            btDouble: b := var1^.tdouble > GetReal( Var2, Result);
            btExtended: b := var1^.textended > GetReal( Var2, Result);
            btCurrency: b := var1^.tcurrency > GetReal( Var2, Result);
          else
            Result := False;
          end;
          ConvertToBoolean(Self, FUseUsedTypes, Var1, b);
        end;
      otLess:
        begin { < }
          case Var1.FType.BaseType of
            btU8: b := var1^.tu8 < GetUint(Var2, Result);
            btS8: b := var1^.ts8 < Getint(Var2, Result);
            btU16: b := var1^.tu16 < GetUint(Var2, Result);
            btS16: b := var1^.ts16 < Getint(Var2, Result);
            btU32: b := var1^.tu32 < GetUint(Var2, Result);
            btS32: b := var1^.ts32 < Getint(Var2, Result);
            {$IFNDEF PS_NOINT64}btS64: b := var1^.ts64 < GetInt64(Var2, Result); {$ENDIF}
            btSingle: b := var1^.tsingle < GetReal( Var2, Result);
            btDouble: b := var1^.tdouble < GetReal( Var2, Result);
            btExtended: b := var1^.textended < GetReal( Var2, Result);
            btCurrency: b := var1^.tcurrency < GetReal( Var2, Result);
          else
            Result := False;
          end;
          ConvertToBoolean(Self, FUseUsedTypes, Var1, b);
        end;
      otNotEqual:
        begin { <> }
          case Var1.FType.BaseType of
            btU8: b := var1^.tu8 <> GetUint(Var2, Result);
            btS8: b := var1^.ts8 <> Getint(Var2, Result);
            btU16: b := var1^.tu16 <> GetUint(Var2, Result);
            btS16: b := var1^.ts16 <> Getint(Var2, Result);
            btU32: b := var1^.tu32 <> GetUint(Var2, Result);
            {$IFNDEF PS_NOINT64}btS64: b := var1^.ts64 <> GetInt64(Var2, Result); {$ENDIF}
            btS32: b := var1^.ts32 <> Getint(Var2, Result);
            btSingle: b := var1^.tsingle <> GetReal( Var2, Result);
            btDouble: b := var1^.tdouble <> GetReal( Var2, Result);
            btExtended: b := var1^.textended <> GetReal( Var2, Result);
            btCurrency: b := var1^.tcurrency <> GetReal( Var2, Result);
            btEnum: b := var1^.ts32 <> Getint(Var2, Result);
            btString: b := tbtstring(var1^.tstring) <> GetString(var2, Result);
            btChar: b := var1^.tchar <> GetString(var2, Result);
            {$IFNDEF PS_NOWIDESTRING}
            btWideString: b := tbtWideString(var1^.twidestring) <> GetWideString(var2, Result);
            btUnicodeString: b := tbtUnicodeString(var1^.tunistring) <> GetUnicodeString(var2, Result);
            btWideChar: b := var1^.twidechar <> GetUnicodeString(var2, Result);
            {$ENDIF}
            btSet:
              begin
                if (var1.FType = var2.FType) then
                begin
                  Set_Equal(var1.tstring, var2.tstring, TPSSetType(var1.FType).GetByteSize, b);
                  b := not b;
                end else Result := False;
              end;
          else
            Result := False;
          end;
          ConvertToBoolean(Self, FUseUsedTypes, Var1, b);
        end;
      otEqual:
        begin { = }
          case Var1.FType.BaseType of
            btU8: b := var1^.tu8 = GetUint(Var2, Result);
            btS8: b := var1^.ts8 = Getint(Var2, Result);
            btU16: b := var1^.tu16 = GetUint(Var2, Result);
            btS16: b := var1^.ts16 = Getint(Var2, Result);
            btU32: b := var1^.tu32 = GetUint(Var2, Result);
            btS32: b := var1^.ts32 = Getint(Var2, Result);
            {$IFNDEF PS_NOINT64}btS64: b := var1^.ts64 = GetInt64(Var2, Result); {$ENDIF}
            btSingle: b := var1^.tsingle = GetReal( Var2, Result);
            btDouble: b := var1^.tdouble = GetReal( Var2, Result);
            btExtended: b := var1^.textended = GetReal( Var2, Result);
            btCurrency: b := var1^.tcurrency = GetReal( Var2, Result);
            btEnum: b := var1^.ts32 = Getint(Var2, Result);
            btString: b := tbtstring(var1^.tstring) = GetString(var2, Result);
            btChar: b := var1^.tchar = GetString(var2, Result);
            {$IFNDEF PS_NOWIDESTRING}
            btWideString: b := tbtWideString(var1^.twidestring) = GetWideString(var2, Result);
            btUnicodeString: b := tbtUnicodeString(var1^.twidestring) = GetUnicodeString(var2, Result);
            btWideChar: b := var1^.twidechar = GetUnicodeString(var2, Result);
            {$ENDIF}
            btSet:
              begin
                if (var1.FType = var2.FType) then
                begin
                  Set_Equal(var1.tstring, var2.tstring, TPSSetType(var1.FType).ByteSize, b);
                end else Result := False;
              end;
          else
            Result := False;
          end;
          ConvertToBoolean(Self, FUseUsedTypes, Var1, b);
        end;
      otIn:
        begin
          if (var2.Ftype.BaseType = btset) and (TPSSetType(var2).SetType = Var1.FType) then
          begin
            Set_membership(GetUint(var1, result), var2.tstring, b);
          end else Result := False;
        end;
      else
        Result := False;
    end;
  except
    on E: EDivByZero do
    begin
      Result := False;
      MakeError('', ecDivideByZero, '');
      Exit;
    end;
    on E: EZeroDivide do
    begin
      Result := False;
      MakeError('', ecDivideByZero, '');
      Exit;
    end;
    on E: EMathError do
    begin
      Result := False;
      MakeError('', ecMathError, tbtstring(e.Message));
      Exit;
    end;
    on E: Exception do
    begin
      Result := False;
      MakeError('', ecInternalError, tbtstring(E.Message));
      Exit;
    end;
  end;
  if not Result then
  begin
    with MakeError('', ecTypeMismatch, '') do
    begin
      FPosition := Pos;
      FRow := Row;
      FCol := Col;
    end;
  end;
end;

function TPSPascalCompiler.IsDuplicate(const s: tbtString; const check: TPSDuplicCheck): Boolean;
var
  h, l: Longint;
  x: TPSProcedure;
begin
  if (s = 'RESULT') then
  begin
    Result := True;
    exit;
  end;
  h := MakeHash(s);
  if dcTypes in Check then
  for l := FTypes.Count - 1 downto 0 do
  begin
    if (TPSType(FTypes.Data[l]).NameHash = h) and
      (TPSType(FTypes.Data[l]).Name = s) then
    begin
      Result := True;
      exit;
    end;
  end;

  if dcProcs in Check then
  for l := FProcs.Count - 1 downto 0 do
  begin
    x := FProcs.Data[l];
    if x.ClassType = TPSInternalProcedure then
    begin
      if (h = TPSInternalProcedure(x).NameHash) and (s = TPSInternalProcedure(x).Name) then
      begin
        Result := True;
        exit;
      end;
    end
    else
    begin
      if (TPSExternalProcedure(x).RegProc.NameHash = h) and
        (TPSExternalProcedure(x).RegProc.Name = s) then
      begin
        Result := True;
        exit;
      end;
    end;
  end;
  if dcVars in Check then
  for l := FVars.Count - 1 downto 0 do
  begin
    if (TPSVar(FVars.Data[l]).NameHash = h) and
      (TPSVar(FVars.Data[l]).Name = s) then
    begin
      Result := True;
      exit;
    end;
  end;
  if dcConsts in Check then
  for l := FConstants.Count -1 downto 0 do
  begin
    if (TPSConstant(FConstants.Data[l]).NameHash = h) and
      (TPSConstant(FConstants.Data[l]).Name = s) then
    begin
      Result := TRue;
      exit;
    end;
  end;
  Result := False;
end;

procedure ClearRecSubVals(RecSubVals: TPSList);
var
  I: Longint;
begin
  for I := 0 to RecSubVals.Count - 1 do
    TPSRecordFieldTypeDef(RecSubVals[I]).Free;
  RecSubVals.Free;
end;

function TPSPascalCompiler.ReadTypeAddProcedure(const Name: tbtString; FParser: TPSPascalParser): TPSType;
var
  IsFunction: Boolean;
  VNames: tbtString;
  modifier: TPSParameterMode;
  Decl: TPSParametersDecl;
  VCType: TPSType;
begin
  if FParser.CurrTokenId = CSTII_Function then
    IsFunction := True
  else
    IsFunction := False;
  Decl := TPSParametersDecl.Create;
  try
    FParser.Next;
    if FParser.CurrTokenId = CSTI_OpenRound then
    begin
      FParser.Next;
      if FParser.CurrTokenId <> CSTI_CloseRound then
      begin
        while True do
        begin
          if FParser.CurrTokenId = CSTII_Const then
          begin
            Modifier := pmIn;
            FParser.Next;
          end else
          if FParser.CurrTokenId = CSTII_Out then
          begin
            Modifier := pmOut;
            FParser.Next;
          end else
          if FParser.CurrTokenId = CSTII_Var then
          begin
            modifier := pmInOut;
            FParser.Next;
          end
          else
            modifier := pmIn;
          if FParser.CurrTokenId <> CSTI_Identifier then
          begin
            Result := nil;
            if FParser = Self.FParser then
            MakeError('', ecIdentifierExpected, '');
            exit;
          end;
          VNames := FParser.OriginalToken + '|';
          FParser.Next;
          while FParser.CurrTokenId = CSTI_Comma do
          begin
            FParser.Next;
            if FParser.CurrTokenId <> CSTI_Identifier then
            begin
              Result := nil;
              if FParser = Self.FParser then
              MakeError('', ecIdentifierExpected, '');
              exit;
            end;
            VNames := VNames + FParser.GetToken + '|';
            FParser.Next;
          end;
          if FParser.CurrTokenId <> CSTI_Colon then
          begin
            Result := nil;
            if FParser = Self.FParser then
              MakeError('', ecColonExpected, '');
            exit;
          end;
          FParser.Next;
          if FParser.CurrTokenId <> CSTI_Identifier then
          begin
            Result := nil;
            if FParser = self.FParser then
            MakeError('', ecIdentifierExpected, '');
            exit;
          end;
          VCType := FindType(FParser.GetToken);
          if VCType = nil then
          begin
            if FParser = self.FParser then
            MakeError('', ecUnknownIdentifier, FParser.OriginalToken);
            Result := nil;
            exit;
          end;
          while Pos(tbtchar('|'), VNames) > 0 do
          begin
            with Decl.AddParam do
            begin
              Mode := modifier;
              OrgName := copy(VNames, 1, Pos(tbtchar('|'), VNames) - 1);
              FType := VCType;
            end;
            Delete(VNames, 1, Pos(tbtchar('|'), VNames));
          end;
          FParser.Next;
          if FParser.CurrTokenId = CSTI_CloseRound then
            break;
          if FParser.CurrTokenId <> CSTI_Semicolon then
          begin
            if FParser = Self.FParser then
            MakeError('', ecSemicolonExpected, '');
            Result := nil;
            exit;
          end;
          FParser.Next;
        end; {while}
      end; {if}
      FParser.Next;
      end; {if}
      if IsFunction then
      begin
        if FParser.CurrTokenId <> CSTI_Colon then
        begin
          if FParser = Self.FParser then
          MakeError('', ecColonExpected, '');
          Result := nil;
          exit;
        end;
      FParser.Next;
      if FParser.CurrTokenId <> CSTI_Identifier then
      begin
        Result := nil;
        if FParser = Self.FParser then
        MakeError('', ecIdentifierExpected, '');
        exit;
      end;
      VCType := self.FindType(FParser.GetToken);
      if VCType = nil then
      begin
        if FParser = self.FParser then
        MakeError('', ecUnknownIdentifier, FParser.OriginalToken);
        Result := nil;
        exit;
      end;
      FParser.Next;
    end
    else
      VCType := nil;
    Decl.Result := VcType;
    VCType := TPSProceduralType.Create;
    VCType.Name := FastUppercase(Name);
    VCType.OriginalName := Name;
    VCType.BaseType := btProcPtr;
    {$IFDEF PS_USESSUPPORT}
    VCType.DeclareUnit:=fModule;
    {$ENDIF}
    VCType.DeclarePos := FParser.CurrTokenPos;
    VCType.DeclareRow := FParser.Row;
    VCType.DeclareCol := FParser.Col;
    TPSProceduralType(VCType).ProcDef.Assign(Decl);
    FTypes.Add(VCType);
    Result := VCType;
  finally
    Decl.Free;
  end;
end; {ReadTypeAddProcedure}


function TPSPascalCompiler.ReadType(const Name: tbtString; FParser: TPSPascalParser): TPSType; // InvalidVal = Invalid
var
  TypeNo: TPSType;
  h, l: Longint;
  FieldName,fieldorgname,s: tbtString;
  RecSubVals: TPSList;
  FArrayStart, FArrayLength: Longint;
  rvv: PIFPSRecordFieldTypeDef;
  p, p2: TPSType;
  tempf: PIfRVariant;
{$IFNDEF PS_NOINTERFACES}
  InheritedFrom: tbtString;
  Guid: TGUID;
  Intf: TPSInterface;
{$ENDIF}
begin
  if (FParser.CurrTokenID = CSTII_Function) or (FParser.CurrTokenID = CSTII_Procedure) then
  begin
     Result := ReadTypeAddProcedure(Name, FParser);
     Exit;
  end else if FParser.CurrTokenId = CSTII_Set then
  begin
    FParser.Next;
    if FParser.CurrTokenId <> CSTII_Of then
    begin
      MakeError('', ecOfExpected, '');
      Result := nil;
      Exit;
    end;
    FParser.Next;
    if FParser.CurrTokenID <> CSTI_Identifier then
    begin
      MakeError('', ecIdentifierExpected, '');
      Result := nil;
      exit;
    end;
    TypeNo := FindType(FParser.GetToken);
    if TypeNo = nil then
    begin
      MakeError('', ecUnknownIdentifier, '');
      Result := nil;
      exit;
    end;
    if (TypeNo.BaseType = btEnum) or (TypeNo.BaseType = btChar) or (TypeNo.BaseType = btU8) then
    begin
      FParser.Next;
      p2 := TPSSetType.Create;
      p2.Name := FastUppercase(Name);
      p2.OriginalName := Name;
      p2.BaseType := btSet;
      {$IFDEF PS_USESSUPPORT}
      p2.DeclareUnit:=fModule;
      {$ENDIF}
      p2.DeclarePos := FParser.CurrTokenPos;
      p2.DeclareRow := FParser.Row;
      p2.DeclareCol := FParser.Col;
      TPSSetType(p2).SetType := TypeNo;
      FTypes.Add(p2);
      Result := p2;
    end else
    begin
      MakeError('', ecTypeMismatch, '');
      Result := nil;
    end;
    exit;
  end else if FParser.CurrTokenId = CSTI_OpenRound then
  begin
    FParser.Next;
    L := 0;
    P2 := TPSEnumType.Create;
    P2.Name := FastUppercase(Name);
    p2.OriginalName := Name;
    p2.BaseType := btEnum;
    {$IFDEF PS_USESSUPPORT}
    p2.DeclareUnit:=fModule;
    {$ENDIF}
    p2.DeclarePos := FParser.CurrTokenPos;
    p2.DeclareRow := FParser.Row;
    p2.DeclareCol := FParser.Col;
    FTypes.Add(p2);

    repeat
      if FParser.CurrTokenId <> CSTI_Identifier then
      begin
        if FParser = Self.FParser then
        MakeError('', ecIdentifierExpected, '');
        Result := nil;
        exit;
      end;
      s := FParser.OriginalToken;
      if IsDuplicate(FastUppercase(s), [dcTypes]) then
      begin
        if FParser = Self.FParser then
        MakeError('', ecDuplicateIdentifier, s);
        Result := nil;
        Exit;
      end;
      with AddConstant(s, p2) do
      begin
        FValue.tu32 := L;
        {$IFDEF PS_USESSUPPORT}
        DeclareUnit:=fModule;
        {$ENDIF}
        DeclarePos:=FParser.CurrTokenPos;
        DeclareRow:=FParser.Row;
        DeclareCol:=FParser.Col;
      end;
      Inc(L);
      FParser.Next;
      if FParser.CurrTokenId = CSTI_CloseRound then
        Break
      else if FParser.CurrTokenId <> CSTI_Comma then
      begin
        if FParser = Self.FParser then
        MakeError('', ecCloseRoundExpected, '');
        Result := nil;
        Exit;
      end;
      FParser.Next;
    until False;
    FParser.Next;
    TPSEnumType(p2).HighValue := L-1;
    Result := p2;
    exit;
  end else
  if FParser.CurrTokenId = CSTII_Array then
  begin
    FParser.Next;
    if FParser.CurrTokenID = CSTI_OpenBlock then
    begin
      FParser.Next;
      tempf := ReadConstant(FParser, CSTI_TwoDots);
      if tempf = nil then
      begin
        Result := nil;
        exit;
      end;
      case tempf.FType.BaseType of
        btU8: FArrayStart := tempf.tu8;
        btS8: FArrayStart := tempf.ts8;
        btU16: FArrayStart := tempf.tu16;
        btS16: FArrayStart := tempf.ts16;
        btU32: FArrayStart := tempf.tu32;
        btS32: FArrayStart := tempf.ts32;
        {$IFNDEF PS_NOINT64}
        bts64: FArrayStart := tempf.ts64;
        {$ENDIF}
      else
        begin
          DisposeVariant(tempf);
          MakeError('', ecTypeMismatch, '');
          Result := nil;
          exit;
        end;
      end;
      DisposeVariant(tempf);
      if FParser.CurrTokenID <> CSTI_TwoDots then
      begin
        MakeError('', ecPeriodExpected, '');
        Result := nil;
        exit;
      end;
      FParser.Next;
      tempf := ReadConstant(FParser, CSTI_CloseBlock);
      if tempf = nil then
      begin
        Result := nil;
        exit;
      end;
      case tempf.FType.BaseType of
        btU8: FArrayLength := tempf.tu8;
        btS8: FArrayLength := tempf.ts8;
        btU16: FArrayLength := tempf.tu16;
        btS16: FArrayLength := tempf.ts16;
        btU32: FArrayLength := tempf.tu32;
        btS32: FArrayLength := tempf.ts32;
        {$IFNDEF PS_NOINT64}
        bts64: FArrayLength := tempf.ts64;
        {$ENDIF}
      else
        DisposeVariant(tempf);
        MakeError('', ecTypeMismatch, '');
        Result := nil;
        exit;
      end;
      DisposeVariant(tempf);
      FArrayLength := FArrayLength - FArrayStart + 1;
      if (FArrayLength < 0) or (FArrayLength > MaxInt div 4) then
      begin
        MakeError('', ecTypeMismatch, '');
        Result := nil;
        exit;
      end;
      if FParser.CurrTokenID <> CSTI_CloseBlock then
      begin
        MakeError('', ecCloseBlockExpected, '');
        Result := nil;
        exit;
      end;
      FParser.Next;
    end else
    begin
      FArrayStart := 0;
      FArrayLength := -1;
    end;
    if FParser.CurrTokenId <> CSTII_Of then
    begin
      if FParser = Self.FParser then
      MakeError('', ecOfExpected, '');
      Result := nil;
      exit;
    end;
    FParser.Next;
    TypeNo := ReadType('', FParser);
    if TypeNo = nil then
    begin
      if FParser = Self.FParser then
      MakeError('', ecUnknownIdentifier, '');
      Result := nil;
      exit;
    end;
    if (Name = '') and (FArrayLength = -1) then
    begin
      if TypeNo.Used then
      begin
        for h := 0 to FTypes.Count -1 do
        begin
          p := FTypes[H];
          if (p.BaseType = btArray) and (TPSArrayType(p).ArrayTypeNo = TypeNo) and (Copy(p.Name, 1, 1) <> '!') then
          begin
            Result := p;
            exit;
          end;
        end;
      end;
    end;
    if FArrayLength <> -1 then
    begin
      p := TPSStaticArrayType.Create;
      TPSStaticArrayType(p).StartOffset := FArrayStart;
      TPSStaticArrayType(p).Length := FArrayLength;
      p.BaseType := btStaticArray;
    end else
    begin
      p := TPSArrayType.Create;
      p.BaseType := btArray;
    end;
    p.Name := FastUppercase(Name);
    p.OriginalName := Name;
    {$IFDEF PS_USESSUPPORT}
    p.DeclareUnit:=fModule;
    {$ENDIF}
    p.DeclarePos := FParser.CurrTokenPos;
    p.DeclareRow := FParser.Row;
    p.DeclareCol := FParser.Col;
    TPSArrayType(p).ArrayTypeNo := TypeNo;
    FTypes.Add(p);
    Result := p;
    Exit;
  end
  else if FParser.CurrTokenId = CSTII_Record then
  begin
    FParser.Next;
    RecSubVals := TPSList.Create;
    repeat
      repeat
        if FParser.CurrTokenId <> CSTI_Identifier then
        begin
          ClearRecSubVals(RecSubVals);
          if FParser = Self.FParser then
          MakeError('', ecIdentifierExpected, '');
          Result := nil;
          exit;
        end;
        FieldName := FParser.GetToken;
        s := S+FParser.OriginalToken+'|';
        FParser.Next;
        h := MakeHash(FieldName);
        for l := 0 to RecSubVals.Count - 1 do
        begin
          if (PIFPSRecordFieldTypeDef(RecSubVals[l]).FieldNameHash = h) and
            (PIFPSRecordFieldTypeDef(RecSubVals[l]).FieldName = FieldName) then
          begin
            if FParser = Self.FParser then
              MakeError('', ecDuplicateIdentifier, FParser.OriginalToken);
            ClearRecSubVals(RecSubVals);
            Result := nil;
            exit;
          end;
        end;
        if FParser.CurrTokenID = CSTI_Colon then Break else
        if FParser.CurrTokenID <> CSTI_Comma then
        begin
          if FParser = Self.FParser then
            MakeError('', ecColonExpected, '');
          ClearRecSubVals(RecSubVals);
          Result := nil;
          exit;
        end;
        FParser.Next;
      until False;
      FParser.Next;
      p := ReadType('', FParser);
      if p = nil then
      begin
        ClearRecSubVals(RecSubVals);
        Result := nil;
        exit;
      end;
      p := GetTypeCopyLink(p);
      if FParser.CurrTokenId <> CSTI_Semicolon then
      begin
        ClearRecSubVals(RecSubVals);
        if FParser = Self.FParser then
        MakeError('', ecSemicolonExpected, '');
        Result := nil;
        exit;
      end; {if}
      FParser.Next;
      while Pos(tbtchar('|'), s) > 0 do
      begin
        fieldorgname := copy(s, 1, Pos(tbtchar('|'), s)-1);
        Delete(s, 1, length(FieldOrgName)+1);
        rvv := TPSRecordFieldTypeDef.Create;
        rvv.FieldOrgName := fieldorgname;
        rvv.FType := p;
        RecSubVals.Add(rvv);
      end;
    until FParser.CurrTokenId = CSTII_End;
    FParser.Next; // skip CSTII_End
    P := TPSRecordType.Create;
    p.Name := FastUppercase(Name);
    p.OriginalName := Name;
    p.BaseType := btRecord;
    {$IFDEF PS_USESSUPPORT}
    p.DeclareUnit:=fModule;
    {$ENDIF}
    p.DeclarePos := FParser.CurrTokenPos;
    p.DeclareRow := FParser.Row;
    p.DeclareCol := FParser.Col;
    for l := 0 to RecSubVals.Count -1 do
    begin
      rvv := RecSubVals[l];
      with TPSRecordType(p).AddRecVal do
      begin
        FieldOrgName := rvv.FieldOrgName;
        FType := rvv.FType;
      end;
      rvv.Free;
    end;
    RecSubVals.Free;
    FTypes.Add(p);
    Result := p;
    Exit;
{$IFNDEF PS_NOINTERFACES}
  end else if FParser.CurrTokenId = CSTII_Interface then
  begin
    FParser.Next;
    if FParser.CurrTokenId <> CSTI_OpenRound then
    begin
      MakeError('', ecOpenRoundExpected, '');
      Result := nil;
      Exit;
    end;
    FParser.Next;
    if FParser.CurrTokenID <> CSTI_Identifier then
    begin
      MakeError('', ecIdentifierExpected, '');
      Result := nil;
      exit;
    end;
    InheritedFrom := FParser.GetToken;
    TypeNo := FindType(InheritedFrom);
    if TypeNo = nil then
    begin
      MakeError('', ecUnknownType, FParser.GetToken);
      Result := nil;
      exit;
    end;
    if TypeNo.BaseType <> btInterface then
    begin
      MakeError('', ecTypeMismatch, '');
      Result := nil;
      Exit;
    end;
    FParser.Next;
    if FParser.CurrTokenId <> CSTI_CloseRound then
    begin
      MakeError('', ecCloseRoundExpected, '');
      Result := nil;
      Exit;
    end;
{$IFNDEF PS_NOINTERFACEGUIDBRACKETS}
    FParser.Next;
    if FParser.CurrTokenId <> CSTI_OpenBlock then
    begin
      MakeError('', ecOpenBlockExpected, '');
      Result := nil;
      Exit;
    end;
{$ENDIF}
    FParser.Next;
    if FParser.CurrTokenId <> CSTI_String then
    begin
      MakeError('', ecStringExpected, '');
      Result := nil;
      Exit;
    end;
    s := FParser.GetToken;
    try
      Guid := StringToGuid(String(Copy(s, 2, Length(s)-2)));
    except
      on e: Exception do
      begin
        MakeError('', ecCustomError, tbtstring(e.Message));
        Result := nil;
        Exit;
      end;
    end;
{$IFNDEF PS_NOINTERFACEGUIDBRACKETS}
    FParser.Next;
    if FParser.CurrTokenId <> CSTI_CloseBlock then
    begin
      MakeError('', ecCloseBlockExpected, '');
      Result := nil;
      Exit;
    end;
{$ENDIF}
    Intf := AddInterface(FindInterface(InheritedFrom), Guid, Name);
    FParser.Next;
    repeat
      if not Intf.RegisterMethodEx('', cdStdCall, FParser) then begin
        MakeError('', ecCustomError, 'Invalid method');
        Result := nil;
        Exit;
      end;
      FParser.Next;
    until FParser.CurrTokenId = CSTII_End;
    FParser.Next; // skip CSTII_End
    Result := Intf.FType;
    Exit;
{$ENDIF}
  end else if FParser.CurrTokenId = CSTI_Identifier then
  begin
    s := FParser.GetToken;
    h := MakeHash(s);
    Typeno := nil;
    for l := 0 to FTypes.Count - 1 do
    begin
      p2 := FTypes[l];
      if (p2.NameHash = h) and (p2.Name = s) then
      begin
        FParser.Next;
        Typeno := GetTypeCopyLink(p2);
        Break;
      end;
    end;
    if Typeno = nil then
    begin
      Result := nil;
      if FParser = Self.FParser then
      MakeError('', ecUnknownType, FParser.OriginalToken);
      exit;
    end;
    if Name <> '' then
    begin
      p := TPSTypeLink.Create;
      p.Name := FastUppercase(Name);
      p.OriginalName := Name;
      p.BaseType := BtTypeCopy;
      {$IFDEF PS_USESSUPPORT}
      p.DeclareUnit:=fModule;
      {$ENDIF}
      p.DeclarePos := FParser.CurrTokenPos;
      p.DeclareRow := FParser.Row;
      p.DeclareCol := FParser.Col;
      TPSTypeLink(p).LinkTypeNo := TypeNo;
      FTypes.Add(p);
      Result := p;
      Exit;
    end else
    begin
      Result := TypeNo;
      exit;
    end;
  end;
  Result := nil;
  if FParser = Self.FParser then
  MakeError('', ecIdentifierExpected, '');
  Exit;
end;

function TPSPascalCompiler.VarIsDuplicate(Proc: TPSInternalProcedure; const Varnames, s: tbtString): Boolean;
var
  h, l: Longint;
  x: TPSProcedure;
  v: tbtString;
begin
  h := MakeHash(s);
  if (s = 'RESULT') then
  begin
    Result := True;
    exit;
  end;

  for l := FProcs.Count - 1 downto 0 do
  begin
    x := FProcs.Data[l];
    if x.ClassType = TPSInternalProcedure then
    begin
      if (h = TPSInternalProcedure(x).NameHash) and (s = TPSInternalProcedure(x).Name) then
      begin
        Result := True;
        exit;
      end;
    end
    else
    begin
      if (TPSExternalProcedure(x).RegProc.NameHash = h) and (TPSExternalProcedure(x).RegProc.Name = s) then
      begin
        Result := True;
        exit;
      end;
    end;
  end;
  if proc <> nil then
  begin
    for l := proc.ProcVars.Count - 1 downto 0 do
    begin
      if (PIFPSProcVar(proc.ProcVars.Data[l]).NameHash = h) and
        (PIFPSProcVar(proc.ProcVars.Data[l]).Name = s) then
      begin
        Result := True;
        exit;
      end;
    end;
    for l := Proc.FDecl.ParamCount -1 downto 0 do
    begin
      if (Proc.FDecl.Params[l].Name = s) then
      begin
        Result := True;
        exit;
      end;
    end;
  end
  else
  begin
    for l := FVars.Count - 1 downto 0 do
    begin
      if (TPSVar(FVars.Data[l]).NameHash = h) and
        (TPSVar(FVars.Data[l]).Name = s) then
      begin
        Result := True;
        exit;
      end;
    end;
  end;
  v := VarNames;
  while Pos(tbtchar('|'), v) > 0 do
  begin
    if FastUppercase(copy(v, 1, Pos(tbtchar('|'), v) - 1)) = s then
    begin
      Result := True;
      exit;
    end;
    Delete(v, 1, Pos(tbtchar('|'), v));
  end;
  for l := FConstants.Count -1 downto 0 do
  begin
    if (TPSConstant(FConstants.Data[l]).NameHash = h) and
      (TPSConstant(FConstants.Data[l]).Name = s) then
    begin
      Result := True;
      exit;
    end;
  end;
  Result := False;
end;


function TPSPascalCompiler.DoVarBlock(proc: TPSInternalProcedure): Boolean;
var
  VarName, s: tbtString;
  VarType: TPSType;
  VarNo: Cardinal;
  v: TPSVar;
  vp: PIFPSProcVar;
  EPos, ERow, ECol: Integer;
begin
  Result := False;
  FParser.Next; // skip CSTII_Var
  if FParser.CurrTokenId <> CSTI_Identifier then
  begin
    MakeError('', ecIdentifierExpected, '');
    exit;
  end;
  repeat
    VarNAme := '';
    if VarIsDuplicate(proc, VarName, FParser.GetToken) then
    begin
      MakeError('', ecDuplicateIdentifier, FParser.OriginalToken);
      exit;
    end;
    VarName := FParser.OriginalToken + '|';
    Varno := 0;
    if @FOnUseVariable <> nil then
    begin
      if Proc <> nil then
        FOnUseVariable(Self, ivtVariable, Proc.ProcVars.Count + VarNo, FProcs.Count -1, FParser.CurrTokenPos, '')
      else
        FOnUseVariable(Self, ivtGlobal, FVars.Count + VarNo, InvalidVal, FParser.CurrTokenPos, '')
    end;
    EPos:=FParser.CurrTokenPos;
    ERow:=FParser.Row;
    ECol:=FParser.Col;
    FParser.Next;
    while FParser.CurrTokenId = CSTI_Comma do
    begin
      FParser.Next;
      if FParser.CurrTokenId <> CSTI_Identifier then
      begin
        MakeError('', ecIdentifierExpected, '');
      end;
      if VarIsDuplicate(proc, VarName, FParser.GetToken) then
      begin
        MakeError('', ecDuplicateIdentifier, FParser.OriginalToken);
        exit;
      end;
      VarName := VarName + FParser.OriginalToken + '|';
      Inc(varno);
      if @FOnUseVariable <> nil then
      begin
        if Proc <> nil then
          FOnUseVariable(Self, ivtVariable, Proc.ProcVars.Count + VarNo, FProcs.Count -1, FParser.CurrTokenPos, '')
        else
          FOnUseVariable(Self, ivtGlobal, FVars.Count + VarNo, InvalidVal, FParser.CurrTokenPos, '')
      end;
      FParser.Next;
    end;
    if FParser.CurrTokenId <> CSTI_Colon then
    begin
      MakeError('', ecColonExpected, '');
      exit;
    end;
    FParser.Next;
    VarType := at2ut(ReadType('', FParser));
    if VarType = nil then
    begin
      exit;
    end;
    while Pos(tbtchar('|'), VarName) > 0 do
    begin
      s := copy(VarName, 1, Pos(tbtchar('|'), VarName) - 1);
      Delete(VarName, 1, Pos(tbtchar('|'), VarName));
      if proc = nil then
      begin
        v := TPSVar.Create;
        v.OrgName := s;
        v.Name := FastUppercase(s);
        {$IFDEF PS_USESSUPPORT}
        v.DeclareUnit:=fModule;
        {$ENDIF}
        v.DeclarePos := EPos;
        v.DeclareRow := ERow;
        v.DeclareCol := ECol;
        v.FType := VarType;
        FVars.Add(v);
      end
      else
      begin
        vp := TPSProcVar.Create;
        vp.OrgName := s;
        vp.Name := FastUppercase(s);
        vp.aType := VarType;
        {$IFDEF PS_USESSUPPORT}
        vp.DeclareUnit:=fModule;
        {$ENDIF}
        vp.DeclarePos := EPos;
        vp.DeclareRow := ERow;
        vp.DeclareCol := ECol;
        proc.ProcVars.Add(vp);
      end;
    end;
    if FParser.CurrTokenId <> CSTI_Semicolon then
    begin
      MakeError('', ecSemicolonExpected, '');
      exit;
    end;
    FParser.Next;
  until FParser.CurrTokenId <> CSTI_Identifier;
  Result := True;
end;

function TPSPascalCompiler.NewProc(const OriginalName, Name: tbtString): TPSInternalProcedure;
begin
  Result := TPSInternalProcedure.Create;
  Result.OriginalName := OriginalName;
  Result.Name := Name;
  {$IFDEF PS_USESSUPPORT}
  Result.DeclareUnit:=fModule;
  {$ENDIF}
  Result.DeclarePos := FParser.CurrTokenPos;
  Result.DeclareRow := FParser.Row;
  Result.DeclareCol := FParser.Col;
  FProcs.Add(Result);
end;

function TPSPascalCompiler.IsProcDuplicLabel(Proc: TPSInternalProcedure; const s: tbtString): Boolean;
var
  i: Longint;
  h: Longint;
  u: tbtString;
begin
  h := MakeHash(s);
  if s = 'RESULT' then
    Result := True
  else if Proc.Name = s then
    Result := True
  else if IsDuplicate(s, [dcVars, dcConsts, dcProcs]) then
    Result := True
  else
  begin
    for i := 0 to Proc.Decl.ParamCount -1 do
    begin
      if Proc.Decl.Params[i].Name = s then
      begin
        Result := True;
        exit;
      end;
    end;
    for i := 0 to Proc.ProcVars.Count -1 do
    begin
      if (PIFPSProcVar(Proc.ProcVars[I]).NameHash = h) and (PIFPSProcVar(Proc.ProcVars[I]).Name = s) then
      begin
        Result := True;
        exit;
      end;
    end;
    for i := 0 to Proc.FLabels.Count -1 do
    begin
      u := Proc.FLabels[I];
      delete(u, 1, 4);
      if Longint((@u[1])^) = h then
      begin
        delete(u, 1, 4);
        if u = s then
        begin
          Result := True;
          exit;
        end;
      end;
    end;
    Result := False;
  end;
end;


function TPSPascalCompiler.ProcessLabel(Proc: TPSInternalProcedure): Boolean;
var
  CurrLabel: tbtString;
begin
  FParser.Next;
  while true do
  begin
    if FParser.CurrTokenId <> CSTI_Identifier then
    begin
      MakeError('', ecIdentifierExpected, '');
      Result := False;
      exit;
    end;
    CurrLabel := FParser.GetToken;
    if IsProcDuplicLabel(Proc, CurrLabel) then
    begin
      MakeError('', ecDuplicateIdentifier, CurrLabel);
      Result := False;
      exit;
    end;
    FParser.Next;
    Proc.FLabels.Add(#$FF#$FF#$FF#$FF+PS_mi2s(MakeHash(CurrLabel))+CurrLabel);
    if FParser.CurrTokenId = CSTI_Semicolon then
    begin
      FParser.Next;
      Break;
    end;
    if FParser.CurrTokenId <> CSTI_Comma then
    begin
      MakeError('', ecCommaExpected, '');
      Result := False;
      exit;
    end;
    FParser.Next;
  end;
  Result := True;
end;

procedure TPSPascalCompiler.Debug_SavePosition(ProcNo: Cardinal; Proc: TPSInternalProcedure);
var
  Row,
  Col,
  Pos: Cardinal;
  s: tbtString;
begin
  Row := FParser.Row;
  Col := FParser.Col;
  Pos := FParser.CurrTokenPos;
  {$IFNDEF PS_USESSUPPORT}
  s := '';
  {$ELSE}
  s := fModule;
  {$ENDIF}
  if @FOnTranslateLineInfo <> nil then
    FOnTranslateLineInfo(Self, Pos, Row, Col, S);
  {$IFDEF FPC}
  WriteDebugData(#4 + s + #1);
  WriteDebugData(Ps_mi2s(ProcNo));
  WriteDebugData(Ps_mi2s(Length(Proc.Data)));
  WriteDebugData(Ps_mi2s(Pos));
  WriteDebugData(Ps_mi2s(Row));
  WriteDebugData(Ps_mi2s(Col));
  {$ELSE}
  WriteDebugData(#4 + s + #1 + PS_mi2s(ProcNo) + PS_mi2s(Length(Proc.Data)) + PS_mi2s(Pos) + PS_mi2s(Row)+ PS_mi2s(Col));
  {$ENDIF}
end;

procedure TPSPascalCompiler.Debug_WriteParams(ProcNo: Cardinal; Proc: TPSInternalProcedure);
var
  I: Longint;
  s: tbtString;
begin
  s := #2 + PS_mi2s(ProcNo);
  if Proc.Decl.Result <> nil then
  begin
    s := s + 'Result' + #1;
  end;
  for i := 0 to Proc.Decl.ParamCount -1 do
    s := s + Proc.Decl.Params[i].OrgName + #1;
  s := s + #0#3 + PS_mi2s(ProcNo);
  for I := 0 to Proc.ProcVars.Count - 1 do
  begin
    s := s + PIFPSProcVar(Proc.ProcVars[I]).OrgName + #1;
  end;
  s := s + #0;
  WriteDebugData(s);
end;

procedure TPSPascalCompiler.CheckForUnusedVars(Func: TPSInternalProcedure);
var
  i: Integer;
  p: PIFPSProcVar;
begin
  for i := 0 to Func.ProcVars.Count -1 do
  begin
    p := Func.ProcVars[I];
    if not p.Used then
    begin
      with MakeHint({$IFDEF PS_USESSUPPORT}p.DeclareUnit{$ELSE}''{$ENDIF}, ehVariableNotUsed, p.Name) do
      begin
        FRow := p.DeclareRow;
        FCol := p.DeclareCol;
        FPosition := p.DeclarePos;
      end;
    end;
  end;
  if (not Func.ResultUsed) and (Func.Decl.Result <> nil) then
  begin
      with MakeHint({$IFDEF PS_USESSUPPORT}Func.DeclareUnit{$ELSE}''{$ENDIF}, ehVariableNotUsed, 'Result') do
      begin
        FRow := Func.DeclareRow;
        FCol := Func.DeclareCol;
        FPosition := Func.DeclarePos;
      end;
  end;
end;

function TPSPascalCompiler.ProcIsDuplic(Decl: TPSParametersDecl; const FunctionName, FunctionParamNames: tbtString; const s: tbtString; Func: TPSInternalProcedure): Boolean;
var
  i: Longint;
  u: tbtString;
begin
  if s = 'RESULT' then
    Result := True
  else if FunctionName = s then
    Result := True
  else
  begin
    for i := 0 to Decl.ParamCount -1 do
    begin
      if Decl.Params[i].Name = s then
      begin
        Result := True;
        exit;
      end;
      GRFW(u);
    end;
    u := FunctionParamNames;
    while Pos(tbtchar('|'), u) > 0 do
    begin
      if copy(u, 1, Pos(tbtchar('|'), u) - 1) = s then
      begin
        Result := True;
        exit;
      end;
      Delete(u, 1, Pos(tbtchar('|'), u));
    end;
    if Func = nil then
    begin
      result := False;
      exit;
    end;
    for i := 0 to Func.ProcVars.Count -1 do
    begin
      if s = PIFPSProcVar(Func.ProcVars[I]).Name then
      begin
        Result := True;
        exit;
      end;
    end;
    for i := 0 to Func.FLabels.Count -1 do
    begin
      u := Func.FLabels[I];
      delete(u, 1, 4);
      if u = s then
      begin
        Result := True;
        exit;
      end;
    end;
    Result := False;
  end;
end;
procedure WriteProcVars(Func:TPSInternalProcedure; t: TPSList);
var
  l: Longint;
  v: PIFPSProcVar;
begin
  for l := 0 to t.Count - 1 do
  begin
    v := t[l];
    Func.Data := Func.Data  + chr(cm_pt)+ PS_mi2s(v.AType.FinalTypeNo);
  end;
end;


function TPSPascalCompiler.ApplyAttribsToFunction(func: TPSProcedure): boolean;
var
  i: Longint;
begin
  for i := 0 to Func.Attributes.Count -1 do
  begin
    if @Func.Attributes.Items[i].AType.OnApplyAttributeToProc <> nil then
    begin
      if not Func.Attributes.Items[i].AType.OnApplyAttributeToProc(Self, Func, Func.Attributes.Items[i]) then
      begin
        Result := false;
        exit;
      end;
    end;
  end;
  result := true;
end;


function TPSPascalCompiler.ProcessFunction(AlwaysForward: Boolean; Att: TPSAttributes): Boolean;
var
  FunctionType: TFuncType;
  OriginalName, FunctionName: tbtString;
  FunctionParamNames: tbtString;
  FunctionTempType: TPSType;
  ParamNo: Cardinal;
  FunctionDecl: TPSParametersDecl;
  modifier: TPSParameterMode;
  Func: TPSInternalProcedure;
  F2: TPSProcedure;
  EPos, ECol, ERow: Cardinal;
  E2Pos, E2Col, E2Row: Cardinal;
  pp: TPSRegProc;
  pp2: TPSExternalProcedure;
  FuncNo, I: Longint;
  Block: TPSBlockInfo;
begin
  if att = nil then
  begin
    Att := TPSAttributes.Create;
    if not ReadAttributes(Att) then
    begin
      att.free;
      Result := false;
      exit;
    end;
  end;

  if FParser.CurrTokenId = CSTII_Procedure then
    FunctionType := ftProc
  else
    FunctionType := ftFunc;
  Func := nil;
  EPos := FParser.CurrTokenPos;
  ERow := FParser.Row;
  ECol := FParser.Col;
  FParser.Next;
  Result := False;
  if FParser.CurrTokenId <> CSTI_Identifier then
  begin
    MakeError('', ecIdentifierExpected, '');
    att.free;
    exit;
  end;
  if assigned(FOnFunctionStart) then
  {$IFDEF PS_USESSUPPORT}
     FOnFunctionStart(fModule + '.' + FParser.OriginalToken, EPos, ERow, ECol);
  {$ELSE}
     FOnFunctionStart(FParser.OriginalToken, EPos, ERow, ECol);
  {$ENDIF}
  EPos := FParser.CurrTokenPos;
  ERow := FParser.Row;
  ECol := FParser.Col;
  OriginalName := FParser.OriginalToken;
  FunctionName := FParser.GetToken;
  FuncNo := -1;
  for i := 0 to FProcs.Count -1 do
  begin
    f2 := FProcs[I];
    if (f2.ClassType = TPSInternalProcedure) and (TPSInternalProcedure(f2).Name = FunctionName) and (TPSInternalProcedure(f2).Forwarded) then
    begin
      Func := FProcs[I];
      FuncNo := i;
      Break;
    end;
  end;
  if (Func = nil) and IsDuplicate(FunctionName, [dcTypes, dcProcs, dcVars, dcConsts]) then
  begin
    att.free;
    MakeError('', ecDuplicateIdentifier, FunctionName);
    exit;
  end;
  FParser.Next;
  FunctionDecl := TPSParametersDecl.Create;
  try
    if FParser.CurrTokenId = CSTI_OpenRound then
    begin
      FParser.Next;
      if FParser.CurrTokenId = CSTI_CloseRound then
      begin
        FParser.Next;
      end
      else
      begin
        if FunctionType = ftFunc then
          ParamNo := 1
        else
          ParamNo := 0;
        while True do
        begin
          if FParser.CurrTokenId = CSTII_Const then
          begin
            modifier := pmIn;
            FParser.Next;
          end
          else
          if FParser.CurrTokenId = CSTII_Out then
          begin
            modifier := pmOut;
            FParser.Next;
          end
          else
          if FParser.CurrTokenId = CSTII_Var then
          begin
            modifier := pmInOut;
            FParser.Next;
          end
          else
            modifier := pmIn;
          if FParser.CurrTokenId <> CSTI_Identifier then
          begin
            MakeError('', ecIdentifierExpected, '');
            exit;
          end;
          E2Pos := FParser.CurrTokenPos;
          E2Row := FParser.Row;
          E2Col := FParser.Col;
          FunctionParamNames := '';
          if ProcIsDuplic(FunctionDecl, FunctionName, FunctionParamNames, FParser.GetToken, Func) then
          begin
            MakeError('', ecDuplicateIdentifier, FParser.OriginalToken);
            exit;
          end;
          FunctionParamNames := FParser.OriginalToken + '|';
          if @FOnUseVariable <> nil then
          begin
            FOnUseVariable(Self, ivtParam, ParamNo, FProcs.Count, FParser.CurrTokenPos, '');
          end;
          inc(ParamNo);
          FParser.Next;
          while FParser.CurrTokenId = CSTI_Comma do
          begin
            FParser.Next;
            if FParser.CurrTokenId <> CSTI_Identifier then
            begin
              MakeError('', ecIdentifierExpected, '');
              exit;
            end;
          if ProcIsDuplic(FunctionDecl, FunctionName, FunctionParamNames, FParser.GetToken, Func) then
            begin
              MakeError('', ecDuplicateIdentifier, '');
              exit;
            end;
            if @FOnUseVariable <> nil then
            begin
              FOnUseVariable(Self, ivtParam, ParamNo, FProcs.Count, FParser.CurrTokenPos, '');
            end;
            inc(ParamNo);
            FunctionParamNames := FunctionParamNames + FParser.OriginalToken +
              '|';
            FParser.Next;
          end;
          if FParser.CurrTokenId <> CSTI_Colon then
          begin
            MakeError('', ecColonExpected, '');
            exit;
          end;
          FParser.Next;
          FunctionTempType := at2ut(ReadType('', FParser));
          if FunctionTempType = nil then
          begin
            exit;
          end;
          while Pos(tbtchar('|'), FunctionParamNames) > 0 do
          begin
            with FunctionDecl.AddParam do
            begin
              OrgName := copy(FunctionParamNames, 1, Pos(tbtchar('|'), FunctionParamNames) - 1);
              Mode := modifier;
              aType := FunctionTempType;
              {$IFDEF PS_USESSUPPORT}
              DeclareUnit:=fModule;
              {$ENDIF}
              DeclarePos:=E2Pos;
              DeclareRow:=E2Row;
              DeclareCol:=E2Col;
            end;
            Delete(FunctionParamNames, 1, Pos(tbtchar('|'), FunctionParamNames));
          end;
          if FParser.CurrTokenId = CSTI_CloseRound then
            break;
          if FParser.CurrTokenId <> CSTI_Semicolon then
          begin
            MakeError('', ecSemicolonExpected, '');
            exit;
          end;
          FParser.Next;
        end;
        FParser.Next;
      end;
    end;
    if FunctionType = ftFunc then
    begin
      if FParser.CurrTokenId <> CSTI_Colon then
      begin
        MakeError('', ecColonExpected, '');
        exit;
      end;
      FParser.Next;
      FunctionTempType := at2ut(ReadType('', FParser));
      if FunctionTempType = nil then
        exit;
      FunctionDecl.Result := FunctionTempType;
    end;
    if FParser.CurrTokenId <> CSTI_Semicolon then
    begin
      MakeError('', ecSemicolonExpected, '');
      exit;
    end;
    FParser.Next;
    if (Func = nil) and (FParser.CurrTokenID = CSTII_External) then
    begin
      FParser.Next;
      if FParser.CurrTokenID <> CSTI_String then
      begin
        MakeError('', ecStringExpected, '');
        exit;
      end;
      FunctionParamNames := FParser.GetToken;
      FunctionParamNames := copy(FunctionParamNames, 2, length(FunctionParamNames) - 2);
      FParser.Next;
      if FParser.CurrTokenID <> CSTI_Semicolon then
      begin
        MakeError('', ecSemicolonExpected, '');
        exit;
      end;
      FParser.Next;
      if @FOnExternalProc = nil then
      begin
        MakeError('', ecSemicolonExpected, '');
        exit;
      end;
      pp := FOnExternalProc(Self, FunctionDecl, OriginalName, FunctionParamNames);
      if pp = nil then
      begin
        MakeError('', ecCustomError, '');
        exit;
      end;
      pp2 := TPSExternalProcedure.Create;
      pp2.Attributes.Assign(att, true);
      pp2.RegProc := pp;
      FProcs.Add(pp2);
      FRegProcs.Add(pp);
      Result := ApplyAttribsToFunction(pp2);
      Exit;
    end else if (FParser.CurrTokenID = CSTII_Forward) or AlwaysForward then
    begin
      if Func <> nil then
      begin
        MakeError('', ecBeginExpected, '');
        exit;
      end;
      if not AlwaysForward then
      begin
        FParser.Next;
        if FParser.CurrTokenID  <> CSTI_Semicolon then
        begin
          MakeError('', ecSemicolonExpected, '');
          Exit;
        end;
        FParser.Next;
      end;
      Func := NewProc(OriginalName, FunctionName);
      Func.Attributes.Assign(Att, True);
      Func.Forwarded := True;
      {$IFDEF PS_USESSUPPORT}
      Func.FDeclareUnit := fModule;
      {$ENDIF}
      Func.FDeclarePos := EPos;
      Func.FDeclareRow := ERow;
      Func.FDeclarePos := ECol;
      Func.Decl.Assign(FunctionDecl);
      Result := ApplyAttribsToFunction(Func);
      exit;
    end;
    if (Func = nil) then
    begin
      Func := NewProc(OriginalName, FunctionName);
      Func.Attributes.Assign(att, True);
      Func.Decl.Assign(FunctionDecl);
      {$IFDEF PS_USESSUPPORT}
      Func.FDeclareUnit := fModule;
      {$ENDIF}
      Func.FDeclarePos := EPos;
      Func.FDeclareRow := ERow;
      Func.FDeclareCol := ECol;
      FuncNo := FProcs.Count -1;
      if not ApplyAttribsToFunction(Func) then
      begin
        result := false;
        exit;
      end;
    end else begin
      if not FunctionDecl.Same(Func.Decl) then
      begin
        MakeError('', ecForwardParameterMismatch, '');
        Result := false;
        exit;
      end;
      Func.Forwarded := False;
    end;
    if FParser.CurrTokenID = CSTII_Export then
    begin
      FParser.Next;
      if FParser.CurrTokenID <> CSTI_Semicolon then
      begin
        MakeError('', ecSemicolonExpected, '');
        exit;
      end;
      FParser.Next;
    end;
    while FParser.CurrTokenId <> CSTII_Begin do
    begin
      if FParser.CurrTokenId = CSTII_Var then
      begin
        if not DoVarBlock(Func) then
          exit;
      end else if FParser.CurrTokenId = CSTII_Label then
      begin
        if not ProcessLabel(Func) then
          Exit;
      end else
      begin
        MakeError('', ecBeginExpected, '');
        exit;
      end;
    end;
    Debug_WriteParams(FuncNo, Func);
    WriteProcVars(Func, Func.ProcVars);
    Block := TPSBlockInfo.Create(FGlobalBlock);
    Block.SubType := tProcBegin;
    Block.ProcNo := FuncNo;
    Block.Proc := Func;
    if not ProcessSub(Block) then
    begin
      Block.Free;
      exit;
    end;
    Block.Free;
    CheckForUnusedVars(Func);
    Result := ProcessLabelForwards(Func);
    if assigned(FOnFunctionEnd) then
    {$IFDEF PS_USESSUPPORT}
      OnFunctionEnd(fModule + '.' + OriginalName, FParser.CurrTokenPos, FParser.Row, FParser.Col);
    {$ELSE}
      OnFunctionEnd(OriginalName, FParser.CurrTokenPos, FParser.Row, FParser.Col);
    {$ENDIF}
  finally
    FunctionDecl.Free;
    att.Free;
  end;
end;

function GetParamType(BlockInfo: TPSBlockInfo; I: Longint): TPSType;
begin
  if BlockInfo.Proc.Decl.Result <> nil then dec(i);
  if i = -1 then
    Result := BlockInfo.Proc.Decl.Result
  else
  begin
    Result := BlockInfo.Proc.Decl.Params[i].aType;
  end;
end;

function TPSPascalCompiler.GetTypeNo(BlockInfo: TPSBlockInfo; p: TPSValue): TPSType;
begin
  if p.ClassType = TPSUnValueOp then
    Result := TPSUnValueOp(p).aType
  else if p.ClassType = TPSBinValueOp then
    Result := TPSBinValueOp(p).aType
  else if p.ClassType = TPSValueArray then
    Result := at2ut(FindType('TVariantArray'))
  else if p.ClassType = TPSValueData then
    Result := TPSValueData(p).Data.FType
  else if p is TPSValueProc then
    Result := TPSValueProc(p).ResultType
  else if (p is TPSValueVar) and (TPSValueVar(p).RecCount > 0) then
    Result := TPSValueVar(p).RecItem[TPSValueVar(p).RecCount - 1].aType
  else if p.ClassType = TPSValueGlobalVar then
    Result := TPSVar(FVars[TPSValueGlobalVar(p).GlobalVarNo]).FType
  else if p.ClassType = TPSValueParamVar then
    Result := GetParamType(BlockInfo, TPSValueParamVar(p).ParamNo)
  else if p is TPSValueLocalVar then
    Result := TPSProcVar(BlockInfo.Proc.ProcVars[TPSValueLocalVar(p).LocalVarNo]).AType
  else if p.classtype = TPSValueReplace then
    Result := GetTypeNo(BlockInfo, TPSValueReplace(p).NewValue)
  else
    Result := nil;
end;

function TPSPascalCompiler.IsVarInCompatible(ft1, ft2: TPSType): Boolean;
begin
  ft1 := GetTypeCopyLink(ft1);
  ft2 := GetTypeCopyLink(ft2);
  Result := (ft1 <> ft2) and (ft2 <> nil);
end;

function TPSPascalCompiler.ValidateParameters(BlockInfo: TPSBlockInfo; Params: TPSParameters; ParamTypes: TPSParametersDecl): boolean;
var
  i, c: Longint;
  pType: TPSType;

begin
  UseProc(ParamTypes);
  c := 0;
  for i := 0 to ParamTypes.ParamCount -1 do
  begin
    while (c < Longint(Params.Count)) and (Params[c].Val = nil) do
      Inc(c);
    if c >= Longint(Params.Count) then
    begin
      MakeError('', ecInvalidnumberOfParameters, '');
      Result := False;
      exit;
    end;
    Params[c].ExpectedType := ParamTypes.Params[i].aType;
    Params[c].ParamMode := ParamTypes.Params[i].Mode;
    if ParamTypes.Params[i].Mode <> pmIn then
    begin
      if not (Params[c].Val is TPSValueVar) then
      begin
        with MakeError('', ecVariableExpected, '') do
        begin
          Row := Params[c].Val.Row;
          Col := Params[c].Val.Col;
          Pos := Params[c].Val.Pos;
        end;
        result := false;
        exit;
      end;
        PType := Params[c].ExpectedType;
        if (PType = nil) or ((PType.BaseType = btArray) and (TPSArrayType(PType).ArrayTypeNo = nil) and (GetTypeNo(BlockInfo, Params[c].Val).BaseType = btArray)) or
          (PType = FAnyString) then
        begin
          Params[c].ExpectedType := GetTypeNo(BlockInfo, Params[c].Val);
          if PType <> nil then
          if (Params[c].ExpectedType = nil) or not IsStringOrCharType(Params[c].ExpectedType.BaseType) then
          begin
            MakeError('', ecTypeMismatch, '');
            Result := False;
            exit;
          end;
          if Params[c].ExpectedType.BaseType = btChar then
            Params[c].ExpectedType := FindBaseType(btString) else
{$IFNDEF PS_NOWIDESTRING}
          if Params[c].ExpectedType.BaseType = btWideChar then
            Params[c].ExpectedType := FindBaseType(btUnicodeString);
{$ENDIF}            
        end else if (PType.BaseType = btArray) and (GetTypeNo(BlockInfo, Params[c].Val).BaseType = btArray) then
        begin
          if TPSArrayType(GetTypeNo(BlockInfo, Params[c].Val)).ArrayTypeNo <> TPSArrayType(PType).ArrayTypeNo then
          begin
            MakeError('', ecTypeMismatch, '');
            Result := False;
            exit;
          end;
        end else if IsVarInCompatible(PType, GetTypeNo(BlockInfo, Params[c].Val)) then
        begin
          MakeError('', ecTypeMismatch, '');
          Result := False;
          exit;
        end;
    end;
    Inc(c);
  end;
  for i := c to Params.Count -1 do
  begin
    if Params[i].Val <> nil then
    begin
      MakeError('', ecInvalidnumberOfParameters, '');
      Result := False;
      exit;
    end;
  end;
  Result := true;
end;

function TPSPascalCompiler.DoTypeBlock(FParser: TPSPascalParser): Boolean;
var
  VOrg,VName: tbtString;
  Attr: TPSAttributes;
  FType: TPSType;
  i: Longint;
begin
  Result := False;
  FParser.Next;
  repeat
    Attr := TPSAttributes.Create;
    if not ReadAttributes(Attr) then
    begin
      Attr.Free;
      exit;
    end;
    if (FParser.CurrTokenID = CSTII_Procedure) or (FParser.CurrTokenID = CSTII_Function) then
    begin
      Result := ProcessFunction(false, Attr);
      exit;
    end;
    if FParser.CurrTokenId <> CSTI_Identifier then
    begin
      MakeError('', ecIdentifierExpected, '');
      Attr.Free;
      exit;
    end;

    VName := FParser.GetToken;
    VOrg := FParser.OriginalToken;
    if IsDuplicate(VName, [dcTypes, dcProcs, dcVars]) then
    begin
      MakeError('', ecDuplicateIdentifier, FParser.OriginalToken);
      Attr.Free;
      exit;
    end;

    FParser.Next;
    if FParser.CurrTokenId <> CSTI_Equal then
    begin
      MakeError('', ecIsExpected, '');
      Attr.Free;
      exit;
    end;
    FParser.Next;
    FType := ReadType(VOrg, FParser);
    if Ftype = nil then
    begin
      Attr.Free;
      Exit;
    end;
    FType.Attributes.Assign(Attr, True);
    for i := 0 to FType.Attributes.Count -1 do
    begin
      if (@FType.Attributes[i].FAttribType.OnApplyAttributeToType <> nil) and
          not FType.Attributes[i].FAttribType.OnApplyAttributeToType(Self, FType, FType.Attributes[i]) then begin
        Attr.Free;
        Exit;
      end;
    end;
    Attr.Free;
    if FParser.CurrTokenID <> CSTI_Semicolon then
    begin
      MakeError('', ecSemicolonExpected, '');
      Exit;
    end;
    FParser.Next;
  until (FParser.CurrTokenId <> CSTI_Identifier) and (FParser.CurrTokenID <> FAttributesOpenTokenID);
  Result := True;
end;

procedure TPSPascalCompiler.Debug_WriteLine(BlockInfo: TPSBlockInfo);
begin
  Debug_WriteLine2(BlockInfo, False);
end;

procedure TPSPascalCompiler.Debug_WriteLine2(BlockInfo: TPSBlockInfo; IsProcExit: Boolean);
var
  b: Boolean;
begin
  if @FOnWriteLine2 <> nil then begin
    {$IFNDEF PS_USESSUPPORT}
    b := FOnWriteLine2(Self, FParser.CurrTokenPos, IsProcExit);
    {$ELSE}
    b := FOnWriteLine2(Self, FModule, FParser.CurrTokenPos, IsProcExit);
    {$ENDIF}
  end else if @FOnWriteLine <> nil then begin
    {$IFNDEF PS_USESSUPPORT}
    b := FOnWriteLine(Self, FParser.CurrTokenPos);
    {$ELSE}
    b := FOnWriteLine(Self, FModule, FParser.CurrTokenPos);
    {$ENDIF}
  end else
    b := true;
  if b then Debug_SavePosition(BlockInfo.ProcNo, BlockInfo.Proc);
end;


function TPSPascalCompiler.ReadReal(const s: tbtString): PIfRVariant;
var
  C: Integer;
begin
  New(Result);
  InitializeVariant(Result, FindBaseType(btExtended));
  Val(string(s), Result^.textended, C);
end;

function TPSPascalCompiler.ReadString: PIfRVariant;
{$IFNDEF PS_NOWIDESTRING}var wchar: Boolean;{$ENDIF}

  function ParseString({$IFNDEF PS_NOWIDESTRING}var res: tbtunicodestring{$ELSE}var res: tbtString{$ENDIF}): Boolean;
  var
    temp3: {$IFNDEF PS_NOWIDESTRING}tbtunicodestring{$ELSE}tbtString{$ENDIF};

    function ChrToStr(s: tbtString): {$IFNDEF PS_NOWIDESTRING}widechar{$ELSE}tbtchar{$ENDIF};
    var
      w: Longint;
    begin
      Delete(s, 1, 1); {First char : #}
      w := StrToInt(s);
      Result := {$IFNDEF PS_NOWIDESTRING}widechar{$ELSE}tbtchar{$ENDIF}(w);
      {$IFNDEF PS_NOWIDESTRING}if w > $FF then wchar := true;{$ENDIF}
    end;

    function PString(s: tbtString): tbtString;
    var
      i: Longint;
    begin
      s := copy(s, 2, Length(s) - 2);
      i := length(s);
      while i > 0 do
      begin
        if (i < length(s)) and (s[i] = #39) and (s[i + 1] = #39) then
        begin
          Delete(s, i, 1);
          dec(i);
        end;
        dec(i);
      end;
      PString := s;
    end;
  var
    lastwasstring: Boolean;
  begin
    temp3 := '';
    while (FParser.CurrTokenId = CSTI_String) or (FParser.CurrTokenId = CSTI_Char) do
    begin
      lastwasstring := FParser.CurrTokenId = CSTI_String;
      if FParser.CurrTokenId = CSTI_String then
      begin
        if UTF8Decode then
        begin
        temp3 := temp3 + {$IFNDEF PS_NOWIDESTRING}{$IFDEF DELPHI6UP}System.{$IFDEF DELPHI2009UP}UTF8ToWidestring{$ELSE}UTF8Decode{$ENDIF}{$ENDIF}{$ENDIF}(PString(FParser.GetToken));
        {$IFNDEF PS_NOWIDESTRING}wchar:=true;{$ENDIF}
        end else
          temp3 := temp3 + {$IFNDEF PS_NOWIDESTRING}tbtUnicodestring{$ENDIF}(PString(FParser.GetToken));

        FParser.Next;
        if FParser.CurrTokenId = CSTI_String then
          temp3 := temp3 + #39;
      end {if}
      else
      begin
        temp3 := temp3 + ChrToStr(FParser.GetToken);
        FParser.Next;
      end; {else if}
      if  lastwasstring and (FParser.CurrTokenId = CSTI_String) then
      begin
        MakeError('', ecSyntaxError, '');
        result := false;
        exit;
      end;
    end; {while}
    res := temp3;
    result := true;
  end;
var
{$IFNDEF PS_NOWIDESTRING}
  w: tbtunicodestring;
{$ENDIF}
  s: tbtString;
begin
  {$IFNDEF PS_NOWIDESTRING}wchar:=false;{$ENDIF}
  if not ParseString({$IFDEF PS_NOWIDESTRING} s {$ELSE}  w {$ENDIF}) then
  begin
    result := nil;
    exit;
  end;
{$IFNDEF PS_NOWIDESTRING}
  if wchar then
  begin
    New(Result);
    if Length(w) = 1 then
    begin
      InitializeVariant(Result, at2ut(FindBaseType(btwidechar)));
      Result^.twidechar := w[1];
    end else begin
      InitializeVariant(Result, at2ut(FindBaseType(btUnicodeString)));
      tbtunicodestring(Result^.tunistring) := w;
     end;
  end else begin
    s := tbtstring(w);
{$ENDIF}
    New(Result);
    if Length(s) = 1 then
    begin
      InitializeVariant(Result, at2ut(FindBaseType(btchar)));
      Result^.tchar := s[1];
    end else begin
      InitializeVariant(Result, at2ut(FindBaseType(btstring)));
      tbtstring(Result^.tstring) := s;
    end;
{$IFNDEF PS_NOWIDESTRING}
  end;
{$ENDIF}
end;


function TPSPascalCompiler.ReadInteger(const s: tbtString): PIfRVariant;
var
  R: {$IFNDEF PS_NOINT64}Int64;{$ELSE}Longint;{$ENDIF}
begin
  New(Result);
{$IFNDEF PS_NOINT64}
  r := StrToInt64Def(string(s), 0);
  if (r >= Low(Integer)) and (r <= High(Integer)) then
  begin
    InitializeVariant(Result, at2ut(FindBaseType(bts32)));
    Result^.ts32 := r;
  end else if (r <= $FFFFFFFF) then
  begin
    InitializeVariant(Result, at2ut(FindBaseType(btu32)));
    Result^.tu32 := r;
  end else
  begin
    InitializeVariant(Result, at2ut(FindBaseType(bts64)));
    Result^.ts64 := r;
  end;
{$ELSE}
  r := StrToIntDef(s, 0);
  InitializeVariant(Result, at2ut(FindBaseType(bts32)));
  Result^.ts32 := r;
{$ENDIF}
end;

function TPSPascalCompiler.ProcessSub(BlockInfo: TPSBlockInfo): Boolean;

  function AllocStackReg2(MType: TPSType): TPSValue;
  var
    x: TPSProcVar;
  begin
{$IFDEF DEBUG}
    if (mtype = nil) or (not mtype.Used) then asm int 3; end;
{$ENDIF}
    x := TPSProcVar.Create;
    {$IFDEF PS_USESSUPPORT}
    x.DeclareUnit:=fModule;
    {$ENDIF}
    x.DeclarePos := FParser.CurrTokenPos;
    x.DeclareRow := FParser.Row;
    x.DeclareCol := FParser.Col;
    x.Name := '';
    x.AType := MType;
    x.Use;
    BlockInfo.Proc.ProcVars.Add(x);
    Result := TPSValueAllocatedStackVar.Create;
    Result.SetParserPos(FParser);
    TPSValueAllocatedStackVar(Result).Proc := BlockInfo.Proc;
    with TPSValueAllocatedStackVar(Result) do
    begin
      LocalVarNo := proc.ProcVars.Count -1;
    end;
  end;

  function AllocStackReg(MType: TPSType): TPSValue;
  begin
    Result := AllocStackReg2(MType);
    BlockWriteByte(BlockInfo, Cm_Pt);
    BlockWriteLong(BlockInfo, MType.FinalTypeNo);
  end;

  function AllocPointer(MDestType: TPSType): TPSValue;
  begin
    Result := AllocStackReg(at2ut(FindBaseType(btPointer)));
    TPSProcVar(BlockInfo.Proc.ProcVars[TPSValueAllocatedStackVar(Result).LocalVarNo]).AType := MDestType;
  end;

  function WriteCalculation(InData, OutReg: TPSValue): Boolean; forward;
  function PreWriteOutRec(var X: TPSValue; FArrType: TPSType): Boolean; forward;
  function WriteOutRec(x: TPSValue; AllowData: Boolean): Boolean; forward;
  procedure AfterWriteOutRec(var x: TPSValue); forward;

  function CheckCompatType(V1, v2: TPSValue): Boolean;
  var
    p1, P2: TPSType;
  begin
    p1 := GetTypeNo(BlockInfo, V1);
    P2 := GetTypeNo(BlockInfo, v2);
    if (p1 = nil) or (p2 = nil) then
    begin
      if ((p1 <> nil) and ({$IFNDEF PS_NOINTERFACES}(p1.ClassType = TPSInterfaceType) or {$ENDIF}(p1.BaseType = btProcPtr)) and (v2.ClassType = TPSValueNil)) or
        ((p2 <> nil) and ({$IFNDEF PS_NOINTERFACES}(p2.ClassType = TPSInterfaceType) or {$ENDIF}(p2.BaseType = btProcPtr)) and (v1.ClassType = TPSValueNil)) then
      begin
        Result := True;
        exit;
      end else
      if ((p1 <> nil) and ({$IFNDEF PS_NOINTERFACES}(p1.ClassType = TPSInterfaceType) or {$ENDIF}(p1.ClassType = TPSClassType)) and (v2.ClassType = TPSValueNil)) or
        ((p2 <> nil) and ({$IFNDEF PS_NOINTERFACES}(p2.ClassType = TPSInterfaceType) or {$ENDIF}(p2.ClassType = TPSClassType)) and (v1.ClassType = TPSValueNil)) then
      begin
        Result := True;
        exit;
      end else
      if ((p1 <> nil) and ({$IFNDEF PS_NOINTERFACES}(p1.ClassType = TPSInterfaceType) or {$ENDIF}(p1.ClassType = TPSUndefinedClassType)) and (v2.ClassType = TPSValueNil)) or
        ((p2 <> nil) and ({$IFNDEF PS_NOINTERFACES}(p2.ClassType = TPSInterfaceType) or {$ENDIF}(p2.ClassType = TPSUndefinedClassType)) and (v1.ClassType = TPSValueNil)) then
      begin
        Result := True;
        exit;
      end else
      if (v1.ClassType = TPSValueProcPtr) and (p2 <> nil) and (p2.BaseType = btProcPtr) then
      begin
        Result := CheckCompatProc(p2, TPSValueProcPtr(v1).ProcPtr);
        exit;
      end else if (v2.ClassType = TPSValueProcPtr) and (p1 <> nil) and (p1.BaseType = btProcPtr) then
      begin
        Result := CheckCompatProc(p1, TPSValueProcPtr(v2).ProcPtr);
        exit;
      end;
      Result := False;
    end else
    if (p1 <> nil) and (p1.BaseType = btSet) and (v2 is TPSValueArray) then
    begin
      Result := True;
    end else
      Result := IsCompatibleType(p1, p2, False);
  end;

  function _ProcessFunction(ProcCall: TPSValueProc; ResultRegister: TPSValue): Boolean; forward;
  function ProcessFunction2(ProcNo: Cardinal; Par: TPSParameters; ResultReg: TPSValue): Boolean;
  var
    Temp: TPSValueProcNo;
    i: Integer;
  begin
    Temp := TPSValueProcNo.Create;
    Temp.Parameters := Par;
    Temp.ProcNo := ProcNo;
    if TObject(FProcs[ProcNo]).ClassType = TPSInternalProcedure then
      Temp.ResultType := TPSInternalProcedure(FProcs[ProcNo]).Decl.Result
    else
      Temp.ResultType := TPSExternalProcedure(FProcs[ProcNo]).RegProc.Decl.Result;
    if (Temp.ResultType <> nil) and (Temp.ResultType = FAnyString) then begin // workaround to make the result type match
      for i := 0 to Par.Count -1 do begin
        if Par[i].ExpectedType.BaseType in [btString{$IFNDEF PS_NOWIDESTRING}, btWideString{$ENDIF}] then
          Temp.ResultType := Par[i].ExpectedType;
      end;
    end;
    Result := _ProcessFunction(Temp, ResultReg);
    Temp.Parameters := nil;
    Temp.Free;
  end;

  function MakeNil(NilPos, NilRow, nilCol: Cardinal;ivar: TPSValue): Boolean;
  var
    Procno: Cardinal;
    PF: TPSType;
    Par: TPSParameters;
  begin
    Pf := GetTypeNo(BlockInfo, IVar);
    if not (Ivar is TPSValueVar) then
    begin
      with MakeError('', ecTypeMismatch, '') do
      begin
        FPosition := nilPos;
        FRow := NilRow;
        FCol := nilCol;
      end;
      Result := False;
      exit;
    end;
    if (pf.BaseType = btProcPtr) then
    begin
      Result := True;
    end else
    if (pf.BaseType = btString) or (pf.BaseType = btPChar) then
    begin
      if not PreWriteOutRec(iVar, nil) then
      begin
        Result := false;
        exit;
      end;
      BlockWriteByte(BlockInfo, CM_A);
      WriteOutRec(ivar, False);
      BlockWriteByte(BlockInfo, 1);
      BlockWriteLong(BlockInfo, GetTypeNo(BlockInfo, IVar).FinalTypeNo);
      BlockWriteLong(BlockInfo, 0); //empty tbtString
      AfterWriteOutRec(ivar);
      Result := True;
    end else if (pf.BaseType = btClass) {$IFNDEF PS_NOINTERFACES}or (pf.BaseType = btInterface){$ENDIF} then
    begin
{$IFNDEF PS_NOINTERFACES}
      if (pf.BaseType = btClass) then
      begin
{$ENDIF}
        if not TPSClassType(pf).Cl.SetNil(ProcNo) then
        begin
          with MakeError('', ecTypeMismatch, '') do
          begin
            FPosition := nilPos;
            FRow := NilRow;
            FCol := nilCol;
          end;
          Result := False;
          exit;
        end;
{$IFNDEF PS_NOINTERFACES}
      end else
      begin
        if not TPSInterfaceType(pf).Intf.SetNil(ProcNo) then
        begin
          with MakeError('', ecTypeMismatch, '') do
          begin
            FPosition := nilPos;
            FRow := NilRow;
            FCol := nilCol;
          end;
          Result := False;
          exit;
        end;
      end;
{$ENDIF}
      Par := TPSParameters.Create;
      with par.Add do
      begin
        Val := IVar;
        ExpectedType := GetTypeNo(BlockInfo, ivar);
{$IFDEF DEBUG}
        if not ExpectedType.Used then asm int 3; end;
{$ENDIF}
        ParamMode := pmInOut;
      end;
      Result := ProcessFunction2(ProcNo, Par, nil);

      Par[0].Val := nil; // don't free IVAR

      Par.Free;
    end else if pf.BaseType = btExtClass then
    begin
      if not TPSUndefinedClassType(pf).ExtClass.SetNil(ProcNo) then
      begin
        with MakeError('', ecTypeMismatch, '') do
        begin
          FPosition := nilPos;
          FRow := NilRow;
          FCol := nilCol;
        end;
        Result := False;
        exit;
      end;
      Par := TPSParameters.Create;
      with par.Add do
      begin
        Val := IVar;
        ExpectedType := GetTypeNo(BlockInfo, ivar);
        ParamMode := pmInOut;
      end;
      Result := ProcessFunction2(ProcNo, Par, nil);

      Par[0].Val := nil; // don't free IVAR

      Par.Free;
    end else begin
      with MakeError('', ecTypeMismatch, '') do
      begin
        FPosition := nilPos;
        FRow := NilRow;
        FCol := nilCol;
      end;
      Result := False;
    end;
  end;
  function DoBinCalc(BVal: TPSBinValueOp; Output: TPSValue): Boolean;
  var
    tmpp, tmpc: TPSValue;
    jend, jover: Cardinal;
    procno: Cardinal;

  begin
    if BVal.Operator >= otGreaterEqual then
    begin
      if BVal.FVal1.ClassType = TPSValueNil then
      begin
        tmpp := AllocStackReg(GetTypeNo(BlockInfo, BVal.FVal2));
        if not MakeNil(BVal.FVal1.Pos, BVal.FVal1.Row, BVal.FVal1.Col, tmpp) then
        begin
          tmpp.Free;
          Result := False;
          exit;
        end;
        tmpc := TPSValueReplace.Create;
        with TPSValueReplace(tmpc) do
        begin
          OldValue := BVal.FVal1;
          NewValue := tmpp;
        end;
        BVal.FVal1 := tmpc;
      end;
      if BVal.FVal2.ClassType = TPSValueNil then
      begin
        tmpp := AllocStackReg(GetTypeNo(BlockInfo, BVal.FVal1));
        if not MakeNil(BVal.FVal2.Pos, BVal.FVal2.Row, BVal.FVal2.Col, tmpp) then
        begin
          tmpp.Free;;
          Result := False;
          exit;
        end;
        tmpc := TPSValueReplace.Create;
        with TPSValueReplace(tmpc) do
        begin
          OldValue := BVal.FVal2;
          NewValue := tmpp;
        end;
        BVal.FVal2 := tmpc;
      end;
      if GetTypeNo(BlockInfo, BVal.FVal1).BaseType = btExtClass then
      begin
        if not TPSUndefinedClassType(GetTypeNo(BlockInfo, BVal.FVal1)).ExtClass.CompareClass(GetTypeNo(BlockInfo, Bval.FVal2), ProcNo) then
        begin
          Result := False;
          exit;
        end;
        tmpp := TPSValueProcNo.Create;
        with TPSValueProcNo(tmpp) do
        begin
          ResultType := at2ut(FDefaultBoolType);
          Parameters := TPSParameters.Create;
          ProcNo := procno;
          Pos := BVal.Pos;
          Col := BVal.Col;
          Row := BVal.Row;
          with parameters.Add do
          begin
            Val := BVal.FVal1;
            ExpectedType := GetTypeNo(BlockInfo, Val);
          end;
          with parameters.Add do
          begin
            Val := BVal.FVal2;
            ExpectedType := GetTypeNo(BlockInfo, Val);
          end;
        end;
        if Bval.Operator = otNotEqual then
        begin
          tmpc := TPSUnValueOp.Create;
          TPSUnValueOp(tmpc).Operator := otNot;
          TPSUnValueOp(tmpc).Val1 := tmpp;
          TPSUnValueOp(tmpc).aType := GetTypeNo(BlockInfo, tmpp);
        end else tmpc := tmpp;
        Result := WriteCalculation(tmpc, Output);
        with TPSValueProcNo(tmpp) do
        begin
          Parameters[0].Val := nil;
          Parameters[1].Val := nil;
        end;
        tmpc.Free;
        if BVal.Val1.ClassType = TPSValueReplace then
        begin
          tmpp := TPSValueReplace(BVal.Val1).OldValue;
          BVal.Val1.Free;
          BVal.Val1 := tmpp;
        end;
        if BVal.Val2.ClassType = TPSValueReplace then
        begin
          tmpp := TPSValueReplace(BVal.Val2).OldValue;
          BVal.Val2.Free;
          BVal.Val2 := tmpp;
        end;
        exit;
      end;
      if not (PreWriteOutRec(Output, nil) and PreWriteOutRec(BVal.FVal1, GetTypeNo(BlockInfo, BVal.FVal2)) and PreWriteOutRec(BVal.FVal2, GetTypeNo(BlockInfo, BVal.FVal1))) then
      begin
        Result := False;
        exit;
      end;
      BlockWriteByte(BlockInfo, CM_CO);
      case BVal.Operator of
        otGreaterEqual: BlockWriteByte(BlockInfo, 0);
        otLessEqual: BlockWriteByte(BlockInfo, 1);
        otGreater: BlockWriteByte(BlockInfo, 2);
        otLess: BlockWriteByte(BlockInfo, 3);
        otEqual: BlockWriteByte(BlockInfo, 5);
        otNotEqual: BlockWriteByte(BlockInfo, 4);
        otIn: BlockWriteByte(BlockInfo, 6);
        otIs: BlockWriteByte(BlockInfo, 7);
      end;

      if not (WriteOutRec(Output, False) and writeOutRec(BVal.FVal1, True) and writeOutRec(BVal.FVal2, True)) then
      begin
        Result := False;
        exit;
      end;
      AfterWriteOutrec(BVal.FVal1);
      AfterWriteOutrec(BVal.FVal2);
      AfterWriteOutrec(Output);
      if BVal.Val1.ClassType = TPSValueReplace then
      begin
        tmpp := TPSValueReplace(BVal.Val1).OldValue;
        BVal.Val1.Free;
        BVal.Val1 := tmpp;
      end;
      if BVal.Val2.ClassType = TPSValueReplace then
      begin
        tmpp := TPSValueReplace(BVal.Val2).OldValue;
        BVal.Val2.Free;
        BVal.Val2 := tmpp;
      end;
    end else begin
      if (BVal.aType <> nil) and (BVal.aType <> GetTypeNo(BlockInfo, Output)) then begin
        tmpp := AllocStackReg(BVal.aType);
        PreWriteOutrec(tmpp, nil);
        DoBinCalc(BVal, tmpp);
        afterwriteoutrec(tmpp);
        result := WriteCalculation(tmpp, output);
        tmpp.Free;
        exit;
      end;

      if not PreWriteOutRec(Output, nil) then
      begin
        Result := False;
        exit;
      end;
      if not SameReg(Output, BVal.Val1) then
      begin
        if not WriteCalculation(BVal.FVal1, Output) then
        begin
          Result := False;
          exit;
        end;
      end;
      if (FBooleanShortCircuit) and (IsBoolean(BVal.aType)) then
      begin
        if BVal.Operator = otAnd then
        begin
          BlockWriteByte(BlockInfo, Cm_CNG);
          jover := Length(BlockInfo.Proc.FData);
          BlockWriteLong(BlockInfo, 0);
          WriteOutRec(Output, True);
          jend := Length(BlockInfo.Proc.FData);
        end else if BVal.Operator = otOr then
        begin
          BlockWriteByte(BlockInfo, Cm_CG);
          jover := Length(BlockInfo.Proc.FData);
          BlockWriteLong(BlockInfo, 0);
          WriteOutRec(Output, True);
          jend := Length(BlockInfo.Proc.FData);
        end else
        begin
          jover := 0;
          jend := 0;
        end;
      end else
      begin
        jover := 0;
        jend := 0;
      end;
      if not PreWriteOutrec(BVal.FVal2, GetTypeNo(BlockInfo, Output)) then
      begin
        Result := False;
        exit;
      end;
      BlockWriteByte(BlockInfo, Cm_CA);
      if BVAL.Operator = otIntDiv then
        BlockWriteByte(BlockInfo, Ord(otDiv))
      else
        BlockWriteByte(BlockInfo, Ord(BVal.Operator));
      if not (WriteOutRec(Output, False) and WriteOutRec(BVal.FVal2, True)) then
      begin
        Result := False;
        exit;
      end;
      AfterWriteOutRec(BVal.FVal2);
      if FBooleanShortCircuit and (IsBoolean(BVal.aType)) and (JOver <> JEnd) then
      begin
        {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
        unaligned(Cardinal((@BlockInfo.Proc.FData[jover+1])^)) := Cardinal(Length(BlockInfo.Proc.FData)) - jend;
	{$else}
        Cardinal((@BlockInfo.Proc.FData[jover+1])^) := Cardinal(Length(BlockInfo.Proc.FData)) - jend;
	{$endif}
      end;
      AfterWriteOutRec(Output);
    end;
    Result := True;
  end;

  function DoUnCalc(Val: TPSUnValueOp; Output: TPSValue): Boolean;
  var
    Tmp: TPSValue;
  begin
    if not PreWriteOutRec(Output, nil) then
    begin
      Result := False;
      exit;
    end;
    case Val.Operator of
      otNot:
        begin
          if not SameReg(Val.FVal1, Output) then
          begin
            if not WriteCalculation(Val.FVal1, Output) then
            begin
              Result := False;
              exit;
            end;
          end;
          if IsBoolean(GetTypeNo(BlockInfo, Val)) then
            BlockWriteByte(BlockInfo, cm_bn)
          else
            BlockWriteByte(BlockInfo, cm_in);
          if not WriteOutRec(Output, True) then
          begin
            Result := False;
            exit;
          end;
        end;
      otMinus:
        begin
          if not SameReg(Val.FVal1, Output) then
          begin
            if not WriteCalculation(Val.FVal1, Output) then
            begin
              Result := False;
              exit;
            end;
          end;
          BlockWriteByte(BlockInfo, cm_vm);
          if not WriteOutRec(Output, True) then
          begin
            Result := False;
            exit;
          end;
        end;
      otCast:
        begin
          if ((Val.aType.BaseType = btChar) and (Val.aType.BaseType <> btU8)) {$IFNDEF PS_NOWIDESTRING}or
            ((Val.aType.BaseType = btWideChar) and (Val.aType.BaseType <> btU16)){$ENDIF} then
          begin
            Tmp := AllocStackReg(Val.aType);
          end else
            Tmp := Output;
          if not (PreWriteOutRec(Val.FVal1, GetTypeNo(BlockInfo, Tmp)) and PreWriteOutRec(Tmp, GetTypeNo(BlockInfo, Tmp))) then
          begin
            Result := False;
            if tmp <> Output then Tmp.Free;
            exit;
          end;
          BlockWriteByte(BlockInfo, CM_A);
          if not (WriteOutRec(Tmp, False) and WriteOutRec(Val.FVal1, True)) then
          begin
            Result := false;
            if tmp <> Output then Tmp.Free;
            exit;
          end;
          AfterWriteOutRec(val.Fval1);
          if Tmp <> Output then
          begin
            if not WriteCalculation(Tmp, Output) then
            begin
              Result := false;
              Tmp.Free;
              exit;
            end;
          end;
          AfterWriteOutRec(Tmp);
          if Tmp <> Output then
            Tmp.Free;
        end;
      {else donothing}
    end;
    AfterWriteOutRec(Output);
    Result := True;
  end;


  function GetAddress(Val: TPSValue): Cardinal;
  begin
    if Val.ClassType = TPSValueGlobalVar then
      Result := TPSValueGlobalVar(val).GlobalVarNo
    else if Val.ClassType = TPSValueLocalVar then
      Result := PSAddrStackStart + TPSValueLocalVar(val).LocalVarNo + 1
    else if Val.ClassType = TPSValueParamVar then
      Result := PSAddrStackStart - TPSValueParamVar(val).ParamNo -1
    else if Val.ClassType =  TPSValueAllocatedStackVar then
      Result := PSAddrStackStart + TPSValueAllocatedStackVar(val).LocalVarNo + 1
    else
      Result := InvalidVal;
  end;


  function PreWriteOutRec(var X: TPSValue; FArrType: TPSType): Boolean;
  var
    rr: TPSSubItem;
    tmpp,
      tmpc: TPSValue;
    i: Longint;
    function MakeSet(SetType: TPSSetType; arr: TPSValueArray): Boolean;
    var
      c, i: Longint;
      dataval: TPSValueData;
      mType: TPSType;
    begin
      Result := True;
      dataval := TPSValueData.Create;
      dataval.Data := NewVariant(FarrType);
      for i := 0 to arr.count -1 do
      begin
        mType := GetTypeNo(BlockInfo, arr.Item[i]);
        if (mType <> SetType.SetType) and
           not (IsIntType(mType.FBaseType) and IsIntType(SetType.SetType.BaseType)) and
           not (IsCharType(mType.FBaseType) and IsCharType(SetType.SetType.BaseType)) then
        begin
          with MakeError('', ecTypeMismatch, '') do
          begin
            FCol := arr.item[i].Col;
            FRow := arr.item[i].Row;
            FPosition := arr.item[i].Pos;
          end;
          DataVal.Free;
          Result := False;
          exit;
        end;
        if arr.Item[i] is TPSValueData then
        begin
          c := GetInt(TPSValueData(arr.Item[i]).Data, Result);
          if not Result then
          begin
            dataval.Free;
            exit;
          end;
          if (c < Low(Byte)) or (c > High(Byte)) then
          begin
            with MakeError('', ecTypeMismatch, '') do
            begin
              FCol := arr.item[i].Col;
              FRow := arr.item[i].Row;
              FPosition := arr.item[i].Pos;
            end;
            DataVal.Free;
            Result := False;
            exit;
          end;
          Set_MakeMember(c, dataval.Data.tstring);
        end else
        begin
          DataVal.Free;
          MakeError('', ecTypeMismatch, '');
          Result := False;
          exit;
        end;
      end;
      tmpc := TPSValueReplace.Create;
      with TPSValueReplace(tmpc) do
      begin
        OldValue := x;
        NewValue := dataval;
        PreWriteAllocated := True;
      end;
      x := tmpc;
    end;
  begin
    Result := True;
    if x.ClassType = TPSValueReplace then
    begin
      if TPSValueReplace(x).PreWriteAllocated then
      begin
        inc(TPSValueReplace(x).FReplaceTimes);
      end;
    end else
    if x.ClassType = TPSValueProcPtr then
    begin
      if FArrType = nil then
      begin
        MakeError('', ecTypeMismatch, '');
        Result := False;
        Exit;
      end;
      tmpp := TPSValueData.Create;
      TPSValueData(tmpp).Data := NewVariant(FArrType);
      TPSValueData(tmpp).Data.tu32 := TPSValueProcPtr(x).ProcPtr;
      tmpc := TPSValueReplace.Create;
      with TPSValueReplace(tmpc) do
      begin
        PreWriteAllocated := True;
        OldValue := x;
        NewValue := tmpp;
      end;
      x := tmpc;
    end else
    if x.ClassType = TPSValueNil then
    begin
      if FArrType = nil then
      begin
        MakeError('', ecTypeMismatch, '');
        Result := False;
        Exit;
      end;
      tmpp := AllocStackReg(FArrType);
      if not MakeNil(x.Pos, x.Row, x.Col, tmpp) then
      begin
        tmpp.Free;
        Result := False;
        exit;
      end;
      tmpc := TPSValueReplace.Create;
      with TPSValueReplace(tmpc) do
      begin
        PreWriteAllocated := True;
        OldValue := x;
        NewValue := tmpp;
      end;
      x := tmpc;
    end else
    if x.ClassType = TPSValueArray then
    begin
      if FArrType = nil then
      begin
        MakeError('', ecTypeMismatch, '');
        Result := False;
        Exit;
      end;
      if TPSType(FArrType).BaseType = btSet then
      begin
        Result := MakeSet(TPSSetType(FArrType), TPSValueArray(x));
        exit;
      end;
      if TPSType(FarrType).BaseType = btVariant then
        FArrType := at2ut(FindAndAddType(self, '!OPENARRAYOFVARIANT', 'array of Variant'));
      if TPSType(FarrType).BaseType <> btArray then 
        FArrType := at2ut(FindAndAddType(self, '!OPENARRAYOFVARIANT', 'array of Variant'));

      tmpp := AllocStackReg(FArrType);
      tmpc := AllocStackReg(FindBaseType(bts32));
      BlockWriteByte(BlockInfo, CM_A);
      WriteOutrec(tmpc, False);
      BlockWriteByte(BlockInfo, 1);
      BlockWriteLong(BlockInfo, FindBaseType(bts32).FinalTypeNo);
      BlockWriteLong(BlockInfo, TPSValueArray(x).Count);
      BlockWriteByte(BlockInfo, CM_PV);
      WriteOutrec(tmpp, False);
      BlockWriteByte(BlockInfo, CM_C);
      BlockWriteLong(BlockInfo, FindProc('SetArrayLength'));
      BlockWriteByte(BlockInfo, CM_PO);
      tmpc.Free;
      rr := TPSSubNumber.Create;
      rr.aType := TPSArrayType(FArrType).ArrayTypeNo;
      TPSValueVar(tmpp).RecAdd(rr);
      for i := 0 to TPSValueArray(x).Count -1 do
      begin
        TPSSubNumber(rr).SubNo := i;
        tmpc := TPSValueArray(x).Item[i];
        if not PreWriteOutRec(tmpc, GetTypeNo(BlockInfo, tmpc)) then
        begin
          tmpp.Free;
          Result := false;
          exit;
        end;
        if TPSArrayType(FArrType).ArrayTypeNo.BaseType = btPointer then
          BlockWriteByte(BlockInfo, cm_spc)
        else
          BlockWriteByte(BlockInfo, cm_a);
        if not (WriteOutrec(tmpp, False) and WriteOutRec(tmpc, True)) then
        begin
          Tmpp.Free;
          Result := false;
          exit;
        end;
        AfterWriteOutRec(tmpc);
      end;
      TPSValueVar(tmpp).RecDelete(0);
      tmpc := TPSValueReplace.Create;
      with TPSValueReplace(tmpc) do
      begin
        PreWriteAllocated := True;
        OldValue := x;
        NewValue := tmpp;
      end;
      x := tmpc;
    end else if (x.ClassType = TPSUnValueOp) then
    begin
      tmpp := AllocStackReg(GetTypeNo(BlockInfo, x));
      if not DoUnCalc(TPSUnValueOp(x), tmpp) then
      begin
        Result := False;
        exit;
      end;
      tmpc := TPSValueReplace.Create;
      with TPSValueReplace(tmpc) do
      begin
        PreWriteAllocated := True;
        OldValue := x;
        NewValue := tmpp;
      end;
      x := tmpc;
    end else if (x.ClassType = TPSBinValueOp) then
    begin
      tmpp := AllocStackReg(GetTypeNo(BlockInfo, x));
      if not DoBinCalc(TPSBinValueOp(x), tmpp) then
      begin
        tmpp.Free;
        Result := False;
        exit;
      end;
      tmpc := TPSValueReplace.Create;
      with TPSValueReplace(tmpc) do
      begin
        PreWriteAllocated := True;
        OldValue := x;
        NewValue := tmpp;
      end;
      x := tmpc;
    end else if x is TPSValueProc then
    begin
      tmpp := AllocStackReg(TPSValueProc(x).ResultType);
      if not WriteCalculation(x, tmpp) then
      begin
        tmpp.Free;
        Result := False;
        exit;
      end;
      tmpc := TPSValueReplace.Create;
      with TPSValueReplace(tmpc) do
      begin
        PreWriteAllocated := True;
        OldValue := x;
        NewValue := tmpp;
      end;
      x := tmpc;
    end else if (x is TPSValueVar) and (TPSValueVar(x).RecCount <> 0) then
    begin
      if  TPSValueVar(x).RecCount = 1 then
      begin
        rr := TPSValueVar(x).RecItem[0];
        if rr.ClassType <> TPSSubValue then
          exit; // there is no need pre-calculate anything
        if (TPSSubValue(rr).SubNo is TPSValueVar) and (TPSValueVar(TPSSubValue(rr).SubNo).RecCount = 0) then
          exit;
      end; //if
      tmpp := AllocPointer(GetTypeNo(BlockInfo, x));
      BlockWriteByte(BlockInfo, cm_sp);
      WriteOutRec(tmpp, True);
      BlockWriteByte(BlockInfo, 0);
      BlockWriteLong(BlockInfo, GetAddress(x));
      for i := 0 to TPSValueVar(x).RecCount - 1 do
      begin
        rr := TPSValueVar(x).RecItem[I];
        if rr.ClassType = TPSSubNumber then
        begin
          BlockWriteByte(BlockInfo, cm_sp);
          WriteOutRec(tmpp, false);
          BlockWriteByte(BlockInfo, 2);
          BlockWriteLong(BlockInfo, GetAddress(tmpp));
          BlockWriteLong(BlockInfo, TPSSubNumber(rr).SubNo);
        end else begin // if rr.classtype = TPSSubValue then begin
          tmpc := AllocStackReg(FindBaseType(btU32));
          if not WriteCalculation(TPSSubValue(rr).SubNo, tmpc) then
          begin
            tmpc.Free;
            tmpp.Free;
            Result := False;
            exit;
          end; //if
          BlockWriteByte(BlockInfo, cm_sp);
          WriteOutRec(tmpp, false);
          BlockWriteByte(BlockInfo, 3);
          BlockWriteLong(BlockInfo, GetAddress(tmpp));
          BlockWriteLong(BlockInfo, GetAddress(tmpc));
          tmpc.Free;
        end;
      end; // for
      tmpc := TPSValueReplace.Create;
      with TPSValueReplace(tmpc) do
      begin
        OldValue := x;
        NewValue := tmpp;
        PreWriteAllocated := True;
      end;
      x := tmpc;
    end;

  end;

  procedure AfterWriteOutRec(var x: TPSValue);
  var
    tmp: TPSValue;
  begin
    if (x.ClassType = TPSValueReplace) and (TPSValueReplace(x).PreWriteAllocated) then
    begin
      Dec(TPSValueReplace(x).FReplaceTimes);
      if TPSValueReplace(x).ReplaceTimes = 0 then
      begin
        tmp := TPSValueReplace(x).OldValue;
        x.Free;
        x := tmp;
      end;
    end;
  end; //afterwriteoutrec

  function WriteOutRec(x: TPSValue; AllowData: Boolean): Boolean;
  var
    rr: TPSSubItem;
  begin
    Result := True;
    if x.ClassType = TPSValueReplace then
      Result := WriteOutRec(TPSValueReplace(x).NewValue, AllowData)
    else if x is TPSValueVar then
    begin
      if TPSValueVar(x).RecCount = 0 then
      begin
        BlockWriteByte(BlockInfo, 0);
        BlockWriteLong(BlockInfo, GetAddress(x));
      end
      else
      begin
        rr := TPSValueVar(x).RecItem[0];
        if rr.ClassType = TPSSubNumber then
        begin
          BlockWriteByte(BlockInfo, 2);
          BlockWriteLong(BlockInfo, GetAddress(x));
          BlockWriteLong(BlockInfo, TPSSubNumber(rr).SubNo);
        end
        else
        begin
          BlockWriteByte(BlockInfo, 3);
          BlockWriteLong(BlockInfo, GetAddress(x));
          BlockWriteLong(BlockInfo, GetAddress(TPSSubValue(rr).SubNo));
        end;
      end;
    end else if x.ClassType = TPSValueData then
    begin
      if AllowData then
      begin
        BlockWriteByte(BlockInfo, 1);
        BlockWriteVariant(BlockInfo, TPSValueData(x).Data)
      end
      else
      begin
        Result := False;
        exit;
      end;
    end else
      Result := False;
  end;

  function ReadParameters(IsProperty: Boolean; Dest: TPSParameters): Boolean; forward;
{$IFNDEF PS_NOIDISPATCH}
  function ReadIDispatchParameters(const ProcName: tbtString; aVariantType: TPSVariantType; FSelf: TPSValue): TPSValue; forward;
{$ENDIF}
  function ReadProcParameters(ProcNo: Cardinal; FSelf: TPSValue): TPSValue; forward;
  function ReadVarParameters(ProcNoVar: TPSValue): TPSValue; forward;

  function calc(endOn: TPSPasToken): TPSValue; forward;
  procedure CheckNotificationVariant(var Val: TPSValue);
  var
    aType: TPSType;
    Call: TPSValueProcNo;
    tmp: TPSValue;
  begin
    if not (Val is TPSValueGlobalVar) then exit;
    aType := GetTypeNo(BlockInfo, Val);
    if (AType = nil) or (AType.BaseType <> btNotificationVariant) then exit;
    if FParser.CurrTokenId = CSTI_Assignment then
    begin
      Call := TPSValueProcNo.Create;
      Call.ResultType := nil;
      Call.SetParserPos(FParser);
      Call.ProcNo := FindProc('!NOTIFICATIONVARIANTSET');;
      Call.SetParserPos(FParser);
      Call.Parameters := TPSParameters.Create;
      Tmp := TPSValueData.Create;
      TPSVAlueData(tmp).Data := NewVariant(at2ut(FindBaseType(btString)));
      tbtString(TPSValueData(tmp).Data.tstring) := TPSVar(FVars[TPSValueGlobalVar(Val).GlobalVarNo]).OrgName;
      with call.Parameters.Add do
      begin
        Val := tmp;
        ExpectedType := TPSValueData(tmp).Data.FType;
      end;
      FParser.Next;
      tmp := Calc(CSTI_SemiColon);
      if tmp = nil then
      begin
        Val.Free;
        Val := nil;
        exit;
      end;
      with Call.Parameters.Add do
      begin
        Val := tmp;
        ExpectedType := at2ut(FindBaseType(btVariant));
      end;
      Val.Free;
      Val := Call;
    end else begin
      Call := TPSValueProcNo.Create;
      Call.ResultType := AT2UT(FindBaseType(btVariant));
      Call.SetParserPos(FParser);
      Call.ProcNo := FindProc('!NOTIFICATIONVARIANTGET');
      Call.SetParserPos(FParser);
      Call.Parameters := TPSParameters.Create;
      Tmp := TPSValueData.Create;
      TPSVAlueData(tmp).Data := NewVariant(at2ut(FindBaseType(btString)));
      tbtString(TPSValueData(tmp).Data.tstring) := TPSVar(FVars[TPSValueGlobalVar(Val).GlobalVarNo]).OrgName;
      with call.Parameters.Add do
      begin
        Val := tmp;
        ExpectedType := TPSValueData(tmp).Data.FType;
      end;
      Val.Free;
      Val := Call;
    end;
  end;

    procedure CheckProcCall(var x: TPSValue);
    var
      aType: TPSType;
    begin
      if FParser.CurrTokenId in [CSTI_Dereference, CSTI_OpenRound] then
      begin
        aType := GetTypeNo(BlockInfo, x);
        if (aType = nil) or (aType.BaseType <> btProcPtr) then
        begin
          MakeError('', ecTypeMismatch, '');
          x.Free;
          x := nil;
          Exit;
        end;
        if FParser.CurrTokenId = CSTI_Dereference then
          FParser.Next;
        x := ReadVarParameters(x);
      end;
    end;

    procedure CheckFurther(var x: TPSValue; ImplicitPeriod: Boolean);
    var
      t: Cardinal;
      rr: TPSSubItem;
      L: Longint;
      u: TPSType;
      Param: TPSParameter;
      tmp, tmpn: TPSValue;
      tmp3: TPSValueProcNo;
      tmp2: Boolean;

      function FindSubR(const n: tbtString; FType: TPSType): Cardinal;
      var
        h, I: Longint;
        rvv: PIFPSRecordFieldTypeDef;
      begin
        h := MakeHash(n);
        for I := 0 to TPSRecordType(FType).RecValCount - 1 do
        begin
          rvv := TPSRecordType(FType).RecVal(I);
          if (rvv.FieldNameHash = h) and (rvv.FieldName = n) then
          begin
            Result := I;
            exit;
          end;
        end;
        Result := InvalidVal;
      end;

    begin
(*      if not (x is TPSValueVar) then
        Exit;*)
      u := GetTypeNo(BlockInfo, x);
      if u = nil then exit;
      while True do
      begin
        if (u.BaseType = btClass) {$IFNDEF PS_NOINTERFACES}or (u.BaseType = btInterface){$ENDIF}
        {$IFNDEF PS_NOIDISPATCH}or ((u.BaseType = btNotificationVariant)){$ENDIF} or (u.BaseType = btExtClass) then exit;
        if FParser.CurrTokenId = CSTI_OpenBlock then
        begin
          if (u.BaseType = btString) {$IFNDEF PS_NOWIDESTRING} or
            (u.BaseType = btWideString) or (u.BaseType = btUnicodeString) {$ENDIF}
            {$IFDEF PS_HAVEVARIANT}or (u.BaseType = btVariant){$ENDIF} then
          begin
             FParser.Next;
            tmp := Calc(CSTI_CloseBlock);
            if tmp = nil then
            begin
              x.Free;
              x := nil;
              exit;
            end;
            if not IsIntType(GetTypeNo(BlockInfo, tmp).BaseType) then
            begin
              MakeError('', ecTypeMismatch, '');
              tmp.Free;
              x.Free;
              x := nil;
              exit;
            end;
            FParser.Next;
            if FParser.CurrTokenId = CSTI_Assignment then
            begin
              if not (x is TPSValueVar) then begin
                MakeError('', ecVariableExpected, '');
                tmp.Free;
                x.Free;
                x := nil;
                exit;
              end;
              {$IFDEF PS_HAVEVARIANT}if (u.BaseType = btVariant) then
                l := FindProc('VarArraySet') else
              {$ENDIF}
              {$IFNDEF PS_NOWIDESTRING}
              if (u.BaseType = btWideString) or (u.BaseType = btUnicodeString) then
                l := FindProc('WStrSet')
              else
              {$ENDIF}
                l := FindProc('StrSet');
              if l = -1 then
              begin
                MakeError('', ecUnknownIdentifier, 'StrSet');
                tmp.Free;
                x.Free;
                x := nil;
                exit;
              end;
              tmp3 := TPSValueProcNo.Create;
              tmp3.ResultType := nil;
              tmp3.SetParserPos(FParser);
              tmp3.ProcNo := L;
              tmp3.SetParserPos(FParser);
              tmp3.Parameters := TPSParameters.Create;
              param := tmp3.Parameters.Add;
              with tmp3.Parameters.Add do
              begin
                Val := tmp;
                ExpectedType := GetTypeNo(BlockInfo, tmp);
{$IFDEF DEBUG}
                if not ExpectedType.Used then asm int 3; end;
{$ENDIF}
              end;
              with tmp3.Parameters.Add do
              begin
                Val := x;
                ExpectedType := GetTypeNo(BlockInfo, x);
{$IFDEF DEBUG}
                if not ExpectedType.Used then asm int 3; end;
{$ENDIF}
                ParamMode := pmInOut;
              end;
              x := tmp3;
              FParser.Next;
              tmp := Calc(CSTI_SemiColon);
              if tmp = nil then
              begin
                x.Free;
                x := nil;
                exit;
              end;
              {$IFDEF PS_HAVEVARIANT}if (u.BaseType <> btVariant) then {$ENDIF}
              begin
                if (GetTypeNo(BlockInfo, Tmp).BaseType <> btChar)
                {$IFNDEF PS_NOWIDESTRING} and (GetTypeno(BlockInfo, Tmp).BaseType <> btWideChar) {$ENDIF} then
                begin
                  x.Free;
                  x := nil;
                  Tmp.Free;
                  MakeError('', ecTypeMismatch, '');
                  exit;

                end;
              end;
              param.Val := tmp;
              {$IFDEF PS_HAVEVARIANT}
              if u.BaseType = btVariant then
                Param.ExpectedType := u else{$ENDIF}
              Param.ExpectedType := GetTypeNo(BlockInfo, tmp);
{$IFDEF DEBUG}
              if not Param.ExpectedType.Used then asm int 3; end;
{$ENDIF}
            end else begin
              {$IFDEF PS_HAVEVARIANT}if (u.BaseType = btVariant) then
                l := FindProc('VarArrayGet') else
              {$ENDIF}
              {$IFNDEF PS_NOWIDESTRING}
              if (u.BaseType = btWideString) or (u.BaseType = btUnicodeString) then
                l := FindProc('WStrGet')
              else
              {$ENDIF}
              l := FindProc('StrGet');
              if l = -1 then
              begin
                MakeError('', ecUnknownIdentifier, 'StrGet');
                tmp.Free;
                x.Free;
                x := nil;
                exit;
              end;
              tmp3 := TPSValueProcNo.Create;
              {$IFDEF PS_HAVEVARIANT}if (u.BaseType = btVariant) then
                tmp3.ResultType := FindBaseType(btVariant) else
              {$ENDIF}
              {$IFNDEF PS_NOWIDESTRING}
              if (u.BaseType = btWideString) or (u.BaseType = btUnicodeString) then
                tmp3.ResultType := FindBaseType(btWideChar)
              else
              {$ENDIF}
                tmp3.ResultType := FindBaseType(btChar);
              tmp3.ProcNo := L;
              tmp3.SetParserPos(FParser);
              tmp3.Parameters := TPSParameters.Create;
              with tmp3.Parameters.Add do
              begin
                Val := x;
                ExpectedType := GetTypeNo(BlockInfo, x);
{$IFDEF DEBUG}
                if not ExpectedType.Used then asm int 3; end;
{$ENDIF}

                if x is TPSValueVar then
                  ParamMode := pmInOut
                else
                  parammode := pmIn;
              end;
              with tmp3.Parameters.Add do
              begin
                Val := tmp;
                ExpectedType := GetTypeNo(BlockInfo, tmp);
{$IFDEF DEBUG}
                if not ExpectedType.Used then asm int 3; end;
{$ENDIF}
              end;
              x := tmp3;
            end;
            Break;
          end else if ((u.BaseType = btArray) or (u.BaseType = btStaticArray)) and (x is TPSValueVar) then
          begin
            FParser.Next;
            tmp := calc(CSTI_CloseBlock);
            if tmp = nil then
            begin
              x.Free;
              x := nil;
              exit;
            end;
            if not IsIntType(GetTypeNo(BlockInfo, tmp).BaseType) then
            begin
              MakeError('', ecTypeMismatch, '');
              tmp.Free;
              x.Free;
              x := nil;
              exit;
            end;
            if (tmp.ClassType = TPSValueData) then
            begin
              rr := TPSSubNumber.Create;
              TPSValueVar(x).RecAdd(rr);
              if (u.BaseType = btStaticArray) then
                TPSSubNumber(rr).SubNo := Cardinal(GetInt(TPSValueData(tmp).Data, tmp2) - TPSStaticArrayType(u).StartOffset)
              else
                TPSSubNumber(rr).SubNo := GetUInt(TPSValueData(tmp).Data, tmp2);
              tmp.Free;
              rr.aType := TPSArrayType(u).ArrayTypeNo;
              u := rr.aType;
            end
            else
            begin
              if (u.BaseType = btStaticArray) then
              begin
                tmpn := TPSBinValueOp.Create;
                TPSBinValueOp(tmpn).Operator := otSub;
                TPSBinValueOp(tmpn).Val1 := tmp;
                tmp := TPSValueData.Create;
                TPSValueData(tmp).Data := NewVariant(FindBaseType(btS32));
                TPSValueData(tmp).Data.ts32 := TPSStaticArrayType(u).StartOffset;
                TPSBinValueOp(tmpn).Val2 := tmp;
                TPSBinValueOp(tmpn).aType := FindBaseType(btS32);
                tmp := tmpn;
              end;
              rr := TPSSubValue.Create;
              TPSValueVar(x).recAdd(rr);
              TPSSubValue(rr).SubNo := tmp;
              rr.aType := TPSArrayType(u).ArrayTypeNo;
              u := rr.aType;
            end;
            if FParser.CurrTokenId <> CSTI_CloseBlock then
            begin
              MakeError('', ecCloseBlockExpected, '');
              x.Free;
              x := nil;
              exit;
            end;
            Fparser.Next;
          end else begin
            MakeError('', ecSemicolonExpected, '');
            x.Free;
            x := nil;
            exit;
          end;
        end
        else if ((FParser.CurrTokenId = CSTI_Period) or (ImplicitPeriod))
         {$IFDEF PS_HAVEVARIANT}and not (u.BaseType = btVariant){$ENDIF}
        then
        begin
          if not ImplicitPeriod then
            FParser.Next;
          if u.BaseType = btRecord then
          begin
            t := FindSubR(FParser.GetToken, u);
            if t = InvalidVal then
            begin
              if ImplicitPeriod then exit;
              MakeError('', ecUnknownIdentifier, FParser.GetToken);
              x.Free;
              x := nil;
              exit;
            end;
            if (x is TPSValueProcNo) then
            begin
              ImplicitPeriod := False;
              FParser.Next;

              tmp := AllocStackReg(u);
              WriteCalculation(x,tmp);
              TPSVar(BlockInfo.Proc.FProcVars[TPSValueAllocatedStackVar(tmp).LocalVarNo]).Use;

              rr := TPSSubNumber.Create;
              TPSValueVar(tmp).RecAdd(rr);
              TPSSubNumber(rr).SubNo := t;
              rr.aType := TPSRecordType(u).RecVal(t).FType;
              u := rr.aType;

              tmpn := TPSValueReplace.Create;
              with TPSValueReplace(tmpn) do
              begin
                FreeOldValue := true;
                FreeNewValue := true;
                OldValue := tmp;
                NewValue := AllocStackReg(u);
                PreWriteAllocated := true;
              end;

              if not WriteCalculation(tmp,TPSValueReplace(tmpn).NewValue) then
              begin
                {MakeError('',ecInternalError,'');}
                x.Free;
                x := nil;
                exit;
              end;
              x.Free;
              x := tmpn;
            end else
            begin
              if not (x is TPSValueVar) then begin
                MakeError('', ecVariableExpected, FParser.GetToken);
                x.Free;
                x := nil;
                exit;
              end;
              ImplicitPeriod := False;
              FParser.Next;
              rr := TPSSubNumber.Create;
              TPSValueVar(x).RecAdd(rr);
              TPSSubNumber(rr).SubNo := t;
              rr.aType := TPSRecordType(u).RecVal(t).FType;
              u := rr.aType;
            end;
          end
          {$IFDEF PS_HAVEVARIANT}
          else if (u.BaseType = btVariant) then break else
          {$ELSE}
          ;
          {$ENDIF}

          begin
            x.Free;
            MakeError('', ecSemicolonExpected, '');
            x := nil;
            exit;
          end;
        end
        else
          break;
      end;
    end;



    procedure CheckClassArrayProperty(var P: TPSValue; const VarType: TPSVariableType; VarNo: Cardinal);
    var
      Tempp: TPSValue;
      aType: TPSClassType;
      procno: Cardinal;
      Idx: TPSDelphiClassItem;
      Decl: TPSParametersDecl;
    begin
      if p = nil then exit;
      if (GetTypeNo(BlockInfo, p) = nil) or (GetTypeNo(BlockInfo, p).BaseType <> btClass) then exit;
      aType := TPSClassType(GetTypeNo(BlockInfo, p));
      if FParser.CurrTokenID = CSTI_OpenBlock then
      begin
        if not TPSClassType(aType).Cl.Property_Find('', Idx) then
        begin
          MakeError('', ecPeriodExpected, '');
          p.Free;
          p := nil;
          exit;
        end;
        if VarNo <> InvalidVal then
        begin
          if @FOnUseVariable <> nil then
           FOnUseVariable(Self, VarType, VarNo, BlockInfo.ProcNo, FParser.CurrTokenPos, '[Default]');
        end;
        Decl := TPSParametersDecl.Create;
        TPSClassType(aType).Cl.Property_GetHeader(Idx,  Decl);
        tempp := p;
        P := TPSValueProcNo.Create;
        with TPSValueProcNo(P) do
        begin
          Parameters := TPSParameters.Create;
          Parameters.Add;
        end;
        if not (ReadParameters(True, TPSValueProc(P).Parameters) and
          ValidateParameters(BlockInfo, TPSValueProc(P).Parameters, Decl)) then
        begin
          tempp.Free;
          Decl.Free;
          p.Free;
          p := nil;
          exit;
        end;
        with TPSValueProcNo(p).Parameters[0] do
        begin
          Val := tempp;
          ExpectedType := GetTypeNo(BlockInfo, tempp);
        end;
        if FParser.CurrTokenId = CSTI_Assignment then
        begin
          FParser.Next;
          TempP := Calc(CSTI_SemiColon);
          if TempP = nil then
          begin
            Decl.Free;
            P.Free;
            p := nil;
            exit;
          end;
          with TPSValueProc(p).Parameters.Add do
          begin
            Val := Tempp;
            ExpectedType := at2ut(Decl.Result);
          end;
          if not TPSClassType(aType).Cl.Property_Set(Idx, procno) then
          begin
            Decl.Free;
            MakeError('', ecReadOnlyProperty, '');
            p.Free;
            p := nil;
            exit;
          end;
          TPSValueProcNo(p).ProcNo := procno;
          TPSValueProcNo(p).ResultType := nil;
        end
        else
        begin
          if not TPSClassType(aType).Cl.Property_Get(Idx, procno) then
          begin
            Decl.Free;
            MakeError('', ecWriteOnlyProperty, '');
            p.Free;
            p := nil;
            exit;
          end;
          TPSValueProcNo(p).ProcNo := procno;
          TPSValueProcNo(p).ResultType := TPSExternalProcedure(FProcs[procno]).RegProc.Decl.Result;
        end; // if FParser.CurrTokenId = CSTI_Assign
        Decl.Free;
      end;
    end;

    procedure CheckExtClass(var P: TPSValue; const VarType: TPSVariableType; VarNo: Cardinal; ImplicitPeriod: Boolean);
    var
      Temp, Idx: Cardinal;
      FType: TPSType;
      s: tbtString;

    begin
      FType := GetTypeNo(BlockInfo, p);
      if FType = nil then Exit;
      if FType.BaseType <> btExtClass then Exit;
      while (FParser.CurrTokenID = CSTI_Period) or (ImplicitPeriod) do
      begin
        if not ImplicitPeriod then
          FParser.Next;
        if FParser.CurrTokenID <> CSTI_Identifier then
        begin
          if ImplicitPeriod then exit;
          MakeError('', ecIdentifierExpected, '');
          p.Free;
          P := nil;
          Exit;
        end;
        s := FParser.GetToken;
        if TPSUndefinedClassType(FType).ExtClass.Func_Find(s, Idx) then
        begin
          FParser.Next;
          TPSUndefinedClassType(FType).ExtClass.Func_Call(Idx, Temp);
          P := ReadProcParameters(Temp, P);
          if p = nil then
          begin
            Exit;
          end;
        end else
        begin
          if ImplicitPeriod then exit;
          MakeError('', ecUnknownIdentifier, s);
          p.Free;
          P := nil;
          Exit;
        end;
        ImplicitPeriod := False;
        FType := GetTypeNo(BlockInfo, p);
        if (FType = nil) or (FType.BaseType <> btExtClass) then Exit;
      end; {while}
    end;

    procedure CheckClass(var P: TPSValue; const VarType: TPSVariableType; VarNo: Cardinal; ImplicitPeriod: Boolean);
    var
      Procno: Cardinal;
      Idx: TPSDelphiClassItem;
      FType: TPSType;
      TempP: TPSValue;
      Decl: TPSParametersDecl;
      s: tbtString;

      pinfo, pinfonew: tbtString;
      ppos: Cardinal;

    begin
      FType := GetTypeNo(BlockInfo, p);
      if FType = nil then exit;
      pinfo := '';
      if (FType.BaseType <> btClass) then Exit;
      while (FParser.CurrTokenID = CSTI_Period) or (ImplicitPeriod) do
      begin
        if not ImplicitPeriod then
          FParser.Next;
        if FParser.CurrTokenID <> CSTI_Identifier then
        begin
          if ImplicitPeriod then exit;
          MakeError('', ecIdentifierExpected, '');
          p.Free;
          P := nil;
          Exit;
        end;
        s := FParser.GetToken;
        if TPSClassType(FType).Cl.Func_Find(s, Idx) then
        begin
          FParser.Next;
          VarNo := InvalidVal;
          TPSClassType(FType).cl.Func_Call(Idx, Procno);
          P := ReadProcParameters(Procno, P);
          if p = nil then
          begin
            Exit;
          end;
        end else if TPSClassType(FType).cl.Property_Find(s, Idx) then
        begin
          ppos := FParser.CurrTokenPos;
          pinfonew := FParser.OriginalToken;
          FParser.Next;
          if VarNo <> InvalidVal then
          begin
            if pinfo = '' then
              pinfo := pinfonew
            else
              pinfo := pinfo + '.' + pinfonew;
            if @FOnUseVariable <> nil then
              FOnUseVariable(Self, VarType, VarNo, BlockInfo.ProcNo, ppos, pinfo);
          end;
          Decl := TPSParametersDecl.Create;
          TPSClassType(FType).cl.Property_GetHeader(Idx, Decl);
          TempP := P;
          p := TPSValueProcNo.Create;
          with TPSValueProcNo(p) do
          begin
            Parameters := TPSParameters.Create;
            Parameters.Add;
            Pos := FParser.CurrTokenPos;
            row := FParser.Row;
            Col := FParser.Col;
          end;
          if Decl.ParamCount <> 0 then
          begin
            if not (ReadParameters(True, TPSValueProc(P).Parameters) and
              ValidateParameters(BlockInfo, TPSValueProc(P).Parameters, Decl)) then
            begin
              Tempp.Free;
              Decl.Free;
              p.Free;
              P := nil;
              exit;
            end;
          end; // if
          with TPSValueProcNo(p).Parameters[0] do
          begin
            Val := TempP;
            ExpectedType := at2ut(GetTypeNo(BlockInfo, TempP));
          end;
          if FParser.CurrTokenId = CSTI_Assignment then
          begin
            FParser.Next;
            TempP := Calc(CSTI_SemiColon);
            if TempP = nil then
            begin
              Decl.Free;
              P.Free;
              p := nil;
              exit;
            end;
            with TPSValueProc(p).Parameters.Add do
            begin
              Val := Tempp;
              ExpectedType := at2ut(Decl.Result);
{$IFDEF DEBUG}
              if not ExpectedType.Used then asm int 3; end;
{$ENDIF}
            end;

            if not TPSClassType(FType).cl.Property_Set(Idx, Procno) then
            begin
              MakeError('', ecReadOnlyProperty, '');
              Decl.Free;
              p.Free;
              p := nil;
              exit;
            end;
            TPSValueProcNo(p).ProcNo := Procno;
            TPSValueProcNo(p).ResultType := nil;
            Decl.Free;
            Exit;
          end else begin
            if not TPSClassType(FType).cl.Property_Get(Idx, Procno) then
            begin
              MakeError('', ecWriteOnlyProperty, '');
              Decl.Free;
              p.Free;
              p := nil;
              exit;
            end;
            TPSValueProcNo(p).ProcNo := ProcNo;
            TPSValueProcNo(p).ResultType := TPSExternalProcedure(FProcs[ProcNo]).RegProc.Decl.Result;
          end; // if FParser.CurrTokenId = CSTI_Assign
          Decl.Free;
        end else
        begin
          if ImplicitPeriod then exit;
          MakeError('', ecUnknownIdentifier, s);
          p.Free;
          P := nil;
          Exit;
        end;
        ImplicitPeriod := False;
        FType := GetTypeNo(BlockInfo, p);
        if (FType = nil) or (FType.BaseType <> btClass) then Exit;
      end; {while}
    end;
{$IFNDEF PS_NOIDISPATCH}
    procedure CheckIntf(var P: TPSValue; const VarType: TPSVariableType; VarNo: Cardinal; ImplicitPeriod: Boolean);
    var
      Procno: Cardinal;
      Idx: TPSInterfaceMethod;
      FType: TPSType;
      s: tbtString;

      CheckArrayProperty,HasArrayProperty:boolean;
    begin
      FType := GetTypeNo(BlockInfo, p);
      if FType = nil then exit;
      if (FType.BaseType <> btInterface) and (Ftype.BaseType <> BtVariant) and (FType.BaseType = btNotificationVariant) then Exit;

      CheckArrayProperty:=(FParser.CurrTokenID=CSTI_OpenBlock) and
        (Ftype.BaseType = BtVariant);
      while (FParser.CurrTokenID = CSTI_Period)
      or (ImplicitPeriod) do begin

        HasArrayProperty:=CheckArrayProperty;
        if CheckArrayProperty then begin
         CheckArrayProperty:=false;
        end else begin
         if not ImplicitPeriod then
          FParser.Next;
        end;
        if FParser.CurrTokenID <> CSTI_Identifier then
        begin
          if ImplicitPeriod then exit;
          if not HasArrayProperty then begin
           MakeError('', ecIdentifierExpected, '');
           p.Free;
           P := nil;
           Exit;
          end;
        end;
        if (FType.BaseType = btVariant) or (FType.BaseType = btNotificationVariant) then
        begin
          if HasArrayProperty then begin
           s:='';
          end else begin
           s := FParser.OriginalToken;
           FParser.Next;
          end;
          ImplicitPeriod := False;
          FType := GetTypeNo(BlockInfo, p);
          p := ReadIDispatchParameters(s, TPSVariantType(FType), p);
          if (FType = nil) or (FType.BaseType <> btInterface) then Exit;
        end else
        begin
          s := FParser.GetToken;
          if (FType is TPSInterfaceType) and (TPSInterfaceType(FType).Intf.Func_Find(s, Idx)) then
          begin
            FParser.Next;
            TPSInterfaceType(FType).Intf.Func_Call(Idx, Procno);
            P := ReadProcParameters(Procno, P);
            if p = nil then
            begin
              Exit;
            end;
          end else
          begin
            if ImplicitPeriod then exit;
            MakeError('', ecUnknownIdentifier, s);
            p.Free;
            P := nil;
            Exit;
          end;
          ImplicitPeriod := False;
          FType := GetTypeNo(BlockInfo, p);
          if (FType = nil) or ((FType.BaseType <> btInterface) and (Ftype.BaseType <> btVariant) and (Ftype.BaseType <> btNotificationVariant)) then Exit;
        end;
      end; {while}
    end;
    {$ENDIF}
    function ExtCheckClassType(FType: TPSType; const ParserPos: Cardinal): TPSValue;
    var
      FType2: TPSType;
      ProcNo, Idx: Cardinal;
      Temp, ResV: TPSValue;
    begin
      if FParser.CurrTokenID = CSTI_OpenRound then
      begin
        FParser.Next;
        Temp := Calc(CSTI_CloseRound);
        if Temp = nil then
        begin
          Result := nil;
          exit;
        end;
        if FParser.CurrTokenID <> CSTI_CloseRound then
        begin
          temp.Free;
          MakeError('', ecCloseRoundExpected, '');
          Result := nil;
          exit;
        end;
        FType2 := GetTypeNo(BlockInfo, Temp);
        if (FType.basetype = BtClass) and (ftype2.BaseType = btClass) and (ftype <> ftype2) then
        begin
          if not TPSUndefinedClassType(FType2).ExtClass.CastToType(AT2UT(FType), ProcNo) then
          begin
            temp.Free;
            MakeError('', ecTypeMismatch, '');
            Result := nil;
            exit;
          end;
          Result := TPSValueProcNo.Create;
          TPSValueProcNo(Result).Parameters := TPSParameters.Create;
          TPSValueProcNo(Result).ResultType := at2ut(FType);
          TPSValueProcNo(Result).ProcNo := ProcNo;
          with TPSValueProcNo(Result).Parameters.Add do
          begin
            Val := Temp;
            ExpectedType := GetTypeNo(BlockInfo, temp);
          end;
          with TPSValueProcNo(Result).Parameters.Add do
          begin
            ExpectedType := at2ut(FindBaseType(btu32));
            Val := TPSValueData.Create;
            with TPSValueData(val) do
            begin
              SetParserPos(FParser);
              Data := NewVariant(ExpectedType);
              Data.tu32 := at2ut(FType).FinalTypeNo;
            end;
          end;
          FParser.Next;
          Exit;
        end;
        if not IsCompatibleType(FType, FType2, True) then
        begin
          temp.Free;
          MakeError('', ecTypeMismatch, '');
          Result := nil;
          exit;
        end;
        FParser.Next;
        Result := TPSUnValueOp.Create;
        with TPSUnValueOp(Result) do
        begin
          Operator := otCast;
          Val1 := Temp;
          SetParserPos(FParser);
          aType := AT2UT(FType);
        end;
        exit;
      end;
      if FParser.CurrTokenId <> CSTI_Period then
      begin
        Result := nil;
        MakeError('', ecPeriodExpected, '');
        Exit;
      end;
      if FType.BaseType <> btExtClass then
      begin
        Result := nil;
        MakeError('', ecClassTypeExpected, '');
        Exit;
      end;
      FParser.Next;
      if not TPSUndefinedClassType(FType).ExtClass.ClassFunc_Find(FParser.GetToken, Idx) then
      begin
        Result := nil;
        MakeError('', ecUnknownIdentifier, FParser.OriginalToken);
        Exit;
      end;
      FParser.Next;
      TPSUndefinedClassType(FType).ExtClass.ClassFunc_Call(Idx, ProcNo);
      Temp := TPSValueData.Create;
      with TPSValueData(Temp) do
      begin
        Data := NewVariant(at2ut(FindBaseType(btu32)));
        Data.tu32 := at2ut(FType).FinalTypeNo;
      end;
      ResV := ReadProcParameters(ProcNo, Temp);
      if ResV <> nil then
      begin
        TPSValueProc(Resv).ResultType := at2ut(FType);
        Result := Resv;
      end else begin
        Result := nil;
      end;
    end;

    function CheckClassType(TypeNo: TPSType; const ParserPos: Cardinal): TPSValue;
    var
      FType2: TPSType;
      ProcNo: Cardinal;
      Idx: IPointer;
      Temp, ResV: TPSValue;
      dta: PIfRVariant;
    begin
      if typeno.BaseType = btExtClass then
      begin
        Result := ExtCheckClassType(TypeNo, PArserPos);
        exit;
      end;
      if FParser.CurrTokenID = CSTI_OpenRound then
      begin
        FParser.Next;
        Temp := Calc(CSTI_CloseRound);
        if Temp = nil then
        begin
          Result := nil;
          exit;
        end;
        if FParser.CurrTokenID <> CSTI_CloseRound then
        begin
          temp.Free;
          MakeError('', ecCloseRoundExpected, '');
          Result := nil;
          exit;
        end;
        FType2 := GetTypeNo(BlockInfo, Temp);
        if ((typeno.BaseType = btClass){$IFNDEF PS_NOINTERFACES} or (TypeNo.basetype = btInterface){$ENDIF}) and
          (ftype2<>nil) and
          ((ftype2.BaseType = btClass){$IFNDEF PS_NOINTERFACES} or (ftype2.BaseType = btInterface){$ENDIF}) and (TypeNo <> ftype2) then
        begin
{$IFNDEF PS_NOINTERFACES}
          if FType2.basetype = btClass then
          begin
{$ENDIF}
          if not TPSClassType(FType2).Cl.CastToType(AT2UT(TypeNo), ProcNo) then
          begin
            temp.Free;
            MakeError('', ecTypeMismatch, '');
            Result := nil;
            exit;
          end;
{$IFNDEF PS_NOINTERFACES}
          end else begin
            if not TPSInterfaceType(FType2).Intf.CastToType(AT2UT(TypeNo), ProcNo) then
            begin
              temp.Free;
              MakeError('', ecTypeMismatch, '');
              Result := nil;
              exit;
            end;
          end;
{$ENDIF}
          Result := TPSValueProcNo.Create;
          TPSValueProcNo(Result).Parameters := TPSParameters.Create;
          TPSValueProcNo(Result).ResultType := at2ut(TypeNo);
          TPSValueProcNo(Result).ProcNo := ProcNo;
          with TPSValueProcNo(Result).Parameters.Add do
          begin
            Val := Temp;
            ExpectedType := GetTypeNo(BlockInfo, temp);
{$IFDEF DEBUG}
            if not ExpectedType.Used then asm int 3; end;
{$ENDIF}
          end;
          with TPSValueProcNo(Result).Parameters.Add do
          begin
            ExpectedType := at2ut(FindBaseType(btu32));
{$IFDEF DEBUG}
            if not ExpectedType.Used then asm int 3; end;
{$ENDIF}
            Val := TPSValueData.Create;
            with TPSValueData(val) do
            begin
              SetParserPos(FParser);
              Data := NewVariant(ExpectedType);
              Data.tu32 := at2ut(TypeNo).FinalTypeNo;
            end;
          end;
          FParser.Next;
          Exit;
        end;
        if (FType2=nil) or not IsCompatibleType(TypeNo, FType2, True) then
        begin
          temp.Free;
          MakeError('', ecTypeMismatch, '');
          Result := nil;
          exit;
        end;
        FParser.Next;
        Result := TPSUnValueOp.Create;
        with TPSUnValueOp(Result) do
        begin
          Operator := otCast;
          Val1 := Temp;
          SetParserPos(FParser);
          aType := AT2UT(TypeNo);
        end;

        exit;
      end else
      if FParser.CurrTokenId <> CSTI_Period then
      begin
        Result := TPSValueData.Create;
        Result.SetParserPos(FParser);
        New(dta);
        TPSValueData(Result).Data := dta;
        InitializeVariant(dta, at2ut(FindBaseType(btType)));
        dta.ttype := at2ut(TypeNo);
        Exit;
      end;
      if TypeNo.BaseType <> btClass then
      begin
        Result := nil;
        MakeError('', ecClassTypeExpected, '');
        Exit;
      end;
      FParser.Next;
      if not TPSClassType(TypeNo).Cl.ClassFunc_Find(FParser.GetToken, Idx) then
      begin
        Result := nil;
        MakeError('', ecUnknownIdentifier, FParser.OriginalToken);
        Exit;
      end;
      FParser.Next;
      TPSClassType(TypeNo).Cl.ClassFunc_Call(Idx, ProcNo);
      Temp := TPSValueData.Create;
      with TPSValueData(Temp) do
      begin
        Data := NewVariant(at2ut(FindBaseType(btu32)));
        Data.tu32 := at2ut(TypeNo).FinalTypeNo;
      end;
      ResV := ReadProcParameters(ProcNo, Temp);
      if ResV <> nil then
      begin
        TPSValueProc(Resv).ResultType := at2ut(TypeNo);
        Result := Resv;
      end else begin
        Result := nil;
      end;
    end;

  function GetIdentifier(const FType: Byte): TPSValue;
    {
      FType:
        0 = Anything
        1 = Only variables
        2 = Not constants
    }


  var
    vt: TPSVariableType;
    vno: Cardinal;
    TWith, Temp: TPSValue;
    l, h: Longint;
    s, u: tbtString;
    t: TPSConstant;
    Temp1: TPSType;
    temp2: CArdinal;
    bi: TPSBlockInfo;
    lOldRecCount: Integer;

  begin
    s := FParser.GetToken;

    if FType <> 1 then
    begin
      bi := BlockInfo;
      while bi <> nil do
      begin
        for l := bi.WithList.Count -1 downto 0 do
        begin
          TWith := TPSValueAllocatedStackVar.Create;
          TPSValueAllocatedStackVar(TWith).LocalVarNo := TPSValueAllocatedStackVar(TPSValueReplace(bi.WithList[l]).NewValue).LocalVarNo;
          Temp := TWith;
          VNo := TPSValueAllocatedStackVar(Temp).LocalVarNo;
          lOldRecCount := TPSValueVar(TWith).GetRecCount;
          vt := ivtVariable;
          if Temp = TWith then CheckFurther(TWith, True);
          if Temp = TWith then CheckClass(TWith, vt, vno, True);
          if Temp = TWith then  CheckExtClass(TWith, vt, vno, True);
          if (Temp <> TWith) or (Cardinal(lOldRecCount) <> TPSValueVar(TWith).GetRecCount) then
          begin
            repeat
              Temp := TWith;
              if TWith <> nil then CheckFurther(TWith, False);
              if TWith <> nil then CheckClass(TWith, vt, vno, False);
              if TWith <> nil then  CheckExtClass(TWith, vt, vno, False);
{$IFNDEF PS_NOIDISPATCH}if TWith <> nil then CheckIntf(TWith, vt, vno, False);{$ENDIF}
              if TWith <> nil then CheckProcCall(TWith);
              if TWith <> nil then CheckClassArrayProperty(TWith, vt, vno);
              vno := InvalidVal;
            until (TWith = nil) or (Temp = TWith);
            Result := TWith;
            Exit;
          end;
          TWith.Free;
        end;
        bi := bi.FOwner;
      end;
    end;

    if s = 'RESULT' then
    begin
      if BlockInfo.proc.Decl.Result = nil then
      begin
        Result := nil;
        MakeError('', ecUnknownIdentifier, FParser.OriginalToken);
      end
      else
      begin
        BlockInfo.Proc.ResultUse;
        Result := TPSValueParamVar.Create;
        with TPSValueParamVar(Result) do
        begin
          SetParserPos(FParser);
          ParamNo := 0;
        end;
        vno := 0;
        vt := ivtParam;
        if @FOnUseVariable <> nil then
          FOnUseVariable(Self, vt, vno, BlockInfo.ProcNo, FParser.CurrTokenPos, '');
        FParser.Next;
        repeat
          Temp := Result;
          if Result <> nil then CheckFurther(Result, False);
          if Result <> nil then CheckClass(Result, vt, vno, False);
          if Result <> nil then  CheckExtClass(Result, vt, vno, False);
{$IFNDEF PS_NOIDISPATCH}if Result <> nil then CheckIntf(Result, vt, vno, False);{$ENDIF}
          if Result <> nil then CheckProcCall(Result);
          if Result <> nil then CheckClassArrayProperty(Result, vt, vno);
          vno := InvalidVal;
        until (Result = nil) or (Temp = Result);
      end;
      exit;
    end;
    if BlockInfo.Proc.Decl.Result = nil then
      l := 0
    else
      l := 1;
    for h := 0 to BlockInfo.proc.Decl.ParamCount -1 do
    begin
      if BlockInfo.proc.Decl.Params[h].Name = s then
      begin
        Result := TPSValueParamVar.Create;
        with TPSValueParamVar(Result) do
        begin
          SetParserPos(FParser);
          ParamNo := l;
        end;
        vt := ivtParam;
        vno := L;
        if @FOnUseVariable <> nil then
          FOnUseVariable(Self, vt, vno, BlockInfo.ProcNo, FParser.CurrTokenPos, '');
        FParser.Next;
        repeat
          Temp := Result;
          if Result <> nil then CheckFurther(Result, False);
          if Result <> nil then CheckClass(Result, vt, vno, False);
          if Result <> nil then  CheckExtClass(Result, vt, vno, False);
{$IFNDEF PS_NOIDISPATCH}if Result <> nil then CheckIntf(Result, vt, vno, False);{$ENDIF}
          if Result <> nil then CheckProcCall(Result);
          if Result <> nil then CheckClassArrayProperty(Result, vt, vno);
          vno := InvalidVal;
        until (Result = nil) or (Temp = Result);
        exit;
      end;
      Inc(l);
      GRFW(u);
    end;

    h := MakeHash(s);

    for l := 0 to BlockInfo.Proc.ProcVars.Count - 1 do
    begin
      if (PIFPSProcVar(BlockInfo.Proc.ProcVars[l]).NameHash = h) and
        (PIFPSProcVar(BlockInfo.Proc.ProcVars[l]).Name = s) then
      begin
        PIFPSProcVar(BlockInfo.Proc.ProcVars[l]).Use;
        vno := l;
        vt := ivtVariable;
        if @FOnUseVariable <> nil then
          FOnUseVariable(Self, vt, vno, BlockInfo.ProcNo, FParser.CurrTokenPos, '');
        Result := TPSValueLocalVar.Create;
        with TPSValueLocalVar(Result) do
        begin
          LocalVarNo := l;
          SetParserPos(FParser);
        end;
        FParser.Next;
        repeat
          Temp := Result;
          if Result <> nil then CheckFurther(Result, False);
          if Result <> nil then CheckClass(Result, vt, vno, False);
          if Result <> nil then  CheckExtClass(Result, vt, vno, False);
{$IFNDEF PS_NOIDISPATCH}if Result <> nil then CheckIntf(Result, vt, vno, False);{$ENDIF}
          if Result <> nil then CheckProcCall(Result);
          if Result <> nil then CheckClassArrayProperty(Result, vt, vno);
          vno := InvalidVal;
        until (Result = nil) or (Temp = Result);

        exit;
      end;
    end;

    for l := 0 to FVars.Count - 1 do
    begin
      if (TPSVar(FVars[l]).NameHash = h) and
        (TPSVar(FVars[l]).Name = s)    {$IFDEF PS_USESSUPPORT} and
        (IsInLocalUnitList(TPSVar(FVars[l]).FDeclareUnit)){$ENDIF} then
      begin
        TPSVar(FVars[l]).Use;
        Result := TPSValueGlobalVar.Create;
        with TPSValueGlobalVar(Result) do
        begin
          SetParserPos(FParser);
          GlobalVarNo := l;

        end;
        vt := ivtGlobal;
        vno := l;
        if @FOnUseVariable <> nil then
          FOnUseVariable(Self, vt, vno, BlockInfo.ProcNo, FParser.CurrTokenPos, '');
        FParser.Next;
        repeat
          Temp := Result;
          if Result <> nil then CheckNotificationVariant(Result);
          if Result <> nil then CheckFurther(Result, False);
          if Result <> nil then CheckClass(Result, vt, vno, False);
          if Result <> nil then  CheckExtClass(Result, vt, vno, False);
{$IFNDEF PS_NOIDISPATCH}if Result <> nil then CheckIntf(Result, vt, vno, False);{$ENDIF}
          if Result <> nil then CheckProcCall(Result);
          if Result <> nil then CheckClassArrayProperty(Result, vt, vno);
          vno := InvalidVal;
        until (Result = nil) or (Temp = Result);
        exit;
      end;
    end;
    Temp1 := FindType(FParser.GetToken);
    if Temp1 <> nil then
    begin
      l := FParser.CurrTokenPos;
      if FType = 1 then
      begin
        Result := nil;
        MakeError('', ecVariableExpected, FParser.OriginalToken);
        exit;
      end;
      vt := ivtGlobal;
      vno := InvalidVal;
      FParser.Next;
      Result := CheckClassType(Temp1, l);
        repeat
          Temp := Result;
          if Result <> nil then CheckFurther(Result, False);
          if Result <> nil then CheckClass(Result, vt, vno, False);
          if Result <> nil then  CheckExtClass(Result, vt, vno, False);
{$IFNDEF PS_NOIDISPATCH}if Result <> nil then CheckIntf(Result, vt, vno, False);{$ENDIF}
          if Result <> nil then CheckProcCall(Result);
          if Result <> nil then CheckClassArrayProperty(Result, vt, vno);
          vno := InvalidVal;
        until (Result = nil) or (Temp = Result);

      exit;
    end;
    Temp2 := FindProc(FParser.GetToken);
    if Temp2 <> InvalidVal then
    begin
      if FType = 1 then
      begin
        Result := nil;
        MakeError('', ecVariableExpected, FParser.OriginalToken);
        exit;
      end;
      FParser.Next;
      Result := ReadProcParameters(Temp2, nil);
      if Result = nil then
        exit;
      Result.SetParserPos(FParser);
      vt := ivtGlobal;
      vno := InvalidVal;
      repeat
        Temp := Result;
        if Result <> nil then CheckFurther(Result, False);
        if Result <> nil then CheckClass(Result, vt, vno, False);
        if Result <> nil then  CheckExtClass(Result, vt, vno, False);
{$IFNDEF PS_NOIDISPATCH}if Result <> nil then CheckIntf(Result, vt, vno, False);{$ENDIF}
        if Result <> nil then CheckProcCall(Result);
        if Result <> nil then CheckClassArrayProperty(Result, vt, vno);
        vno := InvalidVal;
      until (Result = nil) or (Temp = Result);
      exit;
    end;
    for l := 0 to FConstants.Count -1 do
    begin
      t := TPSConstant(FConstants[l]);
      if (t.NameHash = h) and (t.Name = s)     {$IFDEF PS_USESSUPPORT}  and
        (IsInLocalUnitList(t.FDeclareUnit)) {$ENDIF} then
      begin
        if FType <> 0 then
        begin
          Result := nil;
          MakeError('', ecVariableExpected, FParser.OriginalToken);
          exit;
        end;
        fparser.next;
        Result := TPSValueData.Create;
        with TPSValueData(Result) do
        begin
          SetParserPos(FParser);
          Data := NewVariant(at2ut(t.Value.FType));
          CopyVariantContents(t.Value, Data);
        end;
        vt := ivtGlobal;
        vno := InvalidVal;
        repeat
          Temp := Result;
          if Result <> nil then CheckFurther(Result, False);
          if Result <> nil then CheckClass(Result, vt, vno, False);
          if Result <> nil then  CheckExtClass(Result, vt, vno, False);
{$IFNDEF PS_NOIDISPATCH}if Result <> nil then CheckIntf(Result, vt, vno, False);{$ENDIF}
          if Result <> nil then CheckProcCall(Result);
          if Result <> nil then CheckClassArrayProperty(Result, vt, vno);
          vno := InvalidVal;
        until (Result = nil) or (Temp = Result);
        exit;
      end;
    end;
    Result := nil;
    MakeError('', ecUnknownIdentifier, FParser.OriginalToken);
  end;

  function calc(endOn: TPSPasToken): TPSValue;
    function TryEvalConst(var P: TPSValue): Boolean; forward;


    function ReadExpression: TPSValue; forward;
    function ReadTerm: TPSValue; forward;
    function ReadFactor: TPSValue;
    var
      NewVar: TPSValue;
      NewVarU: TPSUnValueOp;
      Proc: TPSProcedure;
      function ReadArray: Boolean;
      var
        tmp: TPSValue;
      begin
        FParser.Next;
        NewVar := TPSValueArray.Create;
        NewVar.SetParserPos(FParser);
        if FParser.CurrTokenID <> CSTI_CloseBlock then
        begin
          while True do
          begin
            tmp := nil;
            Tmp := ReadExpression();
            if Tmp = nil then
            begin
              Result := False;
              NewVar.Free;
              exit;
            end;
            if not TryEvalConst(tmp) then
            begin
              tmp.Free;
              NewVar.Free;
              Result := False;
              exit;
            end;
            TPSValueArray(NewVar).Add(tmp);
            if FParser.CurrTokenID = CSTI_CloseBlock then Break;
            if FParser.CurrTokenID <> CSTI_Comma then
            begin
              MakeError('', ecCloseBlockExpected, '');
              NewVar.Free;
              Result := False;
              exit;
            end;
            FParser.Next;
          end;
        end;
        FParser.Next;
        Result := True;
      end;

      function CallAssigned(P: TPSValue): TPSValue;
      var
        temp: TPSValueProcNo;
      begin
        temp := TPSValueProcNo.Create;
        temp.ProcNo := FindProc('!ASSIGNED');
        temp.ResultType := at2ut(FDefaultBoolType);
        temp.Parameters := TPSParameters.Create;
        with Temp.Parameters.Add do
        begin
          Val := p;
          ExpectedType := GetTypeNo(BlockInfo, p);
{$IFDEF DEBUG}
          if not ExpectedType.Used then asm int 3; end;
{$ENDIF}
          FParamMode := pmIn;
        end;
        Result := Temp;
      end;

      function CallSucc(P: TPSValue): TPSValue;
      var
        temp: TPSBinValueOp;
      begin
        temp := TPSBinValueOp.Create;
        temp.SetParserPos(FParser);
        temp.FOperator := otAdd;
        temp.FVal2 := TPSValueData.Create;
        TPSValueData(Temp.FVal2).Data := NewVariant(FindBaseType(bts32));
        TPSValueData(Temp.FVal2).Data.ts32 := 1;
        temp.FVal1 := p;
        Temp.FType := GetTypeNo(BlockInfo, P);
        result := temp;
      end;

      function CallPred(P: TPSValue): TPSValue;
      var
        temp: TPSBinValueOp;
      begin
        temp := TPSBinValueOp.Create;
        temp.SetParserPos(FParser);
        temp.FOperator := otSub;
        temp.FVal2 := TPSValueData.Create;
        TPSValueData(Temp.FVal2).Data := NewVariant(FindBaseType(bts32));
        TPSValueData(Temp.FVal2).Data.ts32 := 1;
        temp.FVal1 := p;
        Temp.FType := GetTypeNo(BlockInfo, P);
        result := temp;
      end;

    begin
      case fParser.CurrTokenID of
        CSTI_OpenBlock:
          begin
            if not ReadArray then
            begin
              Result := nil;
              exit;
            end;
          end;
        CSTII_Not:
        begin
          FParser.Next;
          NewVar := ReadFactor;
          if NewVar = nil then
          begin
            Result := nil;
            exit;
          end;
          NewVarU := TPSUnValueOp.Create;
          NewVarU.SetParserPos(FParser);
          NewVarU.aType := GetTypeNo(BlockInfo, NewVar);
          NewVarU.Operator := otNot;
          NewVarU.Val1 := NewVar;
          NewVar := NewVarU;
        end;
        CSTI_Plus:
        begin
          FParser.Next;
          NewVar := ReadTerm;
          if NewVar = nil then
          begin
            Result := nil;
            exit;
          end;
        end;
        CSTI_Minus:
        begin
          FParser.Next;
          NewVar := ReadTerm;
          if NewVar = nil then
          begin
            Result := nil;
            exit;
          end;
          NewVarU := TPSUnValueOp.Create;
          NewVarU.SetParserPos(FParser);
          NewVarU.aType := GetTypeNo(BlockInfo, NewVar);
          NewVarU.Operator := otMinus;
          NewVarU.Val1 := NewVar;
          NewVar := NewVarU;
        end;
        CSTII_Nil:
          begin
            FParser.Next;
            NewVar := TPSValueNil.Create;
            NewVar.SetParserPos(FParser);
          end;
        CSTI_AddressOf:
          begin
            FParser.Next;
            if FParser.CurrTokenID <> CSTI_Identifier then
            begin
              MakeError('', ecIdentifierExpected, '');
              Result := nil;
              exit;
            end;
            NewVar := TPSValueProcPtr.Create;
            NewVar.SetParserPos(FParser);
            TPSValueProcPtr(NewVar).ProcPtr := FindProc(FParser.GetToken);
            if TPSValueProcPtr(NewVar).ProcPtr = InvalidVal then
            begin
              MakeError('', ecUnknownIdentifier, FParser.OriginalToken);
              NewVar.Free;
              Result := nil;
              exit;
            end;
            Proc := FProcs[TPSValueProcPtr(NewVar).ProcPtr];
            if Proc.ClassType <> TPSInternalProcedure then
            begin
              MakeError('', ecUnknownIdentifier, FParser.OriginalToken);
              NewVar.Free;
              Result := nil;
              exit;
            end;
            FParser.Next;
          end;
        CSTI_OpenRound:
          begin
            FParser.Next;
            NewVar := ReadExpression();
            if NewVar = nil then
            begin
              Result := nil;
              exit;
            end;
            if FParser.CurrTokenId <> CSTI_CloseRound then
            begin
              NewVar.Free;
              Result := nil;
              MakeError('', ecCloseRoundExpected, '');
              exit;
            end;
            FParser.Next;
          end;
        CSTI_Char, CSTI_String:
          begin
            NewVar := TPSValueData.Create;
            NewVar.SetParserPos(FParser);
            TPSValueData(NewVar).Data := ReadString;
            if TPSValueData(NewVar).Data = nil then
            begin
              NewVar.Free;
              Result := nil;
              exit;
            end;
          end;
        CSTI_HexInt, CSTI_Integer:
          begin
            NewVar := TPSValueData.Create;
            NewVar.SetParserPos(FParser);
            TPSValueData(NewVar).Data := ReadInteger(FParser.GetToken);
            FParser.Next;
          end;
        CSTI_Real:
          begin
            NewVar := TPSValueData.Create;
            NewVar.SetParserPos(FParser);
            TPSValueData(NewVar).Data := ReadReal(FParser.GetToken);
            FParser.Next;
          end;
        CSTII_Ord:
          begin
            FParser.Next;
            if fParser.Currtokenid <> CSTI_OpenRound then
            begin
              Result := nil;
              MakeError('', ecOpenRoundExpected, '');
              exit;
            end;
            FParser.Next;
            NewVar := ReadExpression();
            if NewVar = nil then
            begin
              Result := nil;
              exit;
            end;
            if FParser.CurrTokenId <> CSTI_CloseRound then
            begin
              NewVar.Free;
              Result := nil;
              MakeError('', ecCloseRoundExpected, '');
              exit;
            end;
            if not ((GetTypeNo(BlockInfo, NewVar).BaseType = btChar) or
            {$IFNDEF PS_NOWIDESTRING} (GetTypeNo(BlockInfo, NewVar).BaseType = btWideChar) or{$ENDIF}
            (GetTypeNo(BlockInfo, NewVar).BaseType = btEnum) or (IsIntType(GetTypeNo(BlockInfo, NewVar).BaseType))) then
            begin
              NewVar.Free;
              Result := nil;
              MakeError('', ecTypeMismatch, '');
              exit;
            end;
            NewVarU := TPSUnValueOp.Create;
            NewVarU.SetParserPos(FParser);
            NewVarU.Operator := otCast;
            NewVarU.FType := at2ut(FindBaseType(btu32));
            NewVarU.Val1 := NewVar;
            NewVar := NewVarU;
            FParser.Next;
          end;
        CSTII_Chr:
          begin
            FParser.Next;
            if fParser.Currtokenid <> CSTI_OpenRound then
            begin
              Result := nil;
              MakeError('', ecOpenRoundExpected, '');
              exit;
            end;
            FParser.Next;
            NewVar := ReadExpression();
            if NewVar = nil then
            begin
              Result := nil;
              exit;
            end;
            if FParser.CurrTokenId <> CSTI_CloseRound then
            begin
              NewVar.Free;
              Result := nil;
              MakeError('', ecCloseRoundExpected, '');
              exit;
            end;
            if not (IsIntType(GetTypeNo(BlockInfo, NewVar).BaseType)) then
            begin
              NewVar.Free;
              Result := nil;
              MakeError('', ecTypeMismatch, '');
              exit;
            end;
            NewVarU := TPSUnValueOp.Create;
            NewVarU.SetParserPos(FParser);
            NewVarU.Operator := otCast;
            NewVarU.FType := at2ut(FindBaseType(btChar));
            NewVarU.Val1 := NewVar;
            NewVar := NewVarU;
            FParser.Next;
          end;
        CSTI_Identifier:
          begin
            if FParser.GetToken = 'SUCC' then
            begin
              FParser.Next;
              if FParser.CurrTokenID <> CSTI_OpenRound then
              begin
                Result := nil;
                MakeError('', ecOpenRoundExpected, '');
                exit;
              end;
              FParser.Next;
              NewVar := ReadExpression;
              if NewVar = nil then
              begin
                result := nil;
                exit;
              end;
              if (GetTypeNo(BlockInfo, NewVar) = nil) or (not IsIntType(GetTypeNo(BlockInfo, NewVar).BaseType) and
                (GetTypeNo(BlockInfo, NewVar).BaseType <> btEnum)) then
              begin
                NewVar.Free;
                Result := nil;
                MakeError('', ecTypeMismatch, '');
                exit;
              end;
              if FParser.CurrTokenID <> CSTI_CloseRound then
              begin
                NewVar.Free;
                Result := nil;
                MakeError('', eccloseRoundExpected, '');
                exit;
              end;
              NewVar := CallSucc(NewVar);
              FParser.Next;
            end else
            if FParser.GetToken = 'PRED' then
            begin
              FParser.Next;
              if FParser.CurrTokenID <> CSTI_OpenRound then
              begin
                Result := nil;
                MakeError('', ecOpenRoundExpected, '');
                exit;
              end;
              FParser.Next;
              NewVar := ReadExpression;
              if NewVar = nil then
              begin
                result := nil;
                exit;
              end;
              if (GetTypeNo(BlockInfo, NewVar) = nil) or (not IsIntType(GetTypeNo(BlockInfo, NewVar).BaseType) and
                (GetTypeNo(BlockInfo, NewVar).BaseType <> btEnum)) then
              begin
                NewVar.Free;
                Result := nil;
                MakeError('', ecTypeMismatch, '');
                exit;
              end;
              if FParser.CurrTokenID <> CSTI_CloseRound then
              begin
                NewVar.Free;
                Result := nil;
                MakeError('', eccloseRoundExpected, '');
                exit;
              end;
              NewVar := CallPred(NewVar);
              FParser.Next;
            end else
            if FParser.GetToken = 'ASSIGNED' then
            begin
              FParser.Next;
              if FParser.CurrTokenID <> CSTI_OpenRound then
              begin
                Result := nil;
                MakeError('', ecOpenRoundExpected, '');
                exit;
              end;
              FParser.Next;
              NewVar := GetIdentifier(0);
              if NewVar = nil then
              begin
                result := nil;
                exit;
              end;
              if (GetTypeNo(BlockInfo, NewVar) = nil) or 
                ((GetTypeNo(BlockInfo, NewVar).BaseType <> btClass) and
                (GetTypeNo(BlockInfo, NewVar).BaseType <> btInterface) and
                (GetTypeNo(BlockInfo, NewVar).BaseType <> btPChar) and
                (GetTypeNo(BlockInfo, NewVar).BaseType <> btString)) then
              begin
                NewVar.Free;
                Result := nil;
                MakeError('', ecTypeMismatch, '');
                exit;
              end;
              if FParser.CurrTokenID <> CSTI_CloseRound then
              begin
                NewVar.Free;
                Result := nil;
                MakeError('', eccloseRoundExpected, '');
                exit;
              end;
              NewVar := CallAssigned(NewVar);
              FParser.Next;
            end  else
            begin
              NewVar := GetIdentifier(0);
              if NewVar = nil then
              begin
                Result := nil;
                exit;
              end;
            end;
          end;
      else
        begin
          MakeError('', ecSyntaxError, '');
          Result := nil;
          exit;
        end;
      end; {case}
      Result := NewVar;
    end; // ReadFactor

    function GetResultType(p1, P2: TPSValue; Cmd: TPSBinOperatorType): TPSType;
    var
      pp, t1, t2: PIFPSType;
    begin
      t1 := GetTypeNo(BlockInfo, p1);
      t2 := GetTypeNo(BlockInfo, P2);
      if (t1 = nil) or (t2 = nil) then
      begin
        if ((p1.ClassType = TPSValueNil) or (p2.ClassType = TPSValueNil)) and ((t1 <> nil) or (t2 <> nil)) then
        begin
          if p1.ClassType = TPSValueNil then
            pp := t2
          else
            pp := t1;
          if (pp.BaseType = btPchar) or (pp.BaseType = btString) or (pp.BaseType = btClass) {$IFNDEF PS_NOINTERFACES}or (pp.BaseType =btInterface){$ENDIF} or (pp.BaseType = btProcPtr) then
            Result := AT2UT(FDefaultBoolType)
          else
            Result := nil;
          exit;
        end;
        Result := nil;
        exit;
      end;
      case Cmd of
        otAdd: {plus}
          begin
            if ((t1.BaseType = btVariant) or (t1.BaseType = btNotificationVariant)) and (
              ((t2.BaseType = btVariant) or (t2.BaseType = btNotificationVariant)) or
              (t2.BaseType = btString) or
              {$IFNDEF PS_NOWIDESTRING}
              (t2.BaseType = btwideString) or
              (t2.BaseType = btUnicodestring) or
              (t2.BaseType = btwidechar) or
              {$ENDIF}
              (t2.BaseType = btPchar) or
              (t2.BaseType = btChar) or
              (isIntRealType(t2.BaseType))) then
              Result := t1
            else
            if ((t2.BaseType = btVariant) or (t2.BaseType = btNotificationVariant)) and (
              ((t1.BaseType = btVariant) or (t1.BaseType = btNotificationVariant)) or
              (t1.BaseType = btString) or
              {$IFNDEF PS_NOWIDESTRING}
              (t1.BaseType = btUnicodestring) or
              (t1.BaseType = btwideString) or
              (t1.BaseType = btwidechar) or
              {$ENDIF}
              (t1.BaseType = btPchar) or
              (t1.BaseType = btChar) or
              (isIntRealType(t1.BaseType))) then
              Result := t2
            else if ((t1.BaseType = btSet) and (t2.BaseType = btSet)) and (t1 = t2) then
              Result := t1
            else if IsIntType(t1.BaseType) and IsIntType(t2.BaseType) then
              Result := t1
            else if IsIntRealType(t1.BaseType) and
              IsIntRealType(t2.BaseType) then
            begin
              if IsRealType(t1.BaseType) then
                Result := t1
              else
                Result := t2;
            end
            else if (t1.basetype = btSet) and (t2.Name = 'TVARIANTARRAY') then
              Result := t1
            else if (t2.basetype = btSet) and (t1.Name = 'TVARIANTARRAY') then
              Result := t2
            else if ((t1.BaseType = btPchar) or(t1.BaseType = btString) or (t1.BaseType = btChar)) and ((t2.BaseType = btPchar) or(t2.BaseType = btString) or (t2.BaseType = btChar)) then
              Result := at2ut(FindBaseType(btString))
            {$IFNDEF PS_NOWIDESTRING}
            else if ((t1.BaseType = btString) or (t1.BaseType = btChar) or (t1.BaseType = btPchar)or (t1.BaseType = btWideString) or (t1.BaseType = btWideChar) or (t1.BaseType = btUnicodeString)) and
            ((t2.BaseType = btString) or (t2.BaseType = btChar) or (t2.BaseType = btPchar) or (t2.BaseType = btWideString) or (t2.BaseType = btWideChar) or (t2.BaseType = btUnicodeString)) then
              Result := at2ut(FindBaseType(btUnicodeString))
            {$ENDIF}
            else
              Result := nil;
          end;

        otSub, otMul, otIntDiv, otDiv: { -  * / }
          begin
            if ((t1.BaseType = btVariant) or (t1.BaseType = btNotificationVariant)) and (
              ((t2.BaseType = btVariant) or (t2.BaseType = btNotificationVariant)) or
              (isIntRealType(t2.BaseType))) then
            begin
              Result := t1;
{$IFDEF PS_DELPHIDIV}
              if Cmd = otDiv then
                result := FindBaseType(btExtended);
{$ENDIF}
            end
            else if ((t1.BaseType = btSet) and (t2.BaseType = btSet)) and (t1 = t2) and ((cmd = otSub) or (cmd = otMul))  then
              Result := t1
            else if (t1.basetype = btSet) and (t2.Name = 'TVARIANTARRAY') and ((cmd = otSub) or (cmd = otMul)) then
              Result := t1
            else if (t2.basetype = btSet) and (t1.Name = 'TVARIANTARRAY') and ((cmd = otSub) or (cmd = otMul)) then
              Result := t2
            else
            if ((t2.BaseType = btVariant) or (t2.BaseType = btNotificationVariant)) and (
              ((t1.BaseType = btVariant) or (t1.BaseType = btNotificationVariant)) or
              (isIntRealType(t1.BaseType))) then
            begin
              Result := t2;
{$IFDEF PS_DELPHIDIV}
              if Cmd = otDiv then
                result := FindBaseType(btExtended);
{$ENDIF}
            end
            else if IsIntType(t1.BaseType) and IsIntType(t2.BaseType) then begin
              Result := t1;
{$IFDEF PS_DELPHIDIV}
              if Cmd = otDiv then
                result := FindBaseType(btExtended);
{$ENDIF}
            end else if IsIntRealType(t1.BaseType) and
              IsIntRealType(t2.BaseType) then
            begin
              if IsRealType(t1.BaseType) then
                Result := t1
              else
                Result := t2;
{$IFDEF PS_DELPHIDIV}
              if Cmd = otIntDiv then //intdiv only works
                result := nil;
{$ENDIF}
            end
            else
              Result := nil;
          end;
        otAnd, otOr, otXor: {and,or,xor}
          begin
            if ((t1.BaseType = btVariant) or (t1.BaseType = btNotificationVariant)) and (
              ((t2.BaseType = btVariant) or (t2.BaseType = btNotificationVariant)) or
              (isIntType(t2.BaseType))) then
              Result := t1
            else
            if ((t2.BaseType = btVariant) or (t2.BaseType = btNotificationVariant)) and (
              ((t1.BaseType = btVariant) or (t1.BaseType = btNotificationVariant)) or
              (isIntType(t1.BaseType))) then
              Result := t2
            else if IsIntType(t1.BaseType) and IsIntType(t2.BaseType) then
              Result := t1
            else if (IsBoolean(t1)) and ((t2 = t1)  or ((t2.BaseType = btVariant)
              or (t2.BaseType = btNotificationVariant))) then
            begin
              Result := t1;
              if ((p1.ClassType = TPSValueData) or (p2.ClassType = TPSValueData)) then
              begin
                if cmd = otAnd then {and}
                begin
                  if p1.ClassType = TPSValueData then
                  begin
                    if (TPSValueData(p1).FData^.tu8 <> 0) then
                    begin
                      with MakeWarning('', ewIsNotNeeded, '"True and"') do
                      if p1.Pos>0 then
                      begin
                        FRow := p1.Row;
                        FCol := p1.Col;
                        FPosition := p1.Pos;
                      end;
                    end else
                    begin
                      with MakeWarning('', ewCalculationAlwaysEvaluatesTo, 'False') do
                      begin
                        FRow := p1.Row;
                        FCol := p1.Col;
                        FPosition := p1.Pos;
                      end;
                    end;
                  end else begin
                    if (TPSValueData(p2).Data.tu8 <> 0) then
                    begin
                      with MakeWarning('', ewIsNotNeeded, '"and True"') do
                      if p2.Pos>0 then
                      begin
                        FRow := p2.Row;
                        FCol := p2.Col;
                        FPosition := p2.Pos;
                      end;
                    end
                    else
                    begin
                      with MakeWarning('', ewCalculationAlwaysEvaluatesTo, 'False') do
                      begin
                        FRow := p2.Row;
                        FCol := p2.Col;
                        FPosition := p2.Pos;
                      end;
                    end;
                  end;
                end else if cmd = otOr then {or}
                begin
                  if p1.ClassType = TPSValueData then
                  begin
                    if (TPSValueData(p1).Data.tu8 <> 0) then
                    begin
                      with MakeWarning('', ewCalculationAlwaysEvaluatesTo, 'True') do
                      begin
                        FRow := p1.Row;
                        FCol := p1.Col;
                        FPosition := p1.Pos;
                      end;
                    end
                    else
                    begin
                      with MakeWarning('', ewIsNotNeeded, '"False or"') do
                      begin
                        FRow := p1.Row;
                        FCol := p1.Col;
                        FPosition := p1.Pos;
                      end;
                    end
                  end else begin
                    if (TPSValueData(p2).Data.tu8 <> 0) then
                    begin
                      with MakeWarning('', ewCalculationAlwaysEvaluatesTo, 'True') do
                      begin
                        FRow := p2.Row;
                        FCol := p2.Col;
                        FPosition := p1.Pos;
                      end;
                    end
                    else
                    begin
                      with MakeWarning('', ewIsNotNeeded, '"or False"') do
                      begin
                        FRow := p2.Row;
                        FCol := p2.Col;
                        FPosition := p2.Pos;
                      end;
                    end
                  end;
                end;
              end;
            end else
              Result := nil;
          end;
        otMod, otShl, otShr: {mod,shl,shr}
          begin
            if ((t1.BaseType = btVariant) or (t1.BaseType = btNotificationVariant)) and (
              ((t2.BaseType = btVariant) or (t2.BaseType = btNotificationVariant)) or
              (isIntType(t2.BaseType))) then
              Result := t1
            else
            if ((t2.BaseType = btVariant) or (t2.BaseType = btNotificationVariant)) and (
              ((t1.BaseType = btVariant) or (t1.BaseType = btNotificationVariant)) or
              (isIntType(t1.BaseType))) then
              Result := t2
            else if IsIntType(t1.BaseType) and IsIntType(t2.BaseType) then
              Result :=  t1
            else
              Result := nil;
          end;
        otGreater, otLess, otGreaterEqual, otLessEqual: { >=, <=, >, <}
          begin
            if ((t1.BaseType = btVariant) or (t1.BaseType = btNotificationVariant)) and (
              ((t2.BaseType = btVariant) or (t2.BaseType = btNotificationVariant)) or
              (t2.BaseType = btString) or
              (t2.BaseType = btPchar) or
              (t2.BaseType = btChar) or
              (isIntRealType(t2.BaseType))) then
              Result := FDefaultBoolType
            else if ((t1.BaseType = btSet) and (t2.BaseType = btSet)) and (t1 = t2) and ((cmd = otGreaterEqual) or (cmd = otLessEqual))  then
              Result := FDefaultBoolType
            else
            if ((t2.BaseType = btVariant) or (t2.BaseType = btNotificationVariant)) and (
              ((t1.BaseType = btVariant) or (t1.BaseType = btNotificationVariant)) or
              (t1.BaseType = btString) or
              (t1.BaseType = btPchar) or
              (t1.BaseType = btChar) or
              (isIntRealType(t1.BaseType))) then
              Result := FDefaultBoolType
            else if IsIntType(t1.BaseType) and IsIntType(t2.BaseType) then
              Result := FDefaultBoolType
            else if IsIntRealType(t1.BaseType) and
              IsIntRealType(t2.BaseType) then
              Result := FDefaultBoolType
            else if
            ((t1.BaseType = btString) or (t1.BaseType = btChar) {$IFNDEF PS_NOWIDESTRING} or (t1.BaseType = btWideString) or (t1.BaseType = btWideChar) or (t1.BaseType = btUnicodestring){$ENDIF}) and
            ((t2.BaseType = btString) or (t2.BaseType = btChar) {$IFNDEF PS_NOWIDESTRING} or (t2.BaseType = btWideString) or (t2.BaseType = btWideChar) or (t2.BaseType = btUnicodestring){$ENDIF}) then
              Result := FDefaultBoolType
            else if ((t1.BaseType = btVariant) or (t1.BaseType = btNotificationVariant)) or ((t2.BaseType = btVariant) or (t2.BaseType = btNotificationVariant)) then
              Result := FDefaultBoolType
            else
              Result := nil;
          end;
        otEqual, otNotEqual: {=, <>}
          begin
            if ((t1.BaseType = btVariant) or (t1.BaseType = btNotificationVariant)) and (
              ((t2.BaseType = btVariant) or (t2.BaseType = btNotificationVariant)) or
              (t2.BaseType = btString) or
              (t2.BaseType = btPchar) or
              (t2.BaseType = btChar) or
              (isIntRealType(t2.BaseType))) then
              Result := FDefaultBoolType
            else if ((t1.BaseType = btSet) and (t2.BaseType = btSet)) and (t1 = t2) then
              Result := FDefaultBoolType
            else
            if ((t2.BaseType = btVariant) or (t2.BaseType = btNotificationVariant)) and (
              ((t1.BaseType = btVariant) or (t1.BaseType = btNotificationVariant)) or
              (t1.BaseType = btString) or
              (t1.BaseType = btPchar) or
              (t1.BaseType = btChar) or
              (isIntRealType(t1.BaseType))) then
              Result := FDefaultBoolType
            else if IsIntType(t1.BaseType) and IsIntType(t2.BaseType) then
              Result := FDefaultBoolType
            else if IsIntRealType(t1.BaseType) and
              IsIntRealType(t2.BaseType) then
              Result := FDefaultBoolType
            else if
            ((t1.BaseType = btString) or (t1.BaseType = btChar) {$IFNDEF PS_NOWIDESTRING} or (t1.BaseType = btWideString) or (t1.BaseType = btWideChar)  or (t1.BaseType = btUnicodestring){$ENDIF}) and
            ((t2.BaseType = btString) or (t2.BaseType = btChar) {$IFNDEF PS_NOWIDESTRING} or (t2.BaseType = btWideString) or (t2.BaseType = btWideChar)  or (t2.BaseType = btUnicodestring){$ENDIF}) then
              Result := FDefaultBoolType
            else if (t1.basetype = btSet) and (t2.Name = 'TVARIANTARRAY') then
              Result := FDefaultBoolType
            else if (t2.basetype = btSet) and (t1.Name = 'TVARIANTARRAY') then
              Result := FDefaultBoolType
            else if (t1.BaseType = btEnum) and (t1 = t2) then
              Result := FDefaultBoolType
            else if (t1.BaseType = btClass) and (t2.BaseType = btClass) then
              Result := FDefaultBoolType
            else if (t1 = t2) then
              Result := FDefaultBoolType
            else if ((t1.BaseType = btVariant) or (t1.BaseType = btNotificationVariant)) or ((t2.BaseType = btVariant) or (t2.BaseType = btNotificationVariant)) then
              Result := FDefaultBoolType
            else Result := nil;
          end;
        otIn:
          begin
            if (t2.Name = 'TVARIANTARRAY')  then
              Result := FDefaultBoolType
            else
            if (t2.BaseType = btSet) and (TPSSetType(t2).SetType = t1) then
              Result := FDefaultBoolType
            else
              Result := nil;
          end;
        otIs:
          begin
            if t2.BaseType = btType then
            begin
              Result := FDefaultBoolType
            end else
            Result := nil;
          end;
        otAs:
          begin
            if t2.BaseType = btType then
            begin
              Result := at2ut(TPSValueData(p2).Data.ttype);
            end else
              Result := nil;
          end;
      else
        Result := nil;
      end;
    end;


    function ReadTerm: TPSValue;
    var
      F1, F2: TPSValue;
      fType: TPSType;
      F: TPSBinValueOp;
      Token: TPSPasToken;
      Op: TPSBinOperatorType;
    begin
      F1 := ReadFactor;
      if F1 = nil then
      begin
        Result := nil;
        exit;
      end;
      while FParser.CurrTokenID in [CSTI_Multiply, CSTI_Divide, CSTII_Div, CSTII_Mod, CSTII_And, CSTII_Shl, CSTII_Shr, CSTII_As] do
      begin
        Token := FParser.CurrTokenID;
        FParser.Next;
        F2 := ReadFactor;
        if f2 = nil then
        begin
          f1.Free;
          Result := nil;
          exit;
        end;
        case Token of
          CSTI_Multiply: Op := otMul;
          CSTI_Divide: Op := otDiv;
          CSTII_div: Op := otIntDiv;
          CSTII_mod: Op := otMod;
          CSTII_and: Op := otAnd;
          CSTII_shl: Op := otShl;
          CSTII_shr: Op := otShr;
          CSTII_As:  Op := otAs;
        else
          Op := otAdd;
        end;
        if (Op = otAs) and (f2 is TPSValueData) and (TPSValueData(f2).Data.FType.BaseType = btType) then begin
          fType := TPSValueData(f2).Data.ttype;
          f2.Free;
          f2 := TPSUnValueOp.Create;
          TPSUnValueOp(F2).Val1 := f1;
          TPSUnValueOp(F2).SetParserPos(FParser);
          TPSUnValueOp(f2).FType := fType;
          TPSUnValueOp(f2).Operator := otCast;
          f1 := f2;
        end else begin
          F := TPSBinValueOp.Create;
          f.Val1 := F1;
          f.Val2 := F2;
          f.Operator := Op;
          f.aType := GetResultType(F1, F2, Op);
          if f.aType = nil then
          begin
            MakeError('', ecTypeMismatch, '');
            f.Free;
            Result := nil;
            exit;
          end;
          f1 := f;
        end;
      end;
      Result := F1;
    end;  // ReadTerm

    function ReadSimpleExpression: TPSValue;
    var
      F1, F2: TPSValue;
      F: TPSBinValueOp;
      Token: TPSPasToken;
      Op: TPSBinOperatorType;
    begin
      F1 := ReadTerm;
      if F1 = nil then
      begin
        Result := nil;
        exit;
      end;
      while FParser.CurrTokenID in [CSTI_Plus, CSTI_Minus, CSTII_Or, CSTII_Xor] do
      begin
        Token := FParser.CurrTokenID;
        FParser.Next;
        F2 := ReadTerm;
        if f2 = nil then
        begin
          f1.Free;
          Result := nil;
          exit;
        end;
        case Token of
          CSTI_Plus: Op := otAdd;
          CSTI_Minus: Op := otSub;
          CSTII_or: Op := otOr;
          CSTII_xor: Op := otXor;
        else
          Op := otAdd;
        end;
        F := TPSBinValueOp.Create;
        f.Val1 := F1;
        f.Val2 := F2;
        f.Operator := Op;
        f.aType := GetResultType(F1, F2, Op);
        if f.aType = nil then
        begin
          MakeError('', ecTypeMismatch, '');
          f.Free;
          Result := nil;
          exit;
        end;
        f1 := f;
      end;
      Result := F1;
    end;  // ReadSimpleExpression


    function ReadExpression: TPSValue;
    var
      F1, F2: TPSValue;
      F: TPSBinValueOp;
      Token: TPSPasToken;
      Op: TPSBinOperatorType;
    begin
      F1 := ReadSimpleExpression;
      if F1 = nil then
      begin
        Result := nil;
        exit;
      end;
      while FParser.CurrTokenID in [ CSTI_GreaterEqual, CSTI_LessEqual, CSTI_Greater, CSTI_Less, CSTI_Equal, CSTI_NotEqual, CSTII_in, CSTII_is] do
      begin
        Token := FParser.CurrTokenID;
        FParser.Next;
        F2 := ReadSimpleExpression;
        if f2 = nil then
        begin
          f1.Free;
          Result := nil;
          exit;
        end;
        case Token of
          CSTI_GreaterEqual: Op := otGreaterEqual;
          CSTI_LessEqual: Op := otLessEqual;
          CSTI_Greater: Op := otGreater;
          CSTI_Less: Op := otLess;
          CSTI_Equal: Op := otEqual;
          CSTI_NotEqual: Op := otNotEqual;
          CSTII_in: Op := otIn;
          CSTII_is: Op := otIs;
        else
          Op := otAdd;
        end;
        F := TPSBinValueOp.Create;
        f.Val1 := F1;
        f.Val2 := F2;
        f.Operator := Op;
        f.aType := GetResultType(F1, F2, Op);
        if f.aType = nil then
        begin
          MakeError('', ecTypeMismatch, '');
          f.Free;
          Result := nil;
          exit;
        end;
        f1 := f;
      end;
      Result := F1;
    end;  // ReadExpression

    function TryEvalConst(var P: TPSValue): Boolean;
    var
      preplace: TPSValue;
    begin
      if p is TPSBinValueOp then
      begin
        if not (TryEvalConst(TPSBinValueOp(p).FVal1) and TryEvalConst(TPSBinValueOp(p).FVal2)) then
        begin
          Result := False;
          exit;
        end;
        if (TPSBinValueOp(p).FVal1.ClassType = TPSValueData) and (TPSBinValueOp(p).FVal2.ClassType = TPSValueData) then
        begin
          if not PreCalc(True, 0, TPSValueData(TPSBinValueOp(p).Val1).Data, 0, TPSValueData(TPSBinValueOp(p).Val2).Data, TPSBinValueOp(p).Operator, p.Pos, p.Row, p.Col) then
          begin
            Result := False;
            exit;
          end;
          preplace := TPSValueData.Create;
          preplace.Pos := p.Pos;
          preplace.Row := p.Row;
          preplace.Col := p.Col;
          TPSValueData(preplace).Data := TPSValueData(TPSBinValueOp(p).Val1).Data;
          TPSValueData(TPSBinValueOp(p).Val1).Data := nil;
          p.Free;
          p := preplace;
        end;
      end else if p is TPSUnValueOp then
      begin
        if not TryEvalConst(TPSUnValueOp(p).FVal1) then
        begin
          Result := False;
          exit;
        end;
        if TPSUnValueOp(p).FVal1.ClassType = TPSValueData then
        begin
//
          case TPSUnValueOp(p).Operator of
            otNot:
              begin
                case TPSValueData(TPSUnValueOp(p).FVal1).Data^.FType.BaseType of
                  btEnum:
                    begin
                      if IsBoolean(TPSValueData(TPSUnValueOp(p).FVal1).Data^.FType) then
                      begin
                        TPSValueData(TPSUnValueOp(p).FVal1).Data^.tu8 := (not TPSValueData(TPSUnValueOp(p).FVal1).Data^.tu8) and 1;
                      end else
                      begin
                        MakeError('', ecTypeMismatch, '');
                        Result := False;
                        exit;
                      end;
                    end;
                  btU8: TPSValueData(TPSUnValueOp(p).FVal1).Data^.tu8 := not TPSValueData(TPSUnValueOp(p).FVal1).Data^.tu8;
                  btU16: TPSValueData(TPSUnValueOp(p).FVal1).Data^.tu16 := not TPSValueData(TPSUnValueOp(p).FVal1).Data^.tu16;
                  btU32: TPSValueData(TPSUnValueOp(p).FVal1).Data^.tu32 := not TPSValueData(TPSUnValueOp(p).FVal1).Data^.tu32;
                  bts8: TPSValueData(TPSUnValueOp(p).FVal1).Data^.ts8 := not TPSValueData(TPSUnValueOp(p).FVal1).Data^.ts8;
                  bts16: TPSValueData(TPSUnValueOp(p).FVal1).Data^.ts16 := not TPSValueData(TPSUnValueOp(p).FVal1).Data^.ts16;
                  bts32: TPSValueData(TPSUnValueOp(p).FVal1).Data^.ts32 := not TPSValueData(TPSUnValueOp(p).FVal1).Data^.ts32;
                  {$IFNDEF PS_NOINT64}
                  bts64: TPSValueData(TPSUnValueOp(p).FVal1).Data^.ts64 := not TPSValueData(TPSUnValueOp(p).FVal1).Data^.ts64;
                  {$ENDIF}
                else
                  begin
                    MakeError('', ecTypeMismatch, '');
                    Result := False;
                    exit;
                  end;
                end;
                preplace := TPSUnValueOp(p).Val1;
                TPSUnValueOp(p).Val1 := nil;
                p.Free;
                p := preplace;
              end;
            otMinus:
              begin
                case TPSValueData(TPSUnValueOp(p).FVal1).Data^.FType.BaseType of
                  btU8: TPSValueData(TPSUnValueOp(p).FVal1).Data^.tu8 := -TPSValueData(TPSUnValueOp(p).FVal1).Data^.tu8;
                  btU16: TPSValueData(TPSUnValueOp(p).FVal1).Data^.tu16 := -TPSValueData(TPSUnValueOp(p).FVal1).Data^.tu16;
                  btU32: TPSValueData(TPSUnValueOp(p).FVal1).Data^.tu32 := -TPSValueData(TPSUnValueOp(p).FVal1).Data^.tu32;
                  bts8: TPSValueData(TPSUnValueOp(p).FVal1).Data^.ts8 := -TPSValueData(TPSUnValueOp(p).FVal1).Data^.ts8;
                  bts16: TPSValueData(TPSUnValueOp(p).FVal1).Data^.ts16 := -TPSValueData(TPSUnValueOp(p).FVal1).Data^.ts16;
                  bts32: TPSValueData(TPSUnValueOp(p).FVal1).Data^.ts32 := -TPSValueData(TPSUnValueOp(p).FVal1).Data^.ts32;
                  {$IFNDEF PS_NOINT64}
                  bts64: TPSValueData(TPSUnValueOp(p).FVal1).Data^.ts64 := -TPSValueData(TPSUnValueOp(p).FVal1).Data^.ts64;
                  {$ENDIF}
                  btSingle: TPSValueData(TPSUnValueOp(p).FVal1).Data^.tsingle := -TPSValueData(TPSUnValueOp(p).FVal1).Data^.tsingle;
                  btDouble: TPSValueData(TPSUnValueOp(p).FVal1).Data^.tdouble := -TPSValueData(TPSUnValueOp(p).FVal1).Data^.tdouble;
                  btExtended: TPSValueData(TPSUnValueOp(p).FVal1).Data^.textended := -TPSValueData(TPSUnValueOp(p).FVal1).Data^.textended;
                  btCurrency: TPSValueData(TPSUnValueOp(p).FVal1).Data^.tcurrency := -TPSValueData(TPSUnValueOp(p).FVal1).Data^.tcurrency;
                else
                  begin
                    MakeError('', ecTypeMismatch, '');
                    Result := False;
                    exit;
                  end;
                end;
                preplace := TPSUnValueOp(p).Val1;
                TPSUnValueOp(p).Val1 := nil;
                p.Free;
                p := preplace;
              end;
            otCast:
              begin
                preplace := TPSValueData.Create;
                TPSValueData(preplace).Data := NewVariant(TPSUnValueOp(p).FType);
                case TPSUnValueOp(p).FType.BaseType of
                  btU8:
                    begin
                      case TPSValueData(TPSUnValueOp(p).FVal1).Data.Ftype.basetype of
                        btchar: TPSValueData(preplace).Data.tu8 := ord(TPSValueData(TPSUnValueOp(p).FVal1).Data^.tchar);
                        {$IFNDEF PS_NOWIDESTRING}
                        btwidechar: TPSValueData(preplace).Data.tu8 := ord(TPSValueData(TPSUnValueOp(p).FVal1).Data^.twidechar);
                        {$ENDIF}
                        btU8: TPSValueData(preplace).Data.tu8 := TPSValueData(TPSUnValueOp(p).FVal1).Data^.tu8;
                        btS8: TPSValueData(preplace).Data.tu8 := TPSValueData(TPSUnValueOp(p).FVal1).Data^.tS8;
                        btU16: TPSValueData(preplace).Data.tu8 := TPSValueData(TPSUnValueOp(p).FVal1).Data^.tu16;
                        btS16: TPSValueData(preplace).Data.tu8 := TPSValueData(TPSUnValueOp(p).FVal1).Data^.tS16;
                        btU32: TPSValueData(preplace).Data.tu8 := TPSValueData(TPSUnValueOp(p).FVal1).Data^.tU32;
                        btS32: TPSValueData(preplace).Data.tu8 := TPSValueData(TPSUnValueOp(p).FVal1).Data^.tS32;
                        {$IFNDEF PS_NOINT64}
                        btS64: TPSValueData(preplace).Data.tu8 := TPSValueData(TPSUnValueOp(p).FVal1).Data^.ts64;
                        {$ENDIF}
                      else
                        begin
                          MakeError('', ecTypeMismatch, '');
                          preplace.Free;
                          Result := False;
                          exit;
                        end;
                      end;
                    end;
                  btS8:
                    begin
                      case TPSValueData(TPSUnValueOp(p).FVal1).Data.Ftype.basetype of
                        btchar: TPSValueData(preplace).Data.ts8 := ord(TPSValueData(TPSUnValueOp(p).FVal1).Data^.tchar);
                        {$IFNDEF PS_NOWIDESTRING}
                        btwidechar: TPSValueData(preplace).Data.ts8 := ord(TPSValueData(TPSUnValueOp(p).FVal1).Data^.twidechar);
                        {$ENDIF}
                        btU8: TPSValueData(preplace).Data.ts8 := TPSValueData(TPSUnValueOp(p).FVal1).Data^.tu8;
                        btS8: TPSValueData(preplace).Data.ts8 := TPSValueData(TPSUnValueOp(p).FVal1).Data^.tS8;
                        btU16: TPSValueData(preplace).Data.ts8 := TPSValueData(TPSUnValueOp(p).FVal1).Data^.tu16;
                        btS16: TPSValueData(preplace).Data.ts8 := TPSValueData(TPSUnValueOp(p).FVal1).Data^.tS16;
                        btU32: TPSValueData(preplace).Data.ts8 := TPSValueData(TPSUnValueOp(p).FVal1).Data^.tU32;
                        btS32: TPSValueData(preplace).Data.ts8 := TPSValueData(TPSUnValueOp(p).FVal1).Data^.tS32;
                        {$IFNDEF PS_NOINT64}
                        btS64: TPSValueData(preplace).Data.ts8 := TPSValueData(TPSUnValueOp(p).FVal1).Data^.ts64;
                        {$ENDIF}
                      else
                        begin
                          MakeError('', ecTypeMismatch, '');
                          preplace.Free;
                          Result := False;
                          exit;
                        end;
                      end;
                    end;
                  btU16:
                    begin
                      case TPSValueData(TPSUnValueOp(p).FVal1).Data.Ftype.basetype of
                        btchar: TPSValueData(preplace).Data.tu16 := ord(TPSValueData(TPSUnValueOp(p).FVal1).Data^.tchar);
                        {$IFNDEF PS_NOWIDESTRING}
                        btwidechar: TPSValueData(preplace).Data.tu16 := ord(TPSValueData(TPSUnValueOp(p).FVal1).Data^.twidechar);
                        {$ENDIF}
                        btU8: TPSValueData(preplace).Data.tu16 := TPSValueData(TPSUnValueOp(p).FVal1).Data^.tu8;
                        btS8: TPSValueData(preplace).Data.tu16 := TPSValueData(TPSUnValueOp(p).FVal1).Data^.tS8;
                        btU16: TPSValueData(preplace).Data.ts16 := TPSValueData(TPSUnValueOp(p).FVal1).Data^.tu16;
                        btS16: TPSValueData(preplace).Data.tu16 := TPSValueData(TPSUnValueOp(p).FVal1).Data^.tS16;
                        btU32: TPSValueData(preplace).Data.tu16 := TPSValueData(TPSUnValueOp(p).FVal1).Data^.tU32;
                        btS32: TPSValueData(preplace).Data.tu16 := TPSValueData(TPSUnValueOp(p).FVal1).Data^.tS32;
                        {$IFNDEF PS_NOINT64}
                        btS64: TPSValueData(preplace).Data.tu16 := TPSValueData(TPSUnValueOp(p).FVal1).Data^.ts64;
                        {$ENDIF}
                      else
                        begin
                          MakeError('', ecTypeMismatch, '');
                          preplace.Free;
                          Result := False;
                          exit;
                        end;
                      end;
                    end;
                  bts16:
                    begin
                      case TPSValueData(TPSUnValueOp(p).FVal1).Data.Ftype.basetype of
                        btchar: TPSValueData(preplace).Data.ts16 := ord(TPSValueData(TPSUnValueOp(p).FVal1).Data^.tchar);
                        {$IFNDEF PS_NOWIDESTRING}
                        btwidechar: TPSValueData(preplace).Data.ts16 := ord(TPSValueData(TPSUnValueOp(p).FVal1).Data^.twidechar);
                        {$ENDIF}
                        btU8: TPSValueData(preplace).Data.ts16 := TPSValueData(TPSUnValueOp(p).FVal1).Data^.tu8;
                        btS8: TPSValueData(preplace).Data.ts16 := TPSValueData(TPSUnValueOp(p).FVal1).Data^.tS8;
                        btU16: TPSValueData(preplace).Data.ts16 := TPSValueData(TPSUnValueOp(p).FVal1).Data^.tu16;
                        btS16: TPSValueData(preplace).Data.ts16 := TPSValueData(TPSUnValueOp(p).FVal1).Data^.tS16;
                        btU32: TPSValueData(preplace).Data.ts16 := TPSValueData(TPSUnValueOp(p).FVal1).Data^.tU32;
                        btS32: TPSValueData(preplace).Data.ts16 := TPSValueData(TPSUnValueOp(p).FVal1).Data^.tS32;
                        {$IFNDEF PS_NOINT64}
                        btS64: TPSValueData(preplace).Data.ts16 := TPSValueData(TPSUnValueOp(p).FVal1).Data^.ts64;
                        {$ENDIF}
                      else
                        begin
                          MakeError('', ecTypeMismatch, '');
                          preplace.Free;
                          Result := False;
                          exit;
                        end;
                      end;
                    end;
                  btU32:
                    begin
                      case TPSValueData(TPSUnValueOp(p).FVal1).Data.Ftype.basetype of
                        btchar: TPSValueData(preplace).Data.tu32 := ord(TPSValueData(TPSUnValueOp(p).FVal1).Data^.tchar);
                        {$IFNDEF PS_NOWIDESTRING}
                        btwidechar: TPSValueData(preplace).Data.tu32 := ord(TPSValueData(TPSUnValueOp(p).FVal1).Data^.twidechar);
                        {$ENDIF}
                        btU8: TPSValueData(preplace).Data.tu32 := TPSValueData(TPSUnValueOp(p).FVal1).Data^.tu8;
                        btS8: TPSValueData(preplace).Data.tu32 := TPSValueData(TPSUnValueOp(p).FVal1).Data^.tS8;
                        btU16: TPSValueData(preplace).Data.tu32 := TPSValueData(TPSUnValueOp(p).FVal1).Data^.tu16;
                        btS16: TPSValueData(preplace).Data.tu32 := TPSValueData(TPSUnValueOp(p).FVal1).Data^.tS16;
                        btU32: TPSValueData(preplace).Data.tu32 := TPSValueData(TPSUnValueOp(p).FVal1).Data^.tU32;
                        btS32: TPSValueData(preplace).Data.tu32 := TPSValueData(TPSUnValueOp(p).FVal1).Data^.tS32;
                        {$IFNDEF PS_NOINT64}
                        btS64: TPSValueData(preplace).Data.tu32 := TPSValueData(TPSUnValueOp(p).FVal1).Data^.ts64;
                        {$ENDIF}
                      else
                        begin
                          MakeError('', ecTypeMismatch, '');
                          preplace.Free;
                          Result := False;
                          exit;
                        end;
                      end;
                    end;
                  btS32:
                    begin
                      case TPSValueData(TPSUnValueOp(p).FVal1).Data.Ftype.basetype of
                        btchar: TPSValueData(preplace).Data.ts32 := ord(TPSValueData(TPSUnValueOp(p).FVal1).Data^.tchar);
                        {$IFNDEF PS_NOWIDESTRING}
                        btwidechar: TPSValueData(preplace).Data.ts32 := ord(TPSValueData(TPSUnValueOp(p).FVal1).Data^.twidechar);
                        {$ENDIF}
                        btU8: TPSValueData(preplace).Data.ts32 := TPSValueData(TPSUnValueOp(p).FVal1).Data^.tu8;
                        btS8: TPSValueData(preplace).Data.ts32 := TPSValueData(TPSUnValueOp(p).FVal1).Data^.tS8;
                        btU16: TPSValueData(preplace).Data.ts32 := TPSValueData(TPSUnValueOp(p).FVal1).Data^.tu16;
                        btS16: TPSValueData(preplace).Data.ts32 := TPSValueData(TPSUnValueOp(p).FVal1).Data^.tS16;
                        btU32: TPSValueData(preplace).Data.ts32 := TPSValueData(TPSUnValueOp(p).FVal1).Data^.tU32;
                        btS32: TPSValueData(preplace).Data.ts32 := TPSValueData(TPSUnValueOp(p).FVal1).Data^.tS32;
                        {$IFNDEF PS_NOINT64}
                        btS64: TPSValueData(preplace).Data.tu32 := TPSValueData(TPSUnValueOp(p).FVal1).Data^.ts64;
                        {$ENDIF}
                      else
                        begin
                          MakeError('', ecTypeMismatch, '');
                          preplace.Free;
                          Result := False;
                          exit;
                        end;
                      end;
                    end;
                  {$IFNDEF PS_NOINT64}
                  btS64:
                    begin
                      case TPSValueData(TPSUnValueOp(p).FVal1).Data.Ftype.basetype of
                        btchar: TPSValueData(preplace).Data.ts64 := ord(TPSValueData(TPSUnValueOp(p).FVal1).Data^.tchar);
                        {$IFNDEF PS_NOWIDESTRING}
                        btwidechar: TPSValueData(preplace).Data.ts64 := ord(TPSValueData(TPSUnValueOp(p).FVal1).Data^.twidechar);
                        {$ENDIF}
                        btU8: TPSValueData(preplace).Data.ts64 := TPSValueData(TPSUnValueOp(p).FVal1).Data^.tu8;
                        btS8: TPSValueData(preplace).Data.ts64 := TPSValueData(TPSUnValueOp(p).FVal1).Data^.tS8;
                        btU16: TPSValueData(preplace).Data.ts64 := TPSValueData(TPSUnValueOp(p).FVal1).Data^.tu16;
                        btS16: TPSValueData(preplace).Data.ts64 := TPSValueData(TPSUnValueOp(p).FVal1).Data^.tS16;
                        btU32: TPSValueData(preplace).Data.ts64 := TPSValueData(TPSUnValueOp(p).FVal1).Data^.tU32;
                        btS32: TPSValueData(preplace).Data.ts64 := TPSValueData(TPSUnValueOp(p).FVal1).Data^.tS32;
                        btS64: TPSValueData(preplace).Data.ts64 := TPSValueData(TPSUnValueOp(p).FVal1).Data^.ts64;
                      else
                        begin
                          MakeError('', ecTypeMismatch, '');
                          preplace.Free;
                          Result := False;
                          exit;
                        end;
                      end;
                    end;
                  {$ENDIF}
                  btChar:
                    begin
                      case TPSValueData(TPSUnValueOp(p).FVal1).Data.Ftype.basetype of
                        btchar: TPSValueData(preplace).Data.tchar := TPSValueData(TPSUnValueOp(p).FVal1).Data^.tchar;
                        btU8: TPSValueData(preplace).Data.tchar := tbtchar(TPSValueData(TPSUnValueOp(p).FVal1).Data^.tu8);
                        btS8: TPSValueData(preplace).Data.tchar := tbtchar(TPSValueData(TPSUnValueOp(p).FVal1).Data^.tS8);
                        btU16: TPSValueData(preplace).Data.tchar := tbtchar(TPSValueData(TPSUnValueOp(p).FVal1).Data^.tu16);
                        btS16: TPSValueData(preplace).Data.tchar := tbtchar(TPSValueData(TPSUnValueOp(p).FVal1).Data^.tS16);
                        btU32: TPSValueData(preplace).Data.tchar := tbtchar(TPSValueData(TPSUnValueOp(p).FVal1).Data^.tU32);
                        btS32: TPSValueData(preplace).Data.tchar := tbtchar(TPSValueData(TPSUnValueOp(p).FVal1).Data^.tS32);
                        {$IFNDEF PS_NOINT64}
                        btS64: TPSValueData(preplace).Data.tchar := tbtchar(TPSValueData(TPSUnValueOp(p).FVal1).Data^.ts64);
                        {$ENDIF}
                      else
                        begin
                          MakeError('', ecTypeMismatch, '');
                          Result := False;
                          preplace.Free;
                          exit;
                        end;
                      end;
                    end;
                else
                  begin
                    MakeError('', ecTypeMismatch, '');
                    Result := False;
                    preplace.Free;
                    exit;
                  end;
                end;
                p.Free;
                p := preplace;
              end;
            else
              begin
                MakeError('', ecTypeMismatch, '');
                Result := False;
                exit;
              end;
          end; // case
        end; // if
      end;
      Result := True;
    end;

  var
    Temp, Val: TPSValue;
    vt: TPSVariableType;

begin
    Val := ReadExpression;
    if Val = nil then
    begin
      Result := nil;
      exit;
    end;
    vt := ivtGlobal;
    repeat
      Temp := Val;
      if Val <> nil then CheckFurther(Val, False);
      if Val <> nil then CheckClass(Val, vt, InvalidVal, False);
      if Val <> nil then  CheckExtClass(Val, vt, InvalidVal, False);
{$IFNDEF PS_NOIDISPATCH}if Val <> nil then CheckIntf(Val, vt, InvalidVal, False);{$ENDIF}
      if Val <> nil then CheckProcCall(Val);
      if Val<> nil then CheckClassArrayProperty(Val, vt, InvalidVal);
    until (Val = nil) or (Temp = Val);

    if not TryEvalConst(Val) then
    begin
      Val.Free;
      Result := nil;
      exit;
    end;
    Result := Val;
  end;

  function ReadParameters(IsProperty: Boolean; Dest: TPSParameters): Boolean;
  var
    sr,cr: TPSPasToken;
  begin
    if IsProperty then
    begin
      sr := CSTI_OpenBlock;
      cr := CSTI_CloseBlock;
    end else begin
      sr := CSTI_OpenRound;
      cr := CSTI_CloseRound;
    end;
    if FParser.CurrTokenId = sr then
    begin
      FParser.Next;
      if FParser.CurrTokenId = cr then
      begin
        FParser.Next;
        Result := True;
        exit;
      end;
    end else
    begin
      result := True;
      exit;
    end;
    repeat
      with Dest.Add do
      begin
        Val := calc(CSTI_CloseRound);
        if Val = nil then
        begin
          result := false;
          exit;
        end;
        Val.SetParserPos(FParser);
      end;
      if FParser.CurrTokenId = cr then
      begin
        FParser.Next;
        Break;
      end;
      if FParser.CurrTokenId <> CSTI_Comma then
      begin
        MakeError('', ecCommaExpected, '');
        Result := false;
        exit;
      end; {if}
      FParser.Next;
    until False;
    Result := true;
  end;

  function ReadProcParameters(ProcNo: Cardinal; FSelf: TPSValue): TPSValue;
  var
    Decl: TPSParametersDecl;
  begin
    if TPSProcedure(FProcs[ProcNo]).ClassType = TPSInternalProcedure then
      Decl := TPSInternalProcedure(FProcs[ProcNo]).Decl
    else
      Decl := TPSExternalProcedure(FProcs[ProcNo]).RegProc.Decl;
    UseProc(Decl);
    Result := TPSValueProcNo.Create;
    TPSValueProcNo(Result).ProcNo := ProcNo;
    TPSValueProcNo(Result).ResultType := Decl.Result;
    with TPSValueProcNo(Result) do
    begin
      SetParserPos(FParser);
      Parameters := TPSParameters.Create;
      if FSelf <> nil then
      begin
        Parameters.Add;
      end;
    end;

    if not ReadParameters(False, TPSValueProc(Result).Parameters) then
    begin
      FSelf.Free;
      Result.Free;
      Result := nil;
      exit;
    end;

    if not ValidateParameters(BlockInfo, TPSValueProc(Result).Parameters, Decl) then
    begin
      FSelf.Free;
      Result.Free;
      Result := nil;
      exit;
    end;
    if FSelf <> nil then
    begin
      with TPSValueProcNo(Result).Parameters[0] do
      begin
        Val := FSelf;
        ExpectedType := GetTypeNo(BlockInfo, FSelf);
      end;
    end;
  end;
  {$IFNDEF PS_NOIDISPATCH}

  function ReadIDispatchParameters(const ProcName: tbtString; aVariantType: TPSVariantType; FSelf: TPSValue): TPSValue;
  var
    Par: TPSParameters;
    PropSet: Boolean;
    i: Longint;
    Temp: TPSValue;
  begin
    Par := TPSParameters.Create;
    try
      if not ReadParameters(FParser.CurrTokenID = CSTI_OpenBlock, Par) then
      begin
        FSelf.Free;
        Result := nil;
        exit;
      end;

      if FParser.CurrTokenID = CSTI_Assignment then
      begin
        FParser.Next;
        PropSet := True;
        Temp := calc(CSTI_SemiColon);
        if temp = nil then
        begin
          FSelf.Free;
          Result := nil;
          exit;
        end;
        with par.Add do
        begin
          FValue := Temp;
        end;
      end else
      begin
        PropSet := False;
      end;

      Result := TPSValueProcNo.Create;
      TPSValueProcNo(Result).ResultType := aVariantType;
      with TPSValueProcNo(Result) do
      begin
        SetParserPos(FParser);
        Parameters := TPSParameters.Create;
        if FSelf <> nil then
        begin
          with Parameters.Add do
          begin
            Val := FSelf;
            ExpectedType := aVariantType.GetDynIvokeSelfType(Self);
          end;
          with Parameters.Add do
          begin
            Val := TPSValueData.Create;
            TPSValueData(Val).Data := NewVariant(FDefaultBoolType);
            TPSValueData(Val).Data.tu8 := Ord(PropSet);
            ExpectedType := FDefaultBoolType;
          end;

          with Parameters.Add do
          begin
            Val := TPSValueData.Create;
            TPSValueData(Val).Data := NewVariant(FindBaseType(btString));
            tbtString(TPSValueData(Val).data.tString) := Procname;
            ExpectedType := FindBaseType(btString);
          end;

          with Parameters.Add do
          begin
            val := TPSValueArray.Create;
            ExpectedType := aVariantType.GetDynInvokeParamType(Self);
            temp := Val;
          end;
          for i := 0 to Par.Count -1 do
          begin
            TPSValueArray(Temp).Add(par.Item[i].Val);
            par.Item[i].val := nil;
          end;
        end;
      end;
      TPSValueProcNo(Result).ProcNo := aVariantType.GetDynInvokeProcNo(Self, ProcName, TPSValueProcNo(Result).Parameters);
    finally
      Par.Free;
    end;

  end;

  {$ENDIF}

  function ReadVarParameters(ProcNoVar: TPSValue): TPSValue;
  var
    Decl: TPSParametersDecl;
  begin
    Decl := TPSProceduralType(GetTypeNo(BlockInfo, ProcnoVar)).ProcDef;
    UseProc(Decl);

    Result := TPSValueProcVal.Create;

    with TPSValueProcVal(Result) do
    begin
      ResultType := Decl.Result;
      ProcNo := ProcNoVar;
      Parameters := TPSParameters.Create;
    end;

    if not ReadParameters(False, TPSValueProc(Result).Parameters) then
    begin
      Result.Free;
      Result := nil;
      exit;
    end;

    if not ValidateParameters(BlockInfo, TPSValueProc(Result).Parameters, Decl) then
    begin
      Result.Free;
      Result := nil;
      exit;
    end;
  end;


  function WriteCalculation(InData, OutReg: TPSValue): Boolean;

    function CheckOutreg(Where, Outreg: TPSValue; aRoot: Boolean): Boolean;
    var
      i: Longint;
    begin
      Result := False;
      if Outreg is TPSValueReplace
        then Outreg:=TPSValueReplace(Outreg).OldValue;
      if Where is TPSValueVar then begin
        if TPSValueVar(Where).GetRecCount > 0 then result := true;
        if SAmeReg(Where, OutReg) and not aRoot then
          result := true;
      end else
      if Where.ClassType = TPSUnValueOp then
      begin
        if CheckOutReg(TPSUnValueOp(Where).Val1, OutReg, aRoot) then
          Result := True;
      end else if Where.ClassType = TPSBinValueOp then
      begin
        if CheckOutreg(TPSBinValueOp(Where).Val1, OutReg, aRoot) or CheckOutreg(TPSBinValueOp(Where).Val2, OutReg, False) then
          Result := True;
      end else if Where is TPSValueVar then
      begin
        if SameReg(Where, OutReg) then
          Result := True;
      end else if Where is TPSValueProc then
      begin
        for i := 0 to TPSValueProc(Where).Parameters.Count -1 do
        begin
          if Checkoutreg(TPSValueProc(Where).Parameters[i].Val, Outreg, false) then
          begin
            Result := True;
            break;
          end;
        end;
      end;
    end;
  begin
    if not CheckCompatType(Outreg, InData) then
    begin
      MakeError('', ecTypeMismatch, '');
      Result := False;
      exit;
    end;
    if SameReg(OutReg, InData) then
    begin
      Result := True;
      exit;
    end;
    if InData is TPSValueProc then
    begin
      Result := _ProcessFunction(TPSValueProc(indata), OutReg)
    end else begin
      if not PreWriteOutRec(OutReg, nil) then
      begin
        Result := False;
        exit;
      end;
      if (not CheckOutReg(InData, OutReg, true)) and (InData is TPSBinValueOp) or (InData is TPSUnValueOp) then
      begin
        if InData is TPSBinValueOp then
        begin
          if not DoBinCalc(TPSBinValueOp(InData), OutReg) then
          begin
            AfterWriteOutRec(OutReg);
            Result := False;
            exit;
          end;
        end else
        begin
          if not DoUnCalc(TPSUnValueOp(InData), OutReg) then
          begin
            AfterWriteOutRec(OutReg);
            Result := False;
            exit;
          end;
        end;
      end else if (InData is TPSBinValueOp) and (not CheckOutReg(TPSBinValueOp(InData).Val2, OutReg, false)) then
      begin
        if not DoBinCalc(TPSBinValueOp(InData), OutReg) then
        begin
          AfterWriteOutRec(OutReg);
          Result := False;
          exit;
        end;
      end else begin
        if not PreWriteOutRec(InData, GetTypeNo(BlockInfo, OutReg)) then
        begin
          Result := False;
          exit;
        end;
        BlockWriteByte(BlockInfo, CM_A);
        if not (WriteOutRec(OutReg, False) and WriteOutRec(InData, True)) then
        begin
          Result := False;
          exit;
        end;
        AfterWriteOutRec(InData);
      end;
      AfterWriteOutRec(OutReg);
      Result := True;
    end;
  end; {WriteCalculation}


  function _ProcessFunction(ProcCall: TPSValueProc; ResultRegister: TPSValue): Boolean;
  var
    res: TPSType;
    tmp: TPSParameter;
    lTv: TPSValue;
    resreg: TPSValue;
    l: Longint;

    function Cleanup: Boolean;
    var
      i: Longint;
    begin
      for i := 0 to ProcCall.Parameters.Count -1 do
      begin
        if ProcCall.Parameters[i].TempVar <> nil then
          ProcCall.Parameters[i].TempVar.Free;
        ProcCall.Parameters[i].TempVar := nil;
      end;
      if ProcCall is TPSValueProcVal then
        AfterWriteOutRec(TPSValueProcVal(ProcCall).fProcNo);
      if ResReg <> nil then
        AfterWriteOutRec(resreg);
      if ResReg <> nil then
      begin
        if ResReg <> ResultRegister then
        begin
          if ResultRegister <> nil then
          begin
            if not WriteCalculation(ResReg, ResultRegister) then
            begin
              Result := False;
              resreg.Free;
              exit;
            end;
          end;
          resreg.Free;
        end;
      end;
      Result := True;
    end;

  begin
    Res := ProcCall.ResultType;
    if ProcCall.ResultType = FAnyString then
    begin
      for l := ProcCall.Parameters.Count - 1 downto 0 do
      begin
        Tmp := ProcCall.Parameters[l];
        if (Tmp.ParamMode <> pmOut) and (Tmp.ExpectedType = FAnyString) then
        begin
          Res := GetTypeNo(BlockInfo, tmp.Val);
          Break;
        end;
      end;
    end;
    Result := False;
    if (res = nil) and (ResultRegister <> nil) then
    begin
      MakeError('', ecNoResult, '');
      exit;
    end
    else if (res <> nil)  then
    begin
      if (ResultRegister = nil) or (Res <> GetTypeNo(BlockInfo, ResultRegister)) then
      begin
        resreg := AllocStackReg(res);

      end else resreg := ResultRegister;
    end
    else
      resreg := nil;
    if ResReg <> nil then
    begin
      if not PreWriteOutRec(resreg, nil) then
      begin
        Cleanup;
        exit;
      end;
    end;
    if Proccall is TPSValueProcVal then
    begin
      if not PreWriteOutRec(TPSValueProcVal(ProcCall).fProcNo, nil) then
      begin
        Cleanup;
        exit;
      end;
    end;
    for l := ProcCall.Parameters.Count - 1 downto 0 do
    begin
      Tmp := ProcCall.Parameters[l];
      if (Tmp.ParamMode <> pmIn)  then
      begin
        if IsVarInCompatible(GetTypeNo(BlockInfo, tmp.Val), tmp.ExpectedType) then
        begin
          with MakeError('', ecTypeMismatch, '') do
          begin
            pos := tmp.Val.Pos;
            row := tmp.Val.row;
            col := tmp.Val.col;
          end;
          Cleanup;
          exit;
        end;
        if Copy(tmp.ExpectedType.Name, 1, 10) = '!OPENARRAY' then begin
          tmp.TempVar := AllocPointer(tmp.ExpectedType);
          lTv := AllocStackReg(tmp.ExpectedType);
          if not PreWriteOutRec(Tmp.FValue, nil) then
          begin
            cleanup;
            exit;
          end;
          BlockWriteByte(BlockInfo, CM_A);
          WriteOutRec(lTv, False);
          WriteOutRec(Tmp.FValue, False);
          AfterWriteOutRec(Tmp.FValue);

          BlockWriteByte(BlockInfo, cm_sp);
          WriteOutRec(tmp.TempVar, False);
          WriteOutRec(lTv, False);

          lTv.Free;
//          BlockWriteByte(BlockInfo, CM_PO); // pop the temp var

        end else begin
        tmp.TempVar := AllocPointer(GetTypeNo(BlockInfo, Tmp.FValue));
        if not PreWriteOutRec(Tmp.FValue, nil) then
        begin
          cleanup;
          exit;
        end;
        BlockWriteByte(BlockInfo, cm_sp);
        WriteOutRec(tmp.TempVar, False);
        WriteOutRec(Tmp.FValue, False);
        AfterWriteOutRec(Tmp.FValue);
        end;
      end
      else
      begin
        if Tmp.ExpectedType = nil then
          Tmp.ExpectedType := GetTypeNo(BlockInfo, tmp.Val)
        else if Tmp.ExpectedType = FAnyString then begin
          Tmp.ExpectedType := GetTypeNo(BlockInfo, tmp.Val);
          if not IsStringOrCharType(Tmp.ExpectedType.BaseType) then
          begin
            MakeError('', ecTypeMismatch, '');
            Cleanup;
            exit;
          end;
        end;
        if Tmp.ExpectedType.BaseType = btPChar then
        begin
          Tmp.TempVar := AllocStackReg(at2ut(FindBaseType(btstring)))
        end else
        begin
        Tmp.TempVar := AllocStackReg(Tmp.ExpectedType);
        end;
        if not WriteCalculation(Tmp.Val, Tmp.TempVar) then
        begin
          Cleanup;
          exit;
        end;
      end;
    end; {for}
    if res <> nil then
    begin
      BlockWriteByte(BlockInfo, CM_PV);

      if not WriteOutRec(resreg, False) then
      begin
        Cleanup;
        MakeError('', ecInternalError, '00015');
        exit;
      end;
    end;
    if ProcCall is TPSValueProcVal then
    begin
      BlockWriteByte(BlockInfo, Cm_cv);
      WriteOutRec(TPSValueProcVal(ProcCall).ProcNo, True);
    end else begin
      BlockWriteByte(BlockInfo, CM_C);
      BlockWriteLong(BlockInfo, TPSValueProcNo(ProcCall).ProcNo);
    end;
    if res <> nil then
      BlockWriteByte(BlockInfo, CM_PO);
    if not Cleanup then
    begin
      Result := False;
      exit;
    end;
    Result := True;
  end; {ProcessVarFunction}

	function HasInvalidJumps(StartPos, EndPos: Cardinal): Boolean;
  var
    I, J: Longint;
    Ok: LongBool;
    FLabelsInBlock: TIfStringList;
    s: tbtString;
	begin
		FLabelsInBlock := TIfStringList.Create;
		for i := 0 to BlockInfo.Proc.FLabels.Count -1 do
		begin
			s := BlockInfo.Proc.FLabels[I];
			if (Cardinal((@s[1])^) >= StartPos) and (Cardinal((@s[1])^) <= EndPos) then
			begin
				Delete(s, 1, 8);
				FLabelsInBlock.Add(s);
			end;
		end;
		for i := 0 to BlockInfo.Proc.FGotos.Count -1 do
		begin
			s := BlockInfo.Proc.FGotos[I];
			if (Cardinal((@s[1])^) >= StartPos) and (Cardinal((@s[1])^) <= EndPos) then
			begin
				Delete(s, 1, 4);
				s := BlockInfo.Proc.FLabels[Cardinal((@s[1])^)];
				Delete(s,1,8);
				OK := False;
        for J := 0 to FLabelsInBlock.Count -1 do
        begin
          if FLabelsInBlock[J] = s then
          begin
            Ok := True;
            Break;
          end;
        end;
        if not Ok then
        begin
          MakeError('', ecInvalidJump, '');
          Result := True;
          FLabelsInBlock.Free;
          exit;
        end;
      end else begin
				Delete(s, 1, 4);
				s := BlockInfo.Proc.FLabels[Cardinal((@s[1])^)];
				Delete(s,1,8);
				OK := True;
        for J := 0 to FLabelsInBlock.Count -1 do
        begin
          if FLabelsInBlock[J] = s then
          begin
            Ok := False;
            Break;
          end;
        end;
        if not Ok then
        begin
          MakeError('', ecInvalidJump, '');
          Result := True;
          FLabelsInBlock.Free;
          exit;
        end;
      end;
    end;
    FLabelsInBlock.Free;
    Result := False;
  end;

  function ProcessFor: Boolean;
    { Process a for x := y to z do }
  var
    VariableVar: TPSValue;
      TempBool,
      InitVal,
      finVal: TPSValue;
    Block: TPSBlockInfo;
    Backwards: Boolean;
    FPos, NPos, EPos, RPos: Longint;
    OldCO, OldBO: TPSList;
    I: Longint;
		iOldWithCount: Integer;
		iOldTryCount: Integer;
		iOldExFnlCount: Integer;
    lType: TPSType;
  begin
    Debug_WriteLine(BlockInfo);
    Result := False;
    FParser.Next;
    if FParser.CurrTokenId <> CSTI_Identifier then
    begin
      MakeError('', ecIdentifierExpected, '');
      exit;
    end;
    VariableVar := GetIdentifier(1);
    if VariableVar = nil then
      exit;
    lType := GetTypeNo(BlockInfo, VariableVar);
    if lType = nil then begin
      MakeError('', ecTypeMismatch, '');
      VariableVar.Free;
      exit;
    end;
    case lType.BaseType of
      btU8, btS8, btU16, btS16, btU32, btS32, {$IFNDEF PS_NOINT64} btS64, {$ENDIF} btVariant, btEnum: ;
    else
      begin
        MakeError('', ecTypeMismatch, '');
        VariableVar.Free;
        exit;
      end;
    end;
    if FParser.CurrTokenId <> CSTI_Assignment then
    begin
      MakeError('', ecAssignmentExpected, '');
      VariableVar.Free;
      exit;
    end;
    FParser.Next;
    InitVal := calc(CSTII_DownTo);
    if InitVal = nil then
    begin
      VariableVar.Free;
      exit;
    end;
    if FParser.CurrTokenId = CSTII_To then
      Backwards := False
    else if FParser.CurrTokenId = CSTII_DownTo then
      Backwards := True
    else
    begin
      MakeError('', ecToExpected, '');
      VariableVar.Free;
      InitVal.Free;
      exit;
    end;
    FParser.Next;
    finVal := calc(CSTII_do);
    if finVal = nil then
    begin
      VariableVar.Free;
      InitVal.Free;
      exit;
    end;
    lType := GetTypeNo(BlockInfo, finVal);
    if lType = nil then begin
      MakeError('', ecTypeMismatch, '');
      VariableVar.Free;
      InitVal.Free;
      exit;
    end;
    case lType.BaseType of
      btVariant, btEnum, btU8, btS8, btU16, btS16, btU32, {$IFNDEF PS_NOINT64} btS64, {$ENDIF} btS32: ;
    else
      begin
        MakeError('', ecTypeMismatch, '');
        VariableVar.Free;
        InitVal.Free;
        exit;
      end;
    end;
    if FParser.CurrTokenId <> CSTII_do then
    begin
      MakeError('', ecDoExpected, '');
      finVal.Free;
      InitVal.Free;
      VariableVar.Free;
      exit;
    end;
    FParser.Next;
    if not WriteCalculation(InitVal, VariableVar) then
    begin
      VariableVar.Free;
      InitVal.Free;
      finVal.Free;
      exit;
    end;
    InitVal.Free;
    TempBool := AllocStackReg(at2ut(FDefaultBoolType));
    NPos := Length(BlockInfo.Proc.Data);
    if not (PreWriteOutRec(VariableVar, nil) and PreWriteOutRec(finVal, nil)) then
    begin
      TempBool.Free;
      VariableVar.Free;
      finVal.Free;
      exit;
    end;
    BlockWriteByte(BlockInfo, CM_CO);
    if Backwards then
    begin
      BlockWriteByte(BlockInfo, 0); { >= }
    end
    else
    begin
      BlockWriteByte(BlockInfo, 1); { <= }
    end;
    if not (WriteOutRec(TempBool, False) and WriteOutRec(VariableVar, True) and WriteOutRec(finVal, True)) then
    begin
      TempBool.Free;
      VariableVar.Free;
      finVal.Free;
      exit;
    end;
    AfterWriteOutRec(finVal);
    AfterWriteOutRec(VariableVar);
    finVal.Free;
    BlockWriteByte(BlockInfo, Cm_CNG);
    EPos := Length(BlockInfo.Proc.Data);
    BlockWriteLong(BlockInfo, $12345678);
    WriteOutRec(TempBool, False);
    RPos := Length(BlockInfo.Proc.Data);
    OldCO := FContinueOffsets;
    FContinueOffsets := TPSList.Create;
    OldBO := FBreakOffsets;
    FBreakOffsets := TPSList.Create;
    Block := TPSBlockInfo.Create(BlockInfo);
    Block.SubType := tOneLiner;

		iOldWithCount := FWithCount;
		FWithCount := 0;
		iOldTryCount := FTryCount;
		FTryCount := 0;
		iOldExFnlCount := FExceptFinallyCount;
    FExceptFinallyCount := 0;

    if not ProcessSub(Block) then
    begin
      Block.Free;
      TempBool.Free;
      VariableVar.Free;
      FBreakOffsets.Free;
      FContinueOffsets.Free;
      FContinueOffsets := OldCO;
      FBreakOffsets := OldBo;

			FWithCount := iOldWithCount;
			FTryCount := iOldTryCount;
      FExceptFinallyCount := iOldExFnlCount;

			exit;
		end;
		Block.Free;
		FPos := Length(BlockInfo.Proc.Data);
		if not PreWriteOutRec(VariableVar, nil) then
		begin
			TempBool.Free;
			VariableVar.Free;
			FBreakOffsets.Free;
			FContinueOffsets.Free;
			FContinueOffsets := OldCO;
			FBreakOffsets := OldBo;

			FWithCount := iOldWithCount;
			FTryCount := iOldTryCount;
      FExceptFinallyCount := iOldExFnlCount;

      exit;
    end;
    if Backwards then
      BlockWriteByte(BlockInfo, cm_dec)
    else
      BlockWriteByte(BlockInfo, cm_inc);
    if not WriteOutRec(VariableVar, False) then
    begin
      TempBool.Free;
      VariableVar.Free;
      FBreakOffsets.Free;
      FContinueOffsets.Free;
      FContinueOffsets := OldCO;
      FBreakOffsets := OldBo;

			FWithCount := iOldWithCount;
			FTryCount := iOldTryCount;
      FExceptFinallyCount := iOldExFnlCount;

      exit;
    end;
    AfterWriteOutRec(VariableVar);
    BlockWriteByte(BlockInfo, Cm_G);
    BlockWriteLong(BlockInfo, Longint(NPos - Length(BlockInfo.Proc.Data) - 4));
    {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
    unaligned(Longint((@BlockInfo.Proc.Data[EPos + 1])^)) := Length(BlockInfo.Proc.Data) - RPos;
    {$else}
    Longint((@BlockInfo.Proc.Data[EPos + 1])^) := Length(BlockInfo.Proc.Data) - RPos;
    {$endif}
    for i := 0 to FBreakOffsets.Count -1 do
    begin
      EPos := IPointer(FBreakOffsets[I]);
      {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
      unaligned(Longint((@BlockInfo.Proc.Data[EPos - 3])^)) := Length(BlockInfo.Proc.Data) - Longint(EPos);
      {$else}
      Longint((@BlockInfo.Proc.Data[EPos - 3])^) := Length(BlockInfo.Proc.Data) - Longint(EPos);
      {$endif}
    end;
    for i := 0 to FContinueOffsets.Count -1 do
    begin
      EPos := IPointer(FContinueOffsets[I]);
      {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
      unaligned(Longint((@BlockInfo.Proc.Data[EPos - 3])^)) := Longint(FPos) - Longint(EPos);
      {$else}
      Longint((@BlockInfo.Proc.Data[EPos - 3])^) := Longint(FPos) - Longint(EPos);
      {$endif}
    end;
    FBreakOffsets.Free;
    FContinueOffsets.Free;
    FContinueOffsets := OldCO;
    FBreakOffsets := OldBo;

		FWithCount := iOldWithCount;
    FTryCount := iOldTryCount;
    FExceptFinallyCount := iOldExFnlCount;

		TempBool.Free;
		VariableVar.Free;
		if HasInvalidJumps(RPos, Length(BlockInfo.Proc.Data)) then
    begin
      Result := False;
      exit;
    end;
    Result := True;
  end; {ProcessFor}

  function ProcessWhile: Boolean;
  var
    vin, vout: TPSValue;
    SPos, EPos: Cardinal;
    OldCo, OldBO: TPSList;
    I: Longint;
    Block: TPSBlockInfo;

		iOldWithCount: Integer;
    iOldTryCount: Integer;
    iOldExFnlCount: Integer;

  begin
    Result := False;
    Debug_WriteLine(BlockInfo);
    FParser.Next;
    vout := calc(CSTII_do);
    if vout = nil then
      exit;
    if FParser.CurrTokenId <> CSTII_do then
    begin
      vout.Free;
      MakeError('', ecDoExpected, '');
      exit;
    end;
    vin := AllocStackReg(at2ut(FDefaultBoolType));
    SPos := Length(BlockInfo.Proc.Data); // start position
    OldCo := FContinueOffsets;
    FContinueOffsets := TPSList.Create;
    OldBO := FBreakOffsets;
    FBreakOffsets := TPSList.Create;
    if not WriteCalculation(vout, vin) then
    begin
      vout.Free;
      vin.Free;
      FBreakOffsets.Free;
      FContinueOffsets.Free;
      FContinueOffsets := OldCO;
      FBreakOffsets := OldBo;
      exit;
    end;
    vout.Free;
    FParser.Next; // skip DO
    BlockWriteByte(BlockInfo, Cm_CNG); // only goto if expression is false
    BlockWriteLong(BlockInfo, $12345678);
    EPos := Length(BlockInfo.Proc.Data);
    if not WriteOutRec(vin, False) then
    begin
      MakeError('', ecInternalError, '00017');
      vin.Free;
      FBreakOffsets.Free;
      FContinueOffsets.Free;
      FContinueOffsets := OldCO;
      FBreakOffsets := OldBo;
      exit;
    end;
    Block := TPSBlockInfo.Create(BlockInfo);
    Block.SubType := tOneLiner;

    iOldWithCount := FWithCount;
    FWithCount := 0;
    iOldTryCount := FTryCount;
    FTryCount := 0;
    iOldExFnlCount := FExceptFinallyCount;
    FExceptFinallyCount := 0;

    if not ProcessSub(Block) then
    begin
      Block.Free;
      vin.Free;
      FBreakOffsets.Free;
      FContinueOffsets.Free;
      FContinueOffsets := OldCO;
      FBreakOffsets := OldBo;

      FWithCount := iOldWithCount;
			FTryCount := iOldTryCount;
      FExceptFinallyCount := iOldExFnlCount;

      exit;
    end;
    Block.Free;
    Debug_WriteLine(BlockInfo);
    BlockWriteByte(BlockInfo, Cm_G);
    BlockWriteLong(BlockInfo, Longint(SPos) - Length(BlockInfo.Proc.Data) - 4);
    {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
    unaligned(Longint((@BlockInfo.Proc.Data[EPos - 3])^)) := Length(BlockInfo.Proc.Data) - Longint(EPos) - 5;
    {$else}
    Longint((@BlockInfo.Proc.Data[EPos - 3])^) := Length(BlockInfo.Proc.Data) - Longint(EPos) - 5;
    {$endif}
    for i := 0 to FBreakOffsets.Count -1 do
    begin
      EPos := Cardinal(FBreakOffsets[I]);
      {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
      unaligned(Longint((@BlockInfo.Proc.Data[EPos - 3])^)) := Length(BlockInfo.Proc.Data) - Longint(EPos);
      {$else}
      Longint((@BlockInfo.Proc.Data[EPos - 3])^) := Length(BlockInfo.Proc.Data) - Longint(EPos);
      {$endif}
    end;
    for i := 0 to FContinueOffsets.Count -1 do
    begin
      EPos := Cardinal(FContinueOffsets[I]);
      {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
      unaligned(Longint((@BlockInfo.Proc.Data[EPos - 3])^)) := Longint(SPos) - Longint(EPos);
      {$else}
      Longint((@BlockInfo.Proc.Data[EPos - 3])^) := Longint(SPos) - Longint(EPos);
      {$endif}
    end;
    FBreakOffsets.Free;
    FContinueOffsets.Free;
    FContinueOffsets := OldCO;
    FBreakOffsets := OldBo;

    FWithCount := iOldWithCount;
    FTryCount := iOldTryCount;
    FExceptFinallyCount := iOldExFnlCount;

    vin.Free;
		if HasInvalidJumps(EPos, Length(BlockInfo.Proc.Data)) then
    begin
      Result := False;
      exit;
    end;
    Result := True;
  end;

  function ProcessRepeat: Boolean;
  var
    vin, vout: TPSValue;
    CPos, SPos, EPos: Cardinal;
    I: Longint;
    OldCo, OldBO: TPSList;
    Block: TPSBlockInfo;

    iOldWithCount: Integer;
    iOldTryCount: Integer;
    iOldExFnlCount: Integer;

  begin
    Result := False;
    Debug_WriteLine(BlockInfo);
    FParser.Next;
    OldCo := FContinueOffsets;
    FContinueOffsets := TPSList.Create;
    OldBO := FBreakOffsets;
    FBreakOffsets := TPSList.Create;
    vin := AllocStackReg(at2ut(FDefaultBoolType));
    SPos := Length(BlockInfo.Proc.Data);
    Block := TPSBlockInfo.Create(BlockInfo);
    Block.SubType := tRepeat;

    iOldWithCount := FWithCount;
    FWithCount := 0;
    iOldTryCount := FTryCount;
    FTryCount := 0;
    iOldExFnlCount := FExceptFinallyCount;
    FExceptFinallyCount := 0;

    if not ProcessSub(Block) then
    begin
      Block.Free;
      FBreakOffsets.Free;
      FContinueOffsets.Free;
      FContinueOffsets := OldCO;
      FBreakOffsets := OldBo;

      FWithCount := iOldWithCount;
      FTryCount := iOldTryCount;
      FExceptFinallyCount := iOldExFnlCount;

      vin.Free;
      exit;
    end;
    Block.Free;
    FParser.Next; //cstii_until
    vout := calc(CSTI_Semicolon);
    if vout = nil then
    begin
      FBreakOffsets.Free;
      FContinueOffsets.Free;
      FContinueOffsets := OldCO;
      FBreakOffsets := OldBo;

      FWithCount := iOldWithCount;
      FTryCount := iOldTryCount;
      FExceptFinallyCount := iOldExFnlCount;

      vin.Free;
      exit;
    end;
    CPos := Length(BlockInfo.Proc.Data);
    if not WriteCalculation(vout, vin) then
    begin
      vout.Free;
      vin.Free;
      FBreakOffsets.Free;
      FContinueOffsets.Free;
      FContinueOffsets := OldCO;
      FBreakOffsets := OldBo;

      FWithCount := iOldWithCount;
      FTryCount := iOldTryCount;
      FExceptFinallyCount := iOldExFnlCount;

      exit;
    end;
    vout.Free;
    BlockWriteByte(BlockInfo, Cm_CNG);
    BlockWriteLong(BlockInfo, $12345678);
    EPos := Length(BlockInfo. Proc.Data);
    if not WriteOutRec(vin, False) then
    begin
      MakeError('', ecInternalError, '00016');
      vin.Free;
      FBreakOffsets.Free;
      FContinueOffsets.Free;
      FContinueOffsets := OldCO;
      FBreakOffsets := OldBo;

      FWithCount := iOldWithCount;
      FTryCount := iOldTryCount;
      FExceptFinallyCount := iOldExFnlCount;

      exit;
    end;
    {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
    unaligned(Longint((@BlockInfo.Proc.Data[EPos - 3])^)) := Longint(SPos) -
      Length(BlockInfo.Proc.Data);
    {$else}
    Longint((@BlockInfo.Proc.Data[EPos - 3])^) := Longint(SPos) -
      Length(BlockInfo.Proc.Data);
    {$endif}
    for i := 0 to FBreakOffsets.Count -1 do
    begin
      EPos := Cardinal(FBreakOffsets[I]);
      {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
      unaligned(Longint((@BlockInfo.Proc.Data[EPos - 3])^)) := Length(BlockInfo. Proc.Data) - Longint(EPos);
      {$else}
      Longint((@BlockInfo.Proc.Data[EPos - 3])^) := Length(BlockInfo. Proc.Data) - Longint(EPos);
      {$endif}
    end;
    for i := 0 to FContinueOffsets.Count -1 do
    begin
      EPos := Cardinal(FContinueOffsets[I]);
      {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
      unaligned(Longint((@BlockInfo.Proc.Data[EPos - 3])^)) := Longint(CPos) - Longint(EPos);
      {$else}
      Longint((@BlockInfo.Proc.Data[EPos - 3])^) := Longint(CPos) - Longint(EPos);
      {$endif}
    end;
    FBreakOffsets.Free;
    FContinueOffsets.Free;
    FContinueOffsets := OldCO;
    FBreakOffsets := OldBo;

    FWithCount := iOldWithCount;
    FTryCount := iOldTryCount;
    FExceptFinallyCount := iOldExFnlCount;

    vin.Free;
    if HasInvalidJumps(SPos, Length(BlockInfo. Proc.Data)) then
    begin
      Result := False;
      exit;
    end;
    Result := True;
  end; {ProcessRepeat}

  function ProcessIf: Boolean;
  var
    vout, vin: TPSValue;
    SPos, EPos: Cardinal;
    Block: TPSBlockInfo;
  begin
    Result := False;
    Debug_WriteLine(BlockInfo);
    FParser.Next;
    vout := calc(CSTII_Then);
    if vout = nil then
      exit;
    if FParser.CurrTokenId <> CSTII_Then then
    begin
      vout.Free;
      MakeError('', ecThenExpected, '');
      exit;
    end;
    vin := AllocStackReg(at2ut(FDefaultBoolType));
    if not WriteCalculation(vout, vin) then
    begin
      vout.Free;
      vin.Free;
      exit;
    end;
    vout.Free;
    BlockWriteByte(BlockInfo, cm_sf);
    if not WriteOutRec(vin, False) then
    begin
      MakeError('', ecInternalError, '00018');
      vin.Free;
      exit;
    end;
    BlockWriteByte(BlockInfo, 1);
    vin.Free;
    BlockWriteByte(BlockInfo, cm_fg);
    BlockWriteLong(BlockInfo, $12345678);
    SPos := Length(BlockInfo.Proc.Data);
    FParser.Next; // skip then
    Block := TPSBlockInfo.Create(BlockInfo);
    Block.SubType := tifOneliner;
    if not ProcessSub(Block) then
    begin
      Block.Free;
      exit;
    end;
    Block.Free;
    if FParser.CurrTokenId = CSTII_Else then
    begin
      BlockWriteByte(BlockInfo, Cm_G);
      BlockWriteLong(BlockInfo, $12345678);
      EPos := Length(BlockInfo.Proc.Data);
      {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
      unaligned(Longint((@BlockInfo.Proc.Data[SPos - 3])^)) := Length(BlockInfo.Proc.Data) - Longint(SPos);
      {$else}
      Longint((@BlockInfo.Proc.Data[SPos - 3])^) := Length(BlockInfo.Proc.Data) - Longint(SPos);
      {$endif}
      FParser.Next;
      Block := TPSBlockInfo.Create(BlockInfo);
      Block.SubType := tOneLiner;
      if not ProcessSub(Block) then
      begin
        Block.Free;
        exit;
      end;
      Block.Free;
      {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
      unaligned(Longint((@BlockInfo.Proc.Data[EPos - 3])^)) := Length(BlockInfo.Proc.Data) - Longint(EPos);
      {$else}
      Longint((@BlockInfo.Proc.Data[EPos - 3])^) := Length(BlockInfo.Proc.Data) - Longint(EPos);
      {$endif}
    end
    else
    begin
      {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
      unaligned(Longint((@BlockInfo.Proc.Data[SPos - 3])^)) := Length(BlockInfo.Proc.Data) - Longint(SPos) + 5 - 5;
      {$else}
      Longint((@BlockInfo.Proc.Data[SPos - 3])^) := Length(BlockInfo.Proc.Data) - Longint(SPos) + 5 - 5;
      {$endif}
    end;
    Result := True;
  end; {ProcessIf}

  function _ProcessLabel: Longint; {0 = failed; 1 = successful; 2 = no label}
  var
    I, H: Longint;
    s: tbtString;
  begin
    h := MakeHash(FParser.GetToken);
    for i := 0 to BlockInfo.Proc.FLabels.Count -1 do
    begin
      s := BlockInfo.Proc.FLabels[I];
      delete(s, 1, 4);
      if Longint((@s[1])^) = h then
      begin
        delete(s, 1, 4);
        if s = FParser.GetToken then
        begin
          s := BlockInfo.Proc.FLabels[I];
          Cardinal((@s[1])^) := Length(BlockInfo.Proc.Data);
          BlockInfo.Proc.FLabels[i] := s;
          FParser.Next;
          if fParser.CurrTokenId = CSTI_Colon then
          begin
            Result := 1;
            FParser.Next;
            exit;
          end else begin
            MakeError('', ecColonExpected, '');
            Result := 0;
            Exit;
          end;
        end;
      end;
    end;
    result := 2;
  end;

  function ProcessIdentifier: Boolean;
  var
    vin, vout: TPSValue;
  begin
    Result := False;
    Debug_WriteLine(BlockInfo);
    vin := Calc(CSTI_Assignment);//GetIdentifier(2);
    if vin <> nil then
    begin
      if vin is TPSValueVar then
      begin // assignment needed
        if FParser.CurrTokenId <> CSTI_Assignment then
        begin
          MakeError('', ecAssignmentExpected, '');
          vin.Free;
          exit;
        end;
        FParser.Next;
        vout := calc(CSTI_Semicolon);
        if vout = nil then
        begin
          vin.Free;
          exit;
        end;
        if not WriteCalculation(vout, vin) then
        begin
          vin.Free;
          vout.Free;
          exit;
        end;
        vin.Free;
        vout.Free;
      end else if vin is TPSValueProc then
      begin
        Result := _ProcessFunction(TPSValueProc(vin), nil);
        vin.Free;
        Exit;
      end else
      begin
        MakeError('', ecInternalError, '20');
        vin.Free;
        REsult := False;
        exit;
      end;
    end
    else
    begin
      Result := False;
      exit;
    end;
    Result := True;
  end; {ProcessIdentifier}

  function ProcessCase: Boolean;
  var
    V1, V2, TempRec, Val, CalcItem: TPSValue;
    p: TPSBinValueOp;
    SPos, CurrP: Cardinal;
    I: Longint;
    EndReloc: TPSList;
    Block: TPSBlockInfo;

    function NewRec(val: TPSValue): TPSValueReplace;
    begin
      Result := TPSValueReplace.Create;
      Result.SetParserPos(FParser);
      Result.FNewValue := Val;
      Result.FreeNewValue := False;
    end;

    function Combine(v1, v2: TPSValue; Op: TPSBinOperatorType): TPSValue;
    begin
      if V1 = nil then
      begin
        Result := v2;
      end else if v2 = nil then
      begin
        Result := V1;
      end else
      begin
        Result := TPSBinValueOp.Create;
        TPSBinValueOp(Result).FType := FDefaultBoolType;
        TPSBinValueOp(Result).Operator := Op;
        Result.SetParserPos(FParser);
        TPSBinValueOp(Result).FVal1 := V1;
        TPSBinValueOp(Result).FVal2 := V2;
      end;
    end;


  begin
    Debug_WriteLine(BlockInfo);
    FParser.Next;
    Val := calc(CSTII_of);
    if Val = nil then
    begin
      ProcessCase := False;
      exit;
    end; {if}
    if FParser.CurrTokenId <> CSTII_Of then
    begin
      MakeError('', ecOfExpected, '');
      val.Free;
      ProcessCase := False;
      exit;
    end; {if}
    FParser.Next;
    TempRec := AllocStackReg(GetTypeNo(BlockInfo, Val));
    if not WriteCalculation(Val, TempRec) then
    begin
      TempRec.Free;
      val.Free;
      ProcessCase := False;
      exit;
    end; {if}
    val.Free;
    EndReloc := TPSList.Create;
    CalcItem := AllocStackReg(at2ut(FDefaultBoolType));
    SPos := Length(BlockInfo.Proc.Data);
    repeat
      V1 := nil;
      while true do
      begin
        Val := calc(CSTI_Colon);
        if (Val = nil) then
        begin
          V1.Free;
          CalcItem.Free;
          TempRec.Free;
          EndReloc.Free;
          ProcessCase := False;
          exit;
        end; {if}
        if fParser.CurrTokenID = CSTI_TwoDots then begin
          FParser.Next;
          V2 := Calc(CSTI_colon);
          if V2 = nil then begin
            V1.Free;
            CalcItem.Free;
            TempRec.Free;
            EndReloc.Free;
            ProcessCase := False;
            Val.Free;
            exit;
          end;
          p := TPSBinValueOp.Create;
          p.SetParserPos(FParser);
          p.Operator := otGreaterEqual;
          p.aType := at2ut(FDefaultBoolType);
          p.Val2 := Val;
          p.Val1 := NewRec(TempRec);
          Val := p;
          p := TPSBinValueOp.Create;
          p.SetParserPos(FParser);
          p.Operator := otLessEqual;
          p.aType := at2ut(FDefaultBoolType);
          p.Val2 := V2;
          p.Val1 := NewRec(TempRec);
          P := TPSBinValueOp(Combine(Val,P, otAnd));
        end else begin
          p := TPSBinValueOp.Create;
          p.SetParserPos(FParser);
          p.Operator := otEqual;
          p.aType := at2ut(FDefaultBoolType);
          p.Val1 := Val;
          p.Val2 := NewRec(TempRec);
        end;
        V1 := Combine(V1, P, otOr);
        if FParser.CurrTokenId = CSTI_Colon then Break;
        if FParser.CurrTokenID <> CSTI_Comma then
        begin
          MakeError('', ecColonExpected, '');
          V1.Free;
          CalcItem.Free;
          TempRec.Free;
          EndReloc.Free;
          ProcessCase := False;
          exit;
        end;
        FParser.Next;
      end;
      FParser.Next;
      if not WriteCalculation(V1, CalcItem) then
      begin
        CalcItem.Free;
        v1.Free;
        EndReloc.Free;
        ProcessCase := False;
        exit;
      end;
      v1.Free;
      BlockWriteByte(BlockInfo, Cm_CNG);
      BlockWriteLong(BlockInfo, $12345678);
      CurrP := Length(BlockInfo.Proc.Data);
      WriteOutRec(CalcItem, False);
      Block := TPSBlockInfo.Create(BlockInfo);
      Block.SubType := tifOneliner;
      if not ProcessSub(Block) then
      begin
        Block.Free;
        CalcItem.Free;
        TempRec.Free;
        EndReloc.Free;
        ProcessCase := False;
        exit;
      end;
      Block.Free;
      BlockWriteByte(BlockInfo, Cm_G);
      BlockWriteLong(BlockInfo, $12345678);
      EndReloc.Add(Pointer(Length(BlockInfo.Proc.Data)));
      {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
      unaligned(Cardinal((@BlockInfo.Proc.Data[CurrP - 3])^)) := Cardinal(Length(BlockInfo.Proc.Data)) - CurrP - 5;
      {$else}
      Cardinal((@BlockInfo.Proc.Data[CurrP - 3])^) := Cardinal(Length(BlockInfo.Proc.Data)) - CurrP - 5;
      {$endif}
      if FParser.CurrTokenID = CSTI_Semicolon then FParser.Next;
      if FParser.CurrTokenID = CSTII_Else then
      begin
        FParser.Next;
        Block := TPSBlockInfo.Create(BlockInfo);
        Block.SubType := tOneliner;
        if not ProcessSub(Block) then
        begin
          Block.Free;
          CalcItem.Free;
          TempRec.Free;
          EndReloc.Free;
          ProcessCase := False;
          exit;
        end;
        Block.Free;
        if FParser.CurrTokenID = CSTI_Semicolon then FParser.Next;
        if FParser.CurrtokenId <> CSTII_End then
        begin
          MakeError('', ecEndExpected, '');
          CalcItem.Free;
          TempRec.Free;
          EndReloc.Free;
          ProcessCase := False;
          exit;
        end;
      end;
    until FParser.CurrTokenID = CSTII_End;
    FParser.Next;
    for i := 0 to EndReloc.Count -1 do
    begin
      {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
      unaligned(Cardinal((@BlockInfo.Proc.Data[Cardinal(EndReloc[I])- 3])^)) := Cardinal(Length(BlockInfo.Proc.Data)) - Cardinal(EndReloc[I]);
      {$else}
      Cardinal((@BlockInfo.Proc.Data[Cardinal(EndReloc[I])- 3])^) := Cardinal(Length(BlockInfo.Proc.Data)) - Cardinal(EndReloc[I]);
      {$endif}
    end;
    CalcItem.Free;
    TempRec.Free;
    EndReloc.Free;
    if FContinueOffsets <> nil then
    begin
      for i := 0 to FContinueOffsets.Count -1 do
      begin
        if Cardinal(FContinueOffsets[i]) >= SPos then
        begin
          {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
          unaligned(Byte((@BlockInfo.Proc.Data[Longint(FContinueOffsets[i]) - 4])^)) := Cm_P2G;
	  {$else}
          Byte((@BlockInfo.Proc.Data[Longint(FContinueOffsets[i]) - 4])^) := Cm_P2G;
	  {$endif}
        end;
      end;
    end;
    if FBreakOffsets <> nil then
    begin
      for i := 0 to FBreakOffsets.Count -1 do
      begin
        if Cardinal(FBreakOffsets[i]) >= SPos then
        begin
          {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
          unaligned(Byte((@BlockInfo.Proc.Data[Longint(FBreakOffsets[i]) - 4])^)) := Cm_P2G;
	  {$else}
          Byte((@BlockInfo.Proc.Data[Longint(FBreakOffsets[i]) - 4])^) := Cm_P2G;
	  {$endif}
        end;
      end;
    end;
    if HasInvalidJumps(SPos, Length(BlockInfo.Proc.Data)) then
    begin
      Result := False;
      exit;
    end;
    Result := True;
  end; {ProcessCase}
	function ProcessGoto: Boolean;
  var
    I, H: Longint;
    s: tbtString;
  begin
    Debug_WriteLine(BlockInfo);
    FParser.Next;
    h := MakeHash(FParser.GetToken);
		for i := 0 to BlockInfo.Proc.FLabels.Count -1 do
    begin
      s := BlockInfo.Proc.FLabels[I];
      delete(s, 1, 4);
      if Longint((@s[1])^) = h then
      begin
        delete(s, 1, 4);
        if s = FParser.GetToken then
        begin
          FParser.Next;
          BlockWriteByte(BlockInfo, Cm_G);
          BlockWriteLong(BlockInfo, $12345678);
          BlockInfo.Proc.FGotos.Add(PS_mi2s(length(BlockInfo.Proc.Data))+PS_mi2s(i));
          Result := True;
          exit;
        end;
      end;
    end;
    MakeError('', ecUnknownIdentifier, FParser.OriginalToken);
    Result := False;
  end; {ProcessGoto}

  function ProcessWith: Boolean;
  var
    Block: TPSBlockInfo;
    aVar, aReplace: TPSValue;
    aType: TPSType;

    iStartOffset: Integer;

    tmp: TPSValue;
  begin
    Debug_WriteLine(BlockInfo);
    Block := TPSBlockInfo.Create(BlockInfo);
    Block.SubType := tOneLiner;

    FParser.Next;
    repeat
      aVar := GetIdentifier(0);
      if aVar = nil then
      begin
        block.Free;
        Result := False;
        exit;
      end;
      AType := GetTypeNo(BlockInfo, aVar);
      if (AType = nil) or ((aType.BaseType <> btRecord) and (aType.BaseType <> btClass)) then
      begin
        MakeError('', ecClassTypeExpected, '');
        Block.Free;
        Result := False;
        exit;
      end;

      aReplace := TPSValueReplace.Create;
      aReplace.SetParserPos(FParser);
      TPSValueReplace(aReplace).FreeOldValue := True;
      TPSValueReplace(aReplace).FreeNewValue := True;
      TPSValueReplace(aReplace).OldValue := aVar;

      //if aVar.InheritsFrom(TPSVar) then TPSVar(aVar).Use;
      tmp := AllocPointer(GetTypeNo(BlockInfo, aVar));
      TPSProcVar(BlockInfo.Proc.ProcVars[TPSValueAllocatedStackVar(tmp).LocalVarNo]).Use;
      PreWriteOutRec(tmp,GetTypeNo(BlockInfo, tmp));
      PreWriteOutRec(aVar,GetTypeNo(BlockInfo, aVar));
      BlockWriteByte(BlockInfo, cm_sp);
      WriteOutRec(tmp, false);
      WriteOutRec(aVar, false);
      TPSValueReplace(aReplace).NewValue := tmp;



      Block.WithList.Add(aReplace);

      if FParser.CurrTokenID = CSTII_do then
      begin
        FParser.Next;
        Break;
      end else
      if FParser.CurrTokenId <> CSTI_Comma then
      begin
        MakeError('', ecDoExpected, '');
        Block.Free;
        Result := False;
        exit;
      end;
      FParser.Next;
    until False;


    inc(FWithCount);

    iStartOffset := Length(Block.Proc.Data);

    if not (ProcessSub(Block) and (not HasInvalidJumps(iStartOffset,Length(BlockInfo.Proc.Data) + 1)) )  then
    begin
      dec(FWithCount);
      Block.Free;
      Result := False;
      exit;
    end;
    dec(FWithCount);

    AfterWriteOutRec(aVar);
    AfterWriteOutRec(tmp);
    Block.Free;
    Result := True;
  end;

  function ProcessTry: Boolean;
  var
    FStartOffset: Cardinal;
    iBlockStartOffset: Integer;
    Block: TPSBlockInfo;
  begin
    FParser.Next;
    BlockWriteByte(BlockInfo, cm_puexh);
    FStartOffset := Length(BlockInfo.Proc.Data) + 1;
    BlockWriteLong(BlockInfo, InvalidVal);
    BlockWriteLong(BlockInfo, InvalidVal);
    BlockWriteLong(BlockInfo, InvalidVal);
    BlockWriteLong(BlockInfo, InvalidVal);
    Block := TPSBlockInfo.Create(BlockInfo);
    Block.SubType := tTry;
    inc(FTryCount);
    if ProcessSub(Block) and (not HasInvalidJumps(FStartOffset,Length(BlockInfo.Proc.Data) + 1))  then
    begin
      dec(FTryCount);
      Block.Free;
      BlockWriteByte(BlockInfo, cm_poexh);
      BlockWriteByte(BlockInfo, 0);
      if FParser.CurrTokenID = CSTII_Except then
      begin
        FParser.Next;
        Cardinal((@BlockInfo.Proc.Data[FStartOffset + 4])^) := Cardinal(Length(BlockInfo.Proc.Data)) - FStartOffset - 15;
        iBlockStartOffset := Length(BlockInfo.Proc.Data) ;
        Block := TPSBlockInfo.Create(BlockInfo);
        Block.SubType := tTryEnd;
        inc(FExceptFinallyCount);
        if ProcessSub(Block) and (not HasInvalidJumps(iBlockStartOffset,Length(BlockInfo.Proc.Data) + 1))  then
        begin
          dec(FExceptFinallyCount);
          Block.Free;
          BlockWriteByte(BlockInfo, cm_poexh);
          BlockWriteByte(BlockInfo, 2);
          if FParser.CurrTokenId = CSTII_Finally then
          begin
            Cardinal((@BlockInfo.Proc.Data[FStartOffset + 8])^) := Cardinal(Length(BlockInfo.Proc.Data)) - FStartOffset - 15;
            iBlockStartOffset := Length(BlockInfo.Proc.Data) ;
            Block := TPSBlockInfo.Create(BlockInfo);
            Block.SubType := tTryEnd;
            FParser.Next;
           inc(FExceptFinallyCount);
            if ProcessSub(Block)  and (not HasInvalidJumps(iBlockStartOffset,Length(BlockInfo.Proc.Data) + 1))  then
            begin
              dec(FExceptFinallyCount);
              Block.Free;
              if FParser.CurrTokenId = CSTII_End then
              begin
                BlockWriteByte(BlockInfo, cm_poexh);
                BlockWriteByte(BlockInfo, 3);
              end else begin
                MakeError('', ecEndExpected, '');
                Result := False;
                exit;
              end;
            end else
            begin
              Block.Free;
              Result := False;
              dec(FExceptFinallyCount);
              exit;
            end;
          end else if FParser.CurrTokenID <> CSTII_End then
          begin
            MakeError('', ecEndExpected, '');
            Result := False;
            exit;
          end;
          FParser.Next;
        end else
        begin
          Block.Free;
          Result := False;
          dec(FExceptFinallyCount);
          exit;
        end;
      end else if FParser.CurrTokenId = CSTII_Finally then
      begin
        FParser.Next;
        Cardinal((@BlockInfo.Proc.Data[FStartOffset])^) := Cardinal(Length(BlockInfo.Proc.Data)) - FStartOffset - 15;
        iBlockStartOffset := Length(BlockInfo.Proc.Data) ;
        Block := TPSBlockInfo.Create(BlockInfo);
        Block.SubType := tTryEnd;
        inc(FExceptFinallyCount);
        if ProcessSub(Block)  and (not HasInvalidJumps(iBlockStartOffset,Length(BlockInfo.Proc.Data) + 1)) then
        begin
          dec(FExceptFinallyCount);
          Block.Free;
          BlockWriteByte(BlockInfo, cm_poexh);
          BlockWriteByte(BlockInfo, 1);
          if FParser.CurrTokenId = CSTII_Except then
          begin
            Cardinal((@BlockInfo.Proc.Data[FStartOffset + 4])^) := Cardinal(Length(BlockInfo.Proc.Data)) - FStartOffset - 15;
            iBlockStartOffset := Length(BlockInfo.Proc.Data) ;
            FParser.Next;
            Block := TPSBlockInfo.Create(BlockInfo);
            Block.SubType := tTryEnd;
            inc(FExceptFinallyCount);
            if ProcessSub(Block) and (not HasInvalidJumps(iBlockStartOffset,Length(BlockInfo.Proc.Data) + 1)) then
            begin
              dec(FExceptFinallyCount);
              Block.Free;
              if FParser.CurrTokenId = CSTII_End then
              begin
                BlockWriteByte(BlockInfo, cm_poexh);
                BlockWriteByte(BlockInfo, 2);
              end else begin
                MakeError('', ecEndExpected, '');
                Result := False;
                exit;
              end;
            end else
            begin
              Block.Free;
              Result := False;
              dec(FExceptFinallyCount);
              exit;
            end;
          end else if FParser.CurrTokenID <> CSTII_End then
          begin
            MakeError('', ecEndExpected, '');
            Result := False;
            exit;
          end;
          FParser.Next;
        end else
        begin
          Block.Free;
          Result := False;
          dec(FExceptFinallyCount);
          exit;
        end;
      end;
    end else
    begin
      Block.Free;
      Result := False;
      dec(FTryCount);
      exit;
    end;
    Cardinal((@BlockInfo.Proc.Data[FStartOffset + 12])^) := Cardinal(Length(BlockInfo.Proc.Data)) - FStartOffset - 15;
    Result := True;
  end; {ProcessTry}

var
  i: Integer;
  Block: TPSBlockInfo;

begin
  ProcessSub := False;
  if (BlockInfo.SubType = tProcBegin) or (BlockInfo.SubType= tMainBegin) or
{$IFDEF PS_USESSUPPORT}
     (BlockInfo.SubType = tUnitInit) or (BlockInfo.SubType= tUnitFinish) or // NvdS
{$endif}
     (BlockInfo.SubType= tSubBegin) then
  begin
    FParser.Next; // skip CSTII_Begin
  end;
  while True do
  begin
    case FParser.CurrTokenId of
      CSTII_Goto:
        begin
          if not ProcessGoto then
            Exit;
          if (BlockInfo.SubType = tifOneliner) or (BlockInfo.SubType = TOneLiner) then
            break;
        end;
      CSTII_With:
        begin
          if not ProcessWith then
            Exit;
          if (BlockInfo.SubType = tifOneliner) or (BlockInfo.SubType = TOneLiner) then
            break;
        end;
      CSTII_Try:
        begin
          if not ProcessTry then
            Exit;
          if (BlockInfo.SubType = tifOneliner) or (BlockInfo.SubType = TOneLiner) then
            break;
        end;
      CSTII_Finally, CSTII_Except:
        begin
          if (BlockInfo.SubType = tTry) or (BlockInfo.SubType = tTryEnd) then
            Break
          else
            begin
              MakeError('', ecEndExpected, '');
              Exit;
            end;
        end;
      CSTII_Begin:
        begin
          Block := TPSBlockInfo.Create(BlockInfo);
          Block.SubType := tSubBegin;
          if not ProcessSub(Block) then
          begin
            Block.Free;
            Exit;
          end;
          Block.Free;

          FParser.Next; // skip END
          if (BlockInfo.SubType = tifOneliner) or (BlockInfo.SubType = TOneLiner) then
            break;
        end;
      CSTI_Semicolon:
        begin

          if (BlockInfo.SubType = tifOneliner) or (BlockInfo.SubType = TOneLiner) then
            break
          else FParser.Next;
        end;
      CSTII_until:
        begin
          Debug_WriteLine(BlockInfo);
          if BlockInfo.SubType = tRepeat then
          begin
            break;
          end
          else
          begin
            MakeError('', ecIdentifierExpected, '');
            exit;
          end;
          if (BlockInfo.SubType = tifOneliner) or (BlockInfo.SubType = TOneLiner) then
            break;
        end;
      CSTII_Else:
        begin
          if BlockInfo.SubType = tifOneliner then
            break
          else
          begin
            MakeError('', ecIdentifierExpected, '');
            exit;
          end;
        end;
      CSTII_repeat:
        begin
          if not ProcessRepeat then
            exit;
          if (BlockInfo.SubType = tifOneliner) or (BlockInfo.SubType = TOneLiner) then
            break;
        end;
      CSTII_For:
        begin
          if not ProcessFor then
            exit;
          if (BlockInfo.SubType = tifOneliner) or (BlockInfo.SubType = TOneLiner) then
            break;
        end;
      CSTII_While:
        begin
          if not ProcessWhile then
            exit;
          if (BlockInfo.SubType = tifOneliner) or (BlockInfo.SubType = TOneLiner) then
            break;
        end;
      CSTII_Exit:
        begin
          Debug_WriteLine2(BlockInfo, BlockInfo.SubType = tProcBegin);
          BlockWriteByte(BlockInfo, Cm_R);
          FParser.Next;
          if (BlockInfo.SubType = tifOneliner) or (BlockInfo.SubType = TOneLiner) then
            break;
        end;
      CSTII_Case:
        begin
          if not ProcessCase then
            exit;
          if (BlockInfo.SubType = tifOneliner) or (BlockInfo.SubType = TOneLiner) then
            break;
        end;
      CSTII_If:
        begin
          if not ProcessIf then
            exit;
          if (BlockInfo.SubType = tifOneliner) or (BlockInfo.SubType = TOneLiner) then
            break;
        end;
      CSTI_OpenRound,
      CSTI_Identifier:
        begin
          case _ProcessLabel of
            0: Exit;
            1: ;
            else
            begin
              if FParser.GetToken = 'BREAK' then
              begin
                if FBreakOffsets = nil then
                begin
                  MakeError('', ecNotInLoop, '');
                  exit;
                end;
                for i := 0 to FExceptFinallyCount - 1 do
                begin
                  BlockWriteByte(BlockInfo, cm_poexh);
                  BlockWriteByte(BlockInfo, 1);
                end;

                for i := 0 to FTryCount - 1 do
                begin
                  BlockWriteByte(BlockInfo, cm_poexh);
                  BlockWriteByte(BlockInfo, 0);
                  BlockWriteByte(BlockInfo, cm_poexh);
                  BlockWriteByte(BlockInfo, 1);
                end;

                for i := 0 to FWithCount - 1 do
									BlockWriteByte(BlockInfo,cm_po);
                BlockWriteByte(BlockInfo, Cm_G);
                BlockWriteLong(BlockInfo, $12345678);
                FBreakOffsets.Add(Pointer(Length(BlockInfo.Proc.Data)));
                FParser.Next;
                if (BlockInfo.SubType= tifOneliner) or (BlockInfo.SubType = TOneLiner) then
                  break;
              end else if FParser.GetToken = 'CONTINUE' then
              begin
                if FBreakOffsets = nil then
                begin
                  MakeError('', ecNotInLoop, '');
                  exit;
                end;
                for i := 0 to FExceptFinallyCount - 1 do
                begin
                  BlockWriteByte(BlockInfo, cm_poexh);
                  BlockWriteByte(BlockInfo, 1);
                end;

                for i := 0 to FTryCount - 1 do
                begin
                  BlockWriteByte(BlockInfo, cm_poexh);
                  BlockWriteByte(BlockInfo, 0);
                  BlockWriteByte(BlockInfo, cm_poexh);
                  BlockWriteByte(BlockInfo, 1);
                end;

                for i := 0 to FWithCount - 1 do
									BlockWriteByte(BlockInfo,cm_po);
                BlockWriteByte(BlockInfo, Cm_G);
                BlockWriteLong(BlockInfo, $12345678);
                FContinueOffsets.Add(Pointer(Length(BlockInfo.Proc.Data)));
                FParser.Next;
                if (BlockInfo.SubType = tifOneliner) or (BlockInfo.SubType = TOneLiner) then
                  break;
              end else
              if not ProcessIdentifier then
                exit;
              if (BlockInfo.SubType = tifOneliner) or (BlockInfo.SubType = TOneLiner) then
                break;
            end;
          end; {case}
          
          if (BlockInfo.SubType = tifOneliner) or (BlockInfo.SubType = TOneLiner) then
            break;

        end;
    {$IFDEF PS_USESSUPPORT}
      CSTII_Finalization:                            //NvdS
        begin                                        //
          if (BlockInfo.SubType = tUnitInit) then    //
          begin                                      //
            break;                                   //
          end                                        //
          else                                       //
          begin                                      //
            MakeError('', ecIdentifierExpected, ''); //
            exit;                                    //
          end;                                       //
        end;                                         //nvds
    {$endif}
      CSTII_End:
        begin
          if (BlockInfo.SubType = tTryEnd) or (BlockInfo.SubType = tMainBegin) or
             (BlockInfo.SubType = tSubBegin) or (BlockInfo.SubType = tifOneliner) or
             (BlockInfo.SubType = tProcBegin) or (BlockInfo.SubType = TOneLiner)
    {$IFDEF PS_USESSUPPORT} or (BlockInfo.SubType = tUnitInit) or (BlockInfo.SubType = tUnitFinish) {$endif} then //nvds
          begin
            break;
          end
          else
          begin
            MakeError('', ecIdentifierExpected, '');
            exit;
          end;
        end;
      CSTI_EOF:
        begin
          MakeError('', ecUnexpectedEndOfFile, '');
          exit;
        end;
    else
      begin
        MakeError('', ecIdentifierExpected, '');
        exit;
      end;
    end;
  end;
  if (BlockInfo.SubType = tMainBegin) or (BlockInfo.SubType = tProcBegin)
 {$IFDEF PS_USESSUPPORT} or (BlockInfo.SubType = tUnitInit) or (BlockInfo.SubType = tUnitFinish) {$endif} then  //nvds
  begin
    Debug_WriteLine2(BlockInfo, BlockInfo.SubType = tProcBegin);
    BlockWriteByte(BlockInfo, Cm_R);
    {$IFDEF PS_USESSUPPORT}
    if FParser.CurrTokenId = CSTII_End then //nvds
    begin
    {$endif}
      FParser.Next; // skip end
      if ((BlockInfo.SubType = tMainBegin)
    {$IFDEF PS_USESSUPPORT} or (BlockInfo.SubType = tUnitInit) or (BlockInfo.SubType = tUnitFinish){$endif}) //nvds
         and (FParser.CurrTokenId <> CSTI_Period) then
      begin
        MakeError('', ecPeriodExpected, '');
        exit;
      end;
      if (BlockInfo.SubType = tProcBegin) and (FParser.CurrTokenId <> CSTI_Semicolon) then
      begin
        MakeError('', ecSemicolonExpected, '');
        exit;
      end;
      FParser.Next;
    {$IFDEF PS_USESSUPPORT}
    end;   //nvds
    {$endif}
  end
  else if (BlockInfo.SubType = tifOneliner) or (BlockInfo.SubType = TOneLiner) then
  begin
    if (FParser.CurrTokenID <> CSTII_Else) and (FParser.CurrTokenID <> CSTII_End) then
      if FParser.CurrTokenID <> CSTI_Semicolon then
      begin
        MakeError('', ecSemicolonExpected, '');
        exit;
      end;
  end;

  ProcessSub := True;
end;
procedure TPSPascalCompiler.UseProc(procdecl: TPSParametersDecl);
var
  i: Longint;
begin
  if procdecl.Result <> nil then
    procdecl.Result := at2ut(procdecl.Result);
  for i := 0 to procdecl.ParamCount -1 do
  begin
    procdecl.Params[i].aType := at2ut(procdecl.Params[i].aType);
  end;
end;

function TPSPascalCompiler.at2ut(p: TPSType): TPSType;
var
  i: Longint;
begin
  p := GetTypeCopyLink(p);
  if p = nil then
  begin
    Result := nil;
    exit;
  end;
  if not p.Used then
  begin
    p.Use;
    case p.BaseType of
      btStaticArray, btArray: TPSArrayType(p).ArrayTypeNo := at2ut(TPSArrayType(p).ArrayTypeNo);
      btRecord:
        begin
          for i := 0 to TPSRecordType(p).RecValCount -1 do
          begin
            TPSRecordType(p).RecVal(i).aType := at2ut(TPSRecordType(p).RecVal(i).aType);
          end;
        end;
      btSet: TPSSetType(p).SetType := at2ut(TPSSetType(p).SetType);
      btProcPtr:
        begin
          UseProc(TPSProceduralType(p).ProcDef);
        end;
    end;
    p.FFinalTypeNo := FCurrUsedTypeNo;
    inc(FCurrUsedTypeNo);
  end;
  Result := p;
end;

function TPSPascalCompiler.ProcessLabelForwards(Proc: TPSInternalProcedure): Boolean;
var
  i: Longint;
  s, s2: tbtString;
begin
  for i := 0 to Proc.FLabels.Count -1 do
  begin
    s := Proc.FLabels[I];
    if Longint((@s[1])^) = -1 then
    begin
      delete(s, 1, 8);
      MakeError('', ecUnSetLabel, s);
      Result := False;
      exit;
    end;
  end;
  for i := Proc.FGotos.Count -1 downto 0 do
  begin
    s := Proc.FGotos[I];
    s2 := Proc.FLabels[Cardinal((@s[5])^)];
    Cardinal((@Proc.Data[Cardinal((@s[1])^)-3])^) :=  Cardinal((@s2[1])^) - Cardinal((@s[1])^) ;
  end;
  Result := True;
end;


type
  TCompilerState = (csStart, csProgram, csUnit, csUses, csInterface, csInterfaceUses, csImplementation);

function TPSPascalCompiler.Compile(const s: tbtString): Boolean;
var
  Position: TCompilerState;
  i: Longint;
  {$IFDEF PS_USESSUPPORT}
  OldFileName: tbtString;
  OldParser  : TPSPascalParser;
  OldIsUnit  : Boolean;
  OldUnit    : TPSUnit;
  {$ENDIF}

  procedure Cleanup;
  var
    I: Longint;
    PT: TPSType;
  begin
    {$IFDEF PS_USESSUPPORT}
    if fInCompile>1 then
    begin
      dec(fInCompile);
      exit;
    end;
    {$ENDIF}

    if @FOnBeforeCleanup <> nil then
      FOnBeforeCleanup(Self);        // no reason it actually read the result of this call
    FGlobalBlock.Free;
    FGlobalBlock := nil;

    for I := 0 to FRegProcs.Count - 1 do
      TObject(FRegProcs[I]).Free;
    FRegProcs.Free;
    for i := 0 to FConstants.Count -1 do
    begin
      TPSConstant(FConstants[I]).Free;
    end;
    Fconstants.Free;
    for I := 0 to FVars.Count - 1 do
    begin
      TPSVar(FVars[I]).Free;
    end;
    FVars.Free;
    FVars := nil;
    for I := 0 to FProcs.Count - 1 do
      TPSProcedure(FProcs[I]).Free;
    FProcs.Free;
    FProcs := nil;
    //reverse free types: a custom type's attribute value type may point to a base type
    for I := FTypes.Count - 1 downto 0 do
    begin
      PT := FTypes[I];
      pt.Free;
    end;
    FTypes.Free;

{$IFNDEF PS_NOINTERFACES}
    for i := FInterfaces.Count -1 downto 0 do
      TPSInterface(FInterfaces[i]).Free;
    FInterfaces.Free;
{$ENDIF}

    for i := FClasses.Count -1 downto 0 do
    begin
      TPSCompileTimeClass(FClasses[I]).Free;
    end;
    FClasses.Free;
    for i := FAttributeTypes.Count -1 downto 0 do
    begin
      TPSAttributeType(FAttributeTypes[i]).Free;
    end;
    FAttributeTypes.Free;
    FAttributeTypes := nil;

    {$IFDEF PS_USESSUPPORT}
    for I := 0 to FUnitInits.Count - 1 do        //nvds
    begin                                        //nvds
      TPSBlockInfo(FUnitInits[I]).free;          //nvds
    end;                                         //nvds
    FUnitInits.Free;                             //nvds
    FUnitInits := nil;                           //
    for I := 0 to FUnitFinits.Count - 1 do       //nvds
    begin                                        //nvds
      TPSBlockInfo(FUnitFinits[I]).free;         //nvds
    end;                                         //nvds
    FUnitFinits.Free;                            //
    FUnitFinits := nil;                          //

    FreeAndNil(fUnits);
    FreeAndNil(FUses);
    fInCompile:=0;
    {$ENDIF}
  end;

  function MakeOutput: Boolean;

    procedure WriteByte(b: Byte);
    begin
      FOutput := FOutput + tbtChar(b);
    end;

    procedure WriteData(const Data; Len: Longint);
    var
      l: Longint;
    begin
      if Len < 0 then Len := 0;
      l := Length(FOutput);
      SetLength(FOutput, l + Len);
      Move(Data, FOutput[l + 1], Len);
    end;

    procedure WriteLong(l: Cardinal);
    begin
      WriteData(l, 4);
    end;

    procedure WriteVariant(p: PIfRVariant);
    begin
      WriteLong(p^.FType.FinalTypeNo);
      case p.FType.BaseType of
      btType: WriteLong(p^.ttype.FinalTypeNo);
      {$IFNDEF PS_NOWIDESTRING}
      btWideString:
        begin
          WriteLong(Length(tbtWideString(p^.twidestring)));
          WriteData(tbtwidestring(p^.twidestring)[1], 2*Length(tbtWideString(p^.twidestring)));
        end;
      btUnicodeString:
        begin
          WriteLong(Length(tbtUnicodestring(p^.twidestring)));
          WriteData(tbtUnicodestring(p^.twidestring)[1], 2*Length(tbtUnicodestring(p^.twidestring)));
        end;
      btWideChar: WriteData(p^.twidechar, 2);
      {$ENDIF}
      btSingle: WriteData(p^.tsingle, sizeof(tbtSingle));
      btDouble: WriteData(p^.tsingle, sizeof(tbtDouble));
      btExtended: WriteData(p^.tsingle, sizeof(tbtExtended));
      btCurrency: WriteData(p^.tsingle, sizeof(tbtCurrency));
      btChar: WriteData(p^.tchar, 1);
      btSet:
        begin
          WriteData(tbtString(p^.tstring)[1], Length(tbtString(p^.tstring)));
        end;
      btString:
        begin
          WriteLong(Length(tbtString(p^.tstring)));
          WriteData(tbtString(p^.tstring)[1], Length(tbtString(p^.tstring)));
        end;
      btenum:
        begin
          if TPSEnumType(p^.FType).HighValue <=256 then
            WriteData( p^.tu32, 1)
          else if TPSEnumType(p^.FType).HighValue <=65536 then
            WriteData(p^.tu32, 2)
          else
            WriteData(p^.tu32, 4);
        end;
      bts8,btu8: WriteData(p^.tu8, 1);
      bts16,btu16: WriteData(p^.tu16, 2);
      bts32,btu32: WriteData(p^.tu32, 4);
      {$IFNDEF PS_NOINT64}
      bts64: WriteData(p^.ts64, 8);
      {$ENDIF}
      btProcPtr: WriteData(p^.tu32, 4);
      {$IFDEF DEBUG}
      else
          asm int 3; end;
      {$ENDIF}
      end;
    end;

    procedure WriteAttributes(attr: TPSAttributes);
    var
      i, j: Longint;
    begin
      WriteLong(attr.Count);
      for i := 0 to Attr.Count -1 do
      begin
        j := Length(attr[i].FAttribType.Name);
        WriteLong(j);
        WriteData(Attr[i].FAttribType.Name[1], j);
        WriteLong(Attr[i].Count);
        for j := 0 to Attr[i].Count -1 do
        begin
          WriteVariant(Attr[i][j]);
        end;
      end;
    end;

    procedure WriteTypes;
    var
      l, n: Longint;
      bt: TPSBaseType;
      x: TPSType;
      s: tbtString;
      FExportName: tbtString;
      Items: TPSList;
      procedure WriteTypeNo(TypeNo: Cardinal);
      begin
        WriteData(TypeNo, 4);
      end;
    begin
      Items := TPSList.Create;
      try
        for l := 0 to FCurrUsedTypeNo -1 do
          Items.Add(nil);
        for l := 0 to FTypes.Count -1 do
        begin
          x := FTypes[l];
          if x.Used then
            Items[x.FinalTypeNo] := x;
        end;
        for l := 0 to Items.Count - 1 do
        begin
          x := Items[l];
          if x.FExportName then
            FExportName := x.Name
          else
            FExportName := '';
          if (x.BaseType = btExtClass) and (x is TPSUndefinedClassType) then
          begin
            x := GetTypeCopyLink(TPSUndefinedClassType(x).ExtClass.SelfType);
          end;
          bt := x.BaseType;
          if (x.BaseType = btType) or (x.BaseType = btNotificationVariant) then
          begin
            bt := btU32;
          end else
          if (x.BaseType = btEnum) then begin
            if TPSEnumType(x).HighValue <= 256 then
              bt := btU8
            else if TPSEnumType(x).HighValue <= 65536 then
              bt := btU16
            else
              bt := btU32;
          end;
          if FExportName <> '' then
          begin
            WriteByte(bt + 128);
          end
          else
            WriteByte(bt);
{$IFNDEF PS_NOINTERFACES} if x.BaseType = btInterface then
          begin
            WriteData(TPSInterfaceType(x).Intf.Guid, Sizeof(TGuid));
          end else {$ENDIF} if x.BaseType = btClass then
          begin
            WriteLong(Length(TPSClassType(X).Cl.FClassName));
            WriteData(TPSClassType(X).Cl.FClassName[1], Length(TPSClassType(X).Cl.FClassName));
          end else
          if (x.BaseType = btProcPtr) then
          begin
            s := DeclToBits(TPSProceduralType(x).ProcDef);
            WriteLong(Length(s));
            WriteData(s[1], Length(s));
          end else
          if (x.BaseType = btSet) then
          begin
            WriteLong(TPSSetType(x).BitSize);
          end else
          if (x.BaseType = btArray) or (x.basetype = btStaticArray) then
          begin
            WriteLong(TPSArrayType(x).ArrayTypeNo.FinalTypeNo);
            if (x.baseType = btstaticarray) then begin
              WriteLong(TPSStaticArrayType(x).Length);
              WriteLong(TPSStaticArrayType(x).StartOffset);      //<-additional StartOffset
            end;
          end else if x.BaseType = btRecord then
          begin
            n := TPSRecordType(x).RecValCount;
            WriteData( n, 4);
            for n := 0 to TPSRecordType(x).RecValCount - 1 do
              WriteTypeNo(TPSRecordType(x).RecVal(n).FType.FinalTypeNo);
          end;
          if FExportName <> '' then
          begin
            WriteLong(Length(FExportName));
            WriteData(FExportName[1], length(FExportName));
          end;
          WriteAttributes(x.Attributes);
        end;
      finally
        Items.Free;
      end;
    end;

    procedure WriteVars;
    var
      l,j : Longint;
      x: TPSVar;
    begin
      for l := 0 to FVars.Count - 1 do
      begin
        x := FVars[l];
        if x.SaveAsPointer then
        begin
          for j := FTypes.count -1 downto 0 do
          begin
            if TPSType(FTypes[j]).BaseType = btPointer then
            begin
              WriteLong(TPSType(FTypes[j]).FinalTypeNo);
              break;
            end;
          end;
        end else
          WriteLong(x.FType.FinalTypeNo);
        if x.exportname <> '' then
        begin
          WriteByte( 1);
          WriteLong(Length(X.ExportName));
          WriteData( X.ExportName[1], length(X.ExportName));
        end else
          WriteByte( 0);
      end;
    end;

    procedure WriteProcs;
    var
      l: Longint;
      xp: TPSProcedure;
      xo: TPSInternalProcedure;
      xe: TPSExternalProcedure;
      s: tbtString;
      att: Byte;
    begin
      for l := 0 to FProcs.Count - 1 do
      begin
        xp := FProcs[l];
        if xp.Attributes.Count <> 0 then att := 4 else att := 0;
        if xp.ClassType = TPSInternalProcedure then
        begin
          xo := TPSInternalProcedure(xp);
          xo.OutputDeclPosition := Length(FOutput);
          WriteByte(att or 2); // exported
          WriteLong(0); // offset is unknown at this time
          WriteLong(0); // length is also unknown at this time
          WriteLong(Length(xo.Name));
          WriteData( xo.Name[1], length(xo.Name));
          s := MakeExportDecl(xo.Decl);
          WriteLong(Length(s));
          WriteData( s[1], length(S));
        end
        else
        begin
          xe := TPSExternalProcedure(xp);
          if xe.RegProc.ImportDecl <> '' then
          begin
            WriteByte( att or 3); // imported
            if xe.RegProc.FExportName then
            begin
              WriteByte(Length(xe.RegProc.Name));
              WriteData(xe.RegProc.Name[1], Length(xe.RegProc.Name) and $FF);
            end else begin
              WriteByte(0);
            end;
            WriteLong(Length(xe.RegProc.ImportDecl));
            WriteData(xe.RegProc.ImportDecl[1], Length(xe.RegProc.ImportDecl));
          end else begin
            WriteByte(att or 1); // imported
            WriteByte(Length(xe.RegProc.Name));
            WriteData(xe.RegProc.Name[1], Length(xe.RegProc.Name) and $FF);
          end;
        end;
        if xp.Attributes.Count <> 0 then
          WriteAttributes(xp.Attributes);
      end;
    end;

    procedure WriteProcs2;
    var
      l: Longint;
      L2: Cardinal;
      x: TPSProcedure;
    begin
      for l := 0 to FProcs.Count - 1 do
      begin
        x := FProcs[l];
        if x.ClassType = TPSInternalProcedure then
        begin
          if TPSInternalProcedure(x).Data = '' then
            TPSInternalProcedure(x).Data := Chr(Cm_R);
          L2 := Length(FOutput);
          Move(L2, FOutput[TPSInternalProcedure(x).OutputDeclPosition + 2], 4);
          // write position
          WriteData(TPSInternalProcedure(x).Data[1], Length(TPSInternalProcedure(x).Data));
          L2 := Cardinal(Length(FOutput)) - L2;
          Move(L2, FOutput[TPSInternalProcedure(x).OutputDeclPosition + 6], 4); // write length
        end;
      end;
    end;



    {$IFDEF PS_USESSUPPORT}
    function FindMainProc: Cardinal;
    var
      l: Longint;
      Proc : TPSInternalProcedure;
      ProcData : tbtString;
      Calls : Integer;

      procedure WriteProc(const aData: Longint);
      var
        l: Longint;
      begin
        ProcData := ProcData + Chr(cm_c);
        l := Length(ProcData);
        SetLength(ProcData, l + 4);
        Move(aData, ProcData[l + 1], 4);
        inc(Calls);
      end;
    begin
      ProcData := ''; Calls := 1;
      for l := 0 to FUnitInits.Count-1 do
        if (FUnitInits[l] <> nil) and
           (TPSBlockInfo(FUnitInits[l]).Proc.Data<>'') then
          WriteProc(TPSBlockInfo(FUnitInits[l]).FProcNo);

      WriteProc(FGlobalBlock.FProcNo);

      for l := FUnitFinits.Count-1 downto 0 do
        if (FUnitFinits[l] <> nil) and
           (TPSBlockInfo(FUnitFinits[l]).Proc.Data<>'') then
          WriteProc(TPSBlockInfo(FUnitFinits[l]).FProcNo);

      if Calls = 1 then begin
        Result := FGlobalBlock.FProcNo;
      end else
      begin
        Proc := NewProc('Master proc', '!MASTERPROC');
        Result := FindProc('!MASTERPROC');
        Proc.data := Procdata + Chr(cm_R);
      end;
    end;
    {$ELSE}
    function FindMainProc: Cardinal;
    var
      l: Longint;
    begin
      for l := 0 to FProcs.Count - 1 do
      begin
        if (TPSProcedure(FProcs[l]).ClassType = TPSInternalProcedure) and
          (TPSInternalProcedure(FProcs[l]).Name = PSMainProcName) then
        begin
          Result := l;
          exit;
        end;
      end;
      Result := InvalidVal;
    end;
    {$ENDIF}

    procedure CreateDebugData;
    var
      I: Longint;
      p: TPSProcedure;
      pv: TPSVar;
      s: tbtString;
    begin
      s := #0;
      for I := 0 to FProcs.Count - 1 do
      begin
        p := FProcs[I];
        if p.ClassType = TPSInternalProcedure then
        begin
          if TPSInternalProcedure(p).Name = PSMainProcName then
            s := s + #1
          else
            s := s + TPSInternalProcedure(p).OriginalName + #1;
        end
        else
        begin
          s := s+ TPSExternalProcedure(p).RegProc.OrgName + #1;
        end;
      end;
      s := s + #0#1;
      for I := 0 to FVars.Count - 1 do
      begin
        pv := FVars[I];
        s := s + pv.OrgName + #1;
      end;
      s := s + #0;
      WriteDebugData(s);
    end;

  var                       //nvds
    MainProc : Cardinal;    //nvds

  begin
    if @FOnBeforeOutput <> nil then
    begin
      if not FOnBeforeOutput(Self) then
      begin
        Result := false;
        exit;
      end;
    end;
    MainProc := FindMainProc; //NvdS (need it here becose FindMainProc can create a New proc.
    CreateDebugData;
    WriteLong(PSValidHeader);
    WriteLong(PSCurrentBuildNo);
    WriteLong(FCurrUsedTypeNo);
    WriteLong(FProcs.Count);
    WriteLong(FVars.Count);
    WriteLong(MainProc);  //nvds
    WriteLong(0);
    WriteTypes;
    WriteProcs;
    WriteVars;
    WriteProcs2;

    Result := true;
  end;

  function CheckExports: Boolean;
  var
    i: Longint;
    p: TPSProcedure;
  begin
    if @FOnExportCheck = nil then
    begin
      result := true;
      exit;
    end;
    for i := 0 to FProcs.Count -1 do
    begin
      p := FProcs[I];
      if p.ClassType = TPSInternalProcedure then
      begin
        if not FOnExportCheck(Self, TPSInternalProcedure(p), MakeDecl(TPSInternalProcedure(p).Decl)) then
        begin
          Result := false;
          exit;
        end;
      end;
    end;
    Result := True;
  end;
  function DoConstBlock: Boolean;
  var
    COrgName: tbtString;
    CTemp, CValue: PIFRVariant;
    Cp: TPSConstant;
    TokenPos, TokenRow, TokenCol: Integer;
  begin
    FParser.Next;
    repeat
      if FParser.CurrTokenID <> CSTI_Identifier then
      begin
        MakeError('', ecIdentifierExpected, '');
        Result := False;
        Exit;
      end;
      TokenPos := FParser.CurrTokenPos;
      TokenRow := FParser.Row;
      TokenCol := FParser.Col;
      COrgName := FParser.OriginalToken;
      if IsDuplicate(FastUpperCase(COrgName), [dcVars, dcProcs, dcConsts]) then
      begin
        MakeError('', ecDuplicateIdentifier, COrgName);
        Result := False;
        exit;
      end;
      FParser.Next;
      if FParser.CurrTokenID <> CSTI_Equal then
      begin
        MakeError('', ecIsExpected, '');
        Result := False;
        Exit;
      end;
      FParser.Next;
      CValue := ReadConstant(FParser, CSTI_SemiColon);
      if CValue = nil then
      begin
        Result := False;
        Exit;
      end;
      if FParser.CurrTokenID <> CSTI_Semicolon then
      begin
        MakeError('', ecSemicolonExpected, '');
        Result := False;
        exit;
      end;
      cp := TPSConstant.Create;
      cp.Orgname := COrgName;
      cp.Name := FastUpperCase(COrgName);
      {$IFDEF PS_USESSUPPORT}
      cp.DeclareUnit:=fModule;
      {$ENDIF}
      cp.DeclarePos := TokenPos;
      cp.DeclareRow := TokenRow;
      cp.DeclareCol := TokenCol;
      New(CTemp);
      InitializeVariant(CTemp, CValue.FType);
      CopyVariantContents(cvalue, CTemp);
      cp.Value := CTemp;
      FConstants.Add(cp);
      DisposeVariant(CValue);
      FParser.Next;
    until FParser.CurrTokenId <> CSTI_Identifier;
    Result := True;
  end;

  function ProcessUses: Boolean;
  var
    {$IFNDEF PS_USESSUPPORT}
    FUses: TIfStringList;
    {$ENDIF}
    I: Longint;
    s: tbtString;
    {$IFDEF PS_USESSUPPORT}
    Parse: Boolean;
    ParseUnit: tbtString;
    ParserPos: TPSPascalParser;
    {$ENDIF}
  begin
    FParser.Next;
    {$IFNDEF PS_USESSUPPORT}
    FUses := TIfStringList.Create;
    FUses.Add('System');
    {$ENDIF}
    repeat
      if FParser.CurrTokenID <> CSTI_Identifier then
      begin
        MakeError('', ecIdentifierExpected, '');
        {$IFNDEF PS_USESSUPPORT}
        FUses.Free;
        {$ENDIF}
        Result := False;
        exit;
      end;
      s := FParser.GetToken;
      {$IFDEF PS_USESSUPPORT}
      Parse:=true;
      {$ENDIF}
      for i := 0 to FUses.Count -1 do
      begin
        if FUses[I] = s then
        begin
          {$IFNDEF PS_USESSUPPORT}
          MakeError('', ecDuplicateIdentifier, s);
          FUses.Free;
          Result := False;
          exit;
          {$ELSE}
          Parse:=false;
          {$ENDIF}
        end;
      end;
    {$IFDEF PS_USESSUPPORT}
      if fUnits.GetUnit(S).HasUses(fModule) then
      begin
        MakeError('', ecCrossReference, s);
        Result := False;
        exit;
      end;

      fUnit.AddUses(S);

      if Parse then
      begin
      {$ENDIF}
        FUses.Add(s);
        if @FOnUses <> nil then
        begin
          try
            {$IFDEF PS_USESSUPPORT}
            OldFileName:=fModule;
            fModule:=FParser.OriginalToken;
            ParseUnit:=FParser.OriginalToken;
            ParserPos:=FParser;
            {$ENDIF}
            if not OnUses(Self, FParser.GetToken) then
            begin
              {$IFNDEF PS_USESSUPPORT}
              FUses.Free;
              {$ELSE}
              FParser:=ParserPos;
              fModule:=OldFileName;
              MakeError(OldFileName, ecUnitNotFoundOrContainsErrors, ParseUnit);
              {$ENDIF}
              Result := False;
              exit;
            end;
            {$IFDEF PS_USESSUPPORT}
            fModule:=OldFileName;
            {$ENDIF}
          except
            on e: Exception do
            begin
              MakeError('', ecCustomError, tbtstring(e.Message));
              {$IFNDEF PS_USESSUPPORT}
              FUses.Free;
              {$ENDIF}
              Result := False;
              exit;
            end;
          end;
        end;
      {$IFDEF PS_USESSUPPORT}
      end;
      {$ENDIF}
      FParser.Next;
      if FParser.CurrTokenID = CSTI_Semicolon then break
      else if FParser.CurrTokenId <> CSTI_Comma then
      begin
        MakeError('', ecSemicolonExpected, '');
        Result := False;
        {$IFNDEF PS_USESSUPPORT}
        FUses.Free;
        {$ENDIF}
        exit;
      end;
      FParser.Next;
    until False;
    {$IFNDEF PS_USESSUPPORT}
    FUses.Free;
    {$ENDIF}
    FParser.next;
    Result := True;
  end;

var
  Proc: TPSProcedure;
  {$IFDEF PS_USESSUPPORT}
  Block : TPSBlockInfo; //nvds
  {$ENDIF}
begin
  Result := False;
  FWithCount := -1;

  {$IFDEF PS_USESSUPPORT}
  if fInCompile=0 then
  begin
  {$ENDIF}
    FUnitName := '';
    FCurrUsedTypeNo := 0;
    FIsUnit := False;
    Clear;
    FParserHadError := False;
    FParser.SetText(s);
    FAttributeTypes := TPSList.Create;
    FProcs := TPSList.Create;
    FConstants := TPSList.Create;
    FVars := TPSList.Create;
    FTypes := TPSList.Create;
    FRegProcs := TPSList.Create;
    FClasses := TPSList.Create;

    {$IFDEF PS_USESSUPPORT}
    FUnitInits := TPSList.Create; //nvds
    FUnitFinits:= TPSList.Create; //nvds

    FUses:=TIFStringList.Create;
    FUnits:=TPSUnitList.Create;
    {$ENDIF}
  {$IFNDEF PS_NOINTERFACES}  FInterfaces := TPSList.Create;{$ENDIF}

    FGlobalBlock := TPSBlockInfo.Create(nil);
    FGlobalBlock.SubType := tMainBegin;

    FGlobalBlock.Proc := NewProc(PSMainProcNameOrg, PSMainProcName);
    FGlobalBlock.ProcNo := FindProc(PSMainProcName);

    {$IFDEF PS_USESSUPPORT}
    OldFileName:=fModule;
    fModule:='System';
    FUses.Add('System');
    {$ENDIF}
    {$IFNDEF PS_NOSTANDARDTYPES}
    DefineStandardTypes;
    DefineStandardProcedures;
	{$ENDIF}
    if @FOnUses <> nil then
    begin
      try
        if not OnUses(Self, 'SYSTEM') then
        begin
          Cleanup;
          exit;
        end;
      except
        on e: Exception do
        begin
          MakeError('', ecCustomError, tbtstring(e.Message));
          Cleanup;
          exit;
        end;
      end;
    end;
  {$IFDEF PS_USESSUPPORT}
    fModule:=OldFileName;
    OldParser:=nil;
    OldUnit:=nil;
    OldIsUnit:=false; // defaults
  end
  else
  begin
    OldParser:=FParser;
    OldIsUnit:=FIsUnit;
    OldUnit:=fUnit;
    FParser:=TPSPascalParser.Create;
    FParser.SetText(s);
  end;

  fUnit:=fUnits.GetUnit(fModule);

  inc(fInCompile);
  {$ENDIF}

  Position := csStart;
  repeat
    if FParser.CurrTokenId = CSTI_EOF then
    begin
      if FParserHadError then
      begin
        Cleanup;
        exit;
      end;
      if FAllowNoEnd then
        Break
      else
      begin
        MakeError('', ecUnexpectedEndOfFile, '');
        Cleanup;
        exit;
      end;
    end;
    if (FParser.CurrTokenId = CSTII_Program) and (Position = csStart) then
    begin
      {$IFDEF PS_USESSUPPORT}
      if fInCompile>1 then
      begin
        MakeError('', ecNotAllowed, 'program');
        Cleanup;
        exit;
      end;
      {$ENDIF}
      Position := csProgram;
      FParser.Next;
      if FParser.CurrTokenId <> CSTI_Identifier then
      begin
        MakeError('', ecIdentifierExpected, '');
        Cleanup;
        exit;
      end;
      FParser.Next;
      if FParser.CurrTokenId <> CSTI_Semicolon then
      begin
        MakeError('', ecSemicolonExpected, '');
        Cleanup;
        exit;
      end;
      FParser.Next;
    end else
    if (Fparser.CurrTokenID = CSTII_Implementation) and ((Position = csinterface) or (position = csInterfaceUses)) then
    begin
      Position := csImplementation;
      FParser.Next;
    end else
    if (Fparser.CurrTokenID = CSTII_Interface) and (Position = csUnit) then
    begin
      Position := csInterface;
      FParser.Next;
    end else
    if (FParser.CurrTokenId = CSTII_Unit) and (Position = csStart) and (FAllowUnit) then
    begin
      Position := csUnit;
      FIsUnit := True;
      FParser.Next;
      if FParser.CurrTokenId <> CSTI_Identifier then
      begin
        MakeError('', ecIdentifierExpected, '');
        Cleanup;
        exit;
      end;
      if fInCompile = 1 then
        FUnitName := FParser.OriginalToken;
      FParser.Next;
      if FParser.CurrTokenId <> CSTI_Semicolon then
      begin
        MakeError('', ecSemicolonExpected, '');
        Cleanup;
        exit;
      end;
      FParser.Next;
    end
    else if (FParser.CurrTokenID = CSTII_Uses) and ((Position < csuses) or (Position = csInterface)) then
    begin
      if (Position = csInterface) or (Position =csInterfaceUses)
        then Position := csInterfaceUses
        else Position := csUses;
      if not ProcessUses then
      begin
         Cleanup;
        exit;
      end;
    end else if (FParser.CurrTokenId = CSTII_Procedure) or
      (FParser.CurrTokenId = CSTII_Function) or (FParser.CurrTokenID = FAttributesOpenTokenID) then
    begin
      if (Position = csInterface) or (position = csInterfaceUses) then
      begin
        if not ProcessFunction(True, nil) then
        begin
          Cleanup;
          exit;
        end;
      end else begin
        Position := csUses;
        if not ProcessFunction(False, nil) then
        begin
          Cleanup;
          exit;
        end;
      end;
    end
    else if (FParser.CurrTokenId = CSTII_Label) then
    begin
      if (Position = csInterface) or (Position =csInterfaceUses)
        then Position := csInterfaceUses
        else Position := csUses;
      if not ProcessLabel(FGlobalBlock.Proc) then
      begin
        Cleanup;
        exit;
      end;
    end
    else if (FParser.CurrTokenId = CSTII_Var) then
    begin
      if (Position = csInterface) or (Position =csInterfaceUses)
        then Position := csInterfaceUses
        else Position := csUses;
      if not DoVarBlock(nil) then
      begin
        Cleanup;
        exit;
      end;
    end
    else if (FParser.CurrTokenId = CSTII_Const) then
    begin
      if (Position = csInterface) or (Position =csInterfaceUses)
        then Position := csInterfaceUses
        else Position := csUses;
      if not DoConstBlock then
      begin
        Cleanup;
        exit;
      end;
    end
    else if (FParser.CurrTokenId = CSTII_Type) then
    begin
      if (Position = csInterface) or (Position =csInterfaceUses)
        then Position := csInterfaceUses
        else Position := csUses;
      if not DoTypeBlock(FParser) then
      begin
        Cleanup;
        exit;
      end;
    end
    else if (FParser.CurrTokenId = CSTII_Begin)
      {$IFDEF PS_USESSUPPORT}
             or ((FParser.CurrTokenID = CSTII_initialization) and FIsUnit) {$ENDIF}  then //nvds
    begin
      {$IFDEF PS_USESSUPPORT}
      if FIsUnit then
      begin
        Block := TPSBlockInfo.Create(nil); //nvds
        Block.SubType := tUnitInit;        //nvds
        Block.Proc := NewProc(PSMainProcNameOrg+'_'+fModule, FastUpperCase(PSMainProcName+'_'+fModule)); //nvds
        Block.ProcNo := FindProc(PSMainProcName+'_'+fModule);  //nvds
        Block.Proc.DeclareUnit:= fModule;
        Block.Proc.DeclarePos := FParser.CurrTokenPos;
        Block.Proc.DeclareRow := FParser.Row;
        Block.Proc.DeclareCol := FParser.Col;
        Block.Proc.Use;
        FUnitInits.Add(Block);
        if ProcessSub(Block) then
        begin
          if (Fparser.CurrTokenId = CSTI_EOF) THEN break;
        end
        else
        begin
          Cleanup;
          exit;
        end;
      end
      else
      begin
        FGlobalBlock.Proc.DeclareUnit:= fModule;
      {$ENDIF}
        FGlobalBlock.Proc.DeclarePos := FParser.CurrTokenPos;
        FGlobalBlock.Proc.DeclareRow := FParser.Row;
        FGlobalBlock.Proc.DeclareCol := FParser.Col;
        if ProcessSub(FGlobalBlock) then
        begin
          break;
        end
        else
        begin
          Cleanup;
          exit;
        end;
      {$IFDEF PS_USESSUPPORT}
      end;
      {$ENDIF}
    end
    {$IFDEF PS_USESSUPPORT}
    else if ((FParser.CurrTokenID = CSTII_finalization) and FIsUnit) then //NvdS
    begin
      Block := TPSBlockInfo.Create(nil);
      Block.SubType := tUnitFinish;
      Block.Proc := NewProc('!Finish_'+fModule, '!FINISH_'+FastUppercase(fModule));
      Block.ProcNo := FindProc('!FINISH_'+FastUppercase(fModule));
      Block.Proc.DeclareUnit:= fModule;

      Block.Proc.DeclarePos := FParser.CurrTokenPos;
      Block.Proc.DeclareRow := FParser.Row;
      Block.Proc.DeclareCol := FParser.Col;
      Block.Proc.use;
      FUnitFinits.Add(Block);
      if ProcessSub(Block) then
      begin
        break;
      end else begin
        Cleanup;
        Result :=  False; //Cleanup;
        exit;
      end;
    end
    {$endif}
    else if (Fparser.CurrTokenId = CSTII_End) and (FAllowNoBegin or FIsUnit) then
    begin
      FParser.Next;
      if FParser.CurrTokenID <> CSTI_Period then
      begin
        MakeError('', ecPeriodExpected, '');
        Cleanup;
        exit;
      end;
      break;
    end else
    begin
      MakeError('', ecBeginExpected, '');
      Cleanup;
      exit;
    end;
  until False;

  {$IFDEF PS_USESSUPPORT}
  dec(fInCompile);
  if fInCompile=0 then
  begin
  {$ENDIF}
    if not ProcessLabelForwards(FGlobalBlock.Proc) then
    begin
      Cleanup;
      exit;
    end;
    // NVDS: Do we need to check here also do a ProcessLabelForwards() for each Initialisation/finalization block?

    for i := 0 to FProcs.Count -1 do
    begin
      Proc := FProcs[I];
      if (Proc.ClassType = TPSInternalProcedure) and (TPSInternalProcedure(Proc).Forwarded) then
      begin
        with MakeError('', ecUnsatisfiedForward, TPSInternalProcedure(Proc).Name) do
        begin
          FPosition := TPSInternalProcedure(Proc).DeclarePos;
          FRow := TPSInternalProcedure(Proc).DeclareRow;
          FCol := TPSInternalProcedure(Proc).DeclareCol;
        end;
        Cleanup;
        Exit;
      end;
    end;
    if not CheckExports then
    begin
      Cleanup;
      exit;
    end;
    for i := 0 to FVars.Count -1 do
    begin
      if not TPSVar(FVars[I]).Used then
      begin
        with MakeHint({$IFDEF PS_USESSUPPORT}TPSVar(FVars[I]).DeclareUnit{$ELSE}''{$ENDIF}, ehVariableNotUsed, TPSVar(FVars[I]).Name) do
        begin
          FPosition := TPSVar(FVars[I]).DeclarePos;
          FRow := TPSVar(FVars[I]).DeclareRow;
          FCol := TPSVar(FVars[I]).DeclareCol;
        end;
      end;
    end;

    Result := MakeOutput;
    Cleanup;
  {$IFDEF PS_USESSUPPORT}
  end
  else
  begin
    fParser.Free;
    fParser:=OldParser;
    fIsUnit:=OldIsUnit;
    fUnit:=OldUnit;
    result:=true;
  end;
  {$ENDIF}
end;

constructor TPSPascalCompiler.Create;
begin
  inherited Create;
  FParser := TPSPascalParser.Create;
  FParser.OnParserError := ParserError;
  FAutoFreeList := TPSList.Create;
  FOutput := '';
  FAllowDuplicateRegister := true;
  {$IFDEF PS_USESSUPPORT}
  FAllowUnit := true;
  {$ENDIF}
  FMessages := TPSList.Create;
  FAttributesOpenTokenID := CSTI_OpenBlock;
  FAttributesCloseTokenID := CSTI_CloseBlock;
end;

destructor TPSPascalCompiler.Destroy;
begin
  Clear;
  FAutoFreeList.Free;

  FMessages.Free;
  FParser.Free;
  inherited Destroy;
end;

function TPSPascalCompiler.GetOutput(var s: tbtString): Boolean;
begin
  if Length(FOutput) <> 0 then
  begin
    s := FOutput;
    Result := True;
  end
  else
    Result := False;
end;

function TPSPascalCompiler.GetMsg(l: Longint): TPSPascalCompilerMessage;
begin
  Result := FMessages[l];
end;

function TPSPascalCompiler.GetMsgCount: Longint;
begin
  Result := FMessages.Count;
end;

procedure TPSPascalCompiler.DefineStandardTypes;
var
  i: Longint;
begin
  AddType('Byte', btU8);
  FDefaultBoolType := AddTypeS('Boolean', '(False, True)');
  FDefaultBoolType.ExportName := True;
  with TPSEnumType(AddType('LongBool', btEnum)) do
  begin
    HighValue := 2147483647; // make sure it's gonna be a 4 byte var
  end;
  with TPSEnumType(AddType('WordBool', btEnum)) do
  begin
    HighValue := 65535; // make sure it's gonna be a 2 byte var
  end;
  with TPSEnumType(AddType('ByteBool', btEnum)) do
  begin
    HighValue := 255; // make sure it's gonna be a 1 byte var
  end;
  //following 2 IFDEFs should actually be UNICODE IFDEFs...
  AddType({$IFDEF PS_PANSICHAR}'AnsiChar'{$ELSE}'Char'{$ENDIF}, btChar);
  {$IFDEF PS_PANSICHAR}
  AddType('Char', btWideChar);
  {$ENDIF}
  {$IFNDEF PS_NOWIDESTRING}
  AddType('WideChar', btWideChar);
  AddType('WideString', btWideString);
  AddType('UnicodeString', btUnicodeString);
  {$ENDIF}
  AddType('AnsiString', btString);
  {$IFNDEF PS_NOWIDESTRING}
    {$IFDEF DELPHI2009UP}
    AddType('string', btUnicodeString);
    AddType('NativeString', btUnicodeString);
    {$ELSE}
    AddType('string', btString);
    AddType('NativeString', btString);
    {$ENDIF}
  {$ELSE}
  AddType('string', btString);
  AddType('NativeString', btString);
  {$ENDIF}
  FAnyString := AddType('AnyString', btString);
  FAnyMethod := AddTypeS('AnyMethod', 'procedure');
  AddType('ShortInt', btS8);
  AddType('Word', btU16);
  AddType('SmallInt', btS16);
  AddType('LongInt', btS32);
  at2ut(AddType('___Pointer', btPointer));
  AddType('LongWord', btU32);
  AddTypeCopyN('Integer', 'LongInt');
  AddTypeCopyN('Cardinal', 'LongWord');
  AddType('tbtString', btString);
  {$IFNDEF PS_NOINT64}
  AddType('Int64', btS64);
  {$ENDIF}
  AddType('Single', btSingle);
  AddType('Double', btDouble);
  AddType('Extended', btExtended);
  AddType('Currency', btCurrency);
  AddType({$IFDEF PS_PANSICHAR}'PAnsiChar'{$ELSE}'PChar'{$ENDIF}, btPChar);
  AddType('Variant', btVariant);
  AddType('!NotificationVariant', btNotificationVariant);
  for i := FTypes.Count -1 downto 0 do AT2UT(FTypes[i]);
  TPSArrayType(AddType('TVariantArray', btArray)).ArrayTypeNo := FindType('Variant');

  with AddFunction('function Assigned(I: LongInt): Boolean;') do
  begin
    Name := '!ASSIGNED';
  end;

  with AddFunction('procedure _T(Name: tbtString; V: Variant);') do
  begin
    Name := '!NOTIFICATIONVARIANTSET';
  end;
  with AddFunction('function _T(Name: tbtString): Variant;') do
  begin
    Name := '!NOTIFICATIONVARIANTGET';
  end;
end;


function TPSPascalCompiler.FindType(const Name: tbtString): TPSType;
var
  i, n: Longint;
  RName: tbtString;
begin
  if FProcs = nil then begin Result := nil; exit;end;
  RName := Fastuppercase(Name);
  n := makehash(rname);
  for i := FTypes.Count - 1 downto 0 do
  begin
    Result := FTypes.Data[I];
    if (Result.NameHash = n) and (Result.name = rname) then
    begin
      Result := GetTypeCopyLink(Result);
      exit;
    end;
  end;
  result := nil;
end;

function TPSPascalCompiler.AddConstant(const Name: tbtString; FType: TPSType): TPSConstant;
var
  pc: TPSConstant;
  val: PIfRVariant;
begin
  if FProcs = nil then raise EPSCompilerException.Create(RPS_OnUseEventOnly);

  FType := GetTypeCopyLink(FType);
  if FType = nil then
    Raise EPSCompilerException.CreateFmt(RPS_UnableToRegisterConst, [name]);

  if not(AllowDuplicateRegister) and IsDuplicate(FastUpperCase(Name),[dcProcs, dcVars, dcConsts]) then
      Raise EPSCompilerException.CreateFmt(RPS_DuplicateIdent, [Name]);

  pc := TPSConstant.Create;
  pc.OrgName := name;
  pc.Name := FastUppercase(name);
  pc.DeclarePos:=InvalidVal;
  {$IFDEF PS_USESSUPPORT}
  pc.DeclareUnit:=fModule;
  {$ENDIF}
  New(Val);
  InitializeVariant(Val, FType);
  pc.Value := Val;
  FConstants.Add(pc);
  result := pc;
end;

function TPSPascalCompiler.ReadAttributes(Dest: TPSAttributes): Boolean;
var
  Att: TPSAttributeType;
  at: TPSAttribute;
  varp: PIfRVariant;
  h, i: Longint;
  s: tbtString;
begin
  if FParser.CurrTokenID <> FAttributesOpenTokenID then begin Result := true; exit; end;
  FParser.Next;
  if FParser.CurrTokenID <> CSTI_Identifier then
  begin
    MakeError('', ecIdentifierExpected, '');
    Result := False;
    exit;
  end;
  s := FParser.GetToken;
  h := MakeHash(s);
  att := nil;
  for i := FAttributeTypes.count -1 downto 0 do
  begin
    att := FAttributeTypes[i];
    if (att.FNameHash = h) and (att.FName = s) then
      Break;
    att := nil;
  end;
  if att = nil then
  begin
    MakeError('', ecUnknownIdentifier, FParser.OriginalToken);
    Result := False;
    exit;
  end;
  FParser.Next;
  i := 0;
  at := Dest.Add(att);
  {$IFDEF PS_USESSUPPORT}
  at.DeclareUnit:=fModule;
  {$ENDIF}
  at.DeclarePos := FParser.CurrTokenPos;
  at.DeclareRow := FParser.Row;
  at.DeclareCol := FParser.Col;
  while att.Fields[i].Hidden do
  begin
    at.AddValue(NewVariant(at2ut(att.Fields[i].FieldType)));
    inc(i);
  end;
  if FParser.CurrTokenId <> CSTI_OpenRound then
  begin
    MakeError('', ecOpenRoundExpected, '');
    Result := False;
    exit;
  end;
  FParser.Next;
  if i < Att.FieldCount then
  begin
    while i < att.FieldCount do
    begin
      varp := ReadConstant(FParser, CSTI_CloseRound);
      if varp = nil then
      begin
        Result := False;
        exit;
      end;
      at.AddValue(varp);
      if not IsCompatibleType(varp.FType, Att.Fields[i].FieldType, False) then
      begin
        MakeError('', ecTypeMismatch, '');
        Result := False;
        exit;
      end;
      Inc(i);
      while (i < Att.FieldCount) and (att.Fields[i].Hidden)  do
      begin
        at.AddValue(NewVariant(at2ut(att.Fields[i].FieldType)));
        inc(i);
      end;
      if i >= Att.FieldCount then
      begin
        break;
      end else
      begin
        if FParser.CurrTokenID <> CSTI_Comma then
        begin
          MakeError('', ecCommaExpected, '');
          Result := False;
          exit;
        end;
      end;
      FParser.Next;
    end;
  end;
  if FParser.CurrTokenID <> CSTI_CloseRound then
  begin
    MakeError('', ecCloseRoundExpected, '');
    Result := False;
    exit;
  end;
  FParser.Next;
  if FParser.CurrTokenID <> FAttributesCloseTokenID then
  begin
    MakeError('', ecUnClosedAttributes, '');
    Result := False;
    exit;
  end;
  FParser.Next;
  Result := True;
end;

type
  TConstOperation = class(TObject)
  private
    FDeclPosition, FDeclRow, FDeclCol: Cardinal;
  public
    property DeclPosition: Cardinal read FDeclPosition write FDeclPosition;
    property DeclRow: Cardinal read FDeclRow write FDeclRow;
    property DeclCol: Cardinal read FDeclCol write FDeclCol;
    procedure SetPos(Parser: TPSPascalParser);
  end;

  TUnConstOperation = class(TConstOperation)
  private
    FOpType: TPSUnOperatorType;
    FVal1: TConstOperation;
  public
    property OpType: TPSUnOperatorType read FOpType write FOpType;
    property Val1: TConstOperation read FVal1 write FVal1;

    destructor Destroy; override;
  end;

  TBinConstOperation = class(TConstOperation)
  private
    FOpType: TPSBinOperatorType;
    FVal2: TConstOperation;
    FVal1: TConstOperation;
  public
    property OpType: TPSBinOperatorType read FOpType write FOpType;
    property Val1: TConstOperation read FVal1 write FVal1;
    property Val2: TConstOperation read FVal2 write FVal2;

    destructor Destroy; override;
  end;

  TConstData = class(TConstOperation)
  private
    FData: PIfRVariant;
  public
    property Data: PIfRVariant read FData write FData;
    destructor Destroy; override;
  end;


function TPSPascalCompiler.IsBoolean(aType: TPSType): Boolean;
begin
  Result := (AType = FDefaultBoolType)
    or (AType.Name = 'LONGBOOL')
    or (AType.Name = 'WORDBOOL')
    or (AType.Name = 'BYTEBOOL');
end;


function TPSPascalCompiler.ReadConstant(FParser: TPSPascalParser; StopOn: TPSPasToken): PIfRVariant;

  function ReadExpression: TConstOperation; forward;
  function ReadTerm: TConstOperation; forward;
  function ReadFactor: TConstOperation;
  var
    NewVar: TConstOperation;
    NewVarU: TUnConstOperation;
    function GetConstantIdentifier: PIfRVariant;
    var
      s: tbtString;
      sh: Longint;
      i: Longint;
      p: TPSConstant;
    begin
      s := FParser.GetToken;
      sh := MakeHash(s);
      for i := FConstants.Count -1 downto 0 do
      begin
        p := FConstants[I];
        if (p.NameHash = sh) and (p.Name = s) then
        begin
          New(Result);
          InitializeVariant(Result, p.Value.FType);
          CopyVariantContents(P.Value, Result);
          FParser.Next;
          exit;
        end;
      end;
      MakeError('', ecUnknownIdentifier, '');
      Result := nil;
    end;
  begin
    case fParser.CurrTokenID of
      CSTII_Not:
      begin
        FParser.Next;
        NewVar := ReadFactor;
        if NewVar = nil then
        begin
          Result := nil;
          exit;
        end;
        NewVarU := TUnConstOperation.Create;
        NewVarU.OpType := otNot;
        NewVarU.Val1 := NewVar;
        NewVar := NewVarU;
      end;
      CSTI_Minus:
      begin
        FParser.Next;
        NewVar := ReadTerm;
        if NewVar = nil then
        begin
          Result := nil;
          exit;
        end;
        NewVarU := TUnConstOperation.Create;
        NewVarU.OpType := otMinus;
        NewVarU.Val1 := NewVar;
        NewVar := NewVarU;
      end;
      CSTI_OpenRound:
        begin
          FParser.Next;
          NewVar := ReadExpression;
          if NewVar = nil then
          begin
            Result := nil;
            exit;
          end;
          if FParser.CurrTokenId <> CSTI_CloseRound then
          begin
            NewVar.Free;
            Result := nil;
            MakeError('', ecCloseRoundExpected, '');
            exit;
          end;
          FParser.Next;
        end;
      CSTI_Char, CSTI_String:
        begin
          NewVar := TConstData.Create;
          NewVar.SetPos(FParser);
          TConstData(NewVar).Data := ReadString;
        end;
      CSTI_HexInt, CSTI_Integer:
        begin
          NewVar := TConstData.Create;
          NewVar.SetPos(FParser);
          TConstData(NewVar).Data := ReadInteger(FParser.GetToken);
          FParser.Next;
        end;
      CSTI_Real:
        begin
          NewVar := TConstData.Create;
          NewVar.SetPos(FParser);
          TConstData(NewVar).Data := ReadReal(FParser.GetToken);
          FParser.Next;
        end;
      CSTI_Identifier:
        begin
          NewVar := TConstData.Create;
          NewVar.SetPos(FParser);
          TConstData(NewVar).Data := GetConstantIdentifier;
          if TConstData(NewVar).Data = nil then
          begin
            NewVar.Free;
            Result := nil;
            exit;
          end
        end;
    else
      begin
        MakeError('', ecSyntaxError, '');
        Result := nil;
        exit;
      end;
    end; {case}
    Result := NewVar;
  end; // ReadFactor

  function ReadTerm: TConstOperation;
  var
    F1, F2: TConstOperation;
    F: TBinConstOperation;
    Token: TPSPasToken;
    Op: TPSBinOperatorType;
  begin
    F1 := ReadFactor;
    if F1 = nil then
    begin
      Result := nil;
      exit;
    end;
    while FParser.CurrTokenID in [CSTI_Multiply, CSTI_Divide, CSTII_Div, CSTII_Mod, CSTII_And, CSTII_Shl, CSTII_Shr] do
    begin
      Token := FParser.CurrTokenID;
      FParser.Next;
      F2 := ReadFactor;
      if f2 = nil then
      begin
        f1.Free;
        Result := nil;
        exit;
      end;
      case Token of
        CSTI_Multiply: Op := otMul;
        CSTI_Divide: Op := otDiv;
        CSTII_Div: Op := otIntDiv;
        CSTII_mod: Op := otMod;
        CSTII_and: Op := otAnd;
        CSTII_shl: Op := otShl;
        CSTII_shr: Op := otShr;
      else
        Op := otAdd;
      end;
      F := TBinConstOperation.Create;
      f.Val1 := F1;
      f.Val2 := F2;
      f.OpType := Op;
      f1 := f;
    end;
    Result := F1;
  end;  // ReadTerm

  function ReadSimpleExpression: TConstOperation;
  var
    F1, F2: TConstOperation;
    F: TBinConstOperation;
    Token: TPSPasToken;
    Op: TPSBinOperatorType;
  begin
    F1 := ReadTerm;
    if F1 = nil then
    begin
      Result := nil;
      exit;
    end;
    while FParser.CurrTokenID in [CSTI_Plus, CSTI_Minus, CSTII_Or, CSTII_Xor] do
    begin
      Token := FParser.CurrTokenID;
      FParser.Next;
      F2 := ReadTerm;
      if f2 = nil then
      begin
        f1.Free;
        Result := nil;
        exit;
      end;
      case Token of
        CSTI_Plus: Op := otAdd;
        CSTI_Minus: Op := otSub;
        CSTII_or: Op := otOr;
        CSTII_xor: Op := otXor;
      else
        Op := otAdd;
      end;
      F := TBinConstOperation.Create;
      f.Val1 := F1;
      f.Val2 := F2;
      f.OpType := Op;
      f1 := f;
    end;
    Result := F1;
  end;  // ReadSimpleExpression


  function ReadExpression: TConstOperation;
  var
    F1, F2: TConstOperation;
    F: TBinConstOperation;
    Token: TPSPasToken;
    Op: TPSBinOperatorType;
  begin
    F1 := ReadSimpleExpression;
    if F1 = nil then
    begin
      Result := nil;
      exit;
    end;
    while FParser.CurrTokenID in [ CSTI_GreaterEqual, CSTI_LessEqual, CSTI_Greater, CSTI_Less, CSTI_Equal, CSTI_NotEqual] do
    begin
      Token := FParser.CurrTokenID;
      FParser.Next;
      F2 := ReadSimpleExpression;
      if f2 = nil then
      begin
        f1.Free;
        Result := nil;
        exit;
      end;
      case Token of
        CSTI_GreaterEqual: Op := otGreaterEqual;
        CSTI_LessEqual: Op := otLessEqual;
        CSTI_Greater: Op := otGreater;
        CSTI_Less: Op := otLess;
        CSTI_Equal: Op := otEqual;
        CSTI_NotEqual: Op := otNotEqual;
      else
        Op := otAdd;
      end;
      F := TBinConstOperation.Create;
      f.Val1 := F1;
      f.Val2 := F2;
      f.OpType := Op;
      f1 := f;
    end;
    Result := F1;
  end;  // ReadExpression


  function EvalConst(P: TConstOperation): PIfRVariant;
  var
    p1, p2: PIfRVariant;
  begin
    if p is TBinConstOperation then
    begin
      p1 := EvalConst(TBinConstOperation(p).Val1);
      if p1 = nil then begin Result := nil; exit; end;
      p2 := EvalConst(TBinConstOperation(p).Val2);
      if p2 = nil then begin DisposeVariant(p1); Result := nil; exit; end;
      if not PreCalc(False, 0, p1, 0, p2, TBinConstOperation(p).OpType, p.DeclPosition, p.DeclRow, p.DeclCol) then
      begin
        DisposeVariant(p1);
        DisposeVariant(p2);
//        MakeError('', ecTypeMismatch, '');
        result := nil;
        exit;
      end;
      DisposeVariant(p2);
      Result := p1;
    end else if p is TUnConstOperation then
    begin
      with TUnConstOperation(P) do
      begin
        p1 := EvalConst(Val1);
        case OpType of
          otNot:
            case p1.FType.BaseType of
              btU8: p1.tu8 := not p1.tu8;
              btU16: p1.tu16 := not p1.tu16;
              btU32: p1.tu32 := not p1.tu32;
              bts8: p1.ts8 := not p1.ts8;
              bts16: p1.ts16 := not p1.ts16;
              bts32: p1.ts32 := not p1.ts32;
              {$IFNDEF PS_NOINT64}
              bts64: p1.ts64 := not p1.ts64;
              {$ENDIF}
            else
              begin
                MakeError('', ecTypeMismatch, '');
                DisposeVariant(p1);
                Result := nil;
                exit;
              end;
            end;
          otMinus:
            case p1.FType.BaseType of
              btU8: p1.tu8 := -p1.tu8;
              btU16: p1.tu16 := -p1.tu16;
              btU32: p1.tu32 := -p1.tu32;
              bts8: p1.ts8 := -p1.ts8;
              bts16: p1.ts16 := -p1.ts16;
              bts32: p1.ts32 := -p1.ts32;
              {$IFNDEF PS_NOINT64}
              bts64: p1.ts64 := -p1.ts64;
              {$ENDIF}
              btDouble: p1.tdouble := - p1.tDouble;
              btSingle: p1.tsingle := - p1.tsingle;
              btCurrency: p1.tcurrency := - p1.tcurrency;
              btExtended: p1.textended := - p1.textended;
            else
              begin
                MakeError('', ecTypeMismatch, '');
                DisposeVariant(p1);
                Result := nil;
                exit;
              end;
            end;
        else
          begin
            DisposeVariant(p1);
            Result := nil;
            exit;
          end;
        end;
      end;
      Result := p1;
    end else
    begin
      if ((p as TConstData).Data.FType.BaseType = btString)
      and (length(tbtstring((p as TConstData).Data.tstring)) =1) then
      begin
        New(p1);
        InitializeVariant(p1, FindBaseType(btChar));
        p1.tchar := tbtstring((p as TConstData).Data.tstring)[1];
        Result := p1;
      end else begin
        New(p1);
        InitializeVariant(p1, (p as TConstData).Data.FType);
        CopyVariantContents((p as TConstData).Data, p1);
        Result := p1;
      end;
    end;
  end;

var
  Val: TConstOperation;
begin
  Val := ReadExpression;
  if val = nil then
  begin
    Result := nil;
    exit;
  end;
  Result := EvalConst(Val);
  Val.Free;
end;

procedure TPSPascalCompiler.WriteDebugData(const s: tbtString);
begin
  FDebugOutput := FDebugOutput + s;
end;

function TPSPascalCompiler.GetDebugOutput(var s: tbtString): Boolean;
begin
  if Length(FDebugOutput) <> 0 then
  begin
    s := FDebugOutput;
    Result := True;
  end
  else
    Result := False;
end;

function TPSPascalCompiler.AddUsedFunction(var Proc: TPSInternalProcedure): Cardinal;
begin
  if FProcs = nil then raise EPSCompilerException.Create(RPS_OnUseEventOnly);
  Proc := TPSInternalProcedure.Create;
  FProcs.Add(Proc);
  Result := FProcs.Count - 1;
end;

{$IFNDEF PS_NOINTERFACES}
const
  IUnknown_Guid: TGuid = (D1: 0; d2: 0; d3: 0; d4: ($c0,00,00,00,00,00,00,$46));
  IDispatch_Guid: Tguid = (D1: $20400; D2: $0; D3: $0; D4:($C0, $0, $0, $0, $0, $0, $0, $46));
{$ENDIF}

procedure TPSPascalCompiler.DefineStandardProcedures;
var
  p: TPSRegProc;
begin
  { The following needs to be in synch in these 3 functions:
    -UPSCompiler.TPSPascalCompiler.DefineStandardProcedures
    -UPSRuntime.DefProc
    -UPSRuntime.TPSExec.RegisterStandardProcs
  }
  {$IFNDEF PS_NOINT64}
  AddFunction('function IntToStr(I: Int64): string;');
  {$ELSE}
  AddFunction('function IntToStr(I: Integer): string;');
  {$ENDIF}
  AddFunction('function StrToInt(S: string): LongInt;');
  AddFunction('function StrToIntDef(S: string; def: LongInt): LongInt;');
  AddFunction('function Copy(S: AnyString; iFrom, iCount: LongInt): AnyString;');
  AddFunction('function Pos(SubStr, S: AnyString): LongInt;');
  AddFunction('procedure Delete(var S: AnyString; iFrom, iCount: LongInt);');
  AddFunction('procedure Insert(S: AnyString; var s2: AnyString; iPos: LongInt);');
  AddFunction('function GetArrayLength: Integer;').Decl.AddParam.OrgName := 'Arr';
  p := AddFunction('procedure SetArrayLength;');
  with P.Decl.AddParam do
  begin
    OrgName := 'arr';
    Mode := pmInOut;
  end;
  with P.Decl.AddParam do
  begin
    OrgName := 'count';
    aType := FindBaseType(btS32);
  end;
  AddFunction('function StrGet(var S: string; I: Integer): Char;');
  AddFunction('function StrGet2(S: string; I: Integer): Char;');
  AddFunction('procedure StrSet(C: Char; I: Integer; var S: string);');
  {$IFNDEF PS_NOWIDESTRING}
  AddFunction('function WStrGet(var S: AnyString; I: Integer): WideChar;');
  AddFunction('procedure WStrSet(C: AnyString; I: Integer; var S: AnyString);');
  {$ENDIF}
  AddDelphiFunction('function VarArrayGet(var S: Variant; I: Integer): Variant;');
  AddDelphiFunction('procedure VarArraySet(C: Variant; I: Integer; var S: Variant);');
  AddFunction('function AnsiUpperCase(S: string): string;');
  AddFunction('function AnsiLowerCase(S: string): string;');
  AddFunction('function UpperCase(S: AnyString): AnyString;');
  AddFunction('function LowerCase(S: AnyString): AnyString;');
  AddFunction('function Trim(S: AnyString): AnyString;');
  AddFunction('function Length: Integer;').Decl.AddParam.OrgName := 'S';
  with AddFunction('procedure SetLength;').Decl do
  begin
    with AddParam do
    begin
      OrgName:='s';
      Mode:=pmInOut;
    end;
    with AddParam do
    begin
      OrgName:='NewLength';
      aType:=FindBaseType(btS32);  //Integer
    end;
  end;
  {$IFNDEF PS_NOINT64}
  AddFunction('function Low: Int64;').Decl.AddParam.OrgName := 'X';
  AddFunction('function High: Int64;').Decl.AddParam.OrgName := 'X';
  {$ELSE}
  AddFunction('function Low: Integer;').Decl.AddParam.OrgName := 'X';
  AddFunction('function High: Integer;').Decl.AddParam.OrgName := 'X';
  {$ENDIF}
  with AddFunction('procedure Dec;').Decl do begin
    with AddParam do
    begin
      OrgName:='x';
      Mode:=pmInOut;
    end;
  end;
  with AddFunction('procedure Inc;').Decl do begin
    with AddParam do
    begin
      OrgName:='x';
      Mode:=pmInOut;
    end;
  end;
  with AddFunction('procedure Include;').Decl do begin
    with AddParam do
    begin
      OrgName:='s';
      Mode:=pmInOut;
    end;
    with AddParam do
    begin
      OrgName:='m';
      Mode:=pmIn;
    end;
  end;
  with AddFunction('procedure Exclude;').Decl do begin
    with AddParam do
    begin
      OrgName:='s';
      Mode:=pmInOut;
    end;
    with AddParam do
    begin
      OrgName:='m';
      Mode:=pmIn;
    end;
  end;
  AddFunction('function Sin(E: Extended): Extended;');
  AddFunction('function Cos(E: Extended): Extended;');
  AddFunction('function Sqrt(E: Extended): Extended;');
  AddFunction('function Round(E: Extended): LongInt;');
  AddFunction('function Trunc(E: Extended): LongInt;');
  AddFunction('function Int(E: Extended): Extended;');
  AddFunction('function Pi: Extended;');
  AddFunction('function Abs(E: Extended): Extended;');
  AddFunction('function StrToFloat(S: string): Extended;');
  AddFunction('function FloatToStr(E: Extended): string;');
  AddFunction('function PadL(S: AnyString; I: LongInt): AnyString;');
  AddFunction('function PadR(S: AnyString; I: LongInt): AnyString;');
  AddFunction('function PadZ(S: AnyString; I: LongInt): AnyString;');
  AddFunction('function Replicate(C: Char; I: LongInt): string;');
  AddFunction('function StringOfChar(C: Char; I: LongInt): string;');
  AddTypeS('TVarType', 'Word');
  AddConstantN('varEmpty', 'Word').Value.tu16 := varempty;
  AddConstantN('varNull', 'Word').Value.tu16 := varnull;
  AddConstantN('varSmallInt', 'Word').Value.tu16 := varsmallint;
  AddConstantN('varInteger', 'Word').Value.tu16 := varinteger;
  AddConstantN('varSingle', 'Word').Value.tu16 := varsingle;
  AddConstantN('varDouble', 'Word').Value.tu16 := vardouble;
  AddConstantN('varCurrency', 'Word').Value.tu16 := varcurrency;
  AddConstantN('varDate', 'Word').Value.tu16 := vardate;
  AddConstantN('varOleStr', 'Word').Value.tu16 := varolestr;
  AddConstantN('varDispatch', 'Word').Value.tu16 := vardispatch;
  AddConstantN('varError', 'Word').Value.tu16 := varerror;
  AddConstantN('varBoolean', 'Word').Value.tu16 := varboolean;
  AddConstantN('varVariant', 'Word').Value.tu16 := varvariant;
  AddConstantN('varUnknown', 'Word').Value.tu16 := varunknown;
{$IFDEF DELPHI6UP}
  AddConstantN('varShortInt', 'Word').Value.tu16 := varshortint;
  AddConstantN('varByte', 'Word').Value.tu16 := varbyte;
  AddConstantN('varWord', 'Word').Value.tu16 := varword;
  AddConstantN('varLongWord', 'Word').Value.tu16 := varlongword;
  AddConstantN('varInt64', 'Word').Value.tu16 := varint64;
{$ENDIF}
{$IFDEF DELPHI5UP}
  AddConstantN('varStrArg', 'Word').Value.tu16 := varstrarg;
  AddConstantN('varAny', 'Word').Value.tu16 := varany;
{$ENDIF}
  AddConstantN('varString', 'Word').Value.tu16 := varstring;
  AddConstantN('varTypeMask', 'Word').Value.tu16 := vartypemask;
  AddConstantN('varArray', 'Word').Value.tu16 := vararray;
  AddConstantN('varByRef', 'Word').Value.tu16 := varByRef;
{$IFDEF UNICODE}
  AddConstantN('varUString', 'Word').Value.tu16 := varUString;
{$ENDIF}
  AddDelphiFunction('function Unassigned: Variant;');
  AddDelphiFunction('function VarIsEmpty(const V: Variant): Boolean;');
{$IFDEF DELPHI7UP}
  AddDelphiFunction('function VarIsClear(const V: Variant): Boolean;');
{$ENDIF}
  AddDelphiFunction('function Null: Variant;');
  AddDelphiFunction('function VarIsNull(const V: Variant): Boolean;');
  AddDelphiFunction('function VarType(const V: Variant): TVarType;');
 addTypeS('TIFException', '(ErNoError, erCannotImport, erInvalidType, ErInternalError, '+
   'erInvalidHeader, erInvalidOpcode, erInvalidOpcodeParameter, erNoMainProc, erOutOfGlobalVarsRange, '+
    'erOutOfProcRange, ErOutOfRange, erOutOfStackRange, ErTypeMismatch, erUnexpectedEof, '+
    'erVersionError, ErDivideByZero, ErMathError,erCouldNotCallProc, erOutofRecordRange, '+
    'erOutOfMemory, erException, erNullPointerException, erNullVariantError, erInterfaceNotSupported, erCustomError)');
  AddFunction('procedure RaiseLastException;');
  AddFunction('procedure RaiseException(Ex: TIFException; Param: string);');
  AddFunction('function ExceptionType: TIFException;');
  AddFunction('function ExceptionParam: string;');
  AddFunction('function ExceptionProc: Cardinal;');
  AddFunction('function ExceptionPos: Cardinal;');
  AddFunction('function ExceptionToString(er: TIFException; Param: string): string;');
  {$IFNDEF PS_NOINT64}
  AddFunction('function StrToInt64(S: string): Int64;');
  AddFunction('function Int64ToStr(I: Int64): string;');
  AddFunction('function StrToInt64Def(S: string; def: Int64): Int64;');
  {$ENDIF}

  with AddFunction('function SizeOf: LongInt;').Decl.AddParam do
  begin
    OrgName := 'Data';
  end;
{$IFNDEF PS_NOINTERFACES}
  with AddInterface(nil, IUnknown_Guid, 'IUnknown') do
  begin
    RegisterDummyMethod; // Query Interface
    RegisterDummyMethod; // _AddRef
    RegisterDummyMethod; // _Release
  end;
  with AddInterface(nil, IUnknown_Guid, 'IInterface') do
  begin
    RegisterDummyMethod; // Query Interface
    RegisterDummyMethod; // _AddRef
    RegisterDummyMethod; // _Release
  end;

 {$IFNDEF PS_NOIDISPATCH}
  with AddInterface(FindInterface('IUnknown'), IDispatch_Guid, 'IDispatch') do
  begin
    RegisterDummyMethod; // GetTypeCount
    RegisterDummyMethod; // GetTypeInfo
    RegisterDummyMethod; // GetIdsOfName
    RegisterDummyMethod; // Invoke
  end;
  with TPSInterfaceType(FindType('IDispatch')) do
  begin
    ExportName := True;
  end;
  AddDelphiFunction('function IdispatchInvoke(Self: IDispatch; PropertySet: Boolean; const Name: AnsiString; Par: array of Variant): Variant;');
 {$ENDIF}
{$ENDIF}
end;

function TPSPascalCompiler.GetTypeCount: Longint;
begin
  if FProcs = nil then raise EPSCompilerException.Create(RPS_OnUseEventOnly);
  Result := FTypes.Count;
end;

function TPSPascalCompiler.GetType(I: Longint): TPSType;
begin
  if FProcs = nil then raise EPSCompilerException.Create(RPS_OnUseEventOnly);
  Result := FTypes[I];
end;

function TPSPascalCompiler.GetVarCount: Longint;
begin
  if FProcs = nil then raise EPSCompilerException.Create(RPS_OnUseEventOnly);
  Result := FVars.Count;
end;

function TPSPascalCompiler.GetVar(I: Longint): TPSVar;
begin
  if FProcs = nil then raise EPSCompilerException.Create(RPS_OnUseEventOnly);
  Result := FVars[i];
end;

function TPSPascalCompiler.GetProcCount: Longint;
begin
  if FProcs = nil then raise EPSCompilerException.Create(RPS_OnUseEventOnly);
  Result := FProcs.Count;
end;

function TPSPascalCompiler.GetProc(I: Longint): TPSProcedure;
begin
  if FProcs = nil then raise EPSCompilerException.Create(RPS_OnUseEventOnly);
  Result := FProcs[i];
end;




function TPSPascalCompiler.AddUsedFunction2(var Proc: TPSExternalProcedure): Cardinal;
begin
  if FProcs = nil then raise EPSCompilerException.Create(RPS_OnUseEventOnly);
  Proc := TPSExternalProcedure.Create;
  FProcs.Add(Proc);
  Result := FProcs.Count -1;
end;

function TPSPascalCompiler.AddVariable(const Name: tbtString; FType: TPSType): TPSVar;
var
  P: TPSVar;
  s:tbtString;
begin
  if FProcs = nil then raise EPSCompilerException.Create(RPS_OnUseEventOnly);
  if FType = nil then raise EPSCompilerException.CreateFmt(RPS_InvalidTypeForVar, [Name]);
  s := Fastuppercase(Name);
  if IsDuplicate(s,[dcVars]) then raise EPSCompilerException.CreateFmt(RPS_DuplicateIdent, [Name]);

  p := TPSVar.Create;
  p.OrgName := Name;
  p.Name := s;
  p.FType := AT2UT(FType);
  p.exportname := p.Name;
  FVars.Add(p);
  Result := P;
end;

function TPSPascalCompiler.AddAttributeType: TPSAttributeType;
begin
  if FAttributeTypes = nil then Raise Exception.Create(RPS_OnUseEventOnly);
  Result := TPSAttributeType.Create;
  FAttributeTypes.Add(Result);
end;

function TPSPascalCompiler.FindAttributeType(const Name: tbtString): TPSAttributeType;
var
  h, i: Integer;
  n: tbtString;
begin
  if FAttributeTypes = nil then Raise Exception.Create(RPS_OnUseEventOnly);
  n := FastUpperCase(Name);
  h := MakeHash(n);
  for i := FAttributeTypes.Count -1 downto 0 do
  begin
    result := TPSAttributeType(FAttributeTypes[i]);
    if (Result.NameHash = h) and (Result.Name = n) then
      exit;
  end;
  result := nil;
end;
function TPSPascalCompiler.GetConstCount: Longint;
begin
  if FProcs = nil then raise EPSCompilerException.Create(RPS_OnUseEventOnly);
  result := FConstants.Count;
end;

function TPSPascalCompiler.GetConst(I: Longint): TPSConstant;
begin
  if FProcs = nil then raise EPSCompilerException.Create(RPS_OnUseEventOnly);
  Result := TPSConstant(FConstants[i]);
end;

function TPSPascalCompiler.GetRegProcCount: Longint;
begin
  if FProcs = nil then raise EPSCompilerException.Create(RPS_OnUseEventOnly);
  Result := FRegProcs.Count;
end;

function TPSPascalCompiler.GetRegProc(I: Longint): TPSRegProc;
begin
  if FProcs = nil then raise EPSCompilerException.Create(RPS_OnUseEventOnly);
  Result := TPSRegProc(FRegProcs[i]);
end;


procedure TPSPascalCompiler.AddToFreeList(Obj: TObject);
begin
  FAutoFreeList.Add(Obj);
end;

function TPSPascalCompiler.AddConstantN(const Name,
  FType: tbtString): TPSConstant;
begin
  Result := AddConstant(Name, FindType(FType));
end;

function TPSPascalCompiler.AddTypeCopy(const Name: tbtString;
  TypeNo: TPSType): TPSType;
begin
  if FProcs = nil then raise EPSCompilerException.Create(RPS_OnUseEventOnly);
  TypeNo := GetTypeCopyLink(TypeNo);
  if Typeno = nil then raise EPSCompilerException.Create(RPS_InvalidType);
  Result := AddType(Name, BtTypeCopy);
  TPSTypeLink(Result).LinkTypeNo := TypeNo;
end;

function TPSPascalCompiler.AddTypeCopyN(const Name,
  FType: tbtString): TPSType;
begin
  Result := AddTypeCopy(Name, FindType(FType));
end;


function TPSPascalCompiler.AddUsedVariable(const Name: tbtString;
  FType: TPSType): TPSVar;
begin
  Result := AddVariable(Name, FType);
  if Result <> nil then
    Result.Use;
end;

function TPSPascalCompiler.AddUsedVariableN(const Name,
  FType: tbtString): TPSVar;
begin
  Result := AddVariable(Name, FindType(FType));
  if Result <> nil then
    Result.Use;
end;

function TPSPascalCompiler.AddVariableN(const Name,
  FType: tbtString): TPSVar;
begin
  Result := AddVariable(Name, FindType(FType));
end;

function TPSPascalCompiler.AddUsedPtrVariable(const Name: tbtString; FType: TPSType): TPSVar;
begin
  Result := AddVariable(Name, FType);
  if Result <> nil then
  begin
    result.SaveAsPointer := True;
    Result.Use;
  end;
end;

function TPSPascalCompiler.AddUsedPtrVariableN(const Name, FType: tbtString): TPSVar;
begin
  Result := AddVariable(Name, FindType(FType));
  if Result <> nil then
  begin
    result.SaveAsPointer := True;
    Result.Use;
  end;
end;

function TPSPascalCompiler.AddTypeS(const Name, Decl: tbtString): TPSType;
var
  Parser: TPSPascalParser;
begin
  if FProcs = nil then raise EPSCompilerException.Create(RPS_OnUseEventOnly);
  Parser := TPSPascalParser.Create;
  Parser.SetText(Decl);

  if not(AllowDuplicateRegister) and (FindType(Name)<>nil) then
      Raise EPSCompilerException.CreateFmt(RPS_DuplicateIdent, [Name]);

  Result := ReadType(Name, Parser);
  if Result<>nil then
  begin
    Result.ExportName := True;
    Result.DeclarePos:=InvalidVal;
    {$IFDEF PS_USESSUPPORT}
    Result.DeclareUnit:=fModule;
    {$ENDIF}
    Result.DeclareRow:=0;
    Result.DeclareCol:=0;
  end;
  Parser.Free;
  if result = nil then Raise EPSCompilerException.CreateFmt(RPS_UnableToRegisterType, [name]);
end;


function TPSPascalCompiler.CheckCompatProc(P: TPSType; ProcNo: Cardinal): Boolean;
var
  i: Longint;
  s1, s2: TPSParametersDecl;
begin
  if p.BaseType <> btProcPtr then begin
    Result := False;
    Exit;
  end;

  if p = FAnyMethod then begin
    Result := True;
    Exit;
  end;

  S1 := TPSProceduralType(p).ProcDef;

  if TPSProcedure(FProcs[ProcNo]).ClassType = TPSInternalProcedure then
    s2 := TPSInternalProcedure(FProcs[ProcNo]).Decl
  else
    s2 := TPSExternalProcedure(FProcs[ProcNo]).RegProc.Decl;
  if (s1.Result <> s2.Result) or (s1.ParamCount <> s2.ParamCount) then
  begin
    Result := False;
    Exit;
  end;
  for i := 0 to s1.ParamCount -1 do
  begin
    if (s1.Params[i].Mode <> s2.Params[i].Mode) or (s1.Params[i].aType <> s2.Params[i].aType) then
    begin
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

function TPSPascalCompiler.MakeExportDecl(decl: TPSParametersDecl): tbtString;
var
  i: Longint;
begin
  if Decl.Result = nil then result := '-1' else
  result := IntToStr(Decl.Result.FinalTypeNo);

  for i := 0 to decl.ParamCount -1 do
  begin
    if decl.GetParam(i).Mode = pmIn then
      Result := Result + ' @'
    else
      Result := Result + ' !';
    Result := Result + inttostr(decl.GetParam(i).aType.FinalTypeNo);
  end;
end;


function TPSPascalCompiler.IsIntBoolType(aType: TPSType): Boolean;
begin
  if Isboolean(aType) then begin Result := True; exit;end;

  case aType.BaseType of
    btU8, btS8, btU16, btS16, btU32, btS32{$IFNDEF PS_NOINT64}, btS64{$ENDIF}: Result := True;
  else
    Result := False;
  end;
end;


procedure TPSPascalCompiler.ParserError(Parser: TObject;
  Kind: TPSParserErrorKind);
begin
  FParserHadError := True;
  case Kind of
    ICOMMENTERROR: MakeError('', ecCommentError, '');
    ISTRINGERROR: MakeError('', ecStringError, '');
    ICHARERROR: MakeError('', ecCharError, '');
  else
    MakeError('', ecSyntaxError, '');
  end;
end;


function TPSPascalCompiler.AddDelphiFunction(const Decl: tbtString): TPSRegProc;
var
  p: TPSRegProc;
  pDecl: TPSParametersDecl;
  DOrgName: tbtString;
  FT: TPMFuncType;
  i: Longint;

begin
  pDecl := TPSParametersDecl.Create;
{$IFNDEF DELPHI_TOKYO_UP}
  p := nil;
{$ENDIF}
  try
    if not ParseMethod(Self, '', Decl, DOrgName, pDecl, FT) then
      Raise EPSCompilerException.CreateFmt(RPS_UnableToRegisterFunction, [Decl]);

    if (FindProc(DOrgName)<>InvalidVal) and not(FAllowDuplicateRegister) then
      Raise EPSCompilerException.CreateFmt(RPS_DuplicateIdent, [Decl]);

    p := TPSRegProc.Create;
    P.Name := FastUppercase(DOrgName);
    p.OrgName := DOrgName;
    p.ExportName := True;
    p.Decl.Assign(pDecl);

    FRegProcs.Add(p);

    if pDecl.Result = nil then
    begin
      p.ImportDecl := p.ImportDecl + #0;
    end else
      p.ImportDecl := p.ImportDecl + #1;
    for i := 0 to pDecl.ParamCount -1 do
    begin
      if pDecl.Params[i].Mode <> pmIn then
        p.ImportDecl := p.ImportDecl + #1
      else
        p.ImportDecl := p.ImportDecl + #0;
    end;
  finally
    pDecl.Free;
  end;
  Result := p;
end;

{$IFNDEF PS_NOINTERFACES}
function TPSPascalCompiler.AddInterface(InheritedFrom: TPSInterface; Guid: TGuid; const Name: tbtString): TPSInterface;
var
  f: TPSType;
begin
  if FProcs = nil then raise EPSCompilerException.Create(RPS_OnUseEventOnly);
  f := FindType(Name);
  if (f<>nil) and not(FAllowDuplicateRegister) then
    Raise EPSCompilerException.CreateFmt(RPS_DuplicateIdent, [Name]);

  if (f <> nil) and (f is TPSInterfaceType) then
  begin
    result := TPSInterfaceType(f).Intf;
    Result.Guid := Guid;
    Result.InheritedFrom := InheritedFrom;
    exit;
  end;
  f := AddType(Name, btInterface);
  Result := TPSInterface.Create(Self, InheritedFrom, GUID, FastUppercase(Name), f);
  FInterfaces.Add(Result);
  TPSInterfaceType(f).Intf := Result;
end;

function TPSPascalCompiler.FindInterface(const Name: tbtString): TPSInterface;
var
  n: tbtString;
  i, nh: Longint;
begin
  if FProcs = nil then raise EPSCompilerException.Create(RPS_OnUseEventOnly);
  n := FastUpperCase(Name);
  nh := MakeHash(n);
  for i := FInterfaces.Count -1 downto 0 do
  begin
    Result := FInterfaces[i];
    if (Result.NameHash = nh) and (Result.Name = N) then
      exit;
  end;
  raise EPSCompilerException.CreateFmt(RPS_UnknownInterface, [Name]);
end;
{$ENDIF}
function TPSPascalCompiler.AddClass(InheritsFrom: TPSCompileTimeClass; aClass: TClass): TPSCompileTimeClass;
var
  f: TPSType;
begin
  if FProcs = nil then raise EPSCompilerException.Create(RPS_OnUseEventOnly);
  Result := FindClass(tbtstring(aClass.ClassName));
  if (Result<>nil) and not(FAllowDuplicateRegister) then
    Raise EPSCompilerException.CreateFmt(RPS_DuplicateIdent, [aClass.ClassName]);
  if Result <> nil then
  begin
    if InheritsFrom <> nil then
      Result.FInheritsFrom := InheritsFrom;
    exit;
  end;
  f := AddType(tbtstring(aClass.ClassName), btClass);
  Result := TPSCompileTimeClass.CreateC(aClass, Self, f);
  Result.FInheritsFrom := InheritsFrom;
  FClasses.Add(Result);
  TPSClassType(f).Cl := Result;
  f.ExportName := True;
end;

function TPSPascalCompiler.AddClassN(InheritsFrom: TPSCompileTimeClass; const aClass: tbtString): TPSCompileTimeClass;
var
  f: TPSType;
begin
  if FProcs = nil then raise EPSCompilerException.Create(RPS_OnUseEventOnly);
  Result := FindClass(aClass);
  if (Result<>nil) and (Result.FInheritsFrom<>nil) and not(FAllowDuplicateRegister) then
    Raise EPSCompilerException.CreateFmt(RPS_DuplicateIdent, [aClass]);
  if Result <> nil then
  begin
    if InheritsFrom <> nil then
      Result.FInheritsFrom := InheritsFrom;
    exit;
  end;
  f := AddType(aClass, btClass);
  Result := TPSCompileTimeClass.Create(FastUppercase(aClass), Self, f);
  TPSClassType(f).Cl := Result;
  Result.FInheritsFrom := InheritsFrom;
  FClasses.Add(Result);
  TPSClassType(f).Cl := Result;
  f.ExportName := True;
end;

function TPSPascalCompiler.FindClass(const aClass: tbtString): TPSCompileTimeClass;
var
  i: Longint;
  Cl: tbtString;
  H: Longint;
  x: TPSCompileTimeClass;
begin
  cl := FastUpperCase(aClass);
  H := MakeHash(Cl);
  for i :=0 to FClasses.Count -1 do
  begin
    x := FClasses[I];
    if (X.FClassNameHash = H) and (X.FClassName = Cl) then
    begin
      Result := X;
      Exit;
    end;
  end;
  Result := nil;
end;



{  }

function TransDoubleToStr(D: Double): tbtString;
begin
  SetLength(Result, SizeOf(Double));
  Double((@Result[1])^) := D;
end;

function TransSingleToStr(D: Single): tbtString;
begin
  SetLength(Result, SizeOf(Single));
  Single((@Result[1])^) := D;
end;

function TransExtendedToStr(D: Extended): tbtString;
begin
  SetLength(Result, SizeOf(Extended));
  Extended((@Result[1])^) := D;
end;

function TransLongintToStr(D: Longint): tbtString;
begin
  SetLength(Result, SizeOf(Longint));
  Longint((@Result[1])^) := D;
end;

function TransCardinalToStr(D: Cardinal): tbtString;
begin
  SetLength(Result, SizeOf(Cardinal));
  Cardinal((@Result[1])^) := D;
end;

function TransWordToStr(D: Word): tbtString;
begin
  SetLength(Result, SizeOf(Word));
  Word((@Result[1])^) := D;
end;

function TransSmallIntToStr(D: SmallInt): tbtString;
begin
  SetLength(Result, SizeOf(SmallInt));
  SmallInt((@Result[1])^) := D;
end;

function TransByteToStr(D: Byte): tbtString;
begin
  SetLength(Result, SizeOf(Byte));
  Byte((@Result[1])^) := D;
end;

function TransShortIntToStr(D: ShortInt): tbtString;
begin
  SetLength(Result, SizeOf(ShortInt));
  ShortInt((@Result[1])^) := D;
end;

function TPSPascalCompiler.GetConstant(const Name: tbtString): TPSConstant;
var
  h, i: Longint;
  n: tbtString;

begin
  n := FastUppercase(name);
  h := MakeHash(n);
  for i := 0 to FConstants.Count -1 do
  begin
    result := TPSConstant(FConstants[i]);
    if (Result.NameHash = h) and (Result.Name = n) then exit;
  end;
  result := nil;
end;

{$IFDEF PS_USESSUPPORT}
function TPSPascalCompiler.IsInLocalUnitList(s: tbtstring): Boolean;
begin
  s:=FastUpperCase(s);
  if (s = '') or (s=FastUpperCase(fModule)) or (s='SYSTEM') then
  begin
    result:=true;
    exit;
  end;
  result:=fUnit.HasUses(S);
end;
{$ENDIF}

{ TPSType }

constructor TPSType.Create;
begin
  inherited Create;
  FAttributes := TPSAttributes.Create;
  FFinalTypeNo := InvalidVal;
end;

destructor TPSType.Destroy;
begin
  FAttributes.Free;
  inherited Destroy;
end;

procedure TPSType.SetName(const Value: tbtString);
begin
  FName := Value;
  FNameHash := MakeHash(Value);
end;

procedure TPSType.Use;
begin
  FUsed := True;
end;

{ TPSRecordType }

function TPSRecordType.AddRecVal: PIFPSRecordFieldTypeDef;
begin
  Result := TPSRecordFieldTypeDef.Create;
  FRecordSubVals.Add(Result);
end;

constructor TPSRecordType.Create;
begin
  inherited Create;
  FRecordSubVals := TPSList.Create;
end;

destructor TPSRecordType.Destroy;
var
  i: Longint;
begin
  for i := FRecordSubVals.Count -1 downto 0 do
    TPSRecordFieldTypeDef(FRecordSubVals[I]).Free;
  FRecordSubVals.Free;
  inherited Destroy;
end;

function TPSRecordType.RecVal(I: Longint): PIFPSRecordFieldTypeDef;
begin
  Result := FRecordSubVals[I]
end;

function TPSRecordType.RecValCount: Longint;
begin
  Result := FRecordSubVals.Count;
end;


{ TPSRegProc }

constructor TPSRegProc.Create;
begin
  inherited Create;
  FDecl := TPSParametersDecl.Create;
end;

destructor TPSRegProc.Destroy;
begin
  FDecl.Free;
  inherited Destroy;
end;

procedure TPSRegProc.SetName(const Value: tbtString);
begin
  FName := Value;
  FNameHash := MakeHash(FName);
end;

{ TPSRecordFieldTypeDef }

procedure TPSRecordFieldTypeDef.SetFieldOrgName(const Value: tbtString);
begin
  FFieldOrgName := Value;
  FFieldName := FastUppercase(Value);
  FFieldNameHash := MakeHash(FFieldName);
end;

{ TPSProcVar }

procedure TPSProcVar.SetName(const Value: tbtString);
begin
  FName := Value;
  FNameHash := MakeHash(FName);
end;

procedure TPSProcVar.Use;
begin
  FUsed := True;
end;



{ TPSInternalProcedure }

constructor TPSInternalProcedure.Create;
begin
  inherited Create;
  FProcVars := TPSList.Create;
  FLabels := TIfStringList.Create;
  FGotos := TIfStringList.Create;
  FDecl := TPSParametersDecl.Create;
end;

destructor TPSInternalProcedure.Destroy;
var
  i: Longint;
begin
  FDecl.Free;
  for i := FProcVars.Count -1 downto 0 do
    TPSProcVar(FProcVars[I]).Free;
  FProcVars.Free;
  FGotos.Free;
  FLabels.Free;
  inherited Destroy;
end;

procedure TPSInternalProcedure.ResultUse;
begin
  FResultUsed := True;
end;

procedure TPSInternalProcedure.SetName(const Value: tbtString);
begin
  FName := Value;
  FNameHash := MakeHash(FName);
end;

procedure TPSInternalProcedure.Use;
begin
  FUsed := True;
end;

{ TPSProcedure }
constructor TPSProcedure.Create;
begin
  inherited Create;
  FAttributes := TPSAttributes.Create;
end;

destructor TPSProcedure.Destroy;
begin
  FAttributes.Free;
  inherited Destroy;
end;

{ TPSVar }

procedure TPSVar.SetName(const Value: tbtString);
begin
  FName := Value;
  FNameHash := MakeHash(Value);
end;

procedure TPSVar.Use;
begin
  FUsed := True;
end;

{ TPSConstant }

destructor TPSConstant.Destroy;
begin
  DisposeVariant(Value);
  inherited Destroy;
end;

procedure TPSConstant.SetChar(c: tbtChar);
begin
  if (FValue <> nil) then
  begin
    case FValue.FType.BaseType of
      btChar: FValue.tchar := c;
      btString: tbtString(FValue.tstring) := c;
      {$IFNDEF PS_NOWIDESTRING}
      btWideString: tbtwidestring(FValue.twidestring) := tbtWidestring(c);
      btUnicodeString: tbtUnicodestring(FValue.twidestring) := tbtUnicodestring(c);
      {$ENDIF}
    else
      raise EPSCompilerException.Create(RPS_ConstantValueMismatch);
    end;
  end else
    raise EPSCompilerException.Create(RPS_ConstantValueNotAssigned)
end;

procedure TPSConstant.SetExtended(const Val: Extended);
begin
  if (FValue <> nil) then
  begin
    case FValue.FType.BaseType of
      btSingle: FValue.tsingle := Val;
      btDouble: FValue.tdouble := Val;
      btExtended: FValue.textended := Val;
      btCurrency: FValue.tcurrency := Val;
    else
      raise EPSCompilerException.Create(RPS_ConstantValueMismatch);
    end;
  end else
    raise EPSCompilerException.Create(RPS_ConstantValueNotAssigned)
end;

procedure TPSConstant.SetInt(const Val: Longint);
begin
  if (FValue <> nil) then
  begin
    case FValue.FType.BaseType of
      btEnum: FValue.tu32 := Val;
      btU32, btS32: FValue.ts32 := Val;
      btU16, btS16: FValue.ts16 := Val;
      btU8, btS8: FValue.ts8 := Val;
      btSingle: FValue.tsingle := Val;
      btDouble: FValue.tdouble := Val;
      btExtended: FValue.textended := Val;
      btCurrency: FValue.tcurrency := Val;
      {$IFNDEF PS_NOINT64}
      bts64: FValue.ts64 := Val;
      {$ENDIF}
    else
      raise EPSCompilerException.Create(RPS_ConstantValueMismatch);
    end;
  end else
    raise EPSCompilerException.Create(RPS_ConstantValueNotAssigned)
end;
{$IFNDEF PS_NOINT64}
procedure TPSConstant.SetInt64(const Val: Int64);
begin
  if (FValue <> nil) then
  begin
    case FValue.FType.BaseType of
      btEnum: FValue.tu32 := Val;
      btU32, btS32: FValue.ts32 := Val;
      btU16, btS16: FValue.ts16 := Val;
      btU8, btS8: FValue.ts8 := Val;
      btSingle: FValue.tsingle := Val;
      btDouble: FValue.tdouble := Val;
      btExtended: FValue.textended := Val;
      btCurrency: FValue.tcurrency := Val;
      bts64: FValue.ts64 := Val;
    else
      raise EPSCompilerException.Create(RPS_ConstantValueMismatch);
    end;
  end else
    raise EPSCompilerException.Create(RPS_ConstantValueNotAssigned)
end;
{$ENDIF}
procedure TPSConstant.SetName(const Value: tbtString);
begin
  FName := Value;
  FNameHash := MakeHash(Value);
end;


procedure TPSConstant.SetSet(const val);
begin
  if (FValue <> nil) then
  begin
    case FValue.FType.BaseType of
      btSet:
        begin
          if length(tbtstring(FValue.tstring)) <> TPSSetType(FValue.FType).ByteSize then
            SetLength(tbtstring(FValue.tstring), TPSSetType(FValue.FType).ByteSize);
          Move(Val, FValue.tstring^, TPSSetType(FValue.FType).ByteSize);
        end;
    else
      raise EPSCompilerException.Create(RPS_ConstantValueMismatch);
    end;
  end else
    raise EPSCompilerException.Create(RPS_ConstantValueNotAssigned)
end;

procedure TPSConstant.SetString(const Val: tbtString);
begin
  if (FValue <> nil) then
  begin
    case FValue.FType.BaseType of
      btChar: FValue.tchar := (Val+#0)[1];
      btString: tbtString(FValue.tstring) := val;
      {$IFNDEF PS_NOWIDESTRING}
      btWideChar: FValue.twidechar := WideChar((Val+#0)[1]);
      btWideString: tbtwidestring(FValue.twidestring) := tbtwidestring(val);
      btUnicodeString: tbtunicodestring(FValue.tunistring) := tbtunicodestring(val);
      {$ENDIF}
    else
      raise EPSCompilerException.Create(RPS_ConstantValueMismatch);
    end;
  end else
    raise EPSCompilerException.Create(RPS_ConstantValueNotAssigned)
end;

procedure TPSConstant.SetUInt(const Val: Cardinal);
begin
  if (FValue <> nil) then
  begin
    case FValue.FType.BaseType of
      btEnum: FValue.tu32 := Val;
      btU32, btS32: FValue.tu32 := Val;
      btU16, btS16: FValue.tu16 := Val;
      btU8, btS8: FValue.tu8 := Val;
      btSingle: FValue.tsingle := Val;
      btDouble: FValue.tdouble := Val;
      btExtended: FValue.textended := Val;
      btCurrency: FValue.tcurrency := Val;
      {$IFNDEF PS_NOINT64}
      bts64: FValue.ts64 := Val;
      {$ENDIF}
    else
      raise EPSCompilerException.Create(RPS_ConstantValueMismatch);
    end;
  end else
    raise EPSCompilerException.Create(RPS_ConstantValueNotAssigned)
end;

{$IFNDEF PS_NOWIDESTRING}
procedure TPSConstant.SetWideChar(const val: WideChar);
begin
  if (FValue <> nil) then
  begin
    case FValue.FType.BaseType of
      btString: tbtString(FValue.tstring) := tbtstring(val);
      btWideChar: FValue.twidechar := val;
      btWideString: tbtwidestring(FValue.twidestring) := val;
      btUnicodeString: tbtUnicodestring(FValue.tUniString) := val;
    else
      raise EPSCompilerException.Create(RPS_ConstantValueMismatch);
    end;
  end else
    raise EPSCompilerException.Create(RPS_ConstantValueNotAssigned)
end;

procedure TPSConstant.SetWideString(const val: tbtwidestring);
begin
  if (FValue <> nil) then
  begin
    case FValue.FType.BaseType of
      btString: tbtString(FValue.tstring) := tbtstring(val);
      btWideString: tbtwidestring(FValue.twidestring) := val;
      btUnicodeString: tbtunicodestring(FValue.tunistring) := val;
    else
      raise EPSCompilerException.Create(RPS_ConstantValueMismatch);
    end;
  end else
    raise EPSCompilerException.Create(RPS_ConstantValueNotAssigned)
end;
procedure TPSConstant.SetUnicodeString(const val: tbtunicodestring);
begin
  if (FValue <> nil) then
  begin
    case FValue.FType.BaseType of
      btString: tbtString(FValue.tstring) := tbtstring(val);
      btWideString: tbtwidestring(FValue.twidestring) := val;
      btUnicodeString: tbtunicodestring(FValue.tunistring) := val;
    else
      raise EPSCompilerException.Create(RPS_ConstantValueMismatch);
    end;
  end else
    raise EPSCompilerException.Create(RPS_ConstantValueNotAssigned)
end;
{$ENDIF}
{ TPSPascalCompilerError }

function TPSPascalCompilerError.ErrorType: tbtString;
begin
  Result := tbtstring(RPS_Error);
end;

function TPSPascalCompilerError.ShortMessageToString: tbtString;
begin
  case Error of
    ecUnknownIdentifier: Result := tbtstring(Format (RPS_UnknownIdentifier, [Param]));
    ecIdentifierExpected: Result := tbtstring(RPS_IdentifierExpected);
    ecCommentError: Result := tbtstring(RPS_CommentError);
    ecStringError: Result := tbtstring(RPS_StringError);
    ecCharError: Result := tbtstring(RPS_CharError);
    ecSyntaxError: Result := tbtstring(RPS_SyntaxError);
    ecUnexpectedEndOfFile: Result := tbtstring(RPS_EOF);
    ecSemicolonExpected: Result := tbtstring(RPS_SemiColonExpected);
    ecBeginExpected: Result := tbtstring(RPS_BeginExpected);
    ecPeriodExpected: Result := tbtstring(RPS_PeriodExpected);
    ecDuplicateIdentifier: Result := tbtstring(Format (RPS_DuplicateIdent, [Param]));
    ecColonExpected: Result := tbtstring(RPS_ColonExpected);
    ecUnknownType: Result := tbtstring(Format (RPS_UnknownType, [Param]));
    ecCloseRoundExpected: Result := tbtstring(RPS_CloseRoundExpected);
    ecTypeMismatch: Result := tbtstring(RPS_TypeMismatch);
    ecInternalError: Result := tbtstring(Format (RPS_InternalError, [Param]));
    ecAssignmentExpected: Result := tbtstring(RPS_AssignmentExpected);
    ecThenExpected: Result := tbtstring(RPS_ThenExpected);
    ecDoExpected: Result := tbtstring(RPS_DoExpected);
    ecNoResult: Result := tbtstring(RPS_NoResult);
    ecOpenRoundExpected: Result := tbtstring(RPS_OpenRoundExpected);
    ecCommaExpected: Result := tbtstring(RPS_CommaExpected);
    ecToExpected: Result := tbtstring(RPS_ToExpected);
    ecIsExpected: Result := tbtstring(RPS_IsExpected);
    ecOfExpected: Result := tbtstring(RPS_OfExpected);
    ecCloseBlockExpected: Result := tbtstring(RPS_CloseBlockExpected);
    ecVariableExpected: Result := tbtstring(RPS_VariableExpected);
    ecStringExpected: result := tbtstring(RPS_StringExpected);
    ecEndExpected: Result := tbtstring(RPS_EndExpected);
    ecUnSetLabel: Result := tbtstring(Format (RPS_UnSetLabel, [Param]));
    ecNotInLoop: Result := tbtstring(RPS_NotInLoop);
    ecInvalidJump: Result := tbtstring(RPS_InvalidJump);
    ecOpenBlockExpected: Result := tbtstring(RPS_OpenBlockExpected);
    ecWriteOnlyProperty: Result := tbtstring(RPS_WriteOnlyProperty);
    ecReadOnlyProperty: Result := tbtstring(RPS_ReadOnlyProperty);
    ecClassTypeExpected: Result := tbtstring(RPS_ClassTypeExpected);
    ecCustomError: Result := Param;
    ecDivideByZero: Result := tbtstring(RPS_DivideByZero);
    ecMathError: Result := tbtstring(RPS_MathError);
    ecUnsatisfiedForward: Result := tbtstring(Format (RPS_UnsatisfiedForward, [Param]));
    ecForwardParameterMismatch: Result := tbtstring(RPS_ForwardParameterMismatch);
    ecInvalidnumberOfParameters: Result := tbtstring(RPS_InvalidNumberOfParameter);
    {$IFDEF PS_USESSUPPORT}
    ecNotAllowed : Result:=tbtstring(Format(RPS_NotAllowed,[Param]));
    ecUnitNotFoundOrContainsErrors: Result:=tbtstring(Format(RPS_UnitNotFound,[Param]));
    ecCrossReference: Result:=tbtstring(Format(RPS_CrossReference,[Param]));
    {$ENDIF}
    ecUnClosedAttributes: Result:=tbtstring(RPS_UnClosedAttributes);
  else
    Result := tbtstring(RPS_UnknownError);
  end;
  Result := Result;
end;


{ TPSPascalCompilerHint }

function TPSPascalCompilerHint.ErrorType: tbtString;
begin
  Result := tbtstring(RPS_Hint);
end;

function TPSPascalCompilerHint.ShortMessageToString: tbtString;
begin
  case Hint of
    ehVariableNotUsed: Result := tbtstring(Format (RPS_VariableNotUsed, [Param]));
    ehFunctionNotUsed: Result := tbtstring(Format (RPS_FunctionNotUsed, [Param]));
    ehCustomHint: Result := Param;
  else
    Result := tbtstring(RPS_UnknownHint);
  end;
end;

{ TPSPascalCompilerWarning }

function TPSPascalCompilerWarning.ErrorType: tbtString;
begin
  Result := tbtstring(RPS_Warning);
end;

function TPSPascalCompilerWarning.ShortMessageToString: tbtString;
begin
  case Warning of
    ewCustomWarning: Result := Param;
    ewCalculationAlwaysEvaluatesTo: Result := tbtstring(Format (RPS_CalculationAlwaysEvaluatesTo, [Param]));
    ewIsNotNeeded: Result := tbtstring(Format (RPS_IsNotNeeded, [Param]));
    ewAbstractClass: Result := tbtstring(RPS_AbstractClass);
  else
    Result := tbtstring(RPS_UnknownWarning);
  end;
end;

{ TPSPascalCompilerMessage }

function TPSPascalCompilerMessage.MessageToString: tbtString;
begin
  Result := '['+ErrorType+'] '+FModuleName+'('+IntToStr(FRow)+':'+IntToStr(FCol)+'): '+ShortMessageToString;
end;

procedure TPSPascalCompilerMessage.SetParserPos(Parser: TPSPascalParser);
begin
  FPosition := Parser.CurrTokenPos;
  FRow := Parser.Row;
  FCol := Parser.Col;
end;

procedure TPSPascalCompilerMessage.SetCustomPos(Pos, Row, Col: Cardinal);
begin
  FPosition := Pos;
  FRow := Row;
  FCol := Col;
end;

{ TUnConstOperation }

destructor TUnConstOperation.Destroy;
begin
  FVal1.Free;
  inherited Destroy;
end;


{ TBinConstOperation }

destructor TBinConstOperation.Destroy;
begin
  FVal1.Free;
  FVal2.Free;
  inherited Destroy;
end;

{ TConstData }

destructor TConstData.Destroy;
begin
  DisposeVariant(FData);
  inherited Destroy;
end;


{ TConstOperation }

procedure TConstOperation.SetPos(Parser: TPSPascalParser);
begin
  FDeclPosition := Parser.CurrTokenPos;
  FDeclRow := Parser.Row;
  FDeclCol := Parser.Col;
end;

{ TPSValue }

procedure TPSValue.SetParserPos(P: TPSPascalParser);
begin
  FPos := P.CurrTokenPos;
  FRow := P.Row;
  FCol := P.Col;
end;

{ TPSValueData }

destructor TPSValueData.Destroy;
begin
  DisposeVariant(FData);
  inherited Destroy;
end;


{ TPSValueReplace }

constructor TPSValueReplace.Create;
begin
  FFreeNewValue := True;
  FReplaceTimes := 1;
end;

destructor TPSValueReplace.Destroy;
begin
  if FFreeOldValue then
    FOldValue.Free;
  if FFreeNewValue then
    FNewValue.Free;
  inherited Destroy;
end;



{ TPSUnValueOp }

destructor TPSUnValueOp.Destroy;
begin
  FVal1.Free;
  inherited Destroy;
end;

{ TPSBinValueOp }

destructor TPSBinValueOp.Destroy;
begin
  FVal1.Free;
  FVal2.Free;
  inherited Destroy;
end;




{ TPSSubValue }

destructor TPSSubValue.Destroy;
begin
  FSubNo.Free;
  inherited Destroy;
end;

{ TPSValueVar }

constructor TPSValueVar.Create;
begin
  inherited Create;
  FRecItems := TPSList.Create;
end;

destructor TPSValueVar.Destroy;
var
  i: Longint;
begin
  for i := 0 to FRecItems.Count -1 do
  begin
    TPSSubItem(FRecItems[I]).Free;
  end;
  FRecItems.Free;
  inherited Destroy;
end;

function TPSValueVar.GetRecCount: Cardinal;
begin
  Result := FRecItems.Count;
end;

function TPSValueVar.GetRecItem(I: Cardinal): TPSSubItem;
begin
  Result := FRecItems[I];
end;

function TPSValueVar.RecAdd(Val: TPSSubItem): Cardinal;
begin
  Result := FRecItems.Add(Val);
end;

procedure TPSValueVar.RecDelete(I: Cardinal);
var
  rr :TPSSubItem;
begin
  rr := FRecItems[i];
  FRecItems.Delete(I);
  rr.Free;
end;

{ TPSValueProc }

destructor TPSValueProc.Destroy;
begin
  FSelfPtr.Free;
  FParameters.Free;
end;
{ TPSParameter }

destructor TPSParameter.Destroy;
begin
  FTempVar.Free;
  FValue.Free;
  inherited Destroy;
end;


  { TPSParameters }

function TPSParameters.Add: TPSParameter;
begin
  Result := TPSParameter.Create;
  FItems.Add(Result);
end;

constructor TPSParameters.Create;
begin
  inherited Create;
  FItems := TPSList.Create;
end;

procedure TPSParameters.Delete(I: Cardinal);
var
  p: TPSParameter;
begin
  p := FItems[I];
  FItems.Delete(i);
  p.Free;
end;

destructor TPSParameters.Destroy;
var
  i: Longint;
begin
  for i := FItems.Count -1 downto 0 do
  begin
    TPSParameter(FItems[I]).Free;
  end;
  FItems.Free;
  inherited Destroy;
end;

function TPSParameters.GetCount: Cardinal;
begin
  Result := FItems.Count;
end;

function TPSParameters.GetItem(I: Longint): TPSParameter;
begin
  Result := FItems[I];
end;


{ TPSValueArray }

function TPSValueArray.Add(Item: TPSValue): Cardinal;
begin
  Result := FItems.Add(Item);
end;

constructor TPSValueArray.Create;
begin
  inherited Create;
  FItems := TPSList.Create;
end;

procedure TPSValueArray.Delete(I: Cardinal);
begin
  FItems.Delete(i);
end;

destructor TPSValueArray.Destroy;
var
  i: Longint;
begin
  for i := FItems.Count -1 downto 0 do
    TPSValue(FItems[I]).Free;
  FItems.Free;

  inherited Destroy;
end;

function TPSValueArray.GetCount: Cardinal;
begin
  Result := FItems.Count;
end;

function TPSValueArray.GetItem(I: Cardinal): TPSValue;
begin
  Result := FItems[I];
end;


{ TPSValueAllocatedStackVar }

destructor TPSValueAllocatedStackVar.Destroy;
var
  pv: TPSProcVar;
begin
  {$IFDEF DEBUG}
  if Cardinal(LocalVarNo +1) <> proc.ProcVars.Count then
  begin
    Abort;
    exit;
  end;
  {$ENDIF}
  if Proc <> nil then
  begin
    pv := Proc.ProcVars[Proc.ProcVars.Count -1];
    Proc.ProcVars.Delete(Proc.ProcVars.Count -1);
    pv.Free;
    Proc.Data := Proc.Data + tbtChar(CM_PO);
  end;
  inherited Destroy;
end;




function AddImportedClassVariable(Sender: TPSPascalCompiler; const VarName, VarType: tbtString): Boolean;
var
  P: TPSVar;
begin
  P := Sender.AddVariableN(VarName, VarType);
  if p = nil then
  begin
    Result := False;
    Exit;
  end;
  SetVarExportName(P, FastUppercase(VarName));
  p.Use;
  Result := True;
end;


{'class:'+CLASSNAME+'|'+FUNCNAME+'|'+chr(CallingConv)+chr(hasresult)+params

For property write functions there is an '@' after the funcname.
}

const
  ProcHDR = 'procedure a;';



{ TPSCompileTimeClass }

function TPSCompileTimeClass.CastToType(IntoType: TPSType;
  var ProcNo: Cardinal): Boolean;
var
  P: TPSExternalProcedure;
begin
  if (IntoType <> nil) and (IntoType.BaseType <> btClass) and (IntoType.BaseType <> btInterface) then
  begin
    Result := False;
    exit;
  end;
  if FCastProc <> InvalidVal then
  begin
    Procno := FCastProc;
    Result := True;
    exit;
  end;
  ProcNo := FOwner. AddUsedFunction2(P);
  P.RegProc := FOwner.AddFunction(ProcHDR);
  P.RegProc.Name := '';

  with P.RegProc.Decl.AddParam do
  begin
    OrgName := 'Org';
    aType := Self.FType;
  end;
  with P.RegProc.Decl.AddParam do
  begin
    OrgName := 'TypeNo';
    aType := FOwner.at2ut(FOwner.FindBaseType(btU32));
  end;
  P.RegProc.Decl.Result := IntoType;
  P.RegProc.ImportDecl := 'class:+';
  FCastProc := ProcNo;
  Result := True;
end;


function TPSCompileTimeClass.ClassFunc_Call(Index: IPointer;
  var ProcNo: Cardinal): Boolean;
var
  C: TPSDelphiClassItemConstructor;
  P: TPSExternalProcedure;
  s: tbtString;
  i: Longint;

begin
  if FIsAbstract then
    FOwner.MakeWarning('', ewAbstractClass, '');
  C := Pointer(Index);
  if c.MethodNo = InvalidVal then
  begin
    ProcNo := FOwner.AddUsedFunction2(P);
    P.RegProc := FOwner.AddFunction(ProcHDR);
    P.RegProc.Name := '';
    P.RegProc.Decl.Assign(c.Decl);
    s := 'class:' + C.Owner.FClassName + '|' + C.Name + '|'+ chr(0);
    if c.Decl.Result = nil then
      s := s + #0
    else
      s := s + #1;
    for i := 0 to C.Decl.ParamCount -1 do
    begin
      if c.Decl.Params[i].Mode <> pmIn then
        s := s + #1
      else
        s := s + #0;
    end;
    P.RegProc.ImportDecl := s;
    C.MethodNo := ProcNo;
  end else begin
     ProcNo := c.MethodNo;
  end;
  Result := True;
end;

function TPSCompileTimeClass.ClassFunc_Find(const Name: tbtString;
  var Index: IPointer): Boolean;
var
  H: Longint;
  I: Longint;
  CurrClass: TPSCompileTimeClass;
  C: TPSDelphiClassItem;
begin
  H := MakeHash(Name);
  CurrClass := Self;
  while CurrClass <> nil do
  begin
    for i := CurrClass.FClassItems.Count -1 downto 0 do
    begin
      C := CurrClass.FClassItems[I];
      if (c is TPSDelphiClassItemConstructor) and (C.NameHash = H) and (C.Name = Name) then
      begin
        Index := IPointer(C);
        Result := True;
        exit;
      end;
    end;
    CurrClass := CurrClass.FInheritsFrom;
  end;
  Result := False;
end;


class function TPSCompileTimeClass.CreateC(FClass: TClass; aOwner: TPSPascalCompiler; aType: TPSType): TPSCompileTimeClass;
begin
  Result := TPSCompileTimeClass.Create(FastUpperCase(tbtstring(FClass.ClassName)), aOwner, aType);
  Result.FClass := FClass;
end;

constructor TPSCompileTimeClass.Create(ClassName: tbtString; aOwner: TPSPascalCompiler; aType: TPSType);
begin
  inherited Create;
  FType := aType;
  FCastProc := InvalidVal;
  FNilProc := InvalidVal;

  FDefaultProperty := InvalidVal;
  FClassName := Classname;
  FClassNameHash := MakeHash(FClassName);
  FClassItems := TPSList.Create;
  FOwner := aOwner;
end;

destructor TPSCompileTimeClass.Destroy;
var
  I: Longint;
begin
  for i := FClassItems.Count -1 downto 0 do
    TPSDelphiClassItem(FClassItems[I]).Free;
  FClassItems.Free;
  inherited Destroy;
end;


function TPSCompileTimeClass.Func_Call(Index: TPSDelphiClassItem;
  var ProcNo: Cardinal): Boolean;
var
  C: TPSDelphiClassItemMethod;
  P: TPSExternalProcedure;
  i: Longint;
  s: tbtString;

begin
  C := Index as TPSDelphiClassItemMethod;
  if c.MethodNo = InvalidVal then
  begin
    ProcNo := FOwner.AddUsedFunction2(P);
    P.RegProc := FOwner.AddFunction(ProcHDR);
    P.RegProc.Name := '';
    p.RegProc.Decl.Assign(c.Decl);
    s := 'class:' + C.Owner.FClassName + '|' + C.Name + '|'+ chr(0);
    if c.Decl.Result = nil then
      s := s + #0
    else
      s := s + #1;
    for i := 0 to c.Decl.ParamCount -1 do
    begin
      if c.Decl.Params[i].Mode <> pmIn then
        s := s + #1
      else
        s := s + #0;
    end;
    P.RegProc.ImportDecl := s;
    C.MethodNo := ProcNo;
  end else begin
     ProcNo := c.MethodNo;
  end;
  Result := True;
end;

function TPSCompileTimeClass.Func_Find(const Name: tbtString;
  var Index: TPSDelphiClassItem): Boolean;
var
  H: Longint;
  I: Longint;
  CurrClass: TPSCompileTimeClass;
  C: TPSDelphiClassItem;
begin
  H := MakeHash(Name);
  CurrClass := Self;
  while CurrClass <> nil do
  begin
    for i := CurrClass.FClassItems.Count -1 downto 0 do
    begin
      C := CurrClass.FClassItems[I];
      if (c is TPSDelphiClassItemMethod) and (C.NameHash = H) and (C.Name = Name) then
      begin
        Index := C;
        Result := True;
        exit;
      end;
    end;
    CurrClass := CurrClass.FInheritsFrom;
  end;
  Result := False;
end;

function TPSCompileTimeClass.GetCount: Longint;
begin
  Result := FClassItems.Count;
end;

function TPSCompileTimeClass.GetItem(i: Longint): TPSDelphiClassItem;
begin
  Result := FClassItems[i];
end;

function TPSCompileTimeClass.IsCompatibleWith(aType: TPSType): Boolean;
var
  Temp: TPSCompileTimeClass;
begin
  if (atype.BaseType <> btClass) then
  begin
    Result := False;
    exit;
  end;
  temp := TPSClassType(aType).Cl;
  while Temp <> nil do
  begin
    if Temp = Self then
    begin
      Result := True;
      exit;
    end;
    Temp := Temp.FInheritsFrom;
  end;
  Result := False;
end;

function TPSCompileTimeClass.Property_Find(const Name: tbtString;
  var Index: TPSDelphiClassItem): Boolean;
var
  H: Longint;
  I: Longint;
  CurrClass: TPSCompileTimeClass;
  C: TPSDelphiClassItem;
begin
  if Name = '' then
  begin
    CurrClass := Self;
    while CurrClass <> nil do
    begin
      if CurrClass.FDefaultProperty <> InvalidVal then
      begin
        Index := TPSDelphiClassItem(CurrClass.FClassItems[Currclass.FDefaultProperty]);
        result := True;
        exit;
      end;
      CurrClass := CurrClass.FInheritsFrom;
    end;
    Result := False;
    exit;
  end;
  H := MakeHash(Name);
  CurrClass := Self;
  while CurrClass <> nil do
  begin
    for i := CurrClass.FClassItems.Count -1 downto 0 do
    begin
      C := CurrClass.FClassItems[I];
      if (c is TPSDelphiClassItemProperty) and (C.NameHash = H) and (C.Name = Name) then
      begin
        Index := C;
        Result := True;
        exit;
      end;
    end;
    CurrClass := CurrClass.FInheritsFrom;
  end;
  Result := False;
end;

function TPSCompileTimeClass.Property_Get(Index: TPSDelphiClassItem;
  var ProcNo: Cardinal): Boolean;
var
  C: TPSDelphiClassItemProperty;
  P: TPSExternalProcedure;
  s: tbtString;

begin
  C := Index as TPSDelphiClassItemProperty;
  if c.AccessType = iptW then
  begin
    Result := False;
    exit;
  end;
  if c.ReadProcNo = InvalidVal then
  begin
    ProcNo := FOwner.AddUsedFunction2(P);
    P.RegProc := FOwner.AddFunction(ProcHDR);
    P.RegProc.Name := '';
    P.RegProc.Decl.Result := C.Decl.Result;
    s := 'class:' + C.Owner.FClassName + '|' + C.Name + '|'+#0#0#0#0;
    Longint((@(s[length(s)-3]))^) := c.Decl.ParamCount +1;
    P.RegProc.ImportDecl := s;
    C.ReadProcNo := ProcNo;
  end else begin
     ProcNo := c.ReadProcNo;
  end;
  Result := True;
end;

function TPSCompileTimeClass.Property_GetHeader(Index: TPSDelphiClassItem;
  Dest: TPSParametersDecl): Boolean;
var
  c: TPSDelphiClassItemProperty;
begin
  C := Index as TPSDelphiClassItemProperty;
  FOwner.UseProc(c.Decl);
  Dest.Assign(c.Decl);
  Result := True;
end;

function TPSCompileTimeClass.Property_Set(Index: TPSDelphiClassItem;
  var ProcNo: Cardinal): Boolean;
var
  C: TPSDelphiClassItemProperty;
  P: TPSExternalProcedure;
  s: tbtString;

begin
  C := Index as TPSDelphiClassItemProperty;
  if c.AccessType = iptR then
  begin
    Result := False;
    exit;
  end;
  if c.WriteProcNo = InvalidVal then
  begin
    ProcNo := FOwner.AddUsedFunction2(P);
    P.RegProc := FOwner.AddFunction(ProcHDR);
    P.RegProc.Name := '';
    s := 'class:' + C.Owner.FClassName + '|' + C.Name + '@|'#0#0#0#0;
    Longint((@(s[length(s)-3]))^) := C.Decl.ParamCount+1;
    P.RegProc.ImportDecl := s;
    C.WriteProcNo := ProcNo;
  end else begin
     ProcNo := c.WriteProcNo;
  end;
  Result := True;
end;

function TPSCompileTimeClass.RegisterMethod(const Decl: tbtString): Boolean;
var
  DOrgName: tbtString;
  DDecl: TPSParametersDecl;
  FT: TPMFuncType;
  p: TPSDelphiClassItemMethod;
begin
  DDecl := TPSParametersDecl.Create;
  try
    if not ParseMethod(FOwner, FClassName, Decl, DOrgName, DDecl, FT) then
    begin
      Result := False;
      {$IFDEF DEBUG} raise EPSCompilerException.CreateFmt(RPS_UnableToRegister, [Decl]); {$ENDIF}
      exit;
    end;
    if ft = mftConstructor then
      p := TPSDelphiClassItemConstructor.Create(Self)
    else
      p := TPSDelphiClassItemMethod.Create(self);
    p.OrgName := DOrgName;
    p.Decl.Assign(DDecl);
    p.MethodNo := InvalidVal;
    FClassItems.Add(p);
    Result := True;
  finally
    DDecl.Free;
  end;
end;

procedure TPSCompileTimeClass.RegisterProperty(const PropertyName,
  PropertyType: tbtString; PropAC: TPSPropType);
var
  FType: TPSType;
  Param: TPSParameterDecl;
  p: TPSDelphiClassItemProperty;
  PT: tbtString;
begin
  pt := PropertyType;
  p := TPSDelphiClassItemProperty.Create(Self);
  p.AccessType := PropAC;
  p.ReadProcNo := InvalidVal;
  p.WriteProcNo := InvalidVal;
  p.OrgName := PropertyName;
  repeat
    FType := FOwner.FindType(FastUpperCase(grfw(pt)));
    if FType = nil then
    begin
      p.Free;
      Exit;
    end;
    if p.Decl.Result = nil  then p.Decl.Result := FType else
    begin
      param := p.Decl.AddParam;
      Param.OrgName := 'param'+IntToStr(p.Decl.ParamCount);
      Param.aType := FType;
    end;
  until pt = '';
  FClassItems.Add(p);
end;


procedure TPSCompileTimeClass.RegisterPublishedProperties;
var
  p: PPropList;
  i, Count: Longint;
  a: TPSPropType;
begin
  if (Fclass = nil) or (Fclass.ClassInfo = nil) then exit;
  Count := GetTypeData(fclass.ClassInfo)^.PropCount;
  GetMem(p, Count * SizeOf(Pointer));
  GetPropInfos(fclass.ClassInfo, p);
  for i := Count -1 downto 0 do
  begin
    if p^[i]^.PropType^.Kind in [tkLString, tkInteger, tkChar, tkEnumeration, tkFloat, tkString, tkSet, tkClass, tkMethod{$IFNDEF PS_NOWIDESTRING}, tkWString{$ENDIF}{$IFDEF DELPHI2009UP}, tkUString{$ENDIF}] then
    begin
      if (p^[i]^.GetProc <> nil) then
      begin
        if p^[i]^.SetProc = nil then
          a := iptr
        else
          a := iptrw;
      end else
      begin
        a := iptW;
        if p^[i]^.SetProc = nil then continue;
      end;
      RegisterProperty(p^[i]^.Name, p^[i]^.PropType^.Name, a);
    end;
  end;
  FreeMem(p);
end;

function TPSCompileTimeClass.RegisterPublishedProperty(const Name: tbtString): Boolean;
var
  p: PPropInfo;
  a: TPSPropType;
begin
  if (Fclass = nil) or (Fclass.ClassInfo = nil) then begin Result := False; exit; end;
  p := GetPropInfo(fclass.ClassInfo, string(Name));
  if p = nil then begin Result := False; exit; end;
  if (p^.GetProc <> nil) then
  begin
    if p^.SetProc = nil then
      a := iptr
    else
      a := iptrw;
  end else
  begin
    a := iptW;
    if p^.SetProc = nil then begin result := False; exit; end;
  end;
  RegisterProperty(p^.Name, p^.PropType^.Name, a);
  Result := True;
end;


procedure TPSCompileTimeClass.SetDefaultPropery(const Name: tbtString);
var
  i,h: Longint;
  p: TPSDelphiClassItem;
  s: tbtString;

begin
  s := FastUppercase(name);
  h := MakeHash(s);
  for i := FClassItems.Count -1 downto 0 do
  begin
    p := FClassItems[i];
    if (p.NameHash = h) and (p.Name = s) then
    begin
      if p is TPSDelphiClassItemProperty then
      begin
        if p.Decl.ParamCount = 0 then
          raise EPSCompilerException.CreateFmt(RPS_NotArrayProperty, [Name]);
        FDefaultProperty := I;
        exit;
      end else raise EPSCompilerException.CreateFmt(RPS_NotProperty, [Name]);
    end;
  end;
  raise EPSCompilerException.CreateFmt(RPS_UnknownProperty, [Name]);
end;

function TPSCompileTimeClass.SetNil(var ProcNo: Cardinal): Boolean;
var
  P: TPSExternalProcedure;

begin
  if FNilProc <> InvalidVal then
  begin
    Procno := FNilProc;
    Result := True;
    exit;
  end;
  ProcNo := FOwner.AddUsedFunction2(P);
  P.RegProc := FOwner.AddFunction(ProcHDR);
  P.RegProc.Name := '';
  with P.RegProc.Decl.AddParam do
  begin
    OrgName := 'VarNo';
    aType := FOwner.at2ut(FType);
  end;
  P.RegProc.ImportDecl := 'class:-';
  FNilProc := Procno;
  Result := True;
end;

{ TPSSetType }

function TPSSetType.GetBitSize: Longint;
begin
  case SetType.BaseType of
    btEnum: begin Result := TPSEnumType(setType).HighValue+1; end;
    btChar, btU8: Result := 256;
  else
    Result := 0;
  end;
end;

function TPSSetType.GetByteSize: Longint;
var
  r: Longint;
begin
  r := BitSize;
  if r mod 8 <> 0 then inc(r, 7);
   Result := r div 8;
end;


{ TPSBlockInfo }

procedure TPSBlockInfo.Clear;
var
  i: Longint;
begin
  for i := WithList.Count -1 downto 0 do
  begin
    TPSValue(WithList[i]).Free;
    WithList.Delete(i);
  end;
end;

constructor TPSBlockInfo.Create(Owner: TPSBlockInfo);
begin
  inherited Create;
  FOwner := Owner;
  FWithList := TPSList.Create;
  if FOwner <> nil then
  begin
    FProcNo := FOwner.ProcNo;
    FProc := FOwner.Proc;
  end;
end;

destructor TPSBlockInfo.Destroy;
begin
  Clear;
  FWithList.Free;
  inherited Destroy;
end;

{ TPSAttributeTypeField }
procedure TPSAttributeTypeField.SetFieldOrgName(const Value: tbtString);
begin
  FFieldOrgName := Value;
  FFieldName := FastUpperCase(Value);
  FFieldNameHash := MakeHash(FFieldName);
end;

constructor TPSAttributeTypeField.Create(AOwner: TPSAttributeType);
begin
  inherited Create;
  FOwner := AOwner;
end;

{ TPSAttributeType }

function TPSAttributeType.GetField(I: Longint): TPSAttributeTypeField;
begin
  Result := TPSAttributeTypeField(FFields[i]);
end;

function TPSAttributeType.GetFieldCount: Longint;
begin
  Result := FFields.Count;
end;

procedure TPSAttributeType.SetName(const s: tbtString);
begin
  FOrgname := s;
  FName := FastUppercase(s);
  FNameHash := MakeHash(FName);
end;

constructor TPSAttributeType.Create;
begin
  inherited Create;
  FFields := TPSList.Create;
end;

destructor TPSAttributeType.Destroy;
var
  i: Longint;
begin
  for i := FFields.Count -1 downto 0 do
  begin
    TPSAttributeTypeField(FFields[i]).Free;
  end;
  FFields.Free;
  inherited Destroy;
end;

function TPSAttributeType.AddField: TPSAttributeTypeField;
begin
  Result := TPSAttributeTypeField.Create(self);
  FFields.Add(Result);
end;

procedure TPSAttributeType.DeleteField(I: Longint);
var
  Fld: TPSAttributeTypeField;
begin
  Fld := FFields[i];
  FFields.Delete(i);
  Fld.Free;
end;

{ TPSAttribute }
function TPSAttribute.GetValueCount: Longint;
begin
  Result := FValues.Count;
end;

function TPSAttribute.GetValue(I: Longint): PIfRVariant;
begin
  Result := FValues[i];
end;

constructor TPSAttribute.Create(AttribType: TPSAttributeType);
begin
  inherited Create;
  FValues := TPSList.Create;
  FAttribType := AttribType;
end;

procedure TPSAttribute.DeleteValue(i: Longint);
var
  Val: PIfRVariant;
begin
  Val := FValues[i];
  FValues.Delete(i);
  DisposeVariant(Val);
end;

function TPSAttribute.AddValue(v: PIFRVariant): Longint;
begin
  Result := FValues.Add(v);
end;


destructor TPSAttribute.Destroy;
var
  i: Longint;
begin
  for i := FValues.Count -1 downto 0 do
  begin
    DisposeVariant(FValues[i]);
  end;
  FValues.Free;
  inherited Destroy;
end;


procedure TPSAttribute.Assign(Item: TPSAttribute);
var
  i: Longint;
  p: PIfRVariant;
begin
  for i := FValues.Count -1 downto 0 do
  begin
    DisposeVariant(FValues[i]);
  end;
  FValues.Clear;
  FAttribType := Item.FAttribType;
  for i := 0 to Item.FValues.Count -1 do
  begin
    p := DuplicateVariant(Item.FValues[i]);
    FValues.Add(p);
  end;
end;

{ TPSAttributes }

function TPSAttributes.GetCount: Longint;
begin
  Result := FItems.Count;
end;

function TPSAttributes.GetItem(I: Longint): TPSAttribute;
begin
  Result := TPSAttribute(FItems[i]);
end;

procedure TPSAttributes.Delete(i: Longint);
var
  item: TPSAttribute;
begin
  item := TPSAttribute(FItems[i]);
  FItems.Delete(i);
  Item.Free;
end;

function TPSAttributes.Add(AttribType: TPSAttributeType): TPSAttribute;
begin
  Result := TPSAttribute.Create(AttribType);
  FItems.Add(Result);
end;

constructor TPSAttributes.Create;
begin
  inherited Create;
  FItems := TPSList.Create;
end;

destructor TPSAttributes.Destroy;
var
  i: Longint;
begin
  for i := FItems.Count -1 downto 0 do
  begin
    TPSAttribute(FItems[i]).Free;
  end;
  FItems.Free;
  inherited Destroy;
end;

procedure TPSAttributes.Assign(attr: TPSAttributes; Move: Boolean);
var
  newitem, item: TPSAttribute;
  i: Longint;
begin
  for i := ATtr.FItems.Count -1 downto 0 do
  begin
    Item := Attr.Fitems[i];
    if Move then
    begin
      FItems.Add(Item);
      Attr.FItems.Delete(i);
    end else
    begin
      newitem := TPSAttribute.Create(Item.FAttribType );
      newitem.Assign(item);
      FItems.Add(NewItem);
    end;
  end;

end;


function TPSAttributes.FindAttribute(
  const Name: tbtString): TPSAttribute;
var
  h, i: Longint;

begin
  h := MakeHash(name);
  for i := FItems.Count -1 downto 0 do
  begin
    Result := FItems[i];
    if (Result.FAttribType.NameHash = h) and (Result.FAttribType.Name = Name) then
      exit;
  end;
  result := nil;
end;

{ TPSParameterDecl }
procedure TPSParameterDecl.SetName(const s: tbtString);
begin
  FOrgName := s;
  FName := FastUppercase(s);
end;


{ TPSParametersDecl }

procedure TPSParametersDecl.Assign(Params: TPSParametersDecl);
var
  i: Longint;
  np, orgp: TPSParameterDecl;
begin
  for i := FParams.Count -1 downto 0 do
  begin
    TPSParameterDecl(Fparams[i]).Free;
  end;
  FParams.Clear;
  FResult := Params.Result;

  for i := 0 to Params.FParams.count -1 do
  begin
    orgp := Params.FParams[i];
    np := AddParam;
    np.OrgName := orgp.OrgName;
    np.Mode := orgp.Mode;
    np.aType := orgp.aType;
    np.DeclarePos:=orgp.DeclarePos;
    np.DeclareRow:=orgp.DeclareRow;
    np.DeclareCol:=orgp.DeclareCol;
  end;
end;


function TPSParametersDecl.GetParam(I: Longint): TPSParameterDecl;
begin
  Result := FParams[i];
end;

function TPSParametersDecl.GetParamCount: Longint;
begin
  Result := FParams.Count;
end;

function TPSParametersDecl.AddParam: TPSParameterDecl;
begin
  Result := TPSParameterDecl.Create;
  FParams.Add(Result);
end;

procedure TPSParametersDecl.DeleteParam(I: Longint);
var
  param: TPSParameterDecl;
begin
  param := FParams[i];
  FParams.Delete(i);
  Param.Free;
end;

constructor TPSParametersDecl.Create;
begin
  inherited Create;
  FParams := TPSList.Create;
end;

destructor TPSParametersDecl.Destroy;
var
  i: Longint;
begin
  for i := FParams.Count -1 downto 0 do
  begin
    TPSParameterDecl(Fparams[i]).Free;
  end;
  FParams.Free;
  inherited Destroy;
end;

function TPSParametersDecl.Same(d: TPSParametersDecl): boolean;
var
  i: Longint;
begin
  if (d = nil) or (d.ParamCount <> ParamCount) or (d.Result <> Self.Result) then
    Result := False
  else begin
    for i := 0 to d.ParamCount -1 do
    begin
      if (d.Params[i].Mode <> Params[i].Mode) or (d.Params[i].aType <> Params[i].aType) then
      begin
        Result := False;
        exit;
      end;
    end;
    Result := True;
  end;
end;

{ TPSProceduralType }

constructor TPSProceduralType.Create;
begin
  inherited Create;
  FProcDef := TPSParametersDecl.Create;

end;

destructor TPSProceduralType.Destroy;
begin
  FProcDef.Free;
  inherited Destroy;
end;

{ TPSDelphiClassItem }

procedure TPSDelphiClassItem.SetName(const s: tbtString);
begin
  FOrgName := s;
  FName := FastUpperCase(s);
  FNameHash := MakeHash(FName);
end;

constructor TPSDelphiClassItem.Create(Owner: TPSCompileTimeClass);
begin
  inherited Create;
  FOwner := Owner;
  FDecl := TPSParametersDecl.Create;
end;

destructor TPSDelphiClassItem.Destroy;
begin
  FDecl.Free;
  inherited Destroy;
end;

{$IFNDEF PS_NOINTERFACES}
{ TPSInterface }

function TPSInterface.CastToType(IntoType: TPSType;
  var ProcNo: Cardinal): Boolean;
var
  P: TPSExternalProcedure;
begin
  if (IntoType <> nil) and (IntoType.BaseType <> btInterface) then
  begin
    Result := False;
    exit;
  end;
  if FCastProc <> InvalidVal then
  begin
    ProcNo := FCastProc;
    Result := True;
    exit;
  end;
  ProcNo := FOwner.AddUsedFunction2(P);
  P.RegProc := FOwner.AddFunction(ProcHDR);
  P.RegProc.Name := '';
  with P.RegProc.Decl.AddParam do
  begin
    OrgName := 'Org';
    aType := Self.FType;
  end;
  with P.RegProc.Decl.AddParam do
  begin
    OrgName := 'TypeNo';
    aType := FOwner.at2ut(FOwner.FindBaseType(btU32));
  end;
  P.RegProc.Decl.Result := FOwner.at2ut(IntoType);

  P.RegProc.ImportDecl := 'class:+';
  FCastProc := ProcNo;
  Result := True;
end;

constructor TPSInterface.Create(Owner: TPSPascalCompiler; InheritedFrom: TPSInterface; Guid: TGuid; const Name: tbtString; aType: TPSType);
begin
  inherited Create;
  FCastProc := InvalidVal;
  FNilProc := InvalidVal;

  FType := aType;
  FOWner := Owner;
  FGuid := GUID;
  Self.InheritedFrom := InheritedFrom;

  FItems := TPSList.Create;
  FName := Name;
  FNameHash := MakeHash(Name);
end;

procedure TPSInterface.SetInheritedFrom(p: TPSInterface);
begin
  FInheritedFrom := p;
end;

destructor TPSInterface.Destroy;
var
  i: Longint;
begin
  for i := FItems.Count -1 downto 0 do
  begin
    TPSInterfaceMethod(FItems[i]).Free;
  end;
  FItems.Free;
  inherited Destroy;
end;

function TPSInterface.Func_Call(Index: TPSInterfaceMethod;
  var ProcNo: Cardinal): Boolean;
var
  c: TPSInterfaceMethod;
  P: TPSExternalProcedure;
  s: tbtString;
  i: Longint;
begin
  c := TPSInterfaceMethod(Index);
  if c.FScriptProcNo <> InvalidVal then
  begin
    Procno := c.FScriptProcNo;
    Result := True;
    exit;
  end;
  ProcNo := FOwner.AddUsedFunction2(P);
  P.RegProc := FOwner.AddFunction(ProcHDR);
  P.RegProc.Name := '';
  FOwner.UseProc(C.Decl);
  P.RegProc.Decl.Assign(c.Decl);
  s := tbtstring('intf:.') + PS_mi2s(c.AbsoluteProcOffset) + tbtchar(ord(c.CC));
  if c.Decl.Result = nil then
    s := s + #0
  else
    s := s + #1;
  for i := 0 to C.Decl.ParamCount -1 do
  begin
    if c.Decl.Params[i].Mode <> pmIn then
      s := s + #1
    else
      s := s + #0;
  end;
  P.RegProc.ImportDecl := s;
  C.FScriptProcNo := ProcNo;
  Result := True;
end;

function TPSInterface.Func_Find(const Name: tbtString;
  var Index: TPSInterfaceMethod): Boolean;
var
  H: Longint;
  I: Longint;
  CurrClass: TPSInterface;
  C: TPSInterfaceMethod;
begin
  H := MakeHash(Name);
  CurrClass := Self;
  while CurrClass <> nil do
  begin
    for i := CurrClass.FItems.Count -1 downto 0 do
    begin
      C := CurrClass.FItems[I];
      if (C.NameHash = H) and (C.Name = Name) then
      begin
        Index := c;
        Result := True;
        exit;
      end;
    end;
    CurrClass := CurrClass.FInheritedFrom;
  end;
  Result := False;
end;

function TPSInterface.IsCompatibleWith(aType: TPSType): Boolean;
var
  Temp: TPSInterface;
begin
  if (atype.BaseType = btClass) then // just support it, we'll see what happens
  begin
    Result := true;
    exit;
  end;
  if atype.BaseType <> btInterface then
  begin
    Result := False;
    exit;
  end;
  temp := TPSInterfaceType(atype).FIntf;
  while Temp <> nil do
  begin
    if Temp = Self then
    begin
      Result := True;
      exit;
    end;
    Temp := Temp.FInheritedFrom;
  end;
  Result := False;
end;

procedure TPSInterface.RegisterDummyMethod;
begin
  FItems.Add(TPSInterfaceMethod.Create(self));
end;

function TPSInterface.RegisterMethod(const Declaration: tbtString;
  const cc: TPSCallingConvention): Boolean;
begin
  Result := RegisterMethodEx(Declaration, cc, nil);
end;

function TPSInterface.RegisterMethodEx(const Declaration: tbtString;
  const cc: TPSCallingConvention; const CustomParser: TPSPascalParser): Boolean;
var
  M: TPSInterfaceMethod;
  DOrgName: tbtString;
  Func: TPMFuncType;
begin
  M := TPSInterfaceMethod.Create(Self);
  if not ParseMethodEx(FOwner, '', Declaration, DOrgname, m.Decl, Func, CustomParser) then
  begin
    FItems.Add(m); // in any case, add a dummy item
    Result := False;
    exit;
  end;
  m.FName := FastUppercase(DOrgName);
  m.FOrgName := DOrgName;
  m.FNameHash := MakeHash(m.FName);
  m.FCC := CC;
  m.FScriptProcNo := InvalidVal;
  FItems.Add(M);
  Result := True;
end;


function TPSInterface.SetNil(var ProcNo: Cardinal): Boolean;
var
  P: TPSExternalProcedure;

begin
  if FNilProc <> InvalidVal then
  begin
    Procno := FNilProc;
    Result := True;
    exit;
  end;
  ProcNo := FOwner.AddUsedFunction2(P);
  P.RegProc := FOwner.AddFunction(ProcHDR);
  P.RegProc.Name := '';
  with p.RegProc.Decl.AddParam do
  begin
    Mode := pmInOut;
    OrgName := 'VarNo';
    aType := FOwner.at2ut(Self.FType);
  end;
  P.RegProc.ImportDecl := 'class:-';
  FNilProc := Procno;
  Result := True;
end;

{ TPSInterfaceMethod }

constructor TPSInterfaceMethod.Create(Owner: TPSInterface);
begin
  inherited Create;
  FDecl := TPSParametersDecl.Create;
  FOwner := Owner;
  FOffsetCache := InvalidVal;
end;

function TPSInterfaceMethod.GetAbsoluteProcOffset: Cardinal;
var
  ps: TPSInterface;
begin
  if FOffsetCache = InvalidVal then
  begin
    FOffsetCache := FOwner.FItems.IndexOf(Self);
    ps := FOwner.FInheritedFrom;
    while ps <> nil do
    begin
      FOffsetCache := FOffsetCache + ps.FItems.Count;
      ps := ps.FInheritedFrom;
    end;
  end;
  result := FOffsetCache;
end;


destructor TPSInterfaceMethod.Destroy;
begin
  FDecl.Free;
  inherited Destroy;
end;
{$ENDIF}

{ TPSVariantType }

function TPSVariantType.GetDynInvokeParamType(Owner: TPSPascalCompiler) : TPSType;
begin
  Result := Owner.at2ut(FindAndAddType(owner, '!OPENARRAYOFVARIANT', 'array of Variant'));
end;

function TPSVariantType.GetDynInvokeProcNo(Owner: TPSPascalCompiler; const Name: tbtString;
  Params: TPSParameters): Cardinal;
begin
  Result := Owner.FindProc('IdispatchInvoke');
end;

function TPSVariantType.GetDynIvokeResulType(
  Owner: TPSPascalCompiler): TPSType;
begin
  Result := Owner.FindType('VARIANT');
end;

function TPSVariantType.GetDynIvokeSelfType(Owner: TPSPascalCompiler): TPSType;
begin
  Result := Owner.at2ut(Owner.FindType('IDISPATCH'));
end;


{ TPSExternalClass }
function TPSExternalClass.SetNil(var ProcNo: Cardinal): Boolean;
begin
  Result := False;
end;

constructor TPSExternalClass.Create(Se: TIFPSPascalCompiler; TypeNo: TPSType);
begin
  inherited Create;
  Self.SE := se;
  Self.FTypeNo := TypeNo;
end;

function TPSExternalClass.Func_Call(Index: Cardinal;
  var ProcNo: Cardinal): Boolean;
begin
  Result := False;
end;

function TPSExternalClass.Func_Find(const Name: tbtString;
  var Index: Cardinal): Boolean;
begin
  Result := False;
end;

function TPSExternalClass.IsCompatibleWith(
  Cl: TPSExternalClass): Boolean;
begin
  Result := False;
end;

function TPSExternalClass.SelfType: TPSType;
begin
  Result := nil;
end;

function TPSExternalClass.CastToType(IntoType: TPSType;
  var ProcNo: Cardinal): Boolean;
begin
  Result := False;
end;

function TPSExternalClass.CompareClass(OtherTypeNo: TPSType;
  var ProcNo: Cardinal): Boolean;
begin
  Result := false;
end;

function TPSExternalClass.ClassFunc_Find(const Name: tbtString; var Index: Cardinal): Boolean;
begin
  result := false;
end;

function TPSExternalClass.ClassFunc_Call(Index: Cardinal; var ProcNo: Cardinal): Boolean;
begin
  result := false;
end;


{ TPSValueProcVal }

destructor TPSValueProcVal.Destroy;
begin
  FProcNo.Free;
  inherited;
end;


{

Internal error counter: 00020 (increase and then use)

}
end.
