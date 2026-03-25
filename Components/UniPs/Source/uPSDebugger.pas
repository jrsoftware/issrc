
unit uPSDebugger;
{$I PascalScript.inc}
interface
uses
  SysUtils, uPSRuntime, uPSUtils;

type
  
  TDebugMode = (dmRun 
  , dmStepOver 
  , dmStepInto 
  , dmPaused 
  );
  
  TPSCustomDebugExec = class(TPSExec)
  protected
    FDebugDataForProcs: TIfList;
    FLastProc: TPSProcRec;
    FCurrentDebugProc: Pointer;
    FProcNames: TIFStringList;
    FGlobalVarNames: TIfStringList;
    FCurrentSourcePos, FCurrentRow, FCurrentCol: Cardinal;
    FCurrentFile: tbtstring;
    
    function GetCurrentProcParams: TIfStringList;
    
    function GetCurrentProcVars: TIfStringList;
  protected
    
    procedure ClearDebug; virtual;
  public
    
    function GetCurrentProcNo: Cardinal;
    
    function GetCurrentPosition: Cardinal;
    
    function TranslatePosition(Proc, Position: Cardinal): Cardinal;
    
    function TranslatePositionEx(Proc, Position: Cardinal; var Pos, Row, Col: Cardinal; var Fn: tbtstring): Boolean;
    
    procedure LoadDebugData(const Data: tbtstring);
	
    procedure Clear; override;
    
    property GlobalVarNames: TIfStringList read FGlobalVarNames;
	
    property ProcNames: TIfStringList read FProcNames;
	
    property CurrentProcVars: TIfStringList read GetCurrentProcVars;
	
    property CurrentProcParams: TIfStringList read GetCurrentProcParams;
    
    function GetGlobalVar(I: Cardinal): PIfVariant;
	
    function GetProcVar(I: Cardinal): PIfVariant;
	
    function GetProcParam(I: Cardinal): PIfVariant;
  
    function GetCallStack(var Count: Cardinal): tbtString;
  
    constructor Create;
	
    destructor Destroy; override;
  end;
  TPSDebugExec = class;
  
  TOnSourceLine = procedure (Sender: TPSDebugExec; const Name: tbtstring; Position, Row, Col: Cardinal);
  
  TOnIdleCall = procedure (Sender: TPSDebugExec);
  
  TPSDebugExec = class(TPSCustomDebugExec)
  private
    FDebugMode: TDebugMode;
    FStepOverProc: TPSInternalProcRec;
    FStepOverStackBase: Cardinal;
    FOnIdleCall: TOnIdleCall;
    FOnSourceLine: TOnSourceLine;
    FDebugEnabled: Boolean;
  protected
    
    procedure SourceChanged;
    procedure ClearDebug; override;
    procedure RunLine; override;
  public
    constructor Create;
    
    function LoadData(const s: tbtstring): Boolean; override;
    
    procedure Pause; override;
    
    procedure Run;
    
    procedure StepInto;
    
    procedure StepOver;
    
    procedure Stop; override;
	
    property DebugMode: TDebugMode read FDebugMode;
    
    property OnSourceLine: TOnSourceLine read FOnSourceLine write FOnSourceLine;
	
    property OnIdleCall: TOnIdleCall read FOnIdleCall write FOnIdleCall;
    
    property DebugEnabled: Boolean read FDebugEnabled write FDebugEnabled;
  end;
  TIFPSDebugExec = TPSDebugExec;

implementation

{$IFDEF DELPHI3UP }
resourceString
{$ELSE }
const
{$ENDIF }

  RPS_ExpectedReturnAddressStackBase = 'Expected return address at stack base';

type
  PPositionData = ^TPositionData;
  TPositionData = packed record
    FileName: tbtstring;
    Position,
    Row,
    Col,
    SourcePosition: Cardinal;
  end;
  PFunctionInfo = ^TFunctionInfo;
  TFunctionInfo = packed record
    Func: TPSProcRec;
    FParamNames: TIfStringList;
    FVariableNames: TIfStringList;
    FPositionTable: TIfList;
  end;

{ TPSCustomDebugExec }

procedure TPSCustomDebugExec.Clear;
begin
  inherited Clear;
  if FGlobalVarNames <> nil then ClearDebug;
end;

procedure TPSCustomDebugExec.ClearDebug;
var
  i, j: Longint;
  p: PFunctionInfo;
begin
  FCurrentDebugProc := nil;
  FLastProc := nil;
  FProcNames.Clear;
  FGlobalVarNames.Clear;
  FCurrentSourcePos := 0;
  FCurrentRow := 0;
  FCurrentCol := 0;
  FCurrentFile := '';
  for i := 0 to FDebugDataForProcs.Count -1 do
  begin
    p := FDebugDataForProcs[I];
    for j := 0 to p^.FPositionTable.Count -1 do
    begin
      Dispose(PPositionData(P^.FPositionTable[J]));
    end;
    p^.FPositionTable.Free;
    p^.FParamNames.Free;
    p^.FVariableNames.Free;
    Dispose(p);
  end;
  FDebugDataForProcs.Clear;
end;

constructor TPSCustomDebugExec.Create;
begin
  inherited Create;
  FCurrentSourcePos := 0;
  FCurrentRow := 0;
  FCurrentCol := 0;
  FCurrentFile := '';
  FDebugDataForProcs := TIfList.Create;
  FLastProc := nil;
  FCurrentDebugProc := nil;
  FProcNames := TIFStringList.Create;
  FGlobalVarNames := TIfStringList.Create;
end;

destructor TPSCustomDebugExec.Destroy;
begin
  Clear;
  FDebugDataForProcs.Free;
  FProcNames.Free;
  FGlobalVarNames.Free;
  FGlobalVarNames := nil;
  inherited Destroy;
end;

function TPSCustomDebugExec.GetCurrentPosition: Cardinal;
begin
  Result := TranslatePosition(GetCurrentProcNo, 0);
end;

function TPSCustomDebugExec.GetCurrentProcNo: Cardinal;
var
  i: Longint;
begin
  for i := 0 to FProcs.Count -1 do
  begin
    if FProcs[i]=  FCurrProc then
    begin
      Result := I;
      Exit;
    end;
  end;
  Result := Cardinal(-1);
end;

function TPSCustomDebugExec.GetCurrentProcParams: TIfStringList;
begin
  if FCurrentDebugProc <> nil then
  begin
    Result := PFunctionInfo(FCurrentDebugProc)^.FParamNames;
  end else Result := nil;
end;

function TPSCustomDebugExec.GetCurrentProcVars: TIfStringList;
begin
  if FCurrentDebugProc <> nil then
  begin
    Result := PFunctionInfo(FCurrentDebugProc)^.FVariableNames;
  end else Result := nil;
end;

function TPSCustomDebugExec.GetGlobalVar(I: Cardinal): PIfVariant;
begin
  Result := FGlobalVars[I];
end;

function TPSCustomDebugExec.GetProcParam(I: Cardinal): PIfVariant;
begin
  Result := FStack[Cardinal(Longint(FCurrStackBase) - Longint(I) - 1)];
end;

function TPSCustomDebugExec.GetProcVar(I: Cardinal): PIfVariant;
begin
  Result := FStack[Cardinal(Longint(FCurrStackBase) + Longint(I) + 1)];
end;

function GetProcDebugInfo(FProcs: TIFList; Proc: TPSProcRec): PFunctionInfo;
var
  i: Longint;
  c: PFunctionInfo;
begin
  if Proc = nil then
  begin
    Result := nil;
    exit;
  end;
  for i := FProcs.Count -1 downto 0 do
  begin
    c := FProcs.Data^[I];
    if c^.Func = Proc then
    begin
      Result := c;
      exit;
    end;
  end;
  new(c);
  c^.Func := Proc;
  c^.FPositionTable := TIfList.Create;
  c^.FVariableNames := TIfStringList.Create;
  c^.FParamNames := TIfStringList.Create;
  FProcs.Add(c);
  REsult := c;
end;

procedure TPSCustomDebugExec.LoadDebugData(const Data: tbtstring);
var
  CP, I: Longint;
  c: tbtchar;
  CurrProcNo, LastProcNo: Cardinal;
  LastProc: PFunctionInfo;
  NewLoc: PPositionData;
  s: tbtstring;
begin
  ClearDebug;
  if FStatus = isNotLoaded then exit;
  CP := 1;
  LastProcNo := Cardinal(-1);
  LastProc := nil;
  while CP <= length(Data) do
  begin
    c := Data[CP];
    inc(cp);
    case c of
      #0:
        begin
          i := cp;
          if i > length(data) then exit;
          while Data[i] <> #0 do
          begin
            if Data[i] = #1 then
            begin
              FProcNames.Add(Copy(Data, cp, i-cp));
              cp := I + 1;
            end;
            inc(I);
            if I > length(data) then exit;
          end;
          cp := i + 1;
        end;
      #1:
        begin
          i := cp;
          if i > length(data) then exit;
          while Data[i] <> #0 do
          begin
            if Data[i] = #1 then
            begin
              FGlobalVarNames.Add(Copy(Data, cp, i-cp));
              cp := I + 1;
            end;
            inc(I);
            if I > length(data) then exit;
          end;
          cp := i + 1;
        end;
      #2:
        begin
          if cp + 4 > Length(data) then exit;
          CurrProcNo := Cardinal((@Data[cp])^);
          if CurrProcNo = Cardinal(-1) then Exit;
          if CurrProcNo <> LastProcNo then
          begin
            LastProcNo := CurrProcNo;
            LastProc := GetProcDebugInfo(FDebugDataForProcs, FProcs[CurrProcNo]);
            if LastProc = nil then exit;
          end;
          inc(cp, 4);

          i := cp;
          if i > length(data) then exit;
          while Data[i] <> #0 do
          begin
            if Data[i] = #1 then
            begin
              LastProc^.FParamNames.Add(Copy(Data, cp, i-cp));
              cp := I + 1;
            end;
            inc(I);
            if I > length(data) then exit;
          end;
          cp := i + 1;
        end;
      #3:
        begin
          if cp + 4 > Length(data) then exit;
          CurrProcNo := Cardinal((@Data[cp])^);
          if CurrProcNo = Cardinal(-1) then Exit;
          if CurrProcNo <> LastProcNo then
          begin
            LastProcNo := CurrProcNo;
            LastProc := GetProcDebugInfo(FDebugDataForProcs, FProcs[CurrProcNo]);
            if LastProc = nil then exit;
          end;
          inc(cp, 4);

          i := cp;
          if i > length(data) then exit;
          while Data[i] <> #0 do
          begin
            if Data[i] = #1 then
            begin
              LastProc^.FVariableNames.Add(Copy(Data, cp, i-cp));
              cp := I + 1;
            end;
            inc(I);
            if I > length(data) then exit;
          end;
          cp := i + 1;
        end;
      #4:
        begin
          i := cp;
          if i > length(data) then exit;
          while Data[i] <> #0 do
          begin
            if Data[i] = #1 then
            begin
              s := Copy(Data, cp, i-cp);
              cp := I + 1;
              Break;
            end;
            inc(I);
            if I > length(data) then exit;
          end;
          if cp + 4 > Length(data) then exit;
          CurrProcNo := Cardinal((@Data[cp])^);
          if CurrProcNo = Cardinal(-1) then Exit;
          if CurrProcNo <> LastProcNo then
          begin
            LastProcNo := CurrProcNo;
            LastProc := GetProcDebugInfo(FDebugDataForProcs, FProcs[CurrProcNo]);
            if LastProc = nil then exit;
          end;
          inc(cp, 4);
          if cp + 16 > Length(data) then exit;
          new(NewLoc);
          NewLoc^.Position := Cardinal((@Data[Cp])^);
          NewLoc^.FileName := s;
          NewLoc^.SourcePosition := Cardinal((@Data[Cp+4])^);
          NewLoc^.Row := Cardinal((@Data[Cp+8])^);
          NewLoc^.Col := Cardinal((@Data[Cp+12])^);
          inc(cp, 16);
          LastProc^.FPositionTable.Add(NewLoc);
        end;
      else
        begin
          ClearDebug;
          Exit;
        end;
    end;

  end;
end;






function TPSCustomDebugExec.TranslatePosition(Proc, Position: Cardinal): Cardinal;
var
  D1, D2: Cardinal;
  s: tbtstring;
begin
  if not TranslatePositionEx(Proc, Position, Result, D1, D2, s) then
    Result := 0;
end;

function TPSCustomDebugExec.TranslatePositionEx(Proc, Position: Cardinal;
  var Pos, Row, Col: Cardinal; var Fn: tbtstring): Boolean;
// Made by Martijn Laan (mlaan@wintax.nl)
var
  i: LongInt;
  fi: PFunctionInfo;
  pt: TIfList;
  r: PPositionData;
  lastfn: tbtstring;
  LastPos, LastRow, LastCol: Cardinal;
  pp: TPSProcRec;
begin
  fi := nil;
  pp := FProcs[Proc];
  for i := 0 to FDebugDataForProcs.Count -1 do
  begin
    fi := FDebugDataForProcs[i];
    if fi^.Func = pp then
      Break;
    fi := nil;
  end;
  LastPos := 0;
  LastRow := 0;
  LastCol := 0;
  if fi <> nil then begin
    pt := fi^.FPositionTable;
    for i := 0 to pt.Count -1 do
    begin
      r := pt[I];
      if r^.Position >= Position then
      begin
        if r^.Position = Position then
        begin
          Pos := r^.SourcePosition;
          Row := r^.Row;
          Col := r^.Col;
          Fn := r^.Filename;
        end
        else
        begin
          Pos := LastPos;
          Row := LastRow;
          Col := LastCol;
          Fn := LastFn;
        end;
        Result := True;
        exit;
      end else
      begin
        LastPos := r^.SourcePosition;
        LastRow := r^.Row;
        LastCol := r^.Col;
        LastFn := r^.FileName;
      end;
    end;
    Pos := LastPos;
    Row := LastRow;
    Col := LastCol;
    Result := True;
  end else
  begin
    Result := False;
  end;
end;

function TPSCustomDebugExec.GetCallStack(var Count: Cardinal): tbtString;

  function GetProcIndex(Proc: TPSInternalProcRec): Cardinal;
  var
    I: Longint;
  begin
    for I := 0 to FProcs.Count -1 do
    begin
      if FProcs[I] = Proc then
      begin
        Result := I;
        Exit;
      end;
    end;
    Result := Cardinal(-1);
  end;

  function ParseParams(ParamList: TIfStringList; StackBase: Cardinal): tbtString;
  var
    I: Integer;
  begin
    Result := '';
    if ParamList.Count > 0 then
    for I := 0 to ParamList.Count do
      if (ParamList.Items[I] = 'Result') or (ParamList.Items[I] = '') then
        Continue
      else
        Result:= Result + ParamList.Items[I] + ': ' +
                 PSVariantToString(NewTPSVariantIFC(FStack[Cardinal(Longint(StackBase) - Longint(I) - 1)], False), '') + '; ';
    Result := tbtString(String(Result).Remove(Length(Result)-2));
  end;

var
  StackBase: Cardinal;
  DebugProc: PFunctionInfo;
  Name: tbtString;
  I: Integer;
begin
  Result := ProcNames[GetProcIndex(FCurrProc)] + '(' +
            ParseParams(GetCurrentProcParams, FCurrStackBase) + ')';
  Count := 1;

  StackBase := FCurrStackBase;

  while StackBase > 0 do
  begin
    DebugProc := nil;

    for I := 0 to FDebugDataForProcs.Count -1 do
      if PFunctionInfo(FDebugDataForProcs[I])^.Func = PPSVariantReturnAddress(FStack[StackBase]).Addr.ProcNo then
      begin
        DebugProc := FDebugDataForProcs[I];
        Break;
      end;

    I := GetProcIndex(PPSVariantReturnAddress(FStack[StackBase]).Addr.ProcNo);
    if I <= 0 then
      if Assigned(PPSVariantReturnAddress(FStack[StackBase]).Addr.ProcNo) then
        Name := PPSVariantReturnAddress(FStack[StackBase]).Addr.ProcNo.ExportName
      else
        Exit
    else
      Name := ProcNames[I];

    StackBase := PPSVariantReturnAddress(FStack[StackBase]).Addr.StackBase;
    if Assigned(DebugProc) then
      Result := Result + #13#10 + Name + '(' + ParseParams(DebugProc.FParamNames, StackBase) + ')'
    else
      Result := Result + #13#10 + Name + '(???)';

    Inc(Count);
  end;
end;

{ TPSDebugExec }
procedure TPSDebugExec.ClearDebug;
begin
  inherited;
  FDebugMode := dmRun;
end;

function TPSDebugExec.LoadData(const s: tbtstring): Boolean;
begin
  Result := inherited LoadData(s);
  FDebugMode := dmRun;
end;

procedure TPSDebugExec.RunLine;
var
  i: Longint;
  pt: TIfList;
  r: PPositionData;
begin
  inherited RunLine;
  if not DebugEnabled then exit;
  if FCurrProc <> FLastProc then
  begin
    FLastProc := FCurrProc;
    FCurrentDebugProc := nil;
    for i := 0 to FDebugDataForProcs.Count -1 do
    begin
      if PFunctionInfo(FDebugDataForProcs[I])^.Func = FLastProc then
      begin
        FCurrentDebugProc := FDebugDataForProcs[I];
        break;
      end;
    end;
  end;
  if FCurrentDebugProc <> nil then
  begin
    pt := PFunctionInfo(FCurrentDebugProc)^.FPositionTable;
    for i := 0 to pt.Count -1 do
    begin
      r := pt[I];
      if r^.Position = FCurrentPosition then
      begin
        FCurrentSourcePos := r^.SourcePosition;
        FCurrentRow := r^.Row;
        FCurrentCol := r^.Col;
        FCurrentFile := r^.FileName;
        SourceChanged;
        break;
      end;
    end;
  end else
  begin
    FCurrentSourcePos := 0;
    FCurrentRow := 0;
    FCurrentCol := 0;
    FCurrentFile := '';
  end;
  while FDebugMode = dmPaused do
  begin
    if @FOnIdleCall <> nil then
    begin
      FOnIdleCall(Self);
    end else break; // endless loop
  end;
end;


procedure TPSDebugExec.SourceChanged;

  function StepOverShouldPause: Boolean;
  var
    I: Cardinal;
    V: PPSVariant;
  begin
    if (FCurrProc <> FStepOverProc) or (FCurrStackBase <> FStepOverStackBase) then
    begin
      { We're not inside the function being stepped, so scan the call stack to
        see if we're inside a function called by the function being stepped }
      I := FCurrStackBase;
      while Longint(I) > Longint(FStepOverStackBase) do
      begin
        V := FStack.Items[I];
        if (V = nil) or (V.FType <> FReturnAddressType) then
          raise Exception.Create(RPS_ExpectedReturnAddressStackBase);
        if (PPSVariantReturnAddress(V).Addr.ProcNo = FStepOverProc) and
           (PPSVariantReturnAddress(V).Addr.StackBase = FStepOverStackBase) then
        begin
          { We are, so don't pause }
          Result := False;
          Exit;
        end;
        I := PPSVariantReturnAddress(V).Addr.StackBase;
      end;
    end;
    Result := True;
  end;

begin
  case FDebugMode of
    dmStepInto:
      begin
        FDebugMode := dmPaused;
      end;
    dmStepOver:
      begin
        if StepOverShouldPause then
        begin
          FDebugMode := dmPaused;
        end;
      end;
  end;
  if @FOnSourceLine <> nil then
    FOnSourceLine(Self, FCurrentFile, FCurrentSourcePos, FCurrentRow, FCurrentCol);
end;


procedure TPSDebugExec.Pause;
begin
  FDebugMode := dmPaused;
end;

procedure TPSDebugExec.Stop;
begin
  FDebugMode := dmRun;
  inherited Stop;
end;

procedure TPSDebugExec.Run;
begin
  FDebugMode := dmRun;
end;

procedure TPSDebugExec.StepInto;
begin
  FDebugMode := dmStepInto;
end;

procedure TPSDebugExec.StepOver;
begin
  FStepOverProc := FCurrProc;
  FStepOverStackBase := FCurrStackBase;
  FDebugMode := dmStepOver;
end;


constructor TPSDebugExec.Create;
begin
  inherited Create;
  FDebugEnabled := True;
end;

end.
