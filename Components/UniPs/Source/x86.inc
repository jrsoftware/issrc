{ implementation of x86         abi }
{$ifdef FPC}
{$define PS_ARRAY_ON_STACK}
{$endif}
function RealFloatCall_Register(p: Pointer;
  _EAX, _EDX, _ECX: Cardinal;
  StackData: Pointer;
  StackDataLen: Longint // stack length are in 4 bytes. (so 1 = 4 bytes)
  ): Extended; Stdcall; // make sure all things are on stack
var
  E: Extended;
begin
  asm
    mov ecx, stackdatalen
    jecxz @@2
    mov eax, stackdata
    @@1:
    mov edx, [eax]
    push edx
    sub eax, 4
    dec ecx
    or ecx, ecx
    jnz @@1
    @@2:
    mov eax,_EAX
    mov edx,_EDX
    mov ecx,_ECX
    call p
    fstp tbyte ptr [e]
  end;
  Result := E;
end;
                 
function RealFloatCall_Other(p: Pointer;
  StackData: Pointer;
  StackDataLen: Longint // stack length are in 4 bytes. (so 1 = 4 bytes)
  ): Extended; Stdcall; // make sure all things are on stack
var
  E: Extended;
begin
  asm
    mov ecx, stackdatalen
    jecxz @@2
    mov eax, stackdata
    @@1:
    mov edx, [eax]
    push edx
    sub eax, 4
    dec ecx
    or ecx, ecx
    jnz @@1
    @@2:
    call p
    fstp tbyte ptr [e]
  end;
  Result := E;
end;

function RealFloatCall_CDecl(p: Pointer;
  StackData: Pointer;
  StackDataLen: Longint // stack length are in 4 bytes. (so 1 = 4 bytes)
  ): Extended; Stdcall; // make sure all things are on stack
var
  E: Extended;
begin
  asm
    mov ecx, stackdatalen
    jecxz @@2
    mov eax, stackdata
    @@1:
    mov edx, [eax]
    push edx
    sub eax, 4
    dec ecx
    or ecx, ecx
    jnz @@1
    @@2:
    call p
    fstp tbyte ptr [e]
    mov ecx, stackdatalen
    jecxz @@4
    @@3:
    pop edx
    dec ecx
    or ecx, ecx
    jnz @@3
    @@4:
  end;
  Result := E;
end;

function RealCall_Register(p: Pointer;
  _EAX, _EDX, _ECX: Cardinal;
  StackData: Pointer;
  StackDataLen: Longint; // stack length are in 4 bytes. (so 1 = 4 bytes)
  ResultLength: Longint; ResEDX: Pointer): Longint; Stdcall; // make sure all things are on stack
var
  r: Longint;
begin
  asm
    mov ecx, stackdatalen
    jecxz @@2
    mov eax, stackdata
    @@1:
    mov edx, [eax]
    push edx
    sub eax, 4
    dec ecx
    or ecx, ecx
    jnz @@1
    @@2:
    mov eax,_EAX
    mov edx,_EDX
    mov ecx,_ECX
    call p
    mov ecx, resultlength
    cmp ecx, 0
    je @@5
    cmp ecx, 1
    je @@3
    cmp ecx, 2
    je @@4
    mov r, eax
    jmp @@5
    @@3:
    xor ecx, ecx
    mov cl, al
    mov r, ecx
    jmp @@5
    @@4:
    xor ecx, ecx
    mov cx, ax
    mov r, ecx
    @@5:
    mov ecx, resedx
    jecxz @@6
    mov [ecx], edx
    @@6:
  end;
  Result := r;
end;

function RealCall_Other(p: Pointer;
  StackData: Pointer;
  StackDataLen: Longint; // stack length are in 4 bytes. (so 1 = 4 bytes)
  ResultLength: Longint; ResEDX: Pointer): Longint; Stdcall; // make sure all things are on stack
var
  r: Longint;
begin
  asm
    mov ecx, stackdatalen
    jecxz @@2
    mov eax, stackdata
    @@1:
    mov edx, [eax]
    push edx
    sub eax, 4
    dec ecx
    or ecx, ecx
    jnz @@1
    @@2:
    call p
    mov ecx, resultlength
    cmp ecx, 0
    je @@5
    cmp ecx, 1
    je @@3
    cmp ecx, 2
    je @@4
    mov r, eax
    jmp @@5
    @@3:
    xor ecx, ecx
    mov cl, al
    mov r, ecx
    jmp @@5
    @@4:
    xor ecx, ecx
    mov cx, ax
    mov r, ecx
    @@5:
    mov ecx, resedx
    jecxz @@6
    mov [ecx], edx
    @@6:
  end;
  Result := r;
end;

function RealCall_CDecl(p: Pointer;
  StackData: Pointer;
  StackDataLen: Longint; // stack length are in 4 bytes. (so 1 = 4 bytes)
  ResultLength: Longint; ResEDX: Pointer): Longint; Stdcall; // make sure all things are on stack
var
  r: Longint;
begin
  asm
    mov ecx, stackdatalen
    jecxz @@2
    mov eax, stackdata
    @@1:
    mov edx, [eax]
    push edx
    sub eax, 4
    dec ecx
    or ecx, ecx
    jnz @@1
    @@2:
    call p
    mov ecx, resultlength
    cmp ecx, 0
    je @@5
    cmp ecx, 1
    je @@3
    cmp ecx, 2
    je @@4
    mov r, eax
    jmp @@5
    @@3:
    xor ecx, ecx
    mov cl, al
    mov r, ecx
    jmp @@5
    @@4:
    xor ecx, ecx
    mov cx, ax
    mov r, ecx
    @@5:
    mov ecx, stackdatalen
    jecxz @@7
    @@6:
    pop eax
    dec ecx
    or ecx, ecx
    jnz @@6
    mov ecx, resedx
    jecxz @@7
    mov [ecx], edx
    @@7:
  end;
  Result := r;
end;

const
  EmptyPchar: array[0..0] of char = #0;

{$IFDEF DELPHI}{$IFDEF MSWINDOWS}{$DEFINE DELPHI_and_MSWINDOWS}{$ENDIF}{$ENDIF}

{$IFDEF DELPHI_and_MSWINDOWS}
{ Delphi Win32 passes a by-value record or static array of 4 bytes
  containing managed data (such as record S: String end) without reference
  counting the raw copy, like a by-value string parameter. Older versions are
  the exception: under stdcall/cdecl/safecall the caller must pass a
  reference counted copy that the callee finalizes at exit. It is unknown
  exactly when this changed, only that it is off in 10.4 and on in 12.2, so
  Delphi 12 is used as the boundary. }
{$IFDEF DELPHI_ATHENS_UP}{$DEFINE DELPHI_SMALL_MANAGED_BORROW}{$ENDIF}
{$ENDIF}

function TPSExec.InnerfuseCall(_Self, Address: Pointer; CallingConv: TPSCallingConvention; Params: TPSList; res: PPSVariantIFC): Boolean;
var
  Stack: ansistring;
  I: Longint;
  RegUsage: Byte;
  CallData: TPSList;
  pp: ^Byte;
{$IFDEF FPC}
  IsConstructor,IsVirtualCons: Boolean;
  MethodData: TMethod;
{$ENDIF}

  EAX, EDX, ECX: Longint;

  function rp(p: PPSVariantIFC): PPSVariantIFC;
  begin
    if p = nil then
    begin
      result := nil;
      exit;
    end;
    if p.aType.BaseType = btPointer then
    begin
      p^.aType := Pointer(Pointer(IPointer(p^.dta) + 4)^);
      p^.Dta := Pointer(p^.dta^);
    end;
    Result := p;
  end;

  function ResultUsesResultPointer(res: PPSVariantIFC): Boolean;
  begin
    {$IFDEF DELPHI_and_MSWINDOWS}
    { Delphi Win32 ABI: a record result is returned in AL/AX at sizes 1 and 2,
      and in EAX at size 4 only when the record contains no managed field;
      size 3, every size above 4, and the managed 4-byte record are written
      through a hidden result pointer (see System.Rtti's UseResultPointer).
      The rule does not depend on the calling convention. Static arrays follow
      the same rule: UseResultPointer treats tkArray like tkRecord. }
    if res^.aType.BaseType in [btRecord, btStaticArray] then
      Result := not (res^.aType.RealSize in [1, 2]) and
        ((res^.aType.RealSize <> 4) or IsManagedType(res^.aType))
    else
    {$ENDIF}
    begin
      {$IFDEF MSWINDOWS}
      Result := res^.aType.RealSize > 4;
      {$ELSE}
      Result := True;
      {$ENDIF}
    end;
  end;

  function GetPtr(fVar: PPSVariantIFC): Boolean;
  var
    varPtr: Pointer;
    UseReg: Boolean;
    tempstr: tbtstring;
    p: Pointer;
    DataSize: Cardinal;
  begin
    Result := False;
    if FVar = nil then exit;
    if fVar.VarParam then
    begin
      case fvar.aType.BaseType of
        btArray:
          begin
            if Copy(fvar.aType.ExportName, 1, 10) = '!OPENARRAY' then
            begin
              p := CreateOpenArray(True, Self, FVar);
              if p = nil then exit;
              CallData.Add(p);
              case RegUsage of
                0: begin EAX := Longint(POpenArray(p)^.Data); Inc(RegUsage); end;
                1: begin EDX := Longint(POpenArray(p)^.Data); Inc(RegUsage); end;
                2: begin ECX := Longint(POpenArray(p)^.Data); Inc(RegUsage); end;
                else begin
                  Stack := StringOfChar(AnsiChar(#0),4) + Stack;
                  Pointer((@Stack[1])^) := POpenArray(p)^.Data;
                end;
              end;
              case RegUsage of
                0: begin EAX := Longint(POpenArray(p)^.ItemCount - 1); Inc(RegUsage); end;
                1: begin EDX := Longint(POpenArray(p)^.ItemCount -1); Inc(RegUsage); end;
                2: begin ECX := Longint(POpenArray(p)^.ItemCount -1); Inc(RegUsage); end;
                else begin
                  Stack := StringOfChar(AnsiChar(#0),4) + Stack;
                  Longint((@Stack[1])^) := POpenArray(p)^.ItemCount -1;
                end;
              end;
              Result := True;
              Exit;
            end else begin
            {$IFDEF PS_DYNARRAY}
              varptr := fvar.Dta;
            {$ELSE}
              Exit;
            {$ENDIF}
            end;
          end;
        btVariant,
        btSet,
        btStaticArray,
        btRecord,
        btInterface,
        btClass,
        {$IFNDEF PS_NOWIDESTRING} btUnicodeString, btWideString, btWideChar, {$ENDIF} btU8, btS8, btU16,
        btS16, btU32, btS32, btSingle, btDouble, btExtended, btString, btPChar, btChar, btCurrency
        {$IFNDEF PS_NOINT64}, bts64, btU64{$ENDIF}:
          begin
            Varptr := fvar.Dta;
          end;
      else begin
          exit; //invalid type
        end;
      end; {case}
      case RegUsage of
        0: begin EAX := Longint(VarPtr); Inc(RegUsage); end;
        1: begin EDX := Longint(VarPtr); Inc(RegUsage); end;
        2: begin ECX := Longint(VarPtr); Inc(RegUsage); end;
        else begin
          Stack := StringOfChar(AnsiChar(#0),4) + Stack;
          Pointer((@Stack[1])^) := VarPtr;
        end;
      end;
    end else begin
      UseReg := True;
      case fVar^.aType.BaseType of
        btSet, btRecord:
          begin
            if fVar^.aType.BaseType = btSet then
              DataSize := TPSTypeRec_Set(fvar.aType).aByteSize
            else
              DataSize := fvar^.aType.RealSize;
            {$IFDEF DELPHI_and_MSWINDOWS}
            { Delphi Win32 ABI: under the register and pascal conventions a
              record is passed by value at sizes 1, 2, and 4 (in a register
              when one is free), by value on the stack at size 3, and by
              reference at 5 and up. Under stdcall/cdecl/safecall a record is
              passed by value on the stack at every size, rounded up to whole
              4-byte slots; except that when it contains managed data the
              caller must reference count the pushed copy. For a 4-byte
              managed record this is done in the size-4 case below (older
              Delphi only; see DELPHI_SMALL_MANAGED_BORROW); for a larger
              managed record it is not implemented, so a pointer is passed
              instead and the native side must declare the parameter as const
              to match (a const record of 5 bytes and up is passed by
              reference). A set is by value at its native size of 1, 2, or 4
              bytes (a 3-byte ROPS set is a native 4-byte set, copied with
              zero extension) and by reference at 5 and up, under all
              conventions. For static arrays see the btVariant/btStaticArray
              branch below }
            if (fVar^.aType.BaseType = btRecord) and (DataSize > 4) and
               (CallingConv in [cdCdecl, cdStdCall, cdSafeCall]) and
               not IsManagedType(fVar^.aType) then
            begin
              UseReg := False;
              tempstr := StringOfChar(AnsiChar(#0), ((DataSize + 3) div 4) * 4);
              Move(fvar.dta^, tempstr[1], DataSize);
            end else
            {$ENDIF}
            begin
              tempstr := StringOfChar(AnsiChar(#0),4);
              case DataSize of
                1: Byte((@tempstr[1])^) := byte(fvar.dta^);
                2: word((@tempstr[1])^) := word(fvar.dta^);
                {$IFDEF DELPHI_and_MSWINDOWS}
                3: begin
                    Move(fvar.dta^, tempstr[1], 3);
                    if fVar^.aType.BaseType = btRecord then
                      UseReg := False;
                  end;
                4: begin
                    {$IFNDEF DELPHI_SMALL_MANAGED_BORROW}
                    if (fVar^.aType.BaseType = btRecord) and
                       (CallingConv in [cdCdecl, cdStdCall, cdSafeCall]) and
                       IsManagedType(fVar^.aType) then
                      { Reference counted copy that the callee finalizes }
                      CopyArrayContents(@tempstr[1], fvar.dta, 1, fVar^.aType)
                    else
                    {$ENDIF}
                      cardinal((@tempstr[1])^) := cardinal(fvar.dta^);
                  end;
                {$ELSE}
                3, 4: cardinal((@tempstr[1])^) := cardinal(fvar.dta^);
                {$ENDIF}
                else
                  pointer((@tempstr[1])^) := fvar.dta;
              end;
            end;
          end;
        btArray:
          begin
            if Copy(fvar^.aType.ExportName, 1, 10) = '!OPENARRAY' then
            begin
              p := CreateOpenArray(False, SElf, FVar);
              if p =nil then exit;
              CallData.Add(p);
              case RegUsage of
                0: begin EAX := Longint(POpenArray(p)^.Data); Inc(RegUsage); end;
                1: begin EDX := Longint(POpenArray(p)^.Data); Inc(RegUsage); end;
                2: begin ECX := Longint(POpenArray(p)^.Data); Inc(RegUsage); end;
                else begin
                  Stack := StringOfChar(AnsiChar(#0),4) + Stack;
                  Pointer((@Stack[1])^) := POpenArray(p)^.Data;
                end;
              end;
              case RegUsage of
                0: begin EAX := Longint(POpenArray(p)^.ItemCount -1); Inc(RegUsage); end;
                1: begin EDX := Longint(POpenArray(p)^.ItemCount -1); Inc(RegUsage); end;
                2: begin ECX := Longint(POpenArray(p)^.ItemCount -1); Inc(RegUsage); end;
                else begin
                  Stack := StringOfChar(AnsiChar(#0),4) + Stack;
                  Longint((@Stack[1])^) := POpenArray(p)^.ItemCount -1;
                end;
              end;
              Result := True;
              exit;
            end else begin
            {$IFDEF PS_DYNARRAY}
              TempStr := StringOfChar(AnsiChar(#0),4);
              Pointer((@TempStr[1])^) := Pointer(fvar.Dta^);
            {$IFDEF PS_ARRAY_ON_STACK}
            UseReg := false;
            {$ENDIF}
            {$ELSE}
              Exit;
            {$ENDIF}
            end;
          end;
        btVariant, btStaticArray:
          begin
            {$IFDEF DELPHI_and_MSWINDOWS}
            { Delphi Win32 ABI: a static array of 1-4 bytes is passed by value
              like a record of that size (1, 2, and 4 in a register when one is
              free, 3 always by value on the stack; see the btSet/btRecord
              branch above), and by reference at 5 and up, under all
              conventions }
            if (fVar^.aType.BaseType = btStaticArray) and
               (fvar^.aType.RealSize <= 4) then
            begin
              DataSize := fvar^.aType.RealSize;
              tempstr := StringOfChar(AnsiChar(#0),4);
              {$IFNDEF DELPHI_SMALL_MANAGED_BORROW}
              if (CallingConv in [cdCdecl, cdStdCall, cdSafeCall]) and
                 IsManagedType(fVar^.aType) then
                { Reference counted copy that the callee finalizes }
                CopyArrayContents(@tempstr[1], fvar.dta, 1, fVar^.aType)
              else
              {$ENDIF}
                Move(fvar.dta^, tempstr[1], DataSize);
              if DataSize = 3 then
                UseReg := False;
            end else
            {$ENDIF}
            begin
              TempStr := StringOfChar(AnsiChar(#0),4);
              Pointer((@TempStr[1])^) := Pointer(fvar.Dta);
            end;
          end;
        btDouble: {8 bytes} begin
            TempStr := StringOfChar(AnsiChar(#0),8);
            UseReg := False;
            double((@TempStr[1])^) := double(fvar.dta^);
          end;
        btCurrency: {8 bytes} begin
            TempStr := StringOfChar(AnsiChar(#0),8);
            UseReg := False;
            currency((@TempStr[1])^) := currency(fvar.dta^);
          end;
        btSingle: {4 bytes} begin
            TempStr := StringOfChar(AnsiChar(#0),4);;
            UseReg := False;
            Single((@TempStr[1])^) := single(fvar.dta^);
          end;

        btExtended: {10 bytes} begin
            UseReg := False;
            TempStr:= StringOfChar(AnsiChar(#0),12);
            Extended((@TempStr[1])^) := extended(fvar.dta^);
          end;
        btChar,
        btU8,
        btS8: begin
            TempStr := tbtchar(fVar^.dta^) + tbtstring(StringOfChar(AnsiChar(#0),3));
          end;
        {$IFNDEF PS_NOWIDESTRING}btWideChar, {$ENDIF}
        btu16, btS16: begin
            TempStr := StringOfChar(AnsiChar(#0),4);
            Word((@TempStr[1])^) := word(fVar^.dta^);
          end;
        btu32, bts32: begin
            TempStr := StringOfChar(AnsiChar(#0),4);
            Longint((@TempStr[1])^) := Longint(fVar^.dta^);
          end;
        btPchar:
          begin
            TempStr := StringOfChar(AnsiChar(#0),4);
            if pointer(fvar^.dta^) = nil then
              Pointer((@TempStr[1])^) := @EmptyPchar
            else
              Pointer((@TempStr[1])^) := pointer(fvar^.dta^);
          end;
        btclass, btinterface, btString:
          begin
            TempStr := StringOfChar(AnsiChar(#0),4);
            Pointer((@TempStr[1])^) := pointer(fvar^.dta^);
          end;
          {$IFNDEF PS_NOWIDESTRING}
        btWideString: begin
            TempStr := StringOfChar(AnsiChar(#0),4);
            Pointer((@TempStr[1])^) := pointer(fvar^.dta^);
          end;
        btUnicodeString: begin
            TempStr := StringOfChar(AnsiChar(#0),4);
            Pointer((@TempStr[1])^) := pointer(fvar^.dta^);
          end;
          {$ENDIF}

        btProcPtr:
          begin
{$IFDEF FPC}
            MethodData := MKMethod(Self, Longint(FVar.Dta^));
            TempStr := StringOfChar(AnsiChar(#0),4);
            Pointer((@TempStr[1])^) := @MethodData;
{$ELSE}
            TempStr := StringOfChar(AnsiChar(#0),8);
            TMethod((@TempStr[1])^) := MKMethod(Self, Longint(FVar.Dta^));
            UseReg := false;
{$ENDIF}
          end;

        {$IFNDEF PS_NOINT64}bts64:
          begin
            TempStr:= StringOfChar(AnsiChar(#0),8);
            Int64((@TempStr[1])^) := int64(fvar^.dta^);
            UseReg := False;
        end;
        btU64:
          begin
            TempStr := StringOfChar(AnsiChar(#0),8);
            UInt64((@TempStr[1])^) := tbtu64(fvar^.dta^);
            UseReg := False;
        end;{$ENDIF}
      end; {case}
      if UseReg then
      begin
        case RegUsage of
          0: begin EAX := Longint((@Tempstr[1])^); Inc(RegUsage); end;
          1: begin EDX := Longint((@Tempstr[1])^); Inc(RegUsage); end;
          2: begin ECX := Longint((@Tempstr[1])^); Inc(RegUsage); end;
          else begin
          {$IFDEF FPC_OLD_FIX}
            if CallingConv = cdRegister then
            Stack := Stack + TempStr
          else
          {$ENDIF}
            Stack := TempStr + Stack;
          end;
        end;
      end else begin
        {$IFDEF FPC_OLD_FIX}
            if CallingConv = cdRegister then
            Stack := Stack + TempStr
          else
        {$ENDIF}
          Stack := TempStr + Stack;
      end;
    end;
    Result := True;
  end;
begin
  {$IFDEF FPC}
  if (Integer(CallingConv) and 128) <> 0 then begin
    IsVirtualCons := true;
    CAllingConv := TPSCallingConvention(Integer(CallingConv) and not 128);
  end else
    IsVirtualCons:= false;
  if (Integer(CallingConv) and 64) <> 0 then begin
    IsConstructor := true;
    CAllingConv := TPSCallingConvention(Integer(CallingConv) and not 64);
  end else
    IsConstructor := false;
  {$ENDIF}
  InnerfuseCall := False;
  if Address = nil then
    exit; // need address
  Stack := '';
  CallData := TPSList.Create;
  res := rp(res);
  if res <> nil then
    res.VarParam := true;
  try
    case CallingConv of
      cdRegister: begin
          EAX := 0;
          EDX := 0;
          ECX := 0;
          RegUsage := 0;

{$IFDEF FPC} // FIX FOR FPC constructor calls
          if IsConstructor then begin
            if not GetPtr(rp(Params[0])) then exit; // this goes first
            RegUsage := 2;
            EDX := Longint(_Self);
            DisposePPSVariantIFC(Params[0]);
            Params.Delete(0);
          end else
{$ENDIF}
          if assigned(_Self) then begin
            RegUsage := 1;
            EAX := Longint(_Self);
          end;

          for I := 0 to Params.Count - 1 do
          begin
            if not GetPtr(rp(Params[I])) then Exit;
          end;

          if assigned(res) then begin
            case res^.aType.BaseType of
              {$IFNDEF PS_NOWIDESTRING}btWideString, btUnicodeString, {$ENDIF}
              btInterface, {$IFNDEF FPC} btArray, {$ENDIF}{$IFNDEF PS_FPCSTRINGWORKAROUND}btstring, {$ENDIF}btVariant{$IFNDEF DELPHI_and_MSWINDOWS}, btStaticArray{$ENDIF}: GetPtr(res);
              btRecord{$IFDEF DELPHI_and_MSWINDOWS}, btStaticArray{$ENDIF}:
                begin
                  if ResultUsesResultPointer(res) then GetPtr(res);
                end;
              btSet:
                begin
                  if TPSTypeRec_Set(res.aType).aByteSize >4 then GetPtr(res);
                end;
            end;
            {$IFDEF DARWIN}
            if (length(Stack) mod 16) <> 0 then begin
              Stack := Stack + StringOfChar(ansichar(#32), 16 - (Length(Stack) mod 16)) ;
            end;
            {$ENDIF} 			
            case res^.aType.BaseType of
              btSet:
                begin
                  case TPSTypeRec_Set(res.aType).aByteSize  of
                    1: byte(res.Dta^) := RealCall_Register(Address, EAX, EDX, ECX, @Stack[Length(Stack)-3], Length(Stack) div 4, 1, nil);
                    2: word(res.Dta^) := RealCall_Register(Address, EAX, EDX, ECX, @Stack[Length(Stack)-3], Length(Stack) div 4, 2, nil);
                    3,
                    4: Longint(res.Dta^) := RealCall_Register(Address, EAX, EDX, ECX, @Stack[Length(Stack)-3], Length(Stack) div 4, 4, nil);
                    else RealCall_Register(Address, EAX, EDX, ECX, @Stack[Length(Stack)-3], Length(Stack) div 4, 0, nil)
                  end;
                end;
              btSingle:      tbtsingle(res.Dta^) := RealFloatCall_Register(Address, EAX, EDX, ECX, @Stack[Length(Stack)-3], Length(Stack) div 4);
              btDouble:      tbtdouble(res.Dta^) := RealFloatCall_Register(Address, EAX, EDX, ECX, @Stack[Length(Stack)-3], Length(Stack) div 4);
              btExtended:    tbtextended(res.Dta^) := RealFloatCall_Register(Address, EAX, EDX, ECX, @Stack[Length(Stack)-3], Length(Stack) div 4);
              btchar,btU8, btS8:    tbtu8(res.dta^) := RealCall_Register(Address, EAX, EDX, ECX, @Stack[Length(Stack)-3], Length(Stack) div 4, 1, nil);
              {$IFNDEF PS_NOWIDESTRING}btWideChar, {$ENDIF}btu16, bts16:  tbtu16(res.dta^) := RealCall_Register(Address, EAX, EDX, ECX, @Stack[Length(Stack)-3], Length(Stack) div 4, 2, nil);
              btClass :
              begin
              {$IF DEFINED (fpc) and (fpc_version < 3)}
                if IsConstructor or IsVirtualCons then
                  tbtu32(res.dta^) := RealCall_Register(Address, EDX, EAX, ECX,
                  @Stack[Length(Stack) - 3], Length(Stack) div 4, 4, nil)
                else
             {$IFEND}
                  tbtu32(res.dta^) := RealCall_Register(Address, EAX, EDX, ECX,
                  @Stack[Length(Stack) - 3], Length(Stack) div 4, 4, nil);
              end;

              btu32,bts32{$IFDEF FPC},btArray{$ENDIF}:   tbtu32(res.dta^) := RealCall_Register(Address, EAX, EDX, ECX, @Stack[Length(Stack)-3], Length(Stack) div 4, 4, nil);
              btPChar:       pansichar(res.dta^) := Pansichar(RealCall_Register(Address, EAX, EDX, ECX, @Stack[Length(Stack)-3], Length(Stack) div 4, 4, nil));
              {$IFNDEF PS_NOINT64}bts64, btU64:
                begin
                  EAX := RealCall_Register(Address, EAX, EDX, ECX, @Stack[Length(Stack)-3], Length(Stack) div 4, 4, @EDX);
                  if res^.aType.BaseType = btU64 then
                    tbtu64(res.dta^) := UInt64(Cardinal(EDX)) shl 32 or Cardinal(EAX)
                  else
                    tbts64(res.dta^) := Int64(Cardinal(EDX)) shl 32 or Cardinal(EAX);
                end;
              {$ENDIF}
              btCurrency:    tbtCurrency(res.Dta^) := RealFloatCall_Register(Address, EAX, EDX, ECX, @Stack[Length(Stack)-3], Length(Stack) div 4) / 10000;
              btInterface,
              {$IFNDEF PS_NOWIDESTRING}btWidestring,btUnicodestring, {$ENDIF}
              btVariant{$IFNDEF DELPHI_and_MSWINDOWS}, btStaticArray{$ENDIF}{$IFNDEF FPC}, btArray{$ENDIF}{$IFNDEF PS_FPCSTRINGWORKAROUND}, btstring{$ENDIF}: RealCall_Register(Address, EAX, EDX, ECX, @Stack[Length(Stack)-3], Length(Stack) div 4, 0, nil);
              btRecord{$IFDEF DELPHI_and_MSWINDOWS}, btStaticArray{$ENDIF}:
                begin
                  if not ResultUsesResultPointer(res) then
                    case res.aType.RealSize of
                      1: PByte(res.Dta)^ := RealCall_Register(Address, EAX, EDX, ECX, @Stack[Length(Stack)-3], Length(Stack) div 4, 1, nil);
                      2: PWord(res.Dta)^ := RealCall_Register(Address, EAX, EDX, ECX, @Stack[Length(Stack)-3], Length(Stack) div 4, 2, nil);
                    else
                      PCardinal(res.Dta)^ := RealCall_Register(Address, EAX, EDX, ECX, @Stack[Length(Stack)-3], Length(Stack) div 4, 4, nil);
                    end
                  else
                    RealCall_Register(Address, EAX, EDX, ECX, @Stack[Length(Stack)-3], Length(Stack) div 4, 0, nil);
                end;
              {$IFDEF PS_FPCSTRINGWORKAROUND}
              btstring: begin
                 eax := RealCall_Register(Address, EAX, EDX, ECX, @Stack[Length(Stack)-3], Length(Stack) div 4, 4, nil);
                 Longint(res.dta^) := eax;
              end;
              {$ENDIF}
            else
              exit;
            end;
          end else begin
            {$IFDEF DARWIN}
            if (length(Stack) mod 16) <> 0 then begin
              Stack := Stack + StringOfChar(ansichar(#32), 16 - (Length(Stack) mod 16)) ;
            end;
            {$ENDIF} 		  
            RealCall_Register(Address, EAX, EDX, ECX, @Stack[Length(Stack)-3], Length(Stack) div 4, 0, nil);
		  end;
          Result := True;
        end;
      cdPascal: begin
          RegUsage := 3;
          for I :=  0 to Params.Count - 1 do begin
            if not GetPtr(Params[i]) then Exit;
          end;
          if assigned(res) then begin
            case res^.aType.BaseType of
              {$IFNDEF PS_NOWIDESTRING}btWideString, btUnicodeString, {$ENDIF}btInterface, btArray, btString, btVariant: GetPtr(res);
              btRecord{$IFDEF DELPHI_and_MSWINDOWS}, btStaticArray{$ENDIF}:
                begin
                  if ResultUsesResultPointer(res) then GetPtr(res);
                end;
            end;
          end;
          if assigned(_Self) then begin
            Stack := StringOfChar(AnsiChar(#0),4) +Stack;
            Pointer((@Stack[1])^) := _Self;
          end;
          {$IFDEF DARWIN}
          if (length(Stack) mod 16) <> 0 then begin
            Stack := Stack + StringOfChar(ansichar(#32), 16 - (Length(Stack) mod 16)) ;
          end;
          {$ENDIF}
          if assigned(res) then begin
            case res^.aType.BaseType of
              btSingle:      tbtsingle(res^.Dta^) := RealFloatCall_Other(Address, @Stack[Length(Stack)-3], Length(Stack) div 4);
              btDouble:      tbtdouble(res^.Dta^) := RealFloatCall_Other(Address, @Stack[Length(Stack)-3], Length(Stack) div 4);
              btExtended:    tbtextended(res^.Dta^) := RealFloatCall_Other(Address, @Stack[Length(Stack)-3], Length(Stack) div 4);
              btCurrency:    tbtCurrency(res^.Dta^) := RealFloatCall_Other(Address, @Stack[Length(Stack)-3], Length(Stack) div 4) / 10000;
              btChar, btU8, btS8:    tbtu8(res^.Dta^) := RealCall_Other(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 1, nil);
              {$IFNDEF PS_NOWIDESTRING}btWideChar, {$ENDIF}btu16, bts16:  tbtu16(res^.Dta^) := RealCall_Other(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 2, nil);
              btClass, btu32, bts32:  tbtu32(res^.Dta^):= RealCall_Other(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 4, nil);
              btPChar:       pansichar(res^.dta^) := Pansichar(RealCall_Other(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 4, nil));
              {$IFNDEF PS_NOINT64}bts64, btU64:
                begin
                  EAX := RealCall_Other(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 4, @EDX);
                  if res^.aType.BaseType = btU64 then
                    tbtu64(res^.dta^) := UInt64(Cardinal(EDX)) shl 32 or Cardinal(EAX)
                  else
                    tbts64(res^.dta^) := Int64(Cardinal(EDX)) shl 32 or Cardinal(EAX);
                end;
              {$ENDIF}
              btVariant, {$IFNDEF PS_NOWIDESTRING}btUnicodeString, btWideString, {$ENDIF}
              btInterface, btArray, btstring: RealCall_Other(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 0, nil);
              btRecord{$IFDEF DELPHI_and_MSWINDOWS}, btStaticArray{$ENDIF}:
                begin
                  if not ResultUsesResultPointer(res) then
                    case res.aType.RealSize of
                      1: PByte(res.Dta)^ := RealCall_Other(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 1, nil);
                      2: PWord(res.Dta)^ := RealCall_Other(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 2, nil);
                    else
                      PCardinal(res.Dta)^ := RealCall_Other(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 4, nil);
                    end
                  else
                    RealCall_Other(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 0, nil);
                end;
            else
              exit;
            end;
          end else
            RealCall_Other(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 0, nil);
          Result := True;
        end;
      cdSafeCall: begin
          RegUsage := 3;
          if assigned(res) then begin
            GetPtr(res);
          end;
          for I :=  Params.Count - 1 downto 0 do begin
            if not GetPtr(Params[i]) then Exit;
          end;
          if assigned(_Self) then begin
            Stack := StringOfChar(AnsiChar(#0),4) +Stack;
            Pointer((@Stack[1])^) := _Self;
          end;
          {$IFDEF DARWIN}
          if (length(Stack) mod 16) <> 0 then begin
            Stack := Stack + StringOfChar(ansichar(#32), 16 - (Length(Stack) mod 16)) ;
          end;
          {$ENDIF}
          OleCheck(RealCall_Other(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 4, nil));
          Result := True;
        end;

      CdCdecl: begin
          RegUsage := 3;
          if assigned(_Self) then begin
            Stack := StringOfChar(AnsiChar(#0),4);
            Pointer((@Stack[1])^) := _Self;
          end;
          for I := Params.Count - 1 downto 0 do begin
            if not GetPtr(Params[I]) then Exit;
          end;
          {$IFDEF DARWIN}
          if (length(Stack) mod 16) <> 0 then begin
            Stack := Stack + StringOfChar(ansichar(#32), 16 - (Length(Stack) mod 16)) ;
          end;
          {$ENDIF}
          if assigned(res) then begin
            case res^.aType.BaseType of
              btSingle:      tbtsingle(res^.dta^) := RealFloatCall_Cdecl(Address, @Stack[Length(Stack)-3], Length(Stack) div 4);
              btDouble:      tbtdouble(res^.dta^) := RealFloatCall_Cdecl(Address, @Stack[Length(Stack)-3], Length(Stack) div 4);
              btExtended:    tbtextended(res^.dta^) := RealFloatCall_Cdecl(Address, @Stack[Length(Stack)-3], Length(Stack) div 4);
              btCurrency:    tbtCurrency(res^.dta^) := RealFloatCall_Cdecl(Address, @Stack[Length(Stack)-3], Length(Stack) div 4) / 10000;
              btCHar, btU8, btS8:    tbtu8(res^.dta^) := RealCall_Cdecl(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 1, nil);
              {$IFNDEF PS_NOWIDESTRING}btWideChar, {$ENDIF}btu16, bts16:  tbtu16(res^.dta^) := RealCall_Cdecl(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 2, nil);
              btClass, btu32, bts32:  tbtu32(res^.dta^) := RealCall_Cdecl(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 4, nil);
              btPChar:       pansichar(res^.dta^) := Pansichar(RealCall_Cdecl(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 4, nil));
              {$IFNDEF PS_NOINT64}bts64, btU64:
                begin
                  EAX := RealCall_CDecl(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 4, @EDX);
                  if res^.aType.BaseType = btU64 then
                    tbtu64(res^.Dta^) := UInt64(Cardinal(EDX)) shl 32 or Cardinal(EAX)
                  else
                    tbts64(res^.Dta^) := Int64(Cardinal(EDX)) shl 32 or Cardinal(EAX);
                end;
              {$ENDIF}
              btVariant, {$IFNDEF PS_NOWIDESTRING}btUnicodeString, btWideString, {$ENDIF}
              btInterface, btArray, btstring: begin GetPtr(res); RealCall_Cdecl(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 0, nil); end;
              btRecord{$IFDEF DELPHI_and_MSWINDOWS}, btStaticArray{$ENDIF}:
                begin
                  if ResultUsesResultPointer(res) then
                  begin
                    GetPtr(res);
                    RealCall_Cdecl(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 0, nil);
                  end
                  else
                    case res.aType.RealSize of
                      1: PByte(res.Dta)^ := RealCall_Cdecl(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 1, nil);
                      2: PWord(res.Dta)^ := RealCall_Cdecl(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 2, nil);
                    else
                      PCardinal(res.Dta)^ := RealCall_Cdecl(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 4, nil);
                    end;
                end;
            else
              exit;
            end;
          end else begin
            RealCall_CDecl(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 0, nil);
          end;
          Result := True;
        end;
      CdStdCall: begin
          RegUsage := 3;
          for I := Params.Count - 1 downto 0 do begin
            if not GetPtr(Params[I]) then exit;
          end;
          if assigned(_Self) then begin
            Stack := StringOfChar(AnsiChar(#0),4) + Stack;
            Pointer((@Stack[1])^) := _Self;
          end;
          if assigned(res) then begin
            case res^.aType.BaseType of
              btSingle:  tbtsingle(res^.dta^) := RealFloatCall_Other(Address, @Stack[Length(Stack)-3], Length(Stack) div 4);
              btDouble:      tbtdouble(res^.dta^) := RealFloatCall_Other(Address, @Stack[Length(Stack)-3], Length(Stack) div 4);
              btExtended:    tbtextended(res^.dta^):= RealFloatCall_Other(Address, @Stack[Length(Stack)-3], Length(Stack) div 4);
              btCurrency:    tbtCurrency(res^.dta^) := RealFloatCall_Other(Address, @Stack[Length(Stack)-3], Length(Stack) div 4) / 10000;
              btChar, btU8, btS8:    tbtu8(res^.dta^) := RealCall_Other(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 1, nil);
              {$IFNDEF PS_NOWIDESTRING}btWideChar, {$ENDIF}btu16, bts16:  tbtu16(res^.dta^) := RealCall_Other(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 2, nil);
              btclass, btu32, bts32:  tbtu32(res^.dta^) := RealCall_Other(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 4, nil);
              btPChar:       pansichar(res^.dta^) := Pansichar(RealCall_Other(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 4, nil));
              {$IFNDEF PS_NOINT64}bts64, btU64:
                begin
                  EAX := RealCall_Other(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 4, @EDX);
                  if res^.aType.BaseType = btU64 then
                    tbtu64(res^.dta^) := UInt64(Cardinal(EDX)) shl 32 or Cardinal(EAX)
                  else
                    tbts64(res^.dta^) := Int64(Cardinal(EDX)) shl 32 or Cardinal(EAX);
                end;
              {$ENDIF}
              btVariant, {$IFNDEF PS_NOWIDESTRING}btUnicodeString, btWideString, {$ENDIF}
              btInterface, btArray, btstring: begin GetPtr(res); RealCall_Other(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 0, nil); end;
              btRecord{$IFDEF DELPHI_and_MSWINDOWS}, btStaticArray{$ENDIF}:
                begin
                  if ResultUsesResultPointer(res) then
                  begin
                    GetPtr(res);
                    RealCall_Other(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 0, nil);
                  end
                  else
                    case res.aType.RealSize of
                      1: PByte(res.Dta)^ := RealCall_Other(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 1, nil);
                      2: PWord(res.Dta)^ := RealCall_Other(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 2, nil);
                    else
                      PCardinal(res.Dta)^ := RealCall_Other(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 4, nil);
                    end;
                end;
            else
              exit;
            end;
          end else begin
            RealCall_Other(Address, @Stack[Length(Stack)-3], Length(Stack) div 4, 0, nil);
          end;
          Result := True;
        end;
    end;
  finally
    for i := CallData.Count -1 downto 0 do
    begin
      pp := CallData[i];
      case pp^ of
        0: DestroyOpenArray(Self, Pointer(pp));
      end;
    end;
    CallData.Free;
  end;
end;
