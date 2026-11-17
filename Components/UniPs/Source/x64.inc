{ implementation of x64 abi }
//procedure DebugBreak; external 'Kernel32.dll';
const
  EmptyPchar: array[0..0] of char = #0;

type
  TRegisters = packed record
    _RCX,                  // 0
    _RDX,                  // 8
    _R8,                   // 16
    _R9: IPointer;         // 24
    _XMM1,                 // 32
    _XMM2,                 // 40
    _XMM3: Double;         // 48
    Stack: Pointer;        // 56
    Items: IntPtr; // 64
    SingleBits: Integer; // 72
  end;

type
  EInvocationError = class(Exception);

procedure x64call({RCX}Address: Pointer;out {RDX}_RAX: IPointer;var {R8}_XMM0: Double; var {R9}RegInfo: TRegisters);
assembler;

  procedure InvokeErrorParamCount; // The number of parameters exceeded 64 (c_par_max_count)
  begin
    raise EInvocationError.Create('Internal: Parameter count exceeded 64');
  end;

const
  c_par_max_count    = 64;                                   // == $40

  c_sz_ptr           = SizeOf(Pointer);                      // == 8
  c_arg_count        = 4;                                    // == 4
  c_arg_size         = c_arg_count * c_sz_ptr;               // == 32
  c_loc_count        = c_par_max_count + 4;                  // == 68
  c_params           = c_loc_count - c_arg_count + 1;        // == 65

  c_loc_offs_adress  = c_loc_count * c_sz_ptr + c_arg_size;  // == 576
  c_loc_offs__rax    = c_loc_offs_adress + 1*c_sz_ptr;       // == 584
  c_loc_offs__xmm0   = c_loc_offs_adress + 2*c_sz_ptr;       // == 592
  c_loc_offs_reginfo = c_loc_offs_adress + 4*c_sz_ptr;       // == 608

  //c_reg_packed = ((SizeOf(TRegisters) mod c_sz_ptr) + c_sz_ptr - 1) div c_sz_ptr; // 1 - packed records; 0 - aligned
asm
  .params c_params // There's actually room for c_loc_count, assembler is saving locals for "Address,_RAX,_XMM0,RegInfo"
                                             //                         dbg: in:  TRegisters(pointer(R9)^),r
  mov     [rbp+c_loc_offs_adress], Address   // save: rcx (@Address)    dbg: loc: qword(pointer(rbp+c_loc_offs_adress)^)   ; in: qword(pointer(RCX))
  mov     [rbp+c_loc_offs__rax], _RAX        // save: rdx (@_RDX)       dbg: loc: qword(pointer(rbp+c_loc_offs__rax)^)     ; in: qword(pointer(RDX))
  mov     [rbp+c_loc_offs__xmm0], _XMM0      // save: r8  (@_XMM0)      dbg: loc: qword(pointer(rbp+c_loc_offs__xmm0)^)    ; in: qword(pointer(R8))
  mov     [rbp+c_loc_offs_reginfo], RegInfo  // save: r9  (@RegInfo)    dbg: loc: qword(pointer(rbp+c_loc_offs_reginfo)^)  ; in: qword(pointer(R9))
                                             //                         dbg:      TRegisters(pointer(pointer(rbp+c_loc_offs_reginfo)^)^),r
  //                                                                    dbg:      tdatetime(pointer(pointer(rbp+c_loc_offs__xmm0)^)^)
  // check Registers.Items (param count limitation)
  //
  mov     rcx, [RegInfo].TRegisters.Items
  test    rcx, rcx
  jz      @@skip_items

  cmp     rcx, c_par_max_count-2
  jbe     @@frame_is_try

  call    InvokeErrorParamCount

@@frame_is_try:
  //
  // copy registers:
  //   source  :  [RegInfo].TRegisters.Stack
  //   dest    :  [rbp + c_arg_count*c_sz_ptr]
  //   count   :  rcx
  //   note    :  All items on stack should be 16 byte aligned. Caller should have handled that, just copy data here.
  //
  mov     r8, [RegInfo].TRegisters.Stack    // source
  lea     rdx, [rbp + c_arg_count*c_sz_ptr] // dest
@copy_loop:
  // copy pointer:
  mov     rax, [r8]
  mov     [rdx], rax
  // next:
  add     r8, c_sz_ptr
  add     rdx, c_sz_ptr
  dec     rcx
  or      rcx, rcx
  jnz     @copy_loop
  // next.
  //
  // copy registers.

@@skip_items:

  // copy registers, handling SingleBits for Single parameters
  mov ecx, dword ptr [RegInfo + offset(TRegisters.SingleBits)]

  // fill: _XMM1
  bt ecx, 1
  jnc @g1
  cvtsd2ss xmm1, [RegInfo + offset(TRegisters._XMM1)]
  jmp @g1e
  @g1:
  movsd xmm1, [RegInfo + offset(TRegisters._XMM1)]
  @g1e:

  // fill: _XMM2
  bt ecx, 2
  jnc @g2
  cvtsd2ss xmm2, [RegInfo + offset(TRegisters._XMM2)]
  jmp @g2e
  @g2:
  movsd xmm2, [RegInfo + offset(TRegisters._XMM2)]
  @g2e:

  // fill: _XMM3
  bt ecx, 3
  jnc @g3
  cvtsd2ss xmm3, [RegInfo + offset(TRegisters._XMM3)]
  jmp @g3e
  @g3:
  movsd xmm3, [RegInfo + offset(TRegisters._XMM3)]
  @g3e:

  // fill: _XMM0
  bt ecx, 0
  jnc @g0
  mov rdx, [rbp+c_loc_offs__xmm0]
  cvtsd2ss xmm0, [rdx]
  jmp @g0e
  @g0:
  mov rdx, [rbp+c_loc_offs__xmm0]
  movsd xmm0, [rdx]
  @g0e:

  // mov *, [r9].*    ;    [r9] == [RegInfo]
  mov     rcx, [RegInfo].TRegisters._RCX
  mov     rdx, [RegInfo].TRegisters._RDX
  mov     r8,  [RegInfo].TRegisters._R8

  mov     r9,  [RegInfo].TRegisters._R9  // !!! Overwritten RegInfo (r9)

  call    [rbp+c_loc_offs_adress]

  // make result
  mov     rdx, [rbp+c_loc_offs__rax] // restore: rdx (@_RDX)
  mov     [rdx], RAX              // fill: _RAX

  //movsd   [rdx+c_sz_ptr], XMM0  // fill: _XMM0
  mov     rdx, [rbp+c_loc_offs__xmm0] // restore: r8 (@_RMM0)

  // handle SingleBits result
  mov     rax, [rbp+c_loc_offs_reginfo] // restore: RegInfo pointer from stack
  bt      dword ptr [rax + offset(TRegisters.SingleBits)], 8
  jnc     @no_single_res
  cvtss2sd xmm1, xmm0
  movsd   [rdx], xmm1            // fill: _XMM0 for single result
  jmp     @res_done
@no_single_res:
  movsd   [rdx], XMM0            // fill: _XMM0
@res_done:
end;

function TPSExec.InnerfuseCall(_Self, Address: Pointer; CallingConv: TPSCallingConvention; Params: TPSList; res: PPSVariantIFC): Boolean;
var
  Stack: array of Byte;
  _RAX: IPointer;
_XMM0: Double;
  Registers: TRegisters;
  RegUsage: Byte;
  CallData: TPSList;
  I: Integer;
  pp: ^Byte;
  IsSafeCall: Boolean;

  function rp(p: PPSVariantIFC): PPSVariantIFC;
  begin
    if (p = nil) or (p.aType = nil) then
    begin
      result := nil;
      exit;
    end;
    if p.aType.BaseType = btPointer then
    begin
      p^.aType := Pointer(Pointer(IPointer(p^.dta) + PointerSize)^);
      p^.Dta := Pointer(p^.dta^);
    end;
    Result := p;
  end;

  function ResultUsesResultPointer(res: PPSVariantIFC): Boolean;
  begin
    { Delphi Win64 ABI: a record result is returned in RAX at sizes 1, 2, and
      4, and at size 8 only when the record contains no managed field; all
      other sizes (3, 5, 6, 7, and > 8) and the managed 8-byte record are
      written through a hidden result pointer (see System.Rtti's
      UseResultPointer). Static arrays follow the same rule: UseResultPointer
      treats tkArray like tkRecord. }
    if res^.aType.BaseType in [btRecord, btStaticArray] then
      Result := not (res^.aType.RealSize in [1, 2, 4]) and
        ((res^.aType.RealSize <> 8) or IsManagedType(res^.aType))
    else
      Result := res^.aType.RealSize > PointerSize;
  end;
  procedure StoreReg(data: IPointer);   overload;
  var p: Pointer;
  begin
    case RegUsage of
      0: begin inc(RegUsage); Registers._RCX:=Data; end;
      1: begin inc(RegUsage); Registers._RDX:=Data; end;
      2: begin inc(RegUsage); Registers._R8:=Data; end;
      3: begin inc(RegUsage); Registers._R9:=Data; end;
    else begin
      SetLength(Stack, Length(Stack)+8);
      p := @Stack[LEngth(Stack)-8];
      IPointer(p^) := data;
    end;
    end;
  end;

  procedure StoreReg(data: Double); overload;
  var p: Pointer;
  begin
    case RegUsage of
      0: begin inc(RegUsage); _XMM0:=Data; end;
      1: begin inc(RegUsage); Registers._XMM1:=Data; end;
      2: begin inc(RegUsage); Registers._XMM2:=Data; end;
      3: begin inc(RegUsage); Registers._XMM3:=Data; end;
    else begin
      SetLength(Stack, Length(Stack)+8);
      p := @Stack[LEngth(Stack)-8];
      Double(p^) := data;
    end;
    end;
  end;
  procedure StoreReg(data: Single); overload;
  var p: Pointer;
  begin
    case RegUsage of
      0: begin inc(RegUsage); Registers.SingleBits := Registers.SingleBits or 1;_XMM0:=Data; end;
      1: begin inc(RegUsage); Registers.SingleBits := Registers.SingleBits or 2; Registers._XMM1:=Data; end;
      2: begin inc(RegUsage); Registers.SingleBits := Registers.SingleBits or 4;Registers._XMM2:=Data; end;
      3: begin inc(RegUsage); Registers.SingleBits := Registers.SingleBits or 8; Registers._XMM3:=Data; end;
    else begin
      SetLength(Stack, Length(Stack)+8);
      p := @Stack[LEngth(Stack)-8];
      Single(p^) := data;
    end;
    end;
  end;

  procedure StoreRegBytes(const Data: Pointer; const Size: Longint);
  var
    RegData: IPointer;
  begin
    RegData := 0;
    Move(Data^, RegData, Size);
    StoreReg(RegData);
  end;
  
  type
    TMethodCallData = record
      AType: Byte; { Must be first field, matching uPSRuntime's TOpenArray }
      Data: TMethod;
    end;
    PMethodCallData = ^TMethodCallData;

  function GetPtr(fVar: PPSVariantIFC): Boolean;
  var
    varPtr: Pointer;
    //UseReg: Boolean;
    //tempstr: tbtstring;
    p: Pointer;
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
              if CallData = nil then
                CallData := TPSList.Create;
              CallData.Add(p);
              StoreReg(IPointer(POpenArray(p)^.Data));
              StoreReg(IPointer(POpenArray(p)^.ItemCount -1));
              Result := True;
              Exit;
            end else begin
              varptr := fvar.Dta;
//              Exit;
            end;
          end;
        btVariant,
        btSet,
        btStaticArray,
        btRecord,
        btInterface,
        btClass,
        btUnicodeString, btWideString, btWideChar, btU8, btS8, btU16,
        btS16, btU32, btS32, btSingle, btDouble, btExtended, btString, btPChar, btChar, btCurrency,
        bts64, btU64:
          begin
            Varptr := fvar.Dta;
          end;
      else begin
          exit; //invalid type
        end;
      end; {case}

      StoreReg(IPointer(VarPtr));
    end else begin
//      UseReg := True;
      case fVar^.aType.BaseType of
        btArray:
          begin
            if Copy(fvar^.aType.ExportName, 1, 10) = '!OPENARRAY' then
            begin
              p := CreateOpenArray(False, SElf, FVar);
              if p =nil then exit;
              if CallData = nil then
                CallData := TPSList.Create;
              CallData.Add(p);
              StoreReg(IPointer(POpenArray(p)^.Data));
              StoreReg(IPointer(POpenArray(p)^.ItemCount -1));
              Result := True;
              exit;
            end else begin
              StoreReg(IPointer(FVar.Dta^));
            end;
          end;
        btSet,
        btRecord,
        btStaticArray:
          begin
            { Delphi Win64 ABI: a record or static array is passed by value (in
              a register) at sizes 1, 2, or 4 and for stdcall/cdecl/safecall
              also at size 8; all other sizes (3, 5, 6, 7, and > 8) are passed
              by reference. A set follows the same rules, except that it has no
              8-byte stdcall/cdecl/safecall by-value case (not in the blog
              below, found empirically; likely because C has no sets) and that Delphi
              rounds set storage up (a native set is never 3 bytes, and on Win64
              never 5-7 bytes), so a 3-byte ROPS set is a native 4-byte set
              passed by value, and a 5-8 byte ROPS set is a native 8-byte set
              passed by reference. See
              https://blogs.embarcadero.com/abi-changes-in-rad-studio-10-3/ }
            if (fvar^.aType.RealSize in [1, 2, 4]) or
               ((fvar^.aType.BaseType = btSet) and (fvar^.aType.RealSize = 3)) or
               ((fvar^.aType.BaseType <> btSet) and (fvar^.aType.RealSize = 8) and (CallingConv in [cdCdecl, cdStdCall, cdSafeCall]))
               then
              StoreRegBytes(fvar.Dta, fvar^.aType.RealSize)
            else
              StoreReg(IPointer(fVar.Dta));
          end;
        btVariant:
          begin
            StoreReg(IPointer(fVar.Dta));
          end;
        btExtended, btDouble: {8 bytes} begin
            StoreReg(double(fvar.dta^));
          end;
        btCurrency: {8 bytes} begin
            StoreReg(IPointer(fvar.dta^));
          end;
        btSingle: {4 bytes} begin
            StoreReg(single(fvar.dta^));
          end;

        btChar,
        btU8,
        btS8: begin
            StoreReg(IPointer(byte(fVar^.dta^)));
          end;
        btWideChar,
        btu16, btS16: begin
            StoreReg(IPointer(word(fVar^.dta^)));
          end;
        btu32, bts32: begin
            StoreReg(IPointer(cardinal(fVar^.dta^)));
          end;
        btPchar:
          begin
            if pointer(fvar^.dta^) = nil then
              StoreReg(IPointer(@EmptyPchar))
            else
              StoreReg(IPointer(fvar^.dta^));
          end;
        btclass, btinterface, btString:
          begin
            StoreReg(IPointer(fvar^.dta^));
          end;
        btWideString: begin
            StoreReg(IPointer(fvar^.dta^));
          end;
        btUnicodeString: begin
            StoreReg(IPointer(fvar^.dta^));
          end;

        btProcPtr:
          begin
            p := New(PMethodCallData);
            TMethodCallData(p^).AType := 255;
            if CallData = nil then
              CallData := TPSList.Create;
            CallData.Add(p);
            // Uncomment when Handler will be implemented
            //TMethodCallData(p^).Data.Code:=nil;
            //TMethodCallData(p^).Data.Data:=nil;
            TMethodCallData(p^).Data := MKMethod(Self, Longint(fvar.dta^)); // It requires the implementation of "Handled"
            StoreReg(IPointer(@TMethodCallData(p^).Data));
          end;
        bts64:
          begin
            StoreReg(IPointer(int64(fvar^.dta^)));
        end;
        btU64:
          begin
            StoreReg(IPointer(tbtu64(fvar^.dta^)));
        end;
      end; {case}
    end;
    Result := True;
  end;
begin
  IsSafeCall := CallingConv = cdSafeCall;

  InnerfuseCall := False;
  if Address = nil then
    exit; // need address
  SetLength(Stack, 0);
  CallData := nil;
  res := rp(res);
  if res <> nil then
    res.VarParam := true;
  try
  _XMM0 := 0;
  FillChar(Registers, Sizeof(REgisters), 0);
    _RAX := 0;
    RegUsage := 0;
    if assigned(_Self) then begin
      StoreReg(IPointer(_Self));
    end;
    if assigned(res) and not IsSafeCall and (res^.atype.basetype = btSingle) then begin
      Registers.Singlebits  := Registers.Singlebits or 256;
    end;
   if assigned(res) and not IsSafeCall then begin
    case res^.aType.BaseType of
      btstring, btWideString, btUnicodeString,
      btInterface, btArray, btVariant:
        GetPtr(res);
      btRecord,
      btSet, btStaticArray:
        begin
          if ResultUsesResultPointer(res) then GetPtr(res);
        end;
    end;
   end;
    for I := 0 to Params.Count - 1 do
    begin
      if not GetPtr(rp(Params[I])) then Exit;
    end;
    if assigned(res) and IsSafeCall then
      GetPtr(res); { SafeCall: res is actually passed as last parameter }
    if assigned(res) and not IsSafeCall then begin
      if (length(Stack) mod 16) <> 0 then begin
        SetLength(Stack, Length(Stack)+16 - (Length(Stack) mod 16));
      end;
      if Stack = nil then pp := nil else pp := @Stack[0];
      Registers.Stack := pp;
      Registers.Items := Length(Stack) div 8;
      x64call(Address, _RAX, _XMM0, Registers);
      case res^.aType.BaseType of
        btRecord, btSet, btStaticArray:
          begin
            { A result that used the hidden result pointer is already in place }
            if not ResultUsesResultPointer(res) then
            begin
              case res.aType.RealSize of
                1: byte(res.Dta^) := _RAX;
                2: word(res.Dta^) := _RAX;
                4: Longint(res.Dta^) := _RAX;
                8: IPointer(res.dta^) := _RAX;
                3, 5, 6, 7: Move(_RAX, res.Dta^, res.aType.RealSize);
              end;
            end;
          end;
        btSingle:      tbtsingle(res.Dta^) := _XMM0;
        btDouble:      tbtdouble(res.Dta^) := _XMM0;
        btExtended:    tbtextended(res.Dta^) := _XMM0;
        btchar,btU8, btS8:    tbtu8(res.dta^) := _RAX;
        btWideChar, btu16, bts16:  tbtu16(res.dta^) := _RAX;
        btClass : IPointer(res.dta^) := _RAX;
        btu32,bts32:   tbtu32(res.dta^) := _RAX;
        btPChar:       pansichar(res.dta^) := Pansichar(_RAX);
        bts64: tbts64(res.dta^) := Int64(_RAX);
        btU64: tbtu64(res.dta^) := UInt64(_RAX);
        btCurrency:    tbts64(res.Dta^) := Int64(_RAX); { Currency is returned in RAX as its scaled Int64 backing, not its logical value }
        btInterface,
        btVariant,
        btWidestring,btUnicodestring, btstring ,
        btArray:;
      else
        exit;
      end;
    end else begin
      if (length(Stack) mod 16) <> 0 then begin
        SetLength(Stack, Length(Stack)+16 - (Length(Stack) mod 16));
      end;
	if Stack = nil then pp := nil else pp := @Stack[0];
        Registers.Stack := pp;
        Registers.Items := Length(Stack) div 8;
        x64call(Address, _RAX, _XMM0, Registers);
      if IsSafeCall then
        OleCheck(_RAX);
    end;
    Result := True;
  finally
    if CallData <> nil then begin
      for i := CallData.Count -1 downto 0 do
      begin
        pp := CallData[i];
        case pp^ of
          0: DestroyOpenArray(Self, Pointer(pp));
          255: Dispose(PMethodCallData(pp));
        end;
      end;
      CallData.Free;
    end;
  end;
end;


