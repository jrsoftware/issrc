{$IFDEF DELPHI}
  {$DEFINE PS_RESBEFOREPARAMETERS}
  {$DEFINE x64_string_result_as_varparameter}
{$ENDIF}

{ implementation of x64 abi }
//procedure DebugBreak; external 'Kernel32.dll';
const
  EmptyPchar: array[0..0] of char = #0;
{$IFDEF FPC}
{$ASMMODE INTEL}
{$ENDIF}
{$IFDEF MSWINDOWS}{$DEFINE WINDOWS}{$ENDIF}

{$UNDEF REG_STACK_PTR_OFFSET0}

{$IFDEF WINDOWS}
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
    Items: {$IFDEF FPC}PtrUInt{$ELSE}IntPtr{$ENDIF}; // 64
    SingleBits: Integer; // 72
  end;

{$IFDEF DELPHI}
  {$DEFINE REG_STACK_PTR_OFFSET0}
{$ENDIF}

{$IFDEF REG_STACK_PTR_OFFSET0} // ! NEW asm-win-x64 version for Delphi, no GPF on exception !
type
  EInvocationError = class(Exception);

procedure x64call({RCX}Address: Pointer;out {RDX}_RAX: IPointer;var {R8}_XMM0: Double; var {R9}RegInfo: TRegisters);
assembler; {$ifdef FPC}nostackframe;{$endif}

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
{$ifdef DELPHI}
  .params c_params // There's actually room for c_loc_count, assembler is saving locals for "Address,_RAX,_XMM0,RegInfo"
{$else} // TODO: FPC: GPF on exception
  push    rbp
  sub     rsp, $210  // ? $268
  mov     rbp, rsp
{$endif}
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

{$ifndef DELPHI}
  lea     rsp, [rbp+$210]
  pop     rbp

  ret
{$endif}
end;

{$ELSE} // ! OLD asm-win-x64 version ! GPF on exception !

procedure x64call(
  Address: Pointer;
  out _RAX: IPointer;
  var _XMM0: Double;
  var Registers: TRegisters); assembler; {$IFDEF FPC}nostackframe;{$ENDIF}
asm
(* Registers:
    RCX: Address
    RDX: *_RAX
    R8:  * _XMM0
    R9: _REGISTERS
    fpc inserts an 20h empty space
*)
//{$IFDEF FPC}
  push rbp
  mov rbp,rsp
//{$ENDIF}
  push rcx  // address         ;rbp -8
  push rdx  // @_rax           ;rbp -16
  push r8   // @_xmm0          ;rbp -24
  push r9   // _registers      ;rbp -32

  mov rax, [rbp-32] //registers

  mov rcx, [rax+64] // items/count
  mov rdx, [rax+56] // stack
  jmp @compareitems
@work:
{$IFDEF FPC}
  push qword ptr [rdx]
{$ELSE}
  push [rdx]
{$ENDIF}
  dec rcx
  sub rdx,8
@compareitems:
  or rcx, rcx
  jnz @work

  // copy registers
  mov rcx, [rax+72] // single bits

  bt rcx, 1
  jnc @g1
  cvtsd2ss xmm1, [rax+32]
  jmp @g1e
  @g1:
  movsd xmm1, [rax+32]
  @g1e:


  bt rcx, 2
  jnc @g2
  cvtsd2ss xmm2, [rax+40]
  jmp @g2e
  @g2:
  movsd xmm2, [rax+40]
  @g2e:

  bt rcx, 3
  jnc @g3
  cvtsd2ss xmm3, [rax+48]
  jmp @g3e
  @g3:
  movsd xmm3, [rax+48]
  @g3e:



  // rbp-16: address of xmm0

  bt rcx, 0
  jnc @g0
  mov rdx, [rbp -24]
  cvtsd2ss xmm0, [rdx]
  jmp @g0e
  @g0:
  mov rdx, [rbp -24]
  movsd xmm0, [rdx]
  @g0e:

  // other registers
  mov rcx, [rax]
  mov rdx, [rax+8]
  mov r8, [rax+16]
  mov r9, [rax+24]


  mov RAX, [rbp-8]

  // weird thing on windows, it needs 32 bytes in the CALLEE side to do whatever in
  sub RSP, 32

  call RAX

  add RSP, 32 // undo the damage done earlier

  // copy result back
  mov RDX, [rbp-16]
  mov [RDX], RAX

  mov rax, [rbp-32] //registers

  bt [rax+72], 8                 // if atype.basetype  <> btSingle
  jnc @g5                        //
  cvtss2sd xmm1,xmm0             // convert single to double  into xmm1
  mov rdx,[rbp-24]               // @_xmm0  ;rbp -24
  movsd qword ptr [rdx], xmm1    // save  xmm1 to param _xmm0
  jmp @g5e                       // exit if atype.basetype  = btSingle

  @g5:                           //else "if atype.basetype  = btSingle"
    mov rdx,[rbp-24]             // @_xmm0  ;rbp -24
    movsd qword ptr [rdx], xmm0  // save  xmm1 to param _xmm0

  @g5e:


  leave
  ret
end;
{$ENDIF}

{$ELSE} //not MSWINDOWS
type
  TRegisters = packed record
    _RDI,               //  0
    _RSI,               //  8
    _RDX,               // 16
    _RCX,               // 24
    _R8,                // 32
    _R9: IPointer;      // 40
    _XMM1,              // 48
    _XMM2,              // 56
    _XMM3,              // 64
    _XMM4,              // 72
    _XMM5,              // 80
    _XMM6,              // 88
    _XMM7: Double;      // 96
    SingleBits: Integer; //104
  end;

procedure x64call(
  Address: Pointer;
  out _RAX: IPointer;

  var Registers: TRegisters;
  aStack: Pointer; aItems: Integer; var _XMM0: Double); assembler; nostackframe;


asm
(* Registers:
    RDI: Address
    RSI: _RAX
    RDX: Registers
    RCX: aStack
    R8:  aItems
    R9:  XMM0

    rbp-8    addr
    rbp-16   _rax
    rbp-24   _xmm0
    rbp-32   regs
*)
  push rbp
  mov rbp,rsp
  push rdi  // address
  push rsi  // _rax
  push r9   // xmm0
  push rdx
{$IFDEF PS_STACKALIGN}
  bt r8, 0
  jnc @skipjump
  sub rsp, 8
@skipjump:
{$ENDIF}
  mov rax, rdx
  jmp @compareitems
@work:
{$IFDEF FPC}
  push qword ptr [rcx]
{$ELSE} 
  push [rcx]
{$ENDIF}  
  dec r8
  sub rcx,8
@compareitems:
  or r8, r8
  jnz @work

  // copy registers
  // xmm0
  mov rdx,[rbp-24]
  bt [rax+104], 0
  jnc @skipxmm0
  cvtsd2ss xmm0,[rdx]
  jmp @skipxmm0re
  @skipxmm0:
  movq xmm0,[rdx]            // move quadword to xmm0 from _XMM0
  @skipxmm0re:

  // xmm1
  bt [rax+104], 1
  jnc @skipxmm1
  cvtsd2ss xmm1,[rax+48]
  jmp @skipxmm1re
  @skipxmm1:
  movq xmm1,[rax+48]         // move quadword to xmm1 from Registers._XMM1
  @skipxmm1re:

  // xmm2
  bt [rax+104], 2
  jnc @skipxmm2
  cvtsd2ss xmm2,[rax+56]
  jmp @skipxmm2re
  @skipxmm2:
  movq xmm2,[rax+56]         // move quadword to xmm2 from Registers._XMM2
  @skipxmm2re:

  // xmm3
  bt [rax+104], 3
  jnc @skipxmm3
  cvtsd2ss xmm3,[rax+64]
  jmp @skipxmm3re
  @skipxmm3:
  movq xmm3,[rax+64]         // move quadword to xmm3 from Registers._XMM3
  @skipxmm3re:

  // xmm4
  bt [rax+104], 4
  jnc @skipxmm4
  cvtsd2ss xmm4,[rax+72]
  jmp @skipxmm4re
  @skipxmm4:
  movq xmm4,[rax+72]         // move quadword to xmm4 from Registers._XMM4
  @skipxmm4re:

  // xmm5
  bt [rax+104], 5
  jnc @skipxmm5
  cvtsd2ss xmm5,[rax+80]
  jmp @skipxmm5re
  @skipxmm5:
  movq xmm5,[rax+80]         // move quadword to xmm5 from Registers._XMM5
  @skipxmm5re:

  // xmm6
  bt [rax+104], 6
  jnc @skipxmm6
  cvtsd2ss xmm6,[rax+88]
  jmp @skipxmm6re
  @skipxmm6:
  movq xmm6,[rax+88]         // move quadword to xmm6 from Registers._XMM6
  @skipxmm6re:

// xmm7
  bt [rax+104], 7
  jnc @skipxmm7
  cvtsd2ss xmm7,[rax+96]
  jmp @skipxmm7re
  @skipxmm7:
  movq xmm7,[rax+96]         // move quadword to xmm7 from Registers._XMM7
  @skipxmm7re:


  mov RDI, [rax]
  mov RSI, [rax+ 8]
  mov RDX, [rax+16]
  mov RCX, [rax+24]
  mov R8,  [rax+32]
  mov R9,  [rax+40]

  // weird thing on windows, it needs 32 bytes in the CALLEE side to do whatever in; not sure about linux
  //sub RSP, 32

  mov rax, [rbp-8]
  call RAX

//  add rsp, 8

  // add RSP, 32 // undo the damage done earlier

  // copy result back
  mov rsi, [rbp-16]          // _RAX parameter
  mov [rsi], RAX
  
{$IFNDEF WINDOWS}
  mov rsi, [rbp-32]          // Registers parameter
  mov [rsi+16], RDX          // preserve RDX for 9..16 byte SYSV aggregate results
{$ENDIF}

  mov rsi, [rbp-24]          // _XMM0 parameter

  // xmm0 res
  mov rax, [rbp-32]          // Registers parameter
  bt [rax+104], 8            // if atype.basetype  <> btSingle
  jnc @skipres               // then goto skipres else begin
  cvtss2sd xmm1,xmm0         // convert single to double  into xmm1
  movq [rsi],xmm1            // move quadword to _XMM0
  jmp @skipresre             // end
  @skipres:
  movq [rsi],xmm0            // move quadword to _XMM0
  @skipresre:


  pop rdx
  pop r9   // xmm0
  pop rsi  // _rax
  pop rdi  // address
  leave
  ret
end;
{$ENDIF}

function TPSExec.InnerfuseCall(_Self, Address: Pointer; CallingConv: TPSCallingConvention; Params: TPSList; res: PPSVariantIFC): Boolean;
var
  Stack: array of Byte;
  _RAX: IPointer;
_XMM0: Double;
  Registers: TRegisters;
{$IFNDEF WINDOWS}
  RegUsageFloat: Byte;
{$ENDIF}
{$IFDEF FPC}
  IsConstructor,IsVirtualCons: Boolean;
{$ENDIF}
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
{$IFDEF WINDOWS}
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
  {$ELSE}
  procedure StoreReg(data: IPointer);   overload;
  var p: Pointer;
  begin
    case RegUsage of
      0: begin inc(RegUsage); Registers._RDI:=Data; end;
      1: begin inc(RegUsage); Registers._RSI:=Data; end;
      2: begin inc(RegUsage); Registers._RDX:=Data; end;
      3: begin inc(RegUsage); Registers._RCX:=Data; end;
      4: begin inc(RegUsage); Registers._R8:=Data; end;
      5: begin inc(RegUsage); Registers._R9:=Data; end;
    else begin
      SetLength(Stack, Length(Stack)+8);
      p := @Stack[LEngth(Stack)-8];
      IPointer(p^) := data;
    end;
    end;
  end;
{$ENDIF}

{$IFNDEF DELPHI}
  procedure StoreStack(const aData; Len: Integer);
  var
    p: Pointer;
  begin
    if Len > 8 then
      if Length(Stack) mod 16 <> 0 then begin
        SetLength(Stack, Length(Stack)+ (16-(Length(Stack) mod 16)));
      end;
    SetLength(Stack, Length(Stack)+Len);
    p := @Stack[Length(Stack)-Len];
    Move(aData, p^, Len);
  end;
{$ENDIF}

{$IFDEF WINDOWS}
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
  {$ELSE}
  procedure StoreReg(data: Double); overload;
  var p: Pointer;
  begin
    case RegUsageFloat of
      0: begin inc(RegUsageFloat); _XMM0:=Data; end;
      1: begin inc(RegUsageFloat); Registers._XMM1:=Data; end;
      2: begin inc(RegUsageFloat); Registers._XMM2:=Data; end;
      3: begin inc(RegUsageFloat); Registers._XMM3:=Data; end;
      4: begin inc(RegUsageFloat); Registers._XMM4:=Data; end;
      5: begin inc(RegUsageFloat); Registers._XMM5:=Data; end;
      6: begin inc(RegUsageFloat); Registers._XMM6:=Data; end;
      7: begin inc(RegUsageFloat); Registers._XMM7:=Data; end;
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
    case RegUsageFloat of
      0: begin inc(RegUsageFloat); Registers.SingleBits := Registers.SingleBits or 1; _XMM0:=Data; end;
      1: begin inc(RegUsageFloat); Registers.SingleBits := Registers.SingleBits or 2; Registers._XMM1:=Data; end;
      2: begin inc(RegUsageFloat); Registers.SingleBits := Registers.SingleBits or 4; Registers._XMM2:=Data; end;
      3: begin inc(RegUsageFloat); Registers.SingleBits := Registers.SingleBits or 8; Registers._XMM3:=Data; end;
      4: begin inc(RegUsageFloat); Registers.SingleBits := Registers.SingleBits or 16; Registers._XMM4:=Data; end;
      5: begin inc(RegUsageFloat); Registers.SingleBits := Registers.SingleBits or 32; Registers._XMM5:=Data; end;
      6: begin inc(RegUsageFloat); Registers.SingleBits := Registers.SingleBits or 64; Registers._XMM6:=Data; end;
      7: begin inc(RegUsageFloat); Registers.SingleBits := Registers.SingleBits or 128; Registers._XMM7:=Data; end;
    else begin
      SetLength(Stack, Length(Stack)+8);
      p := @Stack[LEngth(Stack)-8];
      Single(p^) := data;
    end;
    end;
  end;
  {$ENDIF}
  
  {$IFDEF DELPHI}
  type
    TMethodCallData = record
      AType: Byte; { Must be first field, matching uPSRuntime's TOpenArray }
      Data: TMethod;
    end;
    PMethodCallData = ^TMethodCallData;
  {$ENDIF}

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

      StoreReg(IPointer(VarPtr));
    end else begin
//      UseReg := True;
      case fVar^.aType.BaseType of
        btSet:
          begin
            case TPSTypeRec_Set(fvar.aType).aByteSize of
              1: StoreReg(IPointer(byte(fvar.dta^)));
              2: StoreReg(IPointer(word(fvar.dta^)));
              3, 4: StoreReg(IPointer(cardinal(fvar.dta^)));
              5,6,7,8: StoreReg(IPointer(fVar.Dta^));
              else
                StoreReg(IPointer(fvar.Dta));
            end;
          end;
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
            {$IFDEF FPC}
              StoreReg(IPointer(FVar.Dta));
            {$ELSE}
              StoreReg(IPointer(FVar.Dta^));
            {$ENDIF}
            end;
          end;
        btRecord:
          begin
            if fvar^.aType.RealSize <= sizeof(IPointer) then
              StoreReg(IPointer(fvar.dta^))
            else
              StoreReg(IPointer(fVar.Dta));
          end;
        btVariant
        , btStaticArray:
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
        {$IFNDEF PS_NOWIDESTRING} btWideChar, {$ENDIF}
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
            {$IFDEF DELPHI}
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
            {$ELSE}
              GetMem(p, PointerSize2);
              TMethod(p^) := MKMethod(Self, Longint(FVar.Dta^));
              StoreStack(p^, Pointersize2);
              FreeMem(p);
            {$ENDIF}
          end;
{$IFNDEF PS_NOINT64}
        bts64:
          begin
            StoreReg(IPointer(int64(fvar^.dta^)));
        end;
        btU64:
          begin
            StoreReg(IPointer(tbtu64(fvar^.dta^)));
        end;
{$ENDIF}
      end; {case}
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
{$IFNDEF WINDOWS}
    (*_RSI := 0;
    _RDI := 0;
    _XMM4 := 0;
    _XMM5 := 0;
    _XMM6 := 0;
    _XMM7 := 0;*)
    RegUsageFloat := 0;
{$ENDIF}
  _XMM0 := 0;
  FillChar(Registers, Sizeof(REgisters), 0);
    _RAX := 0;
    RegUsage := 0;
    {$IF DEFINED (fpc) and (fpc_version >= 3)} // FIX FOR FPC constructor calls
    if IsConstructor then begin
      if not GetPtr(rp(Params[0])) then exit; // this goes first
      DisposePPSVariantIFC(Params[0]);
      Params.Delete(0);
    end;
    {$ENDIF}
    if assigned(_Self) then begin
      StoreReg(IPointer(_Self));
    end;
    if assigned(res) and not IsSafeCall and (res^.atype.basetype = btSingle) then begin
      Registers.Singlebits  := Registers.Singlebits or 256;
    end;
{$IFDEF PS_RESBEFOREPARAMETERS}
   if assigned(res) and not IsSafeCall then begin
    case res^.aType.BaseType of
      {$IFDEF x64_string_result_as_varparameter}
      btstring, btWideString, btUnicodeString,
      {$ENDIF}
      btInterface, btArray, btVariant, btStaticArray:
        GetPtr(res);
      btRecord,
      btSet:
        begin
          if res.aType.RealSize > PointerSize then GetPtr(res);
        end;
    end;
   end;
{$ENDIF}
    for I := 0 to Params.Count - 1 do
    begin
      if not GetPtr(rp(Params[I])) then Exit;
    end;
    if assigned(res) and IsSafeCall then
      GetPtr(res); { SafeCall: res is actually passed as last parameter }
    if assigned(res) and not IsSafeCall then begin
{$IFNDEF PS_RESBEFOREPARAMETERS}
      case res^.aType.BaseType of
        {$IFDEF x64_string_result_as_varparameter}
        btstring, btWideString, btUnicodeString,
        {$ENDIF}
        btInterface, btArray, btVariant, btStaticArray:
          GetPtr(res);
        btRecord,
        btSet:
          begin
            if res.aType.RealSize > PointerSize then GetPtr(res);
          end;
      end;
{$ENDIF}
      {$IFDEF WINDOWS}
      if (length(Stack) mod 16) <> 0 then begin
        SetLength(Stack, Length(Stack)+16 - (Length(Stack) mod 16));
      end;
      {$ENDIF} 
      if Stack = nil then pp := nil else pp := @Stack[{$IFDEF REG_STACK_PTR_OFFSET0}0{$ELSE}Length(Stack) -8{$ENDIF}];
{$IFDEF WINDOWS}
      Registers.Stack := pp;
      Registers.Items := Length(Stack) div 8;
      x64call(Address, _RAX, _XMM0, Registers);
{$ELSE}
      x64call(Address, _RAX, Registers, pp, Length(Stack) div 8, _XMM0);
{$ENDIF}
      case res^.aType.BaseType of
        btRecord, btSet:
          begin
            case res.aType.RealSize of
              1: byte(res.Dta^) := _RAX;
              2: word(res.Dta^) := _RAX;
              3,
              4: Longint(res.Dta^) := _RAX;
              5,6,7,8: IPointer(res.dta^) := _RAX;
              {$IFNDEF WINDOWS}
              9..16:
                begin
                  Move(_RAX, res.Dta^, 8);
                  Move(Registers._RDX, Pointer(IPointer(res.Dta) + 8)^, res.aType.RealSize - 8);
                end;
              {$ENDIF}
            end;
          end;
        btSingle:      tbtsingle(res.Dta^) := _XMM0;
        btDouble:      tbtdouble(res.Dta^) := _XMM0;
        btExtended:    tbtextended(res.Dta^) := _XMM0;
        btchar,btU8, btS8:    tbtu8(res.dta^) := _RAX;
        {$IFNDEF PS_NOWIDESTRING} btWideChar, {$ENDIF} btu16, bts16:  tbtu16(res.dta^) := _RAX;
        btClass : IPointer(res.dta^) := _RAX;
        btu32,bts32:   tbtu32(res.dta^) := _RAX;
        btPChar:       pansichar(res.dta^) := Pansichar(_RAX);
{$IFNDEF PS_NOINT64}
        bts64: tbts64(res.dta^) := Int64(_RAX);
        btU64: tbtu64(res.dta^) := UInt64(_RAX);
{$ENDIF}
        btCurrency:    tbts64(res.Dta^) := Int64(_RAX); { Do not change to tbtCurrency: see 08d2743a }
        btInterface,
        btVariant,
        {$IFDEF x64_string_result_as_varparameter}
        btWidestring,btUnicodestring, btstring ,
        {$ENDIF}
        btStaticArray, btArray:;
        {$IFNDEF x64_string_result_as_varparameter}
        btUnicodeString, btWideString, btstring:  Int64(res.dta^) := _RAX;
        {$ENDIF}
      else
        exit;
      end;
    end else begin
      {$IFDEF WINDOWS}
      if (length(Stack) mod 16) <> 0 then begin
        SetLength(Stack, Length(Stack)+16 - (Length(Stack) mod 16));
      end;
      {$ENDIF} 
	if Stack = nil then pp := nil else pp := @Stack[{$IFDEF REG_STACK_PTR_OFFSET0}0{$ELSE}Length(Stack) -8{$ENDIF}];
{$IFDEF WINDOWS}
        Registers.Stack := pp;
        Registers.Items := Length(Stack) div 8;
        x64call(Address, _RAX, _XMM0, Registers);
{$ELSE}
       x64call(Address, _RAX, Registers, pp, Length(Stack) div 8, _XMM0);
{$ENDIF}
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
          {$IFDEF DELPHI} 255: Dispose(PMethodCallData(pp)); {$ENDIF}
        end;
      end;
      CallData.Free;
    end;
  end;
end;


