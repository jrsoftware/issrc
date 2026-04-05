# RemObjects Pascal Script (Inno Setup copy)

These files are copies of the `stable` branch of the
[RemObjects Pascal Script](https://github.com/remobjects/pascalscript)
repository. All files that Inno Setup does not use have been removed. See
`rev.txt` for the original upstream commit hash.

The retained source files still contain a significant amount of dead code
guarded by compiler directives that are never true in Inno Setup builds. This
dead code exists because Pascal Script supports many compilers, platforms, and
configurations that Inno Setup does not use.

## Defines set by Inno Setup

Inno Setup's project files define the following symbols when compiling these
units:

| Define | Meaning |
|--------|---------|
| `PS_MINIVCL` | Minimal VCL mode: excludes large VCL registration blocks |
| `PS_NOGRAPHCONST` | Excludes graphics color constants, computes them instead |
| `PS_PANSICHAR` | Maps `PChar` parameters to `PAnsiChar` in script declarations |
| `PS_NOINTERFACEGUIDBRACKETS` | Interface GUIDs are not enclosed in brackets |

## Defines set by PascalScript.inc

`PascalScript.inc` (included by every unit) sets additional symbols based on
the compiler version:

| Define | Condition | Always true in IS? |
|--------|-----------|-------------------|
| `PS_HAVEVARIANT` | `DELPHI4UP` | Yes |
| `PS_DYNARRAY` | `DELPHI4UP` | Yes |
| `DEBUG` | Explicitly `{$UNDEF}`'d | Always false |

## Defines NOT set by Inno Setup

These symbols are not defined, meaning `{$IFDEF X}` blocks are dead and
`{$IFNDEF X}` blocks are live:

| Define | What it would enable |
|--------|---------------------|
| `PS_USESSUPPORT` | Uses-clause tracking in the compiler |
| `PS_DELPHIDIV` | Alternative integer division behavior |
| `PS_NOINT64` | Disables Int64 support |
| `PS_NOWIDESTRING` | Disables WideString support |
| `PS_NOINTERFACES` | Disables COM interface support |
| `PS_NOIDISPATCH` | Disables IDispatch support |
| `PS_NOSMARTLIST` | Disables smart list optimization |
| `PS_NOSTANDARDTYPES` | Disables standard type registration |
| `PS_STACKALIGN` | Stack alignment (FPC only) |
| `PS_ARRAY_ON_STACK` | Array-on-stack passing (FPC only) |
| `PS_FPCSTRINGWORKAROUND` | FPC string workaround |

## Build environment (always true)

Inno Setup requires Delphi 10.4 Sydney or later and targets Windows only. This
means the following are always true and their guards are dead code:

| Symbol | Why always true |
|--------|----------------|
| `DELPHI` | Inno Setup uses Delphi, not FPC |
| `MSWINDOWS`, `WINDOWS` | Inno Setup targets Windows only |
| `UNICODE` | Delphi 2009+ always defines this |
| `DELPHI4UP` .. `DELPHI27UP` | Delphi 10.4 = internal version 27 |
| `BDS3UP` .. `BDS21UP` | BDS version chain, same reason |
| `DELPHI_SEATTLE_UP` | Alias for `DELPHI23UP`, always true |
| `DELPHI_TOKYO_UP` | Alias for `DELPHI25UP`, always true |
| `DELPHI_SYDNEY_UP` | Alias for `DELPHI27UP`, always true |
| `INLINE_SUPPORT` | Defined for `DELPHI2005UP`, always true |
| `ENDIAN_LITTLE` | Delphi on Windows is always little-endian |
| `DELPHI_or_MSWINDOWS` | Derived from `DELPHI`, always true |
| `TExtended80Rec_present` | Defined for `DELPHI16UP`, always true |
| `UNICODE_SUPPORT` | Derived from `UNICODE` in eDefines.inc, always true |
| `PS_RESBEFOREPARAMETERS` | Defined inside `{$IFDEF DELPHI}` in x64.inc |
| `x64_string_result_as_varparameter` | Defined inside `{$IFDEF DELPHI}` in x64.inc |
| `REG_STACK_PTR_OFFSET0` | Defined inside `{$IFDEF DELPHI}` + `{$IFDEF WINDOWS}` in x64.inc |
| `STRINGSTREAMFIX` | Derived from `UNICODE` + not `FPC` in uPSR_classes.pas |

And the following are always false:

| Symbol | Why always false |
|--------|-----------------|
| `FPC` | Inno Setup uses Delphi |
| `FPC_REQUIRES_PROPER_ALIGNMENT` | FPC-only symbol |
| `FPC_UNICODE` | FPC-only symbol |
| `FPC_HAS_TYPE_EXTENDED` | FPC-only symbol |
| `FPC_OLD_FIX` | FPC-only symbol |
| `FPC_SAFECALL_BUG` | FPC-only symbol |
| `FPC_WIDESTRING_EQUAL_UNICODESTRING` | FPC-only symbol |
| `PS_FPC_HAS_COM` | FPC-only symbol |
| `FPC_OR_KYLIX` | Derived from `FPC` or `KYLIX`, both always false |
| `CLX`, `KYLIX` | Qt/Linux frameworks, not used |
| `LINUX`, `UNIX`, `DARWIN`, `MACOS` | Non-Windows platforms, not used |
| `ANDROID` | Mobile platform, not used |
| `NEXTGEN` | Deprecated mobile compiler, not used |
| `NEXTGEN_or_DELPHILINUX` | Derived from `NEXTGEN` or `DELPHI_LINUX`, both always false |
| `UNIX_OR_KYLIX` | Derived from `UNIX` or `KYLIX`, both always false |
| `CBUILDER` | C++Builder, not used |
| `DOTNET`, `CLR` | .NET compiler, not used |
| `NO_vmtSelfPtr` | Set for FPC or Delphi 2, both always false |
| `D4PLUS` | Custom symbol in uPSC/R_classes.pas, never defined |
| `empty_methods_handler` | Set for FPC on PowerPC/ARM/64-bit or Delphi on ARM/ARM64, always false |
| `WIDESTRING_EQUAL_UNICODESTRING` | Set inside `{$IFNDEF UNICODE}`, always false |

## Symbols that genuinely vary

| Symbol | Why it varies |
|--------|--------------|
| `CPU64` / `CPU32` | Both 32-bit and 64-bit builds are supported |
| `CPUX64` / `Win32` / `Win64` | Same |
| `DELPHI28UP` and above | Depends on the Delphi version used to compile |
