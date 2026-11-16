# RemObjects Pascal Script (Inno Setup version)

These files are a modified fork of the `stable` branch of the
[RemObjects Pascal Script](https://github.com/remobjects/pascalscript)
repository, taken at commit `246a51fd8ec18e011178dbcf14022709289a9b0d`. Inno
Setup no longer follows that repository.

These source files still contain a significant amount of dead code guarded by
compiler directives that are never true in Inno Setup builds. This dead code
exists because upstream Pascal Script supports many compilers, platforms, and
configurations that Inno Setup does not use. For example `PS_NOINT64` is never
true.

There's also a significant amount of useless guards by compiler directives
that are never false in Inno Setup builds. For example `PS_MINIVCL` is never
false.

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

`PascalScript.inc` (included by every unit) includes `eDefines.inc` and sets
additional symbols, the same way in every build:

| Define | How set | Always true in IS? |
|--------|---------|-------------------|
| `PS_HAVEVARIANT` | Defined unconditionally | Yes |
| `PS_DYNARRAY` | Defined unconditionally | Yes |
| `TExtended80Rec_present` | Defined unconditionally | Yes |
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

## Build environment

Inno Setup requires Delphi 10.4 Sydney or later and targets Windows only.
Apart from the local symbols listed below, no guard tests for another
compiler, an older Delphi version, or another platform. `eDefines.inc` only
defines `CPU64` for 64-bit builds, and `DELPHI_ATHENS_UP` for Delphi 12 Athens
or later.

## Local symbols that are always defined

The guards of these symbols are useless:

| Symbol | Why always true |
|--------|----------------|
| `STRINGSTREAMFIX` | Defined unconditionally in uPSR_classes.pas |

## Symbols set by x64.inc

These symbols are defined only for x64 builds, when `uPSRuntime.pas` includes
`x64.inc`; they are not x86 symbols:

| Symbol | Why defined in x64 builds |
|--------|--------------------------|
| `PS_RESBEFOREPARAMETERS` | Defined unconditionally in x64.inc |
| `x64_string_result_as_varparameter` | Defined unconditionally in x64.inc |
| `REG_STACK_PTR_OFFSET0` | Defined unconditionally in x64.inc |

## Local symbols that are never defined

The code these symbols guard is dead:

| Symbol | Why always false |
|--------|-----------------|
| `FPC_OR_KYLIX` | Nothing defines it |
| `UNIX_OR_KYLIX` | Nothing defines it |
| `NO_vmtSelfPtr` | Nothing defines it |
| `empty_methods_handler` | Nothing defines it |

## Symbols that genuinely vary

| Symbol | Why it varies |
|--------|--------------|
| `CPU64` | Both 32-bit and 64-bit builds are supported |
| `CPUX64` / `Win32` / `Win64` | Same |
| `DELPHI_ATHENS_UP` | Depends on the Delphi version used to compile |
| `DELPHI_SMALL_MANAGED_BORROW` | Derived from `DELPHI_ATHENS_UP` in x86.inc |
