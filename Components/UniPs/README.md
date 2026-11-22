# RemObjects Pascal Script (Inno Setup version)

These files are a modified fork of the `stable` branch of the
[RemObjects Pascal Script](https://github.com/remobjects/pascalscript)
repository, taken at commit `246a51fd8ec18e011178dbcf14022709289a9b0d`. Inno
Setup no longer follows that repository.

## Build environment

Inno Setup requires Delphi 10.4 Sydney or later and targets Windows only. No
guard tests for another compiler, an older Delphi version, or another
platform.

## Symbols that genuinely vary

| Symbol | Why it varies |
|--------|--------------|
| `WIN64` | Both 32-bit and 64-bit builds are supported |
| `CPUX64` / `Win32` | Same |
| `DELPHI_SMALL_MANAGED_BORROW` | Defined in x86.inc for Delphi 12 Athens or later |
