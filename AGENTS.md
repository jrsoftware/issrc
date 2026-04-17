# Inno Setup AI Agent Guide

## Introduction
- Source code of Inno Setup, an open-source installation builder for Windows applications by Jordan Russell and Martijn Laan, first released in 1997.
- Comes in 64-bit and 32-bit editions; both can build 32-bit or 64-bit installers.
- Copyrighted software; see `LICENSE.TXT` for distribution restrictions. Commercial users are requested to purchase a commercial license.

## Project Architecture
Delphi projects under `Projects/`:

| Project | Output | Role |
|---------|--------|------|
| `ISIDE` | EXE | GUI compiler front-end (IDE), thin shell around ISCmplr.dll |
| `ISCC` | EXE | CLI compiler front-end, thin shell around ISCmplr.dll |
| `ISCmplr` | DLL | Actual compiler engine; entry point: `Compiler.SetupCompiler.pas` |
| `ISPP` | DLL | Preprocessor |
| `Setup` | EXE | Installer runtime; entry point: `Setup.MainFunc.pas` |
| `SetupCustomStyle` | EXE | Installer runtime with VCL Styles support |
| `SetupLdr` | EXE | Self-extracting loader that decompresses and launches Setup |
| `ISSigTool` | EXE | Signs and verifies files using custom signatures |

Under `ISHelp/ISHelpGen/`:

| Project | Output | Role |
|---------|--------|------|
| `ISHelpGen` | EXE | Generates XML files for help and Pascal Script autocomplete |

## Core System
- Delphi codebase targeting Windows 7 and later via the Windows API. All projects build for x64 and x86.
- Primary project-specific source lives in `Projects/Src/` using namespace prefixes: `Compiler.*`, `Compression.*`, `IDE.*`, `ISPP.*`, `Setup.*`, `SetupLdrAndSetup.*`, `Shared.*`. Projects also compile shared units from `Components/`, and `ISIDE` additionally consumes the generated `ISHelp\isxclasses_wordlists_generated.pas`. New project-specific source files must follow the namespace convention and be placed in `Projects/Src/`.
- `Shared.Struct.pas` defines cross-project record layouts, and `Shared.CompilerInt.Struct.pas` defines the compiler/front-end interface layout. Changing either requires reviewing every consumer and recompiling the relevant projects.
- Other shared units live under `Components` and `Projects\Src\Shared.*.pas`.
- `SetupLdrAndSetup.*` units are shared by SetupLdr, Setup, and SetupCustomStyle; changes must account for all three.
- `SetupCustomStyle` uses the same sources as `Setup` plus `Vcl.Themes`/`Vcl.Styles` and `Setup.TaskDialogForm`.
- Many UI units in `Projects/Src/` are paired with checked-in `.dfm` files; form changes often require coordinated `.pas` and `.dfm` edits.
- Compression engines pull native code from `Projects\Src\Compression.*`.
- Pascal Script integration sits in `Compiler.Script*.pas` and `Setup.Script*.pas`; changes must be mirrored on both compiler and setup sides.
- Setup path redirection: `ApplyPathRedirRules` rewrites System32/SysWOW64/Sysnative and converts to super paths. Unless documented otherwise, functions should receive current-process-bit paths. Use `ApplyRedirToRunEntryPaths` for run entries and `ApplyRedirForRegistrationOperation` for registration/shared-count operations. Old-style FS redirection disabling is still used when running 64-bit system executables from a 32-bit Setup process.
- Extended-length (super) paths (`\\?\`) are used by Setup to avoid MAX_PATH issues and are produced by `ApplyPathRedirRules` unless `rfNormalPath` is used. Only pass super paths to APIs known to accept them; when in doubt (especially for shell APIs or user-facing strings), convert back to a normal path.

## Build
- Run `compile.bat x64` or `compile.bat x86`. Release outputs go to `Files`, debug binaries to `Projects\Bin`.
- Compiler path is configured in `compilesettings.bat`.
- `build.bat` orchestrates full release builds.

## Code Conventions
- Pascal style and review guidelines: `.github/instructions/pascal.instructions.md` (auto-applied to `.pas`, `.dpr`, `.inc`).
- UniPs (Pascal Script) component guidelines: `.github/instructions/unips.instructions.md` (auto-applied to `Components/UniPs/**`).

## Docs
- `whatsnew.htm` contains the Revision History.
- Help content lives under `ISHelp`:
  - When editing `ISHelp\*`, regenerate CHM/Web help: run `compile.bat x64 ishelpgen` then `ISHelp\compile.bat` and verify 0 errors. Do not skip; this regenerates `isxclasses_wordlists_generated.pas`, which is in Git.
  - `isxfunc.xml` documents Pascal Script functions.
  - `isxclasses.pas` documents Pascal Script classes, with `isxclasses.header`, `isxclasses.header2`, and `isxclasses.footer` as helpers. When a `uPSC_*.pas` registration changes for Inno Setup (`PS_MINIVCL` is defined, `FPC` and `CLX` are not), update `isxclasses.pas` to match and regenerate as above.
  - `isx.xml` contains all other Pascal Script documentation.
  - `ispp.xml` documents the ISPP preprocessor.
  - `isetup.xml` contains the remaining documentation.
  - `isetup.dtd`, `ispp.dtd`, `isxfunc.xsl`, and `ispp.xsl` are schema and XSL files consumed by `ISHelpGen`.
  - Do not edit files generated by `ISHelpGen`: `ispp_generated.xml`, `isxclasses_generated.xml`, `isxclasses_wordlists_generated.pas`, `isxfunc_generated.xml`.
- In `whatsnew.htm` and help, wrap script keywords in `<tt></tt>` for monospaced display.
- Do not use em dashes or en dashes; use periods or commas instead.

## Precompiled Binaries
- `Files/` contains precompiled C/C++ DLLs (7-Zip, zlib, bzip2, LZMA, Scintilla) and `.issig` signature files.
- Rebuild with `build-precomp.bat` (requires separate repository checkouts); refresh signatures separately with ISSigTool afterward.

## Testing & Verification
- No automated tests ship with the repo.
- After editing any `.pas`, `.dpr`, `.dproj`, or `.inc` file: run `compile.bat x64` and verify 0 errors.
- When creating a new file: immediately `git add` to stage it.
- Otherwise treat Git as read-only.
- A task is not complete until the relevant compilation succeeds with 0 errors.
