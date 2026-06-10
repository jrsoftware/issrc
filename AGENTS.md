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
| `ISTestTool` | EXE | Internal utility that runs unit tests; invoked by build.bat and build-ce.bat |

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
- Run `compile.bat x64` or `compile.bat x86`. Both output release binaries to `Files`.
- Append a project name, such as `compile.bat x64 ISCC`, to compile only that project.
- Compiler path is configured in `compilesettings.bat`.
- `build.bat` orchestrates full release builds.
- If a release build binary under `Files\` reports "Signature file is not valid", run `issig.bat embed`, then rerun the relevant compile command, usually `compile.bat x64`, and retry the failed command. This recovery step is required even though it updates `Components\TrustFunc.AllowedPublicKeys.inc`.

## Code Conventions
- `**/*.iss`: see `.claude/rules/iss.md`.
- `**/*.pas`, `**/*.dpr`, `**/*.inc`: see `.claude/rules/pascal.md`.
- `**/*Test.pas`, `**/*Test.iss`: see `.claude/rules/test.md`.
- `Components/UniPs/**`: see `.claude/rules/unips.md`.

## Documentation Conventions
- `whatsnew.htm`, `ISHelp/isetup.xml`, `ISHelp/ispp.xml`, `ISHelp/isx.xml`, `ISHelp/isxfunc.xml`, `ISHelp/isxclasses.header*`, `ISHelp/isxclasses.footer`: see `.claude/rules/docs.general.md`.
- `ISHelp/isetup.xml`, `ISHelp/isx.xml`, `ISHelp/isxfunc.xml`, `ISHelp/isxclasses.header*`, `ISHelp/isxclasses.footer`: see `.claude/rules/docs.ishelp.md`.
- `whatsnew.htm`: see `.claude/rules/docs.whatsnew.md`.

## Docs
- `whatsnew.htm` contains the Revision History.
- Help content lives under `ISHelp`:
  - When editing `ISHelp\*`, regenerate CHM/Web help: run `compile.bat x64 ISHelpGen` then `ISHelp\compile.bat` and verify 0 errors. Don't skip; this regenerates `isxclasses_wordlists_generated.pas`, which is in Git.
  - `isxfunc.xml` documents Pascal Script functions.
  - `isxclasses.pas` documents Pascal Script classes, with `isxclasses.header`, `isxclasses.header2`, and `isxclasses.footer` as helpers. When a `uPSC_*.pas` registration changes for Inno Setup (`PS_MINIVCL` is defined, `FPC` and `CLX` are not), update `isxclasses.pas` to match and regenerate as above.
  - `isx.xml` contains all other Pascal Script documentation.
  - `ispp.xml` documents the ISPP preprocessor.
  - `isetup.xml` contains the remaining documentation.
  - `isetup.dtd`, `ispp.dtd`, `isxfunc.xsl`, and `ispp.xsl` are schema and XSL files consumed by `ISHelpGen`.
  - Don't edit files generated by `ISHelpGen`: `ispp_generated.xml`, `isxclasses_generated.xml`, `isxclasses_wordlists_generated.pas`, `isxfunc_generated.xml`.
- Don't use em dashes or en dashes; use periods or commas instead.
- Don't use "e.g."; use "for example" or "such as" instead.

## Precompiled Binaries
- `Files/` contains precompiled C/C++ DLLs (7-Zip, zlib, bzip2, LZMA, Scintilla) and `.issig` signature files.
- Rebuild with `build-precomp.bat` (requires separate repository checkouts); refresh signatures separately with ISSigTool afterward.

## Testing & Verification
- After editing any `.pas`, `.dpr`, `.dproj`, or `.inc` file: run `compile.bat x64` and verify 0 errors.
- Unit tests live in `*.Test.pas` files. No testing framework is used; tests raise an exception on failure.
- To run tests, run `test.bat` and verify 0 exit code. This also compiles `ISTestTool` first.

## Other information
- When creating a new file: immediately `git add` it, but only if still untracked.
- Otherwise treat Git as read-only.
- A task is not complete until the relevant compilation succeeds with 0 errors.
