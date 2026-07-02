---
paths: ["Components/UniPs/**"]
---
# Instructions for UniPs (Pascal Script) components

## Upstream origin

These files are copies of the RemObjects Pascal Script `stable` branch (see `rev.txt` for the commit hash).

You may still edit them, but:
- Do not refactor the code style to match Inno Setup conventions.
- Keep changes minimal and focused. Even though Inno Setup is a Delphi Windows codebase, the Pascal Script code is cross-platform: it compiles under multiple compilers (Delphi and FPC), operating systems (Windows and others), and CPU bitnesses (32- and 64-bit). A change must either be correct on every combination, or be guarded so it only affects the ones you have verified. Useful guards: `{$IFDEF DELPHI}` / `{$IFDEF FPC}` (compiler), `{$IFDEF MSWINDOWS}` (OS), and `{$IFDEF CPU64}` (bitness).
  Inno Setup always builds with Delphi on Windows, in both 32- and 64-bit, so your change must be correct there. Improving support for the platforms Inno Setup does not use (FPC, non-Windows) is a bonus, but extend a change to them only when it is easy and correctness is easy to confirm; otherwise guard the change.
- Don't edit or inspect `rev.txt` as it is an auto-generated bookkeeping file.

## Registration: compile-time (uPSC_) vs runtime (uPSR_)

Pascal Script has two registration layers that must stay in sync:
- **Compile-time** (`uPSC_*.pas`): declares what the script compiler accepts.
- **Runtime** (`uPSR_*.pas`): provides actual method pointers and property helpers.

When a compile-time registration is guarded by `{$IFDEF}` or `{$IFNDEF}`, the matching runtime registration must use the same guard. A mismatch causes scripts to compile successfully but crash at runtime.

A compile-time `RegisterProperty` for a **published** property does not require a matching runtime `RegisterPropertyHelper`. RTTI handles published properties automatically. Only non-published properties need explicit runtime read/write helpers.

Use the actual Pascal type name in `RegisterProperty`, not a shortcut like `'Byte'` for enums or sets. For example, use `'TComponentState'` for a set type and `'TFontPitch'` for an enum, not `'Byte'`.