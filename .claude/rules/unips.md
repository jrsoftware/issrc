---
paths: ["Components/UniPs/**"]
---
# Instructions for UniPs (Pascal Script) components

## Origin

These files are Inno Setup's own version of RemObjects Pascal Script. Inno Setup no longer follows the upstream repository.

When you edit them:
- Do not refactor the code style to match Inno Setup conventions.
- Keep changes minimal and focused. Inno Setup always builds with Delphi on Windows, in both 32- and 64-bit, so a change must be correct on both. Use `{$IFDEF CPU64}` for code that differs by bitness.
- Code for compilers and platforms that Inno Setup does not use (FPC, non-Windows) is dead code, see `Components/UniPs/README.md`. Do not extend a change to it, and do not add new guards for it.

## Registration: compile-time (uPSC_) vs runtime (uPSR_)

Pascal Script has two registration layers that must stay in sync:
- **Compile-time** (`uPSC_*.pas`): declares what the script compiler accepts.
- **Runtime** (`uPSR_*.pas`): provides actual method pointers and property helpers.

When a compile-time registration is guarded by `{$IFDEF}` or `{$IFNDEF}`, the matching runtime registration must use the same guard. A mismatch causes scripts to compile successfully but crash at runtime.

A compile-time `RegisterProperty` for a **published** property does not require a matching runtime `RegisterPropertyHelper`. RTTI handles published properties automatically. Only non-published properties need explicit runtime read/write helpers.

Use the actual Pascal type name in `RegisterProperty`, not a shortcut like `'Byte'` for enums or sets. For example, use `'TComponentState'` for a set type and `'TFontPitch'` for an enum, not `'Byte'`.