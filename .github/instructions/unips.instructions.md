---
applyTo: "Components/UniPs/**"
---
# Instructions for UniPs (Pascal Script) components

## Upstream origin

These files are copies of the RemObjects Pascal Script `stable` branch (see `rev.txt` for the commit hash). Do not refactor upstream code style to match Inno Setup conventions. Keep changes minimal and focused.

## Registration: compile-time (uPSC_) vs runtime (uPSR_)

Pascal Script has two registration layers that must stay in sync:
- **Compile-time** (`uPSC_*.pas`): declares what the script compiler accepts.
- **Runtime** (`uPSR_*.pas`): provides actual method pointers and property helpers.

When a compile-time registration is guarded by `{$IFDEF}` or `{$IFNDEF}`, the matching runtime registration must use the same guard. A mismatch causes scripts to compile successfully but crash at runtime.

A compile-time `RegisterProperty` for a **published** property does not require a matching runtime `RegisterPropertyHelper`. RTTI handles published properties automatically. Only non-published properties need explicit runtime read/write helpers.

Use the actual Pascal type name in `RegisterProperty`, not a shortcut like `'Byte'` for enums or sets. For example, use `'TComponentState'` for a set type and `'TFontPitch'` for an enum, not `'Byte'`.
