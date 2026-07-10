---
paths: ["Components/JvInspector.pas", "Components/JvAutoComplete.pas", "Components/JvInspectorSupport.pas", "Components/jvcl.inc"]
---
# Instructions for the extracted JvInspector components

## Upstream origin

These files are extracted from JVCL, for inclusion with Inno Setup. Only
the inspector is extracted: the rest of JVCL is not present.

- `JvInspector.pas`: extracted and improved upstream unit.
- `JvAutoComplete.pas`: extracted but otherwise unchanged upstream unit.
- `JvInspectorSupport.pas`: a small Inno Setup unit that replaces the JVCL dependencies the two units needed.
- `jvcl.inc`: a minimal Inno Setup version of the upstream include file.

You may still edit them, but:
- Do not refactor the code style to match Inno Setup conventions. The Inno Setup Pascal conventions in `pascal.md` do not apply here; stay close to upstream.
- Keep changes minimal and focused, so the units remain easy to diff against upstream. Record every change in the unit's modification notice at the top.
- The code only has to be correct for how Inno Setup uses it: Delphi 10.4 and later, on Windows, with the VCL, in both 32- and 64-bit.
