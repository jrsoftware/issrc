---
paths: ["Components/JvInspector.pas", "Components/JvAutoComplete.pas", "Components/JvInspectorSupport.pas", "Components/jvcl.inc"]
---
# Instructions for the extracted JvInspector components

## Origin and status

These files were extracted from JVCL (github.com/project-jedi/jvcl) for inclusion with
Inno Setup. Only the inspector was extracted: the rest of JVCL is not present. They are
a permanent fork: upstream syncing is abandoned and the units will never again be
updated from JVCL, so diffability against upstream does not matter.

- `JvInspector.pas`: the inspector control, forked and improved.
- `JvAutoComplete.pas`: autocomplete support for the inspector's in-place editor, forked and trimmed.
- `JvInspectorSupport.pas`: a small Inno Setup unit that replaces the JVCL dependencies the two units needed.
- `jvcl.inc`: a minimal Inno Setup version of the upstream include file.

## Rules

- Do not refactor the code style to match Inno Setup conventions. The Inno Setup Pascal conventions in `pascal.md` do not apply here; keep the upstream style.
- Removing code the Inno Setup IDE does not need is welcome and reducing the line count is a goal. No modification notices are maintained, so do not record changes in the unit headers.
- Keep the MPL license blocks and the Initial Developer attribution in the unit headers.
- The code only has to be correct for how Inno Setup uses it: Delphi 10.4 and later, on Windows, with the VCL, in both 32- and 64-bit.
