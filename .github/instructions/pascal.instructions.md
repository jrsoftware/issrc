---
applyTo: "**/*.pas, **/*.dpr, **/*.inc"
---
# Coding conventions for Pascal source files
- Always use inline variables instead of pre-declaring them at the start of the function.
- Prefer inline constants over inline variables, even for run-time values and not just for compile-time constants.
  Inline constants use `=` as the assignment operator instead of `:=`. For example: const MaxCount = 10;
- Do not use with statements.
- If and begin should be on the same line.
- Else and begin should be on the same line.
- A Result assignment followed by an Exit statement should be combined into a single statement. For example: Exit(ResultValue);
- Do not use begin..end for single statement blocks.
- Do not use Longint or LongWord, use Integer or Cardinal instead.
- Use two spaces for indentation, not tabs.
- Mark read-only parameters as const.
- Use Windows units first, then RTL/VCL units, then Components units, then Shared units, then project specific units.
# Code review guidelines for Pascal source files
- All errors must be checked. Installers should be reliable above all.
- Be alert for `out` parameters initializing to zero/empty upon function entry, unlike `var` parameters.
- Code must be compatible with Delphi 10.4 Sydney and later.
- Code must be compatible with 32-bit and 64-bit builds.
- Code must be compatible with Windows 7 and later.
- Do not suggest applying coding conventions to unchanged code.
- Do not suggest changing an inline constant to an inline variable.
- Do not nitpick, especially not about readability or subjective matters.
