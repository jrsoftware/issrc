---
applyTo: "**/*.pas, **/*.dpr, **/*.inc"
---
# Coding conventions for Pascal source files
- Always use inline variables instead of pre-declaring them at the start of the function.
- Prefer inline constants over inline variables when the value is not expected to change.
- Do not use with statements.
- If and begin should be on the same line.
- Else and begin should be on the same line.
- Do not use begin..end for single statement blocks.
- Do not use Longint or LongWord, use Integer or Cardinal instead.
- Use two spaces for indentation, not tabs.
- Mark read-only parameters as const.
- Use Windows units first, then RTL/VCL units, then Components units, then Shared units, then project specific units.
# Code review guidelines for Pascal source files
- Do not suggest applying coding conventions to unchanged code.
- Accept inline constants and do not suggest changing them to inline variables. For example, accept 'const I = J;' instead of suggesting 'var I := J;'.
- Code must be compatible with Delphi 10.4 Sydney and later.
- Code must be compatible with 32-bit and 64-bit builds.
- All errors must be checked. Installers should be reliable above all.
- Do not nitpick, especially not about readability or subjective matters.
