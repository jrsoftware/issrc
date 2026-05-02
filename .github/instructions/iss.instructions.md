---
applyTo: "**/*.iss"
---
# Coding conventions for Inno Setup script files
Inno Setup `.iss` scripts use the `[Code]` section for Pascal Script code. This is RemObjects Pascal Script (ROPS), not modern Delphi. The following rules apply to code in `[Code]` sections.
## Fundamental language differences from Delphi
These are hard constraints of the ROPS compiler. Violating them causes compile errors.
- No inline var or inline const. All local variables must be declared in a single `var` block before `begin`. All local constants in a `const` block before `begin`.
- No `exit(Value)` syntax. Assign to `Result` first, then call `exit` separately.
- No typed constants (`const X: Integer = 1`). Only untyped constants (`const X = 1`).
- No default parameter values.
- No function or procedure overloading.
- No `raise` keyword. Use `RaiseException(Msg)` instead.
- No `on E: ExceptionType do` in except blocks. Use `ExceptionType`, `ExceptionParam`, and `GetExceptionMessage` to inspect the current exception.
- No script-defined classes. Classes are host-registered only.
- No variant records.
- No `const` or `type` declaration blocks inside procedure or function bodies. Only `var` and `label` blocks are allowed before `begin`.
- `const` parameters are passed by value (copied), not by reference as in modern Delphi.
- The `/` operator always returns `Extended`, even when both operands are integers. Use `div` for integer division.
- `break` and `continue` are not reserved words. They are ordinary identifiers recognized specially inside loop bodies. A local declaration named `Break` or `Continue` will shadow the loop-control behavior.
## Conventions (same spirit as the Pascal rules, adapted for ROPS)
- Pre-declare all local variables in the `var` block before `begin`.
- Do not use `with` statements.
- `if` and `begin` should be on the same line.
- `else` and `begin` should be on the same line.
- Do not use `begin..end` for single statement blocks.
- Do not use `Longint` or `LongWord`; use `Integer` or `Cardinal` instead.
- Use two spaces for indentation, not tabs.
- Mark read-only parameters as `const`.
- Do not shorten descriptive names. Write `Expression`, not `Expr`; `MultiFileHandler`, not `MFH`. Conventional short names (`I`, `J`, `S`, `Res`) and established identifiers (`HiWord`) are fine.
## Code editing guidelines
- Do not modify existing comments unless the code they describe is also being changed.
- Keep new comments short and match the style of existing comments in the file.
