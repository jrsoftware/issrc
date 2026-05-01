---
paths: ["**/*.pas", "**/*.dpr", "**/*.inc"]
---
# Coding conventions for Pascal source files
- Always use inline variables instead of pre-declaring them at the start of the function.
- Prefer inline constants over inline variables, even for run-time values and not just for compile-time constants.
  Inline constants use `=` as the assignment operator instead of `:=`. Examples:
  Inline constants which are an object can still have their properties modified.
  `const MaxCount = MinCount * 2;`
  `const ExpandedFilename = PathExpand(Filenames[I]);`
  Exception: inline constants cannot be passed as `var` parameters, so use an inline variable in that case.
- Do not use `with` statements.
- `if` and `begin` should be on the same line.
- `else` and `begin` should be on the same line.
- A `Result` assignment followed by an `Exit` statement should be combined into a single statement. For example: `Exit(ResultValue);`
- Do not use `begin..end` for single statement blocks.
- Do not use `Longint` or `LongWord`, use `Integer` or `Cardinal` instead.
- Use two spaces for indentation, not tabs.
- Mark read-only parameters as `const`.
- Use Windows units first, then RTL/VCL units, then Components units, then Shared units, then project specific units.
- Class methods should be static when possible.
- Do not shorten descriptive names. Write `Expression`, not `Expr`; `MultiFileHandler`, not `MFH`. Conventional short names (`I`, `J`, `S`, `Res`) and established identifiers (`HiWord`) are fine.
# Code editing guidelines
- Do not modify existing comments unless the code they describe is also being changed.
- Keep new comments short and match the style of existing comments in the file.
- When modifying code that calls Windows APIs, read the actual documentation before writing code. Do not assume parameter semantics based on similar APIs.
- Update Inno Setup copyright header of any file you edit, if the current year is not already included.
- Add tests for new code to the unit's `*.Test.pas` if it exists; otherwise consider creating one if the code is complex enough to warrant testing.
# Code review guidelines
- All errors must be checked. Installers should be reliable above all.
- Be alert for `out` parameters: an `out` parameter, like a `var` parameter, is passed by reference. However, with an `out` parameter, the initial value of the referenced variable for managed types, such as strings or arrays, is discarded by the caller before it is passed to the routine. Meanwhile, the initial value for unmanaged types, such as integers or pointers, is ignored. Therefore, for unmanaged types, it is not guaranteed that the initial value of the referenced variable will be overwritten in the routine to which it is passed.
- Be alert for functions not assigning a value to Result: if a function exits without assigning a value to Result or the function name, then the function's return value is undefined, even for managed types.
- Be alert for dynamic-array aliasing: assigning one dynamic array (e.g. `TBytes`) to another makes both reference the same buffer. Unlike strings and static arrays, copy-on-write is not employed, so writes through one alias (including `FillChar`) are visible through the other. Use `Copy(Source)` when an independent buffer is needed.
- Code must be compatible with Delphi 10.4 Sydney and later.
- Code must be compatible with 32-bit and 64-bit builds.
- Code must be compatible with Windows 7 and later.
- Do not suggest applying coding conventions to unchanged code.
- Do not suggest changing an inline constant to an inline variable.
- Do not nitpick, especially not about readability or subjective matters. This includes:
  - Comment style or presence of comments.