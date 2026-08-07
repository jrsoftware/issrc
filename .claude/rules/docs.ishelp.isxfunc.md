---
paths: ["ISHelp/isxfunc.xml"]
---
# isxfunc.xml documentation conventions

These are specific to `isxfunc.xml`; the general ISHelp conventions in `docs.ishelp.md` apply too.

`<link topic="...">` (with optional `anchor`), `<extlink>`, `<sd>`, `<sn>`, and `<isxfunc>` all work as in the general conventions.

## Function documentation

Each function follows this pattern:
- `<name>`: Function name.
- `<prototype>`: Full function signature.
- `<description>`: One or more `<p>` elements. First sentence is the key description.
  - For functions returning a value: "Returns [description of return value]."
  - For procedures: "Does [action]." or imperative form.
  - For boolean functions: "Returns True if [condition], False otherwise."
- `<remarks>` (optional): Additional notes, edge cases, caveats.
- `<seealso>` (optional): Links to related functions.
- `<example>` (optional): Code example.

Standard description patterns:
- "Returns the [noun] of [context]."
- "Returns True if [condition]."
- "Returns a [type] containing [description]."
- "Gets/Sets the value of [thing]."
- "Creates/Deletes/Removes [thing]."
- "An exception will be raised if [error condition]."
- "If [condition], [function name] returns [value]."

## Differences from the general conventions

- **Examples:** use `<example>`, which may contain `<pre>`. `isxfunc.dtd` has no `<precode>`.
