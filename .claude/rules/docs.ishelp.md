---
paths: ["ISHelp/isetup.xml", "ISHelp/isx.xml", "ISHelp/isxfunc.xml", "ISHelp/isxclasses.header*", "ISHelp/isxclasses.footer"]
---
# ISHelp documentation conventions

## Formatting

- `<link>` for cross-references to other help topics.
- `<extlink>` for external URLs.
- `<precode>` for section-level and topic-level script examples (such as showing a full [Run] section).
- `<example><pre>` for parameter and function examples, which can also be multi-line.
- `<sec>`, `<evt>`, `<key>`, `<com>`, `<con>`, `<str>`, `<num>`, and `<ispp>` for syntax-highlighted script examples.
- Always link to related topics when mentioning directives, sections, or functions. Do not leave them as plain text if a help topic exists.

## Section topic structure

Each entry section topic (such as [Files], [Registry], [Run]) follows this structure:
1. Opening paragraph: "This optional section defines..." or "This section contains...".
2. Introductory notes about default behavior.
3. Example section with `<precode>`.
4. "The following is a list of the supported parameters:" introducing the parameter list.
5. Parameters documented with `<paramlist>` / `<param>`.
6. Links to Common Parameters and Components/Tasks Parameters at the bottom.

## Directive-topic structure

Each [Setup] directive topic uses `<setuptopic>`:
1. `<setuptopic directive="DirectiveName">`: topic wrapper.
2. `<setupvalid>`: valid values (optional, omitted when free-form text).
3. `<setupdefault>`: default value (optional).
4. `<body>`: opening paragraph, followed by detail paragraphs, notes, examples, and optional "See also" links. Common opening patterns: "This directive specifies...", "This tells the compiler...", "Controls whether...", "If set to [value]...".

## Parameter documentation

Each parameter follows this pattern:
1. Description paragraph: what it controls.
2. Additional detail paragraphs as needed.
3. Example with `<example><pre>`.
4. For Flags parameters: "This parameter is a set of extra options. Multiple options may be used by separating them by spaces. The following options are supported:" followed by `<flaglist>`.

## Function documentation (isxfunc.xml)

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

## Conditional behavior

Use the pattern: "If [value] is specified, Setup will [action]." Repeat for each value. Example:
> If <tt>string</tt> is specified, Setup will create a string (REG_SZ) value.
> If <tt>dword</tt> is specified, Setup will create a 32-bit integer (REG_DWORD) value.

## Default values

State defaults explicitly: "Defaults to [value]." or "The default value is [value]." or "(the default setting)".

## Cross-references

- Within the same help system: `<link topic="topicname">Display Text</link>`.
- To specific anchors: `<link topic="topicname" anchor="anchorname">Display Text</link>`.
- External links: `<extlink href="url">Display Text</extlink>`.
