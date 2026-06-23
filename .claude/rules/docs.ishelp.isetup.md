---
paths: ["ISHelp/isetup.xml"]
---
# isetup.xml documentation conventions

These are specific to `isetup.xml`, which documents the entry sections and [Setup] directives; the general ISHelp conventions in `docs.ishelp.md` apply too.

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

## Conditional behavior

Use the pattern: "If [value] is specified, Setup will [action]." Repeat for each value. Example:
> If <tt>string</tt> is specified, Setup will create a string (<tt>REG_SZ</tt>) value.
> If <tt>dword</tt> is specified, Setup will create a 32-bit integer (<tt>REG_DWORD</tt>) value.

## Default values

State defaults explicitly: "Defaults to [value]." or "The default value is [value]." or "(the default setting)".
