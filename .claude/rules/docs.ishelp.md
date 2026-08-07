---
paths: ["ISHelp/isetup.xml", "ISHelp/isx.xml", "ISHelp/isxfunc.xml", "ISHelp/isxclasses.header*", "ISHelp/isxclasses.footer", "ISHelp/ispp.xml"]
---
# ISHelp documentation conventions

These are the general documentation conventions shared by all ISHelp sources. `isetup.xml`, `isx.xml`, and the `isxclasses.*` helpers use `isetup.dtd`; `isxfunc.xml` (`isxfunc.dtd`) and `ispp.xml` (`ispp.dtd`) use their own DTD. They all follow the conventions below, except where a file's own rule file notes a difference or adds file-specific structure: see `docs.ishelp.isetup.md`, `docs.ishelp.isxfunc.md`, and `docs.ishelp.ispp.md`. Prose conventions for all of these files live in `docs.general.md`, not here.

## Additional formatting

- `<precode>` for syntax-highlighted section-level and topic-level script examples (such as showing a full [Run] section).
- `<example><pre>` for syntax-highlighted parameter and function examples, which can also be multi-line.

## Cross-references

- Use `<link topic="topicname">Display Text</link>`.
- Always link to related topics when mentioning directives, sections, or functions. Do not leave them as plain text if a help topic exists.
- Use the `<sd>Directive</sd>`, `<sn>[Section]</sn>`, and `<isxfunc>FunctionName</isxfunc>` shorthands instead of a `<link>` to a [Setup] directive, section, or support function.
- To specific anchors: `<link topic="topicname" anchor="anchorname">Display Text</link>`.
- External links: `<extlink href="url">Display Text</extlink>`.
