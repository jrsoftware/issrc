---
paths: ["ISHelp/ispp.xml"]
---
# ispp.xml documentation conventions

These are specific to `ispp.xml`; the general ISHelp conventions in `docs.ishelp.md` apply too.

## Directive documentation

Preprocessor directives use `<topic>` with `<syntax>`/`<define>`, `<description>`, and `<section>`.

## Function documentation

Functions put the signature in `<section title="Prototype">` (inside `<pre><line>`) and related links in `<section title="See also">`.

Boolean-like functions are documented as "Returns non-zero if [condition]." rather than "Returns True if [condition], False otherwise."

## Differences from the general conventions

- **Cross-reference links:** use `<link href="topicname">Display Text</link>`. `ispp.dtd` has no `anchor` attribute; the `href` carries the target.
- **Examples:** examples sit in a bare `<pre>` inside `<section title="Examples">`.
- **Function shorthand:** `<isxfunc>` not available; link ISPP functions with `<link href="...">`.
