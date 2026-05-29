---
paths: ["whatsnew.htm", "ISHelp/isetup.xml", "ISHelp/ispp.xml", "ISHelp/isx.xml", "ISHelp/isxfunc.xml", "ISHelp/isxclasses.header*", "ISHelp/isxclasses.footer"]
---
# General documentation conventions for Inno Setup

## Voice and Tone

- **Direct and technical.** State facts plainly. Do not hedge or pad with filler.
- **Second person** ("you") when addressing the user who writes scripts or configures Setup.
- **Impersonal** when describing system behavior: "Setup will...", "the compiler will...", "an exception will be raised if...".
- **Not chatty.** No rhetorical questions, no "Note that you might wonder...". Just state the information.
- **Contractions are used.** Write "don't", "isn't", "can't", "won't", "doesn't", "aren't", "hasn't", "didn't", "wouldn't", "couldn't", "shouldn't". These are natural and make the text less stiff for non-native readers.

## Sentence Structure

- Prefer **short, clear sentences**. One idea per sentence when possible.
- Avoid deeply nested clauses. If a sentence grows complex, split it.
- **Active voice** when possible: "Setup creates the directory" rather than "the directory is created by Setup". Passive voice is acceptable when the actor is irrelevant or when it reads more naturally: "An exception will be raised if...".
- Parenthetical notes are common and acceptable: "(which can include constants)", "(the default setting)".

## Vocabulary and Terminology

Use the established Inno Setup terms consistently. Never substitute synonyms:

| Correct term | Do not use |
|---|---|
| Setup, the Setup program, the installer | the installation program (note: "installer" is also standard for the compiled artifact, such as "64-bit installers") |
| Uninstall, the uninstaller | the uninstall program |
| the compiler | the build system, the compilation engine |
| the end user | the customer, the consumer |
| the user | the developer, the script author (when addressing the person writing .iss scripts) |
| support function | helper function, utility function |
| support class | helper class |
| directive | setting, option (for [Setup] section items) |
| parameter | field, property (for section entry items) |
| flag | option, switch (for Flags parameter values) |
| entry | line, row (for a line in a section) |
| section | block, group (for [Setup], [Files], etc.) |
| the Compiler IDE | the editor ("the IDE" is acceptable after "the Compiler IDE" has been established in the same context, in both ISHelp and whatsnew) |
| Pascal Scripting | the scripting engine (for the feature/topic name; "Pascal Script" is acceptable when referring to the RemObjects Pascal Script engine itself) |
| extended-length paths | long paths (use "also known as super paths" parenthetically on first mention within a topic; don't use "super paths" as a standalone term) |
| menu item | menu option |

Additional vocabulary rules:
- "Setup or Uninstall" or "the Setup program and uninstaller" when a feature applies to both. Not "the installer or uninstaller".
- "the user's system" for the target environment. Not "the target machine". "computer" is acceptable for restarts ("restart the computer") and computer names ("the name of the computer").
- "specified" is the standard word for parameters and directives: "the specified file", "the specified parameter". Not "provided".
- "given" is the standard word for function argument descriptions: "the given string", "the given file name".
- "Returns True if..." is the standard pattern for boolean function descriptions.
- "cannot" as one word. Not "can not".
- Use "use" not "utilize".
- Use "non-zero" not "non-zero value".
- Do not shorten established descriptive terms. Write "support function", not "func".
- Repeat the same term for the same concept. Do not rotate between synonyms for variety.

## Punctuation

- **Oxford (serial) comma:** Use it. "A, B, and C", not "A, B and C".
- **Periods** at the end of complete sentences, including list items that are complete sentences.
- **No em dashes or en dashes.** Use periods, commas, or parentheses instead.
- **No "e.g."** Use "for example" or "such as".
- **No "i.e."** Use "that is" or rephrase.
- **"etc." is acceptable** but use sparingly.
- **Colons** before examples and lists.

## Code Formatting

- `<tt>` for all code identifiers: section names, directive names, parameter names, flag names, constant names, type names, function names, file names, values like `yes`/`no`/`True`/`False`.
- `<i>` for emphasis, wizard page names (such as *Select Destination Location*), UI element names, and file titles.
- `<b>` sparingly, for important warnings or the word "NOTE:".

## Review Guidelines

When reviewing documentation, check for:

1. **Terminology consistency.** Are the correct Inno Setup terms used? (See the vocabulary table above.)
2. **Factual accuracy.** Does the documentation match the actual code behavior?
3. **Completeness.** Are all parameters, flags, return values, and error conditions documented?
4. **Cross-references.** Are related topics linked?
5. **Code formatting.** Are identifiers wrapped in `<tt>`? Are examples provided?
6. **Style violations.** Check for: em/en dashes, "e.g.", "i.e.", synonym rotation, missing Oxford commas, "can not" instead of "cannot", "utilize" instead of "use".
7. **Clarity for non-native speakers.** Avoid idioms, complex subordinate clauses, and uncommon vocabulary.
8. **Do not nitpick** about subjective style matters in existing text that is not being changed. Only flag style issues in new or modified text.
