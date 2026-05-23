---
applyTo: "whatsnew.htm"
---
# Inno Setup Revision History documentation conventions

## Formatting

- `<tt>` for script identifiers, same as in help XML.
- `<i>Fix:</i>` prefix for bug fix entries.
- `<i>` for example script file names when they are linked: `<i><a href="...">ScriptName.iss</a></i>`.
- `<b>` for important warnings.
- `<pre><code class="innosetup">` for syntax-highlighted script examples.

## Entry structure

Changelog entries appear as `<li>` items in `<ul>` lists.

**New features:**
- "Added new [type] <tt>Name</tt>." or "Added new [type] <tt>Name</tt> to [context]."
- "Added new support functions <tt>Name1</tt>, <tt>Name2</tt>, and <tt>Name3</tt>."
- "Added new support classes <tt>Name1</tt>, <tt>Name2</tt>, and <tt>Name3</tt>."
- "Added new properties <tt>Name1</tt>, <tt>Name2</tt>, and <tt>Name3</tt> to class <tt>ClassName</tt>."
- "Added support for [feature]."

**Changes:**
- "Directive <tt>Name</tt> now defaults to <tt>value</tt>."
- "<tt>Feature</tt> is now [new behavior]."
- "Enhanced/Improved/Updated [thing] [description of change]."
- "Renamed <tt>OldName</tt> to <tt>NewName</tt>. The old name still works."

**Bug fixes:**
- Always prefixed: "<i>Fix:</i> Description of what was wrong and what is now correct."
- For documentation-only fixes: "<i>Documentation fix:</i> Description."
- Describe the symptom, not just the fix. Include "broken since X.Y.Z" or "broken in X.Y.Z" when known.

**Removals:**
- "Removed support for [feature]." or "Support for [feature] has been dropped."
- "Removed [thing]. Use [replacement] instead."

**Deprecations:**
- "This function is deprecated." followed by what to use instead.
- "The compiler still accepts <tt>OldName</tt> as an alias for <tt>NewName</tt>, but will emit a deprecation warning when used."

## Subcategories

Group related changes under subcategory headers using nested lists:
```html
<li>Pascal Scripting:
<ul>
  <li>Added new support function <tt>Foo</tt>.</li>
  <li><i>Fix:</i> <tt>Bar</tt> now works correctly.</li>
</ul>
</li>
```

Standard subcategory names: "Compiler IDE:", "Pascal Scripting:", "ISPP:", "Setup:", "Compiler:", "Command-Line Compiler (ISCC):", "Examples:", "Security improvement:", "Updated documentation:", and product-specific names such as "Inno Setup Signature Tool:". Other one-off names are acceptable when a group of entries warrants its own heading.

## Catch-all entries

End a version's changes with one of:
- "Other minor improvements."
- "Minor tweaks."
- "Minor tweaks and fixes."
- "Other minor improvements and fixes."
- "Documentation improvements."

## Example script references

- "See updated <i><a href="...">ScriptName.iss</a></i> example script."
- "See updated <i><a href="...">ScriptName.iss</a></i> example script for an example."
- "See new <i><a href="...">ScriptName.iss</a></i> example script."

## Security section intro

When there is a security improvements section:
> Updating is recommended, even if you don't plan to use the other enhancements right away. We continually add extra checks to make your installers safer and more reliable. In this version:
