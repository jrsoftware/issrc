---
paths: ["Files/Languages/*.isl", "Files/Languages/Unofficial/*.isl"]
---
# Conventions for Inno Setup translation files

These files translate `Files/Default.isl` into another language. `Default.isl` is the English baseline and the source of truth for the message set; it changes between Inno Setup versions, so always work from the most recent `Default.isl`, read it fresh, and treat its `[Messages]` and `[CustomMessages]` IDs as authoritative. A translation must stay faithful to its English counterpart: the English is the fixed reference and the ceiling, not something to clarify, complete, or improve.

## Quality rules

### File format and structure
- **Encoding.** UTF-8 without a BOM.
- **Newlines.** Windows CRLF throughout.
- **Extension.** `.isl`. The `.islu` extension is obsolete.
- **Compiles cleanly.** The file must compile with no errors and no warnings.

### Comment header (top-of-file comments)
- The first line is the version line, usually `; *** Inno Setup version X+ ... messages ***`. Keep it consistent with the `Default.isl` header version. It must never be higher than the `Default.isl` version.
- The top of the file must also carry, as comments, the language's English name and the maintainer's name and e-mail address.

### `[LangOptions]` settings
- **LanguageName.** The language's native name (the name as written in the language itself), not its English name. For example Dutch is `Nederlands`, not `Dutch`. It is shown in the *Select Language* dialog. The obsolete `<HHHH>` escape syntax still works but is no longer needed; write the literal characters directly.
- **LanguageID.** A valid hexadecimal language identifier beginning with `$`, matching the language and its region (for example `$0413` for Dutch (Netherlands), `$0813` for Belgian Dutch). It must not be left at `$0409`, which is the English ID of `Default.isl`. Verify against Microsoft's language-identifier list (https://learn.microsoft.com/en-us/windows/win32/intl/language-identifier-constants-and-strings). If no identifier exists for the language, set it to `0`. Together with `LanguageCodePage` it is used to auto-detect the most appropriate default language, so set it correctly.
- **LanguageCodePage.** The Windows ANSI code page the compiler uses to convert any non-Unicode text in the language's files to Unicode (for example 1252 Western European, 1250 Central European, 1251 Cyrillic, 1253 Greek, 1254 Turkish, 1255 Hebrew, 1256 Arabic, 1257 Baltic, 1258 Vietnamese, 874 Thai, 932 Japanese, 936 Chinese Simplified, 949 Korean, 950 Chinese Traditional). Set it to `0` only when no code page exists for the language (a Unicode-only script such as Hindi, Tamil, Bengali, Armenian, Georgian) or the language uses only ASCII characters. It still matters even though the `.isl` is UTF-8, because it converts non-Unicode files such as a README supplied by the script author. (Text in the `.iss` file itself, such as a `[CustomMessages]` entry, is never converted and must already be Unicode.)
- **RightToLeft.** `RightToLeft=yes` must be set for a right-to-left script (Arabic, Hebrew, Persian (Farsi), Urdu, Uyghur) and must not be set (or `RightToLeft=no`) for a left-to-right language.
- **Fonts** (`DialogFontName`, `DialogFontSize`, `WelcomeFontName`, and so on). Leave at the `Default.isl` defaults unless the language needs different faces or sizes to render correctly.

### Sections and message set (compared to `Default.isl`)
- **Translate the existing entries only.** Provide every `[Messages]` and `[CustomMessages]` ID that `Default.isl` defines, translated. Do not add your own custom messages to `[CustomMessages]`, and do not invent new IDs.
- **No missing IDs.** Every ID defined in `Default.isl` must be present.
- **No obsolete IDs.** Do not keep IDs that no longer exist in `Default.isl`.
- **Empty values.** A message that is empty in `Default.isl` must stay empty in the translation, except for the `TranslatorNote` message.

### Per-message rules (each message compared to its English counterpart)
- **Placeholder integrity.** The same set and count of `%1`..`%9` must be present (the order may differ to suit the target grammar, and an argument may legitimately be duplicated, such as `%1 ... %1 %2`). Keep `%n` line breaks and `%%` literal-percent escapes intact. Preserve the same bracketed runtime placeholders, currently `[name]`, `[name/ver]`, `[mb]`, and `[gb]`. Never drop, add, or malform a placeholder or format specifier.
- **Placeholder grammar.** A placeholder is filled at runtime with a value the translator cannot see or inflect, and which is not always the expected token (a wizard-name `%1`, for example, can be a custom name). Phrase the surrounding text so it reads correctly for any value: keep the placeholder in a slot that needs no inflection of, or grammatical agreement with, the inserted value, and never assume the value's gender, number, or case. If the English drops the value into a slot your language would inflect or agree with, rebuild the clause rather than guessing one form.
- **Accelerator keys.** Keep an `&` accelerator wherever the English has one. Do not drop an accelerator, and do not add one where the English has none. Within each accelerator scope every accelerator letter must be unique, compared case-insensitively.
- **No added periods.** Do not add a period to the end of a message that did not have one: Inno Setup appends the period automatically for those messages, so a manual one would show two. Keep a trailing period where the English has one. If the English ends with `...`, keep exactly three dots; do not replace them with a single period or a single ellipsis character.
- **Faithful meaning.** Stay as close to the original English as possible. Do not add to or change the meaning of a message to suit personal taste (for example do not append "Try again one more time." to an error message that did not say that).

### Translation quality (linguistic)
- No spelling or grammar errors.
- No untranslated messages that should have been translated. Some values are legitimately identical to English (`Setup`, `OK`, brand names, placeholder-only messages); only genuine omissions are wrong.
- Consistent terminology: the same concept uses the same term across every message, with consistent button-label style and capitalization.
- Use the language's native standard terminology, taking installer terms from the matching official Setup translation and everything else from Microsoft Terminology plus the language's Style Guide (for example the standard local words for Setup, folder, uninstall, administrator).
- Consistent, language-appropriate tone, formality, casing, punctuation, and quote style (for example German capitalizes nouns and addresses the user formally; Japanese uses full-width punctuation where the platform does).
- Translate naturally. As loose guidance stay near the English length (roughly within about 150 percent), but naturalness wins.

## Faithfulness check before changing a message

Before changing a translated message because it looks wrong, confirm all of these; if any fails, leave it alone:
1. Quote the exact English string. If you can't, you haven't compared against it.
2. The current translation differs from what the English *means*, not from a clearer or more explicit version you imagined. If the English is itself ambiguous, general, or underspecified, an equally ambiguous translation is correct, not a problem.
3. Your change preserves the English meaning. It must not add precision, disambiguation, or detail the English does not have.
4. It is substantive. Awkward-but-acceptable phrasing, debatable terminology, and pure style preferences are nitpicks; leave them.
5. Back-translate both the current text and your replacement into English. Change it only if the current text back-translates to a different meaning than the English *and* your replacement back-translates to the same meaning. If the replacement back-translates to something the English does not say, it adds meaning; drop it.
