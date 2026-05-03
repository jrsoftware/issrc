---
paths: ["**/*Test.pas", "**/Script.Test.iss"]
---
# Instructions for adding tests

## When a correct test fails: allow investigation instead of pure tweaking

If a test that looks correct fails at compile time or at runtime, **do not silently adjust it until it passes**. A test that matches spec behavior but fails is evidence of a bug, not a bad test. The right response is:

1. **Keep the original test** in the test source file, commented out, with a short explanation of the bug it exposed (e.g. `{ bug: 'for I: Byte := 0 to 255' infinite-loops because the loop variable wraps from 255 to 0 }`).
2. **Write an alternate working test** that covers the same functionality without hitting the bug, if one exists. For example, use a narrower range or a different loop structure. This keeps test coverage moving forward.
3. **Fixing the bug and re-enabling the original test is a separate task** - do not block the current phase on it.

Tweaking a test to avoid a real bug - changing types, adding casts, restructuring loops - hides regressions and defeats the purpose of the test suite.
