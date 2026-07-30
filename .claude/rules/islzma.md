---
paths: ["Projects/Src/Compression.LZMACompressor/islzma/**", "Projects/Src/Compression.LZMADecompressor/Lzma2Decode/**"]
---
# Instructions for the native LZMA C code (islzma and Lzma2Decode)

## Indentation

These files keep their existing indentation style; the two-space rule in `indentation.md` does not apply:
- `islzma.c`, `islzma.h`, `islzma_dll.c`, `islzma_exe.c`, `ISLzmaDec.c`, and `test/test.bat` are tab-indented.
- `test/roundtrip.c` is four-space indented.

Do not reformat these files. When editing, match the surrounding indentation of the file you are in.
