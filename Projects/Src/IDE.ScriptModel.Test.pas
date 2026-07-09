unit IDE.ScriptModel.Test;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Test unit for IDE.ScriptModel and IDE.ScriptModel.Metadata

  Runs a self-test if DEBUG is defined
}

interface

procedure IDEScriptModelRunTests;

implementation

uses
  {$IFDEF DEBUG} Winapi.Windows, {$ENDIF} System.SysUtils,
  IDE.ScriptModel, IDE.ScriptModel.Metadata;

{$C+}

procedure TestLineHelpers;
begin
  { ScriptLineSpans: length of at least 3, ending in '\' preceded by whitespace }
  Assert(ScriptLineSpans('a \'));
  Assert(ScriptLineSpans('ab '#9'\'));
  Assert(not ScriptLineSpans(' \'));  { Too short }
  Assert(not ScriptLineSpans('ab\')); { No whitespace before the backslash }
  Assert(not ScriptLineSpans('abc'));
  Assert(not ScriptLineSpans(''));

  { Quoting helpers }
  Assert(UnquoteScriptParameterValue(' "a""b" ') = 'a"b');
  Assert(UnquoteScriptParameterValue('x') = 'x');
  Assert(UnquoteScriptParameterValue('"x') = '"x'); { No closing quote }
  Assert(QuoteScriptParameterValueIfNeeded('x y') = 'x y');
  Assert(QuoteScriptParameterValueIfNeeded('a;b') = '"a;b"');
  Assert(QuoteScriptParameterValueIfNeeded('a"b') = '"a""b"');
  Assert(QuoteScriptParameterValueIfNeeded(' x') = '" x"');
  Assert(QuoteScriptParameterValueIfNeeded('') = '');
  { A value ending in whitespace + '\' is quoted so the written line is not
    read back as an ISPP line continuation; one ending in '\' with no preceding
    whitespace needs no quoting. A value of just '\' is quoted too: the
    separator written before the value ends in whitespace }
  Assert(QuoteScriptParameterValueIfNeeded('a \') = '"a \"');
  Assert(QuoteScriptParameterValueIfNeeded('a\') = 'a\');
  Assert(QuoteScriptParameterValueIfNeeded('\') = '"\"');
  { Forced quoting still doubles embedded quotes }
  Assert(QuoteScriptParameterValueIfNeeded('x y', True) = '"x y"');
  Assert(QuoteScriptParameterValueIfNeeded('a"b', True) = '"a""b"');
end;

procedure TestEntryParseAndSerialize;
begin
  const Entry = TScriptParameterEntry.Create(nil);
  try
    { Basic parse of known and unknown parameters }
    Entry.Parse(['Source: "My Prog.exe"; DestDir: "{app}"; Flags: ignoreversion']);
    Assert(Entry.ParameterCount = 3);
    Assert(Entry.Parameters[0].Name = 'Source');
    Assert(Entry.Parameters[0].HasName);
    Assert(Entry.Parameters[0].Value = 'My Prog.exe');
    Assert(Entry.GetValue('destdir') = '{app}'); { Case-insensitive }
    Assert(Entry.HasParameter('Flags'));
    Assert(not Entry.HasParameter('Missing'));
    var Value: String;
    Assert(not Entry.TryGetValue('Missing', Value));
    Assert(not Entry.Modified);

    { Untouched entries round-trip byte-identical }
    var Lines := Entry.GetLines;
    Assert(Length(Lines) = 1);
    Assert(Lines[0] = 'Source: "My Prog.exe"; DestDir: "{app}"; Flags: ignoreversion');

    { Doubled quotes in values }
    Entry.Parse(['Name: "a""b"']);
    Assert(Entry.GetValue('Name') = 'a"b');

    { Empty value and a trailing semicolon (which parses as an opaque empty
      chunk) survive }
    Entry.Parse(['DestName: ; Foo: 1;']);
    Assert(Entry.TryGetValue('DestName', Value) and (Value = ''));
    Assert(Entry.GetValue('Foo') = '1');
    Assert(Entry.ParameterCount = 3);
    Assert(not Entry.Parameters[2].HasName);
    Lines := Entry.GetLines;
    Assert(Lines[0] = 'DestName: ; Foo: 1;');

    { Writing to a parameter whose old value is empty keeps the whitespace
      after the colon once: an all-whitespace raw value is all leading, not
      leading plus trailing }
    Entry.SetValue('DestName', 'x');
    Lines := Entry.GetLines;
    Assert(Lines[0] = 'DestName: x; Foo: 1;');

    { Garbage input is kept as opaque raw text }
    Entry.Parse(['%$#@!']);
    Assert(Entry.ParameterCount = 1);
    Assert(not Entry.Parameters[0].HasName);
    Lines := Entry.GetLines;
    Assert(Lines[0] = '%$#@!');

    { Editing a value keeps that parameter's own quoting: a quoted value stays
      quoted even when the new value would not need it, and untouched
      parameters keep their raw text }
    Entry.Parse(['A: "x"; B: "y"']);
    Entry.SetValue('A', 'new');
    Assert(Entry.Modified);
    Lines := Entry.GetLines;
    Assert(Lines[0] = 'A: "new"; B: "y"');
    Entry.SetValue('B', 'a;b');
    Lines := Entry.GetLines;
    Assert(Lines[0] = 'A: "new"; B: "a;b"');
    Entry.SetValue('B', ' x');
    Lines := Entry.GetLines;
    Assert(Lines[0] = 'A: "new"; B: " x"');

    { An unquoted value stays unquoted when the new value needs no quotes }
    Entry.Parse(['A: x']);
    Entry.SetValue('A', 'y');
    Lines := Entry.GetLines;
    Assert(Lines[0] = 'A: y');

    { A new parameter is quoted when QuoteNewValues is on (the default for
      parameter sections) and left bare when it is off }
    Entry.Parse(['A: 1']);
    Assert(Entry.QuoteNewValues);
    Entry.SetValue('B', '2');
    Lines := Entry.GetLines;
    Assert(Lines[0] = 'A: 1; B: "2"');
    Entry.Parse(['A: 1']);
    Entry.QuoteNewValues := False;
    Entry.SetValue('B', '2');
    Lines := Entry.GetLines;
    Assert(Lines[0] = 'A: 1; B: 2');
    Entry.QuoteNewValues := True;

    { Whitespace around names and values is preserved on edit }
    Entry.Parse(['Source :  x  ; B: y']);
    Assert(Entry.GetValue('Source') = 'x');
    Entry.SetValue('Source', 'z');
    Lines := Entry.GetLines;
    Assert(Lines[0] = 'Source :  z  ; B: y');

    { The first line's indentation is preserved }
    Entry.Parse(['  Source: a']);
    Assert(Entry.Indent = '  ');
    Entry.SetValue('Source', 'b');
    Lines := Entry.GetLines;
    Assert(Lines[0] = '  Source: b');

    { Removing a parameter }
    Entry.Parse(['A: 1; B: 2; C: 3']);
    Assert(Entry.RemoveParameter('B'));
    Assert(not Entry.RemoveParameter('B'));
    Lines := Entry.GetLines;
    Assert(Lines[0] = 'A: 1; C: 3');
  finally
    Entry.Free;
  end;
end;

procedure TestEntryFlags;
begin
  const Entry = TScriptParameterEntry.Create(nil);
  try
    { Toggling a known flag amid unknown ones only edits that token }
    Entry.Parse(['Flags: foo ignoreversion bar']);
    Assert(Entry.FlagIncluded('Flags', 'IGNOREVERSION'));
    Assert(not Entry.FlagIncluded('Flags', 'missing'));
    Entry.SetFlag('Flags', 'ignoreversion', False);
    var Lines := Entry.GetLines;
    Assert(Lines[0] = 'Flags: foo bar');
    Entry.SetFlag('Flags', 'solidbreak', True);
    Lines := Entry.GetLines;
    Assert(Lines[0] = 'Flags: foo bar solidbreak');
    Entry.SetFlag('Flags', 'solidbreak', False);
    Lines := Entry.GetLines;
    Assert(Lines[0] = 'Flags: foo bar');

    { Excluding the last token removes the whole parameter }
    Entry.Parse(['Source: s; Flags: x']);
    Entry.SetFlag('Flags', 'x', False);
    Lines := Entry.GetLines;
    Assert(Lines[0] = 'Source: s');
    Assert(not Entry.HasParameter('Flags'));

    { Including a flag that is already there changes nothing }
    Entry.Parse(['Flags: a']);
    Entry.SetFlag('Flags', 'a', True);
    Assert(not Entry.Modified);

    { Setting a flag on an absent parameter creates it, unquoted even when the
      new-value quoting option is on (flags are never quoted) }
    Entry.Parse(['Source: s']);
    Assert(Entry.QuoteNewValues);
    Entry.SetFlag('Flags', 'touch', True);
    Lines := Entry.GetLines;
    Assert(Lines[0] = 'Source: s; Flags: touch');

    { Excluding a flag also removes author-written duplicates of it }
    Entry.Parse(['Flags: ignoreversion foo ignoreversion']);
    Entry.SetFlag('Flags', 'ignoreversion', False);
    Assert(not Entry.FlagIncluded('Flags', 'ignoreversion'));
    Lines := Entry.GetLines;
    Assert(Lines[0] = 'Flags: foo');
    Entry.Parse(['Source: s; Flags: x x']);
    Entry.SetFlag('Flags', 'x', False);
    Assert(not Entry.HasParameter('Flags'));

    { Tokens are delimited by literal spaces and trimmed, like the compiler's
      ExtractFlag: a tab alone does not delimit, a tab next to a space does }
    Entry.Parse(['Flags: a'#9'b']);
    Assert(not Entry.FlagIncluded('Flags', 'a'));
    Assert(not Entry.FlagIncluded('Flags', 'b'));
    Entry.Parse(['Flags: a'#9' b']);
    Assert(Entry.FlagIncluded('Flags', 'a'));
    Assert(Entry.FlagIncluded('Flags', 'b'));
    Entry.SetFlag('Flags', 'a', False);
    Lines := Entry.GetLines;
    Assert(Lines[0] = 'Flags: b');
  finally
    Entry.Free;
  end;
end;

procedure IDEScriptModelRunTests;
begin
  TestLineHelpers;
  TestEntryParseAndSerialize;
  TestEntryFlags;
end;

{$IFDEF DEBUG}
{$IFNDEF ISTESTTOOLPROJ}
initialization
  try
    IDEScriptModelRunTests;
  except on E: Exception do
    begin
      MessageBox(0, PChar(E.Message), '', MB_OK);
      raise;
    end;
  end;
{$ENDIF}
{$ENDIF}

end.
