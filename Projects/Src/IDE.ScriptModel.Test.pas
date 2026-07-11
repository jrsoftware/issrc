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

type
  TChangeCounter = class
    Count: Integer;
    procedure HandleChange(Sender: TObject);
  end;

procedure TChangeCounter.HandleChange(Sender: TObject);
begin
  Inc(Count);
end;

procedure TestLineHelpers;
begin
  { ScriptLineSpans: length of at least 3, ending in '\' preceded by whitespace }
  Assert(ScriptLineSpans('a \'));
  Assert(ScriptLineSpans('ab '#9'\'));
  Assert(not ScriptLineSpans(' \'));  { Too short }
  Assert(not ScriptLineSpans('ab\')); { No whitespace before the backslash }
  Assert(not ScriptLineSpans('abc'));
  Assert(not ScriptLineSpans(''));

  { ClassifyScriptLine }
  Assert(ClassifyScriptLine('') = slkBlank);
  Assert(ClassifyScriptLine('   ') = slkBlank);
  Assert(ClassifyScriptLine('; comment') = slkComment);
  Assert(ClassifyScriptLine('  ; comment') = slkComment);
  Assert(ClassifyScriptLine('// comment') = slkComment);
  Assert(ClassifyScriptLine(' #define X 1') = slkISPPDirective);
  Assert(ClassifyScriptLine(';#define X 1') = slkComment);
  Assert(ClassifyScriptLine('Source: a') = slkActual);
  Assert(ClassifyScriptLine('[Files]') = slkActual);

  { JoinSpannedScriptLines: single lines are untouched, spanned groups lose
    the backslash (keeping the whitespace before it) and each line's leading
    whitespace }
  Assert(JoinSpannedScriptLines(['  x']) = '  x');
  Assert(JoinSpannedScriptLines(['Source: "a"; \', '  DestDir: "b"']) =
    'Source: "a"; DestDir: "b"');
  Assert(JoinSpannedScriptLines(['A=1 \', ' 2 \', ' 3']) = 'A=1 2 3');

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

  { Directive line helpers }
  var NameText, RawValue: String;
  Assert(TryParseScriptDirectiveLine('AppName = Foo', NameText, RawValue));
  Assert((NameText = 'AppName ') and (RawValue = ' Foo'));
  Assert(not TryParseScriptDirectiveLine('No directive here', NameText, RawValue));
  Assert(not TryParseScriptDirectiveLine(' = Foo', NameText, RawValue));
  Assert(GetScriptDirectiveDisplayValue(' "My ""quoted"" App" ') = 'My ""quoted"" App');
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

    {$IFDEF ISTESTTOOLPROJ}
    { Values with line breaks and malformed parameter names raise, leaving the
      entry untouched: such text would break apart on the next parse }
    Entry.Parse(['A: 1']);
    var Caught := False;
    try
      Entry.SetValue('A', 'x'#13#10'y');
    except
      on EScriptModelError do Caught := True;
    end;
    Assert(Caught);
    Caught := False;
    try
      Entry.SetValue('', 'x');
    except
      on EScriptModelError do Caught := True;
    end;
    Assert(Caught);
    Caught := False;
    try
      Entry.SetValue('B;C', 'x');
    except
      on EScriptModelError do Caught := True;
    end;
    Assert(Caught);
    Caught := False;
    try
      Entry.SetValue('B C', 'x');
    except
      on EScriptModelError do Caught := True;
    end;
    Assert(Caught);
    Assert(not Entry.Modified);
    Lines := Entry.GetLines;
    Assert(Lines[0] = 'A: 1');
    {$ENDIF}
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

    {$IFDEF ISTESTTOOLPROJ}
    { Flag names that cannot be a single unquoted token raise, leaving the
      entry untouched }
    Entry.Parse(['Flags: a']);
    var Caught := False;
    try
      Entry.SetFlag('Flags', '', True);
    except
      on EScriptModelError do Caught := True;
    end;
    Assert(Caught);
    Caught := False;
    try
      Entry.SetFlag('Flags', 'x y', True);
    except
      on EScriptModelError do Caught := True;
    end;
    Assert(Caught);
    Caught := False;
    try
      Entry.SetFlag('Flags', 'x"y', True);
    except
      on EScriptModelError do Caught := True;
    end;
    Assert(Caught);
    Assert(not Entry.Modified);
    {$ENDIF}
  finally
    Entry.Free;
  end;
end;

procedure TestEntryMetadata;
begin
  var Metadata: TScriptSectionMetadata;
  Assert(TryGetScriptSectionMetadata('files', Metadata)); { Case-insensitive }
  Assert(Metadata.SectionName = 'Files');
  Assert(not TryGetScriptSectionMetadata('Setup', Metadata));
  Assert(TryGetScriptSectionMetadata('Files', Metadata));

  const Entry = TScriptParameterEntry.Create(Metadata);
  try
    Entry.Parse(['Source: a; ExternalSize: 1_048_576; Unknown: u']);

    Entry.SetValue('ExternalSize', '456');
    const Lines = Entry.GetLines;
    { The ExternalSize parameter-includes-flag rule also checks external }
    Assert(Lines[0] = 'Source: a; ExternalSize: 456; Unknown: u; Flags: external');

    var Definition: TScriptParameterDefinition;
    Assert(Entry.TryGetParameterDefinition('flags', Definition));
    Assert(Definition.ValueKind = pvkFlags);
    var FoundFlagName := False;
    for var KnownValue in Definition.KnownValues do
      if KnownValue = 'ignoreversion' then
        FoundFlagName := True;
    Assert(FoundFlagName);
    Assert(Entry.TryGetParameterDefinition('ExternalSize', Definition));
    Assert(Definition.ValueKind = pvkInteger);
    Assert(Entry.TryGetParameterDefinition('MinVersion', Definition));
    Assert(Definition.ValueKind = pvkVersion);

    { Unknown parameters remain accessible as raw text }
    Assert(not Entry.TryGetParameterDefinition('Unknown', Definition));
    Assert(Entry.GetValue('Unknown') = 'u');
  finally
    Entry.Free;
  end;
end;

procedure TestScriptCategories;
begin
  { The categories are shown in this order: the section-specific groups first,
    then Common (the inspector shows the Debug group after these) }
  const Names = ScriptCategoryNames;
  Assert(Length(Names) = 5);
  Assert(Names[0] = 'Compiler');
  Assert(Names[1] = 'Compression');
  Assert(Names[2] = 'Installer');
  Assert(Names[3] = 'Cosmetic');
  Assert(Names[4] = 'Common');

  { Membership maps a name to its category, case-insensitively. Common applies
    to the parameter sections only, so the unrelated [Setup] directive of the
    same name groups with the other [Setup] directives instead }
  var CategoryName: String;
  Assert(TryGetScriptCategory('Files', 'minversion', CategoryName) and
    (CategoryName = 'Common'));
  Assert(TryGetScriptCategory('Setup', 'minversion', CategoryName) and
    (CategoryName = 'Installer'));
  Assert(not TryGetScriptCategory('Setup', 'Check', CategoryName));

  { A category applies only in the sections it lists, so a user-chosen name in
    a section without a fixed name set (such as [CustomMessages]) cannot group }
  Assert(not TryGetScriptCategory('CustomMessages', 'Tasks', CategoryName));
  Assert(not TryGetScriptCategory('CustomMessages', 'AppName', CategoryName));

  { The [Setup] directive categories apply only in [Setup] }
  Assert(TryGetScriptCategory('Setup', 'SolidCompression', CategoryName) and
    (CategoryName = 'Compression'));
  Assert(TryGetScriptCategory('Setup', 'LZMADictionarySize', CategoryName) and
    (CategoryName = 'Compression'));
  Assert(TryGetScriptCategory('Setup', 'SignTool', CategoryName) and
    (CategoryName = 'Compiler'));
  Assert(TryGetScriptCategory('Setup', 'AppName', CategoryName) and
    (CategoryName = 'Installer'));
  Assert(TryGetScriptCategory('Setup', 'wizardstyle', CategoryName) and
    (CategoryName = 'Cosmetic'));

  { A name shared between a [Setup] directive and a parameter of another section
    groups only in [Setup] }
  Assert(TryGetScriptCategory('Setup', 'ExtraDiskSpaceRequired', CategoryName) and
    (CategoryName = 'Installer'));
  Assert(not TryGetScriptCategory('Components', 'ExtraDiskSpaceRequired', CategoryName));
  Assert(not TryGetScriptCategory('Languages', 'LicenseFile', CategoryName));
  Assert(not TryGetScriptCategory('Files', 'AppName', CategoryName));
  Assert(not TryGetScriptCategory('Files', 'Source', CategoryName));

  { Obsolete parameters are flagged in the metadata }
  var Metadata: TScriptSectionMetadata;
  Assert(TryGetScriptSectionMetadata('Files', Metadata));
  var Definition: TScriptParameterDefinition;
  Assert(Metadata.TryGetParameter('CopyMode', Definition));
  Assert(Definition.Obsolete);
  Assert(Metadata.TryGetParameter('Source', Definition));
  Assert(not Definition.Obsolete);
end;

procedure TestEntryRules;
begin
  var Metadata: TScriptSectionMetadata;
  Assert(TryGetScriptSectionMetadata('Files', Metadata));

  const Counter = TChangeCounter.Create;
  var Entry := TScriptParameterEntry.Create(Metadata);
  try
    { Checking extractarchive also checks external and ignoreversion, in one
      change notification, with unknown tokens preserved }
    Entry.Parse(['Source: a; Flags: foo']);
    Entry.OnChange := Counter.HandleChange;
    Entry.SetFlag('Flags', 'extractarchive', True);
    Assert(Counter.Count = 1);
    Assert(Entry.FlagIncluded('Flags', 'extractarchive'));
    Assert(Entry.FlagIncluded('Flags', 'external'));
    Assert(Entry.FlagIncluded('Flags', 'ignoreversion'));
    Assert(Entry.FlagIncluded('Flags', 'foo'));
    var Lines := Entry.GetLines;
    Assert(Lines[0] = 'Source: a; Flags: foo extractarchive external ignoreversion');

    { The rule does not run when the flags are already present }
    Counter.Count := 0;
    Entry.SetFlag('Flags', 'extractarchive', True);
    Assert(Counter.Count = 0);

    { Excluding does not fire the rule }
    Entry.SetFlag('Flags', 'extractarchive', False);
    Assert(Entry.FlagIncluded('Flags', 'external'));
    Lines := Entry.GetLines;
    Assert(Lines[0] = 'Source: a; Flags: foo external ignoreversion');

    { The other [Files] flag-includes rules from the help }
    Entry.Parse(['Source: a']);
    Entry.SetFlag('Flags', 'createallsubdirs', True);
    Assert(Entry.FlagIncluded('Flags', 'recursesubdirs'));
    Entry.Parse(['Source: a']);
    Entry.SetFlag('Flags', 'dontverifychecksum', True);
    Assert(Entry.FlagIncluded('Flags', 'nocompression'));
    Entry.Parse(['Source: a']);
    Entry.SetFlag('Flags', 'uninsnosharedfileprompt', True);
    Assert(Entry.FlagIncluded('Flags', 'sharedfile'));

    { The [Files] parameter-includes-flag rules from the help: an ExternalSize
      value checks external and an ISSigAllowedKeys value checks issigverify }
    Entry.Parse(['Source: a']);
    Entry.SetValue('ExternalSize', '123');
    Assert(Entry.FlagIncluded('Flags', 'external'));
    Entry.Parse(['Source: a']);
    Entry.SetValue('ISSigAllowedKeys', 'mykey');
    Assert(Entry.FlagIncluded('Flags', 'issigverify'));

    { Without metadata there are no rules }
    Entry.Free;
    Entry := TScriptParameterEntry.Create(nil);
    Entry.Parse(['Source: a']);
    Entry.SetFlag('Flags', 'extractarchive', True);
    Assert(Entry.FlagIncluded('Flags', 'extractarchive'));
    Assert(not Entry.FlagIncluded('Flags', 'external'));
  finally
    Entry.Free;
    Counter.Free;
  end;
end;

procedure TestDirectiveSection;
begin
  const Counter = TChangeCounter.Create;
  const Section = TScriptDirectiveSection.Create;
  try
    { Duplicates are both kept; the value scan returns the last occurrence;
      unknown directives, comments, blank lines and ISPP lines are opaque }
    Section.Parse([
      '; comment',
      'AppName=Foo',
      '',
      'AppName = Bar ',
      'Unknown=1',
      '#define X 1']);
    Assert(Section.Count = 6);
    Assert(Section[0].Kind = sdlOther);
    Assert(Section[1].Kind = sdlDirective);
    Assert(Section[1].Name = 'AppName');
    Assert(Section[2].Kind = sdlOther);
    Assert(Section[3].Kind = sdlDirective);
    Assert(Section[3].DisplayValue = 'Bar');
    Assert(Section[4].Kind = sdlDirective);
    Assert(Section[5].Kind = sdlOther);
    var Value: String;
    Assert(Section.TryGetDirectiveValue('appname', Value));
    Assert(Value = 'Bar'); { Last occurrence }
    Assert(not Section.TryGetDirectiveValue('AppVersion', Value));
    Assert(Section.IndexOfDirective('AppName') = 3);

    { Untouched sections round-trip byte-identical }
    var Lines := Section.GetLines;
    Assert(Length(Lines) = 6);
    Assert(Lines[0] = '; comment');
    Assert(Lines[3] = 'AppName = Bar ');
    Assert(Lines[5] = '#define X 1');

    { Directive values keep surrounding quotes out of the display value,
      without treating embedded quotes as doubled }
    Section.Parse(['AppName="My ""quoted"" App"']);
    Assert(Section[0].DisplayValue = 'My ""quoted"" App');

    { Editing a directive only rewrites that line, keeping the name and the
      whitespace around the '=' as written }
    Section.Parse(['; c', 'AppName = Foo', 'Other=1']);
    Section.OnChange := Counter.HandleChange;
    Section.SetDirectiveValue(1, 'Bar');
    Assert(Counter.Count = 1);
    Lines := Section.GetLines;
    Assert(Length(Lines) = 3);
    Assert(Lines[0] = '; c');
    Assert(Lines[1] = 'AppName = Bar');
    Assert(Lines[2] = 'Other=1');

    { A value needing whitespace gets quotes }
    Section.SetDirectiveValue(1, 'B ');
    Lines := Section.GetLines;
    Assert(Lines[1] = 'AppName = "B "');

    { Adding inserts after the last directive; removing removes only that
      line }
    Assert(Section.AddDirective('AppVersion', '1.0') = 3);
    Lines := Section.GetLines;
    Assert(Length(Lines) = 4);
    Assert(Lines[3] = 'AppVersion=1.0');
    Section.RemoveDirective(1);
    Lines := Section.GetLines;
    Assert(Length(Lines) = 3);
    Assert(Lines[0] = '; c');
    Assert(Lines[1] = 'Other=1');
    Assert(Lines[2] = 'AppVersion=1.0');

    {$IFDEF ISTESTTOOLPROJ}
    { Opaque lines cannot be edited or removed; comments must survive }
    var Caught := False;
    try
      Section.RemoveDirective(0);
    except
      on EScriptModelError do Caught := True;
    end;
    Assert(Caught);
    Caught := False;
    try
      Section.SetDirectiveValue(0, 'x');
    except
      on EScriptModelError do Caught := True;
    end;
    Assert(Caught);
    {$ENDIF}

    { With no directives yet, adding appends at the end }
    Section.Parse(['; only comment']);
    Assert(Section.AddDirective('A', '1') = 1);
    Lines := Section.GetLines;
    Assert(Length(Lines) = 2);
    Assert(Lines[1] = 'A=1');

    { Editing a directive keeps its own quoting: a quoted value stays quoted }
    Section.Parse(['AppName="Foo"']);
    Section.SetDirectiveValue(0, 'Bar');
    Lines := Section.GetLines;
    Assert(Lines[0] = 'AppName="Bar"');

    { A value ending in whitespace + '\' gets quotes so the written line is
      not read back as an ISPP line continuation }
    Section.Parse(['AppName=Foo']);
    Section.SetDirectiveValue(0, 'Bar \');
    Lines := Section.GetLines;
    Assert(Lines[0] = 'AppName="Bar \"');

    { A value that itself looks quoted gets surrounding quotes so the literal
      quotes survive read-back, both when editing and when adding }
    Section.Parse(['AppName=Foo']);
    Section.SetDirectiveValue(0, '"Bar"');
    Lines := Section.GetLines;
    Assert(Lines[0] = 'AppName=""Bar""');
    Assert(Section[0].DisplayValue = '"Bar"');
    Section.Parse(['; c']);
    Section.AddDirective('AppName', '"Foo"');
    Lines := Section.GetLines;
    Assert(Lines[1] = 'AppName=""Foo""');
    Assert(Section[1].DisplayValue = '"Foo"');

    { A new directive is left bare by default (QuoteNewValues off for
      directive-style sections) and quoted when the option is turned on }
    Section.Parse(['; c']);
    Assert(not Section.QuoteNewValues);
    Section.AddDirective('AppName', 'Foo');
    Lines := Section.GetLines;
    Assert(Lines[1] = 'AppName=Foo');
    Section.Parse(['; c']);
    Section.QuoteNewValues := True;
    Section.AddDirective('AppName', 'Foo');
    Lines := Section.GetLines;
    Assert(Lines[1] = 'AppName="Foo"');
    Section.QuoteNewValues := False;

    {$IFDEF ISTESTTOOLPROJ}
    { Values with line breaks and names that would not read back as the same
      directive raise, leaving the section untouched }
    Section.Parse(['AppName=Foo']);
    Caught := False;
    try
      Section.SetDirectiveValue(0, 'a'#13#10'b');
    except
      on EScriptModelError do Caught := True;
    end;
    Assert(Caught);
    Caught := False;
    try
      Section.AddDirective('', 'x');
    except
      on EScriptModelError do Caught := True;
    end;
    Assert(Caught);
    Caught := False;
    try
      Section.AddDirective('A=B', 'x');
    except
      on EScriptModelError do Caught := True;
    end;
    Assert(Caught);
    Caught := False;
    try
      Section.AddDirective('; comment', 'x');
    except
      on EScriptModelError do Caught := True;
    end;
    Assert(Caught);
    Caught := False;
    try
      Section.AddDirective('AppName ', 'x');
    except
      on EScriptModelError do Caught := True;
    end;
    Assert(Caught);
    Caught := False;
    try
      Section.AddDirective('AppName', 'a'#10'b');
    except
      on EScriptModelError do Caught := True;
    end;
    Assert(Caught);
    Lines := Section.GetLines;
    Assert(Length(Lines) = 1);
    Assert(Lines[0] = 'AppName=Foo');
    {$ENDIF}
  finally
    Section.Free;
    Counter.Free;
  end;
end;

procedure TestEntrySpanning;
begin
  const Entry = TScriptParameterEntry.Create(nil);
  try
    { A spanned entry parses from its physical lines and remembers the break
      at parameter granularity }
    Entry.Parse(['Source: "a"; \', '  DestDir: "b"; Flags: x']);
    Assert(Entry.ParameterCount = 3);
    Assert(Entry.GetValue('DestDir') = 'b');
    Assert(Entry.BreakCount = 1);
    Assert(Entry.BreakParameterIndexes[0] = 1);

    { Untouched spanned entries round-trip byte-identical }
    var Lines := Entry.GetLines;
    Assert(Length(Lines) = 2);
    Assert(Lines[0] = 'Source: "a"; \');
    Assert(Lines[1] = '  DestDir: "b"; Flags: x');

    { Editing a middle parameter keeps the author's line structure, and its
      quoting (DestDir was quoted) }
    Entry.SetValue('DestDir', 'c');
    Lines := Entry.GetLines;
    Assert(Length(Lines) = 2);
    Assert(Lines[0] = 'Source: "a"; \');
    Assert(Lines[1] = '  DestDir: "c"; Flags: x');

    { Editing the first parameter also keeps the break, and its quoting }
    Entry.Parse(['Source: "a"; \', '  DestDir: "b"; Flags: x']);
    Entry.SetValue('Source', 'z');
    Lines := Entry.GetLines;
    Assert(Length(Lines) = 2);
    Assert(Lines[0] = 'Source: "z"; \');
    Assert(Lines[1] = '  DestDir: "b"; Flags: x');

    { When the parameter at a break point is removed, that break is dropped
      and the remainder goes to the last surviving line }
    Entry.Parse(['Source: "a"; \', '  DestDir: "b"; Flags: x']);
    Assert(Entry.RemoveParameter('DestDir'));
    Lines := Entry.GetLines;
    Assert(Length(Lines) = 1);
    Assert(Lines[0] = 'Source: "a"; Flags: x');

    { Three physical lines, edit in the middle }
    Entry.Parse(['A: 1; \', 'B: 2; \', 'C: 3']);
    Assert(Entry.BreakCount = 2);
    Entry.SetValue('B', '22');
    Lines := Entry.GetLines;
    Assert(Length(Lines) = 3);
    Assert(Lines[0] = 'A: 1; \');
    Assert(Lines[1] = 'B: 22; \');
    Assert(Lines[2] = 'C: 3');

    { Removing a middle parameter drops its own break and shifts the following
      break back onto its now-earlier parameter }
    Entry.Parse(['A: 1; \', 'B: 2; \', 'C: 3']);
    Assert(Entry.RemoveParameter('B'));
    Lines := Entry.GetLines;
    Assert(Length(Lines) = 2);
    Assert(Lines[0] = 'A: 1; \');
    Assert(Lines[1] = 'C: 3');

    { Unusual whitespace around the break reconstructs exactly }
    Entry.Parse(['Source: a ;  \', '   DestDir: b']);
    Entry.SetValue('Source', 'z');
    Lines := Entry.GetLines;
    Assert(Length(Lines) = 2);
    Assert(Lines[0] = 'Source: z ;  \');
    Assert(Lines[1] = '   DestDir: b');
  finally
    Entry.Free;
  end;

  { A spanned directive line is joined for parsing and collapses to one
    physical line when edited }
  const Section = TScriptDirectiveSection.Create;
  try
    Section.Parse(['AppName=Foo \', 'Bar']);
    Assert(Section.Count = 1);
    Assert(Section[0].Kind = sdlDirective);
    Assert(Section[0].DisplayValue = 'Foo Bar');
    var Lines := Section.GetLines;
    Assert(Length(Lines) = 2);
    Assert(Lines[0] = 'AppName=Foo \');
    Assert(Lines[1] = 'Bar');
    Section.SetDirectiveValue(0, 'X');
    Lines := Section.GetLines;
    Assert(Length(Lines) = 1);
    Assert(Lines[0] = 'AppName=X');
  finally
    Section.Free;
  end;
end;

procedure IDEScriptModelRunTests;
begin
  TestLineHelpers;
  TestEntryParseAndSerialize;
  TestEntryFlags;
  TestEntryMetadata;
  TestScriptCategories;
  TestEntryRules;
  TestDirectiveSection;
  TestEntrySpanning;
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
