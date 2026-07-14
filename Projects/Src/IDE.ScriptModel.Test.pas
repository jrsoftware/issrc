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
  Shared.SetupSectionDirectives, Shared.LangOptionsSectionDirectives,
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
  Assert(UnquoteScriptDirectiveValue(' "My ""quoted"" App" ') = 'My ""quoted"" App');
end;

procedure TestEntryParseAndSerialize;
begin
  const Entry = TScriptParameterEntry.Create(nil);
  try
    { Basic parse of known and unknown parameters }
    Entry.Parse(['Source: "My Prog.exe"; DestDir: "{app}"; Flags: ignoreversion']);
    Assert(Entry.Count = 3);
    Assert(Entry.Parameters[0].Name = 'Source');
    Assert(Entry.Parameters[0].Kind = sepParameter);
    Assert(Entry.Parameters[0].Value = 'My Prog.exe');
    var Value: String;
    Assert(Entry.TryGetValue('destdir', Value) and (Value = '{app}')); { Case-insensitive }
    Assert(Entry.Has('Flags'));
    Assert(not Entry.Has('Missing'));
    Assert(not Entry.TryGetValue('Missing', Value));
    Assert(not Entry.Modified);

    { Untouched entries round-trip byte-identical }
    var Lines := Entry.GetLines;
    Assert(Length(Lines) = 1);
    Assert(Lines[0] = 'Source: "My Prog.exe"; DestDir: "{app}"; Flags: ignoreversion');

    { Doubled quotes in values }
    Entry.Parse(['Name: "a""b"']);
    Assert(Entry.TryGetValue('Name', Value) and (Value = 'a"b'));

    { Empty value and a trailing semicolon (which parses as an opaque empty
      chunk) survive }
    Entry.Parse(['DestName: ; Foo: 1;']);
    Assert(Entry.TryGetValue('DestName', Value) and (Value = ''));
    Assert(Entry.TryGetValue('Foo', Value) and (Value = '1'));
    Assert(Entry.Count = 3);
    Assert(Entry.Parameters[2].Kind = sepOther);
    Lines := Entry.GetLines;
    Assert(Lines[0] = 'DestName: ; Foo: 1;');

    { Writing to a parameter whose old value is empty keeps the whitespace
      after the colon once: an all-whitespace raw value is all leading, not
      leading plus trailing }
    Entry.SetValue(0, 'x');
    Lines := Entry.GetLines;
    Assert(Lines[0] = 'DestName: x; Foo: 1;');

    { Garbage input is kept as opaque raw text }
    Entry.Parse(['%$#@!']);
    Assert(Entry.Count = 1);
    Assert(Entry.Parameters[0].Kind = sepOther);
    Lines := Entry.GetLines;
    Assert(Lines[0] = '%$#@!');

    { Editing a value keeps that parameter's own quoting: a quoted value stays
      quoted even when the new value would not need it, and untouched
      parameters keep their raw text }
    Entry.Parse(['A: "x"; B: "y"']);
    Entry.SetValue(0, 'new');
    Assert(Entry.Modified);
    Lines := Entry.GetLines;
    Assert(Lines[0] = 'A: "new"; B: "y"');
    Entry.SetValue(1, 'a;b');
    Lines := Entry.GetLines;
    Assert(Lines[0] = 'A: "new"; B: "a;b"');
    Entry.SetValue(1, ' x');
    Lines := Entry.GetLines;
    Assert(Lines[0] = 'A: "new"; B: " x"');

    { An unquoted value stays unquoted when the new value needs no quotes }
    Entry.Parse(['A: x']);
    Entry.SetValue(0, 'y');
    Lines := Entry.GetLines;
    Assert(Lines[0] = 'A: y');

    { A new parameter is quoted when QuoteNewValues is on (the default for
      parameter sections) and left bare when it is off }
    Entry.Parse(['A: 1']);
    Assert(Entry.QuoteNewValues);
    Entry.Add('B', '2');
    Lines := Entry.GetLines;
    Assert(Lines[0] = 'A: 1; B: "2"');
    Entry.Parse(['A: 1']);
    Entry.QuoteNewValues := False;
    Entry.Add('B', '2');
    Lines := Entry.GetLines;
    Assert(Lines[0] = 'A: 1; B: 2');
    Entry.QuoteNewValues := True;

    { Whitespace around names and values is preserved on edit }
    Entry.Parse(['Source :  x  ; B: y']);
    Assert(Entry.TryGetValue('Source', Value) and (Value = 'x'));
    Entry.SetValue(0, 'z');
    Lines := Entry.GetLines;
    Assert(Lines[0] = 'Source :  z  ; B: y');

    { The first line's indentation is preserved }
    Entry.Parse(['  Source: a']);
    Assert(Entry.Indent = '  ');
    Entry.SetValue(0, 'b');
    Lines := Entry.GetLines;
    Assert(Lines[0] = '  Source: b');

    { Removing a parameter }
    Entry.Parse(['A: 1; B: 2; C: 3']);
    Entry.Remove(1);
    Assert(not Entry.Has('B'));
    Lines := Entry.GetLines;
    Assert(Lines[0] = 'A: 1; C: 3');

    { Removing the first parameter also removes the separator whitespace the
      next chunk carried, so no stray leading space serializes }
    Entry.Parse(['A: 1; B: 2']);
    Entry.Remove(0);
    Lines := Entry.GetLines;
    Assert(Lines[0] = 'B: 2');

    { A duplicated parameter is invalid script, but each occurrence is
      addressable and editable on its own }
    Entry.Parse(['A: B; A: C']);
    Assert(Entry.Count = 2);
    Assert(Entry.Parameters[0].Value = 'B');
    Assert(Entry.Parameters[1].Value = 'C');
    Assert(Entry.IndexOf('A') = 0); { By-name lookup resolves to the first occurrence }
    var Index := -1;
    Assert(Entry.TryResolve('A', Index) and (Index = 0)); { -1 resolves by name }
    Index := 1;
    Assert(Entry.TryResolve('a', Index) and (Index = 1)); { A given index is kept }
    Index := 0;
    Assert(not Entry.TryResolve('B', Index)); { Name mismatch }
    Index := 2;
    Assert(not Entry.TryResolve('A', Index)); { Out of range }
    Entry.SetValue(1, 'D');
    Lines := Entry.GetLines;
    Assert(Lines[0] = 'A: B; A: D');

    {$IFDEF ISTESTTOOLPROJ}
    { Values with line breaks and malformed parameter names raise, leaving the
      entry untouched: such text would break apart on the next parse }
    Entry.Parse(['A: 1']);
    var Caught := False;
    try
      Entry.SetValue(0, 'x'#13#10'y');
    except
      on EScriptModelError do Caught := True;
    end;
    Assert(Caught);
    Caught := False;
    try
      Entry.Add('', 'x');
    except
      on EScriptModelError do Caught := True;
    end;
    Assert(Caught);
    Caught := False;
    try
      Entry.Add('B;C', 'x');
    except
      on EScriptModelError do Caught := True;
    end;
    Assert(Caught);
    Caught := False;
    try
      Entry.Add('B C', 'x');
    except
      on EScriptModelError do Caught := True;
    end;
    Assert(Caught);
    Assert(not Entry.Modified);
    Lines := Entry.GetLines;
    Assert(Lines[0] = 'A: 1');
    { Index access requires a named parameter }
    Entry.Parse(['A: 1; ; B: 2']);
    Caught := False;
    try
      Entry.SetValue(1, 'x'); { The middle chunk has no name }
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

procedure TestEntryFlags;
begin
  const Entry = TScriptParameterEntry.Create(nil);
  try
    { Toggling a known flag amid unknown ones only edits that token }
    Entry.Parse(['Flags: foo ignoreversion bar']);
    Assert(Entry.FlagIncluded(0, 'IGNOREVERSION'));
    Assert(not Entry.FlagIncluded(0, 'missing'));
    Entry.SetFlag(0, 'ignoreversion', False);
    var Lines := Entry.GetLines;
    Assert(Lines[0] = 'Flags: foo bar');
    Entry.SetFlag(0, 'solidbreak', True);
    Lines := Entry.GetLines;
    Assert(Lines[0] = 'Flags: foo bar solidbreak');
    Entry.SetFlag(0, 'solidbreak', False);
    Lines := Entry.GetLines;
    Assert(Lines[0] = 'Flags: foo bar');

    { Excluding the last token removes the whole parameter }
    Entry.Parse(['Source: s; Flags: x']);
    Entry.SetFlag(1, 'x', False);
    Lines := Entry.GetLines;
    Assert(Lines[0] = 'Source: s');
    Assert(not Entry.Has('Flags'));

    { Also when the removed parameter is the entry's first }
    Entry.Parse(['Flags: x; Source: s']);
    Entry.SetFlag(0, 'x', False);
    Lines := Entry.GetLines;
    Assert(Lines[0] = 'Source: s');

    { Including a flag that is already there changes nothing }
    Entry.Parse(['Flags: a']);
    Entry.SetFlag(0, 'a', True);
    Assert(not Entry.Modified);

    { Setting a flag on an absent parameter: without metadata the added
      parameter is presumed text, so the new-value quoting option applies }
    Entry.Parse(['Source: s']);
    Assert(Entry.QuoteNewValues);
    Entry.SetFlag(Entry.Add('Flags', ''), 'touch', True);
    Lines := Entry.GetLines;
    Assert(Lines[0] = 'Source: s; Flags: "touch"');

    { Excluding a flag also removes author-written duplicates of it }
    Entry.Parse(['Flags: ignoreversion foo ignoreversion']);
    Entry.SetFlag(0, 'ignoreversion', False);
    Assert(not Entry.FlagIncluded(0, 'ignoreversion'));
    Lines := Entry.GetLines;
    Assert(Lines[0] = 'Flags: foo');
    Entry.Parse(['Source: s; Flags: x x']);
    Entry.SetFlag(1, 'x', False);
    Assert(not Entry.Has('Flags'));

    { Tokens are delimited by literal spaces and trimmed, like the compiler's
      ExtractFlag: a tab alone does not delimit, a tab next to a space does }
    Entry.Parse(['Flags: a'#9'b']);
    Assert(not Entry.FlagIncluded(0, 'a'));
    Assert(not Entry.FlagIncluded(0, 'b'));
    Entry.Parse(['Flags: a'#9' b']);
    Assert(Entry.FlagIncluded(0, 'a'));
    Assert(Entry.FlagIncluded(0, 'b'));
    Entry.SetFlag(0, 'a', False);
    Lines := Entry.GetLines;
    Assert(Lines[0] = 'Flags: b');

    {$IFDEF ISTESTTOOLPROJ}
    { Flag names that cannot be a single unquoted token raise, leaving the
      entry untouched }
    Entry.Parse(['Flags: a']);
    var Caught := False;
    try
      Entry.SetFlag(0, '', True);
    except
      on EScriptModelError do Caught := True;
    end;
    Assert(Caught);
    Caught := False;
    try
      Entry.SetFlag(0, 'x y', True);
    except
      on EScriptModelError do Caught := True;
    end;
    Assert(Caught);
    Caught := False;
    try
      Entry.SetFlag(0, 'x"y', True);
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
  Assert(not TryGetScriptSectionMetadata('Code', Metadata));
  Assert(TryGetScriptSectionMetadata('Files', Metadata));

  const Entry = TScriptParameterEntry.Create(Metadata);
  try
    Entry.Parse(['Source: a; ExternalSize: 1_048_576; Unknown: u']);

    Entry.SetValue(1, '456');
    const Lines = Entry.GetLines;
    { The ExternalSize parameter-includes-flag rule also checks external }
    Assert(Lines[0] = 'Source: a; ExternalSize: 456; Unknown: u; Flags: external');

    var Definition: TScriptParameterDefinition;
    Assert(Entry.TryGetDefinition('flags', Definition));
    Assert(Definition.ValueKind = pvkFlags);
    var FoundFlagName := False;
    for var KnownValue in Definition.KnownValues do
      if KnownValue = 'ignoreversion' then
        FoundFlagName := True;
    Assert(FoundFlagName);
    Assert(Entry.TryGetDefinition('ExternalSize', Definition));
    Assert(Definition.ValueKind = pvkInteger);
    Assert(Entry.TryGetDefinition('MinVersion', Definition));
    Assert(Definition.ValueKind = pvkVersion);

    { Unknown parameters remain accessible as raw text }
    Assert(not Entry.TryGetDefinition('Unknown', Definition));
    var Value: String;
    Assert(Entry.TryGetValue('Unknown', Value) and (Value = 'u'));

    { Only text parameters are quoted by default when added: a pvkVersion
      value is written bare, a pvkString value is quoted }
    Entry.Parse(['Source: a']);
    Assert(Entry.QuoteNewValues);
    Entry.Add('MinVersion', '6.2');
    Entry.Add('DestName', 'x');
    const AddedLines = Entry.GetLines;
    Assert(AddedLines[0] = 'Source: a; MinVersion: 6.2; DestName: "x"');
  finally
    Entry.Free;
  end;
end;

procedure TestDirectiveSectionMetadata;
begin
  { The [Setup] and [LangOptions] tables are generated from the compiler's
    enums: every directive present, in canonical enum order }
  var Metadata: TScriptSectionMetadata;
  Assert(TryGetScriptSectionMetadata('Setup', Metadata));
  Assert(Metadata.SectionName = 'Setup');
  Assert(Length(Metadata.Parameters) = Ord(High(TSetupSectionDirective))+1);
  Assert(Metadata.Parameters[0].Name = 'AllowCancelDuringInstall');
  var Definition: TScriptParameterDefinition;
  Assert(Metadata.TryGetParameter('AppName', Definition));
  Assert(Definition.ValueKind = pvkString);
  Assert(not Definition.Obsolete);
  Assert(Definition.DefaultValue = '');
  { A yes/no directive still lists its two values, for the drop-down of the
    inspector's text-row fallback when the value isn't yes/no }
  Assert(Metadata.TryGetParameter('SolidCompression', Definition));
  Assert(Definition.ValueKind = pvkYesNo);
  Assert(Length(Definition.KnownValues) = 2);
  Assert(Definition.KnownValues[0] = 'yes');
  Assert(Definition.KnownValues[1] = 'no');
  Assert(Definition.DefaultValue = 'no');
  Assert(Metadata.TryGetParameter('AllowNetworkDrive', Definition));
  Assert(Definition.ValueKind = pvkYesNo);
  Assert(Definition.DefaultValue = 'yes');
  { The auto/yes/no and yes/no-or-scripted directives allow other values, so
    they are choices and not yes/no, and each kind has exceptions to its usual
    default }
  Assert(Metadata.TryGetParameter('DisableDirPage', Definition));
  Assert(Definition.ValueKind = pvkChoice);
  Assert(Length(Definition.KnownValues) = 3);
  Assert(Definition.KnownValues[0] = 'auto');
  Assert(Definition.KnownValues[1] = 'yes');
  Assert(Definition.KnownValues[2] = 'no');
  Assert(Definition.DefaultValue = 'auto');
  Assert(Metadata.TryGetParameter('ShowLanguageDialog', Definition));
  Assert(Definition.DefaultValue = 'yes');
  Assert(Metadata.TryGetParameter('Uninstallable', Definition));
  Assert(Definition.ValueKind = pvkChoice);
  Assert(Length(Definition.KnownValues) = 2);
  Assert(Definition.KnownValues[0] = 'yes');
  Assert(Definition.KnownValues[1] = 'no');
  Assert(Definition.DefaultValue = 'yes');
  Assert(Metadata.TryGetParameter('ChangesAssociations', Definition));
  Assert(Definition.DefaultValue = 'no');
  Assert(Metadata.TryGetParameter('ChangesEnvironment', Definition));
  Assert(Definition.DefaultValue = 'no');
  { The choice directives list their values, including Compression's computed
    list }
  Assert(Metadata.TryGetParameter('Compression', Definition));
  Assert(Definition.ValueKind = pvkChoice);
  Assert(Length(Definition.KnownValues) = 33); { none + zip and bzip with 9 levels each + lzma and lzma2 with 5 levels each }
  Assert(Definition.KnownValues[0] = 'none');
  Assert(Definition.KnownValues[1] = 'zip');
  Assert(Definition.KnownValues[2] = 'zip/1');
  Assert(Definition.KnownValues[32] = 'lzma2/ultra64');
  Assert(Definition.DefaultValue = 'lzma2/max');
  Assert(Metadata.TryGetParameter('LZMAUseSeparateProcess', Definition));
  Assert(Definition.ValueKind = pvkChoice);
  Assert(Length(Definition.KnownValues) = 3);
  Assert(Definition.KnownValues[0] = 'x86');
  Assert(Definition.DefaultValue = 'no');
  Assert(Metadata.TryGetParameter('UninstallLogMode', Definition));
  Assert(Definition.ValueKind = pvkChoice);
  Assert(Length(Definition.KnownValues) = 3);
  Assert(Definition.KnownValues[0] = 'append');
  { The multi-value and expression directives are not choices: their word
    lists are editor autocomplete data, kept by the styler }
  Assert(Metadata.TryGetParameter('ArchitecturesAllowed', Definition));
  Assert(Definition.ValueKind = pvkString);
  Assert(Metadata.TryGetParameter('WizardStyle', Definition));
  Assert(Definition.ValueKind = pvkString);
  { The integer directives, but not the ones with richer forms like
    DiskSliceSize's 'max' and CompressionThreads' 'auto', and the version
    directives like their parameter-table counterparts }
  Assert(Metadata.TryGetParameter('ReserveBytes', Definition));
  Assert(Definition.ValueKind = pvkInteger);
  Assert(Definition.DefaultValue = '0');
  Assert(Metadata.TryGetParameter('UninstallDisplaySize', Definition));
  Assert(Definition.ValueKind = pvkInteger);
  Assert(Definition.DefaultValue = ''); { Calculated automatically when not set }
  Assert(Metadata.TryGetParameter('DiskSliceSize', Definition));
  Assert(Definition.ValueKind = pvkString);
  Assert(Metadata.TryGetParameter('CompressionThreads', Definition));
  Assert(Definition.ValueKind = pvkString);
  Assert(Metadata.TryGetParameter('MinVersion', Definition));
  Assert(Definition.ValueKind = pvkVersion);
  Assert(Definition.DefaultValue = '6.1sp1');
  Assert(Metadata.TryGetParameter('DefaultGroupName', Definition));
  Assert(Definition.ValueKind = pvkString);
  Assert(Definition.DefaultValue = '(Default)');
  Assert(Metadata.TryGetParameter('UninstallStyle', Definition));
  Assert(Definition.Obsolete);
  { Every directive of a yes/no kind has a default: none was left out of the
    generator's default-yes, default-no, and default-auto sets }
  for var Directive := Low(TSetupSectionDirective) to High(TSetupSectionDirective) do begin
    if (Directive in SetupSectionDirectivesYesNo) or
       (Directive in SetupSectionDirectivesAutoYesNo) or
       (Directive in SetupSectionDirectivesYesNoOrScripted) then
      Assert(Metadata.Parameters[Ord(Directive)].DefaultValue <> '');
  end;

  { The section model exposes the definitions like the entry model does }
  const Section = TScriptDirectiveSection.Create(Metadata);
  try
    Assert(Section.Metadata = Metadata);
    Assert(Section.TryGetDefinition('solidcompression', Definition)); { Case-insensitive }
    Assert(Definition.ValueKind = pvkYesNo);
    Assert(not Section.TryGetDefinition('NoSuchDirective', Definition));

    { With the quoting option on, only text directives are quoted: a yes/no
      or integer value is written bare }
    Section.QuoteNewValues := True;
    Section.Add('SolidCompression', 'yes');
    Section.Add('AppName', 'My App');
    Section.Add('ReserveBytes', '4096');
    const Lines = Section.GetLines;
    Assert(Lines[0] = 'SolidCompression=yes');
    Assert(Lines[1] = 'AppName="My App"');
    Assert(Lines[2] = 'ReserveBytes=4096');
  finally
    Section.Free;
  end;
  const SectionWithoutMetadata = TScriptDirectiveSection.Create(nil);
  try
    Assert(SectionWithoutMetadata.Metadata = nil);
    Assert(not SectionWithoutMetadata.TryGetDefinition('AppName', Definition));
  finally
    SectionWithoutMetadata.Free;
  end;

  Assert(TryGetScriptSectionMetadata('LangOptions', Metadata));
  Assert(Metadata.SectionName = 'LangOptions');
  Assert(Length(Metadata.Parameters) = Ord(High(TLangOptionsSectionDirective))+1);
  Assert(Metadata.Parameters[0].Name = 'CopyrightFontName');
  Assert(Metadata.Parameters[0].Obsolete);
  Assert(Metadata.TryGetParameter('RightToLeft', Definition));
  Assert(Definition.ValueKind = pvkYesNo);
  Assert(Length(Definition.KnownValues) = 2); { Like SolidCompression above }
  Assert(Definition.KnownValues[0] = 'yes');
  Assert(Definition.KnownValues[1] = 'no');
  Assert(Definition.DefaultValue = 'no');
  Assert(Metadata.TryGetParameter('LanguageName', Definition));
  Assert(Definition.ValueKind = pvkString);
  Assert(Definition.DefaultValue = 'English');
  { The integer directives, including LanguageID whose '$'-prefixed hex form
    is still a plain integer }
  Assert(Metadata.TryGetParameter('LanguageID', Definition));
  Assert(Definition.ValueKind = pvkInteger);
  Assert(Definition.DefaultValue = '$0409');
  Assert(Metadata.TryGetParameter('DialogFontSize', Definition));
  Assert(Definition.ValueKind = pvkInteger);
  Assert(Definition.DefaultValue = '9');

  { [Messages] names are localized message identifiers and [CustomMessages]
    names are user-defined, so neither has a table }
  Assert(not TryGetScriptSectionMetadata('Messages', Metadata));
  Assert(not TryGetScriptSectionMetadata('CustomMessages', Metadata));
end;

procedure TestSectionMetadataTables;
begin
  { All parameter sections have a table }
  const SectionNames: TArray<String> = [
    'Components', 'Dirs', 'Files', 'Icons', 'INI', 'InstallDelete',
    'ISSigKeys', 'Languages', 'Registry', 'Run', 'Tasks', 'Types',
    'UninstallDelete', 'UninstallRun'];
  var Metadata: TScriptSectionMetadata;
  for var SectionName in SectionNames do begin
    Assert(TryGetScriptSectionMetadata(SectionName, Metadata));
    Assert(Metadata.SectionName = SectionName);
    Assert(Length(Metadata.Parameters) > 0);
  end;
  Assert(not TryGetScriptSectionMetadata('Code', Metadata));

  { [Registry] value types differ from [Files]: Root/ValueType/ValueData }
  Assert(TryGetScriptSectionMetadata('Registry', Metadata));
  const RegistryEntry = TScriptParameterEntry.Create(Metadata);
  try
    RegistryEntry.Parse(['Root: HKA; Subkey: "Software\My Company"; ' +
      'ValueType: string; ValueName: "Path"; ValueData: "{app}"; ' +
      'Flags: uninsdeletekey']);
    var Value: String;
    Assert(RegistryEntry.TryGetValue('Root', Value) and (Value = 'HKA'));
    Assert(RegistryEntry.TryGetValue('ValueData', Value) and (Value = '{app}'));
    Assert(RegistryEntry.FlagIncluded(5, 'uninsdeletekey'));
    var Definition: TScriptParameterDefinition;
    Assert(RegistryEntry.TryGetDefinition('Flags', Definition));
    Assert(Definition.ValueKind = pvkFlags);
    var FoundFlagName := False;
    for var KnownValue in Definition.KnownValues do
      if KnownValue = 'preservestringtype' then
        FoundFlagName := True;
    Assert(FoundFlagName);
    { No rules registered for [Registry]: toggling runs no implications }
    RegistryEntry.SetFlag(5, 'deletevalue', True);
    const Lines = RegistryEntry.GetLines;
    Assert(Lines[0] = 'Root: HKA; Subkey: "Software\My Company"; ' +
      'ValueType: string; ValueName: "Path"; ValueData: "{app}"; ' +
      'Flags: uninsdeletekey deletevalue');
  finally
    RegistryEntry.Free;
  end;

  { [Run] flags }
  Assert(TryGetScriptSectionMetadata('Run', Metadata));
  const RunEntry = TScriptParameterEntry.Create(Metadata);
  try
    RunEntry.Parse(['Filename: "{app}\MyProg.exe"; Flags: nowait postinstall skipifsilent']);
    Assert(RunEntry.FlagIncluded(1, 'postinstall'));
    RunEntry.SetFlag(1, 'postinstall', False);
    const Lines = RunEntry.GetLines;
    Assert(Lines[0] = 'Filename: "{app}\MyProg.exe"; Flags: nowait skipifsilent');
    var Definition: TScriptParameterDefinition;
    Assert(RunEntry.TryGetDefinition('Flags', Definition));
    var FoundFlagName := False;
    for var KnownValue in Definition.KnownValues do
      if KnownValue = 'runasoriginaluser' then
        FoundFlagName := True;
    Assert(FoundFlagName);
  finally
    RunEntry.Free;
  end;

  { [Components] has an integer parameter }
  Assert(TryGetScriptSectionMetadata('Components', Metadata));
  var Definition: TScriptParameterDefinition;
  Assert(Metadata.TryGetParameter('ExtraDiskSpaceRequired', Definition));
  Assert(Definition.ValueKind = pvkInteger);

  { Single-choice parameters carry their known values }
  Assert(TryGetScriptSectionMetadata('Registry', Metadata));
  Assert(Metadata.TryGetParameter('Root', Definition));
  Assert(Definition.ValueKind = pvkChoice);
  Assert(Length(Definition.KnownValues) > 0);
  Assert(Metadata.TryGetParameter('ValueType', Definition));
  Assert(Definition.ValueKind = pvkChoice);
  Assert(TryGetScriptSectionMetadata('InstallDelete', Metadata));
  Assert(Metadata.TryGetParameter('Type', Definition));
  Assert(Definition.ValueKind = pvkChoice);
end;

procedure TestMetadataConsistency;

  function InNames(const Names: TArray<String>; const Name: String): Boolean;
  begin
    Result := False;
    for var KnownName in Names do
      if SameText(KnownName, Name) then
        Exit(True);
  end;

begin
  { Structural checks over every parameter section's table. They catch a
    mistyped rule or flag that the behavioral tests would only find if they
    happened to exercise that exact token }
  const SectionNames: TArray<String> = [
    'Components', 'Dirs', 'Files', 'Icons', 'INI', 'InstallDelete',
    'ISSigKeys', 'Languages', 'Registry', 'Run', 'Tasks', 'Types',
    'UninstallDelete', 'UninstallRun'];
  const CommonMemberNames: TArray<String> = ['Check', 'Components', 'Tasks',
    'Languages', 'MinVersion', 'OnlyBelowVersion', 'BeforeInstall',
    'AfterInstall'];
  for var SectionName in SectionNames do begin
    var Metadata: TScriptSectionMetadata;
    Assert(TryGetScriptSectionMetadata(SectionName, Metadata));

    { Every parameter has a unique name, and only flags and choices carry
      tokens, which are themselves non-empty and unique (and lowercase and
      sorted for flags) }
    for var I := 0 to High(Metadata.Parameters) do begin
      const Parameter = Metadata.Parameters[I];
      Assert(Parameter.Name <> '');
      for var J := 0 to I-1 do
        Assert(not SameText(Metadata.Parameters[J].Name, Parameter.Name));
      if Parameter.ValueKind in [pvkFlags, pvkChoice] then begin
        Assert(Length(Parameter.KnownValues) > 0);
        for var K := 0 to High(Parameter.KnownValues) do begin
          const Token = Parameter.KnownValues[K];
          Assert(Token <> '');
          for var L := 0 to K-1 do
            Assert(not SameText(Parameter.KnownValues[L], Token));
          if Parameter.ValueKind = pvkFlags then begin
            Assert(Token = LowerCase(Token));
            { The inspector gives a flag parameter one child row per flag, in
              table order, so the table decides the order the flags are shown in.
              Choices are not checked: they fill a dropdown, where the table's
              order is meaningful, such as the default first }
            if K > 0 then
              Assert(CompareText(Parameter.KnownValues[K-1], Token) < 0);
          end;
        end;
      end else
        Assert(Length(Parameter.KnownValues) = 0);

      { A parameter shared with the Common category groups under it, so a
        section cannot be forgotten in Common's section list }
      if InNames(CommonMemberNames, Parameter.Name) then begin
        var CategoryName: String;
        Assert(TryGetScriptCategory(SectionName, Parameter.Name, CategoryName) and
          (CategoryName = 'Common'));
      end;
    end;

    { Every flag-includes rule points at a real flag parameter and names only
      flags that exist, so it cannot silently never fire }
    for var Rule in Metadata.FlagIncludesRules do begin
      var Definition: TScriptParameterDefinition;
      Assert(Metadata.TryGetParameter(Rule.ParameterName, Definition));
      Assert(Definition.ValueKind = pvkFlags);
      Assert(InNames(Definition.KnownValues, Rule.FlagName));
      Assert(Length(Rule.OtherFlagNames) > 0);
      for var FlagName in Rule.OtherFlagNames do
        Assert(InNames(Definition.KnownValues, FlagName));
    end;

    { Every parameter-includes-flag rule references a real trigger parameter
      and a real flag in a real flag parameter }
    for var Rule in Metadata.ParameterIncludesFlagRules do begin
      var Definition: TScriptParameterDefinition;
      Assert(Metadata.TryGetParameter(Rule.ParameterName, Definition));
      var FlagDefinition: TScriptParameterDefinition;
      Assert(Metadata.TryGetParameter(Rule.FlagParameterName, FlagDefinition));
      Assert(FlagDefinition.ValueKind = pvkFlags);
      Assert(InNames(FlagDefinition.KnownValues, Rule.FlagName));
    end;

    { Every flag-excludes rule points at a real flag parameter, names only
      flags that exist, and does not exclude its own flag }
    for var Rule in Metadata.FlagExcludesRules do begin
      var Definition: TScriptParameterDefinition;
      Assert(Metadata.TryGetParameter(Rule.ParameterName, Definition));
      Assert(Definition.ValueKind = pvkFlags);
      Assert(InNames(Definition.KnownValues, Rule.FlagName));
      Assert(Length(Rule.OtherFlagNames) > 0);
      for var FlagName in Rule.OtherFlagNames do begin
        Assert(InNames(Definition.KnownValues, FlagName));
        Assert(not SameText(FlagName, Rule.FlagName));
      end;
    end;

    { An includes rule must not contradict an excludes rule: the flags an
      includes rule turns on together must not exclude one another }
    for var IncludeRule in Metadata.FlagIncludesRules do begin
      const IncludedNames: TArray<String> =
        [IncludeRule.FlagName] + IncludeRule.OtherFlagNames;
      for var ExcludeRule in Metadata.FlagExcludesRules do begin
        if SameText(IncludeRule.ParameterName, ExcludeRule.ParameterName) and
           InNames(IncludedNames, ExcludeRule.FlagName) then
          for var FlagName in ExcludeRule.OtherFlagNames do
            Assert(not InNames(IncludedNames, FlagName));
      end;
    end;
  end;
end;

procedure TestScriptCategories;
begin
  { The categories are shown in this order: the section-specific groups first,
    then Common (the inspector shows the Debug group after these) }
  const Names = ScriptCategoryNamesOrdered;
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
  Assert(TryGetScriptSectionMetadata('UninstallRun', Metadata));
  Assert(Metadata.TryGetParameter('StatusMsg', Definition));
  Assert(Definition.Obsolete);
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
    Entry.SetFlag(1, 'extractarchive', True);
    Assert(Counter.Count = 1);
    Assert(Entry.FlagIncluded(1, 'extractarchive'));
    Assert(Entry.FlagIncluded(1, 'external'));
    Assert(Entry.FlagIncluded(1, 'ignoreversion'));
    Assert(Entry.FlagIncluded(1, 'foo'));
    var Lines := Entry.GetLines;
    Assert(Lines[0] = 'Source: a; Flags: foo extractarchive external ignoreversion');

    { The rule does not run when the flags are already present }
    Counter.Count := 0;
    Entry.SetFlag(1, 'extractarchive', True);
    Assert(Counter.Count = 0);

    { Excluding does not fire the rule }
    Entry.SetFlag(1, 'extractarchive', False);
    Assert(Entry.FlagIncluded(1, 'external'));
    Lines := Entry.GetLines;
    Assert(Lines[0] = 'Source: a; Flags: foo external ignoreversion');

    { The other [Files] flag-includes rules from the help. The rules also fire
      on a just-added Flags parameter }
    Entry.Parse(['Source: a']);
    Entry.SetFlag(Entry.Add('Flags', ''), 'createallsubdirs', True);
    Assert(Entry.FlagIncluded(1, 'recursesubdirs'));
    { Flags is pvkFlags, so the quoting option does not apply }
    Lines := Entry.GetLines;
    Assert(Lines[0] = 'Source: a; Flags: createallsubdirs recursesubdirs');
    Entry.Parse(['Source: a']);
    Entry.SetFlag(Entry.Add('Flags', ''), 'dontverifychecksum', True);
    Assert(Entry.FlagIncluded(1, 'nocompression'));
    Entry.Parse(['Source: a']);
    Entry.SetFlag(Entry.Add('Flags', ''), 'uninsnosharedfileprompt', True);
    Assert(Entry.FlagIncluded(1, 'sharedfile'));

    { The [Files] parameter-includes-flag rules from the help: an ExternalSize
      value checks external and an ISSigAllowedKeys value checks issigverify,
      with the rule-created Flags parameter appended after the added one }
    Entry.Parse(['Source: a']);
    Entry.Add('ExternalSize', '123');
    Assert(Entry.FlagIncluded(2, 'external'));
    Entry.Parse(['Source: a']);
    Entry.Add('ISSigAllowedKeys', 'mykey');
    Assert(Entry.FlagIncluded(2, 'issigverify'));

    { A DownloadISSigSource value checks both download and issigverify, and the
      download flag's own flag-includes rule cascades }
    Entry.Parse(['Source: a']);
    Entry.Add('DownloadISSigSource', 'https://example.com/setup.bin.issig');
    Assert(Entry.FlagIncluded(2, 'download'));
    Assert(Entry.FlagIncluded(2, 'issigverify'));
    Assert(Entry.FlagIncluded(2, 'external'));
    Assert(Entry.FlagIncluded(2, 'ignoreversion'));

    { The other download and extract parameters, and StrongAssemblyName }
    Entry.Parse(['Source: a']);
    Entry.Add('DownloadUserName', 'myuser');
    Assert(Entry.FlagIncluded(2, 'download'));
    Entry.Parse(['Source: a']);
    Entry.Add('DownloadPassword', 'mypassword');
    Assert(Entry.FlagIncluded(2, 'download'));
    Entry.Parse(['Source: a']);
    Entry.Add('ExtractArchivePassword', 'mypassword');
    Assert(Entry.FlagIncluded(2, 'extractarchive'));
    Entry.Parse(['Source: a']);
    Entry.Add('StrongAssemblyName', 'MyAssembly');
    Assert(Entry.FlagIncluded(2, 'gacinstall'));

    { A parameter value can also include a flag: setting Verb on a [Run] entry
      checks shellexec and setting OnLog checks logoutput, each in one change }
    Assert(TryGetScriptSectionMetadata('Run', Metadata));
    Entry.Free;
    Entry := TScriptParameterEntry.Create(Metadata);
    Entry.Parse(['Filename: a']);
    Entry.OnChange := Counter.HandleChange;
    Counter.Count := 0;
    Entry.Add('Verb', 'open');
    Assert(Counter.Count = 1);
    Assert(Entry.FlagIncluded(2, 'shellexec'));

    { Clearing the value leaves the included flag in place }
    Entry.SetValue(1, '');
    Assert(Entry.FlagIncluded(2, 'shellexec'));

    { Setting OnLog checks logoutput, which excludes shellexec }
    Entry.Add('OnLog', 'MyOnLog');
    Assert(Entry.FlagIncluded(2, 'logoutput'));
    Assert(not Entry.FlagIncluded(2, 'shellexec'));

    { The [Run] flag-includes rule: checking unchecked also checks postinstall }
    Entry.SetFlag(2, 'unchecked', True);
    Assert(Entry.FlagIncluded(2, 'postinstall'));

    { The rule is section-scoped: UninstallRun has the Verb rule but not OnLog }
    Assert(TryGetScriptSectionMetadata('UninstallRun', Metadata));
    Entry.Free;
    Entry := TScriptParameterEntry.Create(Metadata);
    Entry.Parse(['Filename: a']);
    Entry.Add('Verb', 'open');
    Assert(Entry.FlagIncluded(2, 'shellexec'));
    Entry.Add('OnLog', 'MyOnLog');
    Assert(not Entry.FlagIncluded(2, 'logoutput'));

    { Without metadata there are no rules }
    Entry.Free;
    Entry := TScriptParameterEntry.Create(nil);
    Entry.Parse(['Source: a']);
    Entry.SetFlag(Entry.Add('Flags', ''), 'extractarchive', True);
    Assert(Entry.FlagIncluded(1, 'extractarchive'));
    Assert(not Entry.FlagIncluded(1, 'external'));
  finally
    Entry.Free;
    Counter.Free;
  end;
end;

procedure TestEntryExcludeRules;
begin
  var Metadata: TScriptSectionMetadata;
  Assert(TryGetScriptSectionMetadata('Files', Metadata));

  const Counter = TChangeCounter.Create;
  var Entry := TScriptParameterEntry.Create(Metadata);
  try
    { Checking signonce unchecks sign, in one change notification, with
      unknown tokens preserved }
    Entry.Parse(['Source: a; Flags: foo sign']);
    Entry.OnChange := Counter.HandleChange;
    Counter.Count := 0;
    Entry.SetFlag(1, 'signonce', True);
    Assert(Counter.Count = 1);
    Assert(Entry.FlagIncluded(1, 'signonce'));
    Assert(not Entry.FlagIncluded(1, 'sign'));
    Assert(Entry.FlagIncluded(1, 'foo'));
    var Lines := Entry.GetLines;
    Assert(Lines[0] = 'Source: a; Flags: foo signonce');

    { The rules apply in both directions: sign's rule excludes signcheck when
      sign is checked, and excludes sign when signcheck is checked }
    Entry.Parse(['Source: a; Flags: signcheck']);
    Entry.SetFlag(1, 'sign', True);
    Assert(Entry.FlagIncluded(1, 'sign'));
    Assert(not Entry.FlagIncluded(1, 'signcheck'));
    Entry.Parse(['Source: a; Flags: sign']);
    Entry.SetFlag(1, 'signcheck', True);
    Assert(Entry.FlagIncluded(1, 'signcheck'));
    Assert(not Entry.FlagIncluded(1, 'sign'));

    { Unchecking fires no rules }
    Entry.Parse(['Source: a; Flags: sign signonce']); { Not accepted by the compiler but the model preserves it }
    Entry.SetFlag(1, 'sign', False);
    Assert(Entry.FlagIncluded(1, 'signonce'));

    { One-to-many in both directions: deleteafterinstall excludes sharedfile,
      and checking sharedfile removes deleteafterinstall }
    Entry.Parse(['Source: a; Flags: sharedfile']);
    Entry.SetFlag(1, 'deleteafterinstall', True);
    Assert(Entry.FlagIncluded(1, 'deleteafterinstall'));
    Assert(not Entry.FlagIncluded(1, 'sharedfile'));
    Entry.Parse(['Source: a; Flags: deleteafterinstall']);
    Entry.SetFlag(1, 'sharedfile', True);
    Assert(Entry.FlagIncluded(1, 'sharedfile'));
    Assert(not Entry.FlagIncluded(1, 'deleteafterinstall'));

    { Includes and excludes rules combine: checking download turns on external
      and ignoreversion, and removes comparetimestamp through download's own
      rule and replacesameversion through included ignoreversion's rule, all
      in one change notification }
    Entry.Parse(['Source: a; Flags: comparetimestamp replacesameversion']);
    Counter.Count := 0;
    Entry.SetFlag(1, 'download', True);
    Assert(Counter.Count = 1);
    Lines := Entry.GetLines;
    Assert(Lines[0] = 'Source: a; Flags: download external ignoreversion');

    { [Run]: the three wait flags exclude each other, and shellexec and the
      bitness flags exclude each other }
    Assert(TryGetScriptSectionMetadata('Run', Metadata));
    Entry.Free;
    Entry := TScriptParameterEntry.Create(Metadata);
    Entry.Parse(['Filename: a; Flags: nowait']);
    Entry.SetFlag(1, 'waituntilterminated', True);
    Assert(Entry.FlagIncluded(1, 'waituntilterminated'));
    Assert(not Entry.FlagIncluded(1, 'nowait'));
    Entry.Parse(['Filename: a; Flags: shellexec']);
    Entry.SetFlag(1, '64bit', True);
    Assert(Entry.FlagIncluded(1, '64bit'));
    Assert(not Entry.FlagIncluded(1, 'shellexec'));

    { A parameter-includes-flag rule cascades into the excludes rules: setting
      OnLog checks logoutput which unchecks nowait }
    Entry.Parse(['Filename: a; Flags: nowait']);
    Entry.Add('OnLog', 'MyOnLog');
    Assert(Entry.FlagIncluded(1, 'logoutput'));
    Assert(not Entry.FlagIncluded(1, 'nowait'));

    { The rules are section-scoped: [Dirs] has both flags of the [Files]
      deleteafterinstall rule but not the rule, so both stay checked }
    Assert(TryGetScriptSectionMetadata('Dirs', Metadata));
    Entry.Free;
    Entry := TScriptParameterEntry.Create(Metadata);
    Entry.Parse(['Name: x; Flags: uninsneveruninstall']);
    Entry.SetFlag(1, 'deleteafterinstall', True);
    Assert(Entry.FlagIncluded(1, 'deleteafterinstall'));
    Assert(Entry.FlagIncluded(1, 'uninsneveruninstall'));

    { Without metadata there are no rules }
    Entry.Free;
    Entry := TScriptParameterEntry.Create(nil);
    Entry.Parse(['Source: a; Flags: sign']);
    Entry.SetFlag(1, 'signonce', True);
    Assert(Entry.FlagIncluded(1, 'sign'));
    Assert(Entry.FlagIncluded(1, 'signonce'));
  finally
    Entry.Free;
    Counter.Free;
  end;
end;

procedure TestDirectiveSection;
begin
  const Counter = TChangeCounter.Create;
  const Section = TScriptDirectiveSection.Create(nil);
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
    Assert(Section.Lines[0].Kind = sdlOther);
    Assert(Section.Lines[1].Kind = sdlDirective);
    Assert(Section.Lines[1].Name = 'AppName');
    Assert(Section.Lines[2].Kind = sdlOther);
    Assert(Section.Lines[3].Kind = sdlDirective);
    Assert(Section.Lines[3].Value = 'Bar');
    Assert(Section.Lines[4].Kind = sdlDirective);
    Assert(Section.Lines[5].Kind = sdlOther);
    var Value: String;
    Assert(Section.TryGetValue('appname', Value));
    Assert(Value = 'Bar'); { Last occurrence }
    Assert(not Section.TryGetValue('AppVersion', Value));
    Assert(Section.IndexOf('AppName') = 3);
    var Index := -1;
    Assert(Section.TryResolve('appname', Index) and (Index = 3)); { -1 resolves by name }
    Index := 1;
    Assert(Section.TryResolve('AppName', Index) and (Index = 1)); { A given index is kept }
    Index := 0;
    Assert(not Section.TryResolve('AppName', Index)); { A comment line is not a directive }
    Index := 4;
    Assert(not Section.TryResolve('AppName', Index)); { Name mismatch }
    Index := 6;
    Assert(not Section.TryResolve('AppName', Index)); { Out of range }

    { Untouched sections round-trip byte-identical }
    var Lines := Section.GetLines;
    Assert(Length(Lines) = 6);
    Assert(Lines[0] = '; comment');
    Assert(Lines[3] = 'AppName = Bar ');
    Assert(Lines[5] = '#define X 1');

    { Directive values keep surrounding quotes out of the display value,
      without treating embedded quotes as doubled }
    Section.Parse(['AppName="My ""quoted"" App"']);
    Assert(Section.Lines[0].Value = 'My ""quoted"" App');

    { Editing a directive only rewrites that line, keeping the name and the
      whitespace around the '=' as written }
    Section.Parse(['; c', 'AppName = Foo', 'Other=1']);
    Section.OnChange := Counter.HandleChange;
    Section.SetValue(1, 'Bar');
    Assert(Counter.Count = 1);
    Lines := Section.GetLines;
    Assert(Length(Lines) = 3);
    Assert(Lines[0] = '; c');
    Assert(Lines[1] = 'AppName = Bar');
    Assert(Lines[2] = 'Other=1');

    { A value needing whitespace gets quotes }
    Section.SetValue(1, 'B ');
    Lines := Section.GetLines;
    Assert(Lines[1] = 'AppName = "B "');

    { Adding inserts after the last directive; removing removes only that
      line }
    Assert(Section.Add('AppVersion', '1.0') = 3);
    Lines := Section.GetLines;
    Assert(Length(Lines) = 4);
    Assert(Lines[3] = 'AppVersion=1.0');
    Section.Remove(1);
    Lines := Section.GetLines;
    Assert(Length(Lines) = 3);
    Assert(Lines[0] = '; c');
    Assert(Lines[1] = 'Other=1');
    Assert(Lines[2] = 'AppVersion=1.0');

    {$IFDEF ISTESTTOOLPROJ}
    { Opaque lines cannot be edited or removed; comments must survive }
    var Caught := False;
    try
      Section.Remove(0);
    except
      on EScriptModelError do Caught := True;
    end;
    Assert(Caught);
    Caught := False;
    try
      Section.SetValue(0, 'x');
    except
      on EScriptModelError do Caught := True;
    end;
    Assert(Caught);
    {$ENDIF}

    { With no directives yet, adding appends at the end }
    Section.Parse(['; only comment']);
    Assert(Section.Add('A', '1') = 1);
    Lines := Section.GetLines;
    Assert(Length(Lines) = 2);
    Assert(Lines[1] = 'A=1');

    { Editing a directive keeps its own quoting: a quoted value stays quoted }
    Section.Parse(['AppName="Foo"']);
    Section.SetValue(0, 'Bar');
    Lines := Section.GetLines;
    Assert(Lines[0] = 'AppName="Bar"');

    { A value ending in whitespace + '\' gets quotes so the written line is
      not read back as an ISPP line continuation }
    Section.Parse(['AppName=Foo']);
    Section.SetValue(0, 'Bar \');
    Lines := Section.GetLines;
    Assert(Lines[0] = 'AppName="Bar \"');

    { A value that itself looks quoted gets surrounding quotes so the literal
      quotes survive read-back, both when editing and when adding }
    Section.Parse(['AppName=Foo']);
    Section.SetValue(0, '"Bar"');
    Lines := Section.GetLines;
    Assert(Lines[0] = 'AppName=""Bar""');
    Assert(Section.Lines[0].Value = '"Bar"');
    Section.Parse(['; c']);
    Section.Add('AppName', '"Foo"');
    Lines := Section.GetLines;
    Assert(Lines[1] = 'AppName=""Foo""');
    Assert(Section.Lines[1].Value = '"Foo"');

    { A new directive is left bare by default (QuoteNewValues off for
      directive-style sections) and quoted when the option is turned on }
    Section.Parse(['; c']);
    Assert(not Section.QuoteNewValues);
    Section.Add('AppName', 'Foo');
    Lines := Section.GetLines;
    Assert(Lines[1] = 'AppName=Foo');
    Section.Parse(['; c']);
    Section.QuoteNewValues := True;
    Section.Add('AppName', 'Foo');
    Lines := Section.GetLines;
    Assert(Lines[1] = 'AppName="Foo"');
    Section.QuoteNewValues := False;

    {$IFDEF ISTESTTOOLPROJ}
    { Values with line breaks and names that would not read back as the same
      directive raise, leaving the section untouched }
    Section.Parse(['AppName=Foo']);
    Caught := False;
    try
      Section.SetValue(0, 'a'#13#10'b');
    except
      on EScriptModelError do Caught := True;
    end;
    Assert(Caught);
    Caught := False;
    try
      Section.Add('', 'x');
    except
      on EScriptModelError do Caught := True;
    end;
    Assert(Caught);
    Caught := False;
    try
      Section.Add('A=B', 'x');
    except
      on EScriptModelError do Caught := True;
    end;
    Assert(Caught);
    Caught := False;
    try
      Section.Add('; comment', 'x');
    except
      on EScriptModelError do Caught := True;
    end;
    Assert(Caught);
    Caught := False;
    try
      Section.Add('AppName ', 'x');
    except
      on EScriptModelError do Caught := True;
    end;
    Assert(Caught);
    Caught := False;
    try
      Section.Add('AppName', 'a'#10'b');
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
    Assert(Entry.Count = 3);
    var Value: String;
    Assert(Entry.TryGetValue('DestDir', Value) and (Value = 'b'));
    Assert(Entry.BreakCount = 1);
    Assert(Entry.BreakParameterIndexes[0] = 1);

    { Untouched spanned entries round-trip byte-identical }
    var Lines := Entry.GetLines;
    Assert(Length(Lines) = 2);
    Assert(Lines[0] = 'Source: "a"; \');
    Assert(Lines[1] = '  DestDir: "b"; Flags: x');

    { Editing a middle parameter keeps the author's line structure, and its
      quoting (DestDir was quoted) }
    Entry.SetValue(1, 'c');
    Lines := Entry.GetLines;
    Assert(Length(Lines) = 2);
    Assert(Lines[0] = 'Source: "a"; \');
    Assert(Lines[1] = '  DestDir: "c"; Flags: x');

    { Editing the first parameter also keeps the break, and its quoting }
    Entry.Parse(['Source: "a"; \', '  DestDir: "b"; Flags: x']);
    Entry.SetValue(0, 'z');
    Lines := Entry.GetLines;
    Assert(Length(Lines) = 2);
    Assert(Lines[0] = 'Source: "z"; \');
    Assert(Lines[1] = '  DestDir: "b"; Flags: x');

    { When the parameter at a break point is removed, that break is dropped
      and the remainder goes to the last surviving line }
    Entry.Parse(['Source: "a"; \', '  DestDir: "b"; Flags: x']);
    Entry.Remove(1);
    Lines := Entry.GetLines;
    Assert(Length(Lines) = 1);
    Assert(Lines[0] = 'Source: "a"; Flags: x');

    { Three physical lines, edit in the middle }
    Entry.Parse(['A: 1; \', 'B: 2; \', 'C: 3']);
    Assert(Entry.BreakCount = 2);
    Entry.SetValue(1, '22');
    Lines := Entry.GetLines;
    Assert(Length(Lines) = 3);
    Assert(Lines[0] = 'A: 1; \');
    Assert(Lines[1] = 'B: 22; \');
    Assert(Lines[2] = 'C: 3');

    { Removing a middle parameter drops its own break and shifts the following
      break back onto its now-earlier parameter }
    Entry.Parse(['A: 1; \', 'B: 2; \', 'C: 3']);
    Entry.Remove(1);
    Lines := Entry.GetLines;
    Assert(Length(Lines) = 2);
    Assert(Lines[0] = 'A: 1; \');
    Assert(Lines[1] = 'C: 3');

    { Unusual whitespace around the break reconstructs exactly }
    Entry.Parse(['Source: a ;  \', '   DestDir: b']);
    Entry.SetValue(0, 'z');
    Lines := Entry.GetLines;
    Assert(Length(Lines) = 2);
    Assert(Lines[0] = 'Source: z ;  \');
    Assert(Lines[1] = '   DestDir: b');
  finally
    Entry.Free;
  end;

  { A spanned directive line is joined for parsing and collapses to one
    physical line when edited }
  const Section = TScriptDirectiveSection.Create(nil);
  try
    Section.Parse(['AppName=Foo \', 'Bar']);
    Assert(Section.Count = 1);
    Assert(Section.Lines[0].Kind = sdlDirective);
    Assert(Section.Lines[0].Value = 'Foo Bar');
    var Lines := Section.GetLines;
    Assert(Length(Lines) = 2);
    Assert(Lines[0] = 'AppName=Foo \');
    Assert(Lines[1] = 'Bar');
    Section.SetValue(0, 'X');
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
  TestDirectiveSectionMetadata;
  TestSectionMetadataTables;
  TestMetadataConsistency;
  TestScriptCategories;
  TestEntryRules;
  TestEntryExcludeRules;
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
