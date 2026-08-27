unit IDE.ScriptModel.Test;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Test unit for IDE.ScriptModel and IDE.ScriptModel.Metadata*

  Runs a self-test if DEBUG is defined
}

interface

procedure IDEScriptModelRunTests;

implementation

uses
  {$IFDEF DEBUG} Winapi.Windows, {$ENDIF} System.SysUtils,
  {$IFDEF ISTESTTOOLPROJ} System.Classes, Shared.ScriptFunc, {$ENDIF}
  Shared.SetupSectionDirectives, Shared.LangOptionsSectionDirectives,
  IDE.ScriptModel, IDE.ScriptModel.Metadata, IDE.ScriptModel.Metadata.Extra
  {$IFDEF ISTESTTOOLPROJ}, IDE.ScriptModel.Metadata.Extra.FunctionDefinitions,
  IDE.ScriptModel.Metadata.Extra.WordLists{$ENDIF};

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
    whitespace. The offsets overload reports where each line's content, past
    its leading whitespace, starts within the joined result. }
  var LineStartOffsets: TArray<Integer>;
  Assert(JoinSpannedScriptLines(['  x'], LineStartOffsets) = '  x');
  Assert((Length(LineStartOffsets) = 1) and (LineStartOffsets[0] = 3));
  Assert(JoinSpannedScriptLines(['Source: "a"; \', '  DestDir: "b"']) =
    'Source: "a"; DestDir: "b"');
  Assert(JoinSpannedScriptLines(['A=1 \', ' 2 \', ' 3'], LineStartOffsets) =
    'A=1 2 3');
  Assert((Length(LineStartOffsets) = 3) and (LineStartOffsets[0] = 1) and
    (LineStartOffsets[1] = 5) and (LineStartOffsets[2] = 7));

  { Quoting helpers }
  Assert(UnquoteParameterValue(' "a""b" ') = 'a"b');
  Assert(UnquoteParameterValue('x') = 'x');
  Assert(UnquoteParameterValue('"x') = '"x'); { No closing quote }
  Assert(QuoteParameterValueIfNeeded('x y') = 'x y');
  Assert(QuoteParameterValueIfNeeded('a;b') = '"a;b"');
  Assert(QuoteParameterValueIfNeeded('a"b') = '"a""b"');
  Assert(QuoteParameterValueIfNeeded(' x') = '" x"');
  Assert(QuoteParameterValueIfNeeded('') = '');
  { A value ending in whitespace + '\' is quoted so the written line is not
    read back as an ISPP line continuation; one ending in '\' with no preceding
    whitespace needs no quoting. A value of just '\' is quoted too: the
    separator written before the value ends in whitespace }
  Assert(QuoteParameterValueIfNeeded('a \') = '"a \"');
  Assert(QuoteParameterValueIfNeeded('a\') = 'a\');
  Assert(QuoteParameterValueIfNeeded('\') = '"\"');
  { Forced quoting still doubles embedded quotes }
  Assert(QuoteParameterValueIfNeeded('x y', True) = '"x y"');
  Assert(QuoteParameterValueIfNeeded('a"b', True) = '"a""b"');

  { Key/value line helpers }
  var NameText, RawValue: String;
  Assert(TryParseKeyValueLine('AppName = Foo', NameText, RawValue));
  Assert((NameText = 'AppName ') and (RawValue = ' Foo'));
  Assert(not TryParseKeyValueLine('No key/value here', NameText, RawValue));
  Assert(not TryParseKeyValueLine(' = Foo', NameText, RawValue));
  Assert(UnquoteKeyValueValue(' "My ""quoted"" App" ') = 'My ""quoted"" App');
end;

procedure TestEntryParseAndSerialize;
begin
  const Counter = TChangeCounter.Create;
  const Entry = TScriptModelParameterSectionEntry.Create(nil);
  try
    { Basic parse of known and unknown parameters }
    Entry.Parse(['Source: "My Prog.exe"; DestDir: "{app}"; Flags: ignoreversion']);
    Assert(Entry.Count = 3);
    Assert(Entry.Parameters[0].Name = 'Source');
    Assert(Entry.Parameters[0].Kind = pkParameter);
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
    Assert(Entry.Parameters[2].Kind = pkOther);
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
    Assert(Entry.Parameters[0].Kind = pkOther);
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

    { A same-value set is a no-op: Modified stays False, no OnChange fires and
      the entry round-trips byte-identical, with quoting and whitespace kept.
      Repeating a real edit is a no-op too }
    Entry.Parse(['A: "x"; B :  y  ; C: z']);
    Entry.OnChange := Counter.HandleChange;
    Entry.SetValue(0, 'x');
    Entry.SetValue(1, 'y');
    Entry.SetValue(2, 'z');
    Assert(not Entry.Modified);
    Assert(Counter.Count = 0);
    Lines := Entry.GetLines;
    Assert(Lines[0] = 'A: "x"; B :  y  ; C: z');
    Entry.SetValue(0, 'q');
    Assert(Entry.Modified);
    Assert(Counter.Count = 1);
    Entry.SetValue(0, 'q');
    Assert(Counter.Count = 1);
    Lines := Entry.GetLines;
    Assert(Lines[0] = 'A: "q"; B :  y  ; C: z');
    Entry.OnChange := nil;

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

    { Appending to an entry with a trailing semicolon (an opaque empty chunk,
      see above) inserts the new parameter before that chunk: appending after
      it would serialize ';;' }
    Entry.Parse(['Source: x;']);
    Assert(Entry.Add('Flags', 'touch') = 1);
    Lines := Entry.GetLines;
    Assert(Lines[0] = 'Source: x; Flags: touch;');
    Entry.Parse(['DestName: ; Foo: 1;']);
    Assert(Entry.Add('Bar', '2') = 2);
    Lines := Entry.GetLines;
    Assert(Lines[0] = 'DestName: ; Foo: 1; Bar: 2;');
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
    Assert(Entry.TryResolve('a', Index) and (Index = 1)); { A matching index is kept }
    Index := 0;
    Assert(not Entry.TryResolve('B', Index)); { Name mismatch without an occurrence to rebind to }
    Index := 2;
    Assert(Entry.TryResolve('A', Index) and (Index = 0)); { A stale index rebinds by name }
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
    Counter.Free;
  end;
end;

procedure TestEntryFlags;
begin
  const Entry = TScriptModelParameterSectionEntry.Create(nil);
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
  var Metadata: TScriptModelSectionMetadata;
  Assert(TryGetScriptModelSectionMetadata('files', Metadata)); { Case-insensitive }
  Assert(Metadata.SectionName = 'Files');
  Assert(not TryGetScriptModelSectionMetadata('Code', Metadata));
  Assert(TryGetScriptModelSectionMetadata('Files', Metadata));

  const Entry = TScriptModelParameterSectionEntry.Create(Metadata);
  try
    Entry.Parse(['Source: a; ExternalSize: 1_048_576; Unknown: u']);

    Entry.SetValue(1, '456');
    const Lines = Entry.GetLines;
    { The ExternalSize parameter-includes-flag rule also checks external }
    Assert(Lines[0] = 'Source: a; ExternalSize: 456; Unknown: u; Flags: external');

    var Definition: TMemberDefinition;
    Assert(Entry.TryGetDefinition('flags', Definition));
    Assert(Definition.ValueKind = mvkFlags);
    var FoundFlagName := False;
    for var KnownValue in Definition.KnownValues do
      if KnownValue = 'ignoreversion' then
        FoundFlagName := True;
    Assert(FoundFlagName);
    Assert(Entry.TryGetDefinition('ExternalSize', Definition));
    Assert(Definition.ValueKind = mvkInteger);
    Assert(Entry.TryGetDefinition('MinVersion', Definition));
    Assert(Definition.ValueKind = mvkVersion);
    Assert(Entry.TryGetDefinition('Permissions', Definition));
    Assert(Definition.ValueKind = mvkPermissions);
    Assert(Length(Definition.KnownValues) = 30); { 10 identifiers with 3 access types each }
    Assert(Definition.KnownValues[0] = 'admins-full');
    Assert(Definition.KnownValues[1] = 'admins-modify');
    Assert(Definition.KnownValues[29] = 'users-readexec');
    { [Registry] grants 'read' where [Files] and [Dirs] grant 'readexec' }
    var RegistryMetadata: TScriptModelSectionMetadata;
    Assert(TryGetScriptModelSectionMetadata('Registry', RegistryMetadata));
    Assert(RegistryMetadata.TryGetMember('Permissions', Definition));
    Assert(Definition.ValueKind = mvkPermissions);
    Assert(Length(Definition.KnownValues) = 30);
    Assert(Definition.KnownValues[29] = 'users-read');

    { Unknown parameters remain accessible as raw text }
    Assert(not Entry.TryGetDefinition('Unknown', Definition));
    var Value: String;
    Assert(Entry.TryGetValue('Unknown', Value) and (Value = 'u'));

    { Only text parameters are quoted by default when added: mvkVersion and
      mvkPermissions values are written bare, a mvkString value is quoted }
    Entry.Parse(['Source: a']);
    Assert(Entry.QuoteNewValues);
    Entry.Add('MinVersion', '6.2');
    Entry.Add('DestName', 'x');
    Entry.Add('Permissions', 'users-modify admins-full');
    const AddedLines = Entry.GetLines;
    Assert(AddedLines[0] = 'Source: a; MinVersion: 6.2; DestName: "x"; Permissions: users-modify admins-full');
  finally
    Entry.Free;
  end;
end;

procedure TestKeyValueSectionMetadata;
begin
  { The keys of the [Setup] and [LangOptions] sections are called directives,
    which is why the comments below refer to directives }

  { The [Setup] and [LangOptions] tables are generated from the compiler's
    enums: every directive present, in canonical enum order }
  var Metadata: TScriptModelSectionMetadata;
  Assert(TryGetScriptModelSectionMetadata('Setup', Metadata));
  Assert(Metadata.SectionName = 'Setup');
  Assert(Length(Metadata.Members) = Ord(High(TSetupSectionDirective))+1);
  Assert(Metadata.Members[0].Name = 'AllowCancelDuringInstall');
  var Definition: TMemberDefinition;
  Assert(Metadata.TryGetMember('AppName', Definition));
  Assert(Definition.ValueKind = mvkString);
  Assert(not Definition.Obsolete);
  Assert(Definition.DefaultValue = '');
  { A yes/no key still lists its two values, for the drop-down of the
    inspector's text-row fallback when the value isn't yes/no }
  Assert(Metadata.TryGetMember('SolidCompression', Definition));
  Assert(Definition.ValueKind = mvkYesNo);
  Assert(not Definition.KnownValuesCustomSorted);
  Assert(Length(Definition.KnownValues) = 2);
  Assert(Definition.KnownValues[0] = 'yes');
  Assert(Definition.KnownValues[1] = 'no');
  Assert(Definition.DefaultValue = 'no');
  Assert(Metadata.TryGetMember('AllowNetworkDrive', Definition));
  Assert(Definition.ValueKind = mvkYesNo);
  Assert(Definition.DefaultValue = 'yes');
  { The auto/yes/no and yes/no-or-scripted directives allow other values, so
    they are choices and not yes/no, and each kind has exceptions to its usual
    default }
  Assert(Metadata.TryGetMember('DisableDirPage', Definition));
  Assert(Definition.ValueKind = mvkChoice);
  Assert(Length(Definition.KnownValues) = 3);
  Assert(Definition.KnownValues[0] = 'auto');
  Assert(Definition.KnownValues[1] = 'yes');
  Assert(Definition.KnownValues[2] = 'no');
  Assert(Definition.DefaultValue = 'auto');
  Assert(Metadata.TryGetMember('ShowLanguageDialog', Definition));
  Assert(Definition.DefaultValue = 'yes');
  Assert(Metadata.TryGetMember('Uninstallable', Definition));
  Assert(Definition.ValueKind = mvkChoice);
  Assert(Length(Definition.KnownValues) = 2);
  Assert(Definition.KnownValues[0] = 'yes');
  Assert(Definition.KnownValues[1] = 'no');
  Assert(Definition.DefaultValue = 'yes');
  Assert(Metadata.TryGetMember('ChangesAssociations', Definition));
  Assert(Definition.DefaultValue = 'no');
  Assert(Metadata.TryGetMember('ChangesEnvironment', Definition));
  Assert(Definition.DefaultValue = 'no');
  { The choice directives list their values, including Compression's computed
    list, which is in display order: their sorted order except for the zstd
    levels, which are in increasing order }
  Assert(Metadata.TryGetMember('Compression', Definition));
  Assert(Definition.ValueKind = mvkChoice);
  Assert(Definition.KnownValuesCustomSorted);
  { Which is what ChooseWordList asks for using MemberKnownValuesAreCustomSorted }
  Assert(MemberKnownValuesAreCustomSorted('compression', scSetup));
  Assert(not MemberKnownValuesAreCustomSorted('Flags', scFiles));
  Assert(Length(Definition.KnownValues) = 45); { none + zip and bzip with 9 levels each + lzma and lzma2 with 5 levels each + zstd with its 11 meaningful levels }
  Assert(Definition.KnownValues[0] = 'bzip');
  Assert(Definition.KnownValues[1] = 'bzip/1');
  Assert(Definition.KnownValues[11] = 'lzma/fast');
  Assert(Definition.KnownValues[12] = 'lzma/max'); { Sorted, like InternalCompressLevel shows its levels }
  Assert(Definition.KnownValues[21] = 'lzma2/ultra64');
  Assert(Definition.KnownValues[22] = 'none');
  Assert(Definition.KnownValues[23] = 'zip');
  Assert(Definition.KnownValues[38] = 'zstd/13'); { ASCII order would put it before 'zstd/3' }
  Assert(Definition.KnownValues[44] = 'zstd/22');
  Assert(Definition.DefaultValue = 'lzma2/max');
  Assert(Metadata.TryGetMember('LZMAUseSeparateProcess', Definition));
  Assert(Definition.ValueKind = mvkChoice);
  Assert(Length(Definition.KnownValues) = 3);
  Assert(Definition.KnownValues[0] = 'x86');
  Assert(Definition.DefaultValue = 'no');
  Assert(Metadata.TryGetMember('UninstallLogMode', Definition));
  Assert(Definition.ValueKind = mvkChoice);
  Assert(Length(Definition.KnownValues) = 3);
  Assert(Definition.KnownValues[0] = 'append');
  { The expression directives are not choices: their word lists are editor
    autocomplete data, kept by the styler }
  Assert(Metadata.TryGetMember('ArchitecturesAllowed', Definition));
  Assert(Definition.ValueKind = mvkString);
  Assert(Length(Definition.KnownValues) = 0);
  { The flag-list directives carry their flags like a parameter table's Flags
    entry does, with WizardStyle's styles grouped like the compiler's style
    groups instead of sorted }
  Assert(Metadata.TryGetMember('WizardStyle', Definition));
  Assert(Definition.ValueKind = mvkFlags);
  Assert(Length(Definition.KnownValues) = 14);
  Assert(Definition.KnownValues[0] = 'classic');
  Assert(Definition.KnownValues[13] = 'zircon');
  Assert(Definition.DefaultValue = 'classic');
  Assert(Metadata.TryGetMember('PrivilegesRequiredOverridesAllowed', Definition));
  Assert(Definition.ValueKind = mvkFlags);
  Assert(Length(Definition.KnownValues) = 2);
  Assert(Definition.KnownValues[0] = 'commandline');
  Assert(Definition.DefaultValue = '');
  Assert(Metadata.TryGetMember('DisablePrecompiledFileVerifications', Definition));
  Assert(Definition.ValueKind = mvkFlags);
  Assert(Length(Definition.KnownValues) = 8);
  Assert(Definition.KnownValues[0] = 'setup');
  Assert(Definition.DefaultValue = '');
  { The integer directives, but not the ones with richer forms like
    DiskSliceSize's 'max' and CompressionThreads' 'auto', and the version
    directives like their parameter-table counterparts }
  Assert(Metadata.TryGetMember('ReserveBytes', Definition));
  Assert(Definition.ValueKind = mvkInteger);
  Assert(Definition.DefaultValue = '0');
  Assert(Metadata.TryGetMember('UninstallDisplaySize', Definition));
  Assert(Definition.ValueKind = mvkInteger);
  Assert(Definition.DefaultValue = ''); { Calculated automatically when not set }
  Assert(Metadata.TryGetMember('DiskSliceSize', Definition));
  Assert(Definition.ValueKind = mvkString);
  Assert(Metadata.TryGetMember('CompressionThreads', Definition));
  Assert(Definition.ValueKind = mvkString);
  Assert(Metadata.TryGetMember('MinVersion', Definition));
  Assert(Definition.ValueKind = mvkVersion);
  Assert(Definition.DefaultValue = '6.1sp1');
  { The color directives, whose kind like mvkVersion only prevents quoting }
  Assert(Metadata.TryGetMember('WizardBackColor', Definition));
  Assert(Definition.ValueKind = mvkColor);
  Assert(Metadata.TryGetMember('DefaultGroupName', Definition));
  Assert(Definition.ValueKind = mvkString);
  Assert(Definition.DefaultValue = '(Default)');
  { The compiler-path directives: a source file the compiler reads, a list of
    such files, a directory, and a file the compiler writes, while run-time
    paths stay plain strings }
  Assert(Metadata.TryGetMember('LicenseFile', Definition));
  Assert(Definition.ValueKind = mvkCompilerSourceFile);
  Assert(Metadata.TryGetMember('WizardImageFile', Definition));
  Assert(Definition.ValueKind = mvkCompilerSourceFiles);
  Assert(Metadata.TryGetMember('OutputDir', Definition));
  Assert(Definition.ValueKind = mvkCompilerPath);
  Assert(Metadata.TryGetMember('OutputManifestFile', Definition));
  Assert(Definition.ValueKind = mvkCompilerDestFile);
  Assert(Metadata.TryGetMember('AppReadmeFile', Definition));
  Assert(Definition.ValueKind = mvkString);
  Assert(Metadata.TryGetMember('UninstallStyle', Definition));
  Assert(Definition.Obsolete);
  { Every directive of a yes/no kind has a default: none was left out of the
    generator's default-yes, default-no, and default-auto sets }
  for var Directive := Low(TSetupSectionDirective) to High(TSetupSectionDirective) do begin
    if (Directive in SetupSectionDirectivesYesNo) or
       (Directive in SetupSectionDirectivesAutoYesNo) or
       (Directive in SetupSectionDirectivesYesNoOrScripted) then
      Assert(Metadata.Members[Ord(Directive)].DefaultValue <> '');
  end;

  { The section model exposes the definitions like the entry model does }
  const Section = TScriptModelKeyValueSection.Create(Metadata);
  try
    Assert(Section.Metadata = Metadata);
    Assert(Section.TryGetDefinition('solidcompression', Definition)); { Case-insensitive }
    Assert(Definition.ValueKind = mvkYesNo);
    Assert(not Section.TryGetDefinition('NoSuchKey', Definition));

    { With the quoting option on, only text and compiler-path keys are quoted:
      a yes/no, integer, or color value is written bare }
    Section.QuoteNewValues := True;
    Section.Add('SolidCompression', 'yes');
    Section.Add('AppName', 'My App');
    Section.Add('ReserveBytes', '4096');
    Section.Add('LicenseFile', 'license.txt');
    Section.Add('WizardImageFile', 'image1.bmp,image2.bmp');
    Section.Add('WizardBackColor', '$FF0000');
    const Lines = Section.GetLines;
    Assert(Lines[0] = 'SolidCompression=yes');
    Assert(Lines[1] = 'AppName="My App"');
    Assert(Lines[2] = 'ReserveBytes=4096');
    Assert(Lines[3] = 'LicenseFile="license.txt"');
    Assert(Lines[4] = 'WizardImageFile="image1.bmp,image2.bmp"');
    Assert(Lines[5] = 'WizardBackColor=$FF0000');
  finally
    Section.Free;
  end;
  const SectionWithoutMetadata = TScriptModelKeyValueSection.Create(nil);
  try
    Assert(SectionWithoutMetadata.Metadata = nil);
    Assert(not SectionWithoutMetadata.TryGetDefinition('AppName', Definition));
  finally
    SectionWithoutMetadata.Free;
  end;

  Assert(TryGetScriptModelSectionMetadata('LangOptions', Metadata));
  Assert(Metadata.SectionName = 'LangOptions');
  Assert(Length(Metadata.Members) = Ord(High(TLangOptionsSectionDirective))+1);
  Assert(Metadata.Members[0].Name = 'CopyrightFontName');
  Assert(Metadata.Members[0].Obsolete);
  Assert(Metadata.TryGetMember('RightToLeft', Definition));
  Assert(Definition.ValueKind = mvkYesNo);
  Assert(Length(Definition.KnownValues) = 2); { Like SolidCompression above }
  Assert(Definition.KnownValues[0] = 'yes');
  Assert(Definition.KnownValues[1] = 'no');
  Assert(Definition.DefaultValue = 'no');
  Assert(Metadata.TryGetMember('en.RightToLeft', Definition));
  Assert(Definition.Name = 'RightToLeft');
  Assert(Definition.ValueKind = mvkYesNo);
  Assert(Metadata.TryGetMember('LanguageName', Definition));
  Assert(Definition.ValueKind = mvkString);
  Assert(Definition.DefaultValue = 'English');
  { The integer directives, including LanguageID whose '$'-prefixed hex form
    is still a plain integer }
  Assert(Metadata.TryGetMember('LanguageID', Definition));
  Assert(Definition.ValueKind = mvkInteger);
  Assert(Definition.DefaultValue = '$0409');
  Assert(Metadata.TryGetMember('DialogFontSize', Definition));
  Assert(Definition.ValueKind = mvkInteger);
  Assert(Definition.DefaultValue = '9');
  Assert(Metadata.TryGetMember('en.DialogFontSize', Definition));
  Assert(Definition.Name = 'DialogFontSize');
  Assert(Definition.ValueKind = mvkInteger);
  Assert(not Metadata.TryGetMember('en.NoSuchDirective', Definition));

  { [Messages] names are localized message identifiers and [CustomMessages]
    names are user-defined, so neither has a table }
  Assert(not TryGetScriptModelSectionMetadata('Messages', Metadata));
  Assert(not TryGetScriptModelSectionMetadata('CustomMessages', Metadata));
end;

procedure TestSectionMetadataTables;
begin
  { All parameter sections have a table }
  const SectionNames: TArray<String> = [
    'Components', 'Dirs', 'Files', 'Icons', 'INI', 'InstallDelete',
    'ISSigKeys', 'Languages', 'Registry', 'Run', 'Tasks', 'Types',
    'UninstallDelete', 'UninstallRun'];
  var Metadata: TScriptModelSectionMetadata;
  for var SectionName in SectionNames do begin
    Assert(TryGetScriptModelSectionMetadata(SectionName, Metadata));
    Assert(Metadata.SectionName = SectionName);
    Assert(Length(Metadata.Members) > 0);
  end;
  Assert(not TryGetScriptModelSectionMetadata('Code', Metadata));

  { [Registry] value types differ from [Files]: Root/ValueType/ValueData }
  Assert(TryGetScriptModelSectionMetadata('Registry', Metadata));
  const RegistryEntry = TScriptModelParameterSectionEntry.Create(Metadata);
  try
    RegistryEntry.Parse(['Root: HKA; Subkey: "Software\My Company"; ' +
      'ValueType: string; ValueName: "Path"; ValueData: "{app}"; ' +
      'Flags: uninsdeletekey']);
    var Value: String;
    Assert(RegistryEntry.TryGetValue('Root', Value) and (Value = 'HKA'));
    Assert(RegistryEntry.TryGetValue('ValueData', Value) and (Value = '{app}'));
    Assert(RegistryEntry.FlagIncluded(5, 'uninsdeletekey'));
    var Definition: TMemberDefinition;
    Assert(RegistryEntry.TryGetDefinition('Flags', Definition));
    Assert(Definition.ValueKind = mvkFlags);
    var FoundFlagName := False;
    for var KnownValue in Definition.KnownValues do
      if KnownValue = 'preservestringtype' then
        FoundFlagName := True;
    Assert(FoundFlagName);
    { 'deletevalue' appears in no [Registry] rule: toggling runs no implications }
    RegistryEntry.SetFlag(5, 'deletevalue', True);
    const Lines = RegistryEntry.GetLines;
    Assert(Lines[0] = 'Root: HKA; Subkey: "Software\My Company"; ' +
      'ValueType: string; ValueName: "Path"; ValueData: "{app}"; ' +
      'Flags: uninsdeletekey deletevalue');
  finally
    RegistryEntry.Free;
  end;

  { [Run] flags }
  Assert(TryGetScriptModelSectionMetadata('Run', Metadata));
  const RunEntry = TScriptModelParameterSectionEntry.Create(Metadata);
  try
    RunEntry.Parse(['Filename: "{app}\MyProg.exe"; Flags: nowait postinstall skipifsilent']);
    Assert(RunEntry.FlagIncluded(1, 'postinstall'));
    RunEntry.SetFlag(1, 'postinstall', False);
    const Lines = RunEntry.GetLines;
    Assert(Lines[0] = 'Filename: "{app}\MyProg.exe"; Flags: nowait skipifsilent');
    var Definition: TMemberDefinition;
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
  Assert(TryGetScriptModelSectionMetadata('Components', Metadata));
  var Definition: TMemberDefinition;
  Assert(Metadata.TryGetMember('ExtraDiskSpaceRequired', Definition));
  Assert(Definition.ValueKind = mvkInteger);

  { The compiler source file parameters name a source file the compiler reads,
    MessagesFile a comma-separated list of them. [Files] Source is one too, but
    only if the entry doesn't have the external flag, which the inspector
    checks before browsing }
  Assert(TryGetScriptModelSectionMetadata('Languages', Metadata));
  Assert(Metadata.TryGetMember('LicenseFile', Definition));
  Assert(Definition.ValueKind = mvkCompilerSourceFile);
  Assert(Metadata.TryGetMember('MessagesFile', Definition));
  Assert(Definition.ValueKind = mvkCompilerSourceFiles);
  Assert(TryGetScriptModelSectionMetadata('Files', Metadata));
  Assert(Metadata.TryGetMember('Source', Definition));
  Assert(Definition.ValueKind = mvkCompilerSourceFile);

  { Single-choice parameters carry their known values }
  Assert(TryGetScriptModelSectionMetadata('Registry', Metadata));
  Assert(Metadata.TryGetMember('Root', Definition));
  Assert(Definition.ValueKind = mvkChoice);
  Assert(Length(Definition.KnownValues) > 0);
  Assert(Metadata.TryGetMember('ValueType', Definition));
  Assert(Definition.ValueKind = mvkChoice);
  Assert(TryGetScriptModelSectionMetadata('InstallDelete', Metadata));
  Assert(Metadata.TryGetMember('Type', Definition));
  Assert(Definition.ValueKind = mvkChoice);

  { [Files] DestDir's commonly used values are a UI suggestion list in
    Metadata.Extra, not metadata known values }
  Assert(TryGetScriptModelSectionMetadata('Files', Metadata));
  Assert(Metadata.TryGetMember('DestDir', Definition));
  Assert(Definition.ValueKind = mvkString);
  Assert(Length(Definition.KnownValues) = 0);
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
    var Metadata: TScriptModelSectionMetadata;
    Assert(TryGetScriptModelSectionMetadata(SectionName, Metadata));

    { Every parameter has a unique name, and only flags, choices, and
      permissions carry tokens, which are themselves non-empty and unique
      (and lowercase for flags and permissions, and sorted for flags) }
    for var I := 0 to High(Metadata.Members) do begin
      const Parameter = Metadata.Members[I];
      Assert(Parameter.Name <> '');
      for var J := 0 to I-1 do
        Assert(not SameText(Metadata.Members[J].Name, Parameter.Name));
      if Parameter.ValueKind in [mvkFlags, mvkChoice, mvkPermissions] then begin
        Assert(Length(Parameter.KnownValues) > 0);
        for var K := 0 to High(Parameter.KnownValues) do begin
          const Token = Parameter.KnownValues[K];
          Assert(Token <> '');
          for var L := 0 to K-1 do
            Assert(not SameText(Parameter.KnownValues[L], Token));
          if Parameter.ValueKind in [mvkFlags, mvkPermissions] then
            Assert(Token = LowerCase(Token));
          if Parameter.ValueKind = mvkFlags then begin
            { The inspector gives a flag parameter one child row per flag, in
              table order, so the table decides the order the flags are shown in.
              Choices and permissions are not checked: they fill a dropdown
              which sorts the values itself }
            if K > 0 then
              Assert(CompareText(Parameter.KnownValues[K-1], Token) < 0);
          end;
        end;
      end else
        Assert(Length(Parameter.KnownValues) = 0);

      { A parameter shared with the Common category groups under it, so a
        section cannot be forgotten in Common's section list }
      if InNames(CommonMemberNames, Parameter.Name) then
        Assert(GetScriptCategory(SectionName, Parameter.Name, True, False) = 'Common');
    end;

    { Every flag-includes rule points at a real flag member and names only
      flags that exist, so it cannot silently never fire }
    for var Rule in Metadata.FlagIncludesRules do begin
      var Definition: TMemberDefinition;
      Assert(Metadata.TryGetMember(Rule.MemberName, Definition));
      Assert(Definition.ValueKind = mvkFlags);
      Assert(InNames(Definition.KnownValues, Rule.FlagName));
      Assert(Length(Rule.OtherFlagNames) > 0);
      for var FlagName in Rule.OtherFlagNames do
        Assert(InNames(Definition.KnownValues, FlagName));
    end;

    { Every parameter-includes-flag rule references a real trigger parameter
      and a real flag in a real flag parameter }
    for var Rule in Metadata.ParameterIncludesFlagRules do begin
      var Definition: TMemberDefinition;
      Assert(Metadata.TryGetMember(Rule.ParameterName, Definition));
      var FlagDefinition: TMemberDefinition;
      Assert(Metadata.TryGetMember(Rule.FlagParameterName, FlagDefinition));
      Assert(FlagDefinition.ValueKind = mvkFlags);
      Assert(InNames(FlagDefinition.KnownValues, Rule.FlagName));
    end;

    { Every flag-excludes rule points at a real flag member, names only
      flags that exist, and does not exclude its own flag }
    for var Rule in Metadata.FlagExcludesRules do begin
      var Definition: TMemberDefinition;
      Assert(Metadata.TryGetMember(Rule.MemberName, Definition));
      Assert(Definition.ValueKind = mvkFlags);
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
        if SameText(IncludeRule.MemberName, ExcludeRule.MemberName) and
           InNames(IncludedNames, ExcludeRule.FlagName) then
          for var FlagName in ExcludeRule.OtherFlagNames do
            Assert(not InNames(IncludedNames, FlagName));
      end;
    end;
  end;
end;

{ SectionToSectionName and SectionNameToSection: a round trip over every
  section plus lookups of unknown, empty, and differently cased names.
  GetScriptSectionDefiningParameterValues: every parameter name with a
  defining section, and names without one.
  IsScriptBooleanExpressionParameter: the expression parameters and
  other names }
procedure TestSectionNames;
begin
  for var Section := Low(TInnoSetupSection) to High(TInnoSetupSection) do
    Assert(SectionNameToSection(SectionToSectionName(Section)) = Section);
  Assert(SectionNameToSection('setup') = scSetup);
  Assert(SectionNameToSection('Bogus') = scNone);
  Assert(SectionNameToSection('') = scNone);

  Assert(GetScriptSectionDefiningParameterValues('Components') = scComponents);
  Assert(GetScriptSectionDefiningParameterValues('Languages') = scLanguages);
  Assert(GetScriptSectionDefiningParameterValues('Tasks') = scTasks);
  Assert(GetScriptSectionDefiningParameterValues('TYPES') = scTypes);
  Assert(GetScriptSectionDefiningParameterValues('ISSigAllowedKeys') = scISSigKeys);
  Assert(GetScriptSectionDefiningParameterValues('issigallowedkeys') = scISSigKeys);
  Assert(GetScriptSectionDefiningParameterValues('Name') = scNone);
  Assert(GetScriptSectionDefiningParameterValues('') = scNone);

  Assert(IsScriptBooleanExpressionParameter('Components'));
  Assert(IsScriptBooleanExpressionParameter('Languages'));
  Assert(IsScriptBooleanExpressionParameter('TASKS'));
  Assert(not IsScriptBooleanExpressionParameter('Types'));
  Assert(not IsScriptBooleanExpressionParameter('ISSigAllowedKeys'));
  Assert(not IsScriptBooleanExpressionParameter('Name'));
  Assert(not IsScriptBooleanExpressionParameter(''));
end;

procedure TestScriptCategories;
const
  UnknownAndObsoleteCategoryCount = 2; { Other and Obsolete }
  SpecificCategoryCount = 15; { The fourteen [Setup] ones and Common }
  SharedDefaultCategoryCount = 2; { Install/UninstallDelete have same default category, as do Run/UninstallRun }

  function IsDefaultCategoryName(const AName: String; const ACount: Integer): Boolean;
  { Whether AName is one of the ACount default categories, which follow the
    unknown and obsolete ones }
  begin
    const Names = ScriptCategoryNamesOrdered;
    Result := False;
    for var I := UnknownAndObsoleteCategoryCount to
                 UnknownAndObsoleteCategoryCount+ACount-1 do
      if Names[I] = AName then
        Exit(True);
  end;

begin
  { Per section the categories are shown in this order: Other and Obsolete,
    then its default category, then the categories of its remaining members: the
    [Setup] ones, or Common (the inspector shows the Debug group before these) }
  const Names = ScriptCategoryNamesOrdered;
  const DefaultCategoryCount = Integer(Length(Names)) -
    UnknownAndObsoleteCategoryCount - SpecificCategoryCount;
  Assert(Names[0] = 'Other');
  Assert(Names[1] = 'Obsolete');
  const FirstSpecificCategoryIndex = UnknownAndObsoleteCategoryCount + DefaultCategoryCount;
  Assert(Names[FirstSpecificCategoryIndex] = 'Appearance');
  Assert(Names[FirstSpecificCategoryIndex+1] = 'Application Details');
  Assert(Names[FirstSpecificCategoryIndex+2] = 'Application Directory');
  Assert(Names[FirstSpecificCategoryIndex+3] = 'Applications in Use');
  Assert(Names[FirstSpecificCategoryIndex+4] = 'Compiler Settings');
  Assert(Names[FirstSpecificCategoryIndex+5] = 'Compression');
  Assert(Names[FirstSpecificCategoryIndex+6] = 'Disk Spanning');
  Assert(Names[FirstSpecificCategoryIndex+7] = 'Installation Pages');
  Assert(Names[FirstSpecificCategoryIndex+8] = 'Security');
  Assert(Names[FirstSpecificCategoryIndex+9] = 'Start Menu Folder');
  Assert(Names[FirstSpecificCategoryIndex+10] = 'System Requirements');
  Assert(Names[FirstSpecificCategoryIndex+11] = 'Uninstallation');
  Assert(Names[FirstSpecificCategoryIndex+12] = 'User Information');
  Assert(Names[FirstSpecificCategoryIndex+13] = 'Version Information');
  Assert(Names[FirstSpecificCategoryIndex+14] = 'Common');

  var FirstSetupCategoryIndex, SetupCategoryCount: NativeInt;
  GetScriptSetupCategoryNamesRange(FirstSetupCategoryIndex, SetupCategoryCount);
  Assert(FirstSetupCategoryIndex = FirstSpecificCategoryIndex);
  Assert(SetupCategoryCount = SpecificCategoryCount-1);

  { A duplicated name would give a section two headers of the same name }
  for var I := 0 to High(Names) do begin
    Assert(Names[I] <> '');
    for var J := 0 to I-1 do
      Assert(not SameText(Names[J], Names[I]));
  end;

  { Every section the inspector shows has a default category, so every row of it
    has a category, and that category comes before its specific ones. [Setup] is
    the exception, see below. Taken from the section sets themselves: a section
    added to one of them without a default category raises instead of leaving
    its rows uncategorized }
  var SectionCount := 0;
  for var Section in KeyValueSections + ParameterSections - [scSetup] do begin
    Assert(IsDefaultCategoryName(GetScriptCategory(SectionToSectionName(Section),
      'AMemberWithoutOwnCategory', True, False), DefaultCategoryCount));
    Inc(SectionCount);
  end;
  Assert(SectionCount = DefaultCategoryCount + SharedDefaultCategoryCount);

  { [Setup] needs no default category because every one of its directives has a
    specific category. This also raises for a directive which was forgotten,
    instead of quietly grouping it apart from the others }
  var SetupMetadata: TScriptModelSectionMetadata;
  Assert(TryGetScriptModelSectionMetadata('Setup', SetupMetadata));
  for var Member in SetupMetadata.Members do begin
    Assert(not IsDefaultCategoryName(GetScriptCategory('Setup', Member.Name, True,
      Member.Obsolete), DefaultCategoryCount));
  end;

  { A known member without a category of its own gets its section's default }
  Assert(GetScriptCategory('Files', 'Source', True, False) = 'File');
  Assert(GetScriptCategory('LangOptions', 'DialogFontName', True, False) = 'Language Options');

  { An obsolete member is set apart, in every section }
  Assert(GetScriptCategory('Setup', 'BackColor', True, True) = 'Obsolete');
  Assert(GetScriptCategory('Files', 'CopyMode', True, True) = 'Obsolete');

  { A member which is not in the metadata is set apart too, because it won't work }
  Assert(GetScriptCategory('Setup', 'Check', False, False) = 'Other');
  Assert(GetScriptCategory('Files', 'AppName', False, False) = 'Other');

  { Except in the sections which have no metadata: [CustomMessages] names are the
    script's own, and a [Messages] name is known if it is a message id, with or
    without a language name prefix }
  Assert(GetScriptCategory('CustomMessages', 'MyMessage', False, False) = 'Custom Messages');
  Assert(GetScriptCategory('CustomMessages', 'AppName', False, False) = 'Custom Messages');
  Assert(GetScriptCategory('Messages', 'welcomelabel1', False, False) = 'Messages');
  Assert(GetScriptCategory('Messages', 'nl.WelcomeLabel1', False, False) = 'Messages');
  Assert(GetScriptCategory('Messages', 'MyMessage', False, False) = 'Other');

  { The inspector uses this to know where to add its unknown parameter rows }
  Assert(IsScriptUnknownCategoryName('other'));
  Assert(not IsScriptUnknownCategoryName('Obsolete'));

  { Membership maps a name to its category, case-insensitively. Common applies
    to the parameter sections only, so the unrelated [Setup] directive of the
    same name groups with the other [Setup] directives instead }
  Assert(GetScriptCategory('Files', 'minversion', True, False) = 'Common');
  Assert(GetScriptCategory('Setup', 'minversion', True, False) = 'System Requirements');

  { The [Setup] directive categories apply only in [Setup] }
  Assert(GetScriptCategory('Setup', 'SolidCompression', True, False) = 'Compression');
  Assert(GetScriptCategory('Setup', 'LZMADictionarySize', True, False) = 'Compression');
  Assert(GetScriptCategory('Setup', 'OutputBaseFilename', True, False) = 'Compiler Settings');
  Assert(GetScriptCategory('Setup', 'AppName', True, False) = 'Application Details');
  Assert(GetScriptCategory('Setup', 'wizardstyle', True, False) = 'Appearance');
  Assert(GetScriptCategory('Setup', 'SetupArchitecture', True, False) = 'System Requirements');
  Assert(GetScriptCategory('Setup', 'AppMutex', True, False) = 'Applications in Use');
  Assert(GetScriptCategory('Setup', 'CloseApplications', True, False) = 'Applications in Use');
  Assert(GetScriptCategory('Setup', 'SignTool', True, False) = 'Security');
  Assert(GetScriptCategory('Setup', 'RedirectionGuard', True, False) = 'Security');

  { Directives which go by what they do instead of by what their name suggests }
  Assert(GetScriptCategory('Setup', 'TerminalServicesAware', True, False) = 'Compiler Settings');
  Assert(GetScriptCategory('Setup', 'ASLRCompatible', True, False) = 'Security');
  Assert(GetScriptCategory('Setup', 'Password', True, False) = 'Security');
  Assert(GetScriptCategory('Setup', 'SetupLogging', True, False) = 'Application Details');
  Assert(GetScriptCategory('Setup', 'AppId', True, False) = 'Uninstallation');
  Assert(GetScriptCategory('Setup', 'AppCopyright', True, False) = 'Version Information');
  Assert(GetScriptCategory('Setup', 'FlatComponentsList', True, False) = 'Installation Pages');

  { AppVersion sets the Add/Remove Programs version, but it is also part of
    AppVerName's default, which Setup shows in its window title }
  Assert(GetScriptCategory('Setup', 'AppVersion', True, False) = 'Application Details');

  { The file time stamp directives are resolved at compile time }
  Assert(GetScriptCategory('Setup', 'TouchDate', True, False) = 'Compiler Settings');
  Assert(GetScriptCategory('Setup', 'TimeStampsInUTC', True, False) = 'Compiler Settings');

  { A UsePrevious* directive groups with the setting it remembers, not with the
    other UsePrevious* directives }
  Assert(GetScriptCategory('Setup', 'UsePreviousLanguage', True, False) = 'Installation Pages');
  Assert(GetScriptCategory('Setup', 'UsePreviousPrivileges', True, False) = 'Application Details');

  { A name shared between a [Setup] directive and a parameter of another section
    groups only in [Setup] }
  Assert(GetScriptCategory('Setup', 'ExtraDiskSpaceRequired', True, False) = 'Application Details');
  Assert(GetScriptCategory('Components', 'ExtraDiskSpaceRequired', True, False) = 'Component');
  Assert(GetScriptCategory('Languages', 'LicenseFile', True, False) = 'Language');

{$IFDEF ISTESTTOOLPROJ}
  { A section the inspector doesn't show has no default category }
  var Caught := False;
  try
    GetScriptCategory('Code', 'MyRoutine', True, False);
  except
    Caught := True;
  end;
  Assert(Caught);
{$ENDIF}

  { Obsolete parameters are flagged in the metadata }
  var Metadata: TScriptModelSectionMetadata;
  Assert(TryGetScriptModelSectionMetadata('Files', Metadata));
  var Definition: TMemberDefinition;
  Assert(Metadata.TryGetMember('CopyMode', Definition));
  Assert(Definition.Obsolete);
  Assert(Metadata.TryGetMember('Source', Definition));
  Assert(not Definition.Obsolete);
  Assert(TryGetScriptModelSectionMetadata('UninstallRun', Metadata));
  Assert(Metadata.TryGetMember('StatusMsg', Definition));
  Assert(Definition.Obsolete);
end;

procedure TestScriptBrowseFileTypes;
begin
  { Every member the compiler resolves as a file it reads or writes has a
    browse file type, so the inspector's "..." button can offer a matching
    filter; directory members browse for a folder and need none, and no other
    member has one }
  const SectionNames: TArray<String> = ['Setup', 'LangOptions', 'Components',
    'Dirs', 'Files', 'Icons', 'INI', 'InstallDelete', 'ISSigKeys', 'Languages',
    'Registry', 'Run', 'Tasks', 'Types', 'UninstallDelete', 'UninstallRun'];
  for var SectionName in SectionNames do begin
    var Metadata: TScriptModelSectionMetadata;
    Assert(TryGetScriptModelSectionMetadata(SectionName, Metadata));
    for var Member in Metadata.Members do begin
      var FileType: TScriptBrowseFileType;
      { [Files] Source is the exception: it names a file of any type, so it has
        no filter and the inspector falls back to All Files }
      const AnyFileType = SameText(SectionName, 'Files') and
        SameText(Member.Name, 'Source');
      if (Member.ValueKind in [mvkCompilerSourceFile, mvkCompilerSourceFiles,
         mvkCompilerDestFile]) and not AnyFileType then
        Assert(TryGetScriptBrowseFileType(SectionName, Member.Name, FileType))
      else
        Assert(not TryGetScriptBrowseFileType(SectionName, Member.Name, FileType));
    end;
  end;

  { Membership maps a member to its file type, case-insensitively, and only in
    the sections that list it }
  var FileType: TScriptBrowseFileType;
  Assert(TryGetScriptBrowseFileType('Setup', 'licensefile', FileType) and
    (FileType = bftDocs));
  Assert(TryGetScriptBrowseFileType('Languages', 'LicenseFile', FileType) and
    (FileType = bftDocs));
  Assert(TryGetScriptBrowseFileType('Setup', 'SetupIconFile', FileType) and
    (FileType = bftIco));
  Assert(TryGetScriptBrowseFileType('Setup', 'WizardSmallImageFile', FileType) and
    (FileType = bftImages));
  Assert(TryGetScriptBrowseFileType('Setup', 'WizardStyleFileDynamicDark', FileType) and
    (FileType = bftVclStyle));
  Assert(TryGetScriptBrowseFileType('Setup', 'OutputManifestFile', FileType) and
    (FileType = bftTxt));
  Assert(TryGetScriptBrowseFileType('Languages', 'MessagesFile', FileType) and
    (FileType = bftIsl));
  Assert(TryGetScriptBrowseFileType('ISSigKeys', 'KeyFile', FileType) and
    (FileType = bftKey));
  Assert(not TryGetScriptBrowseFileType('Setup', 'MessagesFile', FileType));
  Assert(not TryGetScriptBrowseFileType('Setup', 'OutputDir', FileType));
end;

procedure TestEntryRules;
begin
  var Metadata: TScriptModelSectionMetadata;
  Assert(TryGetScriptModelSectionMetadata('Files', Metadata));

  const Counter = TChangeCounter.Create;
  var Entry := TScriptModelParameterSectionEntry.Create(Metadata);
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
    { Flags is mvkFlags, so the quoting option does not apply }
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
    Assert(TryGetScriptModelSectionMetadata('Run', Metadata));
    Entry.Free;
    Entry := TScriptModelParameterSectionEntry.Create(Metadata);
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

    { A same-value set is a no-op even for a rule-bearing parameter: re-committing
      an identical Verb value fires no rule, so no shellexec flag is added }
    Entry.Free;
    Entry := TScriptModelParameterSectionEntry.Create(Metadata);
    Entry.Parse(['Filename: a; Verb: open']);
    Entry.OnChange := Counter.HandleChange;
    Counter.Count := 0;
    Entry.SetValue(1, 'open');
    Assert(Counter.Count = 0);
    Assert(not Entry.Modified);
    Assert(Entry.Count = 2); { No Flags parameter was added }

    { The rule is section-scoped: UninstallRun has the Verb rule but not OnLog }
    Assert(TryGetScriptModelSectionMetadata('UninstallRun', Metadata));
    Entry.Free;
    Entry := TScriptModelParameterSectionEntry.Create(Metadata);
    Entry.Parse(['Filename: a']);
    Entry.Add('Verb', 'open');
    Assert(Entry.FlagIncluded(2, 'shellexec'));
    Entry.Add('OnLog', 'MyOnLog');
    Assert(not Entry.FlagIncluded(2, 'logoutput'));

    { Without metadata there are no rules }
    Entry.Free;
    Entry := TScriptModelParameterSectionEntry.Create(nil);
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
  var Metadata: TScriptModelSectionMetadata;
  Assert(TryGetScriptModelSectionMetadata('Files', Metadata));

  const Counter = TChangeCounter.Create;
  var Entry := TScriptModelParameterSectionEntry.Create(Metadata);
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
    Assert(TryGetScriptModelSectionMetadata('Run', Metadata));
    Entry.Free;
    Entry := TScriptModelParameterSectionEntry.Create(Metadata);
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
    Assert(TryGetScriptModelSectionMetadata('Dirs', Metadata));
    Entry.Free;
    Entry := TScriptModelParameterSectionEntry.Create(Metadata);
    Entry.Parse(['Name: x; Flags: uninsneveruninstall']);
    Entry.SetFlag(1, 'deleteafterinstall', True);
    Assert(Entry.FlagIncluded(1, 'deleteafterinstall'));
    Assert(Entry.FlagIncluded(1, 'uninsneveruninstall'));

    { Without metadata there are no rules }
    Entry.Free;
    Entry := TScriptModelParameterSectionEntry.Create(nil);
    Entry.Parse(['Source: a; Flags: sign']);
    Entry.SetFlag(1, 'signonce', True);
    Assert(Entry.FlagIncluded(1, 'sign'));
    Assert(Entry.FlagIncluded(1, 'signonce'));
  finally
    Entry.Free;
    Counter.Free;
  end;
end;

procedure TestKeyValueSection;
begin
  const Counter = TChangeCounter.Create;
  const Section = TScriptModelKeyValueSection.Create(nil);
  try
    { Duplicates are both kept; the value scan returns the last occurrence;
      unknown keys, comments, blank lines and ISPP lines are opaque }
    Section.Parse([
      '; comment',
      'AppName=Foo',
      '',
      'AppName = Bar ',
      'Unknown=1',
      '#define X 1']);
    Assert(Section.Count = 6);
    Assert(Section.Lines[0].Kind = lkOther);
    Assert(Section.Lines[1].Kind = lkKeyValue);
    Assert(Section.Lines[1].Name = 'AppName');
    Assert(Section.Lines[2].Kind = lkOther);
    Assert(Section.Lines[3].Kind = lkKeyValue);
    Assert(Section.Lines[3].Value = 'Bar');
    Assert(Section.Lines[4].Kind = lkKeyValue);
    Assert(Section.Lines[5].Kind = lkOther);
    var Value: String;
    Assert(Section.TryGetValue('appname', Value));
    Assert(Value = 'Bar'); { Last occurrence }
    Assert(not Section.TryGetValue('AppVersion', Value));
    Assert(Section.IndexOf('AppName') = 3);
    var Index := -1;
    Assert(Section.TryResolve('appname', Index) and (Index = 3)); { -1 resolves by name }
    Index := 1;
    Assert(Section.TryResolve('AppName', Index) and (Index = 1)); { A matching index is kept }
    Index := 0;
    Assert(Section.TryResolve('AppName', Index) and (Index = 3)); { A comment line is not a key/value line: rebinds by name }
    Index := 4;
    Assert(Section.TryResolve('AppName', Index) and (Index = 3)); { Name mismatch: rebinds by name }
    Index := 6;
    Assert(Section.TryResolve('AppName', Index) and (Index = 3)); { Out of range: rebinds by name }
    Index := 1;
    Assert(not Section.TryResolve('AppVersion', Index)); { Mismatch without an occurrence to rebind to }

    { Untouched sections round-trip byte-identical }
    var Lines := Section.GetLines;
    Assert(Length(Lines) = 6);
    Assert(Lines[0] = '; comment');
    Assert(Lines[3] = 'AppName = Bar ');
    Assert(Lines[5] = '#define X 1');

    { Every unspanned line contributes one line to GetLines }
    for var I := 0 to Section.Count-1 do
      Assert(Section.GetLineCount(I) = 1);

    { Key/value values keep surrounding quotes out of the display value,
      without treating embedded quotes as doubled }
    Section.Parse(['AppName="My ""quoted"" App"']);
    Assert(Section.Lines[0].Value = 'My ""quoted"" App');

    { A same-value set is a no-op: no OnChange fires and the line stays
      byte-identical }
    Section.Parse(['; c', 'AppName = Foo', 'Other=1']);
    Section.OnChange := Counter.HandleChange;
    Section.SetValue(1, 'Foo');
    Assert(Counter.Count = 0);
    Lines := Section.GetLines;
    Assert(Lines[1] = 'AppName = Foo');

    { Editing a key/value line only rewrites that line, keeping the name and the
      whitespace around the '=' as written; repeating the same value is then
      a no-op too }
    Section.SetValue(1, 'Bar');
    Assert(Counter.Count = 1);
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

    { Adding inserts after the last key; removing removes only that
      line }
    Assert(Section.Add('AppVersion', '1.0') = 3);
    Assert(Section.GetLineCount(3) = 1); { An added line contributes one line }
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

    { With no keys yet, adding appends at the end }
    Section.Parse(['; only comment']);
    Assert(Section.Add('A', '1') = 1);
    Lines := Section.GetLines;
    Assert(Length(Lines) = 2);
    Assert(Lines[1] = 'A=1');

    { Editing a key/value value keeps its own quoting: a quoted value stays quoted }
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

    { A new key/value value is left bare by default (QuoteNewValues off for
      key/value sections) and quoted when the option is turned on }
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
      key raise, leaving the section untouched }
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

procedure TestKeyValueSectionFlags;
begin
  const Counter = TChangeCounter.Create;
  var Section := TScriptModelKeyValueSection.Create(nil);
  try
    { Toggling a flag amid unknown ones only edits that token }
    Section.Parse(['WizardStyle=foo modern bar']);
    Assert(Section.FlagIncluded(0, 'MODERN')); { Case-insensitive }
    Assert(not Section.FlagIncluded(0, 'missing'));
    Section.OnChange := Counter.HandleChange;
    Section.SetFlag(0, 'modern', False);
    Assert(Counter.Count = 1);
    var Lines := Section.GetLines;
    Assert(Lines[0] = 'WizardStyle=foo bar');
    Section.SetFlag(0, 'hidebevels', True);
    Lines := Section.GetLines;
    Assert(Lines[0] = 'WizardStyle=foo bar hidebevels');

    { Including a flag that is already there changes nothing }
    Counter.Count := 0;
    Section.SetFlag(0, 'hidebevels', True);
    Assert(Counter.Count = 0);

    { A quoted value keeps its quotes }
    Section.Parse(['WizardStyle="modern dark"']);
    Section.SetFlag(0, 'dark', False);
    Lines := Section.GetLines;
    Assert(Lines[0] = 'WizardStyle="modern"');

    { Excluding the last flag removes the whole key/value line }
    Section.Parse(['; c', 'WizardStyle=modern', 'Other=1']);
    Section.SetFlag(1, 'modern', False);
    Lines := Section.GetLines;
    Assert(Length(Lines) = 2);
    Assert(Lines[0] = '; c');
    Assert(Lines[1] = 'Other=1');

    { Excluding a flag also removes author-written duplicates of it }
    Section.Parse(['WizardStyle=modern foo modern']);
    Section.SetFlag(0, 'modern', False);
    Lines := Section.GetLines;
    Assert(Lines[0] = 'WizardStyle=foo');

    { Without metadata there are no rules }
    Section.Parse(['WizardStyle=classic']);
    Section.SetFlag(0, 'modern', True);
    Assert(Section.FlagIncluded(0, 'classic'));
    Assert(Section.FlagIncluded(0, 'modern'));

    {$IFDEF ISTESTTOOLPROJ}
    { Flag names that cannot be a single unquoted token raise, leaving the
      section untouched; index access requires a key/value line }
    Section.Parse(['; c', 'WizardStyle=modern']);
    var Caught := False;
    try
      Section.SetFlag(1, 'x y', True);
    except
      on EScriptModelError do Caught := True;
    end;
    Assert(Caught);
    Caught := False;
    try
      Section.SetFlag(0, 'modern', False); { The comment line is not a key/value line }
    except
      on EScriptModelError do Caught := True;
    end;
    Assert(Caught);
    Lines := Section.GetLines;
    Assert((Length(Lines) = 2) and (Lines[1] = 'WizardStyle=modern'));
    {$ENDIF}

    { With the [Setup] metadata, WizardStyle's excludes rules mirror the
      compiler's style groups: a style excludes the other styles of its own
      group, in one change notification, and the other groups are untouched }
    var Metadata: TScriptModelSectionMetadata;
    Assert(TryGetScriptModelSectionMetadata('Setup', Metadata));
    Section.Free;
    Section := TScriptModelKeyValueSection.Create(Metadata);
    Section.Parse(['WizardStyle=classic light excludelightbuttons polar']);
    Section.OnChange := Counter.HandleChange;
    Counter.Count := 0;
    Section.SetFlag(0, 'modern', True);
    Assert(Counter.Count = 1);
    Lines := Section.GetLines;
    Assert(Lines[0] = 'WizardStyle=light excludelightbuttons polar modern');

    { The three-way dark-style group excludes pairwise in both directions }
    Section.Parse(['WizardStyle=light']);
    Section.SetFlag(0, 'dynamic', True);
    Assert(Section.FlagIncluded(0, 'dynamic') and not Section.FlagIncluded(0, 'light'));
    Section.SetFlag(0, 'dark', True);
    Assert(Section.FlagIncluded(0, 'dark') and not Section.FlagIncluded(0, 'dynamic'));
    Section.SetFlag(0, 'light', True);
    Assert(Section.FlagIncluded(0, 'light') and not Section.FlagIncluded(0, 'dark'));

    { The five-way special-style group too }
    Section.Parse(['WizardStyle=windows11']);
    Section.SetFlag(0, 'polar', True);
    Assert(Section.FlagIncluded(0, 'polar') and not Section.FlagIncluded(0, 'windows11'));
    Section.SetFlag(0, 'zircon', True);
    Assert(Section.FlagIncluded(0, 'zircon') and not Section.FlagIncluded(0, 'polar'));
    Section.SetFlag(0, 'slate', True);
    Assert(Section.FlagIncluded(0, 'slate') and not Section.FlagIncluded(0, 'zircon'));
    Section.SetFlag(0, 'stellar', True);
    Assert(Section.FlagIncluded(0, 'stellar') and not Section.FlagIncluded(0, 'slate'));

    { The light-control-styling pair }
    Section.Parse(['WizardStyle=excludelightcontrols']);
    Section.SetFlag(0, 'excludelightbuttons', True);
    Assert(not Section.FlagIncluded(0, 'excludelightcontrols'));
    Section.Parse(['WizardStyle=excludelightbuttons']);
    Section.SetFlag(0, 'excludelightcontrols', True);
    Assert(not Section.FlagIncluded(0, 'excludelightbuttons'));

    { hidebevels and includetitlebar are alone in their groups and exclude
      nothing }
    Section.Parse(['WizardStyle=modern dark polar']);
    Section.SetFlag(0, 'hidebevels', True);
    Section.SetFlag(0, 'includetitlebar', True);
    Lines := Section.GetLines;
    Assert(Lines[0] = 'WizardStyle=modern dark polar hidebevels includetitlebar');

    { The other flag-list keys have no rules }
    Section.Parse(['PrivilegesRequiredOverridesAllowed=commandline']);
    Section.SetFlag(0, 'dialog', True);
    Lines := Section.GetLines;
    Assert(Lines[0] = 'PrivilegesRequiredOverridesAllowed=commandline dialog');
  finally
    Section.Free;
    Counter.Free;
  end;
end;

procedure TestEntrySpanning;
begin
  const Entry = TScriptModelParameterSectionEntry.Create(nil);
  try
    { A spanned entry parses from its physical lines and remembers the break
      at parameter granularity }
    Entry.Parse(['Source: "a"; \', '  DestDir: "b"; Flags: x']);
    Assert(Entry.Count = 3);
    var Value: String;
    Assert(Entry.TryGetValue('DestDir', Value) and (Value = 'b'));
    Assert(Entry.LineSpanCount = 1);
    Assert(Entry.LineSpanParameterIndexes[0] = 1);

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
    Assert(Entry.LineSpanCount = 2);
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

    { Removing the first parameter shifts the following break to parameter
      index 0; a break whose line would carry no parameter is skipped instead
      of serializing a parameter-less continuation-only line }
    Entry.Parse(['A: 1; \', 'B: 2']);
    Entry.Remove(0);
    Lines := Entry.GetLines;
    Assert(Length(Lines) = 1);
    Assert(Lines[0] = 'B: 2');

    { A break inside the first parameter (a value spanned mid-parameter, legal
      because ISPP joins spanned lines before any parameter parsing) maps to
      parameter index 0, round-trips byte-identical while untouched, and is
      skipped the same way once any other parameter is edited }
    Entry.Parse(['Source: foo \', '  bar; DestDir: x']);
    Assert(Entry.LineSpanCount = 1);
    Assert(Entry.LineSpanParameterIndexes[0] = 0);
    Lines := Entry.GetLines;
    Assert(Length(Lines) = 2);
    Assert(Lines[0] = 'Source: foo \');
    Assert(Lines[1] = '  bar; DestDir: x');
    Entry.SetValue(1, 'y');
    Lines := Entry.GetLines;
    Assert(Length(Lines) = 1);
    Assert(Lines[0] = 'Source: foo bar; DestDir: y');

    { A same-value set is a no-op even with a mid-parameter break: the span
      keeps its physical lines }
    Entry.Parse(['Source: foo \', '  bar; DestDir: x']);
    Entry.SetValue(0, 'foo bar');
    Assert(not Entry.Modified);
    Lines := Entry.GetLines;
    Assert(Length(Lines) = 2);
    Assert(Lines[0] = 'Source: foo \');
    Assert(Lines[1] = '  bar; DestDir: x');

    { A break inside a later parameter snaps to the preceding parameter
      boundary on edit }
    Entry.Parse(['A: 1; B: foo \', '  bar; C: 3']);
    Entry.SetValue(2, 'y');
    Lines := Entry.GetLines;
    Assert(Length(Lines) = 2);
    Assert(Lines[0] = 'A: 1; \');
    Assert(Lines[1] = '  B: foo bar; C: y');

    { Unusual whitespace around the break reconstructs exactly }
    Entry.Parse(['Source: a ;  \', '   DestDir: b']);
    Entry.SetValue(0, 'z');
    Lines := Entry.GetLines;
    Assert(Length(Lines) = 2);
    Assert(Lines[0] = 'Source: z ;  \');
    Assert(Lines[1] = '   DestDir: b');

    { A continuation followed only by whitespace parses into a trailing
      whitespace-only chunk with the break mapped past it; appending inserts
      the new parameter before that chunk, keeping the break and the
      whitespace before the backslash }
    Entry.Parse(['Source: x; \', '  ']);
    Assert(Entry.Count = 2);
    Assert(Entry.LineSpanCount = 1);
    Assert(Entry.LineSpanParameterIndexes[0] = 2);
    Entry.QuoteNewValues := False;
    Assert(Entry.Add('Flags', 'touch') = 1);
    Entry.QuoteNewValues := True;
    Lines := Entry.GetLines;
    Assert(Length(Lines) = 2);
    Assert(Lines[0] = 'Source: x; Flags: touch; \');
    Assert(Lines[1] = '  ');

    { Editing such an entry writes the separator once: the trailing
      whitespace-only chunk already owns it, so the continuation adds only the
      backslash }
    Entry.Parse(['Source: x; \', '  ']);
    Entry.SetValue(0, 'y');
    Lines := Entry.GetLines;
    Assert(Length(Lines) = 2);
    Assert(Lines[0] = 'Source: y; \');
    Assert(Lines[1] = '  ');

    { The same when the source had no separator before the backslash: none is
      invented }
    Entry.Parse(['Source: x \', '  ']);
    Entry.SetValue(0, 'y');
    Lines := Entry.GetLines;
    Assert(Length(Lines) = 2);
    Assert(Lines[0] = 'Source: y \');
    Assert(Lines[1] = '  ');

    { Removing the last parameter takes the whitespace before the backslash
      with it: whitespace is restored so the line still spans }
    Entry.Parse(['Source: x; Flags: touch \', '  ']);
    Entry.Remove(1);
    Lines := Entry.GetLines;
    Assert(Length(Lines) = 2);
    Assert(Lines[0] = 'Source: x \');
    Assert(Lines[1] = '  ');

    { A whitespace-only chunk that is not the entry's last keeps its
      separator: such source is compiler-invalid, but editing must not
      silently alter it }
    Entry.Parse(['A: 1; ; \', '  B: 2']);
    Entry.SetValue(0, 'x');
    Lines := Entry.GetLines;
    Assert(Length(Lines) = 2);
    Assert(Lines[0] = 'A: x; ; \');
    Assert(Lines[1] = '  B: 2');
  finally
    Entry.Free;
  end;

  { A spanned key/value line is joined for parsing and collapses to one
    physical line when edited }
  const Section = TScriptModelKeyValueSection.Create(nil);
  try
    Section.Parse(['AppName=Foo \', 'Bar']);
    Assert(Section.Count = 1);
    Assert(Section.Lines[0].Kind = lkKeyValue);
    Assert(Section.Lines[0].Value = 'Foo Bar');
    Assert(Section.GetLineCount(0) = 2); { A spanned line contributes its physical lines }
    var Lines := Section.GetLines;
    Assert(Length(Lines) = 2);
    Assert(Lines[0] = 'AppName=Foo \');
    Assert(Lines[1] = 'Bar');
    { A same-value set is a no-op: the spanned group keeps its physical lines }
    Section.SetValue(0, 'Foo Bar');
    Lines := Section.GetLines;
    Assert(Length(Lines) = 2);
    Section.SetValue(0, 'X');
    Assert(Section.GetLineCount(0) = 1); { Editing collapsed the span }
    Lines := Section.GetLines;
    Assert(Length(Lines) = 1);
    Assert(Lines[0] = 'AppName=X');
  finally
    Section.Free;
  end;
end;

procedure TestEntryParameterIndex;

  procedure Check(const AEntry: TScriptModelParameterSectionEntry;
    const ALineIndex, ACharIndex, AExpectedParameterIndex: Integer);
  begin
    var ParameterIndex: Integer;
    Assert(AEntry.TryGetParameterIndex(ALineIndex, ACharIndex, ParameterIndex));
    Assert(ParameterIndex = AExpectedParameterIndex);
  end;

begin
  const Entry = TScriptModelParameterSectionEntry.Create(nil);
  try
    { Single line with an indent and a quoted ';'. A position right before the
      separating ';' still belongs to the parameter before it, and the quoted
      ';' separates nothing }
    Entry.Parse(['  Source: "a;b"; DestDir: c']);
    Assert(Entry.Count = 2);
    Check(Entry, 0, 0, 0);  { Inside the indent }
    Check(Entry, 0, 2, 0);  { At 'S' }
    Check(Entry, 0, 12, 0); { On the quoted ';' }
    Check(Entry, 0, 15, 0); { Right before the separating ';' }
    Check(Entry, 0, 16, 1); { Right after it }
    Check(Entry, 0, 27, 1); { At the line's end }

    { A spanned entry maps positions on the continuation line, with its own
      indent stripped, into the following parameters }
    Entry.Parse(['Source: "a"; \', '  DestDir: "b"; Flags: x']);
    Assert(Entry.Count = 3);
    Check(Entry, 0, 0, 0);  { At 'S' }
    Check(Entry, 0, 11, 0); { Right before the ';' }
    Check(Entry, 0, 12, 1); { Right after it }
    Check(Entry, 0, 14, 1); { At the line's end, after the stripped '\' }
    Check(Entry, 1, 0, 1);  { Inside the continuation line's indent }
    Check(Entry, 1, 2, 1);  { At 'D' }
    Check(Entry, 1, 14, 1); { Right before the ';' }
    Check(Entry, 1, 15, 2); { Right after it }
    Check(Entry, 1, 24, 2); { At the line's end }

    { Refuses lines outside the parsed lines }
    var ParameterIndex: Integer;
    Assert(not Entry.TryGetParameterIndex(-1, 0, ParameterIndex));
    Assert(not Entry.TryGetParameterIndex(2, 0, ParameterIndex));

    { Refuses once modified: the remembered offsets no longer match }
    Entry.SetValue(0, 'z');
    Assert(not Entry.TryGetParameterIndex(0, 0, ParameterIndex));

    { Refuses an empty entry }
    Entry.Parse(['']);
    Assert(not Entry.TryGetParameterIndex(0, 0, ParameterIndex));
  finally
    Entry.Free;
  end;
end;

procedure TestEntryValuePosition;

  procedure Check(const AEntry: TScriptModelParameterSectionEntry;
    const AParameterIndex, AExpectedStartLineIndex, AExpectedStartCharIndex,
    AExpectedEndLineIndex, AExpectedEndCharIndex: Integer);
  begin
    var Position: TValuePosition;
    Assert(AEntry.TryGetValuePosition(AParameterIndex, Position));
    Assert(Position.StartLineIndex = AExpectedStartLineIndex);
    Assert(Position.StartCharIndex = AExpectedStartCharIndex);
    Assert(Position.EndLineIndex = AExpectedEndLineIndex);
    Assert(Position.EndCharIndex = AExpectedEndCharIndex);
    { The positions must map back to the parameter they came from, so that
      going to a parameter and looking up what is at the caret agree }
    var ParameterIndex: Integer;
    Assert(AEntry.TryGetParameterIndex(Position.StartLineIndex,
      Position.StartCharIndex, ParameterIndex));
    Assert(ParameterIndex = AParameterIndex);
    Assert(AEntry.TryGetParameterIndex(Position.EndLineIndex,
      Position.EndCharIndex, ParameterIndex));
    Assert(ParameterIndex = AParameterIndex);
  end;

begin
  const Entry = TScriptModelParameterSectionEntry.Create(nil);
  try
    { Single line with an indent: the range is that of the value, so from past
      the ':' and the whitespace after it to past the value's end, and quotes
      count as value }
    Entry.Parse(['  Source: "a;b"; DestDir: c']);
    Assert(Entry.Count = 2);
    Check(Entry, 0, 0, 10, 0, 15);
    Check(Entry, 1, 0, 26, 0, 27);

    { A spanned entry: a parameter whose whitespace started on the previous
      line still reports the range of its value on the continuation line }
    Entry.Parse(['Source: "a"; \', '  DestDir: "b"; Flags: x']);
    Assert(Entry.Count = 3);
    Check(Entry, 0, 0, 8, 0, 11);
    Check(Entry, 1, 1, 11, 1, 14);
    Check(Entry, 2, 1, 23, 1, 24);

    { A continuation line holding nothing but its indent and the backslash
      doesn't claim the parameter which follows it }
    Entry.Parse(['Source: x; \', '  \', '  DestDir: y']);
    Assert(Entry.Count = 2);
    Check(Entry, 0, 0, 8, 0, 9);
    Check(Entry, 1, 2, 11, 2, 12);

    { A value spanning onto a continuation line ends there }
    Entry.Parse(['Source: x \', '  y; DestDir: z']);
    Assert(Entry.Count = 2);
    Check(Entry, 0, 0, 8, 1, 3);
    Check(Entry, 1, 1, 14, 1, 15);

    { The end excludes whitespace before the ';', and an empty value's end is
      its start }
    Entry.Parse(['Source: x ; DestDir:']);
    Assert(Entry.Count = 2);
    Check(Entry, 0, 0, 8, 0, 9);
    Check(Entry, 1, 0, 20, 0, 20);

    { Refuses a chunk which isn't a named parameter, so has no value }
    Entry.Parse(['Source: x;']);
    Assert(Entry.Count = 2);
    Check(Entry, 0, 0, 8, 0, 9);
    var Position: TValuePosition;
    Assert(not Entry.TryGetValuePosition(1, Position));

    { Refuses parameters outside the parsed parameters }
    Assert(not Entry.TryGetValuePosition(-1, Position));
    Assert(not Entry.TryGetValuePosition(2, Position));

    { Refuses once modified: the remembered offsets no longer match }
    Entry.SetValue(0, 'z');
    Assert(not Entry.TryGetValuePosition(0, Position));

    { Refuses an empty entry }
    Entry.Parse(['']);
    Assert(not Entry.TryGetValuePosition(0, Position));
  finally
    Entry.Free;
  end;
end;

procedure TestKeyValueSectionValuePosition;

  procedure Check(const ASection: TScriptModelKeyValueSection;
    const AIndex, AExpectedStartLineIndex, AExpectedStartCharIndex,
    AExpectedEndLineIndex, AExpectedEndCharIndex: Integer);
  begin
    var Position: TValuePosition;
    Assert(ASection.TryGetValuePosition(AIndex, Position));
    Assert(Position.StartLineIndex = AExpectedStartLineIndex);
    Assert(Position.StartCharIndex = AExpectedStartCharIndex);
    Assert(Position.EndLineIndex = AExpectedEndLineIndex);
    Assert(Position.EndCharIndex = AExpectedEndCharIndex);
  end;

begin
  const Section = TScriptModelKeyValueSection.Create(nil);
  try
    { Single lines: the range is that of the value, so from past the '=' and
      the whitespace after it to past the value's end, quotes count as value,
      an empty value's range is empty at the line's end, and trailing
      whitespace is excluded }
    Section.Parse(['AppName=foo', '  AppVersion = "1.0"', 'AppId=',
      'AppCopyright=bar  ']);
    Assert(Section.Count = 4);
    Check(Section, 0, 0, 8, 0, 11);
    Check(Section, 1, 0, 15, 0, 20);
    Check(Section, 2, 0, 6, 0, 6);
    Check(Section, 3, 0, 13, 0, 16);

    { A spanned line reports the range on the continuation line holding the
      value, past that line's indent }
    Section.Parse(['AppName= \', '  foo', 'AppVersion=1.0']);
    Assert(Section.Count = 2);
    Check(Section, 0, 1, 2, 1, 5);
    Check(Section, 1, 0, 11, 0, 14);

    { A spanned line whose value starts on the first line and ends on the
      continuation line }
    Section.Parse(['AppName=My \', '  Program']);
    Assert(Section.Count = 1);
    Check(Section, 0, 0, 8, 1, 9);

    { A spanned line with an indent: the joining trimmed it, but the range is
      reported with it }
    Section.Parse(['  AppName=foo \', 'bar']);
    Assert(Section.Count = 1);
    Check(Section, 0, 0, 10, 1, 3);

    { Refuses a line which isn't a key/value, so has no value }
    Section.Parse(['; comment', 'AppName=foo']);
    Assert(Section.Count = 2);
    var Position: TValuePosition;
    Assert(not Section.TryGetValuePosition(0, Position));
    Check(Section, 1, 0, 8, 0, 11);

    { Refuses lines outside the parsed lines }
    Assert(not Section.TryGetValuePosition(-1, Position));
    Assert(not Section.TryGetValuePosition(2, Position));

    { Refuses a line once modified: the remembered lines no longer match }
    Section.SetValue(1, 'bar');
    Assert(not Section.TryGetValuePosition(1, Position));
  finally
    Section.Free;
  end;
end;

{$IFDEF ISTESTTOOLPROJ}
procedure TestFunctionDefinitions;
begin
  { TFunctionDefinition.Create and CreateISPP parse the header kind and the
    parameter presence out of a prototype }
  var Definition := TFunctionDefinition.Create(
    'function MsgBox(const Text: String; const Typ: TMsgBoxType; const Buttons: Integer): Integer;');
  Assert(Definition.HeaderKind = hkFunction);
  Assert(Definition.HasParameters);
  Assert(Definition.ScriptFuncWithoutHeader =
    'MsgBox(const Text: String; const Typ: TMsgBoxType; const Buttons: Integer): Integer;');
  Definition := TFunctionDefinition.Create('procedure InitializeWizard;');
  Assert(Definition.HeaderKind = hkProcedure);
  Assert(not Definition.HasParameters);
  Definition := TFunctionDefinition.Create('constructor Create(AOwner: TComponent);');
  Assert(Definition.HeaderKind = hkConstructor);
  Assert(Definition.HasParameters);
  Definition := TFunctionDefinition.CreateISPP('str GetEnv(str Name)');
  Assert(Definition.HeaderKind = hkISPPStr);
  Assert(Definition.HasParameters);
  Assert(Definition.ScriptFuncWithoutHeader = 'GetEnv(str Name)');
  Definition := TFunctionDefinition.CreateISPP('int FindCode');
  Assert(Definition.HeaderKind = hkISPPInt);
  Assert(not Definition.HasParameters);
  Definition := TFunctionDefinition.CreateISPP('void EmitLanguagesSection');
  Assert(Definition.HeaderKind = hkISPPVoid);
  Assert(not Definition.HasParameters);

  { CreateUserDefined takes the kind and the parameter presence from the
    caller, and separates the header on any whitespace, not just the single
    space a cleaned prototype has }
  Definition := TFunctionDefinition.CreateUserDefined('procedure'#9'Foo(const A: Integer);', hkProcedure, True);
  Assert(Definition.HeaderKind = hkProcedure);
  Assert(Definition.ScriptFuncWithoutHeader = 'Foo(const A: Integer);');
  Assert(Definition.HasParameters);
  Definition := TFunctionDefinition.CreateUserDefined('function  Bar: Boolean;', hkFunction, False);
  Assert(Definition.HeaderKind = hkFunction);
  Assert(Definition.ScriptFuncWithoutHeader = 'Bar: Boolean;');
  Assert(not Definition.HasParameters);

  { The caller's parameter presence wins over the prototype text, so an empty
    parameter list still counts as parameterless and the single-definition
    rule hides its call tip }
  Definition := TFunctionDefinition.CreateUserDefined('procedure Foo();', hkProcedure, False);
  Assert(not Definition.HasParameters);

  { Only a procedure or a function can be user-defined }
  var CaughtInvalidHeaderKind := False;
  try
    Definition := TFunctionDefinition.CreateUserDefined('constructor Create;', hkConstructor, False);
  except
    CaughtInvalidHeaderKind := True;
  end;
  Assert(CaughtInvalidHeaderKind);

  { GetISPPFunctionDefinition: a known function, case-insensitively, and an
    unknown name }
  var Count: Integer;
  Definition := GetISPPFunctionDefinition('getenv', 0, Count);
  Assert(Count = 1);
  Assert(Definition.HeaderKind = hkISPPStr);
  Assert(Pos(AnsiString('GetEnv('), Definition.ScriptFuncWithoutHeader) = 1);
  GetISPPFunctionDefinition('NoSuchFunction', 0, Count);
  Assert(Count = 0);

  { GetScriptFunctionDefinition: a single-prototype function with no
    user-defined definitions, with both overloads, and an unknown name }
  Definition := GetScriptFunctionDefinition(False, 'MsgBox', 0, [], Count);
  Assert(Count = 1);
  Assert(Definition.HeaderKind = hkFunction);
  Assert(Pos(AnsiString('MsgBox('), Definition.ScriptFuncWithoutHeader) = 1);
  const DefinitionFromOverload = GetScriptFunctionDefinition(False, 'MsgBox', 0, []);
  Assert(DefinitionFromOverload.ScriptFuncWithoutHeader = Definition.ScriptFuncWithoutHeader);
  GetScriptFunctionDefinition(False, 'NoSuchFunction', 0, [], Count);
  Assert(Count = 0);

  { A class-member lookup with multiple prototypes: an out-of-range index
    clamps to the last prototype }
  const FirstDefinition = GetScriptFunctionDefinition(True, 'Add', 0, [], Count);
  Assert(Count > 1);
  const LastDefinition = GetScriptFunctionDefinition(True, 'Add', Count-1, [], Count);
  Assert(LastDefinition.ScriptFuncWithoutHeader <> FirstDefinition.ScriptFuncWithoutHeader);
  const ClampedDefinition = GetScriptFunctionDefinition(True, 'Add', MaxInt, [], Count);
  Assert(ClampedDefinition.ScriptFuncWithoutHeader = LastDefinition.ScriptFuncWithoutHeader);
  Definition := GetScriptFunctionDefinition(True, 'Create', 0, [], Count);
  Assert(Count > 1);
  Assert(Definition.HeaderKind = hkConstructor);

  { Non-empty user-defined definitions: those matching the name come before
    the built-in definitions, and the count covers the matching user-defined
    definitions plus the built-in count only }
  var UserDefined: TFunctionDefinitionsWithName;
  SetLength(UserDefined, 2);
  UserDefined[0].Name := 'MyRoutine';
  UserDefined[0].Definition := TFunctionDefinition.Create('function MyRoutine(const A: Integer): Boolean;');
  UserDefined[1].Name := 'MsgBox';
  UserDefined[1].Definition := TFunctionDefinition.Create('procedure MsgBox(const Text: String);');

  { A user-defined-only name, matched case-insensitively }
  Definition := GetScriptFunctionDefinition(False, 'myroutine', 0, UserDefined, Count);
  Assert(Count = 1);
  Assert(Definition.ScriptFuncWithoutHeader = 'MyRoutine(const A: Integer): Boolean;');

  { A name both user-defined and built-in: the user-defined one first, then
    the built-in, with an out-of-range index clamping to the last }
  Definition := GetScriptFunctionDefinition(False, 'MsgBox', 0, UserDefined, Count);
  Assert(Count = 2);
  Assert(Definition.HeaderKind = hkProcedure);
  Definition := GetScriptFunctionDefinition(False, 'MsgBox', 1, UserDefined, Count);
  Assert(Definition.HeaderKind = hkFunction);
  Assert(Pos(AnsiString('MsgBox('), Definition.ScriptFuncWithoutHeader) = 1);
  Definition := GetScriptFunctionDefinition(False, 'MsgBox', MaxInt, UserDefined, Count);
  Assert(Definition.HeaderKind = hkFunction);

  { The overload without a count, with user-defined definitions }
  const DefinitionFromUserDefinedOverload = GetScriptFunctionDefinition(False, 'MsgBox', 0, UserDefined);
  Assert(DefinitionFromUserDefinedOverload.HeaderKind = hkProcedure);

  { A name matching neither a user-defined definition nor a built-in }
  GetScriptFunctionDefinition(False, 'NoSuchFunction', 0, UserDefined, Count);
  Assert(Count = 0);

  { User-defined definitions whose names don't match are left out, so a lookup
    which does have built-in definitions keeps its own count }
  var BuiltInCount: Integer;
  GetScriptFunctionDefinition(False, 'ExpandConstant', 0, [], BuiltInCount);
  Assert(BuiltInCount > 0);
  GetScriptFunctionDefinition(False, 'ExpandConstant', 0, UserDefined, Count);
  Assert(Count = BuiltInCount);
end;

procedure TestWordLists;

  function ListHasEntry(const List, Word: AnsiString; const Typ: Integer): Boolean;
  begin
    const Entry = AutoCompleteWordListSeparator + Word +
      AutoCompleteWordListTypeSeparator + AnsiString(IntToStr(Typ)) +
      AutoCompleteWordListSeparator;
    Result := Pos(Entry, AutoCompleteWordListSeparator + List +
      AutoCompleteWordListSeparator) <> 0;
  end;

  function HasName(const Names: TArray<AnsiString>; const Name: String): Boolean;
  begin
    Result := False;
    for var KnownName in Names do
      if SameText(String(KnownName), Name) then
        Exit(True);
  end;

begin
  { BuildAutoCompleteWordList appends the type separator and the type to each
    word and joins the words with the list separator, in case-insensitive
    ASCII sort order: 'abc' after 'ab' because the type separator sorts
    before any word character, 'Def' after 'abc' because the sort upcases
    'a'..'z', and '_x' last because '_' sorts after the uppercased letters }
  const TypeSeparator: AnsiString = AutoCompleteWordListTypeSeparator;
  const ListSeparator: AnsiString = AutoCompleteWordListSeparator;
  const MemberValueType = AnsiString(IntToStr(awtMemberValue));
  const ScriptFunctionType = AnsiString(IntToStr(awtScriptFunction));
  Assert(BuildAutoCompleteWordList(['abc', '_x', 'ab', 'Def'], awtMemberValue) =
    'ab' + TypeSeparator + MemberValueType + ListSeparator +
    'abc' + TypeSeparator + MemberValueType + ListSeparator +
    'Def' + TypeSeparator + MemberValueType + ListSeparator +
    '_x' + TypeSeparator + MemberValueType);
  Assert(BuildAutoCompleteWordList([], awtMemberValue) = '');

  { With Sort=False the words keep their given order, for lists shown with
    sacoCustom }
  Assert(BuildAutoCompleteWordList(['abc', 'ab'], awtMemberValue, False) =
    'abc' + TypeSeparator + MemberValueType + ListSeparator +
    'ab' + TypeSeparator + MemberValueType);

  { AddAutoCompleteWordToList and the string list overload build a list of
    words which do not all have the same type }
  const WordsList = TStringList.Create;
  try
    AddAutoCompleteWordToList(WordsList, 'abc', awtScriptFunction);
    AddAutoCompleteWordToList(WordsList, 'ab', awtMemberValue);
    Assert(BuildAutoCompleteWordList(WordsList, False) =
      'abc' + TypeSeparator + ScriptFunctionType + ListSeparator +
      'ab' + TypeSeparator + MemberValueType);
    Assert(BuildAutoCompleteWordList(WordsList) =
      'ab' + TypeSeparator + MemberValueType + ListSeparator +
      'abc' + TypeSeparator + ScriptFunctionType);
  finally
    WordsList.Free;
  end;

  { MergeAutoCompleteWordLists merges two sorted word lists into one sorted
    list, case-insensitively deduped on the full entry: on a collision the
    base list's entry survives and the extra is dropped }
  const BaseList = BuildAutoCompleteWordList(['Beta', 'Echo'], awtMemberValue);
  Assert(MergeAutoCompleteWordLists(BaseList,
    BuildAutoCompleteWordList(['Alpha', 'Charlie', 'Zulu'], awtMemberValue)) =
    'Alpha' + TypeSeparator + MemberValueType + ListSeparator +
    'Beta' + TypeSeparator + MemberValueType + ListSeparator +
    'Charlie' + TypeSeparator + MemberValueType + ListSeparator +
    'Echo' + TypeSeparator + MemberValueType + ListSeparator +
    'Zulu' + TypeSeparator + MemberValueType);
  Assert(MergeAutoCompleteWordLists(BaseList,
    BuildAutoCompleteWordList(['beta', 'beta'], awtMemberValue)) = BaseList);

  { Duplicates among the extras collapse as well }
  Assert(MergeAutoCompleteWordLists(BaseList,
    BuildAutoCompleteWordList(['Alpha', 'Alpha'], awtMemberValue)) =
    'Alpha' + TypeSeparator + MemberValueType + ListSeparator + BaseList);

  { The same word under a different type digit is a different entry, and '1'
    sorting before '3' puts the awtScriptFunction entry first }
  Assert(MergeAutoCompleteWordLists(BaseList,
    BuildAutoCompleteWordList(['Beta'], awtScriptFunction)) =
    'Beta' + TypeSeparator + ScriptFunctionType + ListSeparator + BaseList);

  { An empty base or empty extras returns the other list unchanged }
  Assert(MergeAutoCompleteWordLists('', BaseList) = BaseList);
  Assert(MergeAutoCompleteWordLists(BaseList, '') = BaseList);
  Assert(MergeAutoCompleteWordLists('', '') = '');

  { The sections word list, without the sections SectionMap excludes }
  Assert(ListHasEntry(SectionsAutoCompleteWordList, '[Files]', awtSectionName));
  Assert(ListHasEntry(SectionsAutoCompleteWordList, '[Code]', awtSectionName));
  Assert(not ListHasEntry(SectionsAutoCompleteWordList, '[CodeBlock]', awtSectionName));

  { The ISPP directives and pragma sub-directives word lists }
  Assert(ListHasEntry(ISPPDirectivesAutoCompleteWordList, '#define', awtPreprocessorDirective));
  Assert(ListHasEntry(ISPPDirectivesAutoCompleteWordList, '#pragma', awtPreprocessorDirective));
  Assert(ListHasEntry(ISPPPragmaAutoCompleteWordList, 'verboselevel', awtPreprocessorSubDirective));

  { The constants word list, including the '{#' entries built because
    ISPPInstalled was True }
  Assert(ListHasEntry(ConstantsAutoCompleteWordList, '{app}', awtConstant));
  Assert(ListHasEntry(ConstantsAutoCompleteWordList, '{{', awtConstant));
  Assert(ListHasEntry(ConstantsAutoCompleteWordList, '{cm', awtConstant));
  Assert(ListHasEntry(ConstantsAutoCompleteWordList, '{#', awtConstant));
  Assert(ListHasEntry(ConstantsAutoCompleteWordList, '{#SourcePath}', awtConstant));

  { The event functions word lists, split per header kind }
  Assert(ListHasEntry(GetEventFunctionsAutoCompleteWordList(False), 'InitializeSetup: Boolean;', awtScriptEvent));
  Assert(not ListHasEntry(GetEventFunctionsAutoCompleteWordList(False), 'InitializeWizard;', awtScriptEvent));
  Assert(ListHasEntry(GetEventFunctionsAutoCompleteWordList(True), 'InitializeWizard;', awtScriptEvent));
  Assert(not ListHasEntry(GetEventFunctionsAutoCompleteWordList(True), 'InitializeSetup: Boolean;', awtScriptEvent));

  { The member names word lists, with the [UninstallRun] StatusMsg exclusion }
  Assert(ListHasEntry(MemberNamesAutoCompleteWordList[scFiles], 'Source', awtParameterName));
  Assert(ListHasEntry(MemberNamesAutoCompleteWordList[scSetup], 'AppName', awtKeyName));
  Assert(ListHasEntry(MemberNamesAutoCompleteWordList[scLangOptions], 'RightToLeft', awtKeyName));
  Assert(ListHasEntry(MemberNamesAutoCompleteWordList[scMessages], 'WelcomeLabel1', awtKeyName));
  Assert(ListHasEntry(MemberNamesAutoCompleteWordList[scRun], 'StatusMsg', awtParameterName));
  Assert(not ListHasEntry(MemberNamesAutoCompleteWordList[scUninstallRun], 'StatusMsg', awtParameterName));

  { The member values word lists, keyed case-insensitively on section and
    member, with the [Setup] expression directive values from the extra
    metadata }
  Assert(ListHasEntry(GetMemberValuesAutoCompleteWordList(scFiles, 'Flags'), 'ignoreversion', awtMemberValue));
  Assert(ListHasEntry(GetMemberValuesAutoCompleteWordList(scSetup, 'wizardstyle'), 'modern', awtMemberValue));
  Assert(ListHasEntry(GetMemberValuesAutoCompleteWordList(scSetup, 'ArchitecturesAllowed'), 'x64compatible', awtMemberValue));
  Assert(GetMemberValuesAutoCompleteWordList(scFiles, 'Source') = '');

  { Compression's word list keeps the display order of its known values, so
    'zstd/13' follows 'zstd/8' }
  Assert(Pos('zstd/8' + TypeSeparator + MemberValueType + ListSeparator +
    'zstd/13' + TypeSeparator + MemberValueType,
    GetMemberValuesAutoCompleteWordList(scSetup, 'Compression')) <> 0);

  { The ISPP expression word list, with the function names from the
    dictionaries }
  Assert(ListHasEntry(ISPPExpressionAutoCompleteWordList, 'GetEnv', awtISPPFunction));
  Assert(ListHasEntry(ISPPExpressionAutoCompleteWordList, '__LINE__', awtISPPVariable));
  Assert(ListHasEntry(ISPPExpressionAutoCompleteWordList, 'PREPROCVER', awtISPPConstant));

  { The script word lists, split between plain words and class or record
    members, with the function names from the dictionaries }
  Assert(ListHasEntry(GetScriptAutoCompleteWordList(False), 'MsgBox', awtScriptFunction));
  Assert(ListHasEntry(GetScriptAutoCompleteWordList(False), 'begin', awtScriptKeyword));
  Assert(ListHasEntry(GetScriptAutoCompleteWordList(False), 'WizardForm', awtScriptVariable));
  Assert(ListHasEntry(GetScriptAutoCompleteWordList(False), 'MaxInt', awtScriptConstant));
  Assert(ListHasEntry(GetScriptAutoCompleteWordList(False), 'clBlack', awtScriptConstant));
  Assert(ListHasEntry(GetScriptAutoCompleteWordList(False), 'TStringList', awtScriptType));
  Assert(ListHasEntry(GetScriptAutoCompleteWordList(False), 'IUnknown', awtScriptInterface));
  Assert(ListHasEntry(GetScriptAutoCompleteWordList(False), 'stAll', awtScriptEnumValue));
  Assert(ListHasEntry(GetScriptAutoCompleteWordList(False), 'mbInformation', awtScriptEnumValue));
  Assert(ListHasEntry(GetScriptAutoCompleteWordList(True), 'Add', awtScriptFunction));
  Assert(ListHasEntry(GetScriptAutoCompleteWordList(True), 'Caption', awtScriptProperty));
  Assert(not ListHasEntry(GetScriptAutoCompleteWordList(True), 'MsgBox', awtScriptFunction));

  { FlagsWords: the known values of each section's Flags parameter, looked up
    case-insensitively }
  Assert(FlagsWords[scFiles].IndexOf('ignoreversion') >= 0);
  Assert(FlagsWords[scFiles].IndexOf('IgnoreVersion') >= 0);
  Assert(FlagsWords[scFiles].IndexOf('nosuchflag') < 0);
  Assert(FlagsWords[scCode].Count = 0);

  { NoHighlightAtCursorWords: the member names of each section plus the
    [Code] reserved words }
  Assert(NoHighlightAtCursorWords[scFiles].IndexOf('Source') >= 0);
  Assert(NoHighlightAtCursorWords[scSetup].IndexOf('AppName') >= 0);
  Assert(NoHighlightAtCursorWords[scCode].IndexOf('begin') >= 0);
  Assert(NoHighlightAtCursorWords[scCode].IndexOf('Source') < 0);

  { ParameterNames, with the [UninstallRun] StatusMsg exclusion }
  Assert(HasName(ParameterNames[scFiles], 'Source'));
  Assert(HasName(ParameterNames[scRun], 'StatusMsg'));
  Assert(not HasName(ParameterNames[scUninstallRun], 'StatusMsg'));
  Assert(HasName(ParameterNames[scUninstallRun], 'RunOnceId'));
  Assert(Length(ParameterNames[scSetup]) = 0); { Not a parameter section }
end;
{$ENDIF}

procedure TestPrepareCodeSectionText;
begin
  { Joins with CRLF and UTF-8 encodes, keeping the line count }
  Assert(PrepareCodeSectionText([]) = '');
  Assert(UTF8ToString(PrepareCodeSectionText(['a'])) = 'a');
  Assert(UTF8ToString(PrepareCodeSectionText(['a', 'b', ''])) = 'a'#13#10'b'#13#10);

  { ISPP directive lines are blanked: an unblanked '#' would make the
    tokenizer abort the whole scan }
  Assert(UTF8ToString(PrepareCodeSectionText(['#define X 1', 'var A: Integer;'])) =
    #13#10'var A: Integer;');
  Assert(UTF8ToString(PrepareCodeSectionText([' #include "x.iss"'])) = '');

  { A spanned directive's continuation lines are blanked too, and the chain
    ends at the first continuation line which does not span itself }
  Assert(UTF8ToString(PrepareCodeSectionText(['#define X \', '  1 + \', '  2',
    'const C = 1;'])) = #13#10#13#10#13#10'const C = 1;');

  { A spanned code line group is joined onto its first line like ISPP joins
    it (ISPP joins spanned lines whether or not they are directives), with
    blank lines keeping the line count. The whitespace before the span
    symbol survives, every piece loses its leading whitespace. }
  Assert(UTF8ToString(PrepareCodeSectionText(['X := 1 \', '+ 2;'])) =
    'X := 1 + 2;'#13#10);
  Assert(UTF8ToString(PrepareCodeSectionText(['A := 1 \', '  + 2 \', '  + 3;',
    'B := 4;'])) = 'A := 1 + 2 + 3;'#13#10#13#10#13#10'B := 4;');

  { The join keeps spanned strings and spanned '//' comments intact }
  Assert(UTF8ToString(PrepareCodeSectionText(['S := ''a \', 'b'';'])) =
    'S := ''a b'';'#13#10);
  Assert(UTF8ToString(PrepareCodeSectionText(['// note \', 'X := 1;'])) =
    '// note X := 1;'#13#10);

  { Inline ISPP directives are kept: the tokenizer skips them as comments }
  Assert(UTF8ToString(PrepareCodeSectionText(['C := {#X} 1;'])) = 'C := {#X} 1;');

  { Non-ASCII text in strings and comments round-trips through the UTF-8
    encoding (identifiers cannot carry non-ASCII: the tokenizer rejects it) }
  const NonASCII = 'S := ''h'#$00E9'llo''; { '#$20AC' }';
  const Encoded = PrepareCodeSectionText([NonASCII]);
  Assert(UTF8ToString(Encoded) = NonASCII);
  Assert(Length(Encoded) = Length(NonASCII)+3); { #$00E9 is two UTF-8 bytes, #$20AC three }
end;

procedure TestCodeSection;
begin
  const Section = TScriptModelCodeSection.Create;
  try
    { Functions and procedures, in source order, with 0-based first lines }
    Section.Parse([
      'var',
      '  Global: Integer;',
      '',
      'function InitializeSetup: Boolean;',
      'begin',
      '  Result := True;',
      'end;',
      '',
      'procedure DoSomething(const A: Integer);',
      'begin',
      'end;']);
    Assert(Section.RoutineCount = 2);
    Assert(Section.Routines[0].Name = 'InitializeSetup');
    Assert(Section.Routines[0].Kind = rkFunction);
    Assert(Section.Routines[0].FirstLine = 3);
    Assert(Section.Routines[0].Prototype = 'function InitializeSetup: Boolean;');
    Assert(Section.Routines[0].ResultTypeText = 'Boolean');
    Assert(Section.Routines[0].BodyFirstLine = 4);
    Assert(Section.Routines[0].LastLine = 6);
    Assert(Section.Routines[0].BodilessType = btNo);
    Assert(Section.Routines[1].Name = 'DoSomething');
    Assert(Section.Routines[1].Kind = rkProcedure);
    Assert(Section.Routines[1].FirstLine = 8);
    Assert(Section.Routines[1].Prototype = 'procedure DoSomething(const A: Integer);');
    Assert(Section.Routines[1].ResultTypeText = '');
    Assert(Section.Routines[1].BodyFirstLine = 9);
    Assert(Section.Routines[1].LastLine = 10);

    { The next Parse replaces the previous items }
    Section.Parse(['procedure P;', 'begin', 'end;']);
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].Name = 'P');
    Section.Parse([]);
    Assert(Section.RoutineCount = 0);

    { A multi-line header: FirstLine is the keyword's line. The keyword and
      the name can even sit on different lines. The prototype's embedded line
      breaks collapse to single spaces together with the surrounding
      whitespace. }
    Section.Parse([
      '',
      'function MyFunc(const A: String;',
      '  const B: Integer): Boolean;',
      'begin',
      'end;']);
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].Name = 'MyFunc');
    Assert(Section.Routines[0].FirstLine = 1);
    Assert(Section.Routines[0].Prototype =
      'function MyFunc(const A: String; const B: Integer): Boolean;');
    Assert(Section.Routines[0].ResultTypeText = 'Boolean');
    Section.Parse(['procedure', '  Below;', 'begin', 'end;']);
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].Name = 'Below');
    Assert(Section.Routines[0].FirstLine = 0);
    Assert(Section.Routines[0].Prototype = 'procedure Below;');

    { Procedural types are not routines: a routine keyword after '=', ':',
      or 'of' does not start a header }
    Section.Parse([
      'type',
      '  TProc = procedure(Sender: TObject);',
      '  TFunc = function: Integer;',
      '  TProcArray = array of procedure;',
      'var',
      '  P: procedure;',
      '  F: function(A: Integer): Boolean;',
      'procedure TakesCallback(Callback: TProc);',
      'begin',
      'end;']);
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].Name = 'TakesCallback');
    Assert(Section.Routines[0].FirstLine = 7);

    { A name after the keyword does start a header, even after one of those
      tokens: ROPS never names a procedural type, so the declaration above is
      still being typed and keeps the routine below it }
    const UnfinishedDeclarations: TArray<String> = [
      'type X =', 'var P:', 'type X = array of'];
    for var I := 0 to High(UnfinishedDeclarations) do begin
      Section.Parse([
        UnfinishedDeclarations[I], { 0 }
        'procedure Existing;',     { 1 }
        'begin',                   { 2 }
        'end;']);                  { 3 }
      Assert(Section.RoutineCount = 1);
      Assert(Section.Routines[0].Name = 'Existing');
      Assert(Section.Routines[0].FirstLine = 1);
      Assert(Section.Routines[0].BodyFirstLine = 2);
      Assert(Section.Routines[0].LastLine = 3);
    end;

    { Also not after ':' inside a parameter list or as a procedural return
      type. A parameter list's ';' separators do not terminate the header. }
    Section.Parse([
      'function Weird(P: procedure; F: function: Integer): Boolean;',
      'begin',
      'end;']);
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].Name = 'Weird');
    Assert(Section.Routines[0].Prototype =
      'function Weird(P: procedure; F: function: Integer): Boolean;');
    Assert(Section.Routines[0].ResultTypeText = 'Boolean');
    Section.Parse([
      'function GetHandler: function(A, B: Integer): Boolean;',
      'begin',
      'end;']);
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].Name = 'GetHandler');
    Assert(Section.Routines[0].Prototype =
      'function GetHandler: function(A, B: Integer): Boolean;');
    Assert(Section.Routines[0].ResultTypeText = 'function(A, B: Integer): Boolean');

    { A procedural return type whose own parameter list contains ';' }
    Section.Parse([
      'function GetCompare: function(const A: String; const B: String): Integer;',
      'begin',
      'end;']);
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].Prototype =
      'function GetCompare: function(const A: String; const B: String): Integer;');
    Assert(Section.Routines[0].ResultTypeText =
      'function(const A: String; const B: String): Integer');

    { Something else can share the header's first or last physical line: the
      prototype is exactly the keyword through its ';' }
    Section.Parse(['const C = 1; procedure Shared; begin end;']);
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].Name = 'Shared');
    Assert(Section.Routines[0].Prototype = 'procedure Shared;');
    Assert(Section.Routines[0].ResultTypeText = '');

    { Every whitespace run collapses to a single space, also within a line,
      except before ')', ';', ',' and ']' }
    Section.Parse(['function  Spaced  (A: Integer) : Boolean ;', 'begin', 'end;']);
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].Prototype =
      'function Spaced (A: Integer) : Boolean;');
    Assert(Section.Routines[0].ResultTypeText = 'Boolean');
    Section.Parse(['function'#9'Tabbed(A:'#9'Integer):'#9'Boolean;', 'begin', 'end;']);
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].Prototype = 'function Tabbed(A: Integer): Boolean;');
    Assert(Section.Routines[0].ResultTypeText = 'Boolean');

    { A comment separates tokens like whitespace does, so it collapses into
      the surrounding run instead of merging its neighbours. Both block forms
      are recognized. }
    Section.Parse([
      'function{ a }Commented(A: Integer (* the size *)): Boolean;',
      'begin',
      'end;']);
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].Name = 'Commented');
    Assert(Section.Routines[0].Prototype =
      'function Commented(A: Integer): Boolean;');
    Assert(Section.Routines[0].ResultTypeText = 'Boolean');

    { A '(*' opener's own '*' may close the comment, like the ROPS tokenizer,
      so '(*)' is complete. See Script.ROPS.Test.iss for a compiler witness. }
    Section.Parse([
      'procedure Degenerate(A: Integer (*) );',
      'begin',
      'end;']);
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].Name = 'Degenerate');
    Assert(Section.Routines[0].Prototype = 'procedure Degenerate(A: Integer);');

    { For the same reason a '*' keeps its space before a ')' }
    Section.Parse(['procedure Starred(A: Integer = (2 * ));', 'begin', 'end;']);
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].Prototype = 'procedure Starred(A: Integer = (2 * ));');

    { A '//' comment ends at its line break, so the rest of the header
      survives, and it runs to the end when there is no line break left }
    Section.Parse([
      'function Split(A: // cut here',
      '  Integer): Boolean;',
      'begin',
      'end;']);
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].Prototype = 'function Split(A: Integer): Boolean;');
    Section.Parse(['procedure Trailing(A: Integer // note']);
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].Prototype = 'procedure Trailing(A: Integer');

    { A comment surrounded by whitespace merges into a single separator, and
      one trailing the result type leaves no trailing space }
    Section.Parse(['function Edges: { kind } Boolean { done };', 'begin', 'end;']);
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].Prototype = 'function Edges: Boolean;');
    Assert(Section.Routines[0].ResultTypeText = 'Boolean');

    { A comment's opening characters inside a string literal are kept: the
      literal is not a comment }
    Section.Parse(['procedure Literal(const S: String = ''{app}'');', 'begin', 'end;']);
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].Prototype =
      'procedure Literal(const S: String = ''{app}'');');
    Section.Parse(['procedure Quoted(const S: String = ''it''''s { x }'');', 'begin', 'end;']);
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].Prototype =
      'procedure Quoted(const S: String = ''it''''s { x }'');');

    { A non-ASCII comment leaves the prototype ASCII, unlike a non-ASCII string
      literal, which the call tip's ASCII test then still rejects }
    Section.Parse(['procedure Accented(A: Integer { caf'#$00E9' });', 'begin', 'end;']);
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].Prototype = 'procedure Accented(A: Integer);');
    Section.Parse(['procedure Kept(const S: String = ''caf'#$00E9''');', 'begin', 'end;']);
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].Prototype =
      'procedure Kept(const S: String = ''caf'#$00E9''');');

    { An unterminated header is cut short by the next declaration's keyword,
      its own body's 'begin', a tokenize error, or the section's end, keeping
      what is there }
    Section.Parse([
      'function Foo(A: Integer): Boolean',
      'procedure Bar;',
      'begin',
      'end;']);
    Assert(Section.RoutineCount = 2);
    Assert(Section.Routines[0].Prototype = 'function Foo(A: Integer): Boolean');
    Assert(Section.Routines[0].ResultTypeText = 'Boolean');
    Assert(Section.Routines[0].BodyFirstLine = -1);
    Assert(Section.Routines[0].LastLine = 0); { Line before the next declaration }
    Assert(Section.Routines[1].Name = 'Bar');
    Assert(Section.Routines[1].BodyFirstLine = 2);
    Assert(Section.Routines[1].LastLine = 3);
    Section.Parse(['function Typing(A: Integer): Str']);
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].Prototype = 'function Typing(A: Integer): Str');
    Assert(Section.Routines[0].ResultTypeText = 'Str');
    Assert(Section.Routines[0].BodyFirstLine = -1);
    Assert(Section.Routines[0].LastLine = 0); { Section's last line }

    { A header missing its ';' is cut by its own body's 'begin', which cannot
      be part of a header; the body is still parsed }
    Section.Parse([
      'function NoSemicolon: Boolean',  { 0 }
      'begin',                          { 1 }
      '  Result := True;',              { 2 }
      'end;',                           { 3 }
      'procedure After;',               { 4 }
      'begin',                          { 5 }
      'end;']);                         { 6 }
    Assert(Section.RoutineCount = 2);
    Assert(Section.Routines[0].Name = 'NoSemicolon');
    Assert(Section.Routines[0].Prototype = 'function NoSemicolon: Boolean');
    Assert(Section.Routines[0].ResultTypeText = 'Boolean');
    Assert(Section.Routines[0].BodyFirstLine = 1);
    Assert(Section.Routines[0].LastLine = 3);
    Assert(Section.Routines[1].Name = 'After');
    Assert(Section.Routines[1].BodyFirstLine = 5);
    Assert(Section.Routines[1].LastLine = 6);
    Section.Parse([
      'procedure NoSemicolon',  { 0 }
      'begin',                  { 1 }
      'end;']);                 { 2 }
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].Prototype = 'procedure NoSemicolon');
    Assert(Section.Routines[0].ResultTypeText = '');
    Assert(Section.Routines[0].BodyFirstLine = 1);
    Assert(Section.Routines[0].LastLine = 2);

    { A header missing its ';' is also cut by a declaration block start,
      possibly its own local blocks, which the 'begin' search then parses
      into its locals }
    Section.Parse([
      'function Foo: Boolean',  { 0 }
      'var',                    { 1 }
      '  X: Integer;',          { 2 }
      'begin',                  { 3 }
      'end;']);                 { 4 }
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].Prototype = 'function Foo: Boolean');
    Assert(Section.Routines[0].ResultTypeText = 'Boolean');
    Assert(Section.Routines[0].BodyFirstLine = 3);
    Assert(Section.Routines[0].LastLine = 4);
    Assert(Section.Routines[0].LocalCount = 1);
    Assert(Section.Routines[0].Locals[0].Name = 'X');

    { 'var' and 'const' in a parameter list do not cut the header }
    Section.Parse([
      'procedure P(var A: Integer; const B: String);',
      'begin',
      'end;']);
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].Prototype =
      'procedure P(var A: Integer; const B: String);');
    Assert(Section.Routines[0].BodyFirstLine = 1);

    { Nor does the 'end' of an inline record parameter type, which ROPS accepts }
    Section.Parse([
      'procedure P(A: record X: Integer; end);',  { 0 }
      'begin',                                    { 1 }
      'end;']);                                   { 2 }
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].Prototype =
      'procedure P(A: record X: Integer; end);');
    Assert(Section.Routines[0].BodyFirstLine = 1);
    Assert(Section.Routines[0].LastLine = 2);

    { A header left unterminated inside its parameter list is cut by the next
      routine's keyword, which is never legal there, so the routines below a
      half-typed 'procedure X(' survive }
    Section.Parse([
      'procedure X(',                        { 0 }
      '',                                    { 1 }
      'function InitializeSetup: Boolean;',  { 2 }
      'begin',                               { 3 }
      '  Result := True;',                   { 4 }
      'end;']);                              { 5 }
    Assert(Section.RoutineCount = 2);
    Assert(Section.Routines[0].Name = 'X');
    Assert(Section.Routines[0].Prototype = 'procedure X(');
    Assert(Section.Routines[0].BodyFirstLine = -1);
    Assert(Section.Routines[0].LastLine = 1); { Line before the next declaration }
    Assert(Section.Routines[1].Name = 'InitializeSetup');
    Assert(Section.Routines[1].Prototype = 'function InitializeSetup: Boolean;');
    Assert(Section.Routines[1].FirstLine = 2);
    Assert(Section.Routines[1].BodyFirstLine = 3);
    Assert(Section.Routines[1].LastLine = 5);
    Section.Parse([
      'procedure X(A: Integer;',  { 0 }
      'procedure After;',         { 1 }
      'begin',                    { 2 }
      'end;']);                   { 3 }
    Assert(Section.RoutineCount = 2);
    Assert(Section.Routines[0].Prototype = 'procedure X(A: Integer;');
    Assert(Section.Routines[0].LastLine = 0);
    Assert(Section.Routines[1].Name = 'After');
    Assert(Section.Routines[1].BodyFirstLine = 2);
    Section.Parse([
      'procedure X(A,',                      { 0 }
      'function InitializeSetup: Boolean;',  { 1 }
      'begin',                               { 2 }
      'end;']);                              { 3 }
    Assert(Section.RoutineCount = 2);
    Assert(Section.Routines[0].Prototype = 'procedure X(A,');
    Assert(Section.Routines[0].LastLine = 0);
    Assert(Section.Routines[1].Name = 'InitializeSetup');
    Assert(Section.Routines[1].BodyFirstLine = 2);

    { Its own 'begin' also cuts inside the parameter list, and the body is
      still parsed }
    Section.Parse([
      'procedure X(',  { 0 }
      'begin',         { 1 }
      'end;']);        { 2 }
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].Prototype = 'procedure X(');
    Assert(Section.Routines[0].BodyFirstLine = 1);
    Assert(Section.Routines[0].LastLine = 2);

    { An open parameter list does not swallow the type block below it:
      'type' never appears inside one }
    Section.Parse([
      'procedure Foo(',    { 0 }
      'type',              { 1 }
      '  Y = Integer;']);  { 2 }
    Assert(Section.RoutineCount = 1);
    Assert(Section.TypeCount = 1);
    Assert(Section.Types[0].Name = 'Y');

    { 'label' cuts inside one too. The block is then searched through as a
      possibly local one, so the routine still extends over it. }
    Section.Parse([
      'procedure Foo(',  { 0 }
      'label',           { 1 }
      '  L;']);          { 2 }
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].Prototype = 'procedure Foo(');
    Assert(Section.Routines[0].BodyFirstLine = -1);
    Assert(Section.Routines[0].LastLine = 2);

    { ROPS has no local type block, so one after a bodyless header is parsed
      rather than skipped. The search for 'begin' goes on past it. }
    Section.Parse([
      'procedure Foo;',      { 0 }
      'type',                { 1 }
      '  TBar = Integer;',   { 2 }
      'procedure Baz;',      { 3 }
      'begin',               { 4 }
      'end;']);              { 5 }
    Assert(Section.RoutineCount = 2);
    Assert(Section.Routines[0].Name = 'Foo');
    Assert(Section.Routines[0].BodyFirstLine = -1);
    Assert(Section.Routines[0].LastLine = 2);
    Assert(Section.TypeCount = 1);
    Assert(Section.Types[0].Name = 'TBar');
    Assert(Section.Types[0].Line = 2);
    Assert(Section.Routines[1].Name = 'Baz');
    Assert(Section.Routines[1].BodyFirstLine = 4);

    { A 'var' block is the routine's own, parsed into its locals, and the
      search for 'begin' goes on past it }
    Section.Parse([
      'procedure Foo;',   { 0 }
      'var',              { 1 }
      '  A: Integer;',    { 2 }
      'begin',            { 3 }
      'end;']);           { 4 }
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].BodyFirstLine = 3);
    Assert(Section.Routines[0].LocalCount = 1);
    Assert(Section.Routines[0].Locals[0].Name = 'A');
    Assert(Section.GlobalVariableCount = 0);
    Assert(Section.Routines[0].LastLine = 4);
    Assert(Section.TypeCount = 0);

    { A routine whose body is still missing does not swallow the type block
      below it, whichever way its body is unfinished }
    const UnfinishedRoutines: TArray<String> = [
      'procedure x;', 'procedure x; end;', 'procedure x; forward',
      'procedure x; const C = 1;'];
    for var I := 0 to High(UnfinishedRoutines) do begin
      Section.Parse([UnfinishedRoutines[I], 'type', '  Y = Integer;']);
      Assert(Section.RoutineCount = 1);
      Assert(Section.TypeCount = 1);
      Assert(Section.Types[0].Name = 'Y');
      Assert(Section.Types[0].Line = 2);
    end;

    { <event('...')> attributes before a header are tolerated; FirstLine stays
      the keyword's line }
    Section.Parse([
      '<event(''InitializeWizard'')>',
      'procedure MyInitializeWizard2;',
      'begin',
      'end;']);
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].Name = 'MyInitializeWizard2');
    Assert(Section.Routines[0].FirstLine = 1);
    Assert(Section.Routines[0].Prototype = 'procedure MyInitializeWizard2;'); { The attribute is not part of the prototype }

    { Keyword text inside comments and strings is not a routine }
    Section.Parse([
      '{ procedure InComment1; }',
      '// procedure InComment2;',
      '(* function InComment3: Integer; *)',
      'const S = ''procedure InString;'';',
      'procedure RealOne;',
      'begin',
      '  Log(''function InString2'');',
      'end;']);
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].Name = 'RealOne');
    Assert(Section.Routines[0].FirstLine = 4);

    { ISPP directive lines are blanked without shifting later line numbers }
    Section.Parse([
      '#define MyVersion "1.0"',
      'procedure AfterDirective;',
      'begin',
      'end;',
      '#define Multi \',
      '  1',
      'procedure AfterSpanned;',
      'begin',
      'end;']);
    Assert(Section.RoutineCount = 2);
    Assert(Section.Routines[0].Name = 'AfterDirective');
    Assert(Section.Routines[0].FirstLine = 1);
    Assert(Section.Routines[1].Name = 'AfterSpanned');
    Assert(Section.Routines[1].FirstLine = 6);

    { A blanked ISPP directive line inside a header collapses like a line
      break }
    Section.Parse([
      'function Directive(A: Integer;',
      '#define X 1',
      '  B: Integer): Boolean;',
      'begin',
      'end;']);
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].Prototype =
      'function Directive(A: Integer; B: Integer): Boolean;');

    { A header split with ISPP's span symbol parses like ISPP's joined line:
      the group sits on its first line and later line numbers do not shift }
    Section.Parse([
      'procedure Spanned(A: Integer; \',
      '  B: Integer);',
      'begin',
      'end;',
      'procedure AfterSpannedCode;',
      'begin',
      'end;']);
    Assert(Section.RoutineCount = 2);
    Assert(Section.Routines[0].Name = 'Spanned');
    Assert(Section.Routines[0].FirstLine = 0);
    Assert(Section.Routines[0].Prototype =
      'procedure Spanned(A: Integer; B: Integer);');
    Assert(Section.Routines[0].BodyFirstLine = 2);
    Assert(Section.Routines[0].LastLine = 3);
    Assert(Section.Routines[1].Name = 'AfterSpannedCode');
    Assert(Section.Routines[1].FirstLine = 4);

    { A spanned string does not abort the scan }
    Section.Parse([
      'const S = ''a \',
      '  b'';',
      'procedure AfterSpannedString;',
      'begin',
      'end;']);
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].Name = 'AfterSpannedString');
    Assert(Section.Routines[0].FirstLine = 2);

    { A spanned '//' comment comments out its continuation }
    Section.Parse([
      '// disabled: \',
      'procedure Old;',
      'procedure Current;',
      'begin',
      'end;']);
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].Name = 'Current');
    Assert(Section.Routines[0].FirstLine = 2);

    { Body ranges: BodyFirstLine is the 'begin' line, and LastLine the
      matching 'end' line found by depth counting: 'begin', 'case' and 'try'
      nest }
    Section.Parse([
      'procedure Nested(A: Integer);',
      'begin',
      '  case A of',
      '    0: begin',
      '      try',
      '        Log(''x'');',
      '      finally',
      '      end;',
      '    end;',
      '  end;',
      'end;',
      '',
      'procedure After;',
      'begin',
      'end;']);
    Assert(Section.RoutineCount = 2);
    Assert(Section.Routines[0].BodyFirstLine = 1);
    Assert(Section.Routines[0].LastLine = 10);
    Assert(Section.Routines[0].BodilessType = btNo);
    Assert(Section.Routines[1].BodyFirstLine = 13);
    Assert(Section.Routines[1].LastLine = 14);

    { Bodiless routines: 'forward' and 'external' mean no body. LastLine is
      the directive's ';' line, which can sit below the header. A forward
      declaration and its implementation are two separate items. }
    Section.Parse([
      'procedure Later(A: Integer); forward;',
      '',
      'function GetSysDir: String;',
      '  external ''GetSystemDirectoryW@kernel32.dll stdcall'';',
      '',
      'procedure Later(A: Integer);',
      'begin',
      'end;']);
    Assert(Section.RoutineCount = 3);
    Assert(Section.Routines[0].Name = 'Later');
    Assert(Section.Routines[0].BodilessType = btForward);
    Assert(Section.Routines[0].BodyFirstLine = -1);
    Assert(Section.Routines[0].LastLine = 0);
    Assert(Section.Routines[0].Prototype = 'procedure Later(A: Integer);');
    Assert(Section.Routines[1].Name = 'GetSysDir');
    Assert(Section.Routines[1].BodilessType = btExternal);
    Assert(Section.Routines[1].BodyFirstLine = -1);
    Assert(Section.Routines[1].FirstLine = 2);
    Assert(Section.Routines[1].LastLine = 3);
    Assert(Section.Routines[1].Prototype = 'function GetSysDir: String;');
    Assert(Section.Routines[2].Name = 'Later');
    Assert(Section.Routines[2].BodilessType = btNo);
    Assert(Section.Routines[2].BodyFirstLine = 6);
    Assert(Section.Routines[2].LastLine = 7);

    { 'export' says nothing about the body }
    Section.Parse([
      'procedure Exported; export;',
      'begin',
      'end;']);
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].BodilessType = btNo);
    Assert(Section.Routines[0].BodyFirstLine = 1);
    Assert(Section.Routines[0].LastLine = 2);

    { 'label' and 'var' blocks before the body do not hide it: the 'label'
      block is skipped and the 'var' block parses into the locals }
    Section.Parse([
      'procedure WithLabel;',
      'label',
      '  Retry;',
      'var',
      '  A: Integer;',
      'begin',
      '  Retry:',
      '  A := 0;',
      'end;']);
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].BodyFirstLine = 5);
    Assert(Section.Routines[0].LastLine = 8);
    Assert(Section.Routines[0].LocalCount = 1);
    Assert(Section.Routines[0].Locals[0].Name = 'A');

    { A terminated header with no body yet: the begin search stops at the
      next declaration, or at the section's end }
    Section.Parse([
      'procedure NewProc;',
      '',
      'procedure Existing;',
      'begin',
      'end;']);
    Assert(Section.RoutineCount = 2);
    Assert(Section.Routines[0].Name = 'NewProc');
    Assert(Section.Routines[0].BodyFirstLine = -1);
    Assert(Section.Routines[0].LastLine = 1);
    Assert(Section.Routines[0].BodilessType = btNo);
    Assert(Section.Routines[1].Name = 'Existing');
    Section.Parse(['procedure NewProc;', '']);
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].BodyFirstLine = -1);
    Assert(Section.Routines[0].LastLine = 1);
    Assert(Section.Routines[0].BodilessType = btNo);
    Section.Parse(['procedure A; procedure B; begin end;']);
    Assert(Section.RoutineCount = 2);
    Assert(Section.Routines[0].Name = 'A');
    Assert(Section.Routines[0].LastLine = 0); { Clamped to FirstLine }
    Assert(Section.Routines[1].Name = 'B');
    Assert(Section.Routines[1].BodyFirstLine = 0);
    Assert(Section.Routines[1].LastLine = 0);

    { An unterminated body keeps its 'begin' line and gives a span running
      to the line before the next declaration, or to the section's last
      line, so a caret query inside a body still being typed reports the
      routine }
    Section.Parse([
      'procedure Typing;',
      'begin',
      '  X := 1;',
      '',
      'procedure Next;',
      'begin',
      'end;']);
    Assert(Section.RoutineCount = 2);
    Assert(Section.Routines[0].Name = 'Typing');
    Assert(Section.Routines[0].BodyFirstLine = 1);
    Assert(Section.Routines[0].LastLine = 3);
    Assert(Section.Routines[0].BodilessType = btNo);
    Assert(Section.Routines[1].Name = 'Next');
    Assert(Section.Routines[1].BodyFirstLine = 5);
    Assert(Section.Routines[1].LastLine = 6);
    Section.Parse([
      'procedure Typing;',
      'begin',
      '  if X then begin',
      '  end;',
      '']);
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].BodyFirstLine = 1);
    Assert(Section.Routines[0].LastLine = 4);

    { A declaration block also ends an unterminated body's span, so the
      block is not swallowed as body text: a type block still gets its types,
      and a record's 'end' inside it does not pose as the body's 'end' }
    Section.Parse([
      'procedure Typing;',  { 0 }
      'begin',              { 1 }
      '  X := 1;',          { 2 }
      '',                   { 3 }
      'type',               { 4 }
      '  TFoo = record',    { 5 }
      '    A: Integer;',    { 6 }
      '  end;',             { 7 }
      'var',                { 8 }
      '  V: Integer;']);    { 9 }
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].Name = 'Typing');
    Assert(Section.Routines[0].BodyFirstLine = 1);
    Assert(Section.Routines[0].LastLine = 3);
    Assert(Section.TypeCount = 1);
    Assert(Section.Types[0].Name = 'TFoo');
    Assert(Section.Types[0].TypeText = 'record');
    Assert(Section.Types[0].Line = 5);
    Section.Parse([
      'procedure Typing;',  { 0 }
      'begin',              { 1 }
      'var',                { 2 }
      '  V: Integer;']);    { 3 }
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].BodyFirstLine = 1);
    Assert(Section.Routines[0].LastLine = 1);
    Section.Parse([
      'procedure Typing;',        { 0 }
      'begin var A: Integer;']);  { 1 }
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].BodyFirstLine = 1);
    Assert(Section.Routines[0].LastLine = 1); { Clamped to BodyFirstLine }

    { Malformed input never raises: declarations before a tokenize error are
      kept, and the scan resyncs past the error (see TestCodeSectionResync) }
    Section.Parse([
      'procedure BeforeError;',
      'begin',
      'end;',
      'S := ''unterminated',
      'procedure AfterError;']);
    Assert(Section.RoutineCount = 2);
    Assert(Section.Routines[0].Name = 'BeforeError');
    Assert(Section.Routines[1].Name = 'AfterError');
    Section.Parse(['%!?']);
    Assert(Section.RoutineCount = 0);
  finally
    Section.Free;
  end;
end;

procedure TestCodeSectionTypes;
begin
  const Section = TScriptModelCodeSection.Create;
  try
    { Each definition kind gives its normalized kind word. Name and Line come
      from the member's identifier. A record ends at its 'end'. }
    Section.Parse([
      'type',
      '  TMyRecord = record',
      '    A: Integer;',
      '    B: String;',
      '  end;',
      '  TIntArray = array of Integer;',
      '  TByteSet = set of Byte;',
      '  TProc = procedure(Sender: TObject);',
      '  TFunc = function(A, B: Integer): Boolean;',
      '  TMyState = (msOne, msTwo);',
      '  TInt = Integer;']);
    Assert(Section.RoutineCount = 0);
    Assert(Section.TypeCount = 7);
    Assert(Section.Types[0].Name = 'TMyRecord');
    Assert(Section.Types[0].TypeText = 'record');
    Assert(Section.Types[0].Line = 1);
    Assert(Section.Types[1].Name = 'TIntArray');
    Assert(Section.Types[1].TypeText = 'array');
    Assert(Section.Types[1].Line = 5);
    Assert(Section.Types[2].Name = 'TByteSet');
    Assert(Section.Types[2].TypeText = 'set');
    Assert(Section.Types[2].Line = 6);
    Assert(Section.Types[3].Name = 'TProc');
    Assert(Section.Types[3].TypeText = 'procedure');
    Assert(Section.Types[3].Line = 7);
    Assert(Section.Types[4].Name = 'TFunc');
    Assert(Section.Types[4].TypeText = 'function');
    Assert(Section.Types[4].Line = 8);
    Assert(Section.Types[5].Name = 'TMyState');
    Assert(Section.Types[5].TypeText = 'enumeration');
    Assert(Section.Types[5].Line = 9);
    Assert(Section.Types[6].Name = 'TInt');
    Assert(Section.Types[6].TypeText = 'Integer'); { An alias gives the identifier as written }
    Assert(Section.Types[6].Line = 10);

    { The next Parse replaces the previous items }
    Section.Parse(['type', '  TOnly = Integer;']);
    Assert(Section.TypeCount = 1);
    Assert(Section.Types[0].Name = 'TOnly');
    Section.Parse(['type']);
    Assert(Section.TypeCount = 0);
    Section.Parse([]);
    Assert(Section.TypeCount = 0);

    { An interface is consumed as one definition: one type item and none of
      its method declarations as top-level routines }
    Section.Parse([
      'type',
      '  IPersistFile = interface(IPersist)',
      '    ''{0000010B-0000-0000-C000-000000000046}''',
      '    procedure Save(pszFileName: String; fRemember: BOOL); safecall;',
      '    function GetCurFile: String; safecall;',
      '  end;',
      '',
      'procedure AfterInterface;',
      'begin',
      'end;']);
    Assert(Section.TypeCount = 1);
    Assert(Section.Types[0].Name = 'IPersistFile');
    Assert(Section.Types[0].TypeText = 'interface');
    Assert(Section.Types[0].Line = 1);
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].Name = 'AfterInterface');
    Assert(Section.Routines[0].FirstLine = 7);

    { A routine header ends the block; the routine still parses }
    Section.Parse([
      'type',
      '  TState = (stOne, stTwo);',
      'procedure UseState(S: TState);',
      'begin',
      'end;']);
    Assert(Section.TypeCount = 1);
    Assert(Section.Types[0].Name = 'TState');
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].Name = 'UseState');
    Assert(Section.Routines[0].FirstLine = 2);
    Assert(Section.Routines[0].LastLine = 4);

    { A routine header also ends an unterminated definition, keeping the type }
    Section.Parse([
      'type',
      '  TFoo = Integer',
      'procedure P;',
      'begin',
      'end;']);
    Assert(Section.TypeCount = 1);
    Assert(Section.Types[0].Name = 'TFoo');
    Assert(Section.Types[0].TypeText = 'Integer');
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].Name = 'P');
    Assert(Section.Routines[0].FirstLine = 2);

    { A declaration block start also ends an unterminated definition, keeping
      the next block's members }
    Section.Parse([
      'type',             { 0 }
      '  T = Integer',    { 1 }
      'type',             { 2 }
      '  U = String;']);  { 3 }
    Assert(Section.TypeCount = 2);
    Assert(Section.Types[0].Name = 'T');
    Assert(Section.Types[0].TypeText = 'Integer');
    Assert(Section.Types[0].Line = 1);
    Assert(Section.Types[1].Name = 'U');
    Assert(Section.Types[1].TypeText = 'String');
    Assert(Section.Types[1].Line = 3);

    { 'var' and 'const' in a procedural type's parameter list do not end the
      definition }
    Section.Parse([
      'type',
      '  TProc = procedure(var A: Integer; const B: String);',
      '  TAfter = Integer;']);
    Assert(Section.TypeCount = 2);
    Assert(Section.Types[0].Name = 'TProc');
    Assert(Section.Types[0].TypeText = 'procedure');
    Assert(Section.Types[1].Name = 'TAfter');

    { An unterminated record does not hide the routines below it }
    Section.Parse([
      'type',
      '  TFoo = record',
      '    A: Integer;',
      '',
      'procedure P;',
      'begin',
      'end;']);
    Assert(Section.TypeCount = 1);
    Assert(Section.Types[0].Name = 'TFoo');
    Assert(Section.Types[0].TypeText = 'record');
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].Name = 'P');
    Assert(Section.Routines[0].FirstLine = 4);

    { Also not when the record's unterminated procedural field leaves its
      parentheses open }
    Section.Parse([
      'type',
      '  TFoo = record',
      '    A: function(B: Integer;',
      '',
      'procedure P;',
      'begin',
      'end;']);
    Assert(Section.TypeCount = 1);
    Assert(Section.Types[0].Name = 'TFoo');
    Assert(Section.Types[0].TypeText = 'record');
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].Name = 'P');
    Assert(Section.Routines[0].FirstLine = 4);

    { Nor when an unterminated enumeration or procedural type leaves its
      parentheses open }
    Section.Parse([
      'type',
      '  TState = (stOne, stTwo',
      'procedure P;',
      'begin',
      'end;']);
    Assert(Section.TypeCount = 1);
    Assert(Section.Types[0].Name = 'TState');
    Assert(Section.Types[0].TypeText = 'enumeration');
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].Name = 'P');
    Assert(Section.Routines[0].FirstLine = 2);
    Section.Parse([
      'type',
      '  TF = function(A: Integer;',
      'procedure P;',
      'begin',
      'end;']);
    Assert(Section.TypeCount = 1);
    Assert(Section.Types[0].Name = 'TF');
    Assert(Section.Types[0].TypeText = 'function');
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].Name = 'P');
    Assert(Section.Routines[0].FirstLine = 2);

    { An unterminated interface does hide them: its methods are
      indistinguishable from routine headers }
    Section.Parse([
      'type',
      '  IFoo = interface',
      '    procedure M1;',
      '',
      'procedure P;',
      'begin',
      'end;']);
    Assert(Section.TypeCount = 1);
    Assert(Section.Types[0].Name = 'IFoo');
    Assert(Section.Types[0].TypeText = 'interface');
    Assert(Section.RoutineCount = 0);

    { But a declaration block start does end an unterminated interface: it
      can never be one of its members }
    Section.Parse([
      'type',               { 0 }
      '  IFoo = interface', { 1 }
      '    procedure M1;',  { 2 }
      'var',                { 3 }
      '  X: Integer;',      { 4 }
      'procedure P;',       { 5 }
      'begin',              { 6 }
      'end;']);             { 7 }
    Assert(Section.TypeCount = 1);
    Assert(Section.Types[0].Name = 'IFoo');
    Assert(Section.Types[0].TypeText = 'interface');
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].Name = 'P');
    Assert(Section.Routines[0].FirstLine = 5);
    Assert(Section.Routines[0].BodyFirstLine = 6);

    { A block below a routine }
    Section.Parse([
      'procedure P;',
      'begin',
      'end;',
      'type',
      '  TAfter = String;']);
    Assert(Section.RoutineCount = 1);
    Assert(Section.TypeCount = 1);
    Assert(Section.Types[0].Name = 'TAfter');
    Assert(Section.Types[0].TypeText = 'String');
    Assert(Section.Types[0].Line = 4);

    { A subrange definition is not valid ROPS; it only must not derail the
      scanner. The kind is not a listed one, so it gives ''. }
    Section.Parse([
      'type',
      '  TRange = 0..9;',
      '  TAfter = Integer;',
      'procedure P;',
      'begin',
      'end;']);
    Assert(Section.TypeCount = 2);
    Assert(Section.Types[0].Name = 'TRange');
    Assert(Section.Types[0].TypeText = '');
    Assert(Section.Types[0].Line = 1);
    Assert(Section.Types[1].Name = 'TAfter');
    Assert(Section.Types[1].TypeText = 'Integer');
    Assert(Section.RoutineCount = 1);

    { The next declaration is found while a record or a parameter list of the
      one being typed is still open }
    const UnfinishedStructuredTypes: TArray<String> = [
      '  X = record A: Integer;', '  X = procedure(A: Integer'];
    for var I := 0 to High(UnfinishedStructuredTypes) do begin
      Section.Parse(['type', UnfinishedStructuredTypes[I], '  Y = Integer;']);
      Assert(Section.TypeCount = 2);
      Assert(Section.Types[1].Name = 'Y');
      Assert(Section.Types[1].TypeText = 'Integer');
      Assert(Section.Types[1].Line = 2);
    end;

    { A name followed by '=' after the keyword is the next type declaration,
      not a routine name }
    Section.Parse([
      'type',              { 0 }
      '  X = procedure',   { 1 }
      '  Y = Integer;']);  { 2 }
    Assert(Section.RoutineCount = 0);
    Assert(Section.TypeCount = 2);
    Assert(Section.Types[1].Name = 'Y');
    Assert(Section.Types[1].TypeText = 'Integer');

    { A malformed declaration does not hide the finished ones below it }
    Section.Parse([
      'type',                 { 0 }
      '  = Integer;',         { 1 }
      '  ;',                  { 2 }
      '  TKept = String;']);  { 3 }
    Assert(Section.TypeCount = 1);
    Assert(Section.Types[0].Name = 'TKept');
    Assert(Section.Types[0].TypeText = 'String');
    Assert(Section.Types[0].Line = 3);
  finally
    Section.Free;
  end;
end;

procedure TestCodeSectionEnumerationValues;
begin
  const Section = TScriptModelCodeSection.Create;
  try
    { An enumeration definition yields its type item plus one item per value,
      with each value pointing back at the type }
    Section.Parse([
      'type',
      '  TMyState = (msOne, msTwo);']);
    Assert(Section.TypeCount = 1);
    Assert(Section.Types[0].Name = 'TMyState');
    Assert(Section.Types[0].TypeText = 'enumeration');
    Assert(Section.EnumerationValueCount = 2);
    Assert(Section.EnumerationValues[0].Name = 'msOne');
    Assert(Section.EnumerationValues[0].DeclarationTypeIndex = 0);
    Assert(Section.EnumerationValues[0].Line = 1);
    Assert(Section.EnumerationValues[1].Name = 'msTwo');
    Assert(Section.EnumerationValues[1].DeclarationTypeIndex = 0);
    Assert(Section.EnumerationValues[1].Line = 1);

    { The next Parse replaces the previous items }
    Section.Parse(['type', '  TOther = (a);']);
    Assert(Section.EnumerationValueCount = 1);
    Assert(Section.EnumerationValues[0].Name = 'a');
    Assert(Section.EnumerationValues[0].DeclarationTypeIndex = 0);
    Section.Parse([]);
    Assert(Section.EnumerationValueCount = 0);

    { Values spread over multiple lines report their own lines }
    Section.Parse([
      'type',               { 0 }
      '  TMyState = (',     { 1 }
      '    msOne,',         { 2 }
      '    msTwo, msThree', { 3 }
      '  );']);             { 4 }
    Assert(Section.EnumerationValueCount = 3);
    Assert(Section.EnumerationValues[0].Name = 'msOne');
    Assert(Section.EnumerationValues[0].Line = 2);
    Assert(Section.EnumerationValues[1].Name = 'msTwo');
    Assert(Section.EnumerationValues[1].Line = 3);
    Assert(Section.EnumerationValues[2].Name = 'msThree');
    Assert(Section.EnumerationValues[2].Line = 3);

    { Two enumerations keep their values apart }
    Section.Parse([
      'type',
      '  TA = (aOne, aTwo);',
      '  TB = (bOne);']);
    Assert(Section.TypeCount = 2);
    Assert(Section.Types[0].Name = 'TA');
    Assert(Section.Types[1].Name = 'TB');
    Assert(Section.EnumerationValueCount = 3);
    Assert(Section.EnumerationValues[0].Name = 'aOne');
    Assert(Section.EnumerationValues[0].DeclarationTypeIndex = 0);
    Assert(Section.EnumerationValues[1].Name = 'aTwo');
    Assert(Section.EnumerationValues[1].DeclarationTypeIndex = 0);
    Assert(Section.EnumerationValues[2].Name = 'bOne');
    Assert(Section.EnumerationValues[2].DeclarationTypeIndex = 1);

    { Two enumerations sharing a name still keep their values apart }
    Section.Parse([
      'type',
      '  TSame = (sOne);',
      '  TSame = (sTwo);']);
    Assert(Section.TypeCount = 2);
    Assert(Section.EnumerationValueCount = 2);
    Assert(Section.EnumerationValues[0].Name = 'sOne');
    Assert(Section.EnumerationValues[0].DeclarationTypeIndex = 0);
    Assert(Section.EnumerationValues[1].Name = 'sTwo');
    Assert(Section.EnumerationValues[1].DeclarationTypeIndex = 1);

    { An anonymous enumeration after 'of' belongs to the type being declared }
    Section.Parse([
      'type',
      '  TItems = array of (one, two);']);
    Assert(Section.TypeCount = 1);
    Assert(Section.Types[0].Name = 'TItems');
    Assert(Section.Types[0].TypeText = 'array');
    Assert(Section.EnumerationValueCount = 2);
    Assert(Section.EnumerationValues[0].Name = 'one');
    Assert(Section.EnumerationValues[0].DeclarationTypeIndex = 0);
    Assert(Section.EnumerationValues[1].Name = 'two');
    Assert(Section.EnumerationValues[1].DeclarationTypeIndex = 0);

    { The same for one after ':', in a record field }
    Section.Parse([
      'type',                        { 0 }
      '  TFoo = record',             { 1 }
      '    A: (aOne, aTwo);',        { 2 }
      '    B: Integer;',             { 3 }
      '  end;',                      { 4 }
      '  TAfter = (bOne);']);        { 5 }
    Assert(Section.TypeCount = 2);
    Assert(Section.Types[0].Name = 'TFoo');
    Assert(Section.Types[0].TypeText = 'record');
    Assert(Section.Types[1].Name = 'TAfter');
    Assert(Section.EnumerationValueCount = 3);
    Assert(Section.EnumerationValues[0].Name = 'aOne');
    Assert(Section.EnumerationValues[0].DeclarationTypeIndex = 0);
    Assert(Section.EnumerationValues[0].Line = 2);
    Assert(Section.EnumerationValues[1].Name = 'aTwo');
    Assert(Section.EnumerationValues[1].DeclarationTypeIndex = 0);
    Assert(Section.EnumerationValues[2].Name = 'bOne');
    Assert(Section.EnumerationValues[2].DeclarationTypeIndex = 1);

    { One in a procedural type's parameter list keeps the brace depth balanced,
      so the definition still terminates on its own semicolon }
    Section.Parse([
      'type',
      '  TProc = procedure(A: (pOne, pTwo); B: Integer);',
      '  TAfter = Integer;']);
    Assert(Section.TypeCount = 2);
    Assert(Section.Types[0].Name = 'TProc');
    Assert(Section.Types[0].TypeText = 'procedure');
    Assert(Section.Types[1].Name = 'TAfter');
    Assert(Section.EnumerationValueCount = 2);
    Assert(Section.EnumerationValues[0].Name = 'pOne');
    Assert(Section.EnumerationValues[0].DeclarationTypeIndex = 0);
    Assert(Section.EnumerationValues[1].Name = 'pTwo');

    { Non-enumeration definitions with parentheses contribute no values }
    Section.Parse([
      'type',
      '  TProc = procedure(Sender: TObject);',
      '  IFoo = interface',
      '    procedure M1(A: Integer);',
      '  end;']);
    Assert(Section.TypeCount = 2);
    Assert(Section.EnumerationValueCount = 0);

    { An enumeration cut by a routine header keeps the values already seen }
    Section.Parse([
      'type',
      '  TState = (stOne, stTwo',
      'procedure P;',
      'begin',
      'end;']);
    Assert(Section.EnumerationValueCount = 2);
    Assert(Section.EnumerationValues[0].Name = 'stOne');
    Assert(Section.EnumerationValues[1].Name = 'stTwo');
    Assert(Section.RoutineCount = 1);

    { A declaration block start ends an unterminated definition }
    Section.Parse([
      'type',
      '  TState = (stOne, stTwo',
      'type',
      '  TOther = Integer;']);
    Assert(Section.EnumerationValueCount = 2);
    Assert(Section.TypeCount = 2);
    Assert(Section.Types[0].Name = 'TState');
    Assert(Section.Types[1].Name = 'TOther');

    { A record following an unterminated list contributes no values, not even
      its comma-separated fields, and is still found as a type of its own }
    Section.Parse([
      'type',
      '  TState = (stOne, stTwo',
      '  TRec = record A, B: Integer; end;']);
    Assert(Section.EnumerationValueCount = 2);
    Assert(Section.EnumerationValues[0].Name = 'stOne');
    Assert(Section.EnumerationValues[1].Name = 'stTwo');
    Assert(Section.TypeCount = 2);
    Assert(Section.Types[0].Name = 'TState');
    Assert(Section.Types[1].Name = 'TRec');
    Assert(Section.Types[1].TypeText = 'record');

    { Every stage of typing a new type above an existing one keeps that one }
    const TypedSoFar: TArray<String> = [
      '  X', '  X =', '  X = (', '  X = (one', '  X = (one,',
      '  X = (one)', '  X = (one;', '  X = (one);'];
    for var I := 0 to High(TypedSoFar) do begin
      Section.Parse(['type', TypedSoFar[I], '  Y = Integer;']);
      Assert(Section.Types[Section.TypeCount-1].Name = 'Y');
      Assert(Section.Types[Section.TypeCount-1].TypeText = 'Integer');
      Assert(Section.Types[Section.TypeCount-1].Line = 2);
    end;
  finally
    Section.Free;
  end;
end;

procedure TestCodeSectionConstants;
begin
  const Section = TScriptModelCodeSection.Create;
  try
    { A const block yields one item per constant, with a display type
      inferred from a single-literal value }
    Section.Parse([
      'const',                      { 0 }
      '  MyInt = 5;',               { 1 }
      '  MyHex = $FF;',             { 2 }
      '  MyNegative = -5;',         { 3 }
      '  MyPositive = +5;',         { 4 }
      '  MyReal = 1.5;',            { 5 }
      '  MyNegativeReal = -0.5;',   { 6 }
      '  MyExponent = 2.5E10;',     { 7 }
      '  MyString = ''abc'';',      { 8 }
      '  MyChar = #13;',            { 9 }
      '  MyParts = ''a''#13''b'';', { 10 }
      '  MyTrue = True;',           { 11 }
      '  MyFalse = FALSE;']);       { 12 }
    Assert(Section.ConstantCount = 12);
    Assert(Section.Constants[0].Name = 'MyInt');
    Assert(Section.Constants[0].TypeText = 'Integer');
    Assert(Section.Constants[0].Line = 1);
    Assert(Section.Constants[1].Name = 'MyHex');
    Assert(Section.Constants[1].TypeText = 'Integer');
    Assert(Section.Constants[2].Name = 'MyNegative');
    Assert(Section.Constants[2].TypeText = 'Integer');
    Assert(Section.Constants[3].Name = 'MyPositive');
    Assert(Section.Constants[3].TypeText = 'Integer');
    Assert(Section.Constants[4].Name = 'MyReal');
    Assert(Section.Constants[4].TypeText = 'Extended');
    Assert(Section.Constants[5].Name = 'MyNegativeReal');
    Assert(Section.Constants[5].TypeText = 'Extended');
    Assert(Section.Constants[6].Name = 'MyExponent');
    Assert(Section.Constants[6].TypeText = 'Extended');
    Assert(Section.Constants[7].Name = 'MyString');
    Assert(Section.Constants[7].TypeText = 'String');
    Assert(Section.Constants[8].Name = 'MyChar');
    Assert(Section.Constants[8].TypeText = 'String');
    Assert(Section.Constants[9].Name = 'MyParts');
    Assert(Section.Constants[9].TypeText = 'String');
    Assert(Section.Constants[10].Name = 'MyTrue');
    Assert(Section.Constants[10].TypeText = 'Boolean');
    Assert(Section.Constants[11].Name = 'MyFalse');
    Assert(Section.Constants[11].TypeText = 'Boolean');
    Assert(Section.Constants[11].Line = 12);

    { The next Parse replaces the previous items }
    Section.Parse(['const', '  A = 1;']);
    Assert(Section.ConstantCount = 1);
    Assert(Section.Constants[0].Name = 'A');
    Section.Parse([]);
    Assert(Section.ConstantCount = 0);

    { An expression gets a coarse type: operands are literals and already
      declared constants, a comparison makes the value 'Boolean' even with
      unresolved operands, mod/shl/shr keep it 'Integer', and other
      operators, including '/' and 'div', take their operands' type, an
      integer next to a real widening to 'Extended' }
    Section.Parse([
      'const',                      { 0 }
      '  CA = 10;',                 { 1 }
      '  CB = CA + 5;',             { 2 }
      '  CC = CA * CB div 3;',      { 3 }
      '  CD = CA > CB;',            { 4 }
      '  CE = not False;',          { 5 }
      '  CF = (CA shl 2) or $0F;',  { 6 }
      '  CG = CA mod 3;',           { 7 }
      '  CH = CA / 2;',             { 8 }
      '  CI = 1 + 1.5;',            { 9 }
      '  CJ = ''a'' + ''b'';',      { 10 }
      '  CK = CE and (CA <> CB);',  { 11 }
      '  CL = not not True;',       { 12 }
      '  CM = CA / 1.5;',           { 13 }
      '  CN = CA div 1.5;',         { 14 }
      '  CO = MaxInt > 0;']);       { 15 }
    Assert(Section.ConstantCount = 15);
    Assert(Section.Constants[0].Name = 'CA');
    Assert(Section.Constants[0].TypeText = 'Integer');
    Assert(Section.Constants[1].Name = 'CB');
    Assert(Section.Constants[1].TypeText = 'Integer');
    Assert(Section.Constants[2].TypeText = 'Integer');
    Assert(Section.Constants[3].TypeText = 'Boolean');
    Assert(Section.Constants[4].TypeText = 'Boolean');
    Assert(Section.Constants[5].TypeText = 'Integer');
    Assert(Section.Constants[6].TypeText = 'Integer');
    { Without PS_DELPHIDIV, '/' on integers is an integer division in ROPS }
    Assert(Section.Constants[7].TypeText = 'Integer');
    Assert(Section.Constants[8].TypeText = 'Extended');
    Assert(Section.Constants[9].TypeText = 'String');
    Assert(Section.Constants[10].TypeText = 'Boolean');
    Assert(Section.Constants[11].TypeText = 'Boolean');
    Assert(Section.Constants[12].TypeText = 'Extended');
    { ROPS 'div' takes real operands too, and then gives a real result
      like '/' }
    Assert(Section.Constants[13].TypeText = 'Extended');
    { A comparison is 'Boolean' although MaxInt itself is out of reach }
    Assert(Section.Constants[14].TypeText = 'Boolean');

    { An '=' comparing an already declared constant on the value's own line
      is not the next declaration }
    Section.Parse([
      'const',              { 0 }
      '  A = 1;',           { 1 }
      '  IsOne = A = 1;',   { 2 }
      '  Below = 2;']);     { 3 }
    Assert(Section.ConstantCount = 3);
    Assert(Section.Constants[0].Name = 'A');
    Assert(Section.Constants[1].Name = 'IsOne');
    Assert(Section.Constants[1].TypeText = 'Boolean');
    Assert(Section.Constants[1].Line = 2);
    Assert(Section.Constants[2].Name = 'Below');
    Assert(Section.Constants[2].TypeText = 'Integer');

    { A value with an unresolved operand or token has no display type: an
      unknown name, a predefined script constant (the model only knows the
      constants the section itself declares), a forward-referenced or
      self-referencing name, an operand mix no operator allows, a real
      operand of an integer-only operator, or a set literal }
    Section.Parse([
      'const',
      '  MyUnknown = SomethingUndefined;',
      '  MyPredefined = MaxInt;',
      '  MyForward = MyLater + 1;',
      '  MySelf = MySelf + 1;',
      '  MyMix = ''a'' + 5;',
      '  MyBooleanMix = True + 1;',
      '  MyRealMod = 1.5 mod 2;',
      '  MySet = [1];',
      '  MyLater = 1;']);
    Assert(Section.ConstantCount = 9);
    for var I := 0 to 7 do
      Assert(Section.Constants[I].TypeText = '');
    Assert(Section.Constants[8].Name = 'MyLater');
    Assert(Section.Constants[8].TypeText = 'Integer');

    { Every const block is top-level, also between a routine's header and
      its body: ROPS has no local const blocks }
    Section.Parse([
      'const',           { 0 }
      '  A = 1;',        { 1 }
      'procedure P;',    { 2 }
      'const',           { 3 }
      '  B = 2;',        { 4 }
      'begin',           { 5 }
      'end;',            { 6 }
      'const',           { 7 }
      '  C = ''x'';']);  { 8 }
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].BodyFirstLine = 5);
    Assert(Section.ConstantCount = 3);
    Assert(Section.Constants[0].Name = 'A');
    Assert(Section.Constants[0].TypeText = 'Integer');
    Assert(Section.Constants[0].Line = 1);
    Assert(Section.Constants[1].Name = 'B');
    Assert(Section.Constants[1].TypeText = 'Integer');
    Assert(Section.Constants[1].Line = 4);
    Assert(Section.Constants[2].Name = 'C');
    Assert(Section.Constants[2].TypeText = 'String');
    Assert(Section.Constants[2].Line = 8);

    { An unfinished forward or external decoration does not swallow a
      following block }
    Section.Parse([
      'procedure P; forward',  { 0 }
      'const',                 { 1 }
      '  A = 1;']);            { 2 }
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].BodilessType = btForward);
    Assert(Section.ConstantCount = 1);
    Assert(Section.Constants[0].Name = 'A');
    Assert(Section.Constants[0].Line = 2);

    { A value still being typed is cut by the next declaration }
    Section.Parse([
      'const',      { 0 }
      '  A =',      { 1 }
      '  B = 2;']); { 2 }
    Assert(Section.ConstantCount = 2);
    Assert(Section.Constants[0].Name = 'A');
    Assert(Section.Constants[0].TypeText = '');
    Assert(Section.Constants[0].Line = 1);
    Assert(Section.Constants[1].Name = 'B');
    Assert(Section.Constants[1].TypeText = 'Integer');
    Assert(Section.Constants[1].Line = 2);

    { And by a routine header, still inferring from what is there }
    Section.Parse([
      'const',         { 0 }
      '  A = 5',       { 1 }
      'procedure P;',  { 2 }
      'begin',         { 3 }
      'end;']);        { 4 }
    Assert(Section.ConstantCount = 1);
    Assert(Section.Constants[0].TypeText = 'Integer');
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].Name = 'P');
    Assert(Section.Routines[0].FirstLine = 2);

    { And by another declaration block }
    Section.Parse([
      'const',           { 0 }
      '  A =',           { 1 }
      'var',             { 2 }
      '  X: Integer;']); { 3 }
    Assert(Section.ConstantCount = 1);
    Assert(Section.Constants[0].TypeText = '');
    Assert(Section.GlobalVariableCount = 1);
    Assert(Section.GlobalVariables[0].Name = 'X');

    { A name alone is not yet a declaration }
    Section.Parse(['const', '  X']);
    Assert(Section.ConstantCount = 0);

    { A malformed declaration does not hide the finished ones below it }
    Section.Parse([
      'const',       { 0 }
      '  X;',        { 1 }
      '  ;',         { 2 }
      '  Y = 1;']);  { 3 }
    Assert(Section.ConstantCount = 1);
    Assert(Section.Constants[0].Name = 'Y');
    Assert(Section.Constants[0].TypeText = 'Integer');
    Assert(Section.Constants[0].Line = 3);

    { Every stage of typing a new constant above an existing one keeps it }
    const TypedSoFar: TArray<String> = ['  X', '  X =', '  X = 5', '  X = 5;'];
    for var I := 0 to High(TypedSoFar) do begin
      Section.Parse(['const', TypedSoFar[I], '  Y = 1;']);
      const Last = Section.Constants[Section.ConstantCount-1];
      Assert(Last.Name = 'Y');
      Assert(Last.TypeText = 'Integer');
      Assert(Last.Line = 2);
    end;
  finally
    Section.Free;
  end;
end;

procedure TestCodeSectionGlobalVariables;
begin
  const Section = TScriptModelCodeSection.Create;
  try
    { A var block yields one item per variable, with the type as written }
    Section.Parse([
      'var',                                                { 0 }
      '  MyInt: Integer;',                                  { 1 }
      '  MyList: array of String;',                         { 2 }
      '  MyProc: procedure(A: Integer; var B: String);']);  { 3 }
    Assert(Section.GlobalVariableCount = 3);
    Assert(Section.GlobalVariables[0].Name = 'MyInt');
    Assert(Section.GlobalVariables[0].TypeText = 'Integer');
    Assert(Section.GlobalVariables[0].Line = 1);
    Assert(Section.GlobalVariables[1].Name = 'MyList');
    Assert(Section.GlobalVariables[1].TypeText = 'array of String');
    Assert(Section.GlobalVariables[1].Line = 2);
    Assert(Section.GlobalVariables[2].Name = 'MyProc');
    Assert(Section.GlobalVariables[2].TypeText =
      'procedure(A: Integer; var B: String)');
    Assert(Section.GlobalVariables[2].Line = 3);

    { The next Parse replaces the previous items }
    Section.Parse(['var', '  A: Integer;']);
    Assert(Section.GlobalVariableCount = 1);
    Assert(Section.GlobalVariables[0].Name = 'A');
    Section.Parse([]);
    Assert(Section.GlobalVariableCount = 0);

    { A multi-name group splits into one item per name, each with the line
      its own name sits on }
    Section.Parse([
      'var',               { 0 }
      '  A, B: Integer;',  { 1 }
      '  C,',              { 2 }
      '    D: String;']);  { 3 }
    Assert(Section.GlobalVariableCount = 4);
    Assert(Section.GlobalVariables[0].Name = 'A');
    Assert(Section.GlobalVariables[0].TypeText = 'Integer');
    Assert(Section.GlobalVariables[0].Line = 1);
    Assert(Section.GlobalVariables[1].Name = 'B');
    Assert(Section.GlobalVariables[1].TypeText = 'Integer');
    Assert(Section.GlobalVariables[1].Line = 1);
    Assert(Section.GlobalVariables[2].Name = 'C');
    Assert(Section.GlobalVariables[2].TypeText = 'String');
    Assert(Section.GlobalVariables[2].Line = 2);
    Assert(Section.GlobalVariables[3].Name = 'D');
    Assert(Section.GlobalVariables[3].TypeText = 'String');
    Assert(Section.GlobalVariables[3].Line = 3);

    { A declaration spanning lines: the item has the name's line and the
      type's line breaks collapse to single spaces }
    Section.Parse([
      'var',              { 0 }
      '  X:',             { 1 }
      '    array of',     { 2 }
      '      Integer;']); { 3 }
    Assert(Section.GlobalVariableCount = 1);
    Assert(Section.GlobalVariables[0].Name = 'X');
    Assert(Section.GlobalVariables[0].TypeText = 'array of Integer');
    Assert(Section.GlobalVariables[0].Line = 1);

    { Blocks between and below routines are top-level ones, a routine's local
      block is not }
    Section.Parse([
      'var',              { 0 }
      '  A: Integer;',    { 1 }
      'procedure P;',     { 2 }
      'var',              { 3 }
      '  Local: String;', { 4 }
      'begin',            { 5 }
      'end;',             { 6 }
      'var',              { 7 }
      '  B: Boolean;']);  { 8 }
    Assert(Section.RoutineCount = 1);
    Assert(Section.GlobalVariableCount = 2);
    Assert(Section.GlobalVariables[0].Name = 'A');
    Assert(Section.GlobalVariables[0].Line = 1);
    Assert(Section.GlobalVariables[1].Name = 'B');
    Assert(Section.GlobalVariables[1].TypeText = 'Boolean');
    Assert(Section.GlobalVariables[1].Line = 8);
    Assert(Section.Routines[0].LocalCount = 1);
    Assert(Section.Routines[0].Locals[0].Name = 'Local');

    { A type still being typed is ended by the next group: a name followed
      by ':' or ',' in it is that group's first name }
    Section.Parse([
      'var',             { 0 }
      '  X:',            { 1 }
      '  Y: String;']);  { 2 }
    Assert(Section.GlobalVariableCount = 2);
    Assert(Section.GlobalVariables[0].Name = 'X');
    Assert(Section.GlobalVariables[0].TypeText = '');
    Assert(Section.GlobalVariables[0].Line = 1);
    Assert(Section.GlobalVariables[1].Name = 'Y');
    Assert(Section.GlobalVariables[1].TypeText = 'String');
    Assert(Section.GlobalVariables[1].Line = 2);
    Section.Parse([
      'var',                { 0 }
      '  X: Intege',        { 1 }
      '  Y, Z: String;']);  { 2 }
    Assert(Section.GlobalVariableCount = 3);
    Assert(Section.GlobalVariables[0].Name = 'X');
    Assert(Section.GlobalVariables[0].TypeText = 'Intege');
    Assert(Section.GlobalVariables[1].Name = 'Y');
    Assert(Section.GlobalVariables[1].TypeText = 'String');
    Assert(Section.GlobalVariables[2].Name = 'Z');
    Assert(Section.GlobalVariables[2].TypeText = 'String');

    { A name alone is kept with no type yet }
    Section.Parse(['var', '  X']);
    Assert(Section.GlobalVariableCount = 1);
    Assert(Section.GlobalVariables[0].Name = 'X');
    Assert(Section.GlobalVariables[0].TypeText = '');

    { A block above a routine keeps the routine, cutting an unterminated
      type }
    Section.Parse([
      'var',           { 0 }
      '  X: TColor',   { 1 }
      'procedure P;',  { 2 }
      'begin',         { 3 }
      'end;']);        { 4 }
    Assert(Section.GlobalVariableCount = 1);
    Assert(Section.GlobalVariables[0].TypeText = 'TColor');
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].Name = 'P');
    Assert(Section.Routines[0].FirstLine = 2);
    Assert(Section.Routines[0].BodyFirstLine = 3);

    { Another declaration block also ends it }
    Section.Parse([
      'var',            { 0 }
      '  X: Integer;',  { 1 }
      'const',          { 2 }
      '  A = 1;']);     { 3 }
    Assert(Section.GlobalVariableCount = 1);
    Assert(Section.ConstantCount = 1);

    { An unfinished external decoration does not swallow a following block }
    Section.Parse([
      'function GetX: Integer; external ''x@y.dll''',  { 0 }
      'var',                                           { 1 }
      '  G: Integer;']);                               { 2 }
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].BodilessType = btExternal);
    Assert(Section.GlobalVariableCount = 1);
    Assert(Section.GlobalVariables[0].Name = 'G');
    Assert(Section.GlobalVariables[0].Line = 2);

    { An unclosed '[' does not block the next-group cut or a block keyword:
      ':', ';' and block keywords are never legal inside brackets }
    Section.Parse([
      'var',               { 0 }
      '  X: array[0..5',   { 1 }
      '  Y: Integer;']);   { 2 }
    Assert(Section.GlobalVariableCount = 2);
    Assert(Section.GlobalVariables[0].Name = 'X');
    Assert(Section.GlobalVariables[0].TypeText = 'array[0..5');
    Assert(Section.GlobalVariables[1].Name = 'Y');
    Assert(Section.GlobalVariables[1].TypeText = 'Integer');
    Assert(Section.GlobalVariables[1].Line = 2);
    Section.Parse([
      'var',               { 0 }
      '  X: array[0..5',   { 1 }
      'const',             { 2 }
      '  A = 1;']);        { 3 }
    Assert(Section.GlobalVariableCount = 1);
    Assert(Section.GlobalVariables[0].TypeText = 'array[0..5');
    Assert(Section.ConstantCount = 1);
    Assert(Section.Constants[0].Name = 'A');

    { A ',' inside brackets is an array bound list, not a next group }
    Section.Parse([
      'var',
      '  X: array[A, B] of Integer;',
      '  Y: String;']);
    Assert(Section.GlobalVariableCount = 2);
    Assert(Section.GlobalVariables[0].Name = 'X');
    Assert(Section.GlobalVariables[0].TypeText = 'array[A, B] of Integer');
    Assert(Section.GlobalVariables[1].Name = 'Y');

    { Inside an open '(' nothing distinguishes a next group from a parameter
      list, so it is swallowed; a routine below still survives }
    Section.Parse([
      'var',              { 0 }
      '  X: procedure(',  { 1 }
      '  Y: Integer;',    { 2 }
      'procedure P;',     { 3 }
      'begin',            { 4 }
      'end;']);           { 5 }
    Assert(Section.GlobalVariableCount = 1);
    Assert(Section.GlobalVariables[0].Name = 'X');
    Assert(Section.GlobalVariables[0].TypeText = 'procedure( Y: Integer;');
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].Name = 'P');
    Assert(Section.Routines[0].FirstLine = 3);

    { A malformed declaration does not hide the finished ones below it }
    Section.Parse([
      'var',             { 0 }
      '  Integer;',      { 1 }
      '  ;',             { 2 }
      '  Y: String;']);  { 3 }
    Assert(Section.GlobalVariableCount = 2);
    Assert(Section.GlobalVariables[0].Name = 'Integer');
    Assert(Section.GlobalVariables[0].TypeText = '');
    Assert(Section.GlobalVariables[1].Name = 'Y');
    Assert(Section.GlobalVariables[1].TypeText = 'String');
    Assert(Section.GlobalVariables[1].Line = 3);

    { Every stage of typing a new variable above an existing one keeps it }
    const TypedSoFar: TArray<String> = [
      '  X', '  X,', '  X:', '  X: array', '  X: array of', '  X: Intege',
      '  X: Integer', '  X: Integer;'];
    for var I := 0 to High(TypedSoFar) do begin
      Section.Parse(['var', TypedSoFar[I], '  Y: String;']);
      Assert(Section.GlobalVariableCount = 2);
      Assert(Section.GlobalVariables[0].Name = 'X');
      const Last = Section.GlobalVariables[1];
      Assert(Last.Name = 'Y');
      Assert(Last.TypeText = 'String');
      Assert(Last.Line = 2);
    end;
  finally
    Section.Free;
  end;
end;

procedure TestCodeSectionInterfaceMethods;
begin
  const Section = TScriptModelCodeSection.Create;
  try
    { Every method of an interface definition, in source order, with its name,
      kind, result type, prototype, declaring type and line. The parent
      interface and the GUID string are tolerated, and trailing decoration
      such as 'safecall' stays out of the prototype. }
    Section.Parse([
      'type',                                                                  { 0 }
      '  IPersistFile = interface(IPersist)',                                  { 1 }
      '    ''{0000010B-0000-0000-C000-000000000046}''',                        { 2 }
      '    procedure Save(pszFileName: String; fRemember: BOOL); safecall;',   { 3 }
      '    function GetCurFile: String; safecall;',                            { 4 }
      '  end;']);                                                              { 5 }
    Assert(Section.TypeCount = 1);
    Assert(Section.Types[0].Name = 'IPersistFile');
    Assert(Section.Types[0].TypeText = 'interface');
    Assert(Section.RoutineCount = 0);
    Assert(Section.InterfaceMethodCount = 2);
    Assert(Section.InterfaceMethods[0].Name = 'Save');
    Assert(Section.InterfaceMethods[0].Kind = rkProcedure);
    Assert(Section.InterfaceMethods[0].DeclarationTypeIndex = 0);
    Assert(Section.InterfaceMethods[0].ResultTypeText = '');
    Assert(Section.InterfaceMethods[0].Prototype =
      'procedure Save(pszFileName: String; fRemember: BOOL);');
    Assert(Section.InterfaceMethods[0].Line = 3);
    Assert(Section.InterfaceMethods[1].Name = 'GetCurFile');
    Assert(Section.InterfaceMethods[1].Kind = rkFunction);
    Assert(Section.InterfaceMethods[1].DeclarationTypeIndex = 0);
    Assert(Section.InterfaceMethods[1].ResultTypeText = 'String');
    Assert(Section.InterfaceMethods[1].Prototype = 'function GetCurFile: String;');
    Assert(Section.InterfaceMethods[1].Line = 4);

    { The next Parse replaces the previous items }
    Section.Parse(['type', '  IOnly = interface', '    procedure M;', '  end;']);
    Assert(Section.InterfaceMethodCount = 1);
    Assert(Section.InterfaceMethods[0].Name = 'M');
    Section.Parse([]);
    Assert(Section.InterfaceMethodCount = 0);

    { A method header spanning lines: Line is the keyword's line and the
      prototype's line breaks collapse to single spaces }
    Section.Parse([
      'type',                                                     { 0 }
      '  IShellLinkW = interface(IUnknown)',                      { 1 }
      '    procedure GetIconLocation(pszIconPath: String;',       { 2 }
      '      cchIconPath: Integer); safecall;',                   { 3 }
      '  end;']);                                                 { 4 }
    Assert(Section.InterfaceMethodCount = 1);
    Assert(Section.InterfaceMethods[0].Name = 'GetIconLocation');
    Assert(Section.InterfaceMethods[0].Prototype =
      'procedure GetIconLocation(pszIconPath: String; cchIconPath: Integer);');
    Assert(Section.InterfaceMethods[0].Line = 2);

    { Several interfaces in one block keep their methods apart, and the list
      is flat across them }
    Section.Parse([
      'type',                   { 0 }
      '  IFirst = interface',   { 1 }
      '    procedure A;',       { 2 }
      '  end;',                 { 3 }
      '  ISecond = interface',  { 4 }
      '    procedure B;',       { 5 }
      '    function C: Integer;', { 6 }
      '  end;']);               { 7 }
    Assert(Section.TypeCount = 2);
    Assert(Section.InterfaceMethodCount = 3);
    Assert(Section.InterfaceMethods[0].Name = 'A');
    Assert(Section.InterfaceMethods[0].DeclarationTypeIndex = 0);
    Assert(Section.InterfaceMethods[0].Line = 2);
    Assert(Section.InterfaceMethods[1].Name = 'B');
    Assert(Section.InterfaceMethods[1].DeclarationTypeIndex = 1);
    Assert(Section.InterfaceMethods[1].Line = 5);
    Assert(Section.InterfaceMethods[2].Name = 'C');
    Assert(Section.InterfaceMethods[2].DeclarationTypeIndex = 1);
    Assert(Section.InterfaceMethods[2].ResultTypeText = 'Integer');
    Assert(Section.InterfaceMethods[2].Line = 6);

    { A record contributes nothing, not even its procedural fields }
    Section.Parse([
      'type',
      '  TRec = record',
      '    A: Integer;',
      '    P: procedure(X: Integer);',
      '  end;']);
    Assert(Section.TypeCount = 1);
    Assert(Section.RoutineCount = 0);
    Assert(Section.InterfaceMethodCount = 0);

    { An unterminated method header is cut by the next method's keyword,
      keeping what is there }
    Section.Parse([
      'type',                { 0 }
      '  IFoo = interface',  { 1 }
      '    procedure M1',    { 2 }
      '    procedure M2;',   { 3 }
      '  end;']);            { 4 }
    Assert(Section.InterfaceMethodCount = 2);
    Assert(Section.InterfaceMethods[0].Name = 'M1');
    Assert(Section.InterfaceMethods[0].Prototype = 'procedure M1');
    Assert(Section.InterfaceMethods[1].Name = 'M2');
    Assert(Section.InterfaceMethods[1].Prototype = 'procedure M2;');

    { It is also cut by the interface's own 'end', so a method being typed
      does not swallow the routines below }
    Section.Parse([
      'type',                { 0 }
      '  IFoo = interface',  { 1 }
      '    procedure M1;',   { 2 }
      '    function GetX',   { 3 }
      '  end;',              { 4 }
      'procedure P;',        { 5 }
      'begin',               { 6 }
      'end;']);              { 7 }
    Assert(Section.TypeCount = 1);
    Assert(Section.InterfaceMethodCount = 2);
    Assert(Section.InterfaceMethods[1].Name = 'GetX');
    Assert(Section.InterfaceMethods[1].Prototype = 'function GetX');
    Assert(Section.InterfaceMethods[1].ResultTypeText = '');
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].Name = 'P');
    Assert(Section.Routines[0].FirstLine = 5);

    { An interface left unterminated does take the routines below it as its
      methods: they are indistinguishable from method declarations }
    Section.Parse([
      'type',                { 0 }
      '  IFoo = interface',  { 1 }
      '    procedure M1;',   { 2 }
      '',                    { 3 }
      'procedure P;',        { 4 }
      'begin',               { 5 }
      'end;']);              { 6 }
    Assert(Section.TypeCount = 1);
    Assert(Section.RoutineCount = 0);
    Assert(Section.InterfaceMethodCount = 2);
    Assert(Section.InterfaceMethods[0].Name = 'M1');
    Assert(Section.InterfaceMethods[1].Name = 'P');

    { An interface nested in an array type is anonymous, like in ROPS, so its
      methods point to the array type instead }
    Section.Parse([
      'type',                                             { 0 }
      '  TItems = array of interface(IUnknown)',          { 1 }
      '    ''{148BD527-A2AB-11CE-B11F-00AA00530503}''',   { 2 }
      '    procedure Foo;',                               { 3 }
      '  end;']);                                         { 4 }
    Assert(Section.TypeCount = 1);
    Assert(Section.Types[0].Name = 'TItems');
    Assert(Section.Types[0].TypeText = 'array');
    Assert(Section.RoutineCount = 0);
    Assert(Section.InterfaceMethodCount = 1);
    Assert(Section.InterfaceMethods[0].Name = 'Foo');
    Assert(Section.InterfaceMethods[0].DeclarationTypeIndex = 0);
    Assert(Section.InterfaceMethods[0].Line = 3);

    { The same for one nested in a record type }
    Section.Parse([
      'type',                                             { 0 }
      '  TRec = record',                                  { 1 }
      '    F: interface(IUnknown)',                       { 2 }
      '      ''{148BD527-A2AB-11CE-B11F-00AA00530503}''', { 3 }
      '      procedure Foo;',                             { 4 }
      '    end;',                                         { 5 }
      '  end;']);                                         { 6 }
    Assert(Section.TypeCount = 1);
    Assert(Section.Types[0].TypeText = 'record');
    Assert(Section.RoutineCount = 0);
    Assert(Section.InterfaceMethodCount = 1);
    Assert(Section.InterfaceMethods[0].Name = 'Foo');
    Assert(Section.InterfaceMethods[0].DeclarationTypeIndex = 0);

    { A type without methods before it still takes an index }
    Section.Parse([
      'type',                              { 0 }
      '  TRecA = record A: Integer; end;', { 1 }
      '  TRecB = record',                  { 2 }
      '    F: interface',                  { 3 }
      '      procedure Foo;',              { 4 }
      '    end;',                          { 5 }
      '  end;']);                          { 6 }
    Assert(Section.TypeCount = 2);
    Assert(Section.InterfaceMethodCount = 1);
    Assert(Section.InterfaceMethods[0].Name = 'Foo');
    Assert(Section.InterfaceMethods[0].DeclarationTypeIndex = 1);

    { Two of them stay apart through their types, a state ROPS itself rejects
      as a duplicate identifier but the model still keeps apart while typing }
    Section.Parse([
      'type',                           { 0 }
      '  TFirst = array of interface',  { 1 }
      '    procedure Foo;',             { 2 }
      '  end;',                         { 3 }
      '  TSecond = array of interface', { 4 }
      '    procedure Bar;',             { 5 }
      '  end;']);                       { 6 }
    Assert(Section.TypeCount = 2);
    Assert(Section.InterfaceMethodCount = 2);
    Assert(Section.InterfaceMethods[0].DeclarationTypeIndex = 0);
    Assert(Section.InterfaceMethods[1].DeclarationTypeIndex = 1);

    { A named and an anonymous one in a single block }
    Section.Parse([
      'type',                           { 0 }
      '  IFoo = interface',             { 1 }
      '    procedure Foo;',             { 2 }
      '  end;',                         { 3 }
      '  TItems = array of interface',  { 4 }
      '    procedure Bar;',             { 5 }
      '  end;']);                       { 6 }
    Assert(Section.TypeCount = 2);
    Assert(Section.InterfaceMethodCount = 2);
    Assert(Section.InterfaceMethods[0].DeclarationTypeIndex = 0);
    Assert(Section.InterfaceMethods[1].DeclarationTypeIndex = 1);

    { The indexes also run across type blocks }
    Section.Parse([
      'type',                           { 0 }
      '  TFirst = array of interface',  { 1 }
      '    procedure Foo;',             { 2 }
      '  end;',                         { 3 }
      'type',                           { 4 }
      '  TSecond = array of interface', { 5 }
      '    procedure Bar;',             { 6 }
      '  end;']);                       { 7 }
    Assert(Section.InterfaceMethodCount = 2);
    Assert(Section.InterfaceMethods[0].DeclarationTypeIndex = 0);
    Assert(Section.InterfaceMethods[1].DeclarationTypeIndex = 1);

    { The interface's 'end' also cuts a method whose parameter list is left
      open, so the routines below it stay top-level }
    Section.Parse([
      'type',                { 0 }
      '  IFoo = interface',  { 1 }
      '    procedure M1;',   { 2 }
      '    procedure M2(',   { 3 }
      '  end;',              { 4 }
      'procedure P;',        { 5 }
      'begin',               { 6 }
      'end;']);              { 7 }
    Assert(Section.TypeCount = 1);
    Assert(Section.InterfaceMethodCount = 2);
    Assert(Section.InterfaceMethods[1].Name = 'M2');
    Assert(Section.InterfaceMethods[1].Prototype = 'procedure M2(');
    Assert(Section.InterfaceMethods[1].DeclarationTypeIndex = 0);
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].Name = 'P');
    Assert(Section.Routines[0].FirstLine = 5);
  finally
    Section.Free;
  end;
end;

procedure TestCodeSectionParameters;
begin
  const Section = TScriptModelCodeSection.Create;
  try
    { Parameters of a routine, in source order: modifiers stay out of name
      and type, a multi-name group splits into one parameter per name, and
      an untyped 'var' parameter gets an empty type }
    Section.Parse([
      'procedure Foo(const A: String; var B, C: Integer; var D; out E: Boolean);', { 0 }
      'begin',                                                                     { 1 }
      'end;']);                                                                    { 2 }
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].ParameterCount = 5);
    Assert(Section.Routines[0].Parameters[0].Name = 'A');
    Assert(Section.Routines[0].Parameters[0].TypeText = 'String');
    Assert(Section.Routines[0].Parameters[0].Line = 0);
    Assert(Section.Routines[0].Parameters[1].Name = 'B');
    Assert(Section.Routines[0].Parameters[1].TypeText = 'Integer');
    Assert(Section.Routines[0].Parameters[2].Name = 'C');
    Assert(Section.Routines[0].Parameters[2].TypeText = 'Integer');
    Assert(Section.Routines[0].Parameters[3].Name = 'D');
    Assert(Section.Routines[0].Parameters[3].TypeText = '');
    Assert(Section.Routines[0].Parameters[4].Name = 'E');
    Assert(Section.Routines[0].Parameters[4].TypeText = 'Boolean');

    { A parameterless routine, without and with parentheses }
    Section.Parse([
      'procedure NoParams;',              { 0 }
      'begin',                            { 1 }
      'end;',                             { 2 }
      'function EmptyParams(): Boolean;', { 3 }
      'begin',                            { 4 }
      'end;']);                           { 5 }
    Assert(Section.RoutineCount = 2);
    Assert(Section.Routines[0].ParameterCount = 0);
    Assert(Section.Routines[1].ParameterCount = 0);
    Assert(Section.Routines[1].ResultTypeText = 'Boolean');

    { A header spanning physical lines: each parameter reports the line its
      name sits on }
    Section.Parse([
      'function Bar(const A: String;', { 0 }
      '  var B: Integer): Boolean;',   { 1 }
      'begin',                         { 2 }
      'end;']);                        { 3 }
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].ParameterCount = 2);
    Assert(Section.Routines[0].Parameters[0].Name = 'A');
    Assert(Section.Routines[0].Parameters[0].Line = 0);
    Assert(Section.Routines[0].Parameters[1].Name = 'B');
    Assert(Section.Routines[0].Parameters[1].Line = 1);

    { A procedural-type parameter: its own parameters stay inside its type
      instead of joining the routine's }
    Section.Parse([
      'procedure Callback(Handler: procedure(A: Integer); const B: String);', { 0 }
      'begin',                                                                { 1 }
      'end;']);                                                               { 2 }
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].ParameterCount = 2);
    Assert(Section.Routines[0].Parameters[0].Name = 'Handler');
    Assert(Section.Routines[0].Parameters[0].TypeText = 'procedure(A: Integer)');
    Assert(Section.Routines[0].Parameters[1].Name = 'B');
    Assert(Section.Routines[0].Parameters[1].TypeText = 'String');

    { A procedural result type keeps its own parameters out of the routine's:
      the header's own list can only come before the result-type colon }
    Section.Parse([
      'function GetHandler: function(A, B: Integer): Boolean;', { 0 }
      'begin',                                                  { 1 }
      'end;']);                                                 { 2 }
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].ParameterCount = 0);

    { The same with a parameter list of its own before it }
    Section.Parse([
      'function Mixed(N: Integer): function(A, B: Integer): Boolean;', { 0 }
      'begin',                                                         { 1 }
      'end;']);                                                        { 2 }
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].ParameterCount = 1);
    Assert(Section.Routines[0].Parameters[0].Name = 'N');
    Assert(Section.Routines[0].Parameters[0].TypeText = 'Integer');

    { A parameter list left open takes no parameters: it could have swallowed
      the text below the header. The routine below it is still kept. }
    Section.Parse([
      'procedure Typing(A: Integer; B', { 0 }
      'procedure P;',                   { 1 }
      'begin',                          { 2 }
      'end;']);                         { 3 }
    Assert(Section.RoutineCount = 2);
    Assert(Section.Routines[0].ParameterCount = 0);
    Assert(Section.Routines[1].Name = 'P');

    { The same at the end of the section }
    Section.Parse(['procedure Typing2(A: Integer']);
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].ParameterCount = 0);

    { A 'var' block below an open list is shaped like a parameter group, so
      keeping the parameters found would take its variables for parameters }
    Section.Parse([
      'function InitializeSetup(: Boolean;', { 0 }
      'var',                                 { 1 }
      '  X: Integer;',                       { 2 }
      'begin',                               { 3 }
      'end;']);                              { 4 }
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].Name = 'InitializeSetup');
    Assert(Section.Routines[0].ParameterCount = 0);

    { A closed list does keep its parameters, also when the header itself is
      left unterminated }
    Section.Parse([
      'procedure Closed(A: Integer)', { 0 }
      'begin',                        { 1 }
      'end;']);                       { 2 }
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].ParameterCount = 1);
    Assert(Section.Routines[0].Parameters[0].Name = 'A');
    Assert(Section.Routines[0].Parameters[0].TypeText = 'Integer');

    { Parameters of interface methods, following the same rules, with a
      parameterless method written both without and with parentheses }
    Section.Parse([
      'type',                                       { 0 }
      '  IFoo = interface',                         { 1 }
      '    procedure M1(const A: String; var B);',  { 2 }
      '    function M2(X, Y: Integer): Boolean;',   { 3 }
      '    procedure M3;',                          { 4 }
      '    procedure M4();',                        { 5 }
      '  end;']);                                   { 6 }
    Assert(Section.InterfaceMethodCount = 4);
    Assert(Section.InterfaceMethods[0].ParameterCount = 2);
    Assert(Section.InterfaceMethods[0].Parameters[0].Name = 'A');
    Assert(Section.InterfaceMethods[0].Parameters[0].TypeText = 'String');
    Assert(Section.InterfaceMethods[0].Parameters[0].Line = 2);
    Assert(Section.InterfaceMethods[0].Parameters[1].Name = 'B');
    Assert(Section.InterfaceMethods[0].Parameters[1].TypeText = '');
    Assert(Section.InterfaceMethods[1].ParameterCount = 2);
    Assert(Section.InterfaceMethods[1].Parameters[0].Name = 'X');
    Assert(Section.InterfaceMethods[1].Parameters[0].TypeText = 'Integer');
    Assert(Section.InterfaceMethods[1].Parameters[1].Name = 'Y');
    Assert(Section.InterfaceMethods[1].Parameters[1].TypeText = 'Integer');
    Assert(Section.InterfaceMethods[1].Parameters[1].Line = 3);
    Assert(Section.InterfaceMethods[2].ParameterCount = 0);
    Assert(Section.InterfaceMethods[3].ParameterCount = 0);

    { A method's procedural result type keeps its parameters out too }
    Section.Parse([
      'type',                                                     { 0 }
      '  IFoo = interface',                                       { 1 }
      '    function GetHandler: function(A: Integer): Boolean;',  { 2 }
      '  end;']);                                                 { 3 }
    Assert(Section.InterfaceMethodCount = 1);
    Assert(Section.InterfaceMethods[0].ParameterCount = 0);
  finally
    Section.Free;
  end;
end;

procedure TestCodeSectionLocals;
begin
  const Section = TScriptModelCodeSection.Create;
  try
    { 'var' blocks between a routine's header and its 'begin' are the
      routine's locals, following the global variable rules: one item per
      name with the type as written, multi-name groups split }
    Section.Parse([
      'procedure Foo;',    { 0 }
      'var',               { 1 }
      '  A: Integer;',     { 2 }
      '  B, C: String;',   { 3 }
      'begin',             { 4 }
      'end;']);            { 5 }
    Assert(Section.RoutineCount = 1);
    Assert(Section.GlobalVariableCount = 0);
    Assert(Section.Routines[0].LocalCount = 3);
    Assert(Section.Routines[0].Locals[0].Name = 'A');
    Assert(Section.Routines[0].Locals[0].TypeText = 'Integer');
    Assert(Section.Routines[0].Locals[0].Line = 2);
    Assert(Section.Routines[0].Locals[1].Name = 'B');
    Assert(Section.Routines[0].Locals[1].TypeText = 'String');
    Assert(Section.Routines[0].Locals[1].Line = 3);
    Assert(Section.Routines[0].Locals[2].Name = 'C');
    Assert(Section.Routines[0].Locals[2].TypeText = 'String');
    Assert(Section.Routines[0].Locals[2].Line = 3);
    Assert(Section.Routines[0].BodyFirstLine = 4);

    { The next Parse replaces the previous items }
    Section.Parse(['procedure Foo;', 'var', '  X: Integer;', 'begin', 'end;']);
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].LocalCount = 1);
    Section.Parse([]);
    Assert(Section.RoutineCount = 0);

    { Several blocks all fill the same routine's list, and a 'label' block
      between them is skipped }
    Section.Parse([
      'function Bar: Boolean;', { 0 }
      'var',                    { 1 }
      '  A: Integer;',          { 2 }
      'label',                  { 3 }
      '  Skip;',                { 4 }
      'var',                    { 5 }
      '  B: String;',           { 6 }
      'begin',                  { 7 }
      'end;']);                 { 8 }
    Assert(Section.RoutineCount = 1);
    Assert(Section.GlobalVariableCount = 0);
    Assert(Section.Routines[0].LocalCount = 2);
    Assert(Section.Routines[0].Locals[0].Name = 'A');
    Assert(Section.Routines[0].Locals[0].TypeText = 'Integer');
    Assert(Section.Routines[0].Locals[0].Line = 2);
    Assert(Section.Routines[0].Locals[1].Name = 'B');
    Assert(Section.Routines[0].Locals[1].TypeText = 'String');
    Assert(Section.Routines[0].Locals[1].Line = 6);
    Assert(Section.Routines[0].BodyFirstLine = 7);

    { Locals leak neither into the global list nor across routines }
    Section.Parse([
      'var',               { 0 }
      '  G1: Integer;',    { 1 }
      'procedure P1;',     { 2 }
      'var',               { 3 }
      '  L1: String;',     { 4 }
      'begin',             { 5 }
      'end;',              { 6 }
      'procedure P2;',     { 7 }
      'var',               { 8 }
      '  L2: Boolean;',    { 9 }
      'begin',             { 10 }
      'end;',              { 11 }
      'var',               { 12 }
      '  G2: Integer;']);  { 13 }
    Assert(Section.RoutineCount = 2);
    Assert(Section.GlobalVariableCount = 2);
    Assert(Section.GlobalVariables[0].Name = 'G1');
    Assert(Section.GlobalVariables[1].Name = 'G2');
    Assert(Section.Routines[0].LocalCount = 1);
    Assert(Section.Routines[0].Locals[0].Name = 'L1');
    Assert(Section.Routines[0].Locals[0].TypeText = 'String');
    Assert(Section.Routines[0].Locals[0].Line = 4);
    Assert(Section.Routines[1].LocalCount = 1);
    Assert(Section.Routines[1].Locals[0].Name = 'L2');
    Assert(Section.Routines[1].Locals[0].TypeText = 'Boolean');
    Assert(Section.Routines[1].Locals[0].Line = 9);

    { A block of a routine whose 'begin' is still missing keeps its locals }
    Section.Parse([
      'procedure Typing;',  { 0 }
      'var',                { 1 }
      '  A: Integer;']);    { 2 }
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].BodyFirstLine = -1);
    Assert(Section.Routines[0].LocalCount = 1);
    Assert(Section.Routines[0].Locals[0].Name = 'A');
    Assert(Section.GlobalVariableCount = 0);

    { A group still being typed does not swallow the routine below, which
      keeps its own empty list }
    Section.Parse([
      'procedure P;',   { 0 }
      'var',            { 1 }
      '  X: Intege',    { 2 }
      'procedure Q;',   { 3 }
      'begin',          { 4 }
      'end;']);         { 5 }
    Assert(Section.RoutineCount = 2);
    Assert(Section.Routines[0].LocalCount = 1);
    Assert(Section.Routines[0].Locals[0].Name = 'X');
    Assert(Section.Routines[0].Locals[0].TypeText = 'Intege');
    Assert(Section.Routines[1].Name = 'Q');
    Assert(Section.Routines[1].BodyFirstLine = 4);
    Assert(Section.Routines[1].LocalCount = 0);
    Assert(Section.GlobalVariableCount = 0);

    { A tokenizer error inside a local block resumes it after the resync,
      still into the same routine's locals (also see TestCodeSectionResync) }
    Section.Parse([
      'procedure P;',           { 0 }
      'var',                    { 1 }
      '  Bad: ''unterminated',  { 2 }
      '  Kept: Integer;',       { 3 }
      'begin',                  { 4 }
      'end;']);                 { 5 }
    Assert(Section.RoutineCount = 1);
    Assert(Section.GlobalVariableCount = 0);
    Assert(Section.Routines[0].LocalCount = 2);
    Assert(Section.Routines[0].Locals[0].Name = 'Bad');
    Assert(Section.Routines[0].Locals[0].TypeText = '');
    Assert(Section.Routines[0].Locals[0].Line = 2);
    Assert(Section.Routines[0].Locals[1].Name = 'Kept');
    Assert(Section.Routines[0].Locals[1].TypeText = 'Integer');
    Assert(Section.Routines[0].Locals[1].Line = 3);
    Assert(Section.Routines[0].BodyFirstLine = -1);
    Assert(Section.Routines[0].LastLine = 5);
  finally
    Section.Free;
  end;
end;

procedure TestCodeSectionRoutineAtLine;
begin
  const Section = TScriptModelCodeSection.Create;
  try
    Section.Parse([
      'var',                                                      { 0 }
      '  Global: Integer;',                                       { 1 }
      '',                                                         { 2 }
      'procedure Later(A: Integer); forward;',                    { 3 }
      '',                                                         { 4 }
      'function GetSysDir: String;',                              { 5 }
      '  external ''GetSystemDirectoryW@kernel32.dll stdcall'';', { 6 }
      '',                                                         { 7 }
      'function MyFunc(const A: String;',                         { 8 }
      '  const B: Integer): Boolean;',                            { 9 }
      'var',                                                      { 10 }
      '  L: Integer;',                                            { 11 }
      'begin',                                                    { 12 }
      '  L := 0;',                                                { 13 }
      '  Result := True;',                                        { 14 }
      'end;',                                                     { 15 }
      '',                                                         { 16 }
      'procedure Later(A: Integer);',                             { 17 }
      'begin',                                                    { 18 }
      'end;',                                                     { 19 }
      '']);                                                       { 20 }
    Assert(Section.RoutineCount = 4);

    var Routine: TCodeSectionRoutine;

    { Lines outside every span: the global var block, the gaps between
      routines, the trailing empty line, and lines outside the section }
    Assert(not Section.TryGetRoutine(0, Routine));
    Assert(Routine = nil);
    Assert(not Section.TryGetRoutine(1, Routine));
    Assert(not Section.TryGetRoutine(2, Routine));
    Assert(not Section.TryGetRoutine(4, Routine));
    Assert(not Section.TryGetRoutine(7, Routine));
    Assert(not Section.TryGetRoutine(16, Routine));
    Assert(not Section.TryGetRoutine(20, Routine));
    Assert(not Section.TryGetRoutine(-1, Routine));
    Assert(not Section.TryGetRoutine(21, Routine));

    { Bodiless routines: the span is the header plus a trailing directive }
    Assert(Section.TryGetRoutine(3, Routine));
    Assert(Routine = Section.Routines[0]);
    Assert(Section.TryGetRoutine(5, Routine));
    Assert(Routine = Section.Routines[1]);
    Assert(Section.TryGetRoutine(6, Routine)); { The directive's own line }
    Assert(Routine = Section.Routines[1]);

    { Inside the header, the local declarations, and the body }
    Assert(Section.TryGetRoutine(8, Routine));
    Assert(Routine = Section.Routines[2]);
    Assert(Section.TryGetRoutine(9, Routine));
    Assert(Routine = Section.Routines[2]);
    Assert(Section.TryGetRoutine(10, Routine));
    Assert(Routine = Section.Routines[2]);
    Assert(Section.TryGetRoutine(11, Routine));
    Assert(Routine = Section.Routines[2]);
    Assert(Section.TryGetRoutine(13, Routine));
    Assert(Routine = Section.Routines[2]);
    Assert(Section.TryGetRoutine(15, Routine));
    Assert(Routine = Section.Routines[2]);
    Assert(Section.TryGetRoutine(18, Routine));
    Assert(Routine = Section.Routines[3]);

    { Multiple routines on one physical line: the first one wins }
    Section.Parse([
      'procedure A;',
      'begin',
      'end; procedure B;',
      'begin',
      'end;']);
    Assert(Section.RoutineCount = 2);
    Assert(Section.Routines[0].LastLine = 2);
    Assert(Section.Routines[1].FirstLine = 2);
    Assert(Section.TryGetRoutine(2, Routine));
    Assert(Routine = Section.Routines[0]);
    Section.Parse(['procedure A; procedure B; begin end;']);
    Assert(Section.RoutineCount = 2);
    Assert(Section.TryGetRoutine(0, Routine));
    Assert(Routine = Section.Routines[0]);
    Assert(Section.TryGetRoutine(0, Routine, True)); { A has no 'begin' }
    Assert(Routine = Section.Routines[1]);

    { TryGetRoutine with AFromBodyOnly matches from the body's 'begin'
      onwards: not the header, not the local declarations, not a bodiless
      routine, and not the lines between routines. A body missing its 'end'
      matches to the end of the routine's span. }
    Section.Parse([
      'procedure Bodiless(A: Integer); forward;',   { 0 }
      '',                                           { 1 }
      'function Closed(const A: String): Boolean;', { 2 }
      'var',                                        { 3 }
      '  L: Integer;',                              { 4 }
      'begin',                                      { 5 }
      '  Result := True;',                          { 6 }
      'end;',                                       { 7 }
      '',                                           { 8 }
      'procedure Typing;',                          { 9 }
      'begin',                                      { 10 }
      '  X := 1;',                                  { 11 }
      '']);                                         { 12 }
    Assert(Section.RoutineCount = 3);
    Assert(not Section.TryGetRoutine(0, Routine, True)); { Bodiless }
    Assert(Routine = nil);
    Assert(not Section.TryGetRoutine(1, Routine, True)); { Between routines }
    Assert(not Section.TryGetRoutine(2, Routine, True)); { Header }
    Assert(not Section.TryGetRoutine(3, Routine, True)); { Local 'var' block }
    Assert(not Section.TryGetRoutine(4, Routine, True));
    Assert(Section.TryGetRoutine(5, Routine, True)); { The 'begin' line }
    Assert(Routine = Section.Routines[1]);
    Assert(Section.TryGetRoutine(6, Routine, True));
    Assert(Routine = Section.Routines[1]);
    Assert(Section.TryGetRoutine(7, Routine, True)); { The 'end;' line }
    Assert(Routine = Section.Routines[1]);
    Assert(not Section.TryGetRoutine(8, Routine, True)); { Between routines }
    Assert(not Section.TryGetRoutine(9, Routine, True)); { Header of an open body }
    Assert(Section.TryGetRoutine(10, Routine, True)); { The open body's 'begin' }
    Assert(Routine = Section.Routines[2]);
    Assert(Section.TryGetRoutine(11, Routine, True));
    Assert(Routine = Section.Routines[2]);
    Assert(Section.TryGetRoutine(12, Routine, True)); { To the span's end }
    Assert(Routine = Section.Routines[2]);
    Section.Parse(['procedure Foo;', 'begin var A: Integer;']);
    Assert(Section.TryGetRoutine(1, Routine, True)); { A cut on the 'begin' line itself still matches it }
    Assert(Routine = Section.Routines[0]);
  finally
    Section.Free;
  end;
end;

procedure TestCodeSectionResync;
begin
  const Section = TScriptModelCodeSection.Create;
  try
    { A tokenize error in the middle does not end the scan: it resyncs at the
      next physical line, keeping the declarations before and after the error
      with their line numbers }
    Section.Parse([
      'procedure Before;',    { 0 }
      'begin',                { 1 }
      'end;',                 { 2 }
      'S := ''unterminated',  { 3 }
      'procedure After;',     { 4 }
      'begin',                { 5 }
      'end;']);               { 6 }
    Assert(Section.RoutineCount = 2);
    Assert(Section.Routines[0].Name = 'Before');
    Assert(Section.Routines[0].LastLine = 2);
    Assert(Section.Routines[1].Name = 'After');
    Assert(Section.Routines[1].FirstLine = 4);
    Assert(Section.Routines[1].Prototype = 'procedure After;');
    Assert(Section.Routines[1].BodyFirstLine = 5);
    Assert(Section.Routines[1].LastLine = 6);

    { An error inside a body: the routine keeps its 'begin' line and its
      span ends on the line before the next declaration found after the
      resync }
    Section.Parse([
      'procedure Typing;',      { 0 }
      'begin',                  { 1 }
      '  S := ''unterminated',  { 2 }
      'end;',                   { 3 }
      '',                       { 4 }
      'procedure After;',       { 5 }
      'begin',                  { 6 }
      'end;']);                 { 7 }
    Assert(Section.RoutineCount = 2);
    Assert(Section.Routines[0].Name = 'Typing');
    Assert(Section.Routines[0].BodyFirstLine = 1);
    Assert(Section.Routines[0].LastLine = 4);
    Assert(Section.Routines[1].Name = 'After');
    Assert(Section.Routines[1].FirstLine = 5);
    Assert(Section.Routines[1].LastLine = 7);
    var Routine: TCodeSectionRoutine;
    Assert(Section.TryGetRoutine(3, Routine)); { The leftover 'end;' line }
    Assert(Routine = Section.Routines[0]);
    Assert(Section.TryGetRoutine(6, Routine));
    Assert(Routine = Section.Routines[1]);

    { A type block found after the resync also ends the cut routine's span }
    Section.Parse([
      'procedure Typing;',      { 0 }
      'begin',                  { 1 }
      '  S := ''unterminated',  { 2 }
      'type',                   { 3 }
      '  TAfter = Integer;']);  { 4 }
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].Name = 'Typing');
    Assert(Section.Routines[0].BodyFirstLine = 1);
    Assert(Section.Routines[0].LastLine = 2);
    Assert(Section.TypeCount = 1);
    Assert(Section.Types[0].Name = 'TAfter');
    Assert(Section.Types[0].Line = 4);
    Assert(Section.TryGetRoutine(2, Routine));
    Assert(Routine = Section.Routines[0]);
    Assert(not Section.TryGetRoutine(3, Routine));
    Assert(not Section.TryGetRoutine(4, Routine));

    { 'const', 'var' and 'label' blocks also end the span, and 'const' and
      'var' blocks after it are top-level ones: the 'var' block's name counts
      as a global, while the 'const' block's name is not yet a declaration
      without its '=' }
    const BlockKeywords: TArray<String> = ['const', 'var', 'label'];
    for var BlockKeyword in BlockKeywords do begin
      Section.Parse([
        'procedure Typing;',      { 0 }
        'begin',                  { 1 }
        '  S := ''unterminated',  { 2 }
        BlockKeyword,             { 3 }
        '  X']);                  { 4 }
      Assert(Section.RoutineCount = 1);
      Assert(Section.Routines[0].LastLine = 2);
      Assert(not Section.TryGetRoutine(3, Routine));
      Assert(Section.ConstantCount = 0);
      if BlockKeyword = 'var' then
        Assert(Section.GlobalVariableCount = 1)
      else
        Assert(Section.GlobalVariableCount = 0);
    end;

    { But when the error hit before the routine's 'begin', the span stays
      open whatever block follows: the 'begin' may still follow it. A 'var'
      block can be the routine's own local block, so it is parsed into its
      locals, while a 'const' or 'type' block is always a top-level one. }
    Section.Parse([
      'procedure Typing;',     { 0 }
      'const',                 { 1 }
      '  C = ''unterminated',  { 2 }
      'var',                   { 3 }
      '  X: Integer;',         { 4 }
      'begin',                 { 5 }
      'end;']);                { 6 }
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].LastLine = 6);
    Assert(Section.ConstantCount = 1);
    Assert(Section.Constants[0].Name = 'C');
    Assert(Section.Constants[0].TypeText = '');
    Assert(Section.GlobalVariableCount = 0);
    Assert(Section.Routines[0].LocalCount = 1);
    Assert(Section.Routines[0].Locals[0].Name = 'X');
    Assert(Section.Routines[0].Locals[0].TypeText = 'Integer');
    Assert(Section.Routines[0].Locals[0].Line = 4);
    Assert(Section.TryGetRoutine(5, Routine));
    Assert(Routine = Section.Routines[0]);
    Section.Parse([
      'procedure Typing;',     { 0 }
      'const',                 { 1 }
      '  C = ''unterminated',  { 2 }
      'type',                  { 3 }
      '  T = Integer;']);      { 4 }
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].LastLine = 4);
    Assert(Section.ConstantCount = 1);
    Assert(Section.TypeCount = 1);
    Assert(Section.Types[0].Name = 'T');
    Assert(Section.Types[0].Line = 4);
    Assert(Section.TryGetRoutine(4, Routine));
    Assert(Routine = Section.Routines[0]);

    { A 'begin' met after the resync ends the search for it, so the span ends
      at the next block and that block's variables are global ones }
    Section.Parse([
      'procedure Typing;',     { 0 }
      'const',                 { 1 }
      '  C = ''unterminated',  { 2 }
      'begin',                 { 3 }
      'end;',                  { 4 }
      'var',                   { 5 }
      '  G: Integer;']);       { 6 }
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].LastLine = 4);
    Assert(Section.Routines[0].BodyFirstLine = -1);
    Assert(Section.ConstantCount = 1);
    Assert(Section.Constants[0].Name = 'C');
    Assert(Section.GlobalVariableCount = 1);
    Assert(Section.GlobalVariables[0].Name = 'G');
    Assert(Section.GlobalVariables[0].TypeText = 'Integer');
    Assert(Section.GlobalVariables[0].Line = 6);
    Assert(Section.TryGetRoutine(4, Routine));
    Assert(not Section.TryGetRoutine(6, Routine));

    { The same when the error cut the header itself, which keeps the text up
      to the cut }
    Section.Parse([
      'procedure P(const S: String = ''x);',  { 0 }
      'begin',                                { 1 }
      'end;',                                 { 2 }
      'var',                                  { 3 }
      '  G: Integer;']);                      { 4 }
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].Name = 'P');
    Assert(Section.Routines[0].Prototype = 'procedure P(const S: String =');
    Assert(Section.Routines[0].LastLine = 2);
    Assert(Section.Routines[0].BodyFirstLine = -1);
    Assert(Section.GlobalVariableCount = 1);
    Assert(Section.GlobalVariables[0].Name = 'G');
    Assert(Section.GlobalVariables[0].TypeText = 'Integer');
    Assert(Section.GlobalVariables[0].Line = 4);

    { A block cut by an error while the routine's body was being searched for
      is resumed too, so its finished declarations below survive.
      Known limitation: the 'begin' after it is no longer taken as the body. }
    Section.Parse([
      'procedure P;',              { 0 }
      'const',                     { 1 }
      '  Broken = ''unfinished',   { 2 }
      '  Finished = 1;',           { 3 }
      'begin',                     { 4 }
      'end;']);                    { 5 }
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].BodyFirstLine = -1);
    Assert(Section.ConstantCount = 2);
    Assert(Section.Constants[0].Name = 'Broken');
    Assert(Section.Constants[0].TypeText = '');
    Assert(Section.Constants[1].Name = 'Finished');
    Assert(Section.Constants[1].TypeText = 'Integer');
    Assert(Section.Constants[1].Line = 3);

    { With no declaration after the error the cut routine's span runs to the
      section's last line }
    Section.Parse([
      'procedure Typing;',      { 0 }
      'begin',                  { 1 }
      '  S := ''unterminated',  { 2 }
      '  X := 1;',              { 3 }
      '']);                     { 4 }
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].BodyFirstLine = 1);
    Assert(Section.Routines[0].LastLine = 4);

    { An unterminated header cut by an error keeps the text up to the cut }
    Section.Parse([
      'function Cut(A: Integer): Bool ''x', { 0 }
      'procedure After;',                   { 1 }
      'begin',                              { 2 }
      'end;']);                             { 3 }
    Assert(Section.RoutineCount = 2);
    Assert(Section.Routines[0].Name = 'Cut');
    Assert(Section.Routines[0].Prototype = 'function Cut(A: Integer): Bool');
    Assert(Section.Routines[0].ResultTypeText = 'Bool');
    Assert(Section.Routines[0].LastLine = 0);
    Assert(Section.Routines[1].Name = 'After');
    Assert(Section.Routines[1].LastLine = 3);

    { A bad char literal and an unrecognized byte resync the same way }
    Section.Parse([
      'X := #$;',       { 0 }
      'procedure P1;',  { 1 }
      'begin',          { 2 }
      'end;',           { 3 }
      'X := 1 ~ 2;',    { 4 }
      'procedure P2;',  { 5 }
      'begin',          { 6 }
      'end;']);         { 7 }
    Assert(Section.RoutineCount = 2);
    Assert(Section.Routines[0].Name = 'P1');
    Assert(Section.Routines[0].FirstLine = 1);
    Assert(Section.Routines[0].LastLine = 3);
    Assert(Section.Routines[1].Name = 'P2');
    Assert(Section.Routines[1].FirstLine = 5);
    Assert(Section.Routines[1].LastLine = 7);

    { The resync point can itself error right away (SetText calls Next) }
    Section.Parse([
      'S := ''one',    { 0 }
      '''two',         { 1 }
      'procedure P;',  { 2 }
      'begin',         { 3 }
      'end;']);        { 4 }
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].Name = 'P');
    Assert(Section.Routines[0].FirstLine = 2);

    { An error inside a spanned group resyncs after the group's first
      physical line (the joined line), keeping later line numbers }
    Section.Parse([
      'X := ~ 1 \',     { 0 }
      '  + 2;',         { 1 }
      'procedure P;',   { 2 }
      'begin',          { 3 }
      'end;']);         { 4 }
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].Name = 'P');
    Assert(Section.Routines[0].FirstLine = 2);
    Assert(Section.Routines[0].LastLine = 4);

    { An error inside a top-level block: the declarations found before it are
      kept, and the resync resumes the block, so the declarations after it
      are kept too }
    Section.Parse([
      'type',                   { 0 }
      '  TBefore = Integer;',   { 1 }
      '  TBad = ''x',           { 2 }
      '  TKept = String;',      { 3 }
      'type',                   { 4 }
      '  TFound = Boolean;',    { 5 }
      'procedure P;',           { 6 }
      'begin',                  { 7 }
      'end;']);                 { 8 }
    Assert(Section.TypeCount = 4);
    Assert(Section.Types[0].Name = 'TBefore');
    Assert(Section.Types[0].Line = 1);
    Assert(Section.Types[1].Name = 'TBad');
    Assert(Section.Types[1].TypeText = ''); { The error cut it before its kind }
    Assert(Section.Types[1].Line = 2);
    Assert(Section.Types[2].Name = 'TKept');
    Assert(Section.Types[2].TypeText = 'String');
    Assert(Section.Types[2].Line = 3);
    Assert(Section.Types[3].Name = 'TFound');
    Assert(Section.Types[3].Line = 5);
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].Name = 'P');
    Assert(Section.Routines[0].FirstLine = 6);

    { The same for const and var blocks, including a second error in the
      resumed block }
    Section.Parse([
      'const',                    { 0 }
      '  Typing = ''unfinished',  { 1 }
      '  Finished = 1;']);        { 2 }
    Assert(Section.ConstantCount = 2);
    Assert(Section.Constants[0].Name = 'Typing');
    Assert(Section.Constants[0].TypeText = '');
    Assert(Section.Constants[0].Line = 1);
    Assert(Section.Constants[1].Name = 'Finished');
    Assert(Section.Constants[1].TypeText = 'Integer');
    Assert(Section.Constants[1].Line = 2);
    Section.Parse([
      'const',          { 0 }
      '  A = ''one',    { 1 }
      '  B = ''two',    { 2 }
      '  C = 1;',       { 3 }
      'procedure P;',   { 4 }
      'begin',          { 5 }
      'end;']);         { 6 }
    Assert(Section.ConstantCount = 3);
    Assert(Section.Constants[0].Name = 'A');
    Assert(Section.Constants[1].Name = 'B');
    Assert(Section.Constants[2].Name = 'C');
    Assert(Section.Constants[2].TypeText = 'Integer');
    Assert(Section.Constants[2].Line = 3);
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].FirstLine = 4);
    Section.Parse([
      'var',                     { 0 }
      '  Typing: #$',            { 1 }
      '  Finished: Integer;']);  { 2 }
    Assert(Section.GlobalVariableCount = 2);
    Assert(Section.GlobalVariables[0].Name = 'Typing');
    Assert(Section.GlobalVariables[0].TypeText = '');
    Assert(Section.GlobalVariables[0].Line = 1);
    Assert(Section.GlobalVariables[1].Name = 'Finished');
    Assert(Section.GlobalVariables[1].TypeText = 'Integer');
    Assert(Section.GlobalVariables[1].Line = 2);

    { A resync landing on a value's continuation line keeps the block: what
      it lands on is skipped instead of ending it }
    Section.Parse([
      'const',                { 0 }
      '  A = ''unfinished',   { 1 }
      '    + 1;',             { 2 }
      '  B = 3;']);           { 3 }
    Assert(Section.ConstantCount = 2);
    Assert(Section.Constants[0].Name = 'A');
    Assert(Section.Constants[0].TypeText = '');
    Assert(Section.Constants[1].Name = 'B');
    Assert(Section.Constants[1].TypeText = 'Integer');
    Assert(Section.Constants[1].Line = 3);

    { An unterminated comment of either syntax is not resynced past: the
      tokenizer consumes it to the end of the text }
    Section.Parse([
      'procedure Before;',
      'begin',
      'end;',
      '{ unterminated',
      'procedure Hidden;',
      'begin',
      'end;']);
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].Name = 'Before');
    Section.Parse([
      'procedure Before;',
      'begin',
      'end;',
      '(* unterminated',
      'procedure Hidden;']);
    Assert(Section.RoutineCount = 1);
    Assert(Section.Routines[0].Name = 'Before');

    { Arbitrary garbage never raises, including an error on the last line
      with nothing left to resync to }
    Section.Parse(['%', '''', 'X := #$', '~~~']);
    Assert(Section.RoutineCount = 0);
    Assert(Section.TypeCount = 0);
    Section.Parse(['{']);
    Assert(Section.RoutineCount = 0);
    Section.Parse(['(*']);
    Assert(Section.RoutineCount = 0);
    Section.Parse(['''']);
    Assert(Section.RoutineCount = 0);
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
  TestKeyValueSectionMetadata;
  TestSectionMetadataTables;
  TestMetadataConsistency;
  TestSectionNames;
  TestScriptCategories;
  TestScriptBrowseFileTypes;
  TestEntryRules;
  TestEntryExcludeRules;
  TestKeyValueSection;
  TestKeyValueSectionFlags;
  TestEntrySpanning;
  TestEntryParameterIndex;
  TestEntryValuePosition;
  TestKeyValueSectionValuePosition;
  TestPrepareCodeSectionText;
  TestCodeSection;
  TestCodeSectionTypes;
  TestCodeSectionEnumerationValues;
  TestCodeSectionConstants;
  TestCodeSectionGlobalVariables;
  TestCodeSectionInterfaceMethods;
  TestCodeSectionParameters;
  TestCodeSectionLocals;
  TestCodeSectionRoutineAtLine;
  TestCodeSectionResync;
  {$IFDEF ISTESTTOOLPROJ}
  { ISTestTool only: under the ISIDE DEBUG self-test the initializers would
    run at unit initialization, so MainForm's own calls would find the lists
    already built, with the ISPPInstalled value passed here instead of the
    real one }
  InitializeFunctionDefinitions;
  InitializeWordLists(True);
  TestFunctionDefinitions;
  TestWordLists;
  {$ENDIF}
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
