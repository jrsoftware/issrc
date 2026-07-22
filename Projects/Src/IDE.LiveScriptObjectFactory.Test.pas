unit IDE.LiveScriptObjectFactory.Test;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Test unit for IDE.LiveScriptObjectFactory

  Does NOT run a self-test if DEBUG is defined, because these tests cannot run
  at unit initialization: they require a live and parented TScintEdit.

  So to get a self-test, call IDELiveScriptObjectFactoryRunTests. The
  TScintEdit's text, styler, change handler and read-only state are restored on
  exit.
}

interface

uses
  ScintEdit,
  IDE.ScintStylerInnoSetup;

procedure IDELiveScriptObjectFactoryRunTests(const AMemo: TScintEdit;
  const AStyler: TInnoSetupStyler);

implementation

uses
  System.SysUtils,
  IDE.ScriptModel, IDE.ScriptModel.Metadata, IDE.ScriptModel.Metadata.Extra,
  IDE.LiveScriptObjectFactory;

{$C+}

type
  TFactoryTestContext = class
  private
    FMemo: TScintEdit;
    FFactory: TLiveScriptObjectFactory;
    procedure MemoChange(Sender: TObject; const Info: TScintEditChangeInfo);
  public
    constructor Create(const AMemo: TScintEdit; const AStyler: TInnoSetupStyler;
      const AScriptLines: array of String);
    destructor Destroy; override;
    property Memo: TScintEdit read FMemo;
    property Factory: TLiveScriptObjectFactory read FFactory;
  end;

function JoinScriptLines(const ALines: array of String): String;
begin
  Result := '';
  for var I := 0 to High(ALines) do begin
    if I > 0 then
      Result := Result + #13#10;
    Result := Result + ALines[I];
  end;
end;

{ TFactoryTestContext }

constructor TFactoryTestContext.Create(const AMemo: TScintEdit;
  const AStyler: TInnoSetupStyler; const AScriptLines: array of String);
begin
  inherited Create;
  FMemo := AMemo;
  FMemo.OnChange := nil;
  FMemo.ReadOnly := False;
  FMemo.Styler := AStyler;
  FMemo.Lines.Text := JoinScriptLines(AScriptLines);
  Assert(FMemo.Lines.Count = Length(AScriptLines));
  FFactory := TLiveScriptObjectFactory.Create(AMemo, AStyler);
  FFactory.InvalidateIndex;
  FMemo.OnChange := MemoChange;
end;

destructor TFactoryTestContext.Destroy;
begin
  if FMemo <> nil then
    FMemo.OnChange := nil;
  FFactory.Free;
  inherited;
end;

procedure TFactoryTestContext.MemoChange(Sender: TObject;
  const Info: TScintEditChangeInfo);
begin
  FFactory.Change(Info);
end;

{ Section indexing: count, per-header fields, and that a header spanning two
  physical lines is one section, not two }
procedure TestSectionIndexing(const AMemo: TScintEdit;
  const AStyler: TInnoSetupStyler);

  procedure AssertSectionHeader(const ASectionHeader: TLiveScriptSectionHeader;
    const AExpectedStylerSection: TInnoSetupSection; const AExpectedLine: Integer;
    const AExpectedName: String);
  begin
    Assert(ASectionHeader.StylerSection = AExpectedStylerSection);
    Assert(ASectionHeader.Line = AExpectedLine);
    Assert(ASectionHeader.Name = AExpectedName);
  end;

begin
  const Context = TFactoryTestContext.Create(AMemo, AStyler, [
    '[Setup]',                              { 0 }
    'AppName=My App',                       { 1 }
    '[Files]',                              { 2 }
    'Source: "a.txt"; DestDir: "{app}"',    { 3 }
    '[Setup]',                              { 4 }
    'AppComments=hi',                       { 5 }
    '[Code]',                               { 6 }
    'procedure P; begin end;',              { 7 }
    '[_ThirdParty]',                        { 8 }
    'data',                                 { 9 }
    '[Bogus]',                              { 10 }
    'junk',                                 { 11 }
    '[Files] \',                            { 12, spanned header }
    '  Source: "b.txt"']);                  { 13, continuation of line 12 }
  try
    const Factory = Context.Factory;
    Assert(Factory.SectionCount = 7);
    AssertSectionHeader(Factory.SectionHeaders[0], scSetup, 0, 'Setup');
    AssertSectionHeader(Factory.SectionHeaders[1], scFiles, 2, 'Files');
    AssertSectionHeader(Factory.SectionHeaders[2], scSetup, 4, 'Setup'); { Duplicate kept }
    AssertSectionHeader(Factory.SectionHeaders[3], scCode, 6, 'Code');
    AssertSectionHeader(Factory.SectionHeaders[4], scThirdParty, 8, '_ThirdParty');
    AssertSectionHeader(Factory.SectionHeaders[5], scUnknown, 10, 'Bogus');
    { The spanned header at line 12 is a single section whose continuation
      (line 13) is not itself indexed }
    AssertSectionHeader(Factory.SectionHeaders[6], scFiles, 12, 'Files');
  finally
    Context.Free;
  end;
end;

{ TryGetSectionAtLine over a header line, content lines, a blank line inside
  a section, a [Code] line, a line before any section, and a line after a
  section end tag }
procedure TestTryGetSectionAtLine(const AMemo: TScintEdit;
  const AStyler: TInnoSetupStyler);

  procedure AssertAt(const AFactory: TLiveScriptObjectFactory; const ALine: Integer;
    const AExpectedIndex: Integer);
  begin
    var SectionIndex: Integer;
    Assert(AFactory.TryGetSectionAtLine(ALine, SectionIndex));
    Assert(SectionIndex = AExpectedIndex);
  end;

  procedure AssertNoneAt(const AFactory: TLiveScriptObjectFactory; const ALine: Integer);
  begin
    var SectionIndex: Integer;
    Assert(not AFactory.TryGetSectionAtLine(ALine, SectionIndex));
  end;

begin
  const Context = TFactoryTestContext.Create(AMemo, AStyler, [
    '; comment before any section',   { 0, belongs to no section }
    '[Setup]',                        { 1 }
    'AppName=x',                      { 2 }
    '',                               { 3, blank inside [Setup] }
    '[Code]',                         { 4 }
    'procedure Q; begin end;',        { 5 }
    '[/Code]',                        { 6, end tag closes the section }
    'orphan line',                    { 7, belongs to no section }
    '[Files]',                        { 8 }
    'Source: a']);                    { 9 }
  try
    const Factory = Context.Factory;
    Assert(Factory.SectionCount = 3); { [Setup] 1, [Code] 4, [Files] 8 }
    AssertNoneAt(Factory, 0);         { Before the first header }
    AssertAt(Factory, 1, 0);          { Header line }
    AssertAt(Factory, 2, 0);          { Content line }
    AssertAt(Factory, 3, 0);          { Blank line still inside [Setup] }
    AssertAt(Factory, 4, 1);          { [Code] header }
    AssertAt(Factory, 5, 1);          { [Code] content }
    AssertNoneAt(Factory, 7);         { After the [/Code] end tag }
    AssertAt(Factory, 8, 2);          { [Files] header }
    AssertAt(Factory, 9, 2);          { [Files] content }
  finally
    Context.Free;
  end;
end;

{ TryCreateParameterSectionEntry: every refusal reason plus the two accept
  paths (a real parameter line, and a blank line yielding an empty entry) }
procedure TestTryCreateParameterSectionEntry(const AMemo: TScintEdit;
  const AStyler: TInnoSetupStyler);

  procedure AssertRefusal(const AFactory: TLiveScriptObjectFactory; const ALine: Integer;
    const AExpectedReason: TRefusalReason);
  begin
    var Entry: TLiveScriptParameterSectionEntry;
    var Reason: TRefusalReason;
    Assert(not AFactory.TryCreateParameterSectionEntry(ALine, Entry, Reason));
    Assert(Entry = nil);
    Assert(Reason = AExpectedReason);
  end;

begin
  const Context = TFactoryTestContext.Create(AMemo, AStyler, [
    '; before any section',                 { 0 }
    '[Setup]',                              { 1 }
    'AppName=x',                            { 2 }
    '[Files]',                              { 3 }
    'Source: "a.txt"; DestDir: "{app}"',    { 4 }
    '; a comment in files',                 { 5 }
    '#define MyDef 1',                      { 6 }
    '',                                     { 7, blank inside [Files] }
    'Source: "b.txt"',                      { 8 }
    '[Code]',                              { 9 }
    'procedure R; begin end;',              { 10 }
    '[_Third]',                             { 11 }
    'tpdata']);                             { 12 }
  try
    const Factory = Context.Factory;
    { Refusals }
    AssertRefusal(Factory, -1, rrLineOutOfRange);
    AssertRefusal(Factory, 13, rrLineOutOfRange);
    AssertRefusal(Factory, 0, rrNotInsideSection);
    AssertRefusal(Factory, 2, rrDirectiveStyleSection);
    AssertRefusal(Factory, 5, rrComment);
    AssertRefusal(Factory, 6, rrISPPDirective);
    AssertRefusal(Factory, 10, rrInCodeSection);
    AssertRefusal(Factory, 12, rrUnrecognizedSection);

    { Accept: a real parameter line, parameters readable }
    var Entry: TLiveScriptParameterSectionEntry;
    var Reason: TRefusalReason;
    Assert(Factory.TryCreateParameterSectionEntry(4, Entry, Reason));
    try
      Assert(Entry.StylerSection = scFiles);
      var Value: String;
      Assert(Entry.Entry.TryGetValue('Source', Value) and (Value = 'a.txt'));
      Assert(Entry.Entry.TryGetValue('DestDir', Value) and (Value = '{app}'));
    finally
      Entry.Free;
    end;

    { Accept: a blank line inside a section yields an empty entry }
    Assert(Factory.TryCreateParameterSectionEntry(7, Entry, Reason));
    try
      Assert(Entry.Entry.Count = 0);
    finally
      Entry.Free;
    end;
  finally
    Context.Free;
  end;
end;

{ Editing an entry's value through the model writes back to the memo. Covers
  a single-line entry (with one undo restoring it), a spanned entry whose line
  breaks are preserved, and a blank-line entry that inserts itself above the
  blank }
procedure TestEntryRoundTrip(const AMemo: TScintEdit;
  const AStyler: TInnoSetupStyler);
begin
  { Single-line entry, edit a value, then a single undo restores the original }
  begin
    const Context = TFactoryTestContext.Create(AMemo, AStyler, [
      '[Files]',
      'Source: "a.txt"; DestDir: "{app}"',
      'Source: "keep.txt"']);
    try
      var Entry: TLiveScriptParameterSectionEntry;
      var Reason: TRefusalReason;
      Assert(Context.Factory.TryCreateParameterSectionEntry(1, Entry, Reason));
      try
        Entry.Entry.SetValue(1, '{tmp}');
        Assert(AMemo.Lines[1] = 'Source: "a.txt"; DestDir: "{tmp}"');
        Assert(AMemo.Lines[2] = 'Source: "keep.txt"'); { Neighbor untouched }
        AMemo.Undo; { Write-back is a single undo action }
        Assert(AMemo.Lines[1] = 'Source: "a.txt"; DestDir: "{app}"');
      finally
        Entry.Free;
      end;
    finally
      Context.Free;
    end;
  end;

  { Spanned entry: the author's line break is preserved on write-back }
  begin
    const Context = TFactoryTestContext.Create(AMemo, AStyler, [
      '[Files]',
      'Source: "a.txt"; \',
      '  DestDir: "{app}"; Flags: ignoreversion']);
    try
      var Entry: TLiveScriptParameterSectionEntry;
      var Reason: TRefusalReason;
      Assert(Context.Factory.TryCreateParameterSectionEntry(1, Entry, Reason));
      try
        Entry.Entry.SetValue(1, '{tmp}');
        Assert(AMemo.Lines.Count = 3);
        Assert(AMemo.Lines[1] = 'Source: "a.txt"; \');
        Assert(AMemo.Lines[2] = '  DestDir: "{tmp}"; Flags: ignoreversion');
      finally
        Entry.Free;
      end;
    finally
      Context.Free;
    end;
  end;

  { Blank-line entry: the new lines are inserted above the blank, keeping it as
    a separator }
  begin
    const Context = TFactoryTestContext.Create(AMemo, AStyler, [
      '[Files]',
      'Source: "a.txt"',
      '',
      'Source: "c.txt"']);
    try
      var Entry: TLiveScriptParameterSectionEntry;
      var Reason: TRefusalReason;
      Assert(Context.Factory.TryCreateParameterSectionEntry(2, Entry, Reason));
      try
        Entry.Entry.Add('Source', 'b.txt');
        Assert(AMemo.Lines.Count = 5);
        Assert(AMemo.Lines[2] = 'Source: "b.txt"');
        Assert(AMemo.Lines[3] = '');            { The old blank, now a separator }
        Assert(AMemo.Lines[4] = 'Source: "c.txt"');
        AMemo.Undo;
        Assert(AMemo.Lines.Count = 4);
        Assert(AMemo.Lines[2] = '');
      finally
        Entry.Free;
      end;
    finally
      Context.Free;
    end;
  end;
end;

{ Directive-style sections: last-occurrence value lookup, editing a populated
  section, refusals, an empty section that a directive is added to, and
  removing directives up to and including the last one }
procedure TestDirectiveSections(const AMemo: TScintEdit;
  const AStyler: TInnoSetupStyler);
begin
  { TryGetSetupDirectiveValue walks all [Setup] blocks; last occurrence wins and
    not-found is distinct from an empty value }
  begin
    const Context = TFactoryTestContext.Create(AMemo, AStyler, [
      '[Setup]',
      'AppName=First',
      'AppVersion=1.0',
      '[Files]',
      'Source: a',
      '[Setup]',
      'AppName=Second',
      'AppComments=']);
    try
      const Factory = Context.Factory;
      var Value: String;
      Assert(Factory.TryGetSetupDirectiveValue('AppName', Value));
      Assert(Value = 'Second'); { Last occurrence across the two [Setup] blocks }
      Assert(Factory.TryGetSetupDirectiveValue('AppVersion', Value));
      Assert(Value = '1.0');
      Assert(Factory.TryGetSetupDirectiveValue('AppComments', Value));
      Assert(Value = '');       { Present but empty }
      Assert(not Factory.TryGetSetupDirectiveValue('Missing', Value));

      { Occurrence numbering across the duplicate [Setup] blocks }
      var OccurrenceIndex, OccurrenceCount: Integer;
      Factory.GetSectionOccurrence(0, OccurrenceIndex, OccurrenceCount);
      Assert((OccurrenceIndex = 1) and (OccurrenceCount = 2));
      Factory.GetSectionOccurrence(2, OccurrenceIndex, OccurrenceCount);
      Assert((OccurrenceIndex = 2) and (OccurrenceCount = 2));
      Factory.GetSectionOccurrence(1, OccurrenceIndex, OccurrenceCount);
      Assert((OccurrenceIndex = 1) and (OccurrenceCount = 1)); { [Files] }
    finally
      Context.Free;
    end;
  end;

  { TryCreateDirectiveSection on a populated [Setup]: edit one directive and see
    it written back; refuse a parameter section and an out-of-range index }
  begin
    const Context = TFactoryTestContext.Create(AMemo, AStyler, [
      '[Setup]',
      'AppName=Foo',
      'AppVersion=1.0',
      '[Files]',
      'Source: a']);
    try
      const Factory = Context.Factory;
      var DirectiveSection: TLiveScriptDirectiveSection;
      var Reason: TRefusalReason;
      Assert(Factory.TryCreateDirectiveSection(0, DirectiveSection, Reason));
      try
        const List = DirectiveSection.Section;
        List.SetValue(List.IndexOf('AppName'), 'Edited');
        Assert(AMemo.Lines[1] = 'AppName=Edited');
        Assert(AMemo.Lines[2] = 'AppVersion=1.0'); { Other directive untouched }
      finally
        DirectiveSection.Free;
      end;
      Assert(not Factory.TryCreateDirectiveSection(1, DirectiveSection, Reason));
      Assert(Reason = rrNotDirectiveStyleSection);
      Assert(not Factory.TryCreateDirectiveSection(99, DirectiveSection, Reason));
      Assert(Reason = rrSectionIndexOutOfRange);
    finally
      Context.Free;
    end;
  end;

  { An empty directive-style section (a header with no body): adding a directive
    inserts it into the empty range }
  begin
    const Context = TFactoryTestContext.Create(AMemo, AStyler, [
      '[Messages]',
      '[Files]',
      'Source: a']);
    try
      var DirectiveSection: TLiveScriptDirectiveSection;
      var Reason: TRefusalReason;
      Assert(Context.Factory.TryCreateDirectiveSection(0, DirectiveSection, Reason));
      try
        Assert(DirectiveSection.Section.Count = 0);
        DirectiveSection.Section.Add('MyMsg', 'Hello');
        Assert(AMemo.Lines.Count = 4);
        Assert(AMemo.Lines[1] = 'MyMsg=Hello');
        Assert(AMemo.Lines[2] = '[Files]');
      finally
        DirectiveSection.Free;
      end;
    finally
      Context.Free;
    end;
  end;

  { Removing directives: removing one of two leaves the other line untouched,
    and removing the last one removes the physical line instead of leaving a
    blank line behind, after which the section is empty again and an Add
    reinserts a line }
  begin
    const Context = TFactoryTestContext.Create(AMemo, AStyler, [
      '[Setup]',
      'AppName=Foo',
      'AppVersion=1.0',
      '[Files]',
      'Source: a']);
    try
      var DirectiveSection: TLiveScriptDirectiveSection;
      var Reason: TRefusalReason;
      Assert(Context.Factory.TryCreateDirectiveSection(0, DirectiveSection, Reason));
      try
        const List = DirectiveSection.Section;
        List.Remove(List.IndexOf('AppName'));
        Assert(AMemo.Lines.Count = 4);
        Assert(AMemo.Lines[1] = 'AppVersion=1.0'); { Other directive untouched }
        Assert(AMemo.Lines[2] = '[Files]');
        List.Remove(List.IndexOf('AppVersion'));
        Assert(AMemo.Lines.Count = 3);
        Assert(AMemo.Lines[1] = '[Files]');
        { The empty range a body-less section is created with }
        Assert(DirectiveSection.LastLine < DirectiveSection.FirstLine);
        List.Add('AppComments', 'hi');
        Assert(AMemo.Lines.Count = 4);
        Assert(AMemo.Lines[1] = 'AppComments=hi');
        Assert(AMemo.Lines[2] = '[Files]');
      finally
        DirectiveSection.Free;
      end;
    finally
      Context.Free;
    end;
  end;

  { The same, but with the section body ending the document: there is no line
    ending below the last line, so the header's is taken and the header becomes
    the document's last line }
  begin
    const Context = TFactoryTestContext.Create(AMemo, AStyler, [
      '[Files]',
      'Source: a',
      '[Setup]',
      'AppName=Foo']);
    try
      var DirectiveSection: TLiveScriptDirectiveSection;
      var Reason: TRefusalReason;
      Assert(Context.Factory.TryCreateDirectiveSection(1, DirectiveSection, Reason));
      try
        const List = DirectiveSection.Section;
        List.Remove(List.IndexOf('AppName'));
        Assert(AMemo.Lines.Count = 3);
        Assert(AMemo.Lines[2] = '[Setup]');
        Assert(DirectiveSection.LastLine < DirectiveSection.FirstLine);
        List.Add('AppName', 'Bar');
        Assert(AMemo.Lines.Count = 4);
        Assert(AMemo.Lines[3] = 'AppName=Bar');
      finally
        DirectiveSection.Free;
      end;
    finally
      Context.Free;
    end;
  end;
end;

{ Edit tracking: the factory only learns of edits through Change }
procedure TestEditTracking(const AMemo: TScintEdit;
  const AStyler: TInnoSetupStyler);
begin
  const EOL = String(AMemo.LineEndingString);

  { Inserting lines above a live entry shifts its range and the section index,
    the entry stays valid, and the change bumps the factory's ChangeCount }
  begin
    const Context = TFactoryTestContext.Create(AMemo, AStyler, [
      '[Files]',
      'Source: "a.txt"']);
    try
      const Factory = Context.Factory;
      Assert(Factory.SectionCount = 1); { Build the index before editing }
      var Entry: TLiveScriptParameterSectionEntry;
      var Reason: TRefusalReason;
      Assert(Factory.TryCreateParameterSectionEntry(1, Entry, Reason));
      try
        Assert(Entry.FirstLine = 1);
        Assert(Entry.LastLine = 1);
        const ChangeCountBefore = Factory.ChangeCount;
        AMemo.ReplaceTextRange(0, 0, 'X' + EOL); { Insert a line at the top }
        Assert(Factory.ChangeCount > ChangeCountBefore);
        Assert(Entry.Valid);
        Assert(Entry.FirstLine = 2);
        Assert(Entry.LastLine = 2);
        Assert(Factory.SectionCount = 1);
        Assert(Factory.SectionHeaders[0].Line = 1); { [Files] header shifted down }
      finally
        Entry.Free;
      end;
    finally
      Context.Free;
    end;
  end;

  { Deleting the lines an entry occupies invalidates it, and the factory keeps
    working for the lines that remain }
  begin
    const Context = TFactoryTestContext.Create(AMemo, AStyler, [
      '[Files]',
      'Source: "a.txt"',
      'Source: "b.txt"',
      'Source: "c.txt"']);
    try
      const Factory = Context.Factory;
      Assert(Factory.SectionCount = 1);
      var Entry: TLiveScriptParameterSectionEntry;
      var Reason: TRefusalReason;
      Assert(Factory.TryCreateParameterSectionEntry(2, Entry, Reason));
      try
        Assert(Entry.FirstLine = 2);
        AMemo.ReplaceTextRange(AMemo.GetPositionFromLine(2),
          AMemo.GetPositionFromLine(3), ''); { Delete line 2 }
        Assert(not Entry.Valid);
      finally
        Entry.Free;
      end;
      { Line 2 now holds the former line 3 and is still parseable }
      var NewEntry: TLiveScriptParameterSectionEntry;
      Assert(Factory.TryCreateParameterSectionEntry(2, NewEntry, Reason));
      try
        var Value: String;
        Assert(NewEntry.Entry.TryGetValue('Source', Value) and (Value = 'c.txt'));
      finally
        NewEntry.Free;
      end;
    finally
      Context.Free;
    end;
  end;

  { Splitting the line of a one-line entry mid-line extends the range to the
    tail fragment, and a write-back rejoins the fragments }
  begin
    const Context = TFactoryTestContext.Create(AMemo, AStyler, [
      '[Files]',
      'Source: "a.txt"; DestDir: "{app}"']);
    try
      const Factory = Context.Factory;
      Assert(Factory.SectionCount = 1);
      var Entry: TLiveScriptParameterSectionEntry;
      var Reason: TRefusalReason;
      Assert(Factory.TryCreateParameterSectionEntry(1, Entry, Reason));
      try
        const SplitPos = AMemo.GetPositionFromLine(1) + Length('Source: "a.txt";');
        AMemo.ReplaceTextRange(SplitPos, SplitPos, EOL);
        Assert(AMemo.Lines.Count = 3);
        Assert(Entry.Valid);
        Assert(Entry.FirstLine = 1);
        Assert(Entry.LastLine = 2);
        Entry.Entry.SetValue(1, '{tmp}');
        Assert(AMemo.Lines.Count = 2);
        Assert(AMemo.Lines[1] = 'Source: "a.txt"; DestDir: "{tmp}"');
      finally
        Entry.Free;
      end;
    finally
      Context.Free;
    end;
  end;

  { Splitting a spanned entry's last physical line mid-line extends the range,
    and a write-back rejoins the fragments, keeping the author's break }
  begin
    const Context = TFactoryTestContext.Create(AMemo, AStyler, [
      '[Files]',
      'Source: "a.txt"; \',
      '  DestDir: "{app}"; Flags: ignoreversion']);
    try
      const Factory = Context.Factory;
      Assert(Factory.SectionCount = 1);
      var Entry: TLiveScriptParameterSectionEntry;
      var Reason: TRefusalReason;
      Assert(Factory.TryCreateParameterSectionEntry(1, Entry, Reason));
      try
        Assert(Entry.FirstLine = 1);
        Assert(Entry.LastLine = 2);
        const SplitPos = AMemo.GetPositionFromLine(2) + Length('  DestDir: "{app}";');
        AMemo.ReplaceTextRange(SplitPos, SplitPos, EOL);
        Assert(AMemo.Lines.Count = 4);
        Assert(Entry.Valid);
        Assert(Entry.FirstLine = 1);
        Assert(Entry.LastLine = 3);
        Entry.Entry.SetValue(1, '{tmp}');
        Assert(AMemo.Lines.Count = 3);
        Assert(AMemo.Lines[1] = 'Source: "a.txt"; \');
        Assert(AMemo.Lines[2] = '  DestDir: "{tmp}"; Flags: ignoreversion');
      finally
        Entry.Free;
      end;
    finally
      Context.Free;
    end;
  end;

  { Inserting a line at the exact start of the line following an entry extends
    nothing }
  begin
    const Context = TFactoryTestContext.Create(AMemo, AStyler, [
      '[Files]',
      'Source: "a.txt"',
      'Source: "b.txt"']);
    try
      const Factory = Context.Factory;
      Assert(Factory.SectionCount = 1);
      var Entry: TLiveScriptParameterSectionEntry;
      var Reason: TRefusalReason;
      Assert(Factory.TryCreateParameterSectionEntry(1, Entry, Reason));
      try
        const Pos = AMemo.GetPositionFromLine(2);
        AMemo.ReplaceTextRange(Pos, Pos, 'Source: "new.txt"' + EOL);
        Assert(Entry.Valid);
        Assert(Entry.FirstLine = 1);
        Assert(Entry.LastLine = 1);
      finally
        Entry.Free;
      end;
    finally
      Context.Free;
    end;
  end;

  { Deleting the line ending at the end of an entry's last line joins the
    following line's text into the covered range and invalidates the entry }
  begin
    const Context = TFactoryTestContext.Create(AMemo, AStyler, [
      '[Files]',
      'Source: "a.txt"',
      'Source: "b.txt"']);
    try
      const Factory = Context.Factory;
      Assert(Factory.SectionCount = 1);
      var Entry: TLiveScriptParameterSectionEntry;
      var Reason: TRefusalReason;
      Assert(Factory.TryCreateParameterSectionEntry(1, Entry, Reason));
      try
        AMemo.ReplaceTextRange(AMemo.GetLineEndPosition(1),
          AMemo.GetPositionFromLine(2), ''); { Join line 2 into line 1 }
        Assert(not Entry.Valid);
      finally
        Entry.Free;
      end;
    finally
      Context.Free;
    end;
  end;

  { Adding and removing a section header updates SectionCount and the index }
  begin
    const Context = TFactoryTestContext.Create(AMemo, AStyler, [
      '[Setup]',
      'AppName=x',
      '[Files]',
      'Source: a']);
    try
      const Factory = Context.Factory;
      Assert(Factory.SectionCount = 2);
      AMemo.ReplaceTextRange(AMemo.GetPositionFromLine(2),
        AMemo.GetPositionFromLine(2), '[Icons]' + EOL); { Add a header }
      Assert(Factory.SectionCount = 3);
      Assert(Factory.SectionHeaders[1].StylerSection = scIcons);
      Assert(Factory.SectionHeaders[1].Line = 2);
      Assert(Factory.SectionHeaders[2].StylerSection = scFiles);
      Assert(Factory.SectionHeaders[2].Line = 3);
      AMemo.ReplaceTextRange(AMemo.GetPositionFromLine(2),
        AMemo.GetPositionFromLine(3), ''); { Remove the header again }
      Assert(Factory.SectionCount = 2);
      Assert(Factory.SectionHeaders[1].StylerSection = scFiles);
      Assert(Factory.SectionHeaders[1].Line = 2);
    finally
      Context.Free;
    end;
  end;

  { InvalidateIndex (a simulated file reload) invalidates outstanding range
    objects }
  begin
    const Context = TFactoryTestContext.Create(AMemo, AStyler, [
      '[Files]',
      'Source: a']);
    try
      const Factory = Context.Factory;
      Assert(Factory.SectionCount = 1);
      var Entry: TLiveScriptParameterSectionEntry;
      var Reason: TRefusalReason;
      Assert(Factory.TryCreateParameterSectionEntry(1, Entry, Reason));
      try
        Assert(Entry.Valid);
        Factory.InvalidateIndex;
        Assert(not Entry.Valid);
      finally
        Entry.Free;
      end;
    finally
      Context.Free;
    end;
  end;
end;

procedure IDELiveScriptObjectFactoryRunTests(const AMemo: TScintEdit;
  const AStyler: TInnoSetupStyler);
begin
  const SavedText = AMemo.Lines.Text;
  const SavedOnChange = AMemo.OnChange;
  const SavedStyler = AMemo.Styler;
  const SavedReadOnly = AMemo.ReadOnly;
  try
    TestSectionIndexing(AMemo, AStyler);
    TestTryGetSectionAtLine(AMemo, AStyler);
    TestTryCreateParameterSectionEntry(AMemo, AStyler);
    TestEntryRoundTrip(AMemo, AStyler);
    TestDirectiveSections(AMemo, AStyler);
    TestEditTracking(AMemo, AStyler);
  finally
    AMemo.OnChange := nil;
    AMemo.Styler := SavedStyler;
    { Restore the text before the read-only state: Scintilla ignores
      modifications while a document is read-only }
    AMemo.Lines.Text := SavedText;
    AMemo.ClearUndo;
    AMemo.ReadOnly := SavedReadOnly;
    AMemo.OnChange := SavedOnChange;
  end;
end;

end.
