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
  TScintEdit's styler, change handler and read-only state are restored on
  exit. Its text must be empty.
}

interface

uses
  ScintEdit,
  IDE.ScintStylerInnoSetup;

procedure IDELiveScriptObjectFactoryRunTests(const AMemo: TScintEdit;
  const AStyler: TInnoSetupStyler);

implementation

uses
  System.SysUtils, System.Classes,
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

{ TFactoryTestContext }

constructor TFactoryTestContext.Create(const AMemo: TScintEdit;
  const AStyler: TInnoSetupStyler; const AScriptLines: array of String);

  function JoinScriptLines(const ALines: array of String): String;
  begin
    Result := '';
    for var I := 0 to High(ALines) do begin
      if I > 0 then
        Result := Result + #13#10;
      Result := Result + ALines[I];
    end;
  end;

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
    const AExpectedSection: TInnoSetupSection; const AExpectedLine: Integer;
    const AExpectedName: String);
  begin
    Assert(ASectionHeader.Section = AExpectedSection);
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

{ TryCreateParameterSectionEntries: every refusal reason plus the two accept
  paths (a real parameter line, and a blank line yielding an empty entry) }
procedure TestTryCreateParameterSectionEntries(const AMemo: TScintEdit;
  const AStyler: TInnoSetupStyler);

  procedure AssertRefusal(const AFactory: TLiveScriptObjectFactory; const ALine: Integer;
    const AExpectedReason: TRefusalReason);
  begin
    var Entries: TLiveScriptParameterSectionEntries;
    var Reason: TRefusalReason;
    Assert(not AFactory.TryCreateParameterSectionEntries([], [], ALine, Entries, Reason));
    Assert(Entries = nil);
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
    AssertRefusal(Factory, 2, rrNotParameterSection);
    AssertRefusal(Factory, 5, rrComment);
    AssertRefusal(Factory, 6, rrISPPDirective);
    AssertRefusal(Factory, 10, rrInCodeSection);
    AssertRefusal(Factory, 12, rrUnrecognizedSection);

    { Accept: a real parameter line, parameters readable }
    var Entries: TLiveScriptParameterSectionEntries;
    var Reason: TRefusalReason;
    Assert(Factory.TryCreateParameterSectionEntries([], [], 4, Entries, Reason));
    try
      Assert(Entries.Section = scFiles);
      var Value: String;
      Assert(Entries.PrimaryEntry.TryGetValue('Source', Value) and (Value = 'a.txt'));
      Assert(Entries.PrimaryEntry.TryGetValue('DestDir', Value) and (Value = '{app}'));
    finally
      Entries.Free;
    end;

    { Accept: a blank line inside a section yields an empty entry }
    Assert(Factory.TryCreateParameterSectionEntries([], [], 7, Entries, Reason));
    try
      Assert(Entries.PrimaryEntry.Count = 0);
    finally
      Entries.Free;
    end;
  finally
    Context.Free;
  end;
end;

function LineRange(const AStartLine, AEndLine: Integer): TScintLineRange;
begin
  Result.StartLine := AStartLine;
  Result.EndLine := AEndLine;
end;

{ TryCreateParameterSectionEntries with line ranges covering several lines:
  skipping of blank, comment, ISPP directive, and section header lines;
  a blank line with a selection of its own yielding an empty entry;
  combining two [Files] occurrences; two ranges extending to the same spanned
  entry; the mixed refusal; the fall backs to the caret line; and the
  resulting entry line ranges }
procedure TestTryCreateParameterSectionEntriesFromLineRanges(const AMemo: TScintEdit;
  const AStyler: TInnoSetupStyler);

  procedure AssertRefusal(const AFactory: TLiveScriptObjectFactory;
    const ALineRanges, AIndividualLineRanges: TArray<TScintLineRange>;
    const ACaretLine: Integer; const AExpectedReason: TRefusalReason);
  begin
    var Entries: TLiveScriptParameterSectionEntries;
    var Reason: TRefusalReason;
    Assert(not AFactory.TryCreateParameterSectionEntries(ALineRanges,
      AIndividualLineRanges, ACaretLine, Entries, Reason));
    Assert(Entries = nil);
    Assert(Reason = AExpectedReason);
  end;

  procedure AssertEntryLines(const AEntries: TLiveScriptParameterSectionEntries;
    const AIndex, AExpectedFirstLine, AExpectedLastLine: Integer);
  begin
    Assert(AEntries.Entries[AIndex].FirstLine = AExpectedFirstLine);
    Assert(AEntries.Entries[AIndex].LastLine = AExpectedLastLine);
  end;

begin
  const Context = TFactoryTestContext.Create(AMemo, AStyler, [
    '[Setup]',                              { 0 }
    'AppName=x',                            { 1 }
    '[Files]',                              { 2 }
    'Source: "a.txt"; DestDir: "{app}"',    { 3 }
    '; a comment in files',                 { 4 }
    '#define MyDef 1',                      { 5 }
    '',                                     { 6, blank inside [Files] }
    'Source: "b.txt"; \',                   { 7, spanned entry }
    '  DestDir: "{tmp}"; \',                { 8, continuation of line 7 }
    '  Flags: ignoreversion',               { 9, continuation of line 7 }
    '[Icons]',                              { 10 }
    'Name: "{group}\My"; Filename: "x"',    { 11 }
    '[Files]',                              { 12 }
    'Source: "c.txt"']);                    { 13 }
  try
    const Factory = Context.Factory;

    { Refusals. Mixed selections do not fall back to the caret line, even
      though inspecting the given caret line would succeed. }
    AssertRefusal(Factory, [LineRange(3, 99)], [LineRange(3, 99)], 3,
      rrLineOutOfRange);
    AssertRefusal(Factory, [LineRange(3, 3), LineRange(11, 11)],
      [LineRange(3, 3), LineRange(11, 11)], 3, rrMixedSelection); { [Files] plus [Icons] entries }
    AssertRefusal(Factory, [LineRange(1, 3)], [LineRange(1, 3)], 3,
      rrMixedSelection); { An entry plus key/value content }

    { Accept: skips the header, comment, ISPP directive, and blank lines,
      combines the two [Files] occurrences, extends the partially covered
      spanned entry to all of its lines, and ignores the caret line }
    var Entries: TLiveScriptParameterSectionEntries;
    var Reason: TRefusalReason;
    Assert(Factory.TryCreateParameterSectionEntries(
      [LineRange(2, 7), LineRange(12, 13)],
      [LineRange(2, 7), LineRange(12, 13)], 1, Entries, Reason));
    try
      Assert(Entries.Count = 3);
      Assert(Entries.Valid);
      Assert(Entries.Section = scFiles);
      AssertEntryLines(Entries, 0, 3, 3);
      AssertEntryLines(Entries, 1, 7, 9);
      AssertEntryLines(Entries, 2, 13, 13);
      Assert(Entries.PrimaryFirstLine = 3);
      Assert(Entries.PrimaryLastLine = 3);
      var Value: String;
      Assert(Entries.PrimaryEntry.TryGetValue('Source', Value) and (Value = 'a.txt'));
      Assert(Entries.Entries[1].Entry.TryGetValue('DestDir', Value) and (Value = '{tmp}'));
      Assert(Entries.Entries[2].Entry.TryGetValue('Source', Value) and (Value = 'c.txt'));
    finally
      Entries.Free;
    end;

    { Two ranges extending to the same spanned entry yield it once }
    Assert(Factory.TryCreateParameterSectionEntries(
      [LineRange(7, 7), LineRange(9, 9)],
      [LineRange(7, 7), LineRange(9, 9)], 7, Entries, Reason));
    try
      Assert(Entries.Count = 1);
      AssertEntryLines(Entries, 0, 7, 9);
      Assert(Entries.GetFlagCheckState('Flags', -1, 'ignoreversion') = fcsAll);
    finally
      Entries.Free;
    end;

    { Ranges covering a single line inspect the caret line instead, which can
      be a different line: selecting a whole line leaves the caret below it }
    Assert(Factory.TryCreateParameterSectionEntries([LineRange(4, 4)],
      [LineRange(4, 4)], 13, Entries, Reason));
    try
      Assert(Entries.Count = 1);
      AssertEntryLines(Entries, 0, 13, 13);
    finally
      Entries.Free;
    end;

    { A selection without entries falls back to the caret line: to an entry,
      to a blank line yielding an empty entry, and to a refused line }
    Assert(Factory.TryCreateParameterSectionEntries([LineRange(4, 6)],
      [LineRange(4, 6)], 13, Entries, Reason)); { Only comment/ISPP/blank lines }
    try
      Assert(Entries.Count = 1);
      AssertEntryLines(Entries, 0, 13, 13);
    finally
      Entries.Free;
    end;
    Assert(Factory.TryCreateParameterSectionEntries([LineRange(0, 1)],
      [LineRange(0, 1)], 3, Entries, Reason)); { Only key/value content }
    try
      AssertEntryLines(Entries, 0, 3, 3);
    finally
      Entries.Free;
    end;
    Assert(Factory.TryCreateParameterSectionEntries([LineRange(4, 5)],
      [LineRange(4, 5)], 6, Entries, Reason)); { Caret on the blank line }
    try
      Assert(Entries.PrimaryEntry.Count = 0);
    finally
      Entries.Free;
    end;
    AssertRefusal(Factory, [LineRange(4, 6)], [LineRange(4, 6)], 1,
      rrNotParameterSection); { Caret on key/value content }

    { A blank line with a selection of its own is not skipped: it becomes an
      empty entry among the others, so a new entry can be created on it, and
      it counts toward the mixed refusal }
    Assert(Factory.TryCreateParameterSectionEntries(
      [LineRange(3, 3), LineRange(6, 6)],
      [LineRange(3, 3), LineRange(6, 6)], 3, Entries, Reason));
    try
      Assert(Entries.Count = 2);
      AssertEntryLines(Entries, 0, 3, 3);
      AssertEntryLines(Entries, 1, 6, 6);
      Assert(Entries.Entries[1].Entry.Count = 0);
    finally
      Entries.Free;
    end;
    AssertRefusal(Factory, [LineRange(6, 6), LineRange(11, 11)],
      [LineRange(6, 6), LineRange(11, 11)], 6, rrMixedSelection); { A blank [Files] line plus an [Icons] entry }
  finally
    Context.Free;
  end;
end;

{ Editing an entry's value through the model writes back to the memo. Covers
  a single-line entry (with one undo restoring it), a spanned entry whose line
  breaks are preserved, and a blank-line entry that inserts itself above the
  blank, alone and in a multi-entry write }
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
      var Entries: TLiveScriptParameterSectionEntries;
      var Reason: TRefusalReason;
      Assert(Context.Factory.TryCreateParameterSectionEntries([], [], 1, Entries, Reason));
      try
        Entries.PrimaryEntry.SetValue(1, '{tmp}');
        Assert(AMemo.Lines[1] = 'Source: "a.txt"; DestDir: "{tmp}"');
        Assert(AMemo.Lines[2] = 'Source: "keep.txt"'); { Neighbor untouched }
        AMemo.Undo; { Write-back is a single undo action }
        Assert(AMemo.Lines[1] = 'Source: "a.txt"; DestDir: "{app}"');
      finally
        Entries.Free;
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
      var Entries: TLiveScriptParameterSectionEntries;
      var Reason: TRefusalReason;
      Assert(Context.Factory.TryCreateParameterSectionEntries([], [], 1, Entries, Reason));
      try
        Entries.PrimaryEntry.SetValue(1, '{tmp}');
        Assert(AMemo.Lines.Count = 3);
        Assert(AMemo.Lines[1] = 'Source: "a.txt"; \');
        Assert(AMemo.Lines[2] = '  DestDir: "{tmp}"; Flags: ignoreversion');
      finally
        Entries.Free;
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
      var Entries: TLiveScriptParameterSectionEntries;
      var Reason: TRefusalReason;
      Assert(Context.Factory.TryCreateParameterSectionEntries([], [], 2, Entries, Reason));
      try
        Entries.PrimaryEntry.Add('Source', 'b.txt');
        Assert(AMemo.Lines.Count = 5);
        Assert(AMemo.Lines[2] = 'Source: "b.txt"');
        Assert(AMemo.Lines[3] = '');            { The old blank, now a separator }
        Assert(AMemo.Lines[4] = 'Source: "c.txt"');
        AMemo.Undo;
        Assert(AMemo.Lines.Count = 4);
        Assert(AMemo.Lines[2] = '');
      finally
        Entries.Free;
      end;
    finally
      Context.Free;
    end;
  end;

  { Blank-line entry in a multi-entry write: the existing entry is edited and
    the blank-line entry inserts itself above the blank, in one undo action }
  begin
    const Context = TFactoryTestContext.Create(AMemo, AStyler, [
      '[Files]',
      'Source: "a.txt"',
      '',
      'Source: "c.txt"']);
    try
      var Entries: TLiveScriptParameterSectionEntries;
      var Reason: TRefusalReason;
      { Like two carets, one on the entry and one on the blank line: the line
        ranges merge, the individual line ranges do not }
      Assert(Context.Factory.TryCreateParameterSectionEntries([LineRange(1, 2)],
        [LineRange(1, 1), LineRange(2, 2)], 1, Entries, Reason));
      try
        Assert(Entries.Count = 2);
        Assert(Entries.Entries[1].Entry.Count = 0);
        Entries.SetValue('DestDir', -1, '{app}');
        Assert(AMemo.Lines.Count = 5);
        Assert(AMemo.Lines[1] = 'Source: "a.txt"; DestDir: "{app}"');
        Assert(AMemo.Lines[2] = 'DestDir: "{app}"'); { The new entry, above the blank }
        Assert(AMemo.Lines[3] = '');
        Assert(AMemo.Lines[4] = 'Source: "c.txt"');
        Assert(Entries.Valid);
        AMemo.Undo; { The whole multi-entry write is a single undo action }
        Assert(AMemo.Lines.Count = 4);
        Assert(AMemo.Lines[1] = 'Source: "a.txt"');
        Assert(AMemo.Lines[2] = '');
        { Don't edit more here: the undo made the entries' models stale }
      finally
        Entries.Free;
      end;
    finally
      Context.Free;
    end;
  end;
end;

{ The name-plus-index-hint reads and writes of TLiveScriptParameterSectionEntries,
  exercised with a single entry: aggregate reads, writes with their
  add-when-absent rules, and one undo action per write }
procedure TestParameterSectionEntries(const AMemo: TScintEdit;
  const AStyler: TInnoSetupStyler);
begin
  { Reads: common value (with the hint resolving a duplicated parameter name
    to the right occurrence), flag check state, and presence }
  begin
    const Context = TFactoryTestContext.Create(AMemo, AStyler, [
      '[Files]',
      'Source: "a.txt"; DestDir: "{app}"; DestDir: "{tmp}"; Flags: ignoreversion',
      'Source: "b.txt"']);
    try
      var Entries: TLiveScriptParameterSectionEntries;
      var Reason: TRefusalReason;
      Assert(Context.Factory.TryCreateParameterSectionEntries([], [], 1, Entries, Reason));
      try
        Assert(Entries.Count = 1);
        Assert(Entries.Valid);
        Assert(Entries.GetValue('Source', -1) = 'a.txt');
        Assert(Entries.GetValue('Source', 0) = 'a.txt');
        Assert(Entries.GetValue('Missing', -1) = '');
        Assert(Entries.GetValue('DestDir', -1) = '{app}'); { First occurrence }
        Assert(Entries.GetValue('DestDir', 2) = '{tmp}');  { The hint picks the second }
        Assert(Entries.GetValue('DestDir', 99) = '{app}'); { A stale hint falls back to the name }
        Assert(Entries.GetFlagCheckState('Flags', -1, 'ignoreversion') = fcsAll);
        Assert(Entries.GetFlagCheckState('Flags', -1, 'solidbreak') = fcsNone);
        Assert(Entries.MemberPresent('DestDir', -1));
        Assert(Entries.MemberPresent('DestDir', 99));
        Assert(not Entries.MemberPresent('Missing', -1));
      finally
        Entries.Free;
      end;

      { A missing Flags parameter counts as every flag being excluded }
      Assert(Context.Factory.TryCreateParameterSectionEntries([], [], 2, Entries, Reason));
      try
        Assert(Entries.GetFlagCheckState('Flags', -1, 'ignoreversion') = fcsNone);
      finally
        Entries.Free;
      end;
    finally
      Context.Free;
    end;
  end;

  { SetValue: resolve then set, else add, but only when the hint is -1 and the
    value is non-empty }
  begin
    const Context = TFactoryTestContext.Create(AMemo, AStyler, [
      '[Files]',
      'Source: "a.txt"; DestDir: "{app}"; DestDir: "{tmp}"']);
    try
      var Entries: TLiveScriptParameterSectionEntries;
      var Reason: TRefusalReason;
      Assert(Context.Factory.TryCreateParameterSectionEntries([], [], 1, Entries, Reason));
      try
        Entries.SetValue('DestDir', 2, '{sys}'); { The hint picks the second occurrence }
        Assert(AMemo.Lines[1] = 'Source: "a.txt"; DestDir: "{app}"; DestDir: "{sys}"');
        Entries.SetValue('DestDir', -1, '{autopf}');
        Assert(AMemo.Lines[1] = 'Source: "a.txt"; DestDir: "{autopf}"; DestDir: "{sys}"');
        Entries.SetValue('DestName', 99, 'b.txt'); { A stale real hint must not add }
        Entries.SetValue('DestName', -1, '');      { Neither must an empty value }
        Assert(AMemo.Lines[1] = 'Source: "a.txt"; DestDir: "{autopf}"; DestDir: "{sys}"');
        Entries.SetValue('DestName', -1, 'b.txt');
        Assert(AMemo.Lines[1] = 'Source: "a.txt"; DestDir: "{autopf}"; DestDir: "{sys}"; DestName: "b.txt"');
        AMemo.Undo; { The write including the add is a single undo action }
        Assert(AMemo.Lines[1] = 'Source: "a.txt"; DestDir: "{autopf}"; DestDir: "{sys}"');
        { Don't edit more here: the undo made the entry's model stale }
      finally
        Entries.Free;
      end;
    finally
      Context.Free;
    end;
  end;

  { SetFlag: include and exclude on a present Flags parameter, the
    add-when-absent rule on include, and Remove }
  begin
    const Context = TFactoryTestContext.Create(AMemo, AStyler, [
      '[Files]',
      'Source: "a.txt"; Flags: ignoreversion',
      'Source: "b.txt"']);
    try
      var Entries: TLiveScriptParameterSectionEntries;
      var Reason: TRefusalReason;
      Assert(Context.Factory.TryCreateParameterSectionEntries([], [], 1, Entries, Reason));
      try
        Entries.SetFlag('Flags', -1, 'solidbreak', True);
        Assert(AMemo.Lines[1] = 'Source: "a.txt"; Flags: ignoreversion solidbreak');
        Entries.SetFlag('Flags', -1, 'solidbreak', False);
        Assert(AMemo.Lines[1] = 'Source: "a.txt"; Flags: ignoreversion');
        Entries.Remove('Flags', -1);
        Assert(AMemo.Lines[1] = 'Source: "a.txt"');
        Entries.Remove('Missing', -1); { Removing an absent parameter is a no-op }
        Assert(AMemo.Lines[1] = 'Source: "a.txt"');
      finally
        Entries.Free;
      end;

      Assert(Context.Factory.TryCreateParameterSectionEntries([], [], 2, Entries, Reason));
      try
        Entries.SetFlag('Flags', -1, 'solidbreak', False); { Excluding without the parameter is a no-op }
        Entries.SetFlag('Flags', 99, 'solidbreak', True);  { A stale real hint must not add }
        Assert(AMemo.Lines[2] = 'Source: "b.txt"');
        Entries.SetFlag('Flags', -1, 'solidbreak', True);  { Adds the Flags parameter first }
        Assert(AMemo.Lines[2] = 'Source: "b.txt"; Flags: solidbreak');
        AMemo.Undo; { The add plus the include is a single undo action }
        Assert(AMemo.Lines[2] = 'Source: "b.txt"');
      finally
        Entries.Free;
      end;
    finally
      Context.Free;
    end;
  end;
end;

{ The name-plus-index-hint reads and writes of TLiveScriptParameterSectionEntries,
  exercised with several entries: reads aggregating common, differing, and
  absent-in-some values and flags, writes applying to every entry with the
  add-when-absent and flag rules per entry, one undo action per multi-entry
  write, real index hints resolving per entry, a write that changes the line
  count shifting the entries still to be written, and the fail-safe when one
  entry is invalidated }
procedure TestParameterSectionEntriesMultiEntry(const AMemo: TScintEdit;
  const AStyler: TInnoSetupStyler);
begin
  { Reads: a value common to every entry, values differing between entries
    (including by case only, the comparison is case-sensitive), a value absent
    in some entries, and flag check states where a missing Flags parameter
    counts as every flag being excluded }
  begin
    const Context = TFactoryTestContext.Create(AMemo, AStyler, [
      '[Files]',                                                                { 0 }
      'Source: "same.txt"; DestDir: "{app}"; Flags: ignoreversion solidbreak',  { 1 }
      'Source: "same.txt"; DestDir: "{tmp}"; Flags: ignoreversion',             { 2 }
      'Source: "same.txt"; DestDir: "{app}"; DestName: "new.txt"',              { 3 }
      'Source: "SAME.txt"']);                                                   { 4 }
    try
      var Entries: TLiveScriptParameterSectionEntries;
      var Reason: TRefusalReason;
      Assert(Context.Factory.TryCreateParameterSectionEntries([LineRange(1, 3)],
        [LineRange(1, 3)], 1, Entries, Reason));
      try
        Assert(Entries.Count = 3);
        Assert(Entries.GetValue('Source', -1) = 'same.txt'); { Common to every entry }
        Assert(Entries.GetValue('DestDir', -1) = '');        { Present in every entry but differing }
        Assert(Entries.GetValue('DestName', -1) = '');       { Absent in some entries }
        Assert(Entries.GetValue('Flags', -1) = '');          { The Flags parent row: differing }
        Assert(Entries.MemberPresent('DestName', -1));       { Present in at least one entry }
        Assert(not Entries.MemberPresent('Missing', -1));
        Assert(Entries.GetFlagCheckState('Flags', -1, 'ignoreversion') = fcsSome); { The entry without Flags counts as excluded }
        Assert(Entries.GetFlagCheckState('Flags', -1, 'solidbreak') = fcsSome);
        Assert(Entries.GetFlagCheckState('Flags', -1, 'external') = fcsNone);
      finally
        Entries.Free;
      end;

      { Restricted to the two entries that both have ignoreversion }
      Assert(Context.Factory.TryCreateParameterSectionEntries([LineRange(1, 2)],
        [LineRange(1, 2)], 1, Entries, Reason));
      try
        Assert(Entries.GetFlagCheckState('Flags', -1, 'ignoreversion') = fcsAll);
        Assert(Entries.GetFlagCheckState('Flags', -1, 'solidbreak') = fcsSome);
      finally
        Entries.Free;
      end;

      { Values differing by case only are not common }
      Assert(Context.Factory.TryCreateParameterSectionEntries([LineRange(3, 4)],
        [LineRange(3, 4)], 3, Entries, Reason));
      try
        Assert(Entries.GetValue('Source', -1) = '');
      finally
        Entries.Free;
      end;
    finally
      Context.Free;
    end;
  end;

  { SetValue: sets the value in every entry, adding the parameter where
    missing, and one undo restores every entry }
  begin
    const Context = TFactoryTestContext.Create(AMemo, AStyler, [
      '[Files]',
      'Source: "a.txt"; DestDir: "{app}"',
      'Source: "b.txt"',
      'Source: "c.txt"; DestDir: "{tmp}"']);
    try
      var Entries: TLiveScriptParameterSectionEntries;
      var Reason: TRefusalReason;
      Assert(Context.Factory.TryCreateParameterSectionEntries([LineRange(1, 3)],
        [LineRange(1, 3)], 1, Entries, Reason));
      try
        Assert(Entries.Count = 3);
        Entries.SetValue('DestDir', -1, '{sys}');
        Assert(AMemo.Lines[1] = 'Source: "a.txt"; DestDir: "{sys}"');
        Assert(AMemo.Lines[2] = 'Source: "b.txt"; DestDir: "{sys}"'); { Added where missing }
        Assert(AMemo.Lines[3] = 'Source: "c.txt"; DestDir: "{sys}"');
        Assert(Entries.GetValue('DestDir', -1) = '{sys}'); { Common now }
        AMemo.Undo; { The whole multi-entry write is a single undo action }
        Assert(AMemo.Lines[1] = 'Source: "a.txt"; DestDir: "{app}"');
        Assert(AMemo.Lines[2] = 'Source: "b.txt"');
        Assert(AMemo.Lines[3] = 'Source: "c.txt"; DestDir: "{tmp}"');
        { Don't edit more here: the undo made the entries' models stale }
      finally
        Entries.Free;
      end;
    finally
      Context.Free;
    end;
  end;

  { SetFlag: includes the flag in every entry with the includes rule applied
    per entry (createallsubdirs also includes recursesubdirs, already present
    in one entry), adding the Flags parameter where missing, and one undo
    restores every entry }
  begin
    const Context = TFactoryTestContext.Create(AMemo, AStyler, [
      '[Files]',
      'Source: "a.txt"; Flags: recursesubdirs',
      'Source: "b.txt"']);
    try
      var Entries: TLiveScriptParameterSectionEntries;
      var Reason: TRefusalReason;
      Assert(Context.Factory.TryCreateParameterSectionEntries([LineRange(1, 2)],
        [LineRange(1, 2)], 1, Entries, Reason));
      try
        Entries.SetFlag('Flags', -1, 'createallsubdirs', True);
        Assert(AMemo.Lines[1] = 'Source: "a.txt"; Flags: recursesubdirs createallsubdirs');
        Assert(AMemo.Lines[2] = 'Source: "b.txt"; Flags: createallsubdirs recursesubdirs'); { Flags parameter added, rule ran }
        Assert(Entries.GetFlagCheckState('Flags', -1, 'createallsubdirs') = fcsAll);
        Assert(Entries.GetFlagCheckState('Flags', -1, 'recursesubdirs') = fcsAll);
        AMemo.Undo; { The adds, includes, and rule includes are a single undo action }
        Assert(AMemo.Lines[1] = 'Source: "a.txt"; Flags: recursesubdirs');
        Assert(AMemo.Lines[2] = 'Source: "b.txt"');
        { Don't edit more here: the undo made the entries' models stale }
      finally
        Entries.Free;
      end;
    finally
      Context.Free;
    end;
  end;

  { SetFlag: including a flag included in only some entries (the indeterminate
    checkbox click) includes it in every entry, with the excludes rule
    (setntfscompression excludes unsetntfscompression) applied only in the
    entry that has the excluded flag; excluding then removes it from every
    entry, removing the Flags parameter where it was the only flag }
  begin
    const Context = TFactoryTestContext.Create(AMemo, AStyler, [
      '[Files]',
      'Source: "a.txt"; Flags: setntfscompression solidbreak',
      'Source: "b.txt"; Flags: unsetntfscompression',
      'Source: "c.txt"']);
    try
      var Entries: TLiveScriptParameterSectionEntries;
      var Reason: TRefusalReason;
      Assert(Context.Factory.TryCreateParameterSectionEntries([LineRange(1, 3)],
        [LineRange(1, 3)], 1, Entries, Reason));
      try
        Assert(Entries.GetFlagCheckState('Flags', -1, 'setntfscompression') = fcsSome);
        Entries.SetFlag('Flags', -1, 'setntfscompression', True);
        Assert(AMemo.Lines[1] = 'Source: "a.txt"; Flags: setntfscompression solidbreak'); { Already included, untouched }
        Assert(AMemo.Lines[2] = 'Source: "b.txt"; Flags: setntfscompression'); { unsetntfscompression excluded by the rule }
        Assert(AMemo.Lines[3] = 'Source: "c.txt"; Flags: setntfscompression'); { Flags parameter added }
        Assert(Entries.GetFlagCheckState('Flags', -1, 'setntfscompression') = fcsAll);
        Assert(Entries.GetFlagCheckState('Flags', -1, 'unsetntfscompression') = fcsNone);
        Entries.SetFlag('Flags', -1, 'setntfscompression', False);
        Assert(AMemo.Lines[1] = 'Source: "a.txt"; Flags: solidbreak');
        Assert(AMemo.Lines[2] = 'Source: "b.txt"'); { The whole Flags parameter removed }
        Assert(AMemo.Lines[3] = 'Source: "c.txt"');
        Assert(Entries.GetFlagCheckState('Flags', -1, 'setntfscompression') = fcsNone);
      finally
        Entries.Free;
      end;
    finally
      Context.Free;
    end;
  end;

  { Remove: removes the parameter from every entry where it is present,
    leaving the other entries untouched }
  begin
    const Context = TFactoryTestContext.Create(AMemo, AStyler, [
      '[Files]',
      'Source: "a.txt"; DestDir: "{app}"; Flags: ignoreversion',
      'Source: "b.txt"',
      'Source: "c.txt"; DestDir: "{tmp}"']);
    try
      var Entries: TLiveScriptParameterSectionEntries;
      var Reason: TRefusalReason;
      Assert(Context.Factory.TryCreateParameterSectionEntries([LineRange(1, 3)],
        [LineRange(1, 3)], 1, Entries, Reason));
      try
        Assert(Entries.MemberPresent('DestDir', -1));
        Entries.Remove('DestDir', -1);
        Assert(AMemo.Lines[1] = 'Source: "a.txt"; Flags: ignoreversion');
        Assert(AMemo.Lines[2] = 'Source: "b.txt"');
        Assert(AMemo.Lines[3] = 'Source: "c.txt"');
        Assert(not Entries.MemberPresent('DestDir', -1));
      finally
        Entries.Free;
      end;
    finally
      Context.Free;
    end;
  end;

  { A real (non-negative) index hint applies per entry with TryResolve
    semantics: it keeps the occurrence at the hint where the name matches
    (disambiguating a duplicated name in the primary entry), falls back to
    name lookup where it does not, and never adds where the parameter is
    missing }
  begin
    const Context = TFactoryTestContext.Create(AMemo, AStyler, [
      '[Files]',                                              { 0 }
      'Source: "a.txt"; DestDir: "{app}"; DestDir: "{sys}"',  { 1 }
      'Source: "b.txt"; DestDir: "{sys}"; DestName: "n.txt"', { 2, index 2 names a different parameter }
      'Source: "c.txt"']);                                    { 3 }
    try
      var Entries: TLiveScriptParameterSectionEntries;
      var Reason: TRefusalReason;
      Assert(Context.Factory.TryCreateParameterSectionEntries([LineRange(1, 2)],
        [LineRange(1, 2)], 1, Entries, Reason));
      try
        { The hint picks the second occurrence in the first entry and falls
          back to the name in the second, so the common value is 'sys' even
          though the first entry's first occurrence is 'app' }
        Assert(Entries.GetValue('DestDir', 2) = '{sys}');
      finally
        Entries.Free;
      end;

      Assert(Context.Factory.TryCreateParameterSectionEntries([LineRange(1, 3)],
        [LineRange(1, 3)], 1, Entries, Reason));
      try
        Entries.SetValue('DestDir', 2, '{tmp}');
        Assert(AMemo.Lines[1] = 'Source: "a.txt"; DestDir: "{app}"; DestDir: "{tmp}"');
        Assert(AMemo.Lines[2] = 'Source: "b.txt"; DestDir: "{tmp}"; DestName: "n.txt"');
        Assert(AMemo.Lines[3] = 'Source: "c.txt"'); { A real hint must not add }
      finally
        Entries.Free;
      end;
    finally
      Context.Free;
    end;
  end;

  { A write that changes the line count: removing the only spanned parameter
    of the first entry shrinks it to one line, and the following entry's write
    must land on its shifted line. One undo restores both entries. }
  begin
    const Context = TFactoryTestContext.Create(AMemo, AStyler, [
      '[Files]',                            { 0 }
      'Source: "a.txt"; \',                 { 1, spanned entry }
      '  DestDir: "{app}"',                 { 2, continuation of line 1 }
      'Source: "b.txt"; DestDir: "{tmp}"']); { 3 }
    try
      var Entries: TLiveScriptParameterSectionEntries;
      var Reason: TRefusalReason;
      Assert(Context.Factory.TryCreateParameterSectionEntries([LineRange(1, 3)],
        [LineRange(1, 3)], 1, Entries, Reason));
      try
        Assert(Entries.Count = 2);
        Assert(Entries.Entries[1].FirstLine = 3);
        Entries.Remove('DestDir', -1);
        Assert(AMemo.Lines.Count = 3);
        Assert(AMemo.Lines[1] = 'Source: "a.txt"');
        Assert(AMemo.Lines[2] = 'Source: "b.txt"');
        Assert(Entries.Valid);
        Assert(Entries.Entries[1].FirstLine = 2); { Shifted by the first entry's write }
        AMemo.Undo; { Both writes are a single undo action }
        Assert(AMemo.Lines.Count = 4);
        Assert(AMemo.Lines[1] = 'Source: "a.txt"; \');
        Assert(AMemo.Lines[2] = '  DestDir: "{app}"');
        Assert(AMemo.Lines[3] = 'Source: "b.txt"; DestDir: "{tmp}"');
        { Don't edit more here: the undo made the entries' models stale }
      finally
        Entries.Free;
      end;
    finally
      Context.Free;
    end;
  end;

  { One entry becoming invalid makes the whole container invalid: reads fail
    safe and writes are no-ops, while the surviving entries stay individually
    valid }
  begin
    const Context = TFactoryTestContext.Create(AMemo, AStyler, [
      '[Files]',
      'Source: "a.txt"; DestDir: "{app}"; Flags: ignoreversion',
      'Source: "b.txt"; DestDir: "{app}"; Flags: ignoreversion',
      'Source: "c.txt"; DestDir: "{app}"; Flags: ignoreversion']);
    try
      var Entries: TLiveScriptParameterSectionEntries;
      var Reason: TRefusalReason;
      Assert(Context.Factory.TryCreateParameterSectionEntries([LineRange(1, 3)],
        [LineRange(1, 3)], 1, Entries, Reason));
      try
        Assert(Entries.GetValue('DestDir', -1) = '{app}');
        Assert(Entries.GetFlagCheckState('Flags', -1, 'ignoreversion') = fcsAll);
        AMemo.ReplaceTextRange(AMemo.GetPositionFromLine(2),
          AMemo.GetPositionFromLine(3), ''); { Delete the middle entry's line }
        Assert(not Entries.Valid);
        Assert(Entries.Entries[0].Valid);
        Assert(not Entries.Entries[1].Valid);
        Assert(Entries.Entries[2].Valid);
        Assert(Entries.GetValue('DestDir', -1) = '');
        Assert(Entries.GetFlagCheckState('Flags', -1, 'ignoreversion') = fcsNone);
        Assert(not Entries.MemberPresent('DestDir', -1));
        Entries.SetValue('DestDir', -1, '{tmp}'); { Writes are no-ops }
        Assert(AMemo.Lines[1] = 'Source: "a.txt"; DestDir: "{app}"; Flags: ignoreversion');
        Assert(AMemo.Lines[2] = 'Source: "c.txt"; DestDir: "{app}"; Flags: ignoreversion');
      finally
        Entries.Free;
      end;
    finally
      Context.Free;
    end;
  end;
end;

{ CollectParameterValues: values collected across every occurrence of the
  given section or of every parameter section, skipping comment and blank
  lines, reading spanned entries whole, unquoting, skipping empty values,
  leaving ordering and duplicate handling to the passed list, and
  splitting values into words when asked }
procedure TestCollectParameterValues(const AMemo: TScintEdit;
  const AStyler: TInnoSetupStyler);
begin
  const Context = TFactoryTestContext.Create(AMemo, AStyler, [
    '[Tasks]',                                                       { 0 }
    'Name: desktopicon; Description: "Desktop icon"',                { 1 }
    'Name: "desktopicon\common"; Description: "For all users"',      { 2 }
    '; a comment',                                                   { 3 }
    '',                                                              { 4 }
    'Name: desktopicon; Description: "Duplicate name"',              { 5 }
    'Description: "No name"',                                        { 6 }
    'Name: ""; Description: "Empty name"',                           { 7 }
    '[Files]',                                                       { 8 }
    'Source: "a.txt"; Tasks: "not portable"',                        { 9 }
    'Source: "b.txt"; Tasks: NOT PORTABLE',                          { 10, duplicate value ignoring case }
    'Source: "c.txt"; \',                                            { 11, spanned entry }
    '  Tasks: desktopicon\common',                                   { 12, continuation of line 11 }
    'Source: "d.txt"',                                               { 13, no Tasks parameter }
    '[Icons]',                                                       { 14 }
    'Name: "{group}\My"; Filename: "x"; Tasks: portable',            { 15 }
    '[Tasks]',                                                       { 16, second occurrence }
    'Name: portable; Description: "Portable mode"',                  { 17 }
    '[ISSigKeys]',                                                   { 18 }
    'Name: mykey1; Group: "all extra"',                              { 19 }
    'Name: mykey2; Group: ALL']);                                    { 20, duplicate word ignoring case }
  try
    const Factory = Context.Factory;
    const Values = TStringList.Create;
    try
      Values.CaseSensitive := False;
      Values.Duplicates := dupIgnore; { Only effective on a sorted list }
      Values.Sorted := True;

      { The Name values of the [Tasks] occurrences only: the [Icons] Name must
        not appear }
      Factory.CollectParameterValues(scTasks, 'Name', Values);
      Assert(Values.Count = 3);
      Assert(Values[0] = 'desktopicon');
      Assert(Values[1] = 'desktopicon\common');
      Assert(Values[2] = 'portable');

      { The Tasks values of every parameter section }
      Values.Clear;
      Factory.CollectParameterValues(scNone, 'Tasks', Values);
      Assert(Values.Count = 3);
      Assert(Values[0] = 'desktopicon\common');
      Assert(Values[1] = 'not portable');
      Assert(Values[2] = 'portable');

      { The space-separated words of the Group values }
      Values.Clear;
      Factory.CollectParameterValues(scISSigKeys, 'Group', Values, True);
      Assert(Values.Count = 2);
      Assert(Values[0] = 'all');
      Assert(Values[1] = 'extra');

      { A repeated collection adds nothing: the list ignores the duplicates }
      Factory.CollectParameterValues(scISSigKeys, 'Group', Values, True);
      Assert(Values.Count = 2);
    finally
      Values.Free;
    end;
  finally
    Context.Free;
  end;
end;

{ Key/value sections: last-occurrence value lookup, editing a populated
  section, refusals, an empty section that a key is added to, and
  removing keys up to and including the last one }
procedure TestKeyValueSections(const AMemo: TScintEdit;
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

  { TryCreateKeyValueSection on a populated [Setup]: edit one directive and see
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
      var KeyValueSection: TLiveScriptKeyValueSection;
      var Reason: TRefusalReason;
      Assert(Factory.TryCreateKeyValueSection(0, KeyValueSection, Reason));
      try
        const List = KeyValueSection.Section;
        List.SetValue(List.IndexOf('AppName'), 'Edited');
        Assert(AMemo.Lines[1] = 'AppName=Edited');
        Assert(AMemo.Lines[2] = 'AppVersion=1.0'); { Other directive untouched }
      finally
        KeyValueSection.Free;
      end;
      Assert(not Factory.TryCreateKeyValueSection(1, KeyValueSection, Reason));
      Assert(Reason = rrNotKeyValueSection);
      Assert(not Factory.TryCreateKeyValueSection(99, KeyValueSection, Reason));
      Assert(Reason = rrSectionIndexOutOfRange);
    finally
      Context.Free;
    end;
  end;

  { An empty key/value section (a header with no body): adding a key/value line
    inserts it into the empty range }
  begin
    const Context = TFactoryTestContext.Create(AMemo, AStyler, [
      '[Messages]',
      '[Files]',
      'Source: a']);
    try
      var KeyValueSection: TLiveScriptKeyValueSection;
      var Reason: TRefusalReason;
      Assert(Context.Factory.TryCreateKeyValueSection(0, KeyValueSection, Reason));
      try
        Assert(KeyValueSection.Section.Count = 0);
        KeyValueSection.Section.Add('MyMsg', 'Hello');
        Assert(AMemo.Lines.Count = 4);
        Assert(AMemo.Lines[1] = 'MyMsg=Hello');
        Assert(AMemo.Lines[2] = '[Files]');
      finally
        KeyValueSection.Free;
      end;
    finally
      Context.Free;
    end;
  end;

  { Removing keys/value lines: removing one of two leaves the other line untouched,
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
      var KeyValueSection: TLiveScriptKeyValueSection;
      var Reason: TRefusalReason;
      Assert(Context.Factory.TryCreateKeyValueSection(0, KeyValueSection, Reason));
      try
        const List = KeyValueSection.Section;
        List.Remove(List.IndexOf('AppName'));
        Assert(AMemo.Lines.Count = 4);
        Assert(AMemo.Lines[1] = 'AppVersion=1.0'); { Other directive untouched }
        Assert(AMemo.Lines[2] = '[Files]');
        List.Remove(List.IndexOf('AppVersion'));
        Assert(AMemo.Lines.Count = 3);
        Assert(AMemo.Lines[1] = '[Files]');
        { The empty range a body-less section is created with }
        Assert(KeyValueSection.LastLine < KeyValueSection.FirstLine);
        List.Add('AppComments', 'hi');
        Assert(AMemo.Lines.Count = 4);
        Assert(AMemo.Lines[1] = 'AppComments=hi');
        Assert(AMemo.Lines[2] = '[Files]');
      finally
        KeyValueSection.Free;
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
      var KeyValueSection: TLiveScriptKeyValueSection;
      var Reason: TRefusalReason;
      Assert(Context.Factory.TryCreateKeyValueSection(1, KeyValueSection, Reason));
      try
        const List = KeyValueSection.Section;
        List.Remove(List.IndexOf('AppName'));
        Assert(AMemo.Lines.Count = 3);
        Assert(AMemo.Lines[2] = '[Setup]');
        Assert(KeyValueSection.LastLine < KeyValueSection.FirstLine);
        List.Add('AppName', 'Bar');
        Assert(AMemo.Lines.Count = 4);
        Assert(AMemo.Lines[3] = 'AppName=Bar');
      finally
        KeyValueSection.Free;
      end;
    finally
      Context.Free;
    end;
  end;
end;

{ The name-plus-index-hint reads and writes of TLiveScriptKeyValueSection:
  reads, and writes with their add-when-absent rules, which unlike the
  parameter section ones also involve the compiler default }
procedure TestKeyValueSectionReadsAndWrites(const AMemo: TScintEdit;
  const AStyler: TInnoSetupStyler);
begin
  { Reads: value (with the hint resolving a duplicated key name to the right
    occurrence), flag check state, and presence }
  begin
    const Context = TFactoryTestContext.Create(AMemo, AStyler, [
      '[Setup]',
      'AppName=First',
      'AppName=Second',
      'WizardStyle=modern',
      '[Files]',
      'Source: a']);
    try
      var KeyValueSection: TLiveScriptKeyValueSection;
      var Reason: TRefusalReason;
      Assert(Context.Factory.TryCreateKeyValueSection(0, KeyValueSection, Reason));
      try
        Assert(KeyValueSection.Valid);
        Assert(KeyValueSection.GetValue('AppName', -1) = 'Second'); { With duplicate keys the last one wins }
        Assert(KeyValueSection.GetValue('AppName', 0) = 'First');   { The hint picks the first }
        Assert(KeyValueSection.GetValue('AppName', 99) = 'Second'); { A stale hint falls back to the name }
        Assert(KeyValueSection.GetValue('Missing', -1) = '');
        Assert(KeyValueSection.GetFlagCheckState('WizardStyle', -1, 'modern') = fcsAll);
        Assert(KeyValueSection.GetFlagCheckState('WizardStyle', -1, 'dark') = fcsNone);
        Assert(KeyValueSection.GetFlagCheckState('Missing', -1, 'modern') = fcsNone);
        Assert(KeyValueSection.MemberPresent('AppName', -1));
        Assert(KeyValueSection.MemberPresent('AppName', 99));
        Assert(not KeyValueSection.MemberPresent('Missing', -1));
      finally
        KeyValueSection.Free;
      end;
    finally
      Context.Free;
    end;
  end;

  { SetValue: resolve then set, else add, but only when the hint is -1 and the
    value is non-empty and not the compiler default }
  begin
    const Context = TFactoryTestContext.Create(AMemo, AStyler, [
      '[Setup]',
      'AppName=My App',
      '[Files]',
      'Source: a']);
    try
      var KeyValueSection: TLiveScriptKeyValueSection;
      var Reason: TRefusalReason;
      Assert(Context.Factory.TryCreateKeyValueSection(0, KeyValueSection, Reason));
      try
        KeyValueSection.SetValue('AppName', -1, 'Edited');
        Assert(AMemo.Lines[1] = 'AppName=Edited');
        KeyValueSection.SetValue('AppVersion', 99, '2.0'); { A stale real hint must not add }
        KeyValueSection.SetValue('AppVersion', -1, '');    { Neither must an empty value }
        KeyValueSection.SetValue('AllowCancelDuringInstall', -1, 'yes'); { Nor a value equal to the compiler default }
        Assert(AMemo.Lines[2] = '[Files]');
        KeyValueSection.SetValue('AllowCancelDuringInstall', -1, 'no');
        Assert(AMemo.Lines[2] = 'AllowCancelDuringInstall=no');
        Assert(AMemo.Lines[3] = '[Files]');
      finally
        KeyValueSection.Free;
      end;
    finally
      Context.Free;
    end;
  end;

  { SetFlag: include and exclude on a present key, the add-when-absent rule
    on include, which seeds the new key with the compiler default, and Remove }
  begin
    const Context = TFactoryTestContext.Create(AMemo, AStyler, [
      '[Setup]',
      'WizardStyle=modern',
      '[Files]',
      'Source: a']);
    try
      var KeyValueSection: TLiveScriptKeyValueSection;
      var Reason: TRefusalReason;
      Assert(Context.Factory.TryCreateKeyValueSection(0, KeyValueSection, Reason));
      try
        KeyValueSection.SetFlag('WizardStyle', -1, 'dark', True);
        Assert(AMemo.Lines[1] = 'WizardStyle=modern dark');
        KeyValueSection.SetFlag('WizardStyle', -1, 'dark', False);
        Assert(AMemo.Lines[1] = 'WizardStyle=modern');
        KeyValueSection.Remove('WizardStyle', -1);
        Assert(AMemo.Lines[1] = '[Files]');
        KeyValueSection.Remove('Missing', -1); { Removing an absent key is a no-op }
        KeyValueSection.SetFlag('WizardStyle', 99, 'dark', True); { A stale real hint must not add }
        KeyValueSection.SetFlag('WizardStyle', -1, 'dark', False); { Excluding without the key is a no-op }
        Assert(AMemo.Lines.Count = 3);
        KeyValueSection.SetFlag('WizardStyle', -1, 'dark', True); { Adds the key first, seeded with the 'classic' default }
        Assert(AMemo.Lines[1] = 'WizardStyle=classic dark');
        Assert(AMemo.Lines[2] = '[Files]');
        AMemo.Undo; { The add plus the include is a single undo action }
        Assert(AMemo.Lines.Count = 3);
        Assert(AMemo.Lines[1] = '[Files]');
        { Don't edit more here: the undo made the section's model stale }
      finally
        KeyValueSection.Free;
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
      var Entries: TLiveScriptParameterSectionEntries;
      var Reason: TRefusalReason;
      Assert(Factory.TryCreateParameterSectionEntries([], [], 1, Entries, Reason));
      try
        Assert(Entries.PrimaryFirstLine = 1);
        Assert(Entries.PrimaryLastLine = 1);
        const ChangeCountBefore = Factory.ChangeCount;
        AMemo.ReplaceTextRange(0, 0, 'X' + EOL); { Insert a line at the top }
        Assert(Factory.ChangeCount > ChangeCountBefore);
        Assert(Entries.Valid);
        Assert(Entries.PrimaryFirstLine = 2);
        Assert(Entries.PrimaryLastLine = 2);
        Assert(Factory.SectionCount = 1);
        Assert(Factory.SectionHeaders[0].Line = 1); { [Files] header shifted down }
      finally
        Entries.Free;
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
      var Entries: TLiveScriptParameterSectionEntries;
      var Reason: TRefusalReason;
      Assert(Factory.TryCreateParameterSectionEntries([], [], 2, Entries, Reason));
      try
        Assert(Entries.PrimaryFirstLine = 2);
        AMemo.ReplaceTextRange(AMemo.GetPositionFromLine(2),
          AMemo.GetPositionFromLine(3), ''); { Delete line 2 }
        Assert(not Entries.Valid);
      finally
        Entries.Free;
      end;
      { Line 2 now holds the former line 3 and is still parseable }
      var NewEntries: TLiveScriptParameterSectionEntries;
      Assert(Factory.TryCreateParameterSectionEntries([], [], 2, NewEntries, Reason));
      try
        var Value: String;
        Assert(NewEntries.PrimaryEntry.TryGetValue('Source', Value) and (Value = 'c.txt'));
      finally
        NewEntries.Free;
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
      var Entries: TLiveScriptParameterSectionEntries;
      var Reason: TRefusalReason;
      Assert(Factory.TryCreateParameterSectionEntries([], [], 1, Entries, Reason));
      try
        const SplitPos = AMemo.GetPositionFromLine(1) + Length('Source: "a.txt";');
        AMemo.ReplaceTextRange(SplitPos, SplitPos, EOL);
        Assert(AMemo.Lines.Count = 3);
        Assert(Entries.Valid);
        Assert(Entries.PrimaryFirstLine = 1);
        Assert(Entries.PrimaryLastLine = 2);
        Entries.PrimaryEntry.SetValue(1, '{tmp}');
        Assert(AMemo.Lines.Count = 2);
        Assert(AMemo.Lines[1] = 'Source: "a.txt"; DestDir: "{tmp}"');
      finally
        Entries.Free;
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
      var Entries: TLiveScriptParameterSectionEntries;
      var Reason: TRefusalReason;
      Assert(Factory.TryCreateParameterSectionEntries([], [], 1, Entries, Reason));
      try
        Assert(Entries.PrimaryFirstLine = 1);
        Assert(Entries.PrimaryLastLine = 2);
        const SplitPos = AMemo.GetPositionFromLine(2) + Length('  DestDir: "{app}";');
        AMemo.ReplaceTextRange(SplitPos, SplitPos, EOL);
        Assert(AMemo.Lines.Count = 4);
        Assert(Entries.Valid);
        Assert(Entries.PrimaryFirstLine = 1);
        Assert(Entries.PrimaryLastLine = 3);
        Entries.PrimaryEntry.SetValue(1, '{tmp}');
        Assert(AMemo.Lines.Count = 3);
        Assert(AMemo.Lines[1] = 'Source: "a.txt"; \');
        Assert(AMemo.Lines[2] = '  DestDir: "{tmp}"; Flags: ignoreversion');
      finally
        Entries.Free;
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
      var Entries: TLiveScriptParameterSectionEntries;
      var Reason: TRefusalReason;
      Assert(Factory.TryCreateParameterSectionEntries([], [], 1, Entries, Reason));
      try
        const Pos = AMemo.GetPositionFromLine(2);
        AMemo.ReplaceTextRange(Pos, Pos, 'Source: "new.txt"' + EOL);
        Assert(Entries.Valid);
        Assert(Entries.PrimaryFirstLine = 1);
        Assert(Entries.PrimaryLastLine = 1);
      finally
        Entries.Free;
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
      var Entries: TLiveScriptParameterSectionEntries;
      var Reason: TRefusalReason;
      Assert(Factory.TryCreateParameterSectionEntries([], [], 1, Entries, Reason));
      try
        AMemo.ReplaceTextRange(AMemo.GetLineEndPosition(1),
          AMemo.GetPositionFromLine(2), ''); { Join line 2 into line 1 }
        Assert(not Entries.Valid);
      finally
        Entries.Free;
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
      Assert(Factory.SectionHeaders[1].Section = scIcons);
      Assert(Factory.SectionHeaders[1].Line = 2);
      Assert(Factory.SectionHeaders[2].Section = scFiles);
      Assert(Factory.SectionHeaders[2].Line = 3);
      AMemo.ReplaceTextRange(AMemo.GetPositionFromLine(2),
        AMemo.GetPositionFromLine(3), ''); { Remove the header again }
      Assert(Factory.SectionCount = 2);
      Assert(Factory.SectionHeaders[1].Section = scFiles);
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
      var Entries: TLiveScriptParameterSectionEntries;
      var Reason: TRefusalReason;
      Assert(Factory.TryCreateParameterSectionEntries([], [], 1, Entries, Reason));
      try
        Assert(Entries.Valid);
        Factory.InvalidateIndex;
        Assert(not Entries.Valid);
      finally
        Entries.Free;
      end;
    finally
      Context.Free;
    end;
  end;
end;

procedure IDELiveScriptObjectFactoryRunTests(const AMemo: TScintEdit;
  const AStyler: TInnoSetupStyler);
begin
  Assert(AMemo.Lines.Text = '');

  const SavedOnChange = AMemo.OnChange;
  const SavedStyler = AMemo.Styler;
  const SavedReadOnly = AMemo.ReadOnly;
  try
    TestSectionIndexing(AMemo, AStyler);
    TestTryGetSectionAtLine(AMemo, AStyler);
    TestTryCreateParameterSectionEntries(AMemo, AStyler);
    TestTryCreateParameterSectionEntriesFromLineRanges(AMemo, AStyler);
    TestEntryRoundTrip(AMemo, AStyler);
    TestParameterSectionEntries(AMemo, AStyler);
    TestParameterSectionEntriesMultiEntry(AMemo, AStyler);
    TestCollectParameterValues(AMemo, AStyler);
    TestKeyValueSections(AMemo, AStyler);
    TestKeyValueSectionReadsAndWrites(AMemo, AStyler);
    TestEditTracking(AMemo, AStyler);
  finally
    AMemo.OnChange := nil;
    AMemo.Styler := SavedStyler;
    AMemo.Lines.Text := '';
    AMemo.ClearUndo;
    AMemo.ReadOnly := SavedReadOnly;
    AMemo.OnChange := SavedOnChange;
  end;
end;

end.
