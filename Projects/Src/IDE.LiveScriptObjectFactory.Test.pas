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
  IDE.ScriptModel, IDE.ScriptModel.Metadata, IDE.LiveScriptObjectFactory;

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

  procedure AssertSection(const ASection: TLiveScriptSection;
    const AExpectedSection: TInnoSetupStylerSection; const AExpectedLine: Integer;
    const AExpectedName: String);
  begin
    Assert(ASection.Section = AExpectedSection);
    Assert(ASection.Line = AExpectedLine);
    Assert(ASection.Name = AExpectedName);
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
    AssertSection(Factory.Sections[0], scSetup, 0, 'Setup');
    AssertSection(Factory.Sections[1], scFiles, 2, 'Files');
    AssertSection(Factory.Sections[2], scSetup, 4, 'Setup'); { Duplicate kept }
    AssertSection(Factory.Sections[3], scCode, 6, 'Code');
    AssertSection(Factory.Sections[4], scThirdParty, 8, '_ThirdParty');
    AssertSection(Factory.Sections[5], scUnknown, 10, 'Bogus');
    { The spanned header at line 12 is a single section whose continuation
      (line 13) is not itself indexed }
    AssertSection(Factory.Sections[6], scFiles, 12, 'Files');
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

{ Edit tracking: the factory only learns of edits through Change }
procedure TestEditTracking(const AMemo: TScintEdit;
  const AStyler: TInnoSetupStyler);
begin
  const EOL = String(AMemo.LineEndingString);

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
      Assert(Factory.Sections[1].Section = scIcons);
      Assert(Factory.Sections[1].Line = 2);
      Assert(Factory.Sections[2].Section = scFiles);
      Assert(Factory.Sections[2].Line = 3);
      AMemo.ReplaceTextRange(AMemo.GetPositionFromLine(2),
        AMemo.GetPositionFromLine(3), ''); { Remove the header again }
      Assert(Factory.SectionCount = 2);
      Assert(Factory.Sections[1].Section = scFiles);
      Assert(Factory.Sections[1].Line = 2);
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
