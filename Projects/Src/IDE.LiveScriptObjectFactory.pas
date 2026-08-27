unit IDE.LiveScriptObjectFactory;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Factory of live script objects: attaches to one TScintEdit and its
  TInnoSetupStyler and hands out IDE.ScriptModel-backed objects for a range
  of lines of the script in the memo, with their changes written back to the
  memo immediately. Does not keep anything else alive in memory, except a
  small section index, always up-to-date. So besides being a factory like
  the name says, it is actually also a section index.
}

interface

uses
  Classes, Generics.Collections,
  ScintEdit,
  IDE.ScintStylerInnoSetup, IDE.ScriptModel, IDE.ScriptModel.Metadata,
  IDE.ScriptModel.Metadata.Extra;

type
  TLiveScriptObjectFactory = class;

  { Why TryCreateParameterSectionEntries, TryCreateKeyValueSection, or
    TryAcquireCodeSection refused to create an object }
  TRefusalReason = (rrLineOutOfRange, rrNotInsideSection,
    rrInCodeSection, rrUnrecognizedSection, rrNotParameterSection, rrComment,
    rrISPPDirective, rrMixedSelection, rrSectionIndexOutOfRange,
    rrNotKeyValueSection, rrNotCodeSection);

  TLiveScriptSectionHeader = record
    Line: Integer;
    Section: TInnoSetupSection;
    Name: String;
  end;

  TLiveScriptObject = class
  private
    FFactory: TLiveScriptObjectFactory;
    FFirstLine, FLastLine: Integer; { The lines for which the object was created,
      always up-to-date. An edit inside them still makes the parsed content
      stale. Use the factory's ChangeCount to detect this. Without Change's
      updates, a write-back which inserts a line during multi-entry editing
      would make the entries below it write back at the wrong lines }
    FValid: Boolean; { False if some or all of the object's lines were deleted since creation }
    FParseTimeMilliseconds: Double;
    constructor Create(const AFactory: TLiveScriptObjectFactory; const AFirstLine,
      ALastLine: Integer);
  public
    destructor Destroy; override;
    property FirstLine: Integer read FFirstLine;
    property LastLine: Integer read FLastLine;
    property ParseTimeMilliseconds: Double read FParseTimeMilliseconds;
    property Valid: Boolean read FValid;
  end;

  { An entry of a parameter section }
  TLiveScriptParameterSectionEntry = class(TLiveScriptObject)
  private
    FEntry: TScriptModelParameterSectionEntry;
    FSection: TInnoSetupSection;
    FCreatedFromBlankLine: Boolean;
    constructor Create(const AFactory: TLiveScriptObjectFactory; const AFirstLine,
      ALastLine: Integer; const ASection: TInnoSetupSection;
      const AMetadata: TScriptModelSectionMetadata; const ALines: TArray<String>;
      const ACreatedFromBlankLine: Boolean);
    procedure OnChange(Sender: TObject);
  public
    destructor Destroy; override;
    property Entry: TScriptModelParameterSectionEntry read FEntry;
    property Section: TInnoSetupSection read FSection;
  end;

  TLiveScriptFlagCheckState = (fcsNone, fcsAll, fcsSome);

  { One or more entries of one parameter section }
  TLiveScriptParameterSectionEntries = class
  private
    FItems: TObjectList<TLiveScriptParameterSectionEntry>;
    procedure Add(const AEntry: TLiveScriptParameterSectionEntry);
    procedure BeginUndoAction;
    procedure EndUndoAction;
    function GetCount: Integer;
    function GetEntry(Index: Integer): TLiveScriptParameterSectionEntry;
    function GetParseTimeMilliseconds: Double;
    function GetPrimaryEntry: TScriptModelParameterSectionEntry;
    function GetPrimaryFirstLine: Integer;
    function GetPrimaryLastLine: Integer;
    function GetSection: TInnoSetupSection;
    function GetValid: Boolean;
    procedure SetQuoteNewValues(const Value: Boolean);
  public
    constructor Create(const APrimaryEntry: TLiveScriptParameterSectionEntry);
    destructor Destroy; override;
    { Reads and writes addressing a parameter by name plus an index hint,
      applied per entry. Reads aggregate over all entries, writes change
      every entry in one undo action. }
    function GetValue(const AName: String; const AIndexHint: Integer): String;
    function GetFlagCheckState(const AParameterName: String;
      const AIndexHint: Integer; const AFlagName: String): TLiveScriptFlagCheckState;
    function MemberPresent(const AName: String; const AIndexHint: Integer): Boolean;
    procedure SetValue(const AName: String; const AIndexHint: Integer;
      const AValue: String);
    procedure SetFlag(const AParameterName: String; const AIndexHint: Integer;
      const AFlagName: String; const AInclude: Boolean);
    procedure Remove(const AName: String; const AIndexHint: Integer);
    property Count: Integer read GetCount;
    property Entries[Index: Integer]: TLiveScriptParameterSectionEntry read GetEntry;
    { The entries' parse times added up }
    property ParseTimeMilliseconds: Double read GetParseTimeMilliseconds;
    { The first entry's model. Metadata is common to all entries, so definition
      lookups for any entry can use it. }
    property PrimaryEntry: TScriptModelParameterSectionEntry read GetPrimaryEntry;
    property PrimaryFirstLine: Integer read GetPrimaryFirstLine;
    property PrimaryLastLine: Integer read GetPrimaryLastLine;
    property QuoteNewValues: Boolean write SetQuoteNewValues;
    property Section: TInnoSetupSection read GetSection;
    property Valid: Boolean read GetValid;
  end;

  { A single occurrence of a key/value section }
  TLiveScriptKeyValueSection = class(TLiveScriptObject)
  private
    FSection: TScriptModelKeyValueSection;
    constructor Create(const AFactory: TLiveScriptObjectFactory; const AFirstLine,
      ALastLine: Integer; const AMetadata: TScriptModelSectionMetadata;
      const ALines: TArray<String>);
    procedure OnChange(Sender: TObject);
    procedure SetQuoteNewValues(const Value: Boolean);
  public
    destructor Destroy; override;
    { Reads and writes addressing a key by name plus an index hint. Same set
      as TLiveScriptParameterSectionEntries has, but working on the single
      section, so GetFlagCheckState never returns fcsSome. }
    function GetValue(const AName: String; const AIndexHint: Integer): String;
    function GetFlagCheckState(const AKeyName: String;
      const AIndexHint: Integer; const AFlagName: String): TLiveScriptFlagCheckState;
    function MemberPresent(const AName: String; const AIndexHint: Integer): Boolean;
    procedure SetValue(const AName: String; const AIndexHint: Integer;
      const AValue: String);
    procedure SetFlag(const AKeyName: String; const AIndexHint: Integer;
      const AFlagName: String; const AInclude: Boolean);
    procedure Remove(const AName: String; const AIndexHint: Integer);
    property QuoteNewValues: Boolean write SetQuoteNewValues;
    property Section: TScriptModelKeyValueSection read FSection;
  end;

  { A single occurrence of a [Code] section. Read-only. Shared: handed out by
    the factory's TryAcquireCodeSection and returned with ReleaseAndNil. }
  TLiveScriptCodeSection = class(TLiveScriptObject)
  private
    FSection: TScriptModelCodeSection;
    FSectionIndex: Integer; { Factory section index it was created for }
    FChangeCountAtParse: Int64; { The factory's ChangeCount when the object parsed }
    FAcquireCount: Integer;
    constructor Create(const AFactory: TLiveScriptObjectFactory;
      const ASectionIndex, AFirstLine, ALastLine: Integer;
      const ALines: TArray<String>);
  public
    destructor Destroy; override;
    function TryGetRoutine(const AMemoLine: Integer;
      out ARoutine: TCodeSectionRoutine): Boolean;
    property Section: TScriptModelCodeSection read FSection;
  end;

  TLiveScriptObjectFactory = class
  private
    FMemo: TScintEdit;
    FStyler: TInnoSetupStyler;
    FSectionHeaders: TList<TLiveScriptSectionHeader>; { Includes scUnknown/scThirdParty section }
    FIndexValid: Boolean;
    FDirtyFirstLine, FDirtyLastLine: Integer; { -1 when nothing is dirty, used by UpdateIndexForDirtyLines }
    FLiveScriptObjects: TList<TLiveScriptObject>;
    FWritingBackObject: TLiveScriptObject;
    FChangeCount: Int64;
    procedure EnsureIndex;
    procedure EnsureStyled;
    function GetLinesText(const AFirstLine, ALastLine: Integer): TArray<String>;
    function GetLinesTextAndClassify(const AFirstLine, ALastLine: Integer;
      out ALineKind: TScriptLineKind; out AJoinedText: String): TArray<String>; overload;
    function GetLinesTextAndClassify(const AFirstLine, ALastLine: Integer;
      out ALineKind: TScriptLineKind): TArray<String>; overload;
    function GetLogicalLineFirstLine(const ALine: Integer): Integer;
    function GetLogicalLineLastLine(const ALine: Integer): Integer;
    function GetSectionBodyLines(const ASectionIndex: Integer;
      out AFirstLine, ALastLine: Integer): TArray<String>;
    function GetSectionHeader(Index: Integer): TLiveScriptSectionHeader;
    procedure GetSectionLines(const ASectionIndex: Integer;
      out AFirstLine, ALastLine: Integer);
    function LineSpans(const ALine: Integer): Boolean;
    function TryGetSectionForCreation(const ASectionIndex: Integer;
      out ASection: TInnoSetupSection; out ARefusalReason: TRefusalReason): Boolean;
    procedure WriteBackChange(const ALiveScriptObject: TLiveScriptObject;
      const ALines: TArray<String>; const ACreatedFromBlankLine: Boolean = False);
  public
    constructor Create(const AMemo: TScintEdit; const AStyler: TInnoSetupStyler);
    destructor Destroy; override;
    procedure Change(const Info: TScintEditChangeInfo);
    procedure Reset;
    function SectionCount: Integer;
    function TryGetSectionAtLine(const ALine: Integer;
      out ASectionIndex: Integer): Boolean;
    function GetSectionFirstSignificantLine(const ASectionIndex: Integer): Integer;
    procedure GetSectionOccurrence(const ASectionIndex: Integer;
      out AOccurrenceIndex, AOccurrenceCount: Integer);
    function TryGetSetupDirectiveValue(const ADirectiveName: String;
      out AValue: String): Boolean;
    procedure CollectParameterValues(const AOnlySection: TInnoSetupSection;
      const AParameterName: String; const AValues: TStringList;
      const ASplitValueWords: Boolean = False);
    { ARefusalReason is only set when the result is False }
    function TryCreateParameterSectionEntries(
      const ALineRanges, AIndividualLineRanges: TArray<TScintLineRange>;
      const ACaretLine: Integer;
      out AEntries: TLiveScriptParameterSectionEntries;
      out ARefusalReason: TRefusalReason): Boolean;
    function TryCreateKeyValueSection(const ASectionIndex: Integer;
      out ASection: TLiveScriptKeyValueSection;
      out ARefusalReason: TRefusalReason): Boolean;
    function TryAcquireCodeSection(const ASectionIndex: Integer;
      out ASection: TLiveScriptCodeSection; { Always use ReleaseAndNil when done }
      out ARefusalReason: TRefusalReason): Boolean; overload;
    function TryAcquireCodeSection(const ASectionIndex: Integer;
      out ASection: TLiveScriptCodeSection): Boolean; overload;
    class procedure ReleaseAndNil(var ASection: TLiveScriptCodeSection); static;
    { Bumped on every Change and Reset call, so a consumer can tell whether
      the memo changed since it last read something. The memo being changed
      does not mean each live object's parsed content is stale, but that
      staleness isn't tracked because it wouldn't help much: consumers also
      keep data from outside their objects at creation, such as the object's
      section index, so any change anywhere will make them want to rebuild,
      even with exact tracking. }
    property ChangeCount: Int64 read FChangeCount;
    property Memo: TScintEdit read FMemo;
    property SectionHeaders[Index: Integer]: TLiveScriptSectionHeader read GetSectionHeader;
    property Styler: TInnoSetupStyler read FStyler;
  end;

function CollectParameterValuesFromFactories(
  const AFactories: array of TLiveScriptObjectFactory;
  const AParameterName: String): TArray<String>;

implementation

uses
  SysUtils, Diagnostics,
  PathFunc,
  Shared.CommonFunc;

{ TLiveScriptObject }

constructor TLiveScriptObject.Create(const AFactory: TLiveScriptObjectFactory;
  const AFirstLine, ALastLine: Integer);
begin
  inherited Create;
  FFactory := AFactory;
  FFirstLine := AFirstLine;
  FLastLine := ALastLine;
  FValid := True;
  FFactory.FLiveScriptObjects.Add(Self);
end;

destructor TLiveScriptObject.Destroy;
begin
  if FFactory <> nil then
    FFactory.FLiveScriptObjects.Remove(Self);
  inherited;
end;

{ TLiveScriptParameterSectionEntry }

constructor TLiveScriptParameterSectionEntry.Create(const AFactory: TLiveScriptObjectFactory;
  const AFirstLine, ALastLine: Integer; const ASection: TInnoSetupSection;
  const AMetadata: TScriptModelSectionMetadata; const ALines: TArray<String>;
  const ACreatedFromBlankLine: Boolean);
begin
  inherited Create(AFactory, AFirstLine, ALastLine);
  FSection := ASection;
  FCreatedFromBlankLine := ACreatedFromBlankLine;
  FEntry := TScriptModelParameterSectionEntry.Create(AMetadata);
  const Stopwatch = TStopwatch.StartNew;
  FEntry.Parse(ALines);
  FParseTimeMilliseconds := Stopwatch.Elapsed.TotalMilliseconds;
  FEntry.OnChange := OnChange;
end;

destructor TLiveScriptParameterSectionEntry.Destroy;
begin
  FEntry.Free;
  inherited;
end;

procedure TLiveScriptParameterSectionEntry.OnChange(Sender: TObject);
begin
  if (FFactory <> nil) and FValid then begin
    FFactory.WriteBackChange(Self, FEntry.GetLines, FCreatedFromBlankLine);
    FCreatedFromBlankLine := False; { An entry created from a blank line inserts itself above that line }
  end;
end;

{ TLiveScriptParameterSectionEntries }

constructor TLiveScriptParameterSectionEntries.Create(
  const APrimaryEntry: TLiveScriptParameterSectionEntry);
begin
  inherited Create;
  FItems := TObjectList<TLiveScriptParameterSectionEntry>.Create;
  FItems.Add(APrimaryEntry);
end;

destructor TLiveScriptParameterSectionEntries.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TLiveScriptParameterSectionEntries.Add(
  const AEntry: TLiveScriptParameterSectionEntry);
begin
  FItems.Add(AEntry);
end;

procedure TLiveScriptParameterSectionEntries.BeginUndoAction;
begin
  const Factory = FItems[0].FFactory;
  if Factory <> nil then
    Factory.Memo.BeginUndoAction;
end;

procedure TLiveScriptParameterSectionEntries.EndUndoAction;
begin
  const Factory = FItems[0].FFactory;
  if Factory <> nil then
    Factory.Memo.EndUndoAction;
end;

function TLiveScriptParameterSectionEntries.GetValue(const AName: String;
  const AIndexHint: Integer): String;
{ Returns the value only when every entry has the parameter with the exact
  same value; otherwise returns '' }
begin
  Result := '';
  if not Valid then
    Exit;
  var First := True;
  for var LiveEntry in FItems do begin
    var Index := AIndexHint;
    if not LiveEntry.Entry.TryResolve(AName, Index) then
      Exit('');
    const Value = LiveEntry.Entry.Parameters[Index].Value;
    if First then begin
      Result := Value;
      First := False;
    end else if Value <> Result then
      Exit('');
  end;
end;

function TLiveScriptParameterSectionEntries.GetFlagCheckState(
  const AParameterName: String; const AIndexHint: Integer;
  const AFlagName: String): TLiveScriptFlagCheckState;
{ An entry missing the parameter counts as the flag being excluded }
begin
  Result := fcsNone;
  if not Valid then
    Exit;
  var IncludedCount := 0;
  for var LiveEntry in FItems do begin
    var Index := AIndexHint;
    if LiveEntry.Entry.TryResolve(AParameterName, Index) and
       LiveEntry.Entry.FlagIncluded(Index, AFlagName) then
      Inc(IncludedCount);
  end;
  if IncludedCount = Count then
    Result := fcsAll
  else if IncludedCount > 0 then
    Result := fcsSome;
end;

function TLiveScriptParameterSectionEntries.MemberPresent(const AName: String;
  const AIndexHint: Integer): Boolean;
{ True when the parameter is present in at least one entry }
begin
  Result := False;
  if not Valid then
    Exit;
  for var LiveEntry in FItems do begin
    var Index := AIndexHint;
    if LiveEntry.Entry.TryResolve(AName, Index) then
      Exit(True);
  end;
end;

procedure TLiveScriptParameterSectionEntries.SetValue(const AName: String;
  const AIndexHint: Integer; const AValue: String);
{ Sets the parameter's value in every entry }
begin
  if not Valid then
    Exit;
  BeginUndoAction;
  try
    for var LiveEntry in FItems do begin
      var Index := AIndexHint;
      if LiveEntry.Entry.TryResolve(AName, Index) then
        LiveEntry.Entry.SetValue(Index, AValue)
      else if (AIndexHint < 0) and (AValue <> '') then
        LiveEntry.Entry.Add(AName, AValue);
    end;
  finally
    EndUndoAction;
  end;
end;

procedure TLiveScriptParameterSectionEntries.SetFlag(const AParameterName: String;
  const AIndexHint: Integer; const AFlagName: String; const AInclude: Boolean);
{ Sets the flag in every entry, which may adjust related flags as well. If the
  parameter is not present it is first added if AInclude is True. }
begin
  if not Valid then
    Exit;
  BeginUndoAction;
  try
    for var LiveEntry in FItems do begin
      var Index := AIndexHint;
      if LiveEntry.Entry.TryResolve(AParameterName, Index) then
        LiveEntry.Entry.SetFlag(Index, AFlagName, AInclude)
      else if (AIndexHint < 0) and AInclude then
        LiveEntry.Entry.SetFlag(LiveEntry.Entry.Add(AParameterName, ''), AFlagName, True);
    end;
  finally
    EndUndoAction;
  end;
end;

procedure TLiveScriptParameterSectionEntries.Remove(const AName: String;
  const AIndexHint: Integer);
{ Removes the parameter from every entry where it is present }
begin
  if not Valid then
    Exit;
  BeginUndoAction;
  try
    for var LiveEntry in FItems do begin
      var Index := AIndexHint;
      if LiveEntry.Entry.TryResolve(AName, Index) then
        LiveEntry.Entry.Remove(Index);
    end;
  finally
    EndUndoAction;
  end;
end;

function TLiveScriptParameterSectionEntries.GetCount: Integer;
begin
  Result := Integer(FItems.Count);
end;

function TLiveScriptParameterSectionEntries.GetEntry(
  Index: Integer): TLiveScriptParameterSectionEntry;
begin
  Result := FItems[Index];
end;

function TLiveScriptParameterSectionEntries.GetParseTimeMilliseconds: Double;
begin
  Result := 0;
  for var LiveEntry in FItems do
    Result := Result + LiveEntry.ParseTimeMilliseconds;
end;

function TLiveScriptParameterSectionEntries.GetPrimaryEntry: TScriptModelParameterSectionEntry;
begin
  Result := FItems[0].Entry;
end;

function TLiveScriptParameterSectionEntries.GetPrimaryFirstLine: Integer;
begin
  Result := FItems[0].FirstLine;
end;

function TLiveScriptParameterSectionEntries.GetPrimaryLastLine: Integer;
begin
  Result := FItems[0].LastLine;
end;

function TLiveScriptParameterSectionEntries.GetSection: TInnoSetupSection;
begin
  Result := FItems[0].Section;
end;

function TLiveScriptParameterSectionEntries.GetValid: Boolean;
begin
  for var LiveEntry in FItems do
    if not LiveEntry.Valid then
      Exit(False);
  Result := True;
end;

procedure TLiveScriptParameterSectionEntries.SetQuoteNewValues(const Value: Boolean);
begin
  for var LiveEntry in FItems do
    LiveEntry.Entry.QuoteNewValues := Value;
end;

{ TLiveScriptKeyValueSection }

constructor TLiveScriptKeyValueSection.Create(const AFactory: TLiveScriptObjectFactory;
  const AFirstLine, ALastLine: Integer; const AMetadata: TScriptModelSectionMetadata;
  const ALines: TArray<String>);
begin
  inherited Create(AFactory, AFirstLine, ALastLine);
  FSection := TScriptModelKeyValueSection.Create(AMetadata);
  const Stopwatch = TStopwatch.StartNew;
  FSection.Parse(ALines);
  FParseTimeMilliseconds := Stopwatch.Elapsed.TotalMilliseconds;
  FSection.OnChange := OnChange;
end;

destructor TLiveScriptKeyValueSection.Destroy;
begin
  FSection.Free;
  inherited;
end;

procedure TLiveScriptKeyValueSection.OnChange(Sender: TObject);
begin
  if (FFactory <> nil) and FValid then
    FFactory.WriteBackChange(Self, FSection.GetLines);
end;

function TLiveScriptKeyValueSection.GetValue(const AName: String;
  const AIndexHint: Integer): String;
{ Returns the key's value, or '' when the key is not present }
begin
  Result := '';
  if not Valid then
    Exit;
  var Index := AIndexHint;
  if FSection.TryResolve(AName, Index) then
    Result := FSection.Lines[Index].Value;
end;

function TLiveScriptKeyValueSection.GetFlagCheckState(const AKeyName: String;
  const AIndexHint: Integer; const AFlagName: String): TLiveScriptFlagCheckState;
{ A missing key counts as the flag being excluded }
begin
  Result := fcsNone;
  if not Valid then
    Exit;
  var Index := AIndexHint;
  if FSection.TryResolve(AKeyName, Index) and
     FSection.FlagIncluded(Index, AFlagName) then
    Result := fcsAll;
end;

function TLiveScriptKeyValueSection.MemberPresent(const AName: String;
  const AIndexHint: Integer): Boolean;
{ True when the key is present }
begin
  var Index := AIndexHint;
  Result := Valid and FSection.TryResolve(AName, Index);
end;

procedure TLiveScriptKeyValueSection.SetValue(const AName: String;
  const AIndexHint: Integer; const AValue: String);
{ Sets the key's value, adding the key when not present, unless the new value
  is empty or (case-sensitively) same as the compiler default }
begin
  if not Valid then
    Exit;
  var Index := AIndexHint;
  if FSection.TryResolve(AName, Index) then
    FSection.SetValue(Index, AValue)
  else if (AIndexHint < 0) and (AValue <> '') and
          (AValue <> FSection.DefaultValue(AName)) then
    FSection.Add(AName, AValue);
end;

procedure TLiveScriptKeyValueSection.SetFlag(const AKeyName: String;
  const AIndexHint: Integer; const AFlagName: String; const AInclude: Boolean);
{ Sets the flag, which may adjust related flags as well. If the key is not
  present it is first added if AInclude is True, using the compiler default.
  Excluding a flag of a key not present is ignored: no valid script text can
  be written for that (currently applies only to WizardStyle defaulting to
  'classic'). }
begin
  if not Valid then
    Exit;
  var Index := AIndexHint;
  if FSection.TryResolve(AKeyName, Index) then
    FSection.SetFlag(Index, AFlagName, AInclude)
  else if (AIndexHint < 0) and AInclude then begin
    { Group Add's and SetFlag's writes into a single undo action }
    if FFactory <> nil then
      FFactory.Memo.BeginUndoAction;
    try
      FSection.SetFlag(FSection.Add(AKeyName, FSection.DefaultValue(AKeyName)),
        AFlagName, True);
    finally
      if FFactory <> nil then
        FFactory.Memo.EndUndoAction;
    end;
  end;
end;

procedure TLiveScriptKeyValueSection.Remove(const AName: String;
  const AIndexHint: Integer);
{ Removes the key if present }
begin
  if not Valid then
    Exit;
  var Index := AIndexHint;
  if FSection.TryResolve(AName, Index) then
    FSection.Remove(Index);
end;

procedure TLiveScriptKeyValueSection.SetQuoteNewValues(const Value: Boolean);
begin
  FSection.QuoteNewValues := Value;
end;

{ TLiveScriptCodeSection }

constructor TLiveScriptCodeSection.Create(const AFactory: TLiveScriptObjectFactory;
  const ASectionIndex, AFirstLine, ALastLine: Integer;
  const ALines: TArray<String>);
begin
  inherited Create(AFactory, AFirstLine, ALastLine);
  FSectionIndex := ASectionIndex;
  FChangeCountAtParse := AFactory.FChangeCount;
  FSection := TScriptModelCodeSection.Create;
  const Stopwatch = TStopwatch.StartNew;
  FSection.Parse(ALines);
  FParseTimeMilliseconds := Stopwatch.Elapsed.TotalMilliseconds;
end;

destructor TLiveScriptCodeSection.Destroy;
begin
  FSection.Free;
  inherited;
end;

function TLiveScriptCodeSection.TryGetRoutine(const AMemoLine: Integer;
  out ARoutine: TCodeSectionRoutine): Boolean;
begin
  ARoutine := nil;
  Result := Valid and FSection.TryGetRoutine(AMemoLine - FFirstLine, ARoutine);
end;

{ TLiveScriptObjectFactory }

constructor TLiveScriptObjectFactory.Create(const AMemo: TScintEdit;
  const AStyler: TInnoSetupStyler);
begin
  inherited Create;
  FMemo := AMemo;
  FStyler := AStyler;
  FSectionHeaders := TList<TLiveScriptSectionHeader>.Create;
  FLiveScriptObjects := TList<TLiveScriptObject>.Create;
  FDirtyFirstLine := -1;
  FDirtyLastLine := -1;
end;

destructor TLiveScriptObjectFactory.Destroy;
begin
  if FLiveScriptObjects <> nil then
    for var LiveScriptObject in FLiveScriptObjects do
      LiveScriptObject.FFactory := nil;
  FLiveScriptObjects.Free;
  FSectionHeaders.Free;
  inherited;
end;

function TLiveScriptObjectFactory.LineSpans(const ALine: Integer): Boolean;
begin
  Result := TInnoSetupStyler.LineSpans(FMemo.Lines.RawLines[ALine]);
end;

procedure TLiveScriptObjectFactory.EnsureIndex;

  function ExtractSectionHeaderName(const S: String): String;
  begin
    { See TInnoSetupStyler.StyleNeeded }
    Result := '';
    const P = Pos('[', S);
    if P = 0 then
      Exit;
    var I := P+1;
    while (I <= Length(S)) and CharInSet(S[I], AlphaUnderscoreChars) do
      Inc(I);
    if (I <= Length(S)) and (S[I] = ']') then
      Result := Copy(S, P+1, I-P-1);
  end;

  function TryGetSectionHeader(const ALine: Integer;
    out ASectionHeader: TLiveScriptSectionHeader): Boolean;
  begin
    { ISPP's line continuation (see LineSpans) joins physical lines into one
      logical line, and the styler gives them all the same line state. This
      also applies to spanned headers, regardless of the fact that those
      don't compile. There's no detection for this issue and callers must
      just pass only the first physical line of a spanned header. }
    var Section: TInnoSetupSection;
    Result := TInnoSetupStyler.LineSectionHeader(FMemo.Lines.State[ALine], Section);
    if Result then begin
      ASectionHeader.Line := ALine;
      ASectionHeader.Section := Section;
      ASectionHeader.Name := ExtractSectionHeaderName(FMemo.Lines[ALine]);
    end;
  end;

  procedure BuildIndex;
  begin
    FSectionHeaders.Clear;
    EnsureStyled;
    const LineCount = FMemo.Lines.Count;
    var PreviousLineSpans := False;
    for var I := 0 to LineCount-1 do begin
      if not PreviousLineSpans then begin
        var SectionHeader: TLiveScriptSectionHeader;
        if TryGetSectionHeader(I, SectionHeader) then
          FSectionHeaders.Add(SectionHeader);
      end;
      PreviousLineSpans := LineSpans(I);
    end;
    FIndexValid := True;
    FDirtyFirstLine := -1;
    FDirtyLastLine := -1;
  end;

  procedure UpdateIndexForDirtyLines;
  begin
    var FirstLine := FDirtyFirstLine;
    var LastLine := FDirtyLastLine;
    FDirtyFirstLine := -1;
    FDirtyLastLine := -1;

    const LineCount = FMemo.Lines.Count;
    if FirstLine < 0 then
      FirstLine := 0;
    if FirstLine > LineCount-1 then
      FirstLine := LineCount-1;
    if LastLine < FirstLine then
      LastLine := FirstLine
    else if LastLine > LineCount-1 then
      LastLine := LineCount-1;

    { Extend to whole logical (spanned) lines, plus one following logical line:
      an edit can detach that line from a span without its own text being edited }
    FirstLine := GetLogicalLineFirstLine(FirstLine);
    LastLine := GetLogicalLineLastLine(LastLine);
    if LastLine < LineCount-1 then
      LastLine := GetLogicalLineLastLine(LastLine+1);

    { Restyle the affected lines to refresh their per-line section state }
    FMemo.RestyleLine(FirstLine);
    if LastLine > FirstLine then begin
      var EndPos: Integer;
      if LastLine >= LineCount-1 then
        EndPos := FMemo.RawTextLength
      else
        EndPos := FMemo.GetPositionFromLine(LastLine+1);
      FMemo.StyleNeeded(EndPos);
    end;

    { Rescan the affected lines, replacing that slice of the index }
    for var I := Integer(FSectionHeaders.Count)-1 downto 0 do
      if (FSectionHeaders[I].Line >= FirstLine) and (FSectionHeaders[I].Line <= LastLine) then
        FSectionHeaders.Delete(I);
    var InsertAt := 0;
    while (InsertAt < FSectionHeaders.Count) and (FSectionHeaders[InsertAt].Line < FirstLine) do
      Inc(InsertAt);
    var PreviousLineSpans := False; { FirstLine was extended back to the start of a group }
    for var I := FirstLine to LastLine do begin
      if not PreviousLineSpans then begin
        var SectionHeader: TLiveScriptSectionHeader;
        if TryGetSectionHeader(I, SectionHeader) then begin
          FSectionHeaders.Insert(InsertAt, SectionHeader);
          Inc(InsertAt);
        end;
      end;
      PreviousLineSpans := LineSpans(I);
    end;
  end;

begin
  if not FIndexValid then
    BuildIndex
  else if FDirtyFirstLine >= 0 then
    UpdateIndexForDirtyLines;
end;

procedure TLiveScriptObjectFactory.EnsureStyled;
begin
  FMemo.StyleNeeded(FMemo.RawTextLength);
end;

procedure TLiveScriptObjectFactory.Reset;
begin
  Inc(FChangeCount);
  FIndexValid := False; { Index will be rebuilt next time it is needed }
  FDirtyFirstLine := -1;
  FDirtyLastLine := -1;
  FSectionHeaders.Clear;
  for var LiveScriptObject in FLiveScriptObjects do
    LiveScriptObject.FValid := False;
end;

procedure TLiveScriptObjectFactory.Change(const Info: TScintEditChangeInfo);

  procedure MarkLinesDirty(const AFirstLine, ALastLine: Integer);
  begin
    if FDirtyFirstLine < 0 then begin
      FDirtyFirstLine := AFirstLine;
      FDirtyLastLine := ALastLine;
    end else begin
      if AFirstLine < FDirtyFirstLine then
        FDirtyFirstLine := AFirstLine;
      if ALastLine > FDirtyLastLine then
        FDirtyLastLine := ALastLine;
    end;
  end;

begin
  Inc(FChangeCount);

  if not FIndexValid then
    Exit;

  { Also see TMainForm.MemoChange }
  var FirstLine := FMemo.GetLineFromPosition(Info.StartPos);
  const FirstAffectedLine = FirstLine;
  if Info.StartPos > FMemo.GetPositionFromLine(FirstLine) then
    Inc(FirstLine);

  if Info.LinesDelta > 0 then begin
    const Count = Info.LinesDelta;
    for var I := 0 to Integer(FSectionHeaders.Count)-1 do begin
      if FSectionHeaders[I].Line >= FirstLine then begin
        var SectionHeader := FSectionHeaders[I];
        Inc(SectionHeader.Line, Count);
        FSectionHeaders[I] := SectionHeader;
      end;
    end;
    for var LiveScriptObject in FLiveScriptObjects do begin
      if LiveScriptObject.FValid and (LiveScriptObject <> FWritingBackObject) then begin
        { If the lines were added before or inside the live object, update its
          line properties. An edit inside still makes the parsed content
          stale, including any line numbers from that parse, even if they are
          relative (like in TScriptModelCodeSection). }
        if LiveScriptObject.FFirstLine >= FirstLine then begin
          Inc(LiveScriptObject.FFirstLine, Count);
          Inc(LiveScriptObject.FLastLine, Count);
        end else if LiveScriptObject.FLastLine >= FirstAffectedLine then begin
          { FirstAffectedLine, not FirstLine: a line break inserted into the
            object's last line moves the rest of that line onto a new line,
            which still belongs to the object, but the edit's FirstLine is
            then FLastLine + 1, so testing FirstLine would not extend the
            range }
          Inc(LiveScriptObject.FLastLine, Count);
        end;
      end;
    end;
    if FDirtyFirstLine >= 0 then begin
      if FDirtyFirstLine >= FirstLine then
        Inc(FDirtyFirstLine, Count);
      if FDirtyLastLine >= FirstLine then
        Inc(FDirtyLastLine, Count);
    end;
    MarkLinesDirty(FirstAffectedLine, FirstAffectedLine + Count);
  end else if Info.LinesDelta < 0 then begin
    const Count = -Info.LinesDelta;
    const DeleteFirst = FirstLine;
    const DeleteLast = FirstLine + Count - 1;
    for var I := Integer(FSectionHeaders.Count)-1 downto 0 do begin
      if FSectionHeaders[I].Line > DeleteLast then begin
        var SectionHeader := FSectionHeaders[I];
        Dec(SectionHeader.Line, Count);
        FSectionHeaders[I] := SectionHeader;
      end else if FSectionHeaders[I].Line >= DeleteFirst then
        FSectionHeaders.Delete(I);
    end;
    for var LiveScriptObject in FLiveScriptObjects do begin
      if LiveScriptObject.FValid and (LiveScriptObject <> FWritingBackObject) then begin
        { If lines were removed inside the live object or joined into its last
          line, make it invalid. If lines were removed before it, update its
          line properties. }
        if ((LiveScriptObject.FFirstLine <= DeleteLast) and
            (LiveScriptObject.FLastLine >= DeleteFirst)) or
           ((FirstLine > FirstAffectedLine) and
            (LiveScriptObject.FLastLine = FirstAffectedLine)) then
          LiveScriptObject.FValid := False
        else if LiveScriptObject.FFirstLine > DeleteLast then begin
          Dec(LiveScriptObject.FFirstLine, Count);
          Dec(LiveScriptObject.FLastLine, Count);
        end;
      end;
    end;
    if FDirtyFirstLine >= 0 then begin
      if FDirtyFirstLine > DeleteLast then
        Dec(FDirtyFirstLine, Count)
      else if FDirtyFirstLine >= DeleteFirst then
        FDirtyFirstLine := FirstAffectedLine;
      if FDirtyLastLine > DeleteLast then
        Dec(FDirtyLastLine, Count)
      else if FDirtyLastLine >= DeleteFirst then
        FDirtyLastLine := FirstAffectedLine;
      if FDirtyLastLine < FDirtyFirstLine then
        FDirtyLastLine := FDirtyFirstLine;
    end;
    MarkLinesDirty(FirstAffectedLine, FirstAffectedLine);
  end else
    MarkLinesDirty(FirstAffectedLine, FirstAffectedLine);
end;

function TLiveScriptObjectFactory.SectionCount: Integer;
begin
  EnsureIndex;
  Result := Integer(FSectionHeaders.Count);
end;

function TLiveScriptObjectFactory.GetSectionHeader(Index: Integer): TLiveScriptSectionHeader;
begin
  EnsureIndex;
  Result := FSectionHeaders[Index];
end;

function TLiveScriptObjectFactory.TryGetSectionAtLine(const ALine: Integer;
  out ASectionIndex: Integer): Boolean;
begin
  EnsureIndex;
  EnsureStyled; { For GetSectionLines }
  Result := False;
  for var I := Integer(FSectionHeaders.Count)-1 downto 0 do begin
    if FSectionHeaders[I].Line <= ALine then begin
      var FirstLine, LastLine: Integer;
      GetSectionLines(I, FirstLine, LastLine);
      if (ALine < FirstLine) or (ALine <= LastLine) then begin
        ASectionIndex := I;
        Result := True;
      end;
      Exit;
    end;
  end;
end;

procedure TLiveScriptObjectFactory.GetSectionOccurrence(const ASectionIndex: Integer;
  out AOccurrenceIndex, AOccurrenceCount: Integer);
{ Does not include special support for scUnknown/scThirdParty }
begin
  const Section = SectionHeaders[ASectionIndex].Section;
  AOccurrenceIndex := 0;
  AOccurrenceCount := 0;
  for var I := 0 to SectionCount-1 do begin
    if SectionHeaders[I].Section = Section then begin
      Inc(AOccurrenceCount);
      if I = ASectionIndex then
        AOccurrenceIndex := AOccurrenceCount;
    end;
  end;
end;

procedure TLiveScriptObjectFactory.GetSectionLines(const ASectionIndex: Integer;
  out AFirstLine, ALastLine: Integer);
{ Requires the lines to be styled already. The returned range can be empty (ALastLine < AFirstLine).
  Note: Section tags themselves are not associated with any section, so this doesn't
  read into the next section if there's two adjacent sections of the same type. }
begin
  const Header = FSectionHeaders[ASectionIndex];
  const LineCount = FMemo.Lines.Count;
  const HeaderLastLine = GetLogicalLineLastLine(Header.Line);
  AFirstLine := HeaderLastLine+1;
  var L := AFirstLine;
  while (L < LineCount) and
        (TInnoSetupStyler.GetSectionFromLineState(FMemo.Lines.State[L]) = Header.Section) do
    Inc(L);
  ALastLine := L-1;
end;

function TLiveScriptObjectFactory.GetSectionFirstSignificantLine(
  const ASectionIndex: Integer): Integer;
{ The first non blank line of the section's body, or the body's first line
  when the whole body is blank, or the header's line when the body is empty }
begin
  EnsureIndex;
  EnsureStyled; { For GetSectionLines }
  var FirstLine, LastLine: Integer;
  GetSectionLines(ASectionIndex, FirstLine, LastLine);
  if LastLine < FirstLine then
    Exit(FSectionHeaders[ASectionIndex].Line);
  Result := FirstLine;
  for var L := FirstLine to LastLine do
    if Trim(FMemo.Lines[L]) <> '' then
      Exit(L);
end;

function TLiveScriptObjectFactory.GetLinesText(const AFirstLine,
  ALastLine: Integer): TArray<String>;
begin
  if ALastLine < AFirstLine then
    Exit(nil);
  SetLength(Result, ALastLine-AFirstLine+1);
  for var I := AFirstLine to ALastLine do
    Result[I-AFirstLine] := FMemo.Lines[I];
end;

function TLiveScriptObjectFactory.GetLinesTextAndClassify(const AFirstLine,
  ALastLine: Integer; out ALineKind: TScriptLineKind;
  out AJoinedText: String): TArray<String>;
begin
  Result := GetLinesText(AFirstLine, ALastLine);
  AJoinedText := JoinSpannedScriptLines(Result);
  ALineKind := ClassifyScriptLine(AJoinedText);
end;

function TLiveScriptObjectFactory.GetLinesTextAndClassify(const AFirstLine,
  ALastLine: Integer; out ALineKind: TScriptLineKind): TArray<String>;
begin
  var JoinedText: String;
  Result := GetLinesTextAndClassify(AFirstLine, ALastLine, ALineKind, JoinedText);
end;

function TLiveScriptObjectFactory.GetLogicalLineFirstLine(const ALine: Integer): Integer;
begin
  { Find first line in series of spanned lines }
  Result := ALine;
  while (Result > 0) and LineSpans(Result-1) do
    Dec(Result);
end;

function TLiveScriptObjectFactory.GetLogicalLineLastLine(const ALine: Integer): Integer;
begin
  { Find final line in series of spanned lines }
  Result := ALine;
  const LineCount = FMemo.Lines.Count;
  while (Result < LineCount-1) and LineSpans(Result) do
    Inc(Result);
end;

function TLiveScriptObjectFactory.TryGetSetupDirectiveValue(const ADirectiveName: String;
  out AValue: String): Boolean;
begin
  { Returns the last occurrence found. The compiler does not accept duplicate
    keys (except SignTool), but it only sees the script after preprocessing.
    Before preprocessing having duplicates does not always mean there's an error. }
  EnsureIndex;
  EnsureStyled; { For GetSectionLines }
  Result := False;
  for var I := 0 to Integer(FSectionHeaders.Count)-1 do begin
    if FSectionHeaders[I].Section = scSetup then begin
      var FirstLine, LastLine: Integer;
      GetSectionLines(I, FirstLine, LastLine);
      if LastLine >= FirstLine then begin
        const Section = TScriptModelKeyValueSection.Create(nil); { Just reading, metadata not needed }
        try
          Section.Parse(GetLinesText(FirstLine, LastLine));
          var Value: String;
          if Section.TryGetValue(ADirectiveName, Value) then begin
            AValue := Value;
            Result := True;
          end;
        finally
          Section.Free;
        end;
      end;
    end;
  end;
end;

procedure TLiveScriptObjectFactory.CollectParameterValues(
  const AOnlySection: TInnoSetupSection; const AParameterName: String;
  const AValues: TStringList; const ASplitValueWords: Boolean);
{ Collects the non-empty values of the AParameterName parameter of every
  parameter section entry into AValues, restricted to AOnlySection when
  not scNone and skipping sections which do not have the parameter according
  to their metadata. When ASplitValueWords is True the values' space-separated
  words are collected instead. }
begin
  var Sections: TInnoSetupSections := ParameterSections;
  if AOnlySection <> scNone then
    Sections := Sections * [AOnlySection];
  for var Section in ParameterSections do begin
    var Metadata: TScriptModelSectionMetadata;
    var Definition: TMemberDefinition;
    if (Section in Sections) and
       TryGetScriptModelSectionMetadata(SectionToSectionName(Section), Metadata) and
       not Metadata.TryGetMember(AParameterName, Definition) then
      Exclude(Sections, Section);
  end;
  if Sections = [] then
    Exit;

  EnsureIndex;
  EnsureStyled; { For GetSectionLines }
  const Entry = TScriptModelParameterSectionEntry.Create(nil); { Just reading, metadata not needed }
  try
    for var I := 0 to Integer(FSectionHeaders.Count)-1 do begin
      if not (FSectionHeaders[I].Section in Sections) then
        Continue;
      var FirstLine, LastLine: Integer;
      GetSectionLines(I, FirstLine, LastLine);
      var Line := FirstLine;
      while Line <= LastLine do begin
        const EntryFirstLine = Line;
        const EntryLastLine = GetLogicalLineLastLine(Line);
        Line := EntryLastLine+1;
        var LineKind: TScriptLineKind;
        var JoinedText: String;
        const EntryLines = GetLinesTextAndClassify(EntryFirstLine, EntryLastLine,
          LineKind, JoinedText);
        if (LineKind <> slkActual) or
           (PathStrFind(PChar(JoinedText), Length(JoinedText),
            PChar(AParameterName), Length(AParameterName)) < 0) then
          Continue; { Parameter name not present at all, skip Parse }
        Entry.Parse(EntryLines);
        var Value: String;
        if Entry.TryGetValue(AParameterName, Value) then begin
          if ASplitValueWords then begin
            while True do begin
              const ValueWord = ExtractStr(Value, ' ');
              if ValueWord = '' then
                Break;
              AValues.Add(ValueWord);
            end;
          end else if Value <> '' then
            AValues.Add(Value);
        end;
      end;
    end;
  finally
    Entry.Free;
  end;
end;

function CollectParameterValuesFromFactories(
  const AFactories: array of TLiveScriptObjectFactory;
  const AParameterName: String): TArray<String>;
{ Collects the distinct values which are valid for the AParameterName parameter
  from the given factories' script. nil and duplicate factories are skipped.
  Sorts using same sort as autocompletion and Scintilla, so using CompareText.
  Also see BuildAutoCompleteWordList. }

  function FactoryAlreadyProcessed(const AIndex: NativeInt): Boolean;
  begin
    Result := False;
    for var I := 0 to AIndex-1 do
      if AFactories[I] = AFactories[AIndex] then
        Exit(True);
  end;

begin
  const NamesSection = GetScriptSectionDefiningParameterValues(AParameterName);
  var Values: TStringList := nil;
  var DefinedNames: TStringList := nil;
  try
    Values := TStringList.Create;
    Values.CaseSensitive := False;
    Values.UseLocale := False; { Make sure it uses CompareText and not AnsiCompareText }
    Values.Duplicates := dupIgnore;
    Values.Sorted := True;
    DefinedNames := TStringList.Create;
    DefinedNames.CaseSensitive := False;
    DefinedNames.UseLocale := False; { See above }
    DefinedNames.Duplicates := dupIgnore;
    DefinedNames.Sorted := True;
    for var I := 0 to High(AFactories) do begin
      const Factory = AFactories[I];
      if (Factory = nil) or FactoryAlreadyProcessed(I) then
        Continue;
      if NamesSection <> scNone then begin
        { Lookup defined names }
        Factory.CollectParameterValues(NamesSection, 'Name', DefinedNames);
        if NamesSection = scISSigKeys then
          Factory.CollectParameterValues(scISSigKeys, 'Group', DefinedNames, True); { Group names are valid values too }
      end;
      { Lookup uses: finds extra expression forms }
      Factory.CollectParameterValues(scNone, AParameterName, Values);
    end;
    if (DefinedNames.Count = 0) and (NamesSection = scTypes) then begin
      { Add automatically created default types: see Compiler.SetupCompiler's DefaultTypeEntryNames.
        The automatically created 'default' language is not added: it's always the sole language,
        so specifying it does nothing. }
      DefinedNames.AddStrings(['full', 'compact', 'custom']);
    end;
    Values.AddStrings(DefinedNames);
    Result := Values.ToStringArray;
  finally
    DefinedNames.Free;
    Values.Free;
  end;
end;

function TryGetCommonSectionRefusalReason(const ASection: TInnoSetupSection;
  out ARefusalReason: TRefusalReason): Boolean;
begin
  Result := True;
  if ASection = scNone then
    ARefusalReason := rrNotInsideSection
  else if ASection = scCode then
    ARefusalReason := rrInCodeSection
  else if ASection in [scUnknown, scThirdParty] then
    ARefusalReason := rrUnrecognizedSection
  else
    Result := False;
end;

type
  TExtendedLineRange = record
    LineRange: TScintLineRange;
    CreatedFromBlankLine: Boolean;
  end;

function TLiveScriptObjectFactory.TryCreateParameterSectionEntries(
  const ALineRanges, AIndividualLineRanges: TArray<TScintLineRange>;
  const ACaretLine: Integer;
  out AEntries: TLiveScriptParameterSectionEntries;
  out ARefusalReason: TRefusalReason): Boolean;
{ ALineRanges must be sorted and merged and AIndividualLineRanges must contain
  the individual line ranges before merging, both as returned by
  TScintEdit.GetSelectionLineRanges. When ALineRanges covers one line or none,
  or contains no entries, ACaretLine is inspected instead. That line can lie
  outside the ranges: Scintilla's Select Line commands like triple click
  select one line but leave the caret below it, and inspection follows the
  caret. }

  function AnySelectionWithinLines(const AFirstLine, ALastLine: Integer): Boolean;
  { Returns True when there was an individual selection within the given lines.
    Note that in Scintilla a regular caret is an empty selection. }
  begin
    Result := False;
    for var LineRange in AIndividualLineRanges do
      if (LineRange.StartLine >= AFirstLine) and (LineRange.EndLine <= ALastLine) then
        Exit(True);
  end;

begin
  if (Length(ALineRanges) > 0) and (Length(AIndividualLineRanges) = 0) then
    raise Exception.Create('Internal error: TryCreateParameterSectionEntries: AIndividualLineRanges is empty');
  AEntries := nil;
  Result := False;
  EnsureIndex;
  EnsureStyled;

  const LineCount = FMemo.Lines.Count;
  var CoveredLineCount := 0;
  for var LineRange in ALineRanges do begin
    if (LineRange.StartLine < 0) or (LineRange.EndLine >= LineCount) or
       (LineRange.EndLine < LineRange.StartLine) then begin
      ARefusalReason := rrLineOutOfRange;
      Exit;
    end;
    Inc(CoveredLineCount, LineRange.EndLine-LineRange.StartLine+1);
  end;

  if CoveredLineCount > 1 then begin
    const EntryLineRanges = TList<TExtendedLineRange>.Create;
    try
      { Collect the entry line ranges, creating no objects yet }
      var EntriesSection := scNone;
      var HasOtherActualContent := False;
      var LastHandledLogicalFirstLine := -1;
      for var LineRange in ALineRanges do begin
        var Line := LineRange.StartLine;
        while Line <= LineRange.EndLine do begin
          const FirstLine = GetLogicalLineFirstLine(Line);
          const LastLine = GetLogicalLineLastLine(Line);
          Line := LastLine+1;
          if FirstLine = LastHandledLogicalFirstLine then
            Continue; { Two ranges can extend to the same logical line, and with sorted ranges a duplicate is always the previous one }
          LastHandledLogicalFirstLine := FirstLine;
          { Skip section header lines, else they cause rrMixedSelection }
          var HeaderSection: TInnoSetupSection;
          if TInnoSetupStyler.LineSectionHeader(FMemo.Lines.State[FirstLine], HeaderSection) then
            Continue;
          { Skip blank lines, comments, and ISPP directive lines, except a
            blank line in a parameter section with a selection of its own:
            this is so a new entry can be created on a blank line, same as
            in single-entry path below }
          const Section = TInnoSetupStyler.GetSectionFromLineState(FMemo.Lines.State[FirstLine]);
          var LineKind: TScriptLineKind;
          GetLinesTextAndClassify(FirstLine, LastLine, LineKind);
          if (LineKind <> slkActual) and
             ((LineKind <> slkBlank) or not (Section in ParameterSections) or not AnySelectionWithinLines(FirstLine, LastLine)) then
            Continue;
          if Section in ParameterSections then begin
            if EntriesSection = scNone then
              EntriesSection := Section
            else if Section <> EntriesSection then begin
              ARefusalReason := rrMixedSelection;
              Exit;
            end;
            var EntryLineRange: TExtendedLineRange;
            EntryLineRange.LineRange.StartLine := FirstLine;
            EntryLineRange.LineRange.EndLine := LastLine;
            EntryLineRange.CreatedFromBlankLine := LineKind = slkBlank;
            EntryLineRanges.Add(EntryLineRange);
          end else
            HasOtherActualContent := True;
        end;
      end;

      if EntryLineRanges.Count > 0 then begin
        if HasOtherActualContent then begin
          ARefusalReason := rrMixedSelection;
          Exit;
        end;
        var Metadata: TScriptModelSectionMetadata := nil;
        TryGetScriptModelSectionMetadata(SectionToSectionName(EntriesSection), Metadata);
        for var EntryLineRange in EntryLineRanges do begin
          const LineRange = EntryLineRange.LineRange;
          const Entry = TLiveScriptParameterSectionEntry.Create(Self,
            LineRange.StartLine, LineRange.EndLine, EntriesSection, Metadata,
            GetLinesText(LineRange.StartLine, LineRange.EndLine),
            EntryLineRange.CreatedFromBlankLine);
          if AEntries = nil then
            AEntries := TLiveScriptParameterSectionEntries.Create(Entry)
          else
            AEntries.Add(Entry);
        end;
        Exit(True);
      end;
      { The selection contains no entries: fall through to inspecting
        ACaretLine }
    finally
      EntryLineRanges.Free;
    end;
  end;

  if (ACaretLine < 0) or (ACaretLine >= LineCount) then begin
    ARefusalReason := rrLineOutOfRange;
    Exit;
  end;

  const Section = TInnoSetupStyler.GetSectionFromLineState(FMemo.Lines.State[ACaretLine]);
  if TryGetCommonSectionRefusalReason(Section, ARefusalReason) then
    Exit;
  if not (Section in ParameterSections) then begin
    ARefusalReason := rrNotParameterSection;
    Exit;
  end;

  const FirstLine = GetLogicalLineFirstLine(ACaretLine);
  const LastLine = GetLogicalLineLastLine(ACaretLine);

  var LineKind: TScriptLineKind;
  const EntryLines = GetLinesTextAndClassify(FirstLine, LastLine, LineKind);
  case LineKind of
  { slkBlank is not refused. This is so a new entry can be created on a blank line,
    same as in multi-entry path above. }
    slkComment:
      begin
        ARefusalReason := rrComment;
        Exit;
      end;
    slkISPPDirective:
      begin
        ARefusalReason := rrISPPDirective;
        Exit;
      end;
  end;

  var Metadata: TScriptModelSectionMetadata := nil;
  TryGetScriptModelSectionMetadata(SectionToSectionName(Section), Metadata);
  const PrimaryEntry = TLiveScriptParameterSectionEntry.Create(Self, FirstLine,
    LastLine, Section, Metadata, EntryLines, LineKind = slkBlank);
  AEntries := TLiveScriptParameterSectionEntries.Create(PrimaryEntry);
  Result := True;
end;

function TLiveScriptObjectFactory.TryGetSectionForCreation(const ASectionIndex: Integer;
  out ASection: TInnoSetupSection; out ARefusalReason: TRefusalReason): Boolean;
begin
  Result := False;
  EnsureIndex;
  EnsureStyled;
  if (ASectionIndex < 0) or (ASectionIndex >= FSectionHeaders.Count) then begin
    ARefusalReason := rrSectionIndexOutOfRange;
    Exit;
  end;
  ASection := FSectionHeaders[ASectionIndex].Section;
  Result := True;
end;

function TLiveScriptObjectFactory.GetSectionBodyLines(const ASectionIndex: Integer;
  out AFirstLine, ALastLine: Integer): TArray<String>;
begin
  GetSectionLines(ASectionIndex, AFirstLine, ALastLine);
  Result := GetLinesText(AFirstLine, ALastLine);
end;

function TLiveScriptObjectFactory.TryCreateKeyValueSection(const ASectionIndex: Integer;
  out ASection: TLiveScriptKeyValueSection;
  out ARefusalReason: TRefusalReason): Boolean;
begin
  ASection := nil;
  Result := False;
  var Section: TInnoSetupSection;
  if not TryGetSectionForCreation(ASectionIndex, Section, ARefusalReason) then
    Exit;
  if TryGetCommonSectionRefusalReason(Section, ARefusalReason) then
    Exit;
  if not (Section in KeyValueSections) then begin
    ARefusalReason := rrNotKeyValueSection;
    Exit;
  end;

  var FirstLine, LastLine: Integer;
  const SectionLines = GetSectionBodyLines(ASectionIndex, FirstLine, LastLine);
  var Metadata: TScriptModelSectionMetadata := nil;
  TryGetScriptModelSectionMetadata(FSectionHeaders[ASectionIndex].Name, Metadata);
  ASection := TLiveScriptKeyValueSection.Create(Self, FirstLine, LastLine,
    Metadata, SectionLines);
  Result := True;
end;

function TLiveScriptObjectFactory.TryAcquireCodeSection(const ASectionIndex: Integer;
  out ASection: TLiveScriptCodeSection;
  out ARefusalReason: TRefusalReason): Boolean;
begin
  ASection := nil;
  Result := False;
  var Section: TInnoSetupSection;
  if not TryGetSectionForCreation(ASectionIndex, Section, ARefusalReason) then
    Exit;
  if Section <> scCode then begin
    if not TryGetCommonSectionRefusalReason(Section, ARefusalReason) then
      ARefusalReason := rrNotCodeSection;
    Exit;
  end;

  { Try to share existing }
  for var LiveScriptObject in FLiveScriptObjects do begin
    if LiveScriptObject is TLiveScriptCodeSection then begin
      const CodeSection = TLiveScriptCodeSection(LiveScriptObject);
      if (CodeSection.FSectionIndex = ASectionIndex) and CodeSection.Valid and
         (CodeSection.FChangeCountAtParse = FChangeCount) then begin
        Inc(CodeSection.FAcquireCount);
        ASection := CodeSection;
        Exit(True);
      end;
    end;
  end;

  var FirstLine, LastLine: Integer;
  const SectionLines = GetSectionBodyLines(ASectionIndex, FirstLine, LastLine);
  ASection := TLiveScriptCodeSection.Create(Self, ASectionIndex, FirstLine,
    LastLine, SectionLines);
  Inc(ASection.FAcquireCount);
  Result := True;
end;

function TLiveScriptObjectFactory.TryAcquireCodeSection(const ASectionIndex: Integer;
  out ASection: TLiveScriptCodeSection): Boolean;
begin
  var RefusalReason: TRefusalReason;
  Result := TryAcquireCodeSection(ASectionIndex, ASection, RefusalReason);
end;

class procedure TLiveScriptObjectFactory.ReleaseAndNil(
  var ASection: TLiveScriptCodeSection);
begin
  if ASection = nil then
    Exit;
  const CodeSection = ASection;
  ASection := nil;
  Dec(CodeSection.FAcquireCount);
  if CodeSection.FAcquireCount = 0 then
    CodeSection.Free;
end;

procedure TLiveScriptObjectFactory.WriteBackChange(const ALiveScriptObject: TLiveScriptObject;
  const ALines: TArray<String>; const ACreatedFromBlankLine: Boolean);
{ Updates the object's lines to the new text, directly in the memo attached to this factory }
begin
  { Sanity checks }
  if ALiveScriptObject.FFactory <> Self then
    raise Exception.Create('Internal error: WriteBackChange: FFactory <> Self');
  if not ALiveScriptObject.FValid then
    raise Exception.Create('Internal error: WriteBackChange: not FValid');
  for var Line in ALines do
    if ContainsLineBreak(Line) then
      raise Exception.Create('Internal error: WriteBackChange: ALines element contains a line break');
  if (Length(ALines) = 0) and not (ALiveScriptObject is TLiveScriptKeyValueSection) then
    raise Exception.Create('Internal error: WriteBackChange: empty ALines but not a key/value section');

  const LineEnding = String(FMemo.LineEndingString);
  const Text = String.Join(LineEnding, ALines);
  FWritingBackObject := ALiveScriptObject; { Make sure Change doesn't update the object's FFirst/LastLine, we set FLastLine below instead }
  FMemo.BeginUndoAction;
  try
    if ACreatedFromBlankLine and (Length(ALines) > 0) then begin
      { Insert the new lines plus a line ending at the start of the blank
        line, which itself ends up below the inserted lines }
      const Pos = FMemo.GetPositionFromLine(ALiveScriptObject.FFirstLine);
      FMemo.ReplaceTextRange(Pos, Pos, Text + LineEnding);
    end else if ALiveScriptObject.FLastLine >= ALiveScriptObject.FFirstLine then begin
      if Length(ALines) = 0 then begin
        { Remove the object's lines entirely, taking one line ending with them
          so no blank line is left behind: the last line's own line ending,
          or, if that line ends the document, the line ending above the first
          line, which leaves the section header as the last line }
        var StartPos: Integer;
        if ALiveScriptObject.FLastLine = FMemo.Lines.Count-1 then
          StartPos := FMemo.GetLineEndPosition(ALiveScriptObject.FFirstLine-1)
        else
          StartPos := FMemo.GetPositionFromLine(ALiveScriptObject.FFirstLine);
        FMemo.ReplaceTextRange(StartPos,
          FMemo.GetPositionFromLine(ALiveScriptObject.FLastLine+1), '',
          srmMinimal);
      end else begin
        { Replace all of the object's lines' text, from the start of the first
          line to the end of the last line but excluding its line ending, with
          the new lines }
        FMemo.ReplaceTextRange(
          FMemo.GetPositionFromLine(ALiveScriptObject.FFirstLine),
          FMemo.GetLineEndPosition(ALiveScriptObject.FLastLine), Text,
          srmMinimal);
      end;
    end else if Length(ALines) > 0 then begin
      { The object has no lines yet (a key/value section without lines):
        there is nothing to replace, so the new lines are inserted }
      if ALiveScriptObject.FFirstLine <= FMemo.Lines.Count-1 then begin
        { Insert the new lines plus a line ending at the start of the line
          following the section header, pushing that line and the rest of the
          document down }
        const Pos = FMemo.GetPositionFromLine(ALiveScriptObject.FFirstLine);
        FMemo.ReplaceTextRange(Pos, Pos, Text + LineEnding);
      end else begin
        { There is no line following the section header. Append a line ending
          plus the new lines after the header. }
        const Pos = FMemo.RawTextLength;
        FMemo.ReplaceTextRange(Pos, Pos, LineEnding + Text);
      end;
    end;
    { The object now covers one line per element of ALines, so an empty ALines
      leaves it with an empty range (LastLine < FirstLine) }
    ALiveScriptObject.FLastLine := ALiveScriptObject.FFirstLine + Integer(Length(ALines)) - 1;
  finally
    FMemo.EndUndoAction;
    FWritingBackObject := nil;
  end;
end;

end.
