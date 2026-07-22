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

  { Why TryCreateParameterSectionEntry or TryCreateKeyValueSection refused to
    create an object }
  TRefusalReason = (rrLineOutOfRange, rrNotInsideSection,
    rrInCodeSection, rrUnrecognizedSection, rrNotParameterSection, rrComment,
    rrISPPDirective, rrSectionIndexOutOfRange, rrNotKeyValueSection);

  TLiveScriptSectionHeader = record
    Line: Integer;
    StylerSection: TInnoSetupSection;
    Name: String;
  end;

  TLiveScriptObject = class
  private
    FFactory: TLiveScriptObjectFactory;
    FFirstLine, FLastLine: Integer; { The lines for which the object was created, always up-to-date }
    FValid: Boolean; { False if some or all of the object's lines were deleted since creation }
    constructor Create(const AFactory: TLiveScriptObjectFactory; const AFirstLine,
      ALastLine: Integer);
  public
    destructor Destroy; override;
    property FirstLine: Integer read FFirstLine;
    property LastLine: Integer read FLastLine;
    property Valid: Boolean read FValid;
  end;

  { An entry of a parameter section }
  TLiveScriptParameterSectionEntry = class(TLiveScriptObject)
  private
    FEntry: TScriptModelParameterSectionEntry;
    FStylerSection: TInnoSetupSection;
    FCreatedFromBlankLine: Boolean;
    constructor Create(const AFactory: TLiveScriptObjectFactory; const AFirstLine,
      ALastLine: Integer; const AStylerSection: TInnoSetupSection;
      const AMetadata: TScriptModelSectionMetadata; const ALines: TArray<String>;
      const ACreatedFromBlankLine: Boolean);
    procedure OnChange(Sender: TObject);
  public
    destructor Destroy; override;
    property Entry: TScriptModelParameterSectionEntry read FEntry;
    property StylerSection: TInnoSetupSection read FStylerSection;
  end;

  { A single occurrence of a key/value section }
  TLiveScriptKeyValueSection = class(TLiveScriptObject)
  private
    FSection: TScriptModelKeyValueSection;
    constructor Create(const AFactory: TLiveScriptObjectFactory; const AFirstLine,
      ALastLine: Integer; const AMetadata: TScriptModelSectionMetadata;
      const ALines: TArray<String>);
    procedure OnChange(Sender: TObject);
  public
    destructor Destroy; override;
    property Section: TScriptModelKeyValueSection read FSection;
  end;

  TLiveScriptObjectFactory = class
  private
    FMemo: TScintEdit;
    FStyler: TInnoSetupStyler;
    FSectionHeaders: TList<TLiveScriptSectionHeader>; { Includes scUnknown/scThirdParty section }
    FIndexValid: Boolean;
    FDirtyFirstLine, FDirtyLastLine: Integer; { -1 when nothing is dirty }
    FLiveScriptObjects: TList<TLiveScriptObject>;
    FWritingBackObject: TLiveScriptObject;
    FChangeCount: Int64;
    procedure EnsureIndex;
    procedure EnsureStyled;
    function GetLinesText(const AFirstLine, ALastLine: Integer): TArray<String>;
    function GetSectionHeader(Index: Integer): TLiveScriptSectionHeader;
    procedure GetSectionLines(const ASectionIndex: Integer;
      out AFirstLine, ALastLine: Integer);
    function LineSpans(const ALine: Integer): Boolean;
    procedure WriteBackChange(const ALiveScriptObject: TLiveScriptObject;
      const ALines: TArray<String>; const ACreatedFromBlankLine: Boolean = False);
  public
    constructor Create(const AMemo: TScintEdit; const AStyler: TInnoSetupStyler);
    destructor Destroy; override;
    procedure Change(const Info: TScintEditChangeInfo);
    procedure InvalidateIndex;
    function SectionCount: Integer;
    function TryGetSectionAtLine(const ALine: Integer;
      out ASectionIndex: Integer): Boolean;
    procedure GetSectionOccurrence(const ASectionIndex: Integer;
      out AOccurrenceIndex, AOccurrenceCount: Integer);
    function TryGetSetupDirectiveValue(const ADirectiveName: String;
      out AValue: String): Boolean;
    { ARefusalReason is only set when the result is False }
    function TryCreateParameterSectionEntry(const ALine: Integer;
      out AEntry: TLiveScriptParameterSectionEntry;
      out ARefusalReason: TRefusalReason): Boolean;
    function TryCreateKeyValueSection(const ASectionIndex: Integer;
      out ASection: TLiveScriptKeyValueSection;
      out ARefusalReason: TRefusalReason): Boolean;
    { Bumped on every Change call, so a consumer can tell whether the memo
      changed since it last read something }
    property ChangeCount: Int64 read FChangeCount;
    property Memo: TScintEdit read FMemo;
    property SectionHeaders[Index: Integer]: TLiveScriptSectionHeader read GetSectionHeader;
    property Styler: TInnoSetupStyler read FStyler;
  end;

implementation

uses
  SysUtils;

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
  const AFirstLine, ALastLine: Integer; const AStylerSection: TInnoSetupSection;
  const AMetadata: TScriptModelSectionMetadata; const ALines: TArray<String>;
  const ACreatedFromBlankLine: Boolean);
begin
  inherited Create(AFactory, AFirstLine, ALastLine);
  FStylerSection := AStylerSection;
  FCreatedFromBlankLine := ACreatedFromBlankLine;
  FEntry := TScriptModelParameterSectionEntry.Create(AMetadata);
  FEntry.Parse(ALines);
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

{ TLiveScriptKeyValueSection }

constructor TLiveScriptKeyValueSection.Create(const AFactory: TLiveScriptObjectFactory;
  const AFirstLine, ALastLine: Integer; const AMetadata: TScriptModelSectionMetadata;
  const ALines: TArray<String>);
begin
  inherited Create(AFactory, AFirstLine, ALastLine);
  FSection := TScriptModelKeyValueSection.Create(AMetadata);
  FSection.Parse(ALines);
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
    var StylerSection: TInnoSetupSection;
    Result := TInnoSetupStyler.LineSectionHeader(FMemo.Lines.State[ALine], StylerSection);
    if Result then begin
      ASectionHeader.Line := ALine;
      ASectionHeader.StylerSection := StylerSection;
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
    while (FirstLine > 0) and LineSpans(FirstLine-1) do
      Dec(FirstLine);
    while (LastLine < LineCount-1) and LineSpans(LastLine) do
      Inc(LastLine);
    if LastLine < LineCount-1 then begin
      Inc(LastLine);
      while (LastLine < LineCount-1) and LineSpans(LastLine) do
        Inc(LastLine);
    end;

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

procedure TLiveScriptObjectFactory.InvalidateIndex;
begin
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
        if ((LiveScriptObject.FFirstLine <= DeleteLast) and
            (LiveScriptObject.FLastLine >= DeleteFirst)) or
           ((FirstLine > FirstAffectedLine) and
            (LiveScriptObject.FLastLine = FirstAffectedLine)) then begin
          { Some or all of the object's lines were deleted, or the next line
            was joined into the object's last line }
          LiveScriptObject.FValid := False;
        end else if LiveScriptObject.FFirstLine > DeleteLast then begin
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
  const StylerSection = SectionHeaders[ASectionIndex].StylerSection;
  AOccurrenceIndex := 0;
  AOccurrenceCount := 0;
  for var I := 0 to SectionCount-1 do begin
    if SectionHeaders[I].StylerSection = StylerSection then begin
      Inc(AOccurrenceCount);
      if I = ASectionIndex then
        AOccurrenceIndex := AOccurrenceCount;
    end;
  end;
end;

procedure TLiveScriptObjectFactory.GetSectionLines(const ASectionIndex: Integer;
  out AFirstLine, ALastLine: Integer);
{ Requires the lines to be styled already. The returned range can be empty (ALastLine < AFirstLine) }
begin
  const Header = FSectionHeaders[ASectionIndex];
  const LineCount = FMemo.Lines.Count;
  var HeaderLastLine := Header.Line;
  while (HeaderLastLine < LineCount-1) and LineSpans(HeaderLastLine) do
    Inc(HeaderLastLine);
  AFirstLine := HeaderLastLine+1;
  var L := AFirstLine;
  while (L < LineCount) and
        (TInnoSetupStyler.GetSectionFromLineState(FMemo.Lines.State[L]) = Header.StylerSection) do
    Inc(L);
  ALastLine := L-1;
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
    if FSectionHeaders[I].StylerSection = scSetup then begin
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

function TryGetCommonSectionRefusalReason(const AStylerSection: TInnoSetupSection;
  out ARefusalReason: TRefusalReason): Boolean;
begin
  Result := True;
  if AStylerSection = scNone then
    ARefusalReason := rrNotInsideSection
  else if AStylerSection = scCode then
    ARefusalReason := rrInCodeSection
  else if AStylerSection in [scUnknown, scThirdParty] then
    ARefusalReason := rrUnrecognizedSection
  else
    Result := False;
end;

function TLiveScriptObjectFactory.TryCreateParameterSectionEntry(const ALine: Integer;
  out AEntry: TLiveScriptParameterSectionEntry;
  out ARefusalReason: TRefusalReason): Boolean;
begin
  AEntry := nil;
  Result := False;
  EnsureIndex;
  EnsureStyled;

  const LineCount = FMemo.Lines.Count;
  if (ALine < 0) or (ALine >= LineCount) then begin
    ARefusalReason := rrLineOutOfRange;
    Exit;
  end;

  const StylerSection = TInnoSetupStyler.GetSectionFromLineState(FMemo.Lines.State[ALine]);
  if TryGetCommonSectionRefusalReason(StylerSection, ARefusalReason) then
    Exit;
  if not (StylerSection in ParameterSections) then begin
    ARefusalReason := rrNotParameterSection;
    Exit;
  end;

  var FirstLine := ALine;
  while (FirstLine > 0) and LineSpans(FirstLine-1) do
    Dec(FirstLine);
  var LastLine := ALine;
  while (LastLine < LineCount-1) and LineSpans(LastLine) do
    Inc(LastLine);

  const EntryLines = GetLinesText(FirstLine, LastLine);
  const LineKind = ClassifyScriptLine(JoinSpannedScriptLines(EntryLines));
  case LineKind of
  { slkBlank is not refused. This is so a new entry can be created on a blank line. }
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
  TryGetScriptModelSectionMetadata(SectionToSectionName(StylerSection), Metadata);
  AEntry := TLiveScriptParameterSectionEntry.Create(Self, FirstLine, LastLine,
    StylerSection, Metadata, EntryLines, LineKind = slkBlank);
  Result := True;
end;

function TLiveScriptObjectFactory.TryCreateKeyValueSection(const ASectionIndex: Integer;
  out ASection: TLiveScriptKeyValueSection;
  out ARefusalReason: TRefusalReason): Boolean;
begin
  ASection := nil;
  Result := False;
  EnsureIndex;
  EnsureStyled;

  if (ASectionIndex < 0) or (ASectionIndex >= FSectionHeaders.Count) then begin
    ARefusalReason := rrSectionIndexOutOfRange;
    Exit;
  end;
  const StylerSection = FSectionHeaders[ASectionIndex].StylerSection;
  if TryGetCommonSectionRefusalReason(StylerSection, ARefusalReason) then
    Exit;
  if not (StylerSection in KeyValueSections) then begin
    ARefusalReason := rrNotKeyValueSection;
    Exit;
  end;

  var FirstLine, LastLine: Integer;
  GetSectionLines(ASectionIndex, FirstLine, LastLine);
  var SectionLines: TArray<String>;
  if LastLine >= FirstLine then
    SectionLines := GetLinesText(FirstLine, LastLine)
  else
    SectionLines := nil;
  var Metadata: TScriptModelSectionMetadata := nil;
  TryGetScriptModelSectionMetadata(FSectionHeaders[ASectionIndex].Name, Metadata);
  ASection := TLiveScriptKeyValueSection.Create(Self, FirstLine, LastLine,
    Metadata, SectionLines);
  Result := True;
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
