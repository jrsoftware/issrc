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
  IDE.ScintStylerInnoSetup, IDE.ScriptModel, IDE.ScriptModel.Metadata;

type
  TLiveScriptObjectFactory = class;

  TLiveScriptSection = record
    Line: Integer;
    Section: TInnoSetupStylerSection;
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
  TLiveScriptEntry = class(TLiveScriptObject)
  private
    FEntry: TScriptParameterEntry;
    FSection: TInnoSetupStylerSection;
    FCreatedFromBlankLine: Boolean;
    constructor Create(const AFactory: TLiveScriptObjectFactory; const AFirstLine,
      ALastLine: Integer; const ASection: TInnoSetupStylerSection;
      const AMetadata: TScriptSectionMetadata; const ALines: TArray<String>;
      const ACreatedFromBlankLine: Boolean);
    procedure EntryChange(Sender: TObject);
  public
    destructor Destroy; override;
    property Entry: TScriptParameterEntry read FEntry;
    property Section: TInnoSetupStylerSection read FSection;
  end;

  { A single occurrence of a directive-style section }
  TLiveScriptDirectiveSection = class(TLiveScriptObject)
  private
    FSection: TScriptDirectiveSection;
    constructor Create(const AFactory: TLiveScriptObjectFactory; const AFirstLine,
      ALastLine: Integer; const AMetadata: TScriptSectionMetadata;
      const ALines: TArray<String>);
    procedure SectionChange(Sender: TObject);
  public
    destructor Destroy; override;
    property Section: TScriptDirectiveSection read FSection;
  end;

  TLiveScriptObjectFactory = class
  private
    FMemo: TScintEdit;
    FStyler: TInnoSetupStyler;
    FSections: TList<TLiveScriptSection>; { Includes scUnknown/scThirdParty section }
    FIndexValid: Boolean;
    FDirtyFirstLine, FDirtyLastLine: Integer; { -1 when nothing is dirty }
    FLiveScriptObjects: TList<TLiveScriptObject>;
    FWritingBackObject: TLiveScriptObject;
    procedure EnsureIndex;
    procedure EnsureStyled;
    function GetLinesText(const AFirstLine, ALastLine: Integer): TArray<String>;
    function GetSection(Index: Integer): TLiveScriptSection;
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
    function TryGetSetupDirectiveValue(const ADirectiveName: String;
      out AValue: String): Boolean;
    function TryCreateEntry(const ALine: Integer; out AEntry: TLiveScriptEntry;
      out ARefusalReason: String): Boolean;
    function TryCreateDirectiveSection(const ASectionIndex: Integer;
      out ASection: TLiveScriptDirectiveSection;
      out ARefusalReason: String): Boolean;
    property Memo: TScintEdit read FMemo;
    property Sections[Index: Integer]: TLiveScriptSection read GetSection;
    property Styler: TInnoSetupStyler read FStyler;
  end;

function ParameterSectionToSectionName(const ASection: TInnoSetupStylerSection): String;

implementation

uses
  SysUtils;

function ParameterSectionToSectionName(const ASection: TInnoSetupStylerSection): String;
{ Returns an empty string for sections which aren't parameter sections }
begin
  case ASection of
    scComponents: Result := 'Components';
    scDirs: Result := 'Dirs';
    scFiles: Result := 'Files';
    scIcons: Result := 'Icons';
    scINI: Result := 'INI';
    scInstallDelete: Result := 'InstallDelete';
    scISSigKeys: Result := 'ISSigKeys';
    scLanguages: Result := 'Languages';
    scRegistry: Result := 'Registry';
    scRun: Result := 'Run';
    scTasks: Result := 'Tasks';
    scTypes: Result := 'Types';
    scUninstallDelete: Result := 'UninstallDelete';
    scUninstallRun: Result := 'UninstallRun';
  else
    Result := '';
  end;
end;

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

{ TLiveScriptEntry }

constructor TLiveScriptEntry.Create(const AFactory: TLiveScriptObjectFactory;
  const AFirstLine, ALastLine: Integer; const ASection: TInnoSetupStylerSection;
  const AMetadata: TScriptSectionMetadata; const ALines: TArray<String>;
  const ACreatedFromBlankLine: Boolean);
begin
  inherited Create(AFactory, AFirstLine, ALastLine);
  FSection := ASection;
  FCreatedFromBlankLine := ACreatedFromBlankLine;
  FEntry := TScriptParameterEntry.Create(AMetadata);
  FEntry.Parse(ALines);
  FEntry.OnChange := EntryChange;
end;

destructor TLiveScriptEntry.Destroy;
begin
  FEntry.Free;
  inherited;
end;

procedure TLiveScriptEntry.EntryChange(Sender: TObject);
begin
  if (FFactory <> nil) and FValid then begin
    FFactory.WriteBackChange(Self, FEntry.GetLines, FCreatedFromBlankLine);
    FCreatedFromBlankLine := False; { An entry created from a blank line inserts itself above that line }
  end;
end;

{ TLiveScriptDirectiveSection }

constructor TLiveScriptDirectiveSection.Create(const AFactory: TLiveScriptObjectFactory;
  const AFirstLine, ALastLine: Integer; const AMetadata: TScriptSectionMetadata;
  const ALines: TArray<String>);
begin
  inherited Create(AFactory, AFirstLine, ALastLine);
  FSection := TScriptDirectiveSection.Create(AMetadata);
  FSection.Parse(ALines);
  FSection.OnChange := SectionChange;
end;

destructor TLiveScriptDirectiveSection.Destroy;
begin
  FSection.Free;
  inherited;
end;

procedure TLiveScriptDirectiveSection.SectionChange(Sender: TObject);
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
  FSections := TList<TLiveScriptSection>.Create;
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
  FSections.Free;
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

  function GetHeaderSection(const ALine: Integer;
    out ASection: TLiveScriptSection): Boolean;
  begin
    { ISPP's line continuation (see LineSpans) joins physical lines into one
      logical line, and the styler gives them all the same line state. This
      also applies to spanned headers, regardless of the fact that those
      don't compile. There's no detection for this issue and callers must
      just pass only the first physical line of a spanned header. }
    var Section: TInnoSetupStylerSection;
    Result := TInnoSetupStyler.LineSectionHeader(FMemo.Lines.State[ALine], Section);
    if Result then begin
      ASection.Line := ALine;
      ASection.Section := Section;
      ASection.Name := ExtractSectionHeaderName(FMemo.Lines[ALine]);
    end;
  end;

  procedure BuildIndex;
  begin
    FSections.Clear;
    EnsureStyled;
    const LineCount = FMemo.Lines.Count;
    var PreviousLineSpans := False;
    for var I := 0 to LineCount-1 do begin
      if not PreviousLineSpans then begin
        var Section: TLiveScriptSection;
        if GetHeaderSection(I, Section) then
          FSections.Add(Section);
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
    for var I := Integer(FSections.Count)-1 downto 0 do
      if (FSections[I].Line >= FirstLine) and (FSections[I].Line <= LastLine) then
        FSections.Delete(I);
    var InsertAt := 0;
    while (InsertAt < FSections.Count) and (FSections[InsertAt].Line < FirstLine) do
      Inc(InsertAt);
    var PreviousLineSpans := False; { FirstLine was extended back to the start of a group }
    for var I := FirstLine to LastLine do begin
      if not PreviousLineSpans then begin
        var Section: TLiveScriptSection;
        if GetHeaderSection(I, Section) then begin
          FSections.Insert(InsertAt, Section);
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
  FSections.Clear;
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
  if not FIndexValid then
    Exit;

  { Also see TMainForm.MemoChange }
  var FirstLine := FMemo.GetLineFromPosition(Info.StartPos);
  const FirstAffectedLine = FirstLine;
  if Info.StartPos > FMemo.GetPositionFromLine(FirstLine) then
    Inc(FirstLine);

  if Info.LinesDelta > 0 then begin
    const Count = Info.LinesDelta;
    for var I := 0 to Integer(FSections.Count)-1 do begin
      if FSections[I].Line >= FirstLine then begin
        var Section := FSections[I];
        Inc(Section.Line, Count);
        FSections[I] := Section;
      end;
    end;
    for var LiveScriptObject in FLiveScriptObjects do begin
      if LiveScriptObject.FValid and (LiveScriptObject <> FWritingBackObject) then begin
        if LiveScriptObject.FFirstLine >= FirstLine then begin
          Inc(LiveScriptObject.FFirstLine, Count);
          Inc(LiveScriptObject.FLastLine, Count);
        end else if LiveScriptObject.FLastLine >= FirstLine then
          Inc(LiveScriptObject.FLastLine, Count);
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
    for var I := Integer(FSections.Count)-1 downto 0 do begin
      if FSections[I].Line > DeleteLast then begin
        var Section := FSections[I];
        Dec(Section.Line, Count);
        FSections[I] := Section;
      end else if FSections[I].Line >= DeleteFirst then
        FSections.Delete(I);
    end;
    for var LiveScriptObject in FLiveScriptObjects do begin
      if LiveScriptObject.FValid and (LiveScriptObject <> FWritingBackObject) then begin
        if (LiveScriptObject.FFirstLine <= DeleteLast) and
           (LiveScriptObject.FLastLine >= DeleteFirst) then
          LiveScriptObject.FValid := False { Some or all of the object's lines were deleted }
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
  Result := Integer(FSections.Count);
end;

function TLiveScriptObjectFactory.GetSection(Index: Integer): TLiveScriptSection;
begin
  EnsureIndex;
  Result := FSections[Index];
end;

function TLiveScriptObjectFactory.TryGetSectionAtLine(const ALine: Integer;
  out ASectionIndex: Integer): Boolean;
begin
  EnsureIndex;
  EnsureStyled; { For GetSectionLines }
  Result := False;
  for var I := Integer(FSections.Count)-1 downto 0 do begin
    if FSections[I].Line <= ALine then begin
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

procedure TLiveScriptObjectFactory.GetSectionLines(const ASectionIndex: Integer;
  out AFirstLine, ALastLine: Integer);
{ Requires the lines to be styled already. The returned range can be empty (ALastLine < AFirstLine) }
begin
  const Header = FSections[ASectionIndex];
  const LineCount = FMemo.Lines.Count;
  var HeaderLastLine := Header.Line;
  while (HeaderLastLine < LineCount-1) and LineSpans(HeaderLastLine) do
    Inc(HeaderLastLine);
  AFirstLine := HeaderLastLine+1;
  var L := AFirstLine;
  while (L < LineCount) and
        (TInnoSetupStyler.GetSectionFromLineState(FMemo.Lines.State[L]) = Header.Section) do
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
    directives (except SignTool), but it only sees the script after preprocessing.
    Before preprocessing having duplicates does not always mean there's an error. }
  EnsureIndex;
  EnsureStyled; { For GetSectionLines }
  Result := False;
  for var I := 0 to Integer(FSections.Count)-1 do begin
    if FSections[I].Section = scSetup then begin
      var FirstLine, LastLine: Integer;
      GetSectionLines(I, FirstLine, LastLine);
      if LastLine >= FirstLine then begin
        const Section = TScriptDirectiveSection.Create(nil); { Just reading, metadata not needed }
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

function TLiveScriptObjectFactory.TryCreateEntry(const ALine: Integer;
  out AEntry: TLiveScriptEntry; out ARefusalReason: String): Boolean;
begin
  AEntry := nil;
  Result := False;
  EnsureIndex;
  EnsureStyled;

  const LineCount = FMemo.Lines.Count;
  if (ALine < 0) or (ALine >= LineCount) then begin
    ARefusalReason := 'The line number is out of range';
    Exit;
  end;

  const Section = TInnoSetupStyler.GetSectionFromLineState(FMemo.Lines.State[ALine]);
  if Section = scNone then begin
    ARefusalReason := 'The line is not inside a section';
    Exit;
  end;
  if Section = scCode then begin
    ARefusalReason := 'The line is in the [Code] section';
    Exit;
  end;
  if Section in [scUnknown, scThirdParty] then begin
    ARefusalReason := 'The line is in an unrecognized section';
    Exit;
  end;
  if not TInnoSetupStyler.IsParamSection(Section) then begin
    ARefusalReason := 'The line is in a directive-style section';
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
        ARefusalReason := 'The line is a comment';
        Exit;
      end;
    slkISPPDirective:
      begin
        ARefusalReason := 'The line is an ISPP directive';
        Exit;
      end;
  end;

  var Metadata: TScriptSectionMetadata := nil;
  TryGetScriptSectionMetadata(ParameterSectionToSectionName(Section), Metadata);
  AEntry := TLiveScriptEntry.Create(Self, FirstLine, LastLine, Section,
    Metadata, EntryLines, LineKind = slkBlank);
  Result := True;
end;

function TLiveScriptObjectFactory.TryCreateDirectiveSection(const ASectionIndex: Integer;
  out ASection: TLiveScriptDirectiveSection;
  out ARefusalReason: String): Boolean;
begin
  ASection := nil;
  Result := False;
  EnsureIndex;
  EnsureStyled;

  if (ASectionIndex < 0) or (ASectionIndex >= FSections.Count) then begin
    ARefusalReason := 'The section index is out of range';
    Exit;
  end;
  if not (FSections[ASectionIndex].Section in [scSetup, scMessages, scCustomMessages, scLangOptions]) then begin
    ARefusalReason := 'The section is not a directive-style section';
    Exit;
  end;

  var FirstLine, LastLine: Integer;
  GetSectionLines(ASectionIndex, FirstLine, LastLine);
  var SectionLines: TArray<String>;
  if LastLine >= FirstLine then
    SectionLines := GetLinesText(FirstLine, LastLine)
  else
    SectionLines := nil;
  var Metadata: TScriptSectionMetadata := nil;
  TryGetScriptSectionMetadata(FSections[ASectionIndex].Name, Metadata);
  ASection := TLiveScriptDirectiveSection.Create(Self, FirstLine, LastLine,
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
  if (Length(ALines) = 0) and not (ALiveScriptObject is TLiveScriptDirectiveSection) then
    raise Exception.Create('Internal error: WriteBackChange: empty ALines but not a directive section');

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
      { Replace all of the object's lines' text, from the start of the first line
        to the end of the last line but excluding its line ending, with the new
        lines, so an empty ALines leaves a single empty line behind }
      FMemo.ReplaceTextRange(
        FMemo.GetPositionFromLine(ALiveScriptObject.FFirstLine),
        FMemo.GetLineEndPosition(ALiveScriptObject.FLastLine), Text,
        srmMinimal);
    end else if Length(ALines) > 0 then begin
      { The object has no lines yet (a directive-style section without lines):
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
    { The object now covers one line per element of ALines, or the single empty
      line left behind when ALines is empty }
    if Length(ALines) = 0 then
      ALiveScriptObject.FLastLine := ALiveScriptObject.FFirstLine
    else
      ALiveScriptObject.FLastLine := ALiveScriptObject.FFirstLine + Integer(Length(ALines)) - 1;
  finally
    FMemo.EndUndoAction;
    FWritingBackObject := nil;
  end;
end;

end.
