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
  IDE.ScintStylerInnoSetup;

type
  TLiveScriptSection = record
    Line: Integer;
    Section: TInnoSetupStylerSection;
    Name: String;
  end;

  TLiveScriptObjectFactory = class
  private
    FMemo: TScintEdit;
    FStyler: TInnoSetupStyler;
    FSections: TList<TLiveScriptSection>; { Includes scUnknown/scThirdParty section }
    FIndexValid: Boolean;
    FDirtyFirstLine, FDirtyLastLine: Integer; { -1 when nothing is dirty }
    procedure EnsureIndex;
    procedure EnsureStyled;
    function GetHeaderSection(const ALine: Integer;
      out ASection: TLiveScriptSection): Boolean;
    function GetSection(Index: Integer): TLiveScriptSection;
    procedure GetSectionContentRange(const ASectionIndex: Integer;
      out AFirstLine, ALastLine: Integer);
    function LineSpans(const ALine: Integer): Boolean;
    procedure MarkLinesDirty(const AFirstLine, ALastLine: Integer);
  public
    constructor Create(const AMemo: TScintEdit; const AStyler: TInnoSetupStyler);
    destructor Destroy; override;
    procedure Change(const Info: TScintEditChangeInfo);
    procedure InvalidateIndex;
    function SectionCount: Integer;
    function TryGetSectionAtLine(const ALine: Integer;
      out ASectionIndex: Integer): Boolean;
    property Sections[Index: Integer]: TLiveScriptSection read GetSection;
    property Styler: TInnoSetupStyler read FStyler;
  end;

implementation

uses
  SysUtils;

{ TLiveScriptObjectFactory }

constructor TLiveScriptObjectFactory.Create(const AMemo: TScintEdit;
  const AStyler: TInnoSetupStyler);
begin
  inherited Create;
  FMemo := AMemo;
  FStyler := AStyler;
  FSections := TList<TLiveScriptSection>.Create;
  FDirtyFirstLine := -1;
  FDirtyLastLine := -1;
end;

destructor TLiveScriptObjectFactory.Destroy;
begin
  FSections.Free;
  inherited;
end;

function TLiveScriptObjectFactory.LineSpans(const ALine: Integer): Boolean;
begin
  Result := TInnoSetupStyler.LineSpans(FMemo.Lines.RawLines[ALine]);
end;

function TLiveScriptObjectFactory.GetHeaderSection(const ALine: Integer;
  out ASection: TLiveScriptSection): Boolean;

  function ExtractSectionHeaderName(const S: String): String;
  begin
    { Matches the styler's section header recognition in
      TInnoSetupStyler.StyleNeeded: '[' + AlphaUnderscoreChars + ']' }
    Result := '';
    const P = Pos('[', S);
    if P = 0 then
      Exit;
    var I := P+1;
    while (I <= Length(S)) and CharInSet(S[I], ['A'..'Z', 'a'..'z', '_']) do
      Inc(I);
    if (I <= Length(S)) and (S[I] = ']') then
      Result := Copy(S, P+1, I-P-1);
  end;

begin
  { ISPP's line continuation (see LineSpans) joins physical lines into one
    logical line, and the styler gives them all the same line state, so a
    spanned header's continuation lines would also report the header, even
    if such a header doesn't compile. Callers must pass only the first
    physical line of a spanned header. }
  var Section: TInnoSetupStylerSection;
  Result := TInnoSetupStyler.LineSectionHeader(FMemo.Lines.State[ALine], Section);
  if Result then begin
    ASection.Line := ALine;
    ASection.Section := Section;
    ASection.Name := ExtractSectionHeaderName(FMemo.Lines[ALine]);
  end;
end;

procedure TLiveScriptObjectFactory.EnsureIndex;

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
end;

procedure TLiveScriptObjectFactory.MarkLinesDirty(const AFirstLine, ALastLine: Integer);
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

procedure TLiveScriptObjectFactory.Change(const Info: TScintEditChangeInfo);
begin
  if not FIndexValid then
    Exit;

  { Also see TMainForm.MemoChange }

  var FirstLine := FMemo.GetLineFromPosition(Info.StartPos);
  const FirstAffectedLine = FirstLine;
  { If the change does not start on the first character of the line, the
    line itself keeps its number (same convention as TMainForm.MemoChange) }
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
  EnsureStyled;
  Result := False;
  for var I := Integer(FSections.Count)-1 downto 0 do begin
    if FSections[I].Line <= ALine then begin
      var FirstLine, LastLine: Integer;
      GetSectionContentRange(I, FirstLine, LastLine);
      if (ALine < FirstLine) or (ALine <= LastLine) then begin
        ASectionIndex := I;
        Result := True;
      end;
      Exit;
    end;
  end;
end;

procedure TLiveScriptObjectFactory.GetSectionContentRange(const ASectionIndex: Integer;
  out AFirstLine, ALastLine: Integer);
{ The returned range can be empty (ALastLine < AFirstLine) }
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

end.
