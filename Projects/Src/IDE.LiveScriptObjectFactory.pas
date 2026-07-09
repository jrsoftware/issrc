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
  public
    constructor Create(const AMemo: TScintEdit; const AStyler: TInnoSetupStyler);
    destructor Destroy; override;
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

begin
  if not FIndexValid then
    BuildIndex
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
