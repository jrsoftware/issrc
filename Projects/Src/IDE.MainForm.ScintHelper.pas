unit IDE.MainForm.ScintHelper;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Compiler form - Scintilla helper which has the auto complete and call tips helper as ancestor

  Not used by MainForm: it uses IDE.MainForm.FinalHelper instead
}

interface

uses
  Menus,
  ScintEdit,
  IDE.MainForm, IDE.MainForm.AutoCompleteAndCallTipsHelper;

type
  TMainFormScintHelper = class helper(TMainFormAutoCompleteAndCallTipsHelper) for TMainForm
    procedure SimplifySelection(const AMemo: TScintEdit);
    procedure AddCursorUpOrDown(const AMemo: TScintEdit; const Up: Boolean);
    procedure AddCursorsToLineEnds(const AMemo: TScintEdit);
    function MultipleSelectionPasteFromClipboard(const AMemo: TScintEdit): Boolean;
    procedure ToggleLinesComment(const AMemo: TScintEdit);
    procedure SelectAllFindMatches(const AMemo: TScintEdit);
  end;

implementation

uses
  SysUtils, Clipbrd, Math,
  ScintInt,
  IDE.HelperFunc, IDE.ScintStylerInnoSetup;

procedure TMainFormScintHelper.SimplifySelection(const AMemo: TScintEdit);
begin
  { The built in Esc (SCI_CANCEL) simply drops all additional selections
    and does not empty the main selection, It doesn't matter if Esc is
    pressed once or twice. Implement our own behaviour, same as VSCode.
    Also see https://github.com/microsoft/vscode/issues/118835. }
  if AMemo.SelectionCount > 1 then
    AMemo.RemoveAdditionalSelections
  else if not AMemo.SelEmpty then
   AMemo.SetEmptySelection;
  AMemo.ScrollCaretIntoView;
end;

procedure TMainFormScintHelper.AddCursorUpOrDown(const AMemo: TScintEdit; const Up: Boolean);
begin
  { Does not try to keep the main selection. }

  var Selections: TScintCaretAndAnchorList := nil;
  var VirtualSpaces: TScintCaretAndAnchorList := nil;
  try
    Selections := TScintCaretAndAnchorList.Create;
    VirtualSpaces := TScintCaretAndAnchorList.Create;
    { Get all the virtual spaces as well before we start doing modifications }
    AMemo.GetSelections(Selections, VirtualSpaces);
    for var I := 0 to Selections.Count-1 do begin
      var Selection := Selections[I];
      var LineCaret := AMemo.GetLineFromPosition(Selection.CaretPos);
      var LineAnchor := AMemo.GetLineFromPosition(Selection.AnchorPos);
      if LineCaret = LineAnchor then begin
        { Add selection with same caret and anchor offsets one line up or down. }
        var OtherLine := LineCaret + IfThen(Up, -1, 1);
        if (OtherLine < 0) or (OtherLine >= AMemo.Lines.Count) then
          Continue { Already at the top or bottom, can't add }
        else begin
          var LineStartPos := AMemo.GetPositionFromLine(LineCaret);
          var CaretCharacterCount := AMemo.GetCharacterCount(LineStartPos, Selection.CaretPos) + VirtualSpaces[I].CaretPos;
          var AnchorCharacterCount := AMemo.GetCharacterCount(LineStartPos, Selection.AnchorPos) + VirtualSpaces[I].AnchorPos;
          var OtherLineStart := AMemo.GetPositionFromLine(OtherLine);
          var MaxCharacterCount := AMemo.GetCharacterCount(OtherLineStart, AMemo.GetLineEndPosition(OtherLine));
          var NewCaretCharacterCount := CaretCharacterCount;
          //var NewCaretVirtualSpace := 0;
          var NewAnchorCharacterCount := AnchorCharacterCount;
          //var NewAnchorVirtualSpace := 0;
          if NewCaretCharacterCount > MaxCharacterCount then begin
            //NewCaretVirtualSpace := NewCaretCharacterCount - MaxCharacterCount;
            NewCaretCharacterCount := MaxCharacterCount;
          end;
          if NewAnchorCharacterCount > MaxCharacterCount then begin
            //NewAnchorVirtualSpace := NewAnchorCharacterCount - MaxCharacterCount;
            NewAnchorCharacterCount := MaxCharacterCount;
          end;
          var NewSelection: TScintCaretAndAnchor;
          NewSelection.CaretPos := AMemo.GetPositionRelative(OtherLineStart, NewCaretCharacterCount);
          NewSelection.AnchorPos := AMemo.GetPositionRelative(OtherLineStart, NewAnchorCharacterCount);
          { AddSelection trims selections except for the main selection so
            we need to check that ourselves unfortunately. Not doing a check
            gives a problem when you AddCursor two times starting with an
            empty single selection. The result will be 4 cursors, with 2 of
            them in the same place. The check below fixes this but not
            other cases when there's only partial overlap and Scintilla still
            behaves weird. The check also doesn't handle virtual space which
            is why we ultimately don't set virtual space: it leads to duplicate
            selections. }
          var MainSelection := AMemo.Selection;
          if not NewSelection.Range.Within(MainSelection) then begin
            AMemo.AddSelection(NewSelection.CaretPos, NewSelection.AnchorPos);
            { if svsUserAccessible in AMemo.VirtualSpaceOptions then begin
              var MainSel := AMemo.MainSelection;
              AMemo.SelectionCaretVirtualSpace[MainSel] := NewCaretVirtualSpace;
              AMemo.SelectionAnchorVirtualSpace[MainSel] := NewAnchorVirtualSpace;
            end; }
          end;
        end;
      end else begin
        { Extend multiline selection up or down. This is not the same as
          LineExtendUp/Down because those can shrink instead of extend. }
        var CaretBeforeAnchor := Selection.CaretPos < Selection.AnchorPos;
        var Down := not Up;
        var LineStartOrEnd, StartOrEndPos, VirtualSpace: Integer;
        { Does it start (when going up) or end (when going down) at the caret or the anchor? }
        if (Up and CaretBeforeAnchor) or (Down and not CaretBeforeAnchor) then begin
          LineStartOrEnd := LineCaret;
          StartOrEndPos := Selection.CaretPos;
          VirtualSpace := VirtualSpaces[I].CaretPos;
        end else begin
          LineStartOrEnd := LineAnchor;
          StartOrEndPos := Selection.AnchorPos;
          VirtualSpace := VirtualSpaces[I].AnchorPos;
        end;
        var NewStartOrEndPos: Integer;
        var NewVirtualSpace := 0;
        { Go up or down one line or to the start or end of the document }
        if (Up and (LineStartOrEnd > 0)) or (Down and  (LineStartOrEnd < AMemo.Lines.Count-1))  then begin
          var CharacterCount := AMemo.GetCharacterCount(AMemo.GetPositionFromLine(LineStartOrEnd), StartOrEndPos) + VirtualSpace;
          var OtherLine := LineStartOrEnd + IfThen(Up, -1, 1);
          var OtherLineStart := AMemo.GetPositionFromLine(OtherLine);
          var MaxCharacterCount := AMemo.GetCharacterCount(OtherLineStart, AMemo.GetLineEndPosition(OtherLine));
          var NewCharacterCount := CharacterCount;
          if NewCharacterCount > MaxCharacterCount then begin
            NewVirtualSpace := NewCharacterCount - MaxCharacterCount;
            NewCharacterCount := MaxCharacterCount;
          end;
          NewStartOrEndPos := AMemo.GetPositionRelative(OtherLineStart, NewCharacterCount);
        end else
          NewStartOrEndPos := IfThen(Up, 0, AMemo.GetPositionFromLine(AMemo.Lines.Count));
        { Move the caret or the anchor up or down to extend the selection }
        if (Up and CaretBeforeAnchor) or (Down and not CaretBeforeAnchor) then begin
          AMemo.SelectionCaretPosition[I] := NewStartOrEndPos;
          if svsUserAccessible in AMemo.VirtualSpaceOptions then
            AMemo.SelectionCaretVirtualSpace[I] := NewVirtualSpace;
        end else begin
          AMemo.SelectionAnchorPosition[I] := NewStartOrEndPos;
          if svsUserAccessible in AMemo.VirtualSpaceOptions then
            AMemo.SelectionAnchorVirtualSpace[I] := NewVirtualSpace;
        end;
      end;
    end;
  finally
    VirtualSpaces.Free;
    Selections.Free;
  end;
end;

procedure TMainFormScintHelper.AddCursorsToLineEnds(const AMemo: TScintEdit);
begin
  { Does not try to keep the main selection. Otherwise behaves the same as
    observed in Visual Studio Code, see comments. }

  var Selections: TScintCaretAndAnchorList := nil;
  var VirtualSpaces: TScintCaretAndAnchorList := nil;
  try
    Selections := TScintCaretAndAnchorList.Create;
    VirtualSpaces := TScintCaretAndAnchorList.Create;
    AMemo.GetSelections(Selections, VirtualSpaces);

    { First remove all empty selections }
    for var I := Selections.Count-1 downto 0 do begin
      var Selection := Selections[I];
      var VirtualSpace := VirtualSpaces[I];
      if (Selection.CaretPos + VirtualSpace.CaretPos) =
         (Selection.AnchorPos + VirtualSpace.AnchorPos) then begin
        Selections.Delete(I);
        VirtualSpaces.Delete(I);
      end;
    end;

    { If all selections were empty do nothing }
    if Selections.Count = 0 then
      Exit;

    { Handle non empty selections }
    for var I := Selections.Count-1 downto 0 do begin
      var Selection := Selections[I];
      var Line1 := AMemo.GetLineFromPosition(Selection.CaretPos);
      var Line2 := AMemo.GetLineFromPosition(Selection.AnchorPos);
      var SelSingleLine := Line1 = Line2;
      if SelSingleLine then begin
        { Single line selections are updated into empty selection at end of selection }
        var VirtualSpace := VirtualSpaces[I];
        if Selection.CaretPos + VirtualSpace.CaretPos > Selection.AnchorPos + VirtualSpace.AnchorPos then begin
          Selection.AnchorPos := Selection.CaretPos;
          VirtualSpace.AnchorPos := VirtualSpace.CaretPos;
        end else begin
          Selection.CaretPos := Selection.AnchorPos;
          VirtualSpace.CaretPos := VirtualSpace.AnchorPos;
        end;
        Selections[I] := Selection;
        VirtualSpaces[I] := VirtualSpace;
      end else begin
        { Multiline selections are replaced by empty selections at each end of line }
        if Line1 > Line2 then begin
          var TmpLine := Line1;
          Line1 := Line2;
          Line2 := TmpLine;
        end;
        { Ignore last line if the selection doesn't really select anything on that line }
        if Selection.Range.EndPos = AMemo.GetPositionFromLine(Line2) then
          Dec(Line2);
        for var Line := Line1 to Line2 do begin
          Selection.CaretPos := AMemo.GetLineEndPosition(Line);
          Selection.AnchorPos := Selection.CaretPos;
          Selections.Add(Selection);
          VirtualSpaces.Add(TScintCaretAndAnchor.Create(0, 0));
        end;
        Selections.Delete(I);
        VirtualSpaces.Delete(I);
      end;
    end;

    { Send updated selections to memo }
    for var I := 0 to Selections.Count-1 do begin
      var Selection := Selections[I];
      var VirtualSpace := VirtualSpaces[I];
      if I = 0 then
        AMemo.SetSingleSelection(Selection.CaretPos, Selection.AnchorPos)
      else
        AMemo.AddSelection(Selection.CaretPos, Selection.AnchorPos);
      AMemo.SelectionCaretVirtualSpace[I] := VirtualSpaces[I].CaretPos;
      AMemo.SelectionAnchorVirtualSpace[I] := VirtualSpaces[I].AnchorPos;
    end;
  finally
    VirtualSpaces.Free;
    Selections.Free;
  end;
end;

function TMainFormScintHelper.MultipleSelectionPasteFromClipboard(const AMemo: TScintEdit): Boolean;
begin
  { Scintilla doesn't yet properly support multiple selection paste. Handle it
    here, just like VS and VSCode do: if there's multiple selections and the paste
    text has the same amount of lines then paste 1 line per selection. Do this even
    if the paste text is marked as rectangular. Otherwise (so no match between
    the selection count and the line count) paste all lines into each selection.
    For the latter we don't need handling here: this is Scintilla's default
    behaviour if SC_MULTIPASTE_EACH is on. }
  Result := False;
  var SelectionCount := AMemo.SelectionCount;
  if SelectionCount > 1 then begin
    var PasteLines := Clipboard.AsText.Replace(#13#10, #13).Split([#13, #10]);
    if SelectionCount = Length(PasteLines) then begin
      AMemo.BeginUndoAction;
      try
        for var I := 0 to SelectionCount-1 do begin
          var StartPos := AMemo.SelectionStartPosition[I]; { Can't use AMemo.GetSelections because each paste can update other selections }
          var EndPos := AMemo.SelectionEndPosition[I];
          AMemo.ReplaceTextRange(StartPos, EndPos, PasteLines[I], srmMinimal);
          { Update the selection to an empty selection at the end of the inserted
            text, just like ReplaceMainSelText }
          var Pos := AMemo.Target.EndPos; { ReplaceTextRange updates the target }
          AMemo.SelectionCaretPosition[I] := Pos;
          AMemo.SelectionAnchorPosition[I] := Pos;
        end;
        { Be like SCI_PASTE }
        AMemo.ChooseCaretX;
        AMemo.ScrollCaretIntoView;
      finally
        AMemo.EndUndoAction;
      end;
      Result := True;
    end;
  end;
end;

procedure TMainFormScintHelper.ToggleLinesComment(const AMemo: TScintEdit);
begin
  { Based on SciTE 5.50's SciTEBase::StartBlockComment - only toggles comments
    for the main selection }

  var Selection := AMemo.Selection;
  var CaretPosition := AMemo.CaretPosition;
  // checking if caret is located in _beginning_ of selected block
  var MoveCaret := CaretPosition < Selection.EndPos;
  var SelStartLine := AMemo.GetLineFromPosition(Selection.StartPos);
  var SelEndLine := AMemo.GetLineFromPosition(Selection.EndPos);
  var Lines := SelEndLine - SelStartLine;
  var FirstSelLineStart := AMemo.GetPositionFromLine(SelStartLine);
  // "caret return" is part of the last selected line
  if (Lines > 0) and (Selection.EndPos = AMemo.GetPositionFromLine(SelEndLine)) then
    Dec(SelEndLine);
  { We rely on the styler to identify [Code] section lines, but we
    may be searching into areas that haven't been styled yet }
  AMemo.StyleNeeded(Selection.EndPos);
  AMemo.BeginUndoAction;
  try
    var LastLongCommentLength := 0;
    for var I := SelStartLine to SelEndLine do begin
      var LineIndent := AMemo.GetLineIndentPosition(I);
      var LineEnd := AMemo.GetLineEndPosition(I);
      var LineBuf := AMemo.GetTextRange(LineIndent, LineEnd);
      // empty lines are not commented
      if LineBuf = '' then
        Continue;
      var Comment: String;
      if LineBuf.StartsWith('//') or
         (FMemosStyler.GetSectionFromLineState(AMemo.Lines.State[I]) = scCode) then
        Comment := '//'
      else
        Comment := ';';
      var LongComment := Comment + ' ';
      LastLongCommentLength := Length(LongComment);
      if LineBuf.StartsWith(Comment) then begin
        var CommentLength := Length(Comment);
        if LineBuf.StartsWith(LongComment) then begin
          // Removing comment with space after it.
          CommentLength := Length(LongComment);
        end;
        AMemo.Selection := TScintRange.Create(LineIndent, LineIndent + CommentLength);
        AMemo.SelText := '';
        if I = SelStartLine then // is this the first selected line?
          Dec(Selection.StartPos, CommentLength);
        Dec(Selection.EndPos, CommentLength); // every iteration
        Continue;
      end;
      if I = SelStartLine then // is this the first selected line?
        Inc(Selection.StartPos, Length(LongComment));
      Inc(Selection.EndPos, Length(LongComment)); // every iteration
      AMemo.Call(SCI_INSERTTEXT, LineIndent, AMemo.ConvertStringToRawString(LongComment));
    end;
    // after uncommenting selection may promote itself to the lines
    // before the first initially selected line;
    // another problem - if only comment symbol was selected;
    if Selection.StartPos < FirstSelLineStart then begin
      if Selection.StartPos >= Selection.EndPos - (LastLongCommentLength - 1) then
        Selection.EndPos := FirstSelLineStart;
      Selection.StartPos := FirstSelLineStart;
    end;
    if MoveCaret then begin
      // moving caret to the beginning of selected block
      AMemo.CaretPosition := Selection.EndPos;
      AMemo.CaretPositionWithSelectFromAnchor := Selection.StartPos;
    end else
      AMemo.Selection := Selection;
  finally
    AMemo.EndUndoAction;
  end;
end;

procedure TMainFormScintHelper.SelectAllFindMatches(const AMemo: TScintEdit);
begin
  var StartPos := 0;
  var EndPos := AMemo.RawTextLength;
  var FoundRange: TScintRange;
  var ClosestSelection := -1;
  var ClosestSelectionDistance := 0; { Silence compiler }
  var CaretPos := AMemo.CaretPosition;

  while (StartPos < EndPos) and
        AMemo.FindText(StartPos, EndPos, FLastFindText,
          FindOptionsToSearchOptions(FLastFindOptions, FLastFindRegEx), FoundRange) do begin
    if StartPos = 0 then
      AMemo.SetSingleSelection(FoundRange.EndPos, FoundRange.StartPos)
    else
      AMemo.AddSelection(FoundRange.EndPos, FoundRange.StartPos);

    var Distance := Abs(CaretPos-FoundRange.EndPos);
    if (ClosestSelection = -1) or (Distance < ClosestSelectionDistance) then begin
      ClosestSelection := AMemo.SelectionCount-1;
      ClosestSelectionDistance := Distance;
    end;

    StartPos := FoundRange.EndPos;
  end;
  if ClosestSelection <> -1 then begin
    AMemo.MainSelection := ClosestSelection;
    AMemo.ScrollCaretIntoView;
  end;
end;

end.
