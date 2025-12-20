unit IDE.MainForm.FindReplaceHelper;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Compiler form - Find & Replace helper which has the navigation helper as ancestor

  Not used by MainForm: it uses IDE.MainForm.FinalHelper instead
}

interface

uses
  Dialogs,
  IDE.MainForm, IDE.MainForm.NavigationHelper;

type
  TMainFormFindReplaceHelper = class helper(TMainFormNavigationHelper) for TMainForm
    procedure ShowFindDialog(const Down: Boolean);
    procedure ShowFindInFilesDialog;
    procedure DoFindNext(const Down: Boolean);
    procedure DoFindOrReplaceDialogFind(const Dialog: TFindDialog);
    procedure DoFindInFilesDialogFind;
    procedure UpdateFindResult(const FindResult: TFindResult; const ItemIndex: Integer;
      const NewLine, NewLineStartPos: Integer);
    function FindSetupDirectiveValue(const DirectiveName,
      DefaultValue: String): String; overload;
    function FindSetupDirectiveValue(const DirectiveName: String;
      DefaultValue: Boolean): Boolean; overload;
    procedure ShowReplaceDialog;
    procedure DoReplaceDialogReplace;
    { Private }
    procedure _InitializeFindText(Dlg: TFindDialog);
    procedure _FindNext(const ReverseDirection: Boolean);
    function _StoreAndTestLastFindOptions(const Dialog: TFindDialog): Boolean;
    function _TestLastFindOptions: Boolean;
  end;

implementation

uses
  Windows, Messages,
  Classes, SysUtils, StrUtils,  Menus,
  ScintEdit,
  Shared.CommonFunc, Shared.CommonFunc.Vcl,
  IDE.Messages, IDE.HelperFunc, IDE.ScintStylerInnoSetup;

const
  OldFindReplaceWndProcProp = 'OldFindReplaceWndProc';

function FindReplaceWndProc(Wnd: HWND; Msg: Cardinal; WParam: WPARAM; LParam: LPARAM): LRESULT; stdcall;

  function CallDefWndProc: LRESULT;
  begin
    Result := CallWindowProc(Pointer(GetProp(Wnd, OldFindReplaceWndProcProp)), Wnd,
      Msg, WParam, LParam);
  end;

begin
  case Msg of
    WM_MENUCHAR:
      if LoWord(wParam) = VK_RETURN then begin
        var hwndCtl := GetDlgItem(Wnd, idOk);
        if (hWndCtl <> 0) and IsWindowEnabled(hWndCtl) then
          PostMessage(Wnd, WM_COMMAND, MakeWParam(idOk, BN_CLICKED), Windows.LPARAM(hWndCtl));
      end;
    WM_NCDESTROY:
      begin
        Result := CallDefWndProc;
        RemoveProp(Wnd, OldFindReplaceWndProcProp);
        Exit;
      end;
   end;
   Result := CallDefWndProc;
end;

procedure ExecuteFindDialogAllowingAltEnter(const FindDialog: TFindDialog);
begin
  var DoHook := FindDialog.Handle = 0;
  FindDialog.Execute;
  if DoHook then begin
    SetProp(FindDialog.Handle, OldFindReplaceWndProcProp, GetWindowLongPtr(FindDialog.Handle, GWL_WNDPROC));
    SetWindowLongPtr(FindDialog.Handle, GWL_WNDPROC, LONG_PTR(@FindReplaceWndProc));
  end;
end;

{  TMainFormFindReplaceHelper }

procedure TMainFormFindReplaceHelper._InitializeFindText(Dlg: TFindDialog);
var
  S: String;
begin
  S := FActiveMemo.MainSelText;
  if (S <> '') and (Pos(#13, S) = 0) and (Pos(#10, S) = 0) then
    Dlg.FindText := S
  else
    Dlg.FindText := FLastFindText;
end;

procedure TMainFormFindReplaceHelper.ShowFindDialog(const Down: Boolean);
begin
  ReplaceDialog.CloseDialog;
  if FindDialog.Handle = 0 then
    _InitializeFindText(FindDialog);
  if Down then
    FindDialog.Options := FindDialog.Options + [frDown]
  else
    FindDialog.Options := FindDialog.Options - [frDown];
  ExecuteFindDialogAllowingAltEnter(FindDialog);
end;

procedure TMainFormFindReplaceHelper.ShowFindInFilesDialog;
begin
  _InitializeFindText(FindInFilesDialog);
  FindInFilesDialog.Execute;
end;

procedure TMainFormFindReplaceHelper.DoFindNext(const Down: Boolean);
begin
  if FLastFindText = '' then
    ShowFindDialog(Down)
  else begin
    if Down then
      FLastFindOptions := FLastFindOptions + [frDown]
    else
      FLastFindOptions := FLastFindOptions - [frDown];
    FLastFindRegEx := FOptions.FindRegEx;
    if not _TestLastFindOptions then
      Exit;
    _FindNext(False);
  end;
end;

procedure TMainFormFindReplaceHelper._FindNext(const ReverseDirection: Boolean);
var
  StartPos, EndPos: Integer;
  Range: TScintRange;
begin
  var Down := frDown in FLastFindOptions;
  if ReverseDirection then
    Down := not Down;
  if Down then begin
    StartPos := FActiveMemo.Selection.EndPos;
    EndPos := FActiveMemo.RawTextLength;
  end
  else begin
    StartPos := FActiveMemo.Selection.StartPos;
    EndPos := 0;
  end;
  if FActiveMemo.FindText(StartPos, EndPos, FLastFindText,
     FindOptionsToSearchOptions(FLastFindOptions, FLastFindRegEx), Range) then
    FActiveMemo.SelectAndEnsureVisible(Range)
  else
    MsgBoxFmt('Cannot find "%s"', [FLastFindText], SCompilerFormCaption,
      mbInformation, MB_OK);
end;

function TMainFormFindReplaceHelper._StoreAndTestLastFindOptions(const Dialog: TFindDialog): Boolean;
begin
  { TReplaceDialog is a subclass of TFindDialog must check for TReplaceDialog first }
  if Dialog is TReplaceDialog then begin
    with Dialog as TReplaceDialog do begin
      FLastFindOptions := Options;
      FLastFindText := FindText;
    end;
  end else begin
    with Dialog do begin
      FLastFindOptions := Options;
      FLastFindText := FindText;
    end;
  end;
  FLastFindRegEx := FOptions.FindRegEx;

  Result := _TestLastFindOptions;
end;

function TMainFormFindReplaceHelper._TestLastFindOptions;
begin
  if FLastFindRegEx then begin
    Result := FActiveMemo.TestRegularExpression(FLastFindText);
    if not Result then
      MsgBoxFmt('Invalid regular expression "%s"', [FLastFindText], SCompilerFormCaption,
        mbError, MB_OK);
  end else
    Result := True;
end;

procedure TMainFormFindReplaceHelper.DoFindOrReplaceDialogFind(const Dialog: TFindDialog);
begin
  if not _StoreAndTestLastFindOptions(Dialog) then
    Exit;

  if GetKeyState(VK_MENU) < 0 then begin
    { Alt+Enter was used to close the dialog }
    Dialog.CloseDialog;
    ESelectAllFindMatchesClick(Self); { Uses the copy made above }
  end else
    _FindNext(GetKeyState(VK_SHIFT) < 0);
end;

procedure TMainFormFindReplaceHelper.DoFindInFilesDialogFind;
begin
  if not _StoreAndTestLastFindOptions(FindInFilesDialog) then
    Exit;

  FindResultsList.Clear;
  SendMessage(FindResultsList.Handle, LB_SETHORIZONTALEXTENT, 0, 0);
  FFindResults.Clear;

  var Hits := 0;
  var Files := 0;

  for var Memo in FFileMemos do begin
    if Memo.Used then begin
      var StartPos := 0;
      var EndPos := Memo.RawTextLength;
      var FileHits := 0;
      var Range: TScintRange;
      while (StartPos < EndPos) and
            Memo.FindText(StartPos, EndPos, FLastFindText,
              FindOptionsToSearchOptions(FLastFindOptions, FLastFindRegEx), Range) do begin
        { Also see UpdateFindResult }
        var Line := Memo.GetLineFromPosition(Range.StartPos);
        var Prefix := Format('  Line %d: ', [Line+1]);
        var FindResult := TFindResult.Create;
        FindResult.Filename := Memo.Filename;
        FindResult.Line := Line;
        FindResult.LineStartPos := Memo.GetPositionFromLine(Line);
        FindResult.Range := Range;
        FindResult.PrefixStringLength := Length(Prefix);
        FFindResults.Add(FindResult);
        FindResultsList.Items.AddObject(Prefix + Memo.Lines[Line], FindResult);
        Inc(FileHits);
        StartPos := Range.EndPos;
      end;
      Inc(Files);
      if FileHits > 0 then begin
        Inc(Hits, FileHits);
        FindResultsList.Items.Insert(FindResultsList.Count-FileHits, Format('%s (%d hits):', [Memo.Filename, FileHits]));
      end;
    end;
  end;

  FindResultsList.Items.Insert(0, Format('Find "%s" (%d hits in %d files)', [FindInFilesDialog.FindText, Hits, Files]));

  FindInFilesDialog.CloseDialog;

  OutputTabSet.TabIndex := tiFindResults;
  SetStatusPanelVisible(True);
end;

procedure TMainFormFindReplaceHelper.UpdateFindResult(const FindResult: TFindResult; const ItemIndex: Integer;
  const NewLine, NewLineStartPos: Integer);
begin
  { Also see DoFindInFilesDialogFind }
  const OldPrefix = Format('  Line %d: ', [FindResult.Line+1]);
  FindResult.Line := NewLine;
  const NewPrefix = Format('  Line %d: ', [FindResult.Line+1]);
  FindResultsList.Items[ItemIndex] := NewPrefix + Copy(FindResultsList.Items[ItemIndex], Length(OldPrefix)+1, MaxInt);
  FindResult.PrefixStringLength := Length(NewPrefix);
  const PosChange = NewLineStartPos - FindResult.LineStartPos;
  FindResult.LineStartPos := NewLineStartPos;
  FindResult.Range.StartPos := FindResult.Range.StartPos + PosChange;
  FindResult.Range.EndPos := FindResult.Range.EndPos + PosChange;
end;

function TMainFormFindReplaceHelper.FindSetupDirectiveValue(const DirectiveName,
  DefaultValue: String): String;
begin
  Result := DefaultValue;

  var Memo := FMainMemo; { This function only searches the main file }
  var StartPos := 0;
  var EndPos := Memo.RawTextLength;
  var Range: TScintRange;

  { We rely on the styler to identify [Setup] section lines, but we
    may be searching into areas that haven't been styled yet }
  Memo.StyleNeeded(EndPos);

  while (StartPos < EndPos) and
        Memo.FindText(StartPos, EndPos, DirectiveName, [sfoWholeWord], Range) do begin
    var Line := Memo.GetLineFromPosition(Range.StartPos);
    if FMemosStyler.GetSectionFromLineState(Memo.Lines.State[Line]) = scSetup then begin
      var LineValue := Memo.Lines[Line].Trim; { LineValue can't be empty }
      if LineValue[1] <> ';' then begin
        var LineParts := LineValue.Split(['=']);
        if (Length(LineParts) = 2) and SameText(LineParts[0].Trim, DirectiveName) then begin
          Result := LineParts[1].Trim;
          { If Result is surrounded in quotes, remove them, just like TSetupCompiler.SeparateDirective }
          if (Length(Result) >= 2) and
             (Result[1] = '"') and (Result[Length(Result)] = '"') then
            Result := Copy(Result, 2, Length(Result)-2);
          Exit; { Compiler doesn't allow a directive to be specified twice so we can exit now }
        end;
      end;
    end;
    StartPos := Range.EndPos;
  end;
end;

function TMainFormFindReplaceHelper.FindSetupDirectiveValue(const DirectiveName: String;
  DefaultValue: Boolean): Boolean;
begin
  var Value := FindSetupDirectiveValue(DirectiveName, IfThen(DefaultValue, '1', '0'));
  if not TryStrToBoolean(Value, Result) then
    Result := DefaultValue;
end;

procedure TMainFormFindReplaceHelper.ShowReplaceDialog;
begin
  FindDialog.CloseDialog;
  if ReplaceDialog.Handle = 0 then begin
    _InitializeFindText(ReplaceDialog);
    ReplaceDialog.ReplaceText := FLastReplaceText;
  end;
  ExecuteFindDialogAllowingAltEnter(ReplaceDialog);
end;

procedure TMainFormFindReplaceHelper.DoReplaceDialogReplace;
begin
  if not _StoreAndTestLastFindOptions(ReplaceDialog) then
    Exit;

  FLastReplaceText := ReplaceDialog.ReplaceText;
  var ReplaceMode := RegExToReplaceMode(FLastFindRegEx);

  if frReplaceAll in FLastFindOptions then begin
    var ReplaceCount := 0;
    FActiveMemo.BeginUndoAction;
    try
      var Pos := 0;
      var Range: TScintRange;
      while FActiveMemo.FindText(Pos, FActiveMemo.RawTextLength, FLastFindText,
         FindOptionsToSearchOptions(FLastFindOptions, FLastFindRegEx), Range) do begin
        var NewRange := FActiveMemo.ReplaceTextRange(Range.StartPos, Range.EndPos, FLastReplaceText, ReplaceMode);
        Pos := NewRange.EndPos;
        Inc(ReplaceCount);
      end;
    finally
      FActiveMemo.EndUndoAction;
    end;
    if ReplaceCount = 0 then
      MsgBoxFmt('Cannot find "%s"', [FLastFindText], SCompilerFormCaption,
        mbInformation, MB_OK)
    else
      MsgBoxFmt('%d occurrence(s) replaced.', [ReplaceCount], SCompilerFormCaption,
        mbInformation, MB_OK);
  end
  else begin
    if FActiveMemo.MainSelTextEquals(FLastFindText, FindOptionsToSearchOptions(frMatchCase in FLastFindOptions, FLastFindRegEx)) then begin
      { Note: the MainSelTextEquals above performs a search so the replacement
        below is safe even if the user just enabled regex }
      FActiveMemo.ReplaceMainSelText(FLastReplaceText, ReplaceMode);
    end;
    _FindNext(GetKeyState(VK_SHIFT) < 0);
  end;
end;

end.
