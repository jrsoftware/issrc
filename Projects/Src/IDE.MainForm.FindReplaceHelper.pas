unit IDE.MainForm.FindReplaceHelper;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
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
    procedure ShowReplaceDialog;
    procedure DoReplaceDialogReplace;
    { Private }
    procedure _InitializeFindText(Dialog: TFindDialog);
    procedure _PrepareToExecute(const Dialog: TFindDialog);
    procedure _FindNext(const ReverseDirection: Boolean);
    function _StoreAndTestLastFindOptions(const Dialog: TFindDialog): Boolean;
    function _TestLastFindOptions: Boolean;
  end;

implementation

uses
  Windows, Messages,
  SysUtils,
  ScintEdit,
  Shared.CommonFunc, Shared.CommonFunc.Vcl,
  IDE.Messages, IDE.LocalizeFunc, IDE.HelperFunc;

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
    SetProp(FindDialog.Handle, OldFindReplaceWndProcProp, THandle(GetWindowLongPtr(FindDialog.Handle, GWLP_WNDPROC)));
    SetWindowLongPtr(FindDialog.Handle, GWLP_WNDPROC, LONG_PTR(@FindReplaceWndProc));
  end;
end;

function FindResultLinePrefix(const LineNumber: Integer): String;
begin
  Result := '  ' + LFmtMessage(SFindResultLinePrefix, [LineNumber]) + ' ';
end;

{  TMainFormFindReplaceHelper }

procedure TMainFormFindReplaceHelper._InitializeFindText(Dialog: TFindDialog);
var
  S: String;
begin
  S := FActiveMemo.MainSelText;
  if (S <> '') and (Pos(#13, S) = 0) and (Pos(#10, S) = 0) then
    Dialog.FindText := S
  else
    Dialog.FindText := FLastFindText;
end;

procedure TMainFormFindReplaceHelper._PrepareToExecute(const Dialog: TFindDialog);
{ Updates the non-TFindDialog-UI Options. To be called before executing the
  dialog, because TFindDialog restores them to the values it was opened with on
  any Find/Replace/close action (in Vcl.Dialogs). Setting them only when they
  change in our own UI, like EFindRegEx, doesn't work, because they would get
  lost when the dialog happens to be open }
begin
  if FOptions.FindRegEx then
    Dialog.Options := Dialog.Options + [frHideWholeWord]
  else
    Dialog.Options := Dialog.Options - [frHideWholeWord];
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
  _PrepareToExecute(FindDialog);
  ExecuteFindDialogAllowingAltEnter(FindDialog);
end;

procedure TMainFormFindReplaceHelper.ShowFindInFilesDialog;
begin
  _InitializeFindText(FindInFilesDialog);
  _PrepareToExecute(FindInFilesDialog);
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
    FActiveMemo.SelectAndEnsureInView(Range)
  else
    MsgBox(LFmtMessage(SFindNotFound, [FLastFindText]), LFmtMessage(SCompilerFormCaption),
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
      MsgBox(LFmtMessage(SFindInvalidRegEx, [FLastFindText]), LFmtMessage(SCompilerFormCaption),
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
  FFindResults.Clear;

  var Hits := 0;
  var Files := 0;

  for var Memo in FFileMemos do begin
    if Memo.Used then begin
      var StartPos := 0;
      var EndPos := Memo.RawTextLength;
      var FileHits := 0;
      var Range: TScintRange;
      while Memo.FindText(StartPos, EndPos, FLastFindText,
              FindOptionsToSearchOptions(FLastFindOptions, FLastFindRegEx), Range) do begin
        { Also see UpdateFindResult }
        var Line := Memo.GetLineFromPosition(Range.StartPos);
        var Prefix := FindResultLinePrefix(Line+1);
        var FindResult := TFindResult.Create;
        FindResult.Filename := Memo.Filename;
        FindResult.Line := Line;
        FindResult.LineStartPos := Memo.GetPositionFromLine(Line);
        FindResult.Range := Range;
        FindResult.StartIndex := Length(Prefix) + Memo.GetCodeUnitCount(FindResult.LineStartPos, Range.StartPos) + 1;
        FindResult.EndIndex := Length(Prefix) + Memo.GetCodeUnitCount(FindResult.LineStartPos, Range.EndPos) + 1;
        FFindResults.Add(FindResult);
        FindResultsList.Items.AddObject(Prefix + Memo.Lines[Line], FindResult);
        Inc(FileHits);
        StartPos := Range.EndPos;
        { If the match was empty the next search would match at the same position, so must manually progress }
        if Range.Empty then begin
          if StartPos = EndPos then
            Break;
          StartPos := Memo.GetPositionAfter(StartPos);
        end;
      end;
      if FileHits > 0 then begin
        Inc(Files);
        Inc(Hits, FileHits);
        FindResultsList.Items.Insert(FindResultsList.Count-FileHits, LFmtMessage(SFindResultFileHeader, [Memo.Filename, FileHits]));
      end;
    end;
  end;

  FindResultsList.Items.Insert(0, LFmtMessage(SFindResultSummary, [FindInFilesDialog.FindText, Hits, Files]));
  UpdateScrollWidth(FindResultsList);

  FindInFilesDialog.CloseDialog;

  OutputTabSet.TabIndex := tiFindResults;
  SetStatusPanelVisible(True);
end;

procedure TMainFormFindReplaceHelper.UpdateFindResult(const FindResult: TFindResult; const ItemIndex: Integer;
  const NewLine, NewLineStartPos: Integer);
begin
  { Also see DoFindInFilesDialogFind }
  const OldPrefix = FindResultLinePrefix(FindResult.Line+1);
  FindResult.Line := NewLine;
  const NewPrefix = FindResultLinePrefix(FindResult.Line+1);
  FindResultsList.Items[ItemIndex] := NewPrefix + Copy(FindResultsList.Items[ItemIndex], Length(OldPrefix)+1, MaxInt);
  const PosChange = NewLineStartPos - FindResult.LineStartPos;
  FindResult.LineStartPos := NewLineStartPos;
  FindResult.Range.StartPos := FindResult.Range.StartPos + PosChange;
  FindResult.Range.EndPos := FindResult.Range.EndPos + PosChange;
  const PrefixChange = Length(NewPrefix) - Length(OldPrefix);
  FindResult.StartIndex := FindResult.StartIndex + PrefixChange;
  FindResult.EndIndex := FindResult.EndIndex + PrefixChange;
end;

procedure TMainFormFindReplaceHelper.ShowReplaceDialog;
begin
  FindDialog.CloseDialog;
  if ReplaceDialog.Handle = 0 then begin
    _InitializeFindText(ReplaceDialog);
    ReplaceDialog.ReplaceText := FLastReplaceText;
  end;
  _PrepareToExecute(ReplaceDialog);
  ExecuteFindDialogAllowingAltEnter(ReplaceDialog);
end;

procedure TMainFormFindReplaceHelper.DoReplaceDialogReplace;
begin
  if not _StoreAndTestLastFindOptions(ReplaceDialog) then
    Exit;

  FLastReplaceText := ReplaceDialog.ReplaceText;

  { The Replace dialog is modeless, so the active memo can be read-only }
  if FActiveMemo.ReadOnly then begin
    SysUtils.Beep;
    Exit; { Replace All would loop endlessly }
  end;

  var ReplaceMode := RegExToReplaceMode(FLastFindRegEx);

  if frReplaceAll in FLastFindOptions then begin
    var ReplaceCount := 0;
    FActiveMemo.BeginUndoAction;
    try
      var StartPos := 0;
      var EndPos := FActiveMemo.RawTextLength;
      var Range: TScintRange;
      while FActiveMemo.FindText(StartPos, EndPos, FLastFindText,
              FindOptionsToSearchOptions(FLastFindOptions, FLastFindRegEx), Range) do begin
        var NewRange := FActiveMemo.ReplaceTextRange(Range.StartPos, Range.EndPos, FLastReplaceText, ReplaceMode);
        Inc(ReplaceCount);
        StartPos := NewRange.EndPos;
        EndPos := FActiveMemo.RawTextLength; { Update for replacement }
        { If the match was empty there's no consumed text guaranteeing progress past it, so must manually progress }
        if Range.Empty then begin
          if StartPos = EndPos then
            Break;
          StartPos := FActiveMemo.GetPositionAfter(StartPos);
        end;
      end;
    finally
      FActiveMemo.EndUndoAction;
    end;
    if ReplaceCount = 0 then
      MsgBox(LFmtMessage(SFindNotFound, [FLastFindText]), LFmtMessage(SCompilerFormCaption),
        mbInformation, MB_OK)
    else
      MsgBox(LFmtMessage(SReplaceCount, [ReplaceCount]), LFmtMessage(SCompilerFormCaption),
        mbInformation, MB_OK);
  end
  else begin
    if FActiveMemo.MainSelTextEquals(FLastFindText, FindOptionsToSearchOptions(FLastFindOptions, FLastFindRegEx)) then begin
      { Note: the MainSelTextEquals above performs a search so the replacement
        below is safe even if the user just enabled regex }
      FActiveMemo.ReplaceMainSelText(FLastReplaceText, ReplaceMode);
    end;
    _FindNext(GetKeyState(VK_SHIFT) < 0);
  end;
end;

end.
