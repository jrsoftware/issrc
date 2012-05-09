unit IsppDebug;

interface

implementation

uses Windows, SysUtils, JclHookExcept, JclDebug, TypInfo, IsppTranslate,
  IsppExceptWindow, Forms, IsppIdentMan;

procedure NotifyException(ExceptObj: TObject; ExceptAddr: Pointer;
  OSException: Boolean);
var
  TmpS: string;
  ModInfo: TJclLocationInfo;
  I: Integer;
  ExceptionHandled: Boolean;
  HandlerLocation: Pointer;
  ExceptFrame: TJclExceptFrame;
begin
  if ExceptObj is EPreprocError then Exit;
  if ExceptObj is EIdentError then Exit;
  with TIsppExceptWnd.Create(nil) do
  try
    mmLog.Lines.Clear;
    TmpS := 'Exception ' + ExceptObj.ClassName;
    if ExceptObj is Exception then
      TmpS := TmpS + ': ' + Exception(ExceptObj).Message;
    if OSException then
      TmpS := TmpS + ' (OS Exception)';
    mmLog.Lines.Add(TmpS);
    ModInfo := GetLocationInfo(ExceptAddr);
    mmLog.Lines.Add('');
    mmLog.Lines.Add(Format(
      '  Exception occured at $%p (%s.%s@%d)',
      [ModInfo.Address,
       ModInfo.UnitName,
       ModInfo.ProcedureName,
       //ModInfo.SourceName,
       ModInfo.LineNumber]));
    if stExceptFrame in JclStackTrackingOptions then
    begin
      mmLog.Lines.Add('');
      mmLog.Lines.Add('');
      mmLog.Lines.Add('  Except frame-dump:');
      mmLog.Lines.Add('');
      I := 0;
      ExceptionHandled := False;
      while not ExceptionHandled and
        (I < JclLastExceptFrameList.Count) do
      begin
        ExceptFrame := JclLastExceptFrameList.Items[I];
        ExceptionHandled := ExceptFrame.HandlerInfo(ExceptObj, HandlerLocation);
        if (ExceptFrame.FrameKind = efkFinally) or
            (ExceptFrame.FrameKind = efkUnknown) or
            not ExceptionHandled then
          HandlerLocation := ExceptFrame.CodeLocation;
        ModInfo := GetLocationInfo(HandlerLocation);
        TmpS := Format(
          '    Frame at $%p (type: %s',
          [ExceptFrame.ExcFrame,
           GetEnumName(TypeInfo(TExceptFrameKind), Ord(ExceptFrame.FrameKind))]);
        if ExceptionHandled then
          TmpS := TmpS + ', handles exception)'
        else
          TmpS := TmpS + ')';
        mmLog.Lines.Add(TmpS);
        if ExceptionHandled then
          mmLog.Lines.Add(Format(
            '      Handler at $%p',
            [HandlerLocation]))
        else
          mmLog.Lines.Add(Format(
            '      Code at $%p',
            [HandlerLocation]));
        mmLog.Lines.Add(Format(
          '      %s.%s@%d',
          [ModInfo.UnitName,
           ModInfo.ProcedureName,
           //ModInfo.SourceName,
           ModInfo.LineNumber]));
        Inc(I);
        mmLog.Lines.Add('');
      end;
    end;
    mmLog.Lines.Add('');
    ShowModal;
  finally
    Release
  end;
end;

initialization
  JclStackTrackingOptions := JclStackTrackingOptions + [stExceptFrame];
  JclStartExceptionTracking;
  JclAddExceptNotifier(NotifyException);

finalization
  JclRemoveExceptNotifier(NotifyException);
  JclStopExceptionTracking;


end.
