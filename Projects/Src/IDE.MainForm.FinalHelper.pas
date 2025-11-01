unit IDE.MainForm.FinalHelper;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Compiler form - final helper to be used by MainForm

  Includes some general functions
}

interface

uses
  SysUtils,
  IDE.MainForm, IDE.MainForm.ScintHelper;

type
  TMainFormFinalHelper = class helper(TMainFormScintHelper) for TMainForm
    class procedure AppOnException(Sender: TObject; E: Exception);
    function ToCurrentPPI(const XY: Integer): Integer;
    function FromCurrentPPI(const XY: Integer): Integer;
  end;

implementation

uses
  Windows,
  Shared.CommonFunc, Shared.CommonFunc.Vcl,
  IDE.Messages;

class procedure TMainFormFinalHelper.AppOnException(Sender: TObject; E: Exception);
begin
  MsgBox(AddPeriod(E.Message), SCompilerFormCaption, mbCriticalError, MB_OK);
end;

function TMainFormFinalHelper.ToCurrentPPI(const XY: Integer): Integer;
begin
  Result := MulDiv(XY, CurrentPPI, 96);
end;

function TMainFormFinalHelper.FromCurrentPPI(const XY: Integer): Integer;
begin
  Result := MulDiv(XY, 96, CurrentPPI);
end;

end.
