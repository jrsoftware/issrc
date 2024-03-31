unit TaskbarProgressFunc;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Wrappers for ITaskbarList3.SetProgressState & SetProgressValue
}

interface

type
  TTaskbarProgressState = (tpsNoProgress, tpsIndeterminate, tpsNormal,
    tpsError, tpsPaused);

procedure SetAppTaskbarProgressState(const State: TTaskbarProgressState);
procedure SetAppTaskbarProgressValue(const Completed, Total: Cardinal);

implementation

uses
  Windows, ActiveX, Forms, dwTaskbarList;

var
  TaskbarListInitialized: Boolean;
  TaskbarListInterface: ITaskbarList3;

function InitializeTaskbarList: Boolean;
var
  Intf: ITaskbarList3;
begin
  if not TaskbarListInitialized then begin
    if CoCreateInstance(CLSID_TaskbarList, nil, CLSCTX_INPROC_SERVER, IID_TaskbarList3, Intf) = S_OK then
      if Intf.HrInit = S_OK then begin
        { Safety: don't allow the instance to be destroyed at shutdown }
        Intf._AddRef;
        TaskbarListInterface := Intf;
      end;
    TaskbarListInitialized := True;
  end;
  Result := Assigned(TaskbarListInterface);
end;

procedure SetAppTaskbarProgressState(const State: TTaskbarProgressState);
const
  StateFlags: array[TTaskbarProgressState] of Integer = (
    TBPF_NOPROGRESS, TBPF_INDETERMINATE, TBPF_NORMAL, TBPF_ERROR, TBPF_PAUSED);
begin
  if InitializeTaskbarList then
    TaskbarListInterface.SetProgressState(Application.Handle, StateFlags[State]);
end;

procedure SetAppTaskbarProgressValue(const Completed, Total: Cardinal);
var
  Completed64, Total64: dwInteger64;
begin
  if InitializeTaskbarList then begin
    Completed64.Lo := Completed;
    Completed64.Hi := 0;
    Total64.Lo := Total;
    Total64.Hi := 0;
    TaskbarListInterface.SetProgressValue(Application.Handle, Completed64, Total64);
  end;
end;

end.
