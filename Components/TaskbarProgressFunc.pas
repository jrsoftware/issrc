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
  ActiveX, Forms, ShlObj;

var
  TaskbarListInitialized: Boolean;
  TaskbarListInterface: ITaskbarList3;

function InitializeTaskbarList: Boolean;
var
  Intf: ITaskbarList3;
begin
  if not TaskbarListInitialized then begin
    if CoCreateInstance(CLSID_TaskbarList, nil, CLSCTX_INPROC_SERVER, IID_ITaskbarList3, Intf) = S_OK then
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
  if InitializeTaskbarList and Assigned(Application.MainForm) and
     Application.MainForm.HandleAllocated then
    TaskbarListInterface.SetProgressState(Application.MainForm.Handle, StateFlags[State]);
end;

procedure SetAppTaskbarProgressValue(const Completed, Total: Cardinal);
begin
  if InitializeTaskbarList and Assigned(Application.MainForm) and
     Application.MainForm.HandleAllocated then
    TaskbarListInterface.SetProgressValue(Application.MainForm.Handle, Completed, Total);
end;

end.
