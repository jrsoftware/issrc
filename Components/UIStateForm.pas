unit UIStateForm;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  TUIStateForm, a TForm descendant which correctly handles the hiding of
  accelerator key characters and focus rectangles when the
  "Hide keyboard navigation indicators" option is enabled.
}

interface

uses
  Windows, SysUtils, Messages, Classes, Graphics, Controls, Forms, Dialogs;

type
  TUIStateForm = class(TForm)
  private
    procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
  end;

implementation

const
  WM_CHANGEUISTATE = $0127;
  UIS_SET = 1;
  UIS_CLEAR = 2;
  UIS_INITIALIZE = 3;
  UISF_HIDEFOCUS = $1;
  UISF_HIDEACCEL = $2;

procedure TUIStateForm.CMShowingChanged(var Message: TMessage);
begin
  if Showing then
    SendMessage(Handle, WM_CHANGEUISTATE, UIS_INITIALIZE, 0);
  inherited;
end;

procedure TUIStateForm.CMDialogKey(var Message: TCMDialogKey);
begin
  case Message.CharCode of
    VK_LEFT..VK_DOWN, VK_TAB:
      SendMessage(Handle, WM_CHANGEUISTATE, UIS_CLEAR or (UISF_HIDEFOCUS shl 16), 0);
    VK_MENU:
      SendMessage(Handle, WM_CHANGEUISTATE, UIS_CLEAR or ((UISF_HIDEFOCUS or UISF_HIDEACCEL) shl 16), 0);
  end;
  inherited;
end;

end.
