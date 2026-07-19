unit Toolbar.Accessibility;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Accessibility support for TToolBar when ShowCaptions is False

  Requires buttons to have their Caption set. The accessibility is achieved
  by simply telling Windows about these Captions, which VCL does not do.
  Works for both regular buttons and split buttons.

  Activates only after receiving WM_GETOBJECT from a screen reader.

  Does not use MSAA or UIA functions. MSAA dynamic annotation was tested as
  well, but that does not work for split buttons.

  Does not change looks or layout of the buttons. This has been tested all
  the way back to Windows 7 with themes turned off. Narration has been tested
  on Windows 8 with Narrator (use Caps Lock+Arrow), and with NVDA and
  Narrator on Windows 11.

  Tip for Narrator on Windows 11: Turn on its mouse-tracking option.
  Tip for NVDA: Set sound volume to 0 and use Tools->Speech viewer.
}

interface

uses
  Messages, Classes, Controls, ComCtrls;

type
  TToolBarAccessibility = class(TComponent)
  private
    FToolBar: TToolBar;
    FSavedWindowProc: TWndMethod;
    FDestroyingHandle: Boolean;
    procedure WindowProc(var Message: TMessage);
  public
    constructor Create(const AToolBar: TToolBar); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  Windows, CommCtrl, SysUtils;

constructor TToolBarAccessibility.Create(const AToolBar: TToolBar);
begin
  inherited Create(AToolBar);
  FToolBar := AToolBar;
  FSavedWindowProc := FToolBar.WindowProc;
  FToolBar.WindowProc := WindowProc;
end;

destructor TToolBarAccessibility.Destroy;
begin
  FToolBar.WindowProc := FSavedWindowProc;
  inherited;
end;

procedure TToolBarAccessibility.WindowProc(var Message: TMessage);

  procedure SetButtonTexts;
  { Sets the native text of each button to its Caption }
  const
    BufferSize = 255;
  var
    Buffer: array[0..BufferSize-1] of Char;
  begin
    const Handle = FToolBar.Handle;

    { Make extra sure the captions are not displayed. They are already not
      displayed, but better safe than sorry }
    const ExStyle = SendMessage(Handle, TB_GETEXTENDEDSTYLE, 0, 0);
    if ExStyle and TBSTYLE_EX_MIXEDBUTTONS = 0 then
      SendMessage(Handle, TB_SETEXTENDEDSTYLE, 0, ExStyle or TBSTYLE_EX_MIXEDBUTTONS);

    for var I := 0 to FToolBar.ButtonCount - 1 do begin
      { The Buttons[] property is strange: it casts to TToolButton, but the result
        can still be some other control: InsertButton accepts any TControl and
        puts it into the same list as Buttons[] uses. To keep things clean we
        start by casting the 'button' back to TControl. }
      const Control: TControl = FToolBar.Buttons[I];
      if Control is TToolButton then begin
        const Button = TToolButton(Control);
        if not (Button.Style in [tbsSeparator, tbsDivider]) and (Button.Caption <> '') then begin
          var ButtonInfo: TTBButtonInfo;
          FillChar(ButtonInfo, SizeOf(ButtonInfo), 0);
          ButtonInfo.cbSize := SizeOf(ButtonInfo);
          ButtonInfo.dwMask := TBIF_TEXT or TBIF_BYINDEX;
          ButtonInfo.pszText := Buffer;
          ButtonInfo.cchText := BufferSize;
          if (SendMessage(Handle, TB_GETBUTTONINFO, I, LPARAM(@ButtonInfo)) >= 0) and
             (StrComp(Buffer, PChar(Button.Caption)) <> 0) then begin
            ButtonInfo.pszText := PChar(Button.Caption);
            SendMessage(Handle, TB_SETBUTTONINFO, I, LPARAM(@ButtonInfo));
          end;
        end;
      end;
    end;
  end;

begin
  case Message.Msg of
    WM_CREATE:
      FDestroyingHandle := False;
    WM_DESTROY:
      FDestroyingHandle := True;
    WM_GETOBJECT:
      if not FDestroyingHandle then
        SetButtonTexts;
  end;
  FSavedWindowProc(Message);
end;

end.
