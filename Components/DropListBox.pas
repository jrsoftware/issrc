unit DropListBox;

{
  Inno Setup
  Copyright (C) 1997-2004 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  This unit provides a listbox with drop files support.

  $jrsoftware: issrc/Components/DropListBox.pas,v 1.1 2004/06/05 16:07:10 mlaan Exp $
}

interface

uses
  StdCtrls,
  Messages;

type
  TDropListBox = class;

  TDropFileEvent = procedure(Sender: TDropListBox; const FileName: String) of object;

  TDropListBox = class(TCustomListBox)
  private
    FOnDropFile: TDropFileEvent;
  protected
    procedure CreateWnd; override;
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
  published
    property Align;
    property BorderStyle;
    property Color;
    property Columns;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property ExtendedSelect;
    property Font;
    property ImeMode;
    property ImeName;
    property IntegralHeight;
    property ItemHeight;
    property Items;
    property MultiSelect;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property Style;
    property TabOrder;
    property TabStop;
    property TabWidth;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropFile: TDropFileEvent read FOnDropFile write FOnDropFile;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

procedure Register;

implementation

uses
  Classes,
  Windows, ShellAPI;

procedure TDropListBox.CreateWnd;
begin
  inherited;
  if csDesigning in ComponentState then
    Exit;

  DragAcceptFiles(Handle, True);
end;

procedure TDropListBox.WMDropFiles(var Msg: TWMDropFiles);
var
  FileName: array[0..MAX_PATH] of Char;
  I, FileCount: Integer;
begin
  try
    if Assigned(FOnDropFile) then begin
      FileCount := DragQueryFile(Msg.Drop, $FFFFFFFF, nil, 0);
      for I := 0 to FileCount-1 do
        if DragQueryFile(Msg.Drop, I, FileName, SizeOf(FileName)) > 0 then
          FOnDropFile(Self, FileName);
    end;
    Msg.Result := 0;
  finally
    DragFinish(Msg.Drop);
  end;
end;

procedure Register;
begin
  RegisterComponents('JR', [TDropListBox]);
end;

end.