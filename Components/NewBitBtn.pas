unit NewBitBtn;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  TNewBitBtn - a simple TBitBtn-like compontent which its a true
  button but only paints a bitmap and nothing else, except a focus
  rectangle when focused - in other words: an accessible TImage
}

interface

uses
  System.Classes, Winapi.Messages, Vcl.Controls;

type
  TNewBitBtn = class(TCustomControl)
  private
    FOnClick: TNotifyEvent;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property Anchors;
    property Caption;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

procedure Register;

implementation

uses
  Vcl.Graphics;

constructor TNewBitBtn.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csParentBackground];
  TabStop := True;
  Width := 100;
  Height := 24;
end;

procedure TNewBitBtn.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  CreateSubClass(Params, 'BUTTON');
end;

procedure TNewBitBtn.Paint;
begin
  if csDesigning in ComponentState then begin
    Canvas.Pen.Style := psDash;
    Canvas.Brush.Style := bsClear;
    Canvas.Rectangle(0, 0, Width, Height);
  end;

  var R := ClientRect;

  if Focused then begin
    { See TBitBtn.DrawItem in Vcl.Buttons.pas }
    Canvas.Pen.Color := clWindowFrame;
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := clBtnFace;
    Canvas.DrawFocusRect(R);
  end;

  {!!!}
end;

procedure TNewBitBtn.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  Invalidate;
end;

procedure TNewBitBtn.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  Invalidate;
end;

procedure TNewBitBtn.CNCommand(var Message: TWMCommand);
begin
  if (Message.NotifyCode = BN_CLICKED) and Assigned(FOnClick) then
    FOnClick(Self);
end;

procedure Register;
begin
  RegisterComponents('JR', [TNewBitBtn]);
end;

end.