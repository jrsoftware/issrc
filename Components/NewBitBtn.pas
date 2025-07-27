unit NewBitBtn;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  TNewBitBtn - a simple TBitBtn-like compontent which is a true
  button but only paints a bitmap and nothing else, except a focus
  rectangle when focused - in other words: an accessible TImage
}

interface

uses
  System.Classes, System.SysUtils, System.Types, Winapi.Messages, Vcl.Controls, Vcl.Graphics;

type
  TPaintEvent = procedure(Sender: TObject; Canvas: TCanvas; var ARect: TRect) of object;

  TNewBitBtn = class(TCustomControl)
  private
    FOnClick: TNotifyEvent;
    FOnPaint: TPaintEvent;
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
    property OnPaint: TPaintEvent read FOnPaint write FOnPaint;
  end;

procedure Register;

implementation

uses
  Winapi.Windows;

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
  Canvas.Font := Font;
  Canvas.Brush.Color := Color;
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

  { Note: On Windows 11 the focus rectangle border is always 2 pixels wide / high, even at 200% DPI }
  var FocusBorderWidth: UINT := 2;
  var FocusBorderHeight: UINT := 2;
  SystemParametersInfo(SPI_GETFOCUSBORDERWIDTH, 0, @FocusBorderWidth, 0);
  SystemParametersInfo(SPI_GETFOCUSBORDERHEIGHT, 0, @FocusBorderHeight, 0);

  InflateRect(R, -FocusBorderWidth, -FocusBorderHeight);

  if Assigned(FOnPaint) then
    FOnPaint(Self, Canvas, R);

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