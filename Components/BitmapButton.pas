unit BitmapButton;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  A TImage-like component for bitmaps without the TPicture bloat and
  which is actually a button with a focus rectangle when focused - in
  other words: an accessible TImage
  
  Make sure you set the Caption property even if it isn't visible

  Also see TBitmapImage which is the TGraphicControl version
}

interface

uses
  Windows, Messages, Controls, Graphics, Classes,
  BitmapImage;

type
  TPaintEvent = procedure(Sender: TObject; Canvas: TCanvas; var ARect: TRect) of object;

  TBitmapButton = class(TCustomControl)
  private
    FImpl: TBitmapImageImplementation;
    FOnClick: TNotifyEvent;
    FOnDblClick: TNotifyEvent;
    FOnPaint: TPaintEvent;
    procedure SetBackColor(Value: TColor);
    procedure SetBitmap(Value: TBitmap);
    procedure SetCenter(Value: Boolean);
    procedure SetReplaceColor(Value: TColor);
    procedure SetReplaceWithColor(Value: TColor);
    procedure SetStretch(Value: Boolean);
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    function GetPalette: HPALETTE; override;
    procedure Paint; override;
    procedure SetAutoSize(Value: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function InitializeFromIcon(const Instance: HINST; const Name: PChar; const BkColor: TColor; const AscendingTrySizes: array of Integer): Boolean;
  published
    property Align;
    property Anchors;
    property AutoSize: Boolean read FImpl.AutoSize write SetAutoSize default False;
    property BackColor: TColor read FImpl.BackColor write SetBackColor default clNone;
    property Caption;
    property Center: Boolean read FImpl.Center write SetCenter default False;
    property Enabled;
    property ParentShowHint;
    property Bitmap: TBitmap read FImpl.Bitmap write SetBitmap;
    property PopupMenu;
    property ShowHint;
    property Stretch: Boolean read FImpl.Stretch write SetStretch default False;
    property ReplaceColor: TColor read FImpl.ReplaceColor write SetReplaceColor default clNone;
    property ReplaceWithColor: TColor read FImpl.ReplaceWithColor write SetReplaceWithColor default clNone;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnPaint: TPaintEvent read FOnPaint write FOnPaint;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('JR', [TBitmapButton]);
end;

constructor TBitmapButton.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csReplicatable];
  FImpl.Init(Self);
  TabStop := True;
  Height := 105;
  Width := 105;
end;

procedure TBitmapButton.CreateParams(var Params: TCreateParams);
begin
  inherited;
  CreateSubClass(Params, 'BUTTON');
end;

destructor TBitmapButton.Destroy;
begin
  FImpl.DeInit;
  inherited;
end;

function TBitmapButton.InitializeFromIcon(const Instance: HINST; const Name: PChar; const BkColor: TColor; const AscendingTrySizes: array of Integer): Boolean;
begin
  Result := FImpl.InitializeFromIcon(HInstance, Name, BkColor, AscendingTrySizes);
end;

procedure TBitmapButton.SetAutoSize(Value: Boolean);
begin
  FImpl.SetAutoSize(Self, Value);
end;

procedure TBitmapButton.SetBackColor(Value: TColor);
begin
  FImpl.SetBackColor(Self, Value);
end;

procedure TBitmapButton.SetBitmap(Value: TBitmap);
begin
  FImpl.SetBitmap(Value);
end;

procedure TBitmapButton.SetCenter(Value: Boolean);
begin
  FImpl.SetCenter(Self, Value);
end;

procedure TBitmapButton.SetReplaceColor(Value: TColor);
begin
  FImpl.SetReplaceColor(Self, Value);
end;

procedure TBitmapButton.SetReplaceWithColor(Value: TColor);
begin
  FImpl.SetReplaceWithColor(Self, Value);
end;

procedure TBitmapButton.SetStretch(Value: Boolean);
begin
  FImpl.SetStretch(Self, Value);
end;

function TBitmapButton.GetPalette: HPALETTE;
begin
  Result := FImpl.GetPalette;
end;

procedure TBitmapButton.Paint;
begin
  Canvas.Font := Font;
  Canvas.Brush.Color := Color;

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

  FImpl.Paint(Canvas, R);

  if Assigned(FOnPaint) then
    FOnPaint(Self, Canvas, R);
end;

procedure TBitmapButton.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  Invalidate;
end;

procedure TBitmapButton.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  Invalidate;
end;

procedure TBitmapButton.CNCommand(var Message: TWMCommand);
begin
  if (Message.NotifyCode = BN_CLICKED) and Assigned(FOnClick) then
    FOnClick(Self)
  else if (Message.NotifyCode = BN_DBLCLK) and Assigned(FOnDblClick) then
    FOnDblClick(Self);
end;

end.