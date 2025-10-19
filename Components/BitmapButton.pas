unit BitmapButton;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  A TImage-like component for bitmaps and png files without the TPicture bloat and
  which is actually a button with a focus rectangle when focused - in
  other words: an accessible TImage

  Also supports other TGraphic types which can be assigned to a TBitmap
  
  Make sure to set the Caption property, even if it isn't visible

  Also see TBitmapImage which is the TGraphicControl version
}

interface

uses
  Windows, Messages, ShellAPI, Controls, Graphics, Classes, Imaging.pngimage,
  BitmapImage;

type
  TBitmapButton = class(TCustomControl)
  private
    FFocusBorderWidthHeight: Integer;
    FImpl: TBitmapImageImplementation;
    procedure SetBackColor(Value: TColor);
    procedure SetBitmap(Value: TBitmap);
    procedure SetCenter(Value: Boolean);
    procedure SetGraphic(Value: TGraphic);
    procedure SetPngImage(Value: TPngImage);
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
    function InitializeFromStockIcon(const Siid: SHSTOCKICONID; const BkColor: TColor; const AscendingTrySizes: array of Integer): Boolean;
    property Bitmap: TBitmap read FImpl.Bitmap write SetBitmap;
    property Graphic: TGraphic write SetGraphic;
  published
    property Align;
    property Anchors;
    property AutoSize: Boolean read FImpl.AutoSize write SetAutoSize default False;
    property BackColor: TColor read FImpl.BackColor write SetBackColor default clNone;
    property Caption;
    property Center: Boolean read FImpl.Center write SetCenter default True;
    property Enabled;
    property ParentShowHint;
    property PngImage: TPngImage read FImpl.PngImage write SetPngImage;
    property PopupMenu;
    property ShowHint;
    property Stretch: Boolean read FImpl.Stretch write SetStretch default False;
    property ReplaceColor: TColor read FImpl.ReplaceColor write SetReplaceColor default clNone;
    property ReplaceWithColor: TColor read FImpl.ReplaceWithColor write SetReplaceWithColor default clNone;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnPaint: TPaintEvent read FImpl.OnPaint write FImpl.OnPaint;
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
  ControlStyle := ControlStyle + [csParentBackground, csReplicatable] - [csClickEvents];
  { Using a fixed focus border width/height to avoid design problems between systems }
  FFocusBorderWidthHeight := 2;
  const DoubleFBWH = 2*FFocusBorderWidthHeight;
  FImpl.Init(Self, DoubleFBWH, DoubleFBWH);
  Center := True;
  TabStop := True;
  Width := 75+DoubleFBWH;
  Height := 25+DoubleFBWH;
end;

procedure TBitmapButton.CreateParams(var Params: TCreateParams);
begin
  inherited;
  CreateSubClass(Params, 'BUTTON');
  Params.Style := Params.Style or BS_NOTIFY; { For BN_DBLCLK }
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

function TBitmapButton.InitializeFromStockIcon(const Siid: SHSTOCKICONID; const BkColor: TColor; const AscendingTrySizes: array of Integer): Boolean;
begin
  Result := FImpl.InitializeFromStockIcon(siid, BkColor, AscendingTrySizes);
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

procedure TBitmapButton.SetGraphic(Value: TGraphic);
begin
  FImpl.SetGraphic(Value);
end;

procedure TBitmapButton.SetPngImage(Value: TPngImage);
begin
  FImpl.SetPngImage(Value);
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

  if Focused and (SendMessage(Handle, WM_QUERYUISTATE, 0, 0) and UISF_HIDEFOCUS = 0) then begin
    { See TBitBtn.DrawItem in Vcl.Buttons.pas }
    Canvas.Pen.Color := clWindowFrame;
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := clBtnFace;
    { This might draw a focus border thinner or thicker than our FFocusBorderWidthHeight but that's okay }
    Canvas.DrawFocusRect(R);
  end;

  InflateRect(R, -FFocusBorderWidthHeight, -FFocusBorderWidthHeight);

  FImpl.Paint(Self, Canvas, R);
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
  if (Message.NotifyCode = BN_CLICKED) then
    Click
  else if (Message.NotifyCode = BN_DBLCLK) then
    DblClick;
end;

end.