unit BitmapButton;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  A TImage-like component for bitmaps and png files without the TPicture bloat and
  which is actually a button with a focus rectangle when focused - in
  other words: an accessible TImage

  Descends from TCustomButton so it clicks on both Space and Enter and takes
  part in the VCL's default button handling

  Also supports other TGraphic types which can be assigned to a TBitmap

  Make sure to set the Caption property, even if it isn't visible

  Also see TBitmapImage which is the TGraphicControl version
}

interface

uses
  Windows, Messages, ShellAPI,
  Controls, StdCtrls, Graphics, Classes, Imaging.pngimage,
  BitmapImage;

type
  TBitmapButton = class(TCustomButton)
  private
    FCanvas: TCanvas;
    FFocusBorderWidthHeight: Integer;
    FImpl: TBitmapImageImplementation;
    procedure SetBackColor(Value: TColor);
    procedure SetBitmap(Value: TBitmap);
    procedure SetCenter(Value: Boolean);
    procedure SetGraphic(Value: TGraphic);
    procedure SetOpacity(Value: Byte);
    procedure SetPngImage(Value: TPngImage);
    procedure SetReplaceColor(Value: TColor);
    procedure SetReplaceWithColor(Value: TColor);
    procedure SetStretch(Value: Boolean);
    procedure DrawItem(const DrawItemStruct: TDrawItemStruct);
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    function GetPalette: HPALETTE; override;
    procedure SetAutoSize(Value: Boolean); override;
    procedure SetButtonStyle(ADefault: Boolean); override;
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
    property Cancel;
    property Caption;
    property Center: Boolean read FImpl.Center write SetCenter default True;
    property Default;
    property Enabled;
    property ModalResult;
    property Opacity: Byte read FImpl.Opacity write SetOpacity default 255;
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

uses
  Themes;

procedure Register;
begin
  RegisterComponents('JR', [TBitmapButton]);
end;

constructor TBitmapButton.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csParentBackground, csReplicatable];
  FCanvas := TCanvas.Create; { Same as TBitBtn }
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
  Params.Style := Params.Style or BS_OWNERDRAW; { BS_OWNERDRAW also enables automatic BN_DBLCLK notification without BS_NOTIFY }
end;

destructor TBitmapButton.Destroy;
begin
  FImpl.DeInit;
  FCanvas.Free;
  inherited;
end;

function TBitmapButton.InitializeFromIcon(const Instance: HINST; const Name: PChar; const BkColor: TColor; const AscendingTrySizes: array of Integer): Boolean;
begin
  Result := FImpl.InitializeFromIcon(Instance, Name, BkColor, AscendingTrySizes);
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

procedure TBitmapButton.SetButtonStyle(ADefault: Boolean);
begin
  { Just here to prevent TCustomButton.SetButtonStyle from running }
end;

procedure TBitmapButton.SetCenter(Value: Boolean);
begin
  FImpl.SetCenter(Self, Value);
end;

procedure TBitmapButton.SetGraphic(Value: TGraphic);
begin
  FImpl.SetGraphic(Value);
end;

procedure TBitmapButton.SetOpacity(Value: Byte);
begin
  FImpl.SetOpacity(Self, Value);
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

procedure TBitmapButton.DrawItem(const DrawItemStruct: TDrawItemStruct);
begin
  { Only handle full redraws: partial ODA_FOCUS and ODA_SELECT draws are not
    preceded by an erase, and drawing an alpha image twice gives different
    pixels. Focus changes already cause full redraws via WMSetFocus and
    WMKillFocus, and there is no pressed look. }
  if DrawItemStruct.itemAction and ODA_DRAWENTIRE = 0 then
    Exit;

  FCanvas.Handle := DrawItemStruct.hDC;
  try
    { Erase using the parent's background: TButtonControl.WMEraseBkGnd disables
      normal erasing when themed. }
    PerformEraseBackground(Self, DrawItemStruct.hDC);

    FCanvas.Font := Font;
    FCanvas.Brush.Color := Color;

    var R := ClientRect;

    if Focused and (SendMessage(Handle, WM_QUERYUISTATE, 0, 0) and UISF_HIDEFOCUS = 0) then begin
      { See TBitBtn.DrawItem in Vcl.Buttons.pas }
      FCanvas.Pen.Color := clWindowFrame;
      FCanvas.Brush.Style := bsSolid;
      FCanvas.Brush.Color := clBtnFace;
      { This might draw a focus border thinner or thicker than our FFocusBorderWidthHeight but that's okay }
      FCanvas.DrawFocusRect(R);
    end;

    InflateRect(R, -FFocusBorderWidthHeight, -FFocusBorderWidthHeight);

    FImpl.Paint(Self, FCanvas, R);
  finally
    FCanvas.Handle := 0;
  end;
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

procedure TBitmapButton.CMTextChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TBitmapButton.CNCommand(var Message: TWMCommand);
begin
  { BN_CLICKED is handled by TCustomButton.CNCommand }
  if Message.NotifyCode = BN_DBLCLK then
    DblClick
  else
    inherited;
end;

procedure TBitmapButton.CNDrawItem(var Message: TWMDrawItem);
begin
  DrawItem(Message.DrawItemStruct^);
  Message.Result := 1;
end;

initialization
  { Ensure VCL Styles leaves our painting alone }
  TCustomStyleEngine.RegisterStyleHook(TBitmapButton, TStyleHook);
finalization
  TCustomStyleEngine.UnRegisterStyleHook(TBitmapButton, TStyleHook);
end.
