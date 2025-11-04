unit FormBackgroundStyleHook;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Style hook for custom form backgrounds
}

interface

uses
  Vcl.Forms, Vcl.Controls, Vcl.Graphics,
  BitmapImage;

type
  TFormBackgroundStyleHook = class(TFormStyleHook)
  private
    class destructor Destroy;
    class var FBitmapImageImpl: TBitmapImageImplementation;
    class var FBackColor: TColor;
    class var FGraphic: TGraphic;
    class var FCurrentControl: TControl;
  protected
    procedure PaintBackground(Canvas: TCanvas); override;
  public
    class property BackColor: TColor write FBackColor;
    class property Graphic: TGraphic write FGraphic;
  end;

implementation

uses
  System.Classes;

{ TFormBackgroundStyleHook }

class destructor TFormBackgroundStyleHook.Destroy;
begin
  if FCurrentControl <> nil then
    FBitmapImageImpl.DeInit;
end;

procedure TFormBackgroundStyleHook.PaintBackground(Canvas: TCanvas);
begin
  if Control <> FCurrentControl then begin
    if FCurrentControl <> nil then
      FBitmapImageImpl.DeInit;
    FCurrentControl := Control;
    FBitmapImageImpl.Init(FCurrentControl);
    FBitmapImageImpl.BackColor := FBackColor;
    if FGraphic <> nil then
      FBitmapImageImpl.SetGraphic(FGraphic);
  end;

  var R := Rect(0, 0, Control.Width, Control.Height);
  FBitmapImageImpl.Paint(Self, Canvas, R);
end;

end.
