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
  Vcl.Forms, Vcl.Controls, Vcl.Graphics {$IFDEF VCLSTYLES}, BitmapImage {$ENDIF};

type
  TFormBackgroundStyleHook = class(TFormStyleHook)
{$IFDEF VCLSTYLES}
  private
    class constructor Create;
    class destructor Destroy;
    class var FBitmapImageImpl: TBitmapImageImplementation;
    class var FBitmapImageImplInitialized: Boolean;
    class var FGraphic: TGraphic;
{$ENDIF}
    class var FBackColor: TColor;
    class var FCenter: Boolean;
    class var FGraphicTarget: TControl;
    class var FStretch: Boolean;
    class procedure SetGraphic(Value: TGraphic); static;
{$IFDEF VCLSTYLES}
  protected
    procedure PaintBackground(Canvas: TCanvas); override;
{$ENDIF}
  public
    class property BackColor: TColor write FBackColor;
    class property Center: Boolean write FCenter;
    class property Graphic: TGraphic write SetGraphic;
    class property GraphicTarget: TControl write FGraphicTarget;
    class property Stretch: Boolean write FStretch;
  end;

implementation

{$IFDEF VCLSTYLES}

uses
  System.Classes, System.SysUtils;

{ TFormBackgroundStyleHook }

class constructor TFormBackgroundStyleHook.Create;
begin
  FBackColor := clNone;
end;

class destructor TFormBackgroundStyleHook.Destroy;
begin
  if FBitmapImageImplInitialized then
    FBitmapImageImpl.DeInit;
  FGraphic.Free;
end;

procedure TFormBackgroundStyleHook.PaintBackground(Canvas: TCanvas);
begin
  var R := Rect(0, 0, Control.ClientWidth, Control.ClientHeight);

  if (FGraphicTarget = Control) and (FBitmapImageImplInitialized or (FGraphic <> nil)) then begin
    if not FBitmapImageImplInitialized then begin
      FBitmapImageImpl.Init(Control);
      FBitmapImageImpl.Center := FCenter;
      FBitmapImageImpl.SetGraphic(FGraphic);
      FBitmapImageImpl.Stretch := FStretch;
      FreeAndNil(FGraphic);
      FBitmapImageImplInitialized := True;
    end;
    FBitmapImageImpl.BackColor := FBackColor;
    FBitmapImageImpl.Paint(Control, Canvas, R);
  end else if FBackColor <> clNone then begin
    Canvas.Brush.Color := FBackColor;
    Canvas.FillRect(R);
  end;
end;

{$ENDIF}

class procedure TFormBackgroundStyleHook.SetGraphic(Value: TGraphic);
begin
{$IFDEF VCLSTYLES}
  if not FBitmapImageImplInitialized then begin
    if FGraphic <> nil then
      FreeAndNil(FGraphic);
    if Value <> nil then begin
      FGraphic := TGraphicClass(Value.ClassType).Create;
      FGraphic.Assign(Value);
    end;
  end;
{$ENDIF}
end;


end.
