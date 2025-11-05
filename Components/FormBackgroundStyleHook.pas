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
{$ENDIF}
    class var FBackColor: TColor;
    class var FGraphic: TGraphic;
    class var FGraphicTarget: TControl;
{$IFDEF VCLSTYLES}
  protected
    procedure PaintBackground(Canvas: TCanvas); override;
{$ENDIF}
  public
    class property BackColor: TColor write FBackColor;
    class property Graphic: TGraphic write FGraphic;
    class property GraphicTarget: TControl write FGraphicTarget;
  end;

implementation

{$IFDEF VCLSTYLES}

uses
  System.Classes;

{ TFormBackgroundStyleHook }

class constructor TFormBackgroundStyleHook.Create;
begin
  FBackColor := clNone;
end;

class destructor TFormBackgroundStyleHook.Destroy;
begin
  if FBitmapImageImplInitialized then
    FBitmapImageImpl.DeInit;
end;

procedure TFormBackgroundStyleHook.PaintBackground(Canvas: TCanvas);
begin
  var R := Rect(0, 0, Control.Width, Control.Height);

  if (FGraphic <> nil) and (FGraphicTarget = Control) then begin
    if not FBitmapImageImplInitialized then begin
      FBitmapImageImpl.Init(Control);
      FBitmapImageImpl.SetGraphic(FGraphic);
      FBitmapImageImplInitialized := True;
    end;
    FBitmapImageImpl.BackColor := FBackColor;
    FBitmapImageImpl.Paint(Self, Canvas, R);
  end else if FBackColor <> clNone then begin
    Canvas.Brush.Color := FBackColor;
    Canvas.FillRect(R);
  end;
end;

{$ENDIF}

end.
