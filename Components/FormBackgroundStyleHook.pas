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
  Vcl.Forms, Vcl.Controls, Vcl.Graphics {$IFDEF VCLSTYLES}, System.Classes, BitmapImage {$ENDIF};

type
{$IFDEF VCLSTYLES}
  TGraphicTargetNotifier = class(TComponent)
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  end;
{$ENDIF}

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
{$IFDEF VCLSTYLES}
    class var FGraphicTargetNotifier: TGraphicTargetNotifier;
{$ENDIF}
    class var FOpacity: Byte;
    class var FStretch: Boolean;
    class procedure SetGraphic(Value: TGraphic); static;
    class procedure SetGraphicTarget(Value: TControl); static;
{$IFDEF VCLSTYLES}
  protected
    procedure PaintBackground(Canvas: TCanvas); override;
{$ENDIF}
  public
    class property BackColor: TColor write FBackColor;
    class property Center: Boolean write FCenter;
    class property Graphic: TGraphic write SetGraphic;
    class property GraphicTarget: TControl write SetGraphicTarget;
    class property Opacity: Byte write FOpacity;
    class property Stretch: Boolean write FStretch;
  end;

implementation

{$IFDEF VCLSTYLES}

uses
  System.SysUtils;

{ TGraphicTargetNotifier }

procedure TGraphicTargetNotifier.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = TFormBackgroundStyleHook.FGraphicTarget) then
    TFormBackgroundStyleHook.FGraphicTarget := nil;
end;

{ TFormBackgroundStyleHook }

class constructor TFormBackgroundStyleHook.Create;
begin
  FBackColor := clNone;
  FGraphicTargetNotifier := TGraphicTargetNotifier.Create(nil);
end;

class destructor TFormBackgroundStyleHook.Destroy;
begin
  if FBitmapImageImplInitialized then
    FBitmapImageImpl.DeInit;
  FGraphic.Free;
  FGraphicTargetNotifier.Free;
end;

procedure TFormBackgroundStyleHook.PaintBackground(Canvas: TCanvas);
begin
  var R := Rect(0, 0, Control.ClientWidth, Control.ClientHeight);

  if (FGraphicTarget = Control) and (FBitmapImageImplInitialized or (FGraphic <> nil)) then begin
    if not FBitmapImageImplInitialized then begin
      FBitmapImageImpl.Init(Control);
      FBitmapImageImpl.SetGraphic(FGraphic);
      FreeAndNil(FGraphic);
      FBitmapImageImplInitialized := True;
    end;
    FBitmapImageImpl.BackColor := FBackColor;
    FBitmapImageImpl.Center := FCenter;
    FBitmapImageImpl.Opacity := FOpacity;
    FBitmapImageImpl.Stretch := FStretch;
    FBitmapImageImpl.Paint(Control, Canvas, R);
  end else if (FBackColor <> clNone) and (FBackColor <> clWindow) then begin
    Canvas.Brush.Color := TBitmapImageImplementation.AdjustColorForStyle(Control, FBackColor);
    Canvas.FillRect(R);
  end else
    inherited;
end;

{$ENDIF}

class procedure TFormBackgroundStyleHook.SetGraphic(Value: TGraphic);
begin
{$IFDEF VCLSTYLES}
  if FBitmapImageImplInitialized then begin
    FBitmapImageImpl.DeInit;
    FBitmapImageImplInitialized := False;
  end;
  if FGraphic <> nil then
    FreeAndNil(FGraphic);
  if Value <> nil then begin
    FGraphic := TGraphicClass(Value.ClassType).Create;
    FGraphic.Assign(Value);
  end;
{$ENDIF}
end;

class procedure TFormBackgroundStyleHook.SetGraphicTarget(Value: TControl);
begin
{$IFDEF VCLSTYLES}
  if FGraphicTarget <> nil then
    FGraphicTarget.RemoveFreeNotification(FGraphicTargetNotifier);
  FGraphicTarget := Value;
  if FGraphicTarget <> nil then
    FGraphicTarget.FreeNotification(FGraphicTargetNotifier);
{$ENDIF}
end;

end.
