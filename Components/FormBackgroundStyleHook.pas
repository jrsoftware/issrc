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
  Vcl.Forms, Vcl.Graphics;

type
  TFormBackgroundStyleHook = class(TFormStyleHook)
    procedure PaintBackground(Canvas: TCanvas); override;
  end;

implementation

uses
  System.Classes;

{ TFormBackgroundStyleHook }

procedure TFormBackgroundStyleHook.PaintBackground(Canvas: TCanvas);
begin
  Canvas.Brush.Color := clRed;
  Canvas.FillRect(Rect(0, 0, Control.Width, Control.Height));
end;

end.
