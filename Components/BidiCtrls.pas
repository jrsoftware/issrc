unit BidiCtrls;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Previously this unit had RTL-capable versions of standard controls
  
  But now standard controls are RTL-capable already, and there's not much code left here

  Define VCLSTYLES to include an improved version TButtonStyleHook.DrawButton for command
  link buttons
}

interface

uses
  Windows, SysUtils, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  {$IFDEF VCLSTYLES} Vcl.Themes, {$ELSE} Themes, {$ENDIF}
  StdCtrls, ExtCtrls;

type
  TNewEdit = class(TEdit);

  TNewMemo = class(TMemo);

  TNewComboBox = class(TComboBox);

  TNewListBox = class(TListBox);

  TNewButton = class(TButton)
  private
    class constructor Create;
    class destructor Destroy;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    function AdjustHeightIfCommandLink: Integer;
  end;

  TNewButtonStyleHook = class(TButtonStyleHook)
{$IFDEF VCLSTYLES}
  private
    class function DrawOrMeasureCommandLink(const Draw: Boolean;
      const Control: TNewButton; const LStyle: TCustomStyleServices; const FPressed: Boolean;
      const ACanvas: TCanvas; const AMouseInControl: Boolean): Integer;
    class function GetIdealHeightIfCommandLink(const Control: TNewButton; const Style: TCustomStyleServices): Integer;
  protected
    procedure DrawButton(ACanvas: TCanvas; AMouseInControl: Boolean); override;
{$ENDIF}
  end;

  TNewCheckBox = class(TCheckBox);

  TNewRadioButton = class(TRadioButton);

  TNewLinkLabel = class(TLinkLabel)
  public
    function AdjustHeight: Integer;
  end;

procedure Register;
  
implementation

uses
  CommCtrl, Types,
  BidiUtils;

procedure Register;
begin
  RegisterComponents('JR', [TNewEdit, TNewMemo, TNewComboBox, TNewListBox,
    TNewButton, TNewCheckBox, TNewRadioButton]);
end;

{ TNewButton }

class constructor TNewButton.Create;
begin
  TCustomStyleEngine.RegisterStyleHook(TNewButton, TNewButtonStyleHook);
end;

procedure TNewButton.CreateParams(var Params: TCreateParams);
begin
  inherited;
  if (Style = bsCommandLink) and IsRightToLeft then begin
    { Command link buttons need to have WS_EX_LAYOUTRTL enabled for full RTL, in addition to
      WS_EX_RTLREADING and WS_EX_LEFTSCROLLBAR, but not WS_EX_RIGHT. This can be confirmed by
      inspecting the style of a task dialog command link button. However, if VCL Styles is
      active, this should not be done since the style hook does not expect it at all. }
    if not IsCustomStyleActive then
      Params.ExStyle := Params.ExStyle or WS_EX_LAYOUTRTL;
  end;
end;

class destructor TNewButton.Destroy;
begin
  TCustomStyleEngine.UnregisterStyleHook(TNewButton, TNewButtonStyleHook);
end;

function TNewButton.AdjustHeightIfCommandLink: Integer;
begin
  Result := 0;
  if Style = bsCommandLink then begin
    var OldHeight := Height;

    {$IFDEF VCLSTYLES}
    var LStyle := StyleServices(Self);
    if not LStyle.Enabled or LStyle.IsSystemStyle then
      LStyle := nil;

    if LStyle <> nil then begin
      const IdealHeight = TNewButtonStyleHook.GetIdealHeightIfCommandLink(Self, LStyle);
      if IdealHeight <> 0 then begin
        Height := IdealHeight;
        Exit(Height- OldHeight);
      end;
    end;
    {$ENDIF}

    var IdealSize: TSize;
    IdealSize.cx := Width;
    IdealSize.cy := 0; { Not needed according to docs and tests, but clearing anyway }
    if SendMessage(Handle, BCM_GETIDEALSIZE, 0, LPARAM(@IdealSize)) <> 0 then begin
      Height := IdealSize.cy;
      Result := Height - OldHeight;
    end;
  end;
end;

{ TNewLinkLabel }

function TNewLinkLabel.AdjustHeight: Integer;
begin
  var OldHeight := Height;
  var IdealSize: TSize;
  SendMessage(Handle, LM_GETIDEALSIZE, Width, LPARAM(@IdealSize));
  Height := IdealSize.cy;
  Result := Height - OldHeight;
end;

{$IFDEF VCLSTYLES}

{ TNewButtonStyleHook - same as Vcl.StdCtrls' TButtonStyleHook except that for command links it:
  -Adds support for measuring height
  -Fixes RTL support for CommandLinkHint
  -Actually flips the text and icons on RTL
  -Improves alignment of shield icons, especially at high dpi
  -Avoids drawing empty notes
  -Respects the font of the control
  -Properly centers glyphs vertically on the first text line
  For other button styles it just calls the original code, and the code for those styles is not copied here }

class function TNewButtonStyleHook.DrawOrMeasureCommandLink(const Draw: Boolean;
  const Control: TNewButton; const LStyle: TCustomStyleServices; const FPressed: Boolean;
  const ACanvas: TCanvas; const AMouseInControl: Boolean): Integer;
var
  Details:  TThemedElementDetails;
  LParentRect, DrawRect, R, RSingleLine: TRect;
  LIsRightToLeft: Boolean;
  IL: BUTTON_IMAGELIST;
  IW, IH: Integer;
  TextFormat: TTextFormatFlags;
  ThemeTextColor: TColor;
  Buffer: string;
  BufferLength: Integer;
  ImgIndex: Integer;
  BCaption: String;
  IsDefault: Boolean;
  IsElevationRequired: Boolean;
  LPPI: Integer;
begin
  const Handle = Control.Handle;

  LPPI := Control.CurrentPPI;

  LParentRect := Control.ClientRect;
  LIsRightToLeft := Control.IsRightToLeft;

  BCaption := Control.Caption;
  ImgIndex := 0;
  IsDefault := Control.Active;
  IsElevationRequired := CheckWin32Version(6, 0) and Control.ElevationRequired;
  if not Control.Enabled then
  begin
    Details := LStyle.GetElementDetails(tbPushButtonDisabled);
    ImgIndex := 3;
  end
  else
  if FPressed then
  begin
    Details := LStyle.GetElementDetails(tbPushButtonPressed);
    ImgIndex := 2;
  end
  else if AMouseInControl then
  begin
    Details := LStyle.GetElementDetails(tbPushButtonHot);
    ImgIndex := 1;
  end
  else if Control.Focused or IsDefault then
  begin
    Details := LStyle.GetElementDetails(tbPushButtonDefaulted);
    ImgIndex := 4;
  end
  else if Control.Enabled then
    Details := LStyle.GetElementDetails(tbPushButtonNormal);

  DrawRect := LParentRect;
  if Draw then
    LStyle.DrawElement(ACanvas.Handle, Details, DrawRect);

  IW := MulDiv(35, LPPI, Screen.DefaultPixelsPerInch);
  Inc(DrawRect.Left, IW);
  Inc(DrawRect.Top, 15);
  Inc(DrawRect.Left, 5);
  ACanvas.Font := TNewButton(Control).Font;
  R := DrawRect;
  TextFormat := TTextFormatFlags(Control.DrawTextBiDiModeFlags(DT_LEFT or DT_WORDBREAK or DT_CALCRECT));
  LStyle.DrawText(ACanvas.Handle, Details, BCaption, R, TextFormat, ACanvas.Font.Color); { R is used directly below for measuring, and later also for the note }
  Result := R.Bottom;
  if Draw then begin
    RSingleLine := DrawRect;
    TextFormat := TTextFormatFlags(Control.DrawTextBiDiModeFlags(DT_LEFT or DT_SINGLELINE or DT_CALCRECT));
    LStyle.DrawText(ACanvas.Handle, Details, BCaption, RSingleLine, TextFormat, ACanvas.Font.Color); { RSingleLine is used below for the glyphs }
    { Following does not use any DT_CALCRECT results }
    TextFormat := TTextFormatFlags(Control.DrawTextBiDiModeFlags(DT_LEFT or DT_WORDBREAK));
    if (seFont in Control.StyleElements) and LStyle.GetElementColor(Details, ecTextColor, ThemeTextColor) then
      ACanvas.Font.Color := ThemeTextColor;
    var R2 := DrawRect;
    FlipRect(R2, LParentRect, LIsRightToLeft);
    LStyle.DrawText(ACanvas.Handle, Details, BCaption, R2, TextFormat, ACanvas.Font.Color);
  end;
  SetLength(Buffer, Button_GetNoteLength(Handle) + 1);
  if Length(Buffer) > 1 then
  begin
    BufferLength := Length(Buffer);
    if Button_GetNote(Handle, PChar(Buffer), BufferLength) then
    begin
      Inc(DrawRect.Top, R.Height + 2); { R is the DT_CALCRECT result } 
      ACanvas.Font.Height := MulDiv(ACanvas.Font.Height, 2, 3);
      R := DrawRect;
      TextFormat := TTextFormatFlags(Control.DrawTextBiDiModeFlags(DT_LEFT or DT_WORDBREAK or DT_CALCRECT));
      LStyle.DrawText(ACanvas.Handle, Details, Buffer, R, TextFormat, ACanvas.Font.Color);  { R is used directly below for measuring }
      if R.Bottom > Result then
        Result := R.Bottom;
      if Draw then begin
        { Following does not use any DT_CALCRECT results }
        TextFormat := TTextFormatFlags(Control.DrawTextBiDiModeFlags(DT_LEFT or DT_WORDBREAK));
        FlipRect(DrawRect, LParentRect, LIsRightToLeft);
        LStyle.DrawText(ACanvas.Handle, Details, Buffer, DrawRect, TextFormat, ACanvas.Font.Color);
      end;
    end;
  end;

  Inc(Result, 15);

  if not Draw then
    Exit;

  if Button_GetImageList(handle, IL) and (IL.himl <> 0) and
     ImageList_GetIconSize(IL.himl, IW, IH) then
  begin
    R.Left := 2;
    R.Top := RSingleLine.Top + (RSingleLine.Height - IH) div 2;
    if IsElevationRequired then
    begin
      ImgIndex := 0;
      Inc(R.Left, MulDiv(8, LPPI, Screen.DefaultPixelsPerInch));
    end;
    R.Right := R.Left + IW;
    R.Bottom := R.Top + IH;
    if Draw then begin
      FlipRect(R, LParentRect, LIsRightToLeft);
      ImageList_Draw(IL.himl, ImgIndex, ACanvas.Handle, R.Left, R.Top, ILD_NORMAL);
    end;
  end else begin
    if not Control.Enabled then
      Details := LStyle.GetElementDetails(tbCommandLinkGlyphDisabled)
    else
    if FPressed then
      Details := LStyle.GetElementDetails(tbCommandLinkGlyphPressed)
    else if Control.Focused then
      Details := LStyle.GetElementDetails(tbCommandLinkGlyphDefaulted)
    else if AMouseInControl then
      Details := LStyle.GetElementDetails(tbCommandLinkGlyphHot)
    else
      Details := LStyle.GetElementDetails(tbCommandLinkGlyphNormal);

    DrawRect.Right := IW;
    DrawRect.Left := 3;
    DrawRect.Top := RSingleLine.Top + (RSingleLine.Height - IW) div 2;
    DrawRect.Bottom := DrawRect.Top + IW;
    if Draw then begin
      if LIsRightToLeft then begin
        FlipRect(DrawRect, LParentRect, True);
        var FlipBitmap := TBitmap.Create;
        try
          FlipBitmap.Width := DrawRect.Width;
          FlipBitmap.Height := DrawRect.Height;
          BitBlt(FlipBitmap.Canvas.Handle, 0, 0, DrawRect.Width, DrawRect.Height, ACanvas.Handle, DrawRect.Left, DrawRect.Top, SRCCOPY);
          LStyle.DrawElement(FlipBitmap.Canvas.Handle, Details, Rect(0, 0, DrawRect.Width, DrawRect.Height), nil, LPPI);
          StretchBlt(ACanvas.Handle, DrawRect.Left, DrawRect.Top, DrawRect.Width, DrawRect.Height,
            FlipBitmap.Canvas.Handle, FlipBitmap.Width-1, 0, -FlipBitmap.Width, FlipBitmap.Height, SRCCOPY);
        finally
          FlipBitmap.Free;
        end;
      end else
        LStyle.DrawElement(ACanvas.Handle, Details, DrawRect, nil, LPPI);
    end;
  end;
end;

class function TNewButtonStyleHook.GetIdealHeightIfCommandLink(const Control: TNewButton; const Style: TCustomStyleServices): Integer;
begin
  const Canvas = TCanvas.Create;
  try
    Canvas.Handle := GetDC(0);
    try
      Result := DrawOrMeasureCommandLink(False, Control, Style, False, Canvas, False);
    finally
      ReleaseDC(0, Canvas.Handle);
    end;
  finally
    Canvas.Handle := 0;
    Canvas.Free;
  end;
end;

procedure TNewButtonStyleHook.DrawButton(ACanvas: TCanvas; AMouseInControl: Boolean);
begin
  const LControlStyle = GetWindowLong(Handle, GWL_STYLE);
  if (LControlStyle and BS_COMMANDLINK) <> BS_COMMANDLINK then begin
    inherited;
    Exit;
  end;

  DrawOrMeasureCommandLink(True, TNewButton(Control), StyleServices, FPressed, ACanvas, AMouseInControl);
end;

{$ENDIF}

end.
