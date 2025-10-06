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
  -Fixes RTL support for CommandLinkHint
  -Actually flips the text and icons on RTL
  -Improves alignment of shield icons, especially at high dpi
  -Avoids drawing empty notes
  For other button styles it just calls the original code, and the code for those styles is not copied here }

procedure TNewButtonStyleHook.DrawButton(ACanvas: TCanvas; AMouseInControl: Boolean);
var
  Details:  TThemedElementDetails;
  LParentRect, DrawRect, R: TRect;
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
  LStyle: TCustomStyleServices;
  LControlStyle: NativeInt;
begin
  LControlStyle := GetWindowLong(Handle, GWL_STYLE);
  if (LControlStyle and BS_COMMANDLINK) <> BS_COMMANDLINK then begin
    inherited;
    Exit;
  end;

  LPPI := Control.CurrentPPI;
  LStyle := StyleServices;

  LParentRect := Control.ClientRect;
  LIsRightToLeft := Control.IsRightToLeft;

  BCaption := Text;
  ImgIndex := 0;
  IsDefault := (Control is TNewButton) and TNewButton(Control).Active;
  IsElevationRequired := (Control is TCustomButton) and CheckWin32Version(6, 0) and
    TCustomButton(Control).ElevationRequired;
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
  LStyle.DrawElement(ACanvas.Handle, Details, DrawRect);

  if Button_GetImageList(handle, IL) and (IL.himl <> 0) and
     ImageList_GetIconSize(IL.himl, IW, IH) then
  begin
    R := DrawRect;
    Inc(R.Left, 2);
    Inc(R.Top, 15);
    if IsElevationRequired then
    begin
      ImgIndex := 0;
      Inc(R.Left, MulDiv(8, LPPI, Screen.DefaultPixelsPerInch));
    end;
    R.Right := R.Left + IW;
    R.Bottom := R.Top + IH;
    FlipRect(R, LParentRect, LIsRightToLeft);
    ImageList_Draw(IL.himl, ImgIndex, ACanvas.Handle, R.Left, R.Top, ILD_NORMAL);
  end;
  IW := MulDiv(35, LPPI, Screen.DefaultPixelsPerInch);
  Inc(DrawRect.Left, IW);
  Inc(DrawRect.Top, 15);
  Inc(DrawRect.Left, 5);
  ACanvas.Font := TNewButton(Control).Font;
  ACanvas.Font.Style := [];
  ACanvas.Font.Size := 12;
  R := DrawRect;
  TextFormat := TTextFormatFlags(Control.DrawTextBiDiModeFlags(DT_LEFT or DT_WORDBREAK or DT_CALCRECT));
  LStyle.DrawText(ACanvas.Handle, Details, BCaption, R, TextFormat, ACanvas.Font.Color);
  TextFormat := TTextFormatFlags(Control.DrawTextBiDiModeFlags(DT_LEFT or DT_WORDBREAK));
  if (seFont in Control.StyleElements) and LStyle.GetElementColor(Details, ecTextColor, ThemeTextColor) then
     ACanvas.Font.Color := ThemeTextColor;
  var R2 := DrawRect;
  FlipRect(R2, LParentRect, LIsRightToLeft);
  LStyle.DrawText(ACanvas.Handle, Details, BCaption, R2, TextFormat, ACanvas.Font.Color);
  SetLength(Buffer, Button_GetNoteLength(Handle) + 1);
  if Length(Buffer) <> 0 then
  begin
    BufferLength := Length(Buffer);
    if Button_GetNote(Handle, PChar(Buffer), BufferLength) and (Buffer <> '') then
    begin
      TextFormat := TTextFormatFlags(Control.DrawTextBiDiModeFlags(DT_LEFT or DT_WORDBREAK));
      Inc(DrawRect.Top, R.Height + 2); { R is the DT_CALCRECT result } 
      ACanvas.Font.Size := 8;
      FlipRect(DrawRect, LParentRect, LIsRightToLeft);
      LStyle.DrawText(ACanvas.Handle, Details, Buffer, DrawRect,
      TextFormat, ACanvas.Font.Color);
    end;
  end;
  if IL.himl = 0 then
  begin
    if not Control.Enabled then
      Details := LStyle.GetElementDetails(tbCommandLinkGlyphDisabled)
    else
    if FPressed then
      Details := LStyle.GetElementDetails(tbCommandLinkGlyphPressed)
    else if Focused then
      Details := LStyle.GetElementDetails(tbCommandLinkGlyphDefaulted)
    else if AMouseInControl then
      Details := LStyle.GetElementDetails(tbCommandLinkGlyphHot)
    else
      Details := LStyle.GetElementDetails(tbCommandLinkGlyphNormal);

    DrawRect.Right := IW;
    DrawRect.Left := 3;
    DrawRect.Top := 10;
    DrawRect.Bottom := DrawRect.Top + IW;
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

{$ENDIF}

end.
