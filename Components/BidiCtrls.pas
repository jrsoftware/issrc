unit BidiCtrls;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Previously this unit had RTL-capable versions of standard controls
  
  But now standard controls are RTL-capable already, and there's not much code left here
}

interface

uses
  Windows, SysUtils, Messages, Classes, Graphics, Controls, Forms, Dialogs,
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
  protected
    procedure DrawButton(ACanvas: TCanvas; AMouseInControl: Boolean); override;
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
  CommCtrl, Themes;

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
    var LStyle := StyleServices(Self);
    if not LStyle.Enabled or LStyle.IsSystemStyle then
      LStyle := nil;
    if LStyle = nil then
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

{ TNewButtonStyleHook }

procedure TNewButtonStyleHook.DrawButton(ACanvas: TCanvas; AMouseInControl: Boolean);
var
  Details:  TThemedElementDetails;
  DrawRect, R: TRect;
  IL: BUTTON_IMAGELIST;
  IW, IH, IX, IY: Integer;
  TextFormat: TTextFormatFlags;
  ThemeTextColor: TColor;
  Buffer: string;
  BufferLength: Integer;
  SaveIndex: Integer;
  X, Y, I, ImgIndex: Integer;
  BCaption: String;
  IsDefault: Boolean;
  IsElevationRequired: Boolean;
  LPPI: Integer;
  LDropDownWidth: Integer;
  LStyle: TCustomStyleServices;
  LSplitButton: Boolean;
  LCommandLink: Boolean;
  LTextFlags: Cardinal;
  LControlStyle: NativeInt;
  LTextRect: TRect;
  LImageDefined: Boolean;

function GetTextWidth: Integer;
var
  R: TRect;
begin
  R := Rect(0, 0, Control.Width, Control.Height);
  ACanvas.Font := TCustomButton(Control).Font;
  DrawControlText(ACanvas, Details, BCaption, R, LTextFlags or DT_CALCRECT);
  Result := R.Width;
end;

begin
  LPPI := Control.CurrentPPI;
  LStyle := StyleServices;
  LControlStyle := GetWindowLong(Handle, GWL_STYLE);
  LSplitButton := (LControlStyle and BS_SPLITBUTTON) = BS_SPLITBUTTON;
  LCommandLink := (LControlStyle and BS_COMMANDLINK) = BS_COMMANDLINK;

  LTextFlags := 0;
  if (LControlStyle and BS_MULTILINE) = BS_MULTILINE then
    LTextFlags := LTextFlags or DT_WORDBREAK;
  if (LControlStyle and BS_CENTER) = BS_LEFT then
    LTextFlags := LTextFlags or DT_LEFT
  else if (LControlStyle and BS_CENTER) = BS_RIGHT then
    LTextFlags := LTextFlags or DT_RIGHT
  else
    LTextFlags := LTextFlags or DT_CENTER;
  if (LControlStyle and BS_VCENTER) = BS_TOP then
    LTextFlags := LTextFlags or DT_TOP
  else if (LControlStyle and BS_VCENTER) = BS_BOTTOM then
    LTextFlags := LTextFlags or DT_BOTTOM
  else
    LTextFlags := LTextFlags or DT_VCENTER;

  if LSplitButton then
  begin
    LDropDownWidth := GetDropDownWidth;
    LSplitButton := LDropDownWidth > 0;
  end
  else
    LDropDownWidth := 0;

  BCaption := Text;
  ImgIndex := 0;
  IsDefault := (Control is TCustomButton) and TCustomButton(Control).FActive;
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

  DrawRect := Control.ClientRect;
  LStyle.DrawElement(ACanvas.Handle, Details, DrawRect);

  LTextRect := TRect.Empty;
  LImageDefined := False;

  if Button_GetImageList(handle, IL) and (IL.himl <> 0) and
     ImageList_GetIconSize(IL.himl, IW, IH) then
  begin
    R := DrawRect;
    if LSplitButton then
      Dec(R.Right, LDropDownWidth);
    IX := R.Left + 2;
    IY := R.Top + (R.Height - IH) div 2;
    if IsElevationRequired then
    begin
      ImgIndex := 0;
      if LCommandLink then
      begin
        IY := R.Top + 15;
        IX := R.Left + MulDiv(15, LPPI, Screen.DefaultPixelsPerInch);
      end
      else
      begin
        Inc(R.Left, IW);
        IX := R.CenterPoint.X - GetTextWidth div 2 - IW - 2;
        if LSplitButton then
          Dec(IX, MulDiv(7, LPPI, Screen.DefaultPixelsPerInch));
      end;
    end
    else
    if LCommandLink then
      IY := R.Top + 15
    else
    if Control is TCustomButton then
      with TCustomButton(Control) do
       case ImageAlignment of
          iaCenter:
            begin
              IX := R.CenterPoint.X - IW div 2;
            end;
          iaLeft:
            begin
              IX := R.Left + 2;
              Inc(IX, ImageMargins.Left);
              Inc(IY, ImageMargins.Top);
              Dec(IY, ImageMargins.Bottom);
              Inc(R.Left, IX + IW + ImageMargins.Right);
            end;
          iaRight:
            begin
              IX := R.Right - IW - 2;
              Dec(IX, ImageMargins.Right);
              Dec(IX, ImageMargins.Left);
              Inc(IY, ImageMargins.Top);
              Dec(IY, ImageMargins.Bottom);
              R.Right := IX;
            end;
          iaTop:
            begin
              IX := R.Left + (R.Width - IW) div 2;
              Inc(IX, ImageMargins.Left);
              Dec(IX, ImageMargins.Right);
              IY := R.Top + 2;
              Inc(IY, ImageMargins.Top);
              Inc(R.Top, IY + IH + ImageMargins.Bottom);
            end;
          iaBottom:
            begin
              IX := R.Left + (R.Width - IW) div 2;
              Inc(IX, ImageMargins.Left);
              Dec(IX, ImageMargins.Right);
              IY := R.Bottom - IH - 2;
              Dec(IY, ImageMargins.Bottom);
              Dec(IY, ImageMargins.Top);
              R.Bottom := IY;
            end;
        end;
    ImageList_Draw(IL.himl, ImgIndex, ACanvas.Handle, IX, IY, ILD_NORMAL);
    LImageDefined := True;
    LTextRect := R;
  end;
  if LCommandLink then
  begin
    IW := MulDiv(35, LPPI, Screen.DefaultPixelsPerInch);
    Inc(DrawRect.Left, IW);
    Inc(DrawRect.Top, 15);
    Inc(DrawRect.Left, 5);
    ACanvas.Font := TCustomButton(Control).Font;
    ACanvas.Font.Style := [];
    ACanvas.Font.Size := 12;
    R := DrawRect;
    TextFormat := TTextFormatFlags(Control.DrawTextBiDiModeFlags(DT_LEFT or DT_WORDBREAK or DT_CALCRECT));
    LStyle.DrawText(ACanvas.Handle, Details, BCaption, R, TextFormat, ACanvas.Font.Color);
    TextFormat := TTextFormatFlags(Control.DrawTextBiDiModeFlags(DT_LEFT or DT_WORDBREAK));
    if (seFont in Control.StyleElements) and LStyle.GetElementColor(Details, ecTextColor, ThemeTextColor) then
       ACanvas.Font.Color := ThemeTextColor;
    LStyle.DrawText(ACanvas.Handle, Details, BCaption, DrawRect, TextFormat, ACanvas.Font.Color);
    SetLength(Buffer, Button_GetNoteLength(Handle) + 1);
    if Length(Buffer) <> 0 then
    begin
      BufferLength := Length(Buffer);
      if Button_GetNote(Handle, PChar(Buffer), BufferLength) then
      begin
        TextFormat := TTextFormatFlags(DT_LEFT or DT_WORDBREAK);
        Inc(DrawRect.Top, R.Height + 2);
        ACanvas.Font.Size := 8;
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
      LStyle.DrawElement(ACanvas.Handle, Details, DrawRect, nil, LPPI);
     end;
  end
  else
  begin
    InflateRect(DrawRect, -2, -2);
    if LSplitButton then
    begin
      Dec(DrawRect.Right, LDropDownWidth);
      if LTextRect.IsEmpty and not LImageDefined then
        LTextRect := DrawRect;
      if not LTextRect.IsEmpty then
        DrawControlText(ACanvas, Details, Text, LTextRect, Control.DrawTextBiDiModeFlags(LTextFlags));
      if FDropDown then
      begin
        Details := LStyle.GetElementDetails(tbPushButtonPressed);
        SaveIndex := SaveDC(ACanvas.Handle);
        try
          IntersectClipRect(ACanvas.Handle, Control.Width - LDropDownWidth, 0,
            Control.Width, Control.Height);
          DrawRect := Rect(Control.Width - LDropDownWidth - 10, 0, Control.Width, Control.Height);
          LStyle.DrawElement(ACanvas.Handle, Details, DrawRect);
        finally
          RestoreDC(ACanvas.Handle, SaveIndex);
        end;
      end;
      with ACanvas do
      begin
        // draw split line
        Pen.Color := LStyle.GetSystemColor(clBtnHighLight);
        MoveTo(Control.Width - LDropDownWidth, 3);
        LineTo(Control.Width - LDropDownWidth, Control.Height - 3);
        if Control.Enabled then
          Pen.Color := LStyle.GetSystemColor(clBtnShadow)
        else
          Pen.Color := Font.Color;
        MoveTo(Control.Width - LDropDownWidth - 1, 3);
        LineTo(Control.Width - LDropDownWidth - 1, Control.Height - 3);
        // draw arrow
        Pen.Color := Font.Color;
        X := Control.Width - LDropDownWidth div 2 - 1;
        IX := MulDiv(3, LDropDownWidth, 15);
        if IX < 3 then
          IX := 3;
        Y := Control.Height div 2 + IX div 2;
        for i := IX downto 0 do
        begin
          MoveTo(X - I, Y - I);
          LineTo(X + I + 1, Y - I);
        end;
      end;
    end
    else
    begin
      if LTextRect.IsEmpty and not LImageDefined then
 	      LTextRect := DrawRect;
      if not LTextRect.IsEmpty then
        DrawControlText(ACanvas, Details, BCaption, LTextRect,
          Control.DrawTextBiDiModeFlags(LTextFlags));
    end;
  end;
end;

end.
