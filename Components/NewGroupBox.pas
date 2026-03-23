unit NewGroupBox;

{
  TNewGroupBox - fixes TGroupBox bug when ShowFrame is False

  In Delphi 13.0 and earlier, TCustomGroupBox.Paint has a bug where Details
  is not initialized when ShowFrame is False, but is still used to draw the
  Text. Embarcadero fixed this in Delphi 13.1 but since we can't detect 13.0
  vs 13.1, the fix must be active for 13.1 also.

  The VCL Styles hook did not have this bug, so no VCL Styles code needed.

  Note: ShowFrame did not exist before Delphi 12.

  Another note: This unit actually fixes a second bug which Embarcadero did
  not fix, even though it's pretty much the same issue: R.Right is used
  without being initialized when ShowFrame is False and RTL is active. This
  bug is also not present in the VCL Styles hook.
}

interface

{$IF CompilerVersion >= 36.0}
  {$DEFINE FIX}
{$ENDIF}

uses
  Vcl.StdCtrls;

type
{$IFDEF FIX}
  TNewGroupBox = class(TCustomGroupBox)
  protected
    procedure Paint; override;
  published
    property Align;
    property Anchors;
    property BiDiMode;
    property Caption;
    property Color;
    property Constraints;
    property Ctl3D;
    property DefaultHeaderFont;
    property DockSite;
    property DoubleBuffered;
    property DoubleBufferedMode;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HeaderFont;
    property Padding;
    property ParentBackground default True;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property ShowFrame;
    property TabOrder;
    property TabStop;
    property Touch;
    property Visible;
    property StyleElements;
    property StyleName;
    property OnAlignInsertBefore;
    property OnAlignPosition;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDockDrop;
    property OnDockOver;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGesture;
    property OnGetSiteInfo;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;
{$ELSE}
  TNewGroupBox = class(TGroupBox);
{$ENDIF}

implementation

uses
  {$IFDEF FIX}
  Winapi.Windows, System.Classes, System.Types,
  {$IFDEF VCLSTYLES} Vcl.Themes, {$ELSE} Themes, {$ENDIF}
  Vcl.Graphics, Vcl.Menus;
  {$ELSE}
  System.Classes;
  {$ENDIF}

procedure Register;
begin
  RegisterComponents('JR', [TNewGroupBox]);
end;

{ TNewGroupBox }

{$IFDEF FIX}

{ The following is based on Delphi 13.1's TCustomGroupBox.Paint, which fixed
  Details not being initialized when ShowFrame is False.

  Changes done:
  -FShowFrame -> ShowFrame
  -Turning off warnings for the implicit integer casts the original code has.
  -Fixed R.Right not being initialized when ShowFrame is False. }

{$WARN IMPLICIT_INTEGER_CAST_LOSS OFF}

{$WARN SYMBOL_DEPRECATED OFF}
procedure TNewGroupBox.Paint;
var
  H: Integer;
  R: TRect;
  Flags: Longint;
  CaptionRect,
  OuterRect: TRect;
  Size: TSize;
  Box: TThemedButton;
  Details: TThemedElementDetails;
  LStyle: TCustomStyleServices;
  S: string;
  TextFormat: TTextFormatFlags;
begin
  with Canvas do
  begin
    Font := HeaderFont;

    if ThemeControl(Self) then
    begin
      LStyle := StyleServices(Self);
      if Text <> '' then
      begin
        S := StripHotkey(Text);
        GetTextExtentPoint32(Handle, S, Length(S), Size);
        CaptionRect := Rect(0, 0, Size.cx, Size.cy);
        if not UseRightToLeftAlignment then
          OffsetRect(CaptionRect, 8, 0)
        else
          OffsetRect(CaptionRect, Width - 8 - CaptionRect.Right, 0);
      end
      else
        CaptionRect := Rect(0, 0, 0, 0);

      OuterRect := ClientRect;
      OuterRect.Top := (CaptionRect.Bottom - CaptionRect.Top) div 2;
      with CaptionRect do
        ExcludeClipRect(Handle, Left, Top, Right, Bottom);
      if Enabled then
        Box := tbGroupBoxNormal
      else
        Box := tbGroupBoxDisabled;
      Details := LStyle.GetElementDetails(Box);
      if ShowFrame then
        LStyle.DrawElement(Handle, Details, OuterRect);
      SelectClipRgn(Handle, 0);
      Brush.Style := bsClear;
      if Text <> '' then
        if IsRightToLeft then
        begin
          TextFormat := TTextFormatFlags(DrawTextBiDiModeFlags(DT_SINGLELINE));
          LStyle.DrawText(Handle, Details, Text, CaptionRect, TextFormat, Font.Color);
        end
        else
          LStyle.DrawText(Handle, Details, Text, CaptionRect, [tfLeft], Font.Color);
    end
    else
    begin
      H := TextHeight('0');
      R := Rect(0, H div 2 - 1, Width, Height);
      if ShowFrame then
      begin
        if Ctl3D then
        begin
          Inc(R.Left);
          Inc(R.Top);
          Brush.Color := clBtnHighlight;
          FrameRect(R);
          OffsetRect(R, -1, -1);
          Brush.Color := clBtnShadow;
        end else
          Brush.Color := clWindowFrame;
        FrameRect(R);
      end;
      if Text <> '' then
      begin
        if not UseRightToLeftAlignment then
          R := Rect(8, 0, 0, H)
        else
          R := Rect(R.Right - Canvas.TextWidth(Text) - 8, 0, 0, H);
        Flags := DrawTextBiDiModeFlags(DT_SINGLELINE);
        DrawText(Handle, Text, Length(Text), R, Flags or DT_CALCRECT);
        Brush.Color := Color;
        DrawText(Handle, Text, Length(Text), R, Flags);
      end;
    end;
  end;
end;
{$WARN SYMBOL_DEPRECATED ON}

{$ENDIF}

end.
