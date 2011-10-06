unit PasswordEdit;

{
  Inno Setup
  Copyright (C) 1997-2007 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  This unit provides a true password edit for Delphi 2.

  $jrsoftware: issrc/Components/PasswordEdit.pas,v 1.3 2007/12/10 18:28:53 jr Exp $
}

interface

uses
  Windows, Classes, Controls, StdCtrls;

type
  TPasswordEdit = class(TCustomEdit)
  private
    FPassword: Boolean;
    procedure SetPassword(Value: Boolean);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property CharCase;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property MaxLength;
    property OEMConvert;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property Password: Boolean read FPassword write SetPassword default True;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

procedure Register;

implementation

uses
  BidiUtils;

procedure Register;
begin
  RegisterComponents('JR', [TPasswordEdit]);
end;

{ TPasswordEdit }

constructor TPasswordEdit.Create(AOwner: TComponent);
begin
  inherited;
  FPassword := True;
end;

procedure TPasswordEdit.CreateParams(var Params: TCreateParams);
begin
  inherited;
  if FPassword then
    Params.Style := Params.Style or ES_PASSWORD;
  SetBiDiStyles(Self, Params);
end;

procedure TPasswordEdit.SetPassword(Value: Boolean);
begin
  if FPassword <> Value then begin
    FPassword := Value;
    RecreateWnd;
  end;
end;

end.
