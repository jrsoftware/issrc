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
  public
    function AdjustHeight: Integer;
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
  CommCtrl;

procedure Register;
begin
  RegisterComponents('JR', [TNewEdit, TNewMemo, TNewComboBox, TNewListBox,
    TNewButton, TNewCheckBox, TNewRadioButton]);
end;

{ TNewButton }

function TNewButton.AdjustHeight: Integer;
begin
  var OldHeight := Height;
  var IdealSize: TSize;
  IdealSize.cx := Width;
  SendMessage(Handle, BCM_GETIDEALSIZE, Width, LPARAM(@IdealSize));
  Height := IdealSize.cy;
  Result := Height - OldHeight;
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

end.
