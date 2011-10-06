unit BidiCtrls;

{
  Inno Setup
  Copyright (C) 1997-2007 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  RTL-capable versions of standard controls

  $jrsoftware: issrc/Components/BidiCtrls.pas,v 1.2 2007/11/27 04:52:53 jr Exp $
}

interface

uses
  Windows, SysUtils, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TNewEdit = class(TEdit)
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  end;

  TNewMemo = class(TMemo)
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  end;

  TNewComboBox = class(TComboBox)
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  end;

  TNewListBox = class(TListBox)
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  end;

  TNewButton = class(TButton)
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  end;

  TNewCheckBox = class(TCheckBox)
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  end;

  TNewRadioButton = class(TRadioButton)
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  end;

procedure Register;
  
implementation

uses
  BidiUtils;

procedure Register;
begin
  RegisterComponents('JR', [TNewEdit, TNewMemo, TNewComboBox, TNewListBox,
    TNewButton, TNewCheckBox, TNewRadioButton]);
end;

{ TNewEdit }

procedure TNewEdit.CreateParams(var Params: TCreateParams);
begin
  inherited;
  SetBiDiStyles(Self, Params);
end;

{ TNewMemo }

procedure TNewMemo.CreateParams(var Params: TCreateParams);
begin
  inherited;
  SetBiDiStyles(Self, Params);
end;

{ TNewComboBox }

procedure TNewComboBox.CreateParams(var Params: TCreateParams);
begin
  inherited;
  SetBiDiStyles(Self, Params);
end;

{ TNewListBox }

procedure TNewListBox.CreateParams(var Params: TCreateParams);
begin
  inherited;
  SetBiDiStyles(Self, Params);
end;

{ TNewButton }

procedure TNewButton.CreateParams(var Params: TCreateParams); 
begin
  inherited;
  SetBiDiStyles(Self, Params);
  Params.ExStyle := Params.ExStyle and not WS_EX_RIGHT;
end;

{ TNewCheckBox }

procedure TNewCheckBox.CreateParams(var Params: TCreateParams);
begin
  inherited;
  SetBiDiStyles(Self, Params);
end;

{ TNewRadioButton }

procedure TNewRadioButton.CreateParams(var Params: TCreateParams);
begin
  inherited;
  SetBiDiStyles(Self, Params);
end;

end.
