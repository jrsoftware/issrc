unit CompSignTools;

{
  Inno Setup
  Copyright (C) 1997-2010 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Compiler SignTools form

  $jrsoftware: issrc/Projects/CompSignTools.pas,v 1.3 2010/03/24 18:34:14 mlaan Exp $
}

interface

uses
  Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, UIStateForm;

type
  TSignToolsForm = class(TUIStateForm)
    OKButton: TButton;
    CancelButton: TButton;
    GroupBox1: TGroupBox;
    SignToolsListBox: TListBox;
    AddButton: TButton;
    RemoveButton: TButton;
    EditButton: TButton;
    procedure SignToolsListBoxClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure AddButtonClick(Sender: TObject);
    procedure RemoveButtonClick(Sender: TObject);
    procedure EditButtonClick(Sender: TObject);
  private
    FSignTools: TStringList;
    procedure UpdateSignTools;
    procedure UpdateSignToolsButtons;
    procedure SetSignTools(SignTools: TStringList);
    function InputSignTool(var SignToolName, SignToolCommand: String;
      ExistingIndex: Integer): Boolean;
  public
    property SignTools: TStringList read FSignTools write SetSignTools;
  end;

implementation

uses
  CmnFunc, CompForm, SysUtils;

{$R *.DFM}

procedure TSignToolsForm.UpdateSignTools;
begin
  SignToolsListBox.Items.Assign(FSignTools);
  UpdateHorizontalExtent(SignToolsListBox);
end;

procedure TSignToolsForm.UpdateSignToolsButtons;
var
  Enabled: Boolean;
begin
  Enabled := SignToolsListBox.ItemIndex >= 0;
  EditButton.Enabled := Enabled;
  RemoveButton.Enabled := Enabled;
end;

procedure TSignToolsForm.SetSignTools(SignTools: TStringList);
begin
  FSignTools.Assign(SignTools);
  UpdateSignTools;
  UpdateSignToolsButtons;
end;

procedure TSignToolsForm.FormCreate(Sender: TObject);
begin
  FSignTools := TStringList.Create();
  InitFormFont(Self);
end;

procedure TSignToolsForm.FormDestroy(Sender: TObject);
begin
  FSignTools.Free();
end;

function TSignToolsForm.InputSignTool(var SignToolName, SignToolCommand: String;
  ExistingIndex: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;

  if InputQuery(Caption, 'Name of the Sign Tool:', SignToolName) then begin
    if (SignToolName = '') or (Pos('=', SignToolName) <> 0) then begin
      AppMessageBox(PChar('Invalid name.'), PChar(Caption), MB_OK or MB_ICONSTOP);
      Exit;
    end;

    for I := 0 to FSignTools.Count-1 do begin
      if (I <> ExistingIndex) and (Pos(SignToolName + '=', FSignTools[I]) = 1) then begin
        AppMessageBox(PChar('Duplicate name.'), PChar(Caption), MB_OK or MB_ICONSTOP);
        Exit;
      end;
    end;

    if InputQuery(Caption, 'Command of the Sign Tool:', SignToolCommand) then begin
      if SignToolCommand = '' then begin
        AppMessageBox(PChar('Invalid command.'), PChar(Caption), MB_OK or MB_ICONSTOP);
        Exit;
      end;
    end;

    Result := True;
  end;
end;

procedure TSignToolsForm.AddButtonClick(Sender: TObject);
var
  SignToolName, SignToolCommand: String;
begin
  SignToolName := '';
  SignToolCommand := '';

  if InputSignTool(SignToolName, SignToolCommand, -1) then begin
    FSignTools.Add(SignToolName + '=' + SignToolCommand);
    UpdateSignTools;
    UpdateSignToolsButtons;
  end;
end;

procedure TSignToolsForm.EditButtonClick(Sender: TObject);
var
  SignToolName, SignToolCommand: String;
  I, P: Integer;
begin
  I := SignToolsListBox.ItemIndex;
  P := Pos('=', FSignTools[I]);
  if P = 0 then
    raise Exception.Create('Internal error: ''='' not found in SignTool');
  SignToolName := Copy(FSignTools[I], 1, P-1);
  SignToolCommand := Copy(FSignTools[I], P+1, MaxInt);

  if InputSignTool(SignToolName, SignToolCommand, I) then begin
    FSignTools[I] := SignToolName + '=' + SignToolCommand;
    UpdateSignTools;
    UpdateSignToolsButtons;
  end;
end;

procedure TSignToolsForm.RemoveButtonClick(Sender: TObject);
var
  I: Integer;
begin
  I := SignToolsListBox.ItemIndex;
  FSignTools.Delete(I);
  UpdateSignTools;
  UpdateSignToolsButtons;
end;

procedure TSignToolsForm.SignToolsListBoxClick(Sender: TObject);
begin
  UpdateSignToolsButtons;
end;

end.
