unit CompWizardRegistryHelper;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Helper to avoid duplicate code between CompWizard and CompRegistryDesigner
}

interface

uses
  Windows, StdCtrls;

type
  TPriviligesRequired = (prAdmin, prLowest, prDynamic);

  TWizardFormRegistryHelper = class
    private
      FHandle: HWND;
      FFileEdit: TEdit;
      FPriviligesRequired: TPriviligesRequired;
      procedure SetPriviligesRequired(const Value: TPriviligesRequired);
      procedure FileButtonClick(Sender: TObject);
    public
      constructor Create(const Handle: HWND; const FileEdit: TEdit;
        const FileButton: TButton);
      procedure AddScript(var Files: String);
      property PriviligesRequired: TPriviligesRequired write SetPriviligesRequired;
    end;

implementation

uses
  CompMsgs, BrowseFunc, CmnFunc2;

{ TWizardFormRegistryHelper }

procedure TWizardFormRegistryHelper.SetPriviligesRequired(
  const Value: TPriviligesRequired);
begin
  FPriviligesRequired := Value;
end;

constructor TWizardFormRegistryHelper.Create(const Handle: HWND;
  const FileEdit: TEdit; const FileButton: TButton);
begin
  TryEnableAutoCompleteFileSystem(FileEdit.Handle);

  FFileEdit := FileEdit;

  FileButton.OnClick := FileButtonClick;
end;

procedure TWizardFormRegistryHelper.FileButtonClick(Sender: TObject);
begin
  var FileName: String := FFileEdit.Text;
  if NewGetOpenFileName('', FileName, '', SWizardAppRegFilter, SWizardAppRegDefaultExt, FHandle) then
    FFileEdit.Text := FileName;
end;

procedure TWizardFormRegistryHelper.AddScript(var Files: String);
begin

end;

end.
