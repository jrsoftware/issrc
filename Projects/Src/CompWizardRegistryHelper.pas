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
      FPriviligesRequired: TPriviligesRequired;
      procedure SetPriviligesRequired(const Value: TPriviligesRequired);
    public
      constructor Create(const Handle: HWND; const AppRegistryFileEdit: TEdit);
      procedure AddScript(var Files: String);
      property PriviligesRequired: TPriviligesRequired write SetPriviligesRequired;
    end;

implementation

uses
  CmnFunc2;

{ TWizardFormRegistryHelper }

procedure TWizardFormRegistryHelper.SetPriviligesRequired(
  const Value: TPriviligesRequired);
begin
  FPriviligesRequired := Value;
end;

constructor TWizardFormRegistryHelper.Create(const Handle: HWND;
  const AppRegistryFileEdit: TEdit);
begin
  TryEnableAutoCompleteFileSystem(AppRegistryFileEdit.Handle);
end;

procedure TWizardFormRegistryHelper.AddScript(var Files: String);
begin

end;

end.
