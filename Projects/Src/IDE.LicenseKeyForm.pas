unit IDE.LicenseKeyForm;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Compiler IDE license key form
}

interface

uses
  Classes, Controls, StdCtrls, UIStateForm;

type
  TLicenseKeyForm = class(TUIStateForm)
    CancelButton: TButton;
    GroupBox1: TGroupBox;
    LicenseKeyMemo: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure LicenseKeyMemoChange(Sender: TObject);
    procedure LicenseKeyMemoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  end;

implementation

{$R *.DFM}

uses
  Windows, Shared.LicenseFunc, IDE.HelperFunc;

procedure TLicenseKeyForm.FormCreate(Sender: TObject);
begin
  InitFormFont(Self);
  InitFormTheme(Self);

  LicenseKeyMemo.Font.Name := GetPreferredMemoFont;
  LicenseKeyMemo.Font.Size := 10;
end;

procedure TLicenseKeyForm.LicenseKeyMemoChange(Sender: TObject);
begin
  var License: TLicense;
  if ParseLicenseKey(LicenseKeyMemo.Text, License) then begin
    UpdateLicense(License);
    ModalResult := mrOk;
  end;
end;

procedure TLicenseKeyForm.LicenseKeyMemoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
end;

end.
