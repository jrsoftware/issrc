unit IDE.RichEditForm;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Compiler IDE RTF Editor form
}

interface

uses
  Classes, Forms,
  IDE.IDEForm;

type
  TRichEditForm = class(TIDEForm)
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  RichEditForm: TRichEditForm;

implementation

{$IF RtlVersion >= 36.0}
uses
  Themes;
{$ENDIF}

{$R *.dfm}

constructor TRichEditForm.Create(AOwner: TComponent);
begin
  { Set PopupParent before it applies the dark title bar: setting it
    afterwards would recreate the handle and drop the dark-mode attribute. }
  PopupMode := pmExplicit;
  PopupParent := Application.MainForm;
  inherited;
  {$IF RtlVersion >= 36.0}
  { See MainForm }
  StyleName := TStyleManager.ActiveStyle.Name;
  {$ENDIF}
end;

procedure TRichEditForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TRichEditForm.FormDestroy(Sender: TObject);
begin
  if RichEditForm = Self then
    RichEditForm := nil;
end;

end.
