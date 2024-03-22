unit CompFileListWin;

interface

uses
  Classes, Controls, Forms, Dialogs, ExtCtrls, StdCtrls,
  NewStaticText, DropListBox, CompWizardFilesHelper;

type
  TCFLWForm = class(TForm)
    Panel1: TPanel;
    OkButton: TButton;
    CancelButton: TButton;
    AppFilesEditButton: TButton;
    AppFilesRemoveButton: TButton;
    AppFilesAddDirButton: TButton;
    AppFilesAddButton: TButton;
    AppFilesListBox: TDropListBox;
    AppFilesLabel: TNewStaticText;
    NotCreateAppDirCheck: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FFilesHelper: TWizardFormFilesHelper;
    function GetText: String;
  public
    property Text: string read GetText;
  end;

implementation

{$R *.dfm}

procedure TCFLWForm.FormCreate(Sender: TObject);
begin
  FFilesHelper := TWizardFormFilesHelper.Create(Handle,
    NotCreateAppDirCheck, AppFilesListBox, AppFilesAddButton, AppFilesAddDirButton,
    AppFilesEditButton, AppFilesRemoveButton);;
end;

procedure TCFLWForm.FormDestroy(Sender: TObject);
begin
  FFilesHelper.Free;
end;

function TCFLWForm.GetText: String;
begin
  Result := '';
  FFilesHelper.AddScript(Result);
end;

end.
