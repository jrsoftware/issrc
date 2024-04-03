unit NewNotebookReg;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  TNewNotebook design-time registration
}

interface

uses
  Classes;

procedure Register;

implementation

uses
  NewNotebook, DesignIntf, DesignEditors;

{ TNewNotebookEditor }

type
  TNewNotebookEditor = class(TComponentEditor)
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): String; override;
    function GetVerbCount: Integer; override;
  end;

procedure TNewNotebookEditor.Edit;
var
  Notebook: TNewNotebook;
begin
  { When a page is double-clicked, select the parent notebook }
  if Component is TNewNotebookPage then begin
    Notebook := TNewNotebookPage(Component).Notebook;
    if Assigned(Notebook) then
      Designer.SelectComponent(Notebook);
  end
end;

procedure TNewNotebookEditor.ExecuteVerb(Index: Integer);
var
  Notebook: TNewNotebook;
  Page: TNewNotebookPage;
begin
  { Find the notebook component to operate on. Note that this same editor class
    is used for both TNewNotebook and TNewNotebookPage. }
  if Component is TNewNotebookPage then begin
    Notebook := TNewNotebookPage(Component).Notebook;
    if Notebook = nil then
      Exit;  { just in case }
  end
  else
    Notebook := Component as TNewNotebook;

  case Index of
    0, 1:
      begin
        Page := Notebook.FindNextPage(Notebook.ActivePage, Index = 0);
        Notebook.ActivePage := Page;
        Designer.Modified;
        Designer.SelectComponent(Page);
      end;
    3:
      begin
        Page := TNewNotebookPage.Create(Notebook.Owner);
        Page.Name := Designer.UniqueName(Page.ClassName);
        Page.Notebook := Notebook;
        Notebook.ActivePage := Page;
        Designer.Modified;
        Designer.SelectComponent(Page);
      end;
  end;
end;

function TNewNotebookEditor.GetVerbCount: Integer;
begin
  Result := 4;
end;

function TNewNotebookEditor.GetVerb(Index: Integer): String;
begin
  case Index of
    0: Result := 'Next Page';
    1: Result := 'Previous Page';
    2: Result := '-';
    3: Result := 'New Page';
  else
    Result := '';
  end;
end;

procedure Register;
begin
  RegisterComponents('JR', [TNewNotebook]);
  RegisterClass(TNewNotebookPage);

  RegisterComponentEditor(TNewNotebook, TNewNotebookEditor);
  RegisterComponentEditor(TNewNotebookPage, TNewNotebookEditor);
end;

end.
