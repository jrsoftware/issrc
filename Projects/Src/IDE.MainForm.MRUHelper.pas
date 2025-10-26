unit IDE.MainForm.MRUHelper;

{
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Compiler form - MRU helper

  Not used by MainForm: it uses IDE.MainForm.FinalHelper instead
}

interface

uses
  IDE.MainForm;

type
  TMainFormMRUHelper = class helper for TMainForm
    procedure ClearMRUMainFilesList;
    procedure ReadMRUMainFilesList;
    procedure ModifyMRUMainFilesList(const AFilename: String; const AddNewItem: Boolean);
    procedure ReadMRUParametersList;
    procedure ModifyMRUParametersList(const AParameter: String; const AddNewItem: Boolean);
  end;

implementation

uses
  Classes, SysUtils, Forms,
  PathFunc,
  Shared.ConfigIniFile;

type
  TMRUItemCompareProc = function(const S1, S2: String): Integer;

procedure ClearMRUList(const MRUList: TStringList; const Section: String);
var
  Ini: TConfigIniFile;
begin
  Ini := TConfigIniFile.Create;
  try
    MRUList.Clear;
    Ini.EraseSection(Section);
  finally
    Ini.Free;
  end;
end;

procedure ReadMRUList(const MRUList: TStringList; const Section, Ident: String);
{ Loads a list of MRU items from the registry }
var
  Ini: TConfigIniFile;
  I: Integer;
  S: String;
begin
  Ini := TConfigIniFile.Create;
  try
    MRUList.Clear;
    for I := 0 to MRUListMaxCount-1 do begin
      S := Ini.ReadString(Section, Ident + IntToStr(I), '');
      if S <> '' then MRUList.Add(S);
    end;
  finally
    Ini.Free;
  end;
end;

procedure ModifyMRUList(const MRUList: TStringList; const Section, Ident: String;
  const AItem: String; const AddNewItem: Boolean; CompareProc: TMRUItemCompareProc);
var
  I: Integer;
  Ini: TConfigIniFile;
  S: String;
begin
  I := 0;
  while I < MRUList.Count do begin
    if CompareProc(MRUList[I], AItem) = 0 then
      MRUList.Delete(I)
    else
      Inc(I);
  end;
  if AddNewItem then
    MRUList.Insert(0, AItem);
  while MRUList.Count > MRUListMaxCount do
    MRUList.Delete(MRUList.Count-1);

  { Save new MRU items }
  Ini := TConfigIniFile.Create;
  try
    { MRU list }
    for I := 0 to MRUListMaxCount-1 do begin
      if I < MRUList.Count then
        S := MRUList[I]
      else
        S := '';
      Ini.WriteString(Section, Ident + IntToStr(I), S);
    end;
  finally
    Ini.Free;
  end;
end;

{ TMainFormMRUHelper }

procedure TMainFormMRUHelper.ClearMRUMainFilesList;
begin
  try
    ClearMRUList(FMRUMainFilesList, 'ScriptFileHistoryNew');
  except
    { Ignore any exceptions. }
  end;
end;

procedure TMainFormMRUHelper.ReadMRUMainFilesList;
begin
  try
    ReadMRUList(FMRUMainFilesList, 'ScriptFileHistoryNew', 'History');
  except
    { Ignore any exceptions. }
  end;
end;

procedure TMainFormMRUHelper.ModifyMRUMainFilesList(const AFilename: String;
  const AddNewItem: Boolean);
begin
  { Load most recent items first, just in case they've changed }
  try
    ReadMRUMainFilesList;
  except
    { Ignore any exceptions. }
  end;
  try
    ModifyMRUList(FMRUMainFilesList, 'ScriptFileHistoryNew', 'History', AFileName, AddNewItem, @PathCompare);
  except
    { Handle exceptions locally; failure to save the MRU list should not be
      a fatal error. }
    Application.HandleException(Self);
  end;
end;

procedure TMainFormMRUHelper.ReadMRUParametersList;
begin
  try
    ReadMRUList(FMRUParametersList, 'ParametersHistory', 'History');
  except
    { Ignore any exceptions. }
  end;
end;

procedure TMainFormMRUHelper.ModifyMRUParametersList(const AParameter: String;
  const AddNewItem: Boolean);
begin
  { Load most recent items first, just in case they've changed }
  try
    ReadMRUParametersList;
  except
    { Ignore any exceptions. }
  end;
  try
    ModifyMRUList(FMRUParametersList, 'ParametersHistory', 'History', AParameter, AddNewItem, @CompareText);
  except
    { Handle exceptions locally; failure to save the MRU list should not be
      a fatal error. }
    Application.HandleException(Self);
  end;
end;

end.
