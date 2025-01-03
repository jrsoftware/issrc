unit Setup.RegSvr;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Registers OLE servers & type libraries after a reboot
}

interface

procedure RunRegSvr;

implementation

uses
  Windows, SysUtils, Classes, Forms, PathFunc, Shared.CommonFunc, Setup.InstFunc, Setup.InstFunc.Ole,
  Shared.FileClass, Shared.CommonFunc.Vcl, Shared.Struct, Setup.MainFunc,
  SetupLdrAndSetup.Messages, Shared.SetupMessageIDs, Setup.RegDLL, Setup.Helper;

procedure DeleteOldTempFiles(const Path: String);
{ Removes any old isRS-???.tmp files from Path. Not strictly necessary, but
  in case a prior multi-install run left behind multiple .tmp files now is a
  good time to clean them up. }
var
  H: THandle;
  FindData: TWin32FindData;
  Filename: String;
begin
  H := FindFirstFile(PChar(Path + 'isRS-???.tmp'), FindData);
  if H <> INVALID_HANDLE_VALUE then begin
    try
      repeat
        { Yes, this StrLIComp is superfluous. When deleting files from
          potentionally the Windows directory I can't help but be *extra*
          careful. :) }
        if (StrLIComp(FindData.cFileName, 'isRS-', Length('isRS-')) = 0) and
           (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY = 0) then begin
          Filename := Path + FindData.cFileName;
          { If the file is read-only, try to strip the attribute }
          if FindData.dwFileAttributes and FILE_ATTRIBUTE_READONLY <> 0 then
            SetFileAttributes(PChar(Filename), FindData.dwFileAttributes
              and not FILE_ATTRIBUTE_READONLY);
          DeleteFile(Filename);
        end;
      until not FindNextFile(H, FindData);
    finally
      Windows.FindClose(H);
    end;
  end;
end;

function RenameToNonRandomTempName(const Filename: String): String;
{ Renames Filename to a name in the format: isRS-nnn.tmp. Returns the new
  filename if successful, or '' if not. Calls MoveFileEx. }
var
  Path, NewFilename: String;
  Attribs: DWORD;
  Attempts, I: Integer;
begin
  Result := '';
  Path := PathExtractPath(Filename);
  Attempts := 0;
  for I := 0 to 999 do begin
    NewFilename := Path + Format('isRS-%.3u.tmp', [I]);
    Attribs := GetFileAttributes(PChar(NewFilename));
    if Attribs <> $FFFFFFFF then begin
      { Skip any directories that happen to named NewFilename }
      if Attribs and FILE_ATTRIBUTE_DIRECTORY <> 0 then
        Continue;
      { If the existing file is read-only, try to strip the attribute }
      if Attribs and FILE_ATTRIBUTE_READONLY <> 0 then
        SetFileAttributes(PChar(NewFilename), Attribs and not FILE_ATTRIBUTE_READONLY);
    end;
    if MoveFileEx(PChar(Filename), PChar(NewFilename), MOVEFILE_REPLACE_EXISTING) then begin
      Result := NewFilename;
      Break;
    end;
    Inc(Attempts);
    { Limit MoveFileEx calls to 10 since it can be really slow over network
      connections when a file is in use }
    if Attempts = 10 then
      Break;
  end;
end;

procedure DeleteSelf;
var
  SelfFilename, NewFilename: String;
begin
  SelfFilename := NewParamStr(0);
  { RestartReplace will fail if the user doesn't have admin
    privileges. We don't want to leak temporary files, so try to rename
    ourself to a non-random name. This way, future runs should just keep
    overwriting the same temp file. }
  DeleteOldTempFiles(PathExtractPath(SelfFilename));
  NewFilename := RenameToNonRandomTempName(SelfFilename);
  if NewFilename <> '' then
    RestartReplace(False, NewFilename, '')
  else
    RestartReplace(False, SelfFilename, '');
end;

procedure RunRegSvr;
var
  CreatedAsAdmin, NoErrorMessages: Boolean;
  Mutex: THandle;
  F: TTextFileReader;
  MsgFilename, ListFilename, L, RegFilename: String;
begin
  if CompareText(NewParamStr(1), '/REG') = 0 then
    CreatedAsAdmin := True
  else if CompareText(NewParamStr(1), '/REGU') = 0 then
    CreatedAsAdmin := False
  else
    Exit;

  { Set default title; it's set again below after the messages are read }
  Application.Title := 'Setup';
  Application.MainFormOnTaskBar := True;

  InitializeCommonVars;

  { Try to create and acquire a mutex.
    In cases where multiple IS installers have each created their own RegSvr
    RunOnce entries in HKCU, Windows Explorer will execute them asynchronously.
    This could have undesirable ramifications -- what might happen if the same
    DLL were registered simultaneously by two RegSvr processes? Could the
    registry entries be in an incomplete/inconsistent state? I'm not sure, so
    a mutex is used here to ensure registrations are serialized. }
  Mutex := Windows.CreateMutex(nil, False, 'Inno-Setup-RegSvr-Mutex');
  if Mutex <> 0 then begin
    { Even though we have no visible windows, process messages while waiting
      so Windows doesn't think we're hung }
    repeat
      Application.ProcessMessages;
    until MsgWaitForMultipleObjects(1, Mutex, False, INFINITE,
      QS_ALLINPUT) <> WAIT_OBJECT_0+1;
  end;
  try
    MsgFilename := PathChangeExt(NewParamStr(0), '.msg');
    ListFilename := PathChangeExt(NewParamStr(0), '.lst');
    { The .lst file may not exist at this point, if we were already run
      previously, but the RunOnce entry could not be removed due to lack of
      admin privileges. }
    if NewFileExists(ListFilename) then begin
      { Need to load messages in order to display exception messages below.
        Note: The .msg file only exists when the .lst file does. }
      LoadSetupMessages(MsgFilename, 0, True);
      SetMessageBoxRightToLeft(lfRightToLeft in MessagesLangOptions.Flags);
      Application.Title := SetupMessages[msgSetupAppTitle];

      try
        { Extract the 64-bit helper }
        CreateTempInstallDirAndExtract64BitHelper;

        F := TTextFileReader.Create(ListFilename, fdOpenExisting, faRead, fsRead);
        try
          while not F.Eof do begin
            L := F.ReadLine;
            if (Length(L) > 4) and (L[1] = '[') and (L[4] = ']') then begin
              RegFilename := Copy(L, 5, Maxint);
              NoErrorMessages := (L[3] = 'q') or (CreatedAsAdmin and not IsAdmin);
              try
                case L[2] of
                  's': RegisterServer(False, False, RegFilename, NoErrorMessages);
                  'S': RegisterServer(False, True, RegFilename, NoErrorMessages);
                  't': RegisterTypeLibrary(RegFilename);
                  'T': HelperRegisterTypeLibrary(False, RegFilename);
                end;
              except
                { Display the exception message (with a caption of 'Setup' so
                  people have some clue of what generated it), and keep going.
                  Exception: Don't display the message if the program was
                  installed as an admin (causing the RunOnce entry to be created
                  in HKLM) and the user isn't logged in as an admin now. That's
                  almost certainly going to result in errors; let's not complain
                  about it. The RunOnce entry should survive a logoff (since
                  only admins can write to HKLM's RunOnce); once the user logs
                  back in as an admin the files will get registered for real,
                  and we won't suppress error messages then. }
                if not NoErrorMessages then
                  AppMessageBox(PChar(RegFilename + SNewLine2 +
                    FmtSetupMessage1(msgErrorRegisterServer, GetExceptMessage)),
                    PChar(SetupMessages[msgSetupAppTitle]), MB_OK or MB_ICONEXCLAMATION);
              end;
            end;
          end;
        finally
          F.Free;
        end;
      finally
        RemoveTempInstallDir;
      end;
    end;

    DeleteFile(ListFilename);
    DeleteFile(MsgFilename);

    try
      DeleteSelf;
    except
      { ignore exceptions }
    end;
  finally
    if Mutex <> 0 then begin
      ReleaseMutex(Mutex);
      CloseHandle(Mutex);
    end;
  end;
end;

end.
