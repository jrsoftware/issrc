unit Compiler.BuiltinPreproc;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Built-in preprocessor

  By default, scripts use ISPP if available, and .isl files use the built-in preprocessor
}

interface

uses
  Shared.PreprocInt;

function BuiltinPreprocessScript(var Params: TPreprocessScriptParams): Integer; stdcall;

implementation

uses
  SysUtils, Classes, PathFunc,
  Shared.CommonFunc, Compiler.Messages, Compiler.HelperFunc;

type
  EBuiltinPreprocessScriptError = class(Exception);

function BuiltinPreprocessScript(var Params: TPreprocessScriptParams): Integer; stdcall;
var
  IncludeStack: TStringList;

  procedure RaiseError(const LineFilename: String; const LineNumber: Integer;
    const Msg: String);
  begin
    Params.ErrorProc(Params.CompilerData, PChar(Msg), PChar(LineFilename),
      LineNumber, 0);
    { Note: This exception is caught and translated into ispePreprocessError }
    raise EBuiltinPreprocessScriptError.Create('BuiltinPreprocessScript error');
  end;

  procedure ProcessLines(const Filename: String; const FileHandle: TPreprocFileHandle);
    forward;

  procedure ProcessLinesFromFile(const LineFilename: String;
    const LineNumber: Integer; const IncludeFilename: String);
  var
    I: Integer;
    FileHandle: TPreprocFileHandle;
  begin
    { Check if it's a recursive include }
    for I := 0 to IncludeStack.Count-1 do
      if PathCompare(IncludeStack[I], IncludeFilename) = 0 then
        RaiseError(LineFilename, LineNumber, Format(SCompilerRecursiveInclude,
          [IncludeFilename]));

    FileHandle := Params.LoadFileProc(Params.CompilerData,
      PChar(IncludeFilename), PChar(LineFilename), LineNumber, 0);
    if FileHandle < 0 then begin
      { Note: The message here shouldn't be seen as LoadFileProc should have
        already called ErrorProc itself }
      RaiseError(LineFilename, LineNumber, 'LoadFileProc failed');
    end;
    ProcessLines(IncludeFilename, FileHandle);
  end;

  procedure ProcessDirective(const LineFilename: String; const LineNumber: Integer;
    D: String);
  var
    Dir, IncludeFilename: String;
  begin
    if Copy(D, 1, Length('include')) = 'include' then begin
      Delete(D, 1, Length('include'));
      if (D = '') or (D[1] > ' ') then
        RaiseError(LineFilename, LineNumber, SCompilerInvalidDirective);
      D := TrimLeft(D);
      if (Length(D) < 3) or (D[1] <> '"') or (PathLastChar(D)^ <> '"') then
        RaiseError(LineFilename, LineNumber, SCompilerInvalidDirective);
      if LineFilename = '' then
        Dir := Params.SourcePath
      else
        Dir := PathExtractPath(LineFilename);
      IncludeFilename := Params.PrependDirNameProc(Params.CompilerData,
        PChar(RemoveQuotes(D)), PChar(Dir), PChar(LineFilename), LineNumber, 0);
      if IncludeFilename = '' then begin
        { Note: The message here shouldn't be seen as PrependDirNameProc
          should have already called ErrorProc itself }
        RaiseError(LineFilename, LineNumber, 'PrependDirNameProc failed');
      end;
      Params.StatusProc(Params.CompilerData,
        PChar(Format(SBuiltinPreprocessStatusIncludingFile, [IncludeFilename])), False);
      ProcessLinesFromFile(LineFilename, LineNumber, PathExpand(IncludeFilename));
    end
    else
      RaiseError(LineFilename, LineNumber, SCompilerInvalidDirective);
  end;

  procedure ProcessLines(const Filename: String; const FileHandle: TPreprocFileHandle);
  var
    I: Integer;
    LineText, L: PChar;
  begin
    IncludeStack.Add(Filename);
    I := 0;
    while True do begin
      LineText := Params.LineInProc(Params.CompilerData, FileHandle, I);
      if LineText = nil then
        Break;
      L := LineText;
      SkipWhitespace(L);
      if L^ = '#' then
        ProcessDirective(Filename, I + 1, L + 1)
      else
        Params.LineOutProc(Params.CompilerData, PChar(Filename), I + 1,
          LineText);
      Inc(I);
    end;
    IncludeStack.Delete(IncludeStack.Count-1);
  end;

begin
  if (Params.Size <> SizeOf(Params)) or
     (Params.InterfaceVersion <> 3) then begin
    Result := ispeInvalidParam;
    Exit;
  end;

  try
    IncludeStack := TStringList.Create;
    try
      ProcessLines(Params.Filename, 0);
    finally
      IncludeStack.Free;
    end;
    Result := ispeSuccess;
  except
    Result := ispePreprocessError;
    if not(ExceptObject is EBuiltinPreprocessScriptError) then
      raise;
  end;
end;

end.