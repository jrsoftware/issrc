unit Shared.PreprocInt;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Compiler preprocessor interface used by ISCmplr and ISPP
}

interface

uses
  Windows;

const
  ispeSuccess = 0;
  ispeInvalidParam = 1;
  ispePreprocessError = 2;
  ispeSilentAbort = 3;

type
  TPreprocCompilerData = type Pointer;
  TPreprocFileHandle = type Integer;
  TPreprocLoadFileProc =
    function(CompilerData: TPreprocCompilerData; Filename: PChar;
      ErrorFilename: PChar; ErrorLine: Integer;
      ErrorColumn: Integer): TPreprocFileHandle; stdcall;
  TPreprocLineInProc =
    function(CompilerData: TPreprocCompilerData; FileHandle: TPreprocFileHandle;
      LineIndex: Integer): PChar; stdcall;
  TPreprocLineOutProc =
    procedure(CompilerData: TPreprocCompilerData; LineFilename: PChar;
      LineNumber: Integer; LineText: PChar); stdcall;
  TPreprocErrorProc =
    procedure(CompilerData: TPreprocCompilerData; ErrorMsg: PChar;
      ErrorFilename: PChar; ErrorLine: Integer; ErrorColumn: Integer); stdcall;
  TPreprocStatusProc =
    procedure(CompilerData: TPreprocCompilerData; StatusMsg: PChar; Warning: BOOL); stdcall;
  TPreprocPrependDirNameProc =
    function(CompilerData: TPreprocCompilerData; Filename: PChar; Dir: PChar;
      ErrorFilename: PChar; ErrorLine: Integer; ErrorColumn: Integer): PChar; stdcall;
  TPreprocCleanupProc = function(CleanupProcData: Pointer): Integer; stdcall;
  TPreprocIdleProc = procedure(CompilerData: TPreprocCompilerData); stdcall;

  PPreprocessScriptParams = ^TPreprocessScriptParams;
  TPreprocessScriptParams = record
    Size: Cardinal;                { [in] Set to SizeOf(TPreprocessScriptParams).
                                     Preprocessor must return ispeInvalidParam
                                     if value is not recognized. }
    InterfaceVersion: Cardinal;    { [in] Currently set to 3.
                                     Preprocessor must return ispeInvalidParam
                                     if value is not recognized. }
    CompilerBinVersion: Cardinal;  { [in] Compiler version as an integer }
    Filename: PChar;               { [in] The full name of the file being
                                     preprocessed, or an empty string if
                                     preprocessing the main script. }
    SourcePath: PChar;             { [in] The default source directory, and
                                     directory to look in for #include files.
                                     Normally, this is the directory containing
                                     the script file. }
    CompilerPath: PChar;           { [in] The "compiler:" directory. This is
                                     the directory which contains the *.e32
                                     files. }
    Options: PChar;                { [in] The 'ISPP:'-prefixed options that
                                     were passed to the compiler in
                                     TCompileScriptParamsEx.Options. }
    CompilerData: TPreprocCompilerData;  { [in] Opaque value supplied by the
                                           compiler that the preprocessor must
                                           pass unchanged when calling the
                                           *Proc callback functions. }

    LoadFileProc: TPreprocLoadFileProc;
                                       { [in] Call to load a new file. Returns
                                         a "handle" to the file which can be
                                         passed to LineInProc. On failure,
                                         returns -1 and internally calls
                                         ErrorProc with a description of the
                                         error. }
    LineInProc: TPreprocLineInProc;    { [in] Call to read a line from the
                                         specified file. LineIndex is
                                         zero-based. The returned pointer is
                                         valid only until the next LineInProc
                                         call is made (or the preprocess
                                         function returns). NULL is returned
                                         if EOF is reached. }
    LineOutProc: TPreprocLineOutProc;  { [in] Call to send preprocessed
                                         line back to the compiler. }
    StatusProc: TPreprocStatusProc;    { [in] Call to log a message. }
    ErrorProc: TPreprocErrorProc;      { [in] Call to report an error. }
    PrependDirNameProc: TPreprocPrependDirNameProc;
                                       { [in] If the specified filename is
                                         relative, prepends the specified
                                         directory name (must include trailing
                                         path separator). If the specified
                                         filename begins with a prefix, such
                                         as "compiler:", expands the prefix.
                                         The returned pointer is valid only
                                         until the next PrependDirNameProc
                                         call is made (or the preprocess
                                         function returns). On failure,
                                         returns NULL and internally calls
                                         ErrorProc with a description of the
                                         error.}
    IdleProc: TPreprocIdleProc;        { [in] Call at various intervals during
                                         preprocessing. Doesn't allow an Abort
                                         by the host. }

    PreprocCleanupProc: TPreprocCleanupProc;
                                       { [out] Preprocessor-defined function
                                         that, if set, is called after
                                         compilation completes or is aborted.
                                         Note: This function will still be
                                         called if the preprocess function
                                         returns a non-ispeSuccess value. }
    PreprocCleanupProcData: Pointer;   { [out] Preprocessor-defined value
                                         passed to PreprocCleanupProc. }
  end;

  TPreprocessScriptProc = function(var Params: TPreprocessScriptParams): Integer;
    stdcall;

implementation

end.
