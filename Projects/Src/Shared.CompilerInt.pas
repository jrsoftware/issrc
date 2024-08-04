unit Shared.CompilerInt;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Compiler interface
}

interface

uses
  Windows;

const
  { Constants passed in Code parameter of callback function }
  iscbReadScript = 1;      { Sent when compiler needs the next script line }
  iscbNotifyStatus = 2;    { Sent to notify the application of compiler status }
  iscbNotifyIdle = 3;      { Sent at various intervals during the compilation }
  iscbNotifySuccess = 4;   { Sent when compilation succeeds }
  iscbNotifyError = 5;     { Sent when compilation fails or is aborted by the
                             application }
  iscbNotifyPreproc = 6;   { Sent to notify the application of preprocessor results }

  { Return values for callback function }
  iscrSuccess = 0;         { Return this for compiler to continue }
  iscrRequestAbort = 1;    { Return this to abort compilation immediately.
                             (When this value is returned, it is not necessary
                             to set any of the "out" fields in the
                             TCompilerCallbackData; the compiler will ignore
                             them.) }

  { Return values for ISDllCompileScript }
  isceNoError = 0;         { Successful }
  isceInvalidParam = 1;    { Bad parameters passed to function }
  isceCompileFailure = 2;  { There was an error compiling or it was aborted
                             by the application }

type
  { TCompilerCallbackData is a record passed to the callback function. The
    fields which you may access vary depending on what Code was passed to the
    callback function. }
  TCompilerCallbackData = record
    case Integer of
      iscbReadScript: (
        Reset: BOOL;          { [in] This field can be ignored in compiler
                                versions 3.0.1 and later. (Previous versions
                                of the compiler made multiple passes over the
                                script, and set Reset to True when it needed
                                to return to the beginning.) }
        LineRead: PChar);     { [out] Application returns pointer to the next
                                line it reads, or a NULL pointer if the end of
                                file is reached. Application is responsible for
                                allocating a buffer to hold the line; LineRead
                                is initially NULL when the callback function
                                is called. The pointer only needs to remain
                                valid until the next time the callback function
                                is called (i.e. the application may return the
                                same pointer each time). }

      iscbNotifyStatus: (
        StatusMsg: PChar;     { [in] Contents of status message. }
        Warning: BOOL);       { [in] Warning indicator (new in 6.0.0) }

      iscbNotifyIdle: (
        CompressProgress: Cardinal;     { [in] Amount compressed so far
                                          (new in 4.1.6) }
        CompressProgressMax: Cardinal;  { [in] Maximum value of CompressProgress
                                          (new in 4.1.6) }
        SecondsRemaining: Integer;      { [in] Estimated time remaining, or -1
                                          if not known (new in 5.1.13) }
        BytesCompressedPerSecond: Cardinal); { [in] Average bytes compressed
                                               per second (new in 5.1.13) }

      iscbNotifyPreproc: (
        PreprocessedScript: PChar; { [in] Preprocessed script (new in 6.1.0) }
        IncludedFilenames: PChar); { [in] Names of #included files. Each name is
                                          a null-terminated string, and the final
                                          name is followed by an additional null
                                          character (new in 6.1.0) }

      iscbNotifySuccess: (
        OutputExeFilename: PChar;  { [in] The name of the resulting setup.exe,
                                          or empty if output was disabled
                                          (latter new in 5.5.5) }
        DebugInfo: Pointer;        { [in] Debug info (new in 3.0.0.1) }
        DebugInfoSize: Cardinal);  { [in] Size of debug info (new in 3.0.0.1) }

      iscbNotifyError: (
        ErrorMsg: PChar;      { [in] The error message, or NULL if compilation
                                was aborted by the application. }
        ErrorFilename: PChar; { [in] Filename in which the error occurred. This
                                is NULL if the file is the main script. }
        ErrorLine: Integer);  { [in] The line number the error occurred on.
                                Zero if the error doesn't apply to any
                                particular line. }
  end;

  TCompilerCallbackProc = function(Code: Integer;
    var Data: TCompilerCallbackData; AppData: Longint): Integer; stdcall;

  PCompileScriptParamsEx = ^TCompileScriptParamsEx;
  TCompileScriptParamsEx = record
    Size: Cardinal;       { [in] Set to SizeOf(TCompileScriptParamsEx). }
    CompilerPath: PChar;  { [in] The "compiler:" directory. This is the
                            directory which contains the *.e32 files. If this
                            is set to NULL, the compiler will use the directory
                            containing the compiler DLL/EXE. }
    SourcePath: PChar;    { [in] The default source directory, and directory to
                            look in for #include files. Normally, this is
                            the directory containing the script file. This
                            cannot be NULL. }
    CallbackProc: TCompilerCallbackProc;
                          { [in] The callback procedure which the compiler calls
                            to read the script and for status notification. }
    AppData: Longint;     { [in] Application-defined. AppData is passed to the
                            callback function. }
    Options: PChar;       { [in] Additional options. Each option is a
                            null-terminated string, and the final option is
                            followed by an additional null character.
                            If you do not wish to specify any options, set this
                            field to NULL or to point to a single null
                            character.

                            Currently supported options:

                            Output=(0|no|false|1|yes|true)
                              Enables or disables output.
                            OutputBaseFilename=[filename]
                              Overrides any OutputBaseFilename setting in the
                              script; causes the compiler to use [filename]
                              instead.
                            OutputDir=[path]
                              Overrides any output directory in the script;
                              causes the compiler to use [path] instead.
                            SignTool-[name]=[command]
                              Configures a SignTool with name [name] and command
                              [command].
                            ISPP:[isppoption]
                              Configures an ISPP option. }
  end;

  { The old TCompileScriptParams record. Use this in place of
    TCompileScriptParamsEx if you need call ISCmplr.dll versions below
    5.0.5. It's the same except it lacks an Options field. }
  TCompileScriptParams = record
    Size: Cardinal;       { [in] Set to SizeOf(TCompileScriptParams). }
    CompilerPath: PChar;
    SourcePath: PChar;
    CallbackProc: TCompilerCallbackProc;
    AppData: Longint;
  end;

  PCompilerVersionInfo = ^TCompilerVersionInfo;
  TCompilerVersionInfo = record
    Title: PAnsiChar;      { Name of compiler engine - 'Inno Setup' }
    Version: PAnsiChar;    { Version number text }
    BinVersion: Cardinal;  { Version number as an integer }
  end;

const
  ISCmplrDLL = 'ISCmplr.dll';

{ The ISDllCompileScript function begins compilation of a script. See the above
  description of the TCompileScriptParams record. Return value is one of the
  isce* constants. }
function ISDllCompileScript(const Params: TCompileScriptParamsEx): Integer;
  stdcall; external ISCmplrDLL name 'ISDllCompileScriptW';

{ The ISDllGetVersion returns a pointer to a TCompilerVersionInfo record which
  contains information about the compiler version. }
function ISDllGetVersion: PCompilerVersionInfo; stdcall; external ISCmplrDLL;

implementation

end.
