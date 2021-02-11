{
  Inno Setup Preprocessor
  Copyright (C) 2001-2002 Alex Yackimoff
  
  Inno Setup
  Copyright (C) 1997-2020 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.
}

library ISPP;

{$IMAGEBASE $01800000}
{$SETPEOSVERSION 6.0}
{$SETPESUBSYSVERSION 6.0}
{$WEAKLINKRTTI ON}
{$I ..\Version.inc}

uses
  SysUtils,
  Windows,
  Classes,
  CompPreprocInt in '..\CompPreprocInt.pas',
  IsppPreprocess in 'IsppPreprocess.pas',
  IsppPreprocessor in 'IsppPreprocessor.pas',
  IsppFuncs in 'IsppFuncs.pas',
  IsppVarUtils in 'IsppVarUtils.pas',
  IsppConsts in 'IsppConsts.pas',
  IsppStack in 'IsppStack.pas',
  IsppIntf in 'IsppIntf.pas',
  IsppParser in 'IsppParser.pas',
  IsppIdentMan in 'IsppIdentMan.pas',
  IsppSessions in 'IsppSessions.pas',
  CTokenizer in 'CTokenizer.pas',
  IsppBase in 'IsppBase.pas',
  PathFunc in '..\..\Components\PathFunc.pas',
  CmnFunc2 in '..\CmnFunc2.pas',
  FileClass in '..\FileClass.pas',
  Int64Em in '..\Int64Em.pas',
  MD5 in '..\MD5.pas',
  SHA1 in '..\SHA1.pas',
  Struct in '..\Struct.pas';
  
{$R *.RES}

exports
  ISPreprocessScript name 'ISPreprocessScriptW';

end.
