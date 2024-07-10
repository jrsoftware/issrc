{
  Inno Setup Preprocessor
  Copyright (C) 2001-2002 Alex Yackimoff
  
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.
}

library ISPP;

uses
  SysUtils,
  Windows,
  Classes,
  CompPreprocInt in 'Src\CompPreprocInt.pas',
  IsppPreprocess in 'Src\ISPP\IsppPreprocess.pas',
  IsppPreprocessor in 'Src\ISPP\IsppPreprocessor.pas',
  IsppFuncs in 'Src\ISPP\IsppFuncs.pas',
  IsppVarUtils in 'Src\ISPP\IsppVarUtils.pas',
  IsppConsts in 'Src\ISPP\IsppConsts.pas',
  IsppStack in 'Src\ISPP\IsppStack.pas',
  IsppIntf in 'Src\ISPP\IsppIntf.pas',
  IsppParser in 'Src\ISPP\IsppParser.pas',
  IsppIdentMan in 'Src\ISPP\IsppIdentMan.pas',
  IsppSessions in 'Src\ISPP\IsppSessions.pas',
  CTokenizer in 'Src\ISPP\CTokenizer.pas',
  IsppBase in 'Src\ISPP\IsppBase.pas',
  PathFunc in '..\Components\PathFunc.pas',
  CmnFunc2 in 'Src\CmnFunc2.pas',
  FileClass in 'Src\FileClass.pas',
  Int64Em in 'Src\Int64Em.pas',
  MD5 in '..\Components\MD5.pas',
  SHA1 in '..\Components\SHA1.pas',
  Struct in 'Src\Struct.pas';
  
{$IMAGEBASE $01800000}
{$SETPEOSVERSION 6.1}
{$SETPESUBSYSVERSION 6.1}
{$WEAKLINKRTTI ON}

{$R Res\ISPP.version.res}

exports
  ISPreprocessScript name 'ISPreprocessScriptW';

end.
