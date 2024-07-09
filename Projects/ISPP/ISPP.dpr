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
  CompPreprocInt in '..\Src\ISCmplr\CompPreprocInt.pas',
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
  CmnFunc2 in '..\Src\CmnFunc2.pas',
  FileClass in '..\Src\FileClass.pas',
  Int64Em in '..\Src\Int64Em.pas',
  MD5 in '..\Src\MD5.pas',
  SHA1 in '..\Src\SHA1.pas',
  Struct in '..\Src\Struct.pas';
  
{$IMAGEBASE $01800000}
{$SETPEOSVERSION 6.1}
{$SETPESUBSYSVERSION 6.1}
{$WEAKLINKRTTI ON}

{$R ISPP.version.res}

exports
  ISPreprocessScript name 'ISPreprocessScriptW';

end.
