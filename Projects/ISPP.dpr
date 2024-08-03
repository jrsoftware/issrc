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
  ISPP.Preprocess in 'Src\ISPP\ISPP.Preprocess.pas',
  ISPP.Preprocessor in 'Src\ISPP\ISPP.Preprocessor.pas',
  ISPP.Funcs in 'Src\ISPP\ISPP.Funcs.pas',
  ISPP.VarUtils in 'Src\ISPP\ISPP.VarUtils.pas',
  ISPP.Consts in 'Src\ISPP\ISPP.Consts.pas',
  ISPP.Stack in 'Src\ISPP\ISPP.Stack.pas',
  ISPP.Intf in 'Src\ISPP\ISPP.Intf.pas',
  ISPP.Parser in 'Src\ISPP\ISPP.Parser.pas',
  ISPP.IdentMan in 'Src\ISPP\ISPP.IdentMan.pas',
  ISPP.Sessions in 'Src\ISPP\ISPP.Sessions.pas',
  ISPP.CTokenizer in 'Src\ISPP\ISPP.CTokenizer.pas',
  ISPP.Base in 'Src\ISPP\ISPP.Base.pas',
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
