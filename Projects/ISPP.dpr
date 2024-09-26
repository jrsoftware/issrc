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
  Shared.PreprocInt in 'Src\Shared.PreprocInt.pas',
  ISPP.Preprocess in 'Src\ISPP.Preprocess.pas',
  ISPP.Preprocessor in 'Src\ISPP.Preprocessor.pas',
  ISPP.Funcs in 'Src\ISPP.Funcs.pas',
  ISPP.VarUtils in 'Src\ISPP.VarUtils.pas',
  ISPP.Consts in 'Src\ISPP.Consts.pas',
  ISPP.Stack in 'Src\ISPP.Stack.pas',
  ISPP.Intf in 'Src\ISPP.Intf.pas',
  ISPP.Parser in 'Src\ISPP.Parser.pas',
  ISPP.IdentMan in 'Src\ISPP.IdentMan.pas',
  ISPP.Sessions in 'Src\ISPP.Sessions.pas',
  ISPP.CTokenizer in 'Src\ISPP.CTokenizer.pas',
  ISPP.Base in 'Src\ISPP.Base.pas',
  PathFunc in '..\Components\PathFunc.pas',
  Shared.CommonFunc in 'Src\Shared.CommonFunc.pas',
  Shared.FileClass in 'Src\Shared.FileClass.pas',
  Shared.Int64Em in 'Src\Shared.Int64Em.pas',
  MD5 in '..\Components\MD5.pas',
  SHA1 in '..\Components\SHA1.pas',
  SHA256 in '..\Components\SHA256.pas',
  Shared.Struct in 'Src\Shared.Struct.pas';
  
{$IMAGEBASE $01800000}
{$SETPEOSVERSION 6.1}
{$SETPESUBSYSVERSION 6.1}
{$WEAKLINKRTTI ON}

{$R Res\ISPP.version.res}

exports
  ISPreprocessScript name 'ISPreprocessScriptW';

end.
