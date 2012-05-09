{
  Inno Setup Preprocessor
  Copyright (C) 2001-2002 Alex Yackimoff
  $Id: ISPP.dpr,v 1.7 2010/12/30 14:58:38 mlaan Exp $
}

library ISPP;

{$IMAGEBASE $01800000}
{$I Version.inc}
{$R *.RES}

uses
  SysUtils,
  //IsppDebug in 'IsppDebug.pas',
  Windows,
  Classes,
  CompPreprocInt in 'CompPreprocInt.pas',
  IsppPreprocess in 'IsppPreprocess.pas',
  IsppTranslate in 'IsppTranslate.pas',
  IsppFuncs in 'IsppFuncs.pas',
  IsppVarUtils in 'IsppVarUtils.pas',
  IsppConsts in 'IsppConsts.pas',
  IsppStacks in 'IsppStacks.pas',
  IsppIntf in 'IsppIntf.pas',
  IsppParser in 'IsppParser.pas',
  IsppIdentMan in 'IsppIdentMan.pas',
  IsppSessions in 'IsppSessions.pas',
  CParser in 'CParser.pas',
  IsppBase in 'IsppBase.pas',
  PathFunc in 'PathFunc.pas',
  CmnFunc2 in 'CmnFunc2.pas';
  //IsppExceptWindow in 'IsppExceptWindow.pas' {IsppExceptWnd};

const
  FuncNameSuffix = {$IFDEF UNICODE} 'W' {$ELSE} 'A' {$ENDIF};
exports
  ISPreprocessScript name 'ISPreprocessScript' + FuncNameSuffix;

end.
