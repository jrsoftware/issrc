{
  Inno Setup Preprocessor
  Copyright (C) 2001-2002 Alex Yackimoff

  Inno Setup
  Copyright (C) 1997-2020 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.
}

unit ISPP.Consts;

interface

const

  { Common words/expressions }

  SLocal = 'LOCAL';
  SGlobal = 'GLOBAL';
  SAny = 'ANY';
  SInt = 'INT';
  SStr = 'STR';
  SIdent = 'Identifier';

  { Directive parsing }

  SByRefNoDefault = 'By-reference parameter cannot have a default value';
  SDirectiveCannotBeInline = 'Directive ''%s'' cannot be inline';
  SDoubleElse = '''else'' was already specified';
  SElifAfterElse = '''elif'' after ''else''';
  SElseWithoutIf = '''else'' without ''if''';
  SEndifExpected = '''endif'' expected';
  SEndifWithoutIf = '''endif'' without ''if''';
  SErrorWhileOpeningFile = 'Error %d while opening file %s';
  SErrorWhileReadingFile = 'Error %d while reading file %s';
  SFileDirectiveCanBeOnlyInline = '''file'' directive can be only inline';
  SFileIsAlreadyBeingIncluded = 'File %s is already being included';
  SFileNotFound = 'File not found: %s';
  SInsertLineNoTooBig = 'Line number %d is out of current translation';
  SInvalidOptionName = 'Invalid option name';
  SInvalidTypeId = 'Invalid type identifier: %s';
  SMacroExpressionExpected = 'Macro expression expected';
  SNonEmptyStringExpected = 'Non empty string expected';
  STooManyFormalParams = 'Too many formal parameters';
  SUnknownPreprocessorDirective = 'Unknown preprocessor directive';
  SUnterminatedPreprocessorDirectiv = 'Unterminated preprocessor directive';

  { Warnings }

  SDirectiveNotYetSupported = 'Directive ''%s'' not yet supported';
  SFailedToParsePragmaDirective = 'Failed to parse ''pragma'' directive';
  SFuncIdentForIfdef = 'Function identifier passed to ''ifdef''/''ifndef'' directive';
  SSpecFuncIdentForIfdef = 'Special function identifier passed to ''ifdef''/''ifndef'' directive, result is undefined';
  SSpecifiedConditionEvalatedToVoid = 'Specified condition evaluated to void, to test variable for existence use "defined" function';

  { Status messages }

  SIncludingFile = 'Including file: %s';
  SProcessingExternalFile = 'Processing file: %s';

  { Verbose messages }

  SChangedVerboseLevel = 'Changed level of verbosity: %d';
  SChangingInsertionPointToLine = 'Changing insertion point to line %d';
  SFinishedConditionalInclusion = 'Finished conditional inclusion (''endif'')';
  SLineEmitted = 'Emitted line: %s';
  SResettingInsertionPoint = 'Resetting insertion point';
  SStartingConditionalInclusionIf = 'Starting conditional inclusion (''if'')';
  STemporaryFileCreated = 'Created temporary file: %s';
  SUpdatingConditionalInclusionElif = 'Updating conditional inclusion (''elif'')';
  SUpdatingConditionalInclusionElse = 'Updating conditional inclusion (''else'')';

  { Ident manager errors }

  SAllocatingMacroLocalArrayUpToEle = 'Allocating macro %s Local array up to element %d';
  SArrayDeclared = '%s array declared: %s';
  SArrayReDimmed = '%s array redimensioned: %s';
  SErrorExecutingMacro = 'Error at %1:d:%d in macro %0:s:'#13#10#13#10'%3:s';
  SErrorExecutingMacroFile = 'Error in %1:s at %d:%d in macro %0:s:'#13#10#13#10'%4:s';
  SErrorExecutingMacroUnexpected = 'Error executing macro %s: %s';
  SFuncError = 'Unexpected error when calling function %s';
  SFuncsNoSupportNamedParams = 'Functions do not support named parameters';
  SIndexIsOutOfArraySize = 'Index %d is out of array %s size';
  SIndexNotSpecifiedForArray = 'Index not specified for array %s';
  SInsufficientParams = 'Insufficient parameters';
  SLocalArraysIndexError = 'Local arrays can have up to 16 elements (0 to 15)';
  SLValueRequiredForByRefParam = 'L-value required for by-reference parameter %s';
  SMacroDefined = '%s macro defined: %s';
  SParameterlessVariable = 'Variable does not expect any parameters';
  SParamSpecifiedTwice = 'Parameter %s specified twice';
  SRedeclaredIdentifier = 'Identifier redeclared: %s';
  SRequiredParamMissing = 'Required parameter %s missing';
  SSuccessfullyCalledFunction = 'Successfully called function %s';
  SSuccessfullyCalledMacro = 'Successfully called macro %s';
  STooManyActualParams = 'Too many actual parameters';
  SUndefined = '%s %s undefined: %s';
  SUnknownParam = 'Unknown parameter: %s';
  SVariableDefined = '%s variable defined: %s';
  SWrongParamType = 'Actual parameter %s is not of the declared type';
  SBadLength = 'Bad dimension';

  { Parser errors }

  SActualParamsNamingConflict = 'All actual parameters must appear either in order of their declaration, or by names';
  SCannotConvertToInteger = 'Cannot convert "%s" to integer';
  SExpectedButFound = '%s expected but %s found';
  SIllegalChar = 'Illegal character in input file: ''%s'' (0x%-2x)';
  SIntegerExpressionExpected = 'Integer expression expected';
  SLValueRequired = 'Left side cannot be assigned to (expression is not an l-value)';
  SOperatorNotApplicableToThisOpera = 'Operator not applicable to this operand type';
  SStringExpressionExpected = 'String expression expected';
  SUndeclaredIdentifier = 'Undeclared identifier: %s';
  SWrongUnaryOperator = 'Wrong unary operator';

implementation

end.
