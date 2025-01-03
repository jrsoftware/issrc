unit UIsxclassesParser;

interface

uses
  Classes;

type
  TIsxclassesParserStoredString = (ssLine, ssType, ssEnumValue, ssConstant, ssMemberName, ssMember, ssProperty);
  TIsxclassesParserStrings = array [TIsxclassesParserStoredString] of TStringList;

  TIsxclassesParser = class
  private
    FStrings: TIsxclassesParserStrings;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Parse(const FileName: String);
    procedure SaveXML(const HeaderFileName, HeaderFileName2, FooterFileName, OutputFileName: String);
    procedure SaveWordLists(const OutputFileName: String);
  end;

implementation

uses
  Windows, SysUtils,
  PathFunc;

constructor TIsxclassesParser.Create;
begin
  inherited;
  for var I := Low(TIsxClassesParserStoredString) to High(TIsxClassesParserStoredString) do
    FStrings[I] := TStringList.Create;
  { Sorted for speed of IndexOf used below }
  FStrings[ssType].Duplicates := dupError;
  FStrings[ssType].Sorted := True;
  { Sorted for sanity checking of duplicates }
  FStrings[ssEnumValue].Duplicates := dupError;
  FStrings[ssEnumValue].Sorted := True;
  FStrings[ssConstant].Duplicates := dupError;
  FStrings[ssConstant].Sorted := True;
  { Sorted for ignoring duplicates }
  FStrings[ssMemberName].Duplicates := dupIgnore;
  FStrings[ssMemberName].Sorted := True;
  FStrings[ssMember].Duplicates := dupIgnore;
  FStrings[ssMember].Sorted := True;
  FStrings[ssProperty].Duplicates := dupIgnore;
  FStrings[ssProperty].Sorted := True;
end;

destructor TIsxclassesParser.Destroy;
begin
  for var I := Low(TIsxClassesParserStoredString) to High(TIsxClassesParserStoredString) do
    FStrings[I].Free;
  inherited;
end;

procedure TIsxclassesParser.Parse(const FileName: String);

  { Also presents in ScriptFunc.pas - changed from AnsiString to String + check for [ added }
  function ExtractScriptFuncWithoutHeaderName(const ScriptFuncWithoutHeader: String): String;
  begin
    Result := ScriptFuncWithoutHeader;

    const C0: String = '[';
    const C1: String = '(';
    const C2: String = ':';
    const C3: String = ';';

    var P := Pos(C0, Result);
    if P = 0 then
      P := Pos(C1, Result);
    if P = 0 then
      P := Pos(C2, Result);
    if P = 0 then
      P := Pos(C3, Result);
    if P = 0 then
      raise Exception.CreateFmt('Invalid ScriptFuncWithoutHeader: %s', [Result]);

    Delete(Result, P, Maxint);
  end;

begin
  var F: TextFile;
  AssignFile(F, FileName);
  Reset(F);
  try
    while not Eof(F) do begin
      var S: String;
      ReadLn(F, S);
      FStrings[ssLine].Add(S);

      var P := Pos('=', S);
      if P > 1 then begin
        { Remember type and if it's an enum also remember the enum values }
        FStrings[ssType].Add(Trim(Copy(S, 1, P-1)));
        Delete(S, 1, P+1);
        var N := Length(S);
        if (N > 3) and (S[1] = '(') and (S[N-1] = ')') and (S[N] = ';') then
          FStrings[ssEnumValue].Add(Copy(S, 2, N-3));
        Continue;
      end;

      P := Pos('{', S);
      if P <> 0 then begin
        { Remember constants }
        P := Pos(': ', S);
        if P <> 0 then begin
          Delete(S, 1, P+1);
          var N := Length(S);
          if (N > 2) and (S[N-1] = ' ') and (S[N] = '}') then
            FStrings[ssConstant].Add(Copy(S, 1, N-2));
        end;
        Continue;
      end;

      var Typ := ssMemberName;
      P := Pos('procedure ', S);
      if P = 0 then
        P := Pos('function ', S);
      if P = 0 then begin
        Typ := ssProperty;
        P := Pos('property ', S);
      end;
      if P <> 0 then begin
        if Typ = ssMemberName then
          FStrings[ssMember].Add(StringReplace(S.TrimLeft, 'const ', '', [rfReplaceAll]));
        Delete(S, 1, P-1);
        P := Pos(' ', S);
        Delete(S, 1, P);
        FStrings[Typ].Add(ExtractScriptFuncWithoutHeaderName(S));
        Continue;
      end;
    end;
  finally
    CloseFile(F);
  end;
end;

procedure TIsxclassesParser.SaveXML(const HeaderFileName, HeaderFileName2, FooterFileName, OutputFileName: String);

  procedure FCopyFile(const SourceFileName, DestFileName: String; AppendToDestFile: Boolean);
  begin
    var F1: TextFile;
    AssignFile(F1, SourceFileName);
    Reset(F1);
    try
      var F2: TextFile;
      AssignFile(F2, DestFileName);
      if AppendToDestFile then begin
        if FileExists(DestFileName) then
          Append(F2)
        else
          Reset(F2);
      end else
        Rewrite(F2);
      try
        while not Eof(F1) do begin
          var S: String;
          ReadLn(F1, S);
          WriteLn(F2, S);
        end;
      finally
        CloseFile(F2);
      end;
    finally
      CloseFile(F1);
    end;
  end;

  function FGetNextPart(var Text: PChar): String;
  begin
    case Text^ of
      #0:
        begin
          Result := '';
        end;
      #1..#32:
        begin
          var P := Text;
          Inc(Text);
          while CharInSet(Text^ , [#1..#32]) do
            Inc(Text);
          SetString(Result, P, Text - P);
        end;
      '(', ')', ',', '=', ':', ';', '[', ']', '{', '}':
        begin
          Result := Text^;
          Inc(Text);
        end;
      '0'..'9', 'A'..'Z', 'a'..'z', '_', '.':
        begin
          var P := Text;
          Inc(Text);
          while CharInSet(Text^ , ['0'..'9', 'A'..'Z', 'a'..'z', '_', '.']) do
            Inc(Text);
          SetString(Result, P, Text - P);
        end;
      else
        raise Exception.CreateFmt('Invalid symbol ''%s'' found', [Text^]);
    end;
  end;

  function FLinkTypes(const S: String): String;
  begin
    Result := '';
    var Text := PChar(S);

    var NextPart := FGetNextPart(Text);
    while NextPart <> '' do begin
      if FStrings[ssType].IndexOf(NextPart) >= 0 then begin
        if Result = '' then //start of line = object definition
          NextPart := '<a name="' + NextPart + '">' + NextPart + '</a>'
        else
          NextPart := '<anchorlink name="' + NextPart + '">' + NextPart + '</anchorlink>';
      end;
      Result := Result + NextPart;
      NextPart := FGetNextPart(Text);
    end;
  end;

  function FConvertLeadingSpacesToNbsp(const S: String): String;
  begin
    Result := S;
    var I := 1;
    while (I <= Length(Result)) and (Result[I] = ' ') do begin
      Delete(Result, I, 1);
      Insert('&nbsp;', Result, I);
      Inc(I, Length('&nbsp;'));
    end;
  end;

begin
  FCopyFile(HeaderFileName, OutputFileName, False);

  var F: TextFile;
  AssignFile(F, OutputFileName);
  Append(F);
  try
    for var Typ in [ssType, ssEnumValue, ssConstant, ssMemberName, ssProperty] do begin
      for var S in FStrings[Typ] do begin
        var A := S.Split([', ']);
        for var S2 in A do begin
          if Typ = ssType then
            WriteLn(F, '<keyword value="' + S2 + '" anchor="' + S2 + '" />')
          else
            WriteLn(F, '<keyword value="' + S2 + '" />')
        end;
      end;
    end;
    WriteLn(F, '<keyword value="WizardForm" />');
    WriteLn(F, '<keyword value="UninstallProgressForm" />');
  finally
    CloseFile(F);
  end;

  FCopyFile(HeaderFileName2, OutputFileName, True);

  AssignFile(F, OutputFileName);
  Append(F);
  try
    WriteLn(F, '<p><br/><tt>');
    for var Line in FStrings[ssLine] do begin
      var S := FLinkTypes(Line);
      S := FConvertLeadingSpacesToNbsp(S);
      WriteLn(F, S, '<br/>');
    end;
    WriteLn(F, '</tt></p>');
  finally
    CloseFile(F);
  end;

  FCopyFile(FooterFileName, OutputFileName, True);
end;

procedure TIsxclassesParser.SaveWordLists(const OutputFileName: String);

  procedure WriteStringArray(const F: TextFile; const Name, Indent: String;
    const Values: TStrings; const NewLineLength: Integer;
    const AddQuotesAroundCommas: Boolean = True;
    const ArrayType: String = 'array of AnsiString');
  begin
    WriteLn(F, Indent + Name + ': ' + ArrayType + ' = [');
    var S: String;
    for var I := 0 to Values.Count-1 do begin
      if S <> '' then
        S := S + ', ';
      var V := Values[I];
      if AddQuotesAroundCommas then begin
        V := StringReplace(V, ', ', ',', [rfReplaceAll]);
        V := StringReplace(V, ',', ''', ''', [rfReplaceAll]);
      end;
      S := S + '''' + V + '''';
      if Length(S) > NewLineLength then begin
        if I <> Values.Count-1 then
          S := S + ',';
        WriteLn(F, Indent + Indent + S);
        S := '';
      end;
    end;
    if S <> '' then
      WriteLn(F, Indent + Indent + S);
    WriteLn(F, Indent + '];');
  end;

begin
  var F: TextFile;
  AssignFile(F, OutputFileName);
  Rewrite(F);
  try
    const Indent = '  ';
    WriteLn(F, 'unit ' + PathChangeExt(PathExtractName(OutputFileName), '') + ';');
    WriteLn(F);
    WriteLn(F, '{ This file is automatically generated by ISHelpGen. Do not edit. }');
    WriteLn(F);
    WriteLn(F, 'interface');
    WriteLn(F);
    WriteLn(F, 'uses');
    WriteLn(F, Indent + 'Shared.ScriptFunc;');
    WriteLn(F);
    WriteLn(F, 'var');
    WriteStringArray(F, 'PascalConstants_Isxclasses', Indent, FStrings[ssConstant], 0);
    WriteLn(F);
    WriteStringArray(F, 'PascalTypes_Isxclasses', Indent, FStrings[ssType], 80);
    WriteLn(F);
    WriteStringArray(F, 'PascalEnumValues_Isxclasses', Indent, FStrings[ssEnumValue], 0);
    WriteLn(F);
    WriteStringArray(F, 'PascalMembers_Isxclasses', Indent, FStrings[ssMember], 0, False, 'TScriptTable');
    WriteLn(F);
    WriteStringArray(F, 'PascalProperties_Isxclasses', Indent, FStrings[ssProperty], 80);
    WriteLn(F);
    WriteLN(F, 'implementation');
    WriteLn(F);
    Write(F, 'end.');
  finally
    CloseFile(F);
  end;
end;


end.
