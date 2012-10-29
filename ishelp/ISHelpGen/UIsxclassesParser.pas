unit UIsxclassesParser;

interface

uses
  Classes;

type
  TIsxclassesParser = class
  private
    FLines: TStringList;
    FTypes: TStringList;
  public
    constructor Create();
    destructor Destroy(); override;
    procedure Parse(const FileName: String);
    procedure SaveXML(const HeaderFileName, HeaderFileName2, OutputFileName: String);
  end;

implementation

uses
  Windows,
  SysUtils;

{$IFNDEF UNICODE}
function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean;
begin
  Result := C in CharSet;
end;
{$ENDIF}

constructor TIsxclassesParser.Create();
begin
  inherited;
  FLines := TStringList.Create();
  FTypes := TStringList.Create();
end;

destructor TIsxclassesParser.Destroy();
begin
  FTypes.Free();
  FLines.Free();
  inherited;
end;

procedure TIsxclassesParser.Parse(const FileName: String);
var
  F: TextFile;
  S: String;
  P: Integer;
begin
  AssignFile(F, FileName);
  Reset(F);
  try
    while not Eof(F) do begin
      ReadLn(F, S);
      FLines.Add(S);
      P := Pos('=', S);
      if P > 1 then
        FTypes.Add(Trim(Copy(S, 1, P-1)))
    end;
  finally
    CloseFile(F);
  end;
end;

procedure TIsxclassesParser.SaveXML(const HeaderFileName, HeaderFileName2, OutputFileName: String);

  procedure FCopyFile(const SourceFileName, DestFileName: String; AppendToDestFile: Boolean);
  var
    F1, F2: TextFile;
    S: String;
  begin
    AssignFile(F1, SourceFileName);
    Reset(F1);
    try
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
  var
    P: PChar;
  begin
    case Text^ of
      #0:
        begin
          Result := '';
        end;
      #1..#32:
        begin
          P := Text;
          Inc(Text);
          while CharInSet(Text^ , [#1..#32]) do
            Inc(Text);
          SetString(Result, P, Text - P);
        end;
      '(', ')', ',', '=', ':', ';', '[', ']':
        begin
          Result := Text^;
          Inc(Text);
        end;
      'A'..'Z', 'a'..'z', '_':
        begin
          P := Text;
          Inc(Text);
          while CharInSet(Text^ , ['0'..'9', 'A'..'Z', 'a'..'z', '_']) do
            Inc(Text);
          SetString(Result, P, Text - P);
        end;
      else
        raise Exception.CreateFmt('Invalid symbol ''%s'' found', [Text^]);
    end;
  end;

  function FLinkTypes(const S: String): String;
  var
    Text: PChar;
    NextPart: String;
  begin
    Result := '';
    Text := PChar(S);

    NextPart := FGetNextPart(Text);
    while NextPart <> '' do begin
      if FTypes.IndexOf(NextPart) >= 0 then begin
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
  var
    I: Integer;
  begin
    Result := S;
    I := 1;
    while (I <= Length(Result)) and (Result[I] = ' ') do begin
      Delete(Result, I, 1);
      Insert('&nbsp;', Result, I);
      Inc(I, Length('&nbsp;'));
    end;
  end;

var
  F: TextFile;
  I: Integer;
  S: String;
begin
  FCopyFile(HeaderFileName, OutputFileName, False);

  AssignFile(F, OutputFileName);
  Append(F);
  try
    for I := 0 to FTypes.Count-1 do begin
      S := '<keyword value="' + FTypes[I] + '" anchor="' + FTypes[I] + '" />';
      WriteLn(F, S);
    end;
    WriteLn(F, '<keyword value="MainForm" />');
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
    for I := 0 to FLines.Count-1 do begin
      S := FLinkTypes(FLines[I]);
      S := FConvertLeadingSpacesToNbsp(S);
      WriteLn(F, S, '<br/>');
    end;
    WriteLn(F, '</tt></p>');
    WriteLn(F, '</body>');
    WriteLn(F, '</topic>');
    WriteLn(F, '</ishelp>');
  finally
    CloseFile(F);
  end;
end;

end.
