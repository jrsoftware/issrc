unit Compiler.StringLists.Test;

{
  Inno Setup
  Copyright (C) 1997-2026 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Test unit for Compiler.StringLists

  Runs a self-test if DEBUG is defined
}

interface

procedure CompilerStringListsRunTests;

implementation

uses
  {$IFDEF DEBUG} Winapi.Windows, {$ENDIF} System.SysUtils, System.Classes, Compiler.StringLists;

{$C+}

procedure CompilerStringListsRunTests;
begin
  var List := THashStringList.Create;
  try
    { List starts empty, lookups fail }
    Assert(List.Count = 0);
    Assert(List.CaseInsensitiveIndexOf('anything') = -1);

    { Add returns the new index, Get/[] return the string }
    Assert(List.Add('Hello') = 0);
    Assert(List.Add('World') = 1);
    Assert(List.Count = 2);
    Assert(List[0] = 'Hello');
    Assert(List[1] = 'World');

    { Case-insensitive lookup hits regardless of casing of the query }
    Assert(List.CaseInsensitiveIndexOf('hello') = 0);
    Assert(List.CaseInsensitiveIndexOf('HELLO') = 0);
    Assert(List.CaseInsensitiveIndexOf('world') = 1);
    Assert(List.CaseInsensitiveIndexOf('WoRlD') = 1);
    Assert(List.CaseInsensitiveIndexOf('missing') = -1);

    { Duplicates are allowed by default; each Add appends a new entry }
    Assert(List.Add('hello') = 2);
    Assert(List.Count = 3);
    Assert(List[2] = 'hello');
    { CaseInsensitiveIndexOf returns the first match }
    Assert(List.CaseInsensitiveIndexOf('hello') = 0);

    { Empty string is a valid entry }
    Assert(List.Add('') = 3);
    Assert(List[3] = '');
    Assert(List.CaseInsensitiveIndexOf('') = 3);

    {$IFDEF ISTESTTOOLPROJ}
    { Get out of bounds raises EStringListError }
    var Caught := False;
    try
      List.Get(-1);
    except
      on EStringListError do Caught := True;
    end;
    Assert(Caught);

    Caught := False;
    try
      List.Get(List.Count);
    except
      on EStringListError do Caught := True;
    end;
    Assert(Caught);

    { Default property routes through Get and raises the same exception }
    Caught := False;
    try
      const OutOfBoundsString = List[List.Count];
    except
      on EStringListError do Caught := True;
    end;
    Assert(Caught);
    {$ENDIF}

    { Clear empties the list and the next Add starts fresh }
    List.Clear;
    Assert(List.Count = 0);
    Assert(List.CaseInsensitiveIndexOf('hello') = -1);
    Assert(List.Add('after-clear') = 0);
    Assert(List.Count = 1);
    Assert(List[0] = 'after-clear');
  finally
    List.Free;
  end;

  { Growth: adding well past the initial capacity works and all
    entries are retrievable in order }
  List := THashStringList.Create;
  try
    const ItemCount = 200;
    for var I := 0 to ItemCount-1 do
      Assert(List.Add('item' + IntToStr(I)) = I);
    Assert(List.Count = ItemCount);
    for var I := 0 to ItemCount-1 do
      Assert(List[I] = 'item' + IntToStr(I));
    { CaseInsensitiveIndexOf still finds entries correctly after growth }
    Assert(List.CaseInsensitiveIndexOf('ITEM0') = 0);
    Assert(List.CaseInsensitiveIndexOf('item' + IntToStr(ItemCount-1)) = ItemCount-1);
  finally
    List.Free;
  end;

  { IgnoreDuplicates: a duplicate Add returns -1 and does not change Count }
  List := THashStringList.Create;
  try
    List.IgnoreDuplicates := True;
    Assert(List.Add('Foo') = 0);
    Assert(List.Add('Bar') = 1);
    Assert(List.Count = 2);

    { Exact duplicate }
    Assert(List.Add('Foo') = -1);
    Assert(List.Count = 2);

    { Case-insensitive duplicate }
    Assert(List.Add('FOO') = -1);
    Assert(List.Count = 2);

    { A non-duplicate still goes in }
    Assert(List.Add('Baz') = 2);
    Assert(List.Count = 3);
    Assert(List[2] = 'Baz');
  finally
    List.Free;
  end;

  { TScriptFileLines: stores filename, line number, and text per row; Text
    joins the rows with sLineBreak and omits a trailing break }
  var Lines := TScriptFileLines.Create;
  try
    Assert(Lines.Count = 0);
    Assert(Lines.Text = '');

    Lines.Add('a.iss', 1, 'first');
    Assert(Lines.Count = 1);
    Assert(Lines[0].LineFilename = 'a.iss');
    Assert(Lines[0].LineNumber = 1);
    Assert(Lines[0].LineText = 'first');
    { Single line: no trailing break }
    Assert(Lines.Text = 'first');

    Lines.Add('a.iss', 2, 'second');
    Assert(Lines.Count = 2);
    Assert(Lines.Text = 'first' + sLineBreak + 'second');
  finally
    Lines.Free;
  end;

  { LineFilename round-trips correctly across mixed sequences: consecutive
    same-name lines, name change, and same name reappearing }
  Lines := TScriptFileLines.Create;
  try
    Lines.Add('shared.iss', 1, 'a');
    Lines.Add('shared.iss', 2, 'b');
    Lines.Add('other.iss', 3, 'c');
    Lines.Add('shared.iss', 4, 'd');
    Assert(Lines[0].LineFilename = 'shared.iss');
    Assert(Lines[1].LineFilename = 'shared.iss');
    Assert(Lines[2].LineFilename = 'other.iss');
    Assert(Lines[3].LineFilename = 'shared.iss');
  finally
    Lines.Free;
  end;

  { Empty LineFilename round-trips and joining still works }
  Lines := TScriptFileLines.Create;
  try
    Lines.Add('', 1, 'a');
    Lines.Add('', 2, 'b');
    Assert(Lines[0].LineFilename = '');
    Assert(Lines[1].LineFilename = '');
    Assert(Lines.Text = 'a' + sLineBreak + 'b');
  finally
    Lines.Free;
  end;

  { Text edge cases: empty LineText is preserved; the sLineBreak separator
    is added between lines but not after the last }
  Lines := TScriptFileLines.Create;
  try
    Lines.Add('a.iss', 1, '');
    Lines.Add('a.iss', 2, '');
    { Two empty lines: a single sLineBreak between them, no trailing break }
    Assert(Lines.Text = sLineBreak);
  finally
    Lines.Free;
  end;
end;

{$IFDEF DEBUG}
initialization
  try
    CompilerStringListsRunTests;
  except on E: Exception do
    begin
      MessageBox(0, PChar(E.Message), '', MB_OK);
      raise;
    end;
  end;
{$ENDIF}

end.
