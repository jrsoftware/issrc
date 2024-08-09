unit Compiler.StringLists;

{
  Inno Setup
  Copyright (C) 1997-2024 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.

  Special string list classes used by TSetupCompiler and TCompressionHandler
}

interface

uses
  Classes;

type
  TLowFragList = class(TList)
  protected
    procedure Grow; override;
  end;

  TLowFragStringList = class
  private
    FInternalList: TLowFragList;
    function Get(Index: Integer): String;
    function GetCount: Integer;
    procedure Put(Index: Integer; const Value: String);
  public
    constructor Create;
    destructor Destroy; override;
    function Add(const S: String): Integer;
    procedure Clear;
    property Count: Integer read GetCount;
    property Strings[Index: Integer]: String read Get write Put; default;
  end;

  THashStringItem = record
    Hash: Longint;
    Str: String;
  end;

const
  MaxHashStringItemListSize = MaxInt div 16;

type
  PHashStringItemList = ^THashStringItemList;
  THashStringItemList = array[0..MaxHashStringItemListSize-1] of THashStringItem;
  THashStringList = class
  private
    FCapacity: Integer;
    FCount: Integer;
    FIgnoreDuplicates: Boolean;
    FList: PHashStringItemList;
    procedure Grow;
  public
    destructor Destroy; override;
    function Add(const S: String): Integer;
    function CaseInsensitiveIndexOf(const S: String): Integer;
    procedure Clear;
    function Get(Index: Integer): String;
    property Count: Integer read FCount;
    property IgnoreDuplicates: Boolean read FIgnoreDuplicates write FIgnoreDuplicates;
    property Strings[Index: Integer]: String read Get; default;
  end;

  PScriptFileLine = ^TScriptFileLine;
  TScriptFileLine = record
    LineFilename: String;
    LineNumber: Integer;
    LineText: String;
  end;

  TScriptFileLines = class
  private
    FLines: TLowFragList;
    function Get(Index: Integer): PScriptFileLine;
    function GetCount: Integer;
    function GetText: String;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const LineFilename: String; const LineNumber: Integer;
      const LineText: String);
    property Count: Integer read GetCount;
    property Lines[Index: Integer]: PScriptFileLine read Get; default;
    property Text: String read GetText;
  end;

implementation

uses
  PathFunc, Compression.Base;

{ TLowFragList }

procedure TLowFragList.Grow;
var
  Delta: Integer;
begin
  { Delphi 2's TList.Grow induces memory fragmentation big time. This is the
    Grow code from Delphi 3 and later. }
  if Capacity > 64 then Delta := Capacity div 4 else
    if Capacity > 8 then Delta := 16 else
      Delta := 4;
  SetCapacity(Capacity + Delta);
end;

{ TLowFragStringList }

constructor TLowFragStringList.Create;
begin
  inherited;
  FInternalList := TLowFragList.Create;
end;

destructor TLowFragStringList.Destroy;
begin
  if Assigned(FInternalList) then begin
    Clear;
    FInternalList.Free;
  end;
  inherited;
end;

function TLowFragStringList.Add(const S: String): Integer;
var
  P: Pointer;
begin
  FInternalList.Expand;
  P := nil;
  String(P) := S;  { bump the ref count }
  Result := FInternalList.Add(P);
end;

procedure TLowFragStringList.Clear;
begin
  if FInternalList.Count <> 0 then
    Finalize(String(FInternalList.List[0]), FInternalList.Count);
  FInternalList.Clear;
end;

function TLowFragStringList.Get(Index: Integer): String;
begin
  Result := String(FInternalList[Index]);
end;

function TLowFragStringList.GetCount: Integer;
begin
  Result := FInternalList.Count;
end;

procedure TLowFragStringList.Put(Index: Integer; const Value: String);
begin
  if (Index < 0) or (Index >= FInternalList.Count) then
    raise EListError.CreateFmt('List index out of bounds (%d)', [Index]);
  String(FInternalList.List[Index]) := Value;
end;

{ THashStringList }

destructor THashStringList.Destroy;
begin
  Clear;
  inherited;
end;

function THashStringList.Add(const S: String): Integer;
var
  LS: String;
begin
  if FIgnoreDuplicates and (CaseInsensitiveIndexOf(S) <> -1) then begin
    Result := -1;
    Exit;
  end;

  Result := FCount;
  if Result = FCapacity then
    Grow;
  LS := PathLowercase(S);
  Pointer(FList[Result].Str) := nil;  { since Grow doesn't zero init }
  FList[Result].Str := S;
  FList[Result].Hash := GetCRC32(Pointer(LS)^, Length(LS)*SizeOf(LS[1]));
  Inc(FCount);
end;

procedure THashStringList.Clear;
begin
  if FCount > 0 then
    Finalize(FList[0], FCount);
  FCount := 0;
  FCapacity := 0;
  ReallocMem(FList, 0);
end;

function THashStringList.Get(Index: Integer): String;
begin
  if (Index < 0) or (Index >= FCount) then
    raise EStringListError.CreateFmt('THashStringList: Index %d is out of bounds',
      [Index]);
  Result := FList[Index].Str;
end;

procedure THashStringList.Grow;
var
  Delta, NewCapacity: Integer;
begin
  if FCapacity > 64 then Delta := FCapacity div 4 else
    if FCapacity > 8 then Delta := 16 else
      Delta := 4;
  NewCapacity := FCapacity + Delta;
  if NewCapacity > MaxHashStringItemListSize then
    raise EStringListError.Create('THashStringList: Exceeded maximum list size');
  ReallocMem(FList, NewCapacity * SizeOf(FList[0]));
  FCapacity := NewCapacity;
end;

function THashStringList.CaseInsensitiveIndexOf(const S: String): Integer;
var
  LS: String;
  Hash: Longint;
  I: Integer;
begin
  LS := PathLowercase(S);
  Hash := GetCRC32(Pointer(LS)^, Length(LS)*SizeOf(LS[1]));
  for I := 0 to FCount-1 do
    if (FList[I].Hash = Hash) and (PathLowercase(FList[I].Str) = LS) then begin
      Result := I;
      Exit;
    end;
  Result := -1;
end;

{ TScriptFileLines }

constructor TScriptFileLines.Create;
begin
  inherited;
  FLines := TLowFragList.Create;
end;

destructor TScriptFileLines.Destroy;
var
  I: Integer;
begin
  if Assigned(FLines) then begin
    for I := FLines.Count-1 downto 0 do
      Dispose(PScriptFileLine(FLines[I]));
    FLines.Free;
  end;
  inherited;
end;

procedure TScriptFileLines.Add(const LineFilename: String;
  const LineNumber: Integer; const LineText: String);
var
  L, PrevLine: PScriptFileLine;
begin
  FLines.Expand;
  New(L);
  try
    { Memory usage optimization: If LineFilename is equal to the previous
      line's LineFilename, then make this line's LineFilename reference the
      same string (i.e. just increment its refcount). }
    PrevLine := nil;
    if (LineFilename <> '') and (FLines.Count > 0) then
      PrevLine := PScriptFileLine(FLines[FLines.Count-1]);
    if Assigned(PrevLine) and (PrevLine.LineFilename = LineFilename) then
      L.LineFilename := PrevLine.LineFilename
    else
      L.LineFilename := LineFilename;
    L.LineNumber := LineNumber;
    L.LineText := LineText;
  except
    Dispose(L);
    raise;
  end;
  FLines.Add(L);
end;

function TScriptFileLines.Get(Index: Integer): PScriptFileLine;
begin
  Result := PScriptFileLine(FLines[Index]);
end;

function TScriptFileLines.GetCount: Integer;
begin
  Result := FLines.Count;
end;

function TScriptFileLines.GetText: String;
var
  I, L, Size, Count: Integer;
  P: PChar;
  S, LB: string;
begin
  Count := GetCount;
  Size := 0;
  LB := sLineBreak;
  for I := 0 to Count-1 do
    Inc(Size, Length(Get(I).LineText) + Length(LB));
  Dec(Size, Length(LB));
  SetString(Result, nil, Size);
  P := Pointer(Result);
  for I := 0 to Count-1 do begin
    S := Get(I).LineText;
    L := Length(S);
    if L <> 0 then begin
      System.Move(Pointer(S)^, P^, L * SizeOf(Char));
      Inc(P, L);
    end;
    if I < Count-1 then begin
      L := Length(LB);
      if L <> 0 then begin
        System.Move(Pointer(LB)^, P^, L * SizeOf(Char));
        Inc(P, L);
      end;
    end;
  end;
end;

end.