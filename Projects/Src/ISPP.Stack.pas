{
  Inno Setup Preprocessor
  Copyright (C) 2001-2002 Alex Yackimoff
  
  Inno Setup
  Copyright (C) 1997-2025 Jordan Russell
  Portions by Martijn Laan
  For conditions of distribution and use, see LICENSE.TXT.
}

unit ISPP.Stack;

interface

uses
  Classes, Generics.Collections;

type

{ Borrowed from Delphi 5 Contnrs unit }

{ TOrdered class }

  TOrderedList<T> = class(TObject)
  private
    FList: TList<T>;
  protected
    procedure PushItem(AItem: T); virtual; abstract;
    function PopItem: T; virtual;
    function PeekItem: T; virtual;
    property List: TList<T> read FList;
  public
    constructor Create;
    destructor Destroy; override;

    function Count: NativeInt;
    function AtLeast(ACount: NativeInt): Boolean;
    procedure Push(AItem: T);
    function Pop: T;
    function Peek: T;
  end;

{ TStack class }

  TStack<T> = class(TOrderedList<T>)
  protected
    procedure PushItem(AItem: T); override;
  end;


implementation

uses
  ISPP.Consts;

{ TOrderedList }

function TOrderedList<T>.AtLeast(ACount: NativeInt): boolean;
begin
  Result := List.Count >= ACount;
end;

function TOrderedList<T>.Peek: T;
begin
  Result := PeekItem;
end;

function TOrderedList<T>.Pop: T;
begin
  Result := PopItem;
end;

procedure TOrderedList<T>.Push(AItem: T);
begin
  PushItem(AItem);
end;

function TOrderedList<T>.Count: NativeInt;
begin
  Result := List.Count;
end;

constructor TOrderedList<T>.Create;
begin
  inherited Create;
  FList := TList<T>.Create;
end;

destructor TOrderedList<T>.Destroy;
begin
  List.Free;
  inherited Destroy;
end;

function TOrderedList<T>.PeekItem: T;
begin
  Result := List[List.Count-1];
end;

function TOrderedList<T>.PopItem: T;
begin
  Result := PeekItem;
  List.Delete(List.Count-1);
end;

{ TStack }

procedure TStack<T>.PushItem(AItem: T);
begin
  List.Add(AItem);
end;


end.

