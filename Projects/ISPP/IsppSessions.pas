{
  Inno Setup Preprocessor
  Copyright (C) 2001-2002 Alex Yackimoff
}

unit IsppSessions;

{$IFDEF VER200}
  {$DEFINE DELPHI2009}
{$ENDIF}

interface

uses IsppTranslate;

procedure PushPreproc(APreproc: TPreprocessor);
function PopPreproc: TPreprocessor;
function PeekPreproc: TPreprocessor;
procedure Warning(const Msg: string; const Args: array of const);
procedure VerboseMsg(Level: Byte; const Msg: string; const Args: array of const);
procedure QueueFileForDeletion(const FileName: string);

implementation

uses SysUtils, Classes, IsppStacks {$IFDEF DELPHI2009}, Windows{$ENDIF};

procedure Warning(const Msg: string; const Args: array of const);
var
  P: TPreprocessor;
begin
  P := PeekPreproc;
  if Assigned(P) then
    P.Warning(Msg, Args)
end;

procedure VerboseMsg(Level: Byte; const Msg: string; const Args: array of const);
var
  P: TPreprocessor;
begin
  P := PeekPreproc;
  if Assigned(P) then
    P.VerboseMsg(Level, Msg, Args);
end;

{ TPreprocessorFlowStack }

type

  TPreprocessorFlowStack = class(TStack)
  private
    FReference: Pointer;
    FTempFiles: TStringList;
  public
    constructor Create(var Reference);
    destructor Destroy; override;
    procedure Push(APreproc: TPreprocessor);
    function Pop: TPreprocessor;
    function Peek: TPreprocessor;
    procedure QueueFile(const FileName: string);
  end;

constructor TPreprocessorFlowStack.Create(var Reference);
begin
  inherited Create;
  TPreprocessorFlowStack(Reference) := Self;
  FReference := @Reference
end;

destructor TPreprocessorFlowStack.Destroy;
var
  I: Integer;
begin
  TPreprocessorFlowStack(FReference^) := nil;
  if FTempFiles <> nil then
  begin
    for I := 0 to FTempFiles.Count - 1 do
      DeleteFile(PChar(FTempFiles[I]));
    FTempFiles.Free;
  end;
  inherited Destroy;
end;

function TPreprocessorFlowStack.Peek: TPreprocessor;
begin
  Result := TPreprocessor(inherited Peek);
end;

function TPreprocessorFlowStack.Pop: TPreprocessor;
begin
  Result := TPreprocessor(inherited Pop);
  if not AtLeast(1) then Free;
end;

procedure TPreprocessorFlowStack.Push(APreproc: TPreprocessor);
begin
  inherited Push(APreproc);
end;

procedure TPreprocessorFlowStack.QueueFile(const FileName: string);
begin
  if FTempFiles = nil then
  begin
    FTempFiles := TStringList.Create;
    FTempFiles.Duplicates := dupIgnore;
  end;
  FTempFiles.Add(FileName);
end;

var
  FlowStack: TPreprocessorFlowStack;

procedure PushPreproc(APreproc: TPreprocessor);
begin
  if FlowStack = nil then
    TPreprocessorFlowStack.Create(FlowStack);
  FlowStack.Push(APreproc)
end;

function PopPreproc: TPreprocessor;
begin
  if FlowStack <> nil then
    Result := FlowStack.Pop
  else
    Result := nil;
end;

function PeekPreproc: TPreprocessor;
begin
  if FlowStack <> nil then
    Result := FlowStack.Peek
  else
    Result := nil;
end;

procedure QueueFileForDeletion(const FileName: string);
begin
  if FlowStack <> nil then
    FlowStack.QueueFile(FileName)
  else
    DeleteFile(PChar(FileName));
end;

end.
