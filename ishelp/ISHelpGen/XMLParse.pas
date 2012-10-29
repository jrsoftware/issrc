unit XMLParse;
{ $jrsoftware: ishelp/ISHelpGen/XMLParse.pas,v 1.5 2009/07/29 09:50:24 mlaan Exp $ }

{ XML parser. Currently just calls MSXML 4.0 to do the real work. }

interface

{$IFNDEF VER80}  { if it's not Delphi 1.0 }
  {$IFNDEF VER90}  { if it's not Delphi 2.0 }
    {$IFNDEF VER93}  { and it's not C++Builder 1.0 }
      {$IFNDEF VER100}  { if it's not Delphi 3.0 }
        {$IFNDEF VER110}  { and it's not C++Builder 3.0 }
          {$IFNDEF VER120} {$IFNDEF VER125}  { if it's not Delphi 4 or C++Builder 4 }
            {$IFNDEF VER130}  { if it's not Delphi 5 or C++Builder 5 }
              {$DEFINE IS_D6}  { then it must be at least Delphi 6 or C++Builder 6 }
            {$ENDIF}
          {$ENDIF} {$ENDIF}
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

uses
  Windows, SysUtils{$IFDEF IS_D6}, Variants{$ENDIF};

type
  IXMLNode = interface
    function GetAttribute(const AName: String): String;
    function GetOptionalAttribute(const AName: String): String;
    function GetFirstChild: IXMLNode;
    function GetNodeName: String;
    function GetNextSibling: IXMLNode;
    function GetNodeType: Integer;
    function GetParentNode: IXMLNode;
    function GetPreviousSibling: IXMLNode;
    function GetRealMSXMLNode: OleVariant;
    function GetText: String;
    function HasAttribute(const AName: String): Boolean;
    function TransformNode(const Stylesheet: IXMLNode): String;
    property Attributes[const AName: String]: String read GetAttribute;
    property OptionalAttributes[const AName: String]: String read GetOptionalAttribute;
    property FirstChild: IXMLNode read GetFirstChild;
    property NextSibling: IXMLNode read GetNextSibling;
    property NodeName: String read GetNodeName;
    property NodeType: Integer read GetNodeType;
    property ParentNode: IXMLNode read GetParentNode;
    property PreviousSibling: IXMLNode read GetPreviousSibling;
    property Text: String read GetText;
  end;

  TXMLDocument = class
  private
    FDoc: OleVariant;
    function GetRoot: IXMLNode;
  public
    constructor Create;
    procedure LoadFromFile(const AFilename: String);
    procedure StripComments;
    property Root: IXMLNode read GetRoot;
  end;

const
  { Values for the NodeType property } 
  NODE_INVALID = 0;
  NODE_ELEMENT = 1;
  NODE_ATTRIBUTE = 2;
  NODE_TEXT = 3;
  NODE_CDATA_SECTION = 4;
  NODE_ENTITY_REFERENCE = 5;
  NODE_ENTITY = 6;
  NODE_PROCESSING_INSTRUCTION = 7;
  NODE_COMMENT = 8;
  NODE_DOCUMENT = 9;
  NODE_DOCUMENT_TYPE = 10;
  NODE_DOCUMENT_FRAGMENT = 11;
  NODE_NOTATION = 12;

implementation

uses
  ActiveX, ComObj;

type
  TXMLNode = class(TInterfacedObject, IXMLNode)
  private
    FRealNode: OleVariant;
    function GetFirstChild: IXMLNode;
    function GetAttribute(const AName: String): String;
    function GetOptionalAttribute(const AName: String): String;
    function GetNextSibling: IXMLNode;
    function GetNodeName: String;
    function GetNodeType: Integer;
    function GetParentNode: IXMLNode;
    function GetPreviousSibling: IXMLNode;
    function GetRealMSXMLNode: OleVariant;
    function GetText: String;
    function HasAttribute(const AName: String): Boolean;
    function TransformNode(const Stylesheet: IXMLNode): String;
  public
    constructor Create(const ARealNode: OleVariant);
  end;

function IsVarAssigned(const AVariant: OleVariant): Boolean;
begin
  case VarType(AVariant) of
    varEmpty: Result := False;
    varDispatch: Result := Assigned(TVarData(AVariant).VDispatch);
  else
    raise Exception.Create('IsVarAssigned: Unexpected variant type');
  end;
end;

function MakeNode(const ARealNode: OleVariant): IXMLNode;
begin
  if IsVarAssigned(ARealNode) then
    Result := TXMLNode.Create(ARealNode)
  else
    Result := nil;
end;

function WideCharToUTF8String(const Src: PWideChar; const SrcLen: Integer): AnsiString;
var
  DestLen, I: Integer;
  DestBuf: AnsiString;
  DestPtr: PAnsiChar;
  C: Word;
begin
  if (SrcLen < 0) or (SrcLen > High(Integer) div 3) then
    raise Exception.Create('WideCharToUTF8String: SrcLen out of range');
  SetString(DestBuf, nil, SrcLen * 3);
  DestLen := 0;
  DestPtr := PAnsiChar(DestBuf);
  for I := 0 to SrcLen-1 do begin
    C := Ord(Src[I]);
    if C <= $7F then begin
      DestPtr[DestLen] := AnsiChar(C);
      Inc(DestLen);
    end
    else if C <= $7FF then begin
      DestPtr[DestLen] := AnsiChar($C0 or (C shr 6));
      DestPtr[DestLen+1] := AnsiChar($80 or (C and $3F));
      Inc(DestLen, 2);
    end
    else begin
      if (C >= $D800) and (C <= $DFFF) then
        raise Exception.Create('WideCharToUTF8String: Surrogate pairs are not supported');
      DestPtr[DestLen] := AnsiChar($E0 or (C shr 12));
      DestPtr[DestLen+1] := AnsiChar($80 or ((C shr 6) and $3F));
      DestPtr[DestLen+2] := AnsiChar($80 or (C and $3F));
      Inc(DestLen, 3);
    end;
  end;
  SetLength(DestBuf, DestLen);
  Result := DestBuf;

  { Following is an alternate version which uses WideCharToMultiByte.
    Consistency across different Windows platforms is uncertain, so I prefer
    my version. }
  (*
  if SrcLen = 0 then
    Result := ''
  else begin
    DestLen := WideCharToMultiByte(CP_UTF8, 0, Src, SrcLen, nil, 0, nil, nil);
    if DestLen = 0 then
      RaiseLastWin32Error;
    SetString(DestBuf, nil, DestLen);
    if WideCharToMultiByte(CP_UTF8, 0, Src, SrcLen, @DestBuf[1], DestLen, nil, nil) <> DestLen then
      raise Exception.Create('VariantToUTF8String: Unexpected result from WideCharToMultiByte');
    Result := DestBuf;
  end;
  *)
end;

function VariantToUTF8String(const V: OleVariant): String;
begin
  if VarType(V) <> varOleStr then
    raise Exception.Create('VariantToUTF8String: Expected varOleStr');
  Result := WideCharToUTF8String(TVarData(V).VOleStr,
    SysStringLen(TVarData(V).VOleStr));
end;

{ TXMLDocument }

constructor TXMLDocument.Create;
begin
  inherited Create;
  FDoc := CreateOleObject('MSXML2.DOMDocument.4.0');
  FDoc.async := False;
  FDoc.preserveWhitespace := True;
end;

function TXMLDocument.GetRoot: IXMLNode;
begin
  Result := MakeNode(FDoc.documentElement);
end;

procedure TXMLDocument.LoadFromFile(const AFilename: String);
begin
  if not FDoc.load(AFilename) then begin
    if Integer(FDoc.parseError.line) <> 0 then
      raise Exception.CreateFmt('XML parse error (line %d, column %d): %s',
        [Integer(FDoc.parseError.line), Integer(FDoc.parseError.linepos),
         FDoc.parseError.reason])
    else
      raise Exception.CreateFmt('XML parse error: %s', [FDoc.parseError.reason]);
  end;
end;

procedure TXMLDocument.StripComments;
begin
  FDoc.selectNodes('//comment()').removeAll;
end;

{ TXMLNode }

constructor TXMLNode.Create(const ARealNode: OleVariant);
begin
  inherited Create;
  FRealNode := ARealNode;
end;

function TXMLNode.GetAttribute(const AName: String): String;
var
  N: OleVariant;
begin
  N := FRealNode.attributes.getNamedItem(AName);
  if not IsVarAssigned(N) then
    raise Exception.CreateFmt('Attribute "%s" does not exist', [AName]);
  Result := VariantToUTF8String(N.value);
end;

function TXMLNode.GetOptionalAttribute(const AName: String): String;
var
  N: OleVariant;
begin
  N := FRealNode.attributes.getNamedItem(AName);
  if not IsVarAssigned(N) then
    Result := ''
  else
    Result := VariantToUTF8String(N.value);
end;

function TXMLNode.GetFirstChild: IXMLNode;
begin
  Result := MakeNode(FRealNode.firstChild);
end;

function TXMLNode.GetNodeName: String;
begin
  Result := VariantToUTF8String(FRealNode.nodeName);
end;

function TXMLNode.GetNextSibling: IXMLNode;
begin
  Result := MakeNode(FRealNode.nextSibling);
end;

function TXMLNode.GetNodeType: Integer;
begin
  Result := FRealNode.nodeType;
end;

function TXMLNode.GetParentNode: IXMLNode;
begin
  Result := MakeNode(FRealNode.parentNode);
end;

function TXMLNode.GetPreviousSibling: IXMLNode;
begin
  Result := MakeNode(FRealNode.previousSibling);
end;

function TXMLNode.GetRealMSXMLNode: OleVariant;
begin
  Result := FRealNode;
end;

function TXMLNode.GetText: String;
begin
  Result := VariantToUTF8String(FRealNode.text);
end;

function TXMLNode.HasAttribute(const AName: String): Boolean;
begin
  Result := IsVarAssigned(FRealNode.attributes.getNamedItem(AName));
end;

function TXMLNode.TransformNode(const Stylesheet: IXMLNode): String;
begin
  Result := VariantToUTF8String(FRealNode.transformNode(Stylesheet.GetRealMSXMLNode));
end;

end.
