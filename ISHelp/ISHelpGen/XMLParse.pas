unit XMLParse;

{ XML parser. Currently just calls MSXML 6.0 to do the real work. }

interface

uses
  Windows, SysUtils, Variants;

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

function VariantToString(const V: OleVariant): String;
begin
  if VarType(V) <> varOleStr then
    raise Exception.Create('VariantToUTF8String: Expected varOleStr');
  Result := TVarData(V).VOleStr;
end;

{ TXMLDocument }

constructor TXMLDocument.Create;
begin
  inherited Create;
  FDoc := CreateOleObject('MSXML2.DOMDocument.6.0');
  FDoc.setProperty('ProhibitDTD', False);
  FDoc.resolveExternals := True;
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
  Result := VariantToString(N.value);
end;

function TXMLNode.GetOptionalAttribute(const AName: String): String;
var
  N: OleVariant;
begin
  N := FRealNode.attributes.getNamedItem(AName);
  if not IsVarAssigned(N) then
    Result := ''
  else
    Result := VariantToString(N.value);
end;

function TXMLNode.GetFirstChild: IXMLNode;
begin
  Result := MakeNode(FRealNode.firstChild);
end;

function TXMLNode.GetNodeName: String;
begin
  Result := VariantToString(FRealNode.nodeName);
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
  Result := VariantToString(FRealNode.text);
end;

function TXMLNode.HasAttribute(const AName: String): Boolean;
begin
  Result := IsVarAssigned(FRealNode.attributes.getNamedItem(AName));
end;

function TXMLNode.TransformNode(const Stylesheet: IXMLNode): String;
begin
  Result := VariantToString(FRealNode.transformNode(Stylesheet.GetRealMSXMLNode));
end;

end.
