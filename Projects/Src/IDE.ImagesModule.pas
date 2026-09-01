unit IDE.ImagesModule;

interface

uses
  System.SysUtils, System.Classes, System.ImageList, Vcl.ImgList, Vcl.Controls,
  Vcl.VirtualImageList, Vcl.BaseImageCollection, Vcl.ImageCollection,
  IDE.ScriptModel.Metadata.Extra.WordLists;

type
  TImagesModule = class(TDataModule)
    private
      function GetBuildImageList(Dark: Boolean): TImageList;
      function GetListImageCollection(Dark: Boolean): TImageCollection;
      function GetMarkersAndACImageCollection(Dark: Boolean): TImageCollection;
      function GetToolbarImageCollection(Dark: Boolean): TImageCollection;
    public
      class function AutoCompleteWordTypeImageName(const AWordType: TAutoCompleteWordType): String; static;
      property BuildImageList[Dark: Boolean]: TImageList read GetBuildImageList;
      property ListImageCollection[Dark: Boolean]: TImageCollection read GetListImageCollection;
      property MarkersAndACImageCollection[Dark: Boolean]: TImageCollection read GetMarkersAndACImageCollection;
      property ToolBarImageCollection[Dark: Boolean]: TImageCollection read GetToolbarImageCollection;
    published
      LightBuildImageList: TImageList;
      DarkBuildImageList: TImageList;
      LightListImageCollection: TImageCollection;
      DarkListImageCollection: TImageCollection;
      LightMarkersAndACImageCollection: TImageCollection;
      DarkMarkersAndACImageCollection: TImageCollection;
      LightToolBarImageCollection: TImageCollection;
      DarkToolBarImageCollection: TImageCollection;
  end;

var
  ImagesModule: TImagesModule;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

{ TImagesModule }

class function TImagesModule.AutoCompleteWordTypeImageName(const AWordType: TAutoCompleteWordType): String;
begin
  case AWordType of
    awtSectionName: Result := 'ac\structure-filled';
    awtParameterName: Result := 'ac\xml-filled';
    awtKeyName: Result := 'ac\xml-filled';
    awtPreprocessorDirective: Result := 'ac\symbol-hashtag';
    awtPreprocessorSubDirective: Result := 'ac\symbol-hashtag-arrow-right-2';
    awtScriptFunction: Result := 'ac\method-filled';
    awtISPPFunction: Result := 'ac\method-filled';
    awtScriptType: Result := 'ac\types';
    awtScriptVariable: Result := 'ac\constant-filled';            { Green }
    awtISPPVariable: Result := 'ac\constant-filled';              { Green }
    awtConstant: Result := 'ac\constant-filled_2';                { Purple }
    awtMemberValue: Result := 'ac\constant-filled_3';             { Blue }
    awtScriptFunctionParameter: Result := 'ac\constant-filled_4'; { Orange }
    awtScriptFunctionVariable: Result := 'ac\constant-filled_5';  { Yellow }
    awtScriptConstant: Result := 'ac\constant-filled_6';          { Gray }
    awtScriptEnumValue: Result := 'ac\constant-filled_6';         { Gray }
    awtISPPConstant: Result := 'ac\constant-filled_6';            { Gray }
    awtScriptInterface: Result := 'ac\interface-filled';
    awtScriptProperty: Result := 'ac\properties-filled';
    awtScriptEvent: Result := 'ac\event-filled';
    awtScriptKeyword: Result := 'ac\list';
  else
    raise Exception.Create('Internal error: AutoCompleteWordTypeImageName: unexpected word type');
  end;
end;

function TImagesModule.GetBuildImageList(Dark: Boolean): TImageList;
begin
  if Dark then
    Result := DarkBuildImageList
  else
    Result := LightBuildImageList;
end;

function TImagesModule.GetListImageCollection(Dark: Boolean): TImageCollection;
begin
  if Dark then
    Result := DarkListImageCollection
  else
    Result := LightListImageCollection;
end;

function TImagesModule.GetMarkersAndACImageCollection(Dark: Boolean): TImageCollection;
begin
  if Dark then
    Result := DarkMarkersAndACImageCollection
  else
    Result := LightMarkersAndACImageCollection;
end;

function TImagesModule.GetToolbarImageCollection(Dark: Boolean): TImageCollection;
begin
  if Dark then
    Result := DarkToolBarImageCollection
  else
    Result := LightToolBarImageCollection;
end;

end.