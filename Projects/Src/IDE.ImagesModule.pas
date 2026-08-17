unit IDE.ImagesModule;

interface

uses
  System.SysUtils, System.Classes, System.ImageList, Vcl.ImgList, Vcl.Controls,
  Vcl.VirtualImageList, Vcl.BaseImageCollection, Vcl.ImageCollection;

type
  TImagesModule = class(TDataModule)
    private
      function GetBuildImageList(Dark: Boolean): TImageList;
      function GetListImageCollection(Dark: Boolean): TImageCollection;
      function GetMarkersAndACImageCollection(Dark: Boolean): TImageCollection;
      function GetToolbarImageCollection(Dark: Boolean): TImageCollection;
    public
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