unit IDE.ImagesModule;

interface

uses
  System.SysUtils, System.Classes, System.ImageList, Vcl.ImgList, Vcl.Controls,
  Vcl.VirtualImageList, Vcl.BaseImageCollection, Vcl.ImageCollection;

type
  TImagesModule = class(TDataModule)
    BuildImageList: TImageList;
    LightToolBarImageCollection: TImageCollection;
    DarkToolBarImageCollection: TImageCollection;
    LightMarkersAndACImageCollection: TImageCollection;
    DarkMarkersAndACImageCollection: TImageCollection;
  end;

var
  ImagesModule: TImagesModule;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

end.