unit IDE.ImagesModule;

interface

uses
  System.SysUtils, System.Classes, System.ImageList, Vcl.ImgList, Vcl.Controls,
  Vcl.VirtualImageList, Vcl.BaseImageCollection, Vcl.ImageCollection;

type
  TImagesModule = class(TDataModule)
    BuildImageList: TImageList;
    LightToolBarImageCollection: TImageCollection;
    ThemedToolbarVirtualImageList: TVirtualImageList;
    LightToolbarVirtualImageList: TVirtualImageList;
    DarkToolBarImageCollection: TImageCollection;
    LightMarkersAndACImageCollection: TImageCollection;
    ThemedMarkersAndACVirtualImageList: TVirtualImageList;
    DarkMarkersAndACImageCollection: TImageCollection;
    procedure DataModuleCreate(Sender: TObject);
  public
    procedure UpdateTheme(const Dark: Boolean);
  end;

var
  ImagesModule: TImagesModule;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure TImagesModule.DataModuleCreate(Sender: TObject);
begin
  ThemedMarkersAndACVirtualImageList.AutoFill := True;
end;

procedure TImagesModule.UpdateTheme(const Dark: Boolean);
begin
  if Dark then begin
    ThemedToolbarVirtualImageList.ImageCollection := DarkToolBarImageCollection;
    ThemedMarkersAndACVirtualImageList.ImageCollection := DarkMarkersAndACImageCollection;
  end else begin
    ThemedToolbarVirtualImageList.ImageCollection := LightToolBarImageCollection;
    ThemedMarkersAndACVirtualImageList.ImageCollection := LightMarkersAndACImageCollection;
  end;
end;

end.
