unit IsppExceptWindow;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TIsppExceptWnd = class(TForm)
    mmLog: TMemo;
    CloseBtn: TButton;
    PaintBox1: TPaintBox;
    procedure FormShow(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
  private
    FIconHandle: HICON;
  public
  end;

var
  IsppExceptWnd: TIsppExceptWnd;

implementation

{$R *.dfm}

procedure TIsppExceptWnd.FormShow(Sender: TObject);
begin
  FIconHandle := LoadIcon(0, IDI_HAND);
  MessageBeep(MB_ICONHAND);
end;

procedure TIsppExceptWnd.PaintBox1Paint(Sender: TObject);
begin
  DrawIcon(PaintBox1.Canvas.Handle, 0, 0, FIconHandle)
end;

end.
