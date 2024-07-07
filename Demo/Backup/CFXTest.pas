unit CFXTest;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, UCL.Form,

  // CFX LIBRARY
  CFX.Forms, CFX.Colors, CFX.ThemeManager, Vcl.StdCtrls, Vcl.TitleBarCtrls,
  Vcl.ExtCtrls, Cod.Panels, Vcl.Imaging.jpeg, Cod.Button, CFX.Button,
  Vcl.Imaging.pngimage, Cod.Image, UCL.CheckBox, CFX.Checkbox, CFX.Panels;

type
  TForm1 = class(FXForm)
    Label1: TLabel;
    Button2: TButton;
    Timer1: TTimer;
    FXButton1: FXButton;
    CImage1: CImage;
    FXButton2: FXButton;
    FXButton4: FXButton;
    FXButton5: FXButton;
    FXButton6: FXButton;
    FXButton3: FXButton;
    FXButton8: FXButton;
    procedure Button2Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: FXForm;

implementation

{$R *.dfm}

procedure TForm1.Button2Click(Sender: TObject);
begin
  Self.SmokeEffect := NOT Self.SmokeEffect;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Self.SmokeEffect := false;
end;

end.
