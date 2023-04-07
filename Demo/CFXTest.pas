unit CFXTest;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Forms, Threading,

  // CFX LIBRARY
  CFX.Forms, CFX.Colors, CFX.ThemeManager, Vcl.StdCtrls, Vcl.TitleBarCtrls,
  Vcl.ExtCtrls, Vcl.Imaging.jpeg, CFX.Button, CFX.Checkbox, CFX.Panels,
  CFX.StandardIcons, CFX.Slider, CFX.Dialogs, CFX.BlurMaterial,
  CFX.Classes, CFX.PopupMenu, CFX.UIConsts, CFX.Types,

  // VCL COMPONENTS
  Vcl.Dialogs, Vcl.Menus, Vcl.Controls, Vcl.Imaging.pngimage, CFX.Controls,
  Cod.Visual.Button, Vcl.ExtDlgs;

type
  TForm1 = class(FXForm)
    Label1: TLabel;
    FXButton1: FXButton;
    FXMinimisePanel1: FXMinimisePanel;
    FXButon2: FXButton;
    FXButton2: FXButton;
    FXSlider2: FXSlider;
    FXCheckBox1: FXCheckBox;
    TitleBarPanel1: TTitleBarPanel;
    FXStandardIcon1: FXStandardIcon;
    FXButton3: FXButton;
    FXBlurMaterial1: FXBlurMaterial;
    CButton1: CButton;
    FXPopupMenu1: FXPopupMenu;
    OpenPictureDialog1: TOpenPictureDialog;
    procedure Button2Click(Sender: TObject);
    procedure FXButton1Click(Sender: TObject);
    procedure FXButton3Click(Sender: TObject);
    procedure CButton1Click(Sender: TObject);
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

  if Application.ComponentState = [] then
    ShowMessage('');
end;

procedure TForm1.CButton1Click(Sender: TObject);
var
  A, B: FXIconSelect;
begin
  A := FXIconSelect.Create;
  B := FXIconSelect.Create;


  B.Assign(A);
end;

procedure TForm1.FXButton1Click(Sender: TObject);
var
  A: FXDialog;
begin
  A := FXDialog.Create;

  A.Title := 'Hello World!';
  A.Text := 'This is a fluent dialog box! Here you can press any of the buttons below!';

  A.Kind := ctWarning;
  A.Buttons := [mbOk, mbCancel];

  A.Execute;

  A.Free;
end;

procedure TForm1.FXButton3Click(Sender: TObject);
var
  P: FXPopupMenu;
  I, O: FXPopupItem;
begin
  P := FXPopupMenu1;

  if P.GetMenuItemCount = 0 then
  begin
    with FXPopupItem.Create(P) do
      begin
        Text := 'Copy';
        ShortCut := 'Ctrl+C';

        Image.Enabled := true;
        Image.SelectSegoe := '';
      end;

    with FXPopupItem.Create(P) do
      begin
        Text := 'Select All';
        ShortCut := 'Ctrl+A';

        Image.Enabled := true;
        Image.SelectSegoe := '';

        IsDefault := true;

        Image.IconType := fitBitMap;
        Image.SelectBitmap.LoadFromFile( 'F:\Assets\By Me\40x40\CheckMark gradient.bmp' );
      end;

    with FXPopupItem.Create(P) do
      begin
        Text := '-';

        Image.Enabled := true;
      end;

    // Sub Item Block
    I := FXPopupItem.Create(P);
    with I do
      begin
        Text := 'More';
        ShortCut := 'LOL';

        Image.Enabled := true;
        Image.SelectSegoe := '';

        OnClick := FXButton1Click;
      end;

    // Create Sub Items
    with FXPopupItem.Create(I) do
      begin
        Text := 'Print';
        ShortCut := 'Alt+Shift+P';

        Image.Enabled := true;
        Image.SelectSegoe := '';
      end;

    with FXPopupItem.Create(I) do
      begin
        Text := '-';

        Image.Enabled := true;
      end;

    O := FXPopupItem.Create(I);
    with O do
      begin
        Text := 'Even More Options';
        ShortCut := '';

        Image.Enabled := true;
        Image.SelectSegoe := '';
      end;

    with FXPopupItem.Create(O) do
      begin
        Text := 'Print';
        ShortCut := 'Alt+Shift+P';

 
      end;

    with FXPopupItem.Create(O) do
      begin
        Text := 'View Selection Source';
        ShortCut := '';

        Image.Enabled := true;
        Image.SelectSegoe := '';
      end;


    // Resume
    with FXPopupItem.Create(P) do
      begin
        Text := '-';

        Image.Enabled := true;
      end;

    with FXPopupItem.Create(P) do
      begin
        Text := 'Print';
        ShortCut := 'Alt+Shift+P';

        Enabled := false;

        Image.Enabled := true;
        Image.SelectSegoe := '';
      end;

    with FXPopupItem.Create(P) do
      begin
        Text := '-';

        Image.Enabled := true;
      end;

    with FXPopupItem.Create(P) do
      begin
        Text := 'Take Screenshot';
        ShortCut := '';

        AutoCheck := true;
        Checked := true;
      end;

    with FXPopupItem.Create(P) do
      begin
        Text := '-';

        Image.Enabled := true;
      end;

    with FXPopupItem.Create(P) do
      begin
        Text := 'View Selection Source';
        ShortCut := '';

        RadioItem := false;
        AutoCheck := true;
        Checked := true;
      end;

    with FXPopupItem.Create(P) do
      begin
        Text := 'Inspect';
        ShortCut := 'F11';

        Image.Enabled := true;
        Image.SelectSegoe := '';

        OnClick := FXButton1Click;
      end;

    with FXPopupItem.Create(P) do
      begin
        Text := '-';

        Image.Enabled := true;
      end;

    with FXPopupItem.Create(P) do
      begin
        Text := 'Inspect';
        ShortCut := 'F11';

        Image.Enabled := true;
        Image.SelectSegoe := '';

        AutoCheck := true;
        RadioItem := true;
      end;

    with FXPopupItem.Create(P) do
      begin
        Text := 'Inspect';
        ShortCut := 'F11';

        Image.Enabled := true;
        Image.SelectSegoe := '';

        AutoCheck := true;
        RadioItem := true;
      end;

    with FXPopupItem.Create(P) do
      begin
        Text := 'Inspect';
        ShortCut := 'F11';

        Image.Enabled := true;
        Image.SelectSegoe := '';

        Checked := true;
        AutoCheck := true;
        RadioItem := true;
      end;

    with FXPopupItem.Create(P) do
      begin
        Text := 'Inspect';
        ShortCut := 'F11';

        Image.Enabled := true;
        Image.SelectSegoe := '';

        AutoCheck := true;
        RadioItem := true;
      end;
  end;

  P.PopupAtCursor;
end;

end.
