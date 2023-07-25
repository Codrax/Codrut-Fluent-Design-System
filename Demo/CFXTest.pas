unit CFXTest;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Forms, Threading, Types,

  // CFX LIBRARY
  CFX.Forms, CFX.Colors, CFX.ThemeManager, Vcl.StdCtrls, Vcl.TitleBarCtrls,
  Vcl.ExtCtrls, Vcl.Imaging.jpeg, CFX.ButtonDesign, CFX.Checkbox, CFX.Panels,
  CFX.StandardIcons, CFX.Dialogs, CFX.BlurMaterial, CFX.Selector,
  CFX.Classes, CFX.PopupMenu, CFX.UIConsts, CFX.Types, CFX.ToolTip, CFX.Hint,
  CFX.Slider, CFX.ImageList, CFX.Controls, CFX.Test, CFX.Labels, CFX.RadioButton,
  CFX.Scrollbar, CFX.ScrollBox, CFX.Edit, Cod.Graphics, CFX.Button,

  // VCL COMPONENTS
  Vcl.Dialogs, Vcl.Menus, Vcl.Controls, Vcl.Imaging.pngimage,
  Vcl.ExtDlgs, System.ImageList, UITypes,
  Vcl.ComCtrls, Vcl.Mask, CFX.PopupConnector, Vcl.Buttons, CFX.IconView;

type
  TForm1 = class(FXForm)
    FXStandardIcon1: FXStandardIcon;
    FXStandardIcon2: FXStandardIcon;
    FXStandardIcon3: FXStandardIcon;
    FXStandardIcon4: FXStandardIcon;
    FXStandardIcon5: FXStandardIcon;
    FXStandardIcon6: FXStandardIcon;
    FXLabel2: FXLabel;
    FXLabel3: FXLabel;
    TitleBarPanel1: TTitleBarPanel;
    FXSlider1: FXSlider;
    FXCheckBox1: FXCheckBox;
    FXScrollbar1: FXScrollbar;
    FXSelector1: FXSelector;
    FXRadioButton1: FXRadioButton;
    FXRadioButton2: FXRadioButton;
    FXEdit1: FXEdit;
    FXEdit2: FXEdit;
    FXButton1: FXButton;
    FXButton4: FXButton;
    FXButton5: FXButton;
    FXMinimisePanel1: FXMinimisePanel;
    FXLabel1: FXLabel;
    FXLabel4: FXLabel;
    FXButton2: FXButton;
    FXButton6: FXButton;
    FXButtonDesign3: FXButtonDesign;
    FXButtonDesign1: FXButtonDesign;
    FXButton3: FXButton;
    FXButton7: FXButton;
    FXButton8: FXButton;
    FXButton9: FXButton;
    FXButtonDesign2: FXButtonDesign;
    FXButtonDesign4: FXButtonDesign;
    FXButton11: FXButton;
    FXButton12: FXButton;
    FXPopupMenu1: FXPopupMenu;
    FXEdit3: FXEdit;
    FXButton10: FXButton;
    FXBlurMaterial1: FXBlurMaterial;
    FXLabel5: FXLabel;
    FXBlurMaterial2: FXBlurMaterial;
    FXIconView1: FXIconView;
    FXIconView2: FXIconView;
    procedure FXButton4Click(Sender: TObject);
    procedure FXButtonDesign3Click(Sender: TObject);
    procedure FXButton5Click(Sender: TObject);
    procedure FXButton12Click(Sender: TObject);
    procedure FXButton10Click(Sender: TObject);
    procedure FXButton13Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormOnMove(Sender: TObject);
  private
    { Private declarations }
    procedure UpdateBlurs;
  public
    { Public declarations }
  end;

var
  Form1: FXForm;

  H: FXHintPopup;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  OnMove := FormOnMove;
end;

procedure TForm1.FormOnMove(Sender: TObject);
begin
  UpdateBlurs;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  UpdateBlurs;
end;

procedure TForm1.FXButton10Click(Sender: TObject);
begin
  FXBUtton10.UpdateTheme(false);
end;

procedure TForm1.FXButton12Click(Sender: TObject);
begin
  FXButton(Sender).Tag := FXButton(Sender).Tag + 1;
  FXButton(Sender).StateText := FXButton(Sender).Tag.ToString;
end;

procedure TForm1.FXButton13Click(Sender: TObject);
begin
  FXButton(Sender).UpdateTheme(false)
end;

procedure TForm1.FXButton4Click(Sender: TObject);
var
  A: FXDialog;
begin
  A := FXDialog.Create;

  A.Title := 'Hello World!';
  A.Text := 'This is a fluent dialog box! Here you can press any of the buttons below!';

  A.Kind := FXMessageType.Warning;
  A.Buttons := [mbOk, mbCancel];

  A.Execute;

  A.Free;
end;

procedure TForm1.FXButton5Click(Sender: TObject);
begin
  if ThemeManager.DarkTheme then
    ThemeManager.DarkThemeMode := FXDarkSetting.ForceLight
  else
    ThemeManager.DarkThemeMode := FXDarkSetting.ForceDark;

  ThemeManager.UpdateSettings;
end;

procedure TForm1.FXButtonDesign3Click(Sender: TObject);
var
  P: FXPopupMenu;
  I, O: FXPopupItem;
  J: integer;
begin
  P := nil;

  if P.GetMenuItemCount = 0 then
  begin
    with FXPopupItem.Create(P) do
      begin
        Text := 'Copy';
        ShortCut := 'Ctrl+C';

        Image.Enabled := true;
        Image.SelectSegoe := '';
      end;

    I := FXPopupItem.Create(nil);
    with I do
      begin
        Text := 'Copys';
        ShortCut := 'Ctrl+C';

        Image.Enabled := true;
        Image.SelectSegoe := '';
      end;

    O := FXPopupItem.Create(nil);
    with O do
      begin
        Text := 'Wooow';
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

        Image.IconType := FXIconType.Image;
        Image.SelectPicture.LoadFromFile( 'C:\Windows\System32\SecurityAndMaintenance_Alert.png' );

        Items.Add(I);
        Items.Add(O);
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
        ShortCut := 'Del';

        Image.Enabled := true;
        Image.SelectSegoe := '';
      end;

    // Create Sub Items
    with FXPopupItem.Create(I) do
      begin
        Text := 'Photo';
        ShortCut := 'Alt+Shift+P';

        Image.Enabled := true;
        Image.SelectSegoe := '';
      end;

    with FXPopupItem.Create(I) do
      begin
        Text := '-';

        Image.Enabled := true;
      end;

    O := FXPopupItem.Create(I);
    with O do
      begin
        Text := 'Show Password';

        ShortCut := 'Alt+Shift+P';
        Image.Enabled := true;
        Image.SelectSegoe := '';
      end;

    with FXPopupItem.Create(O) do
      begin
        Text := 'Reload';

        Image.Enabled := true;
        Image.SelectSegoe := '';
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
        ShortCut := 'Ctrl+P';

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
        Text := 'Inspect';
        ShortCut := 'F11';

        Image.Enabled := true;
        Image.SelectSegoe := '';

        OnClick := FXButton4Click;
      end;

    with FXPopupItem.Create(P) do
      begin
        Text := 'Enable Scripts';
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
        Text := '-';

        Image.Enabled := true;
      end;

    with FXPopupItem.Create(P) do
      begin
        Text := 'Star 1';
        ShortCut := 'Ctrl+1';

        Image.Enabled := true;
        Image.SelectSegoe := '';

        AutoCheck := true;
        RadioItem := true;
        OnCheck := FXButton4Click;
      end;

    with FXPopupItem.Create(P) do
      begin
        Text := 'Star 2';
        ShortCut := 'Ctrl+2';

        Image.Enabled := true;
        Image.SelectSegoe := '';

        AutoCheck := true;
        RadioItem := true;
      end;

    with FXPopupItem.Create(P) do
      begin
        Text := 'Star 3';
        ShortCut := 'Ctrl+3';

        Image.Enabled := true;
        Image.SelectSegoe := '';

        Checked := true;
        AutoCheck := true;
        RadioItem := true;
      end;

    with FXPopupItem.Create(P) do
      begin
        Text := 'Star 4';
        ShortCut := 'Ctrl+4';

        Image.Enabled := true;
        Image.SelectSegoe := '';

        AutoCheck := true;
        RadioItem := true;
      end;

    for J := 0 to P.ComponentCount - 1 do
      P.Items.Add( FXPopupItem(P.Components[J]) );
  end;

  P.PopupAtCursor;
end;

procedure TForm1.UpdateBlurs;
begin
  FXBlurMaterial1.SyncroniseImage;
  FXBlurMaterial2.SyncroniseImage;
end;

end.
