unit CFXTest;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Forms, Threading, Types,

  // CFX LIBRARY
  CFX.Forms, CFX.Colors, CFX.ThemeManager, Vcl.StdCtrls, Vcl.TitleBarCtrls,
  Vcl.ExtCtrls, Vcl.Imaging.jpeg, CFX.Button, CFX.Checkbox, CFX.Panels,
  CFX.StandardIcons, CFX.Dialogs, CFX.BlurMaterial, CFX.Selector,
  CFX.Classes, CFX.PopupMenu, CFX.UIConsts, CFX.Types, CFX.ToolTip, CFX.Hint,
  CFX.Slider, CFX.ImageList, CFX.Controls, CFX.Test, CFX.Labels, CFX.RadioButton,
  CFX.Scrollbar, CFX.ScrollBox, CFX.Edit, Cod.Graphics,

  // VCL COMPONENTS
  Vcl.Dialogs, Vcl.Menus, Vcl.Controls, Vcl.Imaging.pngimage,
  Vcl.ExtDlgs, System.ImageList, UITypes,
  Vcl.ComCtrls, Vcl.Mask, CFX.PopupConnector, Vcl.Buttons;

type
  TForm1 = class(FXForm)
    FXStandardIcon1: FXStandardIcon;
    FXStandardIcon2: FXStandardIcon;
    FXStandardIcon3: FXStandardIcon;
    FXStandardIcon4: FXStandardIcon;
    FXStandardIcon5: FXStandardIcon;
    FXStandardIcon6: FXStandardIcon;
    FXButton1: FXButton;
    FXMinimisePanel1: FXMinimisePanel;
    FXLabel1: FXLabel;
    FXButon2: FXButton;
    FXButton2: FXButton;
    TitleBarPanel1: TTitleBarPanel;
    FXButton3: FXButton;
    FXSlider1: FXSlider;
    FXCheckBox1: FXCheckBox;
    FXScrollbar1: FXScrollbar;
    FXCheckBox2: FXCheckBox;
    FXSelector1: FXSelector;
    FXRadioButton1: FXRadioButton;
    FXRadioButton2: FXRadioButton;
    FXEdit1: FXEdit;
    FXLabel2: FXLabel;
    FXEdit2: FXEdit;
    FXLabel3: FXLabel;
    procedure Button2Click(Sender: TObject);
    procedure FXButton1Click(Sender: TObject);
    procedure FXButton3Click(Sender: TObject);
    procedure CButton1Click(Sender: TObject);
    procedure CImage1Click(Sender: TObject);
    procedure FXButton4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: FXForm;

  H: FXHintPopup;

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

procedure TForm1.CImage1Click(Sender: TObject);
begin
  Self.Repaint;
end;

procedure TForm1.FXButton1Click(Sender: TObject);
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

procedure TForm1.FXButton3Click(Sender: TObject);
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

        OnClick := FXButton1Click;
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
        OnCheck := FXButton1Click;
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

procedure TForm1.FXButton4Click(Sender: TObject);
begin
  with ThemeManager.SystemColor do
    begin
      Foreground := clLime;
      Background := clRed;
      BackgroundInterior := TColors.Cornflowerblue;
      Accent := 16679746;
    end;

  with ThemeManager.SystemColorSet do
    begin
      DarkForeground := clLime;
      DarkBackground := clRed;
      DarkBackGroundInterior := TColors.Slateblue;
      Accent := 16679746;
    end;

  with ThemeManager.SystemAccentInteractStates do
    begin
      None := TColors.Purple;
      Hover := TColors.Cornflowerblue;
      Press := TColors.Olivedrab;
    end;

  ThemeManager.NotifyUpdate;
end;

end.
