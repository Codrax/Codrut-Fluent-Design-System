﻿unit CFXTest;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Forms, Threading, Types, Math,

  // CFX LIBRARY
  CFX.Forms, CFX.Colors, CFX.ThemeManager, Vcl.StdCtrls, Vcl.TitleBarCtrls,
  Vcl.ExtCtrls, Vcl.Imaging.jpeg, CFX.ButtonDesign, CFX.Checkbox, CFX.Panels,
  CFX.StandardIcons, CFX.Dialogs, CFX.BlurMaterial, CFX.Selector,
  CFX.Classes, CFX.PopupMenu, CFX.UIConsts, CFX.Types, CFX.ToolTip, CFX.Hint,
  CFX.Slider, CFX.ImageList, CFX.Controls, CFX.Test, CFX.TextBox, CFX.RadioButton,
  CFX.Scrollbar, CFX.ScrollBox, CFX.Edit, Cod.Graphics, CFX.Button,
  CFX.PopupConnector, Vcl.Buttons, CFX.IconView, CFX.ScrollText, CFX.FormClasses,

  // VCL COMPONENTS
  Vcl.Dialogs, Vcl.Menus, Vcl.Controls, Vcl.Imaging.pngimage,
  Vcl.ExtDlgs, System.ImageList, UITypes,
  Vcl.ComCtrls, Vcl.Mask, Cod.Visual.ColorBox, CFX.Progress;

type
  TForm1 = class(FXForm)
    FXStandardIcon1: FXStandardIcon;
    FXStandardIcon2: FXStandardIcon;
    FXStandardIcon3: FXStandardIcon;
    FXStandardIcon4: FXStandardIcon;
    FXStandardIcon5: FXStandardIcon;
    FXStandardIcon6: FXStandardIcon;
    FXIconView1: FXIconView;
    FXIconView2: FXIconView;
    TitleBarPanel1: TTitleBarPanel;
    FXBlurMaterial2: FXBlurMaterial;
    FXEdit1: FXEdit;
    FXEdit2: FXEdit;
    FXButton1: FXButton;
    FXMinimisePanel1: FXMinimisePanel;
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
    FXScrollText1: FXScrollText;
    FXScrollText2: FXScrollText;
    FXSlider1: FXSlider;
    FXCheckBox1: FXCheckBox;
    FXScrollbar1: FXScrollbar;
    FXSelector1: FXSelector;
    FXRadioButton1: FXRadioButton;
    FXRadioButton2: FXRadioButton;
    FXButton4: FXButton;
    FXButton5: FXButton;
    FXEdit3: FXEdit;
    FXButton10: FXButton;
    FXButton13: FXButton;
    FXTextBox1: FXTextBox;
    FXTextBox2: FXTextBox;
    FXTextBox3: FXTextBox;
    FXTextBox4: FXTextBox;
    FXTextBox5: FXTextBox;
    FXTextBox7: FXTextBox;
    FXPopupMenu1: FXPopupMenu;
    FXBlurMaterial1: FXBlurMaterial;
    FXButton14: FXButton;
    FXTextBox6: FXTextBox;
    FXProgress1: FXProgress;
    FXButton15: FXButton;
    FXAnimatedTextBox1: FXAnimatedTextBox;
    FXButton16: FXButton;
    procedure FXButton4Click(Sender: TObject);
    procedure FXButtonDesign3Click(Sender: TObject);
    procedure FXButton5Click(Sender: TObject);
    procedure FXButton12Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormOnMove(Sender: TObject);
    procedure FXButton13Click(Sender: TObject);
    procedure FXButton11Click(Sender: TObject);
    procedure FXButtonDesign4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FXButton14Click(Sender: TObject);
    procedure FXButton15Click(Sender: TObject);
    procedure FXButton16Click(Sender: TObject);
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

procedure TForm1.FXButton11Click(Sender: TObject);
begin
  FullScreen := not FullScreen;
end;

procedure TForm1.FXButton12Click(Sender: TObject);
begin
  FXButton(Sender).Tag := FXButton(Sender).Tag + 1;
  FXButton(Sender).StateText := FXButton(Sender).Tag.ToString;
end;

procedure TForm1.FXButton13Click(Sender: TObject);
var
  A: FXDialog;
begin
  A := FXDialog.Create;

  with A do
    try
      Title := 'Hello World!';
      Text := 'This is a fluent dialog box! Here you can press any of the buttons below!';

      Kind := FXMessageType.Warning;
      Buttons := [mbOk, mbCancel];
      ParentForm := Self;

      Execute;
    finally
      Free;
    end;
end;

procedure TForm1.FXButton14Click(Sender: TObject);
var
  A: FXFormUpdateTemplate;
begin
  A := FXFormUpdateTemplate.CreateNew(Self);
  with A do
    try
      FillMode := FXFormFill.TitleBar;
      FXBlurMaterial2.Hide;
      Self.Width := Self.Width - 1;

      AppName := 'Cool Cats corp';

      Show;
    finally
      //Free;
    end;
end;

procedure TForm1.FXButton15Click(Sender: TObject);
begin
  FXProgress1.Value := RandomRange(0, 100);
end;

procedure TForm1.FXButton16Click(Sender: TObject);
var
  A: FXFormMessageTemplate;
begin
  A := FXFormMessageTemplate.CreateNew(Self);
  with A do
    try
      FillMode := FXFormFill.TitleBar;
      FXBlurMaterial2.Hide;
      Self.Width := Self.Width - 1;

      Title := 'Hello world!';
      Text := 'This is a text message. Read It carefully as It may aid you in the future.';

      Show;
    finally
      //Free;
    end;
end;

procedure TForm1.FXButton4Click(Sender: TObject);
var
  A: FXDialog;
begin
  A := FXDialog.Create;

  with A do
    try
      Title := 'Hello World!';
      Text := 'This is a fluent dialog box! Here you can press any of the buttons below!';

      Kind := FXMessageType.Warning;
      Buttons := [mbOk, mbCancel];

      Execute;
    finally
      Free;
    end;
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

procedure TForm1.FXButtonDesign4Click(Sender: TObject);
var
  A: FXInputBox;
  D: FXDialog;
  S: string;
begin
  A := FXInputBox.Create;

  with A do
    try
      Title := 'Search';
      Text := 'Enter the search query to begin searching';

      ParentForm := Self;
      Value := 'Text text';

      S := Execute;

      D := FXDialog.Create;
      with D do
        try
          Title := 'Search Query';
          Text := Format('Your search for "%S" returned no results.', [S]);

          ParentForm := Self;

          Execute;
        finally
          Free;
        end;
    finally
      Free;
    end;
end;

procedure TForm1.UpdateBlurs;
begin
  FXBlurMaterial1.SyncroniseImage;
  FXBlurMaterial2.SyncroniseImage;
end;

end.
