unit CFXTest;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Forms, Threading, Types, Math,

  // CFX LIBRARY
  CFX.Forms, CFX.Colors, CFX.ThemeManager, Vcl.StdCtrls, Vcl.TitleBarCtrls,
  Vcl.ExtCtrls, Vcl.Imaging.jpeg, CFX.ButtonDesign, CFX.Checkbox, CFX.Panels,
  CFX.StandardIcons, CFX.Dialogs, CFX.BlurMaterial, CFX.Selector,
  CFX.Classes, CFX.PopupMenu, CFX.Constants, CFX.Types, CFX.ToolTip, CFX.Hint,
  CFX.Slider, CFX.ImageList, CFX.Controls, CFX.Test, CFX.TextBox, CFX.RadioButton,
  CFX.Scrollbar, CFX.ScrollBox, CFX.Edit, Cod.Graphics, CFX.Button,
  CFX.PopupConnector, Vcl.Buttons, CFX.IconView, CFX.ScrollText, CFX.FormClasses,
  CFX.Messages, CFX.VarHelpers, CFX.Graphics, CFX.RatingControl, CFX.Effects,
  CFX.Progress, CFX.GDI, CFX.Utilities, CFX.QuickDialogs, CFX.Instances,
  CFX.PaintBox, CFX.Lists, CFX.TabStrip, CFX.AppManager, CFX.Shapes,

  // VCL COMPONENTS
  Vcl.Dialogs, Vcl.Menus, Vcl.Controls, Vcl.Imaging.pngimage, Vcl.ControlList,
  Vcl.ExtDlgs, System.ImageList, UITypes, Vcl.ComCtrls, Vcl.Mask,
  Vcl.Themes, System.Generics.Collections, CFX.Layouts, CFX.TitlebarPanel;

type
  TForm1 = class(FXForm)
    FXStandardIcon1: FXStandardIcon;
    FXEdit1: FXEdit;
    FXEdit2: FXEdit;
    FXButton1: FXButton;
    FXButtonDesign3: FXButtonDesign;
    FXButtonDesign1: FXButtonDesign;
    FXButton3: FXButton;
    FXButton7: FXButton;
    FXButton8: FXButton;
    FXButtonDesign2: FXButtonDesign;
    FXButtonDesign4: FXButtonDesign;
    FXButton11: FXButton;
    FXButton12: FXButton;
    FXScrollText2: FXScrollText;
    FXSlider1: FXSlider;
    FXCheckBox1: FXCheckBox;
    FXSelector1: FXSelector;
    FXButton4: FXButton;
    FXButton5: FXButton;
    FXTextBox2: FXTextBox;
    FXTextBox3: FXTextBox;
    FXTextBox4: FXTextBox;
    FXTextBox5: FXTextBox;
    FXPopupMenu1: FXPopupMenu;
    FXBlurMaterial1: FXBlurMaterial;
    FXButton14: FXButton;
    FXButton16: FXButton;
    PaintBox1: TPaintBox;
    FXTextBox8: FXTextBox;
    FXIconView1: FXIconView;
    FXAppManager1: FXAppManager;
    FXTabStrip1: FXTabStrip;
    FXTitleBarPanel1: FXTitleBarPanel;
    FXBlurMaterial2: FXBlurMaterial;
    FXButton9: FXButton;
    FXScrollText1: FXScrollText;
    FXButton13: FXButton;
    FXScrollbar1: FXScrollbar;
    FXScrollText3: FXScrollText;
    procedure FXButton5Click(Sender: TObject);
    procedure FXButton12Click(Sender: TObject);
    procedure FXButton13Click(Sender: TObject);
    procedure FXButtonDesign4Click(Sender: TObject);
    procedure FXButton14Click(Sender: TObject);
    procedure FXButton16Click(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure FXSlider1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FXAppManager1UpdateChecked(Sender: TObject);
    procedure FXPaintBox1Draw(Sender: TObject);
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

procedure TForm1.FormCreate(Sender: TObject);
begin
  FXPopupMenu1.Items[1].Text := '-';
  AllowThemeChangeAnimation := true;
end;

procedure TForm1.FXAppManager1UpdateChecked(Sender: TObject);
begin
  if not AppManager.UpdateCheckSuccess then
    OpenMessage('Update checking failed')
  else
    OpenMessage('Latest server version: ' + AppManager.ServerVersion.ToString)
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
      Self.Width := Self.Width - 1;

      AppName := 'Cool Application';

      DownloadURL := 'https://codrutsoft.com/downloads/software/ibroadcast/Cods%20iBroadcast%201.7.0-x64.exe';
      InstallParameters := '-ad';

      Show;
    finally
      //Free;
    end;
end;

procedure TForm1.FXButton16Click(Sender: TObject);
var
  A: FXFormMessageTemplate;
begin
  A := FXFormMessageTemplate.CreateNew(Self);
  with A do
    try
      FillMode := FXFormFill.TitleBar;
      Self.Width := Self.Width - 1;

      IconKind := FXStandardIconType.Warning;

      Title := 'Hello world!';
      Text := 'This is a text message. Read It carefully as It may aid you in the future.';

      Show;
    finally
      //Free;
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
      Value := '';
      TextHint := 'Type here';

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

procedure TForm1.FXPaintBox1Draw(Sender: TObject);
begin
  with FXPaintBox(Sender).Buffer do
    begin
      Brush.Color := clRed;
      Rectangle( ClipRect );
    end;
end;

procedure TForm1.FXSlider1Change(Sender: TObject);
begin
  PaintBox1.Tag := round(FXSlider(Sender).Position / 100 * 360);
  PaintBox1.Repaint;
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
var
  R: TRect;
begin
  with TPaintBox(Sender).Canvas do
    begin
      R := ClipRect;

      Font.Height := 22;

      GDIText('Hello world! This is truly incredibile, text rotating! :)',
        R, [FXTextFlag.WordWrap, FXTextFlag.NoClip, FXTextFlag.VerticalCenter,
          FXTextFlag.Center],
        TPaintBox(Sender).Tag);
    end;
end;

end.
