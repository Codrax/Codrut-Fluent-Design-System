unit CFXTest;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Forms, Threading, Types, Math, Vcl.Clipbrd,

  // CFX LIBRARY
  CFX.Forms, CFX.Colors, CFX.ThemeManager, Vcl.StdCtrls, Vcl.TitleBarCtrls,
  Vcl.ExtCtrls, Vcl.Imaging.jpeg, CFX.ButtonDesign, CFX.Checkbox, CFX.Panels,
  CFX.StandardIcons, CFX.Dialogs, CFX.BlurMaterial, CFX.Selector,
  CFX.Classes, CFX.PopupMenu, CFX.Constants, CFX.Types, CFX.ToolTip, CFX.Hint,
  CFX.Slider, CFX.ImageList, CFX.Controls, CFX.Test, CFX.TextBox, CFX.RadioButton,
  CFX.Scrollbar, CFX.ScrollBox, CFX.Edit, CFX.Button,
  CFX.PopupConnector, Vcl.Buttons, CFX.IconView, CFX.ScrollText, CFX.FormClasses,
  CFX.Messages, CFX.VarHelpers, CFX.Graphics, CFX.RatingControl, CFX.Effects,
  CFX.Progress, CFX.GDI, CFX.Utilities, CFX.QuickDialogs, CFX.Instances,
  CFX.PaintBox, CFX.Lists, CFX.AppManager, CFX.Shapes, CFX.Translations,
  CFX.Layouts, CFX.TitlebarPanel, CFX.FormTemplates, CFX.TabStrip,

  // Cod Windows Runtime


  // VCL COMPONENTS
  Vcl.Dialogs, Vcl.Menus, Vcl.Controls, Vcl.Imaging.pngimage, Vcl.ControlList,
  Vcl.ExtDlgs, System.ImageList, UITypes, Vcl.ComCtrls, Vcl.Mask,
  Vcl.Themes, System.Generics.Collections,
  Vcl.NumberBox, CFX.Components, CFX.Picture, CFX.Animation.Component,
  Vcl.BaseImageCollection, Vcl.ImageCollection, Vcl.ImgList;

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
    FXSelector1: FXSelector;
    FXButton4: FXButton;
    FXButton5: FXButton;
    FXTextBox2: FXTextBox;
    FXTextBox3: FXTextBox;
    FXTextBox4: FXTextBox;
    FXTextBox5: FXTextBox;
    FXPopupMenu1: FXPopupMenu;
    FXButton14: FXButton;
    FXButton16: FXButton;
    FXIconView1: FXIconView;
    FXTitleBarPanel1: FXTitleBarPanel;
    FXBlurMaterial2: FXBlurMaterial;
    FXButton9: FXButton;
    FXScrollText1: FXScrollText;
    FXScrollbar1: FXScrollbar;
    FXButton2: FXButton;
    FXButton15: FXButton;
    FXButton17: FXButton;
    FXAppManager1: FXAppManager;
    FXPicture1: FXPicture;
    FXTextBox1: FXTextBox;
    FXButton6: FXButton;
    FXBlurMaterial1: FXBlurMaterial;
    FXRadioButton1: FXRadioButton;
    FXRadioButton2: FXRadioButton;
    FXSlider1: FXSlider;
    FXRatingControl3: FXRatingControl;
    FXDropdownButton1: FXDropdownButton;
    FXImageList1: FXImageList;
    FXButton13: FXButton;
    FXTabStrip1: FXTabStrip;
    FXLinearStringsList1: FXLinearStringsList;
    procedure FXButton5Click(Sender: TObject);
    procedure FXButton12Click(Sender: TObject);
    procedure FXButtonDesign4Click(Sender: TObject);
    procedure FXButton14Click(Sender: TObject);
    procedure FXButton16Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FXAppManager1UpdateChecked(Sender: TObject);
    procedure FXPaintBox1Draw(Sender: TObject);
    procedure FXButton2Click(Sender: TObject);
    procedure FXButton4Click(Sender: TObject);
    procedure FXButton13Click(Sender: TObject);
    procedure FXTabStrip1ClosePressed(Sender: TObject; Value: Integer);
    procedure FXTabStrip1PlusClicked(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: FXForm;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  AllowThemeChangeAnimation := True;

  //
  FXTabStrip1.ShowCloseButton := true;
  FXTabStrip1.ShowPlusButton := true;
  FXTabStrip1.EnableReordering := true;

  with FXTabStrip1.Tabs.Add do begin
    Text := 'Bon fiscal 1';

    Image.IconType := FXIconType.SegoeIcon;
    Image.SelectSegoe := #$E8A5;
  end;
  with FXTabStrip1.Tabs.Add do begin
    Text := 'Bon fiscal 2';

    Image.IconType := FXIconType.SegoeIcon;
    Image.SelectSegoe := #$E8A5;
  end;
  with FXTabStrip1.Tabs.Add do begin
    Text := 'Bon fiscal 3';

    Image.IconType := FXIconType.SegoeIcon;
    Image.SelectSegoe := #$E8A5;
  end;
  with FXTabStrip1.Tabs.Add do begin
    Text := 'Bon fiscal 4';

    Image.IconType := FXIconType.SegoeIcon;
    Image.SelectSegoe := #$E8A5;
  end;

  FXLinearStringsList1.ItemVisible[1] := false;
  FXLinearStringsList1.ItemVisible[3] := false;
  FXLinearStringsList1.ItemVisible[5] := false;
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
begin
  const F = FXPopupForm.CreateNew(nil);
  with F do begin
    Width := 200;
    Height := 100;

    with FXButton.create(F) do begin
      Parent := F;
      Top := 0;
      left := 0;
    end;

    ShowAtControl(Sender as TControl, 0, (Sender as TControl).Height);
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

procedure TForm1.FXButton2Click(Sender: TObject);
begin
  OpenDialog('Would you like to download the software?', FXDialogKind.Question, [mbYes, mbNo]);
end;

procedure TForm1.FXButton4Click(Sender: TObject);
var
  A: FXModalDialog;
begin
  A := FXModalDialog.Create;

  with A do
    try
      Title := 'Hello World!';
      Text := 'This is a fluent dialog box! Here you can press any of the buttons below!';

      //Kind := FXMessageType.Warning;
      Buttons := [mbOk, mbCancel];
      Parent := Self;

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

procedure TForm1.FXButtonDesign4Click(Sender: TObject);
var
  S: string;
begin
  with FXInputBox.Create do
    try
      Title := 'Search';
      Text := 'Enter the search query to begin searching';

      Parent := Self;
      Value := '';
      TextHint := 'Type here';

      SelectAll := true;
      Value := 'Example';
      if Execute then
        S := Value
      else
        Exit;

      with FXModalDialog.Create do
        try
          Title := 'Search Query';
          Text := Format('Your search for "%S" returned no results.', [S]);

          Buttons := [mbOk, mbCancel];

          Parent := Self;

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

procedure TForm1.FXTabStrip1ClosePressed(Sender: TObject; Value: Integer);
begin
  FXTabStrip(Sender).Tabs.Delete(Value);
end;

procedure TForm1.FXTabStrip1PlusClicked(Sender: TObject);
begin
  with FXTabStrip(Sender).Tabs.Add do begin
    Text := 'Fila noua';

    Image.IconType := FXIconType.SegoeIcon;
    Image.SelectSegoe := #$E8A5;
  end;
end;

end.
