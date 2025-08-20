unit CFX.FormTemplates;

interface
uses
  Windows,
  Vcl.Graphics,
  Classes,
  Types,
  Vcl.Clipbrd,
  CFX.Types,
  CFX.Constants,
  SysUtils,
  CFX.Colors,
  Vcl.Forms,
  CFX.Graphics,
  CFX.VarHelpers,
  CFX.ThemeManager,
  Vcl.Controls,
  CFX.Files,
  CFX.FormClasses,
  Messages,
  TypInfo,
  CFX.Linker,
  CFX.Classes,
  CFX.Forms,
  CFX.ToolTip,
  CFX.TextBox,
  CFX.Panels,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Imaging.pngimage,
  CFX.Imported,
  CFX.Button,
  CFX.Progress,
  CFX.StandardIcons,
  CFX.Utilities,
  CFX.StringUtils,
  CFX.ScrollBox,
  CFX.Internet;

type
  FXFormUpdateTemplate = class(FXFillForm)
  private
    const
    BUTTON_HEIGHT = 50;
    BUTTON_WIDTH = 200;

    var
    FAppName: string;
    FAllowSnooze: boolean;

    Title_Box: FXTextBox;

    LastErrorText: string;

    Main_Contain: FXScrollBox;

    Button_Contain,
    Download_Progress,
    Error_Contain,
    Error_Buttons: FXPanel;

    Animated_Box: FXAnimatedTextBox;
    Progress: FXProgress;

    FDownloadURL,
    FParams: string;

    FInstallThread: TThread;

    Button_Cancel: FXButton;

    // On Click
    procedure ButtonClick(Sender: TObject);

    // Data
    procedure UpdateTitle;

    // Install
    procedure ThreadedInstall;

    // UI
    procedure ShowError(Title, Text: string);

    // Setters
    procedure SetAllowSnooze(const Value: boolean);
    procedure SetAppName(const Value: string);

  protected
    // Update download
    function DoDownload(var From: string; ToFile: string): boolean; virtual;
    procedure DoInstall(FromTempFile: string; Params: string); virtual;
    function BuildTempFile: string; virtual;

    // Build
    procedure BuildControls; override;

    // Init
    procedure InitializeNewForm; override;

  published
    property AppName: string read FAppName write SetAppName;
    property AllowSnooze: boolean read FAllowSnooze write SetAllowSnooze;

    property DownloadURL: string read FDownloadURL write FDownloadURL;
    property InstallParameters: string read FParams write FParams;
  end;

  FXTaskExecutingTemplate = class(FXFillForm)
  private
    const
    BUTTON_HEIGHT = 50;
    BUTTON_WIDTH = 200;

    var
    FCanceled: boolean;
    FOnCancel: TNotifyEvent;

    // Controls
    Title_Box,
    Text_Box: FXTextBox;
    Main_Contain: FXScrollBox;
    Dialog_Icon: FXStandardIcon;

    Button_Contain,
    Download_Progress: FXPanel;

    Animated_Box: FXAnimatedTextBox;
    Progress: FXProgress;

    Button_Cancel: FXButton;

    // On Click
    procedure ButtonClick(Sender: TObject);

    // Setters
    procedure SetProgressText(const Value: string);
    procedure SetText(const Value: string);
    procedure SetTitle(const Value: string);
    procedure SetSelectedIcon(const Value: FXStandardIconType);
    procedure SetShowCancel(const Value: boolean);
    procedure SetProgressKind(const Value: FXProgressKind);
    procedure SetProgressPercent(const Value: FXPercent);

    // Getters
    function GetShowCancel: boolean;
    function GetProgressKind: FXProgressKind;
    function GetProgressPercent: FXPercent;
    function GetProgressText: string;
    function GetSelectedIcon: FXStandardIconType;
    function GetText: string;
    function GetTitle: string;

  protected
    // Build
    procedure BuildControls; override;

    // UI
    procedure AlignLayout;

    // Init
    procedure InitializeNewForm; override;

  published
    property Canceled: boolean read FCanceled;
    property OnCancel: TNotifyEvent read FOnCancel write FOnCancel;

    property Title: string read GetTitle write SetTitle;
    property Text: string read GetText write SetText;
    property ProgressValue: FXPercent read GetProgressPercent write SetProgressPercent;
    property ProgressKind: FXProgressKind read GetProgressKind write SetProgressKind;
    property ProgressText: string read GetProgressText write SetProgressText;
    property SelectedIcon: FXStandardIconType read GetSelectedIcon write SetSelectedIcon;
    property ShowCancel: boolean read GetShowCancel write SetShowCancel;
  end;

  FXFormMessageTemplate = class(FXFillForm)
  private
    const
    BUTTON_HEIGHT = 50;
    BUTTON_WIDTH = 200;
    procedure OkClick(Sender: TObject);

    var
    FTitle,
    FText: string;

    Box_Title,
    Box_Text: FXTextBox;

    FIconKind: FXStandardIconType;
    FShowCancel: boolean;

    Button_Contain: FXPanel;

    Button_OK,
    Button_Cancel: FXButton;

    Icon_Box: FXStandardIcon;

    procedure CancelClick(Sender: TObject);

    // Setters
    procedure SetShowCancel(const Value: boolean);
    procedure SetTitle(const Value: string);
    procedure SetText(const Value: string);
    procedure SetIcon(const Value: FXStandardIconType);

  protected
    // Build
    procedure BuildControls; override;

    // Init
    procedure InitializeNewForm; override;

  published
    property Title: string read FTitle write SetTitle;
    property Text: string read FText write SetText;

    property IconKind: FXStandardIconType read FIconKind write SetIcon;
    property ShowCancel: boolean read FShowCancel write SetShowCancel;
  end;

implementation

{ FXFormUpdateTemplate }

procedure FXFormUpdateTemplate.BuildControls;
begin
  inherited;

  Main_Contain := FXScrollBox.Create(Container);
  with Main_Contain do
    begin
      Parent := Container;

      Align := alClient;
    end;

  with FXTextBox.Create(Main_Contain) do
    begin
      Parent := Main_Contain;
      Text := 'On mobile networks, data charges may occur. After the update, the application will re-open automatically.';

      Font.Size := 12;

      WordWrap := true;

      Align := alTop;
      AlignWithMargins := true;

      with Margins do
        begin
          Left := 75;
          Top := 5;
          Right := 5;
          Bottom := 5;
        end;
    end;

  Title_Box := FXTextBox.Create(Main_Contain);
  with Title_Box do
    begin
      Parent := Main_Contain;
      UpdateTitle;

      Font.Size := 18;

      WordWrap := true;

      Align := alTop;
      AlignWithMargins := true;

      with Margins do
        begin
          Left := 75;
          Top := 50;
          Right := 5;
          Bottom := 5;
        end;
    end;

  with TImage.Create(Main_Contain) do
    begin
      Parent := Main_Contain;
      Top := 50;
      Left := 5;
      Width := 50;
      Height := 50;

      Stretch := true;

      var P: TPngImage;

      P := TPngImage.Create;
      ConvertToPNG(Application.Icon, P);

      Picture.Graphic := P;
    end;

  // Buttons
  Button_Contain := FXPanel.Create(Container);
  with Button_Contain do
    begin
      Parent := Container;

      Height := 50;

      Align := alBottom;
    end;

  with FXButton.Create(Button_Contain) do
    begin
      Parent := Button_Contain;

      Height := BUTTON_HEIGHT;
      Width := BUTTON_WIDTH;
      Align := alRight;

      Anchors := [akBottom, akRight];

      with Image do
        begin
          Enabled := true;
          IconType := FXIconType.SegoeIcon;

          SelectSegoe := #$E118;
        end;

      Text := 'Download';
      OnClick := ButtonClick;
      Tag := 1;

      ButtonKind := FXButtonKind.Accent;
    end;

  Button_Cancel := FXButton.Create(Button_Contain);
  with Button_Cancel do
    begin
      Parent := Button_Contain;

      Height := BUTTON_HEIGHT;
      Width := BUTTON_WIDTH;
      Align := alRight;

      Anchors := [akBottom, akRight];

      Enabled := FAllowSnooze;

      with Image do
        begin
          Enabled := true;
          IconType := FXIconType.SegoeIcon;

          SelectSegoe := #$F2A8;
        end;

      Text := 'Snooze';
      OnClick := ButtonClick;
      Tag := 0;

      ButtonKind := FXButtonKind.Normal;

      AlignWithMargins := true;
      with Margins do
        begin
          Left := 0;
          Top := 0;
          Right := 15;
          Bottom := 0;
        end;
    end;

  // Progress
  Download_Progress := FXPanel.Create(Container);
  with Download_Progress do
    begin
      Parent := Container;

      Height := 50;

      Visible := false;

      Align := alBottom;
    end;

  Animated_Box := FXAnimatedTextBox.Create(Download_Progress);
  with Animated_Box do
    begin
      Parent := Download_Progress;
      AdderMode := true;
      AdderText := 'Downloading updates';

      Items.Add('');
      Items.Add('.');
      Items.Add('..');
      Items.Add('...');

      Font.Size := 12;

      WordWrap := true;

      LayoutHorizontal := TLayout.Center;

      Align := alTop;
      AlignWithMargins := true;

      with Margins do
        begin
          Left := 5;
          Top := 5;
          Right := 5;
          Bottom := 20;
        end;
    end;

  Progress := FXProgress.Create(Download_Progress);
  with Progress do
    begin
      Parent := Download_Progress;

      ProgressKind := FXProgressKind.Intermediate;

      Align := alTop;
    end;

  // Error
  Error_Contain := FXPanel.Create(Main_Contain);
  with Error_Contain do
    begin
      Parent := Main_Contain;

      Height := 150;

      Top := Main_Contain.Height;

      Visible := false;

      Align := alTop;

      AlignWithMargins := true;
      with Margins do
        begin
          Left := 0;
          Top := 50;
          Right := 0;
          Bottom := 0;
        end;
    end;

  with FXTextBox.Create(Error_Contain) do
    begin
      Parent := Error_Contain;

      Font.Size := 14;

      WordWrap := true;

      Align := alTop;
      AlignWithMargins := true;

      Text := 'Unfortunately an error occured and the installation will not be able to continue.';

      with Margins do
        begin
          Left := 75;
          Top := 5;
          Right := 5;
          Bottom := 5;
        end;
    end;

  with FXTextBox.Create(Error_Contain) do
    begin
      Parent := Error_Contain;

      Font.Size := 18;

      WordWrap := true;

      Align := alTop;

      Text := 'An error occured';

      AlignWithMargins := true;
      with Margins do
        begin
          Left := 75;
          Top := 5;
          Right := 5;
          Bottom := 5;
        end;
    end;

  with FXTextBox.Create(Error_Contain) do
    begin
      Parent := Error_Contain;
      Top := 5;
      Left := 5;
      Width := 50;
      Height := 50;

      Font.Size := 40;
      Font.Name := ThemeManager.IconFont;

      Text := #$EA39;

      with Margins do
        begin
          Left := 75;
          Top := 5;
          Right := 5;
          Bottom := 5;
        end;
    end;

  // Error Buttons
  Error_Buttons := FXPanel.Create(Container);
  with Error_Buttons do
    begin
      Parent := Container;

      Height := 50;
      Visible := false;

      Align := alBottom;
    end;

  with FXButton.Create(Error_Buttons) do
    begin
      Parent := Error_Buttons;

      Height := BUTTON_HEIGHT;
      Width := BUTTON_WIDTH;
      Align := alRight;

      Anchors := [akBottom, akRight];

      with Image do
        begin
          Enabled := true;
          IconType := FXIconType.SegoeIcon;

          SelectSegoe := #$E16F;
        end;

      AutoStateToggle := true;
      StateText := 'Copied!';
      with StateImage do
        begin
          Enabled := true;
          IconType := FXIconType.SegoeIcon;

          SelectSegoe := #$E001;
        end;

      Text := 'Copy error';
      OnClick := ButtonClick;
      Tag := 2;

      ButtonKind := FXButtonKind.Normal;

      AlignWithMargins := true;
      with Margins do
        begin
          Left := 0;
          Top := 0;
          Right := 15;
          Bottom := 0;
        end;
    end;

  with FXButton.Create(Error_Buttons) do
    begin
      Parent := Error_Buttons;

      Height := BUTTON_HEIGHT;
      Width := BUTTON_WIDTH;
      Align := alRight;

      Anchors := [akBottom, akRight];

      with Image do
        begin
          Enabled := true;
          IconType := FXIconType.SegoeIcon;

          SelectSegoe := #$E10A;
        end;

      Text := 'Close';
      OnClick := ButtonClick;
      Tag := 3;

      ButtonKind := FXButtonKind.Accent;
    end;
end;

function FXFormUpdateTemplate.BuildTempFile: string;
var
  S: string;
begin
  S := GenerateString(8, [TStrGenFlag.LowercaseLetters, TStrGenFlag.Numbers]);
  Result := ReplaceWinPath(Format('%%TEMP%%\updateinstall_%S.exe', [S]));
end;

function FXFormUpdateTemplate.DoDownload(var From: string; ToFile: string): boolean;
begin
  try
    Result := DownloadFile(From, ToFile);

    if not Result then
      LastErrorText := 'The file could not be downloaded.';
  except
    on E: Exception do
      begin
        Result := false;
        LastErrorText := E.Message;
      end;
  end;
end;

procedure FXFormUpdateTemplate.DoInstall(FromTempFile, Params: string);
begin
  ShellRun( FromTempFile, Params )
end;

procedure FXFormUpdateTemplate.InitializeNewForm;
begin
  inherited;
  FAppName := 'Test Codrut Fluent Application';
  FAllowSnooze := true;
end;

procedure FXFormUpdateTemplate.SetAllowSnooze(const Value: boolean);
begin
  FAllowSnooze := Value;

  Button_Cancel.Enabled := Value;
end;

procedure FXFormUpdateTemplate.SetAppName(const Value: string);
begin
  FAppName := Value;

  UpdateTitle;
end;

procedure FXFormUpdateTemplate.ShowError(Title, Text: string);
begin
  Download_Progress.Visible := false;

  Button_Contain.Visible := false;
  Error_Contain.Visible := true;
  Error_Buttons.Visible := true;
end;

procedure FXFormUpdateTemplate.ButtonClick(Sender: TObject);
begin
  case FXButton(Sender).Tag of
    0, 3: Hide;
    1: begin
      Button_Contain.Hide;
      Download_Progress.Show;

      ThreadedInstall;
    end;
    2: Clipboard.AsText := LastErrorText;
  end;
end;

procedure FXFormUpdateTemplate.ThreadedInstall;
begin
  BuildTempFile;

  FInstallThread := TThread.CreateAnonymousThread(procedure
    begin
      var AFilePath: string;
      AFilePath := BuildTempFile;

      // Wait
      Sleep(ONE_SECOND);

      // Download
      if not DoDownload( FDownloadURL, AFilePath ) then
        begin
          TThread.Synchronize(nil, procedure
          begin
            // Show download error
            ShowError('Download error', 'Unfortunately a download error occured and the install cannot continue. Try again at a later time.');
          end);

          Exit;
        end;

      // Status
      TThread.Synchronize(nil, procedure
        begin
          Animated_Box.AdderText := 'Installing';
        end);

      // Wait
      Sleep(ONE_SECOND);

      // Install
      TThread.Synchronize(nil, procedure
        begin
          DoInstall( AFilePath, FParams );
        end);

      { Auto deletion is handeled by installer }

      // Statis
      TThread.Synchronize(nil, procedure
        begin
          Animated_Box.AdderText := 'Closing';
        end);

      // Close app
      Sleep(FIVE_SECOND);

      TThread.Synchronize(nil, procedure
        begin
          Application.MainForm.Close;
        end);
    end);

  with FInstallThread do
    begin
      FreeOnTerminate := true;
      Start;
    end;
end;

procedure FXFormUpdateTemplate.UpdateTitle;
begin
  Title_Box.Text := Format('%S needs an update. Would you like to download the update now?', [FAppName]);
end;

{ FXFormMessageTemplate }

procedure FXFormMessageTemplate.BuildControls;
begin
  inherited;

  Box_Text := FXTextBox.Create(Container);
  with Box_Text do
    begin
      Parent := Container;
      Text := Text;

      Font.Size := 14;

      WordWrap := true;

      Align := alTop;
      AlignWithMargins := true;

      with Margins do
        begin
          Left := 75;
          Top := 5;
          Right := 5;
          Bottom := 5;
        end;
    end;

  Box_Title := FXTextBox.Create(Container);
  with Box_Title do
    begin
      Parent := Container;
      Text := Title;

      Font.Size := 22;

      WordWrap := true;

      Align := alTop;
      AlignWithMargins := true;

      with Margins do
        begin
          Left := 75;
          Top := 50;
          Right := 5;
          Bottom := 5;
        end;
    end;

  Icon_Box :=  FXStandardIcon.Create(Container);
  with Icon_Box do
    begin
      Parent := Container;
      Top := 50;
      Left := 5;
      Width := 50;
      Height := 50;

      SelectedIcon := IconKind;
    end;

  // Buttons
  Button_Contain := FXPanel.Create(Container);
  with Button_Contain do
    begin
      Parent := Container;

      Height := 50;

      Align := alBottom;
    end;

  Button_OK := FXButton.Create(Button_Contain);
  with Button_OK do
    begin
      Parent := Button_Contain;

      Height := BUTTON_HEIGHT;
      Width := BUTTON_WIDTH;
      Top := 0;
      Left := Container.Width - Width - 10;

      Anchors := [akBottom, akRight];

      with Image do
        begin
          Enabled := true;
          IconType := FXIconType.SegoeIcon;

          SelectSegoe := #$E001;
        end;

      Text := 'Okay';
      OnClick := OkClick;

      ButtonKind := FXButtonKind.Accent;
    end;

  Button_Cancel := FXButton.Create(Button_Contain);
  with Button_Cancel do
    begin
      Parent := Button_Contain;

      Height := BUTTON_HEIGHT;
      Width := BUTTON_WIDTH;
      Top := 0;
      Left := Container.Width - Width - 10 - BUTTON_WIDTH - 10;

      Anchors := [akBottom, akRight];

      Visible := ShowCancel;

      with Image do
        begin
          Enabled := true;
          IconType := FXIconType.SegoeIcon;

          SelectSegoe := #$E10A;
        end;

      Text := 'Cancel';
      OnClick := CancelClick;

      ButtonKind := FXButtonKind.Normal;
    end;
end;

procedure FXFormMessageTemplate.OkClick(Sender: TObject);
begin
  Close;
end;

procedure FXFormMessageTemplate.InitializeNewForm;
begin
  inherited;
  FTitle := 'Message Title';
  FText := 'Message text. Read very carefully.';
  FShowCancel := true;
end;

procedure FXFormMessageTemplate.SetIcon(const Value: FXStandardIconType);
begin
  FIconKind := Value;

  Icon_Box.SelectedIcon := Value;
end;

procedure FXFormMessageTemplate.SetShowCancel(const Value: boolean);
begin
  FShowCancel := Value;

  Button_Cancel.Visible := Value;
end;

procedure FXFormMessageTemplate.SetText(const Value: string);
begin
  FText := Value;

  Box_Text.Text := Value;
end;

procedure FXFormMessageTemplate.SetTitle(const Value: string);
begin
  FTitle := Value;

  Box_Title.Text := Value;
end;

procedure FXFormMessageTemplate.CancelClick(Sender: TObject);
begin
  Close;
end;

{ FXTaskExecutingTemplate }

procedure FXTaskExecutingTemplate.AlignLayout;
begin
  if Dialog_Icon.Visible then begin
    Title_Box.Margins.Left := 75;
    Text_Box.Margins.Left := 75;
  end else begin
    Title_Box.Margins.Left := 5;
    Text_Box.Margins.Left := 5;
  end;
end;

procedure FXTaskExecutingTemplate.BuildControls;
begin
  inherited;

  Main_Contain := FXScrollBox.Create(Container);
  with Main_Contain do
    begin
      Parent := Container;

      Align := alClient;
    end;

  Text_Box := FXTextBox.Create(Main_Contain);
  with Text_Box do
    begin
      Parent := Main_Contain;
      Text := 'Text'; // default

      Font.Size := 12;

      WordWrap := true;

      Align := alTop;
      AlignWithMargins := true;

      with Margins do
        begin
          Left := 75;
          Top := 5;
          Right := 5;
          Bottom := 5;
        end;
    end;

  Title_Box := FXTextBox.Create(Main_Contain);
  with Title_Box do
    begin
      Parent := Main_Contain;
      Text := 'Title'; // default

      Font.Size := 18;

      WordWrap := true;

      Align := alTop;
      AlignWithMargins := true;

      with Margins do
        begin
          Left := 75;
          Top := 50;
          Right := 5;
          Bottom := 5;
        end;
    end;

  Dialog_Icon := FXStandardIcon.Create(Main_Contain);
  with Dialog_Icon do
    begin
      Parent := Main_Contain;
      Proportional := false;
      Top := 50;
      Left := 5;
      Width := 50;
      Height := 50;

      var P: TPngImage;

      P := TPngImage.Create;
      ConvertToPNG(Application.Icon, P);

      SelectedIcon := FXStandardIconType.Information; // default
      Visible := SelectedIcon <> FXStandardIconType.None;
    end;

  // Buttons
  Button_Contain := FXPanel.Create(Container);
  with Button_Contain do
    begin
      Parent := Container;

      Height := 50;

      Align := alBottom;
      Visible := true; // default
    end;

  Button_Cancel := FXButton.Create(Button_Contain);
  with Button_Cancel do
    begin
      Parent := Button_Contain;

      Height := BUTTON_HEIGHT;
      Width := BUTTON_WIDTH;
      Align := alRight;

      Anchors := [akBottom, akRight];

      with Image do
        begin
          Enabled := true;
          IconType := FXIconType.SegoeIcon;

          SelectSegoe := #$E711;
        end;

      Text := 'Cancel';
      OnClick := ButtonClick;

      ButtonKind := FXButtonKind.Normal;

      AlignWithMargins := true;
      with Margins do
        begin
          Left := 0;
          Top := 0;
          Right := 15;
          Bottom := 0;
        end;
    end;

  // Progress
  Download_Progress := FXPanel.Create(Container);
  with Download_Progress do
    begin
      Parent := Container;

      Height := 50;

      Align := alBottom;
    end;

  Animated_Box := FXAnimatedTextBox.Create(Download_Progress);
  with Animated_Box do
    begin
      Parent := Download_Progress;
      AdderMode := true;
      AdderText := 'Loading'; // default

      Items.Add('');
      Items.Add('.');
      Items.Add('..');
      Items.Add('...');

      Font.Size := 12;

      WordWrap := true;

      LayoutHorizontal := TLayout.Center;

      Align := alTop;
      AlignWithMargins := true;

      with Margins do
        begin
          Left := 5;
          Top := 5;
          Right := 5;
          Bottom := 20;
        end;
    end;

  Progress := FXProgress.Create(Download_Progress);
  with Progress do
    begin
      Parent := Download_Progress;

      ProgressKind := FXProgressKind.Intermediate;

      Align := alTop;
    end;

  // Layout
  AlignLayout;
end;

procedure FXTaskExecutingTemplate.ButtonClick(Sender: TObject);
begin
  FCanceled := true;
  if Assigned(FOnCancel) then
    FOnCancel(Self);
end;

function FXTaskExecutingTemplate.GetProgressKind: FXProgressKind;
begin
  Result := Progress.ProgressKind;
end;

function FXTaskExecutingTemplate.GetProgressPercent: FXPercent;
begin
  Result := Progress.Value;
end;

function FXTaskExecutingTemplate.GetProgressText: string;
begin
  Result := Animated_Box.AdderText;
end;

function FXTaskExecutingTemplate.GetSelectedIcon: FXStandardIconType;
begin
  Result := Dialog_Icon.SelectedIcon;
end;

function FXTaskExecutingTemplate.GetShowCancel: boolean;
begin
  Result := Button_Contain.Visible;
end;

function FXTaskExecutingTemplate.GetText: string;
begin
  Result := Text_Box.Text;
end;

function FXTaskExecutingTemplate.GetTitle: string;
begin
  Result := Title_Box.Text;
end;

procedure FXTaskExecutingTemplate.InitializeNewForm;
begin
  inherited;

  FCanceled := false;
end;

procedure FXTaskExecutingTemplate.SetProgressKind(const Value: FXProgressKind);
begin
  Progress.ProgressKind := Value;
end;

procedure FXTaskExecutingTemplate.SetProgressPercent(const Value: FXPercent);
begin
  Progress.Value := Value;
end;

procedure FXTaskExecutingTemplate.SetProgressText(const Value: string);
begin
  Animated_Box.AdderText := Value;
end;

procedure FXTaskExecutingTemplate.SetSelectedIcon(
  const Value: FXStandardIconType);
begin
  Dialog_Icon.SelectedIcon := Value;
  Dialog_Icon.Visible := SelectedIcon <> FXStandardIconType.None;

  AlignLayout;
end;

procedure FXTaskExecutingTemplate.SetShowCancel(const Value: boolean);
begin
  Button_Contain.Visible := Value;
end;

procedure FXTaskExecutingTemplate.SetText(const Value: string);
begin
  Text_Box.Text := Value;
end;

procedure FXTaskExecutingTemplate.SetTitle(const Value: string);
begin
  Title_Box.Text := Value;
end;

end.
