unit CFX.FormClasses;

interface
  uses
    Windows,
    Vcl.Graphics,
    Classes,
    Types,
    Vcl.Clipbrd,
    CFX.Types,
    CFX.UIConsts,
    SysUtils,
    CFX.Colors,
    Vcl.Forms,
    CFX.Graphics,
    CFX.VarHelpers,
    CFX.ThemeManager,
    Vcl.Controls,
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
    FXFillForm = class(TForm, FXControl)
      private
        FCustomColors: FXColorSets;
        FDrawColors: FXColorSet;
        FThemeChange: FXThemeChange;
        FParentForm: TForm;
        FFillMode: FXFormFill;
        FCloseAction: FXFormCloseAction;
        FTitlebarHeight: integer;

        var Margin: integer;
        var Container: FXPanel;

        // Setters
        procedure SetParentForm(const Value: TForm);
        procedure SetFillMode(const Value: FXFormFill);

      protected
        procedure InitializeNewForm; override;

        procedure BuildControls; virtual;
        procedure Resize; override;

        procedure DoClose(var Action: TCloseAction); override;

        procedure ApplyFillMode;
        procedure ApplyMargins;

        // Mouse
        procedure MouseDown(Button : TMouseButton; Shift: TShiftState; X, Y : integer); override;

      published
        property CustomColors: FXColorSets read FCustomColors write FCustomColors;

        // Parent
        property ParentForm: TForm read FParentForm write SetParentForm;

        // Fill Form
        property FillMode: FXFormFill read FFillMode write SetFillMode;
        property CloseAction: FXFormCloseAction read FCloseAction write FCloseAction;

        // Theming Engine
        property OnThemeChange: FXThemeChange read FThemeChange write FThemeChange;

      public
        constructor Create(aOwner: TComponent); override;
        constructor CreateNew(aOwner: TComponent; Dummy: Integer = 0); override;
        destructor Destroy; override;

        procedure InitForm;

        // Procedures
        procedure SetBoundsRect(Bounds: TRect);

        // Interface
        function IsContainer: Boolean;
        procedure UpdateTheme(const UpdateChildren: Boolean);

        function Background: TColor;
    end;

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

        // Position
        procedure Resize; override;

        // Init
        procedure InitializeNewForm; override;

      published
        property AppName: string read FAppName write SetAppName;
        property AllowSnooze: boolean read FAllowSnooze write SetAllowSnooze;

        property DownloadURL: string read FDownloadURL write FDownloadURL;
        property InstallParameters: string read FParams write FParams;
    end;

    FXFormMessageTemplate = class(FXFillForm)
      private
        const
        BUTTON_HEIGHT = 50;
        BUTTON_WIDTH = 200;

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

        procedure OkClick(Sender: TObject);
        procedure CancelClick(Sender: TObject);

        // Setters
        procedure SetShowCancel(const Value: boolean);
        procedure SetTitle(const Value: string);
        procedure SetText(const Value: string);
        procedure SetIcon(const Value: FXStandardIconType);

      protected
        // Build
        procedure BuildControls; override;

        // Position
        procedure Resize; override;

        // Init
        procedure InitializeNewForm; override;

      published
        property Title: string read FTitle write SetTitle;
        property Text: string read FText write SetText;

        property IconKind: FXStandardIconType read FIconKind write SetIcon;
        property ShowCancel: boolean read FShowCancel write SetShowCancel;
    end;

implementation

{ FXFillForm }

procedure FXFillForm.ApplyFillMode;
begin
  FTitlebarHeight := FXForm(ParentForm).GetTitlebarHeight;

  if FFillMode = FXFormFill.Complete then
    SetBoundsRect( ParentForm.ClientRect )
  else
    begin
      var ARect: TRect;

      ARect := ParentForm.ClientRect;
      ARect.Top := FTitlebarHeight;

      SetBoundsRect( ARect );
    end;
end;

procedure FXFillForm.ApplyMargins;
begin
  with Container do
    begin
      LockDrawing;

      with Margins do
        begin
          Top := Margin;
          Left := Margin;
          Right := Margin;
          Bottom := Margin;

          case FFillMode of
            FXFormFill.Complete: Top := Top + FTitlebarHeight;
            //FXFormFill.TitleBar: ;
          end;
        end;

      UnlockDrawing;
    end;
end;

function FXFillForm.Background: TColor;
begin
  Result := Color;
end;

procedure FXFillForm.BuildControls;
begin
  // nothing
end;

constructor FXFillForm.Create(aOwner: TComponent);
begin
  inherited;

  // Initialise
  InitForm;
end;

constructor FXFillForm.CreateNew(aOwner: TComponent; Dummy: Integer);
begin
  inherited;

  if aOwner is TForm then
    ParentForm := TForm(aOwner);

  // Initialise
  InitForm;
end;

destructor FXFillForm.Destroy;
begin

  inherited;
end;

procedure FXFillForm.DoClose(var Action: TCloseAction);
begin
  case FCloseAction of
    FXFormCloseAction.Hide: Action := TCloseAction.caHide;
    else Action := TCloseAction.caFree;
  end;

  inherited;
end;

procedure FXFillForm.InitForm;
begin
  // Default
  Margin := 20;
  DoubleBuffered := true;

  // Settings
  Position := poDesigned;
  BorderStyle := bsNone;
  Caption := '';
  BorderIcons := [];
  AlphaBlend := True;
  Anchors := [akTop, akLeft, akBottom, akRight];

  // Fill Mode
  ApplyFillMode;

  // Container
  Container := FXPanel.Create(Self);
  with Container do
    begin
      Parent := Self;

      Align := alClient;
      AlignWithMargins := true;
    end;
  ApplyMargins;

  // Build
  BuildControls;

  // Update Theme
  UpdateTheme(true);
end;

procedure FXFillForm.InitializeNewForm;
begin
  inherited;
  // Create Classes
  FCustomColors := FXColorSets.Create(Self);
  FDrawColors := FXColorSet.Create(ThemeManager.SystemColorSet, ThemeManager.DarkTheme);
end;

function FXFillForm.IsContainer: Boolean;
begin
  Result := true;
end;

procedure FXFillForm.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: integer);
begin
  inherited;
  ReleaseCapture;
  SendMessage(ParentForm.Handle, WM_NCLBUTTONDOWN, HTCAPTION, 0);
end;

procedure FXFillForm.Resize;
begin
  inherited;
  // next
end;

procedure FXFillForm.SetBoundsRect(Bounds: TRect);
begin
  SetBounds(Bounds.Left, Bounds.Top, Bounds.Width, Bounds.Height);
end;

procedure FXFillForm.SetFillMode(const Value: FXFormFill);
begin
  if FFillMode <> Value then
    begin
      FFillMode := Value;

      ApplyFillMode;
      ApplyMargins;
    end;
end;

procedure FXFillForm.SetParentForm(const Value: TForm);
begin
  FParentForm := Value;
  Parent := Value;
end;

procedure FXFillForm.UpdateTheme(const UpdateChildren: Boolean);
var
  i: integer;
begin
  // Update Colors
  if CustomColors.Enabled then
    begin
      FDrawColors.Background := ExtractColor( CustomColors, FXColorType.BackGround );
      FDrawColors.Foreground := ExtractColor( CustomColors, FXColorType.Foreground );
    end
  else
    begin
      FDrawColors.Background := ThemeManager.SystemColor.BackGround;
      FDrawColors.Foreground := ThemeManager.SystemColor.ForeGround;
    end;

  Container.CustomColors.Assign( CustomColors );

  // Color
  Color := FDrawColors.BackGround;

  //  Update tooltip style
  if ThemeManager.DarkTheme then
    HintWindowClass := FXDarkTooltip
  else
    HintWindowClass := FXLightTooltip;

  // Font Color
  Font.Color := FDrawColors.Foreground;

  // Notify Theme Change
  if Assigned(FThemeChange) then
    FThemeChange(Self, FXThemeType.AppTheme, ThemeManager.DarkTheme, ThemeManager.AccentColor);

  // Update children
  if IsContainer and UpdateChildren then
    begin
      LockWindowUpdate(Handle);
      for i := 0 to ComponentCount -1 do
        if Supports(Components[i], FXControl) then
          (Components[i] as FXControl).UpdateTheme(UpdateChildren);
      LockWindowUpdate(0);
    end;
end;

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
      Text := 'On mobile network, data charges may occur. Any charges are not calculated. After the update, the application will re-open automatically.';

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
      AdderText := 'Downloading Updates';

      Items.Add('');
      Items.Add('.');
      Items.Add('..');
      Items.Add('...');

      Font.Size := 12;

      WordWrap := true;

      LayoutHorizontal := FXLayout.Center;

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

      Caption := 'Unfortunately an error occured and the installation will not be able to continue.';

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

      Caption := 'An error occured';

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

      Caption := #$EA39;

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
  S := GenerateString(8, true, false, true, false);
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

procedure FXFormUpdateTemplate.Resize;
begin
  inherited;

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

procedure FXFormMessageTemplate.Resize;
begin
  inherited;

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

end.
