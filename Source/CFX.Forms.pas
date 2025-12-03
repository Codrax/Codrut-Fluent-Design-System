unit CFX.Forms;

interface

uses
  SysUtils,
  Classes,
  Winapi.Windows,
  Winapi.Dwmapi,
  CFX.Windows.DarkmodeApi,
  CFX.Windows.DarkmodeApi.Types,
  Types,
  CFX.ToolTip,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.Dialogs,
  Winapi.Messages,
  CFX.ThemeManager,
  CFX.Colors,
  CFX.Constants,
  CFX.Animation.Main,
  CFX.Animation.Component,
  Vcl.TitleBarCtrls,
  CFX.Utilities,
  Vcl.ExtCtrls,
  CFX.TitlebarPanel,
  CFX.Classes,
  CFX.ComponentClasses,
  CFX.Types,
  CFX.Messages,
  CFX.Linker;

type
  // Proc
  FXFormProcedure = procedure(Sender: TObject) of object;

  // Types define
  FXThemeType = CFX.Types.FXThemeType;

  // Form
  FXCustomForm = class(TForm, IFXComponent, IFXControl)
  private
    FCustomColors: FXColorSets;
    FDrawColors: FXColorSet;

    FWindowUpdateLock: boolean;

    // Settings
    FRestoredPosition: TRect;
    FRestoredBorder: TBorderStyle;

    FMicaEffect: boolean;
    FSmokeEffect: boolean;
    FAllowThemeChangeAnim: boolean;

    FCornerPreference: FXFormCornerPreference;
    FCornerPreferenceCustomized: FXRoundEllipseSettings;

    // Notify
    FThemeChange: FXThemeChange;
    FOnMove: FXFormProcedure;

    // Status
    FDestColor: TColor;

    // DWM Backdrop
    FBackdrop: FXFormBackdropType;

    // Titlebar
    FTitlebarInitialized: boolean;
    FEnableTitlebar: boolean;
    TTlCtrl: TCustomTitleBarpanel;
    FDisableTitlebarAlign: boolean;
    FDisableCustomTitlebar: boolean;
    FManualCustomTitleBar: boolean;
    FBackground: FXBackgroundColor;
    FPostTitlebarEnableTransparentValue: boolean;

    // Mica
    FPostMicaBlend: boolean;
    FPostMicaBlendValue: byte;

    // Smoke
    Smoke: TForm;

    // Prepare
    procedure CreateSmokeSettings;

    // Messages
    procedure WM_SysCommand(var Msg: TWMSysCommand); message WM_SYSCOMMAND;
    procedure WM_DWMColorizationColorChanged(var Msg: TMessage); message WM_DWMCOLORIZATIONCOLORCHANGED;
    procedure WM_Activate(var Msg: TWMActivate); message WM_ACTIVATE;
    procedure WM_MOVE(var Msg: TWMMove); message WM_MOVE;
    procedure WM_SIZE(var Msg: TWMSize); message WM_SIZE;
    procedure WM_GETMINMAXINFO(var Msg: TMessage); message WM_GETMINMAXINFO;

    procedure WMShowWindow(var Message: TWMShowWindow); message WM_SHOWWINDOW;
    procedure WMClose(var Message: TWMClose); message WM_CLOSE;

    procedure QuickBroadcast(MessageID: integer);

    // Setters
    procedure SetMicaEffect(const Value: boolean);
    procedure SetSmokeEffect(const Value: boolean);
    procedure SetWindowUpdateLock(const Value: boolean);
    procedure SetBackgroundColor(const Value: FXBackgroundColor);

    // Utilities
    procedure FormCloseIgnore(Sender: TObject; var CanClose: Boolean);
    procedure SetCornerPreference(const Value: FXFormCornerPreference);
    procedure SetDisableCustomTitlebar(const Value: boolean);
    procedure SetBackdrop(const Value: FXFormBackdropType);

  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
    procedure Resize; override;

    // Utils
    function HasActiveCustomTitleBar: boolean;
    function IsWindowSnapped: boolean;
    function GetWindowNormalPosition: TRect;

    // Sizing
    function GetClientRect: TRect; override;
    procedure AdjustClientRect(var Rect: TRect); override;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;

    // Catch events
    procedure CornerPreferenceCustomizedChanged(Sender: TObject);

    // Do
    procedure DoShow; override;
    procedure DoMove; virtual;
    procedure DoSize(var AWidth, AHeight: word); virtual;

    // Can
    function CanMove(Position: TPoint): boolean; virtual;

    // Initialization
    procedure InitForm; virtual;

    // Override
    procedure InitializeNewForm; override;

  published
    property MicaEffect: boolean read FMicaEffect write SetMicaEffect default false;
    property SmokeEffect: boolean read FSmokeEffect write SetSmokeEffect default false;
    property CustomColors: FXColorSets read FCustomColors write FCustomColors;
    property AllowThemeChangeAnimation: boolean read FAllowThemeChangeAnim write FAllowThemeChangeAnim default false;

    // Experimental feature - DO NOT USE
    property Backdrop: FXFormBackdropType read FBackdrop write SetBackdrop;

    property CornerPreference: FXFormCornerPreference read FCornerPreference write SetCornerPreference default FXFormCornerPreference.Default;
    property CornerPreferenceCustomized: FXRoundEllipseSettings read FCornerPreferenceCustomized write FCornerPreferenceCustomized;

    property WindowUpdateLocked: boolean read FWindowUpdateLock write SetWindowUpdateLock;

    // Do not adjust the ClientRect with the titlebar's height
    property DisableTitlebarAlign: boolean read FDisableTitlebarAlign write FDisableTitlebarAlign default false;
    // Disable custom titlebar entirely
    property DisableCustomTitlebar: boolean read FDisableCustomTitlebar write SetDisableCustomTitlebar default false;
    // Manage the styling options of the titlebar yourself
    property ManualCustomTitleBar: boolean read FManualCustomTitleBar write FManualCustomTitleBar default false;
    property BackgroundColor: FXBackgroundColor read FBackground write SetBackgroundColor;

    // On Change...
    property OnMove: FXFormProcedure read FOnMove write FOnMove;

    // Theming Engine
    property OnThemeChange: FXThemeChange read FThemeChange write FThemeChange;

  public
    function CloseQuery: Boolean; override;

    // Procedures
    procedure SetBoundsRect(Bounds: TRect);
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

    // Utils
    function IsResizable: Boolean;

    function GetTitlebarHeight: integer;

    // Draw
    procedure Redraw;

    // Interface
    function IsContainer: Boolean;
    procedure UpdateTheme(const UpdateChildren: Boolean);

    function Background: TColor;

    // Constructors
    constructor Create(aOwner: TComponent); override;
    constructor CreateNew(aOwner: TComponent; Dummy: Integer = 0); override;
    destructor Destroy; override;
  end;

  { Specialized for Application Windows (MainWindow) }
  FXForm = class(FXCustomForm)
  private
    FFullScreen: Boolean;

    // Defaults
    FDefaultConstraints: boolean;

    // Setters
    procedure SetFullscreen(const Value: boolean);

  protected
    // Initialization
    procedure InitForm; override;

  published
    property DefaultConstraints: boolean read FDefaultConstraints write FDefaultConstraints;
    property FullScreen: Boolean read FFullScreen write SetFullScreen default false;

  public
  end;

  { Specialized for Dialogs }
  FXDialogForm = class(FXCustomForm)
  private
    FAutoCenter: boolean;
    FAutoSmoke: boolean;
    FAutoMoveParent: boolean;
    FParentForm: TForm;

    FCanMoveParent: boolean;

    // Utils
    procedure CenterDialogInParentForm;

  protected
    // Initialization (after form creation)
    procedure InitForm; override;

    // Params
    procedure CreateParams(var Params: TCreateParams); override;

    // CM Messages
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;

    // Do
    procedure DoShow; override;

    // Can
    function CanMove(Position: TPoint): boolean; override;
    function CanResize(var NewWidth, NewHeight: Integer): Boolean; override;

  public
    property ParentForm: TForm read FParentForm write FParentForm;

    // Props
    property AutoCenter: boolean read FAutoCenter write FAutoCenter;
    property AutoSmoke: boolean read FAutoSmoke write FAutoSmoke;
    property AutoMoveParent: boolean read FAutoMoveParent write FAutoMoveParent;

    // Modal
    function ShowModal: Integer; override;
  end;

  { Specialized for Popups }
  FXPopupForm = class(FXCustomForm)
  private
    FLightDismiss: boolean;
    FFreeOnClose: boolean;

  protected
    // Initialization (after form creation)
    procedure InitForm; override;

    // Do
    procedure DoShow; override;
    procedure DoClose(var Action: TCloseAction); override;

    // Can
    function CanMove(Position: TPoint): boolean; override;
    function CanResize(var NewWidth, NewHeight: Integer): Boolean; override;

    // Params
    procedure CreateParams(var Params: TCreateParams); override;

    // Messages
    procedure WMKeyDown(var Msg: TWMKeyDown); message WM_KEYDOWN;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMActivate(var Msg: TWMActivate); message WM_ACTIVATE;

  public
    property LightDismiss: boolean read FLightDismiss write FLightDismiss;
    property FreeOnClose: boolean read FFreeOnClose write FFreeOnClose;

    procedure Show(Position: TPoint); reintroduce; overload;
    procedure ShowAtCursor;
    procedure ShowAtControl(Executor: TControl; HorizontalOffset: integer=0; VerticalOffset: integer=0); overload;

    // Modal
    function ShowModal: Integer; overload; override;
    function ShowModal(Position: TPoint): Integer; reintroduce; overload;

    function ShowModalAtCursor: Integer;
    function ShowModalAtControl(Executor: TControl; HorizontalOffset: integer=0; VerticalOffset: integer=0): Integer; overload;
  end;

implementation

{ FXCustomForm }

procedure FXCustomForm.AdjustClientRect(var Rect: TRect);
begin
  inherited;
  if DisableTitlebarAlign then
    Dec(Rect.Top, GlassFrame.Top);
end;

function FXCustomForm.Background: TColor;
begin
  Result := Color;
end;

function FXCustomForm.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := inherited;
end;

function FXCustomForm.CanMove(Position: TPoint): boolean;
begin
  Result := true;
end;

function FXCustomForm.CloseQuery: Boolean;
begin
  Result := inherited;

  // Enable close animation
  if Result then
    if MicaEffect then
      AlphaBlend := false;
end;

procedure FXCustomForm.CornerPreferenceCustomizedChanged(Sender: TObject);
begin
  if CornerPreference <> FXFormCornerPreference.Customized then
    Exit;

  // Update region
  var Rgn: HRGN;
  Rgn := CreateRoundRectRgn(0, 0, Width, Height,
    FCornerPreferenceCustomized.RoundWidth, FCornerPreferenceCustomized.RoundHeight);
  SetWindowRgn(Handle, Rgn, True);
end;

constructor FXCustomForm.Create(aOwner: TComponent);
begin
  // Create Form and Components
  inherited;

  // Initialise
  InitForm;
end;

constructor FXCustomForm.CreateNew(aOwner: TComponent; Dummy: Integer);
begin
  inherited;

  // Initialise
  InitForm;
end;

procedure FXCustomForm.CreateParams(var Params: TCreateParams);
begin
  inherited;

  //Params.Style := Params.Style or 200000;
end;

procedure FXCustomForm.CreateSmokeSettings;
begin
  Smoke := TForm.Create(nil);
  Smoke.Position := poDesigned;
  Smoke.WindowState := wsMaximized;
  Smoke.Parent := Self;
  Smoke.BorderStyle := bsNone;
  Smoke.Caption := '';
  Smoke.BorderIcons := [];
  Smoke.OnCloseQuery := FormCloseIgnore;
  Smoke.AlphaBlend := True;
  Smoke.AlphaBlendValue := FORM_SMOKE_BLEND_VALUE;
  Smoke.Color := clBlack;
end;

destructor FXCustomForm.Destroy;
begin
  FreeAndNil(FCustomColors);
  FreeAndNil(FDrawColors);
  FreeAndNil(FCornerPreferenceCustomized);

  inherited;
end;

procedure FXCustomForm.DoMove;
begin
  if Assigned(FOnMove) then
    FOnMove(Self);
end;

procedure FXCustomForm.DoShow;
begin
  inherited;
  if not FTitlebarInitialized and not FDisableCustomTitlebar then
    with Self.CustomTitleBar do
      begin
        Enabled := true;

        if not FManualCustomTitleBar then begin
          SystemButtons := false;
          SystemColors := false;
          SystemHeight := false;

          Height := TTlCtrl.Height;
        end;

        Control := TTlCtrl;

        FTitlebarInitialized := true;

        UpdateTheme(false);
      end;
end;

procedure FXCustomForm.DoSize(var AWidth, AHeight: word);
begin
  //
end;

procedure FXCustomForm.InitForm;
label
  skip_titlebar;
var
  I: Integer;
begin
  FDisableTitlebarAlign := false;
  FDisableCustomTitlebar := false;

  // Settings
  Font.Name := ThemeManager.FormFont;
  Font.Height := ThemeManager.FormFontHeight;

  // Effects
  if not AlphaBlend and (AlphaBlendValue = 255) then // enable mica effect by default if values are default
    MicaEffect := true;
  CreateSmokeSettings;
  FAllowThemeChangeAnim := false;

  // Corner preference
  FCornerPreference := FXFormCornerPreference.Default;
  FCornerPreferenceCustomized := FXRoundEllipseSettings.Create(Self);
  FCornerPreferenceCustomized.OnChange := CornerPreferenceCustomizedChanged;

  // TitleBar
  FTitlebarInitialized := false;
  FEnableTitlebar := NTKernelVersion >= 6.0;
  if not FEnableTitlebar then
    begin
      FTitlebarInitialized := true;
      goto skip_titlebar;
    end;

  (* Scan for existing *)
  for I := 0 to ControlCount - 1 do
    if Controls[I] is TCustomTitleBarPanel then
      begin
        TTlCtrl := TCustomTitleBarPanel(Controls[I]);
        Break;
      end;

  (*Create New*)
  if TTlCtrl = nil then
    begin
      TTlCtrl := FXTitleBarPanel.Create(Self);
      TTlCtrl.Parent := Self;
    end;

  (* Assign *)
  if not Assigned(CustomTitleBar.Control) then
    CustomTitleBar.Control := TTlCtrl;

  (* Edit prop *)
  TTlCtrl.Visible := not FDisableCustomTitlebar;

  // Title Bar End
  skip_titlebar:

  // Needs custom title bar
    FPostTitlebarEnableTransparentValue := TransparentColor;
  CustomTitleBar.Enabled := not FDisableCustomTitlebar;

  // Update Theme
  UpdateTheme(true); // also update children
end;

procedure FXCustomForm.InitializeNewForm;
begin
  inherited;
  // Create Classes
  FCustomColors := FXColorSets.Create(Self);
  FDrawColors := FXColorSet.Create(ThemeManager.SystemColorSet, ThemeManager.DarkTheme);
end;

function FXCustomForm.IsContainer: Boolean;
begin
  Result := true;
end;

function FXCustomForm.IsResizable: Boolean;
begin
  Result := BorderStyle in [bsSizeable, bsSizeToolWin];
end;

function FXCustomForm.IsWindowSnapped: boolean;
var
  P: TWindowPlacement;
begin
  Result := false;

  // Get the window placement
  if not GetWindowPlacement(Handle, P) then
    Exit;

  // Check if the window is maximized or has different normal position
  if (P.showCmd = SW_SHOWMAXIMIZED) or
     ((P.rcNormalPosition.Left <> BoundsRect.Left) and (P.rcNormalPosition.Right <> BoundsRect.Right)) or
     ((P.rcNormalPosition.Top <> BoundsRect.Top) and (P.rcNormalPosition.Bottom <> BoundsRect.Bottom)) then
  begin
    Result := True; // The window is snapped
  end;
end;

procedure FXCustomForm.FormCloseIgnore(Sender: TObject; var CanClose: Boolean);
begin
  if SmokeEffect then
    CanClose := false;
end;

function FXCustomForm.GetClientRect: TRect;
begin
  Result := inherited;
end;

function FXCustomForm.GetTitlebarHeight: integer;
begin
  if CustomTitleBar.Enabled then
    Result := CustomTitleBar.Height
  else
    Result := 0;
  {Result := 0;
  for I := 0 to ControlCount - 1 do
    if Controls[I] is TCustomTitleBarPanel then
      Result := TCustomTitleBarPanel(Controls[I]).Height; }
end;

function FXCustomForm.GetWindowNormalPosition: TRect;
var
  P: TWindowPlacement;
begin
  Result := TRect.Empty;

  // Get the window placement
  if not GetWindowPlacement(Handle, P) then
    Exit;

  Result := P.rcNormalPosition;
end;

function FXCustomForm.HasActiveCustomTitleBar: boolean;
begin
  Result := CustomTitleBar.Enabled and (CustomTitleBar.Control <> nil);
end;

procedure FXCustomForm.Paint;
begin
  inherited;

end;

procedure FXCustomForm.QuickBroadcast(MessageID: integer);
var
  AMsg: TMessage;
begin
  AMsg.Msg := MessageID;
  AMsg.WParam := 0;
  AMsg.LParam := LongInt(Self);
  AMsg.Result := 0;

  Broadcast(AMsg);
end;

procedure FXCustomForm.Redraw;
begin
  Invalidate;
end;

procedure FXCustomForm.Resize;
begin
  inherited;
end;

procedure FXCustomForm.SetBackdrop(const Value: FXFormBackdropType);
begin
  if FBackdrop = Value then
    Exit;
  FBackdrop := Value;

  const EnableEffect = not (Value in [FXFormBackdropType.Automatic, FXFormBackdropType.None]);

  GlassFrame.Enabled := EnableEffect;
  GlassFrame.SheetOfGlass := EnableEffect;

  if EnableEffect then
    SetSystemBackdropType(Handle, TSystemBackdropType(Value));
end;

procedure FXCustomForm.SetBackgroundColor(const Value: FXBackgroundColor);
begin
  if FBackground = Value then
    Exit;

  // Set
  FBackground := Value;

  // Draw
  Invalidate;

  UpdateTheme(true);
end;

procedure FXCustomForm.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
end;

procedure FXCustomForm.SetBoundsRect(Bounds: TRect);
begin
  SetBounds(Bounds.Left, Bounds.Top, Bounds.Width, Bounds.Height);
end;

procedure FXCustomForm.SetCornerPreference(const Value: FXFormCornerPreference);
const
  DWMWA_WINDOW_CORNER_PREFERENCE = 33;
begin
  if FCornerPreference = Value then
    Exit;
  FCornerPreference := Value;

  if Value = FXFormCornerPreference.Customized then begin
    var Rgn: HRGN;
    Rgn := CreateRoundRectRgn(0, 0, Width, Height,
      FCornerPreferenceCustomized.RoundWidth, FCornerPreferenceCustomized.RoundHeight);
    SetWindowRgn(Handle, Rgn, True);
  end else begin
    // Reset DWM-handled window styling
    SetWindowRgn(Handle, 0, True);

    // Set preference
    SetWindowCorner(Handle, TWindowCornerPreference(Value));
  end;
end;

procedure FXCustomForm.SetDisableCustomTitlebar(const Value: boolean);
begin
  if FDisableCustomTitlebar = Value then
    Exit;
  const LastTransparent = TransparentColor;
  FDisableCustomTitlebar := Value;

  // Enable
  CustomTitleBar.Enabled := not Value;
  TTlCtrl.Visible := not Value;

  // Last
  if Value then
    TransparentColor := FPostTitlebarEnableTransparentValue
  else
    FPostTitlebarEnableTransparentValue := LastTransparent;
end;

procedure FXCustomForm.SetMicaEffect(const Value: boolean);
begin
  FMicaEffect := Value;

  if Value then begin
      // Save
      FPostMicaBlendValue := AlphaBlendValue;
      FPostMicaBlend := AlphaBlend;

      // Update
      AlphaBlend := true;
      AlphaBlendValue := FORM_MICA_EFFECT_BLEND_VALUE;
  end else begin
    // Load
    AlphaBlendValue := FPostMicaBlendValue;
    AlphaBlend := FPostMicaBlend;
  end;
end;

procedure FXCustomForm.SetSmokeEffect(const Value: boolean);
begin
  if FSmokeEffect = Value then
    Exit;
  FSmokeEffect := Value;

  if Value then
    Smoke.AlphaBlendValue := 0
  else
    Smoke.AlphaBlendValue := FORM_SMOKE_BLEND_VALUE;

  with FXAsyncIntAnim.Create do
    try
      StartValue := 0;
      EndValue := FORM_SMOKE_BLEND_VALUE;

      Inverse := not Value;

      // Prep
      Kind := FXAnimationKind.Linear;
      if Value then
        Duration := 0.1
      else
        Duration := 0.3;
      LatencyAdjustments := true;
      LatencyCanSkipSteps := true;

      // Proc
      OnValue := procedure(V: integer) begin
        Smoke.AlphaBlendValue := V;
        Smoke.Visible := true;
      end;

      Start;

      if not Value then
        Smoke.Hide;
    finally
      Free;
    end;
end;

procedure FXCustomForm.SetWindowUpdateLock(const Value: boolean);
begin
  if FWindowUpdateLock = Value then
    Exit;

  FWindowUpdateLock := Value;

  if Value then
    LockWindowUpdate(Handle)
  else
    LockWindowUpdate(0);
end;

function ImmersiveDarkMode: integer;
const
  DWMWA_USE_IMMERSIVE_DARK_MODE_BEFORE_20H1 = 19;
  DWMWA_USE_IMMERSIVE_DARK_MODE = 20;
begin
  if IsWindows10OrGreater(18985) then
    Result := DWMWA_USE_IMMERSIVE_DARK_MODE
  else
    Result := DWMWA_USE_IMMERSIVE_DARK_MODE_BEFORE_20H1;
end;

procedure FXCustomForm.UpdateTheme(const UpdateChildren: Boolean);
var
  PrevColor: TColor;
  ThemeReason: FXThemeType;
  BackgroundSelect: FXColorType;
begin
  LockWindowUpdate(Handle);

  BackgroundSelect := FXColorType.Background;
  if BackgroundColor = FXBackgroundColor.Content then
    BackgroundSelect := FXColorType.Content;

  // Update titlebar
  var Value: LongBool := ThemeManager.DarkTheme;
  if Succeeded(DwmSetWindowAttribute(Handle, Ord(ImmersiveDarkMode), @Value, SizeOf(Value))) then
    AllowDarkModeForWindow(Handle, ThemeManager.DarkTheme);

  // Update Colors
  if CustomColors.Enabled then
    begin
      FDrawColors.Background := ExtractColor( CustomColors, BackgroundSelect );
      FDrawColors.Foreground := ExtractColor( CustomColors, FXColorType.Foreground );
    end
  else
    begin
      FDrawColors.Background := ExtractColor(ThemeManager.SystemColor, BackgroundSelect);
      FDrawColors.Foreground := ExtractColor(ThemeManager.SystemColor, FXColorType.Foreground);
    end;

  // Transizion Animation
  PrevColor := FDestColor;

  // Theme Change Engine
  if PrevColor <> FDrawColors.Background then
    ThemeReason := FXThemeType.AppTheme
  else
    ThemeReason := FXThemeType.Redraw;

  //  Update tooltip style
  if ThemeManager.DarkTheme then
    HintWindowClass := FXDarkTooltip
  else
    HintWindowClass := FXLightTooltip;

  // Procedure
  const DoUpdateChildren = procedure begin
    //  Update children
    if IsContainer and UpdateChildren then
      begin
        for var I := 0 to ComponentCount -1 do
          if Supports(Components[I], IFXComponent) then
            (Components[I] as IFXComponent).UpdateTheme(UpdateChildren);
      end;
  end;

  // Start Transition
  FDestColor := FDrawColors.Background;
  if Self.Visible and FAllowThemeChangeAnim then
    with FXAsyncIntAnim.Create do begin
      Duration := 0.1;
      Kind := FXAnimationKind.Exponential;
      Steps := 4;

      LatencyAdjustments := true;
      LatencyCanSkipSteps := true;

      StartValue := 0;
      EndValue := 255;

      var NewColor: TColor;
      OnValue := procedure(Value: integer) begin
        NewColor := ColorBlend(PrevColor, FDestColor, Value);
        if NewColor = Self.Color then
          Exit;

        LockWindowUpdate(Handle);

        // Step
        Self.Color := NewColor;

        if FEnableTitlebar then
          PrepareCustomTitleBar( TForm( Self ), Self.Color, FDrawColors.Foreground );

        // Update chidren
        DoUpdateChildren();

        // Unlock
        Invalidate;

        LockWindowUpdate(0);
        Application.ProcessMessages;
      end;

      FreeOnFinish := true;
      Start;
    end
  else
    // No animation
    begin
      Color := FDestColor;
      Invalidate;
      if FEnableTitlebar then
        PrepareCustomTitleBar( TForm( Self ), FDestColor, FDrawColors.Foreground );
    end;

  // Font Color
  Font.Color := FDrawColors.Foreground;

  // Children
  DoUpdateChildren();

  // Notify Theme Change
  if Assigned(FThemeChange) then
    FThemeChange(Self, ThemeReason, ThemeManager.DarkTheme, ThemeManager.AccentColor);

  LockWindowUpdate(0);
end;

procedure FXCustomForm.WMClose(var Message: TWMClose);
begin
  inherited;
end;

procedure FXCustomForm.WMShowWindow(var Message: TWMShowWindow);
begin
  inherited;

  // Re-enable alpha blend
  if MicaEffect then
    AlphaBlend := true;
end;

procedure FXCustomForm.WM_Activate(var Msg: TWMActivate);
begin
  inherited;

  if SmokeEffect and  (Smoke<> nil) and Smoke.Visible then
    Smoke.SetFocus;
end;

procedure FXCustomForm.WM_DWMColorizationColorChanged(var Msg: TMessage);
begin
  inherited;
  ThemeManager.MeasuredUpdateSettings;

  UpdateTheme(true);
end;

procedure FXCustomForm.WM_GETMINMAXINFO(var Msg: TMessage);
begin
  if SmokeEffect then
    begin
      // When SmokeEffect is true, set the minimum and maximum tracking size to the current size
      with PMinMaxInfo(Msg.LParam)^.ptMinTrackSize do
      begin
        X := Width;
        Y := Height;
      end;
      with PMinMaxInfo(Msg.LParam)^.ptMaxTrackSize do
      begin
        X := Width;
        Y := Height;
      end;
    end
  else
    inherited;
end;

procedure FXCustomForm.WM_MOVE(var Msg: TWMMove);
begin
  if not CanMove(Msg.Pos) then
    Abort;

  inherited;
  DoMove;

  // Broadcast
  QuickBroadcast(WM_WINDOW_MOVE);
end;

procedure FXCustomForm.WM_SIZE(var Msg: TWMSize);
begin
  DoSize(Msg.Width, Msg.Height);

  // Inherit
  inherited;

  // Broadcast
  QuickBroadcast(WM_WINDOW_RESIZE);
end;

procedure FXCustomForm.WM_SysCommand(var Msg: TWMSysCommand);
begin
  inherited;
end;

{ FXDialogForm }

function FXDialogForm.CanMove(Position: TPoint): boolean;
function IsWindowSnapped(Form: TForm): boolean;
var
  P: TWindowPlacement;
  BoundsRect: TRect;
begin
  Result := false;

  // Get rect
  GetWindowRect(Form.Handle, BoundsRect);

  // Compare normals
  if Form.HandleAllocated and IsWindow(Form.Handle) and GetWindowPlacement(Form.Handle, P) then
    Result := (Form.WindowState <> wsNormal)
      or ((P.rcNormalPosition.Left <> BoundsRect.Left) and (P.rcNormalPosition.Right <> BoundsRect.Right)) or
        ((P.rcNormalPosition.Top <> BoundsRect.Top) and (P.rcNormalPosition.Bottom <> BoundsRect.Bottom));
end;
begin
  Result := inherited;

  if Result and Visible and FAutoMoveParent and FCanMoveParent and FAutoCenter and (FParentForm <> nil) then begin
    // Calculate can move
    var CanMove: boolean; CanMove := true;

    // Check if parent snapped
    if IsWindowSnapped(FParentForm) then
      CanMove := false;

    if CanMove then begin
      // Center parent around
      const ACenter = BoundsRect.CenterPoint;
      with FParentForm do begin
        const NewP = Point(ACenter.X - Width div 2, ACenter.Y - Height div 2);

        if Left <> NewP.X then
          Left := NewP.X;
        if Top <> NewP.Y then
          Top := NewP.Y;
      end;
    end else begin
      Result := false;
      CenterDialogInParentForm;
    end;
  end;
end;

function FXDialogForm.CanResize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := inherited;

  // Offset self position in parent
  if Result and Visible and FAutoMoveParent and FCanMoveParent and FAutoCenter and (FParentForm <> nil) then
    CenterDialogInParentForm;
end;

procedure FXDialogForm.CenterDialogInParentForm;
begin
  const NewLeft = FParentForm.Left + (FParentForm.Width - Width) div 2;
  const NewTop = FParentForm.Top + (FParentForm.Height - Height) div 2;

  if (NewLeft <> Left) or (NewTop <> Top) then
    SetWindowPos(Handle, 0, NewLeft, NewTop, 0, 0, SWP_NOSIZE or SWP_NOZORDER or SWP_NOACTIVATE);
end;

procedure FXDialogForm.CMShowingChanged(var Message: TMessage);
begin
  inherited;

  // Settings
  FCanMoveParent := Visible;
end;

procedure FXDialogForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
  //
  if Owner is TForm then
    Params.WndParent := TForm(Owner).Handle
  else
    if Screen.ActiveForm <> nil then
      Params.WndParent := Screen.ActiveForm.Handle;
end;

procedure FXDialogForm.DoShow;
begin
  inherited;

  // Center
  if FAutoCenter and (FParentForm <> nil) and (Position = poDesigned) then
    CenterDialogInParentForm;
end;

procedure FXDialogForm.InitForm;
begin
  if (FParentForm = nil) and (Owner is TForm) then
    FParentForm := TForm(Owner);

  FAutoCenter := true;
  FAutoSmoke := true;
  FAutoMoveParent := true;

  inherited;
end;

function FXDialogForm.ShowModal: Integer;
begin
  const CanChangeSmoke = AutoSmoke and (FParentForm is FXCustomForm);

  // Center
  if FAutoCenter then
    if FParentForm = nil then
      Position := poScreenCenter
    else
      Position := poDesigned;

  // Smoke
  if CanChangeSmoke then
    (FParentForm as FXCustomForm).SmokeEffect := true;
  
  try
    // Modal
    Result := inherited;
  finally
    // Smoke
    if CanChangeSmoke then
      (FParentForm as FXCustomForm).SmokeEffect := false;
  end;
end;

{ FXForm }

procedure FXForm.InitForm;
begin
  // Constraints
  if FDefaultConstraints and
    ((Constraints.MinWidth = 0) and (Constraints.MinHeight = 0)
      and (Constraints.MaxWidth = 0) and (Constraints.MaxWidth = 0)) then begin
    Constraints.MinWidth := 500;
    Constraints.MinHeight := 325;
  end;

  // inherit
  inherited;
end;

procedure FXForm.SetFullscreen(const Value: boolean);
begin
  if Value = FFullScreen then
    Exit;

  FFullScreen := Value;
  if Value then
    begin
      FRestoredPosition := BoundsRect;
      FRestoredBorder := BorderStyle;

      CustomTitleBar.Enabled := false;
      BorderStyle := bsNone;
      SetBoundsRect(Monitor.BoundsRect);
    end
  else
    begin
      CustomTitleBar.Enabled := true;
      BorderStyle := FRestoredBorder;
      SetBoundsRect(FRestoredPosition);
    end;
end;

{ FXPopupForm }

function FXPopupForm.CanMove(Position: TPoint): boolean;
begin
  Result := inherited;
  if Visible and (BorderStyle in [bsSingle, bsDialog]) then
    Result := false;
end;

function FXPopupForm.CanResize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := inherited;
  if Visible and (BorderStyle in [bsSingle, bsDialog]) then
    Result := false;
end;

procedure FXPopupForm.CreateParams(var Params: TCreateParams);
begin
  inherited;

  Params.ExStyle := Params.ExStyle or WS_EX_NOACTIVATE;
  Params.Style := Params.Style and not (WS_CAPTION) or WS_SIZEBOX;
end;

function FXPopupForm.ShowModal: Integer;
begin
  Result := inherited;
end;

function FXPopupForm.ShowModalAtControl(Executor: TControl; HorizontalOffset: integer=0; VerticalOffset: integer=0): Integer;
begin
  Result := ShowModal( Executor.ClientToScreen(Point(HorizontalOffset, VerticalOffset)) );
end;

procedure FXPopupForm.WMActivate(var Msg: TWMActivate);
begin
  inherited;
  if Msg.Active = WA_INACTIVE then
    Close;
end;

procedure FXPopupForm.WMKeyDown(var Msg: TWMKeyDown);
begin
  if (Msg.CharCode = VK_ESCAPE) and LightDismiss then begin
    Close;
    Msg.Result := 0; // eat the key
    Exit;
  end;
  inherited;
end;

procedure FXPopupForm.WMKillFocus(var Msg: TWMKillFocus);
begin
  if FLightDismiss then
    Close; // or Hide, depending on your behavior
end;

{procedure FXPopupForm.DisableFormBorder;
var
  Style: Cardinal;
  ExStyle: Cardinal;
begin
  Style := GetWindowLong(Handle, GWL_STYLE);
  ExStyle := GetWindowLong(Handle, GWL_EXSTYLE);

  // Remove caption bar
  Style := Style and not (WS_CAPTION) or WS_SIZEBOX; // sizeable it MUST be - Yoda
  ExStyle := ExStyle or WS_EX_NOACTIVATE;

  SetWindowLong(Handle, GWL_STYLE, Style);
  SetWindowLong(Handle, GWL_EXSTYLE, ExStyle);

  // Crate
  Perform(WM_NCCREATE, 0, 0);

  // Is minimised?
  if not IsIconic(Handle) then
    // Re-calculate bounds
    SetWindowPos(Handle, 0, 0, 0, 0, 0,
      SWP_NOSIZE or SWP_NOMOVE or SWP_NOZORDER or SWP_NOACTIVATE or SWP_FRAMECHANGED);
end;}

procedure FXPopupForm.DoClose(var Action: TCloseAction);
begin
  if FreeOnClose then
    Action := caFree;
end;

procedure FXPopupForm.DoShow;
begin
  inherited;

  // Re-inforce, in case the border style changed, etc.
  //DisableFormBorder;
end;

procedure FXPopupForm.InitForm;
begin
  inherited;
  PopupMode := pmExplicit;
  BorderStyle := bsDialog;
  BorderIcons := [];
  KeyPreview := true;

  // Titlebar
  CustomTitleBar.ShowCaption := false;
  CustomTitleBar.ShowIcon := false;
  CustomTitleBar.SystemButtons := false;
  CustomTitleBar.SystemHeight := false;
  CustomTitleBar.SystemColors := false;
  CustomTitleBar.Height := 1;

  // Props
  FLightDismiss := true;
  FFreeOnClose := true;

  // UI
  //DisableFormBorder;
end;

procedure FXPopupForm.Show(Position: TPoint);
begin
  Left := Position.X;
  Top := Position.Y;

  Show;
end;

procedure FXPopupForm.ShowAtControl(Executor: TControl; HorizontalOffset,
  VerticalOffset: integer);
var
  Pos: TPoint;
begin
  Pos := Executor.ClientToScreen(Point(HorizontalOffset, VerticalOffset));

  const OutRect = Screen.WorkAreaRect;

  // Keep inbounds horizontally
  if Pos.X + Width > OutRect.Right then
    Pos.X := OutRect.Right - Width
  else if Pos.X < OutRect.Left then
    Pos.X := OutRect.Left;

  // Keep inbounds vertically
  if Pos.Y + Height > OutRect.Bottom then
    Pos.Y := OutRect.Bottom - Height
  else if Pos.Y < OutRect.Top then
    Pos.Y := OutRect.Top;

  // Show
  Show( Pos );
end;

procedure FXPopupForm.ShowAtCursor;
begin
  Show(Mouse.CursorPos);
end;

function FXPopupForm.ShowModal(Position: TPoint): Integer;
begin
  Left := Position.X;
  Top := Position.Y;

  Result := ShowModal;
end;

function FXPopupForm.ShowModalAtCursor: Integer;
begin
  Result := ShowModal(Mouse.CursorPos);
end;

end.
