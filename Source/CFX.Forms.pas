unit CFX.Forms;

interface

uses
  SysUtils,
  Classes,
  Windows,
  CFX.ToolTip,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.Dialogs,
  Messages,
  CFX.ThemeManager,
  CFX.Colors,
  CFX.Constants,
  CFX.Animation.Main,
  Vcl.TitleBarCtrls,
  CFX.Animations,
  CFX.Utilities,
  Vcl.ExtCtrls,
  CFX.TitlebarPanel,
  CFX.Classes,
  CFX.Types,
  CFX.Messages,
  CFX.Linker;

type
  // Proc
  FXFormProcedure = procedure(Sender: TObject) of object;

  // Types define
  FXThemeType = CFX.Types.FXThemeType;

  // Form
  FXForm = class(TForm, IFXComponent, IFXControl)
  private
    FCustomColors: FXColorSets;
    FDrawColors: FXColorSet;

    FWindowUpdateLock: boolean;

    // Settings
    FFullScreen: Boolean;
    FRestoredPosition: TRect;
    FRestoredBorder: TBorderStyle;

    FMicaEffect: boolean;
    FSmokeEffect: boolean;
    FAllowThemeChangeAnim: boolean;

    // Notify
    FThemeChange: FXThemeChange;
    FOnMove: FXFormProcedure;

    // Status
    FDestColor: TColor;

    // Titlebar
    FTitlebarInitialized: boolean;
    FEnableTitlebar: boolean;
    TTlCtrl: TCustomTitleBarpanel;
    FDisableTitlebarAlign: boolean;

    // Smoke
    Smoke: TForm;
    SmokeAnimation: TIntAni;

    // Functions
    procedure SetFullScreen(const Value: Boolean);
    procedure CreateSmokeSettings;

    // Messages
    procedure WM_SysCommand(var Msg: TWMSysCommand); message WM_SYSCOMMAND;
    procedure WM_DWMColorizationColorChanged(var Msg: TMessage); message WM_DWMCOLORIZATIONCOLORCHANGED;
    procedure WM_Activate(var Msg: TWMActivate); message WM_ACTIVATE;
    procedure WM_MOVE(var Msg: Tmessage); message WM_MOVE;
    procedure WM_SIZE(var Msg: Tmessage); message WM_SIZE;
    procedure WM_GETMINMAXINFO(var Msg: TMessage); message WM_GETMINMAXINFO;

    procedure QuickBroadcast(MessageID: integer);

    // Procedures
    procedure SetMicaEffect(const Value: boolean);
    procedure SetSmokeEffect(const Value: boolean);
    procedure SetWindowUpdateLock(const Value: boolean);

    // Utilities
    procedure FormCloseIgnore(Sender: TObject; var CanClose: Boolean);

  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
    procedure Resize; override;

    // Utils
    function HasActiveCustomTitleBar: boolean;

    // Sizing
    function GetClientRect: TRect; override;
    procedure AdjustClientRect(var Rect: TRect); override;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;

    // Do
    procedure DoShow; override;

    // Initialization
    procedure InitForm; virtual;

    // Override
    procedure InitializeNewForm; override;

  published
    property MicaEffect: boolean read FMicaEffect write SetMicaEffect default false;
    property SmokeEffect: boolean read FSmokeEffect write SetSmokeEffect default false;
    property CustomColors: FXColorSets read FCustomColors write FCustomColors;
    property AllowThemeChangeAnimation: boolean read FAllowThemeChangeAnim write FAllowThemeChangeAnim default false;
    property FullScreen: Boolean read FFullScreen write SetFullScreen default false;
    property WindowUpdateLocked: boolean read FWindowUpdateLock write SetWindowUpdateLock;
    property DisableTitlebarAlign: boolean read FDisableTitlebarAlign write FDisableTitlebarAlign default false;

    // On Change...
    property OnMove: FXFormProcedure read FOnMove write FOnMove;

    // Theming Engine
    property OnThemeChange: FXThemeChange read FThemeChange write FThemeChange;

  public

    // Procedures
    procedure SetBoundsRect(Bounds: TRect);

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

  FXDialogForm = class(FXForm)
  private
    FAutoCenter: boolean;
    FAutoSmoke: boolean;

  protected
    // Initialization (after form creation)
    procedure InitForm; override;

    // Do
    procedure DoShow; override;

  public
    // Props
    property AutoCenter: boolean read FAutoCenter write FAutoCenter;
    property AutoSmoke: boolean read FAutoSmoke write FAutoSmoke;

    // Modal
    function ShowModal: Integer; override;
  end;

implementation

{ FXForm }

procedure FXForm.AdjustClientRect(var Rect: TRect);
begin
  inherited;
  if DisableTitlebarAlign then
    Dec(Rect.Top, GlassFrame.Top);
end;

function FXForm.Background: TColor;
begin
  Result := Color;
end;

function FXForm.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := inherited;
end;

constructor FXForm.Create(aOwner: TComponent);
begin
  // Create Form and Components
  inherited;

  // Initialise
  InitForm;
end;

constructor FXForm.CreateNew(aOwner: TComponent; Dummy: Integer);
begin
  inherited;

  // Initialise
  InitForm;
end;

procedure FXForm.CreateParams(var Params: TCreateParams);
begin
  inherited;

  //Params.Style := Params.Style or 200000;
end;

procedure FXForm.CreateSmokeSettings;
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

destructor FXForm.Destroy;
begin
  FCustomColors.Free;
  FDrawColors.Free;

  inherited;
end;

procedure FXForm.DoShow;
begin
  inherited;
    if not FTitlebarInitialized then
      with Self.CustomTitleBar do
        begin
          Enabled := true;

          SystemButtons := false;
          SystemColors := false;
          SystemHeight := false;

          Height := TTlCtrl.Height;

          Control := TTlCtrl;

          FTitlebarInitialized := true;

          UpdateTheme(false);
        end;
end;

procedure FXForm.InitForm;
label
  skip_titlebar;
var
  I: Integer;
begin
  FDisableTitlebarAlign := false;

  // Settings
  Font.Name := ThemeManager.FormFont;
  Font.Height := ThemeManager.FormFontHeight;

  // Effects
  MicaEffect := true;
  CreateSmokeSettings;
  FAllowThemeChangeAnim := false;

  // TitleBar
  FTitlebarInitialized := false;
  FEnableTitlebar := GetNTKernelVersion >= 6.0;
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

  // Title Bar End
  skip_titlebar:

  // Update Theme
  UpdateTheme(false);
end;

procedure FXForm.InitializeNewForm;
begin
  inherited;
  // Create Classes
  FCustomColors := FXColorSets.Create(Self);
  FDrawColors := FXColorSet.Create(ThemeManager.SystemColorSet, ThemeManager.DarkTheme);
end;

function FXForm.IsContainer: Boolean;
begin
  Result := true;
end;

function FXForm.IsResizable: Boolean;
begin
  Result := BorderStyle in [bsSizeable, bsSizeToolWin];
end;

procedure FXForm.FormCloseIgnore(Sender: TObject; var CanClose: Boolean);
begin
  if SmokeEffect then
    CanClose := false;
end;

function FXForm.GetClientRect: TRect;
begin
  Result := inherited;
end;

function FXForm.GetTitlebarHeight: integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to ControlCount - 1 do
    if Controls[I] is TCustomTitleBarPanel then
      Result := TCustomTitleBarPanel(Controls[I]).Height;
end;

function FXForm.HasActiveCustomTitleBar: boolean;
begin
  Result := CustomTitleBar.Enabled and (CustomTitleBar.Control <> nil);
end;

procedure FXForm.Paint;
begin
  inherited;

end;

procedure FXForm.QuickBroadcast(MessageID: integer);
var
  AMsg: TMessage;
begin
  AMsg.Msg := MessageID;
  AMsg.WParam := 0;
  AMsg.LParam := LongInt(Self);
  AMsg.Result := 0;

  Broadcast(AMsg);
end;

procedure FXForm.Redraw;
begin
  Invalidate;
end;

procedure FXForm.Resize;
begin
  inherited;

end;

procedure FXForm.SetBoundsRect(Bounds: TRect);
begin
  SetBounds(Bounds.Left, Bounds.Top, Bounds.Width, Bounds.Height);
end;

procedure FXForm.SetFullScreen(const Value: Boolean);
begin
  if Value <> FFullScreen then
    begin
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
end;

procedure FXForm.SetMicaEffect(const Value: boolean);
begin
  FMicaEffect := Value;

  if Value then
    begin
      AlphaBlend := true;

      AlphaBlendValue := 251;
    end
      else
    begin
      AlphaBlend := false;
    end;
end;

procedure FXForm.SetSmokeEffect(const Value: boolean);
begin
  FSmokeEffect := Value;

  // Wait
  if (SmokeAnimation <> nil) and SmokeAnimation.Running then
    begin
      SmokeAnimation.Terminate;
      SmokeAnimation.WaitFor;
    end;

  // Toggle
  if Value then
    begin
      Smoke.Visible := true;

      Smoke.AlphaBlendValue := FORM_SMOKE_BLEND_VALUE;
    end
      else
    begin
      if SmokeAnimation <> nil then
        SmokeAnimation.Free;

      SmokeAnimation := TIntAni.Create(true, TAniKind.akIn, TAniFunctionKind.afkLinear,
      Smoke.AlphaBlendValue, -Smoke.AlphaBlendValue,
      procedure(Value: integer)
        begin
          Smoke.AlphaBlendValue := Value
        end,
      procedure
        begin
          Smoke.Visible := false;
        end);

      SmokeAnimation.Duration := 100;

      SmokeAnimation.FreeOnTerminate := false;
      SmokeAnimation.Start;
    end;
end;

procedure FXForm.SetWindowUpdateLock(const Value: boolean);
begin
  if FWindowUpdateLock <> Value then
    begin
      FWindowUpdateLock := Value;

      if Value then
        LockWindowUpdate(Handle)
      else
        LockWindowUpdate(0);
    end;
end;

procedure FXForm.UpdateTheme(const UpdateChildren: Boolean);
var
  PrevColor: TColor;
  ThemeReason: FXThemeType;
begin
  LockWindowUpdate(Handle);

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
      Duration := 0.2;
      Kind := FXAnimationKind.Exponential;
      Steps := 8;

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

procedure FXForm.WM_Activate(var Msg: TWMActivate);
begin
  inherited;

  if Smoke.Visible then
    Smoke.SetFocus;
end;

procedure FXForm.WM_DWMColorizationColorChanged(var Msg: TMessage);
begin
  ThemeManager.MeasuredUpdateSettings;

  UpdateTheme(true);
end;

procedure FXForm.WM_GETMINMAXINFO(var Msg: TMessage);
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

procedure FXForm.WM_MOVE(var Msg: Tmessage);
begin
  inherited;
  if Assigned(FOnMove) then
    FOnMove(Self);

  // Broadcast
  QuickBroadcast(WM_WINDOW_MOVE);
end;

procedure FXForm.WM_SIZE(var Msg: Tmessage);
begin
  inherited;

  // Broadcast
  QuickBroadcast(WM_WINDOW_RESIZE);
end;

procedure FXForm.WM_SysCommand(var Msg: TWMSysCommand);
begin
  inherited;

end;

{ FXDialogForm }

procedure FXDialogForm.DoShow;
begin
  inherited;
end;

procedure FXDialogForm.InitForm;
begin
  FAutoCenter := true;
  FAutoSmoke := true;

  inherited;
end;

function FXDialogForm.ShowModal: Integer;
begin
  const CanChangeSmoke = AutoSmoke and (Owner is FXForm);

  // Center
  if FAutoCenter then
    Position := poOwnerFormCenter;

  // Smoke
  if CanChangeSmoke then
    (Owner as FXForm).SmokeEffect := true;
  
  try
    // Modal
    Result := inherited;
  finally
    // Smoke
  if CanChangeSmoke then
    (Owner as FXForm).SmokeEffect := false;
  end;
end;

end.
