unit CFX.Forms;

interface

uses
  SysUtils,
  Classes,
  Windows,
  Types,
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
    FBackground: FXBackgroundColor;

    // Mica
    FPostMicaBlend: boolean;
    FPostMicaBlendValue: byte;

    // Smoke
    Smoke: TForm;
    SmokeAnimation: TIntAni;

    // Prepare
    procedure CreateSmokeSettings;

    // Messages
    procedure WM_SysCommand(var Msg: TWMSysCommand); message WM_SYSCOMMAND;
    procedure WM_DWMColorizationColorChanged(var Msg: TMessage); message WM_DWMCOLORIZATIONCOLORCHANGED;
    procedure WM_Activate(var Msg: TWMActivate); message WM_ACTIVATE;
    procedure WM_MOVE(var Msg: Tmessage); message WM_MOVE;
    procedure WM_SIZE(var Msg: TWMSize); message WM_SIZE;
    procedure WM_GETMINMAXINFO(var Msg: TMessage); message WM_GETMINMAXINFO;

    procedure QuickBroadcast(MessageID: integer);

    // Setters
    procedure SetMicaEffect(const Value: boolean);
    procedure SetSmokeEffect(const Value: boolean);
    procedure SetWindowUpdateLock(const Value: boolean);
    procedure SetBackgroundColor(const Value: FXBackgroundColor);

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
    procedure DoMove; virtual;
    procedure DoSize(var AWidth, AHeight: word); virtual;

    // Initialization
    procedure InitForm; virtual;

    // Override
    procedure InitializeNewForm; override;

  published
    property MicaEffect: boolean read FMicaEffect write SetMicaEffect default false;
    property SmokeEffect: boolean read FSmokeEffect write SetSmokeEffect default false;
    property CustomColors: FXColorSets read FCustomColors write FCustomColors;
    property AllowThemeChangeAnimation: boolean read FAllowThemeChangeAnim write FAllowThemeChangeAnim default false;
    property WindowUpdateLocked: boolean read FWindowUpdateLock write SetWindowUpdateLock;
    property DisableTitlebarAlign: boolean read FDisableTitlebarAlign write FDisableTitlebarAlign default false;
    property BackgroundColor: FXBackgroundColor read FBackground write SetBackgroundColor;

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
    procedure DoMove; override;
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
  FCustomColors.Free;
  FDrawColors.Free;

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

  // Needs custom title bar
  CustomTitleBar.Enabled := true;

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

procedure FXCustomForm.SetBoundsRect(Bounds: TRect);
begin
  SetBounds(Bounds.Left, Bounds.Top, Bounds.Width, Bounds.Height);
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

procedure FXCustomForm.WM_Activate(var Msg: TWMActivate);
begin
  inherited;

  if Smoke.Visible then
    Smoke.SetFocus;
end;

procedure FXCustomForm.WM_DWMColorizationColorChanged(var Msg: TMessage);
begin
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

procedure FXCustomForm.WM_MOVE(var Msg: Tmessage);
begin
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

function FXDialogForm.CanResize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := inherited CanResize(NewWidth, NewHeight);

  // Offset self position in parent
  if Visible and FAutoMoveParent and FCanMoveParent and FAutoCenter and (FParentForm <> nil) then
    CenterDialogInParentForm;
end;

procedure FXDialogForm.CenterDialogInParentForm;
begin
  Left := FParentForm.Left + (FParentForm.Width - Width) div 2;
  Top := FParentForm.Top + (FParentForm.Height - Height) div 2;
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
    Params.WndParent := Screen.ActiveForm.Handle;
end;

procedure FXDialogForm.DoMove;
begin
  inherited;

  if Visible and FAutoMoveParent and FCanMoveParent and FAutoCenter and (FParentForm <> nil) then begin
    // Center parent around
    const ACenter = BoundsRect.CenterPoint;
    with FParentForm do begin
      const NewP = Point(ACenter.X - Width div 2, ACenter.Y - Height div 2);

      if Left <> NewP.X then
        Left := NewP.X;
      if Top <> NewP.Y then
        Top := NewP.Y;
    end;
  end;
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
  FDefaultConstraints := true;

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

end.
