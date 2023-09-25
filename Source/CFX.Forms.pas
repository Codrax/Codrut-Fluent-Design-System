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
  CFX.UIConsts,
  Vcl.TitleBarCtrls,
  CFX.Animations,
  CFX.Utilities,
  Vcl.ExtCtrls,
  CFX.Classes,
  CFX.Types,
  CFX.Linker;

type
  FXFormProcedure = procedure(Sender: TObject) of object;

  FXForm = class(TForm, FXControl)
    private
        FCustomColors: FXColorSets;
        FDrawColors: FXColorSet;

        FFullScreen: Boolean;
        FRestoredPosition: TRect;
        FRestoredBorder: TBorderStyle;

        FMicaEffect: boolean;
        FSmokeEffect: boolean;

        FThemeChange: FXThemeChange;
        FOnMove: FXFormProcedure;

        FAllowThemeChangeAnim: boolean;

        FTitlebarInitialized: boolean;

        FEnableTitlebar: boolean;
        TTlCtrl: TTitleBarpanel;

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
      procedure WM_GETMINMAXINFO(var Msg: TMessage); message WM_GETMINMAXINFO;

      // Procedures
      procedure SetMicaEffect(const Value: boolean);
      procedure SetSmokeEffect(const Value: boolean);

      // Utilities
      procedure FormCloseIgnore(Sender: TObject; var CanClose: Boolean);

    protected
      procedure CreateParams(var Params: TCreateParams); override;
      procedure Paint; override;
      procedure Resize; override;

      procedure DoShow; override;

      procedure InitializeNewForm; override;

    published
      property MicaEffect: boolean read FMicaEffect write SetMicaEffect;
      property SmokeEffect: boolean read FSmokeEffect write SetSmokeEffect;
      property CustomColors: FXColorSets read FCustomColors write FCustomColors;
      property AllowThemeChangeAnimation: boolean read FAllowThemeChangeAnim write FAllowThemeChangeAnim;
      property FullScreen: Boolean read FFullScreen write SetFullScreen default false;

      // On Change...
      property OnMove: FXFormProcedure read FOnMove write FOnMove;

      // Theming Engine
      property OnThemeChange: FXThemeChange read FThemeChange write FThemeChange;

    public
      constructor Create(aOwner: TComponent); override;
      constructor CreateNew(aOwner: TComponent; Dummy: Integer = 0); override;
      destructor Destroy; override;

      procedure InitForm;

      // Procedures
      procedure SetBoundsRect(Bounds: TRect);

      // Utils
      function IsResizable: Boolean;

      function GetTitlebarHeight: integer;

      // Interface
      function IsContainer: Boolean;
      procedure UpdateTheme(const UpdateChildren: Boolean);

      function Background: TColor;
  end;


implementation

{ FXForm }

function FXForm.Background: TColor;
begin
  Result := FDrawColors.Background;
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
  // Settings
  Font.Name := ThemeManager.FormFont;
  Font.Height := ThemeManager.FormFontHeight;

  // Effects
  MicaEffect := true;
  CreateSmokeSettings;
  FAllowThemeChangeAnim := true;

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
    if Controls[I] is TTitleBarPanel then
      begin
        TTlCtrl := TTitleBarPanel(Controls[I]);
        Break;
      end;

  (*Create New*)
  if TTlCtrl = nil then
    begin
      TTlCtrl := TTitlebarPanel.Create(Self);
      TTlCtrl.Parent := Self;
    end;

  (* Assign *)
  if not Assigned(CustomTitleBar.Control) then
    CustomTitleBar.Control := TTlCtrl;

  // Title Bar End
  skip_titlebar:

  // Update Theme
  UpdateTheme(true);
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

function FXForm.GetTitlebarHeight: integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to ControlCount - 1 do
    if Controls[I] is TTitlebarPanel then
      Result := TTitlebarPanel(Controls[I]).Height;
end;

procedure FXForm.Paint;
begin
  inherited;

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

procedure FXForm.UpdateTheme(const UpdateChildren: Boolean);
var
  i: Integer;
  PrevColor: TColor;
  a: TIntAni;
  ThemeReason: FXThemeType;
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

  // Transizion Animation
  PrevColor := Self.Color;

  // Theme Change Engine
  if PrevColor <> FDrawColors.Background then
    ThemeReason := FXThemeType.AppTheme
  else
    ThemeReason := FXThemeType.Redraw;

  // Start Transition
  if Self.Visible and FAllowThemeChangeAnim then
    begin
      a := TIntAni.Create(true, TAniKind.akIn, TAniFunctionKind.afkLinear, 25, 100,
      procedure (Value: integer)
      begin
        Self.Color := ColorBlend(PrevColor, FDrawColors.Background, Value);

        if FEnableTitlebar then
          PrepareCustomTitleBar( TForm( Self ), Self.Color, FDrawColors.Foreground );
      end,
      procedure
      begin
        Self.Color := FDrawColors.Background;
        if FEnableTitlebar then
          PrepareCustomTitleBar( TForm( Self ), FDrawColors.Background, FDrawColors.Foreground );
      end);

      a.Duration := 200;
      a.Step := 6;


      a.Start;
    end
      else
        begin
          Self.Color := FDrawColors.Background;
          if FEnableTitlebar then
            PrepareCustomTitleBar( TForm( Self ), FDrawColors.Background, FDrawColors.Foreground );
        end;

  //  Update tooltip style
  if ThemeManager.DarkTheme then
    HintWindowClass := FXDarkTooltip
  else
    HintWindowClass := FXLightTooltip;

  // Font Color
  Font.Color := FDrawColors.Foreground;

  // Notify Theme Change
  if Assigned(FThemeChange) then
    FThemeChange(Self, ThemeReason, ThemeManager.DarkTheme, ThemeManager.AccentColor);

  //  Update children
  if IsContainer and UpdateChildren then
    begin
      LockWindowUpdate(Handle);
      for i := 0 to ComponentCount -1 do
        if Supports(Components[i], FXControl) then
          (Components[i] as FXControl).UpdateTheme(UpdateChildren);
      LockWindowUpdate(0);
    end;
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
  if Assigned(FOnMove) then
    FOnMove(Self);
end;

procedure FXForm.WM_SysCommand(var Msg: TWMSysCommand);
begin
  inherited;

end;

end.
