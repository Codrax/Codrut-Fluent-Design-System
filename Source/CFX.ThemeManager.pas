unit CFX.ThemeManager;

{$TYPEINFO ON}

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.Dwmapi,
  CFX.Windows.DarkmodeApi,
  System.SysUtils,
  Win.Registry,
  System.UITypes,
  Types,
  Vcl.Clipbrd,
  Vcl.Graphics,
  CFX.Colors,
  CFX.Utilities,
  Vcl.ExtCtrls,
  Vcl.Dialogs,
  Vcl.Forms,
  Classes,
  Vcl.Themes,
  Vcl.Styles,
  CFX.Constants,
  CFX.Types,
  CFX.UXTheme,
  DateUtils,
  CFX.Linker;

type
  // Theme Manager
  FXThemeManager = class
  private
    FRegistryMonitor: TTimer;

    FDarkTheme: boolean;
    FDarkThemeMode: FXDarkSetting;

    FFormFont,
    FIconFont: string;
    FFormFontHeight: integer;
    FSmallFontHeight: integer;
    FLargeFontHeight: integer;

    FSystemMenuAnimation: FXAnimateSelection;
    FSystemMenuFlat: boolean;
    FSystemMenuEnableRadius: boolean;
    FSystemMenuEnableBorder: boolean;

    FAccentColor: TColor;
    FAutoAccentColor: boolean;

    FLegacyFontColor: boolean;
    FHandleApplicationExceptions: boolean;
    FExceptionDisplayHalt: boolean;

    LastUpdate: TTime;
    FDesigning: boolean;

    // Private Declarations
    procedure RegMonitorProc(Sender: TObject);
    procedure SetDarkTheme(const Value: boolean);
    procedure SetDarkMode(const Value: FXDarkSetting);
    procedure UpdateAccentColor;
    function LoadAccentColor: TColor;
    procedure SaveAccentColor(const Value: TColor);
    procedure SetAutoAccent(const Value: boolean);

  protected
    procedure HandleApplicationError(Sender: TObject; E: Exception);

  published
    (* Theme Settings *)
    property DarkTheme: boolean read FDarkTheme write SetDarkTheme;
    property DarkThemeMode: FXDarkSetting read FDarkThemeMode write SetDarkMode;

    (* System *)
    property HandleApplicationExceptions: boolean read FHandleApplicationExceptions write FHandleApplicationExceptions;
    property ExceptionDisplayHalt: boolean read FExceptionDisplayHalt write FExceptionDisplayHalt;

    (* Global Font Settings *)
    property FormFont: string read FFormFont write FFormFont;
    property FormFontHeight: integer read FFormFontHeight write FFormFontHeight;
    property SmallFontHeight: integer read FSmallFontHeight write FSmallFontHeight;
    property LargeFontHeight: integer read FLargeFontHeight write FLargeFontHeight;
    property IconFont: string read FIconFont write FIconFont;

    (* System Menus *)
    property SystemMenuAnimation: FXAnimateSelection read FSystemMenuAnimation write FSystemMenuAnimation;
    property SystemMenuFlat: boolean read FSystemMenuFlat write FSystemMenuFlat;
    property SystemMenuEnableRadius: boolean read FSystemMenuEnableRadius write FSystemMenuEnableRadius;
    property SystemMenuEnableBorder: boolean read FSystemMenuEnableBorder write FSystemMenuEnableBorder;

    (* Accent Color *)
    property AccentColor: TColor read LoadAccentColor write SaveAccentColor;
    property AutoAccentColor: boolean read FAutoAccentColor write SetAutoAccent;

    (* Utils *)
    property Designing: boolean read FDesigning;

  public
    constructor Create;
    destructor Destroy; override;

    (* Global Colors *)
    var
    SystemColorSet: FXCompleteColorSets;
    SystemColor: FXCompleteColorSet;
    SystemGrayControl: FXSingleColorStateSets;
    SystemAccentInteractStates: FXSingleColorStateSet;

    (* Tool Tip *)
    FSystemToolTip: FXCompleteColorSets;

    (* Load Colors *)
    procedure LoadColorSet(var ColorSet: FXColorSet); overload;
    procedure LoadColorSet(var ColorSet: FXColorSets); overload;
    procedure LoadColorSet(var ColorSet: FXCompleteColorSet); overload;
    procedure LoadColorSet(var ColorSet: FXCompleteColorSets); overload;

    (* System Menus *)
    procedure ProcessSystemMenu(Menu: TObject);

    (* Functions *)
    function GetThemePrimaryColor: TColor;

    procedure LoadFontSettings;

    procedure UpdateThemeInformation;

    procedure ShowException(E: Exception);

    procedure UpdateColors;
    procedure UpdateSettings;

    (* Notify *)
    procedure NotifyUpdate;

    (* Time Limited *)
    procedure MeasuredUpdateSettings;
  end;

// Functions
function IsDesigning: boolean;

function ExtractColor(ColorSet: FXColorSet; ClrType: FXColorType): TColor; overload;
function ExtractColor(ColorSet: FXColorSets; ClrType: FXColorType): TColor; overload;

var
  ThemeManager: FXThemeManager;

implementation

uses
  CFX.QuickDialogs, CFX.Dialogs, CFX.PopupMenu;

// In Design Mode
function IsDesigning: boolean;
begin
  if TStyleManager.ActiveStyle.Name = 'Mountain_Mist' then
    Result := true
  else
    Result := false;
end;

function ExtractColor(ColorSet: FXColorSet; ClrType: FXColorType): TColor; overload;
begin
  Result := 0;
  case ClrType of
    FXColorType.Foreground: Result := ColorSet.ForeGround;
    FXColorType.BackGround: Result := ColorSet.BackGround;
    FXColorType.Accent: Result := ColorSet.Accent;
    FXColorType.Content: if ColorSet is FXCompleteColorSet then
      Result := FXCompleteColorSet(ColorSet).BackGroundInterior;
  end
end;

function ExtractColor(ColorSet: FXColorSets; ClrType: FXColorType): TColor;
begin
  Result := 0;
  if ThemeManager.DarkTheme then
  case ClrType of
    FXColorType.Foreground: Result := ColorSet.DarkForeGround;
    FXColorType.BackGround: Result := ColorSet.DarkBackGround;
    FXColorType.Accent: Result := ColorSet.Accent;
    FXColorType.Content: if ColorSet is FXCompleteColorSets then
      Result := FXCompleteColorSets(ColorSet).DarkBackGroundInterior;
  end
    else
  case ClrType of
    FXColorType.Foreground: Result := ColorSet.LightForeGround;
    FXColorType.BackGround: Result := ColorSet.LightBackGround;
    FXColorType.Accent: Result := ColorSet.Accent;
    FXColorType.Content: if ColorSet is FXCompleteColorSets then
      Result := FXCompleteColorSets(ColorSet).LightBackGroundInterior;
  end;
end;

{ FXThemeManager }

procedure FXThemeManager.UpdateAccentColor;
begin
  if FAutoAccentColor then
    FAccentColor := GetAccentColor;
end;

constructor FXThemeManager.Create;
begin
  // Designing
  FDesigning := IsDesigning;

  // Load Settings
  if Designing then
    begin
      FDarkTheme := false;
      DarkThemeMode := FXDarkSetting.ForceLight;
    end
  else
    begin
      FDarkTheme := CFX.Utilities.GetAppsUseDarkTheme;
      DarkThemeMode := FXDarkSetting.Auto;
    end;

  FDarkThemeMode := FXDarkSetting.Auto;
  FAutoAccentColor := true;
  FLegacyFontColor := false;

  FHandleApplicationExceptions := true;
  ExceptionDisplayHalt := true;

  // Menu
  FSystemMenuAnimation := MENU_ANIMATION_SELECTION;
  FSystemMenuEnableBorder:= MENU_ANIMATION_ENABLE_BORDER;
  FSystemMenuEnableRadius:= MENU_ANIMATION_ENABLE_RADIUS;
  FSystemMenuFlat:= MENU_ANIMATION_FLAT;

  // Update Time
  LastUpdate := Now;

  // Load Font
  LoadFontSettings;

  // Default Color Sets
  SystemColorSet := FXCompleteColorSets.Create;
  SystemColor := FXCompleteColorSet.Create(SystemColorSet, DarkTheme);
  SystemGrayControl := FXSingleColorStateSets.Create;
  SystemAccentInteractStates := FXSingleColorStateSet.Create(SystemGrayControl, DarkTheme);

  // Update colors
  UpdateColors;

  // Create color sets for tool tip
  FSystemToolTip := FXCompleteColorSets.Create;

  { IGNORE IF IDE MODE! }
  if not Designing then
    begin
      // Registry Monitor
      FRegistryMonitor := TTimer.Create(nil);
      with FRegistryMonitor do
        begin
          Enabled := true;
          Interval := 100;

          OnTimer := RegMonitorProc;
        end;
    end;

  // Exception handler
  if not FDesigning then
    Application.OnException := HandleApplicationError;
end;

destructor FXThemeManager.Destroy;
begin
  // Registry monitor
  if FRegistryMonitor <> nil then
    begin
      FRegistryMonitor.Enabled := false;
      FreeAndNil( FRegistryMonitor );
    end;

  // Tool tip data
  FreeAndNil( FSystemToolTip );

  // Color classess
  FreeAndNil( SystemColorSet );
  FreeAndNil( SystemColor );
  FreeAndNil( SystemGrayControl );
  FreeAndNil( SystemAccentInteractStates );

  // Exception handlee
  Application.OnException := nil;
end;

function FXThemeManager.LoadAccentColor: TColor;
begin
  Result := FAccentColor;
end;

procedure FXThemeManager.LoadColorSet(var ColorSet: FXColorSet);
begin
  ColorSet.Assign( SystemColor );
end;

procedure FXThemeManager.LoadColorSet(var ColorSet: FXColorSets);
begin
  ColorSet.Assign( SystemColorSet );
end;

procedure FXThemeManager.LoadColorSet(var ColorSet: FXCompleteColorSet);
begin
  ColorSet.Assign( SystemColor );
end;

procedure FXThemeManager.LoadColorSet(var ColorSet: FXCompleteColorSets);
begin
  ColorSet.Assign( SystemColorSet );
end;

function FXThemeManager.GetThemePrimaryColor: TColor;
begin
  if FDarkTheme then
    Result := 0
  else
    Result := TColors.White;
end;

procedure FXThemeManager.HandleApplicationError(Sender: TObject; E: Exception);
begin
  if FHandleApplicationExceptions then
    ShowException(E)
  else
    Application.ShowException(E);
end;

procedure FXThemeManager.LoadFontSettings;
begin
  if Screen.Fonts.IndexOf(FORM_ICON_FONT_NAME_NEW) <> -1 then
    FIconFont := FORM_ICON_FONT_NAME_NEW
  else
    FIconFont := FORM_ICON_FONT_NAME_LEGACY;

  FFormFont := FORM_FONT_NAME;
  FFormFontHeight := FORM_FONT_HEIGHT;
  FSmallFontHeight := SMALL_FONT_HEIGHT;
  FLargeFontHeight := LARGE_FONT_HEIGHT;
end;

procedure FXThemeManager.MeasuredUpdateSettings;
begin
  if SecondsBetween(LastUpdate, Now) < 1 then
    Exit;

  UpdateSettings;
end;

procedure FXThemeManager.NotifyUpdate;
var
  I: integer;
begin
  for I := 0 to Screen.FormCount - 1 do
    if Screen.Forms[i] <> nil then
      if Supports(Screen.Forms[i], IFXComponent) then
        (Screen.Forms[i] as IFXComponent).UpdateTheme(true);
end;

procedure FXThemeManager.ProcessSystemMenu(Menu: TObject);
begin
  if not (Menu is FXPopupMenu) then
    Exit;
  const M = FXPopupMenu(Menu);

  // Set types
  M.AnimationType := SystemMenuAnimation;
  M.EnableBorder := SystemMenuEnableBorder;
  M.EnableRadius := SystemMenuEnableRadius;
  M.FlatMenu := SystemMenuFlat;
end;

procedure FXThemeManager.RegMonitorProc(Sender: TObject);
begin
  // Manual theme override
  if FDarkThemeMode = FXDarkSetting.Auto then
    // Check registry
    UpdateThemeInformation;
end;

procedure FXThemeManager.SaveAccentColor(const Value: TColor);
begin
  FAutoAccentColor := false;
  FAccentColor := Value;

  UpdateSettings;
end;

procedure FXThemeManager.SetAutoAccent(const Value: boolean);
begin
  FAutoAccentColor := Value;

  if Value then
    UpdateSettings;
end;

procedure FXThemeManager.SetDarkMode(const Value: FXDarkSetting);
var
  DarkModeValue: boolean;
begin
  // Set new state
  FDarkThemeMode := Value;

  // Get dark mode value
  if Value = FXDarkSetting.Auto then
    DarkModeValue := DarkTheme
  else
    DarkModeValue := Value = FXDarkSetting.ForceDark;

  // Update UI
  if FDarkTheme <> DarkModeValue then
    begin
      FDarkTheme := DarkModeValue;

      UpdateSettings;
    end;
end;

procedure FXThemeManager.SetDarkTheme(const Value: boolean);
begin
  if FDarkTheme <> Value then
  begin
    FDarkTheme := Value;

    UpdateSettings;
  end;
end;

procedure FXThemeManager.ShowException(E: Exception);
var
  Dialog: FXIconDialog;
begin
  Dialog := FXIconDialog.Create;
  try
    Dialog.Title := 'Application error';
    Dialog.Text := E.ToString;
    Dialog.Kind := FXDialogKind.Error;

    Dialog.AddButton('Close', '', true);
    Dialog.AddButton('Copy', #$E8C8, false);
    if ExceptionDisplayHalt then
      Dialog.AddButton('Halt', #$ECE4, false);

    MessageBeep( MB_ICONERROR );
    try
      case Dialog.Execute of
        1: Clipboard.AsText := E.ToString;
        2: begin
          if OpenDialog('Are you sure?', 'Are you sure you want to halt the application? Data may be lost or corrupted.', [mbYes, mbNo]) = mrYes then
            Halt;
        end;
      end;
    except
      on E: Exception do
        Application.ShowException(Exception.Create('Could not display error.'#13 + E.ToString));
    end;
  finally
    Dialog.Free;
  end;
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

procedure FXThemeManager.UpdateColors;
begin
  // Update system color
  SystemColor.LoadFrom(SystemColorSet, FDarkTheme);

  // Update Accent
  UpdateAccentColor;

  // Get Accent
  SystemColor.Accent := AccentColor;

  // Update app settings
  AllowDarkModeForApp(FDarkTheme);

  // Create System Defaults
  SystemColorSet.Accent := AccentColor;
  SystemGrayControl.Accent := AccentColor;
  SystemAccentInteractStates.LoadColors(AccentColor,
      ChangeColorLight(AccentColor, ACCENT_DIFFERENTIATE_CONST),
      ChangeColorLight(AccentColor, -ACCENT_DIFFERENTIATE_CONST));
end;

procedure FXThemeManager.UpdateSettings;
begin
  // Date
  LastUpdate := Now;

  // Update
  LoadFontSettings;
  UpdateColors;

  // Notify
  NotifyUpdate;
end;

procedure FXThemeManager.UpdateThemeInformation;
var
  DrkMode: boolean;
begin
  // Get current dark theme state
  DrkMode := GetAppsUseDarkTheme;

  if DrkMode <> DarkTheme then
    begin
      DarkTheme := DrkMode;
    end;
end;

initialization
  // Manager
  ThemeManager := FXThemeManager.Create;

finalization
  ThemeManager.Free;
end.

