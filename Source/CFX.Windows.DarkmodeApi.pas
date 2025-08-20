unit CFX.Windows.DarkmodeApi;
// Originally DelphiWindowStyle.Core.Win;

interface

uses
  Winapi.Windows, Winapi.DwmApi, Winapi.UxTheme, System.UITypes,
  CFX.Windows.DarkmodeApi.Types, WinApi.UI.Composition;

{$SCOPEDENUMS ON}
{$ALIGN ON}
{$MINENUMSIZE 4}
{$WEAKPACKAGEUNIT}
{$WARN SYMBOL_PLATFORM OFF}

function SetSystemBackdropType(Handle: THandle; const Value: TSystemBackdropType): Boolean;

function SetExtendFrameIntoClientArea(Handle: THandle; const Value: TRect): Boolean;

function SetWindowCaptionColor(Handle: THandle; const Value: TColor): Boolean;

function SetWindowTextColor(Handle: THandle; const Value: TColor): Boolean;

function SetWindowBorderColor(Handle: THandle; const Value: TColor): Boolean;

function SetWindowCorner(Handle: THandle; const Value: TWindowCornerPreference): Boolean;

function SetWindowColorModeAsSystem(Handle: THandle): Boolean;

function SetWindowColorMode(Handle: THandle; const IsDark: Boolean): Boolean;

procedure TestFuncs(Handle: THandle);

procedure AnimateWindow(Handle: THandle; Time: Cardinal; Animate: Cardinal);

//

procedure RefreshTitleBarThemeColor(Handle: THandle);

function IsHighContrast: Boolean;

function SystemIsDarkMode: Boolean;

function IsDarkModeAllowedForWindow(Handle: THandle): Boolean;

function GetIsImmersiveColorUsingHighContrast(Mode: TImmersiveHCCacheMode): Boolean;

function ShouldAppsUseDarkMode: Boolean;

function SetAccentPolicy(Handle: THandle; GradientColor: TColor): Boolean;

//

function ImmersiveDarkMode: TDwmWindowAttribute;

function AllowDarkModeForWindow(Handle: THandle; Allow: Boolean): Boolean;

procedure AllowDarkModeForApp(Allow: Boolean);

function GetAdjustWindowRect(Handle: THandle): TRect;

function GetAeroColor: TAlphaColor;

implementation

uses
  System.Types, System.Math, System.Classes, Winapi.CommCtrl, Winapi.Messages,
  System.SysUtils, System.Win.Registry;

function GetAeroColor: TAlphaColor;
var
  OpaqueBlend: Bool;
  AColor: DWord;
  //A, R, G, B: Integer;
  OSInfo: TOSVersionInfo;
begin
  ZeroMemory(@OSInfo, SizeOf(OSInfo));
  OSInfo.dwOSVersionInfoSize := SizeOf(TOSVERSIONINFO);
  if (((not GetVersionEx(OSInfo)) and (OSInfo.dwPlatformId <> VER_PLATFORM_WIN32_NT) and (OSInfo.dwMajorVersion < 5))) or (Winapi.Dwmapi.DwmGetColorizationColor(AColor, OpaqueBlend) = S_FALSE) then
  begin
    Result := TColors.SysNone;
    Exit;
  end;             {
  A := (AColor and $FF000000) shr 24;
  R := (AColor and $00FF0000) shr 16;
  G := (AColor and $0000FF00) shr 8;
  B := (AColor and $000000FF);      }

  Result := AColor;
end;

function CheckPerMonitorV2SupportForWindow(AHandle: HWnd): Boolean;
begin
  Result := (AHandle <> 0) and (TOSVersion.Major >= 10) and (TOSVersion.Build >= 14393) and
    AreDpiAwarenessContextsEqual(DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE_V2, GetWindowDpiAwarenessContext(AHandle));
end;

function AdjustWindowRectExForWindow(var lpRect: TRect; dwStyle: DWORD; bMenu: BOOL; dwExStyle: DWORD; AHandle: HWnd): Boolean;
begin
  if CheckPerMonitorV2SupportForWindow(AHandle) then
    Result := AdjustWindowRectExForDpi(lpRect, dwStyle, bMenu, dwExStyle, GetDPIForWindow(AHandle))
  else
    Result := AdjustWindowRectEx(lpRect, dwStyle, bMenu, dwExStyle);
end;

function GetAdjustWindowRect(Handle: THandle): TRect;
var
  dwStyle, dwExStyle: DWORD;
begin
  Result := TRect.Empty;
  dwStyle := GetWindowLong(Handle, GWL_STYLE);
  dwExStyle := GetWindowLong(Handle, GWL_EXSTYLE);
  {$IF CompilerVersion < 34}
  AdjustWindowRectEx(Result, dwStyle, False, dwExStyle);
  {$ELSE}
  AdjustWindowRectExForWindow(Result, dwStyle, False, dwExStyle, Handle);
  {$ENDIF}
end;

procedure AnimateWindow(Handle: THandle; Time: Cardinal; Animate: Cardinal);
begin
  Winapi.Windows.AnimateWindow(Handle, Time, Animate);
end;

function SetAccentPolicy(Handle: THandle; GradientColor: TColor): Boolean;
const
  //
  DrawLeftBorder = $20;
  DrawTopBorder = $40;
  DrawRightBorder = $80;
  DrawBottomBorder = $100;
var
  DWM: THandle;
  CompAttrData: WINDOWCOMPOSITIONATTRIBDATA;
  Accent: ACCENT_POLICY;
var
  SetWindowCompositionAttribute: function(Wnd: hWnd; const AttrData: WINDOWCOMPOSITIONATTRIBDATA): BOOL; stdcall;
begin
  Result := False;

  DWM := LoadLibrary('user32.dll');
  try
    @SetWindowCompositionAttribute := GetProcAddress(DWM, 'SetWindowCompositionAttribute');
    if @SetWindowCompositionAttribute <> nil then
    begin
      if GradientColor <> TColorRec.Null then
      begin
        Accent.GradientColor := GradientColor;
        Accent.AccentState := ACCENT_STATE.ACCENT_ENABLE_ACRYLICBLURBEHIND;
      end
      else
      begin
        Accent.AccentState := ACCENT_STATE.ACCENT_ENABLE_BLURBEHIND;
      end;

      Accent.AccentFlags := DrawLeftBorder or DrawTopBorder or DrawRightBorder or DrawBottomBorder;
      CompAttrData.Attribute := WINDOWCOMPOSITIONATTRIB.WCA_ACCENT_POLICY;
      CompAttrData.DataSize := SizeOf(Accent);
      CompAttrData.Data := @Accent;
      Result := SetWindowCompositionAttribute(Handle, CompAttrData);
    end;
  finally
    FreeLibrary(DWM);
  end;
  SetWindowCorner(Handle, TWindowCornerPreference.DWMWCP_ROUND);
end;

procedure TestFuncs(Handle: THandle);
begin
  // EnableBlur(Handle);
  // COLORREF dwColorBorder = MenuManager::g_bIsDarkMode ? WIN11_POPUP_BORDER_DARK : WIN11_POPUP_BORDER_LIGHT;
  // DwmSetWindowAttribute(hWnd, DWMWA_BORDER_COLOR, &dwColorBorder, sizeof(COLORREF));
end;

function SetSystemBackdropType(Handle: THandle; const Value: TSystemBackdropType): Boolean;
begin
  Result := Succeeded(DwmSetWindowAttribute(Handle, Ord(TDwmWindowAttribute.DWMWA_SYSTEMBACKDROP_TYPE), @Value, SizeOf(Integer)));
end;

function SetExtendFrameIntoClientArea(Handle: THandle; const Value: TRect): Boolean;
begin
  var Margins: TMargins;
  Margins.cxLeftWidth := Value.Left;
  Margins.cxRightWidth := Value.Right;
  Margins.cyTopHeight := Value.Top;
  Margins.cyBottomHeight := Value.Bottom;
  Result := Succeeded(DwmExtendFrameIntoClientArea(Handle, Margins));
end;

function SetWindowCaptionColor(Handle: THandle; const Value: TColor): Boolean;
begin
  if Value = TColors.Null then
  begin
    var
      DefVal := DWMWA_COLOR_DEFAULT;
    Result := Succeeded(DwmSetWindowAttribute(Handle, Ord(TDwmWindowAttribute.DWMWA_CAPTION_COLOR), @DefVal, SizeOf(DWMWA_COLOR_DEFAULT)))
  end
  else
    Result := Succeeded(DwmSetWindowAttribute(Handle, Ord(TDwmWindowAttribute.DWMWA_CAPTION_COLOR), @Value, SizeOf(TColorRef)));
end;

function SetWindowTextColor(Handle: THandle; const Value: TColor): Boolean;
begin
  Result := Succeeded(DwmSetWindowAttribute(Handle, Ord(TDwmWindowAttribute.DWMWA_TEXT_COLOR), @Value, SizeOf(TColorRef)));
end;

function SetWindowBorderColor(Handle: THandle; const Value: TColor): Boolean;
begin
  Result := Succeeded(DwmSetWindowAttribute(Handle, Ord(TDwmWindowAttribute.DWMWA_BORDER_COLOR), @Value, SizeOf(TColorRef)));
end;

function SetWindowCorner(Handle: THandle; const Value: TWindowCornerPreference): Boolean;
begin
  Result := Succeeded(DwmSetWindowAttribute(Handle, Ord(TDwmWindowAttribute.DWMWA_WINDOW_CORNER_PREFERENCE), @Value, SizeOf(Integer)));
end;

function SetWindowColorModeAsSystem(Handle: THandle): Boolean;
begin
  Result := SetWindowColorMode(Handle, SystemIsDarkMode);
end;

function SetWindowColorMode(Handle: THandle; const IsDark: Boolean): Boolean;
begin
  var Value: LongBool := IsDark;
  Result := Succeeded(DwmSetWindowAttribute(Handle, Ord(ImmersiveDarkMode), @Value, SizeOf(Value)));
  if Result then
  begin
    AllowDarkModeForWindow(Handle, IsDark);
    AllowDarkModeForApp(IsDark);
  end;
end;

function SystemIsDarkMode: Boolean;
var
  LRegistry: TRegistry;
begin
  Result := False;
  try
    LRegistry := TRegistry.Create;
    try
      LRegistry.RootKey := HKEY_CURRENT_USER;
      if LRegistry.OpenKeyReadOnly('\SOFTWARE\Microsoft\Windows\CurrentVersion\Themes\Personalize') then
        Result := not LRegistry.ReadBool('AppsUseLightTheme');
    finally
      LRegistry.Free;
    end;
  except
    Result := False;
  end;
end;

function IsHighContrast: Boolean;
var
  Info: HIGHCONTRASTW;
begin
  Info.cbSize := SizeOf(Info);
  if SystemParametersInfo(SPI_GETHIGHCONTRAST, SizeOf(Info), @Info, Ord(False)) then
    Result := Info.dwFlags and HCF_HIGHCONTRASTON <> 0
  else
    Result := False;
end;

//

var
  WinAllowDarkModeForApp: function(Allow: BOOL): BOOL; stdcall;
  WinAllowDarkModeForWindow: function(hWnd: hWnd; Allow: BOOL): BOOL; stdcall;
  WinGetIsImmersiveColorUsingHighContrast: function(Mode: TImmersiveHCCacheMode): BOOL; stdcall;
  WinIsDarkModeAllowedForWindow: function(hWnd: hWnd): BOOL; stdcall;
  WinRefreshImmersiveColorPolicyState: procedure; stdcall;
  WinSetPreferredAppMode: function(appMode: TPreferredAppMode): TPreferredAppMode; stdcall;
  SetWindowCompositionAttribute: function(hWnd: hWnd; pData: PWindowCompositionAttribData): BOOL; stdcall;
  WinShouldAppsUseDarkMode: function: BOOL; stdcall;
  GDarkModeSupported: BOOL = False; // changed type to BOOL
  GDarkModeEnabled: BOOL = False;
  GUxTheme: HMODULE = 0;

const
  LOAD_LIBRARY_SEARCH_SYSTEM32 = $00000800;

const
  themelib = 'uxtheme.dll';

procedure RefreshTitleBarThemeColor(Handle: THandle);
var
  LUseDark: BOOL;
  LData: TWindowCompositionAttribData;
begin
  if not Assigned(WinIsDarkModeAllowedForWindow) then
    Exit;
  if not Assigned(WinShouldAppsUseDarkMode) then
    Exit;
  LUseDark := WinIsDarkModeAllowedForWindow(Handle) and WinShouldAppsUseDarkMode and not IsHighContrast;
  if TOSVersion.Build < 18362 then
    SetProp(Handle, 'UseImmersiveDarkModeColors', THandle(LUseDark))
  else if Assigned(SetWindowCompositionAttribute) then
  begin
    LData.Attribute := TWindowCompositionAttrib.WCA_USEDARKMODECOLORS;
    LData.Data := @LUseDark;
    LData.DataSize := SizeOf(LUseDark);
    SetWindowCompositionAttribute(Handle, @LData);
  end;
end;

function AllowDarkModeForWindow(Handle: THandle; Allow: Boolean): Boolean;
begin
  Result := GDarkModeSupported and WinAllowDarkModeForWindow(Handle, Allow);
end;

function IsDarkModeAllowedForWindow(Handle: THandle): Boolean;
begin
  Result := Assigned(WinIsDarkModeAllowedForWindow) and WinIsDarkModeAllowedForWindow(Handle);
end;

function GetIsImmersiveColorUsingHighContrast(Mode: TImmersiveHCCacheMode): Boolean;
begin
  Result := Assigned(WinGetIsImmersiveColorUsingHighContrast) and WinGetIsImmersiveColorUsingHighContrast(Mode);
end;

//

procedure AllowDarkModeForApp(Allow: Boolean);
begin
  if Assigned(WinAllowDarkModeForApp) then
    WinAllowDarkModeForApp(Allow)
  else if Assigned(WinSetPreferredAppMode) then
  begin
    if Allow then
      WinSetPreferredAppMode(TPreferredAppMode.AllowDarkMode)
    else
      WinSetPreferredAppMode(TPreferredAppMode.DefaultMode);
  end;
end;

function ShouldAppsUseDarkMode: Boolean;
begin
  Result := Assigned(WinShouldAppsUseDarkMode) and WinShouldAppsUseDarkMode;
end;

function IsWindows10OrGreater(buildNumber: DWORD): Boolean;
begin
  Result := (TOSVersion.Major > 10) or
    ((TOSVersion.Major = 10) and (TOSVersion.Minor = 0) and (DWORD(TOSVersion.Build) >= buildNumber));
end;

function IsWindows11OrGreater(buildNumber: DWORD): Boolean;
begin
  Result := IsWindows10OrGreater(22000) or IsWindows10OrGreater(buildNumber);
end;

function CheckBuildNumber(buildNumber: DWORD): Boolean;
begin
  Result := //
    IsWindows10OrGreater(20348) or //
    IsWindows10OrGreater(19045) or //
    IsWindows10OrGreater(19044) or //
    IsWindows10OrGreater(19043) or //
    IsWindows10OrGreater(19042) or //
    IsWindows10OrGreater(19041) or // 2004
    IsWindows10OrGreater(18363) or // 1909
    IsWindows10OrGreater(18362) or // 1903
    IsWindows10OrGreater(17763); // 1809
end;

function ImmersiveDarkMode: TDwmWindowAttribute;
begin
  if IsWindows10OrGreater(18985) then
    Result := TDwmWindowAttribute.DWMWA_USE_IMMERSIVE_DARK_MODE
  else
    Result := TDwmWindowAttribute.DWMWA_USE_IMMERSIVE_DARK_MODE_BEFORE_20H1;
end;

procedure InitDarkMode;
begin
  if (TOSVersion.Major = 10) and (TOSVersion.Minor = 0) and CheckBuildNumber(TOSVersion.Build) then
  begin
    GUxTheme := LoadLibraryEx(themelib, 0, LOAD_LIBRARY_SEARCH_SYSTEM32);
    if GUxTheme <> 0 then
    begin
      @WinAllowDarkModeForWindow := GetProcAddress(GUxTheme, MAKEINTRESOURCEA(133));
      @WinGetIsImmersiveColorUsingHighContrast := GetProcAddress(GUxTheme, MAKEINTRESOURCEA(106));
      @WinIsDarkModeAllowedForWindow := GetProcAddress(GUxTheme, MAKEINTRESOURCEA(137));
      @WinRefreshImmersiveColorPolicyState := GetProcAddress(GUxTheme, MAKEINTRESOURCEA(104));
      @SetWindowCompositionAttribute := GetProcAddress(GetModuleHandle(user32), 'SetWindowCompositionAttribute');
      @WinShouldAppsUseDarkMode := GetProcAddress(GUxTheme, MAKEINTRESOURCEA(132));

      if TOSVersion.Build < 18362 then
        @WinAllowDarkModeForApp := GetProcAddress(GUxTheme, MAKEINTRESOURCEA(135))
      else
        @WinSetPreferredAppMode := GetProcAddress(GUxTheme, MAKEINTRESOURCEA(135));

      if Assigned(WinRefreshImmersiveColorPolicyState) and
        Assigned(WinShouldAppsUseDarkMode) and
        Assigned(WinAllowDarkModeForWindow) and
        (Assigned(WinAllowDarkModeForApp) or Assigned(WinSetPreferredAppMode)) and
        Assigned(WinIsDarkModeAllowedForWindow)
        then
      begin
        GDarkModeSupported := True;
        AllowDarkModeForApp(True);
        WinRefreshImmersiveColorPolicyState;
        GDarkModeEnabled := ShouldAppsUseDarkMode and not IsHighContrast;
      end;
    end;
  end;
end;

procedure DoneDarkMode;
begin
  if GUxTheme <> 0 then
    FreeLibrary(GUxTheme);
end;

initialization
  InitDarkMode;

finalization
  DoneDarkMode;

end.

