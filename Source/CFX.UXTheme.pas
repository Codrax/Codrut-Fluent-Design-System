unit CFX.UXTheme;
{$WARN SYMBOL_PLATFORM OFF}
{$ALIGN ON}
{$MINENUMSIZE 4}

interface
  uses
    Windows, SysUtils;

  type
    TPreferredAppMode = (DefaultMode, AllowDarkMode, ForceDarkMode, ForceLightMode, ModeMax);

implementation

uses
  CFX.ThemeManager;

var
  UXThemeDLL: HMODULE = 0;

type
  SystemcallThemePre18 = function(allow: BOOL): BOOL; stdcall;
    SystemcallTheme = function(appMode: TPreferredAppMode): TPreferredAppMode;

function IsWindows10OrGreater(buildNumber: DWORD): Boolean;
begin
  Result := (TOSVersion.Major > 10) or ((TOSVersion.Major = 10) and (TOSVersion.Minor = 0) and (DWORD(TOSVersion.Build) >= buildNumber));
end;
function CheckBuildNumber(buildNumber: DWORD): Boolean;
begin
  Result :=
    IsWindows10OrGreater(20348) or
    IsWindows10OrGreater(19045) or  //
    IsWindows10OrGreater(19044) or  //
    IsWindows10OrGreater(19043) or  //
    IsWindows10OrGreater(19042) or  //
    IsWindows10OrGreater(19041) or  // 2004
    IsWindows10OrGreater(18363) or  // 1909
    IsWindows10OrGreater(18362) or  // 1903
    IsWindows10OrGreater(17763);    // 1809
end;

initialization
  if ((TOSVersion.Major <> 10) or (TOSVersion.Minor <> 0) or not CheckBuildNumber(TOSVersion.Build)) then
    Exit;

  // Load undocumented DLL proc only for Win64
  {$IFDEF WIN64}
  UXThemeDLL := LoadLibrary('uxtheme.dll');
  if (UXThemeDLL <> 0) and not IsDesigning then begin
    const Proc = GetProcAddress(UXThemeDLL, MakeIntResource(135));
    if TOSVersion.Build < 18362 then
      SystemcallThemePre18(Proc)(true)
    else
      SystemcallTheme(Proc)(TPreferredAppMode.AllowDarkMode);
  end;
  {$ENDIF}

finalization
  if UXThemeDLL <> 0 then
    FreeLibrary(UXThemeDLL);

end.
