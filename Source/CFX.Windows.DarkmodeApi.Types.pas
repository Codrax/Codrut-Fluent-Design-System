unit CFX.Windows.DarkmodeApi.Types;
// Originally DelphiWindowStyle.Types;

{$ALIGN ON}
{$MINENUMSIZE 4}
{$WEAKPACKAGEUNIT}
{$SCOPEDENUMS ON}
{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Winapi.Windows;

type
  TDwmWindowAttribute = (                             //
    DWMWA_NCRENDERING_ENABLED = 1,                    //
    DWMWA_NCRENDERING_POLICY,                         //
    DWMWA_TRANSITIONS_FORCEDISABLED,                  //
    DWMWA_ALLOW_NCPAINT,                              //
    DWMWA_CAPTION_BUTTON_BOUNDS,                      //
    DWMWA_NONCLIENT_RTL_LAYOUT,                       //
    DWMWA_FORCE_ICONIC_REPRESENTATION,                //
    DWMWA_FLIP3D_POLICY,                              //
    DWMWA_EXTENDED_FRAME_BOUNDS,                      //
    DWMWA_HAS_ICONIC_BITMAP,                          //
    DWMWA_DISALLOW_PEEK,                              //
    DWMWA_EXCLUDED_FROM_PEEK,                         //
    DWMWA_CLOAK,                                      //
    DWMWA_CLOAKED,                                    //
    DWMWA_FREEZE_REPRESENTATION,                      //
    DWMWA_PASSIVE_UPDATE_MODE,                        //
    DWMWA_USE_HOSTBACKDROPBRUSH,                      //17
    DWMWA_USE_IMMERSIVE_DARK_MODE_BEFORE_20H1 = 19,   //
    DWMWA_USE_IMMERSIVE_DARK_MODE = 20,               //
    DWMWA_WINDOW_CORNER_PREFERENCE = 33,              //
    DWMWA_BORDER_COLOR,                               //
    DWMWA_CAPTION_COLOR,                              //
    DWMWA_TEXT_COLOR,                                 //
    DWMWA_VISIBLE_FRAME_BORDER_THICKNESS,             //
    DWMWA_SYSTEMBACKDROP_TYPE,                        //
    DWMWA_LAST);                                      //

  TWindowCornerPreference = (    //
    DWMWCP_DEFAULT = 0,          // Default
    DWMWCP_DONOTROUND = 1,       // Rectangular
    DWMWCP_ROUND = 2,            // Default
    DWMWCP_ROUNDSMALL = 3);      // Semi-rounded

  TImmersiveHCCacheMode = (IHCM_USE_CACHED_VALUE, IHCM_REFRESH);

  TPreferredAppMode = (DefaultMode, AllowDarkMode, ForceDarkMode, ForceLightMode, ModeMax);

  WINDOWCOMPOSITIONATTRIB = (               //
    WCA_UNDEFINED = 0,                      //
    WCA_NCRENDERING_ENABLED = 1,            //
    WCA_NCRENDERING_POLICY = 2,             //
    WCA_TRANSITIONS_FORCEDISABLED = 3,      //
    WCA_ALLOW_NCPAINT = 4,                  //
    WCA_CAPTION_BUTTON_BOUNDS = 5,          //
    WCA_NONCLIENT_RTL_LAYOUT = 6,           //
    WCA_FORCE_ICONIC_REPRESENTATION = 7,    //
    WCA_EXTENDED_FRAME_BOUNDS = 8,          //
    WCA_HAS_ICONIC_BITMAP = 9,              //
    WCA_THEME_ATTRIBUTES = 10,              //
    WCA_NCRENDERING_EXILED = 11,            //
    WCA_NCADORNMENTINFO = 12,               //
    WCA_EXCLUDED_FROM_LIVEPREVIEW = 13,     //
    WCA_VIDEO_OVERLAY_ACTIVE = 14,          //
    WCA_FORCE_ACTIVEWINDOW_APPEARANCE = 15, //
    WCA_DISALLOW_PEEK = 16,                 //
    WCA_CLOAK = 17,                         //
    WCA_CLOAKED = 18,                       //
    WCA_ACCENT_POLICY = 19,                 //
    WCA_FREEZE_REPRESENTATION = 20,         //
    WCA_EVER_UNCLOAKED = 21,                //
    WCA_VISUAL_OWNER = 22,                  //
    WCA_HOLOGRAPHIC = 23,                   //
    WCA_EXCLUDED_FROM_DDA = 24,             //
    WCA_PASSIVEUPDATEMODE = 25,             //
    WCA_USEDARKMODECOLORS = 26,             //
    WCA_LAST = 27);

  TWindowCompositionAttrib = WINDOWCOMPOSITIONATTRIB;

  WINDOWCOMPOSITIONATTRIBDATA = record
    Attribute: WINDOWCOMPOSITIONATTRIB;
    Data: Pointer;
    DataSize: NativeUInt;
  end;
  TWindowCompositionAttribData = WINDOWCOMPOSITIONATTRIBDATA;

  PWindowCompositionAttribData = ^TWindowCompositionAttribData;


  ACCENT_STATE = (
    ACCENT_DISABLED = 0,
    ACCENT_ENABLE_GRADIENT = 1,
    ACCENT_ENABLE_TRANSPARENTGRADIENT = 2,
    ACCENT_ENABLE_BLURBEHIND = 3,
    ACCENT_ENABLE_ACRYLICBLURBEHIND = 4,  // Windows 10+
    ACCENT_ENABLE_HOSTBACKDROP = 5,       // Windows 11+
    ACCENT_INVALID_STATE = 6
  );
  TAccentState = ACCENT_STATE;


  ACCENT_POLICY = packed record
    AccentState: ACCENT_STATE;
    AccentFlags: DWORD;
    GradientColor: DWORD;  // ARGB format: 0xAABBGGRR
    AnimationId: DWORD;
  end;
  TAccentPolicy = ACCENT_POLICY;

type
  TSystemBackdropType = (       //
    DWMSBT_AUTO = 0,            // Auto
    DWMSBT_DISABLE = 1,         // None
    DWMSBT_MAINWINDOW = 2,      // Mica
    DWMSBT_TRANSIENTWINDOW = 3, // Acrylic
    DWMSBT_TABBEDWINDOW = 4);   // Tabbed

const
  AW_HOR_POSITIVE = $00000001; // Анимирует окно слева направо. Этот флаг можно использовать с анимацией наката или слайда. Он игнорируется при использовании с AW_CENTER или AW_BLEND.
  AW_HOR_NEGATIVE = $00000002; // Анимирует окно справа налево. Этот флаг можно использовать с анимацией наката или слайда. Он игнорируется при использовании с AW_CENTER или AW_BLEND.
  AW_VER_POSITIVE = $00000004; // Анимирует окно сверху вниз. Этот флаг можно использовать с анимацией наката или слайда. Он игнорируется при использовании с AW_CENTER или AW_BLEND.
  AW_VER_NEGATIVE = $00000008;  // Анимирует окно снизу вверх. Этот флаг можно использовать с анимацией наката или слайда. Он игнорируется при использовании с AW_CENTER или AW_BLEND.
  AW_CENTER = $00000010;       // Делает окно сворачиваться вовнутрь, если используется AW_HIDE , или разворачиваться наружу, если AW_HIDE не используется. Различные флаги направления не оказывают никакого влияния.
  AW_HIDE = $00010000;         // Скрывает окно. По умолчанию отображается окно .
  AW_ACTIVATE = $00020000;     // Активирует окно. Не используйте это значение с AW_HIDE.
  AW_SLIDE = $00040000;        // Использует анимацию слайдов. По умолчанию используется анимация наката. Этот флаг игнорируется при использовании с AW_CENTER.
  AW_BLEND = $00080000;        // Использует эффект исчезания. Этот флаг можно использовать только в том случае, если hwnd является окном верхнего уровня.

const
  DWMWA_COLOR_DEFAULT = $FFFFFFFF;

implementation

end.

