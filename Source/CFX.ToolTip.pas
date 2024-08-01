unit CFX.ToolTip;

interface
uses
  Classes, Types, Windows, Messages, Vcl.Controls, Vcl.Graphics, CFX.Constants,
  CFX.ThemeManager, CFX.Colors, CFX.Graphics;

type
  FXCustomTooltip = class(THintWindow)
  const
    VERT_SPACE: Byte = 5;
    HORZ_SPACE: Byte = 7;
  private
    var ShowShadow: Boolean;
    var BorderThickness: Byte;
    var BorderColor, BackColor: TColor;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
    procedure NCPaint(DC: HDC); override;
  public
    constructor Create(aOwner: TComponent); override;
    function CalcHintRect(MaxWidth: Integer; const AHint: string; AData: Pointer): TRect; override;
  end;

  FXLightTooltip = class(FXCustomTooltip)
  public
    constructor Create(aOwner: TComponent); override;

    procedure ApplyColor;
  end;

  FXDarkTooltip = class(FXCustomTooltip)
  public
    constructor Create(aOwner: TComponent); override;

    procedure ApplyColor;
  end;

implementation

{ FXLightTooltip }
constructor FXLightTooltip.Create(aOwner: TComponent);
begin
  inherited;
  ApplyColor;
end;

procedure FXLightTooltip.ApplyColor;
begin
  inherited;
  BackColor := ThemeManager.FSystemToolTip.LightBackGround;
  BorderColor := ThemeManager.FSystemToolTip.LightBackGroundInterior;
end;

{ FXDarkTooltip }
constructor FXDarkTooltip.Create(aOwner: TComponent);
begin
  inherited;
  ApplyColor;
end;

procedure FXDarkTooltip.ApplyColor;
begin
  inherited;
  BackColor := ThemeManager.FSystemToolTip.DarkBackGround;
  BorderColor := ThemeManager.FSystemToolTip.DarkBackGroundInterior;
end;

{ FXCustomTooltip }
//  MAIN CLASS
constructor FXCustomTooltip.Create(aOwner: TComponent);
begin
  inherited;
  ShowShadow := true;
  BorderThickness := TOOLTIP_WIDTH;
  Font.Name := TOOLTIP_FONT_NAME;
  Font.Size := TOOLTIP_FONT_SIZE;
end;

procedure FXCustomTooltip.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style and not WS_BORDER;
  if not ShowShadow then
    Params.WindowClass.style := Params.WindowClass.style and not CS_DROPSHADOW;
end;

//  CUSTOM METHODS
function FXCustomTooltip.CalcHintRect(MaxWidth: Integer; const AHint: string; AData: Pointer): TRect;
begin
  Canvas.Font.Assign(Font);
  Result := Rect(0, 0, MaxWidth, 0);
  DrawText(Canvas.Handle, AHint, -1, Result,
    DT_CALCRECT or DT_LEFT or DT_WORDBREAK or DT_NOPREFIX or DrawTextBiDiModeFlagsReadingOnly);
  Inc(Result.Right, 2 * (HORZ_SPACE + BorderThickness));
  Inc(Result.Bottom, 2 * (VERT_SPACE + BorderThickness));
end;

procedure FXCustomTooltip.Paint;
var
  TextRect: TRect;
begin
  //  Do not inherited
  //  Paint background
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := BackColor;
  Canvas.FillRect(Rect(0, 0, Width, Height));
  //  Draw border
  Canvas.Brush.Style := bsClear;
  DrawBorder(Canvas, Rect(0, 0, Width, Height), BorderColor, BorderThickness, TOOLTIP_ROUND);
  //  Draw text
  Canvas.Font.Assign(Font);
  Canvas.Font.Color := GetTextColorFromBackground(BackColor);
  TextRect := Rect(
    HORZ_SPACE + BorderThickness, VERT_SPACE + BorderThickness,
    Width - HORZ_SPACE - BorderThickness, Height - VERT_SPACE - BorderThickness);
  DrawText(Canvas.Handle, Caption, -1, TextRect, DT_WORDBREAK or DT_LEFT or DT_VCENTER or DT_END_ELLIPSIS);
end;

procedure FXCustomTooltip.NCPaint(DC: HDC);
begin
  //  Do nothing
end;
end.
