unit CFX.Effects;

interface

uses
  Classes,
  Messages,
  Windows,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.ExtCtrls,
  Types,
  CFX.Colors,
  CFX.ThemeManager,
  CFX.Graphics,
  CFX.Constants,
  SysUtils,
  CFX.Classes,
  CFX.Types,
  CFX.VarHelpers,
  CFX.BlurFunctions,
  CFX.Linker,
  Math,
  CFX.GDI,
  CFX.Controls;

type
  FXEffect = class(FXWindowsControl, FXControl)
  private
    var DrawRect: TRect;
    FDrawColors: FXCompleteColorSet;
    FCustomColors: FXColorSets;
    FHitTest: boolean;

    //  Internal
    procedure UpdateColors;
    procedure UpdateRects;

  protected
    procedure PaintBuffer; override;
    procedure Resize; override;

    procedure DrawBackground(var Background: TBitMap); override;

    procedure ApplyEffect(Background: TBitMap); virtual;

    procedure CreateParams(var Params: TCreateParams); override;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;

    // Scaler
    procedure ScaleChanged(Scaler: single); override;

    // State
    procedure InteractionStateChanged(AState: FXControlState); override;

  published
    // Custom Colors
    property CustomColors: FXColorSets read FCustomColors write FCustomColors stored true;

    // Props
    property HitTest: boolean read FHitTest write FHitTest;

    // Events
    property OnPaintBuffer;

    // Default props
    property Align;
    property PaddingFill;
    property Constraints;
    property Anchors;
    property OnEnter;
    property OnExit;
    property OnClick;
    property OnMouseUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;

  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    // Interface
    function IsContainer: Boolean;
    procedure UpdateTheme(const UpdateChildren: Boolean);

    function Background: TColor;
  end;

  { Blur background }
  FXBlurEffect = class(FXEffect)
  private
    FBlurRadius: real;
    FBlurScale: integer;

    procedure SetRadius(const Value: real);
    procedure SetScale(const Value: integer);

  protected
    procedure ApplyEffect(Background: TBitMap); override;

  published
    property BlurRadius: real read FBlurRadius write SetRadius;
    property BlurScale: integer read FBlurScale write SetScale;

  public
    constructor Create(aOwner: TComponent); override;
  end;

  { Fill with color }
  FXColorEffect = class(FXEffect)
  private
    FColor: FXColor;
    procedure SetColor(const Value: FXColor);

  protected
    procedure ApplyEffect(Background: TBitMap); override;

  published
    property Color: FXColor read FColor write SetColor;

  public
    constructor Create(aOwner: TComponent); override;
  end;

  { Zoom }
  FXZoomEffect = class(FXEffect)
  private
    FZoom: real;
    procedure SetZoom(const Value: real);

  protected
    procedure ApplyEffect(Background: TBitMap); override;

  published
    property Zoom: real read FZoom write SetZoom;

  public
    constructor Create(aOwner: TComponent); override;
  end;

  { Grayscale }
  FXGrayscaleEffect = class(FXEffect)
  private

  protected
    procedure ApplyEffect(Background: TBitMap); override;
  end;

  { Invert }
  FXInvertEffect = class(FXEffect)
  private

  protected
    procedure ApplyEffect(Background: TBitMap); override;
  end;

  { Invert }
  FXDeepFryEffect = class(FXEffect)
  private

  protected
    procedure ApplyEffect(Background: TBitMap); override;
  end;

  { Glow }
  FXGlowEffect = class(FXEffect)
  private

  protected
    procedure ApplyEffect(Background: TBitMap); override;
  end;

implementation

procedure FXEffect.ApplyEffect;
begin
  // none
end;

function FXEffect.Background: TColor;
begin
  Result := FDrawColors.BackGround;
end;

constructor FXEffect.Create(aOwner: TComponent);
begin
  inherited;
  // Props
  FHitTest := false;
  TabStop := false;
  AutoFocusLine := false;

  // Custom Color
  FCustomColors := FXColorSets.Create(Self);

  FDrawColors := FXCompleteColorSet.Create;

  // Sizing
  Height := 40;
  Width := 200;

  // Update
  UpdateRects;
  UpdateColors;
end;

procedure FXEffect.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.ExStyle := Params.ExStyle;
end;

destructor FXEffect.Destroy;
begin
  FreeAndNil( FCustomColors );
  FreeAndNil( FDrawColors );
  inherited;
end;

procedure FXEffect.DrawBackground(var Background: TBitMap);
begin
  inherited;
  if Enabled then
    ApplyEffect(Background);
end;

procedure FXEffect.InteractionStateChanged(AState: FXControlState);
begin
  inherited;
end;

function FXEffect.IsContainer: Boolean;
begin
  Result := false;
end;

procedure FXEffect.PaintBuffer;
begin
  // Background
  Color := FDrawColors.BackGround;
  PaintBackground;

  // Inherit
  inherited;
end;

procedure FXEffect.Resize;
begin
  inherited;
  UpdateRects;
end;

procedure FXEffect.UpdateTheme(const UpdateChildren: Boolean);
begin
  UpdateColors;
  UpdateRects;
  Invalidate;
end;

procedure FXEffect.WMNCHitTest(var Message: TWMNCHitTest);
begin
  if FHitTest then
    inherited
  else
    Message.Result := HTTRANSPARENT;
end;

procedure FXEffect.UpdateColors;
begin
  FDrawColors.Assign( ThemeManager.SystemColor );

  if not Enabled then
    begin
      FDrawColors.Foreground := $808080;
    end
  else
    begin
      // Access theme manager
      if FCustomColors.Enabled then
        // Load custom
        FDrawColors.LoadFrom( FCustomColors, ThemeManager.DarkTheme )
      else
        // Build color palette
        FDrawColors.LoadFrom( ThemeManager.SystemColorSet, ThemeManager.DarkTheme );
    end;
end;

procedure FXEffect.UpdateRects;
begin
  // Rect
  DrawRect := GetClientRect;
end;

procedure FXEffect.ScaleChanged(Scaler: single);
begin
  inherited;
  // update scale
end;

{ FXBlurEffect }

procedure FXBlurEffect.ApplyEffect;
begin
  FastBlur(Background, FBlurRadius, FBlurScale);
end;

constructor FXBlurEffect.Create(aOwner: TComponent);
begin
  inherited;
  FBlurRadius := 1;
  FBlurScale := 1;
end;

procedure FXBlurEffect.SetRadius(const Value: real);
begin
  if FBlurRadius = Value then
    Exit;

  FBlurRadius := Value;
  Invalidate;
end;

procedure FXBlurEffect.SetScale(const Value: integer);
begin
  if FBlurScale = Value then
    Exit;

  FBlurScale := Value;
  Invalidate;
end;

{ FXColorEffect }

procedure FXColorEffect.ApplyEffect(Background: TBitMap);
begin
  with Background.Canvas do
    begin
      GDITint(ClipRect, FColor);
    end;
end;

constructor FXColorEffect.Create(aOwner: TComponent);
begin
  inherited;
  FColor := FXColors.Aquamarine;
end;

procedure FXColorEffect.SetColor(const Value: FXColor);
begin
  if FColor = Value then
    Exit;

  FColor := Value;
  Invalidate;
end;

{ FXZoomEffect }

procedure FXZoomEffect.ApplyEffect(Background: TBitMap);
var
  R: TRect;
begin
  inherited;
  if FZoom <> 1 then
    begin
      R := Background.Canvas.ClipRect;

      R.Width := Max(round(R.Width / Zoom), 1);
      R.Height := Max(round(R.Height / Zoom), 1);

      RectCenter(R, Background.Canvas.ClipRect);

      with Background.Canvas do
        begin
          CopyRect(ClipRect, Background.Canvas, R);
        end;
    end;
end;

constructor FXZoomEffect.Create(aOwner: TComponent);
begin
  inherited;
  FZoom := 1.25;
end;

procedure FXZoomEffect.SetZoom(const Value: real);
begin
  if (FZoom = Value) or (FZoom < 1) then
    Exit;

  FZoom := Value;
  Invalidate;
end;

{ FXGrayscaleEffect }

procedure FXGrayscaleEffect.ApplyEffect(Background: TBitMap);
begin
  Background.PixelFormat := pf32bit;
  GrayscaleBitmap(Background);
end;

{ FXInvertEffect }

procedure FXInvertEffect.ApplyEffect(Background: TBitMap);
var
  B: TBitMap;
  R: TRect;
begin
  B := TBitMap.Create(Background.Width, Background.Height);
  try
    with B.Canvas do
      begin
        FillRect(ClipRect);
      end;

      R := B.Canvas.ClipRect;
      BitBlt(Background.Canvas.Handle, R.Left, R.Top, R.Width, R.Height,
        B.Canvas.Handle, 0, 0, SRCINVERT);
  finally
    B.Free;
  end;
end;

{ FXDeepFryEffect }

procedure FXDeepFryEffect.ApplyEffect(Background: TBitMap);
var
  B: TBitMap;
  R: TRect;
begin
  B := TBitMap.Create(Background.Width, Background.Height);
  try
    with B.Canvas do
      begin
        FillRect(ClipRect);
      end;

      R := B.Canvas.ClipRect;
      BitBlt(Background.Canvas.Handle, R.Left, R.Top, R.Width, R.Height,
        B.Canvas.Handle, 0, 0, PATINVERT);
  finally
    B.Free;
  end;
end;

{ FXGlowEffect }

procedure FXGlowEffect.ApplyEffect(Background: TBitMap);
begin
  ApplyGlowEffect(Background, FXColors.Blue, 0);
end;

end.
