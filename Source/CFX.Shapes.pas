unit CFX.Shapes;

interface

uses
  Classes,
  Messages,
  Windows,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.ExtCtrls,
  Types,
  Math,
  CFX.Colors,
  CFX.ThemeManager,
  CFX.Graphics,
  CFX.Constants,
  SysUtils,
  CFX.Classes,
  CFX.ComponentClasses,
  CFX.Types,
  CFX.VarHelpers,
  CFX.ArrayHelpers,
  CFX.Linker,
  CFX.GDI,
  Winapi.GDIPAPI,
  Winapi.GDIPOBJ,
  CFX.Controls;

type
  FXShapeDrawingSettings = class(FXAssignPersistent)
  private
    FControl: FXWindowsControl;

    FDrawPen,
    FDrawBrush: boolean;

    FColorPen,
    FColorBrush: FXColor;

    FPenSize: single;

    FBrush: TGDIBrush;
    FPen: TGDIPen;

    FOnChanged: TNotifyEvent;

    procedure Updated;

    // Setters
    procedure SetColorBrush(const Value: FXColor);
    procedure SetColorPen(const Value: FXColor);
    procedure SetDrawBrush(const Value: boolean);
    procedure SetDrawPen(const Value: boolean);
    procedure SetPenSize(const Value: single);

  published
    property DrawPen: boolean read FDrawPen write SetDrawPen default true;
    property DrawBrush: boolean read FDrawBrush write SetDrawBrush default true;

    property ColorPen: FXColor read FColorPen write SetColorPen;
    property ColorBrush: FXColor read FColorBrush write SetColorBrush;

    property PenSize: single read FPenSize write SetPenSize;

    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;

  public
    property Brush: TGDIBrush read FBrush;
    property Pen: TGDIPen read FPen;

    // Constructors
    constructor Create(Control: FXWindowsControl);
    destructor Destroy; override;
  end;

  FXShape = class(FXWindowsControl)
  private
    FDrawColors: FXCompleteColorSet;
    FCustomColors: FXColorSets;

    FRotation: FXAngle;
    FProportional: boolean;

    FSettings: FXShapeDrawingSettings;

    FControlPen,
    FControlRotation: boolean;

    // GDI
    FPath: TGPGraphicsPath;

    // Getters

    // Setters
    procedure SetProportional(const Value: boolean);
    procedure SetRotation(const Value: FXAngle);
    procedure SetControlPen(const Value: boolean);
    procedure SetControlRotation(const Value: boolean);

    // Internal
    procedure DrawSettingChanged(Sender: TObject);

  protected
    var DrawRect: TRect;
    var Points: TArray<TPoint>;

    procedure PaintBuffer; override;

    // Internal
    procedure UpdateColors; override;
    procedure UpdateRects; override;

    // Shape building
    procedure BuildPoints; virtual;
    procedure RotatePoints; virtual;
    procedure CreatePath; virtual;

    // State
    procedure InteractionStateChanged(AState: FXControlState); override;

  published
    // Custom Colors
    property CustomColors: FXColorSets read FCustomColors write FCustomColors stored true;

    property Drawing: FXShapeDrawingSettings read FSettings write FSettings;

    // Props
    property Proportional: boolean read FProportional write SetProportional default false;
    property Rotation: FXAngle read FRotation write SetRotation;

    property ControlPen: boolean read FControlPen write SetControlPen default true;
    property ControlRotation: boolean read FControlRotation write SetControlRotation default false;

    // Default props
    property Align;
    property Transparent;
    property HitTest;
    property Constraints;
    property Anchors;
    property Hint;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property FocusFlags;
    property DragKind;
    property DragCursor;
    property DragMode;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnStartDrag;
    property OnEnter;
    property OnExit;
    property OnClick;
    property OnDblClick;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property OnMouseUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;

  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    // Override
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

    // Interface
    function Background: TColor; override;
  end;

  FXShapeLines = class(FXShape)
  protected
    procedure CreatePath; override;
  end;

  FXShapeSquare = class(FXShapeLines)
  protected
    // Shape building
    procedure BuildPoints; override;
  end;

  FXShapeRoundedSquare = class(FXShape)
  private
    FCornerRadius: FXCornerSettings;

    procedure CornerRadiusesUpdated(Sender: TObject);

  protected
    procedure CreatePath; override;

    // Shape building
    procedure BuildPoints; override;

  published
    property CornerRadius: FXCornerSettings read FCornerRadius write FCornerRadius;

  public
    // Constructors
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

  FXShapeCircle = class(FXShape)
  protected

  end;

  FXShapeTriangle = class(FXShapeLines)
  protected
    // Shape building
    procedure BuildPoints; override;
  end;

  FXShapeTriangleCorner = class(FXShapeLines)
  protected
    // Shape building
    procedure BuildPoints; override;
  end;


implementation

function FXShape.Background: TColor;
begin
  Result := FDrawColors.BackGround;
end;

procedure FXShape.BuildPoints;
begin
  Points := [];
end;

constructor FXShape.Create(aOwner: TComponent);
begin
  inherited;
  // Custom Color
  FCustomColors := FXColorSets.Create(Self);

  FDrawColors := FXCompleteColorSet.Create;

  FSettings := FXShapeDrawingSettings.Create( Self );
  FSettings.OnChanged := DrawSettingChanged;

  // Prop
  FProportional := false;
  FRotation := 0;

  FControlPen:= true;
  FControlRotation := false;

  // Sizing
  Height := 100;
  Width := 100;

  // GDI Obj
  FPath := TGPGraphicsPath.Create;
end;

procedure FXShape.CreatePath;
begin
  FPath.Reset;
end;

destructor FXShape.Destroy;
begin
  FreeAndNil( FCustomColors );
  FreeAndNil( FDrawColors );
  FreeAndNil( FSettings );
  FreeAndNil( FPath );
  inherited;
end;

procedure FXShape.DrawSettingChanged(Sender: TObject);
begin
  UpdateRects;
  Redraw;
end;

procedure FXShape.InteractionStateChanged(AState: FXControlState);
begin
  //
end;

procedure FXShape.PaintBuffer;
begin
  // Background
  Color := FDrawColors.BackGround;
  PaintBackground;

  // Draw
  const G = TGPGRaphics.Create(Buffer.Handle);
  try
    G.SetSmoothingMode(SmoothingModeHighQuality);

    if FSettings.DrawBrush then
      G.FillPath( FSettings.Brush, FPath );

    if FSettings.DrawPen then
      G.DrawPath( FSettings.Pen, FPath );
  finally
    G.Free;
  end;

  // Inherit
  inherited;
end;

procedure FXShape.RotatePoints;
begin
  for var I := 0 to High(Points) do
    Points[I] :=
      RotatePointAroundPoint(Points[I], DrawRect.CenterPoint, FRotation);
end;

procedure FXShape.UpdateColors;
begin
  // Access theme manager
  FDrawColors.Assign( ThemeManager.SystemColor );
  if FCustomColors.Enabled then
    // Custom Colors
    FDrawColors.LoadFrom(FCustomColors, ThemeManager.DarkTheme);
end;

procedure FXShape.UpdateRects;
var
  PenSize: integer;
begin
  // Rect
  DrawRect := GetClientRect;
  DrawRect.NormalizeRect;

  // Get values
  PenSize := 0;
  if FSettings.FDrawPen then
    PenSize := round(FSettings.PenSize);

  // Size by pen
  if FControlPen then begin
    DrawRect.Inflate( -PenSize, -PenSize, -PenSize, -PenSize );
  end;

  // Build points
  BuildPoints;

  // Automatic size control
  if ControlRotation then begin
    var TopLeft, BottomRight: TPoint; // extremeties
    TopLeft.SetLocation(0, 0);
    BottomRight.SetLocation(0, 0);

    for var I := 0 to High(Points) do begin
      const S = Point(Points[I].X-DrawRect.Left, Points[I].Y-DrawRect.Top);
      const E = Point(S.X-DrawRect.Width, S.Y-DrawRect.Height);

      if S.X < TopLeft.X then
        TopLeft.X := S.X;
      if S.Y < TopLeft.Y then
        TopLeft.Y := S.Y;

      if E.X > BottomRight.X then
        BottomRight.X := E.X;
      if E.Y > BottomRight.Y then
        BottomRight.Y := E.Y;
    end;

    TopLeft.X := Abs(TopLeft.X);
    TopLeft.X := Abs(TopLeft.Y);

    // Inflate
    if not (TopLeft.IsZero and BottomRight.IsZero) then begin
      DrawRect.Inflate(-TopLeft.X, -TopLeft.Y, -BottomRight.X, -BottomRight.Y);

      // Re-build points
      BuildPoints;
    end;
  end;

  // Rotation
  RotatePoints;

  // Path
  CreatePath;
end;

procedure FXShape.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if FProportional then begin
    const ChangeW = AWidth <> Width;
    const ChangeH = AHeight <> Height;

    if ChangeW and ChangeH then
      AHeight := AWidth
    else
    if ChangeW then
      AHeight := AWidth
    else
    if ChangeH then
      AWidth := AHeight;
  end;

  inherited;
end;

procedure FXShape.SetControlPen(const Value: boolean);
begin
  if FControlPen = Value then
    Exit;

  FControlPen := Value;
  UpdateRects;
  Redraw;
end;

procedure FXShape.SetControlRotation(const Value: boolean);
begin
  if FControlRotation = Value then
    Exit;

  FControlRotation := Value;
  UpdateRects;
  Redraw;
end;

procedure FXShape.SetProportional(const Value: boolean);
begin
  if FProportional = Value then
    Exit;

  if Value and (Height <> Width) then
    inherited Height := Width;

  FProportional := Value;
  UpdateRects;
  Redraw;
end;

procedure FXShape.SetRotation(const Value: FXAngle);
begin
  if FRotation = Value then
    Exit;

  FRotation := Value;
  UpdateRects;
  Redraw;
end;

{ FXShapeDrawingSettings }

constructor FXShapeDrawingSettings.Create(Control: FXWindowsControl);
begin
  FControl := Control;

  FDrawPen := true;
  FDrawBrush := true;

  // Default
  FPenSize := 1;
  FColorPen := FXColors.Blue;
  FColorBrush := FXColors.Lightblue;

  // Create
  FBrush := TGPSolidBrush.Create( FColorBrush );
  FPen := TGPPen.Create( FColorPen, FPenSize );
end;

destructor FXShapeDrawingSettings.Destroy;
begin
  FBrush.Free;
  FPen.Free;

  inherited;
end;

procedure FXShapeDrawingSettings.SetColorBrush(const Value: FXColor);
begin
  FColorBrush := Value;

  FBrush.SetColor( Value );

  Updated;
end;

procedure FXShapeDrawingSettings.SetColorPen(const Value: FXColor);
begin
  FColorPen := Value;

  FPen.SetColor( Value );

  Updated;
end;

procedure FXShapeDrawingSettings.SetDrawBrush(const Value: boolean);
begin
  FDrawBrush := Value;

  Updated;
end;

procedure FXShapeDrawingSettings.SetDrawPen(const Value: boolean);
begin
  FDrawPen := Value;

  Updated;
end;

procedure FXShapeDrawingSettings.SetPenSize(const Value: single);
begin
  if (FPenSize = Value) or (Value < 0) then
    Exit;

  FPenSize := Value;
  FPen.SetWidth( Value );

  Updated;
end;

procedure FXShapeDrawingSettings.Updated;
begin
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

{ FXShapeSquare }

procedure FXShapeSquare.BuildPoints;
begin
  inherited;

  Points := [
    DrawRect.TopLeft,
    Point(DrawRect.Right, DrawRect.Top),
    DrawRect.BottomRight,
    Point(DrawRect.Left, DrawRect.Bottom)
  ];
end;

{ FXShapeTriangle }

procedure FXShapeTriangle.BuildPoints;
begin
  inherited;
  Points := [
    Point(DrawRect.Right, DrawRect.Bottom-1),
    Point(DrawRect.Left, DrawRect.Bottom-1),
    Point(DrawRect.CenterPoint.X, DrawRect.Top)
  ];
end;

{ FXShapeLines }

procedure FXShapeLines.CreatePath;
begin
  inherited;

  for var I := 0 to High(Points)-1 do
    FPath.AddLine( Points[I].X, Points[I].Y, Points[I+1].X, Points[I+1].Y );

  FPath.CloseFigure;
end;

{ FXShapeTriangleCorner }

procedure FXShapeTriangleCorner.BuildPoints;
begin
  inherited;
  Points := [
    DrawRect.TopLeft,
    Point(DrawRect.Right, DrawRect.Top),
    Point(DrawRect.Left, DrawRect.Bottom)
  ];
end;

{ FXShapeRoundedSquare }

procedure FXShapeRoundedSquare.BuildPoints;
begin
  inherited;

  Points := [
    DrawRect.TopLeft,
    Point(DrawRect.Right, DrawRect.Top),
    DrawRect.BottomRight,
    Point(DrawRect.Left, DrawRect.Bottom)
  ];
end;

procedure FXShapeRoundedSquare.CornerRadiusesUpdated(Sender: TObject);
begin
  StandardUpdateComplete;
end;

constructor FXShapeRoundedSquare.Create(aOwner: TComponent);
begin
  inherited;
  FCornerRadius := FXCornerSettings.Create( Self );
  FCornerRadius.OnChange := CornerRadiusesUpdated;
end;

procedure FXShapeRoundedSquare.CreatePath;
begin
  inherited;

  const MinRound = Min(DrawRect.Height, DrawRect.Width);

  const RoundTL = EnsureRange(CornerRadius.AbsoluteTopLeft, 1, MinRound);
  const RoundTR = EnsureRange(CornerRadius.AbsoluteTopRight, 1, MinRound);
  const RoundBR = EnsureRange(CornerRadius.AbsoluteBottomRight, 1, MinRound);
  const RoundBL = EnsureRange(CornerRadius.AbsoluteBottomLeft, 1, MinRound);

  FPath.AddArc(Points[0].X, Points[0].Y, RoundTL, RoundTL, 180+Rotation, 90); // topleft
  FPath.AddArc(Points[1].X - RoundTR, Points[1].Y, RoundTR, RoundTR, 270+Rotation, 90); // topright
  FPath.AddArc(Points[2].X - RoundBR, Points[2].Y - RoundBR, RoundBR, RoundBR, 0+Rotation, 90); // bottomright
  FPath.AddArc(Points[3].X, Points[3].Y - RoundBL, RoundBL, RoundBL, 90+Rotation, 90); // bottomleft
  FPath.CloseFigure();
end;

destructor FXShapeRoundedSquare.Destroy;
begin
  FreeAndNil( FCornerRadius );
  inherited;
end;

end.
