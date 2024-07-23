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
  CFX.Colors,
  CFX.ThemeManager,
  CFX.Graphics,
  CFX.UIConsts,
  SysUtils,
  CFX.Classes,
  CFX.Types,
  CFX.VarHelpers,
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

    procedure Updated;

    // Setters
    procedure SetColorBrush(const Value: FXColor);
    procedure SetColorPen(const Value: FXColor);
    procedure SetDrawBrush(const Value: boolean);
    procedure SetDrawPen(const Value: boolean);
    procedure SetPenSize(const Value: single);

  published
    property DrawPen: boolean read FDrawPen write SetDrawPen;
    property DrawBrush: boolean read FDrawBrush write SetDrawBrush;

    property ColorPen: FXColor read FColorPen write SetColorPen;
    property ColorBrush: FXColor read FColorBrush write SetColorBrush;

    property PenSize: single read FPenSize write SetPenSize;

  public
    property Brush: TGDIBrush read FBrush;
    property Pen: TGDIPen read FPen;

    // Constructors
    constructor Create(Control: FXWindowsControl);
    destructor Destroy; override;
  end;

  FXShape = class(FXWindowsControl, FXControl)
  private
    FDrawColors: FXCompleteColorSet;
    FCustomColors: FXColorSets;

    FRotation: FXAngle;
    FProportional: boolean;

    FSettings: FXShapeDrawingSettings;

    // GDI
    FPath: TGPGraphicsPath;

    // Getters

    // Setters
    procedure SetProportional(const Value: boolean);
    procedure SetRotation(const Value: FXAngle);

  protected
    var DrawRect: TRect;
    var Points: TArray<TPoint>;

    procedure PaintBuffer; override;
    procedure Resize; override;

    //  Internal
    procedure UpdateColors; virtual;
    procedure UpdateRects; virtual;

    // Shape building
    procedure BuildPoints; virtual;
    procedure RotatePoints; virtual;
    procedure CreatePath; virtual;

    // Scaler
    procedure ScaleChanged(Scaler: single); override;

    // State
    procedure InteractionStateChanged(AState: FXControlState); override;

  published
    // Custom Colors
    property CustomColors: FXColorSets read FCustomColors write FCustomColors stored true;

    property Drawing: FXShapeDrawingSettings read FSettings write FSettings;

    // Props
    property Proportional: boolean read FProportional write SetProportional default false;
    property Rotation: FXAngle read FRotation write SetRotation;

    // Default props
    property Align;
    property PaddingFill;
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
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property OnMouseUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;

  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    // Override
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

    // Interface
    function IsContainer: Boolean;
    procedure UpdateTheme(const UpdateChildren: Boolean);

    function Background: TColor;
  end;

  FXShapeSquare = class(FXShape)
  protected
    // Shape building
    procedure BuildPoints; override;
    procedure CreatePath; override;
  end;

  FXShapeCircle = class(FXShape)
  protected

  end;

  FXShapeTriangle = class(FXShape)
  protected

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

  // Prop
  FProportional := false;
  FRotation := 0;

  // Sizing
  Height := 100;
  Width := 100;

  // GDI Obj
  FPath := TGPGraphicsPath.Create;

  // Update
  UpdateRects;
  UpdateColors;
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

procedure FXShape.InteractionStateChanged(AState: FXControlState);
begin
  inherited;
  Invalidate;
end;

function FXShape.IsContainer: Boolean;
begin
  Result := false;
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

procedure FXShape.Resize;
begin
  inherited;
  UpdateRects;
end;

procedure FXShape.RotatePoints;
begin
  for var I := 0 to High(Points) do
    Points[I] :=
      RotatePointAroundPoint(Points[I], DrawRect.CenterPoint, FRotation);
end;

procedure FXShape.UpdateTheme(const UpdateChildren: Boolean);
begin
  UpdateColors;
  UpdateRects;
  Invalidate;
end;

procedure FXShape.UpdateColors;
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

procedure FXShape.UpdateRects;
begin
  // Rect
  DrawRect := GetClientRect;

  // Build points
  BuildPoints;

  // Rotation
  RotatePoints;

  // Path
  CreatePath;
end;

procedure FXShape.ScaleChanged(Scaler: single);
begin
  inherited;
  // update scale
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

procedure FXShape.SetProportional(const Value: boolean);
begin
  if FProportional = Value then
    Exit;

  if Value and (Height <> Width) then
    inherited Height := Width;

  FProportional := Value;
  UpdateRects;
  Invalidate;
end;

procedure FXShape.SetRotation(const Value: FXAngle);
begin
  if FRotation = Value then
    Exit;

  FRotation := Value;
  UpdateRects;
  Invalidate;
end;

{ FXShapeDrawingSettings }

constructor FXShapeDrawingSettings.Create(Control: FXWindowsControl);
begin
  FControl := Control;

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
  FControl.Invalidate;
end;

{ FXShapeSquare }

procedure FXShapeSquare.BuildPoints;
begin
  inherited;

  Points := [
    DrawRect.TopLeft,
    Point(DrawRect.Top, DrawRect.Right),
    DrawRect.BottomRight,
    Point(DrawRect.Bottom, DrawRect.Left)
  ];
end;

procedure FXShapeSquare.CreatePath;
begin
  inherited;

  for var I := 0 to High(Points)-1 do
    FPath.AddLine( Points[I].X, Points[I].Y, Points[I+1].X, Points[I+1].Y );
end;

end.
