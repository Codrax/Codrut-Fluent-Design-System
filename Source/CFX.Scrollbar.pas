unit CFX.Scrollbar;

interface
uses
  Classes,
  Messages,
  Windows,
  Vcl.Controls,
  Vcl.Graphics,
  Types,
  Math,
  UITypes,
  CFX.Colors,
  CFX.ThemeManager,
  CFX.Graphics,
  CFX.Constants,
  SysUtils,
  Vcl.ExtCtrls,
  CFX.Classes,
  CFX.Hint,
  CFX.Animation.Component,
  CFX.Controls,
  CFX.Linker,
  CFX.VarHelpers,
  CFX.Types;

type
  FXScrollbar = class;

  FXScrollbar = class(FXWindowsControl, FXControl)
  private
    var DrawRect, SliderRect, Button1, Button2: TRect;
    FOnChange: TNotifyEvent;
    FOnChangeValue: TNotifyEvent;
    FOrientation: FXOrientation;
    FRoundness: integer;
    FPosition, FMin, FMax: int64;
    FSmallChange,
    FScrollBarHeight,
    FCustomScrollBarHeight: integer;
    FPageSize: integer;
    FSliderSpacing: integer;
    FEnableButtons: boolean;
    FRepeater: TTimer;
    FAutoMinimise: boolean;
    FSliderSize: integer;
    FMinimised: boolean;
    FPreferLeftSide: boolean;

    FAnimation: boolean;
    FAnim: FXIntAnim;

    FPressInitiated: boolean;

    FCustomColors: FXCompleteColorSets;
    FDrawColors: FXCompleteColorSet;

    Contains1, Contains2: boolean;

    // Internal
    procedure InteractSetPosition(Value: integer);

    // Timer
    procedure RepeaterExecute(Sender: TObject);

    // Animation
    procedure SetMinimisedState(Value: boolean);
    function GetSliderSize(AMinimised: boolean): integer;

    // Messages
    procedure SetSmallChange(const Value: integer);

    // Buttons
    function GetButtonsSize: integer;

    // Store
    function StoreMinimised: Boolean;

    // Set
    procedure SetOrientation(const Value: FXOrientation);
    procedure SetMax(const Value: int64);
    procedure SetMin(const Value: int64);
    procedure SetPosition(const Value: int64);
    procedure SetEnableButtons(const Value: boolean);
    procedure SetCustomScrollbarSize(const Value: integer);
    procedure SetPageSize(const Value: integer);

  protected
    procedure PaintBuffer; override;

    // Internal
    procedure UpdateColors; override;
    procedure UpdateRects; override;

    procedure ComponentCreated; override;

    // Scale
    procedure ScaleChanged(Scaler: single); override;

    procedure AnimationStep(Sender: TObject; Step, TotalSteps: integer);

    // State
    procedure InteractionStateChanged(AState: FXControlState); override;

    // Inherited Mouse Detection
    procedure MouseDown(Button : TMouseButton; Shift: TShiftState; X, Y : integer); override;
    procedure MouseUp(Button : TMouseButton; Shift: TShiftState; X, Y : integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

    procedure KeyPress(var Key: Char); override;

  published
    property CustomColors: FXCompleteColorSets read FCustomColors write FCustomColors stored true;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChangeValue: TNotifyEvent read FOnChangeValue write FOnChangeValue;
    property Orientation: FXOrientation read FOrientation write SetOrientation default FXOrientation.Vertical;
    property Position: int64 read FPosition write SetPosition;
    property SmallChange: integer read FSmallChange write SetSmallChange default 1;
    property Min: int64 read FMin write SetMin default 0;
    property Max: int64 read FMax write SetMax default 100;
    property Animation: boolean read FAnimation write FAnimation;
    property Minimised: boolean read FMinimised write SetMinimisedState stored StoreMinimised default true;

    property CustomScrollbarSize: integer read FCustomScrollBarHeight write SetCustomScrollbarSize;
    property PageSize: integer read FPageSize write SetPageSize default 1;
    property EnableButtons: boolean read FEnableButtons write SetEnableButtons default true;
    property AutoMinimise: boolean read FAutoMinimise write FAutoMinimise default true;
    property PreferLeftSide: boolean read FPreferLeftSide write FPreferLeftSide default false;

    property Align;
    property Transparent;
    property Opacity;
    property PaddingFill;
    property Constraints;
    property Anchors;
    property Hint;
    property ShowHint;
    property ParentShowHint;
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

    function GetPercentage: real;
    function GetPercentageCustom(Value: int64): real;

    // Interface
    function Background: TColor; override;
  end;

implementation

procedure FXScrollbar.InteractionStateChanged(AState: FXControlState);
begin
  if AutoMinimise then
    SetMinimisedState( AState = FXControlState.None )
  else
    // Redraw
    UpdateRects;

  inherited; // draw
end;

procedure FXScrollbar.InteractSetPosition(Value: integer);
begin
  Position := Value;
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure FXScrollbar.KeyPress(var Key: Char);
begin
  inherited;
  if (key = '-') or (key = '+') then
    begin
      if Key = '-' then
        InteractSetPosition( Position - FSmallChange )
      else
        InteractSetPosition( Position + FSmallChange );
    end;
end;

procedure FXScrollbar.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: integer);
var
  P: TPoint;
begin
  inherited;
  if Button <> TMouseButton.mbLeft then
    Exit;

  // Move Detection
  MouseMove([], X, Y);

  // Down
  P := Point(X, Y);
  if EnableButtons then
    if not (Contains1 or Contains2) then
      FPressInitiated := true;

  // Contains
  if Contains1 or Contains2 then
    begin
      RepeaterExecute(nil);

      FRepeater.Interval := 500;
      FRepeater.Enabled := true;
    end;
end;

procedure FXScrollbar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewPosition: int64;
  P: TPoint;
begin
  inherited;
  // Point
  P := Point(X, Y);

  // Buttons
  Contains1 := false;
  Contains2 := false;

  if EnableButtons and not FPressInitiated then
    begin
      Contains1 := Button1.Contains(P);
      Contains2 := Button2.Contains(P);
    end;

  // Change Position
  if FPressInitiated then
    begin
      if FMax = FMin then
        NewPosition := FMin
      else
        begin
          if Orientation = FXOrientation.Horizontal then
            NewPosition := round((X-DrawRect.Left - FScrollBarHeight / 2) / (DrawRect.Width - FScrollBarHeight) * (FMax - FMin))
          else
            NewPosition := round((Y-DrawRect.Top - FScrollBarHeight / 2) / (DrawRect.Height - FScrollBarHeight) * (FMax - FMin));
        end;

      InteractSetPosition( NewPosition + FMin );
    end;
end;

procedure FXScrollbar.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: integer);
begin
  inherited;
  FPressInitiated := false;
  if EnableButtons then
    Redraw;

  FRepeater.Enabled := false;
end;

procedure FXScrollbar.UpdateColors;
begin
  // Access theme manager
  FDrawColors.Assign( ThemeManager.SystemColor );
  if FCustomColors.Enabled then
    FDrawColors.LoadFrom(FCustomColors, ThemeManager.DarkTheme)
    else
  begin
    if ThemeManager.DarkTheme then
      FDrawColors.ForeGround := ChangeColorLight(FDrawColors.BackGroundInterior, 75)
    else
      FDrawColors.ForeGround := ChangeColorLight(FDrawColors.BackGroundInterior, -75);
  end;

  // Disabled
  if not Enabled then
    begin
      FDrawColors.Accent := ChangeColorLight( FDrawColors.BackGround, 20 );
      FDrawColors.BackGround := ChangeColorLight( FDrawColors.BackGround, 20 );
    end;
end;

procedure FXScrollbar.UpdateRects;
var
  MaxPossibleLength,
  MaxPosition: integer;
  ButtonsSize: integer;
  SliderSize: integer;
procedure CalculateScrollbarHeight;
begin
  // Scrollbar Size
  if CustomScrollbarSize > 0 then
    FScrollBarHeight := CustomScrollbarSize
  else
    begin
      if PageSize > 1 then
        FScrollBarHeight := Math.Max( trunc(GetPercentageCustom(PageSize) * MaxPossibleLength), SCROLLBAR_MIN_SIZE)
      else
        FScrollBarHeight := SCROLLBAR_DEFAULT_SIZE;
    end;
end;
begin
  // Rect
  DrawRect := GetClientRect;

  // Build Rects
  if FOrientation = FXOrientation.Horizontal then
    begin
      // Slider
      SliderRect := DrawRect;
      SliderSize := DrawRect.Height;

      // Button Rect
      Button1 := Rect(DrawRect.Left, DrawRect.Top,
        DrawRect.Left+SliderSize, DrawRect.Top+SliderSize);
      Button2 := Button1;
      Button2.Offset(DrawRect.Width - SliderSize, 0);

      ButtonsSize := GetButtonsSize;

      // Value
      SliderRect.Inflate(-FSliderSpacing, -FSliderSpacing);
      MaxPossibleLength := SliderRect.Width - FSliderSpacing * 2 - ButtonsSize;
      CalculateScrollbarHeight;
      MaxPosition := MaxPossibleLength - FScrollBarHeight;

      SliderRect.Width := FScrollBarHeight;
      SliderRect.Offset( ButtonsSize div 2 + trunc(GetPercentage * MaxPosition),
        0
        );

      // Round
      FRoundness := DrawRect.Height div 2;
    end
  else
    begin
      // Slider
      SliderRect := DrawRect;
      SliderSize := DrawRect.Width;

      // Button Rect
      Button1 := Rect(DrawRect.Left, DrawRect.Top,
        DrawRect.Left+SliderSize, DrawRect.Top+SliderSize);
      Button2 := Button1;
      Button2.Offset(0, DrawRect.Height - SliderSize);

      ButtonsSize := GetButtonsSize;

      // Value
      SliderRect.Inflate(-FSliderSpacing, -FSliderSpacing);
      MaxPossibleLength := SliderRect.Height - FSliderSpacing * 2 - ButtonsSize;
      CalculateScrollbarHeight;
      MaxPosition := MaxPossibleLength - FScrollBarHeight;

      SliderRect.Height := FScrollBarHeight;
      SliderRect.Offset( 0,
        ButtonsSize div 2 + trunc(GetPercentage * MaxPosition)
        );

      // Round
      FRoundness := DrawRect.Width div 2;
    end;

  // Data
  FSliderSize := GetSliderSize(FMinimised);
end;

procedure FXScrollbar.ComponentCreated;
begin
  inherited;
end;

constructor FXScrollbar.Create(aOwner: TComponent);
begin
  inherited;
  FOrientation := FXOrientation.Vertical;
  FSmallChange := 1;
  FScrollBarHeight := 90;
  FEnableButtons := true;
  FPageSize := 1;
  FMinimised := true;
  FCustomScrollBarHeight := 0;

  FPosition := 0;
  FMin := 0;
  FMax := 100;

  FSliderSpacing := 3;
  FAutoMinimise := true;
  FAnimation := true;

  AutoFocusLine := true;

  // Repeater
  FRepeater := TTimer.Create(nil);
  with FRepeater do
    begin
      Interval := 500;
      Enabled := false;
      OnTimer := RepeaterExecute;
    end;

  // Animation
  FAnim := FXIntAnim.Create(Self);
  with FAnim do
    begin
      LatencyAdjustments := true;

      StartValue := 0;
      EndValue := 255;

      Kind := FXAnimationKind.ReverseExpo;

      OnStep := AnimationStep;
    end;

  // Custom Color
  FCustomColors := FXCompleteColorSets.Create(Self);
  FDrawColors := FXCompleteColorSet.Create;

  // Sizing
  Height := 225;
  Width := DEFAULT_SCROLLBAR_SIZE;
end;

destructor FXScrollbar.Destroy;
begin
  FreeAndNil( FDrawColors );
  FreeAndNil( FCustomColors );
  FreeAndNil( FRepeater );
  FreeAndNil( FAnim );
  inherited;
end;

function FXScrollbar.GetButtonsSize: integer;
begin
  if not FEnableButtons then
    Result := 0
  else
    if Orientation = FXOrientation.Horizontal then
      Result := Button1.Width + Button2.Width
    else
      Result := Button1.Height + Button2.Height;
end;

function FXScrollbar.GetPercentage: real;
begin
  Result := GetPercentageCustom(FPosition);
end;

function FXScrollbar.GetPercentageCustom(Value: int64): real;
var
  Value1, Value2: int64;
begin
  Result := 0;
  Value1 := Value - FMin;
  Value2 := FMax - FMin;
  if Value1 < 0 then
    begin
      Inc(Value2, abs(Value1));
      Inc(Value1, abs(Value1));
    end;
  if Value2 <> 0 then
    Result := Value1 / Value2;
end;

function FXScrollbar.GetSliderSize(AMinimised: boolean): integer;
begin
  if Orientation = FXOrientation.Horizontal then
    begin
      Result := SliderRect.Height div 2;
    end
  else
    begin
      Result := SliderRect.Width div 2;
    end;
end;

procedure FXScrollbar.PaintBuffer;
var
  Points: TArray<TPoint>;
  Spacing1, Spacing2, Shrinked: integer;
  ARect: TRect;
  AColor: TColor;
begin
  // Background
  Color := FDrawColors.BackGround;
  PaintBackground;

  // Draw slider
  Canvas.Brush.Style := bsClear;
  Canvas.Font.Assign(Font);

  with Buffer do
    begin
      Pen.Style := psClear;
      Brush.Style := bsSolid;

      // Slider Background
      AColor := ColorBlend(FDrawColors.BackGround, FDrawColors.BackGroundInterior, FAnim.CurrentValue);
      GDIRoundRect(MakeRoundRect(DrawRect, FRoundness*2), GetRGB(AColor).MakeGDIBrush, nil);

      // Full
      ARect := SliderRect;
      Brush.Color := FDrawColors.ForeGround;

      if FMinimised then
        begin
          if Orientation = FXOrientation.Horizontal then
            begin
              ARect.Height := FSliderSize;

              if not FPreferLeftSide then
                ARect.Offset(0, ARect.Height);
            end
          else
            begin
              ARect.Width := FSliderSize;

              if not FPreferLeftSide then
                ARect.Offset(ARect.Width, 0);
            end;
        end;

      GDIRoundRect(MakeRoundRect(ARect, FRoundness), GetRGB(Brush.Color).MakeGDIBrush, nil);
    end;

  // Draw Buttons
  if EnableButtons and not FMinimised then
    with Buffer do
      begin
        Brush.Color := FDrawColors.ForeGround;
        Pen.Style := psClear;

        // Shrink
        Spacing1 := round(FSliderSpacing/2);
        Spacing2 := Spacing1;
        Shrinked := FSliderSpacing;

        // Press
        if InteractionState = FXControlState.Press then
          begin
            if Contains1 then
              Spacing1 := Shrinked;

            if Contains2 then
              Spacing2 := Shrinked;
          end;

        SetLength(Points, 3);
        if Orientation = FXOrientation.Horizontal then
          begin
            // Button 1
            ARect := Button1;
            ARect.Left := ARect.Left + Shrinked;
            ARect.Inflate(-Spacing1, -Spacing1);
            Points[0] := Point(ARect.Left, ARect.CenterPoint.Y);
            Points[1] := Point(ARect.Right, ARect.Top);
            Points[2] := Point(ARect.Right, ARect.Bottom);

            Polygon(Points);

            // Button 2
            ARect := Button2;
            ARect.Right := ARect.Right - Shrinked;
            ARect.Inflate(-Spacing2, -Spacing2);
            Points[0] := Point(ARect.Right, ARect.CenterPoint.Y);
            Points[1] := Point(ARect.Left, ARect.Top);
            Points[2] := Point(ARect.Left, ARect.Bottom);

            Polygon(Points);
          end
        else
          begin
            // Button 1
            ARect := Button1;
            ARect.Top := ARect.Top + Shrinked;
            ARect.Inflate(-Spacing1, -Spacing1);
            Points[0] := Point(ARect.CenterPoint.X, ARect.Top);
            Points[1] := Point(ARect.Right, ARect.Bottom);
            Points[2] := Point(ARect.Left, ARect.Bottom);

            Polygon(Points);

            // Button 2
            ARect := Button2;
            ARect.Bottom := ARect.Bottom - Shrinked;
            ARect.Inflate(-Spacing2, -Spacing2);
            Points[0] := Point(ARect.CenterPoint.X, ARect.Bottom);
            Points[1] := Point(ARect.Right, ARect.Top);
            Points[2] := Point(ARect.Left, ARect.Top);

            Polygon(Points);
          end;
      end;

  inherited;
end;

procedure FXScrollbar.RepeaterExecute(Sender: TObject);
begin
  FRepeater.Interval := 50;

  if Contains1 then
    InteractSetPosition( Position - SmallChange );
  if Contains2 then
    InteractSetPosition( Position + SmallChange );
end;

procedure FXScrollbar.SetEnableButtons(const Value: boolean);
begin
  if FEnableButtons = Value then
    Exit;

  FEnableButtons := Value;
  StandardUpdateLayout;
end;

procedure FXScrollbar.SetMax(const Value: int64);
begin
  if FMax = Value then
    Exit;

  FMax := Value;

  if not IsReading then
    begin
      if FMin > FMax then
        FMax := FMax;

      if FPosition > FMax then
        FPosition := FMax;
    end;

  // Draw
  StandardUpdateLayout;
end;

procedure FXScrollbar.SetMin(const Value: int64);
begin
  if FMin = Value then
    Exit;

  FMin := Value;

  if not IsReading then
    begin
      if FPosition < FMin then
        FPosition := FMin;

      if FMax < FMin then
        FMax := FMin;
    end;

  StandardUpdateLayout;
end;

procedure FXScrollbar.SetMinimisedState(Value: boolean);
begin
  // Animation
  if FMinimised = Value then
    Exit;

  // Set
  FMinimised := Value;

  if not IsDesigning then begin
    FAnim.Stop;

    if FAnimation then
      FAnim.Duration := 1
    else
      FAnim.Duration := 0;

    FAnim.Inverse := FMinimised;
    FAnim.Start;
  end;

  // Update
  StandardUpdateLayout;
end;

procedure FXScrollbar.SetOrientation(const Value: FXOrientation);
begin
  if (FOrientation = Value) then
    Exit;

  FOrientation := Value;

  if not IsReading then
    SetBounds(Left, Top, Height, Width)
  else
    StandardUpdateLayout;
end;

procedure FXScrollbar.SetPageSize(const Value: integer);
begin
  if (FPageSize = Value) or (Value <= 0) then
    Exit;

  FPageSize := Value;

  StandardUpdateLayout;
end;

procedure FXScrollbar.SetPosition(const Value: int64);
begin
  if FPosition = Value then
    Exit;

  FPosition := Value;

  if not IsReading then
    begin
      if FPosition < FMin then
        FPosition := FMin;

      if FPosition > FMax then
        FPosition := FMax;

      if Assigned(OnChangeValue) then
        OnChangeValue(Self);
    end;

  StandardUpdateLayout;
end;

procedure FXScrollbar.ScaleChanged(Scaler: single);
begin
  CustomScrollbarSize := round(CustomScrollbarSize * Scaler);
  inherited;
end;

procedure FXScrollbar.SetCustomScrollbarSize(const Value: integer);
begin
  if (FCustomScrollBarHeight = Value) or (Value >= Height) then
    Exit;

  FCustomScrollBarHeight := Value;

  StandardUpdateLayout;
end;

procedure FXScrollbar.SetSmallChange(const Value: integer);
begin
  if FSmallChange <> Value then
    if Value > 0 then
      FSmallChange := Value;
end;

function FXScrollbar.StoreMinimised: Boolean;
begin
  Result := FAutoMinimise = false;
end;

procedure FXScrollbar.AnimationStep(Sender: TObject; Step, TotalSteps: integer);
begin
  Redraw;
end;

function FXScrollbar.Background: TColor;
begin
  Result := FDrawColors.Background;
end;

end.
