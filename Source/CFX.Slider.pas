unit CFX.Slider;

interface
uses
  Classes,
  Messages,
  Windows,
  Vcl.Controls,
  Vcl.Graphics,
  Types,
  CFX.Colors,
  CFX.ThemeManager,
  CFX.Graphics,
  CFX.Constants,
  SysUtils,
  Vcl.ExtCtrls,
  CFX.Classes,
  CFX.Hint,
  CFX.Controls,
  CFX.Linker,
  CFX.VarHelpers,
  CFX.Messages,
  CFX.Types;

type
  FXSlider = class;

  FXSliderOnHint = procedure(Sender: FXSlider; var Hint: string) of object;

  FXSlider = class(FXWindowsControl)
  private
    var DrawRect, IconRect, SliderRect, SliderFull: TRect;
    FHint: FXHintPopup;
    FOnChange,
    FOnChangeValue: TNotifyEvent;
    FAutomaticMouseCursor: boolean;
    FOrientation: FXOrientation;
    FSliderHeight: integer;
    FIconSize: integer;
    FRoundness: integer;
    FPosition, FMin, FMax: int64;
    FTotalTicks: integer;
    FSmallChange: integer;
    FEnablePositionHint: boolean;
    FOnHint: FXSliderOnHint;
    FAlwaysSnap: boolean;
    FDrawSliderFilling: boolean;

    FPositionDraw: integer;

    FFillTick: TTimer;
    FCenterFill: integer;
    FDestinedFill: integer;

    FCustomColors: FXCompleteColorSets;
    FDrawColors,
    FIconColors: FXCompleteColorSet;

    // Timer Proc
    procedure FillTickChange(Sender: TObject);

    // Hint
    procedure ShowPositionHint;

    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;

    // Fill Animation
    procedure AnimateToFill;

    // Getters
    function GetSliderBegin: integer;
    function GetSliderSize: integer;
    procedure UpdateSliderPosition;

    // Setters
    procedure SetOrientation(const Value: FXOrientation);
    procedure SetMax(const Value: int64);
    procedure SetMin(const Value: int64);
    procedure SetPosition(const Value: int64);
    procedure SetPositionEx(const Value: int64; ARedraw: boolean; UserExecuted: boolean = true);
    procedure SetSliderHeight(const Value: integer);
    procedure SetIconSize(const Value: integer);
    procedure SetSmallChange(const Value: integer);
    procedure SetTicks(const Value: integer);
    procedure SetDrawSliderFilling(const Value: boolean);

  protected
    procedure PaintBuffer; override;

    // Update
    procedure UpdateColors; override;
    procedure UpdateRects; override;

    // Scale
    procedure ScaleChanged(Scaler: single); override;

    // State
    procedure InteractionStateChanged(AState: FXControlState); override;

    // Inherited Mouse Detection
    procedure MouseDown(Button : TMouseButton; Shift: TShiftState; X, Y : integer); override;
    procedure MouseUp(Button : TMouseButton; Shift: TShiftState; X, Y : integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

    procedure KeyPress(var Key: Char); override;
    procedure HandleKeyDown(var CanHandle: boolean; Key: integer; ShiftState: TShiftState); override;

  published
    property CustomColors: FXCompleteColorSets read FCustomColors write FCustomColors stored true;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChangeValue: TNotifyEvent read FOnChangeValue write FOnChangeValue;
    property AutomaticCursorPointer: boolean read FAutomaticMouseCursor write FAutomaticMouseCursor default true;
    property Orientation: FXOrientation read FOrientation write SetOrientation default FXOrientation.Horizontal;
    property SliderHeight: integer read FSliderHeight write SetSliderHeight default 6;
    property IconSize: integer read FIconSize write SetIconSize default CHECKBOX_ICON_SIZE;
    property Position: int64 read FPosition write SetPosition;
    property SmallChange: integer read FSmallChange write SetSmallChange default 1;
    property Min: int64 read FMin write SetMin default 0;
    property Max: int64 read FMax write SetMax default 100;
    property TotalTicks: integer read FTotalTicks write SetTicks default 0;
    property AlwaysSnap: boolean read FAlwaysSnap write FAlwaysSnap default false;
    property DrawSliderFilling: boolean read FDrawSliderFilling write SetDrawSliderFilling default true;
    property EnablePositionHint: boolean read FEnablePositionHint write FEnablePositionHint default true;
    property OnHint: FXSliderOnHint read FOnHint write FOnHint;

    //  Modify default props
    property ParentColor;

    property Align;
    property Transparent;
    property Opacity;
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
    property OnMouseMove;

  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    function GetPercentage: real;

    // Interface
    function Background: TColor; override;
  end;

implementation

procedure FXSlider.InteractionStateChanged(AState: FXControlState);
begin
  //UpdateRects;
  AnimateToFill;
  inherited;

  if AState <> FXControlState.Press then
    if FEnablePositionHint then
      FHint.AutoHide := true;
end;

procedure FXSlider.KeyPress(var Key: Char);
begin
  inherited;
  if (key = '-') or (key = '+') or (key = '=') then
    begin
      if Key = '-' then
        SetPositionEx(Position - FSmallChange, true)
      else
        SetPositionEx(Position + FSmallChange, true);

      ShowPositionHint;
      FHint.AutoHide := true;
    end;
end;

procedure FXSlider.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: integer);
begin
  inherited;
  AnimateToFill;

  // Move Detection
  MouseMove([], X, Y);

  // Paint
  Redraw;
end;

procedure FXSlider.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  NewPosition: int64;
  //IsHover: boolean;
begin
  inherited;
  // Mouse Enter
  {if (InteractionState = FXControlState.None) then
    begin
      IsHover := false;
      case Orientation of
        FXOrientation.Horizontal: IsHover := (Y > IconRect.Top) and (Y < IconRect.Bottom);
        FXOrientation.Vertical: IsHover := (X > IconRect.Left) and (X < IconRect.Right);
      end;

      if IsHover then
        SetState( FXControlState.Hover );
      AnimateToFill;
    end;}

  // Cursor
  if Self.FAutomaticMouseCursor then
    if PtInRect(IconRect, Point(X, Y)) then
      Self.Cursor := crHandPoint
    else
      Self.Cursor := crDefault;

  // Change Position
  if InteractionState = FXControlState.Press then
    begin
      if FMax = FMin then
        NewPosition := FMin
      else
        begin
          if Orientation = FXOrientation.Horizontal then
            NewPosition := round((X-GetSliderBegin - FIconSize div 2) / (GetSliderSize - FIconSize) * (FMax - FMin))
          else
            NewPosition := round((Y-GetSliderBegin - FIconSize div 2) / (GetSliderSize - FIconSize) * (FMax - FMin));
        end;
      ShowPositionHint;

      // Snap
      if AlwaysSnap then
        UpdateSliderPosition
      else
        begin
          if Orientation = FXOrientation.Horizontal then
            FPositionDraw := X - FIconSize div 2 - DrawRect.Left
          else
            FPositionDraw := Y - FIconSize div 2- DrawRect.Top;

          if FPositionDraw < 0 then
            FPositionDraw := 0;
          if FPositionDraw > GetSliderSize-FIconSize then
            FPositionDraw := GetSliderSize-FIconSize;
        end;

      // Position
      SetPositionEx(NewPosition + Min, false);

      // Update
      UpdateRects;
      Redraw;
    end;
end;

procedure FXSlider.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: integer);
begin
  inherited;

  // Snap to position
  UpdateSliderPosition;
  UpdateRects;

  // Fill
  AnimateToFill;

  // Hint
  ShowPositionHint;
  if FHint.IsVisible then
    begin
      FHint.AutoHide := true;
      FHint.Duration := 100;
    end;

  // Draw
  Redraw;
end;

procedure FXSlider.UpdateColors;
begin
  if FCustomColors.Enabled then
    begin
      FDrawColors.BackGround := ExtractColor(FCustomColors, FXColorType.Background);
      FDrawColors.BackgroundInterior := ExtractColor(FCustomColors, FXColorType.Content);

      FIconColors.BackGroundInterior := ExtractColor(FCustomColors, FXColorType.Foreground);
      FIconColors.BackGround := ChangeColorLight(FIconColors.BackGroundInterior, -25);

      FDrawColors.Accent := ExtractColor(FCustomColors, FXColorType.Accent);
      FIconColors.Accent := ExtractColor(FCustomColors, FXColorType.Accent);
    end
      else
    begin
      FDrawColors.Assign( ThemeManager.SystemColor );
      FIconColors.Assign( ThemeManager.SystemColor );

      FDrawColors.BackGround := GetParentBackgroundColor(FDrawColors.BackGround);

      if ThemeManager.DarkTheme then
        begin
          FDrawColors.BackgroundInterior := ChangeColorLight(ThemeManager.SystemColor.BackgroundInterior, 150);

          FIconColors.Background := ChangeColorLight(FDrawColors.BackgroundInterior, -150);
          FIconColors.BackgroundInterior := ChangeColorLight(FDrawColors.BackgroundInterior, -100);
        end
      else
        begin
          FDrawColors.BackgroundInterior := ChangeColorLight(ThemeManager.SystemColor.BackgroundInterior, -100);

          FIconColors.Background := ChangeColorLight(FDrawColors.BackGround, -25);
          FIconColors.BackgroundInterior := ChangeColorLight(FDrawColors.BackGround, -10);
        end;
    end;

  // Disabled
  if not Enabled then
    begin
      FDrawColors.Accent := ChangeColorLight( FDrawColors.BackGround, 20 );
      FDrawColors.BackGround := ChangeColorLight( FDrawColors.BackGround, 20 );
    end;
end;

procedure FXSlider.UpdateRects;
begin
  // Rect
  DrawRect := GetClientRect;

  // Build rects
  IconRect := DrawRect;
  IconRect.Height := FIconSize;
  IconRect.Width := FIconSize;

  SliderRect := DrawRect;
  if FOrientation = FXOrientation.Horizontal then
    begin
      // Slider
      SliderRect.Offset(0, (SliderRect.Height - FSliderHeight) div 2);
      SliderRect.Height := FSliderHeight;
    end
  else
    begin
      // Slider
      SliderRect.Offset((SliderRect.Width - FSliderHeight) div 2, 0);
      SliderRect.Width := FSliderHeight;
    end;

  // Round
  FRoundness := FSliderHeight div 2;

  // Position
  UpdateSliderPosition;
end;

procedure FXSlider.UpdateSliderPosition;
var
  DisplayPosition: integer;
begin
  // Draw
  FPositionDraw := trunc( GetPercentage * (GetSliderSize - FIconSize) );

  if FOrientation = FXOrientation.Horizontal then
    begin
      // Icon
      DisplayPosition := FPositionDraw;
      IconRect.Offset(DisplayPosition, (DrawRect.Height - FIconSize) div 2);

      // Split
      SliderFull := SliderRect;

      SliderFull.Width := DisplayPosition + IconSize div 2;
    end
  else
    begin
      // Icon
      DisplayPosition := FPositionDraw;
      IconRect.Offset((DrawRect.Width - FIconSize) div 2, DisplayPosition);

      // Split
      SliderFull := SliderRect;

      SliderFull.Height := DisplayPosition + IconSize div 2;
    end;
end;

procedure FXSlider.CMHintShow(var Message: TCMHintShow);
begin
  ShowPositionHint;
  FHint.AutoHide := true;
end;

constructor FXSlider.Create(aOwner: TComponent);
begin
  inherited;
  FAutomaticMouseCursor := true;
  FOrientation := FXOrientation.Horizontal;
  FSliderHeight := 6;
  FIconSize := CHECKBOX_ICON_SIZE;
  FCenterFill := 50;
  FSmallChange := 1;
  FTotalTicks := 0;
  FEnablePositionHint := true;
  FDrawSliderFilling := true;

  ShowHint := true;

  FPosition := 0;
  FMin := 0;
  FMax := 100;

  TabStop := true;
  AutoFocusLine := true;

  // Fill Timer
  FFillTick := TTimer.Create(nil);
  with FFillTick do
    begin
      Enabled := false;
      Interval := 1;
      OnTimer := FillTickChange;
    end;
  AnimateToFill;

  // Hint Class
  FHint := FXHintPopup.Create;

  // Custom Color
  FCustomColors := FXCompleteColorSets.Create(Self);
  FDrawColors := FXCompleteColorSet.Create;
  FIconColors := FXCompleteColorSet.Create;

  // Sizing
  Height := 40;
  Width := 225;
end;

destructor FXSlider.Destroy;
begin
  FreeAndNil( FFillTick );
  FreeAndNil( FDrawColors );
  FreeAndNil( FCustomColors );
  FreeAndNil( FHint );
  inherited;
end;

procedure FXSlider.FillTickChange(Sender: TObject);
begin
  if FCenterFill < FDestinedFill then
    // Increase
    Inc(FCenterFill, 2)
  else
  if FCenterFill > FDestinedFill then
    // Decrease
    Dec(FCenterFill, 2)
  else begin
    // Disable
    FFillTick.Enabled := false;
    Exit;
  end;

  Redraw;
end;

function FXSlider.GetPercentage: real;
var
  Value1, Value2: int64;
begin
  Result := 0;
  Value1 := FPosition - FMin;
  Value2 := FMax - FMin;
  if Value1 < 0 then
    begin
      Inc(Value2, abs(Value1));
      Inc(Value1, abs(Value1));
    end;
  if Value2 <> 0 then
    Result := Value1 / Value2;
end;

function FXSlider.GetSliderBegin: integer;
begin
  if Orientation = FXOrientation.Horizontal then
    Result := DrawRect.Left
  else
    Result := DrawRect.Top;
end;

function FXSlider.GetSliderSize: integer;
begin
  if Orientation = FXOrientation.Horizontal then
    Result := DrawRect.Width
  else
    Result := DrawRect.Height;
end;

procedure FXSlider.HandleKeyDown(var CanHandle: boolean; Key: integer;ShiftState: TShiftState
  );
var
  C: char;
begin
  inherited;
  if Orientation = FXOrientation.Horizontal then
    case Key of
      VK_LEFT: begin
        C := '-';
        KeyPress(C);
        CanHandle := false;
      end;

      VK_RIGHT: begin
        C := '+';
        KeyPress(C);
        CanHandle := false;
      end;
    end
  else
    case Key of
      VK_UP: begin
        C := '-';
        KeyPress(C);
        CanHandle := false;
      end;

      VK_DOWN: begin
        C := '+';
        KeyPress(C);
        CanHandle := false;
      end;
    end;
end;

procedure FXSlider.PaintBuffer;
var
  ValueSize,
  TickMargin,
  TickHeight,
  TickCenter: integer;
  InnerValue: integer;
  InnerRect: TRect;
  I: integer;
begin
  // Background
  Color := FDrawColors.BackGround;
  PaintBackground;

  // Draw
  Canvas.Brush.Style := bsClear;
  Canvas.Font.Assign(Font);

  with Buffer do
    begin
      Pen.Style := psClear;
      Brush.Style := bsSolid;
      Brush.Color := FDrawColors.BackGroundInterior;

      RoundRect(SliderRect, FRoundness, FRoundness);

      // Full
      if FDrawSliderFilling then begin
        Brush.Color := FDrawColors.Accent;

        RoundRect(SliderFull, FRoundness, FRoundness);
      end;
    end;

  // Ticks
  if FTotalTicks > 0 then
    with Buffer do
      begin
        Pen.Style := psClear;
        Brush.Style := bsSolid;
        Brush.Color := FDrawColors.BackGroundInterior;
        TickMargin := IconRect.Width div 2 + GetSliderBegin; // Padding offset

        if Orientation = FXOrientation.Horizontal then
          begin
            ValueSize := SliderRect.Width - IconRect.Width;
            TickHeight := (IconRect.Height - SliderRect.Height) div 2 - SLIDER_TICK_SPACING;

            for I := 0 to FTotalTicks - 1 do
              begin
                if I = 0 then
                  TickCenter := TickMargin
                else
                  TickCenter := TickMargin + trunc(I  / (FTotalTicks - 1) * ValueSize);

                // Top
                InnerRect := Rect(TickCenter - SLIDER_TICK_SIZE div 2, IconRect.Top,
                  TickCenter + SLIDER_TICK_SIZE div 2, IconRect.Top + TickHeight);

                RoundRect(InnerRect, SLIDER_TICK_ROUND, SLIDER_TICK_ROUND);

                // Bottom
                InnerRect := Rect(TickCenter - SLIDER_TICK_SIZE div 2, IconRect.Bottom - TickHeight,
                  TickCenter + SLIDER_TICK_SIZE div 2, IconRect.Bottom);

                RoundRect(InnerRect, SLIDER_TICK_ROUND, SLIDER_TICK_ROUND);
              end;
          end
        else
          begin
            ValueSize := SliderRect.Height - IconRect.Height;
            TickHeight := (IconRect.Width - SliderRect.Width) div 2 - SLIDER_TICK_SPACING;

            for I := 0 to FTotalTicks - 1 do
              begin
                if I = 0 then
                  TickCenter := TickMargin
                else
                  TickCenter := TickMargin + trunc(I  / (FTotalTicks - 1) * ValueSize);

                // Left
                InnerRect := Rect(IconRect.Left, TickCenter - SLIDER_TICK_SIZE div 2,
                  IconRect.Left + TickHeight, TickCenter + SLIDER_TICK_SIZE div 2);

                RoundRect(InnerRect, SLIDER_TICK_ROUND, SLIDER_TICK_ROUND);

                // Right
                InnerRect := Rect(IconRect.Right, TickCenter - SLIDER_TICK_SIZE div 2,
                  IconRect.Right - TickHeight, TickCenter + SLIDER_TICK_SIZE div 2);

                RoundRect(InnerRect, SLIDER_TICK_ROUND, SLIDER_TICK_ROUND);
              end;
          end;
      end;

  // Draw icon
  with Buffer do
    begin
      GDICircle(IconRect,
        GetRGB(FIconColors.Backgroundinterior).MakeGDIBrush,
        GetRGB(FIconColors.Background).MakeGDIPen(0.2)
        );

      InnerRect := IconRect;
      InnerValue := (FIconSize-trunc((FCenterFill) / 100 * FIconSize)) div 2;
      InnerRect.Inflate(-InnerValue, -Innervalue);
      GDICircle(InnerRect,
        GetRGB(FIconColors.Accent).MakeGDIBrush, nil);
    end;

  inherited;
end;

procedure FXSlider.ScaleChanged(Scaler: single);
begin
  inherited;
  FSliderHeight := round(FSliderHeight * Scaler);
  FIconSize := round(FIconSize * Scaler);
end;

procedure FXSlider.SetDrawSliderFilling(const Value: boolean);
begin
  if FDrawSliderFilling = Value then
    Exit;

  FDrawSliderFilling := Value;

  // Update
  StandardUpdateDraw;
end;

procedure FXSlider.SetIconSize(const Value: integer);
begin
  if FIconSize = Value then
    Exit;

  FIconSize := Value;
  StandardUpdateLayout;
end;

procedure FXSlider.SetMax(const Value: int64);
begin
  if FMax = Value then
    Exit;

  FMax := Value;

  // Constraint
  if not IsReading then
    begin
      if FPosition > FMax then
        FPosition := FMax;

      if FMin > FMax then
        FMin := FMax;
    end;

  UpdateSliderPosition;

  // Draw
  StandardUpdateLayout;
end;

procedure FXSlider.SetMin(const Value: int64);
begin
  if FMin = Value then
    Exit;

  FMin := Value;

  // Constraint
  if not IsReading then
    begin
      if FPosition < FMin then
        FPosition := FMin;

      if FMax < FMin then
        FMax := FMin;
    end;

  UpdateSliderPosition;

  // Draw
  StandardUpdateLayout;
end;

procedure FXSlider.SetOrientation(const Value: FXOrientation);
begin
  if (FOrientation = Value) then
    Exit;

  FOrientation := Value;

  if CanUpdate then
    SetBounds(Left, Top, Height, Width);  // this will also invoke UpdateRects() in Sized();
end;

procedure FXSlider.SetPosition(const Value: int64);
begin
  SetPositionEx(Value, true, false);
end;

procedure FXSlider.SetPositionEx(const Value: int64; ARedraw: boolean;
  UserExecuted: boolean);
begin
  if FPosition = Value then
    Exit;

  FPosition := Value;

  // Constraint
  if not IsReading then begin
    if FPosition < FMin then
      FPosition := FMin;

    if FPosition > FMax then
      FPosition := FMax;

    // User update
    if UserExecuted then
      if Assigned(OnChange) then
        OnChange(Self);

    // User/non-user update
    if Assigned(OnChangeValue) then
      OnChangeValue(Self);
  end;

  // Draw
  if ARedraw then begin
    UpdateSliderPosition;

    StandardUpdateLayout;
  end;
end;

procedure FXSlider.SetSliderHeight(const Value: integer);
begin
  if FSliderHeight = Value then
    Exit;

  FSliderHeight := Value;
  StandardUpdateLayout;
end;

procedure FXSlider.SetSmallChange(const Value: integer);
begin
  if FSmallChange <> Value then
    if Value > 0 then
      FSmallChange := Value;
end;

procedure FXSlider.SetTicks(const Value: integer);
begin
  if FTotalTicks <> Value then
    if Value >= 0 then
      begin
        FTotalTicks := Value;

        Redraw;
      end;
end;

procedure FXSlider.ShowPositionHint;
var
  AText: string;
begin
  if FEnablePositionHint then
    begin
      // Get Value
      AText := FPosition.ToString;

      // On Hint procedure
      if Assigned(FOnHint) then
        FOnHint(Self, AText);

      // Prepare Hint
      FHint.Text := AText;

      FHint.Position := ClientToScreen(Point(IconRect.CenterPoint.X, IconRect.CenterPoint.Y));

      if Orientation = FXOrientation.Horizontal then
        FHint.Position.Offset(0, -FIconSize * 2)
      else
        FHint.Position.Offset(-FIconSize * 2, 0);

      FHint.Font.Size := 11;
      FHint.CenterToPosition := true;
      FHint.Duration := CHECKBOX_HINT_DURATION;
      FHint.AutoHide := false;

      FHint.Show;
    end;
end;

procedure FXSlider.AnimateToFill;
begin
  case InteractionState of
    FXControlState.None: FDestinedFill := 50;
    FXControlState.Hover: FDestinedFill := 65;
    FXControlState.Press: FDestinedFill := 45;
  end;

  FFillTick.Enabled := true;
end;

function FXSlider.Background: TColor;
begin
  Result := FDrawColors.Background;
end;

end.
