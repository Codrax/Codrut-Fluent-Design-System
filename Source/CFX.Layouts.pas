unit CFX.Layouts;

interface

uses
  Winapi.Messages,
  Winapi.Windows,
  System.Classes,
  System.Types,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.ExtCtrls,
  Vcl.Dialogs,
  Vcl.Forms,
  Math,
  CFX.Colors,
  CFX.Utilities,
  CFX.ThemeManager,
  CFX.Graphics,
  CFX.Constants,
  CFX.Scrollbar,
  CFX.Animation.Component,
  SysUtils,
  CFX.Classes,
  CFX.Types,
  CFX.VarHelpers,
  CFX.Linker,
  CFX.Controls;

type
  FXCustomLayout = class(FXWindowsControl, FXControl)
  private
    var DrawRect: TRect;
    FDrawColors: FXCompleteColorSet;
    FCustomColors: FXColorSets;
    FKeepSolid: boolean;

    FBackground: FXBackgroundColor;

    // Internal
    procedure SetBackground(const Value: FXBackgroundColor);
    procedure SetKeepSolid(const Value: boolean);

    // Getters

    // Setters

  protected
    procedure PaintBuffer; override;

    //  Internal
    procedure UpdateColors; override;
    procedure UpdateRects; override;

    // State
    procedure InteractionStateChanged(AState: FXControlState); override;

  published
    // Custom Colors
    property CustomColors: FXColorSets read FCustomColors write FCustomColors stored true;

    property BackgroundColor: FXBackgroundColor read FBackground write SetBackground default FXBackgroundColor.Background;
    property KeepSolid: boolean read FKeepSolid write SetKeepSolid default false;

    // Props
    property OnMouseWheel;
    property OnMouseWheelUp;
    property OnMouseWheelDown;

    // Default props
    property Align;
    property Transparent default false;
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

    // Interface
    function IsContainer: Boolean; override;
    function Background: TColor; override;
  end;

  FXLayout = class(FXCustomLayout)
  published
    property AutoSize;
  end;

  FXScrollViewScrollbar = class(FXScrollbar)
  published
    property Height stored false;
    property Width stored false;

  public
    constructor Create(aOwner: TComponent); override;

    procedure CalcAutoRange;
  end;

  FXScrollLayout = class(FXCustomLayout)
  private
    FVertScroll,
    FHorzScroll: FXScrollViewScrollbar;

    FShowScrollbars: boolean;
    FHandleScrolling: boolean;

    FExtendX, FExtendY: integer;
    FAnimX, FAnimY: FXIntAnim;

    FEnableVertical,
    FEnableHorizontal: boolean;

    FKeepScrollAlign: boolean;
    FScrollAnimation: boolean;

    LastScroll: TPoint;

    procedure UpdateRange;

    procedure CalculateRange; // update scroll bar values
    procedure UpdateScrollbars; // update actual position & size of scroll bars!

    procedure ScrollByEx(DeltaX, DeltaY: Integer);

    function ContentRect: TRect;

    // Scroll notifiers
    procedure ScrollChanged(Sender: TObject); // user
    procedure ScrollChangedValue(Sender: TObject); // any

    // Getters
    function GetPositionX: integer;
    function GetPositionY: integer;
    function GetRangeX: integer;
    function GetRangeY: integer;

    // Setters
    procedure SetExtX(const Value: integer);
    procedure SetExtY(const Value: integer);
    procedure SetShowScrollbars(const Value: boolean);
    procedure SetEnableHorizontal(const Value: boolean);
    procedure SetEnableVertical(const Value: boolean);
    procedure SetPositionX(const Value: integer);
    procedure SetPositionY(const Value: integer);

  protected
    procedure Sized; override;

    // Animation
    procedure AnimationStep(Sender: TObject; Step, TotalSteps: integer);

    // Done
    procedure ComponentCreated; override;

    // System events
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;

    procedure AdjustClientRect(var Rect: TRect); override;
    procedure Resize; override;
    procedure AlignControls(AControl: TControl; var ARect: TRect); override;

    // Inner client
    function GetClientRect: TRect; override;

  published
    // Props
    property ShowScrollbars: boolean read FShowScrollbars write SetShowScrollbars default true;
    property ScrollExtendX: integer read FExtendX write SetExtX default 100;
    property ScrollExtendY: integer read FExtendY write SetExtY default 100;
    property HandleScrolling: boolean read FHandleScrolling write FHandleScrolling default true;
    property EnableHorizontal: boolean read FEnableHorizontal write SetEnableHorizontal default true;
    property EnableVertical: boolean read FEnableVertical write SetEnableVertical default true;
    property KeepScrollAlignment: boolean read FKeepScrollAlign write FKeepScrollAlign;
    property ScrollAnimation: boolean read FScrollAnimation write FScrollAnimation default true;

    property PositionX: integer read GetPositionX write SetPositionX;
    property PositionY: integer read GetPositionY write SetPositionY;
    property RangeX: integer read GetRangeX;
    property RangeY: integer read GetRangeY;

    property OnMouseWheel;
    property OnMouseWheelUp;
    property OnMouseWheelDown;

  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

function FXCustomLayout.Background: TColor;
begin
  Result := FDrawColors.BackGround;
end;

constructor FXCustomLayout.Create(aOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csAcceptsControls];
  AlignWithMargins := true;
  Transparent := false;
  FKeepSolid := false;

  // Custom Color
  FCustomColors := FXColorSets.Create(Self);
  FDrawColors := FXCompleteColorSet.Create;

  FBackground := FXBackgroundColor.Background;

  // Sizing
  Height := 200;
  Width := 250;
end;

destructor FXCustomLayout.Destroy;
begin
  FreeAndNil( FCustomColors );
  FreeAndNil( FDrawColors );
  inherited;
end;

procedure FXCustomLayout.InteractionStateChanged(AState: FXControlState);
begin
  inherited;
  Redraw;
end;

function FXCustomLayout.IsContainer: Boolean;
begin
  Result := true;
end;

procedure FXCustomLayout.PaintBuffer;
begin
  // Background
  case FBackground of
    FXBackgroundColor.Background: Color := FDrawColors.BackGround;
    FXBackgroundColor.Content: Color := FDrawColors.BackGroundInterior;
  end;
  PaintBackground(FKeepSolid);

  // Draw
  if FKeepSolid then
    with Buffer do begin
      Brush.Color := Color;
      FillRect(ClipRect);
    end;

  // Inherit
  inherited;
end;

procedure FXCustomLayout.UpdateColors;
begin
  // Access theme manager
  FDrawColors.Assign( ThemeManager.SystemColor );
  if not Enabled then begin
    FDrawColors.Foreground := $808080;
  end
  else
    if FCustomColors.Enabled then
      // Custom Colors
      FDrawColors.LoadFrom(FCustomColors, ThemeManager.DarkTheme);
end;

procedure FXCustomLayout.UpdateRects;
begin
  // Rect
  DrawRect := GetClientRect;
end;

procedure FXCustomLayout.SetBackground(const Value: FXBackgroundColor);
begin
  if FBackground = Value then
    Exit;

  FBackground := Value;
  Redraw;
end;

procedure FXCustomLayout.SetKeepSolid(const Value: boolean);
begin
  if FKeepSolid = Value then
    Exit;

  FKeepSolid := Value;
  Redraw;
end;

{ FXScrollLayout }

procedure FXScrollLayout.AdjustClientRect(var Rect: TRect);
begin
  // Update scrollbars
  UpdateRange;

  // Result Rect
  Rect := Bounds(-FHorzScroll.Position, -FVertScroll.Position,
    Max(FHorzScroll.Max, ClientWidth), Max(ClientHeight,
    FVertScroll.Max));

  // Remove scrollbars from client
  if FVertScroll.Visible or (FKeepScrollAlign and FEnableVertical) then
    Rect.Width := Rect.Width - FVertScroll.Width;

  if FHorzScroll.Visible or (FKeepScrollAlign and FEnableHorizontal) then
    Rect.Height := Rect.Height - FHorzScroll.Height;
end;

procedure FXScrollLayout.AlignControls(AControl: TControl; var ARect: TRect);
begin
  UpdateRange;
  inherited;
end;

procedure FXScrollLayout.AnimationStep(Sender: TObject; Step,
  TotalSteps: integer);
begin
  if Sender = FAnimX then begin
    // Horizontal
    FHorzScroll.Position := FXIntAnim(Sender).CurrentValue;
  end else begin
    // Vertical
    FVertScroll.Position := FXIntAnim(Sender).CurrentValue;
  end;

  // Process messages in order to detect scroll speed update / stop
  Application.ProcessMessages;
end;

constructor FXScrollLayout.Create(aOwner: TComponent);
begin
  inherited;
  FShowScrollbars := true;
  FEnableHorizontal := true;
  FEnableVertical := true;
  FKeepScrollAlign := false;
  FScrollAnimation := true;

  FExtendX := 100;
  FExtendY := 100;

  FHandleScrolling := true;

  // Create scrollbars
  FVertScroll := FXScrollViewScrollbar.Create(Self);
  FHorzScroll := FXScrollViewScrollbar.Create(Self);

  with FVertScroll do
    begin
      Parent := Self;
      Width := DEFAULT_SCROLLBAR_SIZE;

      Orientation := FXOrientation.Vertical;
      Tag := 0;
      OnChange := ScrollChanged;
      OnChangeValue := ScrollChangedValue;

      Max := 0;
    end;
  with FHorzScroll do
    begin
      Parent := Self;
      Height := DEFAULT_SCROLLBAR_SIZE;

      Orientation := FXOrientation.Horizontal;

      Tag := 1;
      OnChange := ScrollChanged;
      OnChangeValue := ScrollChangedValue;

      Max := 0;
    end;

  // Anim
  FAnimX := FXIntAnim.Create(nil);
  with FAnimX do begin
    Kind := FXAnimationKind.ReverseExpo;
    Duration := SCROLL_DURATION;

    LatencyAdjustments := true;
    LatencyCanSkipSteps := true;

    OnStep := AnimationStep;
  end;

  FAnimY := FXIntAnim.Create(nil);
  with FAnimY do begin
    Kind := FXAnimationKind.ReverseExpo;
    Duration := SCROLL_DURATION;

    LatencyAdjustments := true;
    LatencyCanSkipSteps := true;

    OnStep := AnimationStep;
  end;

  // Update Position
  LastScroll := TPoint.Zero;
end;

destructor FXScrollLayout.Destroy;
begin
  FAnimX.Stop;
  FAnimY.Stop;
  FreeAndNil( FAnimX );
  FreeAndNil( FAnimY );

  FreeAndNil( FVertScroll );
  FreeAndNil( FHorzScroll );

  inherited;
end;

function FXScrollLayout.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result := false;

  if FHandleScrolling and not (ssCtrl in Shift) then begin
    const ScrollAmount = GetScrollAmount(WheelDelta, ClientRect.Height);
    const Animate = FScrollAnimation and not IsDesigning;

    Result := true; // handled

    if (ssShift in Shift) or (FVertScroll.Max = 0) then begin
      if not Animate then begin
        FHorzScroll.Position := FHorzScroll.Position+ScrollAmount;
        Exit;
      end;

      // Horizontal
      FAnimX.StartValue := FHorzScroll.Position;

      if FAnimX.Running then
        FAnimX.EndValue := FAnimX.EndValue + ScrollAmount
      else
        FAnimX.EndValue := FHorzScroll.Position+ScrollAmount;
      FAnimX.EndValue := EnsureRange(FAnimX.EndValue, FHorzScroll.Min, FHorzScroll.Max);

      FAnimX.Stop;

      if FAnimX.StartValue <> FAnimX.EndValue then
        FAnimX.Start;
    end else begin
      if not Animate then begin
        FVertScroll.Position := FVertScroll.Position+ScrollAmount;
        Exit;
      end;

      // Vertical
      FAnimY.StartValue := FVertScroll.Position;

      if FAnimY.Running then
        FAnimY.EndValue := FAnimY.EndValue + ScrollAmount
      else
        FAnimY.EndValue := FVertScroll.Position+ScrollAmount;
      FAnimY.EndValue := EnsureRange(FAnimY.EndValue, FVertScroll.Min, FVertScroll.Max);

      FAnimY.Stop;

      if FAnimY.StartValue <> FAnimY.EndValue then
        FAnimY.Start;
    end;
  end;

  if not Result then
    Result := inherited;
end;

function FXScrollLayout.GetClientRect: TRect;
begin
  Result := inherited;

  if (FHorzScroll <> nil) and FHorzScroll.Visible then
    Result.Height := Result.Height - FHorzScroll.Height;
  if (FVertScroll <> nil) and FVertScroll.Visible then
    Result.Width := Result.Width - FVertScroll.Width;
end;

function FXScrollLayout.GetPositionX: integer;
begin
  Result := FHorzScroll.Position;
end;

function FXScrollLayout.GetPositionY: integer;
begin
  Result := FVertScroll.Position;
end;

function FXScrollLayout.GetRangeX: integer;
begin
  Result := FHorzScroll.Max;
end;

function FXScrollLayout.GetRangeY: integer;
begin
  Result := FVertScroll.Max;
end;

procedure FXScrollLayout.CalculateRange;
begin
  FVertScroll.CalcAutoRange;
  FHorzScroll.CalcAutoRange;
end;

procedure FXScrollLayout.ComponentCreated;
begin
  inherited;
  UpdateRange;
end;

function FXScrollLayout.ContentRect: TRect;
begin
  Result := ClientRect;

  // Remove scrollbars from client
  if FVertScroll.Visible then
    Result.Width := Result.Width - FVertScroll.Width;

  if FHorzScroll.Visible then
    Result.Height := Result.Height - FHorzScroll.Height;
end;

procedure FXScrollLayout.Resize;
begin
  inherited;
  UpdateRange;
end;

procedure FXScrollLayout.ScrollByEx(DeltaX, DeltaY: Integer);
var
  IsVisible: Boolean;
  I: Integer;
  Control: TControl;
  UpdateRect: TRect;
begin
  // Get visible
  IsVisible := (WindowHandle <> 0) and IsWindowVisible(WindowHandle);

  // Rect
  UpdateRect := ContentRect;
  UpdateRect.Width := Width;

  // Scroll
  if IsVisible then 
    ScrollWindow(WindowHandle, DeltaX, DeltaY, nil, nil);

  // Not visible
  for I := 0 to ControlCount - 1 do
  begin
    Control := Controls[I];

    // Non TWinControl
    if not (Control is TWinControl) {or (TWinControl(Control).WindowHandle = 0)} then
    begin
      Control.Left := Control.Left + DeltaX;
      Control.Top := Control.Top + DeltaY;
    end else
      // TWinControl
      if not IsVisible and not (Control is FXScrollViewScrollbar) then
        with TWinControl(Control) do
          if (Align <> alClient) then
            SetBounds(Left+DeltaX, Top+DeltaY, Width, Height);
  end;

  // Align
  Realign;

  // Draw background
  Redraw;

  // Designing
  if IsDesigning then
    Invalidate;
end;

procedure FXScrollLayout.ScrollChanged(Sender: TObject);
begin
  // STOP scroll animations
  FAnimX.Stop;
  FAnimY.Stop;
end;

procedure FXScrollLayout.ScrollChangedValue(Sender: TObject);
var
  Delta: TPoint;
  NewScroll: TPoint;
begin
  if IsReading then
    Exit;

  NewScroll := Point(FHorzScroll.Position, FVertScroll.Position);

  // Scroll
  Delta := LastScroll.Subtract(NewScroll);
  ScrollByEx(Delta.X, Delta.Y);

  // Set
  LastScroll := NewScroll;
end;

procedure FXScrollLayout.SetEnableHorizontal(const Value: boolean);
begin
  if FEnableHorizontal = Value then
    Exit;

  FEnableHorizontal := Value;
  UpdateScrollbars;
  UpdateRects;
end;

procedure FXScrollLayout.SetEnableVertical(const Value: boolean);
begin
  if FEnableVertical = Value then
    Exit;

  FEnableVertical := Value;
  UpdateScrollbars;
  UpdateRects;
end;

procedure FXScrollLayout.SetExtX(const Value: integer);
begin
  if FExtendX = Value then
    Exit;

  FExtendX := Value;
  UpdateRects;
end;

procedure FXScrollLayout.SetExtY(const Value: integer);
begin
  if FExtendY = Value then
    Exit;

  FExtendY := Value;
  UpdateRects;
end;

procedure FXScrollLayout.SetPositionX(const Value: integer);
begin
  if IsReading and (FHorzScroll.Max < Value) then begin
    FHorzScroll.Max := Value;
    LastScroll.X := Value;
  end;
  FHorzScroll.Position := Value;
end;

procedure FXScrollLayout.SetPositionY(const Value: integer);
begin
  if IsReading and (FVertScroll.Max < Value) then begin
    FVertScroll.Max := Value;
    LastScroll.Y := Value;
  end;
  FVertScroll.Position := Value;
end;

procedure FXScrollLayout.SetShowScrollbars(const Value: boolean);
begin
  if FShowScrollbars = Value then
    Exit;

  FShowScrollbars := Value;
  UpdateScrollbars;
  UpdateRects;
end;

procedure FXScrollLayout.Sized;
begin
  inherited;
  UpdateRange;
end;

procedure FXScrollLayout.UpdateRange;
begin
  if not CanUpdate then
    Exit;

  CalculateRange;
  UpdateScrollbars;
end;

procedure FXScrollLayout.UpdateScrollbars;
var
  V: integer;
  Vis: boolean;
begin
  // Update Scrollbars
  with FVertScroll do
    begin
      if Top <> 0 then
        Top := 0;
      V := Self.Width - FVertScroll.Width;
      if Left <> V then
        Left := V;

      // Data
      Vis := FShowScrollbars and FEnableVertical and (Max > 0);

      // Size
      V := Self.Height;
      if not Vis then
        V := 0;
      if Height <> V then
        Height := V;

      // Visible
      if Visible <> Vis then
        Visible := Vis;
    end;

  with FHorzScroll do
    begin
      V := Self.Height - FHorzScroll.Height;
      if Top <> V then
        Top := V;
      if Left <> 0 then
        Left := 0;

      // Data
      Vis := FShowScrollbars and FEnableVertical and (Max > 0);

      // Size
      if FVertScroll.Visible then
        V := Parent.Width - FVertScroll.Width
      else
        V := Parent.Width;
      if not Vis then
        V := 0;
      if Width <> V then
        Width := V;

      // Visible
      Vis := FShowScrollbars and FEnableHorizontal and (Max > 0);
      if Visible <> Vis then
        Visible := Vis;
    end;
end;

{ FXScrollViewScrollbar }

procedure FXScrollViewScrollbar.CalcAutoRange;
var
  FControl: FXScrollLayout;
  I: Integer;
  NewRange, AlignMargin, ControlSize, ZoneSize: Integer;

procedure ProcessHorz(Control: TControl);
  begin
    if Control.Visible then
      case Control.Align of
        alLeft, alNone:
          if (Control.Align = alLeft) or (Control.Anchors * [akLeft, akRight] = [akLeft]) then
            NewRange := Math.Max(NewRange, Position + Control.Left + Control.Width);
        alRight: Inc(AlignMargin, Control.Width);
      end;
  end;

procedure ProcessVert(Control: TControl);
  begin
    if Control.Visible then
      case Control.Align of
        alTop, alNone:
          if (Control.Align = alTop) or (Control.Anchors * [akTop, akBottom] = [akTop]) then
            NewRange := Math.Max(NewRange, Position + Control.Top + Control.Height);
        alBottom: Inc(AlignMargin, Control.Height);
      end;
  end;

begin
  if Parent is FXScrollLayout then
    begin
      // Control
      FControl := FXScrollLayout(Parent);

      // Size
      if Orientation = FXOrientation.Vertical then
        ControlSize := FControl.Height - Width
      else
        ControlSize := FControl.Width - Height;

      // Range
      NewRange := 0;
      AlignMargin := 0;
      for I := 0 to FControl.ControlCount - 1 do
        if not (FControl.Controls[I] is FXScrollViewScrollbar) then
          if Orientation = FXOrientation.Horizontal then
            ProcessHorz(FControl.Controls[I])
          else
            ProcessVert(FControl.Controls[I]);

      // Calc Range
      ZoneSize := NewRange;
      NewRange := NewRange + AlignMargin - ControlSize;

      // Extra Range, cancel if controls fit
      if (Position = 0) and (ControlSize >= ZoneSize) then
        NewRange := 0;

      // Larger than position
      if NewRange >= Position then
        NewRange := Math.Max(NewRange, 0);

      // Extend range
      if NewRange > 0 then
        case Orientation of
          FXOrientation.Horizontal: Inc(NewRange, FControl.ScrollExtendX);
          FXOrientation.Vertical: Inc(NewRange, FControl.ScrollExtendY);
        end;

      // Set range
      Max := NewRange;

      // Visible
      if Orientation = FXOrientation.Vertical then
        Visible := (Self.Max > 0) and FControl.EnableVertical and FControl.ShowScrollbars
      else
        Visible := (Self.Max > 0) and FControl.EnableHorizontal and FControl.ShowScrollbars;
    end
end;

constructor FXScrollViewScrollbar.Create(aOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csCaptureMouse, csDesignInteractive];
end;

end.
