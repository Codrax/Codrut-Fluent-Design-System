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
  SysUtils,
  CFX.Classes,
  CFX.ComponentClasses,
  CFX.Accessibility,
  CFX.Animation.Component,
  CFX.Types,
  CFX.VarHelpers,
  CFX.Linker,
  CFX.Controls;

type
  FXCustomLayout = class(FXContainerWindowsControl)
  private
    var DrawRect: TRect;
    FDrawColors: FXCompleteColorSet;
    FCustomColors: FXColorSets;
    FKeepSolid: boolean;

    FBackground: FXBackgroundColor;

    // Internal
    procedure SetBackground(const Value: FXBackgroundColor);
    procedure SetKeepSolid(const Value: boolean);

  protected
    procedure PaintBuffer; override;

    // Accesibility
    function AccessibilityGetControlType: Integer; override;
    function AccessibilityGetControlTypeName: string; override;

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
    property OnResize;

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

    FKeepScrollClientWhenBarHidden: boolean;
    FScrollAnimation: boolean;

    FResetScrollValueToTopOn: boolean;

    (* The offset that is currently PHYSICALLY applied to the child controls.
       This must never be changed without moving the children by the same
       amount - see ApplyScrollPosition. *)
    LastScroll: TPoint;

    // Re-entrancy guards
    FUpdatingRange: boolean;
    FScrolling: boolean;

    procedure UpdateRange;

    procedure CalculateRange; // update scroll bar ranges
    procedure UpdateScrollbarVisibility; // visibility only (used while measuring)
    procedure UpdateScrollbars; // update position / size / visibility of scroll bars
    procedure ClampScrollValues; // keep Value inside [0..Max]
    procedure ApplyScrollPosition; // move children so they match the scroll values
    procedure StopAnimations;

    procedure ScrollByEx(DeltaX, DeltaY: Integer);

    function ContentRect: TRect;
    function ScrollbarThickness: integer;

    // Scroll notifiers
    procedure ScrollChanged(Sender: TObject); // user
    procedure ScrollChangedValue(Sender: TObject); // any

    // Getters
    function GetValueX: integer;
    function GetValueY: integer;
    function GetRangeX: integer;
    function GetRangeY: integer;

    // Setters
    procedure SetExtX(const Value: integer);
    procedure SetExtY(const Value: integer);
    procedure SetShowScrollbars(const Value: boolean);
    procedure SetEnableHorizontal(const Value: boolean);
    procedure SetEnableVertical(const Value: boolean);
    procedure SetValueX(const Value: integer);
    procedure SetValueY(const Value: integer);

  protected
    procedure Sized; override;

    // Animation
    procedure AnimationStep(Sender: TObject; Step, TotalSteps: integer);

    // Done
    procedure ComponentCreated; override;

    // Loaded
    procedure Loaded; override;

    // System events
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;

    procedure AdjustClientRect(var Rect: TRect); override;
    procedure Resize; override;
    procedure AlignControls(AControl: TControl; var ARect: TRect); override;

    // Inner client
    function GetClientRect: TRect; override;

    // Helpers
    class function IsFixedControl(Control: TControl): boolean;
    class function IsScrollbarControl(Control: TControl): boolean;

  published
    // Props
    property ShowScrollbars: boolean read FShowScrollbars write SetShowScrollbars default true;
    property ScrollExtendX: integer read FExtendX write SetExtX default 100;
    property ScrollExtendY: integer read FExtendY write SetExtY default 100;
    property HandleScrolling: boolean read FHandleScrolling write FHandleScrolling default true;
    property EnableHorizontal: boolean read FEnableHorizontal write SetEnableHorizontal default true;
    property EnableVertical: boolean read FEnableVertical write SetEnableVertical default true;
    (* Keeps the same client rect for aligning even if the scrollbar gets hidden *)
    property KeepScrollClientWhenBarHidden: boolean read FKeepScrollClientWhenBarHidden write FKeepScrollClientWhenBarHidden default false;
    (* Scroll smoothly *)
    property ScrollAnimation: boolean read FScrollAnimation write FScrollAnimation default true;
    (* When the scrollbox is created, reset the value to 0,0 (usefull when editing stuff in design mode) *)
    property ResetScrollValueToTopOn: boolean read FResetScrollValueToTopOn write FResetScrollValueToTopOn default true;

    property ValueX: integer read GetValueX write SetValueX;
    property ValueY: integer read GetValueY write SetValueY;
    property RangeX: integer read GetRangeX;
    property RangeY: integer read GetRangeY;

    property OnMouseWheel;
    property OnMouseWheelUp;
    property OnMouseWheelDown;

  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    (* Recalculate ranges and re-sync the children. Safe to call at any time. *)
    procedure UpdateScrollState;

    (* Scroll so that Control is inside the visible area *)
    procedure ScrollInView(Control: TControl);
  end;

implementation

function FXCustomLayout.AccessibilityGetControlType: Integer;
begin
  Result := inherited;
end;

function FXCustomLayout.AccessibilityGetControlTypeName: string;
begin
  Result := 'panel';
end;

function FXCustomLayout.Background: TColor;
begin
  Result := FDrawColors.BackGround;
end;

constructor FXCustomLayout.Create(aOwner: TComponent);
begin
  inherited;
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
  //inherited;
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

class function FXScrollLayout.IsFixedControl(Control: TControl): boolean;
begin
  Result := (Control is FXWindowsControl)
    and (FXControlFlag.Fixed in FXWindowsControl(Control).ControlFlags);
end;

class function FXScrollLayout.IsScrollbarControl(Control: TControl): boolean;
begin
  Result := Control is FXScrollViewScrollbar;
end;

function FXScrollLayout.ScrollbarThickness: integer;
begin
  Result := DEFAULT_SCROLLBAR_SIZE;
end;

procedure FXScrollLayout.AdjustClientRect(var Rect: TRect);
begin
  inherited;

  (* NOTE: this must NOT call UpdateRange. AdjustClientRect is invoked from
     inside the alignment pass, and UpdateRange moves/resizes controls, which
     restarts alignment. The range is refreshed from AlignControls, Resize,
     Sized and ComponentCreated instead. *)

  // Result Rect - the virtual (scrolled) content area
  Rect := Bounds(-FHorzScroll.Value + Padding.Left, -FVertScroll.Value + Padding.Top,
    Max(FHorzScroll.Max + Padding.Left + Padding.Right, ClientWidth - Padding.Left - Padding.Right),
    Max(ClientHeight - Padding.Top - Padding.Bottom, FVertScroll.Max + Padding.Top + Padding.Bottom));

  // Reserve the space of a hidden scrollbar, if requested
  if not FVertScroll.Visible and FKeepScrollClientWhenBarHidden and FEnableVertical then
    Rect.Width := Rect.Width - ScrollbarThickness;

  if not FHorzScroll.Visible and FKeepScrollClientWhenBarHidden and FEnableHorizontal then
    Rect.Height := Rect.Height - ScrollbarThickness;
end;

procedure FXScrollLayout.AlignControls(AControl: TControl; var ARect: TRect);
begin
  inherited;

  // Children moved / were added / removed -> the range may have changed
  UpdateRange;
end;

procedure FXScrollLayout.AnimationStep(Sender: TObject; Step,
  TotalSteps: integer);
begin
  if Sender = FAnimX then
    // Horizontal
    FHorzScroll.Value := EnsureRange(FXIntAnim(Sender).CurrentValue, 0, Max(FHorzScroll.Max, 0))
  else
    // Vertical
    FVertScroll.Value := EnsureRange(FXIntAnim(Sender).CurrentValue, 0, Max(FVertScroll.Max, 0));

  // Process messages in order to detect scroll speed update / stop
  Application.ProcessMessages;
end;

constructor FXScrollLayout.Create(aOwner: TComponent);
begin
  inherited;
  FShowScrollbars := true;
  FEnableHorizontal := true;
  FEnableVertical := true;
  FKeepScrollClientWhenBarHidden := false;
  FScrollAnimation := true;
  FResetScrollValueToTopOn := true;

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
      Visible := false;
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
      Visible := false;
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

  // Update Value
  LastScroll := TPoint.Zero;
end;

destructor FXScrollLayout.Destroy;
begin
  StopAnimations;
  FreeAndNil( FAnimX );
  FreeAndNil( FAnimY );

  FreeAndNil( FVertScroll );
  FreeAndNil( FHorzScroll );

  inherited;
end;

procedure FXScrollLayout.StopAnimations;
begin
  if FAnimX <> nil then
    FAnimX.Stop;
  if FAnimY <> nil then
    FAnimY.Stop;
end;

function FXScrollLayout.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
var
  Horizontal: boolean;
  Bar: FXScrollViewScrollbar;
  Anim: FXIntAnim;
  ScrollAmount, Target: integer;
begin
  Result := false;

  if FHandleScrolling and not (ssCtrl in Shift) then begin
    // Pick the axis
    Horizontal := (ssShift in Shift) or (FVertScroll.Max <= 0) or not FEnableVertical;

    if Horizontal then begin
      Bar := FHorzScroll;
      Anim := FAnimX;
      ScrollAmount := GetScrollAmount(WheelDelta, ClientRect.Width);
    end else begin
      Bar := FVertScroll;
      Anim := FAnimY;
      ScrollAmount := GetScrollAmount(WheelDelta, ClientRect.Height);
    end;

    // Nothing to scroll on this axis -> let the parent handle the wheel
    if (Bar.Max <= 0)
      or (Horizontal and not FEnableHorizontal)
      or (not Horizontal and not FEnableVertical) then begin
      Result := inherited;
      Exit;
    end;

    Result := true; // handled

    if not (FScrollAnimation and not IsDesigning) then begin
      Bar.Value := EnsureRange(Bar.Value + ScrollAmount, 0, Bar.Max);
      Exit;
    end;

    // Accumulate onto the running animation, otherwise start from where we are
    if Anim.Running then
      Target := Anim.EndValue + ScrollAmount
    else
      Target := Bar.Value + ScrollAmount;

    Target := EnsureRange(Target, Max(Bar.Min, 0), Max(Bar.Max, 0));

    Anim.Stop;
    Anim.StartValue := Bar.Value;
    Anim.EndValue := Target;

    if Anim.StartValue <> Anim.EndValue then
      Anim.Start;
  end;

  if not Result then
    Result := inherited;
end;

function FXScrollLayout.GetClientRect: TRect;
begin
  Result := inherited;

  if (FHorzScroll <> nil) and FHorzScroll.Visible then
    Result.Height := Result.Height - ScrollbarThickness;
  if (FVertScroll <> nil) and FVertScroll.Visible then
    Result.Width := Result.Width - ScrollbarThickness;
end;

function FXScrollLayout.GetValueX: integer;
begin
  Result := FHorzScroll.Value;
end;

function FXScrollLayout.GetValueY: integer;
begin
  Result := FVertScroll.Value;
end;

function FXScrollLayout.GetRangeX: integer;
begin
  Result := FHorzScroll.Max;
end;

function FXScrollLayout.GetRangeY: integer;
begin
  Result := FVertScroll.Max;
end;

procedure FXScrollLayout.Loaded;
begin
  inherited;

  (* While reading, SetValueX/SetValueY also set LastScroll, because the child
     positions that were streamed in already include the stored offset. So
     resetting the value here produces a real delta and the children are moved
     back to the top. *)
  if FResetScrollValueToTopOn then begin
    FVertScroll.Value := 0;
    FHorzScroll.Value := 0;
  end;

  UpdateRange;
end;

procedure FXScrollLayout.CalculateRange;
var
  I: integer;
begin
  (* Two passes: the vertical range depends on whether the horizontal bar is
     visible and vice versa, so a single pass can settle on the wrong answer
     when a bar appears or disappears. *)
  for I := 1 to 2 do begin
    FVertScroll.CalcAutoRange;
    FHorzScroll.CalcAutoRange;

    UpdateScrollbarVisibility;
  end;
end;

procedure FXScrollLayout.ComponentCreated;
begin
  inherited;
  UpdateRange;
end;

function FXScrollLayout.ContentRect: TRect;
begin
  // ClientRect already excludes the visible scrollbars
  Result := ClientRect;
end;

procedure FXScrollLayout.Resize;
begin
  inherited;
  UpdateRange;
end;

procedure FXScrollLayout.Sized;
begin
  inherited;
  UpdateRange;
end;

procedure FXScrollLayout.UpdateScrollState;
begin
  UpdateRange;
end;

procedure FXScrollLayout.ScrollInView(Control: TControl);
var
  L, T: integer;
begin
  if (Control = nil) or (Control.Parent <> Self) then
    Exit;
  if IsFixedControl(Control) or IsScrollbarControl(Control) then
    Exit;

  UpdateRange;

  // Vertical
  if FEnableVertical and (FVertScroll.Max > 0) then begin
    T := FVertScroll.Value;
    if Control.Top < 0 then
      T := T + Control.Top
    else
      if Control.Top + Control.Height > ClientHeight then
        T := T + (Control.Top + Control.Height - ClientHeight);

    SetValueY( EnsureRange(T, 0, FVertScroll.Max) );
  end;

  // Horizontal
  if FEnableHorizontal and (FHorzScroll.Max > 0) then begin
    L := FHorzScroll.Value;
    if Control.Left < 0 then
      L := L + Control.Left
    else
      if Control.Left + Control.Width > ClientWidth then
        L := L + (Control.Left + Control.Width - ClientWidth);

    SetValueX( EnsureRange(L, 0, FHorzScroll.Max) );
  end;
end;

procedure FXScrollLayout.ScrollByEx(DeltaX, DeltaY: Integer);
var
  I: Integer;
  Control: TControl;
begin
  if (DeltaX = 0) and (DeltaY = 0) then
    Exit;

  DisableAlign;
  try
    for I := 0 to ControlCount - 1 do begin
      Control := Controls[I];

      // The scroll bars themselves never scroll
      if IsScrollbarControl(Control) then
        Continue;

      // Controls pinned to the viewport never scroll
      if IsFixedControl(Control) then
        Continue;

      (* Aligned controls are positioned by the alignment pass using
         AdjustClientRect, which already contains the -Value offset. Moving
         them here would only be undone by the Realign below. *)
      if Control.Align <> alNone then
        Continue;

      Control.SetBounds(Control.Left + DeltaX, Control.Top + DeltaY,
        Control.Width, Control.Height);
    end;
  finally
    EnableAlign;
  end;

  // Re-place the aligned children into the new virtual client rect
  Realign;

  // Draw background
  Redraw;

  // Designing
  if IsDesigning then
    Invalidate;
end;

procedure FXScrollLayout.ScrollChanged(Sender: TObject);
begin
  // STOP scroll animations (the user took over)
  StopAnimations;
end;

procedure FXScrollLayout.ScrollChangedValue(Sender: TObject);
begin
  ApplyScrollPosition;
end;

procedure FXScrollLayout.ApplyScrollPosition;
var
  NewScroll, Delta: TPoint;
begin
  if (FHorzScroll = nil) or (FVertScroll = nil) then
    Exit;
  if IsReading or (csDestroying in ComponentState) then
    Exit;
  if FScrolling then
    Exit;

  NewScroll := Point(FHorzScroll.Value, FVertScroll.Value);

  Delta := Point(LastScroll.X - NewScroll.X, LastScroll.Y - NewScroll.Y);
  if (Delta.X = 0) and (Delta.Y = 0) then
    Exit;

  (* Record the new offset BEFORE moving, so that anything re-entering through
     the alignment pass sees a consistent state and cannot apply the same
     delta twice. *)
  LastScroll := NewScroll;

  FScrolling := true;
  try
    ScrollByEx(Delta.X, Delta.Y);
  finally
    FScrolling := false;
  end;
end;

procedure FXScrollLayout.ClampScrollValues;
var
  NewValue: integer;
begin
  // Horizontal
  NewValue := EnsureRange(FHorzScroll.Value, 0, Max(FHorzScroll.Max, 0));
  if FHorzScroll.Value <> NewValue then begin
    // A running animation would fight the clamp
    if FAnimX.Running then
      FAnimX.Stop;
    FHorzScroll.Value := NewValue;
  end else
    if FAnimX.Running and not InRange(FAnimX.EndValue, 0, Max(FHorzScroll.Max, 0)) then
      FAnimX.Stop;

  // Vertical
  NewValue := EnsureRange(FVertScroll.Value, 0, Max(FVertScroll.Max, 0));
  if FVertScroll.Value <> NewValue then begin
    if FAnimY.Running then
      FAnimY.Stop;
    FVertScroll.Value := NewValue;
  end else
    if FAnimY.Running and not InRange(FAnimY.EndValue, 0, Max(FVertScroll.Max, 0)) then
      FAnimY.Stop;
end;

procedure FXScrollLayout.SetEnableHorizontal(const Value: boolean);
begin
  if FEnableHorizontal = Value then
    Exit;

  FEnableHorizontal := Value;
  UpdateRange;
  UpdateRects;
end;

procedure FXScrollLayout.SetEnableVertical(const Value: boolean);
begin
  if FEnableVertical = Value then
    Exit;

  FEnableVertical := Value;
  UpdateRange;
  UpdateRects;
end;

procedure FXScrollLayout.SetExtX(const Value: integer);
begin
  if FExtendX = Value then
    Exit;

  FExtendX := Value;
  UpdateRange;
  UpdateRects;
end;

procedure FXScrollLayout.SetExtY(const Value: integer);
begin
  if FExtendY = Value then
    Exit;

  FExtendY := Value;
  UpdateRange;
  UpdateRects;
end;

procedure FXScrollLayout.SetValueX(const Value: integer);
begin
  (* While streaming, the child positions already contain the stored offset,
     so LastScroll must match the stored value - otherwise the first scroll
     would jump by the full amount. *)
  if IsReading then begin
    if FHorzScroll.Max < Value then
      FHorzScroll.Max := Value;
    LastScroll.X := Value;
    FHorzScroll.Value := Value;
    Exit;
  end;

  FHorzScroll.Value := EnsureRange(Value, 0, Max(FHorzScroll.Max, 0));
end;

procedure FXScrollLayout.SetValueY(const Value: integer);
begin
  if IsReading then begin
    if FVertScroll.Max < Value then
      FVertScroll.Max := Value;
    LastScroll.Y := Value;
    FVertScroll.Value := Value;
    Exit;
  end;

  FVertScroll.Value := EnsureRange(Value, 0, Max(FVertScroll.Max, 0));
end;

procedure FXScrollLayout.SetShowScrollbars(const Value: boolean);
begin
  if FShowScrollbars = Value then
    Exit;

  FShowScrollbars := Value;
  UpdateRange;
  UpdateRects;
end;

procedure FXScrollLayout.UpdateRange;
begin
  if (FHorzScroll = nil) or (FVertScroll = nil) then
    Exit;
  if csDestroying in ComponentState then
    Exit;
  if FUpdatingRange then
    Exit;
  if not CanUpdate then
    Exit;

  FUpdatingRange := true;
  try
    // New ranges for the current size / content
    CalculateRange;

    (* The range may have shrunk (the layout got bigger). Pull the values back
       into the valid interval. This does NOT touch LastScroll. *)
    ClampScrollValues;

    // Position / size / show the bars
    UpdateScrollbars;
  finally
    FUpdatingRange := false;
  end;

  (* THE FIX: bring the children back in line with whatever the values ended up
     being. Previously LastScroll was simply overwritten here, which threw the
     clamped-away offset on the floor and left the children stuck in their
     scrolled positions. *)
  ApplyScrollPosition;
end;

procedure FXScrollLayout.UpdateScrollbarVisibility;
var
  Vis: boolean;
begin
  Vis := FShowScrollbars and FEnableVertical and (FVertScroll.Max > 0);
  if FVertScroll.Visible <> Vis then
    FVertScroll.Visible := Vis;

  Vis := FShowScrollbars and FEnableHorizontal and (FHorzScroll.Max > 0);
  if FHorzScroll.Visible <> Vis then
    FHorzScroll.Visible := Vis;
end;

procedure FXScrollLayout.UpdateScrollbars;
var
  V: integer;
begin
  // Visibility first, the geometry depends on it
  UpdateScrollbarVisibility;

  (* Both bars keep their natural thickness at all times. Zeroing the size of a
     hidden bar made CalcAutoRange subtract 0 for the bar thickness on the next
     pass, which produced a wrong range whenever a bar toggled. *)

  // Vertical bar: right edge, full height minus the horizontal bar
  with FVertScroll do begin
    if Width <> ScrollbarThickness then
      Width := ScrollbarThickness;

    if Top <> 0 then
      Top := 0;

    V := Self.Width - ScrollbarThickness;
    if Left <> V then
      Left := V;

    V := Self.Height;
    if FHorzScroll.Visible then
      Dec(V, ScrollbarThickness);
    V := Math.Max(V, 0);
    if Height <> V then
      Height := V;
  end;

  // Horizontal bar: bottom edge, full width minus the vertical bar
  with FHorzScroll do begin
    if Height <> ScrollbarThickness then
      Height := ScrollbarThickness;

    V := Self.Height - ScrollbarThickness;
    if Top <> V then
      Top := V;

    if Left <> 0 then
      Left := 0;

    // NOTE: this used Parent.Width, i.e. the width of the layout's PARENT
    V := Self.Width;
    if FVertScroll.Visible then
      Dec(V, ScrollbarThickness);
    V := Math.Max(V, 0);
    if Width <> V then
      Width := V;
  end;
end;

{ FXScrollViewScrollbar }

procedure FXScrollViewScrollbar.CalcAutoRange;
var
  FControl: FXScrollLayout;
  I: Integer;
  NewRange, AlignMargin, ControlSize, ContentSize, Offset: Integer;

  procedure ProcessHorz(Control: TControl);
  begin
    if not Control.Visible then
      Exit;

    case Control.Align of
      alLeft, alNone:
        if (Control.Align = alLeft) or (Control.Anchors * [akLeft, akRight] = [akLeft]) then
          NewRange := Math.Max(NewRange, Offset + Control.Left + Control.Width);
      alRight: Inc(AlignMargin, Control.Width);
    end;
  end;

  procedure ProcessVert(Control: TControl);
  begin
    if not Control.Visible then
      Exit;

    case Control.Align of
      alTop, alNone:
        if (Control.Align = alTop) or (Control.Anchors * [akTop, akBottom] = [akTop]) then
          NewRange := Math.Max(NewRange, Offset + Control.Top + Control.Height);
      alBottom: Inc(AlignMargin, Control.Height);
    end;
  end;

var
  Control: TControl;
begin
  if not (Parent is FXScrollLayout) then
    Exit;

  FControl := FXScrollLayout(Parent);

  (* Measure against the offset that is actually applied to the children
     (LastScroll), not against Value. During clamping or animation the two
     differ, and using Value there produced a range computed from a mismatched
     pair of numbers. *)
  if Orientation = FXOrientation.Vertical then begin
    ControlSize := FControl.Height;

    // Horizontal scrollbar consumes vertical space
    if FControl.EnableHorizontal and FControl.FHorzScroll.Visible then
      Dec(ControlSize, FControl.ScrollbarThickness);

    Offset := FControl.LastScroll.Y;
  end else begin
    ControlSize := FControl.Width;

    // Vertical scrollbar consumes horizontal space
    if FControl.EnableVertical and FControl.FVertScroll.Visible then
      Dec(ControlSize, FControl.ScrollbarThickness);

    Offset := FControl.LastScroll.X;
  end;
  ControlSize := Math.Max(ControlSize, 0);

  // Measure the content
  NewRange := 0;
  AlignMargin := 0;
  for I := 0 to FControl.ControlCount - 1 do begin
    Control := FControl.Controls[I];

    // The bars are not content
    if FXScrollLayout.IsScrollbarControl(Control) then
      Continue;

    (* Fixed controls do not move with the content, so they must not extend
       the scrollable range either. *)
    if FXScrollLayout.IsFixedControl(Control) then
      Continue;

    if Orientation = FXOrientation.Horizontal then
      ProcessHorz(Control)
    else
      ProcessVert(Control);
  end;

  // Calc Range
  ContentSize := NewRange + AlignMargin;
  NewRange := ContentSize - ControlSize;

  // Content fits
  if ControlSize >= ContentSize then
    NewRange := 0
  else
    NewRange := Math.Max(NewRange, 0);

  // Extend range
  if NewRange > 0 then
    case Orientation of
      FXOrientation.Horizontal: Inc(NewRange, FControl.ScrollExtendX);
      FXOrientation.Vertical: Inc(NewRange, FControl.ScrollExtendY);
    end;

  // Set range (the layout clamps Value afterwards, in ClampScrollValues)
  if Max <> NewRange then
    Max := NewRange;

  (* Visibility is owned by FXScrollLayout.UpdateScrollbarVisibility so that
     both bars are decided together. *)
end;

constructor FXScrollViewScrollbar.Create(aOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csCaptureMouse, csDesignInteractive];
  TabStop := false;
end;

end.

