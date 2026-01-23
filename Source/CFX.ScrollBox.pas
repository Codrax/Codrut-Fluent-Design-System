
unit CFX.ScrollBox;

{$SCOPEDENUMS ON}

interface

uses
  Classes,
  Winapi.Messages,
  Winapi.Windows,
  Vcl.Controls,
  Vcl.Graphics,
  Types,
  Vcl.ExtCtrls,
  CFX.Colors,
  Vcl.Dialogs,
  CFX.ThemeManager,
  CFX.Graphics,
  CFX.Constants,
  SysUtils,
  CFX.Classes,
  CFX.Types,
  CFX.Linker,
  CFX.Controls,
  CFX.Scrollbar,
  CFX.Math,
  Math,
  Vcl.Forms;

type
  // Cardinal
  TScrollPrefer = (Vertical, Horizontal, None);

  // Scrollbox Scrollbar
  FXScrollBoxScrollBar = class(FXScrollbar)
  protected
    procedure PaintBuffer; override;
  public
    procedure CalcAutoRange;
  end;

  FXScrollBox = class(TScrollBox, IFXComponent, IFXControl)
  private
    FCustomColors: FXCompleteColorSets;
    FDrawColors: FXCompleteColorSet;

    FOnScroll: TNotifyEvent;

    FScrollSpeed: integer;
    FKeepScrollAlign: boolean;

    FBackground: FXBackgroundColor;

    FVertScroll,
    FHorzScroll: FXScrollBoxScrollBar;

    FEnableVertical,
    FEnableHorizontal: boolean;

    FPosX, FPosY: integer;
    FExtendX, FExtendY: integer;

    FAnim: TTimer;
    FAnimX,
    FAnimY: integer;

    FAnimation: boolean;

    // Anim
    procedure AnimationTimer(Sender: TObject);

    // Scroll
    procedure WhenScroll(Sender: TObject);

    procedure CalculateRange;
    procedure UpdateScrollbars;
    procedure ResetScrollBars;
    procedure ScrollByEx(DeltaX, DeltaY: Integer);

    // Set
    procedure SetExtX(const Value: integer);
    procedure SetExtY(const Value: integer);
    procedure SetBackground(const Value: FXBackgroundColor);

    // Utils
    function IsDesigning: boolean;

    // Update
    procedure UpdateColors;

  protected
    // Overrode
    procedure Loaded; override;

    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    procedure AdjustClientRect(var Rect: TRect); override;

    function ContentRect: TRect;

  published
    property CustomColors: FXCompleteColorSets read FCustomColors write FCustomColors stored true;
    property BackgroundColor: FXBackgroundColor read FBackground write SetBackground default FXBackgroundColor.Background;

    property AnimateScroll: boolean read FAnimation write FAnimation;
    property ScrollExtendX: integer read FExtendX write SetExtX default 0;
    property ScrollExtendY: integer read FExtendY write SetExtY default 100;
    property KeepScrollAlignment: boolean read FKeepScrollAlign write FKeepScrollAlign;

    property ScrollbarEnableVertical: boolean read FEnableVertical write FEnableVertical default true;
    property ScrollbarEnableHorizontal: boolean read FEnableHorizontal write FEnableHorizontal default true;

    property ScrollSpeed: integer read FScrollSpeed write FScrollSpeed default 10;

    property ScrollBarVert: FXScrollBoxScrollBar read FVertScroll write FVertScroll;
    property ScrollBarHorz: FXScrollBoxScrollBar read FHorzScroll write FHorzScroll;

    property OnScroll: TNotifyEvent read FOnScroll write FOnScroll;

    // Utils
    procedure DestroyAll;

  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    // Draw
    procedure Redraw;

    // Interface
    function IsContainer: Boolean;
    procedure UpdateTheme(const UpdateChildren: Boolean);

    function Background: TColor;
  end;

implementation

{ FXScrollBox }

procedure FXScrollBox.AdjustClientRect(var Rect: TRect);
begin
  // Update scrollbars
  CalculateRange;
  UpdateScrollbars;

  // Result Rect
  if csDesigning in ComponentState then
    inherited
  else
    begin
      Rect := Bounds(-FHorzScroll.Value, -FVertScroll.Value,
        Max(FHorzScroll.Max, ClientWidth), Max(ClientHeight,
        FVertScroll.Max));

      // Remove scrollbars from client
      if FVertScroll.Visible or (FKeepScrollAlign and FEnableVertical) then
        Rect.Width := Rect.Width - FVertScroll.Width;

      if FHorzScroll.Visible or (FKeepScrollAlign and FEnableHorizontal) then
        Rect.Height := Rect.Height - FHorzScroll.Height;
    end;
end;

procedure FXScrollBox.AnimationTimer(Sender: TObject);
var
  X, Y: integer;
begin
  X := Sign(FAnimX) * SCROLL_SPEED_VALUE;
  Y := Sign(FAnimY) * SCROLL_SPEED_VALUE;

  FAnimX := FAnimX - X;
  FAnimY := FAnimY - Y;

  // Scroll
  FVertScroll.Value := FVertScroll.Value + Y;
  FHorzScroll.Value := FHorzScroll.Value + X;

  // Max
  if (FVertScroll.Value = 0) or (FVertScroll.Value = FVertScroll.Max) then
    FAnimY := 0;

  if (FHorzScroll.Value = 0) or (FHorzScroll.Value = FHorzScroll.Max) then
    FAnimX := 0;

  // Disable
  if EqualApprox(FAnimX, 0, SCROLL_SPEED_VALUE)
    and EqualApprox(FAnimY, 0, SCROLL_SPEED_VALUE) then
    begin
      FAnim.Enabled := false;

      // Add leftovers
      if FAnimY <> 0 then
        FVertScroll.Value := FVertScroll.Value + FAnimY;
      if FAnimX <> 0 then
        FHorzScroll.Value := FHorzScroll.Value + FAnimX;

      // Clear data
      FAnimY := 0;
      FAnimY := 0;
    end;
end;

function FXScrollBox.Background: TColor;
begin
  case FBackground of
    FXBackgroundColor.Background: Result := FDrawColors.BackGround;
    FXBackgroundColor.Content: Result := FDrawColors.BackGroundInterior;
    else
      Result := 0;
  end;
end;

procedure FXScrollBox.CalculateRange;
begin
  FVertScroll.CalcAutoRange;
  FHorzScroll.CalcAutoRange;
end;

function FXScrollBox.ContentRect: TRect;
begin
  Result := ClientRect;

  // Remove scrollbars from client
  if FVertScroll.Visible then
    Result.Width := Result.Width - FVertScroll.Width;

  if FHorzScroll.Visible then
    Result.Height := Result.Height - FHorzScroll.Height;
end;

constructor FXScrollBox.Create(aOwner: TComponent);
begin
  inherited;
  FDrawColors := FXCompleteColorSet.Create(Self);
  FCustomColors := FXCompleteColorSets.Create(Self);

  // Scroll (Only works with legacy scrollbars visible)
  //UseWheelForScrolling := true;

  FScrollSpeed := 25;
  FExtendY := 100;

  FEnableVertical := true;
  FEnableHorizontal := true;

  TScrollBox(Self).VertScrollBar.Visible := IsDesigning;
  TScrollBox(Self).HorzScrollBar.Visible := IsDesigning;

  // Create
  FVertScroll := FXScrollBoxScrollBar.Create(Self);
  FHorzScroll := FXScrollBoxScrollBar.Create(Self);

  // Animate
  FAnim := TTimer.Create(nil);
  with FAnim do
    begin
      Enabled := false;
      Interval := 1;
      OnTimer := AnimationTimer;
    end;

  FAnimation := true;

  // Prepare Scrollbars
  with FVertScroll do
    begin
      Parent := Self;

      Orientation := FXOrientation.Vertical;
      Tag := 0;
      OnChangeValue := WhenScroll;

      Max := 0;
    end;

  with FHorzScroll do
    begin
      Parent := Self;

      Orientation := FXOrientation.Horizontal;

      Tag := 1;
      OnChangeValue := WhenScroll;

      Max := 0;
    end;

  Width := 200;
  Height := 150;

  BorderStyle := bsNone;

  // Theme
  UpdateTheme(false);

  // Update Value
  UpdateScrollbars;
end;

destructor FXScrollBox.Destroy;
begin
  FDrawColors.Free;
  FCustomColors.Free;
  FreeAndNil(FVertScroll);
  FreeAndNil(FHorzScroll);
  inherited;
end;

procedure FXScrollBox.DestroyAll;
var
  I: integer;
  Control: TControl;
begin
  for I := ControlCount-1 downto 0 do
    if not (Controls[I] is FXScrollBoxScrollBar) then
      begin
        Control := Controls[I];
        RemoveControl(Control);
        Control.Free;
      end;
end;

function FXScrollBox.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
var
  Prefer: TScrollPrefer;
begin
  if AutoScroll and not (csDesigning in ComponentState) then
    begin
      // Prefer
      Prefer := TScrollPrefer.None;

      // Shift
      if Shift = [ssShift] then
        Prefer := TScrollPrefer.Horizontal
      else
        if Shift = [] then
          Prefer := TScrollPrefer.Vertical;

      case Prefer of
        TScrollPrefer.Vertical: if not FVertScroll.Visible then
          if FHorzScroll.Visible then
            Prefer := TScrollPrefer.Horizontal
          else
            Prefer := TScrollPrefer.None;

        TScrollPrefer.Horizontal: if not FHorzScroll.Visible then
          if FVertScroll.Visible then
            Prefer := TScrollPrefer.Vertical
          else
            Prefer := TScrollPrefer.None;
      end;

      // Scroll
      if FAnimation then
        begin
          case Prefer of
            TScrollPrefer.Vertical: FAnimY := FAnimY + trunc(WheelDelta / abs(WheelDelta) * -ScrollSpeed);
            TScrollPrefer.Horizontal: FAnimX := FAnimX + trunc(WheelDelta / abs(WheelDelta) * -ScrollSpeed);
          end;

          FAnim.Enabled := true;
        end
        else
          case Prefer of
            TScrollPrefer.Vertical: FVertScroll.Value := trunc(FVertScroll.Value + WheelDelta / abs(WheelDelta) * -ScrollSpeed);
            TScrollPrefer.Horizontal: FHorzScroll.Value := trunc(FHorzScroll.Value + WheelDelta / abs(WheelDelta) * -ScrollSpeed);
          end;
    end;

  Result := inherited;
end;

function FXScrollBox.IsContainer: Boolean;
begin
  Result := true;
end;

function FXScrollBox.IsDesigning: boolean;
begin
  Result := csDesigning in ComponentState;
end;

procedure FXScrollBox.Loaded;
begin
  inherited;
  if not IsDesigning then
    ResetScrollBars;
end;

procedure FXScrollBox.Redraw;
begin
  Invalidate;
end;

procedure FXScrollBox.ResetScrollBars;
begin
  FHorzScroll.BringToFront;
  FVertScroll.BringToFront;
end;

procedure FXScrollBox.ScrollByEx(DeltaX, DeltaY: Integer);
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
  if IsVisible then ScrollWindow(WindowHandle, DeltaX, DeltaY, nil, nil);
    {ScrollWindowEx(WindowHandle, DeltaX, DeltaY, @UpdateRect, @UpdateRect, 0, @UpdateRect,
      SW_ERASE or SW_SCROLLCHILDREN or SW_INVALIDATE);}
    //ScrollWindow(WindowHandle, DeltaX, DeltaY, @UpdateRect, @UpdateRect);

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
      if not IsVisible and not (Control is FXScrollBoxScrollBar) then
        with TWinControl(Control) do
          SetWindowPos(WindowHandle, 0, Left + DeltaX, Top + DeltaY,
            Width, Height, SWP_NOZORDER + SWP_NOACTIVATE);
  end;

  // Align
  Realign;
end;

procedure FXScrollBox.SetBackground(const Value: FXBackgroundColor);
begin
  if FBackground <> Value then
    begin
      FBackground := Value;

      UpdateColors;
      Invalidate;

      // Apply to scrollbars
      FVertScroll.UpdateTheme(false);
      FHorzScroll.UpdateTheme(false);
    end;
end;

procedure FXScrollBox.SetExtX(const Value: integer);
begin
  FExtendX := Value;
  CalculateRange;
end;

procedure FXScrollBox.SetExtY(const Value: integer);
begin
  FExtendY := Value;
  CalculateRange;
end;

procedure FXScrollBox.UpdateColors;
begin
  if FCustomColors.Enabled then
    begin
      FDrawColors.LoadFrom(FCustomColors, ThemeManager.DarkTheme)
    end
      else
    begin
      ThemeManager.LoadColorSet( FDrawColors );

      FDrawColors.BackGround := GetParentBackgroundColorEx(Self, FDrawColors.BackGround);
    end;

  // Update
  if FBackground = FXBackgroundColor.Background then
    Color := FDrawColors.BackGround
  else
    Color := FDrawColors.BackGroundInterior;
end;

procedure FXScrollBox.UpdateScrollbars;
begin
  // Design Mode
  if IsDesigning then
    begin
      FVertScroll.Left := -FVertScroll.Width;
      FHorzScroll.Top := -FHorzScroll.Height;

      Exit;
    end;

  // Update Scrollbars
  with FVertScroll do
    begin
      Top := 0;
      Left := Self.Width - Width;

      Height := Self.Height;

      // Enabled
      if Visible then
        if Visible <> FEnableVertical then
          Visible := FEnableVertical;
    end;

  with FHorzScroll do
    begin
      Top := Self.Height - Height;
      Left := 0;

      if FVertScroll.Visible then
        Width := Parent.Width - FVertScroll.Width
      else
        Width := Parent.Width;

      // Enabled
      if Visible then
        if Visible <> FEnableHorizontal then
          Visible := FEnableHorizontal;
    end;
end;

procedure FXScrollBox.UpdateTheme(const UpdateChildren: Boolean);
var
  I: integer;
begin
  UpdateColors;
  UpdateScrollbars;

  FVertScroll.UpdateTheme(true);
  FHorzScroll.UpdateTheme(true);

  // Update Children
  if IsContainer and UpdateChildren then
    begin
      for i := 0 to ControlCount - 1 do
        if Supports(Controls[i], IFXComponent) then
          (Controls[i] as IFXComponent).UpdateTheme(UpdateChildren);
    end;
end;

procedure FXScrollBox.WhenScroll(Sender: TObject);
var
  AScrollBy: integer;
  ANewPos: integer;
begin
  // Design
  if Self.IsDesigning then
    Exit;

  // Move Controls
  with FXScrollBar(Sender) do
    case Tag of
      0: begin
        ANewPos := Value;
        AScrollBy := FPosY-ANewPos;

        ScrollByEx(0, AScrollBy);

        FPosY := ANewPos;
      end;
      1: begin
        ANewPos := Value;
        AScrollBy := FPosX-ANewPos;

        ScrollByEx(AScrollBy, 0);

        FPosX := ANewPos;
      end;
    end;

  // Notify Event
  if Assigned(OnScroll) then
    OnScroll(Self);

  // Update
  UpdateScrollbars;
end;

{ FXScrollBoxScrollBar }

procedure FXScrollBoxScrollBar.CalcAutoRange;
var
  FControl: FXScrollBox;
  I: Integer;
  NewRange, AlignMargin, ControlSize, ZoneSize: Integer;

procedure ProcessHorz(Control: TControl);
  begin
    if Control.Visible then
      case Control.Align of
        alLeft, alNone:
          if (Control.Align = alLeft) or (Control.Anchors * [akLeft, akRight] = [akLeft]) then
            NewRange := Math.Max(NewRange, Value + Control.Left + Control.Width + FControl.ScrollExtendX);
        alRight: Inc(AlignMargin, Control.Width);
      end;
  end;

procedure ProcessVert(Control: TControl);
  begin
    if Control.Visible then
      case Control.Align of
        alTop, alNone:
          if (Control.Align = alTop) or (Control.Anchors * [akTop, akBottom] = [akTop]) then
            NewRange := Math.Max(NewRange, Value + Control.Top + Control.Height + FControl.ScrollExtendY);
        alBottom: Inc(AlignMargin, Control.Height);
      end;
  end;

begin
  if Parent is FXScrollbox then
    begin
      // Control
      FControl := FXScrollBox(Parent);

      // Size
      if Orientation = FXOrientation.Vertical then
        ControlSize := FControl.Height - Width
      else
        ControlSize := FControl.Width - Height;

      // Range
      NewRange := 0;
      AlignMargin := 0;
      for I := 0 to FControl.ControlCount - 1 do
        if not (FControl.Controls[I] is FXScrollBoxScrollBar) then
          if Orientation = FXOrientation.Horizontal then
            ProcessHorz(FControl.Controls[I])
          else
            ProcessVert(FControl.Controls[I]);

      // Calc Range
      ZoneSize := NewRange;
      NewRange := NewRange + AlignMargin - ControlSize;

      // Extra Range, cancel if controls fit
      if (Value = 0) and (ControlSize >= ZoneSize) then
        NewRange := 0;

      // Set Max
      if NewRange >= Value then
        Self.Max := Math.Max(NewRange, 0);

      // Visible
      if Orientation = FXOrientation.Vertical then
        Visible := (Self.Max > 0) and FControl.ScrollbarEnableVertical
      else
        Visible := (Self.Max > 0) and FControl.ScrollbarEnableHorizontal;
    end
end;

procedure FXScrollBoxScrollBar.PaintBuffer;
begin
  if not IsDesigning then
    inherited;
end;

end.
