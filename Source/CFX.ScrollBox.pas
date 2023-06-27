unit CFX.ScrollBox;

{$SCOPEDENUMS ON}

interface

uses
  Classes,
  Messages,
  Windows,
  Vcl.Controls,
  Vcl.Graphics,
  Types,
  CFX.Colors,
  Vcl.Dialogs,
  CFX.ThemeManager,
  CFX.Graphics,
  CFX.UIConsts,
  SysUtils,
  CFX.Classes,
  CFX.Types,
  CFX.Linker,
  CFX.Controls,
  CFX.Scrollbar,
  Math,
  Vcl.Forms;

  type
    // Cardinal
    TScrollPrefer = (Vertical, Horizontal, None);

    // Scrollbox Scrollbar
    FXScrollBoxScrollBar = class(FXScrollbar, FXControl)
    protected
      procedure PaintBuffer; override;
    public
      procedure CalcAutoRange;
    end;

    FXScrollBox = class(TScrollBox, FXControl)
    private
      FCustomColors: FXColorSets;
      FDrawColors: FXColorSet;

      FScrollSpeed: integer;

      FVertScroll,
      FHorzScroll: FXScrollBoxScrollBar;

      FEnableVertical,
      FEnableHorizontal: boolean;

      FPosX, FPosY: integer;
      FExtendX, FExtendY: integer;

      // Scroll
      procedure WhenScroll(Sender: TObject);

      procedure CalculateRange;
      procedure UpdateScrollbars;
      procedure ResetScrollBars;
      procedure ScrollByEx(DeltaX, DeltaY: Integer);

      // Set
      procedure SetExtX(const Value: integer);
      procedure SetExtY(const Value: integer);

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

    published
      property CustomColors: FXColorSets read FCustomColors write FCustomColors stored true;

      property ScrollExtendX: integer read FExtendX write SetExtX default 0;
      property ScrollExtendY: integer read FExtendY write SetExtY default 100;

      property ScrollbarEnableVertical: boolean read FEnableVertical write FEnableVertical default true;
      property ScrollbarEnableHorizontal: boolean read FEnableHorizontal write FEnableHorizontal default true;

      property ScrollSpeed: integer read FScrollSpeed write FScrollSpeed default 10;

      property ScrollBarVert: FXScrollBoxScrollBar read FVertScroll write FVertScroll;
      property ScrollBarHorz: FXScrollBoxScrollBar read FHorzScroll write FHorzScroll;

    public
      constructor Create(aOwner: TComponent); override;
      destructor Destroy; override;

      // Interface
      function IsContainer: Boolean;
      procedure UpdateTheme(const UpdateChildren: Boolean);

      function Background: TColor;
    end;

implementation

{ FXScrollBox }

procedure FXScrollBox.AdjustClientRect(var Rect: TRect);
begin
  if csDesigning in ComponentState then
    inherited
  else
    Rect := Bounds(-FHorzScroll.Position, -FVertScroll.Position,
      Max(FHorzScroll.Max, ClientWidth), Max(ClientHeight,
      FVertScroll.Max));

  CalculateRange;
  UpdateScrollbars;
end;

function FXScrollBox.Background: TColor;
begin
  Result := FDrawColors.BackGround;
end;

procedure FXScrollBox.CalculateRange;
begin
  FVertScroll.CalcAutoRange;
  FHorzScroll.CalcAutoRange;
end;

constructor FXScrollBox.Create(aOwner: TComponent);
begin
  inherited;
  FDrawColors := FXColorSet.Create(Self);
  FCustomColors := FXColorSets.Create(Self);

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

  // Prepare Scrollbars
  with FVertScroll do
    begin
      Parent := Self;

      Orientation := FXOrientation.Vertical;
      Tag := 0;
      OnChange := WhenScroll;

      Max := 0;
    end;

  with FHorzScroll do
    begin
      Parent := Self;

      Orientation := FXOrientation.Horizontal;

      Tag := 1;
      OnChange := WhenScroll;

      Max := 0;
    end;

  Width := 200;
  Height := 150;

  BorderStyle := bsNone;

  // Theme
  UpdateTheme(false);

  // Update Position
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
      case Prefer of
        TScrollPrefer.Vertical: FVertScroll.Position := trunc(FVertScroll.Position + WheelDelta / abs(WheelDelta) * -ScrollSpeed);
        TScrollPrefer.Horizontal: FHorzScroll.Position := trunc(FHorzScroll.Position + WheelDelta / abs(WheelDelta) * -ScrollSpeed);
      end;
    end;

  Result := inherited;
end;

function FXScrollBox.IsContainer: Boolean;
begin
  Result := false;
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
begin
  // Temp
  IsVisible := (WindowHandle <> 0) and IsWindowVisible(WindowHandle);
  if IsVisible then ScrollWindow(WindowHandle, DeltaX, DeltaY, nil, nil);
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
  Realign;
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
        ThemeManager.LoadColorSet( FDrawColors );

  // Update
  Color := FDrawColors.BackGround;
end;

procedure FXScrollBox.UpdateScrollbars;
begin
  // Design Mode
  if IsDesigning then
    begin
      FVertScroll.Left := FVertScroll.Width;
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
        Visible := FEnableVertical;
    end;

  with FHorzScroll do
    begin
      Top := Self.Height - Height;
      Left := 0;

      Width := Self.Width;

      if FVertScroll.Visible then
        Width := Width - FVertScroll.Width;

      // Enabled
      if Visible then
        Visible := FEnableHorizontal;
    end;
end;

procedure FXScrollBox.UpdateTheme(const UpdateChildren: Boolean);
begin
  UpdateColors;
  UpdateScrollbars;
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
        ANewPos := Position;
        AScrollBy := FPosY-ANewPos;

        ScrollByEx(0, AScrollBy);

        FPosY := ANewPos;
      end;
      1: begin
        ANewPos := Position;
        AScrollBy := FPosX-ANewPos;

        ScrollByEx(AScrollBy, 0);

        FPosX := ANewPos;
      end;
    end;

  // Update
  UpdateScrollbars;
end;

{ FXScrollBoxScrollBar }

procedure FXScrollBoxScrollBar.CalcAutoRange;
var
  FControl: FXScrollBox;
  I: Integer;
  NewRange, AlignMargin, ControlSize: Integer;

procedure ProcessHorz(Control: TControl);
  begin
    if Control.Visible then
      case Control.Align of
        alLeft, alNone:
          if (Control.Align = alLeft) or (Control.Anchors * [akLeft, akRight] = [akLeft]) then
            NewRange := Math.Max(NewRange, Position + Control.Left + Control.Width + FControl.ScrollExtendX);
        alRight: Inc(AlignMargin, Control.Width);
      end;
  end;

procedure ProcessVert(Control: TControl);
  begin
    if Control.Visible then
      case Control.Align of
        alTop, alNone:
          if (Control.Align = alTop) or (Control.Anchors * [akTop, akBottom] = [akTop]) then
            NewRange := Math.Max(NewRange, Position + Control.Top + Control.Height + FControl.ScrollExtendY);
        alBottom: Inc(AlignMargin, Control.Height);
      end;
  end;

begin
  if Parent is TScrollbox then
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
        if not (FControl.Controls[I] is FXScrollBoxScrollBar)  then
          if Orientation = FXOrientation.Horizontal then
            ProcessHorz(FControl.Controls[I]) else
            ProcessVert(FControl.Controls[I]);

      NewRange := NewRange + AlignMargin - ControlSize;

      if NewRange > Position then
        Self.Max := Math.Max(NewRange, 0);

      Visible := (Self.Max <> 0);
    end
end;

procedure FXScrollBoxScrollBar.PaintBuffer;
begin
  if not IsDesigning then
    inherited;
end;

end.