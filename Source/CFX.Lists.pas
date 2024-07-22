unit CFX.Lists;

interface

uses
  Classes,
  Messages,
  Windows,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.ExtCtrls,
  Types,
  Vcl.Forms,
  Math,
  CFX.Colors,
  CFX.ArrayHelpers,
  CFX.Animation.Main,
  CFX.Animation.Component,
  CFX.ThemeManager,
  CFX.Graphics,
  CFX.UIConsts,
  CFX.Scrollbar,
  SysUtils,
  CFX.Classes,
  CFX.Types,
  CFX.Utilities,
  CFX.VarHelpers,
  CFX.Linker,
  CFX.Controls;

type
  // Scrollbox Scrollbar
  FXDrawListScrollBar = class(FXScrollbar, FXControl)
  protected
    procedure PaintBuffer; override;
  end;

  FXDrawListOnDraw = procedure(Sender: TObject; AIndex: integer; ARect: TRect; Canvas: TCanvas) of object;

  FXDrawList = class(FXWindowsControl, FXControl)
  private
    procedure SetOpacityHover(const Value: byte);
    procedure SetOpacitySelected(const Value: byte);
    procedure SetDefaultDraw(const Value: boolean);
    type TRectSet = record
      Index: integer;
      Rectangle: TRect;
    end;

    var DrawRect: TRect;
    FDrawColors: FXCompleteColorSet;
    FCustomColors: FXColorSets;
    FNoOutOfBoundsDraw: boolean;
    FShowScrollbars: boolean;
    FHandleScrolling: boolean;
    FVisibleList: TArray<TRectSet>;

    FItemIndex,
    FItemIndexHover: integer;

    FMultiSelect: boolean;

    FVertScroll,
    FHorzScroll: FXDrawListScrollBar;

    FItemRects: TArray<TRect>;
    FItemSelected: TArray<boolean>;
    FItemHoverLocalPosition: TPoint;

    FOpacityHover,
    FOpacitySelected: byte;

    FOnDrawItem: FXDrawListOnDraw;
    FDefaultDraw: boolean;

    FExtendX, FExtendY: integer;

    FAnimX, FAnimY: FXIntAnim;

    FOnItemClick,
    FOnItemDoubleClick,
    FOnItemHover,
    FOnItemSelect: TNotifyEvent;

    //  Internal
    procedure UpdateColors;
    procedure UpdateRects; virtual;

    procedure ClearSelectedInternal;

    procedure UpdateScrollbars; // update actual position & size of scroll bars!
    procedure RecalculateScroll; // update scroll bar values

    // Scroll notifiers
    procedure ScrollChanged(Sender: TObject); // user
    procedure ScrollChangedValue(Sender: TObject); // any

    // Getters
    function GetItemSelected(Index: integer): boolean;

    // Setters
    procedure SetOrientation(const Value: FXOrientation);
    procedure SetNoOutOfBoundsDraw(const Value: boolean);
    procedure SetExtX(const Value: integer);
    procedure SetExtY(const Value: integer);
    procedure SetShowScrollbars(const Value: boolean);
    procedure SetItemCount(const Value: integer);
    procedure SetItemSelected(Index: integer; const Value: boolean);
    procedure SetItemIndex(const Value: integer);
    procedure SetItemIndexHover(const Value: integer);

  protected
    procedure PaintBuffer; override;
    procedure Resize; override;

    procedure DrawItem(Index: integer; ARect: TRect; Canvas: TCanvas); virtual;

    // Animation
    procedure AnimationStep(Sender: TObject; Step, TotalSteps: integer);

    // System events
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure Click; override;
    procedure DblClick; override;

    // padding
    function GetClientRect: TRect; override;

    // Getters
    function GetInBounds(Index: integer): boolean; virtual;
    function GetItemCount: integer; virtual;
    function GetItemDisplayRect(Index: integer): TRect; virtual;
    function GetItemRect(Index: integer): TRect; virtual;

    // Scaler
    procedure ScaleChanged(Scaler: single); override;

    // State
    procedure InteractionStateChanged(AState: FXControlState); override;

    // Props
    property OnDrawItem: FXDrawListOnDraw read FOnDrawItem write FOnDrawItem;

  published
    // Custom Colors
    property CustomColors: FXColorSets read FCustomColors write FCustomColors stored true;

    // Props
    property ShowScrollbars: boolean read FShowScrollbars write SetShowScrollbars default true;
    property NoOutOfBoundsDraw: boolean read FNoOutOfBoundsDraw write SetNoOutOfBoundsDraw;
    property ScrollExtendX: integer read FExtendX write SetExtX default 100;
    property ScrollExtendY: integer read FExtendY write SetExtY default 100;
    property HandleScrolling: boolean read FHandleScrolling write FHandleScrolling default true;
    property DefaultDraw: boolean read FDefaultDraw write SetDefaultDraw default true;

    property MultiSelect: boolean read FMultiSelect write FMultiSelect default false;

    property OnItemClick: TNotifyEvent read FOnItemClick write FOnItemClick;
    property OnItemDoubleClick: TNotifyEvent read FOnItemDoubleClick write FOnItemDoubleClick;
    property OnItemHover: TNotifyEvent read FOnItemHover write FOnItemHover;
    property OnItemSelect: TNotifyEvent read FOnItemSelect write FOnItemSelect;

    property OpacityHover: byte read FOpacityHover write SetOpacityHover;
    property OpacitySelected: byte read FOpacitySelected write SetOpacitySelected;

    property OnMouseWheel;
    property OnMouseWheelUp;
    property OnMouseWheelDown;

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

    // Data
    property ItemIndex: integer read FItemIndex write SetItemIndex;
    property ItemIndexHover: integer read FItemIndexHover write SetItemIndexHover;
    property ItemHoverLocalPosition: TPoint read FItemHoverLocalPosition;

    property ItemCount: integer read GetItemCount write SetItemCount;
    property ItemSelected[Index: integer]: boolean read GetItemSelected write SetItemSelected;
    property ItemInBounds[Index: integer]: boolean read GetInBounds;
    property ItemRect[Index: integer]: TRect read GetItemRect;
    property ItemDisplayRect[Index: integer]: TRect read GetItemDisplayRect;

    procedure ClearSelected;

    // Interface
    function IsContainer: Boolean;
    procedure UpdateTheme(const UpdateChildren: Boolean);

    function Background: TColor;
  end;

  FXLinearDrawList = class(FXDrawList)
  private
    FOrientation: FXOrientation;

    FItemWidth,
    FItemHeight: integer;
    FWrap: boolean;
    FSpacingRow,
    FSpacingColumn: integer;
    FJustifyContent: FXContentJustify;
    FFullLine: boolean;

    //  Internal
    procedure UpdateRects; override;

    procedure RecalculateRects;

    // Setters
    procedure SetOrientation(const Value: FXOrientation);
    procedure SetSpacingColumn(const Value: integer);
    procedure SetSpacingRow(const Value: integer);
    procedure SetWrap(const Value: boolean);
    procedure SetItemHeight(const Value: integer);
    procedure SetItemWidth(const Value: integer);
    procedure SetJustifyContent(const Value: FXContentJustify);
    procedure SetFullLine(const Value: boolean);

  published
    // Props
    property OnDrawItem;

    property Orientation: FXOrientation read FOrientation write SetOrientation default FXOrientation.Vertical;
    property JustifyContent: FXContentJustify read FJustifyContent write SetJustifyContent default FXContentJustify.Start;

    property ItemCount;
    property ItemWidth: integer read FItemWidth write SetItemWidth;
    property ItemHeight: integer read FItemHeight write SetItemHeight;

    property FullLine: boolean read FFullLine write SetFullLine default false;
    property Wrap: boolean read FWrap write SetWrap;
    property SpacingRow: integer read FSpacingRow write SetSpacingRow;
    property SpacingColumn: integer read FSpacingColumn write SetSpacingColumn;

  public
    // Constructors
    constructor Create(aOwner: TComponent); override;
  end;

implementation

procedure FXDrawList.AnimationStep(Sender: TObject; Step, TotalSteps: integer);
begin
  if Sender = FAnimX then begin
    // Horizontal
    FHorzScroll.Position := FXIntAnim(Sender).CurrentValue;
  end else begin
    // Vertical
    FVertScroll.Position := FXIntAnim(Sender).CurrentValue;
  end;
end;

function FXDrawList.Background: TColor;
begin
  Result := FDrawColors.BackGround;
end;

procedure FXDrawList.ClearSelected;
begin
  ClearSelectedInternal;

  Invalidate;
end;

procedure FXDrawList.ClearSelectedInternal;
begin
  for var I := 0 to ItemCount-1 do
    FItemSelected[I] := false;
end;

procedure FXDrawList.Click;
begin
  //ItemIndex := ItemIndexHover;

  // Event
  if (ItemIndex <> -1) and Assigned(FOnItemClick) then
    FOnItemClick(Self);

  inherited;
end;

constructor FXDrawList.Create(aOwner: TComponent);
begin
  inherited;
  // Custom Color
  FCustomColors := FXColorSets.Create(Self);

  FDrawColors := FXCompleteColorSet.Create;

  FShowScrollbars := true;
  FNoOutOfBoundsDraw := true;
  FDefaultDraw := true;

  FExtendX := 100;
  FExtendY := 100;

  FHandleScrolling := true;
  FMultiSelect := false;

  // Style
  FOpacityHover := 35;
  FOpacitySelected := 75;

  // Statuses
  FItemIndex := -1;
  FItemIndexHover := -1;

  // Create scrollbars
  FVertScroll := FXDrawListScrollBar.Create(Self);
  FHorzScroll := FXDrawListScrollBar.Create(Self);

  with FVertScroll do
    begin
      Parent := Self;

      Orientation := FXOrientation.Vertical;
      Tag := 0;
      OnChange := ScrollChanged;
      OnChangeValue := ScrollChangedValue;

      Max := 0;
    end;

  with FHorzScroll do
    begin
      Parent := Self;

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

    OnStep := AnimationStep;
  end;

  FAnimY := FXIntAnim.Create(nil);
  with FAnimY do begin
    Kind := FXAnimationKind.ReverseExpo;
    Duration := SCROLL_DURATION;

    OnStep := AnimationStep;
  end;

  // Sizing
  Width := 350;
  Height := 250;

  // Update
  UpdateRects;
  UpdateColors;

  // Update Position
  UpdateScrollbars;
end;

procedure FXDrawList.DblClick;
begin
  inherited;

  // Event
  if (ItemIndex <> -1) and Assigned(FOnItemDoubleClick) then
    FOnItemDoubleClick(Self);
end;

destructor FXDrawList.Destroy;
begin
  FAnimX.Stop;
  FAnimY.Stop;
  FreeAndNil( FAnimX );
  FreeAndNil( FAnimY );

  FreeAndNil( FCustomColors );
  FreeAndNil( FDrawColors );

  FreeAndNil( FVertScroll );
  FreeAndNil( FHorzScroll );

  inherited;
end;

function FXDrawList.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  if Result then
    Exit( Result );

  if FHandleScrolling and not (ssCtrl in Shift) then begin
    const ScrollAmount = GetScrollAmount(WheelDelta, ClientRect.Height);

    Result := true; // handled

    if (ssShift in Shift) or (FVertScroll.Max = 0) then begin
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

procedure FXDrawList.DrawItem(Index: integer; ARect: TRect; Canvas: TCanvas);
begin
  if FDefaultDraw then
    with Canvas do begin
      Brush.Color := FDrawColors.BackGroundInterior;

      if ItemSelected[Index] then
        Brush.Color := ColorBlend(Brush.Color, FDrawColors.Accent, OpacitySelected)
      else
      if ItemIndexHover = Index then
        Brush.Color := ColorBlend(Brush.Color, FDrawColors.Accent, OpacityHover);


      Fillrect(ARect);
      //DrawTextRect(Canvas, ARect, Index.ToString, [], 10);
    end;

  if Assigned(OnDrawItem) then
    OnDrawItem(Self, Index, ARect, Canvas);
end;

function FXDrawList.GetClientRect: TRect;
begin
  Result := inherited;

  if FShowScrollbars then begin
    Result.Height := Result.Height - FHorzScroll.Height;
    Result.Width := Result.Width - FVertScroll.Width;
  end;
end;

function FXDrawList.GetInBounds(Index: integer): boolean;
var
  ARect: TRect;
begin
  ARect := GetItemDisplayRect(Index);

  // Return if element is visible
  Result := ARect.IntersectsWith( GetClientRect );
end;

function FXDrawList.GetItemCount: integer;
begin
  Result := Length( FItemRects );
end;

function FXDrawList.GetItemDisplayRect(Index: integer): TRect;
begin
  Result := GetItemRect(Index);

  Result.Offset( -FHorzScroll.Position, -FVertScroll.Position )
end;

function FXDrawList.GetItemRect(Index: integer): TRect;
begin
  Result := FItemRects[Index];
end;

function FXDrawList.GetItemSelected(Index: integer): boolean;
begin
  Result := FItemSelected[Index];
end;

procedure FXDrawList.InteractionStateChanged(AState: FXControlState);
begin
  inherited;
  Invalidate;
end;

function FXDrawList.IsContainer: Boolean;
begin
  Result := false;
end;

procedure FXDrawList.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: integer);
begin
  inherited;

  // Select item
  if ItemIndexHover <> -1 then begin
    if (ssCtrl in Shift) and MultiSelect then
      ItemSelected[ItemIndexHover] := not ItemSelected[ItemIndexHover]
    else
      ItemIndex := ItemIndexHover;

    if Assigned(FOnItemSelect) then
      FOnItemSelect(Self);
  end;
end;

procedure FXDrawList.MouseMove(Shift: TShiftState; X, Y: integer);
var
  FoundHover: boolean;
begin
  const Cursor = Point(X, Y);

  // Find hover value
  FoundHover := false;
  for var I := 0 to High(FVisibleList) do
    if FVisibleList[I].Rectangle.Contains(Cursor) then begin
      const Same = ItemIndexHover = FVisibleList[I].Index;
      ItemIndexHover := FVisibleList[I].Index;

      if not Same and Assigned(FOnItemHover) then
        FOnItemHover(Self);

      FItemHoverLocalPosition := Cursor.Subtract( FVisibleList[I].Rectangle.TopLeft );

      FoundHover := true;
      Break;
    end;
  if not FoundHover then
    ItemIndexHover := -1;

  inherited;
end;

procedure FXDrawList.PaintBuffer;
var
  I: Integer;
begin
  // Background
  Color := FDrawColors.BackGround;
  PaintBackground;

  // Draw
  FVisibleList := [];
  for I := 0 to High(FItemRects) do begin
    const InBounds = GetInBounds(I);
    const Display = GetItemDisplayRect(I);

    if InBounds then
      with FVisibleList[TArrayUtils<TRectSet>.AddValue(FVisibleList)] do begin
      Index := I;
      Rectangle := Display;
    end;

    if (not FNoOutOfBoundsDraw or InBounds) then
      DrawItem(I, Display, Buffer);
  end;

  // Inherit
  inherited;
end;

procedure FXDrawList.RecalculateScroll;
var
  MaxX, MaxY: integer;
  Client: TRect;
begin
  Client := GetClientRect;
  MaxX := 0;
  MaxY := 0;

  for var I := 0 to High(FItemRects) do begin
    MaxX := Max( MaxX, FItemRects[I].Right);
    MaxY := Max( MaxY, FItemRects[I].Bottom);
  end;

  // Set maxes
  FHorzScroll.Max := Max(0, MaxX-Client.Width);
  if FHorzScroll.Max > 0 then
    FHorzScroll.Max := FHorzScroll.Max + FExtendX;
  FVertScroll.Max := Max(0, MaxY-Client.Height);
  if FVertScroll.Max > 0 then
    FVertScroll.Max := FVertScroll.Max + FExtendY;
end;

procedure FXDrawList.Resize;
begin
  inherited;
  UpdateRects;
end;

procedure FXDrawList.UpdateTheme(const UpdateChildren: Boolean);
begin
  UpdateColors;
  UpdateRects;
  Invalidate;
end;

procedure FXDrawList.ScrollChanged(Sender: TObject);
begin
  // STOP scroll animations
  FAnimX.Stop;
  FAnimY.Stop;
end;

procedure FXDrawList.ScrollChangedValue(Sender: TObject);
begin
  // Draw
  Invalidate;
end;

procedure FXDrawList.UpdateColors;
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

procedure FXDrawList.UpdateRects;
begin
  // Rect
  DrawRect := GetClientRect;

  // Scroll;
  RecalculateScroll;
  UpdateScrollbars;
end;

procedure FXDrawList.UpdateScrollbars;
begin
  // Update Scrollbars
  with FVertScroll do
    begin
      Top := 0;
      Left := Self.Width - Width;

      Height := Self.Height;

      // Visible
      Visible := FShowScrollbars and (Max > 0);
    end;

  with FHorzScroll do
    begin
      Top := Self.Height - Height;
      Left := 0;

      if FVertScroll.Visible then
        Width := Parent.Width - FVertScroll.Width
      else
        Width := Parent.Width;

      // Visible
      Visible := FShowScrollbars and (Max > 0);
    end;

  // Design mode
  if IsDesigning then begin
    with FVertScroll do
      if not Visible then
        Left := -Width;
    with FHorzScroll do
      if not Visible then
        Top := -Height;
  end;
end;

procedure FXDrawList.ScaleChanged(Scaler: single);
begin
  inherited;
  // update scale
end;

procedure FXDrawList.SetDefaultDraw(const Value: boolean);
begin
  if FDefaultDraw = Value then
    Exit;

  FDefaultDraw := Value;
  Invalidate;
end;

procedure FXDrawList.SetExtX(const Value: integer);
begin
  if FExtendX = Value then
    Exit;

  FExtendX := Value;
  UpdateRects;
end;

procedure FXDrawList.SetExtY(const Value: integer);
begin
  if FExtendY = Value then
    Exit;

  FExtendY := Value;
  UpdateRects;
end;

procedure FXDrawList.SetItemCount(const Value: integer);
begin
  if ItemCount = Value then
    Exit;

  FItemIndex := -1;
  FItemIndexHover := -1;

  SetLength(FItemRects, Value);
  SetLength(FItemSelected, Value);
  UpdateRects;
  Invalidate;
end;

procedure FXDrawList.SetItemIndex(const Value: integer);
begin
  if FItemIndex = Value then
    Exit;

  // Clear selected
  ClearSelectedInternal; // does not invalidate
  FItemSelected[Value] := true;

  // Set
  FItemIndex := Value;
  Invalidate;
end;

procedure FXDrawList.SetItemIndexHover(const Value: integer);
begin
  if FItemIndexHover = Value then
    Exit;

  FItemIndexHover := Value;
  Invalidate;
end;

procedure FXDrawList.SetItemSelected(Index: integer; const Value: boolean);
begin
  FItemSelected[Index] := Value;

  Invalidate;
end;

procedure FXDrawList.SetNoOutOfBoundsDraw(const Value: boolean);
begin
  if (FNoOutOfBoundsDraw = Value) then
    Exit;

  FNoOutOfBoundsDraw := Value;
  Invalidate;
end;

procedure FXDrawList.SetOpacityHover(const Value: byte);
begin
  if FOpacityHover = Value then
    Exit;

  FOpacityHover := Value;
  Invalidate;
end;

procedure FXDrawList.SetOpacitySelected(const Value: byte);
begin
  if FOpacitySelected = Value then
    Exit;

  FOpacitySelected := Value;
  Invalidate;
end;

procedure FXDrawList.SetOrientation(const Value: FXOrientation);
begin

end;

procedure FXDrawList.SetShowScrollbars(const Value: boolean);
begin
  if FShowScrollbars = Value then
    Exit;

  FShowScrollbars := Value;
  UpdateScrollbars;
  UpdateRects;
end;

{ FXDrawListScrollBar }

procedure FXDrawListScrollBar.PaintBuffer;
begin
  inherited;
end;

{ FXLinearDrawList }

constructor FXLinearDrawList.Create(aOwner: TComponent);
begin
  inherited;
  FOrientation := FXOrientation.Vertical;
  FJustifyContent := FXContentJustify.Start;
  FFullLine := false;

  FItemWidth := 150;
  FItemHeight := 100;

  FWrap := true;
  FSpacingRow := 8;
  FSpacingColumn := 8;
end;

procedure FXLinearDrawList.RecalculateRects;
var
  Pos: TPoint;

  Client: TRect;

  OffsetStart,
  OffsetSpacing,
  OffsetSize: integer;

  // Orientation based
  TotalSize, LeftoverSpace, ItemsFit, ItemSize, ItemSpace: integer;
begin
  if (ItemHeight <= 0) or (ItemWidth <= 0) then
    Exit;

  Client := GetClientRect;

  OffsetStart := 0;
  OffsetSpacing := 0;
  OffsetSize := 0;

  // Justify content
  case Orientation of
    FXOrientation.Horizontal: begin
      TotalSize := Client.Height;
      ItemSize := ItemHeight;
      ItemSpace := SpacingRow;
    end;
    FXOrientation.Vertical: begin
      TotalSize := Client.Width;
      ItemSize := ItemWidth;
      ItemSpace := SpacingColumn;
    end;
    else begin
      TotalSize := 0;
      ItemSize := 0;
      ItemSpace := 0;
    end;
  end;
  if FFullLine then begin
    ItemSize := TotalSize;
    ItemSpace := 0;
  end;

  ItemsFit := Max(1,  // at least one item must be
    (TotalSize+ItemSpace) div (ItemSize+ItemSpace)
    );
  LeftoverSpace := TotalSize-Max(0, (ItemSize+ItemSpace)*ItemsFit-ItemSpace);

  case JustifyContent of
    FXContentJustify.Center: OffsetStart := LeftoverSpace div 2;
    FXContentJustify.SpaceBetween:
      if ItemsFit > 1 then
        OffsetSpacing := LeftoverSpace div (ItemsFit-1);
    FXContentJustify.SpaceAround:
    if ItemsFit > 1 then begin
      OffsetStart := LeftoverSpace div (ItemsFit*2-1);
      OffsetSpacing := (LeftoverSpace-2*OffsetStart) div (ItemsFit-1);
    end;
    FXContentJustify.SpaceEvenly: begin
      OffsetStart := LeftoverSpace div (ItemsFit+2);
      OffsetSpacing := OffsetStart;
    end;
    FXContentJustify.Ending: OffsetStart := LeftoverSpace;
    FXContentJustify.Stretch: OffsetSize := LeftoverSpace div ItemsFit;
  end;

  // Start
  Pos := Client.TopLeft;
  case Orientation of
    FXOrientation.Horizontal: Inc(Pos.Y, OffsetStart);
    FXOrientation.Vertical: Inc(Pos.X, OffsetStart);
  end;

  // Calculate all rectanges
  for var I := 0 to High(FItemRects) do
    case Orientation of
      FXOrientation.Horizontal: begin
        FItemRects[I] := TRect.Create(Pos, FItemWidth, ItemSize+OffsetSize);

        // Add spacing
        Inc(Pos.Y, ItemSize+OffsetSize+ItemSpace+OffsetSpacing);

        // Next row?
        if Pos.Y+ItemSize+OffsetSize > Client.Bottom then begin
          Pos.Y := Client.Top+OffsetStart;
          Inc(Pos.X, FItemWidth+SpacingColumn);
        end;
      end;
      FXOrientation.Vertical: begin
        FItemRects[I] := TRect.Create(Pos, ItemSize+OffsetSize, FItemHeight);

        // Add spacing
        Inc(Pos.X, ItemSize+OffsetSize+ItemSpace+OffsetSpacing);

        // Next row?
        if Pos.X+ItemSize+OffsetSize > Client.Right then begin
          Pos.X := Client.Left+OffsetStart;
          Inc(Pos.Y, FItemHeight+SpacingRow);
        end;
      end;
    end;
end;

procedure FXLinearDrawList.SetFullLine(const Value: boolean);
begin
  if FFullLine = Value then
    Exit;

  FFullLine := Value;
  UpdateRects;
  Invalidate;
end;

procedure FXLinearDrawList.SetItemHeight(const Value: integer);
begin
  if (FItemHeight = Value) or (Value <= 0) then
    Exit;

  FVertScroll.SmallChange := Value;
  FItemHeight := Value;
  UpdateRects;
  Invalidate;
end;

procedure FXLinearDrawList.SetItemWidth(const Value: integer);
begin
  if (FItemWidth = Value) or (Value <= 0) then
    Exit;

  FItemWidth := Value;
  UpdateRects;
  Invalidate;
end;

procedure FXLinearDrawList.SetJustifyContent(const Value: FXContentJustify);
begin
  if FJustifyContent = Value then
    Exit;

  FJustifyContent := Value;
  UpdateRects;
  Invalidate;
end;

procedure FXLinearDrawList.SetOrientation(const Value: FXOrientation);
begin
  if FOrientation = Value then
    Exit;

  FOrientation := Value;
  UpdateRects;
  Invalidate;
end;

procedure FXLinearDrawList.SetSpacingColumn(const Value: integer);
begin
  if FSpacingColumn = Value then
    Exit;

  FSpacingColumn := Value;
  UpdateRects;
  Invalidate;
end;

procedure FXLinearDrawList.SetSpacingRow(const Value: integer);
begin
  if FSpacingRow = Value then
    Exit;

  FSpacingRow := Value;
  UpdateRects;
  Invalidate;
end;

procedure FXLinearDrawList.SetWrap(const Value: boolean);
begin
  if FWrap = Value then
    Exit;

  FWrap := Value;
  UpdateRects;
  Invalidate;
end;

procedure FXLinearDrawList.UpdateRects;
begin
  RecalculateRects; // calc rects

  FHorzScroll.SmallChange := ItemWidth + SpacingRow;
  FVertScroll.SmallChange := ItemHeight + SpacingColumn;

  inherited;
end;

end.