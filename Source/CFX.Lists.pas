unit CFX.Lists;

interface

uses
  Classes,
  Winapi.Messages,
  Winapi.Windows,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.ExtCtrls,
  UITypes,
  Types,
  Vcl.Forms,
  Vcl.Dialogs,
  CFX.Panels,
  Math,
  CFX.Colors,
  CFX.ArrayHelpers,
  CFX.Animation.Main,
  CFX.Animation.Component,
  CFX.ThemeManager,
  CFX.Graphics,
  CFX.Constants,
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
  FXDrawListScrollBar = class(FXScrollbar)
  protected
    procedure PaintBuffer; override;
  end;

  FXDrawListOnDraw = procedure(Sender: TObject; AIndex: integer; ARect: TRect; Canvas: TCanvas) of object;

  FXDrawList = class(FXWindowsControl)
  private
    procedure SetNoItemsOutputText(const Value: string);
    type TRectSet = record
      Index: integer;
      Rectangle: TRect;
    end;

    var DrawRect: TRect;
    FDrawColors: FXCompleteColorSet;
    FCustomColors: FXColorSets;
    FNoOutOfBoundsDraw: boolean;
    FVisibleList: TArray<TRectSet>;

    FBackground,
    FBackgroundItems: FXBackgroundColor;

    FItemIndex,
    FItemIndexHover: integer;

    FMultiSelect: boolean;
    FCanDeselect: boolean;
    FDefaultDraw: boolean;

    // Utils
    FNoItemsOutputText: string;

    // Scroll
    FVertScroll,
    FHorzScroll: FXDrawListScrollBar;
    FShowScrollbars: boolean;
    FHandleScrolling: boolean;
    FScrollAnimation: boolean;

    FExtendX, FExtendY: integer;
    FAnimX, FAnimY: FXIntAnim;

    // Items
    FItemRects: TArray<TRect>;
    FItemVisible: TArray<boolean>;
    FItemSelected: TArray<boolean>;
    FItemHoverLocalPosition: TPoint;

    // Data
    FOpacityHover,
    FOpacitySelected: byte;

    FKeyboardNavigation: boolean;

    // Notifiers
    FOnDrawItem,
    FOnBeforeDrawItem,
    FOnAfterDrawItem: FXDrawListOnDraw;

    FOnItemClick,
    FOnItemDoubleClick,
    FOnItemHover,
    FOnItemSelect: TNotifyEvent;

    // Internal
    procedure ClearSelectedInternal;
    procedure ClearHiddenInternal;

    procedure UpdateScrollbars; // update actual position & size of scroll bars!
    procedure CalculateScroll; // update scroll bar values

    procedure StopScrollAnimations;
    procedure EnsureIndexVisible;

    // Scroll notifiers
    procedure ScrollChanged(Sender: TObject); // user
    procedure ScrollChangedValue(Sender: TObject); // any

    // Setter ex
    procedure SetItemVisibleEx(Index: integer; const Value: boolean);

    // Getters
    function GetItemSelected(Index: integer): boolean;
    function GetItemVisible(Index: integer): boolean;

    // Setters
    procedure SetNoOutOfBoundsDraw(const Value: boolean);
    procedure SetExtX(const Value: integer);
    procedure SetExtY(const Value: integer);
    procedure SetShowScrollbars(const Value: boolean);
    procedure SetItemCount(const Value: integer);
    procedure SetItemSelected(Index: integer; const Value: boolean);
    procedure SetItemVisible(Index: integer; const Value: boolean);
    procedure SetItemIndex(const Value: integer);
    procedure SetItemIndexHover(const Value: integer);
    procedure SetOpacityHover(const Value: byte);
    procedure SetOpacitySelected(const Value: byte);
    procedure SetDefaultDraw(const Value: boolean);
    procedure SetBackground(const Value: FXBackgroundColor);
    procedure SetBackgroundItems(const Value: FXBackgroundColor);

  protected
    procedure PaintBuffer; override;

    // Size
    procedure Resize; override;

    // Internal
    procedure UpdateColors; override;
    procedure UpdateRects; override;

    // Draw
    function GetItemBackgroundColor(Index: integer): TColor; virtual;
    procedure DrawItem(Index: integer; ARect: TRect; Canvas: TCanvas); virtual;
    procedure DrawNoItemsText; virtual;

    // Animation
    procedure AnimationStep(Sender: TObject; Step, TotalSteps: integer);

    // System events
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure Click; override;
    procedure DblClick; override;
    procedure HandleKeyDown(var CanHandle: boolean; Key: integer; Shift: TShiftState); override;

    // Status
    procedure ItemVisibilityChanged; virtual;

    // Inner client
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
    property OnBeforeDrawItem: FXDrawListOnDraw read FOnBeforeDrawItem write FOnBeforeDrawItem;
    property OnAfterDrawItem: FXDrawListOnDraw read FOnAfterDrawItem write FOnAfterDrawItem;

  published
    // Custom Colors
    property CustomColors: FXColorSets read FCustomColors write FCustomColors stored true;

    property BackgroundColor: FXBackgroundColor read FBackground write SetBackground default FXBackgroundColor.Background;
    property BackgroundColorItems: FXBackgroundColor read FBackgroundItems write SetBackgroundItems default FXBackgroundColor.Content;

    property NoItemsOutputText: string read FNoItemsOutputText write SetNoItemsOutputText;

    // Props
    property ShowScrollbars: boolean read FShowScrollbars write SetShowScrollbars default true;
    property NoOutOfBoundsDraw: boolean read FNoOutOfBoundsDraw write SetNoOutOfBoundsDraw;
    property ScrollExtendX: integer read FExtendX write SetExtX default 100;
    property ScrollExtendY: integer read FExtendY write SetExtY default 100;
    property HandleScrolling: boolean read FHandleScrolling write FHandleScrolling default true;
    (* Scroll smoothly *)
    property ScrollAnimation: boolean read FScrollAnimation write FScrollAnimation default true;
    property DefaultDraw: boolean read FDefaultDraw write SetDefaultDraw default true;

    property MultiSelect: boolean read FMultiSelect write FMultiSelect default false;
    property CanDeselect: boolean read FCanDeselect write FCanDeselect default false;

    property OnItemClick: TNotifyEvent read FOnItemClick write FOnItemClick;
    property OnItemDoubleClick: TNotifyEvent read FOnItemDoubleClick write FOnItemDoubleClick;
    property OnItemHover: TNotifyEvent read FOnItemHover write FOnItemHover;
    property OnItemSelect: TNotifyEvent read FOnItemSelect write FOnItemSelect;

    property OpacityHover: byte read FOpacityHover write SetOpacityHover default LIST_ITEM_OPACITY_HOVER;
    property OpacitySelected: byte read FOpacitySelected write SetOpacitySelected default LIST_ITEM_OPACITY_SELECTED;

    property OnMouseWheel;
    property OnMouseWheelUp;
    property OnMouseWheelDown;

    // Default props
    property Align;
    property Font;
    //property PaddingFill;
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
    property OnMouseMove;
    property OnResize;

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
    property ItemVisible[Index: integer]: boolean read GetItemVisible write SetItemVisible;

    function SelectedItemCount: integer;
    function GetSelectedItems: TArray<integer>;
    procedure SetVisibility(Items: TArray<integer>; Visible: boolean; Clear: boolean=false);
    procedure ClearSelection; virtual;
    procedure ClearHidden; virtual;
    procedure SelectAll; virtual;

    procedure ResetView;

    // Interface
    function IsContainer: Boolean; override;
    function Background: TColor; override;
  end;

  FXCustomLinearDrawList = class(FXDrawList)
  private
    FOrientation: FXOrientation;

    FItemWidth,
    FItemHeight: integer;
    FWrap: boolean;
    FSpacingRow,
    FSpacingColumn: integer;
    FJustifyContent: FXContentJustify;
    FFullLine: boolean;
    FActualSize: TPoint;

    //  Internal
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

  protected
    // Internal
    procedure UpdateRects; override;

    // Status
    procedure ItemVisibilityChanged; override;

    // Scaler
    procedure ScaleChanged(Scaler: single); override;

    // Props
    property OnDrawItem;
    property OnBeforeDrawItem;

    property Orientation: FXOrientation read FOrientation write SetOrientation default FXOrientation.Vertical;
    property JustifyContent: FXContentJustify read FJustifyContent write SetJustifyContent default FXContentJustify.Start;

    property ItemCount;
    property ItemWidth: integer read FItemWidth write SetItemWidth;
    property ItemHeight: integer read FItemHeight write SetItemHeight;

    property FullLine: boolean read FFullLine write SetFullLine default false;
    property Wrap: boolean read FWrap write SetWrap;
    property SpacingRow: integer read FSpacingRow write SetSpacingRow;
    property SpacingColumn: integer read FSpacingColumn write SetSpacingColumn;

  published
    property ActualSize: TPoint read FActualSize;

  public
    // Constructors
    constructor Create(aOwner: TComponent); override;
  end;

  FXLinearDrawList = class(FXCustomLinearDrawList)
  published
    property OnDrawItem;
    property OnBeforeDrawItem;

    property Orientation;
    property JustifyContent;

    property ItemCount;
    property ItemWidth;
    property ItemHeight;

    property FullLine;
    property Wrap;
    property SpacingRow;
    property SpacingColumn;
  end;

  FXLinearStringsList = class(FXCustomLinearDrawList)
  private
    FStrings: TStringList;
    FItemMargins: FXMargins;

    // Utils
    procedure UpdateCount;

    // Notify
    procedure StringsChanged(Sender: TObject);
    procedure MarginsChanged(Sender: TObject);

    // Getters
    function GetItemCountEx: integer;

    // Setters
    procedure SetStrings(const Value: TStringList);

  protected
    // Draw
    procedure DrawItem(Index: integer; ARect: TRect; Canvas: TCanvas); override;

  published
    // Props
    property Items: TStringList read FStrings write SetStrings;
    property ItemMargins: FXMargins read FItemMargins write FItemMargins;

    property Font;

    property OnDrawItem;
    property OnBeforeDrawItem;

    property Orientation;
    property JustifyContent;

    property ItemWidth;
    property ItemHeight default 40;

    property FullLine default true;
    property Wrap;
    property SpacingRow;
    property SpacingColumn;

  public
    // Public props
    property ItemCount: integer read GetItemCountEx;

    // Constructor
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

  // Container
  FXControlContainer = class(FXWindowsControl)
  private
    FList: FXDrawList;
    FBackgroundColor: TColor;

  protected
    // Draw
    procedure PaintBuffer; override;

    // When a new control is added
    procedure CMControlListChange(var Msg: TCMControlListChange); message CM_CONTROLLISTCHANGE;

  public
    constructor CreateInList(AList: FXDrawList);
    destructor Destroy; override;

    // Interface
    function IsContainer: Boolean; override;
    function Background: TColor; override;
  end;

  FXLinearControlList = class(FXCustomLinearDrawList)
  private
    FContainer: FXControlContainer;

    // Padding and margins
    function GetItemPaddding: FXMargins;
    procedure SetItemPaddding(const Value: FXMargins);
    function GetItemInnerMarginsFill: FXMargins;
    procedure SetItemInnerMarginsFill(const Value: FXMargins);

  protected
    // Draw
    procedure PaintBuffer; override;

    // Control adder
    procedure CMControlListChange(var Msg: TCMControlListChange); message CM_CONTROLLISTCHANGE;

    // Created
    procedure ComponentCreated; override;

    // Internal
    procedure UpdateRects; override;

    procedure DrawItem(Index: integer; ARect: TRect; Canvas: TCanvas); override;
    procedure DrawNoItemsText; override;

    function GetChildParent: TComponent; override; // set the loaded children parent

  published
    property OnDrawItem;
    property OnBeforeDrawItem;

    property Orientation;
    property JustifyContent;

    property ItemCount;
    property ItemWidth;
    property ItemHeight;

    property FullLine;
    property Wrap;
    property SpacingRow;
    property SpacingColumn;

    property Container: FXControlContainer read FContainer write FContainer stored true;

    property ItemPaddding: FXMargins read GetItemPaddding write SetItemPaddding;
    property ItemInnerMarginsFill: FXMargins read GetItemInnerMarginsFill write SetItemInnerMarginsFill;

  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;  // iterate all children

    // Control manage
    procedure AddControlToItem(AControl: TControl);

    // Interface
    function IsContainer: Boolean; override;
  end;

implementation

procedure FXDrawList.AnimationStep(Sender: TObject; Step, TotalSteps: integer);
begin
  if Destroyed then
    Exit;

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

function FXDrawList.Background: TColor;
begin
  Result := FDrawColors.BackGround;
end;

procedure FXDrawList.ClearSelection;
begin
  FItemIndex := -1;
  ClearSelectedInternal;

  StandardUpdateDraw;
end;

procedure FXDrawList.ClearHidden;
begin
  ClearHiddenInternal;

  // Update
  ItemVisibilityChanged;
end;

procedure FXDrawList.ClearHiddenInternal;
begin
  for var I := 0 to High(FItemVisible) do
    FItemVisible[I] := true;
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
  FNoItemsOutputText := '';

  FBackground := FXBackgroundColor.Background;
  FBackgroundItems := FXBackgroundColor.Content;

  FDrawColors := FXCompleteColorSet.Create;

  FKeyboardNavigation := true;

  FScrollAnimation := true;
  FShowScrollbars := true;
  FNoOutOfBoundsDraw := true;
  FDefaultDraw := true;

  FExtendX := 100;
  FExtendY := 100;

  FHandleScrolling := true;
  FMultiSelect := false;

  // Style
  FOpacityHover := LIST_ITEM_OPACITY_HOVER;
  FOpacitySelected := LIST_ITEM_OPACITY_SELECTED;

  // Statuses
  FItemIndex := -1;
  FItemIndexHover := -1;

  // Create scrollbars
  FVertScroll := FXDrawListScrollBar.Create(Self);
  FHorzScroll := FXDrawListScrollBar.Create(Self);

  with FHorzScroll do
    begin
      Parent := Self;

      Orientation := FXOrientation.Horizontal;

      Tag := 1;
      OnChange := ScrollChanged;
      OnChangeValue := ScrollChangedValue;
    end;
  with FVertScroll do
    begin
      Parent := Self;

      Orientation := FXOrientation.Vertical;
      Tag := 0;
      OnChange := ScrollChanged;
      OnChangeValue := ScrollChangedValue;
    end;

  FHorzScroll.Max := 0;
  FVertScroll.Max := 0;

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

  // Sizing
  Width := 350;
  Height := 250;

  // Update Position
  UpdateScrollbars;
end;

procedure FXDrawList.DblClick;
begin
  inherited;

  // Event
  if (ItemIndex <> -1) and (ItemIndex = ItemIndexHover) and Assigned(FOnItemDoubleClick) then
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
  if IsDesigning then
    Exit(false);

  Result := false;

  if FHandleScrolling and not (ssCtrl in Shift) then begin
    const ScrollAmount = GetScrollAmount(WheelDelta, ClientRect.Height);
    const HorizontalScroll = (ssShift in Shift) or (FVertScroll.Max = 0);

    Result := true; // handled

    if not ScrollAnimation then begin
      // Instant
      if HorizontalScroll then
        FHorzScroll.Position:= FHorzScroll.Position +ScrollAmount
      else
        FVertScroll.Position:= FVertScroll.Position +ScrollAmount
    end else begin
      // Animate
      if HorizontalScroll then begin
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
  end;

  if not Result then
    Result := inherited;
end;

procedure FXDrawList.DrawItem(Index: integer; ARect: TRect; Canvas: TCanvas);
begin
  if Assigned(OnBeforeDrawItem) then
    OnBeforeDrawItem(Self, Index, ARect, Canvas);

  if FDefaultDraw then
    with Canvas do begin
      Brush.Color := GetItemBackgroundColor(Index);

      Fillrect(ARect);
      //DrawTextRect(Canvas, ARect, Index.ToString, [], 10);
    end;

  if Assigned(OnDrawItem) then
    OnDrawItem(Self, Index, ARect, Canvas);
end;

procedure FXDrawList.DrawNoItemsText;
begin
  Buffer.Font.Assign( Self.Font );
  Buffer.Font.Color := FDrawColors.ForeGround;
  Buffer.Brush.Style := bsClear;
  DrawTextRect(Buffer, DrawRect, FNoItemsOutputText, [FXTextFlag.WordWrap, FXTextFlag.Center, FXTextFlag.VerticalCenter])
end;

procedure FXDrawList.EnsureIndexVisible;
begin
  if ItemIndex = -1 then
    Exit;

  // Stop
  StopScrollAnimations;

  const Bounds = ItemDisplayRect[ItemIndex];
  const Client = GetClientRect;

  if Bounds.Top < Client.Top then
    FVertScroll.Position := FVertScroll.Position + (Bounds.Top - Client.Top);
  if Bounds.Left < Client.Left then
    FVertScroll.Position := FVertScroll.Position + (Bounds.Left - Client.Left);

  if Bounds.Bottom > Client.Bottom then
    FVertScroll.Position := FVertScroll.Position + (Bounds.Bottom - Client.Bottom);
  if Bounds.Right > Client.Right then
    FHorzScroll.Position := FHorzScroll.Position + (Bounds.Right - Client.Right);
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
  ARect, Client: TRect;
begin
  ARect := GetItemDisplayRect(Index);

  // Return if element is visible
  Client := GetClientRect;
  Result := ARect.IntersectsWith( Client );
end;

function FXDrawList.GetItemBackgroundColor(Index: integer): TColor;
var
  Interior: TColor;
begin
  case FBackgroundItems of
    FXBackgroundColor.Background: Interior := FDrawColors.BackGround;
    FXBackgroundColor.Content: Interior := FDrawColors.BackGroundInterior;
    else Interior := 0;
  end;

  if FItemSelected[Index] then
    Result := ColorBlend(Interior, FDrawColors.Accent, OpacitySelected)
  else
  if ItemIndexHover = Index then
    Result := ColorBlend(Interior, FDrawColors.Accent, OpacityHover)
  else
    Result := Interior;
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

function FXDrawList.GetItemVisible(Index: integer): boolean;
begin
  Result := FItemVisible[Index];
end;

function FXDrawList.GetSelectedItems: TArray<integer>;
begin
  Result := [];
  for var I := 0 to High(FItemSelected) do
    if FItemSelected[I] then
      TArrayUtils<integer>.AddValue(I, Result);
end;

procedure FXDrawList.HandleKeyDown(var CanHandle: boolean; Key: integer;
  Shift: TShiftState);
var
  Value, Next: integer;
function GetNextVisible(From, Direction: integer): integer;
begin
  Result := From;
  repeat
    Inc(Result, Direction);

    if not InRange(Result, 0, ItemCount-1) then
      Exit(From);

  until FItemVisible[Result];
end;
begin
  inherited;
  if FKeyboardNavigation then
    case Key of
      VK_LEFT, VK_UP: begin
        CanHandle := false;

        // Next
        Next := GetNextVisible(FItemIndex, - 1);
        if Next = FItemIndex then
          Exit;

        // Sel mode
        if (ssShift in Shift) and MultiSelect then begin
          Value := Next;
          if Value < 0 then
            Exit;

          if FItemSelected[Value] then
            ItemSelected[FItemIndex] := false
          else
            ItemSelected[Value] := true;
          FItemIndex := Value;

          EnsureIndexVisible;
        end else
          // Item mode
          if ItemIndex > 0 then
            ItemIndex := Next
      end;

      VK_RIGHT, VK_DOWN: begin
        CanHandle := false;

        // Next
        Next := GetNextVisible(FItemIndex, 1);
        if Next = FItemIndex then
          Exit;

        // Sel mode
        if (ssShift in Shift) and MultiSelect then begin
          Value := Next;
          if Value >= ItemCount then
            Exit;

          if FItemSelected[Value] then
            ItemSelected[FItemIndex] := false
          else
            ItemSelected[Value] := true;
          FItemIndex := Value;

          EnsureIndexVisible;
        end else
          // Item mode
          ItemIndex := Next;
      end;

      VK_ESCAPE: if CanDeselect then ClearSelection;

      Ord('A'): if (ssCtrl in Shift) and MultiSelect then
        SelectAll;
    end
end;

procedure FXDrawList.InteractionStateChanged(AState: FXControlState);
begin
  if AState = FXControlState.None then
    if FItemIndexHover <> -1 then
      FItemIndexHover := -1;

  inherited;
end;

function FXDrawList.IsContainer: Boolean;
begin
  Result := false;
end;

procedure FXDrawList.ItemVisibilityChanged;
begin
  //
end;

procedure FXDrawList.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: integer);
begin
  inherited;

  // Select item
  if (ItemIndexHover <> -1) or FCanDeselect then begin
    const LastIndex = ItemIndex;

    // Set new
    if (ssCtrl in Shift) and MultiSelect and (ItemIndexHover <> -1) then begin
      FItemIndex := ItemIndexHover;

      // Set selection
      ItemSelected[ItemIndexHover] := not ItemSelected[ItemIndexHover];

      // Notify
      if Assigned(FOnItemSelect) then
        FOnItemSelect(Self);
    end
    else
    if (ssShift in Shift) and MultiSelect and (ItemIndexHover <> -1) and (ItemIndex <> -1) then begin
      const AMin = Min(ItemIndex, ItemIndexHover);
      const AMax = Max(ItemIndex, ItemIndexHover);
      if AMin = AMax then
        Exit;

      // Set index to...
      FItemIndex := ItemIndexHover;

      // Set all selected in-between
      for var I := AMin to AMax do
        if ItemVisible[I] then
          FItemSelected[I] := true;

      // Draw
      StandardUpdateDraw;

      // Notify
      if Assigned(FOnItemSelect) then
        FOnItemSelect(Self);
    end
    else begin
      ItemIndex := ItemIndexHover;

      // Changed
      if LastIndex <> ItemIndex then
        if Assigned(FOnItemSelect) then
          FOnItemSelect(Self);
    end;
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
    if FItemVisible[FVisibleList[I].Index]
      and FVisibleList[I].Rectangle.Contains(Cursor) then begin
      // Is the same value
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
  case FBackground of
    FXBackgroundColor.Background: Color := FDrawColors.BackGround;
    FXBackgroundColor.Content: Color := FDrawColors.BackGroundInterior;
  end;
  PaintBackground;

  // Draw
  FVisibleList := [];
  for I := 0 to High(FItemRects) do begin
    if not FItemVisible[I] then
      Continue;

    // Data
    const InBounds = GetInBounds(I);
    const Display = GetItemDisplayRect(I);

    // In Bounds
    if InBounds then
      with FVisibleList[TArrayUtils<TRectSet>.AddValue(FVisibleList)] do begin
      Index := I;
      Rectangle := Display;
    end;

    // Draw out of bound?
    if (not FNoOutOfBoundsDraw or InBounds) then
      DrawItem(I, Display, Buffer);

    // Event notifier
    if Assigned(OnAfterDrawItem) then
      OnAfterDrawItem(Self, I, Display, Canvas);
  end;

  // No items
  if (Length(FItemRects) = 0) and (FNoItemsOutputText <> '') then
    DrawNoItemsText;

  // Inherit
  inherited;
end;

procedure FXDrawList.ResetView;
begin
  // Anims
  StopScrollAnimations;

  // Set scroll
  FVertScroll.Position := 0;
  FHorzScroll.Position := 0;

  // updated by scrollbars
end;

procedure FXDrawList.Resize;
begin
  inherited;
  UpdateScrollbars;
end;

procedure FXDrawList.CalculateScroll;
var
  MaxX, MaxY: integer;
  Client: TRect;
begin
  Client := inherited GetClientRect; // inherited to not include scrollbar width
  MaxX := 0;
  MaxY := 0;

  for var I := 0 to High(FItemRects) do
    if FItemVisible[I] then begin
      MaxX := Max( MaxX, FItemRects[I].Right);
      MaxY := Max( MaxY, FItemRects[I].Bottom);
    end;

  // Set maxes
  MaxX := Max(0, MaxX-Client.Width);
  if (MaxX > 0) or (FHorzScroll.Position > 0) then
    FHorzScroll.Max := MaxX + FExtendX;

  MaxY := Max(0, MaxY-Client.Height);
  if (MaxY > 0) or (FVertScroll.Position > 0) then
    FVertScroll.Max := MaxY + FExtendY;
end;

procedure FXDrawList.ScrollChanged(Sender: TObject);
begin
  // STOP scroll animations
  StopScrollAnimations;
end;

procedure FXDrawList.ScrollChangedValue(Sender: TObject);
begin
  // Draw
  StandardUpdateDraw;
end;

procedure FXDrawList.UpdateColors;
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

procedure FXDrawList.UpdateRects;
begin
  // Rect
  DrawRect := GetClientRect;

  // Scroll;
  CalculateScroll;
  UpdateScrollbars;
end;

procedure FXDrawList.UpdateScrollbars;
begin
  if IsReading then
    Exit;

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

procedure FXDrawList.SelectAll;
begin
  // Clear selected
  ClearSelectedInternal; // does not invalidate

  for var I := 0 to ItemCount-1 do
    if FItemVisible[I] then // select all VISIBLE
      FItemSelected[I] := true;

  // Set
  FItemIndex := ItemCount-1;

  // EnsureIndexVisible; // not necesarry

  // Draw
  StandardUpdateDraw;
end;

function FXDrawList.SelectedItemCount: integer;
begin
  Result := 0;
  for var I := 0 to High(FItemSelected) do
    if FItemSelected[I] then
      Inc(Result);
end;

procedure FXDrawList.SetBackground(const Value: FXBackgroundColor);
begin
  if FBackground = Value then
    Exit;

  FBackground := Value;
  Redraw;
end;

procedure FXDrawList.SetBackgroundItems(const Value: FXBackgroundColor);
begin
  if FBackgroundItems = Value then
    Exit;

  FBackgroundItems := Value;
  Redraw;
end;

procedure FXDrawList.SetDefaultDraw(const Value: boolean);
begin
  if FDefaultDraw = Value then
    Exit;

  FDefaultDraw := Value;
  StandardUpdateDraw;
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

  // Selected
  if FItemIndex <> -1 then begin
    FItemIndex := -1;
    ClearSelectedInternal;

    // Update select procedure
    if Assigned(FOnItemSelect) then
      FOnItemSelect(Self);
  end;

  // Hover
  if FItemIndexHover <> -1 then  begin
    FItemIndexHover := -1;

    // Update hober procedure
    if Assigned(FOnItemHover) then
      FOnItemHover(Self);
  end;

  SetLength(FItemRects, Value);
  SetLength(FItemSelected, Value);
  SetLength(FItemVisible, Value);

  ClearHiddenInternal;

  // Draw
  StandardUpdateLayout;
end;

procedure FXDrawList.SetItemIndex(const Value: integer);
begin
  if (FItemIndex = Value) or not InRange(Value, -1, ItemCount-1) then
    Exit;

  // Clear selected
  ClearSelectedInternal; // does not invalidate
  if Value <> -1 then
    FItemSelected[Value] := true;

  // Set
  FItemIndex := Value;

  EnsureIndexVisible;

  // Draw
  StandardUpdateDraw;
end;

procedure FXDrawList.SetItemIndexHover(const Value: integer);
begin
  if FItemIndexHover = Value then
    Exit;

  FItemIndexHover := Value;
  StandardUpdateDraw;
end;

procedure FXDrawList.SetItemSelected(Index: integer; const Value: boolean);
begin
  FItemSelected[Index] := Value;

  StandardUpdateDraw;
end;

procedure FXDrawList.SetItemVisible(Index: integer; const Value: boolean);
begin
  if FItemVisible[Index] = Value then
    Exit;

  SetItemVisibleEx(Index, Value);

  // Changed
  ItemVisibilityChanged;
end;

procedure FXDrawList.SetItemVisibleEx(Index: integer; const Value: boolean);
begin
  // Set
  FItemVisible[Index] := Value;

  // Remove
  if not Value then begin
    if FItemSelected[Index] then
      FItemSelected[Index] := false;
    if ItemIndexHover = Index then
      ItemIndexHover := -1;
    if ItemIndex = Index then
      ItemIndex := -1;
  end;
end;

procedure FXDrawList.SetNoItemsOutputText(const Value: string);
begin
  if FNoItemsOutputText = Value then
    Exit;

  FNoItemsOutputText := Value;
  if ItemCount = 0 then
    StandardUpdateDraw;
end;

procedure FXDrawList.SetNoOutOfBoundsDraw(const Value: boolean);
begin
  if (FNoOutOfBoundsDraw = Value) then
    Exit;

  FNoOutOfBoundsDraw := Value;
end;

procedure FXDrawList.SetOpacityHover(const Value: byte);
begin
  if FOpacityHover = Value then
    Exit;

  FOpacityHover := Value;
  StandardUpdateDraw;
end;

procedure FXDrawList.SetOpacitySelected(const Value: byte);
begin
  if FOpacitySelected = Value then
    Exit;

  FOpacitySelected := Value;
  StandardUpdateDraw;
end;

procedure FXDrawList.SetShowScrollbars(const Value: boolean);
begin
  if FShowScrollbars = Value then
    Exit;

  FShowScrollbars := Value;
  UpdateRects; // also scrollbars are updated
  StandardUpdateLayout;
end;

procedure FXDrawList.SetVisibility(Items: TArray<integer>; Visible: boolean; Clear: boolean);
begin
  // Clear
  if Clear then
    ClearHiddenInternal;

  // Set
  for var I := 0 to High(Items) do
    SetItemVisibleEx( Items[I], Visible );

  // Changed
  ItemVisibilityChanged;
end;

procedure FXDrawList.StopScrollAnimations;
begin
  FAnimX.Stop;
  FAnimY.Stop;
end;

{ FXDrawListScrollBar }

procedure FXDrawListScrollBar.PaintBuffer;
begin
  inherited;
end;

{ FXCustomLinearDrawList }

constructor FXCustomLinearDrawList.Create(aOwner: TComponent);
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

procedure FXCustomLinearDrawList.ItemVisibilityChanged;
begin
  inherited;

  // Update
  StandardUpdateLayout;
end;

procedure FXCustomLinearDrawList.RecalculateRects;
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
      OffsetStart := LeftoverSpace div (ItemsFit+1);
      OffsetSpacing := OffsetStart;
    end;
    FXContentJustify.Ending: OffsetStart := LeftoverSpace;
    FXContentJustify.Stretch: OffsetSize := LeftoverSpace div ItemsFit;
  end;

  // Size changed
  if OffsetSize <> 0 then begin
    case Orientation of
      FXOrientation.Horizontal: Inc(FActualSize.Y, OffsetSize);
      FXOrientation.Vertical: Inc(FActualSize.X, OffsetSize);
    end;
  end;

  // Start
  Pos := Client.TopLeft;
  case Orientation of
    FXOrientation.Horizontal: Inc(Pos.Y, OffsetStart);
    FXOrientation.Vertical: Inc(Pos.X, OffsetStart);
  end;

  // Calculate all rectanges
  for var I := 0 to High(FItemRects) do
    if ItemVisible[I] then
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

procedure FXCustomLinearDrawList.ScaleChanged(Scaler: single);
begin
  FItemWidth := round(FItemWidth * Scaler);
  FItemHeight := round(FItemHeight * Scaler);
  FSpacingRow := round(FSpacingRow * Scaler);
  FSpacingColumn := round(FSpacingColumn * Scaler);
end;

procedure FXCustomLinearDrawList.SetFullLine(const Value: boolean);
begin
  if FFullLine = Value then
    Exit;

  FFullLine := Value;

  StandardUpdateLayout;
end;

procedure FXCustomLinearDrawList.SetItemHeight(const Value: integer);
begin
  if (FItemHeight = Value) or (Value <= 0) then
    Exit;

  FVertScroll.SmallChange := Value;
  FItemHeight := Value;

  StandardUpdateLayout;
end;

procedure FXCustomLinearDrawList.SetItemWidth(const Value: integer);
begin
  if (FItemWidth = Value) or (Value <= 0) then
    Exit;

  FItemWidth := Value;
  StandardUpdateLayout;
end;

procedure FXCustomLinearDrawList.SetJustifyContent(const Value: FXContentJustify);
begin
  if FJustifyContent = Value then
    Exit;

  FJustifyContent := Value;
  StandardUpdateLayout;
end;

procedure FXCustomLinearDrawList.SetOrientation(const Value: FXOrientation);
begin
  if FOrientation = Value then
    Exit;

  FOrientation := Value;
  StandardUpdateLayout;
end;

procedure FXCustomLinearDrawList.SetSpacingColumn(const Value: integer);
begin
  if FSpacingColumn = Value then
    Exit;

  FSpacingColumn := Value;
  StandardUpdateLayout;
end;

procedure FXCustomLinearDrawList.SetSpacingRow(const Value: integer);
begin
  if FSpacingRow = Value then
    Exit;

  FSpacingRow := Value;
  StandardUpdateLayout;
end;

procedure FXCustomLinearDrawList.SetWrap(const Value: boolean);
begin
  if FWrap = Value then
    Exit;

  FWrap := Value;
  StandardUpdateLayout;
end;

procedure FXCustomLinearDrawList.UpdateRects;
begin
  FActualSize := Point(FItemWidth, FItemHeight);

  RecalculateRects; // calc rects

  FHorzScroll.SmallChange := ItemWidth + SpacingRow;
  FVertScroll.SmallChange := ItemHeight + SpacingColumn;

  inherited;
end;

{ FXLinearControlList }

procedure FXLinearControlList.AddControlToItem(AControl: TControl);
begin
  AControl.Parent := FContainer;

  // Draw
  Redraw;
end;

procedure FXLinearControlList.CMControlListChange(
  var Msg: TCMControlListChange);
begin
  if Msg.Inserting and (Msg.Control is FXWindowsControl)
    and not IsReading then begin

    // Set parent
    if Msg.Control.Parent <> FContainer then
      Msg.Control.Parent := FContainer;
  end;
end;

procedure FXLinearControlList.ComponentCreated;
begin
  inherited;
  FContainer.AllocateHandles; // allocate handle for container
end;

constructor FXLinearControlList.Create(aOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csCaptureMouse, csOpaque, csDoubleClicks,
    csNeedsBorderPaint, csPannable, csGestures];

  FContainer := FXControlContainer.CreateInList(Self);
  FContainer.Parent := Self;
  FContainer.Visible := IsDesigning;
end;

destructor FXLinearControlList.Destroy;
begin
  //FreeAndNil( FContainer );
  inherited;
end;

procedure FXLinearControlList.DrawItem(Index: integer; ARect: TRect;
  Canvas: TCanvas);
begin
  // Set the background before drawing occurs
  FContainer.FBackgroundColor := GetItemBackgroundColor(Index);
  FContainer.Redraw;

  // Default draw
  inherited;

  // Quit
  if IsDesigning then begin
    if (Index > 0) then
      Exit;
  end;

  // Overlay
  if IsDesigning then
    with Canvas do begin
      Brush.Color := FDrawColors.BackGroundInterior;
      Brush.Color := ColorBlend(Brush.Color, FDrawColors.Accent, 100);

      Fillrect(ARect);

      FContainer.Top := ARect.Top;
      FContainer.Left := ARect.Left;
    end;

  // Initialize Container
  FContainer.Width := ARect.Width;
  FContainer.Height := ARect.Height;

  // Draw controls
  FContainer.DrawTo(Buffer, ARect);
end;

procedure FXLinearControlList.DrawNoItemsText;
begin
  if not IsDesigning then
    inherited;
end;

function FXLinearControlList.GetChildParent: TComponent;
begin
  Result := FContainer;
end;

procedure FXLinearControlList.GetChildren(Proc: TGetChildProc;
  Root: TComponent);
begin
  FContainer.GetChildren(Proc, Root);
end;

function FXLinearControlList.GetItemInnerMarginsFill: FXMargins;
begin
  Result := FContainer.InnerMarginsFill;
end;

function FXLinearControlList.GetItemPaddding: FXMargins;
begin
  Result := FContainer.PaddingFill;
end;

function FXLinearControlList.IsContainer: Boolean;
begin
  Result := true;
end;

procedure FXLinearControlList.PaintBuffer;
begin
  inherited;

  if IsDesigning and (ItemCount = 0) then begin
    FContainer.FBackgroundColor := ColorBlend(FDrawColors.BackGroundInterior, FDrawColors.Accent, 100);

    FActualSize.X := FItemWidth;
    FActualSize.Y := FItemHeight;

    case Orientation of
      FXOrientation.Horizontal: if FullLine then
        FActualSize.Y := GetClientRect.Height;
      FXOrientation.Vertical: if FullLine then
        FActualSize.X := GetClientRect.Width;
    end;

    // Set
    FContainer.Width := FActualSize.X;
    FContainer.Height := FActualSize.Y;
  end;
end;

procedure FXLinearControlList.SetItemInnerMarginsFill(const Value: FXMargins);
begin
  FContainer.InnerMarginsFill.Assign(Value);
end;

procedure FXLinearControlList.SetItemPaddding(const Value: FXMargins);
begin
  FContainer.PaddingFill.Assign(Value);
end;

procedure FXLinearControlList.UpdateRects;
begin
  inherited;

  if FContainer = nil then
    Exit;

  FContainer.Width := FActualSize.X;
  FContainer.Height := FActualSize.Y;
end;

{ FXControlContainer }

function FXControlContainer.Background: TColor;
begin
  Result := FBackgroundColor;
end;

procedure FXControlContainer.CMControlListChange(var Msg: TCMControlListChange);
begin
  if Msg.Inserting and not (Msg.Control is FXWindowsControl) then begin
    raise Exception.Create('Only CFX Base control are accepted');

    Msg.Control.Parent := Self;
  end;
end;

constructor FXControlContainer.CreateInList(AList: FXDrawList);
begin
  inherited Create(AList);

  FList := AList;

  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csDoubleClicks, csOpaque, csReplicatable, csGestures];

  Parent := AList;
end;

destructor FXControlContainer.Destroy;
begin

  inherited;
end;

function FXControlContainer.IsContainer: Boolean;
begin
  Result := true;
end;

procedure FXControlContainer.PaintBuffer;
begin
  inherited;
  with Buffer do begin
    Brush.Color := FBackgroundColor;
    FillRect( ClipRect );
  end;
end;

{ FXLinearStringsList }

constructor FXLinearStringsList.Create(aOwner: TComponent);
begin
  inherited;
  FStrings := TStringList.Create;
  FullLine := true;
  ItemHeight := 40;

  FStrings.OnChange := StringsChanged;

  FItemMargins := FXMargins.Create(Self);
  FItemMargins.OnChange := MarginsChanged;

  // Count
  UpdateCount;
end;

destructor FXLinearStringsList.Destroy;
begin
  FreeAndNil(FStrings);
  FreeAndNil(FItemMargins);
  inherited;
end;

procedure FXLinearStringsList.DrawItem(Index: integer; ARect: TRect;
  Canvas: TCanvas);
var
  S: string;
begin
  inherited;

  with Canvas do begin
    Font.Assign( Self.Font );
    Font.Color := FDrawColors.ForeGround;

    // Margins
    ARect:= FItemMargins.RectangleInflate(ARect);

    // Text
    S := Items[Index];

    // Draw
    TextRect(ARect, S, [tfSingleLine, tfVerticalCenter]);
  end;
end;

function FXLinearStringsList.GetItemCountEx: integer;
begin
  Result := inherited ItemCount;
end;

procedure FXLinearStringsList.MarginsChanged(Sender: TObject);
begin
  StandardUpdateDraw;
end;

procedure FXLinearStringsList.SetStrings(const Value: TStringList);
begin
  FStrings.Assign( Value );

  UpdateCount;
end;

procedure FXLinearStringsList.StringsChanged(Sender: TObject);
begin
  UpdateCount;
end;

procedure FXLinearStringsList.UpdateCount;
begin
  inherited ItemCount := Items.Count;
end;

end.
