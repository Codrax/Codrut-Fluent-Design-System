unit CFX.TabStrip;

interface

uses
  Classes,
  Messages,
  Windows,
  System.Generics.Defaults,
  System.Generics.Collections,
  Math,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.ExtCtrls,
  Types,
  CFX.Translations,
  CFX.Colors,
  CFX.ThemeManager,
  CFX.Graphics,
  CFX.Utilities,
  CFX.Constants,
  CFX.PopupMenu,
  SysUtils,
  CFX.Animation.Main,
  CFX.Button,
  CFX.Animation.Component,
  CFX.Classes,
  CFX.GDI,
  CFX.Types,
  CFX.VarHelpers,
  CFX.Linker,
  CFX.ComponentClasses,
  CFX.Components,
  CFX.Controls;

type
  FXStripTabs = class;

  FXStripTab = class(TComponent)
  private
    FParent: FXStripTabs;
    FText: string;
    FSize: integer;
    FImage: FXIconSelect;
    FTabIndex: integer;

    // Parent
    procedure StandardUpdateLayout;

    // Set
    procedure SetImage(const Value: FXIconSelect);
    procedure SetTabIndex(Value: integer);
    procedure SetText(const Value: string);
    procedure SetSize(const Value: integer);

  public
    property Parent: FXStripTabs read FParent;

    property TabIndex: integer read FTabIndex write SetTabIndex;

    property Text: string read FText write SetText;
    property Size: integer read FSize write SetSize;
    property Image: FXIconSelect read FImage write SetImage;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;
  FXStripTabs = class(TComponent)
  private
    FItems: TList<FXStripTab>;

    // Parent
    procedure StandardUpdateLayout;
    procedure DoUpdateItemDeleted;

    // Getters
    function GetItem(Index: Integer): FXStripTab;
    function GetCount: integer;

  public
      function Add: FXStripTab; overload;
    procedure Add(const Item: FXStripTab); overload;
    procedure Insert(Index: Integer; const Item: FXStripTab);
    procedure Delete(Index: Integer);
      procedure Remove(const Item: FXStripTab);
    procedure Clear;

    property Items[Index: Integer]: FXStripTab read GetItem; default;
    property Count: integer read GetCount;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  FXTabStrip = class(FXWindowsControl)
  private
    var DrawRect, DrawRectTabs, DrawRectButtonLeft, DrawRectButtonRight, DrawRectButtonPlus: TRect;
    FStaticButtonHoverIndex: integer;
    FCustomColors: FXColorSets;
    FCustomTabColors: FXColorStateSets;
    FCustomButtonColors: FXColorStateSets;
    FDrawColors: FXCompleteColorSet;
    FTabColors: FXColorStateSet;
    FButtonColors: FXColorStateSet;

    FDrawImageSize: integer;
    FIsScrollingRequired: boolean;
    FShowPlusButton: boolean;
    FShowCloseButton: boolean;
    FEnableReordering: boolean;
    FScrollMax: integer;
    FScrollPos: integer;
    FHoverIsOnCloseButton: boolean;

    FAutoRepeat: TTimer;
    FAnimScroll: FXIntAnim;

    FOnChange: TNotifyEvent;
    FOnChangeValue: TNotifyEvent;
    FOnPlusClicked: TNotifyEvent;
    FOnClosePressed: FXControlOnIntegerValue;
    FOnTabMove: FXControlOnIntegerSourceDestination;

    FOrientation: FXOrientation;
    FRoundTabItems: boolean;
    FDrawIsolationLine: boolean;

    FImageScale: single;

    FDefaultMenu: FXPopupMenu;

    FHandleScrolling: boolean;
    FScrollAnimation: boolean;

    FValue: integer;
    FValueHover: integer;
    FItems: FXStripTabs;
    FItemRects: TArray<TRect>;

    // Menu
    procedure PrepDefaultMenu;
    procedure PopupBeforePopup(Sender: TObject; var CanPopup: boolean; Point: TPoint);
    procedure PopupItemClick(Sender: TObject; Item: FXPopupComponent; Index: integer);

    // Timers
    procedure TimerRepeat(Sender: TObject);

    // Getters

    // Setters
    procedure SetOrientation(const Value: FXOrientation);
    procedure SetValue(const Value: integer);
    procedure SetRoundTabItems(const Value: boolean);
    procedure SetImageScale(const Value: single);
    procedure SetDrawIsolationLine(const Value: boolean);
    procedure SetShowPlusButton(const Value: boolean);
    procedure SetScrollPos(Value: integer);

  protected
    procedure PaintBuffer; override;

    // Inherited
    procedure OpenPopupMenu(X, Y: integer); override;

    // Animation
    procedure AnimationStep(Sender: TObject; Step, TotalSteps: integer);

    // Draw
    function GetTextH: integer;

    //  Internal
    procedure UpdateColors; override;
    procedure UpdateRects; override;

    // Scaler
    procedure ScaleChanged(Scaler: single); override;

    // State
    procedure InteractionStateChanged(AState: FXControlState); override;

    // Draw
    function GetCloseButtonRectFromTabRect(TabRect: TRect): TRect;

    // Animate
    procedure DoAddScroll(ScrollAmount: integer);

    // System events
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;

    // Inherited Mouse Detection
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;

  published
    // Custom Colors
    property CustomColors: FXColorSets read FCustomColors write FCustomColors stored true;
    property CustomTabColors: FXColorStateSets read FCustomTabColors write FCustomTabColors stored true;
    property CustomButtonColors: FXColorStateSets read FCustomButtonColors write FCustomButtonColors stored true;

    // Events
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChangeValue: TNotifyEvent read FOnChangeValue write FOnChangeValue;

    property OnPlusClicked: TNotifyEvent read FOnPlusClicked write FOnPlusClicked;
    property OnClosePressed: FXControlOnIntegerValue read FOnClosePressed write FOnClosePressed;
    property OnTabMove: FXControlOnIntegerSourceDestination read FOnTabMove write FOnTabMove;

    // Props
    property ImageScale: single read FImageScale write SetImageScale;
    property Orientation: FXOrientation read FOrientation write SetOrientation default FXOrientation.Horizontal;

    property ScrollPosition: integer read FScrollPos write SetScrollPos;

    property RoundTabItems: boolean read FRoundTabItems write SetRoundTabItems default true;
    property DrawIsolationLine: boolean read FDrawIsolationLine write SetDrawIsolationLine default true;
    property ShowPlusButton: boolean read FShowPlusButton write SetShowPlusButton default true;
    property ShowCloseButton: boolean read FShowCloseButton write FShowCloseButton default true;
    property EnableReordering: boolean read FEnableReordering write FEnableReordering default true;

    property HandleScrolling: boolean read FHandleScrolling write FHandleScrolling default true;
    (* Scroll smoothly *)
    property ScrollAnimation: boolean read FScrollAnimation write FScrollAnimation default true;

    property Value: integer read FValue write SetValue;
    property ValueHover: integer read FValueHover;
    property Tabs: FXStripTabs read FItems;

    // Default props
    property Align;
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
    property OnDblClick;
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
    function Background: TColor; override;
  end;

implementation

procedure FXTabStrip.AnimationStep(Sender: TObject; Step, TotalSteps: integer);
begin
  FScrollPos := FXIntAnim(Sender).CurrentValue;

  // Draw
  StandardUpdateLayout;
end;

function FXTabStrip.Background: TColor;
begin
  Result := FDrawColors.BackGround;
end;

constructor FXTabStrip.Create(aOwner: TComponent);
begin
  inherited;
  // Custom Color
  FCustomColors := FXCompleteColorSets.Create(Self);
  FCustomTabColors := FXColorStateSets.Create(Self);
  FCustomButtonColors := FXColorStateSets.Create(Self);

  FDrawColors := FXCompleteColorSet.Create;
  FTabColors := FXColorStateSet.Create;
  FButtonColors := FXColorStateSet.Create;

  Font.Height := ThemeManager.FormFontHeight-2;

  FStaticButtonHoverIndex := -1;

  // Menu
  FDefaultMenu := FXPopupMenu.Create(Self);
  FDefaultMenu.OnItemClick := PopupItemClick;
  FDefaultMenu.OnBeforePopup := PopupBeforePopup;
  PrepDefaultMenu;

  // Repeat
  FAutoRepeat := TTimer.Create(nil);
  with FAutoRepeat do
    begin
      Enabled := false;
      OnTimer := TimerRepeat;
    end;

  // Items
  FItems := FXStripTabs.Create(Self);

  FValue := -1;
  FOrientation := FXOrientation.Horizontal;

  FRoundTabItems := true;
  FDrawIsolationLine := true;
  FImageScale := TABSTRIP_TAB_IMAGE_SCALE;

  FShowCloseButton := true;
  FShowPlusButton := true;
  FEnableReordering := true;

  FScrollAnimation := true;
  FHandleScrolling := true;

  // Anim
  FAnimScroll := FXIntAnim.Create(nil);
  with FAnimScroll do begin
    Kind := FXAnimationKind.ReverseExpo;
    Duration := SCROLL_DURATION;

    LatencyAdjustments := true;
    LatencyCanSkipSteps := true;

    OnStep := AnimationStep;
  end;

  // Sizing
  Height := 40;
  Width := 300;
end;

destructor FXTabStrip.Destroy;
begin
  FreeAndNil( FCustomColors );
  FreeAndNil( FCustomTabColors );
  FreeAndNil( FCustomButtonColors );
  FreeAndNil( FDrawColors );
  FreeAndNil( FTabColors );
  FreeAndNil( FButtonColors );

  FreeAndNil( FItems );

  FreeAndNil( FAnimScroll );
  FreeAndNil( FAutoRepeat );

  inherited;
end;

procedure FXTabStrip.DoAddScroll(ScrollAmount: integer);
var
  NewEndValue: integer;
begin
  if not FScrollAnimation and not IsDesigning then begin
    FScrollPos := EnsureRange(FScrollPos+ScrollAmount, 0, FScrollMax);
    Exit;
  end;

  // Horizontal
  FAnimScroll.StartValue := FScrollPos;

  if FAnimScroll.Running then
    NewEndValue := FAnimScroll.EndValue + ScrollAmount
  else
    NewEndValue := FScrollPos+ScrollAmount;
  NewEndValue := EnsureRange(NewEndValue, 0, FScrollMax);

  // Same end value, abort
  if NewEndValue = FAnimScroll.EndValue then
    Exit;

  // Start animate
  FAnimScroll.EndValue := NewEndValue;
  FAnimScroll.Stop;

  if FAnimScroll.StartValue <> FAnimScroll.EndValue then
    FAnimScroll.Start;
end;

function FXTabStrip.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result := false;

  if FHandleScrolling and FIsScrollingRequired and not (ssCtrl in Shift) then begin
    const ScrollAmount = GetScrollAmount(WheelDelta, ClientRect.Height);

    Result := true; // handled

    DoAddScroll(ScrollAmount);
  end;

  if not Result then
    Result := inherited;
end;

function FXTabStrip.GetCloseButtonRectFromTabRect(TabRect: TRect): TRect;
begin
  Result := TabRect;
  case Orientation of
    FXOrientation.Horizontal: begin
      Result.Left := Result.Right-TABSTRIP_BUTTON_SIZE - 2;
      Result.Inflate(-2, -4);
    end;
    FXOrientation.Vertical: begin
      Result.Top := Result.Bottom-TABSTRIP_BUTTON_SIZE - 2;
      Result.Inflate(-4, -2);
    end;
  end;
end;

function FXTabStrip.GetTextH: integer;
begin
  with Buffer do
    begin
      Font.Assign(Self.Font);

      Result := TextHeight(TEXT_SIZE_COMPARER)
    end;
end;

procedure FXTabStrip.InteractionStateChanged(AState: FXControlState);
begin
  inherited;
end;

procedure FXTabStrip.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: integer);
begin
  inherited;
  // Scroll buttons
  if InRange(FStaticButtonHoverIndex, 0, 1) then begin
    FAutoRepeat.Interval := REPEAT_START_DELAY;
    FAutoRepeat.Enabled := true;

    var AddTime := 100;
    if FStaticButtonHoverIndex = 0 then
      AddTime := AddTime * -1;

    DoAddScroll(AddTime);

    // Enable
    FAutoRepeat.Tag := AddTime;
  end;
end;

procedure FXTabStrip.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
  APoint: TPoint;
begin
  inherited;
  APoint := Point(X, Y);

  // Hover
  const LastItem = FValueHover;
  const LastHoverOnButton = FHoverIsOnCloseButton;
  FValueHover := -1;
  FHoverIsOnCloseButton := false;
  if DrawRectTabs.Contains(APoint) then
    for I := 0 to High(FItemRects) do
      if FItemRects[I].Contains(APoint) then begin
        FValueHover := I;
        FHoverIsOnCloseButton := ShowCloseButton and GetCloseButtonRectFromTabRect(FItemRects[I]).Contains(APoint);
        break;
      end;

  // Button
  const LastStaticButton = FStaticButtonHoverIndex;
  FStaticButtonHoverIndex := -1;
  if FIsScrollingRequired then begin
    if (FStaticButtonHoverIndex = -1) and DrawRectButtonLeft.Contains(APoint) then
      FStaticButtonHoverIndex := 0;
    if (FStaticButtonHoverIndex = -1) and DrawRectButtonRight.Contains(APoint) then
      FStaticButtonHoverIndex := 1;
  end;
  if ShowPlusButton then begin
    if (FStaticButtonHoverIndex = -1) and DrawRectButtonPlus.Contains(APoint) then
      FStaticButtonHoverIndex := 2;
  end;

  // Changed
  if (LastItem <> FValueHover) or (LastHoverOnButton <> FHoverIsOnCloseButton)
    or (FStaticButtonHoverIndex <> LastStaticButton) then
    Redraw;
end;

procedure FXTabStrip.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: integer);
begin
  inherited;

  // General
  FAutoRepeat.Enabled := false;

  // Tab
  if (FValueHover <> -1) then begin
    // Close
    if FHoverIsOnCloseButton then begin
      if Assigned(FOnClosePressed) then
        FOnClosePressed(Self, FValueHover);
    end else
    // Select tab
    if FValueHover <> Value then begin
      Value := FValueHover;

      // Notify
      if Assigned(FOnChange) then
        FOnChange(Self);

      Redraw;
    end;
  end else begin
    // Buttons
    case FStaticButtonHoverIndex of
      2: if Assigned(OnPlusClicked) then OnPlusClicked(Self);
    end;
  end;
end;

procedure FXTabStrip.OpenPopupMenu(X, Y: integer);
begin
  if not Assigned(PopupMenu) then
    FDefaultMenu.PopupAtPoint(ClientToScreen(Point(X, Y)))
  else
    inherited;
end;

procedure FXTabStrip.PaintBuffer;
  procedure DoDrawTab(DrawRect: TRect; Text: string; MouseOnCloseButton: boolean; Image: FXIconSelect; TabInteractionState: FXControlState; Selected: boolean);
  var
    FBackground, FForeground: TColor;
    ARect: TRect;
    AWidth: integer;
    FPen: TGDIPen;
  begin
    // Out of bounds
    if not Self.DrawRectTabs.IntersectsWith(DrawRect) then
      Exit;

    with Buffer do begin
      // Checked (get color from FDrawColors)
      if Selected then begin
        FBackground := FDrawColors.BackGroundInterior;
        FForeground := FDrawColors.ForeGround;
      end
      // Not checked (get color from FTabColors)
        else
      begin
        var ReadState := TabInteractionState;
        if MouseOnCloseButton then
          ReadState := FXControlState.Hover;
        FBackground := FTabColors.GetColor(False, ReadState);
        FForeground := FTabColors.GetColor(True, ReadState);
      end;

      // Background
      ARect := DrawRect;
      ARect.Width := ARect.Width-1;
      ARect.Height := ARect.Height-1;

      AWidth := 0;
      FPen := nil;

      if Selected then begin
        FPen := GetRGB(ColorBlend(FDrawColors.BackGroundInterior, clGray, 40)).MakeGDIPen(TABSTRIP_TAB_LINE_WIDTH);
        AWidth := round(TABSTRIP_TAB_LINE_WIDTH);
      end;

      ARect.Inflate(trunc(AWidth), 0);
      if ARect.Width > 0 then begin
        var RR := MakeRoundRect(ARect, TABSTRIP_TAB_ITEM_ROUND);
        if not RoundTabItems then begin
          RR.RoundBL := 1;
          RR.RoundBR := 1;
        end;
        GDIRoundRect( RR,
          GetRGB(FBackground).MakeGDIBrush, FPen);
      end;

      // Client
      var ClientRect := DrawRect;
      case Orientation of
        FXOrientation.Horizontal: begin
          ClientRect.Inflate(-10, -2);
          if FShowCloseButton then
            ClientRect.Width := ClientRect.Width - (TABSTRIP_BUTTON_SIZE + 2);
        end;
        FXOrientation.Vertical: begin
          ClientRect.Inflate(-2, -10);
          if FShowCloseButton then
            ClientRect.Height := ClientRect.Height - (TABSTRIP_BUTTON_SIZE + 2);
        end;
      end;

      Font.Assign(Self.Font);
      Font.Color := FForeground;

      // Icon
      if Image.Enabled then begin
        ARect := ClientRect;
        ARect.Width := FDrawImageSize;
        ARect.Height := FDrawImageSize;
        case Orientation of
          FXOrientation.Horizontal: begin
            ARect.Offset(0, (ClientRect.Height-ARect.Height) div 2);
            // Shrink client rect
            ClientRect.Left := ClientRect.Left + ARect.Width + TABSTRIP_TAB_ICON_SPACING;
          end;
          FXOrientation.Vertical: begin
            ARect.Offset((ClientRect.Width-ARect.Width) div 2, 0);
            // Shrink client rect
            ClientRect.Top := ClientRect.Top + ARect.Height + TABSTRIP_TAB_ICON_SPACING;
          end;
        end;

        Image.DrawIcon(Buffer, ARect);
      end;

      // Text
      Brush.Style := bsClear;
      Font.Assign(Self.Font);
      Font.Color := FForeground;

      ARect := ClientRect;
      var AText := Text;
      var FTextDrawFlags: FXTextFlags := [FXTextFlag.WordWrap, FXTextFlag.Left];
      case Orientation of
        FXOrientation.Horizontal: FTextDrawFlags := FTextDrawFlags + [FXTextFlag.VerticalCenter];
        FXOrientation.Vertical: FTextDrawFlags := FTextDrawFlags + [FXTextFlag.Center];
      end;

      DrawTextRect(Buffer, ARect, AText, FTextDrawFlags);

      // Close button
      if FShowCloseButton then begin
        ARect := GetCloseButtonRectFromTabRect(DrawRect);

        var ABackground := FBackground;
        var AForeground := FForeground;
        if MouseOnCloseButton then begin
          ABackground := FTabColors.GetColor(False, TabInteractionState);
          ABackground := ColorBlend(ABackground, clGray, PRIMARY_COLOR_TINT_GRAY_VALUE);

          // else ... keep prev
          if not (TabInteractionState in [FXControlState.Hover, FXControlState.None]) then
            AForeground := FTabColors.GetColor(True, TabInteractionState);
        end;

        // Draw
        GDIRoundRect( MakeRoundRect(ARect, TABSTRIP_TAB_ITEM_ROUND),
          GetRGB(ABackground).MakeGDIBrush, nil);

        // Text
        ARect.Inflate(-5, -5);
        DrawFontIcon(Buffer, #$E711, AForeground, ARect);
      end;


      // Delete overflow
      ARect := TRect.Empty;
      case Orientation of
        FXOrientation.Horizontal:
        if DrawRect.Left < DrawRectTabs.Left then begin
          ARect := DrawRect;
          ARect.Right := DrawRectTabs.Left;
        end else
        if DrawRect.Right > DrawRectTabs.Right then begin
          ARect := DrawRect;
          ARect.Left := DrawRectTabs.Right;
        end;

        FXOrientation.Vertical:
        if DrawRect.Top < DrawRectTabs.Top then begin
          ARect := DrawRect;
          ARect.Bottom := DrawRectTabs.Top;
        end else
        if DrawRect.Bottom > DrawRectTabs.Bottom then begin
          ARect := DrawRect;
          ARect.Top := DrawRectTabs.Bottom;
        end;
      end;
      if Selected then
        ARect.Inflate(TABSTRIP_TAB_LINE_WIDTH, TABSTRIP_TAB_LINE_WIDTH);
      ClearBufferRegion(ARect);
    end;
  end;

var
  FPen: TGDIPen;
  ARect: TRect;

  FBackground, FForeground: TColor;
begin
  // Background
  Color := FDrawColors.BackGround;
  PaintBackground;

  // Draw
  for var I := 0 to Tabs.Count-1 do
    if I <> FValue then begin
      var TabState := FXControlState.None;
      if I = FValueHover then
        TabState := InteractionState;

      // Draw
      DoDrawTab(FItemRects[I], Tabs[I].Text, FHoverIsOnCloseButton and (I = FValueHover), Tabs[I].FImage, TabState, false);
    end;

  // Isolation line
  if FDrawIsolationLine then begin
    FPen := GetRGB(ColorBlend(FDrawColors.BackGroundInterior, clGray, 40)).MakeGDIPen(TABSTRIP_TAB_LINE_WIDTH);
    ARect := DrawRect;
    ARect.Offset(0, -TABSTRIP_TAB_LINE_WIDTH);

    Buffer.GDILine(Line(Point(ARect.Left, ARect.Bottom), ARect.BottomRight), FPen);
  end;

  // Draw selected separately (to draw above)
  if FValue <> -1 then begin
    DoDrawTab(FItemRects[FValue], Tabs[FValue].Text, FHoverIsOnCloseButton and (FValue = FValueHover), Tabs[FValue].FImage, InteractionState, true);
  end;

  // Buttons
  const DoPrepareButton = procedure(IsEnabled: boolean; ButtonIndex: integer) begin
    var AState: FXControlState := FXControlState.None;
    if (IsEnabled) and (FStaticButtonHoverIndex = ButtonIndex) then begin
      AState:=InteractionState;
    end;
    FBackground := FButtonColors.GetColor(False, AState);
    FForeground := FButtonColors.GetColor(True, AState);

    if not IsEnabled then
      FForeGround := ColorBlend(FForeGround, FDrawColors.Background, 150);
  end;
  if FIsScrollingRequired then begin
    // Draw
    DoPrepareButton(FScrollPos>0, 0);
    case Orientation of
      FXOrientation.Horizontal: DrawFXIconButton(Buffer, DrawRectButtonLeft, #$EDD9, FBackground, FForeground);
      FXOrientation.Vertical: DrawFXIconButton(Buffer, DrawRectButtonLeft, #$EDDB, FBackground, FForeground);
    end;

    // Draw
    DoPrepareButton(FScrollPos<FScrollMax, 1);
    case Orientation of
      FXOrientation.Horizontal: DrawFXIconButton(Buffer, DrawRectButtonRight, #$EDDA, FBackground, FForeground);
      FXOrientation.Vertical: DrawFXIconButton(Buffer, DrawRectButtonRight, #$EDDC, FBackground, FForeground);
    end;
  end;
  if ShowPlusButton then begin
    // Draw
    DoPrepareButton(true, 2);
    DrawFXIconButton(Buffer, DrawRectButtonPlus, #$E710, FBackground, FForeground);
  end;

  // Inherit
  inherited;
end;

procedure FXTabStrip.PopupBeforePopup(Sender: TObject; var CanPopup: boolean;
  Point: TPoint);
var
  HasSelection: boolean;
begin
  ThemeManager.ProcessSystemMenu(FDefaultMenu);

  HasSelection := ValueHover <> -1;
  CanPopup := HasSelection and ((FEnableReordering and (Tabs.Count > 1)) or FShowCloseButton);

  FDefaultMenu.Items[0].Visible := FEnableReordering and (ValueHover > 0);
  FDefaultMenu.Items[1].Visible := FEnableReordering and (ValueHover < Tabs.Count-1);
  FDefaultMenu.Items[3].Visible := FShowCloseButton;
    FDefaultMenu.Items[2].Visible := FDefaultMenu.Items[3].Visible;
//  FDefaultMenu.Items[2].Enabled = ...
end;

procedure FXTabStrip.PopupItemClick(Sender: TObject; Item: FXPopupComponent;
  Index: integer);
begin
  case Index of
    0:
    if Assigned(OnTabMove) then
      OnTabMove(Self, ValueHover, ValueHover-1);
    1:
    if Assigned(OnTabMove) then
      OnTabMove(Self, ValueHover, ValueHover+1);
    // 2
    3:
    if Assigned(OnClosePressed) then
      OnClosePressed(Self, ValueHover);
  end;
end;

procedure FXTabStrip.PrepDefaultMenu;
var
  M: FXPopupItem;
begin
  with FDefaultMenu.Items do
    begin
      M := FXPopupItem.Create(FDefaultMenu);
      with M do
        begin
          Image.Enabled := true;
          Image.IconType := FXIconType.SegoeIcon;
          Image.SelectSegoe := #$E76B;

          Text := CFX_String_TabStrip_Tab_Menu_Move_Left;
        end;
      Add(M);

      M := FXPopupItem.Create(FDefaultMenu);
      with M do
        begin
          Image.Enabled := true;
          Image.IconType := FXIconType.SegoeIcon;
          Image.SelectSegoe := #$E76C;

          Text := CFX_String_TabStrip_Tab_Menu_Move_Right;
        end;
      Add(M);

      M := FXPopupItem.Create(FDefaultMenu);
        M.IsSeparator := true;
      Add(M);

      M := FXPopupItem.Create(FDefaultMenu);
      with M do
        begin
          Image.Enabled := true;
          Image.IconType := FXIconType.SegoeIcon;
          Image.SelectSegoe := #$E711;

          Text := CFX_String_TabStrip_Tab_Menu_Close;
        end;
      Add(M);
    end;
end;

procedure FXTabStrip.UpdateColors;
begin
  // Access theme manager
  FDrawColors.Assign( ThemeManager.SystemColor );
  if FCustomColors.Enabled then
    // Custom Colors
    FDrawColors.LoadFrom(FCustomColors, ThemeManager.DarkTheme);

  // Tab Colors
  if FCustomTabColors.Enabled then
    begin
      // Custom Colors
      FTabColors.LoadFrom(FCustomTabColors, ThemeManager.DarkTheme);
    end
  else
    begin
      // Load from FDrawColors
      var AOffset := BUTTON_COLOR_OFFSET;

      with FTabColors do begin
        BackgroundNone := FDrawColors.BackGround;
        BackgroundHover := ChangeColorLight(BackgroundNone, -AOffset);
        BackgroundPress := ChangeColorLight(FDrawColors.BackGroundInterior, AOffset);

        ForeGroundNone := ColorBlend(FDrawColors.ForeGround, BackgroundNone, TABSTRIP_TAB_TEXT_BLEND_FADE);
        ForeGroundHover := ForeGroundNone;
        ForeGroundPress := ColorBlend(FDrawColors.ForeGround, BackgroundNone, TABSTRIP_TAB_TEXT_DOWN_BLEND_FADE);
      end;
    end;

  // Button Colors
  if FCustomButtonColors.Enabled then
    begin
      // Custom Colors
      FButtonColors.LoadFrom(FCustomButtonColors, ThemeManager.DarkTheme);
    end
  else
    begin
      // Load from FDrawColors
      var AOffset := BUTTON_COLOR_OFFSET;

      with FButtonColors do begin
        BackgroundNone := FDrawColors.BackGround;
        BackgroundHover := ChangeColorLight(BackgroundNone, -AOffset);
        BackgroundPress := ChangeColorLight(FDrawColors.BackGroundInterior, AOffset);

        ForeGroundNone := ColorBlend(FDrawColors.ForeGround, BackgroundNone, TABSTRIP_TAB_TEXT_BLEND_FADE);
        ForeGroundHover := ForeGroundNone;
        ForeGroundPress := ColorBlend(FDrawColors.ForeGround, BackgroundNone, TABSTRIP_TAB_TEXT_DOWN_BLEND_FADE);
      end;
    end;

  // Disabled
  if not Enabled then
    begin
      FDrawColors.Accent := GetColorGrayscale(FDrawColors.Accent);
      FDrawColors.ForeGround := GetColorGrayscale(FDrawColors.ForeGround);

      FTabColors.BackgroundNone := GetColorGrayscale(FTabColors.BackgroundNone);
      FTabColors.ForegroundNone := GetColorGrayscale(FTabColors.ForegroundNone);
      FTabColors.ForegroundNone := ColorBlend(FTabColors.BackgroundNone, FTabColors.ForegroundNone, BUTTON_BLEND_FADE);

      FButtonColors.BackgroundNone := GetColorGrayscale(FButtonColors.BackgroundNone);
      FButtonColors.ForegroundNone := GetColorGrayscale(FButtonColors.ForegroundNone);
      FButtonColors.ForegroundNone := ColorBlend(FButtonColors.BackgroundNone, FButtonColors.ForegroundNone, BUTTON_BLEND_FADE);
    end
end;

procedure FXTabStrip.UpdateRects;
var
  Pos: integer;
  ATextHeight: integer;
  AvailableSpace: integer;
  I: integer;
begin
  // Rect
  DrawRect := GetClientRect;

  FIsScrollingRequired := false;

  AvailableSpace := 0;
  case Orientation of
    FXOrientation.Horizontal: AvailableSpace := DrawRect.Width-TABSTRIP_PADDING_START;
    FXOrientation.Vertical: AvailableSpace := DrawRect.Height-TABSTRIP_PADDING_START;
  end;
  if ShowPlusButton then
    Dec(AvailableSpace, TABSTRIP_BUTTON_SIZE+TABSTRIP_BUTTON_SPACING);

  // Rects
  SetLength(FItemRects, Tabs.Count);
  Pos := 0;
  case Orientation of
    FXOrientation.Horizontal: begin
      Pos := DrawRect.Left;
      for I := 0 to Tabs.Count-1 do begin
        FItemRects[I] := Rect(Pos, DrawRect.Top,
          Pos + Tabs[I].Size, DrawRect.Bottom);

        // Next
        Pos := Pos + Tabs[I].Size;
      end;

      // Scroller
      if Pos > AvailableSpace then
        FIsScrollingRequired := true;
    end;
    FXOrientation.Vertical: begin
      Pos := DrawRect.Top;
      for I := 0 to Tabs.Count-1 do begin
        FItemRects[I] := Rect(DrawRect.Left, Pos,
          DrawRect.Right, Pos + Tabs[I].Size);

        // Next
        Pos := Pos + Tabs[I].Size;

        // Scroller
        if Pos > AvailableSpace then
          FIsScrollingRequired := true;
      end;
    end;
  end;

  // Tabs rect
  DrawRectTabs := DrawRect;
  case Orientation of
    FXOrientation.Horizontal: DrawRectTabs.Left := DrawRectTabs.Left + TABSTRIP_PADDING_START;
    FXOrientation.Vertical: DrawRectTabs.Top := DrawRectTabs.Top + TABSTRIP_PADDING_START;
  end;

  // Scroller
  FScrollMax := 0;
  var AddStart: integer := 0;
  var AddEnd: integer := 0;
  if FIsScrollingRequired then begin
    AddStart := AddStart + (TABSTRIP_BUTTON_SIZE+TABSTRIP_BUTTON_SPACING);
    AddEnd := AddEnd - (TABSTRIP_BUTTON_SIZE+TABSTRIP_BUTTON_SPACING);

    if ShowPlusButton then
      AddEnd := AddEnd - (TABSTRIP_BUTTON_SIZE+TABSTRIP_BUTTON_SPACING);
  end;
  case Orientation of
    FXOrientation.Horizontal: begin
      DrawRectTabs.Left := DrawRectTabs.Left + AddStart;
      DrawRectTabs.Right := DrawRectTabs.Right + AddEnd;

      DrawRectTabs.Width := Min(DrawRectTabs.Width, Pos-DrawRect.Left);

      // Scroll amount
      FScrollMax := Max(Pos-DrawRectTabs.Width, 0);
    end;
    FXOrientation.Vertical: begin
      DrawRectTabs.Top := DrawRectTabs.Top + AddStart;
      DrawRectTabs.Bottom := DrawRectTabs.Bottom + AddEnd;

      DrawRectTabs.Height := Min(DrawRectTabs.Height, Pos-DrawRect.Top);

      // Scroll amount
      FScrollMax := Max(Pos-DrawRectTabs.Height, 0);
    end;
  end;
  FScrollPos := EnsureRange(FScrollPos, 0, FScrollMax); // ensure in right interval

  // Offset all tabs for scroll AND for spacing
  for I := 0 to High(FItemRects) do begin
    // Scroll
    case Orientation of
      FXOrientation.Horizontal: FItemRects[I].Offset(-FScrollPos, 0);
      FXOrientation.Vertical: FItemRects[I].Offset(0, -FScrollPos);
    end;

    // Buttons & stuff
    case Orientation of
      FXOrientation.Horizontal: FItemRects[I].Offset(
        DrawRectTabs.Left - DrawRect.Left,
        0);
      FXOrientation.Vertical: FItemRects[I].Offset(0,
        DrawRectTabs.Top - DrawRect.Top
        );
    end;
  end;

  // Shrink tabs rect

  // Rects
  if FIsScrollingRequired then begin
    DrawRectButtonLeft := DrawRectTabs;
    case Orientation of
      FXOrientation.Horizontal: begin
        DrawRectButtonLeft.Inflate(0, -5);
        DrawRectButtonLeft.Width := TABSTRIP_BUTTON_SIZE;
        DrawRectButtonLeft.Offset(-(TABSTRIP_BUTTON_SIZE+TABSTRIP_BUTTON_SPACING), 0);
      end;
      FXOrientation.Vertical: begin
        DrawRectButtonLeft.Inflate(-5, 0);
        DrawRectButtonLeft.Height := TABSTRIP_BUTTON_SIZE;
        DrawRectButtonLeft.Offset(0, -(TABSTRIP_BUTTON_SIZE+TABSTRIP_BUTTON_SPACING));
      end;
    end;

    DrawRectButtonRight := DrawRectTabs;
    case Orientation of
      FXOrientation.Horizontal: begin
        DrawRectButtonRight.Inflate(0, -5);
        DrawRectButtonRight.Left := DrawRectButtonRight.Right - TABSTRIP_BUTTON_SIZE;
        DrawRectButtonRight.Offset((TABSTRIP_BUTTON_SIZE+TABSTRIP_BUTTON_SPACING), 0);
      end;
      FXOrientation.Vertical: begin
        DrawRectButtonRight.Inflate(-5, 0);
        DrawRectButtonRight.Top := DrawRectButtonRight.Bottom - TABSTRIP_BUTTON_SIZE;
        DrawRectButtonRight.Offset(0, (TABSTRIP_BUTTON_SIZE+TABSTRIP_BUTTON_SPACING));
      end;
    end;
  end;


  if ShowPlusButton then begin
    DrawRectButtonPlus := DrawRectTabs;
    case Orientation of
      FXOrientation.Horizontal: begin
        DrawRectButtonPlus.Inflate(0, -5);
        DrawRectButtonPlus.Left := DrawRectButtonPlus.Right - TABSTRIP_BUTTON_SIZE;
        DrawRectButtonPlus.Offset((TABSTRIP_BUTTON_SIZE+TABSTRIP_BUTTON_SPACING), 0);

        if FIsScrollingRequired then
          DrawRectButtonPlus.Offset((TABSTRIP_BUTTON_SIZE+TABSTRIP_BUTTON_SPACING), 0);
      end;
      FXOrientation.Vertical: begin
        DrawRectButtonPlus.Inflate(-5, 0);
        DrawRectButtonPlus.Top := DrawRectButtonPlus.Bottom - TABSTRIP_BUTTON_SIZE;
        DrawRectButtonPlus.Offset(0, (TABSTRIP_BUTTON_SIZE+TABSTRIP_BUTTON_SPACING));

        if FIsScrollingRequired then
          DrawRectButtonPlus.Offset(0, (TABSTRIP_BUTTON_SIZE+TABSTRIP_BUTTON_SPACING));
      end;
    end;
  end;

  // Image
  ATextHeight := GetTextH;
  FDrawImageSize := round(ATextHeight * ImageScale);
end;

procedure FXTabStrip.ScaleChanged(Scaler: single);
begin
  inherited;
  // update scale
end;

procedure FXTabStrip.SetDrawIsolationLine(const Value: boolean);
begin
  if FDrawIsolationLine = Value then
    Exit;

  FDrawIsolationLine := Value;
  StandardUpdateDraw;
end;

procedure FXTabStrip.SetImageScale(const Value: single);
begin
  if FImageScale = Value then
    Exit;

  FImageScale := Value;
  StandardUpdateLayout;
end;

procedure FXTabStrip.SetOrientation(const Value: FXOrientation);
begin
  if (FOrientation = Value) then
    Exit;

  FOrientation := Value;

  if CanUpdate then
    SetBounds(Left, Top, Height, Width);  // this will also invoke UpdateRects() in Sized();
end;

procedure FXTabStrip.SetRoundTabItems(const Value: boolean);
begin
  if FRoundTabItems = Value then
    Exit;

  FRoundTabItems := Value;
  StandardUpdateDraw;
end;

procedure FXTabStrip.SetScrollPos(Value: integer);
begin
  Value := EnsureRange(Value, 0, FScrollMax);
  if Value = FScrollPos then
    Exit;
  FScrollPos := Value;

  // Draw
  StandardUpdateLayout;
end;

procedure FXTabStrip.SetShowPlusButton(const Value: boolean);
begin
  if FShowPlusButton = Value then
    Exit;

  FShowPlusButton := Value;
  StandardUpdateLayout;
end;

procedure FXTabStrip.SetValue(const Value: integer);
begin
  if (FValue = Value) or ((Value < 0) and (Value <> -1)) or ((Value >= FItems.Count) and not IsReading) then
    Exit;

  FValue := Value;

  // Notify
  if not IsReading then
    if Assigned(FOnChangeValue) then
      FOnChangeValue(Self);

  // Draw
  StandardUpdateDraw;
end;

procedure FXTabStrip.TimerRepeat(Sender: TObject);
begin
  FAutoRepeat.Interval := HOLD_REPEAT_INTERVAL;

  DoAddScroll(TTimer(Sender).Tag);
  if (FScrollPos = 0) or (FScrollPos = FScrollMax) then
    FAutoRepeat.Enabled := false;
end;

{ FXStripTabs }

function FXStripTabs.Add: FXStripTab;
begin
  Result := FXStripTab.Create(Self);
  Add( Result );
end;

procedure FXStripTabs.Add(const Item: FXStripTab);
begin
  // Clear previous data if applicable
  if Item.Parent <> nil then
    Item.Parent.Delete( Item.TabIndex );

  // Add to list
  const ATabIndex = FItems.Add(Item);

  Item.FParent := Self;
  Item.FTabIndex := ATabIndex;

  // UI
  StandardUpdateLayout;
end;

procedure FXStripTabs.Clear;
begin
  FItems.Clear;

  // UI
  DoUpdateItemDeleted;
end;

constructor FXStripTabs.Create(AOwner: TComponent);
begin
  inherited;

  FItems := TList<FXStripTab>.Create;
end;

procedure FXStripTabs.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FItems.Count) then
    Exit;

  FItems[Index].FTabIndex := -1;
  FItems[Index].FParent := nil;

  FItems.Delete(Index);

  // UI
  DoUpdateItemDeleted;
end;

destructor FXStripTabs.Destroy;
begin
  FreeAndNil(FItems);

  inherited;
end;

procedure FXStripTabs.DoUpdateItemDeleted;
begin
  if not Assigned(Owner) or not (Owner is FXTabStrip) then
    Exit;

  with (Owner as FXTabStrip) do begin
    if Value >= Count then
      Value := Count-1;
  end;

  StandardUpdateLayout;
end;

function FXStripTabs.GetCount: integer;
begin
  Exit( FItems.Count );
end;

function FXStripTabs.GetItem(Index: Integer): FXStripTab;
begin
  Exit( FItems[Index] );
end;

procedure FXStripTabs.Insert(Index: Integer; const Item: FXStripTab);
begin
  // Clear previous data if applicable
  if Item.Parent <> nil then
    Item.Parent.Delete( Item.TabIndex );

  // Add to list
  Index := EnsureRange(Index, 0, FItems.Count);

  FItems.Insert(Index, Item);

  Item.FParent := Self;
  Item.FTabIndex := Index;

  // UI
  StandardUpdateLayout;
end;

procedure FXStripTabs.Remove(const Item: FXStripTab);
begin
  Delete( FItems.IndexOf(Item) );
end;

procedure FXStripTabs.StandardUpdateLayout;
begin
  if not Assigned(Owner) or not (Owner is FXTabStrip) then
    Exit;

  (Owner as FXTabStrip).StandardUpdateLayout;
end;

{ FXStripTab }

constructor FXStripTab.Create(AOwner: TComponent);
begin
  inherited;

  FImage := FXIconSelect.Create(nil);
  FText := '';
  FSize := TABSTRIP_TAB_ITEM_DEFAULT_SIZE;
end;

destructor FXStripTab.Destroy;
begin
  FreeAndNil( FImage );
  inherited;
end;

procedure FXStripTab.SetImage(const Value: FXIconSelect);
begin

end;

procedure FXStripTab.SetTabIndex(Value: integer);
begin
  if (FTabIndex = Value) or (FParent = nil) then
    Exit;

  // Move
  EnsureRange(Value, 0, FParent.FItems.Count-1);
  FParent.FItems.Move(FTabIndex, Value);
  FTabIndex := Value;

  // UI
  StandardUpdateLayout;
end;

procedure FXStripTab.SetText(const Value: string);
begin
  if FText = Value then
    Exit;

  FText := Value;

  // UI
  StandardUpdateLayout;
end;

procedure FXStripTab.SetSize(const Value: integer);
begin
  if FSize = Value then
    Exit;

  // UI
  StandardUpdateLayout;
end;

procedure FXStripTab.StandardUpdateLayout;
begin
  if Parent = nil then
    Exit;

  Parent.StandardUpdateLayout;
end;

end.
