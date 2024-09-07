unit CFX.Selector;

interface
uses
  Classes,
  Messages,
  Windows,
  Vcl.Controls,
  Vcl.Graphics,
  Types,
  Math,
  CFX.Colors,
  CFX.ThemeManager,
  CFX.Graphics,
  CFX.Constants,
  SysUtils,
  CFX.Classes,
  CFX.VarHelpers,
  CFX.Types,
  CFX.Linker,
  CFX.Animation.Component,
  CFX.Controls;

type
  FXSelector = class(FXWindowsControl)
  private
    var DrawRect, MainRect: TRect;
    ItemRects: TArray<TRect>;
    ItemWidth: integer;
    FOnChange: TNotifyEvent;
    FOnChangeValue: TNotifyEvent;
    FCustomColors: FXCompleteColorSets;
    FItemAccentColors: FXSingleColorStateSet;
    FDrawColors: FXCompleteColorSet;
    FItems: TStringList;
    FHoverOver, FSelectedItem: integer;
    FDrawPosition: integer;
    FAnimation: boolean;
    FAnim: FXIntAnim;

    // Select
    procedure SelectNext;
    procedure SelectLast;
    function MakeDrawPositionRect: TRect;

    procedure SelectorItemsChange(Sender: TObject);

    // Animation
    procedure AnimationDoStep(Sender: TObject; Step, TotalSteps: integer);

    // Paint
    function GetRoundness(OfItems: boolean): integer; // OfItemss -> in contentrect, else clientrect
    procedure AnimateToPosition;

    // Setters
    procedure SetItems(const Value: TStringList);
    procedure SetSelectedItem(const Value: integer);

  protected
    procedure PaintBuffer; override;

    // Internal
    procedure UpdateColors; override;
    procedure UpdateRects; override;

    // Scale
    procedure ScaleChanged(Scaler: single); override;

    // State
    procedure InteractionStateChanged(AState: FXControlState); override;

    // Focus
    procedure UpdateFocusRect; override;

    // Key Presses
    procedure KeyPress(var Key: Char); override;
    procedure HandleKeyDown(var CanHandle: boolean; Key: integer; ShiftState: TShiftState); override;

    // Inherited Mouse Detection
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;

  published
    property CustomColors: FXCompleteColorSets read FCustomColors write FCustomColors stored true;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChangeValue: TNotifyEvent read FOnChangeValue write FOnChangeValue;

    property SelectedItem: integer read FSelectedItem write SetSelectedItem;
    property Items: TStringList read FItems write SetItems;

    property Animation: boolean read FAnimation write FAnimation default true;

    property Align;
    property Font;
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

    // Interface
    function Background: TColor; override;
  end;

implementation

procedure FXSelector.InteractionStateChanged(AState: FXControlState);
begin
  inherited;
  case AState of
    FXControlState.None: FHoverOver := -1;
  end;
  Redraw;
end;

procedure FXSelector.KeyPress(var Key: Char);
begin
  inherited;
  // Move with - or +
  case Key of
    '+', '=': SelectNext;
    '-': SelectLast;
  end;
end;

function FXSelector.MakeDrawPositionRect: TRect;
begin
  Result := Rect(FDrawPosition, MainRect.Top, FDrawPosition + ItemWidth, MainRect.Bottom)
end;

procedure FXSelector.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: integer);
begin
  inherited;
  if (FHoverOver <> SelectedItem) and (FHoverOver <> -1) then
    begin
      SelectedItem := FHoverOver;

      // Notify
      if Assigned(FOnChange) then
        FOnChange(Self);

      Redraw;
    end;
end;

procedure FXSelector.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  I: Integer;
  APoint: TPoint;
  AItem: integer;
begin
  inherited;
  APoint := Point(X, Y);

  // Hover
  AItem := FHoverOver;
  FHoverOver := -1;
  for I := 0 to High(ItemRects) do
    if ItemRects[I].Contains(APoint) then
      FHoverOver := I;

  // Changed
  if AItem <> FHoverOver then
    Redraw;

end;

procedure FXSelector.UpdateColors;
begin
  FDrawColors.Assign( ThemeManager.SystemColor );
  if not Enabled then
    begin
      FItemAccentColors := FXSingleColorStateSet.Create($808080,
                                ChangeColorLight($808080, ACCENT_DIFFERENTIATE_CONST),
                                ChangeColorLight($808080, -ACCENT_DIFFERENTIATE_CONST));
    end
  else
    begin
      // Access theme manager
      if FCustomColors.Enabled then
        FDrawColors.LoadFrom(FCustomColors, ThemeManager.DarkTheme)
      else
        begin
          if ThemeManager.DarkTheme then
            FDrawColors.BackGroundInterior := ChangeColorLight(ThemeManager.SystemColor.BackGroundInterior, 20)
          else
            FDrawColors.BackGroundInterior := ChangeColorLight(ThemeManager.SystemColor.BackGroundInterior, -20);
        end;

      FItemAccentColors := FXSingleColorStateSet.Create(FDrawColors.Accent,
                              ChangeColorLight(FDrawColors.Accent, ACCENT_DIFFERENTIATE_CONST),
                              ChangeColorLight(FDrawColors.Accent, -ACCENT_DIFFERENTIATE_CONST));
    end;
end;

procedure FXSelector.UpdateFocusRect;
begin
  FocusRect := MakeDrawPositionRect;
end;

procedure FXSelector.UpdateRects;
var
  I: Integer;
begin
  // Rect
  DrawRect := ClientRect;
  MainRect := ContentRect;

  // Width
  ItemWidth := trunc(MainRect.Width / FItems.Count);

  // Individual Rects
  SetLength(ItemRects, FItems.Count);
  for I := 0 to High(ItemRects) do
    begin
      with ItemRects[I] do
        begin
          Left := MainRect.Left + ItemWidth * I;
          Right := MainRect.Left + ItemWidth * (I+1);

          Top := MainRect.Top;
          Bottom := MainRect.Bottom;
        end;
    end;

  // Pos
  FDrawPosition := ItemRects[SelectedItem].Left;
end;

constructor FXSelector.Create(aOwner: TComponent);
begin
  inherited;
  AutoFocusLine := true;
  BufferedComponent := true;
  FAnimation := true;

  // Items
  FItems := TStringList.Create;

  FItems.Add('Item1');
  FItems.Add('Item2');
  FItems.Add('Item3');

  FItems.OnChange := SelectorItemsChange;

  FSelectedItem := 0;

  // Anim
  FAnim := FXIntAnim.Create(Self);
  with FAnim do begin
    Duration := 0.4;
    Kind := FXAnimationKind.ReverseExpo;

    LatencyAdjustments := true;
    LatencyCanSkipSteps := true;

    OnStep := AnimationDoStep;
  end;

  // Custom Color
  FCustomColors := FXCompleteColorSets.Create(Self);
  FItemAccentColors := FXSingleColorStateSet.Create;

  FDrawColors := FXCompleteColorSet.Create;

  // Sizing
  Height := 30;
  Width := 250;
end;

destructor FXSelector.Destroy;
begin
  FreeAndNil( FAnim );
  FreeAndNil( FCustomColors );
  FreeAndNil( FDrawColors );
  FreeAndNil( FItemAccentColors );
  FreeAndNil( FItems );
  inherited;
end;

function FXSelector.GetRoundness(OfItems: boolean): integer;
begin
  if OfItems then
    Result := Min(ItemWidth, MainRect.Height)
  else
    Result := Min(ItemWidth, DrawRect.Height)
end;

procedure FXSelector.HandleKeyDown(var CanHandle: boolean; Key: integer;
  ShiftState: TShiftState);
begin
  inherited;
  case Key of
    VK_LEFT: begin
      SelectLast;
      CanHandle := false;
    end;

    VK_RIGHT: begin
      SelectNext;
      CanHandle := false;
    end;
  end;
end;

procedure FXSelector.AnimateToPosition;
begin
  FAnim.Stop;

  FAnim.StartValue := FDrawPosition;
  FAnim.EndValue := ItemRects[SelectedItem].Left;

  FAnim.Start;
end;

procedure FXSelector.AnimationDoStep(Sender: TObject; Step,
  TotalSteps: integer);
begin
  FDrawPosition := FAnim.CurrentValue;

  StandardUpdateDraw;
end;

function FXSelector.Background: TColor;
begin
  Result := FDrawColors.Background;
end;

procedure FXSelector.PaintBuffer;
var
  I: integer;
  ARound: integer;
  DrawBackground: boolean;
  AColor: TColor;
  ARect: TRect;
begin
  // Background
  Color := FDrawColors.BackGround;
  PaintBackground;

  // Draw
  with Buffer do
    begin
      // Main Rectangle
      GDIRoundRect(MakeRoundRect(DrawRect, GetRoundness(false)), GetRGB(FDrawColors.BackGroundInterior).MakeGDIBrush, nil);

      // Draw Backgrounds
      ARound := GetRoundness(true);
      for I := 0 to High(ItemRects) do
        begin
          DrawBackground := (FHoverOver = I);

          // Background Items
          if DrawBackground then
            begin
              ARect := ItemRects[I];
              
              AColor := ChangeColorLight(FDrawColors.backGroundInterior, 10);

              GDIRoundRect(MakeRoundRect(ARect, ARound), GetRGB(AColor).MakeGDIBrush, nil);
            end;
        end;

      // Draw foreground Item
      if FHoverOver = Selecteditem then
        AColor := FItemAccentColors.GetColor(InteractionState)
      else
        AColor := FItemAccentColors.None;
      ARect := MakeDrawPositionRect;
      GDIRoundRect(MakeRoundRect(ARect, ARound), GetRGB(AColor).MakeGDIBrush, nil);

      // Draw Texts
      for I := 0 to High(ItemRects) do
        begin
          // Text
          Brush.Style := bsClear;
          Font.Assign(Self.Font);
          Font.Color := FDrawColors.ForeGround;

          DrawTextRect(Buffer, ItemRects[I], FItems[I], [FXTextFlag.Center, FXTextFlag.VerticalCenter]);
        end;
    end;

  inherited;
end;

procedure FXSelector.ScaleChanged(Scaler: single);
begin
  ItemWidth := round(ItemWidth * Scaler);
  inherited;
end;

procedure FXSelector.SelectLast;
begin
  if SelectedItem > 0 then
    SelectedItem := SelectedItem - 1;

  // Notify
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure FXSelector.SelectNext;
begin
  if SelectedItem < FItems.Count then
    SelectedItem := SelectedItem + 1;

  // Notify
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure FXSelector.SelectorItemsChange(Sender: TObject);
begin
  UpdateRects;
  Redraw;
end;

procedure FXSelector.SetItems(const Value: TStringList);
begin
  FItems.Assign(Value);

  UpdateRects;
  Redraw;
end;

procedure FXSelector.SetSelectedItem(const Value: integer);
begin
  if (FSelectedItem = Value) or (Value < 0) or (Value >= FItems.Count) then
    Exit;


  FSelectedItem := Value;

  if Animation and not IsReading and not IsDesigning then
    AnimateToPosition
  else
    FDrawPosition := ItemRects[Value].Left;

  // Notify
  if not IsReading then
    if Assigned(FOnChangeValue) then
      FOnChangeValue(Self);

  // Draw
  StandardUpdateDraw;
end;

end.
