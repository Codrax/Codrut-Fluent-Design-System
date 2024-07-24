unit CFX.Selector;

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
  CFX.Classes,
  CFX.VarHelpers,
  CFX.Types,
  CFX.Linker,
  CFX.Animations,
  CFX.Controls;

type
  FXSelector = class(FXWindowsControl, FXControl)
    private
      var DrawRect: TRect;
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
      FAnim: TIntAni;

      //  Internal
      procedure UpdateColors;
      procedure UpdateRects;

      // Set properties
      procedure SetItems(const Value: TStringList);
      procedure SetSelectedItem(const Value: integer);

      // Handle Messages
      procedure WM_LButtonUp(var Msg: TWMLButtonUp); message WM_LBUTTONUP;
      procedure WMSize(var Message: TWMSize); message WM_SIZE;

      // Select
      procedure SelectNext;
      procedure SelectLast;
      function MakeDrawPositionRect: TRect;

      procedure SelectorItemsChange(Sender: TObject);

      // Paint
      function GetRoundness: integer;
      procedure AnimateToPosition;

    protected
      procedure PaintBuffer; override;
      procedure Resize; override;
      procedure ChangeScale(M, D: Integer{$IF CompilerVersion > 29}; isDpiChange: Boolean{$ENDIF}); override;

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
      function IsContainer: Boolean;
      procedure UpdateTheme(const UpdateChildren: Boolean);

      function Background: TColor;
  end;

implementation

procedure FXSelector.InteractionStateChanged(AState: FXControlState);
begin
  inherited;
  case AState of
    FXControlState.None: FHoverOver := -1;
  end;
  Invalidate;
end;

function FXSelector.IsContainer: Boolean;
begin
  Result := false;
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
  Result := Rect(FDrawPosition, DrawRect.Top, FDrawPosition + ItemWidth, DrawRect.Bottom)
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
    Invalidate;

end;

procedure FXSelector.UpdateTheme(const UpdateChildren: Boolean);
begin
  UpdateColors;
  UpdateRects;
  Invalidate;
end;

procedure FXSelector.UpdateColors;
var
  AccentColor: TColor;
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
        begin
          // Custom Colors
          AccentColor := FCustomColors.Accent;
          FDrawColors.Foreground := ExtractColor(FCustomColors, FXColorType.Foreground);
          FDrawColors.BackGround := ExtractColor(FCustomColors, FXColorType.BackGround);
          FDrawColors.BackGroundInterior := ExtractColor(FCustomColors, FXColorType.Content);
        end
      else
        begin
          // Global Colors
          AccentColor := ThemeManager.AccentColor;
          FDrawColors.ForeGround := ThemeManager.SystemColor.ForeGround;

          FDrawColors.BackGround := GetParentBackgroundColor(FDrawColors.BackGround);

          if ThemeManager.DarkTheme then
            FDrawColors.BackGroundInterior := ChangeColorLight(ThemeManager.SystemColor.BackGroundInterior, 20)
          else
            FDrawColors.BackGroundInterior := ChangeColorLight(ThemeManager.SystemColor.BackGroundInterior, -20);
        end;

      FItemAccentColors := FXSingleColorStateSet.Create(AccentColor,
                              ChangeColorLight(AccentColor, ACCENT_DIFFERENTIATE_CONST),
                              ChangeColorLight(AccentColor, -ACCENT_DIFFERENTIATE_CONST));
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
  DrawRect := GetClientRect;

  // Width
  ItemWidth := trunc(DrawRect.Width / FItems.Count);

  // Individual Rects
  SetLength(ItemRects, FItems.Count);
  for I := 0 to High(ItemRects) do
    begin
      with ItemRects[I] do
        begin
          Left := DrawRect.Left + ItemWidth * I;
          Right := DrawRect.Left + ItemWidth * (I+1);

          Top := DrawRect.Top;
          Bottom := DrawRect.Bottom;
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
  FAnim := TIntAni.Create;

  // Custom Color
  FCustomColors := FXCompleteColorSets.Create(Self);
  FItemAccentColors := FXSingleColorStateSet.Create;

  FDrawColors := FXCompleteColorSet.Create;

  // Sizing
  Height := 30;
  Width := 250;

  // Update
  UpdateRects;
  UpdateColors;
end;

destructor FXSelector.Destroy;
begin
  FreeAndNil( FCustomColors );
  FreeAndNil( FDrawColors );
  FreeAndNil( FItemAccentColors );
  FreeAndNil( FItems );
  inherited;
end;

function FXSelector.GetRoundness: integer;
begin
  if ItemWidth < Height then
    Result := ItemWidth
  else
    Result := Height;
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
var
  NewPosition: integer;
begin
  // Animate
  FAnim := TIntAni.Create;

  NewPosition := ItemRects[SelectedItem].Left;
  
  FAnim.StartValue := FDrawPosition;
  FAnim.EndValue := NewPosition;

  FAnim.AniFunctionKind := afkExpo;
  FAnim.Duration := 50;

  FAnim.OnSync := procedure(Value: integer)
  begin
    FDrawPosition := Value;
    Invalidate;
  end;

  FAnim.Start;
end;

function FXSelector.Background: TColor;
begin
  Result := FDrawColors.Background;
end;

procedure FXSelector.ChangeScale(M, D: Integer{$IF CompilerVersion > 29}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  UpdateRects;
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
      ARound := GetRoundness;
      GDIRoundRect(MakeRoundRect(DrawRect, ARound), GetRGB(FDrawColors.BackGroundInterior).MakeGDIBrush, nil);

      // Draw Backgrounds
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

procedure FXSelector.Resize;
begin
  inherited;
  UpdateRects;
end;

procedure FXSelector.ScaleChanged(Scaler: single);
begin
  inherited;
  ItemWidth := round(ItemWidth * Scaler);
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
  Invalidate;
end;

procedure FXSelector.SetItems(const Value: TStringList);
begin
  FItems.Assign(Value);

  UpdateRects;
  Invalidate;
end;

procedure FXSelector.SetSelectedItem(const Value: integer);
begin
  if (FSelectedItem <> Value) and (Value >= 0) and (Value < FItems.Count) then
    begin
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
      Invalidate;
    end;
end;

procedure FXSelector.WMSize(var Message: TWMSize);
begin
  UpdateRects;
  Invalidate;
end;

procedure FXSelector.WM_LButtonUp(var Msg: TWMLButtonUp);
begin
  if (FHoverOver <> SelectedItem) and (FHoverOver <> -1) then
    begin
      SelectedItem := FHoverOver;

      // Notify
      if Assigned(FOnChange) then
        FOnChange(Self);

      Invalidate;
    end;
  inherited;
end;

end.
