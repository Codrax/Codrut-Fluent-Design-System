unit CFX.RatingControl;

interface

uses
  Classes,
  Messages,
  Windows,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.ExtCtrls,
  System.Types,
  System.Math,
  CFX.Colors,
  CFX.ThemeManager,
  CFX.Graphics,
  CFX.Constants,
  SysUtils,
  CFX.Classes,
  CFX.Types,
  CFX.VarHelpers,
  CFX.Linker,
  CFX.Controls;

type
  FXRatingControl = class(FXWindowsControl)
  private
    const MAX_VALUE = 10;
    var DrawRect: TRect;
    StarRects: TArray<TRect>;
    FOnChange,
    FOnChangeValue: TNotifyEvent;
    FReadOnly: boolean;
    FValue: single;
    FValuePlaceholder: single;
    FValueIncrement: single;
    FStarCount: integer;
    FStarSpacing: integer;
    FStarPadding: integer;
    FHoverStarIndex: integer;
    FHoverValue: single;
    FRoundValueToIncrement: boolean;
    FClearEnabled: boolean;

    FDrawColors: FXCompleteColorSet;
    FCustomColors: FXColorSets;

    // Getters

    // Setters
    procedure SetStarCount(const Value: integer);
    procedure SetValue(Value: single);
    procedure SetStarSpacing(const Value: integer);
    procedure SetStarPadding(const Value: integer);
    procedure SetValueIncrement(Value: single);
    procedure SetValuePlaceholder(Value: single);

  protected
    procedure PaintBuffer; override;

    // Internal
    procedure UpdateColors; override;
    procedure UpdateRects; override;

    // Scaler
    procedure ScaleChanged(Scaler: single); override;

    // State
    procedure InteractionStateChanged(AState: FXControlState); override;

    // Inherited Mouse Detection
    procedure MouseUp(Button : TMouseButton; Shift: TShiftState; X, Y : integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

    procedure HandleKeyDown(var CanHandle: boolean; Key: integer; ShiftState: TShiftState); override;

  published
    // Custom Colors
    property CustomColors: FXColorSets read FCustomColors write FCustomColors stored true;

    // Props                  
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChangeValue: TNotifyEvent read FOnChangeValue write FOnChangeValue;
    
    property Value: single read FValue write SetValue;
    property ValuePlaceholder: single read FValuePlaceholder write SetValuePlaceholder;
    property ValueIncrement: single read FValueIncrement write SetValueIncrement;
    property RoundValueToIncrement: boolean read FRoundValueToIncrement write FRoundValueToIncrement default true;
    property StarCount: longint read FStarCount write SetStarCount default 5;
    property StarSpacing: integer read FStarSpacing write SetStarSpacing default 0;
    property StarPadding: integer read FStarPadding write SetStarPadding default 1;
    property ReadOnly: boolean read FReadOnly write FReadOnly default false;
    property ClearEnabled: boolean read FClearEnabled write FClearEnabled default false;

    // Default props
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

    // Interface
    function Background: TColor; override;
  end;

implementation

function FXRatingControl.Background: TColor;
begin
  Result := FDrawColors.BackGround;
end;

constructor FXRatingControl.Create(aOwner: TComponent);
begin
  inherited;
  // Custom Color
  FCustomColors := FXColorSets.Create(Self);

  FDrawColors := FXCompleteColorSet.Create;

  FHoverStarIndex := -1;

  FRoundValueToIncrement := true;
  FReadOnly := false;
  FClearEnabled := false;

  FValue := 0;
  FValuePlaceholder := 0;
  FValueIncrement := 0.5;
  FStarCount := 5;
  FStarSpacing := 0;
  FStarPadding := 1;

  // Sizing
  Height := 30;
  Width := 150;
end;

destructor FXRatingControl.Destroy;
begin
  FreeAndNil( FCustomColors );
  FreeAndNil( FDrawColors );
  inherited;
end;

procedure FXRatingControl.HandleKeyDown(var CanHandle: boolean; Key: integer;
  ShiftState: TShiftState);
begin
  inherited;
  if not FReadOnly and CanHandle then
    case Key of
      VK_LEFT: begin
        Value := Value - ValueIncrement;
        CanHandle := false;
      end;

      VK_RIGHT: begin
        Value := Value + ValueIncrement;
        CanHandle := false;
      end;

      VK_DELETE: if ClearEnabled then Value := 0;
    end;
end;

procedure FXRatingControl.InteractionStateChanged(AState: FXControlState);
begin
  inherited;
  Redraw;
end;

procedure FXRatingControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  var P := Point(X, Y); P.Offset(-DrawRect.Left, -DrawRect.Top);

  // Hover value
  if Length(StarRects) > 0 then begin
    var incrementWidth := DrawRect.Width / (10 / FValueIncrement);
    var adjustedX := P.X + incrementWidth / 2 / 2; // middle of increment slice

    FHoverValue := EnsureRange(adjustedX, 0, DrawRect.Width) / incrementWidth * FValueIncrement;
  end;
  if FRoundValueToIncrement then
    FHoverValue := round(FHoverValue / FValueIncrement) * FValueIncrement;
  if not ClearEnabled then
    FHoverValue := Max(FHoverValue, FValueIncrement);

  // Get hover index
  FHoverStarIndex := -1;
  for var I := 0 to High(StarRects) do
    if StarRects[I].Contains(P) then begin
      FHoverStarIndex := I;
      Break;
    end;

  // UI
  StandardUpdateDraw;
end;

procedure FXRatingControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: integer);
begin
  inherited;

  if not FReadOnly and DrawRect.Contains(Point(X, Y)) then begin
    if FClearEnabled and (Value = FHoverValue) then
      Value := 0
    else
      Value := FHoverValue;

    // Event
    if not IsReading and Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure FXRatingControl.PaintBuffer;
procedure DrawValueLayer(AValue: single; StarColor: TColor; OutlineMode: boolean);
var
  R: TRect;
  StarIcons: TArray<string>;
begin
  if OutlineMode then
    StarIcons := [
      #$F0F7,
      #$F0F7,
      #$E734,
      #$E734
    ]
  else
    StarIcons := [
      #$F0CA,
      #$E7C6,
      #$F0CC,
      #$E735
    ];

  if AValue = 0 then Exit; // useless to run code
  const TransposedValue = round(AValue / MAX_VALUE * (FStarCount*4));

  for var I := 0 to FStarCount-1 do begin
    R := StarRects[I];
    R.Inflate(-StarPadding, 0);

    // Draw fill
    const StarValue = I*4;

    var S: string;
    if TransposedValue <= StarValue then
      S := '' // empty
    else
    if TransposedValue >= StarValue+4 then
      S := StarIcons[3] // full
    else
      // partial
      case TransposedValue mod 4 of
        1: S := StarIcons[0]; // 1/4
        2: S := StarIcons[1]; // 2/4
        3: S := StarIcons[2]; // 3/5
      end;

    // Draw fill
    if S <> '' then
      DrawFontIcon(Buffer, S, StarColor, R);
  end;
end;
begin
  // Background
  Color := FDrawColors.BackGround;
  PaintBackground;

  // Draw empty stars
  Buffer.Brush.Style := bsClear;
  for var I := 0 to FStarCount-1 do begin
    var R := StarRects[I];
    R.Inflate(-StarPadding, 0);

    // Draw outline
    DrawFontIcon(Buffer, #$E734, FDrawColors.ForeGround, R);
  end;

  // Draw
  if (InteractionState = FXControlState.None) or (FHoverStarIndex = -1) then begin
    if (FValue = 0) and (FValuePlaceholder > 0) then
      DrawValueLayer(FValuePlaceholder, FDrawColors.ForeGround, false)
    else
      DrawValueLayer(FValue, FDrawColors.Accent, false);
  end
  else begin
    if FValue = 0 then begin
      DrawValueLayer(FHoverValue, ColorBlend(FDrawColors.ForeGround, FDrawColors.BackGround, 225), false);
      DrawValueLayer(FHoverValue, ColorBlend(FDrawColors.ForeGround, FDrawColors.BackGround, 75), true);
    end
    else
      DrawValueLayer(FHoverValue, FDrawColors.Accent, false)
  end;

  // Inherit
  inherited;
end;

procedure FXRatingControl.UpdateColors;
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

procedure FXRatingControl.UpdateRects;
begin
  // Rect
  DrawRect := GetClientRect;

  // Update
  SetLength(StarRects, FStarCount);
  if FStarCount = 0 then
    Exit;

  // Get rects
  const StarWidth = (DrawRect.Width-(FStarCount-1)*FStarSpacing) div FStarCount;
  for var I := 0 to High(StarRects) do begin
    StarRects[I].Left := (StarWidth+FStarSpacing)*I;
    StarRects[I].Right := StarRects[I].Left + StarWidth;

    StarRects[I].Top := DrawRect.Top;
    StarRects[I].Bottom := DrawRect.Bottom;
  end;
end;

procedure FXRatingControl.ScaleChanged(Scaler: single);
begin
  inherited;
  // update scale
end;

procedure FXRatingControl.SetStarCount(const Value: integer);
begin
  if FStarCount = Value then Exit;

  FStarCount := Value;
  StandardUpdateLayout;
end;

procedure FXRatingControl.SetStarPadding(const Value: integer);
begin
  if FStarPadding = Value then Exit;
  
  FStarPadding := Value;
  StandardUpdateDraw;
end;

procedure FXRatingControl.SetStarSpacing(const Value: integer);
begin
  if FStarSpacing = Value then Exit;

  FStarSpacing := Value;
  StandardUpdateLayout;
end;

procedure FXRatingControl.SetValue(Value: single);
begin
  Value := EnsureRange(Value, 0, MAX_VALUE);
  if FValue = Value then Exit;

  FValue := Value;
  StandardUpdateDraw;

  // Event
  if not IsReading and Assigned(FOnChangeValue) then
    FOnChangeValue(Self);
end;

procedure FXRatingControl.SetValueIncrement(Value: single);
begin
  Value := EnsureRange(Value, 0.001, MAX_VALUE);
  if FValueIncrement = Value then
    Exit;
  FValueIncrement := Value;
end;

procedure FXRatingControl.SetValuePlaceholder(Value: single);
begin
  Value := EnsureRange(Value, 0, MAX_VALUE);
  if FValuePlaceholder = Value then Exit;

  FValuePlaceholder := Value;
  StandardUpdateDraw;
end;

end.
