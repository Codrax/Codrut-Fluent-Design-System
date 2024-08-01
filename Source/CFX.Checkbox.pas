unit CFX.Checkbox;

interface

uses
  Classes,
  Messages,
  Windows,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.ExtCtrls,
  Types,
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
  FXCheckBox = class(FXWindowsControl, FXControl)
  private
    var DrawRect, IconRect, TextRect, ImageRect: TRect;
    FIconFont: TFont;
    FAllowGrayed: Boolean;
    FState: FXCheckBoxState;
    FTextSpacing: Integer;
    FOnChange: TNotifyEvent;
    FOnChangeValue: TNotifyEvent;
    FCustomColors: FXColorSets;
    FIconAccentColors: FXSingleColorStateSet;
    FText: string;
    FAutomaticMouseCursor: boolean;
    FDrawColors: FXCompleteColorSet;
    FWordWrap: boolean;
    FAnimationEnabled: boolean;
    FAnimationStatus: integer;
    FAnimateTimer: TTimer;
    FImage: FXIconSelect;
    FImageScale: single;
    FLayout: FXDrawLayout;
    FTextLayout: FXLayout;

    // Set properties
    procedure SetText(const Value: string);
    procedure SetWordWrap(const Value: boolean);
    procedure SetAllowGrayed(const Value: Boolean);
    procedure SetState(const Value: FXCheckBoxState);
    procedure SetTextSpacing(const Value: Integer);
    procedure SetChecked(const Value: Boolean);
    procedure SetImage(const Value: FXIconSelect);
    procedure SetLayout(const Value: FXDrawLayout);
    procedure SetImageScale(const Value: single);

    // State
    procedure ProgressState;

    // Draw functions
    function GetTextHeight: integer;

    // Get properties
    function GetChecked: Boolean;

    // Animation
    procedure AnimationProgress(Sender: TObject);

  protected
    procedure PaintBuffer; override;

    //  Internal
    procedure UpdateColors; override;
    procedure UpdateRects; override;

    // Scaler
    procedure ScaleChanged(Scaler: single); override;

    // Mouse
    procedure Click; override;

    // Key Presses
    procedure KeyPress(var Key: Char); override;

    // Inherited Mouse Detection
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

  published
    property CustomColors: FXColorSets read FCustomColors write FCustomColors stored true;
    property IconFont: TFont read FIconFont write FIconFont;
    property AllowGrayed: Boolean read FAllowGrayed write SetAllowGrayed default false;
    property State: FXCheckBoxState read FState write SetState default FXCheckBoxState.Unchecked;
    property TextSpacing: Integer read FTextSpacing write SetTextSpacing default CHECKBOX_TEXT_SPACE;
    property Checked: Boolean read GetChecked write SetChecked default false;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChangeValue: TNotifyEvent read FOnChangeValue write FOnChangeValue;
    property AutomaticCursorPointer: boolean read FAutomaticMouseCursor write FAutomaticMouseCursor default true;

    property Text: string read FText write SetText;
    property WordWrap: boolean read FWordWrap write SetWordWrap default true;
    property Image: FXIconSelect read FImage write SetImage;
    property ImageScale: single read FImageScale write SetImageScale;
    property Layout: FXDrawLayout read FLayout write SetLayout default FXDrawLayout.Left;

    property AnimationEnabled: boolean read FAnimationEnabled write FAnimationEnabled default true;

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

procedure FXCheckBox.KeyPress(var Key: Char);
begin
  inherited;
  if (Key = #13) or (Key = #32) then
    ProgressState;
end;

procedure FXCheckBox.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Self.FAutomaticMouseCursor then
    if PtInRect(IconRect, Point(X, Y)) then
      Self.Cursor := crHandPoint
    else
      Self.Cursor := crDefault;
end;

procedure FXCheckBox.UpdateColors;
begin
  // Access theme manager
  FDrawColors.Assign( ThemeManager.SystemColor );
  if not Enabled then
    begin
      FIconAccentColors := FXSingleColorStateSet.Create($808080,
                                ChangeColorLight($808080, ACCENT_DIFFERENTIATE_CONST),
                                ChangeColorLight($808080, -ACCENT_DIFFERENTIATE_CONST));
      FDrawColors.Foreground := $808080;
    end
  else
    begin
      if FCustomColors.Enabled then
        // Custom Colors
        FDrawColors.LoadFrom(FCustomColors, ThemeManager.DarkTheme);

      FIconAccentColors := FXSingleColorStateSet.Create(FDrawColors.Accent,
                              ChangeColorLight(FDrawColors.Accent, ACCENT_DIFFERENTIATE_CONST),
                              ChangeColorLight(FDrawColors.Accent, -ACCENT_DIFFERENTIATE_CONST));
    end;
end;

procedure FXCheckBox.UpdateRects;
var
  AWidth, ASize: integer;
begin
  // Rect
  DrawRect := GetClientRect;

  // Font
  Buffer.Font.Assign(IconFont);
  AWidth := Buffer.TextWidth(CHECKBOX_OUTLINE);

  // Image
  ASize := 0;
  if FImage.Enabled then
    ASize := round(GetTextHeight * FImageScale);

  case Layout of
    FXDrawLayout.Left: begin
      IconRect := Rect(DrawRect.Left, DrawRect.Top,
        DrawRect.Left + AWidth + TextSpacing * 2, DrawRect.Bottom);
      TextRect := Rect(IconRect.Right + TextSpacing, DrawRect.Top,
        DrawRect.Right, DrawRect.Bottom);
      FTextLayout := FXLayout.Beginning;

      // Image
      if Image.Enabled then
        begin
          ImageRect := Rect(TextRect.Left, DrawRect.Top, TextRect.Left + ASize, DrawRect.Bottom);
          TextRect.Left := ImageRect.Right + TextSpacing;
        end;
    end;

    FXDrawLayout.Right: begin
      IconRect := Rect(DrawRect.Right-AWidth-TextSpacing, DrawRect.Top, DrawRect.Right, DrawRect.Bottom);
      TextRect := Rect(DrawRect.Left, DrawRect.Top, IconRect.Left - TextSpacing * 2, DrawRect.Bottom);
      FTextLayout := FXLayout.Beginning;

      // Image
      if Image.Enabled then
        begin
          ImageRect := Rect(TextRect.Left, DrawRect.Top, TextRect.Left + ASize, DrawRect.Bottom);
          TextRect.Left := ImageRect.Right + TextSpacing;
        end;
    end;

    FXDrawLayout.Top: begin
      IconRect := Rect(DrawRect.Left, DrawRect.Top, DrawRect.Right, AWidth + TextSpacing * 2);
      TextRect := Rect(DrawRect.Left, IconRect.Bottom + TextSpacing, DrawRect.Right, DrawRect.Bottom);
      IconRect.Width := AWidth;
      IconRect.Height := AWidth;
      IconRect.Offset((DrawRect.Width-AWidth) div 2, 0);

      FTextLayout := FXLayout.Center;

      // Image
      if Image.Enabled then
        begin
          ImageRect := Rect(DrawRect.Left, TextRect.Top, DrawRect.Right, TextRect.Top + ASize);
          TextRect.Top := ImageRect.Bottom + TextSpacing;
        end;
    end;

    FXDrawLayout.Bottom: begin
      IconRect := Rect(DrawRect.Left, DrawRect.Bottom - AWidth - TextSpacing * 2, DrawRect.Right, DrawRect.Bottom);
      TextRect := Rect(DrawRect.Left, DrawRect.Top, DrawRect.Right, DrawRect.Bottom - AWidth + TextSpacing * 2);
      IconRect.Width := AWidth;
      IconRect.Top := DrawRect.Bottom-AWidth;
      IconRect.Offset((DrawRect.Width-AWidth) div 2, 0);

      FTextLayout := FXLayout.Center;

      // Image
      if Image.Enabled then
        begin
          ImageRect := Rect(DrawRect.Left, TextRect.Top, DrawRect.Right, TextRect.Top + ASize);
          TextRect.Top := ImageRect.Bottom + TextSpacing;
        end;
    end;
  end;
end;

procedure FXCheckBox.ScaleChanged(Scaler: single);
begin
  IconFont.Height := round(IconFont.Height * Scaler);
  FTextSpacing := round(FTextSpacing * Scaler);
  FImageScale := FImageScale * Scaler;
  inherited;
end;

procedure FXCheckBox.SetAllowGrayed(const Value: Boolean);
begin
  if Value = FAllowGrayed then
    Exit;

  FAllowGrayed := Value;
  if (not Value) and (FState = FXCheckBoxState.Grayed) then
    FState := FXCheckBoxState.Unchecked;

  // Draw
  StandardUpdateLayout;
end;

procedure FXCheckBox.SetState(const Value: FXCheckBoxState);
begin
  if Value = FState then
    Exit;

  // Animation
  if FAnimationEnabled
    and (Value = FXCheckBoxState.Checked) and (FState = FXCheckBoxState.Unchecked)
    and not ThemeManager.Designing and not IsReading then
      begin
        FAnimationStatus := 0;
        FAnimateTimer.Enabled := true;
      end
        else
          FAnimationStatus := 100;

  // Set
  FState := Value;
  if Assigned(OnChangeValue) then
    OnChangeValue(Self);

  // Draw
  StandardUpdateDraw;
end;

procedure FXCheckBox.SetText(const Value: string);
begin
  if FText = Value then
    Exit;

  FText := Value;
  StandardUpdateLayout;
end;

procedure FXCheckBox.SetTextSpacing(const Value: Integer);
begin
  if Value = FTextSpacing then
    Exit;

  FTextSpacing := Value;
  StandardUpdateLayout;
end;

procedure FXCheckBox.SetWordWrap(const Value: boolean);
begin
  if FWordWrap = Value then
    Exit;

  FWordWrap := Value;
  StandardUpdateLayout;
end;

procedure FXCheckBox.SetChecked(const Value: Boolean);
begin
  if Value then
    State := FXCheckBoxState.Checked
  else
    State := FXCheckBoxState.Unchecked;
end;

procedure FXCheckBox.SetImage(const Value: FXIconSelect);
begin
  if FImage = Value then
    Exit;

  FImage := Value;
  StandardUpdateLayout;
end;

procedure FXCheckBox.SetImageScale(const Value: single);
begin
  if FImageScale = Value then
    Exit;

  FImageScale := Value;
  StandardUpdateLayout;
end;

procedure FXCheckBox.SetLayout(const Value: FXDrawLayout);
begin
  if FLayout = Value then
    Exit;

  FLayout := Value;
  StandardUpdateLayout;
end;

function FXCheckBox.GetChecked;
begin
  Result := State <> FXCheckBoxState.Unchecked;
end;

function FXCheckBox.GetTextHeight: integer;
begin
  with Canvas do
    begin
      Font.Assign(Self.Font);

      Result := TextHeight(TEXT_SIZE_COMPARER)
    end;
end;

procedure FXCheckBox.Click;
begin
  inherited;

  if not Enabled then exit;

  ProgressState;
end;

constructor FXCheckBox.Create(aOwner: TComponent);
begin
  inherited;
  FIconFont := TFont.Create;
  FIconFont.Name := ThemeManager.IconFont;
  FIconFont.Size := 16;
  FAnimationEnabled := true;

  FAnimateTimer := TTimer.Create(nil);
  with FAnimateTimer do
    begin
      Enabled := false;
      Interval := 1;
      OnTimer := AnimationProgress;
    end;

  FAllowGrayed := false;
  FState := FXCheckBoxState.Unchecked;
  FTextSpacing := CHECKBOX_TEXT_SPACE;
  FAutomaticMouseCursor := false;
  AutoFocusLine := true;
  BufferedComponent := true;
  FWordWrap := true;

  // Icon
  FImage := FXIconSelect.Create(Self);
  FImageScale := GENERAL_IMAGE_SCALE;

  // Custom Color
  FCustomColors := FXColorSets.Create(Self);
  FIconAccentColors := FXSingleColorStateSet.Create;

  FDrawColors := FXCompleteColorSet.Create;

  FText := 'Fluent Checkbox';

  // Sizing
  Height := 30;
  Width := 180;
end;

destructor FXCheckBox.Destroy;
begin
  FIconFont.Free;
  FreeAndNil( FImage );
  FreeAndNil( FCustomColors );
  FreeAndNil( FDrawColors );
  FreeAndNil( FIconAccentColors );
  inherited;
end;

procedure FXCheckBox.AnimationProgress(Sender: TObject);
begin
  // Self
  Inc(FAnimationStatus, 5);

  StandardUpdateDraw;

  if FAnimationStatus >= 100 then
    begin
      FAnimationStatus := 100;
      FAnimateTimer.Enabled := false;
    end;
end;

function FXCheckBox.Background: TColor;
begin
  Result := FDrawColors.Background;
end;

procedure FXCheckBox.PaintBuffer;
var
  AText: string;
  IconFormat: TTextFormat;
  DrawFlags: FXTextFlags;
  P1, P2: TPoint;
  ALine: TLine;
begin
  // Background
  Color := FDrawColors.BackGround;
  PaintBackground;

  // Draw
  with Buffer do
    begin
      //  Draw text
      Brush.Style := bsClear;
      Font.Assign(Self.Font);
      Font.Color := FDrawColors.Foreground;
      DrawFlags := [FXTextFlag.VerticalCenter];

      case FTextLayout of
        FXLayout.Beginning: DrawFlags := DrawFlags + [FXTextFlag.Left];
        FXLayout.Center: DrawFlags := DrawFlags + [FXTextFlag.Center];
        FXLayout.Ending: DrawFlags := DrawFlags + [FXTextFlag.Right];
      end;

      if WordWrap then
        DrawFlags := DrawFlags + [FXTextFlag.WordWrap];
      DrawTextRect(Buffer, Self.TextRect, FText, DrawFlags);

      //  Set Brush Accent Color
      Font.Assign(IconFont);
      Font.Color := FIconAccentColors.GetColor(InteractionState);

      // Paint Image
      if Image.Enabled then
        Image.DrawIcon(Buffer, ImageRect);

      //  Draw icon
      IconFormat := [tfVerticalCenter, tfCenter, tfSingleLine];
      case State of
        FXCheckBoxState.Checked:
          begin
            // Animate
            if FAnimationEnabled then
              begin
                AText := CHECKBOX_OUTLINE;
                TextRect(IconRect, AText, IconFormat);
                Font.Size := Font.Size + 1;
                AText := CHECKBOX_FILL;
                TextRect(IconRect, AText, IconFormat);

                // Fix Size
                IconRect.Offset(0, IconRect.Height div 2 - IconRect.Width div 2);
                IconRect.Height := IconRect.Width;

                // Anim
                P1 := Point(IconRect.CenterPoint.X - trunc(IconRect.Width / 6), IconRect.CenterPoint.Y - trunc(IconRect.Height / 18));
                P2 := Point(IconRect.CenterPoint.X - trunc(IconRect.Width / 12), IconRect.CenterPoint.Y + trunc(IconRect.Height / 10));

                ALine := Line(P1, P2);

                if FAnimationStatus <= 50 then
                  ALine.SetPercentage(FAnimationStatus/50*100);

                GDILine(ALine, GetRGB(FDrawColors.BackGround).MakeGDIPen(1.8));

                if FAnimationStatus > 50 then
                  begin
                    P1 := Point(IconRect.CenterPoint.X + trunc(IconRect.Width / 6), IconRect.CenterPoint.Y - trunc(IconRect.Height / 7));
                    ALine := Line(P2, P1);
                    ALine.SetPercentage((FAnimationStatus-50)/50 * 100);

                    GDILine(ALine, GetRGB(FDrawColors.BackGround).MakeGDIPen(1.8));
                  end;

                //TextOut(0, 0, FAnimationStatus.ToString);
              end
            else
              begin
                AText := CHECKBOX_CHECKED;
                TextRect(IconRect, AText, IconFormat);
              end;
          end;

        FXCheckBoxState.Unchecked:
          begin
            Font.Color := FDrawColors.ForeGround;
            AText := CHECKBOX_OUTLINE;
            TextRect(IconRect, AText, IconFormat);
          end;

        FXCheckBoxState.Grayed:
          begin
            AText := CHECKBOX_OUTLINE;
            TextRect(IconRect, AText, IconFormat);
            AText := CHECKBOX_FILL;
            TextRect(IconRect, AText, IconFormat);

            if FAnimationEnabled then
              begin
                P1 := Point(IconRect.Left + round(3/10*IconRect.Width), IconRect.CenterPoint.Y);
                P2 := Point(IconRect.Right - round(3/10*IconRect.Width)-1, IconRect.CenterPoint.Y);
                ALine := Line(P1, P2);

                GDILine(ALine, GetRGB(FDrawColors.BackGround).MakeGDIPen(1.8));
              end
            else
              begin
                Font.Color := FDrawColors.BackGround;
                AText := CHECKBOX_GRAYED;
                TextRect(IconRect, AText, IconFormat);
              end;
          end;
      end;
    end;

  inherited;
end;

procedure FXCheckBox.ProgressState;
begin
  if AllowGrayed then
    case State of
      FXCheckBoxState.Unchecked:
        State := FXCheckBoxState.Checked;
      FXCheckBoxState.Checked:
        State := FXCheckBoxState.Grayed;
      FXCheckBoxState.Grayed:
        State := FXCheckBoxState.Unchecked;
    end
  else
    case State of
      FXCheckBoxState.Unchecked:
        State := FXCheckBoxState.Checked;
      FXCheckBoxState.Checked:
        State := FXCheckBoxState.Unchecked;
      FXCheckBoxState.Grayed:
        State := FXCheckBoxState.Unchecked;
    end;

  // Notify
  if Assigned(OnChange) then
    OnChange(Self);
end;

end.
