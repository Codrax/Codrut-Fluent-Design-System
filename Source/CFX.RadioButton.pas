unit CFX.RadioButton;

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
  CFX.Types,
  CFX.Linker,
  CFX.Controls;

type
  FXRadioButton = class(FXWindowsControl)
  private
    var DrawRect, IconRect, TextRect, ImageRect: TRect;
    FIconFont: TFont;
    FChecked: boolean;
    FTextSpacing: Integer;
    FOnCheck: TNotifyEvent;
    FCustomColors: FXColorSets;
    FIconAccentColors: FXSingleColorStateSet;
    FText: string;
    FAutomaticMouseCursor: boolean;
    FDrawColors: FXCompleteColorSet;
    FWordWrap: boolean;
    FImage: FXIconSelect;
    FImageScale: single;
    FLayout: FXDrawLayout;
    FTextLayout: FXLayout;

    // Internal
    procedure ImageUpdated(Sender: TObject);

    // Draw functions
    function GetTextHeight: integer;

    // Checked
    procedure SetToChecked;

    // Set properties
    procedure SetText(const Value: string);
    procedure SetWordWrap(const Value: boolean);
    procedure SetTextSpacing(const Value: Integer);
    procedure SetChecked(const Value: Boolean);
    procedure SetImage(const Value: FXIconSelect);
    procedure SetLayout(const Value: FXDrawLayout);
    procedure SetImageScale(const Value: single);

    // Get properties
    function GetChecked: Boolean;

  protected
    procedure PaintBuffer; override;

    //  Internal
    procedure UpdateColors; override;
    procedure UpdateRects; override;

    // Scale
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
    property TextSpacing: Integer read FTextSpacing write SetTextSpacing default RADIO_TEXT_SPACE;
    property Checked: Boolean read GetChecked write SetChecked default false;
    property OnCheck: TNotifyEvent read FOnCheck write FOnCheck;
    property AutomaticCursorPointer: boolean read FAutomaticMouseCursor write FAutomaticMouseCursor default false;

    property Text: string read FText write SetText;
    property WordWrap: boolean read FWordWrap write SetWordWrap default true;
    property Image: FXIconSelect read FImage write SetImage;
    property ImageScale: single read FImageScale write SetImageScale;

    property Layout: FXDrawLayout read FLayout write SetLayout default FXDrawLayout.Left;

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

    // Checked Tag
    function GetCheckedTag: integer;

  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    // Interface
    function Background: TColor; override;
  end;

implementation

procedure FXRadioButton.KeyPress(var Key: Char);
begin
  inherited;
  if (Key = #13) or (Key = #32) then
    if not Checked then
      Checked := true;
end;

procedure FXRadioButton.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Self.FAutomaticMouseCursor then
    if PtInRect(IconRect, Point(X, Y)) then
      Self.Cursor := crHandPoint
    else
      Self.Cursor := crDefault;
end;

procedure FXRadioButton.UpdateColors;
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

procedure FXRadioButton.ImageUpdated(Sender: TObject);
begin
  StandardUpdateLayout;
end;

procedure FXRadioButton.UpdateRects;
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

procedure FXRadioButton.SetText(const Value: string);
begin
  if FText = Value then
    Exit;

  FText := Value;
  StandardUpdateLayout;
end;

procedure FXRadioButton.SetTextSpacing(const Value: Integer);
begin
  if Value = FTextSpacing then
    Exit;

  FTextSpacing := Value;
  StandardUpdateLayout;
end;

procedure FXRadioButton.SetToChecked;
var
  I: Integer;
begin
  // Uncheck Parent control
  if Assigned(Parent) then
    begin
      for I := 0 to Parent.ControlCount-1 do
        if Parent.Controls[I] is FXRadioButton then
          if Parent.Controls[I] <> Self then
            (Parent.Controls[I] as FXRadioButton).Checked := false;
    end;

  // Notify
  if Assigned(OnCheck) and not IsReading then
    OnCheck(Self);
end;

procedure FXRadioButton.SetWordWrap(const Value: boolean);
begin
  if FWordWrap = Value then
    Exit;

  FWordWrap := Value;
  StandardUpdateLayout;
end;

procedure FXRadioButton.ScaleChanged(Scaler: single);
begin
  IconFont.Height := round(IconFont.Height * Scaler);
  FTextSpacing := round(FTextSpacing * Scaler);
  FImageScale := FImageScale * Scaler;
  inherited;
end;

procedure FXRadioButton.SetChecked(const Value: Boolean);
begin
  if Value = FChecked then
    Exit;

  FChecked := Value;
  if Value then
    SetToChecked;

  StandardUpdateLayout;
end;

procedure FXRadioButton.SetImage(const Value: FXIconSelect);
begin
  if FImage = Value then
    Exit;

  FImage := Value;
  StandardUpdateLayout;
end;

procedure FXRadioButton.SetImageScale(const Value: single);
begin
  if FImageScale = Value then
    Exit;

  FImageScale := Value;
  StandardUpdateLayout;
end;

procedure FXRadioButton.SetLayout(const Value: FXDrawLayout);
begin
  if FLayout = Value then
    Exit;

  FLayout := Value;
  StandardUpdateLayout;
end;

function FXRadioButton.GetChecked;
begin
  Result := FChecked;
end;

function FXRadioButton.GetCheckedTag: integer;
var
  I: Integer;
begin
  // Uncheck Parent control
  Result := Tag;
  if Assigned(Parent) then
    begin
      for I := 0 to Parent.ControlCount-1 do
        if Parent.Controls[I] is FXRadioButton then
            if (Parent.Controls[I] as FXRadioButton).Checked then
              Exit( (Parent.Controls[I] as FXRadioButton).Tag );
    end;
end;

function FXRadioButton.GetTextHeight: integer;
begin
  with Canvas do
    begin
      Font.Assign(Self.Font);

      Result := TextHeight(TEXT_SIZE_COMPARER)
    end;
end;

procedure FXRadioButton.Click;
begin
  inherited;
  if not Enabled then
    Exit;

  if not Checked then
    Checked := true;
end;

constructor FXRadioButton.Create(aOwner: TComponent);
begin
  inherited;
  FIconFont := TFont.Create;
  FIconFont.Name := ThemeManager.IconFont;
  FIconFont.Size := 14;

  FChecked := false;
  FTextSpacing := RADIO_TEXT_SPACE;
  FAutomaticMouseCursor := false;
  AutoFocusLine := true;
  BufferedComponent := true;
  FWordWrap := true;

  // Icon
  FImage := FXIconSelect.Create(Self);
  FImageScale := GENERAL_IMAGE_SCALE;
  FImage.OnChange := ImageUpdated;

  // Custom Color
  FCustomColors := FXColorSets.Create(Self);
  FIconAccentColors := FXSingleColorStateSet.Create;

  FDrawColors := FXCompleteColorSet.Create;

  FText := 'Fluent Radio Button';

  // Sizing
  Height := 30;
  Width := 180;
end;

destructor FXRadioButton.Destroy;
begin
  FIconFont.Free;
  FreeAndNil( FCustomColors );
  FreeAndNil( FDrawColors );
  FreeAndNil( FIconAccentColors );
  FreeAndNil( FImage );
  inherited;
end;

function FXRadioButton.Background: TColor;
begin
  Result := FDrawColors.Background;
end;

procedure FXRadioButton.PaintBuffer;
var
  AText: string;
  IconFormat: TTextFormat;
  DrawFlags: FXTextFlags;
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
      if Checked then
        begin
          AText := RADIO_FILL;
          TextRect(IconRect, AText, IconFormat);

          case InteractionState of
            FXControlState.Hover: Font.Size := Font.Size + 2;
            FXControlState.Press: Font.Size := Font.Size - 1;
          end;

          Font.Color := FDrawColors.BackGround;
          AText := RADIO_BULLET;
          TextRect(IconRect, AText, IconFormat);
        end
      else
        begin
          Font.Color := FDrawColors.ForeGround;
          AText := RADIO_OUTLINE;
          TextRect(IconRect, AText, IconFormat);
        end;
    end;

  inherited;
end;

end.
