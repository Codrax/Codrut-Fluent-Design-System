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
  CFX.UIConsts,
  SysUtils,
  CFX.Classes,
  CFX.Types,
  CFX.Linker,
  CFX.Controls;

type
  FXRadioButton = class(FXWindowsControl, FXControl)
    private
      var IconRect, TextRect, ImageRect: TRect;
      FTextFont, FIconFont: TFont;
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
      FImageSize: integer;
      FLayout: FXDrawLayout;
      FTextLayout: FXLayout;

      //  Internal
      procedure UpdateColors;
      procedure UpdateRects;

      // Set properties
      procedure SetText(const Value: string);
      procedure SetWordWrap(const Value: boolean);
      procedure SetTextSpacing(const Value: Integer);
      procedure SetChecked(const Value: Boolean);
      procedure SetImage(const Value: FXIconSelect);
      procedure SetImageSize(const Value: integer);

      // Checked
      procedure SetToChecked;

      // Get properties
      function GetChecked: Boolean;

      // Handle Messages
      procedure WM_LButtonUp(var Msg: TWMLButtonUp); message WM_LBUTTONUP;
      procedure WMSize(var Message: TWMSize); message WM_SIZE;
      procedure SetLayout(const Value: FXDrawLayout);

    protected
      procedure PaintBuffer; override;
      procedure Resize; override;
      procedure ChangeScale(M, D: Integer{$IF CompilerVersion > 29}; isDpiChange: Boolean{$ENDIF}); override;

      // State
      procedure InteractionStateChanged(AState: FXControlState); override;

      // Key Presses
      procedure KeyPress(var Key: Char); override;

      // Inherited Mouse Detection
      procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

    published
      property CustomColors: FXColorSets read FCustomColors write FCustomColors stored true;
      property IconFont: TFont read FIconFont write FIconFont;
      property TextSpacing: Integer read FTextSpacing write SetTextSpacing default 6;
      property Checked: Boolean read GetChecked write SetChecked default false;
      property OnCheck: TNotifyEvent read FOnCheck write FOnCheck;
      property AutomaticCursorPointer: boolean read FAutomaticMouseCursor write FAutomaticMouseCursor;

      property Text: string read FText write SetText;
      property Font: TFont read FTextFont write FTextFont;
      property WordWrap: boolean read FWordWrap write SetWordWrap default true;
      property Image: FXIconSelect read FImage write SetImage;
      property ImageSize: integer read FImageSize write SetImageSize;
      property Layout: FXDrawLayout read FLayout write SetLayout default FXDrawLayout.Left;

      property Align;
      property TabStop;
      property TabOrder;
      property Hint;
      property ShowHint;
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

      //  Modify default props
      property ParentColor default true;

      // Checked Tag
      function GetCheckedTag: integer;

    public
      constructor Create(aOwner: TComponent); override;
      destructor Destroy; override;

      // Interface
      function IsContainer: Boolean;
      procedure UpdateTheme(const UpdateChildren: Boolean);

      function Background: TColor;
  end;

implementation

procedure FXRadioButton.InteractionStateChanged(AState: FXControlState);
begin
  inherited;
  PaintBuffer;
end;

function FXRadioButton.IsContainer: Boolean;
begin
  Result := false;
end;

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

procedure FXRadioButton.UpdateTheme(const UpdateChildren: Boolean);
begin
  UpdateColors;
  UpdateRects;
  Invalidate;
end;

procedure FXRadioButton.UpdateColors;
var
  AccentColor: TColor;
begin
  FDrawColors.Assign( ThemeManager.SystemColor );

  if not Enabled then
    begin
      FIconAccentColors := FXSingleColorStateSet.Create($808080,
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
        end
      else
        begin
          // Global Colors
          AccentColor := ThemeManager.AccentColor;
          FDrawColors.ForeGround := ThemeManager.SystemColor.ForeGround;

          FDrawColors.BackGround := GetParentBackgroundColor(FDrawColors.BackGround);
        end;

      FIconAccentColors := FXSingleColorStateSet.Create(AccentColor,
                              ChangeColorLight(AccentColor, ACCENT_DIFFERENTIATE_CONST),
                              ChangeColorLight(AccentColor, -ACCENT_DIFFERENTIATE_CONST));
    end;
end;

procedure FXRadioButton.UpdateRects;
var
  AWidth: integer;
begin
  Buffer.Font.Assign(Self.Font);
  AWidth := Buffer.TextWidth(CHECKBOX_OUTLINE);

  case Layout of
    FXDrawLayout.Left: begin
      IconRect := Rect(0, 0, AWidth + TextSpacing * 2, Height);
      TextRect := Rect(IconRect.Right + TextSpacing, 0, Width, Height);
      FTextLayout := FXLayout.Beginning;


      // Image
      if Image.Enabled then
        begin
          ImageRect := Rect(TextRect.Left, 0, TextRect.Left + FImageSize, Height);
          TextRect.Left := ImageRect.Right + TextSpacing;
        end;
    end;

    FXDrawLayout.Right: begin
      IconRect := Rect(Width-AWidth-TextSpacing, 0, Width, Height);
      TextRect := Rect(0, 0, IconRect.Left - TextSpacing * 2, Height);
      FTextLayout := FXLayout.Beginning;

      // Image
      if Image.Enabled then
        begin
          ImageRect := Rect(TextRect.Left, 0, TextRect.Left + FImageSize, Height);
          TextRect.Left := ImageRect.Right + TextSpacing;
        end;
    end;

    FXDrawLayout.Top: begin
      IconRect := Rect(0, 0, Width, AWidth + TextSpacing * 2);
      TextRect := Rect(0, IconRect.Bottom + TextSpacing, Width, Height);
      FTextLayout := FXLayout.Center;

      // Image
      if Image.Enabled then
        begin
          ImageRect := Rect(0, TextRect.Top, Width, TextRect.Top + FImageSize);
          TextRect.Top := ImageRect.Bottom + TextSpacing;
        end;
    end;

    FXDrawLayout.Bottom: begin
      IconRect := Rect(0, Height - AWidth - TextSpacing * 2, Width, Height);
      TextRect := Rect(0, 0, Width, Height - AWidth + TextSpacing * 2);
      FTextLayout := FXLayout.Center;

      // Image
      if Image.Enabled then
        begin
          ImageRect := Rect(0, TextRect.Top, Width, TextRect.Top + FImageSize);
          TextRect.Top := ImageRect.Bottom + TextSpacing;
        end;
    end;
  end;
end;

procedure FXRadioButton.SetText(const Value: string);
begin
  if FText <> Value then
    begin
      FText := Value;

      Invalidate;
    end;
end;

procedure FXRadioButton.SetTextSpacing(const Value: Integer);
begin
  if Value <> FTextSpacing then
    begin
      FTextSpacing := Value;
      UpdateRects;
      Invalidate;
    end;
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
  if FWordWrap <> Value then
    begin
      FWordWrap := Value;

      Invalidate;
    end;
end;

procedure FXRadioButton.SetChecked(const Value: Boolean);
begin
  if Value <> FChecked then
    begin
      FChecked := Value;
      if Value then
        begin
          SetToChecked;
        end;

      Invalidate;
    end;
end;

procedure FXRadioButton.SetImage(const Value: FXIconSelect);
begin
  if FImage <> Value then
    begin
      FImage := Value;

      UpdateRects;
      Invalidate;
    end;
end;

procedure FXRadioButton.SetImageSize(const Value: integer);
begin
  if FImageSize <> Value then
    begin
      FImageSize := Value;

      UpdateRects;
      Invalidate;
    end;
end;

procedure FXRadioButton.SetLayout(const Value: FXDrawLayout);
begin
  if FLayout <> Value then
    begin
      FLayout := Value;

      UpdateRects;
      Invalidate;
    end;
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

constructor FXRadioButton.Create(aOwner: TComponent);
begin
  inherited;
  FIconFont := TFont.Create;
  FIconFont.Name := ThemeManager.IconFont;
  FIconFont.Size := 14;
  FImageSize := 30;

  FTextFont := TFont.Create;
  FTextFont.Name := FORM_FONT_NAME;
  FTextFont.Size := 12;

  FChecked := false;
  FTextSpacing := 6;
  ParentColor := false;
  FAutomaticMouseCursor := true;
  TabStop := true;
  AutoFocusLine := true;
  BufferedComponent := true;
  FWordWrap := true;

  // Icon
  FImage := FXIconSelect.Create(Self);

  // Custom Color
  FCustomColors := FXColorSets.Create(Self);
  FIconAccentColors := FXSingleColorStateSet.Create;

  FDrawColors := FXCompleteColorSet.Create;

  FText := 'Fluent Checkbox';

  // Sizing
  Height := 30;
  Width := 180;

  // Update
  UpdateRects;
  UpdateColors;
end;

destructor FXRadioButton.Destroy;
begin
  FIconFont.Free;
  FTextFont.Free;
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

procedure FXRadioButton.ChangeScale(M, D: Integer{$IF CompilerVersion > 29}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  FIconFont.Height := MulDiv(FIconFont.Height, M, D);
  FTextSpacing := MulDiv(FTextSpacing, M, D);
  UpdateRects;
end;

procedure FXRadioButton.PaintBuffer;
var
  AText: string;
  IconFormat: TTextFormat;
  DrawFlags: FXTextFlags;
begin
  if not ParentColor then
    Color := FDrawColors.Background;
  with Buffer do
    begin
      //  Paint background
      Pen.Style := psClear;
      Brush.Style := bsSolid;
      Brush.Handle := CreateSolidBrushWithAlpha(Color, 255);
      RoundRect(Rect(0, 0, Width, Height), CHECKBOX_BOX_ROUND, CHECKBOX_BOX_ROUND);

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
        begin
          Image.DrawIcon(Buffer, ImageRect);
        end;

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

procedure FXRadioButton.Resize;
begin
  inherited;
  UpdateRects;
end;

procedure FXRadioButton.WMSize(var Message: TWMSize);
begin
  UpdateRects;
  Invalidate;
end;

procedure FXRadioButton.WM_LButtonUp(var Msg: TWMLButtonUp);
begin
  if not Enabled then exit;

  if not Checked then
    Checked := true;
  inherited;
end;

end.