unit CFX.Button;

interface
uses
  Classes,
  Messages,
  Windows,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.ExtCtrls,
  Vcl.Forms,
  Types,
  Threading,
  Math,
  CFX.GDI,
  CFX.Colors,
  CFX.VarHelpers,
  CFX.ThemeManager,
  CFX.Graphics,
  CFX.UIConsts,
  SysUtils,
  CFX.Utilities,
  CFX.Classes,
  CFX.PopupMenu,
  CFX.Types,
  CFX.Linker,
  CFX.Animations,
  CFX.Controls;

type
  FXBeforeModalResult = procedure(Sender: TObject; var SendModal: boolean) of object;

  FXButton = class(FXWindowsControl, FXControl)
    private
      var DrawRect, CaptionRect, IndicatorRect, ImageRect, TheTextRect: TRect;
      FCustomColors: FXCompleteColorSets;
      FCustomButtonColors: FXColorStateSets;
      FDrawColors: FXCompleteColorSet;
      FButtonColors: FXColorStateSet;
      FText: string;
      FChecked: boolean;
      FWordWrap: boolean;
      FImage: FXIconSelect;
      FImageScale: real;
      FImageLayout: FXDrawLayout;
      FVertLayout: FXLayout;
      FHorizLayout: FXLayout;
      FAutomaticMouseCursor: boolean;
      FCancel: boolean;
      FDefault: boolean;
      FButtonKind: FXButtonKind;
      FTextDrawFlags: FXTextFlags;
      FRoundNess: integer;
      FBorderWidth: real;
      FHyperLinkURL: string;
      FDropDown: FXPopupMenu;
      FMargin: integer;
      FStateText: string;
      FStateImage: FXIconSelect;
      FStateEnabled: boolean;
      FStateDuration: integer;
      FAutoStopState: TTimer;
      FAutoStateToggle: boolean;
      FOnCheck: TNotifyEvent;
      FOnOpenLink: TNotifyEvent;
      FOnDropDown: TNotifyEvent;
      FAnimationRunning: boolean;
      FAnimPos: integer;
      FAutoRepeat: TTimer;
      FRepeatWhenPressed: boolean;
      FModalResult: TModalResult;
      FBeforeModal: FXBeforeModalResult;
      FAnimation: boolean;
      FLineWidth: real;
      FDetail: FXDetailType;
      FShowText: boolean;

      //  Internal
      procedure UpdateColors;
      procedure UpdateRects;

      // Set properties
      procedure SetText(const Value: string);
      procedure SetWordWrap(const Value: boolean);
      procedure SetChecked(const Value: Boolean);
      procedure SetImage(const Value: FXIconSelect);
      procedure SetCancel(const Value: boolean);
      procedure SetDefault(const Value: boolean);
      procedure SetImageScale(const Value: real);
      procedure SetHorizLayout(const Value: FXLayout);
      procedure SetImageLayout(const Value: FXDrawLayout);
      procedure SetStateEnabled(const Value: boolean);
      procedure SetStateImage(const Value: FXIconSelect);
      procedure SetStateText(const Value: string);
      procedure SetStateDuration(const Value: integer);
      procedure SetVertLayout(const Value: FXLayout);
      procedure SetDetail(const Value: FXDetailType);
      procedure SetButtonKind(const Value: FXButtonKind);
      procedure SetRoundness(const Value: integer);
      procedure SetMargin(const Value: integer);
      procedure SetLineWidth(const Value: real);
      procedure SetShowText(const Value: boolean);

      // Draw
      function GetTextH: integer;
      function GetTextW: integer;

      // State
      function GetText: string;
      function GetImage: FXIconSelect;

      // Timers
      procedure TimerRepeat(Sender: TObject);
      procedure StateStop(Sender: TObject);

      // Get properties
      function GetChecked: Boolean;

      // Handle Messages
      procedure WMSize(var Message: TWMSize); message WM_SIZE;

    protected
      procedure PaintBuffer; override;
      procedure Resize; override;
      // Scale
      procedure ScaleChanged(Scaler: single); override;

      // State
      procedure InteractionStateChanged(AState: FXControlState); override;

      // Key Presses
      procedure KeyPress(var Key: Char); override;

      // Mouse
      procedure Click; override;
      procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
      procedure MouseUp(Button : TMouseButton; Shift: TShiftState; X, Y : integer); override;
      procedure MouseDown(Button : TMouseButton; Shift: TShiftState; X, Y : integer); override;

    published
      property CustomColors: FXCompleteColorSets read FCustomColors write FCustomColors stored true;
      property CustomButtonColors: FXColorStateSets read FCustomButtonColors write FCustomButtonColors stored true;
      property Checked: Boolean read GetChecked write SetChecked default false;

      property ShowText: boolean read FShowText write SetShowText default true;
      property Text: string read FText write SetText;
      property Font;
      property WordWrap: boolean read FWordWrap write SetWordWrap default true;
      property Image: FXIconSelect read FImage write SetImage;
      property ImageScale: real read FImageScale write SetImageScale;
      property ImageLayout: FXDrawLayout read FImageLayout write SetImageLayout default FXDrawLayout.Left;
      property LayoutHorizontal: FXLayout read FHorizLayout write SetHorizLayout default FXLayout.Center;
      property LayoutVertical: FXLayout read FVertLayout write SetVertLayout default FXLayout.Center;
      property ButtonKind: FXButtonKind read FButtonKind write SetButtonKind default FXButtonKind.Normal;
      property AutomaticCursorPointer: boolean read FAutomaticMouseCursor write FAutomaticMouseCursor default true;
      property Roundness: integer read FRoundness write SetRoundness default BUTTON_ROUNDNESS;
      property HyperLinkURL: string read FHyperLinkURL write FHyperLinkURL;
      property DropDown: FXPopupMenu read FDropDown write FDropDown;
      property Margin: integer read FMargin write SetMargin default 0;
      property RepeatWhenPressed: boolean read FRepeatWhenPressed write FRepeatWhenPressed default false;
      property Animation: boolean read FAnimation write FAnimation default true;
      property Detail: FXDetailType read FDetail write SetDetail default FXDetailType.None;
      property LineWidth: real read FLineWidth write SetLineWidth;

      property StateText: string read FStateText write SetStateText;
      property StateImage: FXIconSelect read FStateImage write SetStateImage;
      property StateEnabled: boolean read FStateEnabled write SetStateEnabled default false;
      property StateDuration: integer read FStateDuration write SetStateDuration default BUTTON_STATE_DURATION;
      property AutoStateToggle: boolean read FAutoStateToggle write FAutoStateToggle default false;

      property OnCheck: TNotifyEvent read FOnCheck write FOnCheck;
      property OnOpenLink: TNotifyEvent read FOnOpenLink write FOnOpenLink;
      property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
      property OnBeforeModalResult: FXBeforeModalResult read FBeforeModal write FBeforeModal;

      // Button
      property Default: boolean read FDefault write SetDefault default false;
      property Cancel: boolean read FCancel write SetCancel default false;
      property ModalResult: TModalResult read FModalResult write FModalResult default mrNone;

      // Properties
      property Align;
      property PaddingFill;
      property Constraints;
      property Anchors;
      property Hint;
      property ShowHint;
      property TabStop;
      property TabOrder;
      property FocusFlags;
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
      function IsContainer: Boolean;
      procedure UpdateTheme(const UpdateChildren: Boolean);

      function Background: TColor;
  end;

implementation

procedure FXButton.InteractionStateChanged(AState: FXControlState);
var
  FAnim: TIntAni;
begin
  inherited;
  Invalidate;

  // Animation
  if FAnimation and not Creating and not Destroyed and not FAnimationRunning then
    begin
      FAnim := TIntAni.Create;

      FAnim.Duration := 10;
      FAnim.Step := 10;
      FAnim.StartValue := 0;
      FAnim.DeltaValue := 255;

      FAnim.OnSync := procedure(Value: integer)
      begin
        FAnimPos := trunc(FAnim.Percent * 255);

        if Destroyed then
          begin
            FAnim.Terminate;
          end
        else
          Invalidate;
      end;

      FAnim.OnDone := procedure
      begin
        if not Destroyed then
          FAnimationRunning := false;
      end;

      FAnim.FreeOnTerminate := true;
      FAnimationRunning := true;
      FAnim.Start;
    end;
end;

function FXButton.IsContainer: Boolean;
begin
  Result := false;
end;

procedure FXButton.KeyPress(var Key: Char);
begin
  inherited;
  if (Key = #13) or (Key = #32) then
    begin
      PreviousInteractionState := FXControlState.Press;
      Click;
      SetNewInteractionState(FXControlState.None, true, false);
    end;
end;

procedure FXButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: integer);
begin
  inherited;
  if RepeatWhenPressed then
    begin
      FAutoRepeat.Interval := REPEAT_START_DELAY;
      FAutoRepeat.Enabled := true;
    end;
end;

procedure FXButton.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

procedure FXButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: integer);
begin
  inherited;
  if RepeatWhenPressed then
    FAutoRepeat.Enabled := false;
end;

procedure FXButton.UpdateTheme(const UpdateChildren: Boolean);
begin
  UpdateColors;
  UpdateRects;
  Invalidate;
end;

procedure FXButton.UpdateColors;
var
  LoadPreset: FXButtonKind;
  AOffset, ASmallOffset: integer;
begin
  FDrawColors.Assign( ThemeManager.SystemColor );

  // Access theme manager
  if FCustomColors.Enabled then
    begin
      // Custom Colors
      FDrawColors.LoadFrom(FCustomColors, ThemeManager.DarkTheme);
      FDrawColors.BackGround := GetParentBackgroundColor(FDrawColors.BackGround);
    end
  else
    begin
      // Global Colors
      FDrawColors.Assign( ThemeManager.SystemColor );

      FDrawColors.BackGround := GetParentBackgroundColor(FDrawColors.BackGround);
    end;

  // Button Colors
  if FCustomButtonColors.Enabled then
    begin
      // Custom Colors
      FButtonColors.LoadFrom(FCustomButtonColors, ThemeManager.DarkTheme);
    end
  else
    begin
      // Button Kind Color Pallete Logic
      LoadPreset := ButtonKind;

      case LoadPreset of
        FXButtonKind.Toggle: begin
          if Checked then
            LoadPreset := FXButtonKind.Accent
          else
            LoadPreset := FXButtonKind.Normal;
        end;
      end;

      // Load from FDrawColors
      AOffset := BUTTON_COLOR_OFFSET;
      ASmallOffset := BUTTON_COLOR_SMALL_OFFSET;

      if not ThemeManager.DarkTheme then
        begin
          AOffset := AOffset * -1;
          ASmallOffset := ASmallOffset * -1;
        end;

      with FButtonColors do
        case LoadPreset of
          FXButtonKind.Normal, FXButtonKind.Dropdown, FXButtonKind.Toggle:
            begin
              if AutomaticCursorPointer and not IsReading then
                Cursor := crDefault;

              FBorderWidth := 1;
              BackgroundNone := ChangeColorLight(FDrawColors.BackGroundInterior, -ASmallOffset);
              BackgroundHover := ChangeColorLight(BackgroundNone, AOffset);
              BackgroundPress := ChangeColorLight(FDrawColors.BackGroundInterior, -AOffset);

              ForeGroundNone := FDrawColors.ForeGround;
              ForeGroundHover := FDrawColors.ForeGround;
              ForeGroundPress := ColorBlend(FDrawColors.ForeGround, BackgroundNone, BUTTON_BLEND_FADE);
            end;

          FXButtonKind.Flat:
            begin
              if AutomaticCursorPointer and not IsReading then
                Cursor := crDefault;

              FBorderWidth := 0;
              BackgroundNone := FDrawColors.BackGround;
              BackgroundHover := ChangeColorLight(BackgroundNone, AOffset);
              BackgroundPress := ChangeColorLight(FDrawColors.BackGroundInterior, -AOffset);

              ForeGroundNone := FDrawColors.ForeGround;
              ForeGroundHover := FDrawColors.ForeGround;
              ForeGroundPress := ColorBlend(FDrawColors.ForeGround, BackgroundNone, BUTTON_BLEND_FADE);
            end;

          FXButtonKind.Accent:
            begin
              if AutomaticCursorPointer and not IsReading then
                Cursor := crDefault;

              FBorderWidth := 1;
              BackgroundNone := FDrawColors.Accent;
              BackgroundHover := ChangeColorLight(FDrawColors.Accent, -AOffset);
              BackgroundPress := ChangeColorLight(FDrawColors.Accent, -(ASmallOffset+AOffset));

              ForeGroundNone := FDrawColors.BackGround;
              ForeGroundHover := FDrawColors.BackGround;
              ForeGroundPress := ColorBlend(FDrawColors.BackGround, FDrawColors.Accent, BUTTON_BLEND_FADE);
            end;

          FXButtonKind.Link: begin
            with FButtonColors do
              if AutomaticCursorPointer and not IsReading then
                Cursor := crHandPoint;

              FBorderWidth := 0;
              BackgroundNone := FDrawColors.BackGround;
              BackgroundHover := ChangeColorLight(FDrawColors.BackGround, AOffset);
              BackgroundPress := ChangeColorLight(BackgroundNone, -ASmallOffset);

              ForeGroundNone := FDrawColors.Accent;
              ForeGroundHover := FDrawColors.Accent;
              ForeGroundPress := ChangeColorLight(FDrawColors.Accent, -AOffset);
            end;
        end;
    end;

  // Disabled
  if not Enabled then
    begin
      FDrawColors.Accent := GetColorGrayscale(FDrawColors.Accent);
      FButtonColors.BackgroundNone := GetColorGrayscale(FButtonColors.BackgroundNone);
      FButtonColors.ForegroundNone := GetColorGrayscale(FButtonColors.ForegroundNone);
      FButtonColors.ForegroundNone := ColorBlend(FButtonColors.BackgroundNone, FButtonColors.ForegroundNone, BUTTON_BLEND_FADE)
    end
end;

procedure FXButton.UpdateRects;
var
  AMargin, ATextHeight, ATextWidth, ATextLinesCount, ASize: integer;
  ALeft, ATop: integer;
begin
  if Parent = nil then
    Exit;

  DrawRect := GetClientRect;

  // Get Data
  ATextHeight := GetTextH;
  ATextWidth := GetTextW;
  AMargin := -(BUTTON_MARGIN + FMargin + round(FLineWidth));

  // Button Kind
  case ButtonKind of
    FXButtonKind.Normal, FXButtonKind.Accent, FXButtonKind.Toggle, FXButtonKind.Link, FXButtonKind.Flat:
      begin
        CaptionRect := DrawRect;
        CaptionRect.Inflate(AMargin, AMargin);
      end;

    FXButtonKind.Dropdown:
      begin
        CaptionRect := DrawRect;
        CaptionRect.Inflate(AMargin, AMargin);
        IndicatorRect := CaptionRect;

        CaptionRect.Right := CaptionRect.Right - Max(ATextHeight, BUTTON_ICON_SPACE) - BUTTON_MARGIN;

        IndicatorRect.Left := CaptionRect.Right + BUTTON_MARGIN;
      end;
  end;

  // Make Rectangles
  if Image.Enabled then
    begin
      // Flags
      if WordWrap then
        FTextDrawFlags := [FXTextFlag.WordWrap]
      else
        FTextDrawFlags := [];

      // Data
      TheTextRect := CaptionRect;
      ASize := round(ATextHeight * ImageScale);

      // Layout
      ALeft := 0;
      ATop := 0;
      case ImageLayout of
        FXDrawLayout.Left, FXDrawLayout.Right:
          begin
            // Flags
            FTextDrawFlags := FTextDrawFlags + [FXTextFlag.Left];

            // Start Pos
            if ATextWidth > CaptionRect.Width-ASize then
              begin
                ALeft := CaptionRect.Left;
                ATextWidth := CaptionRect.Width-ASize;
              end
            else
              case FHorizLayout of
                FXLayout.Beginning: ALeft := CaptionRect.Left;
                FXLayout.Center: ALeft := CaptionRect.Left + round((CaptionRect.Width - ATextWidth - ASize) /2);
                FXLayout.Ending: ALeft := CaptionRect.Left + CaptionRect.Width - ATextWidth - ASize;
              end;

            // Split
            ImageRect := CaptionRect;
            TheTextRect := CaptionRect;
            if ImageLayout = FXDrawLayout.Left then
              begin
                ImageRect.Left := ALeft;
                TheTextRect.Left := ALeft+ASize+BUTTON_MARGIN;
              end
            else
              begin
                TheTextRect.Left := ALeft;
                ImageRect.Left := ALeft+ATextWidth+BUTTON_MARGIN;
              end;

            // Size
            ImageRect.Width := ASize;
            TheTextRect.Width := ATextWidth;

            // Vertical Align
            case FVertLayout of
              FXLayout.Beginning:
                begin
                  FTextDrawFlags := FTextDrawFlags + [FXTextFlag.Top];
                  ImageRect.Height := ASize;
                end;

              FXLayout.Center:
                begin
                  FTextDrawFlags := FTextDrawFlags + [FXTextFlag.VerticalCenter];
                  ImageRect.Height := ASize;
                  ImageRect.Offset(0, (CaptionRect.Height-ASize) div 2);
                end;

              FXLayout.Ending:
                begin
                  FTextDrawFlags := FTextDrawFlags + [FXTextFlag.Bottom];
                  ImageRect.Height := ASize;
                  ImageRect.Offset(0, CaptionRect.Height-ASize);
                end;
            end;
          end;

        FXDrawLayout.Top, FXDrawLayout.Bottom:
          begin
            if ATextWidth = 0 then
              ATextLinesCount := 0
            else
              ATextLinesCount := ceil(ATextWidth / CaptionRect.Width); (* Use width to calc length *)
            ATextHeight := ATextLinesCount * ATextHeight;

            // Start Pos
            if ATextHeight > CaptionRect.Height-ASize then
              begin
                ATop := CaptionRect.Top;
                ATextHeight := CaptionRect.Height-ASize;
              end
            else
              case FVertLayout of
                FXLayout.Beginning: ATop := CaptionRect.Top;
                FXLayout.Center: ATop := CaptionRect.Top + (CaptionRect.Height - ATextHeight - ASize) div 2;
                FXLayout.Ending: ATop := CaptionRect.Top + CaptionRect.Height - ATextHeight - ASize;
              end;

            // Split
            ImageRect := CaptionRect;
            TheTextRect := CaptionRect;
            if ImageLayout = FXDrawLayout.Top then
              begin
                FTextDrawFlags := FTextDrawFlags + [FXTextFlag.Top];
                ImageRect.Top := ATop;
                TheTextRect.Top := ATop+ASize+BUTTON_MARGIN;
              end
            else
              begin
                FTextDrawFlags := FTextDrawFlags + [FXTextFlag.Bottom];
                TheTextRect.Top := ATop;
                ImageRect.Top := ATop+ATextHeight+BUTTON_MARGIN;
              end;

            // Size
            ImageRect.Height := ASize;
            TheTextRect.Height := ATextHeight;

            // Horizontal Align
            case FHorizLayout of
              FXLayout.Beginning:
                begin
                  FTextDrawFlags := FTextDrawFlags + [FXTextFlag.Left];
                  ImageRect.Width := ASize;
                end;

              FXLayout.Center:
                begin
                  FTextDrawFlags := FTextDrawFlags + [FXTextFlag.Center];
                  ImageRect.Width := ASize;
                  ImageRect.Offset((CaptionRect.Width-ASize) div 2, 0);
                end;

              FXLayout.Ending:
                begin
                  FTextDrawFlags := FTextDrawFlags + [FXTextFlag.Right];
                  ImageRect.Width := ASize;
                  ImageRect.Offset(CaptionRect.Width-ASize, 0);
                end;
            end;
          end;
      end
    end
  else
    begin
      TheTextRect := CaptionRect;

      FTextDrawFlags := [FXTextFlag.WordWrap, FXTextFlag.VerticalCenter];

      // Text Align
      case FHorizLayout of
        FXLayout.Beginning: FTextDrawFlags := FTextDrawFlags + [FXTextFlag.Left];
        FXLayout.Center: FTextDrawFlags := FTextDrawFlags + [FXTextFlag.Center];
        FXLayout.Ending: FTextDrawFlags := FTextDrawFlags + [FXTextFlag.Right];
      end;
    end;
end;

procedure FXButton.SetText(const Value: string);
begin
  if FText <> Value then
    begin
      FText := Value;

      UpdateRects;
      Invalidate;
    end;
end;

procedure FXButton.SetVertLayout(const Value: FXLayout);
begin
  if FVertLayout <> Value then
    begin
      FVertLayout := Value;

      UpdateRects;
      Invalidate;
    end;
end;

procedure FXButton.SetWordWrap(const Value: boolean);
begin
  if FWordWrap <> Value then
    begin
      FWordWrap := Value;

      UpdateRects;
      Invalidate;
    end;
end;

procedure FXButton.StateStop(Sender: TObject);
begin
  StateEnabled := false;
  FAutoStopState.Enabled := false;
end;

procedure FXButton.TimerRepeat(Sender: TObject);
begin
  Click;
  FAutoRepeat.Interval := HOLD_REPEAT_INTERVAL;

  if not RepeatWhenPressed or (InteractionState <> FXControlState.Press) then
    FAutoRepeat.Enabled := false;

  Invalidate;
end;

procedure FXButton.ScaleChanged(Scaler: single);
begin
  inherited;
  FImageScale := FImageScale * Scaler;
  FLineWidth := FLineWidth * Scaler;
  FMargin := round(FMargin * Scaler);
  FBorderWidth := FBorderWidth * Scaler;

  UpdateRects;
end;

procedure FXButton.SetButtonKind(const Value: FXButtonKind);
begin
  if FButtonKind <> Value then
    begin
      FButtonKind := Value;

      UpdateColors;
      UpdateRects;
      Invalidate;
    end;
end;

procedure FXButton.SetCancel(const Value: boolean);
begin
  FCancel := Value;
end;

procedure FXButton.SetChecked(const Value: Boolean);
begin
  if Value <> FChecked then
    begin
      FChecked := Value;

      UpdateColors;
      Invalidate;
    end;
end;

procedure FXButton.SetDefault(const Value: boolean);
begin
  FDefault := Value;
end;

procedure FXButton.SetDetail(const Value: FXDetailType);
begin
  if FDetail <> Value then
    begin
      FDetail := Value;

      Invalidate;
    end;
end;

procedure FXButton.SetHorizLayout(const Value: FXLayout);
begin
  if FHorizLayout <> Value then
    begin
      FHorizLayout := Value;

      UpdateRects;
      Invalidate;
    end;
end;

procedure FXButton.SetImage(const Value: FXIconSelect);
begin
  if FImage <> Value then
    begin
      FImage := Value;

      UpdateRects;
      Invalidate;
    end;
end;

procedure FXButton.SetImageLayout(const Value: FXDrawLayout);
begin
  if FImageLayout <> Value then
    begin
      FImageLayout := Value;

      UpdateRects;
      Invalidate;
    end;
end;

procedure FXButton.SetImageScale(const Value: real);
begin
  if FImageScale <> Value then
    begin
      FImageScale := Value;

      UpdateRects;
      Invalidate;
    end;
end;

procedure FXButton.SetLineWidth(const Value: real);
begin
  if FLineWidth <> Value then
    begin
      FLineWidth := Value;

      UpdateRects;
      Invalidate;
    end;
end;

procedure FXButton.SetMargin(const Value: integer);
begin
  if FMargin <> Value then
    begin
      FMargin := Value;

      UpdateRects;
      Invalidate;
    end;
end;

procedure FXButton.SetRoundness(const Value: integer);
begin
  if FRoundness <> Value then
    begin
      FRoundness := Value;

      Invalidate;
    end;
end;

procedure FXButton.SetShowText(const Value: boolean);
begin
  if FShowText <> Value then
    begin
      FShowText := Value;

      UpdateRects;
      Invalidate;
    end;
end;

procedure FXButton.SetStateDuration(const Value: integer);
begin
  if FStateDuration <> Value then
    begin
      FStateDuration := Value;

      FAutoStopState.Interval := Value;
    end;
end;

procedure FXButton.SetStateEnabled(const Value: boolean);
begin
  if FStateEnabled <> Value then
    begin
      FStateEnabled := Value;

      FAutoStopState.Enabled := false;
      if Value and (StateDuration <> 0) then
        FAutoStopState.Enabled := true;

      UpdateRects;
      Invalidate;
    end;
end;

procedure FXButton.SetStateImage(const Value: FXIconSelect);
begin
  if FStateImage <> Value then
    begin
      FStateImage := Value;

      if StateEnabled then
        begin
          UpdateRects;
          Invalidate;
        end;
    end;
end;

procedure FXButton.SetStateText(const Value: string);
begin
  FStateText := Value;
  if FStateText <> Value then
    begin
      FStateText := Value;

      if FStateEnabled then
        begin
          UpdateRects;
          Invalidate;
        end;
    end;
end;

function FXButton.GetChecked;
begin
  Result := FChecked;
end;

function FXButton.GetCheckedTag: integer;
var
  I: Integer;
begin
  // Uncheck Parent control
  Result := Tag;
  if Assigned(Parent) then
    begin
      for I := 0 to Parent.ControlCount-1 do
        if Parent.Controls[I] is FXButton then
            if (Parent.Controls[I] as FXButton).Checked then
              Exit( (Parent.Controls[I] as FXButton).Tag );
    end;
end;

function FXButton.GetImage: FXIconSelect;
begin
  if StateEnabled then
    Result := StateImage
  else
    Result := Image;
end;

function FXButton.GetText: string;
begin
  if StateEnabled then
    Result := StateText
  else
    Result := Text;
end;

function FXButton.GetTextH: integer;
begin
  with Canvas do
    begin
      Font.Assign(Self.Font);

      Result := TextHeight(TEXT_SIZE_COMPARER)
    end;
end;

function FXButton.GetTextW: integer;
begin
  with Canvas do
    begin
      Font.Assign(Self.Font);

      if FShowText then
        Result := TextWidth(GetText)
      else
        Result := 0;
    end;
end;

constructor FXButton.Create(aOwner: TComponent);
begin
  inherited;
  FShowText := true;
  FChecked := false;
  AutoFocusLine := true;
  BufferedComponent := true;

  FAnimation := true;
  FAnimationRunning := false;

  FAutomaticMouseCursor := true;
  FButtonKind := FXButtonKind.Normal;
  FImageLayout := FXDrawLayout.Left;
  FHorizLayout := FXLayout.Center;
  FVertLayout := FXLayout.Center;
  FImageScale := BUTTON_IMAGE_SCALE;
  FWordWrap := true;
  FCancel := false;
  FDefault := false;
  FRoundness := BUTTON_ROUNDNESS;
  FMargin := 0;
  FRepeatWhenPressed := false;
  FDetail := FXDetailType.None;
  FLineWidth := BUTTON_LINE_WIDTH;

  FStateDuration := BUTTON_STATE_DURATION;
  FAutoStopState := TTimer.Create(nil);
  FAutoStateToggle := false;
  with FAutoStopState do
    begin
      Enabled := false;
      OnTimer := StateStop;
    end;

  // Repeat
  FAutoRepeat := TTimer.Create(nil);
  with FAutoRepeat do
    begin
      Enabled := false;
      OnTimer := TimerRepeat;
    end;

  // Icon
  FImage := FXIconSelect.Create(Self);
  FStateImage := FXIconSelect.Create(Self);

  // Custom Color
  FCustomColors := FXCompleteColorSets.Create(Self);
  FCustomButtonColors := FXColorStateSets.Create(Self);

  FDrawColors := FXCompleteColorSet.Create;
  FButtonColors := FXColorStateSet.Create;

  FText := 'Button';
  FStateText := 'Success';

  // Sizing
  Height := 35;
  Width := 140;

  // Update
  UpdateRects;
  UpdateColors;
end;

destructor FXButton.Destroy;
begin
  FreeAndNil( FCustomColors );
  FreeAndNil( FCustomButtonColors );
  FreeAndNil( FDrawColors );
  FreeAndNil( FButtonColors );
  FreeAndNil( FImage );
  FreeAndNil( FStateImage );
  FreeAndNil( FAutoStopState );
  FreeAndNil( FAutoRepeat );

  inherited;
end;

function FXButton.Background: TColor;
begin
  Result := FDrawColors.Background;
end;

procedure FXButton.Click;
var
  Form: TCustomForm;
  AllowModal: boolean;
begin
  // Toggle Button
  case ButtonKind of
    FXButtonKind.Toggle:
      begin
        Checked := not Checked;

        UpdateColors;
        Invalidate;

        if Assigned(OnCheck) then
          OnCheck(Self);
      end;

    FXButtonKind.Dropdown: begin
      if Assigned(FDropDown) then
        FDropDown.PopupAtPoint( ClientToScreen(Point(0, Height+BUTTON_MARGIN)) );

      if Assigned(OnDropDown) then
          OnDropDown(Self);
    end;

    FXButtonKind.Link: begin
      if HyperLinkURL <> '' then
        ShellRun( HyperLinkURL );

      if Assigned(OnOpenLink) then
          OnOpenLink(Self);
    end;
  end;

  // State
  if AutoStateToggle then
    if StateDuration <> 0 then
      StateEnabled := true
    else
      StateEnabled := not StateEnabled;

  // Modal Result
  Form := GetParentForm(Self);
  if Form <> nil then
    begin
      AllowModal := true;
      if Assigned(FBeforeModal) then
        FBeforeModal(Self, AllowModal);

      if AllowModal then
        Form.ModalResult := ModalResult;
    end;

  inherited;
end;

procedure FXButton.PaintBuffer;
var
  AText: string;
  FBackground, FForeground: TColor;
  ARect: TRect;
  FPen: TGDIPen;
  AWidth: integer;
begin
  // Background
  Color := FDrawColors.BackGround;
  PaintBackground;

  // Draw
  with Buffer do
    begin
      // Colors
      if FAnimationRunning then
        begin
          FBackground := ColorBlend( FButtonColors.GetColor(False, PreviousInteractionState),
            FButtonColors.GetColor(False, InteractionState), FAnimPos);
          FForeground := ColorBlend( FButtonColors.GetColor(True, PreviousInteractionState),
            FButtonColors.GetColor(True, InteractionState), FAnimPos);
        end
      else
        begin
          FBackground := FButtonColors.GetColor(False, InteractionState);
          FForeground := FButtonColors.GetColor(True, InteractionState);
        end;

      // Background
      ARect := DrawRect;
      ARect.Width := ARect.Width-1;
      ARect.Height := ARect.Height-1;

      AWidth := 0;
      FPen := nil;
      case Detail of
        FXDetailType.None, FXDetailType.Underline: begin
          if FBorderWidth <> 0 then
            FPen := GetRGB(FDrawColors.BackGroundInterior).MakeGDIPen(FBorderWidth);
          AWidth := trunc(FBorderWidth);
        end;
        FXDetailType.Outline: begin
          FPen := GetRGB(FDrawColors.BackGroundInterior).MakeGDIPen(LineWidth);
          AWidth := round(LineWidth);
        end;
      end;

      ARect.Inflate(-trunc(AWidth), -trunc(AWidth));
      if ARect.Width > 0 then
        GDIRoundRect( MakeRoundRect(ARect, Roundness),
          GetRGB(FBackground).MakeGDIBrush, FPen);

      // Text
      if FShowText then
        begin
          AText := GetText;
          Brush.Style := bsClear;
          Font.Assign(Self.Font);
          Font.Color := FForeground;
          DrawTextRect(Buffer, TheTextRect, AText, FTextDrawFlags);
        end;

      // Icon
      case ButtonKind of
        FXButtonKind.Dropdown:
          begin
            Brush.Style := bsClear;
            Font.Assign(Self.Font);
            Font.Color := FForeground;
            Font.Name := ThemeManager.IconFont;
            Font.Height := ThemeManager.FormFontHeight;
            AText := #$E70D;

            TextRect(IndicatorRect, AText, [tfSingleLine, tfVerticalCenter, tfCenter]);
          end;
      end;

      // Paint Image
      if GetImage.Enabled then
        begin
          Brush.Style := bsClear;
          Font.Assign(Self.Font);
          Font.Color := FForeground;
          GetImage.DrawIcon(Buffer, ImageRect);
        end;
    end;

  inherited;
end;

procedure FXButton.Resize;
begin
  inherited;
  UpdateRects;
end;

procedure FXButton.WMSize(var Message: TWMSize);
begin
  UpdateRects;
  Invalidate;
end;

end.
