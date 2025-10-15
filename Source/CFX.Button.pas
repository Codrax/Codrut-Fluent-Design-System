unit CFX.Button;

interface
uses
  Classes,
  Winapi.Messages,
  Winapi.Windows,
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
  CFX.Constants,
  CFX.Animation.Component,
  CFX.Animation.Main,
  SysUtils,
  CFX.Utilities,
  CFX.Classes,
  CFX.PopupMenu,
  CFX.Types,
  CFX.Linker,
  CFX.Controls;

type
  FXBeforeModalResult = procedure(Sender: TObject; var SendModal: boolean) of object;

  FXCustomButton = class(FXWindowsControl)
  private
    const
      KEY_PRESS_KEYS = [13, 32];
    procedure SetLeftAccentPill(const Value: boolean);

    var DrawRect, MainRect, CaptionRect, IndicatorRect, ImageRect, TheTextRect: TRect;
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
    FVertLayout: TLayout;
    FHorizLayout: TLayout;
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
    FOnStateTimeEnded: TNotifyEvent;
    FAnim: FXIntAnim;
    FAnimPos: integer;
    FAutoRepeat: TTimer;
    FRepeatWhenPressed: boolean;
    FModalResult: TModalResult;
    FBeforeModal: FXBeforeModalResult;
    FAnimation: boolean;
    FLineWidth: real;
    FDetail: FXDetailType;
    FLeftAccentPill: boolean;
    FShowText: boolean;
    FArrowOffset: integer;
    FAutomaticCheck: boolean;

    // Draw
    function GetTextH: integer;
    function GetTextW: integer;

    // State
    function GetText: string;
    function GetImage: FXIconSelect;

    // Timers
    procedure TimerRepeat(Sender: TObject);
    procedure StateStop(Sender: TObject);

    // Anim
    procedure DoAnimationStep(Sender: TObject; Step, TotalSteps: integer);

    // Update
    procedure ImageUpdated(Sender: TObject);

    // Set properties
    procedure SetText(const Value: string);
    procedure SetWordWrap(const Value: boolean);
    procedure SetChecked(const Value: Boolean);
    procedure SetImage(const Value: FXIconSelect);
    procedure SetCancel(const Value: boolean);
    procedure SetDefault(const Value: boolean);
    procedure SetImageScale(const Value: real);
    procedure SetHorizLayout(const Value: TLayout);
    procedure SetImageLayout(const Value: FXDrawLayout);
    procedure SetStateEnabled(const Value: boolean);
    procedure SetStateImage(const Value: FXIconSelect);
    procedure SetStateText(const Value: string);
    procedure SetStateDuration(const Value: integer);
    procedure SetVertLayout(const Value: TLayout);
    procedure SetDetail(const Value: FXDetailType);
    procedure SetButtonKind(const Value: FXButtonKind);
    procedure SetRoundness(const Value: integer);
    procedure SetMargin(const Value: integer);
    procedure SetLineWidth(const Value: real);
    procedure SetShowText(const Value: boolean);

    // Get properties
    function GetChecked: Boolean;

  protected
    procedure PaintBuffer; override;

    //  Internal
    procedure UpdateColors; override;
    procedure UpdateRects; override;

    // Scale
    procedure ScaleChanged(Scaler: single); override;

    // State
    procedure InteractionStateChanged(AState: FXControlState); override;

    // Key Presses
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;

    // Mouse
    procedure Click; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button : TMouseButton; Shift: TShiftState; X, Y : integer); override;
    procedure MouseDown(Button : TMouseButton; Shift: TShiftState; X, Y : integer); override;

    // Properties
    property Checked: Boolean read GetChecked write SetChecked default false;

    property ShowText: boolean read FShowText write SetShowText default true;
    property Image: FXIconSelect read FImage write SetImage;
    property ImageScale: real read FImageScale write SetImageScale;
    property ImageLayout: FXDrawLayout read FImageLayout write SetImageLayout default FXDrawLayout.Left;
    property LayoutHorizontal: TLayout read FHorizLayout write SetHorizLayout default TLayout.Center;
    property LayoutVertical: TLayout read FVertLayout write SetVertLayout default TLayout.Center;
    property ButtonKind: FXButtonKind read FButtonKind write SetButtonKind default FXButtonKind.Normal;
    property AutomaticCursorPointer: boolean read FAutomaticMouseCursor write FAutomaticMouseCursor default true;

    property HyperLinkURL: string read FHyperLinkURL write FHyperLinkURL;
    property DropDown: FXPopupMenu read FDropDown write FDropDown;
    property Margin: integer read FMargin write SetMargin default 0;
    property RepeatWhenPressed: boolean read FRepeatWhenPressed write FRepeatWhenPressed default false;
    property AutomaticCheck: boolean read FAutomaticCheck write FAutomaticCheck default true;

    property StateText: string read FStateText write SetStateText;
    property StateImage: FXIconSelect read FStateImage write SetStateImage;
    property StateEnabled: boolean read FStateEnabled write SetStateEnabled default false;
    property StateDuration: integer read FStateDuration write SetStateDuration default BUTTON_STATE_DURATION;
    property AutoStateToggle: boolean read FAutoStateToggle write FAutoStateToggle default false;

    property OnCheck: TNotifyEvent read FOnCheck write FOnCheck;
    property OnOpenLink: TNotifyEvent read FOnOpenLink write FOnOpenLink;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
    property OnStateTimeEnded: TNotifyEvent read FOnStateTimeEnded write FOnStateTimeEnded;
    property OnBeforeModalResult: FXBeforeModalResult read FBeforeModal write FBeforeModal;

    // Button
    property Default: boolean read FDefault write SetDefault default false;
    property Cancel: boolean read FCancel write SetCancel default false;
    property ModalResult: TModalResult read FModalResult write FModalResult default mrNone;

  published
    // Public props
    property CustomColors: FXCompleteColorSets read FCustomColors write FCustomColors stored true;
    property CustomButtonColors: FXColorStateSets read FCustomButtonColors write FCustomButtonColors stored true;

    property Text: string read FText write SetText;
    property WordWrap: boolean read FWordWrap write SetWordWrap default true;

    property Roundness: integer read FRoundness write SetRoundness default BUTTON_ROUNDNESS;
    property Animation: boolean read FAnimation write FAnimation default true;
    property Detail: FXDetailType read FDetail write SetDetail default FXDetailType.None;
    property LeftAccentPill: boolean read FLeftAccentPill write SetLeftAccentPill default false;
    property LineWidth: real read FLineWidth write SetLineWidth;

    // Properties
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
    property OnDblClick;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property OnMouseUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;

    // Checked Tag
    function GetCheckedTag: integer;

  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    // Interface
    function Background: TColor; override;
  end;

  FXButton = class(FXCustomButton)
  published
    property Checked;

    property ShowText;
    property Image;
    property ImageScale;
    property ImageLayout;
    property LayoutHorizontal;
    property LayoutVertical;
    property ButtonKind;
    property AutomaticCursorPointer;

    property HyperLinkURL;
    property DropDown;
    property Margin;
    property RepeatWhenPressed;
    property AutomaticCheck;

    property StateText;
    property StateImage;
    property StateEnabled;
    property StateDuration;
    property AutoStateToggle;

    property OnCheck;
    property OnOpenLink;
    property OnDropDown;
    property OnBeforeModalResult;

    property Default;
    property Cancel;
    property ModalResult;
  end;

implementation

procedure FXCustomButton.InteractionStateChanged(AState: FXControlState);
begin
  FArrowOffset := 0;

  // Animation
  if FAnimation and not IsReading and not Destroyed and (FAnim <> nil) and not FAnim.Running then
    FAnim.Start;

  // Draw
  inherited;
end;

procedure FXCustomButton.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Key in KEY_PRESS_KEYS then
    SetNewInteractionState(FXControlState.Press, false, false);
end;

procedure FXCustomButton.KeyPress(var Key: Char);
begin
  inherited;
  if (Key = #13) or (Key = #32) then
    begin
      Click;
    end;
end;

procedure FXCustomButton.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Key in KEY_PRESS_KEYS then
    SetNewInteractionState(FXControlState.None, true, false);
end;

procedure FXCustomButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: integer);
begin
  inherited;
  if RepeatWhenPressed then
    begin
      FAutoRepeat.Interval := REPEAT_START_DELAY;
      FAutoRepeat.Enabled := true;
    end;
end;

procedure FXCustomButton.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

procedure FXCustomButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: integer);
begin
  inherited;
  if RepeatWhenPressed then
    FAutoRepeat.Enabled := false;
end;

procedure FXCustomButton.UpdateColors;
var
  LoadPreset: FXButtonKind;
  AOffset, ASmallOffset: integer;
begin
  // Access theme manager
  FDrawColors.Assign( ThemeManager.SystemColor );
  if FCustomColors.Enabled then
    // Custom Colors
    FDrawColors.LoadFrom(FCustomColors, ThemeManager.DarkTheme);

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
        FXButtonKind.Dropdown: begin
          if Checked then
            LoadPreset := FXButtonKind.Accent
          else
            LoadPreset := FXButtonKind.Normal;
        end;

        FXButtonKind.FlatToggle: begin
          if Checked then
            LoadPreset := FXButtonKind.Accent
          else
            LoadPreset := FXButtonKind.Flat;
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
          FXButtonKind.Normal:
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

procedure FXCustomButton.ImageUpdated(Sender: TObject);
begin
  StandardUpdateLayout;
end;

procedure FXCustomButton.UpdateRects;
var
  AMargin, ATextHeight, ATextWidth, ATextLinesCount, ASize: integer;
  ALeft, ATop: integer;
begin
  inherited;

  DrawRect := ClientRect;
  MainRect := ContentRect;

  // Get Data
  ATextHeight := GetTextH;
  ATextWidth := GetTextW;
  AMargin := -(BUTTON_MARGIN + FMargin + round(FLineWidth));

  // Button Kind
  case ButtonKind of
    FXButtonKind.Normal, FXButtonKind.Accent, FXButtonKind.Toggle, FXButtonKind.FlatToggle, FXButtonKind.Link, FXButtonKind.Flat:
      begin
        CaptionRect := MainRect;
        CaptionRect.Inflate(AMargin, AMargin);
      end;

    FXButtonKind.Dropdown:
      begin
        CaptionRect := MainRect;
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
                TLayout.Beginning: ALeft := CaptionRect.Left;
                TLayout.Center: ALeft := CaptionRect.Left + round((CaptionRect.Width - ATextWidth - ASize) /2);
                TLayout.Ending: ALeft := CaptionRect.Left + CaptionRect.Width - ATextWidth - ASize;
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
              TLayout.Beginning:
                begin
                  FTextDrawFlags := FTextDrawFlags + [FXTextFlag.Top];
                  ImageRect.Height := ASize;
                end;

              TLayout.Center:
                begin
                  FTextDrawFlags := FTextDrawFlags + [FXTextFlag.VerticalCenter];
                  ImageRect.Height := ASize;
                  ImageRect.Offset(0, (CaptionRect.Height-ASize) div 2);
                end;

              TLayout.Ending:
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
                TLayout.Beginning: ATop := CaptionRect.Top;
                TLayout.Center: ATop := CaptionRect.Top + (CaptionRect.Height - ATextHeight - ASize) div 2;
                TLayout.Ending: ATop := CaptionRect.Top + CaptionRect.Height - ATextHeight - ASize;
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
              TLayout.Beginning:
                begin
                  FTextDrawFlags := FTextDrawFlags + [FXTextFlag.Left];
                  ImageRect.Width := ASize;
                end;

              TLayout.Center:
                begin
                  FTextDrawFlags := FTextDrawFlags + [FXTextFlag.Center];
                  ImageRect.Width := ASize;
                  ImageRect.Offset((CaptionRect.Width-ASize) div 2, 0);
                end;

              TLayout.Ending:
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

      FTextDrawFlags := [FXTextFlag.WordWrap];

      // Text Align
      case FHorizLayout of
        TLayout.Beginning: FTextDrawFlags := FTextDrawFlags + [FXTextFlag.Left];
        TLayout.Center: FTextDrawFlags := FTextDrawFlags + [FXTextFlag.Center];
        TLayout.Ending: FTextDrawFlags := FTextDrawFlags + [FXTextFlag.Right];
      end;
      case FVertLayout of
        TLayout.Beginning: FTextDrawFlags := FTextDrawFlags + [FXTextFlag.Top];
        TLayout.Center: FTextDrawFlags := FTextDrawFlags + [FXTextFlag.VerticalCenter];
        TLayout.Ending: FTextDrawFlags := FTextDrawFlags + [FXTextFlag.Bottom];
      end;
    end;
end;

procedure FXCustomButton.SetText(const Value: string);
begin
  if FText = Value then
    Exit;

  FText := Value;
  StandardUpdateLayout;
end;

procedure FXCustomButton.SetVertLayout(const Value: TLayout);
begin
  if FVertLayout = Value then
    Exit;

  FVertLayout := Value;
  StandardUpdateLayout;
end;

procedure FXCustomButton.SetWordWrap(const Value: boolean);
begin
  if FWordWrap = Value then
    Exit;

  FWordWrap := Value;
  StandardUpdateLayout;
end;

procedure FXCustomButton.StateStop(Sender: TObject);
begin
  StateEnabled := false;
  FAutoStopState.Enabled := false;
  if Assigned(FOnStateTimeEnded) then
    FOnStateTimeEnded(Self);
end;

procedure FXCustomButton.TimerRepeat(Sender: TObject);
begin
  Click;
  FAutoRepeat.Interval := HOLD_REPEAT_INTERVAL;

  if not RepeatWhenPressed or (InteractionState <> FXControlState.Press) then
    FAutoRepeat.Enabled := false;

  Redraw;
end;

procedure FXCustomButton.ScaleChanged(Scaler: single);
begin
  //FImageScale := FImageScale * Scaler; // the image scale does NOT need to be changed!
  FLineWidth := FLineWidth * Scaler;
  FMargin := round(FMargin * Scaler);
  FBorderWidth := FBorderWidth * Scaler;

  inherited;
end;

procedure FXCustomButton.SetButtonKind(const Value: FXButtonKind);
begin
  if FButtonKind = Value then
    Exit;

  FButtonKind := Value;
  StandardUpdateComplete;
end;

procedure FXCustomButton.SetCancel(const Value: boolean);
begin
  FCancel := Value;
end;

procedure FXCustomButton.SetChecked(const Value: Boolean);
begin
  if Value <> FChecked then
    begin
      FChecked := Value;

      UpdateColors;
      Redraw;
    end;
end;

procedure FXCustomButton.SetDefault(const Value: boolean);
begin
  FDefault := Value;
end;

procedure FXCustomButton.SetDetail(const Value: FXDetailType);
begin
  if FDetail = Value then
    Exit;
    
  FDetail := Value;
  StandardUpdateDraw;
end;

procedure FXCustomButton.SetHorizLayout(const Value: TLayout);
begin
  if FHorizLayout = Value then
    Exit;
    
  FHorizLayout := Value;
  StandardUpdateLayout;
end;

procedure FXCustomButton.SetImage(const Value: FXIconSelect);
begin
  if FImage = Value then
    Exit;

  FImage := Value;
  StandardUpdateLayout;
end;

procedure FXCustomButton.SetImageLayout(const Value: FXDrawLayout);
begin
  if FImageLayout = Value then
    Exit;
    
  FImageLayout := Value;
  StandardUpdateLayout;
end;

procedure FXCustomButton.SetImageScale(const Value: real);
begin
  if FImageScale = Value then
    Exit;

  FImageScale := Value;
  StandardUpdateLayout;
end;

procedure FXCustomButton.SetLeftAccentPill(const Value: boolean);
begin
  if FLeftAccentPill = Value then
    Exit;

  FLeftAccentPill := Value;
  StandardUpdateDraw;
end;

procedure FXCustomButton.SetLineWidth(const Value: real);
begin
  if FLineWidth = Value then
    Exit;

  FLineWidth := Value;
  StandardUpdateLayout;
end;

procedure FXCustomButton.SetMargin(const Value: integer);
begin
  if FMargin = Value then
    Exit;

  FMargin := Value;
  StandardUpdateLayout;
end;

procedure FXCustomButton.SetRoundness(const Value: integer);
begin
  if FRoundness = Value then
    Exit;

  FRoundness := Value;
  StandardUpdateLayout;
end;

procedure FXCustomButton.SetShowText(const Value: boolean);
begin
  if FShowText = Value then
    Exit;

  FShowText := Value;
  StandardUpdateLayout;
end;

procedure FXCustomButton.SetStateDuration(const Value: integer);
begin
  if FStateDuration = Value then
    Exit;
    
  FStateDuration := Value;
  FAutoStopState.Interval := Value;
end;

procedure FXCustomButton.SetStateEnabled(const Value: boolean);
begin
  if FStateEnabled = Value then begin
    // Reset
    if Value then
      FAutoStopState.ResetTimer;

    Exit;
  end;

  // Set to value
  FStateEnabled := Value;

  FAutoStopState.Enabled := false;
  if Value and (StateDuration <> 0) then
    FAutoStopState.Enabled := true;

  StandardUpdateLayout;
end;

procedure FXCustomButton.SetStateImage(const Value: FXIconSelect);
begin
  if FStateImage = Value then
    Exit;

  FStateImage := Value;
  StandardUpdateLayout;
end;

procedure FXCustomButton.SetStateText(const Value: string);
begin
  if FStateText = Value then
    Exit;

  FStateText := Value;
  StandardUpdateLayout;
end;

function FXCustomButton.GetChecked;
begin
  Result := FChecked;
end;

function FXCustomButton.GetCheckedTag: integer;
var
  I: Integer;
begin
  // Uncheck Parent control
  Result := Tag;
  if Assigned(Parent) then
    begin
      for I := 0 to Parent.ControlCount-1 do
        if Parent.Controls[I] is FXCustomButton then
            if (Parent.Controls[I] as FXCustomButton).Checked then
              Exit( (Parent.Controls[I] as FXCustomButton).Tag );
    end;
end;

function FXCustomButton.GetImage: FXIconSelect;
begin
  if StateEnabled then
    Result := StateImage
  else
    Result := Image;
end;

function FXCustomButton.GetText: string;
begin
  if StateEnabled then
    Result := StateText
  else
    Result := Text;
end;

function FXCustomButton.GetTextH: integer;
begin
  with Buffer do
    begin
      Font.Assign(Self.Font);

      Result := TextHeight(TEXT_SIZE_COMPARER)
    end;
end;

function FXCustomButton.GetTextW: integer;
begin
  with Buffer do
    begin
      Font.Assign(Self.Font);

      if FShowText then
        Result := TextWidth(GetText)
      else
        Result := 0;
    end;
end;

constructor FXCustomButton.Create(aOwner: TComponent);
begin
  inherited;
  FShowText := true;
  FChecked := false;
  AutoFocusLine := true;
  BufferedComponent := true;

  FAnim := FXIntAnim.Create(nil);
  FAnimation := true;

  with FAnim do begin
    OnStep := DoAnimationStep;

    LatencyAdjustments := true;
    LatencyCanSkipSteps := true;

    Duration := 0.15;

    StartValue := 0;
    EndValue := 255;
  end;

  FAutomaticMouseCursor := true;
  FAutomaticCheck := true;
  FButtonKind := FXButtonKind.Normal;
  FImageLayout := FXDrawLayout.Left;
  FHorizLayout := TLayout.Center;
  FVertLayout := TLayout.Center;
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
  FImage.OnChange := ImageUpdated;
  FStateImage := FXIconSelect.Create(Self);
  FImage.OnChange := ImageUpdated;

  // Custom Color
  FCustomColors := FXCompleteColorSets.Create(Self);
  FCustomButtonColors := FXColorStateSets.Create(Self);

  FDrawColors := FXCompleteColorSet.Create;
  FButtonColors := FXColorStateSet.Create;

  FText := 'Button';
  //FStateText := BUTTON_STATE_TEXT;

  // Sizing
  Height := BUTTON_DEFAULT_HEIGHT;
  Width := BUTTON_DEFAULT_WIDTH;
end;

destructor FXCustomButton.Destroy;
begin
  FAnim.Stop;
  FreeAndNil( FAnim );
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

procedure FXCustomButton.DoAnimationStep(Sender: TObject; Step,
  TotalSteps: integer);
begin
  if Destroyed then
    Exit;

  // Update
  FAnimPos := trunc(FAnim.Percent * 255);

  const MaxArrowOffset = GetTextH / 8;

  if (ButtonKind = FXButtonKind.Dropdown) then
    begin
      case PreviousInteractionState of
        FXControlState.Hover: if InteractionState = FXControlState.Press then
          FArrowOffset := round(FAnim.Percent * MaxArrowOffset);

        FXControlState.Press: FArrowOffset := round(MaxArrowOffset - FAnim.Percent * MaxArrowOffset);

        else FArrowOffset := 0;
      end;
    end;

  // Draw
  Redraw;
end;

function FXCustomButton.Background: TColor;
begin
  Result := FDrawColors.Background;
end;

procedure FXCustomButton.Click;
var
  Form: TCustomForm;
  AllowModal: boolean;
begin
  // Toggle Button
  case ButtonKind of
    FXButtonKind.Toggle, FXButtonKind.FlatToggle:
      begin
        if AutomaticCheck then
          Checked := not Checked;

        UpdateColors;
        Redraw;;

        if Assigned(OnCheck) then
          OnCheck(Self);
      end;

    FXButtonKind.Dropdown: begin
      if Assigned(FDropDown) then begin
        FDropDown.MinimumWidth := Self.Width;

        FDropDown.PopupAtPoint( ClientToScreen(Point(0, Height+BUTTON_MARGIN)) );
      end;

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

  // Check freed by form..?
  inherited;
end;

procedure FXCustomButton.PaintBuffer;
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
      if FAnim.Running then
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
        FXDetailType.None: begin
          if FBorderWidth <> 0 then
            FPen := GetRGB(FDrawColors.BackGroundInterior).MakeGDIPen(FBorderWidth);
          AWidth := trunc(FBorderWidth);
        end;
        FXDetailType.Outline: begin
          FPen := GetRGB(FDrawColors.BackGroundInterior).MakeGDIPen(LineWidth);
          AWidth := round(LineWidth);
        end;
        FXDetailType.Underline: begin
          GDIRoundRect(MakeRoundRect(ARect, Roundness), GetRGB(FDrawColors.Accent).MakeGDIBrush, nil);

          // Offset
          ARect.Bottom := round(ARect.Bottom - LineWidth);
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
      { Anim }
      ARect := IndicatorRect;
      ARect.Offset(0, FArrowOffset);

      { Draw }
      case ButtonKind of
        FXButtonKind.Dropdown:
          begin
            Brush.Style := bsClear;
            Font.Assign(Self.Font);
            Font.Color := FForeground;
            Font.Name := ThemeManager.IconFont;
            Font.Height := ThemeManager.FormFontHeight;
            AText := #$E70D;

            TextRect(ARect, AText, [tfSingleLine, tfVerticalCenter, tfCenter]);
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

      // Accemt pill
      if LeftAccentPill then begin
        ARect := DrawRect;
        ARect.Width := 3;
        ARect.Height := round(0.6* ARect.Height);
        ARect.Offset(0, (DrawRect.Height-ARect.Height) div 2);

        GDIRoundRect( MakeRoundRect(ARect, 3),
          GetRGB(FDrawColors.Accent).MakeGDIBrush, nil);
      end;
    end;

  inherited;
end;

end.
