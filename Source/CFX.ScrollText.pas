unit CFX.ScrollText;

interface

uses
  Classes,
  Messages,
  Windows,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.ExtCtrls,
  Types,
  Math,
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
  FXScrollText = class(FXWindowsControl)
  private
    var DrawRect, ImageRect, TextRect: TRect;

    FTextSpacing: Integer;
    FCustomColors: FXColorSets;
    FText: string;
    FDrawColors: FXCompleteColorSet;
    FVertLayout: TLayout;
    FHorzLayout: TLayout;
    FImage: FXIconSelect;
    FImageScale: real;

    FAnimationDelay: integer;
    FAnimationSpeed: integer;

    FSpacePercent: FXPercent;

    FOffset: integer;
    FOffsetEnd: integer;
    FAnimateValue: integer;
    FAnimateMax: integer;
    FAnimateSpace,
    FAnimateWidth: integer;

    FFadeRight,
    FFadeLeft: boolean;
    FFadeSize: integer;

    FAnimateTimer: TTimer;

    // Set properties
    procedure SetText(const Value: string);
    procedure SetTextSpacing(const Value: Integer);
    procedure SetHorzLayout(const Value: TLayout);
    procedure SetVertLayout(const Value: TLayout);
    procedure SetImage(const Value: FXIconSelect);
    procedure SetImageScale(const Value: real);
    procedure SetAnimationDelay(const Value: integer);
    procedure SetAnimationSpeed(const Value: integer);
    procedure SetFadeSize(const Value: integer);

    // Animation
    procedure ResetAnimation;

    procedure AnimationProgress(Sender: TObject);

  protected
    procedure PaintBuffer; override;

    // Internal
    procedure UpdateColors; override;
    procedure UpdateRects; override;

    // Size
    procedure Sized; override;

    // Scale
    procedure ScaleChanged(Scaler: single); override;

    // State
    procedure InteractionStateChanged(AState: FXControlState); override;

  published
    property CustomColors: FXColorSets read FCustomColors write FCustomColors stored true;
    property TextSpacing: Integer read FTextSpacing write SetTextSpacing default CHECKBOX_TEXT_SPACE;

    property Text: string read FText write SetText;
    property SpacePercent: FXPercent read FSpacePercent write FSpacePercent;
    property FadeSize: integer read FFadeSize write SetFadeSize default SCROLL_TEXT_FADE_SIZE;

    property Image: FXIconSelect read FImage write SetImage;
    property ImageScale: real read FImageScale write SetImageScale;

    property AnimationDelay: integer read FAnimationDelay write SetAnimationDelay;
    property AnimationSpeed: integer read FAnimationSpeed write SetAnimationSpeed;

    property LayoutHorizontal: TLayout read FHorzLayout write SetHorzLayout default TLayout.Beginning;
    property LayoutVertical: TLayout read FVertLayout write SetVertLayout default TLayout.Center;

    property Align;
    property Font;
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

procedure FXScrollText.UpdateColors;
begin
  // Access theme manager
  FDrawColors.Assign( ThemeManager.SystemColor );
  if FCustomColors.Enabled then
    // Custom Colors
    FDrawColors.LoadFrom(FCustomColors, ThemeManager.DarkTheme);
end;

procedure FXScrollText.UpdateRects;
var
  ASize, AWidth, SpaceLeft: integer;
  TextSpace: integer;
begin
  // Rect
  DrawRect := GetClientRect;

  // Get Sizes
  with Buffer do
    begin
      Font.Assign(Self.Font);

      ASize := TextHeight(TEXT_SIZE_COMPARER);
      AWidth := TextWidth(FText);
    end;

  // Image
  if Image.Enabled then
    begin
      ASize := round(ASize * FImageScale);
      TextSpace := TextSpacing;
    end
  else
    begin
      ASize := 0;
      TextSpace := 0;
    end;

  // Rects
  ImageRect := Rect(DrawRect.Left, DrawRect.Top,
    DrawRect.Left+ASize + TextSpace * 2, DrawRect.Bottom);

  TextRect := Rect(ImageRect.Right + TextSpace, DrawRect.Top, DrawRect.Right, DrawRect.Bottom);
  TextRect.Width := ImageRect.Right + TextSpace + AWidth;

  // Image alignment
  case FVertLayout of
    TLayout.Beginning: ImageRect.Height := ImageRect.Width;
    TLayout.Ending: ImageRect.Top := ImageRect.Bottom-ImageRect.Width;
  end;

  // Offset
  SpaceLeft := DrawRect.Right - TextRect.Right;
  if SpaceLeft > 0 then
    case FHorzLayout of
      TLayout.Center: begin
        TextRect.Offset(SpaceLeft div 2, 0);
        ImageRect.Offset(SpaceLeft div 2, 0);
      end;
      TLayout.Ending: begin
        TextRect.Offset(SpaceLeft, 0);
        ImageRect.Offset(SpaceLeft, 0);
      end;
    end;

  // Animation Settings
  FAnimateWidth := AWidth;
  FAnimateSpace := FSpacePercent.OfNumberInt(DrawRect.Right);
  FAnimateMax := AWidth + FAnimateSpace;

  // Enable
  FAnimateTimer.Enabled := (SpaceLeft <= 0) and not IsDesigning;

  // Fades
  FFadeRight := FAnimateTimer.Enabled;
end;

procedure FXScrollText.ScaleChanged(Scaler: single);
begin
  FTextSpacing := round(FTextSpacing * Scaler);
  inherited;
end;

procedure FXScrollText.SetAnimationDelay(const Value: integer);
begin
  if FAnimationDelay = Value then
    Exit;

  FAnimationDelay := Value;

  if FAnimateValue < 0 then
    FAnimateValue := -Value;
end;

procedure FXScrollText.SetAnimationSpeed(const Value: integer);
begin
  if FAnimationSpeed = Value then
    Exit;

  FAnimationSpeed := Value;
end;

procedure FXScrollText.SetFadeSize(const Value: integer);
begin
  if FFadeSize = Value then
    Exit;

  FFadeSize := Value;
  StandardUpdateDraw;
end;

procedure FXScrollText.SetHorzLayout(const Value: TLayout);
begin
  if FHorzLayout = Value then
    Exit;

  FHorzLayout := Value;
  StandardUpdateLayout;
end;

procedure FXScrollText.SetImage(const Value: FXIconSelect);
begin
  if FImage = Value then
    Exit;

  FImage := Value;
  StandardUpdateLayout;
end;

procedure FXScrollText.SetImageScale(const Value: real);
begin
  if FImageScale = Value then
    Exit;

  FImageScale := Value;
  StandardUpdateLayout;
end;

procedure FXScrollText.SetText(const Value: string);
begin
  if FText = Value then
    Exit;

  FText := Value;

  ResetAnimation;
  StandardUpdateLayout;
end;

procedure FXScrollText.SetTextSpacing(const Value: Integer);
begin
  if Value = FTextSpacing then
    Exit;

  FTextSpacing := Value;
  ResetAnimation;
  StandardUpdateLayout;
end;

procedure FXScrollText.SetVertLayout(const Value: TLayout);
begin
  if FVertLayout = Value then
    Exit;

  FVertLayout := Value;
  StandardUpdateLayout;
end;

procedure FXScrollText.Sized;
begin
  inherited;
  ResetAnimation;
end;

constructor FXScrollText.Create(aOwner: TComponent);
begin
  inherited;
  TabStop := false;

  FAnimateTimer := TTimer.Create(nil);
  with FAnimateTimer do
    begin
      Enabled := false;
      Interval := 1;
      OnTimer := AnimationProgress;
    end;

  FTextSpacing := CHECKBOX_TEXT_SPACE;
  AutoFocusLine := true;
  BufferedComponent := true;
  FHorzLayout := TLayout.Beginning;
  FVertLayout := TLayout.Center;
  FSpacePercent := SCROLL_TEXT_SPACE;
  FAnimationDelay := SCROLL_TEXT_DELAY;
  FAnimationSpeed := SCROLL_TEXT_SPEED;
  FFadeSize := SCROLL_TEXT_FADE_SIZE;

  // Image
  FImage := FXIconSelect.Create(Self);
  FImageScale := NORMAL_IMAGE_SCALE;

  // Custom Color
  FCustomColors := FXColorSets.Create(Self);

  FDrawColors := FXCompleteColorSet.Create;

  FText := TEXT_LONG_GENERIC;

  // Sizing
  Height := 30;
  Width := 180;
end;

destructor FXScrollText.Destroy;
begin
  FreeAndNil( FImage );
  FreeAndNil( FCustomColors );
  FreeAndNil( FDrawColors );
  FAnimateTimer.Enabled := false;
  FreeAndNil( FAnimateTimer );
  inherited;
end;

procedure FXScrollText.InteractionStateChanged(AState: FXControlState);
begin
  // do not update
end;

procedure FXScrollText.AnimationProgress(Sender: TObject);
begin
  // Draw
  if FAnimateValue > 0 then
    begin
      FOffset := -FAnimateValue;
      FOffsetEnd := -(FAnimateValue - (FAnimateSpace+FAnimateWidth));

      FFadeLeft := FOffsetEnd > FFadeSize;

      Redraw;
    end;

  // Done
  if FAnimateValue >= FAnimateMax then
    begin
      ResetAnimation;
      Redraw;
    end;

  // Increase
  Inc(FAnimateValue, AnimationSpeed);
end;

function FXScrollText.Background: TColor;
begin
  Result := FDrawColors.Background;
end;

procedure FXScrollText.PaintBuffer;
var
  DrawFlags: FXTextFlags;
  RectText, RectImage, DRect: TRect;
  Fade: integer;

  I: Integer;
  ARect: TRect;
begin
  // Background
  Color := FDrawColors.BackGround;
  PaintBackground;

  for I := 1 to 2 do
    begin
      // Offset rects
      RectText := TextRect;
      RectImage := ImageRect;

      case I of
        1: begin
          RectText.Offset(FOffset, 0);
          RectImage.Offset(FOffset, 0);

          if RectText.Right < 0 then
            Continue;
        end;
        2: begin
          RectText.Offset(FOffsetEnd, 0);
          RectImage.Offset(FOffsetEnd, 0);

          if RectText.Right < 0 then
            Continue;
        end;
      end;

      // Draw
      with Buffer do
        begin
          //  Draw text
          Brush.Style := bsClear;
          Font.Assign(Self.Font);
          Font.Color := FDrawColors.Foreground;

          DrawFlags := [];

          case FVertLayout of
            TLayout.Beginning: DrawFlags := DrawFlags + [FXTextFlag.Top];
            TLayout.Center: DrawFlags := DrawFlags + [FXTextFlag.VerticalCenter];
            TLayout.Ending: DrawFlags := DrawFlags + [FXTextFlag.Bottom];
          end;

          DrawTextRect(Buffer, RectText, FText, DrawFlags);

          // Paint Image
          if Image.Enabled then
            Image.DrawIcon(Buffer, RectImage);
        end;
    end;

  // Draw Fade
  with Buffer do
    begin
      // This function will need a custom DrawRect, as the text
      // overflows outside the draw boundaries
      DRect := DrawRect;
      DRect.Left := 0;
      DRect.Right := Width;

      // Draw
      Fade := Min(FadeSize, FAnimateValue);
      if FFadeLeft then
        for I := 1 to Fade do
          begin
            ARect := Rect(DRect.Left+I-1, DRect.Top,
              DRect.Left+I, DRect.Bottom);
      
            CopyRectWithOpacity(Buffer, ARect, GetBackground, ARect, 255-trunc(I / Fade * 255));
          end;
        
      Fade := FadeSize;
      if FFadeRight then
        for I := 1 to Fade do
          begin
            ARect := Rect(DRect.Right-I, DRect.Top, DRect.Right-I+1, DRect.Bottom);
      
            CopyRectWithOpacity(Buffer, ARect, GetBackground, ARect, 255-trunc(I / Fade * 255));
          end;
    end;

  inherited;
end;

procedure FXScrollText.ResetAnimation;
begin
  FOffset := 0;
  FOffsetEnd := 0;
  FFadeLeft := false;
  FAnimateValue := -AnimationDelay;
end;

end.
