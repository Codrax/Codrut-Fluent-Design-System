﻿unit CFX.Panels;

interface

uses
  SysUtils,
  Classes,
  Threading,
  Winapi.Windows,
  Winapi.Messages,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.ExtCtrls,
  CFX.Colors,
  CFX.Animation.Main,
  CFX.Animation.Component,
  CFX.Constants,
  CFX.Classes,
  CFX.Types,
  CFX.Linker,
  CFX.Messages,
  CFX.Controls,
  CFX.ThemeManager;

type
  FXPanelBase = class(TPanel, IFXComponent, IFXControl)
  protected
    procedure WndProc(var Msg: TMessage); override;

  public
    // Draw
    procedure Redraw; virtual;

    // Interfaced
    function IsContainer: Boolean; virtual;
    procedure UpdateTheme(const UpdateChildren: Boolean); virtual;
    function Background: TColor; virtual;
  end;

  FXPanel = class(FXPanelBase)
  private
    FCustomColors: FXCompleteColorSets;

    FDrawColors: FXCompleteColorSet;
    FBackground: FXBackgroundColor;

    FAccentLine: boolean;
    FLineWidth: integer;

    procedure SetAccentLine(const Value: boolean);
    procedure SetAccentLineWidth(const Value: integer);
    procedure SetBackground(const Value: FXBackgroundColor);

  protected
    procedure Paint; override;

    // Inherited
    procedure Resize; override;

    // Update
    procedure UpdateColors;

  published
    property CustomColors: FXCompleteColorSets read FCustomColors write FCustomColors;
    property BackgroundColor: FXBackgroundColor read FBackground write SetBackground default FXBackgroundColor.Background;
    property AccentLine: boolean read FAccentLine write SetAccentLine default False;
    property AccentLineWidth: integer read FLineWidth write SetAccentLineWidth;

    property ShowCaption default false;
    property ParentColor default true;

    property Canvas;

  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    // Draw
    procedure DrawAccentLine; virtual;

    // Interface
    function IsContainer: Boolean; override;
    procedure UpdateTheme(const UpdateChildren: Boolean); override;

    function Background: TColor; override;
  end;

  FXMinimisePanel = class(FXPanelBase)
  private
    var
    FCustomColors: FXCompleteColorSets;
    FCustomHandleColor: FXColorStateSets;

    FHandleColor: FXColorStateSet;
    FDrawColors: FXCompleteColorSet;

    FHandleSize: integer;
    FHandleRound: integer;

    FText: string;
    FSkipRedrawFill: boolean;

    FContentFill: boolean;

    FMinimised: boolean;
    FAnimation: boolean;
    FControlState: FXControlState;
    FMouseInHandle: boolean;

    FImage: FXIconSelect;

    FAutoCursor: boolean;

    FAnim: FXIntAnim;
    FAnGoTo, FAnStart: integer;

    FDefaultHeight: integer;

    // UI
    function TrimEdges: boolean;
    procedure StopAnimation;
    procedure AnimateTranzition;

    // Set
    procedure SetState(AState: FXControlState);
    procedure SetHandleSize(const Value: integer);
    procedure SetHandleRound(const Value: integer);
    procedure SetMinimiseState(statemin: boolean; instant: boolean);
    procedure SetMinimised(const Value: boolean);
    procedure SetText(const Value: string);
    procedure SetContentFill(const Value: boolean);
    procedure SetImage(const Value: FXIconSelect);

    // Theme
    procedure UpdateColors;

  protected
    procedure Paint; override;

    // Animation
    procedure AnimationOnFinish(Sender: TObject);

    // Paint
    procedure PaintHandle;

    // Inherit align rect calculation
    procedure AdjustClientRect(var Rect: TRect); override;

    // Catch Events
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;

    // Override
    procedure Resize; override;

    procedure ChangeScale(M, D: Integer{$IF CompilerVersion > 29}; isDpiChange: Boolean{$ENDIF}); override;

    procedure MouseUp(Button : TMouseButton; Shift: TShiftState; X, Y : integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button : TMouseButton; Shift: TShiftState; X, Y : integer); override;

  published
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnClick;

    property Align;
    property Anchors;
    property Cursor;
    property Visible;
    property Enabled;
    property Constraints;
    property DoubleBuffered;

    property DefaultHeight: integer read FDefaultHeight write FDefaultHeight;
    property CustomColors: FXCompleteColorSets read FCustomColors write FCustomColors;
    property HandleCustomColors: FXColorStateSets read FCustomHandleColor write FCustomHandleColor;

    property HandleText: string read FText write SetText;
    property HandleSize: integer read FHandleSize write SetHandleSize default 60;
    property HandleRoundness: integer read FHandleRound write SetHandleRound default 15;

    property IsMinimised: boolean read FMinimised write SetMinimised;

    property Animation: boolean read FAnimation write FAnimation default false;
    property Image: FXIconSelect read FImage write SetImage;

    property ContentFill: boolean read FContentFill write SetContentFill default true;
    property DynamicCursor: boolean read FAutoCursor write FAutoCursor default true;

  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    // State
    procedure ToggleMinimised;
    procedure ChangeMinimised(Minimised: boolean);

    // Interface
    function IsContainer: Boolean; override;
    procedure UpdateTheme(const UpdateChildren: Boolean); override;

    function Background: TColor; override;
  end;

implementation


{ CProgress }

procedure FXMinimisePanel.AdjustClientRect(var Rect: TRect);
begin
  inherited;
  Rect.Top := Self.HandleSize;
end;

procedure FXMinimisePanel.AnimateTranzition;
begin
  // Prepare
  FAnStart := Height;

  if FMinimised then
    FAnGoTo := FHandleSize
  else
    FAnGoTo := FDefaultHeight;

  // Prepare
  StopAnimation;

  FAnim.StartValue := Height;
  FAnim.EndValue := FAnGoTo;

  // Start
  FAnim.Start;
end;

procedure FXMinimisePanel.AnimationOnFinish(Sender: TObject);
begin
  for var I := 0 to ControlCount - 1 do
    Controls[I].Invalidate;

  PaintHandle;
end;

function FXMinimisePanel.Background: TColor;
begin
  if FContentFill then
    Result := FDrawColors.BackGroundInterior
  else
    Result := FDrawColors.BackGround;
end;

procedure FXMinimisePanel.ChangeMinimised(Minimised: boolean);
begin
  SetMinimiseState(Minimised, false);
end;

procedure FXMinimisePanel.ChangeScale(M, D: Integer{$IF CompilerVersion > 29}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  FDefaultHeight := MulDiv(FDefaultHeight, M, D);
  FHandleSize := MulDiv(FHandleSize, M, D);
  Height := Height;
end;

procedure FXMinimisePanel.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  UpdateTheme(false);
  Redraw;
end;

constructor FXMinimisePanel.Create(AOwner: TComponent);
begin
  inherited;
  Width := 350;
  Height := 200;

  ParentColor := true;
  ParentBackground := true;
  ShowCaption := false;
  TabStop := true;

  FSkipRedrawFill := true;

  FullRepaint := false;

  // Animation
  FAnim := FXIntAnim.Create(Self);
  FAnim.Kind := FXAnimationKind.Sinus;
  FAnim.Duration := 0.2;
  FAnim.LatencyAdjustments := true;
  FAnim.LatencyCanSkipSteps := true;
  FAnim.Component := Self;
  FAnim.PropertyName := 'Height';
  FAnim.OnFinish := AnimationOnFinish;

  // Theme Manager building
  FCustomColors := FXCompleteColorSets.Create(Self);
  FDrawColors := FXCompleteColorSet.Create(ThemeManager.SystemColorSet, ThemeManager.DarkTheme);
  FCustomHandleColor := FXColorStateSets.Create(Self);
  FHandleColor := FXColorStateSet.Create(FCustomHandleColor, ThemeManager.DarkTheme);

  // Default Font
  Font.Size := 11;
  Font.Name := 'Segoe UI Semibold';

  FImage := FXIconSelect.Create(Self);

  // Default Handle
  FHandleRound := MINIMISE_PANEL_ROUND;
  FHandleSize := MINIMISE_PANEL_SIZE;
  FContentFill := true;

  FAutoCursor := true;

  FAnimation := false;
  FText := 'Minimised Panel';

  FDefaultHeight := Height;

  // Update
  UpdateColors;
end;

destructor FXMinimisePanel.Destroy;
begin
  FreeAndNil(FAnim);
  FreeAndNil(FImage);
  FreeAndNil(FAnim);
  FreeAndNil(FCustomHandleColor);
  FreeAndNil(FCustomColors);
  inherited;
end;

function FXMinimisePanel.IsContainer: Boolean;
begin
  Result := true;
end;

procedure FXMinimisePanel.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: integer);
begin
  inherited;
  if FMouseInHandle then
    SetState(FXControlState.Press);
end;

procedure FXMinimisePanel.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FMouseInHandle := (Y <= FHandleSize);

  if FAutoCursor then
    begin
      if FMouseInHandle then
        Cursor := crHandPoint
      else
        Cursor := crDefault;
    end;

  if FMouseInHandle and (FControlState = FXControlState.None) then { Cant be csPress, as if you hold the button while dragging it will hide the effect! }
    SetState(FXControlState.Hover)
      else
        if (not FMouseInHandle) and (FControlState <> FXControlState.None) then
          SetState(FXControlState.None)
end;

procedure FXMinimisePanel.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: integer);
begin
  inherited;

  SetState(FXControlState.Hover);

  if FMouseInHandle then
    ToggleMinimised;
end;

procedure FXMinimisePanel.Paint;
var
  TemporaryCanvas: TBitMap;
  DrawRect: TRect;
begin
  inherited;
  // Handle
  PaintHandle;

  // Prepare Settings
  DrawRect := Rect(0, 0, Width, Height - FHandleSize - HANDLE_SEPARATOR);

  // Exit running useless code
  if DrawRect.Height <=0 then
    Exit;

  // Canvas
  TemporaryCanvas := TBitMap.Create;
  TemporaryCanvas.Width := DrawRect.Width;
  TemporaryCanvas.Height := DrawRect.Height;

  Self.Color := FDrawColors.BackGround;

  // TMP Canvas
  with TemporaryCanvas.canvas do begin
    Brush.Color := FDrawColors.BackGround;
    FillRect(cliprect);

    // Font
    Font.Assign(Self.Font);
    Font.Color := FDrawColors.ForeGround;

    Pen.Style := psClear;

    // Color
    if FContentFill then
      begin
        Brush.Color := FDrawColors.BackGroundInterior;

        RoundRect(0, 0, DrawRect.Width, DrawRect.Height, FHandleRound, FHandleRound);

        { Secondary }
        if TrimEdges then
          Rectangle(0, 0, DrawRect.Width, DrawRect.Height div 2);
      end;
  end;

  canvas.CopyRect(Rect(0, abs(DrawRect.Height-Height), Width, Height),
                  TemporaryCanvas.Canvas, TemporaryCanvas.Canvas.ClipRect);

  TemporaryCanvas.Free;
end;

procedure FXMinimisePanel.PaintHandle;
var
  tmp: TBitMap;
  TmpFont: TFont;
  IconRect: TRect;
  DrawRect: TRect;
  TLeft: integer;
  I: string;
begin
  inherited;
  // Prepare Settings
  DrawRect := Rect(0, 0, Width, FHandleSize);

  tmp := TBitMap.Create;
  tmp.Height := DrawRect.Height;
  tmp.Width := DrawRect.Width;

  // TMP Canvas
  with tmp.canvas do begin
    Brush.Color := FDrawColors.BackGround;
    FillRect(cliprect);

    // Font
    Font.Assign(Self.Font);
    Font.Color := FHandleColor.GetColor(true, FControlState);

    Pen.Style := psClear;

    // Handle
    Brush.Color := FHandleColor.GetColor(false, FControlState);

    //Brush.Color := FDrawColors.BackGroundInterior;

    RoundRect(0, 0, Width, FHandleSize, FHandleRound, FHandleRound);

    { Square Next to contentfill }
    if TrimEdges then
      Rectangle(0, FHandleSize div 2, Width, FHandleSize);

    // Icon
    if FImage.Enabled then
    begin
      TLeft := FHandleSize div 2 + MINIMISE_ICON_MARGIN * 2;

      IconRect := Rect(MINIMISE_ICON_MARGIN, FHandleSize div 4, TLeft - MINIMISE_ICON_MARGIN, FHandleSize - FHandleSize div 4);

      if FImage.IconType <> FXIconType.SegoeIcon then
        FImage.DrawIcon(tmp.Canvas, IconRect)
      else
        begin
          { Font Icon }
          TmpFont := TFont.Create;
          try
            TmpFont.Assign(Font);

            Font.Name := ThemeManager.IconFont;
            Font.Size := round(Self.FHandleSize / 4);

            I := FImage.SelectSegoe;
            TextRect(IconRect, I, [tfSingleLine, tfVerticalCenter, tfCenter]);

            Font.Assign(TmpFont);
          finally
            TmpFont.Free
          end;
        end;
    end
      else
        TLeft := MINIMISE_ICON_MARGIN;

    // Text
    Brush.Style := bsClear;
    TextOut(tleft, FHandleSize div 2 - TextHeight(FText) div 2, FText);

    Pen.Style := psSolid;

    // Icon
    if FMinimised then
      i := ''
    else
      i := '';

    // Font
    Font.Size := round(Self.FHandleSize / 6);
    Font.Name := ThemeManager.IconFont;

    IconRect := Rect(Width - FHandleSize, 0, Width, FHandleSize);
    TextRect(IconRect, i, [tfSingleLine, tfVerticalCenter, tfCenter]);
    // TextOut(Width - TextWidth(i) - MINIMISE_ICON_MARGIN * 2, FHandleSize div 2 - TextHeight(i) div 2 - 3, i);

    // Reset Settings
    FSkipRedrawFill := false;
  end;

  canvas.CopyRect(DrawRect, tmp.Canvas, DrawRect);
end;

procedure FXMinimisePanel.Resize;
begin
  inherited;
  Repaint;
end;

procedure FXMinimisePanel.SetContentFill(const Value: boolean);
begin
  FContentFill := Value;
end;

procedure FXMinimisePanel.SetHandleRound(const Value: integer);
begin
  FHandleRound := Value;

  Paint;
end;

procedure FXMinimisePanel.SetHandleSize(const Value: integer);
begin
  FHandleSize := Value;

  if FMinimised then
    Self.Height := Value;
end;

procedure FXMinimisePanel.SetImage(const Value: FXIconSelect);
begin
  FImage := Value;

  Paint;
end;

procedure FXMinimisePanel.SetMinimised(const Value: boolean);
begin
  FMinimised := Value;

  // Design Mode
  if csDesigning in ComponentState then
    begin
      if IsMinimised then
        begin
          if Height > HandleSize then
            FDefaultHeight := Height;

          Height := HandleSize;
        end
      else
        begin
          Height := FDefaultHeight;
        end;
    end;

  // Update State
  SetMinimiseState(Value, true);
end;

procedure FXMinimisePanel.SetMinimiseState(statemin: boolean; Instant: boolean);
begin
  FMinimised := statemin;

  if (FAnim <> nil) and (FAnim.Running) then
    Exit;

  // Instant or No Animation
  if (NOT FAnimation) or Instant then
  begin
    if statemin then
      Height := FHandleSize
    else
      Height := FDefaultHeight;

    // Stop ani
    StopAnimation;
  end
    else
      // Animated
      AnimateTranzition;
end;

procedure FXMinimisePanel.SetState(AState: FXControlState);
begin
  FControlState := AState;

  FSkipRedrawFill := true;

  // Draw
  PaintHandle;
end;

procedure FXMinimisePanel.SetText(const Value: string);
begin
  FText := Value;

  Paint;
end;

procedure FXMinimisePanel.StopAnimation;
begin
  if FAnim.Running then
    FAnim.Stop;
end;

procedure FXMinimisePanel.ToggleMinimised;
begin
  SetMinimiseState(NOT FMinimised, false)
end;

function FXMinimisePanel.TrimEdges: boolean;
begin
  Result := (not (FMinimised and not FAnim.Running)) and FContentFill
end;

procedure FXMinimisePanel.UpdateColors;
begin
  // Access Theme Manager
  if FCustomColors.Enabled then
    FDrawColors := FXCompleteColorSet.Create( FCustomColors, ThemeManager.DarkTheme )
  else
    FDrawColors := FXCompleteColorSet.Create( ThemeManager.SystemColorSet, ThemeManager.DarkTheme );

  if FCustomHandleColor.Enabled then
    FHandleColor := FXColorStateSet.Create(FCustomHandleColor, ThemeManager.DarkTheme)
  else
    begin
      FHandleColor := FXColorStateSet.Create;
      FHandleColor.ForeGroundNone := FDrawColors.ForeGround;
      FHandleColor.ForeGroundHover := FDrawColors.ForeGround;
      FHandleColor.ForeGroundPress := ChangeColorLight( FDrawColors.ForeGround, -20);

      FHandleColor.BackGroundNone := FDrawColors.BackGroundInterior;
      FHandleColor.BackGroundHover := ChangeColorLight( FDrawColors.BackGroundInterior, MINIMISE_COLOR_CHANGE);
      FHandleColor.BackGroundPress := ChangeColorLight( FDrawColors.BackGroundInterior, -MINIMISE_COLOR_CHANGE);
    end;

  // Font Color
  Font.Color := FDrawColors.ForeGround;

  // Disabled
  if not Enabled then begin
    FHandleColor.Accent := GetColorGrayscale(FDrawColors.Accent);

    FHandleColor.BackgroundNone := ColorBlend(FHandleColor.BackgroundNone, FDrawColors.BackGround, 15);
    FHandleColor.BackGroundHover := FHandleColor.BackgroundNone;
    FHandleColor.BackGroundPress := FHandleColor.BackgroundNone;

    FHandleColor.ForeGroundNone := ColorBlend(FHandleColor.ForeGroundNone, FDrawColors.BackGround, 100);
    FHandleColor.ForeGroundHover := FHandleColor.ForeGroundNone;
    FHandleColor.ForeGroundPress := FHandleColor.ForeGroundNone;
  end;
end;

procedure FXMinimisePanel.UpdateTheme(const UpdateChildren: Boolean);
begin
  UpdateColors;

  // Inherit
  inherited;
end;

{ FXPanel }

function FXPanel.Background: TColor;
begin
  case FBackground of
    FXBackgroundColor.Background: Result := FDrawColors.Background;
    FXBackgroundColor.Content: Result := FDrawColors.BackGroundInterior;
    else
      Result := 0;
  end;
end;

constructor FXPanel.Create(AOwner: TComponent);
begin
  inherited;

  FullRepaint := false;
  ParentBackground := false;
  ParentColor := true;
  ShowCaption := false;

  Font.Name := ThemeManager.FormFont;

  FCustomColors := FXCompleteColorSets.Create(Self);
  FDrawColors := FXCompleteColorSet.Create(ThemeManager.SystemColorSet, ThemeManager.DarkTheme);

  FLineWidth := PANEL_LINE_WIDTH;
  FBackground := FXBackgroundColor.Background;

  BevelKind := bkNone;
  BevelOuter := bvNone;

  UpdateColors;
end;

destructor FXPanel.Destroy;
begin

  inherited;
end;

procedure FXPanel.DrawAccentLine;
begin
  if FAccentLine then
    with Canvas do
      begin
        Brush.Color := FDrawColors.Accent;
        Pen.Style :=psClear;

        RoundRect( PANEL_LINE_SPACING, PANEL_LINE_SPACING, PANEL_LINE_SPACING + FLineWidth, Height - PANEL_LINE_SPACING,
                    PANEL_LINE_ROUND, PANEL_LINE_ROUND);
      end;
end;

function FXPanel.IsContainer: Boolean;
begin
  Result := true;
end;

procedure FXPanel.Paint;
begin
  inherited;
  DrawAccentLine;
end;

procedure FXPanel.Resize;
begin
  inherited;
  if AccentLine and not FullRepaint then
    DrawAccentLine;
end;

procedure FXPanel.SetAccentLine(const Value: boolean);
begin
  FAccentLine := Value;

  if not (csReading in ComponentState) then
    begin
      RePaint;
      DrawAccentLine;
    end;
end;

procedure FXPanel.SetAccentLineWidth(const Value: integer);
begin
  FLineWidth := Value;

  if not (csReading in ComponentState) then
    RePaint;
end;

procedure FXPanel.SetBackground(const Value: FXBackgroundColor);
begin
  if FBackground <> Value then
    begin
      FBackground := Value;

      Invalidate;

      if not (csReading in ComponentState) then
        begin
          ParentColor := false;
          UpdateColors
        end;
    end;
end;

procedure FXPanel.UpdateColors;
begin
  // Access Theme Manager
  if FCustomColors.Enabled then
    FDrawColors.LoadFrom(FCustomColors, ThemeManager.DarkTheme)
  else
    FDrawColors.Assign(ThemeManager.SystemColor);
  //FDrawColors.BackGround := GetParentBackgroundColorEx(Self, FDrawColors.BackGround);

  // Font Color
  Font.Color := FDrawColors.ForeGround;

  // Set Color
  case FBackground of
    FXBackgroundColor.Background: Color := FDrawColors.BackGround;
    FXBackgroundColor.Content: Color := FDrawColors.BackGroundInterior;
  end;
end;

procedure FXPanel.UpdateTheme(const UpdateChildren: Boolean);
begin
  UpdateColors;

  inherited;
end;

{ FXPanelBase }

function FXPanelBase.Background: TColor;
begin
  Result := Color;
end;

function FXPanelBase.IsContainer: Boolean;
begin
  Result := true;
end;

procedure FXPanelBase.Redraw;
begin
  Invalidate;
end;

procedure FXPanelBase.UpdateTheme(const UpdateChildren: Boolean);
begin
  // Update Self
  Invalidate;

  // Update Children
  if IsContainer and UpdateChildren then
    begin
      for var I := 0 to ControlCount - 1 do
        if Supports(Controls[i], IFXComponent) then
          (Controls[i] as IFXComponent).UpdateTheme(UpdateChildren);
    end;
end;

procedure FXPanelBase.WndProc(var Msg: TMessage);
begin
  inherited;
  if (Msg.Msg >= WM_CFX_MESSAGES) and (Msg.Msg <= WM_CFX_MESSAGES_END) then
    Broadcast(Msg);
end;

end.
