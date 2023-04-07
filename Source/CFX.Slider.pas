unit CFX.Slider;

interface

uses
  SysUtils,
  Classes,
  Vcl.Controls,
  Types,
  Math,
  Vcl.ExtCtrls,
  Vcl.Forms,
  WinApi.Windows,
  Vcl.Graphics,
  Vcl.Themes,
  Vcl.Styles,
  System.Messaging,
  CFX.Colors,
  CFX.ThemeManager,
  CFX.Types,
  CFX.Controls,
  CFX.Classes;

type
  FXSlider = class;
  FXSliderChange = procedure(Sender : FXSlider; Position, Max, Min: integer) of object;

  FXSliderOptions = class(TMPersistent)
    private
      FHeight, FWidthMargin, FRoundness: integer;
      FFlatEnd: boolean;
    published
      property Height : integer read FHeight write FHeight;
      property WidthMargin : integer read FWidthMargin write FWidthMargin;
      property Roundness : integer read FRoundness write FRoundness;
      property FlatEnd: boolean read FFlatEnd write FFlatEnd;
  end;

  FXSliderIndicator = class(TMPersistent)
    private
      FHeight, FWidth, FRoundness,
      FBorderThick, FDynBorderThick: integer;
      FMultiColor, FDynamicBorder: boolean;
      FEnabled: boolean;
    published
      property Enabled : boolean read FEnabled write FEnabled;

      property Height : integer read FHeight write FHeight;
      property Width : integer read FWidth write FWidth;
      property Roundness : integer read FRoundness write FRoundness;

      property BorderThick : integer read FBorderThick write FBorderThick;
      property MultiColor : boolean read FMultiColor write FMultiColor;

      property DynamicBorderSize : integer read FDynBorderThick write FDynBorderThick;
      property DynamicBorder : boolean read FDynamicBorder write FDynamicBorder;
  end;

  FXSlider = class(FXTransparentControl, FXControl)
    private
      FAuthor, FSite, FVersion: string;

      // Draw Colors
      FDrawSliderColor,
      FDrawAccentColor,
      FDrawIndicatorColor: FXSingleColorStateSet;

      // Custom Colors
      FCustomSliderColor,
      FCustomAccentColor,
      FCustomIndicatorColor: FXSingleColorStateSets;

      FDrawColors: FXColorSet;
      FCustomColors: FXColorSets;

      FOptions: FXSliderOptions;
      FIndic: FXSliderIndicator;
      FMax, FMin: integer;
      FPosition: integer;
      FOnChange : FXSliderChange;
      FState: FXControlState;

      procedure SetMax(const Value: integer);
      procedure SetMin(const Value: integer);
      procedure SetPosition(const Value: integer);
      procedure SetState(const Value: FXControlState);
      procedure SetIndicator(const Value: FXSliderIndicator);
      procedure SetOptions(const Value: FXSliderOptions);
      procedure SetCustomColor(const Value: FXColorSets);
      procedure SetCustomProperty(const Index: Integer;
        const Value: FXSingleColorStateSets);

    protected
      procedure Paint; override;

      procedure CMMouseEnter(var Message : TMessage); message CM_MOUSEENTER;
      procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
      procedure MouseDown(Button : TMouseButton; Shift: TShiftState; X, Y : integer); override;
      procedure MouseMove(Shift: TShiftState; X, Y : integer); override;
      procedure MouseUp(Button : TMouseButton; Shift: TShiftState; X, Y : integer); override;

    public
      constructor Create(AOwner : TComponent); override;
      destructor Destroy; override;

      // Interface
      function IsContainer: Boolean;
      procedure UpdateTheme(const UpdateChildren: Boolean);

    published
      property OnMouseEnter;
      property OnMouseLeave;
      property OnMouseDown;
      property OnMouseUp;
      property OnMouseMove;
      property OnClick;
      property OnChange : FXSliderChange read FOnChange write FOnChange;

      property ShowHint;
      property Align;
      property Anchors;
      property Cursor;
      property Visible;
      property Enabled;
      property Constraints;
      property DoubleBuffered;
      property PopupMenu;

      property Color;
      property ParentBackground;
      property ParentColor;

      property CustomColors: FXColorSets read FCustomColors write SetCustomColor;

      property CustomSliderColor : FXSingleColorStateSets index 0 read FCustomSliderColor write SetCustomProperty;
      property CustomAccentColor : FXSingleColorStateSets index 1read FCustomAccentColor write SetCustomProperty;
      property CustomIndicatorColor : FXSingleColorStateSets index 2 read FCustomIndicatorColor write SetCustomProperty;
      property SliderOptions : FXSliderOptions read FOptions write SetOptions;
      property Indicator : FXSliderIndicator read FIndic write SetIndicator;

      property State : FXControlState read FState write SetState;

      property Max : integer read FMax write SetMax;
      property Min : integer read FMin write SetMin;
      property Position : integer read FPosition write SetPosition;

      property &&&Author: string Read FAuthor;
      property &&&Site: string Read FSite;
      property &&&Version: string Read FVersion;
  end;

implementation


{ FXSlider }


procedure FXSlider.CMMouseEnter(var Message: TMessage);
begin
  SetState(FXControlState.csHover);
end;

procedure FXSlider.CMMouseLeave(var Message: TMessage);
begin
  SetState(FXControlState.csNone);
end;

constructor FXSlider.Create(AOwner: TComponent);
begin
  inherited;
  FAuthor                       := 'Petculescu Codrut';
  FSite                         := 'https://www.codrutsoftware.cf';
  FVersion                      := '1.4';

  interceptmouse:=True;

  ParentColor := true;
  ParentBackground := true;

  FOptions := FXSliderOptions.Create(Self);
  with FOptions do begin
    FRoundness := 5;
    FHeight := 10;
    FWidthMargin := 10;
  end;

  FIndic := FXSliderIndicator.Create(Self);
  with FIndic do begin
    FHeight := 20;
    FWidth := 20;
    FRoundness := 50;
    FEnabled := true;

    FMultiColor := true;

    FBorderThick := 5;

    FDynBorderThick := 2;
    FDynamicBorder := true;
  end;

  FCustomSliderColor := FXSingleColorStateSets.Create;
  FCustomAccentColor := FXSingleColorStateSets.Create;
  FCustomIndicatorColor := FXSingleColorStateSets.Create;

  FCustomColors := FXColorSets.Create();

  UpdateTheme(false);

  Width := 250;
  Height := 30;

  FPosition := 50;
  FMin := 0;
  FMax := 100;
end;

destructor FXSlider.Destroy;
begin
  FreeAndNil(FOptions);
  FreeAndNil(FIndic);

  FreeAndNil(FDrawSliderColor);
  FreeAndNil(FDrawAccentColor);
  FreeAndNil(FDrawIndicatorColor);

  FreeAndNil(FCustomSliderColor);
  FreeAndNil(FCustomAccentColor);
  FreeAndNil(FCustomIndicatorColor);

  FreeAndNil(FDrawColors);
  FreeAndNil(FCustomColors);

  inherited;
end;

function FXSlider.IsContainer: Boolean;
begin
  Result := false;
end;

procedure FXSlider.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: integer);
begin
  inherited;

  SetState(FXControlState.csPress);
end;

procedure FXSlider.MouseMove(Shift: TShiftState; X, Y: integer);
var
  BRect: TRect;
begin
  inherited;

  if FState = FXControlState.csPress then
    begin
      BRect := Rect(FOptions.FWidthMargin, Height div 2 - FOptions.FHeight div 2,
                    Width - FOptions.WidthMargin, Height div 2 + FOptions.FHeight div 2);

      Position := trunc( (X - BRect.Left) / (BRect.Width - FOptions.WidthMargin) * (FMax - FMin) ) + FMin;

      if Assigned(FOnChange) then
        FOnChange(Self, Position, FMax, FMin);
    end;
end;

procedure FXSlider.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: integer);
begin
  inherited;

  SetState(csHover);
end;

procedure FXSlider.Paint;
var
  workon: TBitMap;
  BRect, FRect, IRect: TRect;
begin
  inherited;

  workon := TBitMap.Create;
  try
    workon.Height := Height;
    workon.Width := Width;
    with workon.Canvas do begin
      // Set Rects
      Brush.Color := FDrawColors.BackGround;
      FillRect(ClipRect);

      BRect := Rect(FOptions.FWidthMargin, Height div 2 - FOptions.FHeight div 2,
                    Width - FOptions.WidthMargin, Height div 2 + FOptions.FHeight div 2);

      FRect := BRect;
      FRect.Width := trunc((FPosition - FMin) / (FMax - FMin) * BRect.Width);

      IRect.Top := BRect.CenterPoint.Y - FIndic.FHeight div 2;
      IRect.Left := FRect.Right - FIndic.FWidth div 2;
      IRect.Width := FIndic.FWidth;
      IRect.Height := FIndic.FHeight;

      // Draw Background
      Brush.Color := FDrawSliderColor.GetColor( FState );

      Pen.Style := psClear;

      RoundRect(BRect, FOptions.FRoundness, FOptions.FRoundness);

      // Draw Foreground
      Brush.Color := FDrawAccentColor.GetColor( FState );

      RoundRect(FRect, FOptions.FRoundness, FOptions.FRoundness);

      // Draw Indicator
      if FIndic.FEnabled then
        begin
          // Foreground
          Brush.Color := FDrawAccentColor.GetColor( FState );

          Pen.Color := FDrawIndicatorColor.GetColor( FState );
          Pen.Width := FIndic.FBorderThick;
          if Pen.Width = 0 then
            Pen.Style := psClear
          else
            Pen.Style := psSolid;

          RoundRect(IRect, FIndic.FRoundness, FIndic.FRoundness);

          { Dynamic Border }
          if FIndic.FDynamicBorder and (FState = FXControlState.csHover) then
            begin
              Pen.Width := FIndic.FDynBorderThick;
              RoundRect(IRect, FIndic.FRoundness, FIndic.FRoundness);
            end;
        end;

    end;
  finally
    Canvas.CopyRect(Rect(0,0,width,height),workon.Canvas,workon.canvas.ClipRect);

    workon.Free;
  end;
end;

procedure FXSlider.SetCustomColor(const Value: FXColorSets);
begin
  FCustomColors := Value;

  Paint;
end;

procedure FXSlider.SetCustomProperty(const Index: Integer;
  const Value: FXSingleColorStateSets);
begin
  case Index of
    0: FCustomSliderColor := Value;
    1: FCustomAccentColor := Value;
    2: FCustomIndicatorColor := Value;
  end;

  UpdateTheme(false);
  Paint;
end;

procedure FXSlider.SetIndicator(const Value: FXSliderIndicator);
begin
  FIndic := Value;

  Paint;
end;

procedure FXSlider.SetMax(const Value: integer);
begin
  FMax := Value;
  if FPosition > FMax then FPosition := FMax;
  Paint;
end;

procedure FXSlider.SetMin(const Value: integer);
begin
  FMin := Value;
  if FPosition < FMin then
    FPosition := FMin;
  Paint;
end;

procedure FXSlider.SetOptions(const Value: FXSliderOptions);
begin
  FOptions := Value;
end;

procedure FXSlider.SetPosition(const Value: integer);
begin
  FPosition := Value;

  if csLoading in ComponentState then
    Exit;

  if FPosition > FMax then
    FPosition := FMax;

  if FPosition < FMin then
    FPosition := FMin;

  Paint;
end;

procedure FXSlider.SetState(const Value: FXControlState);
begin
  FState := Value;

  Paint;
end;

procedure FXSlider.UpdateTheme(const UpdateChildren: Boolean);
begin
  if FCustomSliderColor.Enabled then
    FDrawSliderColor := FXSingleColorStateSet.Create( FCustomSliderColor, ThemeManager.DarkTheme )
      else
        begin
          FDrawSliderColor := FXSingleColorStateSet.Create( ThemeManager.SystemGrayControl, ThemeManager.DarkTheme );

          FDrawSliderColor.Hover := FDrawSliderColor.None;
          FDrawSliderColor.Press := FDrawSliderColor.None;
        end;

  if FCustomAccentColor.Enabled then
    FDrawAccentColor := FXSingleColorStateSet.Create(FCustomAccentColor, ThemeManager.DarkTheme)
      else
        begin
          FDrawAccentColor := FXSingleColorStateSet.Create(Self);
          FDrawAccentColor.CopyFrom( ThemeManager.SystemAccentInteractStates );
        end;

  if FCustomIndicatorColor.Enabled then
    FDrawIndicatorColor := FXSingleColorStateSet.Create(FCustomIndicatorColor, ThemeManager.DarkTheme)
      else
        begin
          FDrawIndicatorColor := FXSingleColorStateSet.Create($00463E39, $00463E39, $00463E39);
        end;


  // Draw BackGround
  if FCustomColors.Enabled then
    FDrawColors := FXColorSet.Create( FCustomColors, ThemeManager.DarkTheme )
  else
    FDrawColors := FXColorSet.Create( ThemeManager.SystemColorSet, ThemeManager.DarkTheme);
end;

end.
