unit CFX.StandardIcons;

interface

uses
  SysUtils,
  Classes,
  Vcl.Controls,
  Vcl.Graphics,
  Messaging,
  Types,
  Math,
  Vcl.Styles,
  Vcl.Themes,
  CFX.Types,
  CFX.Controls,
  CFX.Colors,
  CFX.Graphics,
  CFX.ThemeManager,
  CFX.Constants,
  CFX.Linker,
  CFX.VarHelpers,
  UITypes,
  Windows;

type
  FXStandardIcon = class(FXWindowsControl)
  private
    var DrawRect, MainRect: TRect;
    FIcon : FXStandardIconType;
    FPenWidth: integer;
    FDrawColors: FXColorSet;
    FCustomColors: FXColorSets;
    FProportional: boolean;
    FUseAccentColor: boolean;

    // Star drawing
    class procedure DrawPentacle(Canvas : TCanvas; Pent : TPent);
    class function MakePent(X, Y, L : integer) : TPent;
    class procedure MakeStar(Canvas : TCanvas; cX, cY, size : integer; Colour :TColor; bordersize: integer; bordercolor: TColor);

    // Setters
    procedure SetIcon(const Value: FXStandardIconType);
    procedure SetWid(const Value: integer);
    procedure SetProportional(const Value: boolean);
    procedure SetUseAccentColor(const Value: boolean);

  protected
    procedure PaintBuffer; override;

    // Update
    procedure UpdateColors; override;
    procedure UpdateRects; override;

    // Interaction
    procedure InteractionStateChanged(AState: FXControlState); override;

  published
    property CustomColors: FXColorSets read FCustomColors write FCustomColors stored true;

    property Transparent;
    property PaddingFill;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
    property OnClick;
    property OnDblClick;

    property Color;

    property Align;
    property Constraints;
    property Anchors;
    property Hint;
    property ShowHint;
    property Cursor;
    property Visible;
    property Enabled;

    property Proportional: boolean read FProportional write SetProportional default true;
    property UseAccentColor: boolean read FUseAccentColor write SetUseAccentColor default false;
    property SelectedIcon : FXStandardIconType read FIcon write SetIcon;
    property PenWidth : integer read FPenWidth write SetWid;

  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    // Override
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

    // Interface
    function Background: TColor; override;
  end;

implementation

{ FXStandardIcon }

function FXStandardIcon.Background: TColor;
begin
  Result := FDrawColors.BackGround;
end;

constructor FXStandardIcon.Create(AOwner: TComponent);
begin
  inherited;
  FProportional := true;
  FIcon := FXStandardIconType.Checkmark;
  FPenWidth := 10;
  FUseAccentColor := false;

  FCustomColors := FXColorSets.Create(Self);
  FDrawColors := FXColorSet.Create;

  // Sizing
  Width := 60;
  Height := 60;
end;

destructor FXStandardIcon.Destroy;
begin
  FDrawColors.Free;
  FCustomColors.Free;
  inherited;
end;

class procedure FXStandardIcon.MakeStar(Canvas : TCanvas; cX, cY, size : integer; Colour :TColor; bordersize: integer; bordercolor: TColor);
var
  Pent : TPent;
begin
  Pent := MakePent(cX, cY, size);
  BeginPath(Canvas.Handle);
  DrawPentacle(Canvas, Pent);
  EndPath(Canvas.Handle);
  SetPolyFillMode(Canvas.Handle, WINDING);
  if bordersize <> 0 then
    Canvas.Brush.Color := bordercolor
  else
    Canvas.Brush.Color := Colour;
  FillPath(Canvas.Handle);

  if bordersize <> 0 then begin
    Pent := MakePent(cX, cY + trunc(bordersize / 1.2), size - bordersize);
    BeginPath(Canvas.Handle);
    DrawPentacle(Canvas, Pent);
    EndPath(Canvas.Handle);
    SetPolyFillMode(Canvas.Handle, WINDING);
    Canvas.Brush.Color := Colour;
    FillPath(Canvas.Handle);
  end;
end;

class function FXStandardIcon.MakePent(X, Y, L : integer) : TPent;
var
  DX1, DY1, DX2, DY2 : integer;
const
  Sin54 = 0.809;
  Cos54 = 0.588;
  Tan72 = 3.078;
begin
  DX1 := trunc(L * Sin54);
  DY1 := trunc(L * Cos54);
  DX2 := L div 2;
  DY2 := trunc(L * Tan72 / 2);
  Result[0] := point(X, Y);
  Result[1] := point(X - DX1, Y + DY1);
  Result[2] := point(X - DX2, Y + DY2);
  Result[3] := point(X + DX2, Y + DY2);
  Result[4] := point(X + DX1, Y + DY1);
end;

class procedure FXStandardIcon.DrawPentacle(Canvas : TCanvas; Pent : TPent);
begin
  with Canvas do begin
    MoveTo(Pent[0].X, Pent[0].Y);
    LineTo(Pent[2].X, Pent[2].Y);
    LineTo(Pent[4].X, Pent[4].Y);
    LineTo(Pent[1].X, Pent[1].Y);
    LineTo(Pent[3].X, Pent[3].Y);
    LineTo(Pent[0].X, Pent[0].Y);
  end;
end;

procedure FXStandardIcon.InteractionStateChanged(AState: FXControlState);
begin
  // do not redraw
end;

procedure FXStandardIcon.PaintBuffer;
procedure Move(X, Y: real); overload;
begin
  Buffer.MoveTo( trunc(ContentRect.Left+X), trunc(ContentRect.Top+Y) );
end;
procedure Line(X, Y: real); overload;
begin
  Buffer.LineTo( trunc(ContentRect.Left+X), trunc(ContentRect.Top+Y) );
end;
procedure Text(AStr: string);
begin
  with Buffer do
    TextOut( ContentRect.Width div 2-TextWidth('i') div 2 ,
      ContentRect.Height div 2-TextHeight('i') div 2 ,
      'i');
end;
var
  AWidth, AHeight: integer;
begin
  // Background
  Color := FDrawColors.BackGround;
  PaintBackground;

  // Draw
  with Buffer do begin
    // Circle color
    if UseAccentColor then
      Brush.Color := FDrawColors.Accent
    else
      case FIcon of
        FXStandardIconType.Checkmark: Brush.Color := ICON_GREEN;
        FXStandardIconType.Error: Brush.Color := ICON_ROSE;
        FXStandardIconType.Question: Brush.Color := ICON_ICEBLUE;
        FXStandardIconType.Information: Brush.Color := ICON_ICEBLUE;
        FXStandardIconType.Warning: Brush.Color := ICON_YELLOW;
        FXStandardIconType.Star: Brush.Color := ICON_YELLOW;
        FXStandardIconType.None: Exit;
      end;

    // Circle
    Pen.Style := psClear;
    GDICircle(DrawRect, GetRGB(Brush.Color).MakeGDIBrush, nil);

    // Data
    AWidth := ContentRect.Width;
    AHeight := ContentRect.Height;

    // Brush
    Brush.Style := bsClear;

    // Pen
    Pen.Style := psSolid;
    Pen.Color := FDrawColors.ForeGround;
    Pen.Width := (FPenWidth * AWidth) div 100;

    // Font
    Font.Style := [fsBold];
    Font.Name := 'Calibri';
    Font.Color := FDrawColors.ForeGround;
    Font.Size := trunc(Min(AWidth, AHeight) / 1.8);

    // Icons
    case FIcon of
      // Checkmark
      FXStandardIconType.Checkmark: begin
        Move( AWidth / 4.9, ContentRect.Height / 1.9 );
        Line( AWidth / 2.5, ContentRect.Height / 1.4 );
        Line( AWidth / 1.35, ContentRect.Height / 3.6 );
      end;

      // Error
      FXStandardIconType.Error: begin
        Move( AWidth / 3, AHeight / 3 );
        Line( AWidth - AWidth / 3, AHeight - AHeight / 3 );

        Move( AWidth - AWidth / 3, AHeight / 3 );
        Line( AWidth / 3, AHeight - AHeight / 3 );
      end;

      // Info
      FXStandardIconType.Information: Text('i');

      // Question
      FXStandardIconType.Question: Text('?');

      // Warning
      FXStandardIconType.Warning: Text('!');

      // Star
      FXStandardIconType.Star:
        MakeStar(Buffer, AWidth div 2, round(AHeight / 7.5),
          trunc(AWidth / 2.25), font.Color, 0, $0001BAF8);
    end;
  end;

  inherited;
end;

procedure FXStandardIcon.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if FProportional then begin
    const ChangeW = AWidth <> Width;
    const ChangeH = AHeight <> Height;

    if ChangeW and ChangeH then
      AHeight := AWidth
    else
    if ChangeW then
      AHeight := AWidth
    else
    if ChangeH then
      AWidth := AHeight;
  end;

  inherited;
end;

procedure FXStandardIcon.SetIcon(const Value: FXStandardIconType);
begin
  if FIcon = Value then
    Exit;

  FIcon := Value;
  StandardUpdateLayout;
end;

procedure FXStandardIcon.SetProportional(const Value: boolean);
begin
  if FProportional = Value then
    Exit;

  if Value and (Height <> Width) then
    inherited Height := Width;

  FProportional := Value;
  UpdateRects;
  StandardUpdateLayout;
end;

procedure FXStandardIcon.SetUseAccentColor(const Value: boolean);
begin
  if FUseAccentColor = Value then
    Exit;

  FUseAccentColor := Value;
  StandardUpdateLayout;
end;

procedure FXStandardIcon.SetWid(const Value: integer);
begin
  if FPenWidth = Value then
    Exit;

  FPenWidth := Value;
  StandardUpdateLayout;
end;

procedure FXStandardIcon.UpdateColors;
begin
  // Access theme mangager
  FDrawColors.Assign(ThemeManager.SystemColor);
  if FCustomColors.Enabled then
    // Custom colors
    FDrawColors.LoadFrom(FCustomColors, ThemeManager.DarkTheme)
end;

procedure FXStandardIcon.UpdateRects;
begin
  DrawRect := ClientRect;
  MainRect := ContentRect;
end;

end.

