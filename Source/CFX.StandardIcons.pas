unit CFX.StandardIcons;

interface

uses
  SysUtils,
  Classes,
  Vcl.Controls,
  Vcl.Graphics,
  Messaging,
  Types,
  Vcl.Styles,
  Vcl.Themes,
  CFX.Types,
  CFX.Controls,
  CFX.Colors,
  CFX.Graphics,
  CFX.ThemeManager,
  CFX.UIConsts,
  CFX.Linker,
  CFX.VarHelpers,
  UITypes,
  Windows;

type
  FXStandardIcon = class(FXGraphicControl, FXControl)
    private
      FIcon : FXStandardIconType;
      FProport: boolean;
      FWidth: integer;
      FDrawColors: FXColorSet;
      FCustomColors: FXColorSets;

      procedure SetIcon(const Value: FXStandardIconType);
      procedure SetProport(const Value: boolean);
      procedure SetWid(const Value: integer);

      class procedure DrawPentacle(Canvas : TCanvas; Pent : TPent);
      class function MakePent(X, Y, L : integer) : TPent;
      class procedure MakeStar(Canvas : TCanvas; cX, cY, size : integer; Colour :TColor; bordersize: integer; bordercolor: TColor);

      // Update
      procedure UpdateColors;

    protected
      procedure Paint; override;

      // Size
      procedure Resize; override;

    published
      property CustomColors: FXColorSets read FCustomColors write FCustomColors stored true;

      property Transparent;
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

      property Proportional : boolean read FProport write SetProport;
      property SelectedIcon : FXStandardIconType read FIcon write SetIcon;
      property PenWidth : integer read FWidth write SetWid;

    public
      constructor Create(AOwner : TComponent); override;
      destructor Destroy; override;

      procedure Invalidate; override;

      // Interface
      function IsContainer: Boolean;
      procedure UpdateTheme(const UpdateChidlren: Boolean);

      function Background: TColor;
  end;

implementation

{ CProgress }

function FXStandardIcon.Background: TColor;
begin
  Result := FDrawColors.BackGround;
end;

constructor FXStandardIcon.Create(AOwner: TComponent);
begin
  inherited;
  FProport := true;
  FIcon := FXStandardIconType.Checkmark;
  FWidth := 10;

  FCustomColors := FXColorSets.Create(Self);
  FDrawColors := FXColorSet.Create;

  Width := 60;
  Height := 60;

  UpdateColors;
end;

destructor FXStandardIcon.Destroy;
begin
  FDrawColors.Free;
  FCustomColors.Free;
  inherited;
end;

procedure FXStandardIcon.Invalidate;
begin
  inherited;
end;

function FXStandardIcon.IsContainer: Boolean;
begin
  Result := false;
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

procedure FXStandardIcon.Paint;
var
  s: integer;
begin
  inherited;
  // Background
  if not Transparent then
    with Canvas do
      begin
        Brush.Color := FDrawColors.BackGround;
        FillRect(ClipRect);
      end;

  // Draw
  with canvas do begin
    // Color
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
    GDICircle(ClientRect, GetRGB(Brush.Color).MakeGDIBrush, nil);

    // Pen
    Pen.Style := psSolid;
    pen.Color := FDrawColors.ForeGround;

    // Icons
    case FIcon of
      // Checkmark
      FXStandardIconType.Checkmark: begin
        pen.Width := 10;

        pen.Width := (FWidth * width) div 100;

        moveto( trunc(clientwidth / 4.9), trunc(clientheight / 1.9) );
        lineto( trunc(clientwidth / 2.5), trunc(clientheight / 1.4) );
        lineto( trunc(clientwidth / 1.35), trunc(clientheight / 3.6) );
      end;

      // Error
      FXStandardIconType.Error: begin
        pen.Width := (FWidth * width) div 100;

        moveto( trunc(clientwidth / 3), trunc(clientheight / 3) );
        lineto( clientwidth - trunc(clientwidth / 3), clientheight - trunc(clientheight / 3) );

        moveto( clientwidth - trunc(clientwidth / 3), trunc(clientheight / 3) );
        lineto( trunc(clientwidth / 3), clientheight - trunc(clientheight / 3) );
      end;

      // Info
      FXStandardIconType.Information: begin
        pen.Width := 10;

        font.Style := [fsBold];
        font.Name := 'Calibri';
        font.Color := FDrawColors.ForeGround;
        if clientwidth < clientheight then
          s := clientwidth
        else
          s := clientheight;
        Font.Size := trunc(s / 1.8);

        Brush.Style := bsClear;
        TextOut( (Width div 2) - ( TextWidth('i') div 2 ) , (Height div 2) - ( TextHeight('i') div 2 ) , 'i');
      end;

      // Question
      FXStandardIconType.Question: begin
        pen.Width := 10;
        pen.Color := clWhite;

        font.Style := [fsBold];
        font.Name := 'Calibri';
        font.Color := FDrawColors.ForeGround;
        if clientwidth < clientheight then
          s := clientwidth
        else
          s := clientheight;
        Font.Size := trunc(s / 1.8);

        Brush.Style := bsClear;
        TextOut( (Width div 2) - ( TextWidth('?') div 2 ) , (Height div 2) - ( TextHeight('?') div 2 ) , '?');
      end;

      // Warning
      FXStandardIconType.Warning: begin
        pen.Width := 10;
        pen.Color := Self.Color;

        font.Style := [fsBold];
        font.Name := 'Calibri';
        font.Color := FDrawColors.ForeGround;
        if clientwidth < clientheight then
          s := clientwidth
        else
          s := clientheight;
        Font.Size := trunc(s / 1.8);

        Brush.Style := bsClear;
        TextOut( (Width div 2) - ( TextWidth('!') div 2 ) , (Height div 2) - ( TextHeight('!') div 2 ) , '!');
      end;

      // Star
      FXStandardIconType.Star: begin
        font.Color := FDrawColors.ForeGround;

        MakeStar(Canvas, width div 2, round(height / 7.5), trunc(width / 2.25), font.Color, 0, $0001BAF8);
      end;
    end;
  end;
end;

procedure FXStandardIcon.Resize;
begin
  inherited;
  // Set proportion
  if FProport then begin
    case Align of
      // Defaultt
      alNone: begin
        if width < height then
          height := width
        else
          width := height;
      end;

      alTop, alBottom: begin
        if width <> height then
          width := height;
      end;

      alLeft, alRight: begin
        if width <> height then
          height := width;
      end;
    end;
  end;
end;

procedure FXStandardIcon.SetIcon(const Value: FXStandardIconType);
begin
  if FIcon <> Value then
    begin
      FIcon := Value;

      if not IsReading then
        begin
          if Value = FXStandardIconType.None then
            RePaint
          else
            Paint;
        end;
    end;
end;

procedure FXStandardIcon.SetProport(const Value: boolean);
begin
  if FProport <> Value then
    begin
      FProport := Value;

      if not IsReading then
        Paint;
    end;
end;

procedure FXStandardIcon.SetWid(const Value: integer);
begin
  if FWidth <> Value then
    begin
      FWidth := Value;

      if not IsReading then
        Paint;
    end;
end;

procedure FXStandardIcon.UpdateColors;
begin
  if FCustomColors.Enabled then
    begin
      FDrawColors.LoadFrom(FCustomColors, ThemeManager.DarkTheme)
    end
  else
    begin
      FDrawColors.BackGround := GetParentBackgroundColor(ThemeManager.SystemColor.BackGround);
      FDrawColors.Foreground := FDrawColors.BackGround;
    end;
end;

procedure FXStandardIcon.UpdateTheme(const UpdateChidlren: Boolean);
begin
  UpdateColors;

  Paint;
end;

end.

