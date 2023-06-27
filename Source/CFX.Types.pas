unit CFX.Types;

{$SCOPEDENUMS ON}

interface
  uses
    UITypes, Types, CFX.UIConsts, VCl.GraphUtil, Winapi.Windows,
    Classes, Vcl.Themes, Vcl.Controls, Vcl.Graphics,
    SysUtils, Winapi.GDIPAPI, Winapi.GDIPOBJ;

  type
    // File Type
    TFileType = (Text, BMP, PNG, JPEG, GIF, HEIC, TIFF,
      MP3, MP4, Flac, MDI, OGG, SND, M3U8, EXE, MSI,
      Zip, GZip, Zip7, Cabinet, TAR, RAR, LZIP, ISO,
      PDF, HLP, CHM);

    // Icon Type
    FXIconType = (Image, BitMap, ImageList, SegoeIcon);

    // FXPopupMenu
    FXAnimateSelection = (Instant, Opacity, Linear, Square);

    // FXCheckBox
    FXCheckBoxState = (Checked, Unchecked, Grayed);

    // FXBlurMaterial
    FXGlassRefreshMode = (Manual, Timer);
    FXBlurVersion = (WallpaperBlurred, Wallpaper, Screenshot, None);
    TWallpaperSetting = (Fill, Fit, Stretch, Tile, Center, Span);

    // FXStandardIcons
    FXStandardIconType = (Checkmark, Error, Question, Information, Warning, Star, None);

    // FXSlider
    FXOrientation = (Horizontal, Vertical);

    // Graphics
    FXDrawMode = (Fill, Fit, Stretch, Center, CenterFill,
                 Center3Fill, CenterFit, Normal, Tile);

    FXTextFlag = (WordWrap, Top, VerticalCenter, Bottom, Left, Center, Right,
                 NoClip, Auto);

    FXDrawLayout = (Left, Top, Right, Bottom);
    FXLayout = (Beginning, Center, Ending);

    FXTextFlags= set of FXTextFlag;

    FXCorners = (TopLeft, TopRight, BottomLeft, BottomRight);

    // Theme Color
    FXColorType = (Foreground, Accent, BackGround, Content);
    FXDarkSetting = (Auto, ForceDark, ForceLight);

    // File
    FXUserShell = (User, AppData, AppDataLocal, Documents,
                      Pictures, Desktop, Music, Videos,
                      Network, Recent, StartMenu, Startup,
                      Downloads, Programs);

    // Theme Change Detection
    FXThemeType = (Redraw, Colorization, AppTheme);
    FXThemeChange = procedure(Sender: TObject; ThemeChange: FXThemeType; DarkTheme: boolean; Accent: TColor) of object;

    // Controls
    FXControlState = (None, Hover, Press);
    FXControlOnPaint = procedure(Sender: TObject) of object;

    // Classes
    FXRGBA = record
    public
      R, G, B, A: byte;

      function Create(Red, Green, Blue: Byte; Alpha: Byte = 255): FXRGBA;

      function MakeGDIBrush: TGPSolidBrush;
      function MakeGDIPen(Width: Single = 1): TGPPen;

      function ToColor(Alpha: Byte = 255): TColor;
      procedure FromColor(Color: TColor; Alpha: Byte = 255);
    end;

    TLine = record
      Point1: TPoint;
      Point2: TPoint;

      procedure Create(P1, P2: TPoint);

      procedure SetPercentage(Percentage: real);
      procedure SwapPoints;

      procedure OffSet(const DX, DY: Integer);

      function Rect: TRect;
      function GetHeight: integer;
      function GetWidth: integer;

      function Center: TPoint;
    end;

    TRoundRect = record
      public
        Rect: TRect;

        RoundTL,
        RoundTR,
        RoundBL,
        RoundBR: integer;

        Corners: FXCorners;

        function Left: integer;
        function Right: integer;
        function Top: integer;
        function Bottom: integer;
        function TopLeft: TPoint;
        function BottomRight: TPoint;
        function Height: integer;
        function Width: integer;

        procedure Offset(const DX, DY: Integer);

        procedure SetRoundness(Value: integer);
        function GetRoundness: integer;

        function RoundX: integer;
        function RoundY: integer;

        procedure Create(TopLeft, BottomRight: TPoint; Rnd: integer); overload;
        procedure Create(SRect: TRect; Rnd: integer); overload;
        procedure Create(Left, Top, Right, Bottom: integer; Rnd: integer); overload;
    end;

  { Type Functions }
  function MakeRoundRect(SRect: TRect; Rnd: integer): TRoundRect; overload;
  function MakeRoundRect(SRect: TRect; RndX, RndY: integer): TRoundRect; overload;
  function MakeRoundRect(X1, Y1, X2, Y2: integer; Rnd: integer): TRoundRect; overload;
  function Line(Point1, Point2: TPoint): TLine;

  { Color Conversion }
  function GetRGB(Color: TColor; Alpha: Byte = 255): FXRGBA; overload;
  function GetRGB(R, G, B: Byte; Alpha: Byte = 255): FXRGBA; overload;

  { Rectangles }
  function GetValidRect(Point1, Point2: TPoint): TRect; overload;
  function GetValidRect(Points: TArray<TPoint>): TRect; overload;
  function GetValidRect(Rect: TRect): TRect; overload;

  { Conversion }
  function DecToHex(Dec: int64): string;

implementation

function MakeRoundRect(SRect: TRect; Rnd: integer): TRoundRect;
var
  rec: TRoundRect;
begin
  rec.Create(SRect, Rnd);
  Result := rec;
end;

function MakeRoundRect(SRect: TRect; RndX, RndY: integer): TRoundRect; overload;
var
  rec: TRoundRect;
begin
  rec.Create(SRect, (RndX + RndY) div 2);
  Result := rec;
end;

function MakeRoundRect(X1, Y1, X2, Y2: integer; Rnd: integer): TRoundRect;
var
  rec: TRoundRect;
begin
  rec.Create(Rect(X1, Y1, X2, Y2), Rnd);
  Result := rec;
end;

function Line(Point1, Point2: TPoint): TLine;
begin
  Result.Point1 := Point1;
  Result.Point2 := Point2;
end;

function GetRGB(Color: TColor; Alpha: Byte): FXRGBA;
begin
  Result.FromColor(Color, Alpha);
end;

function GetRGB(R, G, B: Byte; Alpha: Byte): FXRGBA;
begin
  Result.Create(R, G, B, Alpha);
end;

function GetValidRect(Point1, Point2: TPoint): TRect;
begin
  if Point1.X < Point2.X then
    Result.Left := Point1.X
  else
    Result.Left := Point2.X;

  if Point1.Y < Point2.Y then
    Result.Top := Point1.Y
  else
    Result.Top := Point2.Y;

  Result.Width := abs( Point2.X - Point1.X);
  Result.Height := abs( Point2.Y - Point1.Y);
end;

function GetValidRect(Points: TArray<TPoint>): TRect; overload
var
  I: Integer;
begin
  if Length( Points ) = 0 then
    Exit;

  Result.TopLeft := Points[0];
  Result.BottomRight := Points[0];

  for I := 1 to High(Points) do
    begin
      if Points[I].X < Result.Left then
        Result.Left := Points[I].X;
      if Points[I].Y < Result.Top then
        Result.Top := Points[I].Y;

      if Points[I].X > Result.Right then
        Result.Right := Points[I].X;
      if Points[I].Y > Result.Bottom then
        Result.Bottom := Points[I].Y;
    end;
end;

function GetValidRect(Rect: TRect): TRect;
begin
  if Rect.TopLeft.X < Rect.BottomRight.X then
    Result.Left := Rect.TopLeft.X
  else
    Result.Left := Rect.BottomRight.X;

  if Rect.TopLeft.Y < Rect.BottomRight.Y then
    Result.Top := Rect.TopLeft.Y
  else
    Result.Top := Rect.BottomRight.Y;

  Result.Width := abs( Rect.BottomRight.X - Rect.TopLeft.X);
  Result.Height := abs( Rect.BottomRight.Y - Rect.TopLeft.Y);
end;

function DecToHex(Dec: int64): string;
var
  I: Integer;
begin
  //result:= digits[Dec shr 4]+digits[Dec and $0F];
  Result := IntToHex(Dec);

  for I := 1 to length(Result) do
      if (Result[1] = '0') and (Length(Result) > 2) then
        Result := Result.Remove(0, 1)
      else
        Break;

  if Result = '' then
        Result := '00';
end;

{ FXRGBA }

function FXRGBA.Create(Red, Green, Blue, Alpha: Byte): FXRGBA;
begin
  R := Red;
  G := Green;
  B := Blue;

  A := Alpha;

  Result := Self;
end;

procedure FXRGBA.FromColor(Color: TColor; Alpha: Byte);
var
  RBGval: longint;
begin
  RBGval := ColorToRGB(Color);

  try
    R := GetRValue(RBGval);
    G := GetGValue(RBGval);
    B := GetBValue(RBGval);

    A := Alpha;
  finally

  end;
end;

function FXRGBA.MakeGDIBrush: TGPSolidBrush;
begin
  Result := TGPSolidBrush.Create( MakeColor(A, R, G, B) );
end;

function FXRGBA.MakeGDIPen(Width: Single): TGPPen;
begin
  Result := TGPPen.Create( MakeColor(A, R, G, B), Width );
end;

function FXRGBA.ToColor(Alpha: Byte): TColor;
begin
  Result := RGB(R, G, B);

  A := Alpha;
end;

{ TRoundRect }

procedure TRoundRect.Create(TopLeft, BottomRight: TPoint; Rnd: integer);
begin
  Rect := TRect.Create(TopLeft, BottomRight);

  SetRoundness( Rnd );
end;

procedure TRoundRect.Create(SRect: TRect; Rnd: integer);
begin
  Rect := SRect;

  SetRoundness( Rnd );
end;

function TRoundRect.Bottom: integer;
begin
  Result := Rect.Bottom;
end;

function TRoundRect.BottomRight: TPoint;
begin
  Result := Rect.BottomRight;
end;

procedure TRoundRect.Create(Left, Top, Right, Bottom, Rnd: integer);
begin
  Rect := TRect.Create(Left, Top, Right, Bottom);

  SetRoundness( Rnd );
end;

function TRoundRect.GetRoundness: integer;
begin
  Result := round( (Self.RoundTL + Self.RoundTR + Self.RoundBL + Self.RoundBR) / 4 );
end;

function TRoundRect.Height: integer;
begin
  Result := Rect.Height;
end;

function TRoundRect.Left: integer;
begin
  Result := Rect.Left;
end;

procedure TRoundRect.Offset(const DX, DY: Integer);
begin
  Rect.Offset(DX, DY);
end;

function TRoundRect.Right: integer;
begin
  Result := Rect.Right;
end;

function TRoundRect.RoundX: integer;
begin
  Result := round( (Self.RoundTL + Self.RoundTR + Self.RoundBL + Self.RoundBR) / 4 );
end;

function TRoundRect.RoundY: integer;
begin
    Result := round( (Self.RoundTL + Self.RoundTR + Self.RoundBL + Self.RoundBR) / 4 );
end;

procedure TRoundRect.SetRoundness(Value: integer);
begin
  RoundTL := Value;
  RoundTR := Value;
  RoundBL := Value;
  RoundBR := Value;
end;

function TRoundRect.Top: integer;
begin
  Result := Rect.Top;
end;

function TRoundRect.TopLeft: TPoint;
begin
  Result := Rect.TopLeft;
end;

function TRoundRect.Width: integer;
begin
  Result := Rect.Width;
end;

{ TLine }

procedure TLine.Create(P1, P2: TPoint);
begin
  Point1 := P1;
  Point2 := P2;
end;

function TLine.GetHeight: integer;
begin
  Result := abs(Point1.Y - Point2.Y);
end;

function TLine.GetWidth: integer;
begin
  Result := abs(Point1.X - Point2.X);
end;

procedure TLine.OffSet(const DX, DY: Integer);
begin
  Inc( Point1.X, DX );
  Inc( Point1.Y, DY );
  Inc( Point2.X, DX );
  Inc( Point2.Y, DY );
end;

function TLine.Rect: TRect;
begin
  Result := GetValidRect(Point1, Point2);
end;

procedure TLine.SetPercentage(Percentage: real);
var
  DistX, DistY: integer;
begin
  DistX := Point2.X - Point1.X;
  DistY := Point2.Y - Point1.Y;

  Point2.X := Point1.X + round(Percentage / 100 * DistX);
  Point2.Y := Point1.Y + round(Percentage / 100 * DistY);
end;

procedure TLine.SwapPoints;
var
  ATemp: TPoint;
begin
  ATemp := Point1;
  Point1 := Point2;
  Point2 := ATemp;
end;

function TLine.Center: TPoint;
begin
  Result := Point( (Point1.X + Point2.X) div 2, (Point1.Y + Point2.Y) div 2);
end;

end.
