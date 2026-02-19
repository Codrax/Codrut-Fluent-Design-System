unit CFX.Types;

/// ? - What is "Ported from CLB"
///
///  This clause means the following declared type/function is imported as was
///  from Codrut's Library Pack. A suite of system-like units developed for
///  Embarcadero Delphi and Lazarus, and cross-platform devices.

{$SCOPEDENUMS ON}

interface
uses
  UITypes, Types, VCl.GraphUtil, Winapi.Windows,
  Classes, Vcl.Themes, Vcl.Controls, Vcl.Graphics, Math,
  SysUtils, Winapi.GDIPAPI, Winapi.GDIPOBJ, System.Generics.Defaults;

type
  // File Type
  TFileType = (Text, BMP, PNG, JPEG, GIF, HEIC, TIFF,
    MP3, MP4, Flac, MDI, OGG, SND, M3U8, EXE, MSI,
    Zip, GZip, Zip7, Cabinet, TAR, RAR, LZIP, ISO,
    PDF, HLP, CHM);

  // Control Redraw
  FXRedrawFlag = (RedrawBuffer, Paint, Invalidate, UpdateChildren, UpdateAbove, Force);
  FXRedrawFlags = set of FXRedrawFlag;

  // Icon Type
  FXIconType = (None, Image, BitMap, ImageList, SegoeIcon);

  // Color
  FXColorType = (Accent, Foreground, Background, Content, Custom);

  // Component
  FXFocusFlag = (CatchTab, CatchLeft, CatchUp, CatchRight, CatchDown);
  FXFocusFlags = set of FXFocusFlag;

  // FXForm
  FXFormFill = (TitleBar, Complete);
  FXFormCloseAction = (Default, Free, Hide, Minimise);
  FXFormCornerPreference = (    //
    Default = 0,          // Default
    DoNotRound = 1,       // Rectangular
    Round = 2,            // Default
    RoundSmall = 3,       // Semi-rounded
    Customized = 100      // Customized settings
  );
  FXFormBackdropType = (       //
    Automatic = 0,            // Auto
    None = 1,         // None
    Mica = 2,      // Mica
    Acrylic = 3 // Acrylic
    //DWMSBT_TABBEDWINDOW = 4     // Tabbed
  );

  // Animation
  FXTaskStatus = (Stopped, Running, Paused);
  FXAnimationKind = (Linear, Exponential, ReverseExpo, Random, Spring, Sinus,
    SinusArc, Wobbly, Pulse);

  // Number
  FXNumberType = (Integer, Extended, Currency);

  // Detail
  FXDetailType = (None, Underline, Outline);

  // FXPanel
  FXBackgroundColor = (Background, Content);

  // FXButton
  FXButtonKind = (Normal, Accent, Toggle, FlatToggle, Dropdown, Link, Flat);

  // FXPopupMenu
  FXAnimateSelection = (Instant, Opacity, Linear, Square);

  // FXProgress
  FXProgressKind = (Normal, Intermediate, Paused, Error);

  // FXCheckBox
  FXCheckBoxState = (Checked, Unchecked, Grayed);

  // FXAppManager
  TAppManagerFlag = (WantsAppData);
  TAppManagerFlags = set of TAppManagerFlag;

  FXAppTask = (UpdatePrompt, UpdateForce, UpdateShowUserScreen, WindowSaveForm, WindowLoadForm);
  FXAppTasks = set of FXAppTask;
  FXAppFormAssistTask = (WindowSaveForm, WindowLoadForm);
  FXAppFormAssistTasks = set of FXAppFormAssistTask;

  FXAppSetting = (PermitOverrideSingleInstance);
  FXAppSettings = set of FXAppSetting;

  // FXBlurMaterial
  FXGlassRefreshMode = (Automatic, Manual, Timer);
  FXBlurVersion = (WallpaperBlurred, Wallpaper, Screenshot, None);
  TWallpaperSetting = (Fill, Fit, Stretch, Tile, Center, Span);

  // FXStandardIcons
  FXStandardIconType = (None, Checkmark, Error, Question, Information, Warning, Star);

  // FXSlider
  FXOrientation = (Horizontal, Vertical);
  FXContentJustify = (Start, Center, SpaceBetween, SpaceAround, SpaceEvenly, Ending, Stretch(*Not widely supported*));

  // FXPicture
  FXPictureContentFill = (None, Stretch, Fill, Fit);

  // Graphics
  FXTextFlag = (WordWrap,
                Top, VerticalCenter, Bottom, (* Vertical allignment *)
                Left, Center, Right, (* Horizontal allignment *)
                Auto, (* Function Ex, Depracated *)
                NoClip, HotkeyChars, ShowAccelChar, (* Settings *)
                TrimCutoff, TrimDots, (* <!!> Trim Type, requires pos *)
                TrimCharacter, TrimWord, (* <!!> Trim Pos, requires TrimNone or TrimDots *)
                TrimPath (* Trim Pos, requires TrimDots *)
                );
  FXTextFlags= set of FXTextFlag;

  FXDrawLayout = (Left, Top, Right, Bottom);

  // **Ported from CLB
  TLayout = (Beginning, Center, Ending);
  TCorners = (TopLeft, TopRight, BottomLeft, BottomRight);

  // Text
  FXCharCase = (Both, Uppercase, Lowercase);

  // Theme Color
  FXDarkSetting = (Auto=0, ForceLight=1, ForceDark=2);

  // File
  FXUserShell = (User, AppData, AppDataLocal, Documents,
                    Pictures, Desktop, Music, Videos,
                    Network, Recent, StartMenu, Startup,
                    Downloads, Programs);

  FXFileDateType = (Create, Modify, Access);

  // Other instance
  FXOtherInstanceAction = (Nothing, Close, KillProcess);
  FXOnOtherInstance = procedure(Sender: TObject; var Action: FXOtherInstanceAction) of object;

  // Theme Change Detection
  FXThemeType = (Redraw, Colorization, AppTheme);
  FXThemeChange = procedure(Sender: TObject; ThemeChange: FXThemeType) of object;

  // Controls
  FXControlState = (None, Hover, Press);
  FXControlOnPaint = procedure(Sender: TObject) of object;
  FXControlOnIntegerValue = procedure(Sender: TObject; Value: integer) of object;
  FXControlOnIntegerSourceDestination = procedure(Sender: TObject; Source, Destination: integer) of object;
  FXControlOnDrawRectIndex = procedure(Sender: TObject; AIndex: integer; ARect: TRect; Canvas: TCanvas) of object;
  FXControlOnDrawRectIndexDrawDefault = procedure(Sender: TObject; AIndex: integer; ARect: TRect; Canvas: TCanvas; var DrawDefault: boolean) of object;

  // Thingies
  FXPercent = type Single;
  FXAngle = type Single; // three decimal places

  // Switch for any variabile type
  TSwitch<T> = class
    type
    TCase = record
      Values: TArray<T>;
      CallBack: TProc;

      procedure Execute;
    end;
    TCases = TArray<TCase>;

    // Make
    class function Option(Value: T; Call: TProc): TCase; overload;
    class function Option(Values: TArray<T>; Call: TProc): TCase; overload;

    // Switch
    class procedure Switch(Value: T; Cases: TArray<TCase>); overload;
    class procedure Switch(Value: T; Cases: TArray<TCase>; Default: TProc); overload;
  end;

  // Type helper for any
  TType<T> = class(TObject)
  public
    class function IfElse(Condition: boolean; IfTrue: T; IfFalse: T): T;
    class procedure Switch(var A, B: T);
    class procedure SwitchPointer(const A, B: T);
    class function Compare(const A, B: T): TValueRelationship;
  end;

  // Rects
  TRectLayoutContentFill = (None, Stretch, Fill, Fit, SelfProportion, ParentProportion);
  TRectLayoutTileFlag = (ExtendX, ExtendY);
  TRectLayoutTileFlags = set of TRectLayoutTileFlag;
  TRectLayout = record
    // Alignment
    LayoutHorizontal: TLayout;
    LayoutVertical: TLayout;

    CenterDivisor: TSizeF; // divide size by to get the CENTER
    ProportionScale: TSizeF; // scale either CHILD or PARENT by provided scale values

    // Fill method
    ContentFill: TRectLayoutContentFill;

    // Tile content
    Tile: boolean;
    TileFlags: TRectLayoutTileFlags;

    // Margins
    MarginTile: integer;
    MarginParent: integer;
    MarginSelf: integer;

    class function New: TRectLayout; static;
  end;

  // FXColor Helper
  TAlphaColorHelper = record helper for TAlphaColor
  public
    // Change value
    function GetAlpha: byte;
    function GetR: byte;
    function GetG: byte;
    function GetB: byte;

    procedure SetAlpha(Value: byte);
    procedure SetR(Value: byte);
    procedure SetG(Value: byte);
    procedure SetB(Value: byte);

    // Props
    property Alpha: byte read GetAlpha write SetAlpha;
    property R: byte read GetR write SetR;
    property G: byte read GetR write SetG;
    property B: byte read GetR write SetB;

    // Utilities
    function ColorGrayscale(ToneDown: integer = 3): TAlphaColor;
    function ColorInvert: TAlphaColor;
    function ChangeSaturation(ByIncrement: integer): TAlphaColor;
    function Blend(WithColor: TAlphaColor; BlendAmoung: byte): TAlphaColor;
    function GetLightValue: byte;

    // GDI
    function MakeGDIBrush: TGPSolidBrush;
    function MakeGDIPen(Width: Single = 1): TGPPen;

    // Convert
    function ToVclColor: TColor;
    function ToString: string;

    // Constructors
    class function Create(R, G, B: Byte; A: Byte = 255): TAlphaColor; overload; static;
    class function Create(AColor: TColor; A: Byte = 255): TAlphaColor; overload; static;
    class function Create(AString: string): TAlphaColor; overload; static;
    class function RandomColor(RandomAlpha: boolean=false): TAlphaColor; static;
  end;

  // Helper for percentage
  FXPercentHelper = record helper for FXPercent
    public
      function Percentage: real;

      function OfNumber(Value: int64): real; overload;
      function OfNumber(Value: real): real; overload;

      function OfNumberInt(Value: int64): int64; overload;
      function OfNumberInt(Value: real): int64; overload;

      function ToByte: byte;
      function ToString(Decimals: integer=2): string;
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

    Corners: TCorners;

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
function GetRGB(AColor: TColor; Alpha: Byte = 255): TAlphaColor; overload;
function GetRGB(R, G, B: Byte; Alpha: Byte = 255): TAlphaColor; overload;

{ Point }
function RotatePointAroundPoint(APoint: TPoint; ACenter: TPoint; ARotateDegree: real; ACustomRadius: real = -1): TPoint;

{ Rectangles }
///  <summary>Translate a rectangle that is positioned in a bigger client rectangle (parent rect)
///  to a new rectangle. Or: Translate CHILD positions from PARENT1 to PARENT2. </summary>
function TranslateRect(const Rect, Client, Dest: TRect): TRect;
function GetValidRect(Point1, Point2: TPoint): TRect; overload;
function GetValidRect(Points: TArray<TPoint>): TRect; overload;
function GetValidRect(Points: TArray<TPointF>): TRectF; overload;
function GetValidRect(Rect: TRect): TRect; overload;
procedure CenterRectInRect(var ARect: TRect; const ParentRect: TRect);
procedure CenterRectAtPoint(var ARect: TRect; const APoint: TPoint);
function PointInRect(Point: TPoint; Rect: TRect): boolean;
procedure ContainRectInRect(var ARect: TRect; const ParentRect: TRect);
///  Morph rectangle or point from a value to the destination rectangle
///  based on the percent provided. The percent is from 0.00 to 1.00
///  NOTE: Rectangles must be normalised!
/// <summary>Animate from the source to the destination TRect using a percentage.</summary>
function MorphToRect(Source: TRect; Destination: TRect; Percent: single): TRect; overload;
/// <summary>Animate from the source to the destination TRectF using a percentage.</summary>
function MorphToRect(Source: TPoint; Destination: TRect; Percent: single): TRect; overload;
/// <summary>Get the rectangle layouts of a element in a parent rectangle using specified layout settings.</summary>
function RectangleLayouts(const Element: TSize; Parent: TRect; Layout: TRectLayout): TArray<TRect>; overload;
/// <summary>Get the rectangle layouts of a element in a parent rectangle using specified layout settings.</summary>
function RectangleLayouts(const Element: TRect; Parent: TRect; Layout: TRectLayout): TArray<TRect>; overload;

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

function GetRGB(AColor: TColor; Alpha: Byte): TAlphaColor;
begin
  Result := TAlphaColor.Create(AColor, Alpha);
end;

function GetRGB(R, G, B: Byte; Alpha: Byte): TAlphaColor;
begin
  Result := TAlphaColor.Create(R, G, B, Alpha);
end;

function RotatePointAroundPoint(APoint: TPoint; ACenter: TPoint; ARotateDegree: real; ACustomRadius: real): TPoint;
var
  r, dg, cosa, sina, ncos, nsin, dsin, dcos: real;
begin
  dg := (ARotateDegree * pi / 180);

  dsin := sin(dg);
  dcos := cos(dg);

  if ACustomRadius = -1 then
    r := ACenter.Distance(APoint)
  else
    r := ACustomRadius;

  if r <> 0 then
    begin
      cosa := (APoint.X - ACenter.X) / r;
      sina := (APoint.Y - ACenter.Y) / r;


      nsin := sina * dcos + dsin * cosa;
      ncos := cosa * dcos - sina * dsin;


      // Apply New Properties
      Result.X := round( ACenter.X + r * ncos );
      Result.Y := round( ACenter.Y + r * nsin );
    end;
end;

function TranslateRect(const Rect, Client, Dest: TRect): TRect;
var
  OffsetX, OffsetY: Integer;
begin
  // Calculate the offset between the top-left corners of Client and Dest
  OffsetX := Dest.Left - Client.Left;
  OffsetY := Dest.Top - Client.Top;

  // Apply the offset to the coordinates of Rect
  Result.Left := Rect.Left + OffsetX;
  Result.Top := Rect.Top + OffsetY;
  Result.Right := Rect.Right + OffsetX;
  Result.Bottom := Rect.Bottom + OffsetY;
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

function GetValidRect(Points: TArray<TPointF>): TRectF; overload;
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

procedure CenterRectInRect(var ARect: TRect; const ParentRect: TRect);
var
  NewLeft, NewTop: Integer;
begin
  NewLeft := ParentRect.Left + (ParentRect.Width - ARect.Width) div 2;
  NewTop := ParentRect.Top + (ParentRect.Height - ARect.Height) div 2;
  ARect.Offset(NewLeft - ARect.Left, NewTop - ARect.Top);
end;

procedure CenterRectAtPoint(var ARect: TRect; const APoint: TPoint);
var
  ACenter: TPoint;
begin
  ACenter := ARect.CenterPoint;
  ARect.Offset(APoint.X-ACenter.X, APoint.Y-ACenter.Y);
end;

function PointInRect(Point: TPoint; Rect: TRect): boolean;
begin
  Result := Rect.Contains(Point);
end;

procedure ContainRectInRect(var ARect: TRect; const ParentRect: TRect);
var
  Left, Top, Right, Bottom: integer;
begin
  Left := ParentRect.Left - ARect.Left;
  Top := ParentRect.Top - ARect.Top;
  Right := ParentRect.Right - ARect.Right;
  Bottom := ParentRect.Bottom - ARect.Bottom;

  if Left > 0 then
    ARect.Offset(Left, 0);
  if Top > 0 then
    ARect.Offset(0, Top);
  if Right < 0 then
    ARect.Offset(Right, 0);
  if Bottom < 0 then
    ARect.Offset(0, Bottom);
end;

function MorphToRect(Source: TRect; Destination: TRect; Percent: single): TRect;
begin
  Result := Source;

  Inc(Result.Left,
    round((Destination.Left-Source.Left)*Percent)
    );
  Inc(Result.Top,
    round((Destination.Top-Source.Top)*Percent)
    );
  Inc(Result.Right,
    round((Destination.Right-Source.Right)*Percent)
    );
  Inc(Result.Bottom,
    round((Destination.Bottom-Source.Bottom)*Percent)
    );
end;

function MorphToRect(Source: TPoint; Destination: TRect; Percent: single): TRect;
begin
  Result := MorphToRect(TRect.Create(Source), Destination, Percent);
end;

function RectangleLayouts(const Element: TSize; Parent: TRect; Layout: TRectLayout): TArray<TRect>; overload;
var
  Base: TRect;

  BoundBottomRight: TPoint;
begin
  // Shrink Margins
  if Layout.MarginParent <> 0 then
    Parent.Inflate(-Layout.MarginParent, -Layout.MarginParent);

  if (Element.Width = 0) or (Element.Height = 0) then
    Exit;

  // Calculate base
  Base := TRect.Empty;
  case Layout.ContentFill of
    TRectLayoutContentFill.Stretch: Base := Parent;
    TRectLayoutContentFill.Fill: begin
      Base := Parent;

      // Get proportions
      const Scale = Element .Height * (Base.Width / Element.Width);
      if Scale < Base.Height then
        Base.Width := trunc(Element.Width * (Base.Height / Element.Height))
          else
            Base.Height := trunc(Scale);
    end;
    TRectLayoutContentFill.Fit: begin
      Base := Parent;

      // Get proportions
      const Scale = Element.Height * (Base.Width / Element.Width);
      if Scale > Base.Height then
        Base.Width := trunc(Element.Width * (Base.Height / Element.Height))
          else
            Base.Height := trunc(Scale);
    end;
    TRectLayoutContentFill.SelfProportion: begin
      Base := TRect.Create(Parent.TopLeft,
        round(Element.Width * Layout.ProportionScale.cx),
        round(Element.Height * Layout.ProportionScale.cy));
    end;
    TRectLayoutContentFill.ParentProportion: begin
      Base := TRect.Create(Parent.TopLeft,
        round(Parent.Width * Layout.ProportionScale.cx),
        round(Parent.Height * Layout.ProportionScale.cy));
    end

    // Default, keep same size
    else Base := TRect.Create(Parent.TopLeft, Element.Width, Element.Height);
  end;

  // Layout
  if Layout.Tile then begin
    Result := [Base];
    var ColCount, RowCount, DivTotal: integer;
    var ElemSize: TSize;
    ElemSize := TSize.Create(Base.Width+Layout.MarginTile, Base.Height+Layout.MarginTile);

    // Calculate columns
    DivTotal := (Parent.Width+Layout.MarginTile);
    ColCount := DivTotal div ElemSize.cx;
    if TRectLayoutTileFlag.ExtendX in Layout.TileFlags then
      if DivTotal mod ElemSize.cx > 0 then
        Inc(ColCount);


    // Calculate rows
    DivTotal := (Parent.Height+Layout.MarginTile);
    RowCount := DivTotal div ElemSize.cy;
    if TRectLayoutTileFlag.ExtendY in Layout.TileFlags then
      if DivTotal mod ElemSize.cy > 0 then
        Inc(RowCount);

    // Calculate each
    SetLength(Result, RowCount*ColCount);
    BoundBottomRight := Base.TopLeft;
    for var Row := 0 to RowCount-1 do
      for var Col := 0 to ColCount-1 do begin
        const Index = Row*ColCount + Col;
        Result[Index] := Base;
        Result[Index].Offset( ElemSize.cx*Col, ElemSize.cy*Row );
      end;

    // Bottom right
    BoundBottomRight := TPoint.Create(
      Base.Left+ElemSize.cx*(ColCount-1) + Base.Width,
      Base.Top+ElemSize.cy*(RowCount-1) + Base.Height);
  end
    else begin
      Result := [Base];

      BoundBottomRight := Base.BottomRight;
    end;

  // Layout
  if (Layout.LayoutHorizontal <> TLayout.Beginning) or (Layout.LayoutVertical <> TLayout.Beginning) then begin
    var Offset: TPoint;
    Offset := TPoint.Zero;

    // Horizontal offset
    case Layout.LayoutHorizontal of
      TLayout.Center: Offset.X := trunc((Parent.Right - BoundBottomRight.X) / Layout.CenterDivisor.cx);
      TLayout.Ending: Offset.X := Parent.Right - BoundBottomRight.X;
    end;
    // Vertical offset
    case Layout.LayoutVertical of
      TLayout.Center: Offset.Y := trunc((Parent.Bottom - BoundBottomRight.Y) / Layout.CenterDivisor.cy);
      TLayout.Ending: Offset.Y := Parent.Bottom - BoundBottomRight.Y;
    end;

    for var I := 0 to High(Result) do
      Result[I].Offset( Offset );
  end;

  // Margin self
  if Layout.MarginSelf <> 0 then
    for var I := 0 to High(Result) do
      Result[I].Inflate(-Layout.MarginSelf, -Layout.MarginSelf);
end;

function RectangleLayouts(const Element: TRect; Parent: TRect; Layout: TRectLayout): TArray<TRect>;
begin
  Result := RectangleLayouts(TSize.Create(Element.Width, Element.Height), Parent, Layout);
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

{ FXPercentHelper }

function FXPercentHelper.OfNumber(Value: int64): real;
begin
  Result := Value * Percentage;
end;

function FXPercentHelper.OfNumber(Value: real): real;
begin
  Result := Value * Percentage;
end;

function FXPercentHelper.OfNumberInt(Value: int64): int64;
begin
  Result := trunc(OfNumber(Value)*Percentage);
end;

function FXPercentHelper.OfNumberInt(Value: real): int64;
begin
  Result := trunc(OfNumber(Value)*Percentage);
end;

function FXPercentHelper.Percentage: real;
begin
  Result := Self / 100;
end;

function FXPercentHelper.ToByte: byte;
begin
  Result := round(EnsureRange(Percentage, 0, 1)*255);
end;

function FXPercentHelper.ToString(Decimals: integer): string;
begin
  Result := Format('%.'+Decimals.ToString+'f', [Self]);
end;

{ TAlphaColorHelper }

class function TAlphaColorHelper.Create(AColor: TColor; A: Byte): TAlphaColor;
begin
  {$R-}
  Result := (GetBValue(AColor) or (GetGValue(AColor) shl 8) or (GetRValue(AColor) shl 16) or (A shl 24));
  {$R+}
end;

class function TAlphaColorHelper.Create(R, G, B, A: Byte): TAlphaColor;
begin
  Result := (B or (G shl 8) or (R shl 16) or (A shl 24));
end;

function TAlphaColorHelper.Blend(WithColor: TAlphaColor; BlendAmoung: byte): TAlphaColor;
begin
  Result := Create(
    R + (WithColor.R - R) * BlendAmoung div 255,
    G + (WithColor.G - G) * BlendAmoung div 255,
    B + (WithColor.B - B) * BlendAmoung div 255,
  Alpha);
end;

function TAlphaColorHelper.ChangeSaturation(ByIncrement: integer): TAlphaColor;
begin
  Result := Create(EnsureRange(R+ByIncrement, 0, 255),
    EnsureRange(G+ByIncrement, 0, 255),
    EnsureRange(B+ByIncrement, 0, 255), Alpha);
end;

function TAlphaColorHelper.ColorGrayscale(ToneDown: integer = 3): TAlphaColor;
begin
  const Val = (R + G + B) div ToneDown;

  Result := TAlphaColor.Create(Val, Val, Val, Alpha);
end;

function TAlphaColorHelper.ColorInvert: TAlphaColor;
begin
  Result := Create(255-R, 255-G, 255-B, Alpha);
end;

class function TAlphaColorHelper.Create(AString: string): TAlphaColor;
begin
  if AString[1] = '#' then
    Result := StrToInt('$' + Copy(AString, 2, 8))
  else
    Exit( AString.ToInteger );
end;

function TAlphaColorHelper.GetAlpha: byte;
begin
  Result := (Self and $FF000000) shr 24;
end;

function TAlphaColorHelper.GetR: byte;
begin
  Result := (Self and $00FF0000) shr 16;
end;

function TAlphaColorHelper.MakeGDIBrush: TGPSolidBrush;
begin
  Result := TGPSolidBrush.Create( Self );
end;

function TAlphaColorHelper.MakeGDIPen(Width: Single): TGPPen;
begin
  Result := TGPPen.Create( Self, Width );
end;

class function TAlphaColorHelper.RandomColor(RandomAlpha: boolean): TAlphaColor;
begin
  if RandomAlpha then
    Result := Create(Random(256), Random(256), Random(256), Random(256))
  else
    Result := Create(Random(256), Random(256), Random(256), 255);
end;

function TAlphaColorHelper.GetG: byte;
begin
  Result := (Self and $0000FF00) shr 8;
end;

function TAlphaColorHelper.GetLightValue: byte;
begin
  Result := (R + B + G) div 3;
end;

function TAlphaColorHelper.GetB: byte;
begin
  Result := (Self and $000000FF);
end;

procedure TAlphaColorHelper.SetAlpha(Value: byte);
begin                              // Typecast value to larger bit size, $00*4
  Self := (Self and $00FFFFFF) or (TAlphaColor(Value) shl 24);
end;

procedure TAlphaColorHelper.SetR(Value: byte);
begin                              // Typecast value to larger bit size, $00*4
  Self := (Self and $FF00FFFF) or (TAlphaColor(Value) shl 16);
end;

function TAlphaColorHelper.ToString: string;
begin
  Result := '#' + IntToHex(Self, 8);
end;

function TAlphaColorHelper.ToVclColor: TColor;
begin
  // Better to use GetRGB, as FXColor is AARRGGBB, while TColor is 00BBGGRR
  Result := RGB(GetR, GetG, GetB);
end;

procedure TAlphaColorHelper.SetG(Value: byte);
begin                              // Typecast value to larger bit size, $00*4
  Self := (Self and $FFFF00FF) or (TAlphaColor(Value) shl 8);
end;

procedure TAlphaColorHelper.SetB(Value: byte);
begin                              // Typecasting is not required
  Self := (Self and $FFFFFF00) or (Value);
end;

{ TSwitch<T> }

class function TSwitch<T>.Option(Value: T; Call: TProc): TCase;
begin
  Result := Option([Value], Call);
end;

class function TSwitch<T>.Option(Values: TArray<T>; Call: TProc): TCase;
begin
  Result.Values := Values;
  Result.CallBack := Call;
end;

class procedure TSwitch<T>.Switch(Value: T; Cases: TArray<TCase>; Default: TProc);
begin
  for var I := 0 to High(Cases) do
    for var J := 0 to High(Cases[I].Values) do
      if TComparer<T>.Default.Compare(Cases[I].Values[J], Value) = EqualsValue then begin
        Cases[I].Execute;
        Exit;
      end;

  // Default
  if Assigned(Default) then
    Default;
end;

class procedure TSwitch<T>.Switch(Value: T; Cases: TArray<TCase>);
begin
  Switch(Value, Cases, nil);
end;

{ TSwitch<T>.TCase }

procedure TSwitch<T>.TCase.Execute;
begin
  Callback;
end;

{ TType<T> }

class function TType<T>.Compare(const A, B: T): TValueRelationship;
var
  lComparer: IComparer<T>;
begin
  lComparer := TComparer<T>.Default;

  Result := lComparer.Compare(A, B);
end;

class function TType<T>.IfElse(Condition: boolean; IfTrue, IfFalse: T): T;
begin
  if Condition then
    Result := IfTrue
  else
    Result := IfFalse;
end;

class procedure TType<T>.Switch(var A, B: T);
var
  Temp: T;
begin
  Temp := A;
  A := B;
  B := Temp;
end;

class procedure TType<T>.SwitchPointer(const A, B: T);
var
  Temp: T;
type
  PointerT = ^T;
begin
  Temp := A;
  T(Pointer(@A)^) := B;
  T(Pointer(@B)^) := Temp;
end;

{ TRectLayout }

class function TRectLayout.New: TRectLayout;
begin
  Result.LayoutHorizontal := TLayout.Beginning;
  Result.LayoutVertical := TLayout.Beginning;

  Result.CenterDivisor := TSizeF.Create(2, 2);
  Result.ProportionScale := TSizeF.Create(1, 1);

  Result.ContentFill := TRectLayoutContentFill.None;
  Result.Tile := false;
  Result.TileFlags := [TRectLayoutTileFlag.ExtendX, TRectLayoutTileFlag.ExtendY];

  Result.MarginTile := 0;
  Result.MarginParent := 0;
  Result.MarginsELF := 0;
end;

end.
