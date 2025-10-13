unit CFX.Graphics;

interface

uses
  UITypes,
  Types,
  CFX.Constants,
  Vcl.GraphUtil,
  Winapi.Windows,
  Classes,
  SysUtils,
  Vcl.Themes,
  Vcl.Controls,
  Vcl.Graphics,
  CFX.VarHelpers,
  CFX.Types,
  CFX.BlurFunctions,
  CFX.ThemeManager,
  Math,
  CFX.StringUtils,
  CFX.ArrayHelpers;

type
  TPent = array[0..4] of TPoint;

  TDrawMode = (Fill, Fit, Stretch, Center, CenterFill, Center3Fill,
    CenterFit, Normal, Tile); { Windows DWM use a Center3 Fill }

// Graphic Utilities
procedure GetCenterPos(Width, Height: Integer; Rect: TRect; out X, Y: Integer);
function CreateSolidBrushWithAlpha(Color: TColor; Alpha: Byte = $FF): HBRUSH;

// Text Drawing Utils
procedure DrawTextRect(Canvas: TCanvas; ARect: TRect; Text: string;
  Flags: FXTextFlags; AMargin: integer = 0);
function GetTextRect(Canvas: TCanvas; ARect: TRect; Text: string;
  Flags: FXTextFlags; AMargin: integer = 0): TRect;
function GetWordWrapLines(Canvas: TCanvas; Text: string;
  ARect: TRect): TArray<string>;
function WordWrapGetLineHeight(Canvas: TCanvas; Text: string): integer;

// Font Size
function GetMaxFontHeight(Canvas: TCanvas; Text: string; MaxWidth, MaxHeight: Integer): integer;

// Drawing Functions
procedure DrawBorder(const Canvas: TCanvas; R: TRect; Color: TColor;
                     Thickness: Byte; Roundness: integer = 10);
procedure CopyRoundRect(FromCanvas: TCanvas; FromRect: TRoundRect;
                        DestCanvas: TCanvas; DestRect: TRect;
                        shrinkborder: integer = 0);

procedure CopyRectWithOpacity(Dest: TCanvas; DestRect: TRect; Source: TCanvas; SourceRect: TRect; Opacity: Byte);

procedure DrawCheckedboard(Canvas: TCanvas; ARect: TRect; Width, Height: Integer; Color1, Color2: TColor);

// Color Inversion
procedure StretchInvertedMask(Source: TBitMap; Destination: TCanvas; DestRect: TRect); overload;
procedure StretchInvertedMask(Source: TCanvas; Destination: TCanvas; DestRect: TRect); overload;

function Desaturate(Color: TColor): TColor; overload;
procedure Desaturate(Bitmap: TBitmap); overload;

{ Drawing }
function DrawModeToImageLayout(DrawMode: TDrawMode): TRectLayout;
procedure DrawImageInRect(Canvas: TCanvas; Rect: TRect; Image: TGraphic;
  Layout: TRectLayout; ClipImage: boolean = false; Opacity: byte = 255); overload;
procedure DrawImageInRect(Canvas: TCanvas; Rect: TRect; Image: TGraphic;
  DrawMode: TDrawMode = TDrawMode.Fill; ImageMargin: integer = 0;
  ClipImage: boolean = false; Opacity: byte = 255); overload;

// Gradient
procedure GradHorizontal(Canvas:TCanvas; Rect:TRect; FromColor, ToColor:TColor);
procedure GradVertical(Canvas:TCanvas; Rect:TRect; FromColor, ToColor:TColor);

// Bitmap Manipulation
procedure FastBlur(Bitmap: TBitmap; Radius: Real; BlurScale: Integer; HighQuality: Boolean = True);

// Icon drawing
procedure DrawFontIcon(Canvas: TCanvas; Icon: string; Color: TColor; Rect: TRect);

const
  HAlignments: Array[TAlignment] of Longint = (DT_LEFT, DT_RIGHT, DT_CENTER);
  VAlignments: Array[TVerticalAlignment] of Longint = (DT_TOP, DT_BOTTOM, DT_VCENTER);

implementation

procedure GetCenterPos(Width, Height: Integer; Rect: TRect; out X, Y: Integer);
begin
  X := Rect.Left + (Rect.Width - Width) div 2;
  Y := Rect.Top + (Rect.Height - Height) div 2;
end;

function DrawModeToImageLayout(DrawMode: TDrawMode): TRectLayout;
begin
  Result := TRectLayout.New;

  // Set
  case DrawMode of
    TDrawMode.Fill: Result.ContentFill := TRectLayoutContentFill.Fill;
    TDrawMode.Fit: Result.ContentFill := TRectLayoutContentFill.Fit;
    TDrawMode.Stretch: Result.ContentFill := TRectLayoutContentFill.Stretch;
    TDrawMode.Center: begin
      Result.LayoutHorizontal := TLayout.Center;
      Result.LayoutVertical := TLayout.Center;
    end;
    TDrawMode.CenterFill: begin
      Result.ContentFill := TRectLayoutContentFill.Fill;

      Result.LayoutHorizontal := TLayout.Center;
      Result.LayoutVertical := TLayout.Center;
    end;
    TDrawMode.Center3Fill: begin
      Result.ContentFill := TRectLayoutContentFill.Fill;

      Result.LayoutHorizontal := TLayout.Center;
      Result.LayoutVertical := TLayout.Center;

      Result.CenterDivisor := TSizeF.Create(3, 3);
    end;
    TDrawMode.CenterFit: begin
      Result.ContentFill := TRectLayoutContentFill.Fit;

      Result.LayoutHorizontal := TLayout.Center;
      Result.LayoutVertical := TLayout.Center;
    end;
    TDrawMode.Tile: begin
      Result.Tile := true;
      Result.TileFlags := [TRectLayoutTileFlag.ExtendX, TRectLayoutTileFlag.ExtendY];
    end;
  end;
end;

procedure DrawImageInRect(Canvas: TCanvas; Rect: TRect; Image: TGraphic;
  DrawMode: TDrawMode; ImageMargin: integer; ClipImage: boolean; Opacity: byte);
var
  Layout: TRectLayout;
begin
  Layout := DrawModeToImageLayout(DrawMode);
  Layout.MarginParent := ImageMargin;

  // Data
  DrawImageInRect(Canvas, Rect, Image, Layout, ClipImage, Opacity);
end;

procedure DrawImageInRect(Canvas: TCanvas; Rect: TRect; Image: TGraphic;
  Layout: TRectLayout; ClipImage: boolean = false; Opacity: byte = 255);
var
  Rects: TArray<TRect>;
  I: integer;
  Bitmap: TBitMap;
  FRect: TRect;
begin
  // Get Rectangles
  Rects := RectangleLayouts(TSize.Create(Image.Width, Image.Height), Rect, Layout);

  if not ClipImage then
    // Standard Draw
    begin
      for I := 0 to High( Rects ) do
        Canvas.StretchDraw( Rects[I], Image, Opacity );
    end
  else
    // Clip Image Drw
    begin
      for I := 0 to High(Rects) do
        begin
          Bitmap := TBitMap.Create(Rect.Width, Rect.Height);
          Bitmap.PixelFormat := pf32bit;
          Bitmap.Transparent := true;

          const PIXEL_BYTE_SIZE = 4;

          // Fill image with
          for var Y := 0 to Bitmap.Height - 1 do
            FillMemory(Bitmap.ScanLine[Y], PIXEL_BYTE_SIZE * Bitmap.Width, 0);

          Bitmap.Canvas.Lock;
          try
            FRect := Rects[I];
            FRect.Offset( -Rect.Left, -Rect.Top );

            Bitmap.Canvas.StretchDraw(FRect, Image, 255); // Full opacity for temp

            // Image has no alpha channel, set A bytes to 255
            if not Image.Transparent then begin
              const RectZone = TRect.Intersect(Bitmap.Canvas.ClipRect, FRect);

              for var Y := RectZone.Top to RectZone.Bottom-1 do begin
                // Line
                const Pos: PByte = Bitmap.ScanLine[Y];

                // Start left
                for var X := RectZone.Left to RectZone.Right-1 do
                  Pos[X * PIXEL_BYTE_SIZE + 3] := 255;
              end;
            end;

            // Draw
            //Canvas.StretchDraw(Rect, BitMap, Opacity)
            Canvas.Draw(Rect.Left, Rect.Top, BitMap, Opacity);
          finally
            Bitmap.Canvas.Unlock;
            BitMap.Free;
          end;
        end;
    end;
end;

procedure GradHorizontal(Canvas:TCanvas; Rect:TRect; FromColor, ToColor:TColor);
var
   X: integer;
   dr, dg, db:Extended;
   r1, r2, g1, g2, b1, b2:Byte;
   R, G, B:Byte;
   cnt, csize:integer;
begin
  //Unpack Colors
  tocolor := ColorToRGB(tocolor);
  fromcolor := ColorToRGB(fromcolor);

   R1 := GetRValue(FromColor) ;
   G1 := GetGValue(FromColor) ;
   B1 := GetBValue(FromColor) ;

   R2 := GetRValue(ToColor) ;
   G2 := GetGValue(ToColor) ;
   B2 := GetBValue(ToColor) ;

   // Calculate Width
   csize := Rect.Right-Rect.Left;
   if csize <= 0 then Exit;

   // Get Color mdi
   dr := (R2-R1) / csize;
   dg := (G2-G1) / csize;
   db := (B2-B1) / csize;

   // Start Draw
   cnt := 0;
   for X := Rect.Left to Rect.Right-1 do
   begin
     R := R1+Ceil(dr*cnt) ;
     G := G1+Ceil(dg*cnt) ;
     B := B1+Ceil(db*cnt) ;

     Canvas.Pen.Color := RGB(R,G,B) ;
     Canvas.MoveTo(X,Rect.Top) ;
     Canvas.LineTo(X,Rect.Bottom) ;
     inc(cnt) ;
   end;
end;

procedure GradVertical(Canvas:TCanvas; Rect:TRect; FromColor, ToColor:TColor);
var
   Y: integer;
   dr, dg, db:Extended;
   r1, r2, g1, g2, b1, b2:Byte;
   R, G, B:Byte;
   cnt, csize:integer;
begin
  //Unpack Colors
  tocolor := ColorToRGB(tocolor);
  fromcolor := ColorToRGB(fromcolor);

   R1 := GetRValue(FromColor) ;
   G1 := GetGValue(FromColor) ;
   B1 := GetBValue(FromColor) ;

   R2 := GetRValue(ToColor) ;
   G2 := GetGValue(ToColor) ;
   B2 := GetBValue(ToColor) ;

   // Calculate Width
   csize := Rect.Bottom-Rect.Top;
   if csize <= 0 then Exit;

   // Get Color mdi
   dr := (R2-R1) / csize;
   dg := (G2-G1) / csize;
   db := (B2-B1) / csize;

   // Start Draw
   cnt := 0;
   for Y := Rect.Top to Rect.Bottom-1 do
   begin
     R := R1+Ceil(dr*cnt) ;
     G := G1+Ceil(dg*cnt) ;
     B := B1+Ceil(db*cnt) ;

     Canvas.Pen.Color := RGB(R,G,B) ;
     Canvas.MoveTo(Rect.Left,Y) ;
     Canvas.LineTo(Rect.Right,Y) ;
     inc(cnt) ;
   end;
end;

procedure FastBlur(Bitmap: TBitmap; Radius: Real; BlurScale: Integer; HighQuality: Boolean = True);
begin
  CFX.BlurFunctions.FastBlur( Bitmap, Radius, BlurScale, HighQuality );
end;


procedure DrawFontIcon(Canvas: TCanvas; Icon: string; Color: TColor; Rect: TRect);
var
  FontPrevious: TFont;
  TextDraw: string;
begin
  with Canvas do begin
    FontPrevious := TFont.Create;
    try
      FontPrevious.Assign(Font);

      // Draw
      Font.Color := Color;
      Font.Name := ThemeManager.IconFont;
      Font.Height := GetMaxFontHeight(Canvas, Copy(Icon, 1, 1), Rect.Width, Rect.Height);
      for var I := Low(Icon) to High(Icon) do begin
        TextDraw := Icon[I];
        TextRect( Rect, TextDraw, [tfSingleLine, tfCenter, tfVerticalCenter] );
      end;

      Font.Assign(FontPrevious);
    finally
      FontPrevious.Free;
    end;
  end;
end;

procedure DrawTextRect(Canvas: TCanvas; ARect: TRect; Text: string; Flags: FXTextFlags; AMargin: integer);
var
  TextFormat: TTextFormat;
  Lines: TArray<string>;
  Top, LineHeight, I: integer;
  R: TRect;
begin
  // Margin
  if AMargin <> 0 then
    ARect.Inflate(-AMargin, -AMargin);

  // Ignore
  if Text = '' then
    Exit;

  if FXTextFlag.Auto in Flags then
    begin
      if Canvas.TextWidth(Text) > ARect.Width then
        Flags := Flags + [FXTextFlag.WordWrap];
    end;

  if FXTextFlag.WordWrap in Flags then
    begin
      // Line Settings
      TextFormat := [];
      if FXTextFlag.Left in Flags then
        TextFormat := TextFormat + [tfLeft];
      if FXTextFlag.Center in Flags then
        TextFormat := TextFormat + [tfCenter];
      if FXTextFlag.Right in Flags then
        TextFormat := TextFormat + [tfRight];
      if FXTextFlag.NoClip in Flags then
        TextFormat := TextFormat + [tfNoClip];
      if FXTextFlag.TrimPath in Flags then
        TextFormat := TextFormat + [tfPathEllipsis];
      if FXTextFlag.TrimCutoff in Flags then
        TextFormat := TextFormat + [tfEndEllipsis];
      if FXTextFlag.TrimWord in Flags then
        TextFormat := TextFormat + [tfWordEllipsis];
      if not (FXTextFlag.ShowAccelChar in Flags) then
        TextFormat := TextFormat + [tfNoPrefix];

      Lines := GetWordWrapLines(Canvas, Text, ARect);

      // Vertical Align
      Top := 0;
      if FXTextFlag.VerticalCenter in Flags then
        begin
          for I := 0 to High(Lines) do
            Top := Top + WordWrapGetLineHeight(Canvas, Lines[I]);

          Top := round( ARect.Height / 2 - Top / 2 );
        end;
      if FXTextFlag.Bottom in Flags then
        begin
          for I := 0 to High(Lines) do
            Top := Top + WordWrapGetLineHeight(Canvas, Lines[I]);

          Top := ARect.Height - Top;
        end;

      Top := Top + ARect.Top;

      // Draw
      for I := 0 to High(Lines) do
        begin
          LineHeight := WordWrapGetLineHeight(Canvas, Lines[I]);

          R := Rect( ARect.Left, Top, ARect.Right, Top + LineHeight );

          Canvas.TextRect( R, Lines[I], TextFormat );

          Top := Top + LineHeight;
        end;
    end
  else
    begin
      TextFormat := [tfSingleLine];
      if FXTextFlag.Center in Flags then
        TextFormat := TextFormat + [tfCenter];
      if FXTextFlag.Right in Flags then
        TextFormat := TextFormat + [tfRight];
      if FXTextFlag.VerticalCenter in Flags then
        TextFormat := TextFormat + [tfVerticalCenter];
      if FXTextFlag.Bottom in Flags then
        TextFormat := TextFormat + [tfBottom];
      if FXTextFlag.NoClip in Flags then
        TextFormat := TextFormat + [tfNoClip];
      if FXTextFlag.NoClip in Flags then
        TextFormat := TextFormat + [tfNoClip];
      if FXTextFlag.TrimPath in Flags then
        TextFormat := TextFormat + [tfPathEllipsis];
      if FXTextFlag.TrimCutoff in Flags then
        TextFormat := TextFormat + [tfEndEllipsis];
      if FXTextFlag.TrimWord in Flags then
        TextFormat := TextFormat + [tfWordEllipsis];
      if not (FXTextFlag.ShowAccelChar in Flags) then
        TextFormat := TextFormat + [tfNoPrefix];

      Canvas.TextRect(ARect, Text, TextFormat);
    end;
end;

function GetTextRect(Canvas: TCanvas; ARect: TRect; Text: string;
    Flags: FXTextFlags; AMargin: integer = 0): TRect;
var
  TextFormat: TTextFormat;
  Lines: TArray<string>;
  Top, LineHeight, I: integer;
begin
  // Margin
  if AMargin <> 0 then
    ARect.Inflate(-AMargin, -AMargin);

  // Ignore
  if Text = '' then
    Exit;

  if FXTextFlag.Auto in Flags then
    begin
      if Canvas.TextWidth(Text) > ARect.Width then
        Flags := Flags + [FXTextFlag.WordWrap];
    end;

  if FXTextFlag.WordWrap in Flags then
    begin
      // Line Settings
      TextFormat := [];
      if FXTextFlag.Left in Flags then
        TextFormat := TextFormat + [tfLeft];
      if FXTextFlag.Center in Flags then
        TextFormat := TextFormat + [tfCenter];
      if FXTextFlag.Right in Flags then
        TextFormat := TextFormat + [];
      if FXTextFlag.NoClip in Flags then
        TextFormat := TextFormat + [tfNoClip];
      if FXTextFlag.TrimPath in Flags then
        TextFormat := TextFormat + [tfPathEllipsis];
      if FXTextFlag.TrimCutoff in Flags then
        TextFormat := TextFormat + [tfEndEllipsis];
      if FXTextFlag.TrimWord in Flags then
        TextFormat := TextFormat + [tfWordEllipsis];
      if not (FXTextFlag.ShowAccelChar in Flags) then
        TextFormat := TextFormat + [tfNoPrefix];

      // Lines
      Lines := GetWordWrapLines(Canvas, Text, ARect);

      // Vertical Align
      Top := 0;
      if FXTextFlag.VerticalCenter in Flags then
        begin
          for I := 0 to High(Lines) do
            Top := Top + Canvas.TextHeight(Lines[I]);

          Top := round( ARect.Height / 2 - Top / 2 );
        end;
      if FXTextFlag.Bottom in Flags then
        begin
          for I := 0 to High(Lines) do
            Top := Top + Canvas.TextHeight(Lines[I]);

          Top := ARect.Height - Top;
        end;

      Top := Top + ARect.Top;

      // Result
      Result := ARect;
      Result.Top := Top;

      // Draw
      for I := 0 to High(Lines) do
        begin
          LineHeight := WordWrapGetLineHeight(Canvas, Lines[I]);

          Top := Top + LineHeight;
        end;

      // Result
      Result.Bottom := Top;
    end
  else
    begin
      Result := ARect;
    end;
end;

function GetWordWrapLines(Canvas: TCanvas; Text: string; ARect: TRect): TArray<string>;
var
  Temp: string;
  Words: TArray<string>;
  Line, WordWidth, LineWidth: integer;
  I, Index: Integer;
procedure AddLine;
begin
  Inc(Line);
  SetLength(Result, Line + 1);

  LineWidth := 0;
end;
begin
  // Replace WIN CL format
  Text := Text.Replace(#$A#$D, #13);
  Text := Text.Replace(#$A, #13);

  // Get Words
  Words := GetAllSeparatorItems(Text, [' ']);
  for I := 0 to High(Words)-1 do
    Words[I] := Words[I] + ' ';

  // Split values with #13
  I := 0;
  while I < Length(Words) do
    begin
      Index := Words[I].IndexOf(#13);

      if Index <> -1 then
        begin
          if Index = 0 then
            Index := 1;

          Temp := Words[I].Remove(0, Index);
          TArrayUtils<string>.Insert(I+1, Temp, Words);

          Words[I] := Words[I].Remove(Index, Words[I].Length-Index);
        end;

      Inc(I);
    end;

  // Data
  Line := 0;
  LineWidth := 0;

  // Result
  SetLength(Result, 1);

  // Step
  for I := 0 to High(Words) do
    begin
      // Word
      Temp := Words[I];

      if Temp = #13 then
        begin
          AddLine;
          Continue;
        end;

      // Width
      WordWidth := Canvas.TextWidth(Temp);

      // New Line
      if LineWidth + WordWidth > ARect.Width then
        AddLine;

      // Add to line
      Result[Line] := ConCat(Result[Line], Temp);

      // Add
      LineWidth := LineWidth + WordWidth;
    end;
end;

function WordWrapGetLineHeight(Canvas: TCanvas; Text: string): integer;
begin
  Result := Canvas.TextHeight(Text);
  if Result = 0 then
    Result := Canvas.TextHeight('|');
end;


function GetMaxFontHeight(Canvas: TCanvas; Text: string; MaxWidth, MaxHeight: Integer): integer;
var
  Ext: TSize;
begin
  Result := 0;
  if Text = '' then
    Exit;

  Canvas.Font.Height := -10;
  repeat
    Canvas.Font.Height := Canvas.Font.Height - 1;
    Ext := Canvas.TextExtent(Text);
  until ((Ext.cx >= MaxWidth) or (Ext.cy >= MaxHeight));
  repeat
    Canvas.Font.Height := Canvas.Font.Height + 1;
    Ext := Canvas.TextExtent(Text);
  until ((Ext.cx <= MaxWidth) and (Ext.cy <= MaxHeight)) or (Canvas.Font.Height = 1);

  Result := Canvas.Font.Height;
end;

function CreatePreMultipliedRGBQuad(Color: TColor; Alpha: Byte = $FF): TRGBQuad;
begin
  Color := ColorToRGB(Color);
  Result.rgbBlue := MulDiv(GetBValue(Color), Alpha, $FF);
  Result.rgbGreen := MulDiv(GetGValue(Color), Alpha, $FF);
  Result.rgbRed := MulDiv(GetRValue(Color), Alpha, $FF);
  Result.rgbReserved := Alpha;
end;

function CreateSolidBrushWithAlpha(Color: TColor; Alpha: Byte = $FF): HBRUSH;
var
  Info: TBitmapInfo;
begin
  FillChar(Info, SizeOf(Info), 0);
  with Info.bmiHeader do
    begin
      biSize := SizeOf(Info.bmiHeader);
      biWidth := 1;
      biHeight := 1;
      biPlanes := 1;
      biBitCount := 32;
      biCompression := BI_RGB;
    end;
  Info.bmiColors[0] := CreatePreMultipliedRGBQuad(Color, Alpha);
  Result := CreateDIBPatternBrushPt(@Info, 0);
end;

procedure CopyRoundRect(FromCanvas: TCanvas; FromRect: TRoundRect; DestCanvas: TCanvas; DestRect: TRect; shrinkborder: integer);
var
  x, y, A, m: integer;
  Al: Real;
  HS, HD: TLine;
  S, D: TRect;
begin
  // Border Shrink
  if shrinkborder <> 0 then
  begin
    inc(FromRect.Rect.Left, shrinkborder);
    inc(FromRect.Rect.Top, shrinkborder);
    dec(FromRect.Rect.Right, shrinkborder);
    dec(FromRect.Rect.Bottom, shrinkborder);

    inc(DestRect.Left, shrinkborder);
    inc(DestRect.Top, shrinkborder);
    dec(DestRect.Right, shrinkborder);
    dec(DestRect.Bottom, shrinkborder);
  end;

  // Adjust Sizing
  if FromRect.GetRoundness > FromRect.Rect.Height then
    FromRect.SetRoundness( FromRect.Rect.Height );

  m := 0;
  if (FromRect.Rect.Height > m) then
    m := FromRect.Rect.Height;
  if (FromRect.Rect.Width > m) then
    m := FromRect.Rect.Width;
  if (DestRect.Width > m) then
    m := DestRect.Width;
  if (DestRect.Width > m) then
    m := DestRect.Width;

  if (m = 0) or (FromRect.Width = 0) or (FromRect.Height = 0) then
    Exit;

  if m > 90 then
    m := round(m/90) + 1
  else
    m := 1;


  // Start Copy
    for A := 90 * m to 180 * m do
      begin
        Al := A / m;
        X := round( FromRect.RoundX / 2 * cos(Al*pi/180) );
        Y := round( FromRect.RoundY / 2 * sin(Al*pi/180) );

        S.Left := FromRect.Rect.Left + FromRect.RoundX div 2 + X - 1;
        S.Top := FromRect.Rect.Top + FromRect.RoundY div 2 - Y - 1;

        if S.Bottom > FromRect.Bottom + 1 then
          S.Bottom := FromRect.Bottom + 1;

        S.Right := FromRect.Rect.Right - FromRect.RoundX div 2 - X + 1;
        S.Bottom := FromRect.Rect.Top + FromRect.RoundY div 2 - Y + 1;

        D.Left := DestRect.Left + round( (S.Left - FromRect.Rect.Left) / FromRect.Rect.Width
                                   * DestRect.Width );
        D.Right := DestRect.Left + round( (S.Right - FromRect.Rect.Left) / FromRect.Rect.Width
                                   * DestRect.Width );
        D.Top := DestRect.Top + round( (S.Top - FromRect.Rect.Top) / FromRect.Rect.Height
                                   * DestRect.Height );
        D.Bottom := DestRect.Top + round( (S.Bottom - FromRect.Rect.Top) / FromRect.Rect.Height
                                   * DestRect.Height );

        DestCanvas.CopyRect(D, FromCanvas, S);

        if A = 180 * m then
          begin
            HS.Point1 := S.TopLeft;
            HD.Point1 := D.TopLeft;
          end;
      end;
      for A := 180 * m to 270 * m do
      begin
        Al := A / m;
        X := round( FromRect.RoundX / 2 * cos(Al*pi/180) );
        Y := round( FromRect.RoundY / 2 * sin(Al*pi/180) );

        S.Left := FromRect.Rect.Left + FromRect.RoundX div 2 + X - 1;
        S.Top := FromRect.Rect.Bottom - FromRect.RoundY div 2 - Y - 1;

        S.Right := FromRect.Rect.Right - FromRect.RoundX div 2 - X + 1;
        S.Bottom := FromRect.Rect.Bottom - FromRect.RoundY div 2 - Y + 1;

        if S.Bottom > FromRect.Bottom + 1 then
          S.Bottom := FromRect.Bottom + 1;

        D.Left := DestRect.Left + round( (S.Left - FromRect.Rect.Left) / FromRect.Rect.Width
                                   * DestRect.Width );
        D.Right := DestRect.Left + round( (S.Right - FromRect.Rect.Left) / FromRect.Rect.Width
                                   * DestRect.Width );
        D.Top := DestRect.Top + round( (S.Top - FromRect.Rect.Top) / FromRect.Rect.Height
                                   * DestRect.Height );
        D.Bottom := DestRect.Top + round( (S.Bottom - FromRect.Rect.Top) / FromRect.Rect.Height
                                   * DestRect.Height );

        DestCanvas.CopyRect(D, FromCanvas, S);

        if A = 180 * m then
          begin
            HS.Point2 := S.BottomRight;
            HD.Point2 := D.BottomRight;
          end;
      end;

      // Copy Center Rext
      DestCanvas.CopyRect(TRect.Create(HD.Point1, HD.Point2),
                          FromCanvas, TRect.Create(HS.Point1, HS.Point2));
end;

procedure CopyRectWithOpacity(Dest: TCanvas; DestRect: TRect; Source: TCanvas; SourceRect: TRect; Opacity: Byte);
var
  BlendFunction: TBlendFunction;
begin
  // Set up the blending parameters
  BlendFunction.BlendOp := AC_SRC_OVER;
  BlendFunction.BlendFlags := 0;
  BlendFunction.SourceConstantAlpha := Opacity;
  BlendFunction.AlphaFormat := AC_SRC_OVER;

  // Perform the alpha blending
  AlphaBlend(
    Dest.Handle, DestRect.Left, DestRect.Top, DestRect.Width, DestRect.Height,
    Source.Handle, SourceRect.Left, SourceRect.Top, SourceRect.Width, SourceRect.Height,
    BlendFunction
  );
end;

procedure DrawCheckedboard(Canvas: TCanvas; ARect: TRect; Width, Height: Integer; Color1, Color2: TColor);
var
  Row, Col, SquareWidth, SquareHeight: Integer;
  Rect: TRect;
  IsColor1: Boolean;
begin
  SquareWidth := ARect.Width div Width;
  SquareHeight := ARect.Height div Height;
  IsColor1 := True;

  for Row := 0 to Height - 1 do
  begin
    for Col := 0 to Width - 1 do
    begin
      Rect.Left := ARect.Left + Col * SquareWidth;
      Rect.Top := ARect.Top + Row * SquareHeight;
      Rect.Right := Rect.Left + SquareWidth;
      Rect.Bottom := Rect.Top + SquareHeight;

      if IsColor1 then
        Canvas.Brush.Color := Color1
      else
        Canvas.Brush.Color := Color2;

      Canvas.FillRect(Rect);
      IsColor1 := not IsColor1;
    end;

    IsColor1 := not IsColor1;
  end;
end;

procedure StretchInvertedMask(Source: TCanvas; Destination: TCanvas; DestRect: TRect);
begin
  BitBlt(Destination.Handle, DestRect.Left, DestRect.Top, DestRect.Width, DestRect.Height,
    Source.Handle, 0, 0, SRCINVERT);
end;

procedure StretchInvertedMask(Source: TBitMap; Destination: TCanvas; DestRect: TRect);
begin
  StretchInvertedMask(Source.Canvas, Destination, DestRect);
end;

function Desaturate(Color: TColor): TColor;
const
  LuminanceMultR = 54;
  LuminanceMultG = 184;
  LuminanceMultB = 18;
var
  Luminance: byte;
begin
  Luminance :=
    (((Color and $00FF0000) shr 16 * LuminanceMultR) +
     ((Color and $0000FF00) shr 8 * LuminanceMultG) +
     ((Color and $000000FF) * LuminanceMultB)) shr 8;
  Result := (Color and $FF000000) or (Luminance shl 16) or (Luminance shl 8) or Luminance;
end;

procedure Desaturate(Bitmap: TBitmap);
begin
  ASSERT(Bitmap.PixelFormat = pf32bit);
  for var Row := 0 to Bitmap.Height-1 do
  begin
    var p := PDword(Bitmap.ScanLine[Row]);
    var Col := Bitmap.Width;
    while (Col > 0) do
    begin
      p^ := Desaturate(p^);
      inc(p);
      dec(Col);
    end;
  end;
end;

procedure DrawBorder(const Canvas: TCanvas; R: TRect; Color: TColor; Thickness: Byte; Roundness: integer);
var
  TL, BR: Byte;
begin
  if Thickness <> 0 then
    begin
      TL := Thickness div 2;
      if Thickness mod 2 = 0 then
        BR := TL - 1
      else
        BR := TL;
      Canvas.Pen.Style := psSolid;
      Canvas.Pen.Color := Color;
      Canvas.Pen.Width := Thickness;
      if RoundNess <= 0 then
        Canvas.Rectangle(TL, TL, R.Width - BR, R.Height - BR)
      else
        Canvas.RoundRect(TL, TL, R.Width - BR, R.Height - BR, RoundNess, RoundNess);
    end;
end;

end.
