{***********************************************************}
{                  Codruts Variabile Helpers                }
{                                                           }
{                        version 0.2                        }
{                           ALPHA                           }
{                                                           }
{                                                           }
{                                                           }
{                                                           }
{                                                           }
{                   -- WORK IN PROGRESS --                  }
{***********************************************************}

{$SCOPEDENUMS ON}

unit CFX.VarHelpers;

interface
  uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, IdHTTP,
  VCL.Graphics, Winapi.ActiveX, URLMon, IOUtils, System.Generics.Collections,
  System.Generics.Defaults, Vcl.Imaging.pngimage,
  WinApi.GdipObj, WinApi.GdipApi, Win.Registry, CFX.GDI, CFX.Types;

  type
    // Requirements
    THackGraphic = class(TGraphic);
    TRGBAArray = array[Word] of TRGBQuad;
    PRGBAArray = ^TRGBAArray;
    TRGBArray = array[Word] of TRGBTriple;
    PRGBArray = ^TRGBArray;

    // Color Helper
    TColorHelper = record helper for TColor
    public
      function ToString: string; overload; inline;
      function ToInteger: integer; overload; inline;
    end;

    // TFont
    TAdvFont = type string;

    TAdvFontHelper = record helper for TAdvFont
      function ToString: string;
      procedure FromString(AString: string);
    end;

    // Canvas
    TCanvasHelper = class helper for TCanvas
      procedure DrawHighQuality(ARect: TRect; Bitmap: TBitmap; Opacity: Byte = 255; HighQuality: Boolean = False); overload;
      procedure DrawHighQuality(ARect: TRect; Graphic: TGraphic; Opacity: Byte = 255; HighQuality: Boolean = False); overload;

      procedure StretchDraw(DestRect, SrcRect: TRect; Bitmap: TBitmap; Opacity: Byte); overload;
      procedure StretchDraw(Rect: TRect; Graphic: TGraphic; Opacity: Byte); overload;

      procedure GDITint(Rectangle: TRect; Color: TColor; Opacity: byte = 75);
      procedure GDIRectangle(Rectangle: TRect; Brush: TGDIBrush; Pen: TGDIPen);
      procedure GDIRoundRect(RoundRect: TRoundRect; Brush: TGDIBrush; Pen: TGDIPen);
      procedure GDICircle(Rectangle: TRect; Brush: TGDIBrush; Pen: TGDIPen);
      procedure GDIPolygon(Points: TArray<TPoint>; Brush: TGDIBrush; Pen: TGDIPen);
      procedure GDILine(Line: TLine; Pen: TGDIPen);
      procedure GDIGraphic(Graphic: TGraphic; Rect: TRect);
    end;

    // Registry
    TRegHelper = class helper for TRegistry
      procedure RenameKey(const OldName, NewName: string);
      procedure CloneKey(const KeyName: string);

      procedure MoveKeyTo(const OldName, NewKeyPath: string; Delete: Boolean);
    end;

  // Functions for TCanvasHelper
  procedure GraphicAssignToBitmap(Bitmap: TBitmap; Graphic: TGraphic);
  procedure PngImageAssignToBitmap(Bitmap: TBitmap; PngImage: TPngImage; IsPremultipledBitmap: Boolean = True);
  procedure DrawBitmapHighQuality(Handle: THandle; ARect: TRect; Bitmap: TBitmap; Opacity: Byte = 255;
  HighQality: Boolean = False; EgdeFill: Boolean = False);

implementation

// Color
function TColorHelper.ToString: string;
begin
  Result := colortostring( Self );
end;

function TColorHelper.ToInteger: integer;
begin
  Result := ColorToRgb( Self );
end;

// TFont
function TAdvFontHelper.ToString: string;
begin

end;

procedure TAdvFontHelper.FromString(AString: string);
begin
  //TFont(Self).
end;

{ TCanvasHelper }
procedure TCanvasHelper.DrawHighQuality(ARect: TRect; Bitmap: TBitmap; Opacity: Byte = 255; HighQuality: Boolean = False);
begin
  DrawBitmapHighQuality(Handle, ARect, Bitmap, Opacity, HighQuality);
end;

procedure TCanvasHelper.DrawHighQuality(ARect: TRect; Graphic: TGraphic; Opacity: Byte = 255; HighQuality: Boolean = False);
var
  Bitmap: TBitmap;
begin
  if Graphic is TBitmap then
    DrawHighQuality(ARect, TBitmap(Graphic), Opacity, HighQuality)
  else
  begin
    Bitmap := TBitmap.Create;
    try
      GraphicAssignToBitmap(Bitmap, Graphic);
      DrawHighQuality(ARect, Bitmap, Opacity, HighQuality);
    finally
      Bitmap.Free;
    end;
  end;
end;

procedure TCanvasHelper.StretchDraw(DestRect, SrcRect: TRect; Bitmap: TBitmap; Opacity: Byte);
var
  BF: TBlendFunction;
begin
  if Bitmap.Empty then
    Exit;
  BF.BlendOp := AC_SRC_OVER;
  BF.BlendFlags := 0;
  BF.SourceConstantAlpha := Opacity;
  if Bitmap.PixelFormat = pf32bit then
    BF.AlphaFormat := AC_SRC_ALPHA
  else
    BF.AlphaFormat := 0;
  AlphaBlend(Handle, DestRect.Left, DestRect.Top, DestRect.Right - DestRect.Left, DestRect.Bottom - DestRect.Top,
    Bitmap.Canvas.Handle, SrcRect.Left, SrcRect.Top, SrcRect.Right - SrcRect.Left, SrcRect.Bottom - SrcRect.Top, BF);
end;

procedure DrawBitmapHighQuality(Handle: THandle; ARect: TRect; Bitmap: TBitmap; Opacity: Byte = 255;
  HighQality: Boolean = False; EgdeFill: Boolean = False);
var
  Graphics: TGPGraphics;
  GdiPBitmap: TGPBitmap;
  Attr: TGPImageAttributes;
  M: TColorMatrix;
begin
  if Bitmap.Empty then
    Exit;
  GdiPBitmap := nil;
  Graphics := TGPGraphics.Create(Handle);
  try
    Graphics.SetSmoothingMode(SmoothingModeDefault);
    Graphics.SetPixelOffsetMode(PixelOffsetModeHalf);
    if not HighQality then
      Graphics.SetInterpolationMode(InterpolationModeHighQualityBilinear)
    else
      Graphics.SetInterpolationMode(InterpolationModeHighQuality);
    if Bitmap.PixelFormat = pf32bit then
    begin
      Assert(Bitmap.HandleType = bmDIB);
      GdiPBitmap := TGPBitmap.Create(Bitmap.Width, Bitmap.Height, -Bitmap.Width * 4,
        PixelFormat32bppPARGB, Bitmap.ScanLine[0]);
    end else
    if Bitmap.PixelFormat = pf24bit then
    begin
      Assert(Bitmap.HandleType = bmDIB);
      GdiPBitmap := TGPBitmap.Create(Bitmap.Width, Bitmap.Height, -BytesPerScanline(Bitmap.Width, 24, 32),
        PixelFormat24bppRGB, Bitmap.ScanLine[0]);
    end else
      GdiPBitmap := TGPBitmap.Create(Bitmap.Handle, Bitmap.Palette);
    if EgdeFill or (Opacity <> 255) then
    begin
      FillMemory(@M, SizeOf(TColorMatrix), 0);
      M[0, 0] := 1;
      M[1, 1] := 1;
      M[2, 2] := 1;
      M[3, 3] := Opacity / 255;
      M[4, 4] := 1;
      Attr := TGPImageAttributes.Create;
      try
        Attr.SetColorMatrix(M);
        if EgdeFill then Attr.SetWrapMode(WrapModeTileFlipXY);
        Graphics.DrawImage(GdiPBitmap, MakeRect(ARect.Left, ARect.Top, ARect.Width, ARect.Height),
          0, 0, Bitmap.Width, Bitmap.Height, UnitPixel, Attr);
      finally
        Attr.Free;
      end;
    end else
      Graphics.DrawImage(GdiPBitmap, MakeRect(ARect.Left, ARect.Top, ARect.Width, ARect.Height));
  finally
    Graphics.Free;
    GdiPBitmap.Free;
  end;
end;


procedure PngImageAssignToBitmap(Bitmap: TBitmap; PngImage: TPngImage; IsPremultipledBitmap: Boolean = True);
var
  X, Y: Integer;
  pBitmap: PRGBAArray;
  pPng: PRGBArray;
  pPngAlpha: PByteArray;
  pPngTable: PByteArray;
  C: TRGBQuad;
  A: Byte;
begin
  if (PngImage = nil) or (PngImage.Empty) then
  begin
    Bitmap.SetSize(0, 0);
    Exit;
  end;
  if (PngImage.TransparencyMode <> ptmPartial) or (PngImage.Header.BitDepth <> 8) then
  begin
    Bitmap.Assign(PngImage);
  end else
  begin
    Bitmap.SetSize(0, 0);
    if IsPremultipledBitmap then
      Bitmap.AlphaFormat := TAlphaFormat.afPremultiplied
    else
      Bitmap.AlphaFormat := TAlphaFormat.afDefined;
    Bitmap.PixelFormat := pf32bit;
    Bitmap.SetSize(PngImage.Width, PngImage.Height);
    for Y := 0 to Bitmap.Height - 1 do
    begin
      pBitmap := Bitmap.ScanLine[Y];
      pPng := PngImage.Scanline[Y];
      pPngTable := PngImage.Scanline[Y];
      pPngAlpha := PngImage.AlphaScanline[Y];
      if PngImage.Header.ColorType = COLOR_RGBALPHA then
      // RGBA
        if IsPremultipledBitmap then
          for X := 0 to Bitmap.Width - 1 do
          begin
            pBitmap[X].rgbBlue := (pPng[x].rgbtBlue * pPngAlpha[X]) div 255;
            pBitmap[X].rgbGreen := (pPng[x].rgbtGreen * pPngAlpha[X]) div 255;
            pBitmap[X].rgbRed := (pPng[x].rgbtRed * pPngAlpha[X]) div 255;
            pBitmap[X].rgbReserved := pPngAlpha[X];
          end
        else
          for X := 0 to Bitmap.Width - 1 do
          begin
            pBitmap[X].rgbBlue := pPng[x].rgbtBlue;
            pBitmap[X].rgbGreen := pPng[x].rgbtGreen;
            pBitmap[X].rgbRed := pPng[x].rgbtRed;
            pBitmap[X].rgbReserved := pPngAlpha[X];
          end
      else if PngImage.Header.ColorType = COLOR_PALETTE then
      // PALETTE
        if IsPremultipledBitmap then
          for X := 0 to Bitmap.Width - 1 do
          begin
            C := TChunkPLTE(PngImage.Chunks.ItemFromClass(TChunkPLTE)).Item[pPngTable[X]];
            A := TChunktRNS(PngImage.Chunks.ItemFromClass(TChunktRNS)).PaletteValues[pPngTable[X]];
            pBitmap[X].rgbBlue := (C.rgbBlue * A) div 255;
            pBitmap[X].rgbGreen := (C.rgbGreen * A) div 255;
            pBitmap[X].rgbRed := (C.rgbRed * A) div 255;
            pBitmap[X].rgbReserved := A;
          end
        else
          for X := 0 to Bitmap.Width - 1 do
          begin
            C := TChunkPLTE(PngImage.Chunks.ItemFromClass(TChunkPLTE)).Item[pPngTable[X]];
            A := TChunktRNS(PngImage.Chunks.ItemFromClass(TChunktRNS)).PaletteValues[pPngTable[X]];
            pBitmap[X].rgbBlue := C.rgbBlue;
            pBitmap[X].rgbGreen := C.rgbGreen;
            pBitmap[X].rgbRed := C.rgbRed;
            pBitmap[X].rgbReserved := A;
          end
      else
      // GRAYSCALE
        if IsPremultipledBitmap then
          for X := 0 to Bitmap.Width - 1 do
          begin
            pBitmap[X].rgbBlue := (pPngTable[X] * pPngAlpha[X]) div 255;
            pBitmap[X].rgbGreen := pBitmap[X].rgbBlue;
            pBitmap[X].rgbRed := pBitmap[X].rgbBlue;
            pBitmap[X].rgbReserved := pPngAlpha[X];
          end
        else
          for X := 0 to Bitmap.Width - 1 do
          begin
            pBitmap[X].rgbBlue := pPngTable[X];;
            pBitmap[X].rgbGreen := pBitmap[X].rgbBlue;
            pBitmap[X].rgbRed := pBitmap[X].rgbBlue;
            pBitmap[X].rgbReserved := pPngAlpha[X];
          end
    end;
  end;
end;


procedure GraphicAssignToBitmap(Bitmap: TBitmap; Graphic: TGraphic); Inline;
begin
  // standart TPngImage.AssignTo works is bad!
  if Graphic is TPngImage then
    PngImageAssignToBitmap(Bitmap, TPngImage(Graphic))
  else
    Bitmap.Assign(Graphic);
end;

procedure TCanvasHelper.StretchDraw(Rect: TRect; Graphic: TGraphic; Opacity: Byte);
var
  Bitmap: TBitmap;
begin
  if Graphic <> nil then
  begin
    Changing;
    RequiredState([csHandleValid]);
    if Opacity = 255 then
      THackGraphic(Graphic).Draw(Self, Rect)
    else
      // for Opacity <> 255
      if Graphic is TBitmap then
      begin
        // god scenary
        THackGraphic(Graphic).DrawTransparent(Self, Rect, Opacity);
      end
      else
      begin
        // bed, we create temp buffer, it is slowly :(
        Bitmap := TBitmap.Create;
        try
          GraphicAssignToBitmap(Bitmap, Graphic);
          StretchDraw(Rect, Bitmap, Opacity);
        finally
          Bitmap.Free;
        end;
      end;
    Changed;
  end;
end;

procedure TCanvasHelper.GDITint(Rectangle: TRect; Color: TColor; Opacity: byte = 75);
begin
  TintPicture(Self, Rectangle, Color, Opacity);
end;

procedure TCanvasHelper.GDIRectangle(Rectangle: TRect; Brush: TGDIBrush; Pen: TGDIPen);
begin
  DrawRectangle(Self, Rectangle, Brush, Pen);
end;

procedure TCanvasHelper.GDIRoundRect(RoundRect: TRoundRect; Brush: TGDIBrush; Pen: TGDIPen);
begin
  DrawRoundRect(Self, RoundRect, Brush, Pen);
end;

procedure TCanvasHelper.GDICircle(Rectangle: TRect; Brush: TGDIBrush; Pen: TGDIPen);
begin
  DrawCircle(Self, Rectangle, Brush, Pen);
end;

procedure TCanvasHelper.GDIPolygon(Points: TArray<TPoint>; Brush: TGDIBrush; Pen: TGDIPen);
begin
  DrawPolygon(Self, Points, Brush, Pen);
end;

procedure TCanvasHelper.GDILine(Line: TLine; Pen: TGDIPen);
begin
  DrawLine(Self, Line, Pen);
end;

procedure TCanvasHelper.GDIGraphic(Graphic: TGraphic; Rect: TRect);
begin
  DrawGraphic(Self, Graphic, Rect);
end;

{ TRegHelper }
procedure TRegHelper.RenameKey(const OldName, NewName: string);
begin
  Self.MoveKey(OldName, NewName, true);
end;

procedure TRegHelper.CloneKey(const KeyName: string);
var
  CloneNumber: integer;
begin
  CloneNumber := 1;
  repeat
    inc(CloneNumber);
  until not Self.KeyExists(KeyName + '(' + inttostr( CloneNumber ) + ')');

  MoveKey(KeyName, KeyName + '(' + inttostr( CloneNumber ) + ')', false);
end;

procedure TRegHelper.MoveKeyTo(const OldName, NewKeyPath: string; Delete: Boolean);
var
  RegistryOld,
  RegistryNew: TRegistry;

  I: integer;

  ValueNames: TStringList;
  KeyNames: TStringList;

  procedure MoveValue(SrcKey, DestKey: HKEY; const Name: string);
  var
    Len: Integer;
    OldKey, PrevKey: HKEY;
    Buffer: PChar;
    RegData: TRegDataType;
  begin
    OldKey := CurrentKey;
    SetCurrentKey(SrcKey);
    try
      Len := GetDataSize(Name);
      if Len >= 0 then
      begin
        Buffer := AllocMem(Len);
        try
          Len := GetData(Name, Buffer, Len, RegData);
          PrevKey := CurrentKey;
          SetCurrentKey(DestKey);
          try
            PutData(Name, Buffer, Len, RegData);
          finally
            SetCurrentKey(PrevKey);
          end;
        finally
          FreeMem(Buffer);
        end;
      end;
    finally
      SetCurrentKey(OldKey);
    end;
  end;
begin
  /// Attention!
  /// The NewKeyPath requires a registry path in the same HIVE. This NEEDS to be a
  /// path in the HIVE, only giving the new Key Name will create a new Key in the
  /// root of the HIVE!

  ValueNames := TStringList.Create;
  KeyNames := TStringList.Create;

  RegistryNew := TRegistry.Create( Self.Access );
  RegistryOld := TRegistry.Create( Self.Access );
  try
    // Open Keys
    RegistryOld.OpenKey( IncludeTrailingPathDelimiter( Self.CurrentPath + OldName ), false );
    RegistryNew.OpenKey( IncludeTrailingPathDelimiter( NewKeyPath ), True );

    // Index Keys/Values
    RegistryOld.GetValueNames(ValueNames);
    RegistryOld.GetKeyNames(KeyNames);

    // Copy Values
    for I := 1 to ValueNames.Count do
      MoveValue( RegistryOld.CurrentKey, RegistryNew.CurrentKey,
                 ValueNames[I - 1] );

    // Copy subkeys
    for I := 1 to KeyNames.Count do
      RegistryOld.MoveKeyTo(KeyNames[I - 1],
                            RegistryNew.CurrentPath + KeyNames[I - 1] + '\',
                            false);
  finally
    // Free Mem
    RegistryOld.Free;
    RegistryNew.Free;

    ValueNames.Free;
    KeyNames.Free;

    if Delete then
      Self.DeleteKey(OldName);
  end;
end;

end.
