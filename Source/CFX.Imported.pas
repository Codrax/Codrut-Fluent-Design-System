unit CFX.Imported;

interface

  uses
    Windows, Classes, Types, Vcl.Graphics, Vcl.Imaging.pngimage;


  procedure ConvertToPNG(Source: TGraphic; Dest: TPngImage);

implementation

procedure ConvertToPNG(Source: TGraphic; Dest: TPngImage);
type
  TRGBALine = array[Word] of TRGBQuad;
  PRGBALine = ^TRGBALine;
var
  MaskLines: array of Vcl.Imaging.pngimage.PByteArray;

  function ColorToTriple(const Color: TColor): TRGBTriple;
  begin
    Result.rgbtBlue := Color shr 16 and $FF;
    Result.rgbtGreen := Color shr 8 and $FF;
    Result.rgbtRed := Color and $FF;
  end;

  procedure GetAlphaMask(SourceColor: TBitmap);
  type
    TBitmapInfoV4 = packed record
      bmiHeader: TBitmapV4Header; //Otherwise I may not get per-pixel alpha values.
      bmiColors: array[0..2] of TRGBQuad; // reserve space for color lookup table
    end;
  var
    Bits: PRGBALine;
    { The BitmapInfo parameter to GetDIBits is delared as var parameter. So instead of casting around, we simply use
      the absolute directive to refer to the same memory area. }
    BitmapInfo: TBitmapInfoV4;
    BitmapInfoFake: TBitmapInfo absolute BitmapInfo;
    I, X, Y: Integer;
    HasAlpha: Boolean;
    BitsSize: Integer;
    bmpDC: HDC;
    bmpHandle: HBITMAP;
  begin
    BitsSize := 4 * SourceColor.Width * SourceColor.Height;
    Bits := AllocMem(BitsSize);
    try
      FillChar(BitmapInfo, SizeOf(BitmapInfo), 0);
      BitmapInfo.bmiHeader.bV4Size := SizeOf(BitmapInfo.bmiHeader);
      BitmapInfo.bmiHeader.bV4Width := SourceColor.Width;
      BitmapInfo.bmiHeader.bV4Height := -SourceColor.Height; //Otherwise the image is upside down.
      BitmapInfo.bmiHeader.bV4Planes := 1;
      BitmapInfo.bmiHeader.bV4BitCount := 32;
      BitmapInfo.bmiHeader.bV4V4Compression := BI_BITFIELDS;
      BitmapInfo.bmiHeader.bV4SizeImage := BitsSize;
      BitmapInfo.bmiColors[0].rgbRed := 255;
      BitmapInfo.bmiColors[1].rgbGreen := 255;
      BitmapInfo.bmiColors[2].rgbBlue := 255;

      { Getting the bitmap Handle will invalidate the Canvas.Handle, so it is important to retrieve them in the correct
        order. As parameter evaluation order is undefined and differs between Win32 and Win64, we get invalid values
        for Canvas.Handle when we use those properties directly in the call to GetDIBits. }
      bmpHandle := SourceColor.Handle;
      bmpDC := SourceColor.Canvas.Handle;
      if GetDIBits(bmpDC, bmpHandle, 0, SourceColor.Height, Bits, BitmapInfoFake, DIB_RGB_COLORS) > 0 then begin
        //Because Win32 API is a piece of crap when it comes to icons, I have to check
        //whether an has an alpha-channel the hard way.
        HasAlpha := False;
        for I := 0 to (SourceColor.Height * SourceColor.Width) - 1 do begin
          if Bits[I].rgbReserved <> 0 then begin
            HasAlpha := True;
            Break;
          end;
        end;
        if HasAlpha then begin
          //OK, so not all alpha-values are 0, which indicates the existence of an
          //alpha-channel.
          I := 0;
          for Y := 0 to SourceColor.Height - 1 do
            for X := 0 to SourceColor.Width - 1 do begin
              MaskLines[Y][X] := Bits[I].rgbReserved;
              Inc(I);
            end;
        end;
      end;
    finally
      FreeMem(Bits, BitsSize);
    end;
  end;

  function WinXPOrHigher: Boolean;
  var
    Info: TOSVersionInfo;
  begin
    Info.dwOSVersionInfoSize := SizeOf(Info);
    GetVersionEx(Info);
    Result := (Info.dwPlatformId = VER_PLATFORM_WIN32_NT) and
      ((Info.dwMajorVersion > 5) or
      ((Info.dwMajorVersion = 5) and (Info.dwMinorVersion >= 1)));
  end;

var
  Temp, SourceColor, SourceMask: TBitmap;
  X, Y: Integer;
  Line: PRGBLine;
  MaskLine, AlphaLine: Vcl.Imaging.pngimage.PByteArray;
  TransparentColor, CurrentColor: TColor;
  IconInfo: TIconInfo;
  AlphaNeeded: Boolean;
begin
  Assert(Dest <> nil, 'Dest is nil!');
  //A PNG does not have to be converted
  if Source is TPngImage then begin
    Dest.Assign(Source);
    Exit;
  end;

  AlphaNeeded := False;
  Temp := TBitmap.Create;
  SetLength(MaskLines, Source.Height);
  for Y := 0 to Source.Height - 1 do begin
    MaskLines[Y] := AllocMem(Source.Width);
    FillMemory(MaskLines[Y], Source.Width, 255);
  end;
  try
    //Initialize intermediate color bitmap
    Temp.Width := Source.Width;
    Temp.Height := Source.Height;
    Temp.PixelFormat := pf24bit;

    //Now figure out the transparency
    if Source is TBitmap then begin
      if Source.Transparent then begin
        //TBitmap is just about comparing the drawn colors against the TransparentColor
        if TBitmap(Source).TransparentMode = tmFixed then
          TransparentColor := TBitmap(Source).TransparentColor
        else
          TransparentColor := TBitmap(Source).Canvas.Pixels[0, Source.Height - 1];

        for Y := 0 to Temp.Height - 1 do begin
          Line := Temp.ScanLine[Y];
          MaskLine := MaskLines[Y];
          for X := 0 to Temp.Width - 1 do begin
            CurrentColor := GetPixel(TBitmap(Source).Canvas.Handle, X, Y);
            if CurrentColor = TransparentColor then begin
              MaskLine^[X] := 0;
              AlphaNeeded := True;
            end;
            Line[X] := ColorToTriple(CurrentColor);
          end;
        end;
      end
      else begin
        Temp.Canvas.Draw(0, 0, Source);
      end;
    end
    else if Source is TIcon then begin
      //TIcon is more complicated, because there are bitmasked (classic) icons and
      //alphablended (modern) icons. Not to forget about the "inverse" color.
      GetIconInfo(TIcon(Source).Handle, IconInfo);
      SourceColor := TBitmap.Create;
      SourceMask := TBitmap.Create;
      try
        SourceColor.Handle := IconInfo.hbmColor;
        SourceMask.Handle := IconInfo.hbmMask;
        Temp.Canvas.Draw(0, 0, SourceColor);
        for Y := 0 to Temp.Height - 1 do begin
          MaskLine := MaskLines[Y];
          for X := 0 to Temp.Width - 1 do begin
            if GetPixel(SourceMask.Canvas.Handle, X, Y) <> 0 then begin
              MaskLine^[X] := 0;
              AlphaNeeded := True;
            end;
          end;
        end;
        if (GetDeviceCaps(SourceColor.Canvas.Handle, BITSPIXEL) = 32) and WinXPOrHigher then begin
          //This doesn't neccesarily mean we actually have 32bpp in the icon, because the
          //bpp of an icon is always the same as the display settings, regardless of the
          //actual color depth of the icon :(
          AlphaNeeded := True;
          GetAlphaMask(SourceColor);
        end;
        //This still doesn't work for alphablended icons...
      finally
        SourceColor.Free;
        SourceMask.Free
      end;
    end;

    //And finally, assign the destination PNG image
    Dest.Assign(Temp);
    if AlphaNeeded then begin
      Dest.CreateAlpha;
      for Y := 0 to Dest.Height - 1 do begin
        AlphaLine := Dest.AlphaScanline[Y];
        CopyMemory(AlphaLine, MaskLines[Y], Temp.Width);
      end;
    end;

  finally
    for Y := 0 to Source.Height - 1 do
      FreeMem(MaskLines[Y], Source.Width);
    Temp.Free;
  end;
end;


end.
