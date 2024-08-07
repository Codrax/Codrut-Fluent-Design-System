unit CFX.BlurFunctions;

interface
uses
  UITypes, Types, CFX.Constants, VCl.GraphUtil, Winapi.Windows,
  Classes, Vcl.Themes, Vcl.Controls, Vcl.Graphics,
  SysUtils, CFX.VarHelpers;

type
  // Blur Function Dependencies
  TKernelSize = 1..50;
  TKernel = record
    Size: TKernelSize;
    Weights: array[-50..50] of Single;
  end;
  TRGBTriple = packed record
    b: Byte; {easier to type than rgbtBlue}
    g: Byte;
    r: Byte;
  end;
  PRow = ^TRow;
  TRow = array[Word] of TRGBTriple;
  PPRows = ^TPRows;
  TPRows = array[Word] of PRow;


// Declarations
procedure MakeGaussianKernel(var K: TKernel; radius: Real; MaxData, DataGranularity: Real);
procedure BlurRow(var theRow: array of TRGBTriple; K: TKernel; P: PRow);

procedure GaussianBlur(Bitmap: TBitmap; Radius: Real);
procedure FastBlur(Bitmap: TBitmap; Radius: Real; BlurScale: Integer; HighQuality: Boolean = True);

implementation

function TrimInt(Lower, Upper, theInteger: Integer): integer;
begin
  if (theInteger <= Upper) and (theInteger >= Lower) then
    result := theInteger
  else if theInteger > Upper then
    result := Upper
  else
    result := Lower;
end;

function TrimReal(Lower, Upper: Integer; x: Real): integer;
begin
  if (x < upper) and (x >= lower) then
    result := trunc(x)
  else if x > Upper then
    result := Upper
  else
    result := Lower;
end;

procedure BlurRow(var theRow: array of TRGBTriple; K: TKernel; P: PRow);
var
  j, n: Integer;
  tr, tg, tb: Real; {tempRed, etc}
  w: Real;
begin
  for j := 0 to High(theRow) do
  begin
    tb := 0;
    tg := 0;
    tr := 0;
    for n := -K.Size to K.Size do
    begin
      w := K.Weights[n];
      {the TrimInt keeps us from running off the edge of the row...}
      with theRow[TrimInt(0, High(theRow), j - n)] do
      begin
        tb := tb + w * b;
        tg := tg + w * g;
        tr := tr + w * r;
      end;
    end;
    with P[j] do
    begin
      b := TrimReal(0, 255, tb);
      g := TrimReal(0, 255, tg);
      r := TrimReal(0, 255, tr);
    end;
  end;
  Move(P[0], theRow[0], (High(theRow) + 1) * Sizeof(TRGBTriple));
end;


procedure MakeGaussianKernel(var K: TKernel; radius: Real; MaxData, DataGranularity: Real);
{makes K into a gaussian kernel with standard deviation = radius. For the current application
you set MaxData = 255 and DataGranularity = 1. Now the procedure sets the value of K.Size so
that when we use K we will ignore the Weights that are so small they can't possibly matter. (Small
Size is good because the execution time is going to be propertional to K.Size.)}
var
  j: Integer;
  temp, delta: Real;
  KernelSize: TKernelSize;
begin
  for j := Low(K.Weights) to High(K.Weights) do
  begin
    temp := j / radius;
    K.Weights[j] := exp(-temp * temp / 2);
  end;
  {now divide by constant so sum(Weights) = 1:}
  temp := 0;
  for j := Low(K.Weights) to High(K.Weights) do
    temp := temp + K.Weights[j];
  for j := Low(K.Weights) to High(K.Weights) do
    K.Weights[j] := K.Weights[j] / temp;
  {now discard (or rather mark as ignorable by setting Size) the entries that are too small to matter.
  This is important, otherwise a blur with a small radius will take as long as with a large radius...}
  KernelSize := 50;
  delta := DataGranularity / (2 * MaxData);
  temp := 0;
  while (temp < delta) and (KernelSize > 1) do
  begin
    temp := temp + 2 * K.Weights[KernelSize];
    dec(KernelSize);
  end;
  K.Size := KernelSize;
  {now just to be correct go back and jiggle again so the sum of the entries we'll be using is exactly 1}
  temp := 0;
  for j := -K.Size to K.Size do
    temp := temp + K.Weights[j];
  for j := -K.Size to K.Size do
    K.Weights[j] := K.Weights[j] / temp;
  // finally correct
  K.Weights[0] := K.Weights[0] + (0.000001);// HACK
end;

procedure GaussianBlur(Bitmap: TBitmap; Radius: Real);
var
  Row, Col: Integer;
  theRows: PPRows;
  K: TKernel;
  ACol: PRow;
  P: PRow;
begin
  if (Bitmap.HandleType <> bmDIB) or (Bitmap.PixelFormat <> pf24Bit) then
    raise Exception.Create('GaussianBlur only works for 24-bit bitmaps');
  MakeGaussianKernel(K, radius, 255, 1);
  GetMem(theRows, Bitmap.Height * SizeOf(PRow));
  GetMem(ACol, Bitmap.Height * SizeOf(TRGBTriple));
  {record the location of the bitmap data:}
  for Row := 0 to Bitmap.Height - 1 do
    theRows[Row] := Bitmap.Scanline[Row];
  {blur each row:}
  P := AllocMem(Bitmap.Width * SizeOf(TRGBTriple));
  for Row := 0 to Bitmap.Height - 1 do
    BlurRow(Slice(theRows[Row]^, Bitmap.Width), K, P);
  {now blur each column}
  ReAllocMem(P, Bitmap.Height * SizeOf(TRGBTriple));
  for Col := 0 to Bitmap.Width - 1 do
  begin
    {first read the column into a TRow:}
    for Row := 0 to Bitmap.Height - 1 do
      ACol[Row] := theRows[Row][Col];
    BlurRow(Slice(ACol^, Bitmap.Height), K, P);
    {now put that row, um, column back into the data:}
    for Row := 0 to Bitmap.Height - 1 do
      theRows[Row][Col] := ACol[Row];
  end;
  FreeMem(theRows);
  FreeMem(ACol);
  ReAllocMem(P, 0);
end;

procedure FastBlur(Bitmap: TBitmap; Radius: Real; BlurScale: Integer; HighQuality: Boolean = True);
  function Max(A, B: Integer): Integer;
  begin
    if A > B then
      Result := A
    else
      Result := B;
  end;
var
  Mipmap: TBitmap;
begin
  BlurScale := Max(BlurScale, 1);
  Mipmap := TBitmap.Create;
  try
    Mipmap.PixelFormat := pf24bit;
    Mipmap.SetSize(Max(Bitmap.Width div BlurScale, 4), Max(Bitmap.Height div BlurScale, 4));
    // create mipmap
    if HighQuality then
      DrawBitmapHighQuality(Mipmap.Canvas.Handle, Rect(0, 0, Mipmap.Width, Mipmap.Height), Bitmap, 255, False, True)
    else
      Mipmap.Canvas.StretchDraw(Rect(0, 0, Mipmap.Width, Mipmap.Height), Bitmap);
    // gaussian blur
    GaussianBlur(Mipmap, Radius);
    // stretch to source bitmap
    DrawBitmapHighQuality(Bitmap.Canvas.Handle, Rect(0, 0, Bitmap.Width, Bitmap.Height), Mipmap, 255, False, True);
  finally
    Mipmap.Free;
  end;
end;


end.
