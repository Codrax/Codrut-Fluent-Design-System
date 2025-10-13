unit CFX.Utilities;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, Win.Registry, System.UITypes,
  Types, Vcl.Forms, Vcl.Graphics, CFX.Colors, CFX.Registry, Winapi.ShellAPI,
  CFX.Types, IOUTils, RegularExpressions, CFX.Files, CFX.Constants;

function GetAppsUseDarkTheme: Boolean;
function GetAccentColor( brightencolor: boolean = true ): TColor;
/// <summary> Returns the scroll amount in Pixels. </summary>
function GetScrollAmount(Delta: integer; ViewHeight: integer): integer;
function GetLinesPerScroll: integer;
function GetLineScrollHeight: integer;

function GetUserNameString: string;

// Winapi
function NTKernelVersion: single;
function IsWindows10OrGreater(buildNumber: DWORD): Boolean;

// Shell
procedure ShellRun(Command: string; Parameters: string = '');

// String
function IsStringAlphaNumeric(const S: string): Boolean;

// File
function GetFileBytesString(FileName: string; FirstCount: integer): TArray<string>;
function ReadFileSignature(FileName: string): TFileType;

// General Folder
function GetAppDataFolder: string;

// Screen
procedure QuickScreenShot(var BitMap: TBitMap; Monitor: integer = -2);
procedure AppScreenShot(var BitMap: TBitMap; ApplicationCapton: string);

{ Application }
///  <summary> Get parameter by index </summary>
function GetParameter(Index: integer): string; overload; // get parameter by index
///  <summary> Get all parameters as string </summary>
function GetParameters: string;
///  <summary>
///    Check for a Parameter, takes parameter as "value" without shell prefix, return index position
///  </summary>
function FindParameter(Value: string): integer; overload;
///  <summary>
///    Check for a Parameter, takes parameter as "value" without shell prefix
///  </summary>
function HasParameter(Value: string): boolean; overload;
///  <summary> Get value of the following param of the requested value </summary>
function GetParameterValue(Value: string): string; overload;
///  <summary>
///    Check for a single char parameter, return index position
///  </summary>
function FindParameter(Value: char): integer; overload;
///  <summary> Check for a single char Unix parameter </summary>
function HasParameter(Value: char): boolean; overload;
///  <summary> Get single char Unix parameter value </summary>
function GetParameterValue(Value: char): string; overload;
///  <summary>
///    Check for a Unix Parameter alternative, either string or singlechar, return index position
///  </summary>
function FindParameter(Value: string; AltChar: char): integer; overload;
///  <summary> Check for a Unix Parameter alternative, either string or singlechar </summary>
function HasParameter(Value: string; AltChar: char): boolean; overload;
///  <summary> Gets the unix parameter, than returns the value </summary>
function GetParameterValue(Value: string; AltChar: char): string; overload;

const
  PARAM_PREFIX = {$IFDEF MSWINDOWS}'-'{$ELSE}'--'{$ENDIF};
  PARAM_PREFIX_CHAR = '-';

implementation

function NTKernelVersion: single;
begin
  Result := Win32MajorVersion + Win32MinorVersion / 10;
end;

function GetAppsUseDarkTheme: Boolean;
var
  R: TRegistry;
begin
  Result := False;
  R := TRegistry.Create;
  try
    R.RootKey := HKEY_CURRENT_USER;
    if R.OpenKeyReadOnly('Software\Microsoft\Windows\CurrentVersion\Themes\Personalize\') and R.ValueExists('AppsUseLightTheme') then begin
      Result := R.ReadInteger('AppsUseLightTheme') <> 1;
    end;
  finally
    R.Free;
  end;
end;

function IsWindows10OrGreater(buildNumber: DWORD): Boolean;
begin
  Result := (TOSVersion.Major > 10) or
    ((TOSVersion.Major = 10) and (TOSVersion.Minor = 0) and (DWORD(TOSVersion.Build) >= buildNumber));
end;

function GetAccentColor( brightencolor: boolean = true ): TColor;
var
  R: TRegistry;
  ARGB: cardinal;
begin
  Result := $D77800;  //  Default value on error
  R := TRegistry.Create;
  try
    R.RootKey := HKEY_CURRENT_USER;
    if R.OpenKeyReadOnly('Software\Microsoft\Windows\DWM\') and R.ValueExists('AccentColor') then begin
      ARGB := R.ReadCardinal('AccentColor');
      Result := ARGB mod $FF000000; //  ARGB to RGB
    end;
  finally
    R.Free;
  end;

  if brightencolor then
    Result := ChangeColorLight(Result, 50);
end;

function GetLinesPerScroll: integer;
var
  R: TRegistry;
begin
  Result := DEFAULT_SCROLL_LINES; // default value
  R := TRegistry.Create;
  try
    R.RootKey := HKEY_CURRENT_USER;
    if R.OpenKeyReadOnly('Control Panel\Desktop') and R.ValueExists('WheelScrollLines') then begin
      try
        Result := R.ReadString('WheelScrollLines').ToInteger;
      except
      end;
    end;
  finally
    R.Free;
  end;
end;

function GetScrollAmount(Delta: integer; ViewHeight: integer): integer;
begin
  if Delta = 0 then
    Exit(0);

  // Sign
  Result := -(Delta div abs(Delta));

  // Registry
  const LinePerScroll = GetLinesPerScroll;

  // Full page
  if LinePerScroll = -1 then
    Result := Result * ViewHeight
  else
    Result := Result * LinePerScroll * GetLineScrollHeight;
end;

function GetLineScrollHeight: integer;
begin
  Result := DEFAULT_LINE_SIZE;
end;

function GetUserNameString: string;
var
  nSize: DWord;
begin
 nSize := 1024;
 SetLength(Result, nSize);
 if GetUserName(PChar(Result), nSize) then
   SetLength(Result, nSize-1)
 else
   RaiseLastOSError;
end;

procedure ShellRun(Command, Parameters: string);
begin
  ShellExecute(0, 'open', PChar(Command), PChar(Parameters), nil, SW_NORMAL);
end;

function IsStringAlphaNumeric(const S: string): Boolean;
begin
  Result := TRegEx.IsMatch(S, '^[a-zA-Z0-9]+$');
end;

function GetFileBytesString(FileName: string; FirstCount: integer): TArray<string>;
var
  Bytes: TBytes;
  I, Total: Integer;
begin
  // Get File
  Bytes := TFile.ReadAllBytes(FileName);
  Total := Length( Bytes );

  // Total Items
  if (FirstCount = -1) or (FirstCount > Total) then
    FirstCount := Total;

  // Size
  SetLength( Result, FirstCount );

  // Convert
  for I := 0 to FirstCount - 1 do
    Result[I] := DecToHex( Bytes[I] );
end;

function ReadFileSignature(FileName: string): TFileType;
const
  MAX_READ_BUFF = 16;

  FTYP = '66 74 79 70';

  BMP_SIGN: TArray<string> = ['42 4D'];
  PNG_SIGN: TArray<string> = ['89 50 4E 47 0D 0A 1A 0A'];
  GIF_SIGN: TArray<string> = ['47 49 46'];
  JPEG_SIGN: TArray<string> = ['FF D8 FF', '49 46 00 01'];
  HEIF_SIGN: TArray<string> = [FTYP + '68 65 69 63'];
  TIFF_SIGN: TArray<string> = ['49 49 2A 00', '4D 4D 00 2A'];

  MP3_SIGN: TArray<string> = ['49 44 33', 'FF FB', 'FF F3', 'FF F2'];
  MP4_SIGN: TArray<string> = [FTYP + '69 73 6F 6D'];
  FLAC_SIGN: TArray<string> = ['66 4C 61 43'];
  MDI_SIGN: TArray<string> = ['4D 54 68 64'];
  OGG_SIGN: TArray<string> = ['4F 67 67 53'];
  SND_SIGN: TArray<string> = ['2E 73 6E 64'];
  M3U8_SIGN: TArray<string> = ['23 45 58 54 4D 33 55'];

  EXE_SIGN: TArray<string> = ['4D 5A'];
  MSI_SIGN: TArray<string> = ['D0 CF 11 E0 A1 B1 1A E1'];

  ZIP_SIGN: TArray<string> = ['50 4B 03 04', '50 4B 05 06', '50 4B 07 08'];
  GZIP_SIGN: TArray<string> = ['1F 8B'];
  ZIP7_SIGN: TArray<string> = ['37 7A BC AF 27 1C'];
  CABINET_SIGN: TArray<string> = ['4D 53 43 46'];
  TAR_SIGN: TArray<string> = ['75 73 74 61 72 00 30 30', '75 73 74 61 72 20 20 00'];
  RAR_SIGN: TArray<string> = ['52 61 72 21 1A 07 00', '52 61 72 21 1A 07 01 00'];
  LZIP_SIGN: TArray<string> = ['4C 5A 49 50'];

  ISO_SIGN: TArray<string> = ['43 44 30 30 31', '49 73 5A 21'];

  PDF_SIGN: TArray<string> = ['25 50 44 46 2D'];

  HLP_SIGN: TArray<string> = ['3F 5F'];

  CHM_SIGN: TArray<string> = ['49 54 53 46 03 00 00 00'];
var
  HexArray: TArray<string>;
  HEX: string;
  I: Integer;

function HasSignature(HEX: string; ValidSign: TArray<string>): boolean;
var
  I: integer;
begin
  Result := false;
  for I := 0 to High(ValidSign) do
    begin
      ValidSign[I] := ValidSign[I].Replace(' ', '');

      if Copy( HEX, 1, Length(ValidSign[I]) ) = ValidSign[I] then
        Exit(True);
    end;
end;
begin
  Result := TFileType.Text;

  // Get File
  HexArray := GetFileBytesString( FileName, MAX_READ_BUFF );

  HEX := '';
  for I := 0 to High(HexArray) do
    HEX := HEX + HexArray[I];

  SetLength(HexArray, 0);

  // Invalid
  if HEX = '' then
    Exit;

  // All Types Listed (not great)

  (* Picture Types *)
  if HasSignature(HEX, BMP_SIGN) then
    Exit( TFileType.BMP );

  if HasSignature(HEX, PNG_SIGN) then
    Exit( TFileType.PNG );

  if HasSignature(HEX, JPEG_SIGN) then
    Exit( TFileType.JPEG );

  if HasSignature(HEX, GIF_SIGN) then
    Exit( TFileType.GIF );

  if HasSignature(HEX, TIFF_SIGN) then
    Exit( TFileType.TIFF );

  (* Video/Audio Media *)
  if HasSignature(HEX, MP3_SIGN) then
    Exit( TFileType.MP3 );

  if HasSignature(HEX, FLAC_SIGN) then
    Exit( TFileType.Flac );

  if HasSignature(HEX, MDI_SIGN) then
    Exit( TFileType.MDI );

  if HasSignature(HEX, OGG_SIGN) then
    Exit( TFileType.OGG );

  if HasSignature(HEX, SND_SIGN) then
    Exit( TFileType.SND );

  if HasSignature(HEX, M3U8_SIGN) then
    Exit( TFileType.M3U8 );

  (* Executable *)
  if HasSignature(HEX, EXE_SIGN) then
    Exit( TFileType.EXE );

  if HasSignature(HEX, MSI_SIGN) then
    Exit( TFileType.MSI );

  (* Zip *)
  if HasSignature(HEX, ZIP_SIGN) then
    Exit( TFileType.Zip );

  if HasSignature(HEX, GZIP_SIGN) then
    Exit( TFileType.GZip );

  if HasSignature(HEX, ZIP7_SIGN) then
    Exit( TFileType.Zip7 );

  if HasSignature(HEX, CABINET_SIGN) then
    Exit( TFileType.Cabinet );

  if HasSignature(HEX, TAR_SIGN) then
    Exit( TFileType.TAR );

  if HasSignature(HEX, RAR_SIGN) then
    Exit( TFileType.RAR );

  if HasSignature(HEX, LZIP_SIGN) then
    Exit( TFileType.LZIP );

  (* ISO *)
  if HasSignature(HEX, ISO_SIGN) then
    Exit( TFileType.ISO );

  (* PDF *)
  if HasSignature(HEX, PDF_SIGN) then
    Exit( TFileType.PDF );

  (* Help File *)
  if HasSignature(HEX, HLP_SIGN) then
    Exit( TFileType.HLP );

  if HasSignature(HEX, CHM_SIGN) then
    Exit( TFileType.CHM );
end;

function GetAppDataFolder: string;
begin
  Result := IncludeTrailingPathDelimiter(ReplaceWinPath('%APPDATA%'));
end;

procedure QuickScreenShot(var BitMap: TBitMap; Monitor: integer);
var
  C: TCanvas;
  R: TRect;
begin
  /// PARAMETER VALUES               ///
  ///                                ///
  /// -2 All Monitors (Default)      ///
  ///                                ///
  /// -1 Default Monitor             ///
  ///                                ///
  ///  >= 0 Monitor Index            ///
  ///                                ///

  case Monitor of
    -2: R := Rect(Screen.DesktopRect.Left, Screen.DesktopRect.Top, Screen.DesktopRect.Right, Screen.DesktopRect.Bottom);

    -1: R := Rect(Screen.PrimaryMonitor.BoundsRect.Left, Screen.PrimaryMonitor.BoundsRect.Top,
            Screen.PrimaryMonitor.BoundsRect.Right, Screen.PrimaryMonitor.BoundsRect.Bottom);

    else R := Rect(Screen.Monitors[Monitor].BoundsRect.Left, Screen.Monitors[Monitor].BoundsRect.Top,
            Screen.Monitors[Monitor].BoundsRect.Right, Screen.Monitors[Monitor].BoundsRect.Bottom);
  end;



  BitMap.Width := R.Width;
  BitMap.Height := R.Height;

  C := TCanvas.Create;
  try
    C.Handle := GetDC(0);

    BitMap.Canvas.CopyRect( BitMap.Canvas.ClipRect, C, R );
  finally
    C.Free;
  end;
end;

procedure AppScreenShot(var BitMap: TBitMap; ApplicationCapton: string);
var
  Handle: HWND;
  R: TRect;
  DC: HDC;
  Old: HGDIOBJ;

begin
  Handle := FindWindow(nil, PWideChar(ApplicationCapton));
  GetWindowRect(Handle, R);

  Bitmap := TBitmap.Create;
  Bitmap.Width := R.Right - R.Left;
  Bitmap.Height := R.Bottom - R.Top;

  DC := GetDC(Handle);
  Old := SelectObject(DC, Bitmap.Canvas.Handle);
  BitBlt(Bitmap.Canvas.Handle, 0, 0, Bitmap.Width, Bitmap.Height, DC, 0, 0, SRCCOPY);
  SelectObject(DC, Old);
  ReleaseDC(Handle, DC);
end;

function GetParameter(Index: integer): string;
begin
  Result := ParamStr(Index);

  {$IFDEF MSWINDOWS}
  // Fix WinNT
  if (Length(Result)>0) and (Result[1] = '/') then
    Result[1] := '-';
  {$ENDIF}
end;

function GetParameters: string;
var
  I: Integer;
  Parameter: string;
  ACount: integer;
begin
  ACount := ParamCount;
  for I := 1 to ParamCount do
    begin
      Parameter := GetParameter(I);

      if Parameter.IndexOf(' ') <> -1 then
        Parameter := Format('"%S"', [Parameter]);

      Result := Result + Parameter;
      if I <> ACount then
        Result := Result + ' ';
    end;
end;

function FindParameter(Value: string): integer;
var
  S: string;
begin
  Result := -1;
  for var I := 1 to ParamCount do
    begin
      S := GetParameter(I);
      if (Length(S) < Length(PARAM_PREFIX)+1) or (Copy(S, 1, Length(PARAM_PREFIX)) <> PARAM_PREFIX) then
        Continue;

      // Remove prefix
      S := S.Remove(0, Length(PARAM_PREFIX));
      {$IFDEF MSWINDOWS}
      // Ignore casing
      S := Lowercase(S);
      {$ENDIF}

      // Check for equalitry
      if S = Value then
        Exit( I );
    end;
end;

function HasParameter(Value: string): boolean; overload;
begin
  Result := FindParameter( Value ) <> -1;
end;

function GetParameterValue(Value: string): string; overload;
begin
  const Index = FindParameter( Value );
  Result := GetParameter(Index+1);
end;

function FindParameter(Value: char): integer;
var
  S: string;
begin
  Result := -1;
  for var I := 1 to ParamCount do
    begin
      S := GetParameter(I);
      if (Length(S) < Length(PARAM_PREFIX_CHAR)+1)
        or (Copy(S, 1, Length(PARAM_PREFIX_CHAR)) <> PARAM_PREFIX_CHAR)
        {$IFNDEF MSWINDOWS}or (Copy(S, 1, Length(PARAM_PREFIX)) = PARAM_PREFIX){$ENDIF} then
        Continue;

      // Remove prefix
      S := S.Remove(0, Length(PARAM_PREFIX_CHAR));

      // Check for char in list of chars
      if S.IndexOf( Value ) <> -1 then
        Exit( I );
    end;
end;

function HasParameter(Value: char): boolean; overload;
begin
  Result := FindParameter( Value ) <> -1;
end;

function GetParameterValue(Value: char): string; overload;
begin
  const Index = FindParameter( Value );
  Result := GetParameter(Index+1);
end;

function FindParameter(Value: string; AltChar: char): integer;
begin
  Result := FindParameter( Value );
  if Result <> -1 then
    Exit;
  Result := FindParameter( AltChar );
end;

function HasParameter(Value: string; AltChar: char): boolean; overload;
begin
  Result := FindParameter( Value, AltChar ) <> -1;
end;

function GetParameterValue(Value: string; AltChar: char): string; overload;
begin
  const Index = FindParameter( Value, AltChar );
  Result := GetParameter(Index+1);
end;

end.

