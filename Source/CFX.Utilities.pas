unit CFX.Utilities;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, Win.Registry, System.UITypes,
  Types, Vcl.Forms, Vcl.Graphics, CFX.Colors, CFX.Registry, ShellAPI,
  CFX.Types, IOUTils, RegularExpressions;

  function GetAppsUseDarkTheme: Boolean;
  function GetAccentColor( brightencolor: boolean = true ): TColor;
  function GetNTKernelVersion: single;

  function GetUserNameString: string;

  // Shell
  procedure ShellRun(Command: string; Parameters: string = '');

  // String
  function IsStringAlphaNumeric(const S: string): Boolean;

  // File
  function GetFileSize(FileName: WideString): Int64;

  function ReplaceWinPath(SrcString: string): string;
  function GetUserShellLocation(ShellLocation: FXUserShell): string;

  function GetFileBytesString(FileName: string; FirstCount: integer): TArray<string>;
  function ReadFileSignature(FileName: string): TFileType;

  // Screen
  procedure QuickScreenShot(var BitMap: TBitMap; Monitor: integer = -2);
  procedure AppScreenShot(var BitMap: TBitMap; ApplicationCapton: string);


implementation

function GetNTKernelVersion: single;
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
  ShellExecute(0, 'open', PChar(Command), PChar(Parameters), nil, 0);
end;

function IsStringAlphaNumeric(const S: string): Boolean;
begin
  Result := TRegEx.IsMatch(S, '^[a-zA-Z0-9]+$');
end;

function GetFileSize(FileName: WideString): Int64;
var
  sr : TSearchRec;
begin
  {$WARN SYMBOL_PLATFORM OFF}
  if FindFirst(fileName, faAnyFile, sr ) = 0 then
    result := Int64(sr.FindData.nFileSizeHigh) shl Int64(32) + Int64(sr.FindData.nFileSizeLow)
  else
    result := -1;
  FindClose(sr);
   {$WARN SYMBOL_PLATFORM ON}
end;

function ReplaceWinPath(SrcString: string): string;
var
  RFlags: TReplaceFlags;
begin
  RFlags := [rfReplaceAll, rfIgnoreCase];

  Result := SrcString;

  // Remove "
  Result := Result.Replace('"', '');

  // Windows Vista 2008 and above
  Result := StringReplace(Result, '%AppData%', 'C:\Users' + GetUserNameString + '\AppData\Roaming', RFlags);
  Result := StringReplace(Result, '%LocalAppData%', 'C:\Users\' + GetUserNameString + '\AppData\Local', RFlags);
  Result := StringReplace(Result, '%Public%', 'C:\Users\Public', RFlags);
  Result := StringReplace(Result, '%Temp%', 'C:\Users\' + GetUserNameString + '\AppData\Local\Temp', RFlags);
  Result := StringReplace(Result, '%Tmp%', 'C:\Users\' + GetUserNameString + '\AppData\Local\Temp', RFlags);
  Result := StringReplace(Result, '%HomePath%', 'C:\Users\' + GetUserNameString + '\', RFlags);
  Result := StringReplace(Result, '%UserProfile%', 'C:\Users\' + GetUserNameString + '\', RFlags);

  // General
  Result := StringReplace(Result, '%AllUsersProfile%', 'C:\ProgramData', RFlags);
  Result := StringReplace(Result, '%CommonProgramFiles%', 'C:\Program Files\Common Files', RFlags);
  Result := StringReplace(Result, '%CommonProgramFiles(x86)%', 'C:\Program Files (x86)\Common Files', RFlags);
  Result := StringReplace(Result, '%HomeDrive%', 'C:\', RFlags);
  Result := StringReplace(Result, '%ProgramData%', 'C:\ProgramData', RFlags);
  Result := StringReplace(Result, '%ProgramFiles%', 'C:\Program Files', RFlags);
  Result := StringReplace(Result, '%ProgramFiles(x86)%', 'C:\Program Files (x86)', RFlags);
  Result := StringReplace(Result, '%SystemDrive%', 'C:', RFlags);
  Result := StringReplace(Result, '%SystemRoot%', 'C:\Windows', RFlags);
  Result := StringReplace(Result, '%OneDrive%', 'C:\Users\' + GetUserNameString + '\Onedrive\', RFlags);
  Result := StringReplace(Result, '%OneDriveConsumer%', 'C:\Users\' + GetUserNameString + '\Onedrive\', RFlags);

  // Custom additions
  Result := StringReplace(Result, '%WindowsApps%', 'C:\Program Files\WindowsApps', RFlags);
  Result := StringReplace(Result, '%UserWindowsApps%', 'C:\Users\' + GetUserNameString + '\AppData\Local\Microsoft\WindowsApps\', RFlags);
end;


function GetUserShellLocation(ShellLocation: FXUserShell): string;
var
  RegString, RegValue: string;
begin
  case ShellLocation of
    FXUserShell.User: Exit( ReplaceWinPath('%USERPROFILE%') );
    FXUserShell.AppData: RegValue := 'AppData';
    FXUserShell.AppDataLocal: RegValue := 'Local AppData';
    FXUserShell.Documents: RegValue := 'Personal';
    FXUserShell.Pictures: RegValue := 'My Pictures';
    FXUserShell.Desktop: RegValue := 'Desktop';
    FXUserShell.Music: RegValue := 'My Music';
    FXUserShell.Videos: RegValue := 'My Video';
    FXUserShell.Network: RegValue := 'NetHood';
    FXUserShell.Recent: RegValue := 'Recent';
    FXUserShell.StartMenu: RegValue := 'Start Menu';
    FXUserShell.Programs: RegValue := 'Programs';
    FXUserShell.Startup: RegValue := 'Startup';
    FXUserShell.Downloads: RegValue := '{374DE290-123F-4565-9164-39C4925E467B}';
  end;

  RegString := FXQuickReg.GetStringValue('HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\User Shell Folders', RegValue);

  Result := ReplaceWinPath(RegString);
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

end.

