{***********************************************************}
{                     Codruts File Systems                  }
{                                                           }
{                        version 0.1                        }
{                           ALPHA                           }
{                                                           }
{                                                           }
{                                                           }
{                                                           }
{                                                           }
{                   -- WORK IN PROGRESS --                  }
{***********************************************************}

{$WARN SYMBOL_PLATFORM OFF}

unit CFX.Files;

interface
uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  IOUtils,
  ShellAPI,
  Vcl.Forms,
  CFX.Registry,
  ComObj,
  Math,
  Registry,
  CFX.Types;

type
  TFileIOFlag = (fioConfirmMouse, fioSilent, fioNoConfirmation, fioAllowUndo,
                 fioFilesOnly, fioSimpleProgress, fioNoConfirMakeDir, fioNoErrorUI,
                 fioNoSecurityAttrib, fioNoRecursion, fioWantNukeWarning, fioNoUI);
  TFileIOFlags = set of TFileIOFlag;

// File Folder IO
function FileIoFlags(Flags: TFileIOFlags): FILEOP_FLAGS;

(* Path *)
function ReplaceWinPath(SrcString: string): string;

function ReplaceEnviromentVariabiles(SrcString: string): string;
function ReplaceShellLocations(SrcString: string): string;

function GetSystemDrive: string;
function GetSystemRoot: string;

function GetPathDepth(Path: string): integer;

function GetUserShellLocation(ShellLocation: FXUserShell): string;

function FileExtension(FileName: string; includeperiod: boolean = true): string;
function ValidateFileName(AString: string): string;

(* Redeclared *)
procedure RecycleFile(Path: string; Flags: TFileIOFlags = [fioAllowUndo]);
procedure RecycleFolder(Path: string; Flags: TFileIOFlags = [fioAllowUndo]);

(* Disk IO *)
procedure DeleteFromDisk(Path: string; Flags: TFileIOFlags = [fioAllowUndo]);
procedure RenameDiskItem(Source: string; NewName: string; Flags: TFileIOFlags);
procedure MoveDiskItem(Source: string; Destination: string; Flags: TFileIOFlags = [fioAllowUndo]);
procedure CopyDiskItem(Source: string; Destination: string; Flags: TFileIOFlags = [fioAllowUndo, fioNoConfirMakeDir]);

(* Volumes *)
procedure GetDiskSpace(const Disk: string; var FreeBytes, TotalBytes, TotalFreeBytes: int64);

(* File Information *)
function IsFileInUse(const FileName: string): Boolean;
function GetFileDate(const FileName: string; AType: FXFileDateType): TDateTime;
procedure SetFileDate(const FileName: string; AType: FXFileDateType; NewDate: TDateTime);

(* Size *)
function SizeInString(Size: int64; MaxDecimals: cardinal = 2): string;

function GetFolderSize(Path: string): int64;
function GetFolderSizeInStr(path: string): string;

function GetFileSize(FileName: WideString): Int64;
function GetFileSizeInStr(FileName: WideString): string;

(* NTFT Compression *)
function CompressItem(const Path:string;Compress:Boolean; FolderRecursive: boolean = true):integer;
function CompressFile(const FileName:string;Compress:Boolean):integer;
function CompressFolder(const FolderName:string;Recursive, Compress:Boolean): integer;

// Utilities
function GetNTVersion: single;
function GetUserNameString: string;
function GetComputerNameString: string;

implementation

function FileIoFlags(Flags: TFileIOFlags): FILEOP_FLAGS;
begin
  // Converts set TFileIOFlags flags to Bit operation
  Result := 0;
  if fioConfirmMouse in Flags then
    Result := Result or FOF_CONFIRMMOUSE;
  if fioSilent in Flags then
    Result := Result or FOF_SILENT;
  if fioNoConfirmation in Flags then
    Result := Result or FOF_NOCONFIRMATION;
  if fioAllowUndo in Flags then
    Result := Result or FOF_ALLOWUNDO;
  if fioFilesOnly in Flags  then
    Result := Result or FOF_FILESONLY;
  if fioSimpleProgress in Flags  then
    Result := Result or FOF_SIMPLEPROGRESS;
  if fioNoConfirMakeDir in Flags  then
    Result := Result or FOF_NOCONFIRMMKDIR;
  if fioNoErrorUI in Flags  then
    Result := Result or FOF_NOERRORUI;
  if fioNoSecurityAttrib in Flags  then
    Result := Result or FOF_NOCOPYSECURITYATTRIBS;
  if fioNoRecursion in Flags  then
    Result := Result or FOF_NORECURSION;
  if fioWantNukeWarning in Flags  then
    Result := Result or FOF_WANTNUKEWARNING;
  if fioNoUI in Flags  then
    Result := Result or FOF_NO_UI;
end;

procedure RecycleFile(Path: string; Flags: TFileIOFlags);
begin
  DeleteFromDisk( Path, Flags );
end;

procedure RecycleFolder(Path: string; Flags: TFileIOFlags);
begin
  DeleteFromDisk( Path, Flags );
end;

procedure DeleteFromDisk(Path: string; Flags: TFileIOFlags);
var
  FileStructure: TSHFileOpStruct;
begin
  with FileStructure do
  begin
    Wnd := Application.Handle;
    wFunc := FO_DELETE;
    pFrom := PChar( ReplaceWinPath(Path) + #0 );

    // Flags
    fFlags := FileIOFlags( Flags );
  end;
  try
    SHFileOperation(FileStructure);
  except
    on EAccessViolation do
      RaiseLastOSError;
  end;
end;

procedure RenameDiskItem(Source: string; NewName: string; Flags: TFileIOFlags);
var
  FileStructure: TSHFileOpStruct;
begin
  with FileStructure do
  begin
    Wnd := Application.Handle;
    wFunc := FO_RENAME;
    pFrom := PChar( Source );
    pTo := PChar( IncludeTrailingPathDelimiter( ExtractFileDir( Source ) ) + NewName );

    // Flags
    fFlags := FileIOFlags( Flags );
  end;
  try
    SHFileOperation(FileStructure);
  except
    on EAccessViolation do
      RaiseLastOSError;
  end;
end;

procedure MoveDiskItem(Source: string; Destination: string; Flags: TFileIOFlags);
var
  FileStructure: TSHFileOpStruct;
begin
  with FileStructure do
  begin
    Wnd := Application.Handle;
    wFunc := FO_MOVE;
    pFrom := PChar( ExcludeTrailingPathDelimiter( ReplaceWinPath(Source) ) );
    { ExcludeTrailingPathDelimiter is required, as if a / is present the function
    will not work }
    pTo := PChar( ReplaceWinPath(Destination) );

    // Flags
    fFlags := FileIOFlags( Flags );
  end;
  try
    SHFileOperation(FileStructure);
  except
    on EAccessViolation do
      RaiseLastOSError;
  end;
end;

procedure CopyDiskItem(Source: string; Destination: string; Flags: TFileIOFlags);
var
  FileStructure: TSHFileOpStruct;
begin
  with FileStructure do
  begin
    Wnd := Application.Handle;
    wFunc := FO_COPY;
    pFrom := PChar( ExcludeTrailingPathDelimiter( ReplaceWinPath(Source) ) );
    pTo := PChar( ReplaceWinPath(Destination) ); { The reason this PChar does not have
      ExcludeTrailingPathDelimiter is because if the Destination has a final \ it means
      to Copy the folder as it is and to become a subfolder of the Destionation, otherwise
      It will override the folder if the \ is not present. }

    // Flags
    fFlags := FileIOFlags( Flags );
  end;
  try
    SHFileOperation(FileStructure);
  except
    on EAccessViolation do
      RaiseLastOSError;
  end;
end;

procedure GetDiskSpace(const Disk: string; var FreeBytes, TotalBytes, TotalFreeBytes: int64);
var
  RootPath: PChar;
  AFreeBytes, ATotalBytes, ATotalFreeBytes: ULARGE_INTEGER;
begin
  RootPath := PChar(Disk);
  if not SHGetDiskFreeSpace(RootPath, AFreeBytes, ATotalBytes, ATotalFreeBytes) then
    RaiseLastOSError;

  FreeBytes := AFreeBytes.QuadPart;
  TotalBytes := ATotalBytes.QuadPart;
  TotalFreeBytes := ATotalFreeBytes.QuadPart;
end;

function IsFileInUse(const FileName: string): Boolean;
var
  HFileRes: HFILE;
begin
  Result := False;
  HFileRes := CreateFile(PChar(FileName), GENERIC_READ or GENERIC_WRITE, 0, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if HFileRes = INVALID_HANDLE_VALUE then
  begin
    if GetLastError = ERROR_SHARING_VIOLATION then
      Result := True;
  end
  else
  begin
    CloseHandle(HFileRes);
  end;
end;

function GetFileDate(const FileName: string; AType: FXFileDateType): TDateTime;
begin
  if NOT fileexists(FileName) then
    Exit(0);

  // Get by Type
  case AType of
    FXFileDateType.Create: Result := TFile.GetCreationTime(FileName);
    FXFileDateType.Modify: Result := TFile.GetLastWriteTime(FileName);
    FXFileDateType.Access: Result := TFile.GetLastAccessTime(FileName);
    else Result := 0;
  end;
end;

procedure SetFileDate(const FileName: string; AType: FXFileDateType; NewDate: TDateTime);
begin
  if NOT fileexists(FileName) then
    Exit;

  // Get by Type
  case AType of
    FXFileDateType.Create: TFile.SetCreationTime(FileName, NewDate);
    FXFileDateType.Modify: TFile.SetLastWriteTime(FileName, NewDate);
    FXFileDateType.Access: TFile.SetLastAccessTime(FileName, NewDate);
  end;
end;

function ReplaceWinPath(SrcString: string): string;
begin
  Result := SrcString;

  Result := ReplaceShellLocations(Result);
  Result := ReplaceEnviromentVariabiles(Result);
end;

function ReplaceEnviromentVariabiles(SrcString: string): string;
const
  ENV = '%';
var
  PStart, PEnd: integer;
  SContain, SResult: string;
  Valid: boolean;
begin
  // Initialise
  PEnd := 1;

  Result := SrcString;

  repeat
    // Get Positions
    PStart := Pos( ENV, SrcString, PEnd );
    PEnd := Pos( ENV, SrcString, PStart + 1 );

    // Validate
    Valid := (PStart > 0) and (PEnd > 0);

    // Replace
    if Valid then
      begin
        SContain := Copy( SrcString, PStart, PEnd - PStart + 1 );

        SResult := GetEnvironmentVariable( SContain.Replace(ENV, '') );

        if SResult <> '' then
          Result := StringReplace( Result, SContain, SResult, [rfIgnoreCase] );
      end;

  until not Valid;
end;

function ReplaceShellLocations(SrcString: string): string;
const
  GLOBAL_SHELL = 'SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders\';
  USER_SHELL = 'Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders\';

  SHELL_BEGIN = 'shell:';
var
  R: TRegistry;

  Items_Global,
  Items_User: TStringList;
  I: Integer;

  IName,
  IValue: string;
begin
  Result := SrcString;

  // No Val
  if Pos(SHELL_BEGIN, Result) = 0 then
    Exit;

  // Create
  R := TRegistry.Create( KEY_READ );

  Items_Global := TStringList.Create;
  Items_User := TStringList.Create;
  try
    // Read possibile values for global
    R.RootKey := HKEY_LOCAL_MACHINE;
    R.OpenKeyReadOnly( GLOBAL_SHELL );
    R.GetValueNames(Items_Global);

    // Replace Items
    for I := 0 to Items_Global.Count - 1 do
      begin
        IName := AnsiLowerCase(Items_Global[I]);

        if Pos(SHELL_BEGIN + IName, AnsiLowerCase(Result)) <> 0 then
          begin
            // Read Value
            IValue := R.ReadString( IName );

            Result := StringReplace( Result, SHELL_BEGIN + IName, IValue, [rfIgnoreCase, rfReplaceAll] );
          end;
      end;

    // Read possible values for user
    R.RootKey := HKEY_CURRENT_USER;
    R.OpenKeyReadOnly( USER_SHELL );
    R.GetValueNames(Items_User);

    // Replace Items
    for I := 0 to Items_User.Count - 1 do
      begin
        IName := AnsiLowerCase(Items_User[I]);

        if Pos(SHELL_BEGIN + IName, AnsiLowerCase(Result)) <> 0 then
          begin
            // Read Value
            IValue := R.ReadString( IName );

            Result := StringReplace( Result, SHELL_BEGIN + IName, IValue, [rfIgnoreCase, rfReplaceAll] );
          end;
      end;

  finally
    Items_Global.Free;
    Items_User.Free;

    R.Free;
  end;
end;

function GetSystemDrive: string;
begin
  Result := ReplaceEnviromentVariabiles( '%SYSTEMDRIVE%' );
end;

function GetSystemRoot: string;
begin
  Result := ReplaceEnviromentVariabiles( '%SYSTEMROOT%' );
end;

function GetPathDepth(Path: string): integer;
begin
  Path := IncludeTrailingPathDelimiter(Path);
  Result := Path.CountChar('\');
end;

function CompressItem(const Path:string;Compress:Boolean; FolderRecursive: boolean):integer;
begin
  Result := 0;
  if TFile.Exists(Path) then
    CompressFile(Path, Compress)
  else
    CompressFolder(Path, FolderRecursive, Compress);
end;

function CompressFile(const FileName:string;Compress:Boolean):integer;
var
  FSWbemLocator : OLEVariant;
  FWMIService   : OLEVariant;
  FWbemObject   : OLEVariant;
begin;
  FSWbemLocator := CreateOleObject('WbemScripting.SWbemLocator');
  FWMIService   := FSWbemLocator.ConnectServer('localhost', 'root\CIMV2', '', '');
  FWbemObject   := FWMIService.Get(Format('CIM_DataFile.Name="%s"',[StringReplace(FileName,'\','\\',[rfReplaceAll])]));
  if Compress then
    Result:=FWbemObject.Compress()
  else
    Result:=FWbemObject.UnCompress();
end;

function CompressFolder(const FolderName:string;Recursive, Compress:Boolean):integer;
var
  FSWbemLocator : OLEVariant;
  FWMIService   : OLEVariant;
  FWbemObject   : OLEVariant;
  StopFileName  : OLEVariant;
begin;
  FSWbemLocator := CreateOleObject('WbemScripting.SWbemLocator');
  FWMIService   := FSWbemLocator.ConnectServer('localhost', 'root\CIMV2', '', '');
  FWbemObject   := FWMIService.Get(Format('CIM_Directory.Name="%s"',[StringReplace(FolderName,'\','\\',[rfReplaceAll])]));
  if Compress then
    if Recursive then
     Result:=FWbemObject.CompressEx(StopFileName, Null, Recursive)
    else
     Result:=FWbemObject.Compress()
  else
    if Recursive then
     Result:=FWbemObject.UnCompressEx(StopFileName, Null, Recursive)
    else
     Result:=FWbemObject.UnCompress();
end;

function GetNTVersion: single;
begin
  Result := Win32MajorVersion + Win32MinorVersion / 10;
end;

function GetUserNameString: string;
var
  nSize: DWord;
begin
 nSize := 1024;
 SetLength(Result, nSize);

 // Error
 if GetUserName(PChar(Result), nSize) then
   SetLength(Result, nSize-1)
 else
   RaiseLastOSError;
end;

function GetComputerNameString: string;
var
  nSize: DWord;
begin
 nSize := 1024;
 SetLength(Result, nSize);

 if not GetComputerName(PChar(Result), nSize) then
   // Error
   RaiseLastOSError;
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

function FileExtension(FileName: string; includeperiod: boolean): string;
begin
  Result := ExtractFileExt( FileName );

  if not includeperiod then
    Result := Copy( Result, 2, Length( Result ) );
end;

function ValidateFileName(AString: string): string;
var
  x: integer;
const
  IllegalCharSet: TSysCharSet =
    ['|','<','>','\','^','+','=','?','/','[',']','"',';',',','*'];
begin
  for x := 1 to Length(AString) do
    if CharInSet(AString[x], IllegalCharSet) then
      AString[x] := '_';
  Result := AString;
end;

function GetFolderSize( Path: string ): int64;
var
 tsr: TSearchRec;
begin
  result := 0;
  // Path
  Path := ReplaceWinPath( IncludeTrailingPathDelimiter ( path ) );

  // Search
  if FindFirst ( path + '*', faAnyFile, tsr ) = 0 then begin
    repeat
      if ( tsr.attr and faDirectory ) > 0 then
        begin
          if ( tsr.name <> '.' ) and ( tsr.name <> '..' ) then
            inc ( result, GetFolderSize ( path + tsr.name ) );
        end
      else
        begin
          inc ( result, tsr.size );
        end;
    until FindNext ( tsr ) <> 0;
  FindClose ( tsr );
 end;
end;

function GetFolderSizeInStr(path: string): string;
begin
  if DirectoryExists(path) then begin
    Result := SizeInString(GetFolderSize(path));
  end else
    Result := 'NaN';
end;

function SizeInString(Size: int64; MaxDecimals: cardinal): string;
var
  Decim: integer;
  DivValue: integer;
begin
  Decim := Trunc( Power( 10, MaxDecimals ) );

  // Get Div Value
  case Abs( size ) of
    0..1023: DivValue := 0; // B
    1024..1048575: DivValue := 1; // KB
    1048576..1073741823: DivValue := 2; // MB
    else DivValue := 3;
  end;

  // Div
  Result := FloatToStr( Trunc(Size / Power( 1024, DivValue) * Decim ) / Decim ) ;

  // Measurement
  case DivValue of
    0: Result := Concat( Result, ' ', 'B' );
    1: Result := Concat( Result, ' ', 'KB' );
    2: Result := Concat( Result, ' ', 'MB' );
    3: Result := Concat( Result, ' ', 'GB' );
  end;
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

function GetFileSizeInStr(FileName: WideString): string;
begin
  if FileExists(fileName) then begin
    Result := SizeInString(GetFileSize(filename));
  end else
    Result := 'NaN';
end;

end.
