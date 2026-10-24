unit CFX.Instances;

{$SCOPEDENUMS ON}

/// EASY MODE:
///  Initialize the instance with InitializeInstance() as you desire
///  If the instance exists and HasAppInfo() is true, you can use
///  GetAppInfo() to get info about the app like the HWND and PID.
///
///  Nothing needs to be closed as It's done automatically when the app closes.
///

interface

uses
  Winapi.Windows, Winapi.Messages, Vcl.Forms, SysUtils;

type
  TAutoInstanceMode = (TerminateIfOtherExist, TerminateAndFocusOther);

  TAppSharedInfo = packed record
    Valid: boolean;
    PID: DWORD;
    HWND: HWND;
    Data: Pointer;
  end;
  PAppSharedInfo = ^TAppSharedInfo;

// Utils
procedure IPCSendMessage(target: HWND;  const message: string);

(* Easy mode *)
procedure InitializeInstance(EnableSharedMemorySpace: boolean; WriteProcessInfo: boolean=true);

// Info
function HasOtherInstance: boolean;

// Name
function GetSemaphore: string;
procedure SetSemaphore(Value: string);

(* Advanced users *)
// Lock
function LockSemaphore: boolean;
function UnlockSemaphore: boolean;

// Info
function HasAppInfo: boolean;
function OpenAppInfo(WriteAccess: boolean=true): boolean;
procedure CloseAppInfo;

procedure PutAppInfo; overload;
procedure PutAppInfo(hwnd: HWND); overload;
function GetAppInfo: TAppSharedInfo;

(* Automatic tasks *)
procedure InstanceAuto(Mode: TAutoInstanceMode); overload;
procedure InstanceAuto(Mode: TAutoInstanceMode; AExitCode: integer; SetExitCode: boolean); overload;

procedure BringOtherWindowToTopAuto;
procedure SendOtherWindowMessageAuto(Msg: UINT; wParam: WPARAM; lParam: LPARAM);

var
  HaltResultCode: integer = 0;

implementation

var
  // Semafore name
  APP_SEMAFOR: string = '';

  // Handle
  Semafor: THandle;
  SemaforCreated: boolean;
  SemaforRefused: boolean;

  // Map
  SharedAppInfoMap: THandle;
  SharedAppInfo: PAppSharedInfo;

procedure IPCSendMessage(target: HWND; const message: string);
var
  cds: TCopyDataStruct;
begin
  cds.dwData := 0;
  cds.cbData := Length(message) * SizeOf(Char);
  cds.lpData := Pointer(@message[1]);

  SendMessage(target, WM_COPYDATA, 0, LPARAM(@cds));
end;

procedure InitializeInstance(EnableSharedMemorySpace, WriteProcessInfo: boolean);
begin
  // Semaphore
  LockSemaphore;

  // Shared mem?
  if EnableSharedMemorySpace then
    if OpenAppInfo( not SemaforRefused ) then
      // Write info
      if WriteProcessInfo and not SemaforRefused then
        PutAppInfo;
end;

function HasOtherInstance: boolean;
begin
  if not SemaforCreated then
    raise Exception.Create('Semaphore is not created.');

  Result := SemaforRefused;
end;

function GetSemaphore: string;
begin
  if APP_SEMAFOR = '' then begin
    APP_SEMAFOR := StringReplace( Application.ExeName, '.', '_', [rfReplaceAll]);
    APP_SEMAFOR := StringReplace( APP_SEMAFOR, '\', '_', [rfReplaceAll]);
    APP_SEMAFOR := StringReplace( APP_SEMAFOR, ':', '', [rfReplaceAll]);

    if Length( APP_SEMAFOR ) > 100 then
      APP_SEMAFOR := Copy( APP_SEMAFOR, Length(APP_SEMAFOR) - 100, 100 );
  end;

  Result := APP_SEMAFOR;
end;

procedure SetSemaphore(Value: string);
begin
  APP_SEMAFOR := Value;
end;

function LockSemaphore: boolean;
begin
  Semafor := CreateSemaphore( nil, 0, 1, PChar(GetSemaphore) );

  SemaforCreated := true;
  SemaforRefused := ((Semafor <> 0) and { application is already running }
     (GetLastError = ERROR_ALREADY_EXISTS));

  // Successfully locked?
  Result := not SemaforRefused;
end;

function UnlockSemaphore: boolean;
begin
  SemaforCreated := false;
  SemaforRefused := false;

  // Close handle
  Result := CloseHandle(Semafor);
end;

function SHARED_MEM_NAME: string;
begin
  Result := 'mem_appinfo_' + APP_SEMAFOR;
end;

function HasAppInfo: boolean;
begin
  Result := (SharedAppInfo <> nil) and (SharedAppInfoMap <> 0);
end;

function OpenAppInfo(WriteAccess: boolean=true): boolean;
begin
  Result := false;

  // Create mapping
  if WriteAccess then
    SharedAppInfoMap := CreateFileMapping( INVALID_HANDLE_VALUE, nil, PAGE_READWRITE, 0,
      SizeOf(TAppSharedInfo), PChar(SHARED_MEM_NAME))
  else
    SharedAppInfoMap := OpenFileMapping(FILE_MAP_READ, False, PChar(SHARED_MEM_NAME));

  if SharedAppInfoMap = 0 then
    Exit;

  // Read info
  if WriteAccess then
    SharedAppInfo := MapViewOfFile(SharedAppInfoMap, FILE_MAP_WRITE, 0, 0, SizeOf(TAppSharedInfo))
  else
    SharedAppInfo := MapViewOfFile(SharedAppInfoMap, FILE_MAP_READ, 0, 0, SizeOf(TAppSharedInfo));
  if SharedAppInfo = nil then
    Exit;

  Result := true;
end;

procedure CloseAppInfo;
begin
  if not HasAppInfo then
    Exit;

  // Close variabile
  UnmapViewOfFile(SharedAppInfo);
  SharedAppInfo := nil;

  // Close map
  CloseHandle(SharedAppInfoMap);
  SharedAppInfoMap := 0;
end;

procedure PutAppInfo;
begin
  PutAppInfo( Application.MainForm.Handle );
end;

procedure PutAppInfo(hwnd: HWND); overload;
begin
  SharedAppInfo^.Valid := true;
  SharedAppInfo^.PID := GetCurrentProcessId;
  SharedAppInfo^.HWND := hwnd;
end;

function GetAppInfo: TAppSharedInfo;
begin
  Result := SharedAppInfo^;
end;

procedure InstanceAuto(Mode: TAutoInstanceMode);
begin
  InstanceAuto(Mode, 0, false);
end;

procedure InstanceAuto(Mode: TAutoInstanceMode; AExitCode: integer; SetExitCode: boolean);
procedure DoHalt;
begin
  if SetExitCode then
    Halt( AExitCode )
  else
    Halt;
end;
begin
  InitializeInstance(Mode <> TAutoInstanceMode.TerminateIfOtherExist);
  if not HasOtherInstance then
    Exit;

  // Handle case of otuer window
  case Mode of
    TAutoInstanceMode.TerminateIfOtherExist: DoHalt;
    TAutoInstanceMode.TerminateAndFocusOther: begin
      BringOtherWindowToTopAuto;
      DoHalt;
    end;
  end;
end;

procedure BringOtherWindowToTopAuto;
var
  Info: TAppSharedInfo;
begin
  if HasAppInfo then begin
    Info := GetAppInfo;

    SendMessage(Info.HWND, WM_SYSCOMMAND, SC_RESTORE, 0); // restore a minimize window
    SetForegroundWindow(Info.HWND);
    SetActiveWindow(Info.HWND);
    SetWindowPos(Info.HWND, HWND_TOP, 0, 0, 0, 0, SWP_SHOWWINDOW or SWP_NOMOVE or SWP_NOSIZE);
    //redraw to prevent the window blank.
    RedrawWindow(Info.HWND, nil, 0, RDW_FRAME or RDW_INVALIDATE or RDW_ALLCHILDREN );
  end;
end;

procedure SendOtherWindowMessageAuto(Msg: UINT; wParam: WPARAM; lParam: LPARAM);
var
  Info: TAppSharedInfo;
begin
  if HasAppInfo then begin
    Info := GetAppInfo;
    SendMessage(Info.HWND, Msg, WPARAM, wParam);
  end;
end;

initialization

finalization
  // Auto close semaphore
  if SemaforCreated then
    UnlockSemaphore;

  // Auto close mapping
  if HasAppInfo then
    CloseAppInfo;
end.
