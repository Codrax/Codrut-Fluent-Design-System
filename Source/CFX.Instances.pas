unit CFX.Instances;

interface
  uses
    Windows,
    Messages,
    Vcl.Forms,
    SysUtils;

// App Identifier
function GetSemafor: string;
procedure SetSemafor(Value: string);

// Other instance
function HasOtherInstances: boolean;
procedure IPCSendMessage(target: HWND;  const message: string);

// Handle
function GetOtherHandle: HWND;
procedure FocusOtherWindow;


implementation

var
  APP_SEMAFOR: string = '';

function GetSemafor: string;
begin
  if APP_SEMAFOR = '' then
    begin
      APP_SEMAFOR := StringReplace( Application.ExeName, '.', '_', [rfReplaceAll]);
      APP_SEMAFOR := StringReplace( APP_SEMAFOR, '\', '_', [rfReplaceAll]);
      APP_SEMAFOR := StringReplace( APP_SEMAFOR, ':', '', [rfReplaceAll]);

      if Length( APP_SEMAFOR ) > 100 then
        APP_SEMAFOR := Copy( APP_SEMAFOR, Length(APP_SEMAFOR) - 100, 100 );
    end;

  Result := APP_SEMAFOR;
end;

procedure SetSemafor(Value: string);
begin
  APP_SEMAFOR := Value;
end;

function HasOtherInstances: boolean;
var
  Semafor: THandle;
begin
  { Creates }
  Semafor := CreateSemaphore( nil, 0, 1, PChar(GetSemafor) );
  Result := ((Semafor <> 0) and { application is already running }
     (GetLastError = ERROR_ALREADY_EXISTS));
end;

function GetOtherHandle: HWND;
begin
  Result := CreateSemaphore( nil, 0, 1, PChar(GetSemafor) );
end;

procedure FocusOtherWindow;
var
  npadhandle: HWnd;
begin
  npadhandle := GetOtherHandle;

  if npadhandle <> 0 then
    begin
      SetForegroundWindow(npadhandle);
      SendMessage(npadhandle, WM_SYSCOMMAND, SC_RESTORE, 0)
    end;
end;

procedure IPCSendMessage(target: HWND; const message: string);
var
  cds: TCopyDataStruct;
begin
  cds.dwData := 0;
  cds.cbData := Length(message) * SizeOf(Char);
  cds.lpData := Pointer(@message[1]);

  SendMessage(target, WM_COPYDATA, 0, LPARAM(@cds));
end;


end.
