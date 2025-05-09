{***********************************************************}
{                    Codruts Win Register                   }
{                                                           }
{                         version 1.1                       }
{                           RELEASE                         }
{                                                           }
{                                                           }
{                                                           }
{                                                           }
{                                                           }
{                   -- WORK IN PROGRESS --                  }
{***********************************************************}


unit CFX.Registry;

interface
  uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Win.Registry, Vcl.Dialogs;

type
  TRegistryMode = (rmUnloaded, rmWindows32, rmWindows64, rmAutomatic);
  TRegistryError = (reNone, reAccessDenied, reKeyNoExist, reReadError);
  TRegistryNeed = (rnRead, rnWrite, rnComplete);

  // Moved Helper from Cod.VarHelpers for FMX compatability
  TRegHelper = class helper for TRegistry
    procedure RenameKey(const OldName, NewName: string);
    function CloneKey(const KeyName: string): string;

    function ReadCardinal(const Name: string): Cardinal;

    procedure MoveKeyTo(const OldName, NewKeyPath: string; Delete: Boolean);
  end;

  // Predefine
  FXRegistry = class;

  // TQuickReg Class
  FXQuickReg = class
  public
    class function CreateKey(KeyLocation: string): boolean;
    class function KeyExists(KeyLocation: string): boolean;
    class function DeleteKey(KeyLocation: string): boolean;
    class function RenameKey(KeyLocation, NewName: string): boolean;

    class function GetStringValue(KeyLocation, ValueName: string): string;

    class function GetValueExists(KeyLocation, ValueName: string): boolean;
    class function DeleteValue(KeyLocation, ValueName: string): boolean;
  end;

  // TWinRegistry Class
  FXRegistry = class(TObject)
  private
    // Vars
    FLastError: TRegistryError;
    FErrorMessage: boolean;
    FRegistry: TRegistry;
    FRegistryMode: TRegistryMode;
    FHive, FDefaultHive: HKEY;
    FAutoHive: boolean;

    // Registry Edit
    procedure CreateReg(AType: TRegistryNeed; APosition: string = '');

    function GetPathEnd(Path: string): string;
    function GetPathItem(Path: string): string;
    procedure ApplyPath(var Path: string);
    procedure RemovePathLevels(var Path: string; Levels: integer);

    // Error
    procedure RaiseError(Error: TRegistryError);

    // Registry Mode
    function ApplyRegMode(mode: Cardinal = KEY_ALL_ACCESS): Cardinal;

    // Imported Utils
    function IsWOW64Emulated: Boolean;
    function IsWow64Executable: Boolean;
  procedure SetManualHive(const Value: HKEY);

  public
    // Create
    constructor Create;
    destructor Destroy; override;

    // Registry Mode
    procedure SetNewRegistryMode(AMode: TRegistryMode);

    // Key Functions
    function CreateKey(KeyLocation: string): boolean;
    function KeyExists(KeyLocation: string): boolean;
    function DeleteKey(KeyLocation: string): boolean;
    function CloneKey(KeyLocation: string): string;
    function RenameKey(KeyLocation, NewName: string): boolean;
    function MoveKey(KeyLocation, NewLocation: string; AlsoDelete: boolean = true): boolean;
    function CopyKey(KeyLocation, NewLocation: string): boolean;

    function GetKeyNames(KeyLocation: string): TStringList;
    function GetValueNames(KeyLocation: string): TStringList;

    function GetValueExists(KeyLocation, ValueName: string): boolean;
    function DeleteValue(KeyLocation, ValueName: string): boolean;

    procedure WriteValue(KeyLocation, ItemName: string; Value: string = ''); overload;
    procedure WriteValue(KeyLocation, ItemName: string; Value: integer = 0); overload;
    procedure WriteValue(KeyLocation, ItemName: string; Value: boolean = false); overload;
    procedure WriteValue(KeyLocation, ItemName: string; Value: double = 0); overload;
    procedure WriteValue(KeyLocation, ItemName: string; Value: TDateTime); overload;
    procedure WriteValue(KeyLocation, ItemName: string; Value: TDate); overload;
    procedure WriteValue(KeyLocation, ItemName: string; Value: TTime); overload;

    function GetStringValue(KeyLocation, ValueName: string): string;
    function GetIntValue(KeyLocation, ValueName: string): integer;
    function GetDateTimeValue(KeyLocation, ValueName: string): TDateTime;
    function GetBooleanValue(KeyLocation, ValueName: string): boolean;
    function GetFloatValue(KeyLocation, ValueName: string): double;
    function GetCurrencyValue(KeyLocation, ValueName: string): currency;
    function GetTimeValue(KeyLocation, ValueName: string): TTime;
    function GetDateValue(KeyLocation, ValueName: string): TDate;

    function GetValueType(KeyLocation, ValueName: string): TRegDataType;
    function GetValueAsString(KeyLocation, ValueName: string): string;
    function GetValueAsStringEx(KeyLocation, ValueName: string): string;

    procedure WriteStringValue(KeyLocation, ItemName: string; Value: string);
    procedure WriteIntValue(KeyLocation, ItemName: string; Value: integer);
    procedure WriteDateTimeValue(KeyLocation, ItemName: string; Value: TDateTime);
    procedure WriteBooleanValue(KeyLocation, ItemName: string; Value: boolean);
    procedure WriteFloatValue(KeyLocation, ItemName: string; Value: double);
    procedure WriteCurrency(KeyLocation, ItemName: string; Value: Currency);
    procedure WriteTime(KeyLocation, ItemName: string; Value: TTime);
    procedure WriteDate(KeyLocation, ItemName: string; Value: TDate);

    (* Properties *)
    property LastError: TRegistryError read FLastError;
    property ErrorMessage: boolean read FErrorMessage write FErrorMessage;
    property RegistryMode: TRegistryMode read FRegistryMode write FRegistryMode;

    // Registry Mode
    procedure ResetRegistryMode;
    function WinModeLoaded: boolean;

    // Hive
    property AutomaticHive: boolean read FAutoHive write FAutoHive;
    property ManualHive: HKEY read FDefaultHive write SetManualHive;
    (* Detect the hive automatically from the KeyLocation, overriden by DefaultHive *)
    property DefaultHive: HKEY read FDefaultHive write FDefaultHive;
  end;

const
  KEY_SEPAR = '\';
  COMPUTER_BEGIN = 'Computer' + KEY_SEPAR;

implementation

{ TWinRegistry }

function FXRegistry.WinModeLoaded: boolean;
begin
  Result := FRegistryMode <> rmUnloaded;

  if not Result then
    begin
      ResetRegistryMode;

      // Loaded
      Result := true;
    end;
end;

procedure FXRegistry.ResetRegistryMode;
begin
  // Registry Variant
  if IsWOW64Emulated or IsWow64Executable then
    FRegistryMode := rmWindows64
  else
    FRegistryMode := rmWindows32;
end;

function FXRegistry.IsWOW64Emulated: Boolean;
var
  IsWow64: BOOL;
begin
  // Check if the current process is running under WOW64
  if IsWow64Process(GetCurrentProcess, IsWow64) then
    Result := IsWow64
  else
    Result := False;
end;

function FXRegistry.IsWow64Executable: Boolean;
type
  TIsWow64Process = function(AHandle: DWORD; var AIsWow64: BOOL): BOOL; stdcall;

var
  hIsWow64Process: TIsWow64Process;
  hKernel32: DWORD;
  IsWow64: BOOL;

begin
  Result := True;

  hKernel32 := Winapi.Windows.LoadLibrary('kernel32.dll');
  if hKernel32 = 0 then Exit;

  try
    @hIsWow64Process := Winapi.Windows.GetProcAddress(hKernel32, 'IsWow64Process');
    if not System.Assigned(hIsWow64Process) then
      Exit;

    IsWow64 := False;
    if hIsWow64Process(Winapi.Windows.GetCurrentProcess, IsWow64) then
      Result := not IsWow64;

  finally
    Winapi.Windows.FreeLibrary(hKernel32);
  end;
end;

constructor FXRegistry.Create;
begin
  inherited Create;

  // Default
  FDefaultHive := HKEY_CURRENT_USER;
  FErrorMessage := true;
  FLastError := reNone;
  FAutoHive := true;
end;

destructor FXRegistry.Destroy;
begin
  // Free Registry
  FRegistry.Free;

  inherited Destroy;
end;

function FXRegistry.ApplyRegMode(mode: Cardinal): Cardinal;
begin
  // Select a registry based on arhitecture
  case FRegistryMode of
    rmWindows32: Result := mode OR KEY_WOW64_32KEY;
    rmWindows64: Result := mode OR KEY_WOW64_64KEY;
    else Result := mode;
  end;
end;

function FXRegistry.CreateKey(KeyLocation: string): boolean;
begin
  // Prepare bPath & Open
  ApplyPath( KeyLocation );
  CreateReg( TRegistryNeed.rnWrite, GetPathItem(KeyLocation) );

  // Create Key
  Result := false;
  try
    Result := FRegistry.CreateKey( GetPathEnd(KeyLocation) );
  except
    RaiseError( TRegistryError.reAccessDenied );
  end;
end;

function FXRegistry.DeleteKey(KeyLocation: string): boolean;
begin
  // Prepare bPath & Open
  ApplyPath( KeyLocation );
  CreateReg( TRegistryNeed.rnWrite, GetPathItem(KeyLocation) );

  // Create Key
  Result := false;
  try
    Result := FRegistry.DeleteKey( GetPathEnd(KeyLocation) );
  except
    RaiseError( TRegistryError.reAccessDenied );
  end;
end;

function FXRegistry.DeleteValue(KeyLocation, ValueName: string): boolean;
begin
  // Prepare bPath & Open
  ApplyPath( KeyLocation );
  CreateReg( TRegistryNeed.rnWrite, KeyLocation );

  // Create Key
  Result := false;
  try
    Result := FRegistry.DeleteValue( ValueName );
  except
    RaiseError( TRegistryError.reAccessDenied );
  end;
end;

function FXRegistry.GetStringValue(KeyLocation, ValueName: string): string;
begin
  // Prepare bPath & Open
  ApplyPath( KeyLocation );
  CreateReg( TRegistryNeed.rnRead, KeyLocation );

  // Create Key
  try
    Result := FRegistry.ReadString(ValueName);
  except
    RaiseError( TRegistryError.reReadError );
  end;
end;

function FXRegistry.GetTimeValue(KeyLocation, ValueName: string): TTime;
begin
  // Prepare bPath & Open
  ApplyPath( KeyLocation );
  CreateReg( TRegistryNeed.rnRead, KeyLocation );

  // Create Key
  Result := 0;
  try
    Result := FRegistry.ReadTime(ValueName);
  except
    RaiseError( TRegistryError.reReadError );
  end;
end;

function FXRegistry.GetDateTimeValue(KeyLocation, ValueName: string): TDateTime;
begin
  // Prepare bPath & Open
  ApplyPath( KeyLocation );
  CreateReg( TRegistryNeed.rnRead, KeyLocation );

  // Create Key
  Result := 0;
  try
    Result := FRegistry.ReadDateTime(ValueName);
  except
    RaiseError( TRegistryError.reReadError );
  end;
end;

function FXRegistry.GetDateValue(KeyLocation, ValueName: string): TDate;
begin
  // Prepare bPath & Open
  ApplyPath( KeyLocation );
  CreateReg( TRegistryNeed.rnRead, KeyLocation );

  // Create Key
  Result := 0;
  try
    Result := FRegistry.ReadDate(ValueName);
  except
    RaiseError( TRegistryError.reReadError );
  end;
end;

function FXRegistry.GetFloatValue(KeyLocation, ValueName: string): double;
begin
  // Prepare bPath & Open
  ApplyPath( KeyLocation );
  CreateReg( TRegistryNeed.rnRead, KeyLocation );

  // Create Key
  Result := 0;
  try
    Result := FRegistry.ReadFloat(ValueName);
  except
    RaiseError( TRegistryError.reReadError );
  end;
end;

function FXRegistry.GetValueType(KeyLocation, ValueName: string): TRegDataType;
begin
  // Prepare bPath & Open
  ApplyPath( KeyLocation );
  CreateReg( TRegistryNeed.rnRead, KeyLocation );

  // Create Key
  Result := rdUnknown;
  try
    Result := FRegistry.GetDataType( ValueName );
  except
    RaiseError( TRegistryError.reReadError );
  end;
end;

function FXRegistry.GetValueAsStringEx(KeyLocation, ValueName: string): string;
begin
  // Prepare bPath & Open
  ApplyPath( KeyLocation );
  CreateReg( TRegistryNeed.rnRead, KeyLocation );

  // Create Key
  try
    Result := FRegistry.GetDataAsString(ValueName);
  except
    RaiseError( TRegistryError.reReadError );
  end;
end;

function FXRegistry.GetValueAsString(KeyLocation, ValueName: string): string;
var
  ItemType: TRegDataType;
begin
  // Prepare bPath & Open
  ApplyPath( KeyLocation );
  CreateReg( TRegistryNeed.rnRead, KeyLocation );

  // Create Key
  try
    ItemType := FRegistry.GetDataType(ValueName);

    case ItemType of
      rdUnknown, rdString, rdExpandString: Result := GetStringValue(KeyLocation, ValueName);
      rdInteger: Result := inttostr( GetIntValue(KeyLocation, ValueName) );
      rdBinary: GetStringValue(KeyLocation, ValueName);
    end;
  except
    RaiseError( TRegistryError.reReadError );
  end;
end;

function FXRegistry.GetValueExists(KeyLocation, ValueName: string): boolean;
begin
  // Prepare bPath & Open
  ApplyPath( KeyLocation );
  CreateReg( TRegistryNeed.rnRead, KeyLocation );

  // Create Key
  Result := false;
  try
    Result := FRegistry.ValueExists( ValueName );
  except
    RaiseError( TRegistryError.reReadError );
  end;
end;

function FXRegistry.GetValueNames(KeyLocation: string): TStringList;
begin
  // Prepare bPath & Open
  ApplyPath( KeyLocation );
  CreateReg( TRegistryNeed.rnRead, KeyLocation );

  // Create Key
  Result := TStringList.Create;
  try
    FRegistry.GetValueNames( Result );
  except
    RaiseError( TRegistryError.reReadError );
  end;
end;

function FXRegistry.KeyExists(KeyLocation: string): boolean;
begin
  // Prepare bPath & Open
  ApplyPath( KeyLocation );
  CreateReg( TRegistryNeed.rnRead, GetPathItem(KeyLocation) );

  // Create Key
  Result := false;
  try
    Result := FRegistry.KeyExists( GetPathEnd(KeyLocation) );
  except
    RaiseError( TRegistryError.reReadError );
  end;
end;

function FXRegistry.CloneKey(KeyLocation: string): string;
begin
  // Prepare bPath & Open
  ApplyPath( KeyLocation );
  CreateReg( TRegistryNeed.rnComplete, GetPathItem(KeyLocation) );

  // Create Key
  try
    Result := FRegistry.CloneKey( GetPathEnd(KeyLocation) );
  except
    RaiseError( TRegistryError.reAccessDenied );
  end;
end;

function FXRegistry.RenameKey(KeyLocation, NewName: string): boolean;
begin
  // Prepare bPath & Open
  ApplyPath( KeyLocation );
  CreateReg( TRegistryNeed.rnComplete, GetPathItem(KeyLocation) );

  // Create Key
  Result := false;
  try
    FRegistry.RenameKey( GetPathEnd(KeyLocation), NewName );

    Result := FRegistry.KeyExists( NewName );
  except
    RaiseError( TRegistryError.reKeyNoExist );
  end;
end;

procedure FXRegistry.SetManualHive(const Value: HKEY);
begin
  FAutoHive := false;
  FDefaultHive := Value;
end;

procedure FXRegistry.SetNewRegistryMode(AMode: TRegistryMode);
begin
  FRegistryMode := AMode;
end;

function FXRegistry.MoveKey(KeyLocation, NewLocation: string; AlsoDelete: boolean = true): boolean;
begin
  // Prepare bPath & Open
  ApplyPath( KeyLocation );
  CreateReg( TRegistryNeed.rnComplete, GetPathItem(KeyLocation) );

  // Create Key
  Result := false;
  try
    FRegistry.MoveKeyTo( GetPathEnd(KeyLocation), NewLocation, AlsoDelete );
    Result := true;
  except
    RaiseError( TRegistryError.reAccessDenied );
  end;
end;

function FXRegistry.CopyKey(KeyLocation, NewLocation: string): boolean;
begin
  Result := MoveKey(KeyLocation, NewLocation, false);
end;

function FXRegistry.GetBooleanValue(KeyLocation, ValueName: string): boolean;
begin
  // Prepare bPath & Open
  ApplyPath( KeyLocation );
  CreateReg( TRegistryNeed.rnRead, KeyLocation );

  // Create Key
  Result := false;
  try
    Result := FRegistry.ReadBool(ValueName);
  except
    RaiseError( TRegistryError.reReadError );
  end;
end;

function FXRegistry.GetCurrencyValue(KeyLocation, ValueName: string): currency;
begin
  // Prepare bPath & Open
  ApplyPath( KeyLocation );
  CreateReg( TRegistryNeed.rnRead, KeyLocation );

  // Create Key
  Result := 0;
  try
    Result := FRegistry.ReadCurrency(ValueName);
  except
    RaiseError( TRegistryError.reReadError );
  end;
end;

function FXRegistry.GetIntValue(KeyLocation, ValueName: string): integer;
begin
  // Prepare bPath & Open
  ApplyPath( KeyLocation );
  CreateReg( TRegistryNeed.rnRead, KeyLocation );

  // Create Key
  Result := 0;
  try
    Result := FRegistry.ReadInteger(ValueName);
  except
    RaiseError( TRegistryError.reReadError );
  end;
end;

function FXRegistry.GetKeyNames(KeyLocation: string): TStringList;
begin
  // Prepare bPath & Open
  ApplyPath( KeyLocation );
  CreateReg( TRegistryNeed.rnRead, KeyLocation );

  // Create Key
  Result := TStringList.Create;
  try
    FRegistry.GetKeyNames( Result );
  except
    RaiseError( TRegistryError.reReadError );
  end;
end;

procedure FXRegistry.CreateReg(AType: TRegistryNeed; APosition: string);
var
  Access: Cardinal;
begin
  // Select Type
  case AType of
    rnRead: Access := ApplyRegMode(KEY_READ);
    rnWrite: Access := ApplyRegMode(KEY_WRITE);
    rnComplete: Access := ApplyRegMode(KEY_ALL_ACCESS);
    else Access := ApplyRegMode(KEY_ALL_ACCESS);
  end;

  // Create
  FRegistry := TRegistry.Create( Access );

  // Open Hive
  FRegistry.RootKey := FHive;

  // Position
  if APosition <> '' then
    FRegistry.OpenKey( APosition, false );
end;

function FXRegistry.GetPathEnd(Path: string): string;
begin
  Result := ExtractFileName( ExcludeTrailingPathDelimiter( Path ) );
end;

function FXRegistry.GetPathItem(Path: string): string;
begin
  Result := ExtractFileDir( ExcludeTrailingPathDelimiter( Path ) );
end;

procedure FXRegistry.ApplyPath(var Path: string);
label
  FoundItem, ExitSearch;
var
  StrRoot: string;
begin
  // Prepare
  Path := IncludeTrailingPathDelimiter( Path );

  if Pos(COMPUTER_BEGIN, Path) = 1 then
    RemovePathLevels( Path, 1 );

  // Extract Hive
  FHive := FDefaultHive;

  if FAutoHive then
    begin
      StrRoot := Copy( Path, 1, Pos(KEY_SEPAR, Path) - 1 );

      // Cases
      if StrRoot = 'HKEY_CLASSES_ROOT' then
        begin
          FHive := HKEY_CLASSES_ROOT;
          goto FoundItem;
        end;

      if StrRoot = 'HKEY_CURRENT_USER' then
        begin
          FHive := HKEY_CURRENT_USER;
          goto FoundItem;
        end;

      if StrRoot = 'HKEY_LOCAL_MACHINE' then
        begin
          FHive := HKEY_LOCAL_MACHINE;
          goto FoundItem;
        end;

      if StrRoot = 'HKEY_USERS' then
        begin
          FHive := HKEY_USERS;
          goto FoundItem;
        end;

      if StrRoot = 'HKEY_CURRENT_CONFIG' then
        begin
          FHive := HKEY_CURRENT_CONFIG;
          goto FoundItem;
        end;

      // Exit Portal
      goto ExitSearch;

      // Found
      FoundItem:
        begin
          RemovePathLevels( Path, 1 );
        end;

      // Exit
      ExitSearch:
    end;
end;

procedure FXRegistry.RemovePathLevels(var Path: string; Levels: integer);
var
  P: integer;
  I: Integer;
begin
  for I := 1 to Levels do
    begin
      P := Pos( KEY_SEPAR, Path );

      if P <> 0 then
        Path := Copy( Path, P + 1, Length(Path) );
    end;
end;

procedure FXRegistry.RaiseError(Error: TRegistryError);
begin
  case Error of
    reAccessDenied: ShowMessage( 'Windows Registry Error: Access Denied' );
    reKeyNoExist: ShowMessage( 'Windows Registry Error: The specified key does not exist' );
    reReadError: ShowMessage( 'Windows Registry Error: Cannot read from the Windows Registry' );
  end;
end;

procedure FXRegistry.WriteStringValue(KeyLocation, ItemName: string; Value: string);
begin
  // Prepare bPath & Open
  ApplyPath( KeyLocation );
  CreateReg( TRegistryNeed.rnWrite, KeyLocation );

  // Create Key
  try
    FRegistry.WriteString(ItemName, Value);
  except
    RaiseError( TRegistryError.reAccessDenied );
  end;
end;

procedure FXRegistry.WriteTime(KeyLocation, ItemName: string; Value: TTime);
begin
  // Prepare bPath & Open
  ApplyPath( KeyLocation );
  CreateReg( TRegistryNeed.rnWrite, KeyLocation );

  // Create Key
  try
    FRegistry.WriteTime(ItemName, Value);
  except
    RaiseError( TRegistryError.reAccessDenied );
  end;
end;

procedure FXRegistry.WriteValue(KeyLocation, ItemName: string; Value: boolean);
begin
  WriteBooleanValue(KeyLocation, ItemName, Value);
end;

procedure FXRegistry.WriteValue(KeyLocation, ItemName: string; Value: integer);
begin
  WriteIntValue(KeyLocation, ItemName, Value);
end;

procedure FXRegistry.WriteValue(KeyLocation, ItemName, Value: string);
begin
  WriteStringValue(KeyLocation, ItemName, Value);
end;

procedure FXRegistry.WriteValue(KeyLocation, ItemName: string; Value: double);
begin
  WriteFloatValue(KeyLocation, ItemName, Value);
end;

procedure FXRegistry.WriteValue(KeyLocation, ItemName: string; Value: TTime);
begin
  WriteTime(KeyLocation, ItemName, Value);
end;

procedure FXRegistry.WriteValue(KeyLocation, ItemName: string; Value: TDate);
begin
  WriteDate(KeyLocation, ItemName, Value);
end;

procedure FXRegistry.WriteValue(KeyLocation, ItemName: string;
  Value: TDateTime);
begin
  WriteDateTimeValue(KeyLocation, ItemName, Value);
end;

procedure FXRegistry.WriteBooleanValue(KeyLocation, ItemName: string; Value: boolean);
begin
  // Prepare bPath & Open
  ApplyPath( KeyLocation );
  CreateReg( TRegistryNeed.rnWrite, KeyLocation );

  // Create Key
  try
    FRegistry.WriteBool(ItemName, Value);
  except
    RaiseError( TRegistryError.reAccessDenied );
  end;
end;

procedure FXRegistry.WriteCurrency(KeyLocation, ItemName: string; Value: Currency);
begin
  // Prepare bPath & Open
  ApplyPath( KeyLocation );
  CreateReg( TRegistryNeed.rnWrite, KeyLocation );

  // Create Key
  try
    FRegistry.WriteCurrency(ItemName, Value);
  except
    RaiseError( TRegistryError.reAccessDenied );
  end;
end;

procedure FXRegistry.WriteDate(KeyLocation, ItemName: string; Value: TDate);
begin
  // Prepare bPath & Open
  ApplyPath( KeyLocation );
  CreateReg( TRegistryNeed.rnWrite, KeyLocation );

  // Create Key
  try
    FRegistry.WriteDate(ItemName, Value);
  except
    RaiseError( TRegistryError.reAccessDenied );
  end;
end;

procedure FXRegistry.WriteDateTimeValue(KeyLocation, ItemName: string; Value: TDateTime);
begin
  // Prepare bPath & Open
  ApplyPath( KeyLocation );
  CreateReg( TRegistryNeed.rnWrite, KeyLocation );

  // Create Key
  try
    FRegistry.WriteDateTime(ItemName, Value);
  except
    RaiseError( TRegistryError.reAccessDenied );
  end;
end;

procedure FXRegistry.WriteFloatValue(KeyLocation, ItemName: string; Value: double);
begin
  // Prepare bPath & Open
  ApplyPath( KeyLocation );
  CreateReg( TRegistryNeed.rnWrite, KeyLocation );

  // Create Key
  try
    FRegistry.WriteFloat(ItemName, Value);
  except
    RaiseError( TRegistryError.reAccessDenied );
  end;
end;

procedure FXRegistry.WriteIntValue(KeyLocation, ItemName: string; Value: integer);
begin
  // Prepare bPath & Open
  ApplyPath( KeyLocation );
  CreateReg( TRegistryNeed.rnWrite, KeyLocation );

  // Create Key
  try
    FRegistry.WriteInteger(ItemName, Value);
  except
    RaiseError( TRegistryError.reAccessDenied );
  end;
end;

{ TRegHelper }
function TRegHelper.ReadCardinal(const Name: string): Cardinal;
var
  Int: integer;
begin
  Int := ReadInteger(Name);
  if Int < 0 then
    begin
      (* Substract negative value  *)
      Result := Abs(Int + 1);
      Result := Cardinal.MaxValue - Result;
    end
  else
    Result := Int;
 end;

procedure TRegHelper.RenameKey(const OldName, NewName: string);
begin
  Self.MoveKey(OldName, NewName, true);
end;

function TRegHelper.CloneKey(const KeyName: string): string;
var
  CloneNumber: integer;
begin
  // Get Clone Index
  CloneNumber := 1;
  repeat
    inc(CloneNumber);
  until not Self.KeyExists(KeyName + '(' + inttostr( CloneNumber ) + ')');

  // New name
  Result := KeyName + '(' + inttostr( CloneNumber ) + ')';

  // Copy
  MoveKey(KeyName, Result, false);
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
    RegistryOld.OpenKey( IncludeTrailingPathDelimiter( IncludeTrailingPathDelimiter( Self.CurrentPath ) + OldName ), false );
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

{ TQuickReg }

class function FXQuickReg.CreateKey(KeyLocation: string): boolean;
var
  Registry: FXRegistry;
begin
  Registry := FXRegistry.Create;
  try
    Result := Registry.CreateKey(KeyLocation);
  finally
    Registry.Free;
  end;
end;

class function FXQuickReg.DeleteKey(KeyLocation: string): boolean;
var
  Registry: FXRegistry;
begin
  Registry := FXRegistry.Create;
  try
    Result := Registry.DeleteKey(KeyLocation);
  finally
    Registry.Free;
  end;
end;

class function FXQuickReg.DeleteValue(KeyLocation, ValueName: string): boolean;
var
  Registry: FXRegistry;
begin
  Registry := FXRegistry.Create;
  try
    Result := Registry.DeleteValue(KeyLocation, ValueName);
  finally
    Registry.Free;
  end;
end;

class function FXQuickReg.GetStringValue(KeyLocation, ValueName: string): string;
var
  Registry: FXRegistry;
begin
  Registry := FXRegistry.Create;
  try
    Result := Registry.GetStringValue(KeyLocation, ValueName);
  finally
    Registry.Free;
  end;
end;

class function FXQuickReg.GetValueExists(KeyLocation,
  ValueName: string): boolean;
var
  Registry: FXRegistry;
begin
  Registry := FXRegistry.Create;
  try
    Result := Registry.GetValueExists(KeyLocation, ValueName);
  finally
    Registry.Free;
  end;
end;

class function FXQuickReg.KeyExists(KeyLocation: string): boolean;
var
  Registry: FXRegistry;
begin
  Registry := FXRegistry.Create;
  try
    Result := Registry.KeyExists(KeyLocation);
  finally
    Registry.Free;
  end;
end;

class function FXQuickReg.RenameKey(KeyLocation, NewName: string): boolean;
var
  Registry: FXRegistry;
begin
  Registry := FXRegistry.Create;
  try
    Result := Registry.RenameKey(KeyLocation, NewName);
  finally
    Registry.Free;
  end;
end;

end.
