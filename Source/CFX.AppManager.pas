unit CFX.AppManager;

{$TYPEINFO ON}

interface
uses
  Winapi.Windows,
  Winapi.Messages,
  Classes,
  Vcl.Forms,
  CFX.Instances,
  System.SysUtils,
  System.UITypes,
  Types,
  Math,
  DateUtils,
  Vcl.Graphics,
  CFX.ThemeManager,
  CFX.Colors,
  CFX.Files,
  CFX.Version,
  CFX.StringUtils,
  CFX.Registry,
  ShellAPI,
  CFX.QuickDialogs,
  CFX.Utilities,
  CFX.Constants,
  CFX.Types,
  Vcl.Controls,
  IniFiles,
  CFX.AppIntegration,
  IOUTils;

type
  { Background App Manager Class }
  FXAppManagerClass = class(TObject)
  private
    // Constant Folders
    FAppDataPath,
    FAppPackagesPath: string;

    // Dynamic data
    FAppSelfFolder: string;
    FAppConfig: string;
    FAppWindows: string;

    // Variabiles
    FAppIdentifier: string;
    FApplicationName: string;
    FAppVersion: FXVersion;
    FServerVersion: FXVersion;
    FAPIName,
    FAPiEndpoint: string;

    FUpdateResult: TValueRelationship;
    FUpdateCheckSuccess: boolean;
    FCheckingUpdates: boolean;
    FLastUpdateCheck: TDate;

    // Procs
    function GetConfig: TIniFile;
    function GetWindow: TIniFile;

    procedure UpdateFolders;
    procedure VerifyFolders;

    // Setters
    procedure SetIdentifier(const Value: string);

  public
    // Procedures
    procedure CheckForUpdates;

    // Settings
    procedure SaveSettings;
    procedure LoadSettings;

    // Form settings
    procedure SaveFormData(Form: TForm; Closing: boolean = false);
    procedure LoadFormData(Form: TForm);

    // Properties
    property CheckingForUpdates: boolean read FCheckingUpdates;
    property LastUpdateCheck: TDate read FLastUpdateCheck;
    property AppIdentifier: string read FAppIdentifier write SetIdentifier;
    property ApplicationName: string read FApplicationName write FApplicationName;
    property AppData: string read FAppDataPath;
    property AppPackages: string read FAppPackagesPath;
    property AppVersion: FXVersion read FAppVersion write FAppVersion;
    property UpdateCheckSuccess: boolean read FUpdateCheckSuccess;
    property UpdateCheckResult: TValueRelationship read FUpdateResult;
    property ServerVersion: FXVersion read FServerVersion write FServerVersion;
    property APIName: string read FAPIName write FAPIName;
    property APiEndpoint: string read FAPiEndpoint write FAPiEndpoint;

    function NewVersion: boolean;

    // Utils
    procedure RestartApplicationProcess;

    // Constructors
    constructor Create;
    destructor Destroy; override;
  end;

  { App Manager Component - Use on Main Form }
  FXAppManager = class(TComponent)
  private
    const
      DEFAULT_TASKS = [FXAppTask.WindowLoadForm, FXAppTask.WindowSaveForm];
      DEFAULT_USER_UPDATE_DELAY = 2000;
    var
    MainForm: TForm;

    // Main form prompts
    FormPrompt: TForm;

    // Props
    FApplicationIdentifier: string;
    FUpdateCheckInterval: integer;
    FAppVersion: FXVersion;
    FAPIName: string;
    FAPIEndpoint: string;
    FHasAppData: boolean;
    FSingleInstance: boolean;
    FTasks: FXAppTasks;
    FAppDataStructure: TStringList;

    FUpdateCheckUserInitiated: boolean;
    FOnUpdateChecked: TNotifyEvent;
    FOnUpdateStartCheck: TNotifyEvent;
    FOnApplicationLoaded: TNotifyEvent;
    FOnOtherInstance: FXOnOtherInstance;
    FUserUpdateWaitDelay: cardinal;

    FApplicationName: string;
    FAppDataCompany: TCaption;

    // Background
    procedure AppCheckUpdates;

    // Stored
    function IsAppDataStored: Boolean;
    function IsEndpointStored: Boolean;

    // Getters
    function GetAppData: string;
    function GetVersion: string;

    // Setters
    procedure SetApplicationIdentifier(Value: string);
    procedure SetAPIEndpoint(const Value: string);
    procedure SetVersion(const Value: string);
    procedure SetHasAppData(const Value: boolean);
    procedure SetApplicationName(const Value: string);
    procedure SetDataStructure(const Value: TStringList);

  protected
    // Loaded
    procedure Loaded; override;
    procedure ApplySettings;

  published
    property ApplicationIdentifier: string read FApplicationIdentifier write SetApplicationIdentifier;
    // The update checking interval (days)
    property UpdateCheckInterval: integer read FUpdateCheckInterval write FUpdateCheckInterval default -1;
    property AppVersion: string read GetVersion write SetVersion;
    property APIName: string read FAPIName write FAPIName;
    property APIEndpoint: string read FAPIEndpoint write SetAPIEndpoint stored IsEndpointStored;

    property SingleInstance: boolean read FSingleInstance write FSingleInstance default false;

    property ApplicationName: string read FApplicationName write SetApplicationName;
    property HasAppData: boolean read FHasAppData write SetHasAppData default false;
    property AppDataCompany: TCaption read FAppDataCompany write FAppDataCompany stored IsAppDataStored;

    property UpdateCheckUserInitiated: boolean read FUpdateCheckUserInitiated;
    property OnUpdateChecked: TNotifyEvent read FOnUpdateChecked write FOnUpdateChecked;
    property OnUpdateStartCheck: TNotifyEvent read FOnUpdateStartCheck write FOnUpdateStartCheck;
    property OnApplicationLoaded: TNotifyEvent read FOnApplicationLoaded write FOnApplicationLoaded;
    property OnOtherInstance: FXOnOtherInstance read FOnOtherInstance write FOnOtherInstance;

    property AutomaticTasks: FXAppTasks read FTasks write FTasks default DEFAULT_TASKS;
    property AppDataStructure: TStringList read FAppDataStructure write SetDataStructure;

    property AppData: string read GetAppData;

    property UserUpdateWaitDelay: cardinal read FUserUpdateWaitDelay write FUserUpdateWaitDelay;
    function LastUpdateCheck: string;

  public
    // Application
    procedure ApplicationOpen;
    procedure ApplicationClose;

    // Executed for Main Form
    procedure FormClosing;
    procedure FormOpening;

    // Utils
    procedure InitiateUserUpdateCheck;

    // Constructors
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  // App manager instances
  AppManager: FXAppManagerClass;
  AppMgrCount: integer;

  // App manager placed on form
  AppManagerInstance: FXAppManager;

implementation

{ FXAppManager }

procedure FXAppManager.AppCheckUpdates;
begin
  if AppManager.CheckingForUpdates then
    Exit;

  // Notify
  if Assigned(FOnUpdateStartCheck) then
    FOnUpdateStartCheck(Self);

  // Server
  AppManager.CheckForUpdates;

  TThread.CreateAnonymousThread(procedure
  begin
    // Wait
    while AppManager.CheckingForUpdates do
      Sleep(100);

    TThread.Synchronize(nil, procedure
      begin
        // Notify
        if Assigned(FOnUpdateChecked) then
          FOnUpdateChecked(Self);

        // Auto handle
        if FXAppTask.UpdatePrompt in FTasks then
          PromptUpdate(Owner as TForm, FXAppTask.UpdateForce in FTasks);

        // User notify screen (close) (after auto handle, if It's the case)
        if FXAppTask.UpdateShowUserScreen in FTasks then
          PromptUpdateUserClose(FormPrompt);
      end);
  end).Start;
end;

procedure FXAppManager.ApplicationClose;
begin
  // Form is closed
  if FXAppTask.WindowSaveForm in FTasks then
    FormClosing;
  AppManager.SaveSettings;
end;

procedure FXAppManager.ApplicationOpen;
begin
  // Other instance
  if FSingleInstance then
    begin
      SetSemafor( FApplicationIdentifier );

      var Action: FXOtherInstanceAction;
      if HasOtherInstances then
        begin
          Action := FXOtherInstanceAction.Close;

          if Assigned(OnOtherInstance) then
            OnOtherInstance(Self, Action);

          case Action of
            FXOtherInstanceAction.Close: Application.Terminate; // force stop
            FXOtherInstanceAction.KillProcess: Halt;
          end;
        end;
    end;

  // Form is created
  if FXAppTask.WindowLoadForm in FTasks then
    FormOpening;
  AppManager.LoadSettings;

  // Initialise directory structure
  if FAppDataStructure.Count > 0 then
    begin
      const BaseDir = AppData;
      var Path: string;
      for var I := 0 to FAppDataStructure.Count-1 do
        begin
          Path := BaseDir + FAppDataStructure[I];
          if not TDirectory.Exists(Path) then
            TDirectory.CreateDirectory(Path);
        end;
    end;

  // Automatic Update Check
  if (UpdateCheckInterval <> -1) then
    begin
      const ADays = DaysBetween(AppManager.LastUpdateCheck, Now) ;

      if (ADays >= UpdateCheckInterval) then begin
        FUpdateCheckUserInitiated := false;
        AppCheckUpdates;
      end;
    end;
end;

procedure FXAppManager.ApplySettings;
begin
  AppManager.FAPIEndpoint := FAPIEndpoint;
  AppManager.FAPIName := FAPIName;
  AppManager.FAppVersion := FAppVersion;
end;

constructor FXAppManager.Create(AOwner: TComponent);
begin
  // Count
  Inc(AppMgrCount);

  // Is Form
  if (AOwner = nil) or not (AOwner is TForm) then
    begin
      OpenMessage('Application Manager GUI',
        'The Application manager must have the form as a Form.');
      Abort;
    end;

  // Check
  if AppMgrCount > 1 then
    begin
      OpenMessage('Application Manager GUI',
        'Hey! The Application may only have one App Manager.'#13#13'Please place It on the main form.');
      Abort;
    end;

  // Primary instance
  AppManagerInstance := Self;

  // Type
  MainForm := AOwner as TForm;

  // Data
  ApplicationIdentifier := GenerateString(20, true, false, true, false);
  FUpdateCheckInterval := -1;
  FAPIEndpoint := DEFAULT_API;
  FAppVersion.Parse('1.0.0.0');
  FHasAppData := false;
  FAppDataCompany := DEFAULT_COMPANY;
  FTasks := DEFAULT_TASKS;
  FAppDataStructure := TStringList.Create;
  FUserUpdateWaitDelay := DEFAULT_USER_UPDATE_DELAY;

  inherited;
end;

destructor FXAppManager.Destroy;
begin
  // Count
  Dec(AppMgrCount);
  AppMgrCount := Max(AppMgrCount, 0);
  FAppDataStructure.Free;

  // Close
  if not IsDesigning then
    ApplicationClose;

  inherited;
end;

procedure FXAppManager.FormClosing;
begin
  // Save
  AppManager.SaveFormData(MainForm, true);
end;

procedure FXAppManager.FormOpening;
begin
  // Open
  AppManager.LoadFormData(MainForm);
end;

function FXAppManager.GetAppData: string;
begin
  Result := AppManager.AppData;

  if FAppDataCompany <> '' then
    Result := Format('%S%S\', [Result, FAppDataCompany]);

  if ApplicationName <> '' then
    Result := Format('%S%S\', [Result, ApplicationName]);
end;

function FXAppManager.GetVersion: string;
begin
  Result := FAppVersion.ToString(true);
end;

procedure FXAppManager.InitiateUserUpdateCheck;
begin
  // Boolean status
  FUpdateCheckUserInitiated := true;

  // User notify screen
  if FXAppTask.UpdateShowUserScreen in FTasks then
    PromptUpdateUser(Owner as TForm, FormPrompt);

  // Start
  AppCheckUpdates;
end;

function FXAppManager.IsAppDataStored: Boolean;
begin
  Result := FAppDataCompany <> DEFAULT_COMPANY;
end;

function FXAppManager.IsEndpointStored: Boolean;
begin
  Result := FAPIEndpoint <> DEFAULT_API;
end;

function FXAppManager.LastUpdateCheck: string;
begin
  Result := datetostr(AppManager.LastUpdateCheck);
end;

procedure FXAppManager.Loaded;
begin
  inherited;
  // Settings loaded
  if IsDesigning then
    Exit;

  // Apply
  ApplySettings;

  // Opened
  ApplicationOpen;

  // Notify
  if Assigned(FOnApplicationLoaded) then
    FOnApplicationLoaded(Self);
end;

procedure FXAppManager.SetAPIEndpoint(const Value: string);
begin
  if FAPIEndpoint = Value then
    Exit;

  FAPIEndpoint := Value;
  AppManager.FApiEndpoint := Value;
end;

procedure FXAppManager.SetApplicationIdentifier(Value: string);
begin
  Value := LowerCase(Value);
  if (FApplicationIdentifier = Value) or (Value = '') then
    Exit;

  FApplicationIdentifier := Value;
  AppManager.AppIdentifier := ApplicationIdentifier;
end;

procedure FXAppManager.SetApplicationName(const Value: string);
begin
  if FApplicationName = Value then
    Exit;

  AppManager.ApplicationName := Value;
  FApplicationName := Value;
end;

procedure FXAppManager.SetDataStructure(const Value: TStringList);
begin
  FAppDataStructure.Assign( Value );
end;

procedure FXAppManager.SetHasAppData(const Value: boolean);
begin
  FHasAppData := Value;

  if Value and not IsDesigning then
    if not TDirectory.Exists(AppData) then
      TDirectory.CreateDirectory(AppData);
end;

procedure FXAppManager.SetVersion(const Value: string);
begin
  FAppVersion.Parse(Value);
end;

{ FXAppManagerClass }

procedure FXAppManagerClass.CheckForUpdates;
begin
  if FCheckingUpdates then
    Exit;

  // Status
  FCheckingUpdates := true;

  TThread.CreateAnonymousThread(procedure
  begin
    try
      FServerVersion.APILoad(FApiName, FAppVersion, FApiEndpoint);
      FUpdateResult := FServerVersion.CompareTo(FAppVersion);
      FUpdateCheckSuccess := true;
    except
      FUpdateResult := EqualsValue;
      FUpdateCheckSuccess := false;
    end;

    // Status
    FLastUpdateCheck := now;
    FCheckingUpdates := false;
  end).Start;
end;

constructor FXAppManagerClass.Create;
begin
  FAppDataPath := GetAppDataFolder;
  FAppPackagesPath := GetPackagesFolder;
  FAppIdentifier := '';
  FAPIEndpoint := DEFAULT_API;
  FAppVersion.Parse('1.0.0.0');
  FUpdateResult := EqualsValue;

  // Status
  FLastUpdateCheck := 0;


  if not TDirectory.Exists(FAppPackagesPath) then
    TDirectory.CreateDirectory(FAppPackagesPath);

  // Folders
  UpdateFolders;
end;

destructor FXAppManagerClass.Destroy;
begin

  inherited;
end;

function FXAppManagerClass.GetConfig: TIniFile;
begin
  VerifyFolders;

  Result := TIniFile.Create(FAppConfig);
end;

function FXAppManagerClass.GetWindow: TIniFile;
begin
  VerifyFolders;

  Result := TIniFile.Create(FAppWindows);
end;

procedure FXAppManagerClass.LoadFormData(Form: TForm);
var
  Category: string;
  AFile: TIniFile;
begin
  if Form = nil then
    Exit;

  // Prep
  Category := Form.Name;

  AFile := GetWindow;
  with AFile do
    try
      // Exists
      if not SectionExists(Category) then
        Exit;

      // Load
      with Form do
        begin
          WindowState := TWindowState.wsNormal;

          Position := poDesigned;

          Left := ReadInteger(Category, 'Left', Left);
          Top := ReadInteger(Category, 'Top', Top);
          Width := ReadInteger(Category, 'Width', Width);
          Height := ReadInteger(Category, 'Height', Height);

          WindowState := TWindowState(ReadInteger(Category, 'State', integer(WindowState)));
          if WindowState = wsMinimized then
            WindowState := wsNormal;
        end;
    finally
      Free;
    end;
end;

procedure FXAppManagerClass.LoadSettings;
var
  Section: string;
begin
  with GetConfig do
    try
      Section := 'Passive';
      FLastUpdateCheck := ReadDate(Section, 'Last update', LastUpdateCheck);
    finally
      Free;
    end;
end;

function FXAppManagerClass.NewVersion: boolean;
begin
  Result := FUpdateResult = GreaterThanValue;
end;

procedure FXAppManagerClass.RestartApplicationProcess;
begin
  Application.MainForm.Close;

  // Start
  ShellRun( ParamStr(0), '');
end;

procedure FXAppManagerClass.SaveFormData(Form: TForm; Closing: boolean);
var
  Category: string;
  AFile: TIniFile;

  // Previous
  PrevState: TWindowState;
  PrevValue: byte;
  PrevEn: boolean;
begin
  if Form = nil then
    Exit;

  // Prep
  Category := Form.Name;

  AFile := GetWindow;
  with AFile do
    try
      with Form do
        begin
          PrevEn := false;
          PrevValue := 255;

          WriteInteger(Category, 'State', integer(WindowState));
          if WindowState = wsMinimized then
            begin
              PrevEn := AlphaBlend;
              PrevValue := AlphaBlendValue;

              AlphaBlend := true;
              AlphaBlendValue := 0;
            end;
          PrevState := WindowState;
          WindowState := TWindowState.wsNormal;

          WriteInteger(Category, 'Left', Left);
          WriteInteger(Category, 'Top', Top);
          WriteInteger(Category, 'Width', Width);
          WriteInteger(Category, 'Height', Height);

          // Revert
          if not Closing then
            begin
              WindowState := PrevState;
              if WindowState = wsMinimized then
                begin
                  AlphaBlend := PrevEn;
                  AlphaBlendValue := PrevValue;
                end;
              end;
        end;
    finally
      Free;
    end;
end;

procedure FXAppManagerClass.SaveSettings;
var
  Section: string;
begin
  with GetConfig do
    try
      Section := 'Passive';
      WriteDate(Section, 'Last update', LastUpdateCheck);
    finally
      Free;
    end;
end;

procedure FXAppManagerClass.SetIdentifier(const Value: string);
begin
  if FAppIdentifier = Value then
    Exit;

  FAppIdentifier := Value;
  UpdateFolders;
end;

procedure FXAppManagerClass.UpdateFolders;
begin
  FAppSelfFolder := Format('%S%S\', [FAppPackagesPath, FAppIdentifier]);

  FAppConfig := FAppSelfFolder + 'configurations.ini';
  FAppWindows := FAppSelfFolder + 'windows.ini';
end;

procedure FXAppManagerClass.VerifyFolders;
begin
  if FAppIdentifier = '' then
    raise Exception.Create('ERROR: Application identifier has not been initialised.');

  // Check pack
  if not TDirectory.Exists(FAppSelfFolder) then
    TDirectory.CreateDirectory(FAppSelfFolder);
end;

initialization
  AppMgrCount := 0;
  AppManager := FXAppManagerClass.Create;

finalization
  AppManager.Free;
end.
