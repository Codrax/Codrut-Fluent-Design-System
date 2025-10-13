unit CFX.AppManager;

{$TYPEINFO ON}
{$SCOPEDENUMS ON}

interface
uses
  Winapi.Windows,
  Winapi.Messages,
  Classes,
  Vcl.Forms,
  Vcl.Dialogs,
  CFX.Instances,
  System.SysUtils,
  System.UITypes,
  Types,
  Math,
  DateUtils,
  Vcl.Graphics,
  CFX.Components,
  CFX.ThemeManager,
  CFX.Colors,
  CFX.Files,
  CFX.Version,
  CFX.StringUtils,
  CFX.Registry,
  ShellAPI,
  CFX.QuickDialogs,
  CFX.ArrayHelpers,
  CFX.Utilities,
  CFX.Constants,
  CFX.Types,
  Vcl.Controls,
  IniFiles,
  CFX.AppIntegration,
  IOUTils;

type
  FXOnVersionChanged = procedure(PreviousVersion: FXVersion; ActiveVersion: FXVersion) of object;

  { Background App Manager Class }
  FXAppManagerClass = class(TObject)
  strict private
    // Constant Folders
    FFolderGlobalAppData: string;

    // Dynamic data
    FFolderPackage: string;
    FFolderData: string;

    // Manager
    FFlags: TAppManagerFlags;

    // Variabiles
    FAppIdentifier: string;
    FApplicationName: string;
    FPublisherName: string;
    FAppVersion: FXVersion;
    FServerVersion: FXVersion;
    FAPIName,
    FAPiEndpoint: string;

    // Update
    FUpdateResult: TValueRelationship;
    FUpdateCheckSuccess: boolean;
    FCheckingUpdates: boolean;
    FLastUpdateCheck: TDate;
    FLastInstalledVersion: FXVersion;

    // Init
    FInitialized: boolean;

    // Procs
    function GetConfig: TIniFile;
    function GetWindow: TIniFile;

    // Init
    procedure RaiseInit;
    procedure RaiseNotInit;

    // Setters
    procedure SetIdentifier(const Value: string);
    procedure SetPublisherName(const Value: string);
    procedure SetApplicationName(const Value: string);

    // Getters
    function GetFolderData: string;
    function GetFolderPackage: string;
    procedure SetFlags(const Value: TAppManagerFlags);

  public
    // Procedures
    procedure CheckForUpdates;

    // Settings
    procedure SaveSettings;
    procedure LoadSettings;

    // Form settings
    procedure SaveFormData(Form: TForm);
    procedure LoadFormData(Form: TForm);

    // Manager
    property Flags: TAppManagerFlags read FFlags write SetFlags;

    // Info
    property AppIdentifier: string read FAppIdentifier write SetIdentifier;
    property ApplicationName: string read FApplicationName write SetApplicationName;
    property PublisherName: string read FPublisherName write SetPublisherName;
    property AppVersion: FXVersion read FAppVersion write FAppVersion;

    // Data
    property FolderGlobalAppData: string read FFolderGlobalAppData;
    property FolderData: string read GetFolderData;
    property FolderPackage: string read GetFolderPackage;

    // Update
    property CheckingForUpdates: boolean read FCheckingUpdates;
    property LastUpdateCheck: TDate read FLastUpdateCheck;
    property LastInstalledVersion: FXVersion read FLastInstalledVersion;
    property UpdateCheckSuccess: boolean read FUpdateCheckSuccess;
    property UpdateCheckResult: TValueRelationship read FUpdateResult;
    property ServerVersion: FXVersion read FServerVersion write FServerVersion;
    property APIName: string read FAPIName write FAPIName;
    property APIEndpoint: string read FAPiEndpoint write FAPiEndpoint;

    // Status
    function NewVersion: boolean;

    // Data
    procedure Initialize;
    property Initializeed: boolean read FInitialized;

    // Utils
    procedure RestartApplicationProcess;

    // Constructors
    constructor Create;
    destructor Destroy; override;
  end;

  { App Manager Component }
  FXAppManagerComponent = class(FXComponent)
  private
    FPrimaryDisplayForm: boolean;
    FParentForm: TForm;

    // Getters
    function GetAppData: string;

    // Setters
    procedure SetPrimaryDisplayForm(const Value: boolean);

  protected
    // Loaded
    procedure Loaded; override;

  published
    // Props
    property PrimaryDisplayForm: boolean read FPrimaryDisplayForm write SetPrimaryDisplayForm default false;

  public
    property ParentForm: TForm read FParentForm;

    // Func
    procedure FormClosing;
    procedure FormOpening;

    // Props
    property AppData: string read GetAppData;

    // Constructors
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { App Manager - Use on Main Form }
  FXAppManager = class(FXAppManagerComponent)
  private
    const
      DEFAULT_TASKS = [FXAppTask.WindowLoadForm, FXAppTask.WindowSaveForm];
      DEFAULT_USER_UPDATE_DELAY = 2000;
    procedure SetAPIName(const Value: string);
    procedure SetUpdateCheckInterval(const Value: integer);
    function GetHasAppData: boolean;
    type TPendingDisplayAction = (UpdatePrompt);
    type TUnEditableBoolean = type boolean;
    var
    // Active propr
    FormPrompt: TForm;

    // Props
    FUpdateCheckInterval: integer;
    FSingleInstance: boolean;
    FTasks: FXAppTasks;
    FSettings: FXAppSettings;
    FAppDataStructure: TStringList;

    FPendingDisplayActions: TArray<TPendingDisplayAction>;

    FUpdateCheckUserInitiated: boolean;
    FOnUpdateChecked: TNotifyEvent;
    FOnUpdateStartCheck: TNotifyEvent;
    FOnApplicationLoaded: TNotifyEvent;
    FOnOtherInstance: FXOnOtherInstance;
    FOnVersionUpgraded: FXOnVersionChanged;
    FOnVersionDowngraded: FXOnVersionChanged;
    FUserUpdateWaitDelay: cardinal;

    // Manager design fixes (Delphi is dumb sometimes)
    FIdentifierValueSet: boolean;
    FPublisherValueSet: boolean;

    // Background
    procedure AppCheckUpdates;

    // Stored
    function IsEndpointStored: Boolean;

    // Utils
    function HasPrimaryDisplayForm: boolean;
    function GetPrimaryDisplayForm: TForm;

    // Update
    procedure CheckUpdateCheckInterval;

    // Settings (for app manager)
    procedure SaveSettings;
    procedure LoadSettings;

    function GetApplicationIdentifier: string;
    function GetApplicationName: string;
    function GetPublisherName: TCaption;
    function GetVersion: string;

    function GetAPIEndpoint: string;
    function GetAPIName: string;

    // Setters
    procedure SetHasAppData(const Value: boolean);

    procedure SetApplicationIdentifier(Value: string);
    procedure SetApplicationName(const Value: string);
    procedure SetPublisherName(const Value: TCaption);
    procedure SetVersion(const Value: string);

    procedure SetDataStructure(const Value: TStringList);
    procedure SetAPIEndpoint(const Value: string);

  protected
    // Loaded
    procedure Loaded; override;

    procedure ProcessDisplayChange;

  published
    property PrimaryDisplayForm default true;

    // Info
    property ApplicationIdentifier: string read GetApplicationIdentifier write SetApplicationIdentifier;
    property ApplicationName: string read GetApplicationName write SetApplicationName;
    property PublisherName: TCaption read GetPublisherName write SetPublisherName;
    property HasAppData: boolean read GetHasAppData write SetHasAppData default false;

    // Update
    property UpdateCheckInterval: integer read FUpdateCheckInterval write SetUpdateCheckInterval default -1; // The update checking interval (days)
    property AppVersion: string read GetVersion write SetVersion;
    property APIName: string read GetAPIName write SetAPIName;
    property APIEndpoint: string read GetAPIEndpoint write SetAPIEndpoint stored IsEndpointStored;

    // Settings
    property SingleInstance: boolean read FSingleInstance write FSingleInstance default false;

    // Events
    property OnUpdateChecked: TNotifyEvent read FOnUpdateChecked write FOnUpdateChecked;
    property OnUpdateStartCheck: TNotifyEvent read FOnUpdateStartCheck write FOnUpdateStartCheck;
    property OnApplicationLoaded: TNotifyEvent read FOnApplicationLoaded write FOnApplicationLoaded;
    property OnOtherInstance: FXOnOtherInstance read FOnOtherInstance write FOnOtherInstance;
    property OnVersionUpgraded: FXOnVersionChanged read FOnVersionUpgraded write FOnVersionUpgraded;
    property OnVersionDowngraded: FXOnVersionChanged read FOnVersionDowngraded write FOnVersionDowngraded;

    // Tasks
    property AutomaticTasks: FXAppTasks read FTasks write FTasks default DEFAULT_TASKS;
    property Settings: FXAppSettings read FSettings write FSettings default [];
    property AppDataStructure: TStringList read FAppDataStructure write SetDataStructure;

    property UserUpdateWaitDelay: cardinal read FUserUpdateWaitDelay write FUserUpdateWaitDelay;
    function LastUpdateCheck: string;

  public
    // Application
    procedure ApplicationOpen;
    procedure ApplicationClose;

    // Utils
    procedure InitiateUserUpdateCheck;

    // Props
    property UpdateCheckUserInitiated: boolean read FUpdateCheckUserInitiated;

    // Constructors
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { App Manager Form - Assist secondary forms }
  FXAppManagerFormAssist = class(FXAppManagerComponent)
  private
    const
    DEFAULT_TASKS = [FXAppFormAssistTask.WindowLoadForm, FXAppFormAssistTask.WindowSaveForm];
    var
    // Props
    FTasks: FXAppFormAssistTasks;

    // Getters
    function GetApplicationName: string;
    function GetPublisherName: TCaption;

  protected
    // Loaded
    procedure Loaded; override;
    procedure Updated; override;


  published
    // Props
    property AutomaticTasks: FXAppFormAssistTasks read FTasks write FTasks default DEFAULT_TASKS;

    // Read
    property ApplicationName: string read GetApplicationName;
    property PublisherName: TCaption read GetPublisherName;

  public
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

var
  DisplayActiveFormStack: TArray<TForm>; // the form that displays prompts

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
        if FXAppTask.UpdatePrompt in FTasks then begin
          FPendingDisplayActions := FPendingDisplayActions + [TPendingDisplayAction.UpdatePrompt];

          // Display
          ProcessDisplayChange;
        end;

        // User notify screen (close) (after auto handle, if It's the case)
        if FXAppTask.UpdateShowUserScreen in FTasks then
          PromptUpdateUserClose(FormPrompt);
      end);
  end).Start;
end;

procedure FXAppManager.ApplicationClose;
begin
  // Save settings
  if AppManager <> nil then
    try
      AppManager.SaveSettings;
    except
    end;

  // Save form manager settings
  try
    SaveSettings;
  except
  end;
end;

procedure FXAppManager.ApplicationOpen;
begin
  // Other instance
  if FSingleInstance and not ((FXAppSetting.PermitOverrideSingleInstance in FSettings) and (HasParameter('override-single-instance'))) then
    begin
      SetSemafor( AppManager.AppIdentifier );

      var Action: FXOtherInstanceAction;
      if HasOtherInstances then
        begin
          Action := FXOtherInstanceAction.Close;

          // On Other instance
          if Assigned(OnOtherInstance) then
            OnOtherInstance(Self, Action);

          case Action of
            FXOtherInstanceAction.Close: Application.Terminate; // force stop
            FXOtherInstanceAction.KillProcess: Halt;
          end;
        end;
    end;

  // Form is created
  if not IsDesigning and (FXAppTask.WindowLoadForm in FTasks) then
    FormOpening;
  AppManager.LoadSettings;

  // Load form manager settings
  LoadSettings;

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

  // Version upgraded
  if AppManager.LastInstalledVersion.NewerThan(AppManager.AppVersion) and Assigned(FOnVersionUpgraded) then
    FOnVersionUpgraded(AppManager.LastInstalledVersion, AppManager.AppVersion);
  if AppManager.LastInstalledVersion.NewerThan(AppManager.AppVersion) and Assigned(FOnVersionUpgraded) then
    FOnVersionDowngraded(AppManager.LastInstalledVersion, AppManager.AppVersion);

  // Automatic Update Check
  CheckUpdateCheckInterval;
end;

procedure FXAppManager.CheckUpdateCheckInterval;
begin
  if (UpdateCheckInterval = -1) then
    Exit;

  // Verify if last update check was longer than <UpdateCheckInterval> days ago
  if (UpdateCheckInterval = 0) or (DaysBetween(AppManager.LastUpdateCheck, Now) >= UpdateCheckInterval) then begin
    FUpdateCheckUserInitiated := false;
    AppCheckUpdates;
  end;
end;

constructor FXAppManager.Create(AOwner: TComponent);
begin
  inherited;

  // Is Form
  if (AOwner = nil) or not (AOwner is TForm) then
    begin
      OpenMessage('Application Manager GUI',
        'The Application manager must have the form as a Form.');
      Abort;
    end;

  // Check
  if AppMgrCount >= 1 then
    begin
      OpenMessage('Application Manager GUI',
        'Hey! The Application may only have one App Manager.'#13#13'Please place It on the main form.');
      Abort;
    end;

  // Primary instance
  Inc(AppMgrCount);
  AppManagerInstance := Self;

  // Data
  PrimaryDisplayForm := true;
  FUpdateCheckInterval := -1;
  FTasks := DEFAULT_TASKS;
  FSettings := [];
  ApplicationIdentifier := GenerateString(10, [TStrGenFlag.LowercaseLetters, TStrGenFlag.Numbers]);
    FIdentifierValueSet := false;
  PublisherName := DEFAULT_COMPANY;
    FPublisherValueSet := false; // delphi moment
  FAppDataStructure := TStringList.Create;
  FUserUpdateWaitDelay := DEFAULT_USER_UPDATE_DELAY;
end;

destructor FXAppManager.Destroy;
begin
  // Count
  AppMgrCount := Max(AppMgrCount-1, 0);
  AppManagerInstance := nil;
  FAppDataStructure.Free;

  // Form is closed
  if not IsDesigning and AppManager.Initializeed and (FXAppTask.WindowSaveForm in FTasks) then
    try
      FormClosing;
    except
    end;

  // Close
  if not IsDesigning and AppManager.Initializeed then
    ApplicationClose;

  inherited;
end;

function FXAppManager.GetAPIEndpoint: string;
begin
  Result := AppManager.APIEndpoint;
end;

function FXAppManager.GetAPIName: string;
begin
  Result := AppManager.APIName;
end;

function FXAppManager.GetApplicationIdentifier: string;
begin
  Result := AppManager.AppIdentifier;
end;

function FXAppManager.GetApplicationName: string;
begin
  Result := AppManager.ApplicationName;
end;

function FXAppManager.GetHasAppData: boolean;
begin
  Result := TAppManagerFlag.WantsAppData in AppManager.Flags;
end;

function FXAppManager.GetPrimaryDisplayForm: TForm;
begin
  Result := nil;
  if not HasPrimaryDisplayForm then
    Exit;

  // Get top-most stack object
  Result := DisplayActiveFormStack[High(DisplayActiveFormStack)];
end;

function FXAppManager.GetPublisherName: TCaption;
begin
  Result := AppManager.PublisherName;
end;

function FXAppManager.GetVersion: string;
begin
  Result := AppManager.AppVersion.ToString;
end;

function FXAppManager.HasPrimaryDisplayForm: boolean;
begin
  Result := Length(DisplayActiveFormStack) > 0;
end;

procedure FXAppManager.InitiateUserUpdateCheck;
begin
  // Boolean status
  FUpdateCheckUserInitiated := true;

  // User notify screen
  if HasPrimaryDisplayForm then
    if FXAppTask.UpdateShowUserScreen in FTasks then begin
      PromptUpdateUser(GetPrimaryDisplayForm, FormPrompt);
    end;

  // Start
  AppCheckUpdates;
end;

function FXAppManager.IsEndpointStored: Boolean;
begin
  Result := AppManager.APIEndpoint <> DEFAULT_API;
end;

function FXAppManager.LastUpdateCheck: string;
begin
  Result := datetostr(AppManager.LastUpdateCheck);
end;

procedure FXAppManager.Loaded;
begin
  inherited;

  if not FIdentifierValueSet then
    ApplicationIdentifier := '';
  if not FPublisherValueSet then
    PublisherName := '';

  // Settings loaded
  if IsDesigning then
    Exit;

  // Initialize app manager
  AppManager.Initialize;

  // Opened
  ApplicationOpen;

  // Notify
  if Assigned(FOnApplicationLoaded) then
    FOnApplicationLoaded(Self);
end;

procedure FXAppManager.LoadSettings;
var
  Section: string;
begin
  with TIniFile.Create(AppManager.FolderPackage + 'formappmgr.ini') do
    try
      Section := 'Update';
      FUpdateCheckInterval := ReadInteger(Section, 'Check interval', FUpdateCheckInterval);
    finally
      Free;
    end;
end;

procedure FXAppManager.ProcessDisplayChange;
begin
  if not HasPrimaryDisplayForm or (Length(FPendingDisplayActions) = 0) then
    Exit;
  const Display = GetPrimaryDisplayForm;
  if not Display.Visible then
    Exit;

  // Process
  while Length(FPendingDisplayActions) > 0 do
    case TArrayUtils<TPendingDisplayAction>.Shift(FPendingDisplayActions) of
      TPendingDisplayAction.UpdatePrompt: begin
        PromptUpdate(Display, FXAppTask.UpdateForce in FTasks);
      end;
    end;
end;

procedure FXAppManager.SaveSettings;
var
  Section: string;
begin
  with TIniFile.Create(AppManager.FolderPackage + 'formappmgr.ini') do
    try
      Section := 'Update';
      WriteInteger(Section, 'Check interval', FUpdateCheckInterval);
    finally
      Free;
    end;
end;

procedure FXAppManager.SetAPIEndpoint(const Value: string);
begin
  AppManager.APIEndpoint := Value;
end;

procedure FXAppManager.SetAPIName(const Value: string);
begin
  AppManager.APIName := Value;
end;

procedure FXAppManager.SetApplicationIdentifier(Value: string);
begin
  AppManager.AppIdentifier := Value;
  FIdentifierValueSet := true;
end;

procedure FXAppManager.SetApplicationName(const Value: string);
begin
  AppManager.ApplicationName := Value;
end;

procedure FXAppManager.SetDataStructure(const Value: TStringList);
begin
  FAppDataStructure.Assign( Value );
end;

procedure FXAppManager.SetHasAppData(const Value: boolean);
begin
  if Value then
    AppManager.Flags := AppManager.Flags + [TAppManagerFlag.WantsAppData]
  else
    AppManager.Flags := AppManager.Flags - [TAppManagerFlag.WantsAppData];
end;

procedure FXAppManager.SetPublisherName(const Value: TCaption);
begin
  AppManager.PublisherName := Value;
  FPublisherValueSet := true;
end;

procedure FXAppManager.SetUpdateCheckInterval(const Value: integer);
begin
  FUpdateCheckInterval := Value;

  // Perform verification
  if not IsDesigning and not IsReading then
    CheckUpdateCheckInterval;
end;

procedure FXAppManager.SetVersion(const Value: string);
begin
  AppManager.AppVersion.Parse(Value);
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
  FFolderGlobalAppData := GetAppDataFolder;

  FAppIdentifier := '';
  FApplicationName := '';
  FPublisherName := '';

  FFlags := [];

  FAPIEndpoint := DEFAULT_API;
  FAppVersion.Parse('1.0.0.0');
  FUpdateResult := EqualsValue;

  // Status
  FInitialized := false;
  FLastUpdateCheck := 0;
end;

destructor FXAppManagerClass.Destroy;
begin

  inherited;
end;

function FXAppManagerClass.GetConfig: TIniFile;
begin
  Result := TIniFile.Create(FFolderPackage + 'configurations.ini');
end;

function FXAppManagerClass.GetFolderData: string;
begin
  RaiseNotInit;
  Result := FFolderData;
end;

function FXAppManagerClass.GetFolderPackage: string;
begin
  RaiseNotInit;
  Result := FFolderPackage;
end;

function FXAppManagerClass.GetWindow: TIniFile;
begin
  Result := TIniFile.Create(FFolderPackage + 'windows.ini');
end;

procedure FXAppManagerClass.Initialize;
begin
  RaiseInit;

  if AppIdentifier = '' then
    raise Exception.Create('Application cannot be initialized. Missing app identifier.');

  // Packages
  FFolderPackage := FFolderGlobalAppData;
  if PublisherName <> '' then
    FFolderPackage := IncludeTrailingPathDelimiter(FFolderPackage + PublisherName);
  FFolderPackage := IncludeTrailingPathDelimiter(
    IncludeTrailingPathDelimiter(FFolderPackage + 'Packages')+AppIdentifier);

  // App data
  FFolderData := '';
  if TAppManagerFlag.WantsAppData in Flags then begin
    FFolderData := FFolderGlobalAppData;
    if PublisherName <> '' then
      FFolderData := IncludeTrailingPathDelimiter(FFolderData + PublisherName);
    if ApplicationName <> '' then
      FFolderData := IncludeTrailingPathDelimiter(FFolderData + ApplicationName)
    else
      FFolderData := IncludeTrailingPathDelimiter(FFolderData + AppIdentifier);
  end;

  // Create
  if not TDirectory.Exists(FFolderPackage) then
    TDirectory.CreateDirectory(FFolderPackage);
  if TAppManagerFlag.WantsAppData in Flags then begin
    if not TDirectory.Exists(FFolderData) then
      TDirectory.CreateDirectory(FFolderData);
  end;

  // Init
  FInitialized := true;
end;

procedure FXAppManagerClass.LoadFormData(Form: TForm);
var
  Category: string;
  WindowRect: TRect;
  {$IFDEF MSWINDOWS}
  P: TWindowPlacement;
  {$ENDIF}
begin
  if Form = nil then
    Exit;

  // Name
  Category := Form.Name;

  with GetWindow do
    try
      // Force poDesigned for form without triggering SetPosition()
      if (Form.Position <> poDesigned) then
        (PCardinal(@(Form.Position)))^ := Cardinal(poDesigned);

      // Load scales
      const ScaleMultiplier = Form.ScaleFactor / ReadFloat(Category, 'Scale', 1);

      // Get rect
      WindowRect := TRect.Create(
        Point(round(ReadInteger(Category, 'Left', Form.Left)*ScaleMultiplier), round(ReadInteger(Category, 'Top', Form.Top)*ScaleMultiplier)),
        round(ReadInteger(Category, 'Width', Form.Width)*ScaleMultiplier), round(ReadInteger(Category, 'Height', Form.Height)*ScaleMultiplier));
      const WindowState = TWindowState(ReadInteger(Category, 'State', integer(Form.WindowState)));

      // Fix bounds
      const Client = Screen.WorkAreaRect;
      if WindowRect.Left < Client.Left then
        WindowRect.Offset( Client.Left - WindowRect.Left, 0 );
      if WindowRect.Right > Client.Right then
        WindowRect.Offset( Client.Right - WindowRect.Right, 0 );
      if WindowRect.Top < Client.Top then
        WindowRect.Offset( 0, Client.Top - WindowRect.Top );
      if WindowRect.Bottom > Client.Bottom then
        WindowRect.Offset( 0, Client.Bottom - WindowRect.Bottom );

      // Align
      case WindowState of
        TWindowState.wsMaximized: begin
          const RestoreData = ReadBool(Category, 'Restore data', false);

          {$IFDEF MSWINDOWS}
          // If window was snapped, re-load the previous snap restore values
          if RestoreData and Form.HandleAllocated and IsWindow(Form.Handle) and GetWindowPlacement(Form.Handle, P) then begin
            P.showCmd := SW_SHOWMAXIMIZED;
            P.rcNormalPosition := WindowRect;
            SetWindowPlacement(Form.Handle, P);
          end else begin
            if RestoreData then
              Form.SetBounds(WindowRect.Left, WindowRect.Top, WindowRect.Width, WindowRect.Height); // this backup mode is Windows Only
          {$ENDIF}
            Form.WindowState := TWindowState.wsMaximized;
          {$IFDEF MSWINDOWS}
          end;
          {$ENDIF}
        end;

        TWindowState.wsMinimized,
        TWindowState.wsNormal: begin
          Form.WindowState := TWindowState.wsNormal;
          Form.SetBounds(WindowRect.Left, WindowRect.Top, WindowRect.Width, WindowRect.Height);
        end;
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
      FLastUpdateCheck := ReadFloat(Section, 'Last update', LastUpdateCheck);
      FLastInstalledVersion := FXVersion.Create(
        ReadString(Section, 'Last installed version', AppVersion.ToString)
        );
    finally
      Free;
    end;
end;

function FXAppManagerClass.NewVersion: boolean;
begin
  Result := FUpdateResult = GreaterThanValue;
end;

procedure FXAppManagerClass.RaiseInit;
begin
  if FInitialized then
    raise Exception.Create('Application manager already initialized.');
end;

procedure FXAppManagerClass.RaiseNotInit;
begin
  if not FInitialized then
    raise Exception.Create('Application manager has not been initialized.');
end;

procedure FXAppManagerClass.RestartApplicationProcess;
begin
  Application.MainForm.Close;

  // Start
  ShellRun( ParamStr(0), '');
end;

procedure FXAppManagerClass.SaveFormData(Form: TForm);
var
  Category: string;
  WindowRect: TRect;
  {$IFDEF MSWINDOWS}
  P: TWindowPlacement;
  {$ENDIF}
begin
  if Form = nil then
    Exit;

  // Name
  Category := Form.Name;

  with GetWindow do
    try
      WindowRect := Form.BoundsRect;
      var RestoreData: boolean; RestoreData := false;

      {$IFDEF MSWINDOWS}
      // Window is snapped by user (via Windows snapping)
      if Form.HandleAllocated and IsWindow(Form.Handle) and GetWindowPlacement(Form.Handle, P) then
        if (Form.WindowState <> wsNormal)
          or ((P.rcNormalPosition.Left <> WindowRect.Left) and (P.rcNormalPosition.Right <> WindowRect.Right)) or
            ((P.rcNormalPosition.Top <> WindowRect.Top) and (P.rcNormalPosition.Bottom <> WindowRect.Bottom)) then begin
              // Get restore pos
              WindowRect := P.rcNormalPosition;
              RestoreData := true;
            end;
      {$ENDIF}

      WriteInteger(Category, 'State', integer(Form.WindowState));
      WriteInteger(Category, 'Left', WindowRect.Left);
      WriteInteger(Category, 'Top', WindowRect.Top);
      WriteInteger(Category, 'Width', WindowRect.Width);
      WriteInteger(Category, 'Height', WindowRect.Height);
      WriteFloat(Category, 'Scale', Form.ScaleFactor);
      WriteBool(Category, 'Restore data', RestoreData);
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
      WriteFloat(Section, 'Last update', LastUpdateCheck);
      WriteString(Section, 'Last installed version', AppVersion.ToString);
    finally
      Free;
    end;
end;

procedure FXAppManagerClass.SetApplicationName(const Value: string);
begin
  if Value.ToLower = 'packages' then
    raise Exception.Create('Invalid application name.');

  RaiseInit;
  FApplicationName := Value;
end;

procedure FXAppManagerClass.SetFlags(const Value: TAppManagerFlags);
begin
  RaiseInit;
  FFlags := Value;
end;

procedure FXAppManagerClass.SetIdentifier(const Value: string);
var
  Val: string;
const
  RemoveChars : TArray<char> = ['~', '!', '@', '#', '$', '%', '^', '&', '*',
    '(', ')', '[', ']', '{', '}', ';', ':', '"', '\', '|', '<', '>', ',',
    '.', '/', '?', #39, '`'];
begin
  Val := StringRemoveCharacters(Value, RemoveChars).Replace(' ', '').ToLower;

  if Val = 'packages' then
    raise Exception.Create('Invalid application identifier.');
  RaiseInit;
  FAppIdentifier := Val;
end;

procedure FXAppManagerClass.SetPublisherName(const Value: string);
begin
  RaiseInit;
  FPublisherName := Value;
end;

{ FXAppManagerFormAssist }

constructor FXAppManagerFormAssist.Create(AOwner: TComponent);
begin
  inherited;

  // Defaults
  FTasks := DEFAULT_TASKS;
end;

destructor FXAppManagerFormAssist.Destroy;
begin
  // Form is closed
  if not IsDesigning and (FXAppFormAssistTask.WindowSaveForm in FTasks) then
    try
      FormClosing;
    except
    end;

  inherited;
end;

function FXAppManagerFormAssist.GetApplicationName: string;
begin
  Result := '';
  if AppManagerInstance <> nil then
    Result := AppManagerInstance.ApplicationName;
end;

function FXAppManagerFormAssist.GetPublisherName: TCaption;
begin
  Result := '';
  if AppManagerInstance <> nil then
    Result := AppManagerInstance.PublisherName;
end;

procedure FXAppManagerFormAssist.Loaded;
begin
  inherited;
  // Form is created
  if not IsDesigning and (FXAppFormAssistTask.WindowLoadForm in FTasks) then
    FormOpening;
end;

procedure FXAppManagerFormAssist.Updated;
begin
  inherited;
end;

{ FXAppManagerComponent }

constructor FXAppManagerComponent.Create(AOwner: TComponent);
begin
  inherited;

  // Form
  FParentForm := AOwner as TForm;
end;

destructor FXAppManagerComponent.Destroy;
begin
  // Remove from stack
  PrimaryDisplayForm := false;

  // Clear
  FParentForm := nil;

  inherited;
end;

procedure FXAppManagerComponent.FormClosing;
begin
  // Save
  AppManager.SaveFormData(ParentForm);
end;

procedure FXAppManagerComponent.FormOpening;
begin
  // Open
  AppManager.LoadFormData(ParentForm);
end;

function FXAppManagerComponent.GetAppData: string;
begin
  Result := AppManager.FolderData;
end;

procedure FXAppManagerComponent.Loaded;
begin
  inherited;

  // Process display
  with TThread.CreateAnonymousThread(procedure begin
    Sleep(10);
    if not Assigned(Self) or (FParentForm = nil) then
      Exit;

    if AppManagerInstance <> nil then
      TThread.Synchronize(TThread.Current, procedure begin
        AppManagerInstance.ProcessDisplayChange;
      end);
  end) do begin
    FreeOnTerminate := true;
    Start;
  end;
end;

procedure FXAppManagerComponent.SetPrimaryDisplayForm(const Value: boolean);
begin
  if ParentForm = nil then
    Exit;

  // Same
  if FPrimaryDisplayForm = Value then
    Exit;

  FPrimaryDisplayForm := Value;

  //
  if not Value then
    TArrayUtils<TForm>.DeleteValue(ParentForm, DisplayActiveFormStack)
  else
    TArrayUtils<TForm>.AddValue(ParentForm, DisplayActiveFormStack);
end;

initialization
  AppMgrCount := 0;
  AppManager := FXAppManagerClass.Create;

finalization
  AppManager.Free;
  AppManager := nil;
end.
