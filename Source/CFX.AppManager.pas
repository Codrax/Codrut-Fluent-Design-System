unit CFX.AppManager;

{$TYPEINFO ON}
{$SCOPEDENUMS ON}

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
    FLastInstalledVersion: FXVersion;

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
    procedure SaveFormData(Form: TForm);
    procedure LoadFormData(Form: TForm);

    // Properties
    property CheckingForUpdates: boolean read FCheckingUpdates;
    property LastUpdateCheck: TDate read FLastUpdateCheck;
    property AppIdentifier: string read FAppIdentifier write SetIdentifier;
    property ApplicationName: string read FApplicationName write FApplicationName;
    property AppData: string read FAppDataPath;
    property AppPackages: string read FAppPackagesPath;
    property AppVersion: FXVersion read FAppVersion write FAppVersion;
    property LastInstalledVersion: FXVersion read FLastInstalledVersion;
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

  { App Manager Component }
  FXAppManagerComponent = class(FXComponent)
  private
    FPrimaryDisplayForm: boolean;
    FParentForm: TForm;

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
    type TPendingDisplayAction = (UpdatePrompt);
    var
    // Active propr
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

    FPendingDisplayActions: TArray<TPendingDisplayAction>;

    FUpdateCheckUserInitiated: boolean;
    FOnUpdateChecked: TNotifyEvent;
    FOnUpdateStartCheck: TNotifyEvent;
    FOnApplicationLoaded: TNotifyEvent;
    FOnOtherInstance: FXOnOtherInstance;
    FOnVersionUpgraded: FXOnVersionChanged;
    FOnVersionDowngraded: FXOnVersionChanged;
    FUserUpdateWaitDelay: cardinal;

    FApplicationName: string;
    FPublisherName: TCaption;

    // Background
    procedure AppCheckUpdates;

    // Stored
    function IsAppDataStored: Boolean;
    function IsEndpointStored: Boolean;

    // Utils
    function HasPrimaryDisplayForm: boolean;
    function GetPrimaryDisplayForm: TForm;

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

    procedure ProcessDisplayChange;

  published
    property PrimaryDisplayForm default true;

    property ApplicationIdentifier: string read FApplicationIdentifier write SetApplicationIdentifier;
    // The update checking interval (days)
    property UpdateCheckInterval: integer read FUpdateCheckInterval write FUpdateCheckInterval default -1;
    property AppVersion: string read GetVersion write SetVersion;
    property APIName: string read FAPIName write FAPIName;
    property APIEndpoint: string read FAPIEndpoint write SetAPIEndpoint stored IsEndpointStored;

    property SingleInstance: boolean read FSingleInstance write FSingleInstance default false;

    property ApplicationName: string read FApplicationName write SetApplicationName;
    property PublisherName: TCaption read FPublisherName write FPublisherName stored IsAppDataStored;
    property HasAppData: boolean read FHasAppData write SetHasAppData default false;

    property UpdateCheckUserInitiated: boolean read FUpdateCheckUserInitiated;
    property OnUpdateChecked: TNotifyEvent read FOnUpdateChecked write FOnUpdateChecked;
    property OnUpdateStartCheck: TNotifyEvent read FOnUpdateStartCheck write FOnUpdateStartCheck;
    property OnApplicationLoaded: TNotifyEvent read FOnApplicationLoaded write FOnApplicationLoaded;
    property OnOtherInstance: FXOnOtherInstance read FOnOtherInstance write FOnOtherInstance;
    property OnVersionUpgraded: FXOnVersionChanged read FOnVersionUpgraded write FOnVersionUpgraded;
    property OnVersionDowngraded: FXOnVersionChanged read FOnVersionDowngraded write FOnVersionDowngraded;

    property AutomaticTasks: FXAppTasks read FTasks write FTasks default DEFAULT_TASKS;
    property AppDataStructure: TStringList read FAppDataStructure write SetDataStructure;

    property AppData: string read GetAppData;

    property UserUpdateWaitDelay: cardinal read FUserUpdateWaitDelay write FUserUpdateWaitDelay;
    function LastUpdateCheck: string;

  public
    // Application
    procedure ApplicationOpen;
    procedure ApplicationClose;

    // Utils
    procedure InitiateUserUpdateCheck;

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
    function GetAppData: string;
    function GetApplicationName: string;
    function GetPublisherName: TCaption;

  protected
    // Loaded
    procedure Loaded; override;

  published
    // Props
    property AutomaticTasks: FXAppFormAssistTasks read FTasks write FTasks default DEFAULT_TASKS;

    // Read
    property ApplicationName: string read GetApplicationName;
    property PublisherName: TCaption read GetPublisherName;
    property AppData: string read GetAppData;

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
  if AppManager <> nil then
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

  // Version upgraded
  if AppManager.LastInstalledVersion.NewerThan(AppManager.AppVersion) and Assigned(FOnVersionUpgraded) then
    FOnVersionUpgraded(AppManager.LastInstalledVersion, AppManager.AppVersion);
  if AppManager.LastInstalledVersion.NewerThan(AppManager.AppVersion) and Assigned(FOnVersionUpgraded) then
    FOnVersionDowngraded(AppManager.LastInstalledVersion, AppManager.AppVersion);

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
  ApplicationIdentifier := GenerateString(20, true, false, true, false);
  FUpdateCheckInterval := -1;
  FAPIEndpoint := DEFAULT_API;
  FAppVersion.Parse('1.0.0.0');
  FHasAppData := false;
  FPublisherName := DEFAULT_COMPANY;
  FTasks := DEFAULT_TASKS;
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
  if FXAppTask.WindowSaveForm in FTasks then
    FormClosing;

  // Close
  if not IsDesigning then
    ApplicationClose;

  inherited;
end;

function FXAppManager.GetAppData: string;
begin
  Result := AppManager.AppData;

  if FPublisherName <> '' then
    Result := Format('%S%S\', [Result, FPublisherName]);

  if ApplicationName <> '' then
    Result := Format('%S%S\', [Result, ApplicationName])
  else
    Result := Format('%S%S\', [Result, ApplicationIdentifier]);
end;

function FXAppManager.GetPrimaryDisplayForm: TForm;
begin
  Result := nil;
  if not HasPrimaryDisplayForm then
    Exit;

  // Get top-most stack object
  Result := DisplayActiveFormStack[High(DisplayActiveFormStack)];
end;

function FXAppManager.GetVersion: string;
begin
  Result := FAppVersion.ToString(true);
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

function FXAppManager.IsAppDataStored: Boolean;
begin
  Result := FPublisherName <> DEFAULT_COMPANY;
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
      FLastUpdateCheck := ReadDate(Section, 'Last update', LastUpdateCheck);
      FLastInstalledVersion := FXVersion.Create(
        ReadString(Section, 'Last installed version', AppVersion.ToString(true))
        );
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
      WriteDate(Section, 'Last update', LastUpdateCheck);
      WriteString(Section, 'Last installed version', AppVersion.ToString(true));
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
  if FXAppFormAssistTask.WindowSaveForm in FTasks then
    FormClosing;

  inherited;
end;

function FXAppManagerFormAssist.GetAppData: string;
begin
  Result := '';
  if AppManagerInstance <> nil then
    Result := AppManagerInstance.AppData;
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
  if FXAppFormAssistTask.WindowLoadForm in FTasks then
    FormOpening;
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
