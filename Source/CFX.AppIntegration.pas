unit CFX.AppIntegration;

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
    CFX.Types,
    Vcl.Graphics,
    CFX.ThemeManager,
    CFX.Colors,
    CFX.Files,
    CFX.Version,
    CFX.StringUtils,
    CFX.Registry,
    CFX.FormClasses,
    ShellAPI,
    CFX.QuickDialogs,
    CFX.Utilities;


    // UI Utilities
    procedure PromptUpdateUser(MainForm: TForm; var Form: TForm);
    procedure PromptUpdateUserClose(var Form: TForm);
    procedure PromptUpdate(MainForm: TForm; Required: boolean = false);

implementation

uses
  CFX.AppManager;

procedure PromptUpdateUser(MainForm: TForm; var Form: TForm);
begin
  Form := FXTaskExecutingTemplate.CreateNew(MainForm);
  with FXTaskExecutingTemplate(Form) do
    try
      FillMode := FXFormFill.TitleBar;
      CloseAction := FXFormCloseAction.Free;

      Title := 'Checking for updates';
      Text := Format('Checking the update server for a new version of "%S". Please stand by...', [AppManager.ApplicationName]);
      ProgressText := 'Now contacting the update server';
      ShowCancel := false;

      Show;

      // User check timeout
      for var I := 1 to AppManagerInstance.UserUpdateWaitDelay div 5 do begin
        Application.ProcessMessages;
        Sleep(5);
      end;
    finally
      //Free;
    end;
end;

procedure PromptUpdateUserClose(var Form: TForm);
begin
  // End UI
  if Form <> nil then begin
    Form.Hide;

    // Free
    Application.ProcessMessages;
    Form.Free;

    // Set to nil
    Form := nil;
  end;
end;

procedure PromptUpdate(MainForm: TForm; Required: boolean);
var
  URL: string;
begin
  // Err
  if not AppManager.UpdateCheckSuccess then
    begin
      OpenMessage('An error occured', 'We could not check for updates');
      Exit;
    end;

  // New
  if not AppManager.NewVersion then
    Exit;

  // Download link
  URL := AppManager.ServerVersion.GetDownloadLink;
  if URL = '' then
    begin
      OpenMessage('An error occured', 'The update link was not found. You will need to update manually.');
      Exit;
    end;

  with FXFormUpdateTemplate.CreateNew(MainForm) do
    try
      FillMode := FXFormFill.TitleBar;
      CloseAction := FXFormCloseAction.Free;

      AllowSnooze := not Required;

      AppName := AppManager.ApplicationName;

      DownloadURL := URL;
      InstallParameters := '-ad';

      Show;
    finally
      //Free;
    end;
end;
end.
