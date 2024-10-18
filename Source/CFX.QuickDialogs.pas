unit CFX.QuickDialogs;

interface
  uses
    Windows, SysUtils, CFX.Dialogs, Vcl.Forms, System.UITypes;

// Dialogs
procedure OpenMessage(AText: string); overload;
procedure OpenMessage(ATitle, AText: string); overload;
function OpenDialog(AText: string; AButtons: TMsgDlgButtons): TModalResult; overload;
function OpenDialog(ATitle, AText: string; AButtons: TMsgDlgButtons): TModalResult; overload;
function OpenDialog(AText: string; AKind: FXDialogKind; AButtons: TMsgDlgButtons): TModalResult; overload;
function OpenDialog(ATitle, AText: string; AKind: FXDialogKind; AButtons: TMsgDlgButtons): TModalResult; overload;
function OpenInput(ATitle, AText: string; var AValue: string): boolean; overload;
function OpenInput(ATitle, AText: string; var AValue: integer; DefaultValue: integer=0): boolean; overload;

implementation

function GetActiveForm: TForm;
var
  ActiveHandle: HWND;
  I: Integer;
begin
  Result := nil;
  ActiveHandle := Application.ActiveFormHandle;

  for I := 0 to Application.ComponentCount-1 do
    if Application.Components[I] is TForm then
      with TForm(Application.Components[I]) do
        if (Handle = ActiveHandle) and Visible then
          Exit( TForm(Application.Components[I]) );
end;

procedure OpenMessage(AText: string);
begin
  OpenMessage('Message', AText);
end;

procedure OpenMessage(ATitle, AText: string);
begin
  with FXMessageBox.Create do
    try
      Parent := GetActiveForm;

      Title := ATitle;
      Text := AText;

      Execute;
    finally
      Free;
    end;
end;

function OpenDialog(AText: string; AButtons: TMsgDlgButtons): TModalResult; overload;
begin
  Result := OpenDialog('Dialog', AText, AButtons);
end;

function OpenDialog(ATitle, AText: string; AButtons: TMsgDlgButtons): TModalResult;
begin
  with FXModalDialog.Create do
    try
      Parent := GetActiveForm;

      Title := ATitle;
      Text := AText;

      Buttons := AButtons;

      Result := Execute;
    finally
      Free;
    end;
end;

function OpenDialog(AText: string; AKind: FXDialogKind; AButtons: TMsgDlgButtons): TModalResult;
var
  ATitle: string;
begin
  ATitle := '';
  case AKind of
    FXDialogKind.Information: ATitle := 'Information';
    FXDialogKind.Error: ATitle := 'Error';
    FXDialogKind.Question: ATitle := 'Confirmation';
    FXDialogKind.Success: ATitle := 'Sucess';
    FXDialogKind.Warning: ATitle := 'Warning';
    FXDialogKind.Star: ATitle := 'Attention';
  end;
  Result := OpenDialog(ATitle, AText, AKind, AButtons);
end;

function OpenDialog(ATitle, AText: string; AKind: FXDialogKind; AButtons: TMsgDlgButtons): TModalResult;
begin
  with FXModalIconDialog.Create do
    try
      Parent := GetActiveForm;

      Title := ATitle;
      Text := AText;
      Kind := AKind;

      Buttons := AButtons;

      Result := Execute;
    finally
      Free;
    end;
end;

function OpenInput(ATitle, AText: string; var AValue: string): boolean;
begin
  with FXInputBox.Create do
    try
      Parent := GetActiveForm;

      Title := ATitle;
      Text := AText;

      Value := AValue;

      Execute;
      Result := DialogResult = FXInputBoxResult.Ok;
      if Result then
        AValue := Value;
    finally
      Free;
    end;
end;

function OpenInput(ATitle, AText: string; var AValue: integer; DefaultValue: integer): boolean;
begin
  with FXInputBox.Create do
    try
      Parent := GetActiveForm;

      NumbersOnly := true;

      Title := ATitle;
      Text := AText;

      Value := AValue.ToString;

      Execute;
      Result := DialogResult = FXInputBoxResult.Ok;
      if Result then begin
        if Value = '' then
          AValue := DefaultValue
        else
          AValue := Value.ToInteger;
      end;
    finally
      Free;
    end;
end;

end.
