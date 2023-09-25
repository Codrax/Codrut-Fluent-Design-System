unit CFX.Dialogs;

{$SCOPEDENUMS ON}

interface

  uses
    Windows, Vcl.Dialogs, CFX.Button, Types, Classes, Variants,
    Vcl.Forms, Vcl.Themes, Vcl.Styles, Vcl.Graphics, CFX.Types,
    Vcl.Controls, CFX.Colors, SysUtils, Vcl.ExtCtrls, Vcl.ComCtrls,
    Vcl.TitleBarCtrls, Math, CFX.Math, Vcl.StdCtrls, CFX.Forms,
    UITypes, CFX.Edit;

  type
    FXMessageType = (Information, Error, Question, Sucess, Warning, Star);
    FXInputBoxResult = (Cancel, Ok);

    FXButtonDesignHelper = class helper for FXButton
      procedure ApplyButtonSettings(LoadFromButton: FXButton);
    end;

    FXDialogBox = class
      private
        FTitle: string;
        FText: string;
        FTitleFont,
        FTextFont: TFont;
        FButtonDesign: FXButton;
        FFormColor: TColor;

        FParent: TForm;

        FTitlebarHeight: integer;
        FButtonOffset: integer;
        FButtonHeight: integer;

        // Settings
        PromptCreation: boolean;
        DialogUnits: TPoint;
        HasButtons: boolean;
        FScreenCenter: boolean;

        // Inherited Creation
        Form: FXForm;
        MainPrompt,
        Prompt,
        Footer: TLabel;

        // Utils
        function GetTextWidth(Text: string; Font: TFont): integer;
        function GetButonWidth(Text: string; Font: TFont): integer;
        function CalculateButtonHeight: integer;

        function ButtonTypeToModal(AType: TMsgDlgBtn): integer;

        procedure ResizeForm(NewWidth: integer = -1; NewHeight: integer = -1);

        procedure CreateButtons(Buttons: TMsgDlgButtons);
        function FindButton(ModalResult: integer): FXButton;

        function GetCharSize(Canvas: TCanvas): TPoint;

      public
        // Public Settings
        constructor Create; virtual;

        // Settings
        procedure HighlightDefaultButton;

        property ParentForm: TForm read FParent write FParent;
        property ScreenCenter: boolean read FScreenCenter write FScreenCenter;

        // Execute
        procedure ExecuteInherited; virtual;
        function ModalExecution(FreeForm: boolean): integer;

        property Title: string read FTitle write FTitle;
        property Text: string read FText write FText;
        property ButtonDesign: FXButton read FButtonDesign write FButtonDesign;
    end;

    FXMessageBox = class(FXDialogBox)
      private


      public
        constructor Create; override;
        destructor Destroy; override;

        procedure Execute; overload;
    end;

    FXDialog = class(FXDialogBox)
      private
        FKind: FXMessageType;
        FButtons: TMsgDlgButtons;

      public
        constructor Create; override;
        destructor Destroy; override;

        function Execute: integer; overload;

        property Kind: FXMessageType read FKind write FKind;
        property Buttons: TMsgDlgButtons read FButtons write FButtons;
    end;

    FXInputBox = class(FXDialogBox)
      private
        FValue: string;
        FTextHint: string;
        FSelectAll: boolean;
        FCanCancel: boolean;
        FNumbersOnly: boolean;
        FPasswordChar: char;

      public
        DialogResult: FXInputBoxResult;

        constructor Create; override;
        destructor Destroy; override;

        function Execute: string; overload;

        property Value: string read FValue write FValue;
        property TextHint: string read FTextHint write FTextHint;
        property SelectAll: boolean read FSelectAll write FSelectAll;
        property CanCancel: boolean read FCanCancel write FCanCancel;
        property PasswordChar: char read FPasswordChar write FPasswordChar;
        property NumbersOnly: boolean read FNumbersOnly write FNumbersOnly;
    end;

var
  ButtonLabels: TArray<string> =
                        [
                        'Yes',        // Yes
                        'No',         // No
                        'Ok',         // Ok
                        'Cancel',     // Cancel
                        'Abort',      // Abort
                        'Retry',      // Retry
                        'Ignore',     // Ignore
                        'All',        // All
                        'Yes to All', // YesAll
                        'No to All',  // NoAll
                        'Help',       // Help
                        'Close'       //Close
                        ];

implementation

{ FXDialog }

constructor FXDialog.Create;
begin
  inherited;

  FKind := FXMessageType.Information;
  FButtons := [mbOk];
end;

destructor FXDialog.Destroy;
begin
  FreeAndNil(FTextFont);

  inherited;
end;

function FXDialog.Execute: integer;
var
  I: TMsgDlgBtn;
begin
  ExecuteInherited;

  with Form do begin
    // Form Settings
    Form.Constraints.MinHeight := Form.ClientHeight;
    ResizeForm( Form.ClientWidth + FButtonOffset );

    // Create Buttons
    CreateButtons( Buttons );

    // Default Buttons
    if mbCancel in Buttons then
      FindButton( mrCancel ).Cancel := true;
    if mbOK in Buttons then
      FindButton( mrOK ).Default := true
        else
          if mbYes in Buttons then
            FindButton( mrYes ).Default := true;
              if mbAll in Buttons then
                FindButton( mrAll ).Default := true
                  else
                    if mbYesToAll in Buttons then
                      FindButton( mrYesToALl ).Default := true
                        else
                          begin
                            for I in Buttons do
                              begin
                                FindButton( ButtonTypeToModal(I) ).Default := true;

                                Break;
                              end;
                          end;

    HighlightDefaultButton;

    // Result
    Result := ModalExecution(true);
  end;
end;

{ FXMessageBox }

constructor FXMessageBox.Create;
begin
  inherited;
end;

destructor FXMessageBox.Destroy;
begin
  FreeAndNil(FTextFont);

  inherited;
end;

procedure FXMessageBox.Execute;
begin
  ExecuteInherited;

  with Form do begin
    Self.CreateButtons([mbOk]);

    FindButton( mrOk ).Default := true;

    HighlightDefaultButton;
    
    ModalExecution(true);
  end;
end;

{ FXInputBox }

constructor FXInputBox.Create;
begin
  inherited;

  DialogResult := FXInputBoxResult.Cancel;

  FPasswordChar := #0;
  FNumbersOnly := false;
  FCanCancel := true;
end;

destructor FXInputBox.Destroy;
begin
  FreeAndNil(FTextFont);

  inherited;
end;

function FXInputBox.Execute: string;
var
  AText: FXEdit;
begin
  ExecuteInherited;

  with Form do begin
    // Create Text Box
    AText := FXEdit.Create(nil);
    with AText do
    begin
      Parent   := Form;

      Text := FValue;
      TextHint := FTextHint;

      Font.Assign( FTextFont );
      Font.Size := Font.Size + 2;

      PasswordChar := FPasswordChar;
      NumbersOnly := FNumbersOnly;

      Left := Prompt.Left;

      Top := Prompt.Top + Prompt.Height + FButtonOffset;

      Anchors := [akLeft, akTop];

      BorderStyle := bsNone;

      // Select All
      if FSelectAll then
        SelectAll;
    end;

    ResizeForm( -1, Form.ClientHeight + AText.Height + FButtonOffset * 2 );

    // Create Buttons
    if FCanCancel then
      CreateButtons( [mbOk, mbCancel] )
    else
      CreateButtons( [mbOk] );

    // Default Button
    FindButton( mrOK ).Default := true;
    if FCanCancel then
      FindButton( mrCancel ).Cancel := true;

    HighlightDefaultButton;
      
    // Set Edit Width
    AText.Width := Form.ClientWidth - Prompt.Left * 2; // This is set after in case the Buttons span a langer distance that the Form

    // Focus
    //Text.SetFocus;

    if ModalExecution(false) = mrOk then
      begin
        DialogResult := FXInputBoxResult.Ok;
        Result := AText.Text;
      end
    else
      begin
        DialogResult := FXInputBoxResult.Cancel;
        Result := Value;
      end;

    // Free form
    Form.Free;
  end;
end;

{ FXDialogBox }

function FXDialogBox.ButtonTypeToModal(AType: TMsgDlgBtn): integer;
begin
  case AType of
    TMsgDlgBtn.mbYes: Result := mrYes;
    TMsgDlgBtn.mbNo: Result := mrNo;
    TMsgDlgBtn.mbOK: Result := mrOK;
    TMsgDlgBtn.mbCancel: Result := mrCancel;
    TMsgDlgBtn.mbAbort: Result := mrAbort;
    TMsgDlgBtn.mbRetry: Result := mrRetry;
    TMsgDlgBtn.mbIgnore: Result := mrIgnore;
    TMsgDlgBtn.mbAll: Result := mrAll;
    TMsgDlgBtn.mbNoToAll: Result := mrNoToAll;
    TMsgDlgBtn.mbYesToAll: Result := mrYesToAll;
    TMsgDlgBtn.mbHelp: Result := mrHelp;
    TMsgDlgBtn.mbClose: Result := mrClose;
    else Result := mrNone;
  end;
end;

function FXDialogBox.CalculateButtonHeight: integer;
var
  BT: FXButton;
begin
  BT := FXButton.Create(nil);
  try
    // Apply Stiling
    BT.ApplyButtonSettings( FButtonDesign );

    // Get Height
    Result := Bt.Height;
  finally
    BT.Free;
  end;
end;

constructor FXDialogBox.Create;
begin
  // Style Default
  FButtonDesign := FXButton.Create(nil);

  // Font Default
  FTextFont := TFont.Create;
  with FTextFont do
  begin
    Name := 'Segoe UI';
    Size := 11;
    Color := clBlack;
  end;

  FTitleFont := TFont.Create;
  with FTitleFont do
  begin
    Name := 'Segoe UI Bold';
    Size := 14;
    Color := clBlack;
  end;

  // Default Settings
  PromptCreation := true;
  HasButtons := true;

  FFormColor := clWindow;
end;

procedure FXDialogBox.CreateButtons(Buttons: TMsgDlgButtons);
var
  I: TMsgDlgBtn;
  Right,
  ATop: integer;
begin
  // Prepare Values
  Right := Form.Width - FButtonOffset * 2;
  ATop := Form.ClientHeight - FButtonHeight - FButtonOffset;

  // Create
  for I in Buttons do
  with FXButton.Create(Form) do
    begin
      Parent := Form;
      Animation := false;

      Anchors := [akRight,akBottom];

      Height := FButtonHeight;
      Width := GetButonWidth(Text, Self.FButtonDesign.Font);

      Top := ATop;
      Left := Right - Width - FButtonOffset;

      ModalResult := ButtonTypeToModal( I );

      Text := ButtonLabels[ Integer(I) ];

      ApplyButtonSettings( FButtonDesign );

      // Next
      Right := Right - Width - FButtonOffset;
    end;

  // Extend Form Size
  if Right < 0 then
    begin
      Form.ClientWidth := Form.ClientWidth + abs(Right) + FButtonOffset;
      if Footer <> nil then
        Footer.Width := Form.ClientWidth;
    end;

end;

procedure FXDialogBox.ExecuteInherited;
var
  TextLength,
  TxtUnits_X: integer;
  PureValue: double;
begin
  // Styled Form
  if FFormColor = clWindow then
    FFormColor := TStyleManager.ActiveStyle.GetSystemColor(clBtnFace);

  // Default
  Form   := FXForm.CreateNew(Application);
  with Form do
    begin
      // Form Settings
      Constraints.MinWidth := 300;

      Position    := poScreenCenter;
      BorderStyle := bsSizeable;
      BorderIcons := [];

      Caption := '';

      FFormColor := Color;

      // Font & Color
      if ColorToRGB( GetColorLight(FFormColor) ) > 155 then
        FTextFont.Color := clBlack
      else
        FTextFont.Color := clWhite;
      FTitleFont.Color := FTextFont.Color;

      Font.Assign( FTextFont );
      Canvas.Font.Assign( FTextFont );

      // Dialog Units
      DialogUnits := GetCharSize(Canvas);

      // Apply Titlebar
      FTitlebarHeight := Form.GetTitleBarHeight;

      // Init Size
      ClientHeight := FTitlebarHeight;
      ClientWidth := 0;

      // Create Prompt
      if PromptCreation then
        begin
          TextLength := Length ( FText );
          PureValue := TextLength / 80;
          //TxtUnits_Y := ceil( PureValue );

          if PureValue <= 1 then
            TxtUnits_X := TextLength
          else
            TxtUnits_X := 260 div 4;

          MainPrompt      := TLabel.Create(Form);
          with MainPrompt do
          begin
            Parent   := Form;

            Caption  := FTitle;
            Font.Assign( FTitleFont );

            Left     := MulDiv(6, DialogUnits.X, 2);
            Top      := MulDiv(2, DialogUnits.Y, 1);


            WordWrap := False;
          end;

          Prompt      := TLabel.Create(Form);
          with Prompt do
          begin
            Parent   := Form;

            Caption  := FText;
            Font.Assign( FTextFont );

            Left     := MulDiv(6, DialogUnits.X, 2);
            Top      := MainPrompt.Top + MulDiv(2, DialogUnits.Y, 1) + FTitlebarHeight;

            Layout := tlCenter;

            Constraints.MaxWidth := MulDiv(TxtUnits_X, DialogUnits.X, 1);
            WordWrap := True;

            // Respect title
            Constraints.MinWidth := MainPrompt.Width;

            if MainPrompt.Caption = '' then
              Top := MainPrompt.Top;
          end;

          // Add Prompt Size
          ClientHeight := Height + Prompt.Top + Prompt.Height - FTitlebarHeight;
          ClientWidth := Prompt.Left * 2 + Prompt.Width;
        end;

      // Creat Footer
      if HasButtons then
        begin
          // Change form size

          FButtonHeight :=  trunc(80 / 100 * CalculateButtonHeight );
          Form.ClientHeight := Form.ClientHeight + FButtonHeight;

          FButtonOffset := MulDiv(1, DialogUnits.Y, 2);

          // Create Bottom
          Footer := TLabel.Create(Form);
          with Footer do
          begin
            Parent   := Form;

            Anchors := [akLeft, akBottom];

            AutoSize := false;
            Transparent := false;

            Color := ChangeColorLight(Form.Color,-10);

            Height := FButtonHeight + FButtonOffset * 2;
            Width := Form.ClientWidth;
            Left     := 0;
            Top      := Form.ClientHeight - Height;

            Visible := true;
          end;
        end;
    end;
end;

function FXDialogBox.FindButton(ModalResult: integer): FXButton;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Form.ControlCount - 1 do
    if Form.Controls[I] is FXButton then
      if FXButton(Form.Controls[I]).ModalResult = ModalResult then
        Result := FXButton(Form.Controls[I]);
end;

function FXDialogBox.GetButonWidth(Text: string; Font: TFont): integer;
begin
  Result := GetTextWidth(Text, Font) + 50;
  if Result < 90 then
    Result := 90;
end;

function FXDialogBox.GetCharSize(Canvas: TCanvas): TPoint;
var
  I: Integer;
  Buffer: array[0..51] of Char;
begin
  // Gets the avarage letter width
  for I := 0 to 25 do Buffer[I] := Chr(I + Ord('A'));
  for I := 0 to 25 do Buffer[I + 26] := Chr(I + Ord('a'));
  GetTextExtentPoint(Canvas.Handle, Buffer, 52, TSize(Result));
  Result.X := Result.X div 52;
end;


function FXDialogBox.GetTextWidth(Text: string; Font: TFont): integer;
var
  c: TBitMap;
begin
  c := TBitMap.Create;
  try
    c.Canvas.Font.Assign(Font);
    Result := c.Canvas.TextWidth(Text);
  finally
    c.Free;
  end;
end;

procedure FXDialogBox.HighlightDefaultButton;
var
  A: integer;
begin
  for A := 0 to Form.ControlCount - 1 do
      if Form.Controls[A] is FXButton then
        if (Form.Controls[A] as FXButton).Default then
          with (Form.Controls[A] as FXButton) do
            ButtonKind := FXButtonKind.Accent;
end;

function FXDialogBox.ModalExecution(FreeForm: boolean): integer;
begin
  // Lock Form Size
  Form.Constraints.MinWidth := Form.Width;
  Form.Constraints.MaxWidth := Form.Width;
  Form.Constraints.MinHeight := Form.Height;
  Form.Constraints.MaxHeight := Form.Height;

  // Position
  if not ScreenCenter then
    begin
      Form.Position := poDesigned;

      if Assigned(ParentForm) then
        begin
          Form.Left := ParentForm.Left + ParentForm.Width div 2 - Form.Width div 2;
          Form.Top := ParentForm.Top + ParentForm.Height div 2 - Form.Height div 2;
        end
      else
        if Application.MainForm <> nil then
          begin
            Form.Left := Application.MainForm.Left + Application.MainForm.Width div 2 - Form.Width div 2;
            Form.Top := Application.MainForm.Top + Application.MainForm.Height div 2 - Form.Height div 2;
          end
            else
              begin
                Form.Left := Screen.Width div 2 - Form.Width div 2;
                Form.Top := Screen.Height div 2 - Form.Height div 2;
              end;
    end;

  // Execute Modal & Return value
  try
    // Smoke On
    if Assigned(ParentForm) and (ParentForm is FXForm) then
      FXForm(ParentForm).SmokeEffect := true;

    // Modal
    Result := Form.ShowModal;
  finally
    // Smoke Off
    if Assigned(ParentForm) and (ParentForm is FXForm) then
      FXForm(ParentForm).SmokeEffect := false;

    // Free Memory
    if FreeForm then
      Form.Free;
  end;
end;

procedure FXDialogBox.ResizeForm(NewWidth: integer; NewHeight: integer);
begin
  if NewWidth <> -1 then
    Form.ClientWidth := NewWidth;

  if NewHeight <> -1 then
    Form.ClientHeight := NewHeight;

  if footer <> nil then
    begin
      Footer.Width := Form.ClientWidth;
      Footer.Top := Form.ClientHeight - Footer.Height;
    end;
end;

{ FXButtonDesignHelper }

procedure FXButtonDesignHelper.ApplyButtonSettings(LoadFromButton: FXButton);
begin
  with Self do begin
    // Accent
    CustomColors.Assign(LoadFromButton.CustomColors);
    CustomButtonColors.Assign(LoadFromButton.CustomButtonColors);

    Font.Assign(LoadFromButton.Font);
    WordWrap := LoadFromButton.WordWrap;

    ImageLayout := LoadFromButton.ImageLayout;
    LayoutHorizontal := LoadFromButton.LayoutHorizontal;
    LayoutVertical := LoadFromButton.LayoutVertical;
    ButtonKind := LoadFromButton.ButtonKind;
    Roundness := LoadFromButton.Roundness;
  end;
end;

end.
