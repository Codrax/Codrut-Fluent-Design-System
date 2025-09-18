unit CFX.Dialogs;

{$SCOPEDENUMS ON}

interface

uses
  Winapi.Windows, Winapi.Messages, Vcl.Dialogs, CFX.Button, Types, Classes,
  Variants, Vcl.Forms, Vcl.Themes, Vcl.Styles, Vcl.Graphics, CFX.Types,
  Vcl.Controls, CFX.Colors, SysUtils, Vcl.ExtCtrls, Vcl.ComCtrls,
  Vcl.TitleBarCtrls, Math, CFX.Math, Vcl.StdCtrls, CFX.Forms, CFX.Constants,
  UITypes, CFX.Edit, CFX.TextBox, CFX.Panels, CFX.TitlebarPanel,
  CFX.ArrayHelpers, CFX.Graphics, CFX.StandardIcons, CFX.ThemeManager;

type
  FXDialogKind = (None, Information, Error, Question, Success, Warning, Star);
  FXInputBoxResult = (Cancel, Ok);
  FXDialogFlag = (ParentCenter, ParentSmoke, AllowMove, MoveParent, ScreenCenter, GetParentActive, GetParentMain);
  FXDialogFlags = set of FXDialogFlag;

  TButtonLabelsArray = array[TMsgDlgBtn] of string;

  FXButtonDesignHelper = class helper for FXButton
    procedure ApplyButtonSettings(LoadFromButton: FXButton);
  end;

  FXBaseDialog = class;

  FXDialogButtonInfo = record
    Text: string;
    FontIcon: string;
    Highlight: boolean;
    Tag: integer;
  end;

  FXDialogComponentForm = class(FXDialogForm)
  private
    FDialog: FXBaseDialog;

  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;

  public
    constructor CreateFor(ADialog: FXBaseDialog);
  end;

  FXBaseDialog = class
  strict private
    FFormCaption: string;
    FTitlebarIcon,
    FTitlebarClose: boolean;

    FParent: TForm;
    FFlags: FXDialogFlags;

    FContentPadding: TSize;

  protected
    FForm: FXDialogComponentForm;

    // Runtime
    FActualParent: TForm; // fetched during runtime
    FActualClient: TRect;
    FActualContents: TRect;
    FUnusedContents: TRect;

    procedure ExecuteDialog; virtual;

    // Form preparation
    procedure PrepareDialog; virtual;
    procedure CreateDialog; virtual;
    procedure OpenDialog; virtual;
    procedure ClosedDialog; virtual;
    procedure FreeDialog; virtual;

    // Sizing
    function GetMinimumSize: TSize; virtual;
    function GetContainerClient: TRect; virtual;

    // Misc
    property Form: FXDialogComponentForm read FForm;

    // Props
    property ContentPadding: TSize read FContentPadding write FContentPadding;
    property TitlebarIcon: boolean read FTitlebarIcon write FTitlebarIcon;
    property TitlebarClose: boolean read FTitlebarClose write FTitlebarClose;
    property TitlebarText: string read FFormCaption write FFormCaption;
  public
    // Prop
    property Parent: TForm read FParent write FParent;
    property Flags: FXDialogFlags read FFlags write FFlags;

    // Constructors
    constructor Create; overload; virtual;
    constructor Create(AParent: TForm); overload;
    destructor Destroy; override;
  end;

  FXButtonedDialog = class(FXBaseDialog)
  strict private
    FButtons: TArray<FXDialogButtonInfo>;

    FButtonDefault: integer;
    FButtonCancel: integer;

    FButtonSpacing: integer;
    FButtonWidth,
    FButtonHeight: integer;
    FButtonAlignment: TLayout;
    FButtonDynamicSizing: boolean;

    FPanelOfButtons: FXPanel;

    // Events
    procedure ButtonClickEvent(Sender: TObject);

    // Util
    function GetButtonBarWidth: integer;

    // Getters
    function GetButtonCount: integer;
    function GetButton(Index: integer): FXDialogButtonInfo;

    // Setters
    procedure SetButton(Index: integer; const Value: FXDialogButtonInfo);

  protected
    // Runtime
    FClickedButton: integer;
    FClickedButtonTag: integer;

    // Do
    procedure DoButtonClick(Index: integer); virtual;

    // Buttons
    function ComputeButtonSize(Index: integer): integer;

    // Form preparation
    procedure CreateDialog; override;

    // Sizing
    function GetMinimumSize: TSize; override;
    function GetContentSize: TSize; virtual;

    // Props
    property Button[Index: integer]: FXDialogButtonInfo read GetButton write SetButton;
    property ButtonCount: integer read GetButtonCount;
    property ButtonDefault: integer read FButtonDefault write FButtonDefault;
    property ButtonCancel: integer read FButtonCancel write FButtonCancel;

    property ButtonSpacing: integer read FButtonSpacing write FButtonSpacing;
    property ButtonWidth: integer read FButtonWidth write FButtonWidth;
    property ButtonHeight: integer read FButtonHeight write FButtonHeight;
    property ButtonAlignment: TLayout read FButtonAlignment write FButtonAlignment;
    property ButtonDynamicSizing: boolean read FButtonDynamicSizing write FButtonDynamicSizing;

  public
    property ClickedButton: integer read FClickedButton;

    // Utils
    procedure AddButton(Text: string; FontIcon: string; Highlighed: boolean=false; Tag: integer=0);
    procedure RemoveButton(Index: integer);
    procedure ClearButtons;

    // Constructors
    constructor Create; override;
  end;

  FXHeaderedDialog = class(FXButtonedDialog)
  strict private
    const SPACING_TITLE = 8;
    const SPACING_AFTER = 25;
    var

    FTitle,
    FText: string;
    FTitleFont,
    FTextFont: TFont;

  protected
    // Specific
    function GetContentsStartingPoint: TPoint;

    // Form preparation
    procedure CreateDialog; override;

    // Sizing
    function GetContentSize: TSize; override;

    // Props
    property Button;
    property ButtonCount;
    property ButtonDefault;
    property ButtonCancel;
    property ButtonSpacing;
    property ButtonWidth;
    property ButtonHeight;
    property ButtonAlignment;
    property ButtonDynamicSizing;

    property Title: string read FTitle write FTitle;
    property Text: string read FText write FText;
    property TitleFont: TFont read FTitleFont write FTitleFont;
    property TextFont: TFont read FTextFont write FTextFont;

  public
    constructor Create; override;
  end;

  FXMessageBox = class(FXHeaderedDialog)
  public
    property ContentPadding;
    property TitlebarIcon;
    property TitlebarClose;
    property TitlebarText;

    property ButtonWidth;
    property ButtonHeight;
    property ButtonAlignment;
    property ButtonDynamicSizing;

    property Title;
    property Text;
    property TitleFont;
    property TextFont;

    procedure Execute; overload;

      // Constructors
    constructor Create; override;
  end;

  FXDialog = class(FXHeaderedDialog)
  public
    // Props
    property ContentPadding;
    property TitlebarIcon;
    property TitlebarClose;
    property TitlebarText;

    property Button;
    property ButtonCount;
    property ButtonDefault;
    property ButtonCancel;
    property ButtonSpacing;
    property ButtonWidth;
    property ButtonHeight;
    property ButtonAlignment;
    property ButtonDynamicSizing;

    property Title;
    property Text;
    property TitleFont;
    property TextFont;

    function Execute: integer;
  end;

  FXModalDialog = class(FXHeaderedDialog)
  private
    FButtons: TMsgDlgButtons;
    FButtonLabels: TButtonLabelsArray;

  public
    // Props
    property ContentPadding;
    property TitlebarIcon;
    property TitlebarClose;
    property TitlebarText;

    property ButtonSpacing;
    property ButtonWidth;
    property ButtonHeight;
    property ButtonAlignment;
    property ButtonDynamicSizing;

    property Title;
    property Text;
    property TitleFont;
    property TextFont;

    // Props
    property Buttons: TMsgDlgButtons read FButtons write FButtons;
    property ButtonLabels: TButtonLabelsArray read FButtonLabels write FButtonLabels;

    function Execute: TModalResult; overload;

    // Constructors
    constructor Create; override;
  end;

  FXIconDialog = class(FXHeaderedDialog)
  strict private
    const SPACING_RIGHT = 10;
    var
    FKind: FXDialogKind;
    FIconSize: integer;

  protected
    // Sizing
    function GetContainerClient: TRect; override;
    function GetContentSize: TSize; override;

    // Form preparation
    procedure CreateDialog; override;

  public
    // Props
    property ContentPadding;
    property TitlebarIcon;
    property TitlebarClose;
    property TitlebarText;

    property Button;
    property ButtonCount;
    property ButtonDefault;
    property ButtonCancel;
    property ButtonSpacing;
    property ButtonWidth;
    property ButtonHeight;
    property ButtonAlignment;
    property ButtonDynamicSizing;

    property Title;
    property Text;
    property TitleFont;
    property TextFont;

    // Props
    property Kind: FXDialogKind read FKind write FKind;
    property IconSize: integer read FIconSize write FIconSize;

    function Execute: integer;

    // Constructors
    constructor Create; override;
  end;

  FXModalIconDialog = class(FXIconDialog)
  private
    FButtons: TMsgDlgButtons;
    FButtonLabels: TButtonLabelsArray;

  public
    // Props
    property ContentPadding;
    property TitlebarIcon;
    property TitlebarClose;
    property TitlebarText;

    property ButtonSpacing;
    property ButtonWidth;
    property ButtonHeight;
    property ButtonAlignment;
    property ButtonDynamicSizing;

    property Title;
    property Text;
    property TitleFont;
    property TextFont;

    // Props
    property Buttons: TMsgDlgButtons read FButtons write FButtons;
    property ButtonLabels: TButtonLabelsArray read FButtonLabels write FButtonLabels;

    function Execute: TModalResult; overload;

    // Constructors
    constructor Create; override;
  end;

  FXInputBox = class(FXHeaderedDialog)
  strict private
    const SPACING_AFTER = 25;
    var
    FValue: string;
    FTextHint: string;
    FSelectAll: boolean;
    FCanCancel: boolean;
    FNumbersOnly: boolean;
    FPasswordChar: char;

    FEditFont: TFont;

  protected
    FValueEdited: boolean;

    // Do
    procedure DoButtonClick(Index: integer); override;

    // Form preparation
    procedure CreateDialog; override;

    // Sizing
    function GetContentSize: TSize; override;

  public
    // Props
    property ContentPadding;
    property TitlebarIcon;
    property TitlebarClose;
    property TitlebarText;

    property ButtonSpacing;
    property ButtonWidth;
    property ButtonHeight;
    property ButtonAlignment;
    property ButtonDynamicSizing;

    property Title;
    property Text;
    property TitleFont;
    property TextFont;

    // Props
    property Value: string read FValue write FValue;
    property EditFont: TFont read FEditFont write FEditFont;
    property TextHint: string read FTextHint write FTextHint;
    property SelectAll: boolean read FSelectAll write FSelectAll;
    property CanCancel: boolean read FCanCancel write FCanCancel;
    property PasswordChar: char read FPasswordChar write FPasswordChar;
    property NumbersOnly: boolean read FNumbersOnly write FNumbersOnly;

    function Execute: boolean;

    // Constructors
    constructor Create; override;
  end;

// Utils
function ButtonToModal(Btn: TMsgDlgBtn): integer;

var
  GlobalButtonLabels: TButtonLabelsArray = (
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
    );

implementation

function ButtonToModal(Btn: TMsgDlgBtn): integer;
begin
  case Btn of
    mbYes: Result := mrYes;
    mbNo: Result := mrNo;
    mbOK: Result := mrOK;
    mbCancel: Result := mrCancel;
    mbAbort: Result := mrAbort;
    mbRetry: Result := mrRetry;
    mbIgnore: Result := mrIgnore;
    mbAll: Result := mrAll;
    mbNoToAll: Result := mrNoToAll;
    mbYesToAll: Result := mrYesToAll;
    mbHelp: Result := mrHelp;
    mbClose: Result := mrClose;

    else Result := mrNone;
  end;
end;

{ FXDialog }

function FXDialog.Execute: integer;
begin
  ExecuteDialog;

  Result := FClickedButton;
end;

{ FXMessageBox }

constructor FXMessageBox.Create;
begin
  inherited;
  AddButton('OK', '', true);
  ButtonDefault := 0;
end;

procedure FXMessageBox.Execute;
begin
  ExecuteDialog;
end;

{ FXInputBox }

constructor FXInputBox.Create;
begin
  inherited;
  AddButton('OK', '', true);
  AddButton('Cancel', '', false);
  ButtonDefault := 0;
  ButtonCancel := 1;

  FPasswordChar := #0;
  FNumbersOnly := false;
  FCanCancel := true;

  // Fonts
  FEditFont := TFont.Create;
  with FEditFont do
  begin
    Name := 'Segoe UI';
    Size := 12;
  end;
end;

procedure FXInputBox.CreateDialog;
begin
  inherited;

  with FXEdit.Create(FForm) do begin
    Parent := FForm;

    Font.Assign( FEditFont );

    // Data
    Text := Self.Value;

    // Props
    TextHint := FTextHint;
    PasswordChar := FPasswordChar;
    NumbersOnly := FNumbersOnly;

    // Position
    Top := FUnusedContents.Top;
    Left := FUnusedContents.Left;
    Width := FUnusedContents.Width;

    // Select All
    if FSelectAll then
      SelectAll;
  end;
end;

procedure FXInputBox.DoButtonClick(Index: integer);
begin
  inherited;
  FValueEdited := Index = 0;

  // Extract value
  for var I := 0 to FForm.ControlCount-1 do
    if FForm.Controls[I] is FXEdit then
      Value := (FForm.Controls[I] as FXEdit).Text;
end;

function FXInputBox.Execute: boolean;
begin
  // Exec
  ExecuteDialog;

  Result := FValueEdited;
end;

function FXInputBox.GetContentSize: TSize;
begin
  Result := inherited;

  Result.Height := Result.Height + EDIT_DEFAULT_HEIGHT + SPACING_AFTER;
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

{ FXIconDialog }

constructor FXIconDialog.Create;
begin
  inherited;
  FKind := FXDialogKind.Information;
  FIconSize := 50;
end;

procedure FXIconDialog.CreateDialog;
begin
  inherited;
  if Kind <> FXDialogKind.None then
    with FXStandardIcon.Create(FForm) do begin
      Parent := FForm;

      // Data
      case Self.Kind of
        FXDialogKind.Information: SelectedIcon := FXStandardIconType.Information;
        FXDialogKind.Error: SelectedIcon := FXStandardIconType.Error;
        FXDialogKind.Question: SelectedIcon := FXStandardIconType.Question;
        FXDialogKind.Success: SelectedIcon := FXStandardIconType.Checkmark;
        FXDialogKind.Warning: SelectedIcon := FXStandardIconType.Warning;
        FXDialogKind.Star: SelectedIcon := FXStandardIconType.Star;
      end;

      // Pos
      Top := FActualContents.Top;
      Left := FActualContents.Left - FIconSize - SPACING_RIGHT;
      Width := FIconSize;
      Height := FIconSize;
    end;
end;

function FXIconDialog.Execute: integer;
begin
  ExecuteDialog;

  Result := FClickedButton;
end;

function FXIconDialog.GetContainerClient: TRect;
begin
  Result := inherited;
  if Kind = FXDialogKind.None then
    Exit;

  Result.Offset(FIconSize+SPACING_RIGHT, 0);
end;

function FXIconDialog.GetContentSize: TSize;
begin
  Result := inherited;
  if Kind = FXDialogKind.None then
    Exit;

  // Add width
  Result.Width := Result.Width + SPACING_RIGHT + IconSize;

  // Extend height if necesarry
  if Result.Height < SPACING_RIGHT + IconSize then
    Result.Height := SPACING_RIGHT + IconSize;
end;

{ FXBaseDialog }

constructor FXBaseDialog.Create;
begin
  FParent := nil;
  FFormCaption := '';
  FContentPadding := TSize.Create(20, 5);
  FFlags := [FXDialogFlag.ParentCenter, FXDialogFlag.ParentSmoke,
    FXDialogFlag.MoveParent, FXDialogFlag.GetParentActive];
end;

procedure FXBaseDialog.ClosedDialog;
begin
  //
end;

constructor FXBaseDialog.Create(AParent: TForm);
begin
  Create;

  FParent := AParent;
end;

procedure FXBaseDialog.CreateDialog;
begin
  FForm := FXDialogComponentForm.CreateFor( Self );
  FForm.Caption := FFormCaption;

  FForm.BackgroundColor := FXBackgroundColor.Content;

  // Titlebar config
  FForm.CustomTitleBar.ShowIcon := TitlebarIcon;
  FForm.CustomTitleBar.SystemHeight := false;
  FForm.CustomTitleBar.Height := 35;

  // Properties
  if TitlebarClose then
    FForm.BorderIcons := [biSystemMenu]
  else
    FForm.BorderIcons := [];
  FForm.BorderStyle := bsSingle;

  // Type
  FForm.AutoMoveParent := FXDialogFlag.MoveParent in FFlags;
  FForm.AutoSmoke := FXDialogFlag.ParentSmoke in Flags;
  FForm.AutoCenter := FXDialogFlag.ParentCenter in Flags;
  if not FForm.AutoCenter then
    if FXDialogFlag.ScreenCenter in Flags then
      FForm.Position := poScreenCenter;

  // Size
  FActualClient := GetContainerClient;
  Form.ClientHeight := FActualClient.Height + FActualClient.Top*2 + FForm.GetTitlebarHeight;
  Form.ClientWidth := FActualClient.Width + FActualClient.Left*2;

  // Content rect
  FActualContents := FActualClient;
  const BSize = (FForm.Width - FForm.ClientWidth) div 2;
  FActualContents.Offset(BSize, FForm.GetTitlebarHeight);

  FUnusedContents := FActualContents;
end;

destructor FXBaseDialog.Destroy;
begin

  inherited;
end;

procedure FXBaseDialog.ExecuteDialog;
begin
  // Prep
  PrepareDialog;

  // Create
  CreateDialog;
  try
    OpenDialog; // wait

    // Was closed
    ClosedDialog;
  finally
    // Free
    FreeDialog;

    // Finalise
    FActualParent := nil;
  end;
end;

procedure FXBaseDialog.FreeDialog;
begin
  FreeAndNil( FForm );
end;

function FXBaseDialog.GetContainerClient: TRect;
begin
  const MinSize = GetMinimumSize;

  Result := TRect.Create(0, 0, MinSize.Width, MinSize.Height);
  Result.Offset(FContentPadding.cx, FContentPadding.cy);
end;

function FXBaseDialog.GetMinimumSize: TSize;
begin
  Result := TSize.Create( 400, 0 )
end;

procedure FXBaseDialog.OpenDialog;
begin
  FForm.ShowModal;
end;

procedure FXBaseDialog.PrepareDialog;
begin
  FActualParent := FParent;

  // Auto get parent
  if (FActualParent = nil) then begin
    if (FXDialogFlag.GetParentActive in Flags) then
      FActualParent := Screen.ActiveForm
    else
      if (FXDialogFlag.GetParentMain in Flags) then
        FActualParent := Application.MainForm;
  end;
end;

{ FXButtonedDialog }

procedure FXButtonedDialog.AddButton(Text: string; FontIcon: string; Highlighed: boolean; Tag: integer);
var
  Button: FXDialogButtonInfo;
begin
  Button.Text := Text;
  Button.FontIcon := FontIcon;
  Button.Highlight := Highlighed;
  Button.Tag := Tag;

  TArrayUtils<FXDialogButtonInfo>.AddValue(Button, FButtons);
end;

procedure FXButtonedDialog.ButtonClickEvent(Sender: TObject);
begin
  DoButtonClick( FXButton(Sender).Tag );
end;

procedure FXButtonedDialog.ClearButtons;
begin
  FButtons := [];
end;

function FXButtonedDialog.ComputeButtonSize(Index: integer): integer;
begin
  // Compute
  with FForm.Canvas do begin
    Font.Name := ThemeManager.FormFont;
    Font.Height := ThemeManager.FormFontHeight;
    Font.Style := [];

    // Main text
    Result := BUTTON_MARGIN * 4 + TextWidth(FButtons[Index].Text);

    // Icon
    if FButtons[Index].FontIcon <> '' then
      Result := Result + BUTTON_MARGIN + round(TextHeight('Aa.') * BUTTON_IMAGE_SCALE);
  end;

  // Min
  if Result < ButtonWidth then
    Result := ButtonWidth;
end;

constructor FXButtonedDialog.Create;
begin
  inherited;
  FButtons := [];
  FButtonDefault := -1;
  FButtonCancel := -1;
  FButtonAlignment := TLayout.Ending;

  FClickedButton := -1;

  FButtonSpacing := 10;
  FButtonWidth := 110;
  FButtonHeight := BUTTON_DEFAULT_HEIGHT;
end;

procedure FXButtonedDialog.CreateDialog;
begin
  inherited;

  FClickedButton := -1;

  // Create container
  FPanelOfButtons := FXPanel.Create(FForm);
  with FPanelOfButtons do begin
    BackgroundColor := FXBackgroundColor.Background;
    Parent := FForm;

    Height := ButtonHeight + FButtonSpacing * 2;

    Align := alBottom;
    Padding.Top := FButtonSpacing;
    Padding.Left := FButtonSpacing;
    Padding.Bottom := FButtonSpacing;
    Padding.Right := FButtonSpacing;

    // Center offset
    Padding.Right := 0;
    if FButtonAlignment = TLayout.Center then
      Padding.Right := (FForm.ClientWidth - GetButtonBarWidth) div 2;
  end;

  // Create buttons
  for var I := 0 to Length(FButtons)-1 do begin
    const Button = FXButton.Create(FPanelOfButtons);
    with Button do begin
      Parent := FPanelOfButtons;

      // Info
      Text := FButtons[I].Text;
      if FButtons[I].FontIcon <> '' then begin
        Image.IconType := FXIconType.SegoeIcon;
        Image.SelectSegoe := FButtons[I].FontIcon;
      end;
      if FButtons[I].Highlight then
        ButtonKind := FXButtonKind.Accent;

      // Props
      Default := ButtonDefault = I;
      Cancel := ButtonCancel = I;

      // Position
      MarginsFill.Right := FButtonSpacing;
      if FButtonAlignment = TLayout.Beginning then begin
        Align := alLeft;
        Left := FActualClient.Right;
      end
      else begin
        Align := alRight;
        Left := 0;
      end;
      if ButtonDynamicSizing then
        Width := ComputeButtonSize(I)
      else
        Width := ButtonWidth;

      // Event
      Tag := I;
      OnClick := ButtonClickEvent;
      ModalResult := mrClose;
    end;
  end;
end;

procedure FXButtonedDialog.DoButtonClick(Index: integer);
begin
  FClickedButton := Index;
  FClickedButtonTag := FButtons[Index].Tag;
end;

function FXButtonedDialog.GetButton(Index: integer): FXDialogButtonInfo;
begin
  Result := FButtons[Index];
end;

function FXButtonedDialog.GetButtonBarWidth: integer;
begin
  if not ButtonDynamicSizing then
    Exit(
      ButtonCount * (ButtonWidth+ButtonSpacing) + ButtonSpacing );

  // Dyn Btn Size
  Result := 0;
  for var I := 0 to ButtonCount-1 do
    Inc(Result, ComputeButtonSize(I) + ButtonSpacing);
end;

function FXButtonedDialog.GetButtonCount: integer;
begin
  Result := Length(FButtons);
end;

function FXButtonedDialog.GetContentSize: TSize;
begin
  Result := TSize.Create(GetButtonBarWidth, 0);
end;

function FXButtonedDialog.GetMinimumSize: TSize;
begin
  Result := inherited;

  // No buttons
  if ButtonCount <> 0 then begin
    // Set
    Result.Width := Max(Result.Width, GetButtonBarWidth);
    Result.Height := Max(Result.Height, FButtonHeight + 2*ButtonSpacing);
  end;

  // Content layer
  const Content = GetContentSize;
  Result.Width := Max(Result.Width, Content.Width);
  Result.Height := Result.Height + Content.Height;
end;

procedure FXButtonedDialog.RemoveButton(Index: integer);
begin
  TArrayUtils<FXDialogButtonInfo>.Delete(Index, FButtons);
end;

procedure FXButtonedDialog.SetButton(Index: integer;
  const Value: FXDialogButtonInfo);
begin
  FButtons[Index] := Value;
end;

{ FXHeaderedDialog }

constructor FXHeaderedDialog.Create;
begin
  inherited;

  // Data
  FTitle := '';
  FText := '';

  // Fonts
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
end;

procedure FXHeaderedDialog.CreateDialog;
begin
  inherited;

  if (Title = '') and (Text = '') then
    Exit;

  // Title
  if Title <> '' then
    with FXTextBox.Create(FForm) do begin
      Parent := FForm;

      Font.Assign( FTitleFont );

      // Data
      Text := FTitle;

      // Position
      Top := FUnusedContents.Top;
      Left := FUnusedContents.Left;

      FUnusedContents.Top := FUnusedContents.Top + Height + SPACING_TITLE;
    end;

  // Text
  if Text <> '' then
    with FXTextBox.Create(FForm) do begin
      Parent := FForm;

      Font.Assign( FTextFont );

      LayoutVertical := CFX.Types.TLayout.Beginning;

      // Data
      Text := FText;

      // Position
      Top := FUnusedContents.Top;
      Left := FUnusedContents.Left;

      WordWrap := true;
      Width := FUnusedContents.Width;

      FUnusedContents.Top := FUnusedContents.Top + Height;
    end;

  // After
  FUnusedContents.Top := FUnusedContents.Top + SPACING_AFTER;
end;

function FXHeaderedDialog.GetContentsStartingPoint: TPoint;
begin
  Result := FActualContents.TopLeft;
end;

function FXHeaderedDialog.GetContentSize: TSize;
const
  DIALOG_MAX_SIZE = 1000;
var
  Bitmap: TBitMap;
  AWidth, AHeight, ALineHeight: integer;
  Canvas: TCanvas;

function GetWordWrapTextHeight(Text: string; ALineHeight: integer; Width: integer): integer;
begin
  const Lines = Length(GetWordWrapLines(Canvas, Text, Rect(0, 0, Width, 0)));
  Result := Lines*ALineHeight;
end;
begin
  Result := inherited;

  if (Title = '') and (Text = '') then
    Exit;

  // Margins
  Result.Width := Result.Width;

  Bitmap := TBitMap.Create;
  Canvas := Bitmap.Canvas;
  try
    // Title
    if Title <> '' then begin
      Canvas.Font.Assign( FTitleFont );
      Result.Height := Result.Height + Canvas.TextHeight('Abc.')+SPACING_TITLE;

      // Width respect
      AWidth := Canvas.TextWidth( Title );
      if Result.Width < AWidth then
        Result.Width := Min(AWidth, DIALOG_MAX_SIZE);
    end;

    // Text
    if Text <> '' then begin
      Canvas.Font.Assign( FTextFont );

      // Calculate sizing
      ALineHeight := Canvas.TextHeight( 'Abc.' );

      // Get the best aspect ration
      AWidth := Result.Width;
      AHeight := GetWordWrapTextHeight(Text, ALineHeight, AWidth);
      while (AWidth < DIALOG_MAX_SIZE) and (AHeight > 1/3*AWidth) do begin
        // Next
        AWidth := AWidth + 50;
        AHeight := GetWordWrapTextHeight(Text, ALineHeight, AWidth);
      end;

      // Set data (important to check for changes!)
      if AWidth <> Result.Width then
        Result.Width := Min(DIALOG_MAX_SIZE, AWidth);
      AHeight := GetWordWrapTextHeight(Text, ALineHeight, AWidth);

      // Add height
      Result.Height := Result.Height + AHeight;
    end;

    // Add after
    Result.Height := Result.Height + SPACING_AFTER;
  finally
    Bitmap.Free;
  end;
end;

{ FXDialogComponentForm }

constructor FXDialogComponentForm.CreateFor(ADialog: FXBaseDialog);
begin
  FDialog := ADialog;

  inherited CreateNew( ADialog.FActualParent );
end;

procedure FXDialogComponentForm.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  ReleaseCapture;
  SendMessage(Self.Handle, WM_NCLBUTTONDOWN, HTCAPTION, 0);
end;

{ FXModalDialog }

constructor FXModalDialog.Create;
begin
  inherited;
  FButtons := [mbOk];
  FButtonLabels := GlobalButtonLabels;
end;

function FXModalDialog.Execute: TModalResult;
var
  Active: TMsgDlgBtn;
function FailCheckDefault(Btn: TMsgDlgBtn): boolean;
begin
  Result := not (Btn in Buttons);
  if not Result then
    Active := Btn;
end;
begin
  // Add buttons
  ClearButtons;

  // Active
  if FailCheckDefault(mbYes) then
    if FailCheckDefault(mbOk) then
      if FailCheckDefault(mbRetry) then
        if FailCheckDefault(mbAll) then
          FailCheckDefault(mbYesToAll);

  // Return
  for var B in Buttons do
    AddButton( FButtonLabels[B], '', B = Active, integer(B) );

  // Exec
  ExecuteDialog;

  // Data
  Result := ButtonToModal( TMsgDlgBtn(FClickedButtonTag) );
end;

{ FXModalIconDialog }

constructor FXModalIconDialog.Create;
begin
  inherited;
  Buttons := [mbOk];
  FButtonLabels := GlobalButtonLabels;
end;

function FXModalIconDialog.Execute: TModalResult;
var
  Active: TMsgDlgBtn;
function FailCheckDefault(Btn: TMsgDlgBtn): boolean;
begin
  Result := not (Btn in Buttons);
  if not Result then
    Active := Btn;
end;
begin
  // Add buttons
  ClearButtons;

  // Active
  if FailCheckDefault(mbYes) then
    if FailCheckDefault(mbOk) then
      if FailCheckDefault(mbRetry) then
        if FailCheckDefault(mbAll) then
          FailCheckDefault(mbYesToAll);

  // Return
  for var B in Buttons do
    AddButton( FButtonLabels[B], '', B = Active, integer(B) );

  // Exec
  ExecuteDialog;

  // Data
  Result := ButtonToModal( TMsgDlgBtn(FClickedButtonTag) );
end;

end.
