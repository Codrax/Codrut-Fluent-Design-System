unit CFX.FormClasses;

interface
uses
  Windows,
  Vcl.Graphics,
  Classes,
  Types,
  Vcl.Clipbrd,
  CFX.Types,
  CFX.Constants,
  SysUtils,
  CFX.Colors,
  Vcl.Forms,
  CFX.Graphics,
  CFX.VarHelpers,
  CFX.ThemeManager,
  Vcl.Controls,
  CFX.Files,
  Messages,
  TypInfo,
  CFX.Linker,
  CFX.Classes,
  CFX.Forms,
  CFX.ToolTip,
  CFX.TextBox,
  CFX.Panels,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.Imaging.pngimage,
  CFX.Imported,
  CFX.Button,
  CFX.Progress,
  CFX.StandardIcons,
  CFX.Utilities,
  CFX.StringUtils,
  CFX.ScrollBox,
  CFX.Internet;

type
  FXFillForm = class(TForm)
  private
    FCustomColors: FXColorSets;
    FDrawColors: FXColorSet;
    FThemeChange: FXThemeChange;
    FParentForm: TForm;
    FFillMode: FXFormFill;
    FCloseAction: FXFormCloseAction;
    FTitlebarHeight: integer;

    // Setters
    procedure SetParentForm(const Value: TForm);
    procedure SetFillMode(const Value: FXFormFill);

  protected
    var Margin: integer;
    var Container: FXPanel;

    procedure InitializeNewForm; override;

    procedure BuildControls; virtual;
    procedure Resize; override;

    procedure DoClose(var Action: TCloseAction); override;

    procedure ApplyFillMode;
    procedure ApplyMargins;

    // Mouse
    procedure MouseDown(Button : TMouseButton; Shift: TShiftState; X, Y : integer); override;

  published
    property CustomColors: FXColorSets read FCustomColors write FCustomColors;

    // Parent
    property ParentForm: TForm read FParentForm write SetParentForm;

    // Fill Form
    property FillMode: FXFormFill read FFillMode write SetFillMode;
    property CloseAction: FXFormCloseAction read FCloseAction write FCloseAction;

    // Theming Engine
    property OnThemeChange: FXThemeChange read FThemeChange write FThemeChange;

  public
    { Create a FXFillForm based on a FXForm that exists in the project,
      loading It's settings and controls. }
    constructor Create(aOwner: TComponent); override;

    { Create a FXFillForm based on a custom class. Freed on close }
    constructor CreateNew(aOwner: TComponent; Dummy: Integer = 0); override;
    destructor Destroy; override;

    procedure InitForm;

    // Procedures
    procedure SetBoundsRect(Bounds: TRect);

    // Interface
    function IsContainer: Boolean;
    procedure UpdateTheme(const UpdateChildren: Boolean);

    function Background: TColor;
  end;

implementation

{ FXFillForm }

procedure FXFillForm.ApplyFillMode;
begin
  if ParentForm = nil then
    Exit;

  FTitlebarHeight := FXCustomForm(ParentForm).GetTitlebarHeight;

  case FFillMode of
    FXFormFill.TitleBar: begin
      var ARect: TRect;

      ARect := ParentForm.ClientRect;
      ARect.Top := FTitlebarHeight;

      SetBoundsRect( ARect );
    end;
    FXFormFill.Complete: SetBoundsRect( ParentForm.ClientRect );
  end;
end;

procedure FXFillForm.ApplyMargins;
begin
  with Container do
    begin
      LockDrawing;

      with Margins do
        begin
          Top := Margin;
          Left := Margin;
          Right := Margin;
          Bottom := Margin;

          case FFillMode of
            FXFormFill.Complete: Top := Top + FTitlebarHeight;
            //FXFormFill.TitleBar: ;
          end;
        end;

      UnlockDrawing;
    end;
end;

function FXFillForm.Background: TColor;
begin
  Result := Color;
end;

procedure FXFillForm.BuildControls;
begin
  // nothing
end;

constructor FXFillForm.Create(aOwner: TComponent);
begin
  inherited;

  if aOwner is TForm then
    ParentForm := TForm(aOwner);

  FCloseAction := FXFormCloseAction.Hide;

  // Initialise
  InitForm;
end;

constructor FXFillForm.CreateNew(aOwner: TComponent; Dummy: Integer);
begin
  inherited;

  if aOwner is TForm then
    ParentForm := TForm(aOwner);

  FCloseAction := FXFormCloseAction.Free;

  // Initialise
  InitForm;
end;

destructor FXFillForm.Destroy;
begin

  inherited;
end;

procedure FXFillForm.DoClose(var Action: TCloseAction);
begin
  case FCloseAction of
    FXFormCloseAction.Hide: Action := TCloseAction.caHide;
    else Action := TCloseAction.caFree;
  end;

  inherited;
end;

procedure FXFillForm.InitForm;
begin
  // Default
  Margin := 20;
  DoubleBuffered := true;

  // Settings
  Position := poDesigned;
  BorderStyle := bsNone;
  Caption := '';
  BorderIcons := [];
  AlphaBlend := True;
  Anchors := [akTop, akLeft, akBottom, akRight];

  // Fill Mode
  ApplyFillMode;

  // Container
  if ControlCount = 0 then
    begin
      Container := FXPanel.Create(Self);
      with Container do
        begin
          Parent := Self;

          Align := alClient;
          AlignWithMargins := true;
        end;
      ApplyMargins;
    end;

  // Build
  BuildControls;

  // Update Theme
  UpdateTheme(true);
end;

procedure FXFillForm.InitializeNewForm;
begin
  inherited;
  // Create Classes
  FCustomColors := FXColorSets.Create(Self);
  FDrawColors := FXColorSet.Create(ThemeManager.SystemColorSet, ThemeManager.DarkTheme);
end;

function FXFillForm.IsContainer: Boolean;
begin
  Result := true;
end;

procedure FXFillForm.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: integer);
begin
  inherited;
  ReleaseCapture;
  SendMessage(ParentForm.Handle, WM_NCLBUTTONDOWN, HTCAPTION, 0);
end;

procedure FXFillForm.Resize;
begin
  inherited;
  // next
end;

procedure FXFillForm.SetBoundsRect(Bounds: TRect);
begin
  SetBounds(Bounds.Left, Bounds.Top, Bounds.Width, Bounds.Height);
end;

procedure FXFillForm.SetFillMode(const Value: FXFormFill);
begin
  if FFillMode <> Value then
    begin
      FFillMode := Value;

      ApplyFillMode;
      ApplyMargins;
    end;
end;

procedure FXFillForm.SetParentForm(const Value: TForm);
begin
  FParentForm := Value;
  Parent := Value;
end;

procedure FXFillForm.UpdateTheme(const UpdateChildren: Boolean);
var
  i: integer;
begin
  // Update Colors
  if CustomColors.Enabled then
    begin
      FDrawColors.Background := ExtractColor( CustomColors, FXColorType.BackGround );
      FDrawColors.Foreground := ExtractColor( CustomColors, FXColorType.Foreground );
    end
  else
    begin
      FDrawColors.Background := ThemeManager.SystemColor.BackGround;
      FDrawColors.Foreground := ThemeManager.SystemColor.ForeGround;
    end;

  if Container <> nil then
    Container.CustomColors.Assign( CustomColors );

  // Color
  Color := FDrawColors.BackGround;

  //  Update tooltip style
  if ThemeManager.DarkTheme then
    HintWindowClass := FXDarkTooltip
  else
    HintWindowClass := FXLightTooltip;

  // Font Color
  Font.Color := FDrawColors.Foreground;

  // Notify Theme Change
  if Assigned(FThemeChange) then
    FThemeChange(Self, FXThemeType.AppTheme);

  // Update children
  if IsContainer and UpdateChildren then
    begin
      LockWindowUpdate(Handle);
      for i := 0 to ComponentCount -1 do
        if Supports(Components[i], IFXComponent) then
          (Components[i] as IFXComponent).UpdateTheme(UpdateChildren);
      LockWindowUpdate(0);
    end;
end;

end.
