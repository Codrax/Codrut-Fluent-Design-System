unit CFX.TitlebarPanel;

interface

uses
  Winapi.Messages,
  Winapi.Windows,
  System.Classes,
  System.Types,
  Vcl.Controls,
  Vcl.TitleBarCtrls,
  Vcl.Graphics,
  Vcl.ExtCtrls,
  CFX.Colors,
  Math,
  CFX.ThemeManager,
  CFX.Graphics,
  CFX.constants,
  SysUtils,
  CFX.Messages,
  CFX.Classes,
  CFX.Types,
  CFX.VarHelpers,
  CFX.Linker,
  CFX.Controls;

type
  FXTitleBarPanel = class(TCustomTitleBarPanel, IFXComponent, IFXControl)
  private
    var DrawRect: TRect;
    FDrawColors: FXCompleteColorSet;
    FCustomColors: FXColorSets;

    //  Internal
    procedure UpdateColors;
    procedure UpdateRects;

  protected
    // Handle messages
    procedure WndProc(var Message: TMessage); override;

  published
    // Custom Colors
    property CustomColors: FXColorSets read FCustomColors write FCustomColors stored true;

    // Default props
    property Margins;
    property OnPaint;
    property CustomButtons;

  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    // Draw
    procedure Redraw;

    // Interface
    function IsContainer: Boolean;
    procedure UpdateTheme(const UpdateChildren: Boolean);
    function Background: TColor;
  end;

implementation


{ FXTitleBarPanel }

function FXTitleBarPanel.Background: TColor;
begin
  Result := FDrawColors.BackGround;
end;

constructor FXTitleBarPanel.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  // Custom Color
  FCustomColors := FXColorSets.Create(Self);
  FDrawColors := FXCompleteColorSet.Create;

  // Update
  UpdateRects;
  UpdateColors;
end;

destructor FXTitleBarPanel.Destroy;
begin
  FreeAndNil( FCustomColors );
  FreeAndNil( FDrawColors );
  inherited;
end;

function FXTitleBarPanel.IsContainer: Boolean;
begin
  Result := true;
end;

procedure FXTitleBarPanel.Redraw;
begin
  Invalidate;
end;

procedure FXTitleBarPanel.UpdateColors;
begin
  // Access theme manager
  FDrawColors.Assign( ThemeManager.SystemColor );
  if not Enabled then begin
    FDrawColors.Foreground := $808080;
  end
  else
    if FCustomColors.Enabled then
      // Custom Colors
      FDrawColors.LoadFrom(FCustomColors, ThemeManager.DarkTheme);
end;

procedure FXTitleBarPanel.UpdateRects;
begin
  // Rect
  DrawRect := Rect(0, 0, Width, Height);
end;

procedure FXTitleBarPanel.UpdateTheme(const UpdateChildren: Boolean);
begin
  UpdateColors;
  UpdateRects;
  Invalidate;

  // Update children
  if UpdateChildren then
    for var I := 0 to ControlCount-1 do
      if Supports(Controls[I], IFXComponent) then
        (Controls[I] as IFXComponent).UpdateTheme(UpdateChildren);
end;

procedure FXTitleBarPanel.WndProc(var Message: TMessage);
begin
  inherited;
  if InRange(Message.Msg, WM_CFX_MESSAGES, WM_CFX_MESSAGES_END) then
    Broadcast( Message );
end;

end.
