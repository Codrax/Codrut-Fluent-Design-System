unit CFX.Layouts;

interface

uses
  Winapi.Messages,
  Winapi.Windows,
  System.Classes,
  System.Types,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.ExtCtrls,
  CFX.Colors,
  CFX.ThemeManager,
  CFX.Graphics,
  CFX.constants,
  SysUtils,
  CFX.Classes,
  CFX.Types,
  CFX.VarHelpers,
  CFX.Linker,
  CFX.Controls;

type
  FXCustomLayout = class(FXWindowsControl, FXControl)
  private
    var DrawRect: TRect;
    FDrawColors: FXCompleteColorSet;
    FCustomColors: FXColorSets;

    //  Internal
    procedure UpdateColors;
    procedure UpdateRects;

    // Getters

    // Setters

  protected
    procedure PaintBuffer; override;
    procedure Resize; override;

    // Scaler
    procedure ScaleChanged(Scaler: single); override;

    // State
    procedure InteractionStateChanged(AState: FXControlState); override;

  published
    // Custom Colors
    property CustomColors: FXColorSets read FCustomColors write FCustomColors stored true;

    // Props

    // Default props
    property Align;
    property PaddingFill;
    property Constraints;
    property Anchors;
    property Hint;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property FocusFlags;
    property DragKind;
    property DragCursor;
    property DragMode;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnStartDrag;
    property OnEnter;
    property OnExit;
    property OnClick;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property OnMouseUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;

  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    // Interface
    function IsContainer: Boolean;
    procedure UpdateTheme(const UpdateChildren: Boolean);

    function Background: TColor;
  end;

  FXLayout = class(FXCustomLayout)
  public
  end;

implementation

function FXCustomLayout.Background: TColor;
begin
  Result := FDrawColors.BackGround;
end;

constructor FXCustomLayout.Create(aOwner: TComponent);
begin
  inherited;
  ControlStyle := [csAcceptsControls];

  // Custom Color
  FCustomColors := FXColorSets.Create(Self);

  FDrawColors := FXCompleteColorSet.Create;

  // Sizing
  Height := 200;
  Width := 250;

  // Update
  UpdateRects;
  UpdateColors;
end;

destructor FXCustomLayout.Destroy;
begin
  FreeAndNil( FCustomColors );
  FreeAndNil( FDrawColors );
  inherited;
end;

procedure FXCustomLayout.InteractionStateChanged(AState: FXControlState);
begin
  inherited;
  Invalidate;
end;

function FXCustomLayout.IsContainer: Boolean;
begin
  Result := false;
end;

procedure FXCustomLayout.PaintBuffer;
begin
  // Background
  Color := FDrawColors.BackGround;
  PaintBackground;

  // Draw

  // Inherit
  inherited;
end;

procedure FXCustomLayout.Resize;
begin
  inherited;
  UpdateRects;
end;

procedure FXCustomLayout.UpdateTheme(const UpdateChildren: Boolean);
begin
  UpdateColors;
  UpdateRects;
  Invalidate;
end;

procedure FXCustomLayout.UpdateColors;
begin
  FDrawColors.Assign( ThemeManager.SystemColor );

  if not Enabled then
    begin
      FDrawColors.Foreground := $808080;
    end
  else
    begin
      // Access theme manager
      if FCustomColors.Enabled then
        // Load custom
        FDrawColors.LoadFrom( FCustomColors, ThemeManager.DarkTheme )
      else
        // Build color palette
        FDrawColors.LoadFrom( ThemeManager.SystemColorSet, ThemeManager.DarkTheme );
    end;
end;

procedure FXCustomLayout.UpdateRects;
begin
  // Rect
  DrawRect := GetClientRect;
end;

procedure FXCustomLayout.ScaleChanged(Scaler: single);
begin
  inherited;
  // update scale
end;

end.
