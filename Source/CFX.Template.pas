unit CFX.Template;

interface

uses
  Winapi.Messages,
  Winapi.Windows,
  System.Classes,
  System.Types,
  System.SysUtils,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.ExtCtrls,
  CFX.Colors,
  CFX.ThemeManager,
  CFX.Graphics,
  CFX.Constants,
  CFX.Classes,
  CFX.Types,
  CFX.VarHelpers,
  CFX.Linker,
  CFX.Controls;

type
  FXTemplate = class(FXWindowsControl, FXControl)
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
    procedure ApplyPadding; override;

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

implementation

procedure FXTemplate.ApplyPadding;
begin
  inherited;
  UpdateRects;
end;

function FXTemplate.Background: TColor;
begin
  Result := FDrawColors.BackGround;
end;

constructor FXTemplate.Create(aOwner: TComponent);
begin
  inherited;
  // Custom Color
  FCustomColors := FXColorSets.Create(Self);
  FDrawColors := FXCompleteColorSet.Create;

  // Sizing
  Height := 30;
  Width := 180;

  // Update
  UpdateRects;
  UpdateColors;
end;

destructor FXTemplate.Destroy;
begin
  FreeAndNil( FCustomColors );
  FreeAndNil( FDrawColors );
  inherited;
end;

procedure FXTemplate.InteractionStateChanged(AState: FXControlState);
begin
  inherited;
end;

function FXTemplate.IsContainer: Boolean;
begin
  Result := false;
end;

procedure FXTemplate.PaintBuffer;
begin
  // Background
  Color := FDrawColors.BackGround;
  PaintBackground;

  // Draw

  // Inherit
  inherited;
end;

procedure FXTemplate.Resize;
begin
  inherited;
  UpdateRects;
end;

procedure FXTemplate.UpdateTheme(const UpdateChildren: Boolean);
begin
  UpdateColors;
  UpdateRects;
  Redraw;
end;

procedure FXTemplate.UpdateColors;
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

procedure FXTemplate.UpdateRects;
begin
  // Rect
  DrawRect := GetClientRect;
end;

procedure FXTemplate.ScaleChanged(Scaler: single);
begin
  inherited;
  // update scale
end;

end.
