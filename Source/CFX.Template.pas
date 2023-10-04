unit CFX.Template;

interface

uses
  Classes,
  Messages,
  Windows,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.ExtCtrls,
  Types,
  CFX.Colors,
  CFX.ThemeManager,
  CFX.Graphics,
  CFX.UIConsts,
  SysUtils,
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
  Invalidate;
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
  Invalidate;
end;

procedure FXTemplate.UpdateColors;
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
