unit CFX.RatingControl;

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
  CFX.Constants,
  SysUtils,
  CFX.Classes,
  CFX.Types,
  CFX.VarHelpers,
  CFX.Linker,
  CFX.Controls;

type
  FXRatingControl = class(FXWindowsControl, FXControl)
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
      property Transparent;
      property Opacity;
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

function FXRatingControl.Background: TColor;
begin
  Result := FDrawColors.BackGround;
end;

constructor FXRatingControl.Create(aOwner: TComponent);
begin
  inherited;
  // Custom Color
  FCustomColors := FXColorSets.Create(Self);

  FDrawColors := FXCompleteColorSet.Create;

  // Sizing
  Height := 40;
  Width := 200;

  // Update
  UpdateRects;
  UpdateColors;
end;

destructor FXRatingControl.Destroy;
begin
  FreeAndNil( FCustomColors );
  FreeAndNil( FDrawColors );
  inherited;
end;

procedure FXRatingControl.InteractionStateChanged(AState: FXControlState);
begin
  inherited;
  Invalidate;
end;

function FXRatingControl.IsContainer: Boolean;
begin
  Result := false;
end;

procedure FXRatingControl.PaintBuffer;
begin
  // Background
  Color := FDrawColors.BackGround;
  PaintBackground;

  // Draw

  // Inherit
  inherited;
end;

procedure FXRatingControl.Resize;
begin
  inherited;
  UpdateRects;
end;

procedure FXRatingControl.UpdateTheme(const UpdateChildren: Boolean);
begin
  UpdateColors;
  UpdateRects;
  Invalidate;
end;

procedure FXRatingControl.UpdateColors;
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

procedure FXRatingControl.UpdateRects;
begin
  // Rect
  DrawRect := GetClientRect;
end;

procedure FXRatingControl.ScaleChanged(Scaler: single);
begin
  inherited;
  // update scale
end;

end.
