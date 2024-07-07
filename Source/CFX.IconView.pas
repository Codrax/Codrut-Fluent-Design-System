unit CFX.IconView;

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
  FXIconView = class(FXWindowsControl, FXControl)
    private
      var DrawRect, IconRect: TRect;
      FDrawColors: FXCompleteColorSet;
      FCustomColors: FXColorSets;
      FScale: real;
      FImage: FXIconSelect;
      FVertLayout: FXLayout;
      FHorizLayout: FXLayout;

      //  Internal
      procedure UpdateColors;
      procedure UpdateRects;

      // Getters

      // Setters
      procedure SetHorizLayout(const Value: FXLayout);
      procedure SetImage(const Value: FXIconSelect);
      procedure SetScale(const Value: real);
      procedure SetVertLayout(const Value: FXLayout);

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
      property Image: FXIconSelect read FImage write SetImage;
      property Scale: real read FScale write SetScale;
      property LayoutHorizontal: FXLayout read FHorizLayout write SetHorizLayout default FXLayout.Center;
      property LayoutVertical: FXLayout read FVertLayout write SetVertLayout default FXLayout.Center;

      // Default props
      property Align;
      property Font;
      property Transparent;
      property Opacity;
      property PaddingFill;
      property Constraints;
      property Anchors;
      property Hint;
      property ShowHint;
      property ParentShowHint;
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

function FXIconView.Background: TColor;
begin
  Result := FDrawColors.BackGround;
end;

constructor FXIconView.Create(aOwner: TComponent);
begin
  inherited;
  // Props
  FScale := 1;
  FImage := FXIconSelect.Create(Self);
  FImage.Enabled := true;

  FHorizLayout := FXLayout.Center;
  FVertLayout := FXLayout.Center;

  // Custom Color
  FCustomColors := FXColorSets.Create(Self);

  FDrawColors := FXCompleteColorSet.Create;

  // Sizing
  Height := 60;
  Width := 60;

  // Update
  UpdateRects;
  UpdateColors;
end;

destructor FXIconView.Destroy;
begin
  FreeAndNil( FImage );
  FreeAndNil( FCustomColors );
  FreeAndNil( FDrawColors );
  inherited;
end;

procedure FXIconView.InteractionStateChanged(AState: FXControlState);
begin
  inherited;
  Invalidate;
end;

function FXIconView.IsContainer: Boolean;
begin
  Result := false;
end;

procedure FXIconView.PaintBuffer;
begin
  // Background
  Color := FDrawColors.BackGround;
  PaintBackground;

  // Draw
  with Buffer do
    begin
      // Fill
      if not Transparent then
        begin
          Brush.Color := FDrawColors.Background;
          Pen.Style := psClear;
          FillRect(ClipRect);
        end;

      // Write
      Brush.Style := bsClear;
      Font.Color := FDrawColors.ForeGround;
      FImage.DrawIcon(Buffer, IconRect);
    end;

  // Inherit
  inherited;
end;

procedure FXIconView.Resize;
begin
  inherited;
  UpdateRects;
end;

procedure FXIconView.UpdateTheme(const UpdateChildren: Boolean);
begin
  UpdateColors;
  UpdateRects;
  Invalidate;
end;

procedure FXIconView.UpdateColors;
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
      FDrawColors.BackGround := GetParentBackgroundColor(FDrawColors.BackGround);
    end;
end;

procedure FXIconView.UpdateRects;
begin
  // Rect
  DrawRect := ClientRect;

  // Fill
  IconRect := DrawRect;

  // Scale
  IconRect.Width := round(DrawRect.Width * Scale);
  IconRect.Height := round(DrawRect.Height * Scale);

  if IconRect.Height < IconRect.Width then
    IconRect.Width := IconRect.Height
  else
    IconRect.Height := IconRect.Width;

  // Allign
  case FHorizLayout of
    FXLayout.Beginning: ;
    FXLayout.Center: IconRect.Offset((DrawRect.Width - IconRect.Width) div 2, 0);
    FXLayout.Ending: IconRect.Offset(DrawRect.Width - IconRect.Width, 0);
  end;

  case FVertLayout of
    FXLayout.Beginning: ;
    FXLayout.Center: IconRect.Offset(0, (ClientRect.Height - IconRect.Height) div 2);
    FXLayout.Ending: IconRect.Offset(0, DrawRect.Height - IconRect.Height);
  end;
end;

procedure FXIconView.ScaleChanged(Scaler: single);
begin
  inherited;
  // update scale
end;

procedure FXIconView.SetHorizLayout(const Value: FXLayout);
begin
if FHorizLayout <> Value then
    begin
      FHorizLayout := Value;

      UpdateRects;
      Invalidate;
    end;
end;

procedure FXIconView.SetImage(const Value: FXIconSelect);
begin
  if FImage <> Value then
    begin
      FImage := Value;

      Invalidate;
    end;
end;

procedure FXIconView.SetScale(const Value: real);
begin
  if FScale <> Value then
    begin
      FScale := Value;

      UpdateRects;
      Invalidate;
    end;
end;

procedure FXIconView.SetVertLayout(const Value: FXLayout);
begin
  if FVertLayout <> Value then
    begin
      FVertLayout := Value;

      UpdateRects;
      Invalidate;
    end;
end;

end.
