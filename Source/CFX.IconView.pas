unit CFX.IconView;

interface

uses
  Classes,
  Winapi.Messages,
  Winapi.Windows,
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
  CFX.ComponentClasses,
  CFX.Types,
  CFX.VarHelpers,
  CFX.Linker,
  CFX.Controls;

type
  FXIconView = class(FXWindowsControl)
  private
    var DrawRect, IconRect: TRect;
    FDrawColors: FXCompleteColorSet;
    FCustomColors: FXColorSets;
    FUseAccentAsForeground: boolean;
    FScale: real;
    FImage: FXIconSelect;
    FVertLayout: TLayout;
    FHorizLayout: TLayout;

    // Internal
    procedure ImageUpdated(Sender: TObject);

    // Getters

    // Setters
    procedure SetHorizLayout(const Value: TLayout);
    procedure SetImage(const Value: FXIconSelect);
    procedure SetScale(const Value: real);
    procedure SetVertLayout(const Value: TLayout);
    procedure SetUseAccentAsForeground(const Value: boolean);

  protected
    procedure PaintBuffer; override;

    // Internal
    procedure UpdateColors; override;
    procedure UpdateRects; override;

    // Scaler
    procedure ScaleChanged(Scaler: single); override;

    // State
    procedure InteractionStateChanged(AState: FXControlState); override;

  published
    // Custom Colors
    property CustomColors: FXColorSets read FCustomColors write FCustomColors stored true;

    // Style
    property UseAccentAsForeground: boolean read FUseAccentAsForeground write SetUseAccentAsForeground default false;

    // Props
    property Image: FXIconSelect read FImage write SetImage;
    property Scale: real read FScale write SetScale;
    property LayoutHorizontal: TLayout read FHorizLayout write SetHorizLayout default TLayout.Center;
    property LayoutVertical: TLayout read FVertLayout write SetVertLayout default TLayout.Center;

    // Default props
    property Align;
    property Font;
    property Transparent;
    property HitTest;
    property Opacity;
    property Constraints;
    property Anchors;
    property Hint;
    property ShowHint;
    property ParentShowHint;
    property TabStop default false;
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
    property OnDblClick;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
    property OnMouseUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;

  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    // Interface
    function Background: TColor; override;
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
  FImage.OnChange := ImageUpdated;

  FHorizLayout := TLayout.Center;
  FVertLayout := TLayout.Center;

  // Custom Color
  FCustomColors := FXColorSets.Create(Self);

  FDrawColors := FXCompleteColorSet.Create;

  // Sizing
  Height := 60;
  Width := 60;
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
  Redraw;
end;

procedure FXIconView.PaintBuffer;
begin
  // Background
  Color := FDrawColors.BackGround;
  PaintBackground;

  // Draw
  with Buffer do
    begin
      // Write
      Brush.Style := bsClear;
      if UseAccentAsForeground then
        Font.Color := FDrawColors.Accent
      else
        Font.Color := FDrawColors.ForeGround;
      FImage.DrawIcon(Buffer, IconRect);
    end;

  // Inherit
  inherited;
end;

procedure FXIconView.UpdateColors;
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

procedure FXIconView.ImageUpdated(Sender: TObject);
begin
  StandardUpdateLayout;
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
    TLayout.Beginning: ;
    TLayout.Center: IconRect.Offset((DrawRect.Width - IconRect.Width) div 2, 0);
    TLayout.Ending: IconRect.Offset(DrawRect.Width - IconRect.Width, 0);
  end;

  case FVertLayout of
    TLayout.Beginning: ;
    TLayout.Center: IconRect.Offset(0, (ClientRect.Height - IconRect.Height) div 2);
    TLayout.Ending: IconRect.Offset(0, DrawRect.Height - IconRect.Height);
  end;
end;

procedure FXIconView.ScaleChanged(Scaler: single);
begin
  UpdateRects;
  inherited;
end;

procedure FXIconView.SetHorizLayout(const Value: TLayout);
begin
  if FHorizLayout = Value then
    Exit;

  FHorizLayout := Value;
  StandardUpdateLayout;
end;

procedure FXIconView.SetImage(const Value: FXIconSelect);
begin
  if FImage = Value then
    Exit;

  FImage := Value;
  StandardUpdateDraw;
end;

procedure FXIconView.SetScale(const Value: real);
begin
  if FScale = Value then
    Exit;

  FScale := Value;
  StandardUpdateLayout;
end;

procedure FXIconView.SetUseAccentAsForeground(const Value: boolean);
begin
  if FUseAccentAsForeground = Value then
    Exit;

  FUseAccentAsForeground := Value;

  // Update
  StandardUpdateDraw;
end;

procedure FXIconView.SetVertLayout(const Value: TLayout);
begin
  if FVertLayout = Value then
    Exit;

  FVertLayout := Value;
  StandardUpdateLayout;
end;

end.
