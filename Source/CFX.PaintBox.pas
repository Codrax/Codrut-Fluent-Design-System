unit CFX.PaintBox;

interface

uses
  SysUtils,
  Classes,
  Types,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.ExtCtrls,
  CFX.VarHelpers,
  CFX.ThemeManager,
  CFX.Colors,
  CFX.Constants,
  CFX.Controls,
  CFX.Linker;

type
  FXPaintBox = class(FXWindowsControl, FXControl)
  private
    FDarkTintOpacity,
    FWhiteTintOpacity: integer;

    FCustomColors: FXColorSets;
    FDrawColors: FXColorSet;

    FEnableTinting: boolean;

    FOnDraw: TNotifyEvent;

    procedure SetTinting(const Value: boolean);
    procedure SetDarkTint(const Value: integer);
    procedure SetWhiteTint(const Value: integer);
    procedure SetCustomColor(const Value: FXColorSets);

    procedure UpdateColors;

  protected
    procedure PaintBuffer; override;

  published
    property Align;
    property PaddingFill;
    property Anchors;
    property AutoSize;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Touch;
    property Visible;
    property OnClick;

    property OnDraw: TNotifyEvent read FOnDraw write FOnDraw;

    property EnableTinting: boolean read FEnableTinting write SetTinting default true;
    property DarkTintOpacity: integer read FDarkTintOpacity write SetDarkTint default 75;
    property WhiteTintOpacity: integer read FWhiteTintOpacity write SetWhiteTint default 200;

    property CustomColors: FXColorSets read FCustomColors write SetCustomColor;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnGesture;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Inflate(up,right,down,lft: integer);

    // Interface
    function IsContainer: Boolean;
    procedure UpdateTheme(const UpdateChildren: Boolean);

    function Background: TColor;
  end;

implementation

{ FXPaintBox }

function FXPaintBox.Background: TColor;
begin
  Result := FDrawColors.Background;
end;

constructor FXPaintBox.Create(AOwner: TComponent);
begin
  inherited;
  FDrawColors := FXColorSet.Create;
  FCustomColors := FXColorSets.Create(false);
  with FCustomColors do
    begin
      DarkBackground := clBlack;
      LightBackground := clWhite;
      Accent := ThemeManager.AccentColor;
    end;

  ControlStyle := ControlStyle + [csReplicatable, csPannable];

  // Size
  Width := 150;
  Height := 200;

  // Tintin
  FEnableTinting := false;

  FWhiteTintOpacity := LIGHT_TINT_OPACITY;
  FDarkTintOpacity := DARK_TINT_OPACITY;

  // Theme
  UpdateColors;
end;

procedure FXPaintBox.UpdateColors;
begin
  // Access theme manager
  FDrawColors.Assign( ThemeManager.SystemColor );
  if FCustomColors.Enabled then
    // Custom Colors
    FDrawColors.LoadFrom(FCustomColors, ThemeManager.DarkTheme);
end;

destructor FXPaintBox.Destroy;
begin
  FreeAndNil(FCustomColors);
  FreeAndNil(FDrawColors);
  inherited;
end;

procedure FXPaintBox.Inflate(up, right, down, lft: integer);
begin
  // UP
  Top := Top - Up;
  Height := Height + Up;
  // RIGHT
  Width := Width + right;
  // DOWN
  Height := Height + down;
  // LEFT
  Left := Left - lft;
  Width := Width + lft;
end;

function FXPaintBox.IsContainer: Boolean;
begin
  Result := false;
end;

procedure FXPaintBox.PaintBuffer;
var
  //Pict: TBitMap;
  DrawRect{, ImageRect}: Trect;
begin
  // Background
  Color := FDrawColors.BackGround;
  PaintBackground;

  // Draw
  if csDesigning in ComponentState then
    with Buffer do
    begin
      Pen.Color := clAqua;
      Pen.Style := psDash;
      Brush.Style := bsSolid;
      Rectangle(ClientRect);

      Exit;
    end;

  // Default color
  with Buffer do
    begin
      Brush.Color := FDrawColors.BackGround;
      FillRect(ClipRect);
    end;

  // On Paint
  if Assigned(OnDraw) then
    OnDraw(Self);

  // Tint
  with Buffer do
    if EnableTinting then
      begin
        DrawRect := ClipRect;
        DrawRect.Inflate(1, 1);

        if ThemeManager.DarkTheme then
          GDITint( DrawRect, FDrawColors.BackGround, FDarkTintOpacity )
        else
          GDITint( DrawRect, FDrawColors.BackGround, FWhiteTintOpacity );
      end;

  inherited;
end;

procedure FXPaintBox.SetCustomColor(const Value: FXColorSets);
begin
  FCustomColors := Value;

  UpdateTheme(false);
end;

procedure FXPaintBox.SetDarkTint(const Value: integer);
begin
  FDarkTintOpacity := Value;

  Invalidate;
end;

procedure FXPaintBox.SetTinting(const Value: boolean);
begin
  FEnableTinting := Value;

  Invalidate;
end;

procedure FXPaintBox.SetWhiteTint(const Value: integer);
begin
  FWhiteTintOpacity := Value;

  Invalidate;
end;

procedure FXPaintBox.UpdateTheme(const UpdateChildren: Boolean);
begin
  UpdateColors;

  Invalidate;
end;

end.
