unit CFX.Picture;

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
  CFX.Types,
  CFX.VarHelpers,
  CFX.Linker,
  CFX.Controls;

type
  FXPicture = class(FXWindowsControl)
  private
    var DrawRect: TRect;
    procedure SetTileMargins(const Value: integer);
    procedure SetClipPicture(const Value: boolean);
    procedure SetPictureOpacity(const Value: FXPercent);
    var PictureRects: TArray<TRect>;
    FDrawColors: FXCompleteColorSet;
    FCustomColors: FXColorSets;
    FPicture: TPicture;

    FPictureOpacity: FXPercent;
    FClipPicture: boolean;
    FFill: FXPictureContentFill;
    FVertLayout: TLayout;
    FHorizLayout: TLayout;
    FPictureMargins: integer;
    FTile: boolean;
    FTileFlags: TRectLayoutTileFlags;
    FTileMargins: integer;

    // Internal
    procedure ImageUpdated(Sender: TObject);

    // Getters

    // Setters
    procedure SetHorizLayout(const Value: TLayout);
    procedure SetPicture(const Value: TPicture);
    procedure SetVertLayout(const Value: TLayout);
    procedure SetPictureMargins(const Value: integer);
    procedure SetFill(const Value: FXPictureContentFill);
    procedure SetTile(const Value: boolean);
    procedure SetTileFlags(const Value: TRectLayoutTileFlags);

  protected
    procedure PaintBuffer; override;

    // Internal
    procedure UpdateColors; override;
    procedure UpdateRects; override;

    // Task
    procedure DoDrawPicture; virtual;

    // Scaler
    procedure ScaleChanged(Scaler: single); override;

    // State
    procedure InteractionStateChanged(AState: FXControlState); override;

  published
    // Custom Colors
    property CustomColors: FXColorSets read FCustomColors write FCustomColors stored true;

    // Props
    property Picture: TPicture read FPicture write SetPicture;
    property PictureOpacity: FXPercent read FPictureOpacity write SetPictureOpacity;
    property ClipPicture: boolean read FClipPicture write SetClipPicture default true;
    property Fill: FXPictureContentFill read FFill write SetFill default FXPictureContentFill.Fit;
    property LayoutHorizontal: TLayout read FHorizLayout write SetHorizLayout default TLayout.Center;
    property LayoutVertical: TLayout read FVertLayout write SetVertLayout default TLayout.Center;
    property PictureMargins: integer read FPictureMargins write SetPictureMargins default 0;
    property Tile: boolean read FTile write SetTile default false;
    property TileFlags: TRectLayoutTileFlags read FTileFlags write SetTileFlags;
    property TileMargins: integer read FTileMargins write SetTileMargins default 0;

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

function FXPicture.Background: TColor;
begin
  Result := FDrawColors.BackGround;
end;

constructor FXPicture.Create(aOwner: TComponent);
begin
  inherited;
  // Props
  FPicture := TPicture.Create;
  FPicture.OnChange := ImageUpdated;

  PictureOpacity := 100;
  FClipPicture := true;

  FFill := FXPictureContentFill.Fit;

  FHorizLayout := TLayout.Center;
  FVertLayout := TLayout.Center;

  FTileFlags := [TRectLayoutTileFlag.ExtendX, TRectLayoutTileFlag.ExtendY];

  // Custom Color
  FCustomColors := FXColorSets.Create(Self);

  FDrawColors := FXCompleteColorSet.Create;

  // Sizing
  Height := 100;
  Width := 100;
end;

destructor FXPicture.Destroy;
begin
  FreeAndNil( FPicture );
  FreeAndNil( FCustomColors );
  FreeAndNil( FDrawColors );
  inherited;
end;

procedure FXPicture.DoDrawPicture;
var
  I: integer;
  Bitmap: TBitMap;
  FRect: TRect;
begin
  if (Picture.Graphic = nil) or Picture.Graphic.Empty then
    Exit;

  if not ClipPicture then
    // Standard Draw
    begin
      for I := 0 to High( PictureRects ) do
        Buffer.StretchDraw( PictureRects[I], Picture.Graphic, PictureOpacity.ToByte );
    end
  else
    // Clip Image Drw
    begin
      for I := 0 to High(PictureRects) do
        begin
          Bitmap := TBitMap.Create(DrawRect.Width, DrawRect.Height);
          Bitmap.PixelFormat := pf32bit;
          Bitmap.Transparent := true;

          const PIXEL_BYTE_SIZE = 4;

          // Fill image with
          for var Y := 0 to Bitmap.Height - 1 do
            FillMemory(Bitmap.ScanLine[Y], PIXEL_BYTE_SIZE * Bitmap.Width, 0);

          Bitmap.Canvas.Lock;
          try
            FRect := PictureRects[I];
            FRect.Offset( -DrawRect.Left, -DrawRect.Top );

            Bitmap.Canvas.StretchDraw(FRect, Picture.Graphic, 255); // Full opacity for temp

            // Image has no alpha channel, set A bytes to 255
            if not Picture.Graphic.Transparent then begin
              const RectZone = TRect.Intersect(Bitmap.Canvas.ClipRect, FRect);

              for var Y := RectZone.Top to RectZone.Bottom-1 do begin
                // Line
                const Pos: PByte = Bitmap.ScanLine[Y];

                // Start left
                for var X := RectZone.Left to RectZone.Right-1 do
                  Pos[X * PIXEL_BYTE_SIZE + 3] := 255;
              end;
            end;

            // Draw
            //Canvas.StretchDraw(Rect, BitMap, Opacity)
            Buffer.Draw(DrawRect.Left, DrawRect.Top, BitMap, PictureOpacity.ToByte);
          finally
            Bitmap.Canvas.Unlock;
            BitMap.Free;
          end;
        end;
    end;
end;

procedure FXPicture.InteractionStateChanged(AState: FXControlState);
begin
  inherited;
  Redraw;
end;

procedure FXPicture.PaintBuffer;
begin
  // Background
  Color := FDrawColors.BackGround;
  PaintBackground;

  // Draw
  with Buffer do begin
    // Write
    Brush.Style := bsClear;

    // Draw
    DoDrawPicture;
  end;

  // Inherit
  inherited;
end;

procedure FXPicture.UpdateColors;
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

procedure FXPicture.ImageUpdated(Sender: TObject);
begin
  StandardUpdateLayout;
end;

procedure FXPicture.UpdateRects;
var
  Layout: TRectLayout;
begin
  // Rect
  DrawRect := ClientRect;

  // Clear
  PictureRects := [];

  // Settings
  Layout := TRectLayout.New;

  Layout.LayoutHorizontal := FHorizLayout;
  Layout.LayoutVertical := FVertLayout;

  Layout.ContentFill := TRectLayoutContentFill(Fill);

  Layout.Tile := Tile;
  Layout.TileFlags := TileFlags;

  Layout.MarginTile := TileMargins;
  Layout.MarginSelf := PictureMargins;

  // Calculate rect(s)
  PictureRects := RectangleLayouts(TSize.Create(Picture.Width, Picture.Height), DrawRect, Layout);
end;

procedure FXPicture.ScaleChanged(Scaler: single);
begin
  UpdateRects;
  inherited;
end;

procedure FXPicture.SetClipPicture(const Value: boolean);
begin
  if FClipPicture = Value then
    Exit;
  FClipPicture := Value;

  StandardUpdateLayout;
end;

procedure FXPicture.SetFill(const Value: FXPictureContentFill);
begin
  if FFill = Value then
    Exit;
  FFill := Value;

  StandardUpdateLayout;
end;

procedure FXPicture.SetHorizLayout(const Value: TLayout);
begin
  if FHorizLayout = Value then
    Exit;
  FHorizLayout := Value;

  StandardUpdateLayout;
end;

procedure FXPicture.SetPicture(const Value: TPicture);
begin
  if FPicture = Value then
    Exit;
  FPicture.Assign(Value);

  StandardUpdateDraw;
end;

procedure FXPicture.SetPictureMargins(const Value: integer);
begin
  if FPictureMargins = Value then
    Exit;
  FPictureMargins := Value;

  StandardUpdateLayout;
end;

procedure FXPicture.SetPictureOpacity(const Value: FXPercent);
begin
  if FPictureOpacity = Value then
    Exit;
  FPictureOpacity := Value;

  StandardUpdateDraw;
end;

procedure FXPicture.SetTile(const Value: boolean);
begin
  if FTile = Value then
    Exit;
  FTile := Value;

  StandardUpdateLayout;
end;

procedure FXPicture.SetTileFlags(const Value: TRectLayoutTileFlags);
begin
  if FTileFlags = Value then
    Exit;
  FTileFlags := Value;

  StandardUpdateLayout;
end;

procedure FXPicture.SetTileMargins(const Value: integer);
begin
  if FTileMargins = Value then
    Exit;
  FTileMargins := Value;

  StandardUpdateLayout;
end;

procedure FXPicture.SetVertLayout(const Value: TLayout);
begin
  if FVertLayout = Value then
    Exit;
  FVertLayout := Value;

  StandardUpdateLayout;
end;

end.
