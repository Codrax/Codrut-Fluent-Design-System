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
  FXIconView = class(FXGraphicControl, FXControl)
    private
      var IconRect: TRect;
      FScale: real;
      FImage: FXIconSelect;
      FVertLayout: FXLayout;
      FHorizLayout: FXLayout;
      FCustomColors: FXColorSets;
      FDrawColors: FXColorSet;

      //  Internal
      procedure UpdateColors;
      procedure UpdateRects;

      // Set properties
      procedure SetHorizLayout(const Value: FXLayout);
      procedure SetImage(const Value: FXIconSelect);
      procedure SetScale(const Value: real);
      procedure SetVertLayout(const Value: FXLayout);

      // Handle Messages
      procedure WM_LButtonUp(var Msg: TWMLButtonUp); message WM_LBUTTONUP;
      procedure WMSize(var Message: TWMSize); message WM_SIZE;

    protected
      procedure Paint; override;
      procedure Resize; override;
      procedure ChangeScale(M, D: Integer{$IF CompilerVersion > 29}; isDpiChange: Boolean{$ENDIF}); override;

      // State
      procedure ComponentCreated; override;
      procedure InteractionStateChanged(AState: FXControlState); override;

    published
      property CustomColors: FXColorSets read FCustomColors write FCustomColors stored true;
      property Image: FXIconSelect read FImage write SetImage;
      property Scale: real read FScale write SetScale;
      property LayoutHorizontal: FXLayout read FHorizLayout write SetHorizLayout default FXLayout.Center;
      property LayoutVertical: FXLayout read FVertLayout write SetVertLayout default FXLayout.Center;

      property OnClick;
      property OnDblClick;
      property Transparent;

      property Align;
      property Constraints;
      property Anchors;
      property Hint;
      property ShowHint;
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

procedure FXIconView.InteractionStateChanged(AState: FXControlState);
begin
  inherited;
end;

function FXIconView.IsContainer: Boolean;
begin
  Result := false;
end;

procedure FXIconView.UpdateTheme(const UpdateChildren: Boolean);
begin
  UpdateColors;
  UpdateRects;
  Invalidate;
end;

procedure FXIconView.UpdateColors;
begin
  // Access theme manager
  if FCustomColors.Enabled then
    begin
      // Custom Colors
      FDrawColors.LoadFrom(FCustomColors, ThemeManager.DarkTheme);
    end
  else
    begin
      FDrawColors.Assign( ThemeManager.SystemColor );
    end;
end;

procedure FXIconView.UpdateRects;
begin
  // Fill
  IconRect := ClientRect;

  // Scale
  IconRect.Width := round(IconRect.Width * Scale);
  IconRect.Height := round(IconRect.Height * Scale);

  if IconRect.Height < IconRect.Width then
    IconRect.Width := IconRect.Height
  else
    IconRect.Height := IconRect.Width;

  // Allign
  case FHorizLayout of
    FXLayout.Beginning: ;
    FXLayout.Center: IconRect.Offset((ClientRect.Width - IconRect.Width) div 2, 0);
    FXLayout.Ending: IconRect.Offset(ClientRect.Width - IconRect.Width, 0);
  end;

  case FVertLayout of
    FXLayout.Beginning: ;
    FXLayout.Center: IconRect.Offset(0, (ClientRect.Height - IconRect.Height) div 2);
    FXLayout.Ending: IconRect.Offset(0, ClientRect.Height - IconRect.Height);
  end;
end;

constructor FXIconView.Create(aOwner: TComponent);
begin
  inherited;
  FScale := 1;
  FImage := FXIconSelect.Create(Self);
  FImage.Enabled := true;

  FHorizLayout := FXLayout.Center;
  FVertLayout := FXLayout.Center;

  FCustomColors := FXColorSets.Create(Self);
  FDrawColors := FXColorSet.Create(Self);

  Width := 60;
  Height := 60;

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

function FXIconView.Background: TColor;
begin
  Result := FDrawColors.Background;
end;

procedure FXIconView.ChangeScale(M, D: Integer{$IF CompilerVersion > 29}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  UpdateRects;
end;

procedure FXIconView.ComponentCreated;
begin
  inherited;
  Invalidate;
end;

procedure FXIconView.Paint;
begin
  inherited;
  with Canvas do
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
      FImage.DrawIcon(Canvas, IconRect);
    end;
end;

procedure FXIconView.Resize;
begin
  inherited;
  UpdateRects;
  Invalidate;
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

procedure FXIconView.WMSize(var Message: TWMSize);
begin
  UpdateRects;
  Invalidate;
end;

procedure FXIconView.WM_LButtonUp(var Msg: TWMLButtonUp);
begin
  inherited;
end;

end.
