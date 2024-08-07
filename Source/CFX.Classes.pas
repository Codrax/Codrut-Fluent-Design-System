unit CFX.Classes;

interface
uses
  Vcl.Graphics, Classes, Types, CFX.Types, CFX.Constants, SysUtils,
  CFX.Graphics, CFX.VarHelpers, CFX.ThemeManager, Vcl.Controls,
  TypInfo, CFX.Linker, CFX.Colors;

type
  // Base Clases
  FXComponent = class(TComponent)

  end;

  // Persistent
  FXPersistent = class(TPersistent)
    Owner : TPersistent;
    constructor Create(AOwner : TPersistent); overload; virtual;
  end;

  FXAssignPersistent = class(FXPersistent)
  public
    procedure Assign(Source: TPersistent); override;
  end;

  // Side values
  FXSideValues = class(FXPersistent)
  private
    FLeft,
    FTop,
    FRight,
    FBottom,
    FAround,
    FHorizontal,
    FVertical: integer;

    FOnChange: TNotifyEvent;

    // Setters
    procedure SetLeft(const Value: integer);
    procedure SetBottom(const Value: integer);
    procedure SetRight(const Value: integer);
    procedure SetTop(const Value: integer);
    procedure SetAround(const Value: integer);
    procedure SetHorizontal(const Value: integer);
    procedure SetVertical(const Value: integer);

  protected
    procedure Updated; virtual;

  published
    property Left: integer read FLeft write SetLeft default 0;
    property Top: integer read FTop write SetTop default 0;
    property Right: integer read FRight write SetRight default 0;
    property Bottom: integer read FBottom write SetBottom default 0;

    property Around: integer read FAround write SetAround default 0;
    property Horizontal: integer read FHorizontal write SetHorizontal default 0;
    property Vertical: integer read FVertical write SetVertical default 0;

    // Events
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    procedure ScaleChanged(Scaling: single);

  public
    constructor Create(AOwner : TPersistent); override;

    // Assignment
    procedure AssignTo(Dest: TPersistent); override;

    // Data
    function AbsoluteLeft: integer;
    function AbsoluteRight: integer;
    function AbsoluteHorizontal: integer;
    function AbsoluteTop: integer;
    function AbsoluteBottom: integer;
    function AbsoluteVertical: integer;

    // Utils
    function RectangleExpand(ARect: TRect): TRect;
    function RectangleInflate(ARect: TRect): TRect;
  end;

  // Margins and paddings
  FXControlSideValues = class(FXSideValues)
  protected
    procedure Updated; override;
  end;

  FXPadding = FXSideValues;
  FXMargins = FXSideValues;

  // Size class
  FXPointGeneric = class(FXAssignPersistent)
  private
    function GetPoint: TPoint; virtual;
    procedure SetPoint(const Value: TPoint); virtual;

  protected
    // Getters
    function GetX: integer; virtual; abstract;
    function GetY: integer; virtual; abstract;

    // Setters
    procedure SetX(const Value: integer); virtual; abstract;
    procedure SetY(const Value: integer); virtual; abstract;

    // Prop types
    property X: integer read GetX write SetX;
    property Y: integer read GetY write SetY;
    property Width: integer read GetX write SetX;
    property Height: integer read GetY write SetY;

    property Point: TPoint read GetPoint write SetPoint;
  end;

  // Blur Settings
  FXBlurSettings = class(FXPersistent)
  private
    FEnabled: boolean;

    FBlurVersion: FXBlurVersion;

    FTint: boolean;
    FTintLightOpacity: byte;
    FTintDarkOpacity: byte;
    FTintColors: FXColorSets;

    // Update
    procedure UpdateParent;

    // Setters
    procedure SetTintOpacity(const Index: Integer; const Value: byte);
    procedure SetTint(const Value: boolean);

  published
    property Enabled: boolean read FEnabled write FEnabled;

    property BlurVersion: FXBlurVersion read FBlurVersion write FBlurVersion;

    property Tint: boolean read FTint write SetTint;
    property TintLightOpacity: byte index 0 read FTintLightOpacity write SetTintOpacity;
    property TintDarkOpacity: byte index 1 read FTintDarkOpacity write SetTintOpacity;
    property TintColors: FXColorSets read FTintColors write FTintColors;

  public
    constructor Create(AOwner : TPersistent); override;
    destructor Destroy; override;
  end;

  // Icon
  FXIconSelect = class(FXPersistent)
  private
    FEnabled: boolean;

    FType: FXIconType;
    FPicture: TPicture;
    FBitMap: TBitMap;
    FSegoeText: string;
    FImageIndex: integer;

    // Settters
    procedure SetBitMap(const Value: TBitMap);
    procedure SetPicture(const Value: TPicture);
    procedure SetImageIndex(const Value: integer);
    procedure SetSegoe(const Value: string);

    // Update
    procedure UpdateParent;

  published
    property Enabled: boolean read FEnabled write FEnabled default False;
    property IconType: FXIconType read FType write FType default FXIconType.SegoeIcon;

    property SelectPicture: TPicture read FPicture write SetPicture;
    property SelectBitmap: TBitMap read FBitMap write SetBitMap;
    property SelectSegoe: string read FSegoeText write SetSegoe;
    property SelectImageIndex: integer read FImageIndex write SetImageIndex default -1;

  public
    constructor Create(AOwner : TPersistent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    procedure DrawIcon(Canvas: TCanvas; ARectangle: TRect);

    procedure FreeUnusedAssets;
  end;

implementation

{ FXIconSelect }

procedure FXIconSelect.Assign(Source: TPersistent);
begin
  with FXIconSelect(Source) do
    begin
      Self.FEnabled := FEnabled;

      Self.FType := FType;

      Self.FPicture.Assign(FPicture);
      Self.FBitMap.Assign(FBitMap);
      Self.FSegoeText := FSegoeText;
      Self.FImageIndex := FImageIndex;
    end;
end;

constructor FXIconSelect.Create(AOwner : TPersistent);
begin
  inherited;
  Enabled := false;

  FPicture := TPicture.Create;
  FBitMap := TBitMap.Create;

  IconType := FXIconType.SegoeIcon;
  FSegoeText := SEGOE_UI_STAR;
end;

destructor FXIconSelect.Destroy;
begin
  FreeAndNil(FPicture);
  FreeAndNil(FBitMap);

  inherited;
end;

procedure FXIconSelect.DrawIcon(Canvas: TCanvas; ARectangle: TRect);
var
  TextDraw: string;
  FontPrevious: TFont;
  I: integer;
begin
  case IconType of
    FXIconType.Image: DrawImageInRect( Canvas, ARectangle, SelectPicture.Graphic, FXDrawMode.CenterFit );
    FXIconType.BitMap: DrawImageInRect( Canvas, ARectangle, SelectBitmap, FXDrawMode.CenterFit );
    FXIconType.ImageList: (* Work In Progress;*);
    FXIconType.SegoeIcon: begin
      with Canvas do
        begin
          FontPrevious := TFont.Create;
          try
            FontPrevious.Assign(Font);

            // Draw
            Font.Name := ThemeManager.IconFont;
            Font.Height := GetMaxFontHeight(Canvas, Copy(SelectSegoe, 1, 1), ARectangle.Width, ARectangle.Height);
            for I := Low(SelectSegoe) to High(SelectSegoe) do
              begin
                TextDraw := SelectSegoe[I];
                TextRect( ARectangle, TextDraw, [tfSingleLine, tfCenter, tfVerticalCenter] );
              end;

            Font.Assign(FontPrevious);
          finally
            FontPrevious.Free;
          end;
        end;
    end;
  end;
end;

procedure FXIconSelect.FreeUnusedAssets;
begin
  if (IconType <> FXIconType.Image) and (FPicture <> nil) and (not FPicture.Graphic.Empty) then
    FPicture.Free;

  if (IconType <> FXIconType.BitMap) and (FBitMap <> nil) and (not FBitMap.Empty) then
    FBitMap.Free;
end;

procedure FXIconSelect.SetBitMap(const Value: TBitMap);
begin
  if FBitmap = nil then
    FBitmap := TBitMap.Create;

  FBitmap.Assign(value);

  // Update
  if IconType = FXIconType.BitMap then
    UpdateParent;
end;

procedure FXIconSelect.SetImageIndex(const Value: integer);
begin
  FImageIndex := Value;

  // Update
  if IconType = FXIconType.ImageList then
    UpdateParent;
end;

procedure FXIconSelect.SetPicture(const Value: TPicture);
begin
  if FPicture = nil then
    FPicture := TPicture.Create;

  FPicture.Assign(Value);

  // Update
  if IconType = FXIconType.Image then
    UpdateParent;
end;

procedure FXIconSelect.SetSegoe(const Value: string);
begin
  FSegoeText := Value;

  // Update
  if IconType = FXIconType.SegoeIcon then
    UpdateParent;
end;

procedure FXIconSelect.UpdateParent;
begin
  if Owner is TControl then
    TControl(Owner).Invalidate;
end;

{ FXPersistent }

constructor FXPersistent.Create(AOwner: TPersistent);
begin
  inherited Create;
  Owner := AOwner;
end;


{ FXAssignPersistent }

procedure FXAssignPersistent.Assign(Source: TPersistent);
var
  PropList: PPropList;
  PropCount, i: Integer;
begin
  if Source is FXAssignPersistent then
  begin
    PropCount := GetPropList(Source.ClassInfo, tkProperties, nil);
    if PropCount > 0 then
    begin
      GetMem(PropList, PropCount * SizeOf(PPropInfo));
      try
        GetPropList(Source.ClassInfo, tkProperties, PropList);
        for i := 0 to PropCount - 1 do
          SetPropValue(Self, string(PropList^[i]^.Name), GetPropValue(Source, string(PropList^[i]^.Name)));
      finally
        FreeMem(PropList);
      end;
    end;
  end
  else
    inherited Assign(Source);
end;

{ FXSideValues }

function FXSideValues.AbsoluteBottom: integer;
begin
  Result := FBottom + FVertical + FAround;
end;

function FXSideValues.AbsoluteHorizontal: integer;
begin
  Result := FLeft + FRight + FHorizontal + FAround;
end;

function FXSideValues.AbsoluteLeft: integer;
begin
  Result := FLeft + FHorizontal + FAround;
end;

function FXSideValues.AbsoluteRight: integer;
begin
  Result := FRight + FHorizontal + FAround;
end;

function FXSideValues.AbsoluteTop: integer;
begin
  Result := FTop + FVertical + FAround;
end;

function FXSideValues.AbsoluteVertical: integer;
begin
  Result := FTop + FBottom + FVertical + FAround;
end;

procedure FXSideValues.AssignTo(Dest: TPersistent);
begin
  if Dest is FXSideValues then
  begin
    const Destination = FXSideValues(Dest);

    Destination.FLeft := FLeft;
    Destination.FTop := FTop;
    Destination.FRight := FRight;
    Destination.FBottom := FBottom;
    Destination.FAround := FAround;
    Destination.FHorizontal := FHorizontal;
    Destination.FVertical := FVertical;

    // Notify
    Updated;
  end
  else
    inherited Assign(Dest);
end;

function FXSideValues.RectangleExpand(ARect: TRect): TRect;
begin
  Result := ARect;
  Result.Inflate(AbsoluteLeft, AbsoluteTop, AbsoluteRight, AbsoluteBottom);
end;

function FXSideValues.RectangleInflate(ARect: TRect): TRect;
begin
  Result := ARect;
  Result.Inflate(-AbsoluteLeft, -AbsoluteTop, -AbsoluteRight, -AbsoluteBottom);
end;

constructor FXSideValues.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FLeft := 0;
  FTop := 0;
  FRight := 0;
  FBottom := 0;

  FAround := 0;
  FHorizontal := 0;
  FVertical := 0;
end;

procedure FXSideValues.ScaleChanged(Scaling: single);
begin
  FLeft := trunc(FLeft * Scaling);
  FTop := trunc(FTop * Scaling);
  FRight := trunc(FRight * Scaling);
  FBottom := trunc(FBottom * Scaling);

  FAround := trunc(FAround * Scaling);
  FHorizontal := trunc(FHorizontal * Scaling);
  FVertical := trunc(FVertical * Scaling);
end;

procedure FXSideValues.SetAround(const Value: integer);
begin
  if FAround = Value then
    Exit;

  FAround := Value;
  Updated;
end;

procedure FXSideValues.SetBottom(const Value: integer);
begin
  if FBottom = Value then
    Exit;

  FBottom := Value;
  Updated;
end;

procedure FXSideValues.SetHorizontal(const Value: integer);
begin
  if FHorizontal = Value then
    Exit;

  FHorizontal := Value;
  Updated;
end;

procedure FXSideValues.SetLeft(const Value: integer);
begin
  if FLeft = Value then
    Exit;

  FLeft := Value;
  Updated;
end;

procedure FXSideValues.SetRight(const Value: integer);
begin
  if FRight = Value then
    Exit;

  FRight := Value;
  Updated;
end;

procedure FXSideValues.SetTop(const Value: integer);
begin
  if FTop = Value then
    Exit;

  FTop := Value;
  Updated;
end;

procedure FXSideValues.SetVertical(const Value: integer);
begin
  if FVertical = Value then
    Exit;

  FVertical := Value;
  Updated;
end;

procedure FXSideValues.Updated;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{ FXBlurSettings }

constructor FXBlurSettings.Create(AOwner: TPersistent);
begin
  inherited;
  FBlurVersion := FXBlurVersion.WallpaperBlurred;

  FTint := true;
  FTintLightOpacity := LIGHT_TINT_OPACITY;
  FTintDarkOpacity := DARK_TINT_OPACITY;

  FTintColors := FTintColors.Create(AOwner);
end;

destructor FXBlurSettings.Destroy;
begin
  FreeAndNil( FTintColors );
  inherited;
end;

procedure FXBlurSettings.SetTint(const Value: boolean);
begin
  if FTint <> Value then
    begin
      FTint := Value;

      UpdateParent;
    end;
end;

procedure FXBlurSettings.SetTintOpacity(const Index: Integer;
  const Value: byte);
begin
  if Index = 1 then
    FTintDarkOpacity := Value
  else
    FTintLightOpacity := Value;

  UpdateParent;
end;

procedure FXBlurSettings.UpdateParent;
begin
  if Owner is TControl then
    TControl(Owner).Invalidate;
end;

{ FXPointGeneric }

function FXPointGeneric.GetPoint: TPoint;
begin
  Result := TPoint.Create(X, Y);
end;

procedure FXPointGeneric.SetPoint(const Value: TPoint);
begin
  X := Value.X;
  Y := Value.Y;
end;

{ FXControlSideValues }

procedure FXControlSideValues.Updated;
begin
  if (Owner <> nil) and Supports(Owner, FXControl) and not (csReading in TComponent(Owner).ComponentState) then
    (TComponent(Owner) as FXControl).UpdateTheme(true);

  inherited;
end;

end.
