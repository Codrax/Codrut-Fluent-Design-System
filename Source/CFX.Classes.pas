unit CFX.Classes;

interface
  uses
    Vcl.Graphics, Classes, Types, CFX.Types, CFX.UIConsts, SysUtils,
    CFX.Graphics, CFX.VarHelpers, CFX.ThemeManager, Vcl.Controls,
    TypInfo, CFX.Linker;

  type
    // Base Clases
    FXComponent = class(TComponent)

    end;

    // Persistent
    TMPersistent = class(TPersistent)
      Owner : TPersistent;
      constructor Create(AOwner : TPersistent); overload; virtual;
    end;

    TAssignPersistent = class(TMPersistent)
    public
      procedure Assign(Source: TPersistent); override;
    end;

    // Padding
    FXPadding = class(TAssignPersistent)
    private
      FLeft,
      FTop,
      FRight,
      FBottom: integer;

      FOnChange: TNotifyEvent;

      procedure UpdateOwner;

      procedure SetLeft(const Value: integer);
      procedure SetBottom(const Value: integer);
      procedure SetRight(const Value: integer);
      procedure SetTop(const Value: integer);

    published
      property Left: integer read FLeft write SetLeft default 0;
      property Top: integer read FTop write SetTop default 0;
      property Right: integer read FRight write SetRight default 0;
      property Bottom: integer read FBottom write SetBottom default 0;

      property OnChange: TNotifyEvent read FOnChange write FOnChange;

      function PadWidth: integer;
      function PadHeight: integer;

      procedure ScaleChanged(Scaling: single);

    public
      constructor Create(AOwner : TPersistent); override;

      function ApplyTo(ARect: TRect): TRect;
    end;

    // Icon
    FXIconSelect = class(TMPersistent)
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
begin
  case IconType of
    FXIconType.Image: DrawImageInRect( Canvas, ARectangle, SelectPicture.Graphic, FXDrawMode.CenterFit );
    FXIconType.BitMap: DrawImageInRect( Canvas, ARectangle, SelectBitmap, FXDrawMode.CenterFit );
    FXIconType.ImageList: (* Work In Progress;*);
    FXIconType.SegoeIcon: begin
      TextDraw := SelectSegoe;

      with Canvas do
        begin
          FontPrevious := TFont.Create;
          try
            FontPrevious.Assign(Font);

            // Draw
            Font.Name := ThemeManager.IconFont;
            Font.Height := GetMaxFontHeight(Canvas, TextDraw, ARectangle.Width, ARectangle.Height);
            TextRect( ARectangle, TextDraw, [tfSingleLine, tfCenter, tfVerticalCenter] );

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

{ TMPersistent }

constructor TMPersistent.Create(AOwner: TPersistent);
begin
  inherited Create;
  Owner := AOwner;
end;


{ TAssignPersistent }

procedure TAssignPersistent.Assign(Source: TPersistent);
var
  PropList: PPropList;
  PropCount, i: Integer;
begin
  if Source is TAssignPersistent then
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

{ FXPadding }

function FXPadding.ApplyTo(ARect: TRect): TRect;
begin
  Result := ARect;

  Inc(Result.Left, FLeft);
  Inc(Result.Top, FTop);
  Dec(Result.Right, FRight);
  Dec(Result.Bottom, FBottom);
end;

constructor FXPadding.Create(AOwner: TPersistent);
begin
  inherited;
  FLeft := 0;
  FTop := 0;
  FRight := 0;
  FBottom := 0;
end;

function FXPadding.PadHeight: integer;
begin
  Result := Top + Bottom;
end;

function FXPadding.PadWidth: integer;
begin
  Result := Left + Right;
end;

procedure FXPadding.ScaleChanged(Scaling: single);
begin
  FLeft := trunc(FLeft * Scaling);
  FTop := trunc(FTop * Scaling);
  FRight := trunc(FRight * Scaling);
  FBottom := trunc(FBottom * Scaling);
end;

procedure FXPadding.SetBottom(const Value: integer);
begin
  FBottom := Value;

  UpdateOwner;
end;

procedure FXPadding.SetLeft(const Value: integer);
begin
  FLeft := Value;

  UpdateOwner;
end;

procedure FXPadding.SetRight(const Value: integer);
begin
  FRight := Value;

  UpdateOwner;
end;

procedure FXPadding.SetTop(const Value: integer);
begin
  FTop := Value;

  UpdateOwner;
end;

procedure FXPadding.UpdateOwner;
begin
  if (Owner <> nil) and Supports(Owner, FXControl) and not (csReading in TComponent(Owner).ComponentState) then
    (TComponent(Owner) as FXControl).UpdateTheme(true);

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

end.
