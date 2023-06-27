unit CFX.Labels;

interface
uses
  Classes,
  Messages,
  Windows,
  Vcl.Controls,
  Vcl.Graphics,
  Types,
  CFX.Colors,
  CFX.ThemeManager,
  CFX.Graphics,
  CFX.UIConsts,
  SysUtils,
  Vcl.StdCtrls,
  CFX.Classes,
  CFX.Hint,
  CFX.Controls,
  CFX.Linker,
  CFX.VarHelpers,
  CFX.Types;

  type
    FXLabel = class(TLabel, FXControl)
    private
      FCustomColors: FXColorSets;
      FDrawColors: FXColorSet;
      FPreferedColor: FXColorType;

      // Set
      procedure SetPreffered(const Value: FXColorType);

      // Update
      procedure UpdateColors;

    protected
      procedure Paint; override;

    published
      property CustomColors: FXColorSets read FCustomColors write FCustomColors stored true;
      property PrefferedColor: FXColorType read FPreferedColor write SetPreffered default FXColorType.Foreground;

      property Layout default tlCenter;

    public
      constructor Create(aOwner: TComponent); override;
      destructor Destroy; override;

      // Interface
      function IsContainer: Boolean;
      procedure UpdateTheme(const UpdateChildren: Boolean);

      function Background: TColor;
    end;

implementation

{ FXLabel }

function FXLabel.Background: TColor;
begin
  Result := FDrawColors.Background;
end;

constructor FXLabel.Create(aOwner: TComponent);
begin
  inherited;
  FDrawColors := FXColorSet.Create(Self);
  FCustomColors := FXColorSets.Create(Self);

  FPreferedColor := FXColorType.Foreground;

  Layout := tlCenter;

  Font.Name := LABEL_FONT_NAME;
  Font.Height := LABEL_FONT_HEIGHT;

  UpdateTheme(false);
end;

destructor FXLabel.Destroy;
begin
  FDrawColors.Free;
  FCustomColors.Free;
  inherited;
end;

function FXLabel.IsContainer: Boolean;
begin
  Result := false;
end;

procedure FXLabel.Paint;
begin
  // Choose Colors
  Color := FDrawColors.Background;
  Font.Color := ExtractColor( FDrawColors, FPreferedColor);;

  // Paint
  inherited;
end;

procedure FXLabel.SetPreffered(const Value: FXColorType);
begin
  if FPreferedColor <> Value then
    begin
      FPreferedColor := Value;

      Invalidate;
    end;
end;

procedure FXLabel.UpdateColors;
begin
  if FCustomColors.Enabled then
    begin
      FDrawColors.LoadFrom(FCustomColors, ThemeManager.DarkTheme)
    end
      else
        begin
          ThemeManager.LoadColorSet( FDrawColors );

          FDrawColors.BackGround := FXGraphicControl(Self).GetParentBackgroundColor(FDrawColors.BackGround);
        end;
end;

procedure FXLabel.UpdateTheme(const UpdateChildren: Boolean);
begin
  UpdateColors;
  Invalidate;
end;

end.
