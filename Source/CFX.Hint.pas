unit CFX.Hint;

interface
uses
  Classes, Types, Winapi.Windows, Winapi.Messages, Vcl.Controls, Vcl.Graphics,
  CFX.Constants, SysUtils, CFX.ThemeManager, CFX.Colors, CFX.Graphics,
  CFX.ToolTip, Vcl.ExtCtrls;

type
  FXHintPopup = class
  private
    FHintClass: FXCustomTooltip;
    FAutoHideTimer: TTimer;

    FIsVisible: boolean;
    FCenterToPosition: boolean;

    FText: string;
    FPosition: TPoint;
    FDuration: integer;
    FAutoHide: boolean;
    FMaxWidth: integer;

    procedure HideProc(Sender: TObject);
    procedure ApplyColor;
    function MakeHintRect: TRect;
    procedure SetAutoHide(const Value: boolean);
    function GetFont: TFont;

  public
    constructor Create;
    destructor Destroy; override;

    property Font: TFont read GetFont;

    property Text: string read FText write FText;
    property Duration: integer read FDuration write FDuration;
    property Position: TPoint read FPosition write FPosition;
    property AutoHide: boolean read FAutoHide write SetAutoHide;
    property MaxWidth: integer read FMaxWidth write FMaxWidth default 200;
    property CenterToPosition: boolean read FCenterToPosition write FCenterToPosition;

    property IsVisible: boolean read FIsVisible;

    procedure Hide;

    procedure Show;
    procedure ShowAtPoint(APoint: TPoint);
  end;

implementation

{ FXHintPopup }

procedure FXHintPopup.ApplyColor;
begin
  if ThemeManager.DarkTheme then
    FXDarkTooltip(FHintClass).ApplyColor
  else
    FXLightTooltip(FHintClass).ApplyColor;
end;

constructor FXHintPopup.Create;
begin
  inherited;
  FDuration := 1000;
  FMaxWidth := 200;
  FAutoHide := true;
  FText := TEXT_DEFAULT_GENERIC;
  FCenterToPosition := false;

  // Classes
  FHintClass := FXCustomTooltip.Create(nil);
  FAutoHideTimer := TTimer.Create(nil);
  with FAutoHideTimer do
    begin
      Enabled := false;

      OnTimer := HideProc;
    end;
end;

destructor FXHintPopup.Destroy;
begin
  FreeAndNil( FHintClass );
  FAutoHideTimer.Enabled := false;
  FreeAndNil( FAutoHideTimer );
end;

function FXHintPopup.GetFont: TFont;
begin
  Result := FHintClass.Font;
end;

procedure FXHintPopup.Hide;
begin
  FIsVisible := false;
  FHintClass.Hide;
end;

procedure FXHintPopup.HideProc(Sender: TObject);
begin
  if FAutoHide then
    Hide;

  FAutoHideTimer.Enabled := false;
end;

function FXHintPopup.MakeHintRect: TRect;
begin
  Result := FHintClass.CalcHintRect( FMaxWidth, FText, nil );
  Result.Offset(FPosition);

  if FCenterToPosition then
    Result.Offset(-Result.Width div 2, -Result.Height div 2);
end;

procedure FXHintPopup.SetAutoHide(const Value: boolean);
begin
  FAutoHide := Value;

  if Value and IsVisible then
    FAutoHideTimer.Enabled := FAutoHide;
end;

procedure FXHintPopup.Show;
begin
  // Reset Timer
  FAutoHideTimer.Enabled := false;

  // Auto Hide
  FAutoHideTimer.Interval := FDuration;
  FAutoHideTimer.Enabled := FAutoHide;

  // Theme
  ApplyColor;

  // Show
  FIsVisible := true;
  FHintClass.Visible := true;
  FHintClass.ActivateHint( MakeHintRect, FText )
end;

procedure FXHintPopup.ShowAtPoint(APoint: TPoint);
begin
  FPosition := APoint;
  Show;
end;

end.
