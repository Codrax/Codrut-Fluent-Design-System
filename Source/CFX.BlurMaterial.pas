unit CFX.BlurMaterial;

interface

uses
  SysUtils,
  Winapi.Windows,
  Winapi.Messages,
  Classes,
  Types,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.ExtCtrls,
  CFX.Graphics,
  CFX.VarHelpers,
  Vcl.Forms,
  DateUtils,
  System.Threading,
  System.Win.Registry,
  IOUtils,
  CFX.Utilities,
  CFX.ThemeManager,
  CFX.Classes,
  CFX.Math,
  CFX.GDI,
  CFX.Files,
  CFX.Messages,
  CFX.Colors,
  CFX.UIConsts,
  CFX.Types,
  CFX.Controls,
  CFX.Linker,
  Vcl.Imaging.GIFImg,
  Vcl.Imaging.pngimage,
  Vcl.Imaging.jpeg;

type
  FXBlurMaterial = class(FXBufferGraphicControl, FXControl)
  private
    FRefreshMode: FXGlassRefreshMode;
    Tick: TTimer;
    FDrawing: Boolean;
    FInvalidateAbove: boolean;
    FVersion: FXBlurVersion;

    FDarkTintOpacity,
    FWhiteTintOpacity: integer;

    FCustomColors: FXColorSets;
    FDrawColors: FXColorSet;

    FEnableTinting: boolean;

    procedure TimerExecute(Sender: TObject);
    procedure SetRefreshMode(const Value: FXGlassRefreshMode);
    procedure SetVersion(const Value: FXBlurVersion);

    function ImageTypeExists(ImgType: FXBlurVersion): boolean;
    procedure SetTinting(const Value: boolean);
    procedure SetDarkTint(const Value: integer);
    procedure SetWhiteTint(const Value: integer);
    procedure SetCustomColor(const Value: FXColorSets);

    procedure CustomColorGet;

  protected
    function DestRect: TRect;
    procedure PaintBuffer; override;

    procedure InteractionStateChanged(AState: FXControlState); override;

    procedure OnVisibleChange(var Message : TMessage); message CM_VISIBLECHANGED;

    // Handle messages
    procedure WndProc(var Message: TMessage); override;

  published
    property Align;
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

    property OnPaint;
    property OnPaintBuffer;

    property Version: FXBlurVersion read FVersion write SetVersion default FXBlurVersion.WallpaperBlurred;
    property RefreshMode: FXGlassRefreshMode read FRefreshMode write SetRefreshMode default FXGlassRefreshMode.Manual;
    property InvalidateAbove: boolean read FInvalidateAbove write FInvalidateAbove default false;
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

    procedure FormMoveSync;

    procedure SyncroniseImage;
    procedure RebuildImage;
    procedure ReDraw;

    // Interface
    function IsContainer: Boolean;
    procedure UpdateTheme(const UpdateChildren: Boolean);

    function Background: TColor;
  end;

  procedure GetWallpaper;
  procedure GetBlurredScreen(darkmode: boolean);
  function GetWallpaperName(ScreenIndex: integer; TranscodedDefault: boolean = false): string;
  function GetWallpaperSize: integer;
  function GetWallpaperSetting: TWallpaperSetting;
  function GetCurrentExtension: string;
  procedure CreateBySignature(var Wallpaper: TGraphic; Sign: TFileType);
  procedure CreateByExtension(var Wallpaper: TGraphic; Extension: string);

const
  DESKTOP_PROGRAM = 'Program Manager';

var
  WorkingAP: boolean;
  Wallpaper: TBitMap;
  WallpaperBMP: TBitMap;
  WallpaperBlurred: TBitMap;
  ScreenshotBlurred: TBitMap;


  LastDetectedFileSize: integer;
  LastSyncTime: TDateTime;

implementation

function GetWallpaperSize: integer;
begin
  Result := GetFileSize( GetWallpaperName(999) );
end;

function GetWallpaperSetting: TWallpaperSetting;
var
  R: TRegistry;
  Value: integer;
  TileWallpaper: boolean;
begin
  // Create registry
  R := TRegistry.Create(KEY_READ);
  Result := TWallpaperSetting.Stretch;
  R.RootKey := HKEY_CURRENT_USER;
  try
    if R.OpenKeyReadOnly('Control Panel\Desktop') then
      begin
        Value := R.ReadString('WallpaperStyle').ToInteger;
        TileWallpaper := R.ReadString('TileWallpaper').ToBoolean;

        // Clear String
        case Value of
          0: if TileWallpaper then
              Result := TWallpaperSetting.Tile
                else
                  Result := TWallpaperSetting.Center;
          2: Result := TWallpaperSetting.Stretch;
          6: Result := TWallpaperSetting.Fit;
          10: Result := TWallpaperSetting.Fill;
          22: Result := TWallpaperSetting.Span;
          else Result := TWallpaperSetting.Stretch;
        end;
      end;
  finally
    // Free Memory
    R.Free;
  end;
end;

function GetWallpaperName(ScreenIndex: integer; TranscodedDefault: boolean): string;
begin
  if GetNTKernelVersion <= 6.1 then
    Result := GetUserShellLocation(FXUserShell.AppData) + '\Microsoft\Windows\Themes\TranscodedWallpaper.jpg'
  else
    begin
      Result := GetUserShellLocation(FXUserShell.AppData) + '\Microsoft\Windows\Themes\Transcoded_' +
        IntToStrIncludePrefixZeros(ScreenIndex, 3);

      if TranscodedDefault or not TFile.Exists(Result) then
        Result := GetUserShellLocation(FXUserShell.AppData) + '\Microsoft\Windows\Themes\TranscodedWallpaper';
    end;
end;

procedure GetWallpaper;
var
  DeskRect: TRect;
begin
  (* This method is objectively better than loading all files manually and colaging them!
    Screenshot the program Manager! *)
  // Working
  WorkingAP := true;

  // Get Rects
  DeskRect := Screen.DesktopRect;

  // Create Images
  WallpaperBlurred := TBitMap.Create(DeskRect.Width, DeskRect.Height);

  // Screenshot
  AppScreenShot(WallpaperBlurred, DESKTOP_PROGRAM);

  // Normal
  WallpaperBMP := TBitMap.Create(DeskRect.Width, DeskRect.Height);
  WallpaperBMP.Assign( WallpaperBlurred );

  // Blur
  FastBlur(WallpaperBlurred, 8, 10, false); // 8 16

  // Get Size
  LastDetectedFileSize := GetWallpaperSize;
  LastSyncTime := Now;

  // Finish Work
  WorkingAP := false;
end;

procedure GetBlurredScreen(darkmode: boolean);
begin
  // Working
  WorkingAP := true;

  // Get Screenshot
  ScreenshotBlurred := TBitMap.Create;
  QuickScreenShot( ScreenshotBlurred );

  // Effects
  FastBlur(ScreenshotBlurred, 3, 8, false);

  // Time
  LastSyncTime := Now;

  // Finish
  WorkingAP := false;
end;

function GetCurrentExtension: string;
var
  R: TRegistry;
  Bytes: TBytes;
begin
  // Windows7
  if GetNTKernelVersion <= 6.1 then
    Exit('.jpeg');

  // Create registry
  R := TRegistry.Create(KEY_READ);

  R.RootKey := HKEY_CURRENT_USER;
  try
    if R.OpenKeyReadOnly('Control Panel\Desktop') then
      begin
        SetLength(Bytes, R.GetDataSize('TranscodedImageCache'));
        R.ReadBinaryData('TranscodedImageCache', Pointer(Bytes)^, Length(Bytes));

        // Clear String
        Result := ExtractFileName( TEncoding.ASCII.GetString(Bytes) );
        Result := AnsiLowerCase( Trim( ExtractFileExt( Result ) ).Replace(#0, '') );
      end;
  finally
    // Free Memory
    R.Free;
  end;
end;

procedure CreateBySignature(var Wallpaper: TGraphic; Sign: TFileType);
begin
  case Sign of
    { Png }
    TFileType.PNG: Wallpaper := TPngImage.Create;

    { Jpeg }
    TFileType.JPEG: Wallpaper := TJpegImage.Create;

    { Gif }
    TFileType.GIF: Wallpaper := TGifImage.Create;

    { Heif? }
    //dftHEIF: ;

    { Default }
    else Wallpaper := TBitMap.Create;
  end;
end;

procedure CreateByExtension(var Wallpaper: TGraphic; Extension: string);
begin
  { Jpeg }
  if (Extension = '.jpg') or (Extension = '.jpeg') then
    Wallpaper := TJpegImage.Create
      else
        { Png }
        if Extension = '.png' then
          Wallpaper := TPngImage.Create
          else
            { Gif }
            if Extension = '.gif' then
              Wallpaper := TGifImage.Create
                else
                  { Bitmap }
                  if Extension = '.bmp' then
                    Wallpaper := TBitMap.Create
                      else
                        { Default }
                        Wallpaper := TJpegImage.Create;
end;


{ FXBlurMaterial }

function FXBlurMaterial.Background: TColor;
begin
  Result := FDrawColors.Background;
end;

constructor FXBlurMaterial.Create(AOwner: TComponent);
begin
  inherited;
  //interceptmouse:=True;
  //Transparent := false;

  FCustomColors := FXColorSets.Create(false);
  with FCustomColors do
    begin
      DarkBackground := clBlack;
      LightBackground := clWhite;
      Accent := ThemeManager.AccentColor;
    end;

  FDrawColors := FXColorSet.Create;

  ControlStyle := ControlStyle + [csReplicatable, csPannable];

  // Timer
  Tick := TTimer.Create(Self);
  with Tick do
    begin
      Interval := 1;
      Enabled := false;
      OnTimer := TimerExecute;
    end;

  // Settings
  FInvalidateAbove := false;

  FVersion := FXBlurVersion.WallpaperBlurred;

  CustomColorGet;

  // Size
  Width := 150;
  Height := 200;

  // Tintin
  FEnableTinting := true;

  FWhiteTintOpacity := LIGHT_TINT_OPACITY;
  FDarkTintOpacity := DARK_TINT_OPACITY;
end;

procedure FXBlurMaterial.CustomColorGet;
begin
  if CustomColors.Enabled then
    begin
      FDrawColors := FXColorSet.Create(CustomColors, ThemeManager.DarkTheme);
    end
  else
    begin
      FDrawColors.Accent := ThemeManager.AccentColor;

      if ThemeManager.DarkTheme then
        FDrawColors.BackGround := clBlack
      else
        FDrawColors.BackGround := clWhite;
    end;
end;

function FXBlurMaterial.DestRect: TRect;
begin
  Result := Rect(0, 0, Width, Height);
end;

destructor FXBlurMaterial.Destroy;
begin
  Tick.Enabled := false;
  FreeAndNil(Tick);
  FreeAndNil(FDrawColors);
  FreeAndNil(FCustomColors);
  inherited;
end;

procedure FXBlurMaterial.FormMoveSync;
begin
  SyncroniseImage;
end;

function FXBlurMaterial.ImageTypeExists(ImgType: FXBlurVersion): boolean;
begin
  Result := false;
  case ImgType of
    FXBlurVersion.WallpaperBlurred: Result := (WallpaperBlurred  <> nil) and (not WallpaperBlurred.Empty);
    FXBlurVersion.Wallpaper: Result := (WallpaperBMP  <> nil) and (not WallpaperBMP.Empty);
    FXBlurVersion.Screenshot: Result := (ScreenshotBlurred  <> nil) and (not ScreenshotBlurred.Empty);
  end;
end;

procedure FXBlurMaterial.Inflate(up, right, down, lft: integer);
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

procedure FXBlurMaterial.InteractionStateChanged(AState: FXControlState);
begin
  // nothing
end;

function FXBlurMaterial.IsContainer: Boolean;
begin
  Result := false;
end;

procedure FXBlurMaterial.OnVisibleChange(var Message: TMessage);
begin
  if Self.Visible then
    SyncroniseImage;
end;

procedure FXBlurMaterial.PaintBuffer;
var
  Pict: TBitMap;
  DrawRect, ImageRect: Trect;
begin
  // Disable Timer After Successfull Draw
  if (not ImageTypeExists(Version)) and (not (csDesigning in ComponentState)) then
    Tick.Enabled := RefreshMode = FXGlassRefreshMode.Timer;

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

  FDrawing := True;
  try
    with Buffer do
      begin
        // Draw Canvas
        { Image Draw }
        if (WorkingAP) or not ImageTypeExists(Version) then
          begin
            Brush.Color := FDrawColors.BackGround;
            FillRect(ClipRect);

            Exit;
          end;

        DrawRect := Rect(0, 0, Width, Height);

        ImageRect := ClientToScreen( ClientRect );
        ImageRect.Offset(Screen.DesktopRect.Left * -1, Screen.DesktopRect.Top * -1);

        // Create Picture
        Pict := TBitMap.Create(Width, Height);

        // Copy Rect
        case Version of
          FXBlurVersion.WallpaperBlurred: Pict.Canvas.CopyRect(DrawRect, WallpaperBlurred.Canvas, ImageRect);
          FXBlurVersion.Wallpaper: Pict.Canvas.CopyRect(DrawRect, WallpaperBMP.Canvas, ImageRect);
          FXBlurVersion.Screenshot: Pict.Canvas.CopyRect(DrawRect, ScreenshotBlurred.Canvas, ImageRect);
        end;

        // Draw
        DrawHighQuality(DestRect, Pict, 255, false);

        // Tint Item
        if EnableTinting then
          begin
            DrawRect := ClipRect;
            DrawRect.Inflate(1, 1);

            if ThemeManager.DarkTheme then
              GDITint( DrawRect, FDrawColors.BackGround, FDarkTintOpacity )
            else
              GDITint( DrawRect, FDrawColors.BackGround, FWhiteTintOpacity );
          end;
      end;
  finally
    FDrawing := false;
  end;

  inherited;
end;

procedure FXBlurMaterial.RebuildImage;
begin
  case Version of
    FXBlurVersion.WallpaperBlurred, FXBlurVersion.Wallpaper: GetWallpaper;
    FXBlurVersion.Screenshot: GetBlurredScreen( ThemeManager.DarkTheme );
  end;
end;

procedure FXBlurMaterial.ReDraw;
begin
  Invalidate;
end;

procedure FXBlurMaterial.SetCustomColor(const Value: FXColorSets);
begin
  FCustomColors := Value;

  UpdateTheme(false);
end;

procedure FXBlurMaterial.SetDarkTint(const Value: integer);
begin
  FDarkTintOpacity := Value;

  Paint;
end;

procedure FXBlurMaterial.SetRefreshMode(const Value: FXGlassRefreshMode);
begin
  FRefreshMode := Value;

  if not (csDesigning in ComponentState) then
    Tick.Enabled := Value = FXGlassRefreshMode.Timer;
end;

procedure FXBlurMaterial.SetTinting(const Value: boolean);
begin
  FEnableTinting := Value;

  Paint;
end;

procedure FXBlurMaterial.SetVersion(const Value: FXBlurVersion);
begin
  FVersion := Value;

  Paint;
end;

procedure FXBlurMaterial.SetWhiteTint(const Value: integer);
begin
  FWhiteTintOpacity := Value;

  Paint;
end;

procedure FXBlurMaterial.SyncroniseImage;
begin
  // Paint
  ReDraw;

  // Check for different wallpaper
  case Version of
    FXBlurVersion.WallpaperBlurred, FXBlurVersion.Wallpaper: if (GetWallpaperSize <> LastDetectedFileSize) then
      RebuildImage;
    FXBlurVersion.Screenshot: if (ScreenshotBlurred = nil) or (SecondsBetween(LastSyncTime, Now) > 1) then
      RebuildImage;
  end;

  // Full Redraw
  if FInvalidateAbove then
    Invalidate;
end;

procedure FXBlurMaterial.TimerExecute(Sender: TObject);
begin
  if not IsDesigning then
    SyncroniseImage;
end;

procedure FXBlurMaterial.UpdateTheme(const UpdateChildren: Boolean);
begin
  CustomColorGet;

  if not (csReadingState in ControlState) then
    RebuildImage;
end;

procedure FXBlurMaterial.WndProc(var Message: TMessage);
begin
  inherited;
  if Message.Msg = WM_WINDOW_MOVE then
    if FRefreshMode = FXGlassRefreshMode.Automatic then
      SyncroniseImage;
end;

end.
