unit CFX.TextBox;

interface

uses
  Classes,
  Messages,
  Windows,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.ExtCtrls,
  Types,
  Math,
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
  FXTextBox = class(FXWindowsControl, FXControl)
    private
      var TextRect: TRect;

      FCustomColors: FXColorSets;
      FDrawColors: FXCompleteColorSet;

      FText: string;
      FVertLayout: FXLayout;
      FHorzLayout: FXLayout;
      FInnerMargin: integer;

      FAutoSize: boolean;
      FWordWrap: boolean;
      FElipsis: boolean;

      FDrawOffset: TPoint;

      //  Internal
      procedure UpdateColors;
      procedure UpdateRects;

      // Set properties
      procedure SetText(const Value: string);
      procedure SetHorzLayout(const Value: FXLayout);
      procedure SetVertLayout(const Value: FXLayout);
      procedure SetInnermargin(const Value: integer);
      procedure SetElipsis(const Value: boolean);
      procedure SetWordWrap(const Value: boolean);
      procedure SetAutoSizeEx(const Value: boolean);

      // Size
      procedure PaddingUpdated(Sender: TObject);

      // Handle Messages
      procedure WM_LButtonUp(var Msg: TWMLButtonUp); message WM_LBUTTONUP;
      procedure WMSize(var Message: TWMSize); message WM_SIZE;

    protected
      procedure PaintBuffer; override;
      procedure Resize; override;
      procedure ChangeScale(M, D: Integer{$IF CompilerVersion > 29}; isDpiChange: Boolean{$ENDIF}); override;

      // Created
      procedure ComponentCreated; override;

      // Font
      procedure FontUpdate; override;
      function BuildFlags: FXTextFlags;

      // State
      procedure InteractionStateChanged(AState: FXControlState); override;

      // Key Presses
      procedure KeyPress(var Key: Char); override;

      // Inherited Mouse Detection
      procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

    published
      property CustomColors: FXColorSets read FCustomColors write FCustomColors stored true;

      property Caption: string read FText write SetText;
      property Text: string read FText write SetText;

      property LayoutHorizontal: FXLayout read FHorzLayout write SetHorzLayout default FXLayout.Beginning;
      property LayoutVertical: FXLayout read FVertLayout write SetVertLayout default FXLayout.Center;

      property AutoSize: boolean read FAutoSize write SetAutoSizeEx;
      property WordWrap: boolean read FWordWrap write SetWordWrap default false;
      property Elipsis: boolean read FElipsis write SetElipsis;

      property InnerMargin: integer read FInnerMargin write SetInnermargin;

      property Align;
      property Font;
      property PaddingFill;
      property Constraints;
      property Anchors;
      property Hint;
      property ShowHint;
      property TabStop default false;
      property TabOrder;
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

      // Size
      procedure UpdateAutoSize;

    public
      constructor Create(aOwner: TComponent); override;
      destructor Destroy; override;

      // Interface
      function IsContainer: Boolean;
      procedure UpdateTheme(const UpdateChildren: Boolean);

      function Background: TColor;
  end;

  FXAnimatedTextBox = class(FXTextBox)
    private
      FItems: TStringList;

      FAnimating: boolean;
      FDelay: integer;
      FTimer: TTimer;

      FPosition: integer;

      // Settters
      procedure SetAnimating(const Value: boolean);
      procedure SetItems(const Value: TStringList);

      // Tick
      procedure TimerTick(Sender: TObject);
      procedure SetDelay(const Value: integer);

    published
      property Items: TStringList read FItems write SetItems;
      property Delay: integer read FDelay write SetDelay default 100;
      property Animating: boolean read FAnimating write SetAnimating default true;

      function Count: integer;

    public
      constructor Create(aOwner: TComponent); override;
      destructor Destroy; override;
  end;

implementation

procedure FXTextBox.InteractionStateChanged(AState: FXControlState);
begin
  inherited;
end;

function FXTextBox.IsContainer: Boolean;
begin
  Result := false;
end;

procedure FXTextBox.KeyPress(var Key: Char);
begin
  inherited;
end;

procedure FXTextBox.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

procedure FXTextBox.UpdateTheme(const UpdateChildren: Boolean);
begin
  UpdateColors;
  UpdateRects;
  Invalidate;
end;

procedure FXTextBox.UpdateAutoSize;
var
  AWidth, AHeight: integer;
  ARect: TRect;
  AFlags: FXTextFlags;
begin
  with Canvas do
    begin
      Font.Assign( Self.Font );

      if WordWrap then
        // Word Wrap
        begin
          AFlags := BuildFlags;
          ARect := GetTextRect(Canvas, Self.TextRect, FText, AFlags, FInnerMargin);

          Width := ARect.Width + PaddingFill.PadWidth;
          Height := ARect.Height + PaddingFill.PadHeight;
        end
      else
        // Single Line
        begin
          AWidth := TextWidth(FText);
          AHeight := TextHeight(FText);

          Width := AWidth + PaddingFill.PadWidth;
          Height := AHeight + PaddingFill.PadHeight;
        end;
    end;
end;

procedure FXTextBox.UpdateColors;
begin
  FDrawColors.Assign( ThemeManager.SystemColor );

  // Access theme manager
  if FCustomColors.Enabled then
    begin
      // Custom Colors
      FDrawColors.LoadFrom(FCustomColors, ThemeManager.DarkTheme);
    end
  else
    begin
      // Global Colors
      FDrawColors.LoadFrom(ThemeManager.SystemColorSet, ThemeManager.DarkTheme);
      FDrawColors.BackGround := GetParentBackgroundColor(FDrawColors.BackGround);
    end;
end;

procedure FXTextBox.UpdateRects;
begin
  TextRect := GetClientRect;
  FDrawOffset := TPoint.Zero;
  {if Font.Orientation = 0 then
    FDrawOffset := TPoint.Zero
  else
    begin
      var Origin: TPoint;
      var Dest: TPoint;
      var Rotate: TPoint;

      Rotate := TextRect.TopLeft;

      Origin := TextRect.CenterPoint;
      Dest := RotatePointAroundPoint(Origin, Rotate, 360-Font.Orientation / 10);

      FDrawOffset := Origin.Subtract(Dest);
    end; }
end;

procedure FXTextBox.SetAutoSizeEx(const Value: boolean);
begin
  if FAutoSize <> Value then
    begin
      FAutoSize := Value;

      if FAutoSize and not IsReading then
        UpdateAutoSize
      else
        Invalidate;
    end;
end;

procedure FXTextBox.SetElipsis(const Value: boolean);
begin
  if FElipsis <> Value then
    begin
      FElipsis := Value;

      Invalidate;
    end;
end;

procedure FXTextBox.SetHorzLayout(const Value: FXLayout);
begin
  if FHorzLayout <> Value then
    begin
      FHorzLayout := Value;

      UpdateRects;
      Invalidate;
    end;
end;

procedure FXTextBox.SetInnermargin(const Value: integer);
begin
  FInnerMargin := Value;
end;

procedure FXTextBox.SetText(const Value: string);
begin
  if FText <> Value then
    begin
      FText := Value;
      
      UpdateRects;

      if AutoSize and not IsReading then
        UpdateAutoSize;
      
      Invalidate;
    end;
end;

procedure FXTextBox.SetVertLayout(const Value: FXLayout);
begin
    if FVertLayout <> Value then
    begin
      FVertLayout := Value;

      UpdateRects;
      Invalidate;
    end;
end;

procedure FXTextBox.SetWordWrap(const Value: boolean);
begin
  if FWordWrap <> Value then
    begin
      FWordWrap := Value;

      if AutoSize then
        UpdateAutoSize;
      
      UpdateRects;
      Invalidate;
    end;
end;

constructor FXTextBox.Create(aOwner: TComponent);
begin
  inherited;
  TabStop := false;

  AutoFocusLine := true;
  BufferedComponent := true;
  
  FHorzLayout := FXLayout.Beginning;
  FVertLayout := FXLayout.Center;
  FWordWrap := false;
  FAutoSize := true;

  // Padding
  with PaddingFill do
    begin
      OnChange := PaddingUpdated;
    end;

  // Custom Color
  FCustomColors := FXColorSets.Create(Self);

  FDrawColors := FXCompleteColorSet.Create;

  FText := TEXT_DEFAULT_GENERIC;

  // Sizing
  Height := 30;
  Width := 180;

  // Update
  UpdateRects;
  UpdateColors;
end;

destructor FXTextBox.Destroy;
begin
  FreeAndNil( FCustomColors );
  FreeAndNil( FDrawColors );
  inherited;
end;

procedure FXTextBox.FontUpdate;
begin
  // inherited
  if FAutoSize and not IsReading then
    UpdateAutoSize;

  UpdateRects;
  Invalidate;
end;

function FXTextBox.Background: TColor;
begin
  Result := FDrawColors.Background;
end;

function FXTextBox.BuildFlags: FXTextFlags;
begin
  Result := [];
  if WordWrap then
    Result := Result + [FXTextFlag.WordWrap, FXTextFlag.NoClip];

  case FHorzLayout of
    FXLayout.Beginning: Result := Result + [FXTextFlag.Left];
    FXLayout.Center: Result := Result + [FXTextFlag.Center];
    FXLayout.Ending: Result := Result + [FXTextFlag.Right];
  end;

  case FVertLayout of
    FXLayout.Beginning: Result := Result + [FXTextFlag.Top];
    FXLayout.Center: Result := Result + [FXTextFlag.VerticalCenter];
    FXLayout.Ending: Result := Result + [FXTextFlag.Bottom];
  end;
end;

procedure FXTextBox.ChangeScale(M, D: Integer{$IF CompilerVersion > 29}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  UpdateRects;
end;

procedure FXTextBox.ComponentCreated;
begin
  inherited;
  if AutoSize then
    UpdateAutoSize;
end;

procedure FXTextBox.PaddingUpdated(Sender: TObject);
begin
  UpdateRects;
  if AutoSize and not IsReading then
    UpdateAutoSize;
end;

procedure FXTextBox.PaintBuffer;
var
  Flags: FXTextFlags;
  ARect: TRect;
begin
  // Background
  Color := FDrawColors.BackGround;
  PaintBackground;

  // Flags
  Flags := BuildFlags;

  // Draw
  with Buffer do
    begin
      Font.Assign(Self.Font);
      Font.Color := FDrawColors.ForeGround;
      Brush.Style := bsClear;
    end;

  ARect := TextRect;
  ARect.OffSet( FDrawOffset );

  DrawTextRect(Buffer, ARect, FText, Flags, InnerMargin);

  inherited;
end;

procedure FXTextBox.Resize;
begin
  inherited;
  if AutoSize then
    UpdateAutoSize;

  UpdateRects;
end;

procedure FXTextBox.WMSize(var Message: TWMSize);
begin
  UpdateRects;

  if AutoSize and not IsDesigning and not IsReading then
    UpdateAutoSize;
  
  // Invalidate
  Invalidate;
end;

procedure FXTextBox.WM_LButtonUp(var Msg: TWMLButtonUp);
begin
  inherited;
end;

{ FXAnimatedTextBox }

function FXAnimatedTextBox.Count: integer;
begin
  Result := FItems.Count;
end;

constructor FXAnimatedTextBox.Create(aOwner: TComponent);
begin
  inherited;
  FTimer := TTimer.Create(Self);
  FDelay := 1000;
  FAnimating := true;

  FItems := TStringList.Create;
  if IsDesigning then
    begin
      with FItems do
        begin
          Add('Item 1');
          Add('Item 2');
          Add('Item 3');
        end;

      FText := FItems[0];
    end
  else
    FText := TEXT_LIST_EMPTY;

  with FTimer do
    begin
      Enabled := FAnimating;
      Interval := FDelay;
      OnTimer := TimerTick;
    end;
end;

destructor FXAnimatedTextBox.Destroy;
begin
  FTimer.Enabled := false;
  FreeAndNil( FTimer );
  FreeAndNil( FItems );
  inherited;
end;

procedure FXAnimatedTextBox.SetAnimating(const Value: boolean);
begin
  if FAnimating <> Value then
    begin
      FAnimating := Value;

      FTimer.Enabled := Value;
    end;
end;

procedure FXAnimatedTextBox.SetDelay(const Value: integer);
begin
  if FDelay <> Value then
    begin
      FDelay := Value;

      FTimer.Interval := Value;
    end;
end;

procedure FXAnimatedTextBox.SetItems(const Value: TStringList);
begin
  FItems.Assign( Value );
  FPosition := 0;

  if FItems.Count > 0 then
    Text := FItems[0]
  else
    Text := TEXT_LIST_EMPTY;
end;

procedure FXAnimatedTextBox.TimerTick(Sender: TObject);
begin
  if not Visible then
    Exit;

  // Set
  if (Count > 0) and (FPosition < Count) then
    Text := FItems[FPosition];

  // Next
  Inc(FPosition);
  if FPosition >= Count then
    FPosition := 0;
end;

end.
