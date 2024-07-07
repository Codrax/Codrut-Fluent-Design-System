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
  FXCustomTextBox = class(FXWindowsControl, FXControl)
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
      FShowAccelChar: boolean;
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
      procedure SetShowAccelChar(const Value: boolean);

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

      // Properties
      //property Caption: string read FText write SetText;
      property Text: string read FText write SetText;

      property LayoutHorizontal: FXLayout read FHorzLayout write SetHorzLayout default FXLayout.Beginning;
      property LayoutVertical: FXLayout read FVertLayout write SetVertLayout default FXLayout.Center;

      property AutoSize: boolean read FAutoSize write SetAutoSizeEx;
      property WordWrap: boolean read FWordWrap write SetWordWrap default false;
      property ShowAccelChar: boolean read FShowAccelChar write SetShowAccelChar default false;
      property Elipsis: boolean read FElipsis write SetElipsis;

      property InnerMargin: integer read FInnerMargin write SetInnermargin;

    published
      property CustomColors: FXColorSets read FCustomColors write FCustomColors stored true;

      property Align;
      property Font;
      property Transparent;
      property HitTest;
      property Opacity;
      property PaddingFill;
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

  FXTextBox = class(FXCustomTextBox)
    property Text;

    property LayoutHorizontal;
    property LayoutVertical;

    property AutoSize;
    property WordWrap;
    property ShowAccelChar;
    property Elipsis;

    property InnerMargin;
  end;

  FXValueTextBox = class(FXCustomTextBox)
  private
    FValueFormat,
    FValueName,
    FValue: string;

    procedure UpdateText(Update: boolean=true);

    // Setters
    procedure SetValue(const Value: string);
    procedure SetValueFormat(const Value: string);
    procedure SetValueName(const Value: string);

  published
    property LayoutHorizontal;
    property LayoutVertical;

    property AutoSize;
    property WordWrap;
    property Elipsis;

    property ShowAccelChar;
    property InnerMargin;

    property Value: string read FValue write SetValue;
    property ValueName: string read FValueName write SetValueName;
    property ValueFormat: string read FValueFormat write SetValueFormat;

  public
    constructor Create(aOwner: TComponent); override;
  end;

  FXAnimatedTextBox = class(FXCustomTextBox)
    private
      FItems: TStringList;

      FAnimating: boolean;
      FDelay: integer;
      FTimer: TTimer;

      FPosition: integer;

      FAdderMode: boolean;
      FAdderText: string;

      // Settters
      procedure SetAnimating(const Value: boolean);
      procedure SetItems(const Value: TStringList);
      procedure SetAdderMode(const Value: boolean);
      procedure SetAdderText(const Value: string);
      procedure SetDelay(const Value: integer);

      // Text
      procedure LoadItem(Index: integer; Update: boolean = true);
      procedure ReloadItem;

      // Tick
      procedure TimerTick(Sender: TObject);
      procedure ItemsOnChange(Sender: TObject);

    published
      property LayoutHorizontal;
      property LayoutVertical;

      property AutoSize;
      property WordWrap;
      property ShowAccelChar;
      property Elipsis;

      property InnerMargin;

      property Items: TStringList read FItems write SetItems;
      property Delay: integer read FDelay write SetDelay default 1000;
      property Animating: boolean read FAnimating write SetAnimating default true;

      property AdderMode: boolean read FAdderMode write SetAdderMode default false;
      property AdderText: string read FAdderText write SetAdderText;

      function Count: integer;

    public
      constructor Create(aOwner: TComponent); override;
      destructor Destroy; override;
  end;

implementation

procedure FXCustomTextBox.InteractionStateChanged(AState: FXControlState);
begin
  inherited;
end;

function FXCustomTextBox.IsContainer: Boolean;
begin
  Result := false;
end;

procedure FXCustomTextBox.KeyPress(var Key: Char);
begin
  inherited;
end;

procedure FXCustomTextBox.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

procedure FXCustomTextBox.UpdateTheme(const UpdateChildren: Boolean);
begin
  UpdateColors;
  UpdateRects;
  Invalidate;
end;

procedure FXCustomTextBox.UpdateAutoSize;
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

procedure FXCustomTextBox.UpdateColors;
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

procedure FXCustomTextBox.UpdateRects;
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

procedure FXCustomTextBox.SetAutoSizeEx(const Value: boolean);
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

procedure FXCustomTextBox.SetElipsis(const Value: boolean);
begin
  if FElipsis <> Value then
    begin
      FElipsis := Value;

      Invalidate;
    end;
end;

procedure FXCustomTextBox.SetHorzLayout(const Value: FXLayout);
begin
  if FHorzLayout <> Value then
    begin
      FHorzLayout := Value;

      UpdateRects;
      Invalidate;
    end;
end;

procedure FXCustomTextBox.SetInnermargin(const Value: integer);
begin
  FInnerMargin := Value;
end;

procedure FXCustomTextBox.SetShowAccelChar(const Value: boolean);
begin
  if FShowAccelChar <> Value then
    begin
      FShowAccelChar := Value;

      Invalidate;
    end;
end;

procedure FXCustomTextBox.SetText(const Value: string);
begin
  if FText <> Value then
    begin
      FText := Value;
      
      UpdateRects;

      // Reading
      if IsReading or Creating then
        Exit;

      // Update
      if AutoSize then
        UpdateAutoSize;

      Invalidate;
    end;
end;

procedure FXCustomTextBox.SetVertLayout(const Value: FXLayout);
begin
    if FVertLayout <> Value then
    begin
      FVertLayout := Value;

      UpdateRects;
      Invalidate;
    end;
end;

procedure FXCustomTextBox.SetWordWrap(const Value: boolean);
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

constructor FXCustomTextBox.Create(aOwner: TComponent);
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

destructor FXCustomTextBox.Destroy;
begin
  FreeAndNil( FCustomColors );
  FreeAndNil( FDrawColors );
  inherited;
end;

procedure FXCustomTextBox.FontUpdate;
begin
  // inherited
  if FAutoSize and not IsReading then
    UpdateAutoSize;

  UpdateRects;
  Invalidate;
end;

function FXCustomTextBox.Background: TColor;
begin
  Result := FDrawColors.Background;
end;

function FXCustomTextBox.BuildFlags: FXTextFlags;
begin
  Result := [];
  if WordWrap then
    Result := Result + [FXTextFlag.WordWrap, FXTextFlag.NoClip];

  if Elipsis then
    Result := Result + [FXTextFlag.TrimCutoff];
  if ShowAccelChar then
    Result := Result + [FXTextFlag.ShowAccelChar];

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

procedure FXCustomTextBox.ChangeScale(M, D: Integer{$IF CompilerVersion > 29}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  UpdateRects;
end;

procedure FXCustomTextBox.ComponentCreated;
begin
  inherited;
  if AutoSize then
    UpdateAutoSize;
end;

procedure FXCustomTextBox.PaddingUpdated(Sender: TObject);
begin
  UpdateRects;
  if AutoSize then
    UpdateAutoSize;
end;

procedure FXCustomTextBox.PaintBuffer;
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
  //Buffer.GDIText(FText, ARect, Flags);

  inherited;
end;

procedure FXCustomTextBox.Resize;
begin
  inherited;
  if AutoSize then
    UpdateAutoSize;

  UpdateRects;
end;

procedure FXCustomTextBox.WMSize(var Message: TWMSize);
begin
  UpdateRects;

  if AutoSize and not IsDesigning and not IsReading then
    UpdateAutoSize;
  
  // Invalidate
  Invalidate;
end;

procedure FXCustomTextBox.WM_LButtonUp(var Msg: TWMLButtonUp);
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

  FAdderMode := false;
  FAdderText := '';

  FItems := TStringList.Create;
  FItems.OnChange := ItemsOnChange;
  if IsDesigning then
    begin
      with FItems do
        begin
          Add('Item 1');
          Add('Item 2');
          Add('Item 3');
        end;

      LoadItem(0, false);
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

procedure FXAnimatedTextBox.ItemsOnChange(Sender: TObject);
var
  ACount: integer;
begin
  ACount := Count;

  if (FPosition >= ACount) or (ACount = 0) then
    FPosition := 0;
  
  if not IsDesigning then
    ReloadItem;
end;

procedure FXAnimatedTextBox.LoadItem(Index: integer; Update: boolean);
var
  AText: string;
begin
  if (Count > 0) and (Index < Count) then
    if FAdderMode then
      AText := FAdderText + FItems[Index]
    else
      AText := FItems[Index]
  else
    AText := TEXT_LIST_EMPTY;

  if Update then
    Text := AText
  else
    FText := AText;
end;

procedure FXAnimatedTextBox.ReloadItem;
begin
  LoadItem(FPosition);
end;

procedure FXAnimatedTextBox.SetAdderMode(const Value: boolean);
begin
  if FAdderMode <> Value then
    begin
      FAdderMode := Value;

      ReloadItem;
    end;
end;

procedure FXAnimatedTextBox.SetAdderText(const Value: string);
begin
  if FAdderText <> Value then
    begin
      FAdderText := Value;

      ReloadItem;
    end;
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

  LoadItem(0);
end;

procedure FXAnimatedTextBox.TimerTick(Sender: TObject);
begin
  if not Visible then
    Exit;

  // Set
  LoadItem(FPosition);

  // Next
  Inc(FPosition);
  if FPosition >= Count then
    FPosition := 0;
end;

{ FXValueTextBox }

constructor FXValueTextBox.Create(aOwner: TComponent);
begin
  inherited;

  FValueFormat := '%S: %S';
  FValueName := 'Value';
  FValue := STRING_NONE;

  UpdateText;
end;

procedure FXValueTextBox.SetValue(const Value: string);
begin
  if FValue = Value then
    Exit;
  FValue := Value;

  UpdateText;
end;

procedure FXValueTextBox.SetValueFormat(const Value: string);
begin
  if FValueFormat = Value then
    Exit;
  FValueFormat := Value;

  UpdateText;
end;

procedure FXValueTextBox.SetValueName(const Value: string);
begin
  if FValueName = Value then
    Exit;
  FValueName := Value;

  UpdateText;
end;

procedure FXValueTextBox.UpdateText(Update: boolean);
var
  NewText: string;
begin
  NewText := Format(FValueFormat, [FValueName, FValue]);

  if NewText = FText then
    Exit;

  if Update then
    Text := NewText
  else
    FText := NewText;
end;

end.
