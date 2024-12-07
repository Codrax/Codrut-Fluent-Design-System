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
  CFX.Constants,
  SysUtils,
  CFX.Classes,
  CFX.Types,
  CFX.VarHelpers,
  CFX.Linker,
  CFX.Controls;

type
  FXCustomTextBox = class(FXWindowsControl)
  private
    var TextRect: TRect;

    FCustomColors: FXColorSets;
    FDrawColors: FXCompleteColorSet;
    FUseAccentAsForeground: boolean;

    FText: string;
    FVertLayout: FXLayout;
    FHorzLayout: FXLayout;
    FInnerMargin: integer;

    FAutoSize: boolean;
    FWordWrap: boolean;
    FShowAccelChar: boolean;
    FElipsis: boolean;

    FDrawOffset: TPoint;

    // Set properties
    procedure SetText(const Value: string);
    procedure SetHorzLayout(const Value: FXLayout);
    procedure SetVertLayout(const Value: FXLayout);
    procedure SetInnermargin(const Value: integer);
    procedure SetElipsis(const Value: boolean);
    procedure SetWordWrap(const Value: boolean);
    procedure SetAutoSizeEx(const Value: boolean);
    procedure SetShowAccelChar(const Value: boolean);
    procedure SetUseAccentAsForeground(const Value: boolean);

  protected
    procedure PaintBuffer; override;

    // Override
    procedure ApplyInnerMargins; override;
    procedure ApplyPadding; override;

    // Internal
    procedure UpdateColors; override;
    procedure UpdateRects; override;

    // Size
    procedure Sized; override;

    // State
    procedure InteractionStateChanged(AState: FXControlState); override;

    // Font
    procedure FontUpdate; override;
    function BuildFlags: FXTextFlags;

    // Properties
    //property Caption: string read FText write SetText;
    property Text: string read FText write SetText;

    property UseAccentAsForeground: boolean read FUseAccentAsForeground write SetUseAccentAsForeground default false;

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
    function Background: TColor; override;
  end;

  FXTextBox = class(FXCustomTextBox)
  published
    property Text;

    property UseAccentAsForeground;

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
    property UseAccentAsForeground;

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
    property UseAccentAsForeground;

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
  // do not update
end;

procedure FXCustomTextBox.UpdateAutoSize;
var
  AWidth, AHeight: integer;
  ARect: TRect;
  AFlags: FXTextFlags;
begin
  with Canvas do begin
    Font.Assign( Self.Font );

    const Absolute = GetAbsoluteRect;
    const Content = GetContentRect;
    const PadWidth = Absolute.Width - Content.Width;
    const PadHeight = Absolute.Height - Content.Height;

    // Calculate sizing
    if WordWrap then
      // Word Wrap
      begin
        AFlags := BuildFlags;
        ARect := GetTextRect(Canvas, Self.TextRect, FText, AFlags, FInnerMargin);

        AWidth := ARect.Width + PadWidth;
        AHeight := ARect.Height + PadHeight;
      end
    else
      // Single Line
      begin
        AWidth := TextWidth(FText) + PadWidth;
        AHeight := TextHeight(FText) + PadHeight;
      end;

    // Alignment
    case Align of
      alTop, alBottom: AWidth := Width;
      alLeft, alRight: AHeight := Height;
      alClient: Exit;
    end;

    // Set
    if (Width <> AWidth) or (Height <> AHeight) then
      Self.SetBounds(Left, Top, AWidth, AHeight);
  end;
end;

procedure FXCustomTextBox.UpdateColors;
begin
  // Access theme manager
  FDrawColors.Assign( ThemeManager.SystemColor );
  if FCustomColors.Enabled then
    // Custom Colors
    FDrawColors.LoadFrom(FCustomColors, ThemeManager.DarkTheme);
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
  if FAutoSize = Value then
    Exit;

  FAutoSize := Value;

  if FAutoSize and CanUpdate then
    UpdateAutoSize
  else
    StandardUpdateLayout;
end;

procedure FXCustomTextBox.SetElipsis(const Value: boolean);
begin
  if FElipsis = Value then
    Exit;

  FElipsis := Value;
  StandardUpdateLayout;
end;

procedure FXCustomTextBox.SetHorzLayout(const Value: FXLayout);
begin
  if FHorzLayout = Value then
    Exit;

  FHorzLayout := Value;
  StandardUpdateLayout;
end;

procedure FXCustomTextBox.SetInnermargin(const Value: integer);
begin
  if FInnerMargin = Value then
    Exit;

  FInnerMargin := Value;
  StandardUpdateLayout;
end;

procedure FXCustomTextBox.SetShowAccelChar(const Value: boolean);
begin
  if FShowAccelChar = Value then
    Exit;

  FShowAccelChar := Value;
  StandardUpdateLayout;
end;

procedure FXCustomTextBox.SetText(const Value: string);
begin
  if FText = Value then
    Exit;

  FText := Value;

  // Update
  if AutoSize and CanUpdate then
    UpdateAutoSize;

  StandardUpdateLayout;
end;

procedure FXCustomTextBox.SetUseAccentAsForeground(const Value: boolean);
begin
  if FUseAccentAsForeground = Value then
    Exit;

  FUseAccentAsForeground := Value;

  // Update
  StandardUpdateDraw;
end;

procedure FXCustomTextBox.SetVertLayout(const Value: FXLayout);
begin
  if FVertLayout = Value then
    Exit;

  FVertLayout := Value;
  StandardUpdateLayout;
end;

procedure FXCustomTextBox.SetWordWrap(const Value: boolean);
begin
  if FWordWrap = Value then
    Exit;

  FWordWrap := Value;
  if AutoSize and CanUpdate then
    UpdateAutoSize
  else
    StandardUpdateLayout;
end;

procedure FXCustomTextBox.Sized;
begin
  inherited;
  if AutoSize and not IsReading and (Parent <> nil) then
    UpdateAutoSize;
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

  // Custom Color
  FCustomColors := FXColorSets.Create(Self);

  FDrawColors := FXCompleteColorSet.Create;

  FText := TEXT_DEFAULT_GENERIC;

  // Sizing
  Height := 30;
  Width := 180;
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
  Redraw;
end;

procedure FXCustomTextBox.ApplyInnerMargins;
begin
  inherited;
  if AutoSize then
    UpdateAutoSize;
end;

procedure FXCustomTextBox.ApplyPadding;
begin
  inherited;
  if AutoSize then
    UpdateAutoSize;
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
      if UseAccentAsForeground then
        Font.Color := FDrawColors.Accent
      else
        Font.Color := FDrawColors.ForeGround;
      Brush.Style := bsClear;
    end;

  ARect := TextRect;
  ARect.OffSet( FDrawOffset );

  DrawTextRect(Buffer, ARect, FText, Flags, InnerMargin);
  //Buffer.GDIText(FText, ARect, Flags);

  inherited;
end;

{ FXAnimatedTextBox }

function FXAnimatedTextBox.Count: integer;
begin
  Result := 0;
  if (FItems <> nil) and Assigned(FItems) then
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
  if FAdderMode = Value then
    Exit;

  FAdderMode := Value;
  ReloadItem;
end;

procedure FXAnimatedTextBox.SetAdderText(const Value: string);
begin
  if FAdderText = Value then
    Exit;

  FAdderText := Value;
  ReloadItem;
end;

procedure FXAnimatedTextBox.SetAnimating(const Value: boolean);
begin
  if FAnimating = Value then
    Exit;

  FAnimating := Value;
  FTimer.Enabled := Value;
end;

procedure FXAnimatedTextBox.SetDelay(const Value: integer);
begin
  if FDelay =Value then
    Exit;


  FDelay := Value;
  FTimer.Interval := Value;
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

  UpdateText(false);
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
