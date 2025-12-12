unit CFX.TabStrip;

interface

uses
  Classes,
  Messages,
  Windows,
  System.Generics.Defaults,
  System.Generics.Collections,
  Math,
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
  CFX.ComponentClasses,
  CFX.Components,
  CFX.Controls;

type
  FXStripTabs = class;

  FXStripTab = class(TComponent)
  private
    FParent: FXStripTabs;
    FText: string;
    FSize: integer;
    FImage: FXIconSelect;
    FTabIndex: integer;

    // Parent
    procedure StandardUpdateLayout;

    // Set
    procedure SetImage(const Value: FXIconSelect);
    procedure SetTabIndex(Value: integer);
    procedure SetText(const Value: string);
    procedure SetSize(const Value: integer);

  public
    property Parent: FXStripTabs read FParent;

    property TabIndex: integer read FTabIndex write SetTabIndex;

    property Text: string read FText write SetText;
    property Size: integer read FSize write SetSize;
    property Image: FXIconSelect read FImage write SetImage;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;
  FXStripTabs = class(TComponent)
  private
    FItems: TList<FXStripTab>;

    // Parent
    procedure StandardUpdateLayout;

    // Getters
    function GetItem(Index: Integer): FXStripTab;
    function GetCount: integer;

  public
      function Add: FXStripTab; overload;
    procedure Add(const Item: FXStripTab); overload;
    procedure Insert(Index: Integer; const Item: FXStripTab);
    procedure Delete(Index: Integer);
      procedure Remove(const Item: FXStripTab);

    property Items[Index: Integer]: FXStripTab read GetItem; default;
    property Count: integer read GetCount;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  FXTabStrip = class(FXWindowsControl)
  private
    var DrawRect: TRect;
    FDrawColors: FXCompleteColorSet;
    FCustomColors: FXColorSets;

    FItems: FXStripTabs;

    // Getters

    // Setters

  protected
    procedure PaintBuffer; override;

    //  Internal
    procedure UpdateColors; override;
    procedure UpdateRects; override;

    // Scaler
    procedure ScaleChanged(Scaler: single); override;

    // State
    procedure InteractionStateChanged(AState: FXControlState); override;

  published
    // Custom Colors
    property CustomColors: FXColorSets read FCustomColors write FCustomColors stored true;

    // Props
    property Tabs: FXStripTabs read FItems;

    // Default props
    property Align;
    //property PaddingFill;
    property Constraints;
    property Anchors;
    property Hint;
    property ShowHint;
    property TabStop;
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

  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    // Interface
    function Background: TColor; override;
  end;

implementation

function FXTabStrip.Background: TColor;
begin
  Result := FDrawColors.BackGround;
end;

constructor FXTabStrip.Create(aOwner: TComponent);
begin
  inherited;
  // Custom Color
  FCustomColors := FXColorSets.Create(Self);

  FDrawColors := FXCompleteColorSet.Create;

  // Items
  FItems := FXStripTabs.Create(Self);

  // Sizing
  Height := 40;
  Width := 300;
end;

destructor FXTabStrip.Destroy;
begin
  FreeAndNil( FCustomColors );
  FreeAndNil( FDrawColors );

  FreeAndNil( FItems );

  inherited;
end;

procedure FXTabStrip.InteractionStateChanged(AState: FXControlState);
begin
  inherited;
end;

procedure FXTabStrip.PaintBuffer;
begin
  // Background
  Color := FDrawColors.BackGround;
  PaintBackground;

  // Draw

  // Inherit
  inherited;
end;

procedure FXTabStrip.UpdateColors;
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

procedure FXTabStrip.UpdateRects;
begin
  // Rect
  DrawRect := GetClientRect;
end;

procedure FXTabStrip.ScaleChanged(Scaler: single);
begin
  inherited;
  // update scale
end;

{ FXStripTabs }

function FXStripTabs.Add: FXStripTab;
begin
  Result := FXStripTab.Create(Self);
  Add( Result );
end;

procedure FXStripTabs.Add(const Item: FXStripTab);
begin
  // Clear previous data if applicable
  if Item.Parent <> nil then
    Item.Parent.Delete( Item.TabIndex );

  // Add to list
  const ATabIndex = FItems.Add(Item);

  Item.FParent := Self;
  Item.FTabIndex := ATabIndex;

  // UI
  StandardUpdateLayout;
end;

constructor FXStripTabs.Create(AOwner: TComponent);
begin
  inherited;

  FItems := TList<FXStripTab>.Create;
end;

procedure FXStripTabs.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FItems.Count) then
    Exit;

  FItems[Index].FTabIndex := -1;
  FItems[Index].FParent := nil;

  FItems.Delete(Index);

  // UI
  StandardUpdateLayout;
end;

destructor FXStripTabs.Destroy;
begin
  FreeAndNil(FItems);

  inherited;
end;

function FXStripTabs.GetCount: integer;
begin
  Exit( FItems.Count );
end;

function FXStripTabs.GetItem(Index: Integer): FXStripTab;
begin
  Exit( FItems[Index] );
end;

procedure FXStripTabs.Insert(Index: Integer; const Item: FXStripTab);
begin
  // Clear previous data if applicable
  if Item.Parent <> nil then
    Item.Parent.Delete( Item.TabIndex );

  // Add to list
  Index := EnsureRange(Index, 0, FItems.Count);

  FItems.Insert(Index, Item);

  Item.FParent := Self;
  Item.FTabIndex := Index;

  // UI
  StandardUpdateLayout;
end;

procedure FXStripTabs.Remove(const Item: FXStripTab);
begin
  Delete( FItems.IndexOf(Item) );
end;

procedure FXStripTabs.StandardUpdateLayout;
begin
  if not Assigned(Owner) or not (Owner is FXTabStrip) then
    Exit;

  (Owner as FXTabStrip).StandardUpdateLayout;
end;

{ FXStripTab }

constructor FXStripTab.Create(AOwner: TComponent);
begin
  inherited;

  FImage := FXIconSelect.Create(nil);
  FText := '';
  FSize := TABSTRIP_TAB_DEFAULT_WIDTH;
end;

destructor FXStripTab.Destroy;
begin
  FreeAndNil( FImage );
  inherited;
end;

procedure FXStripTab.SetImage(const Value: FXIconSelect);
begin

end;

procedure FXStripTab.SetTabIndex(Value: integer);
begin
  if (FTabIndex = Value) or (FParent = nil) then
    Exit;

  // Move
  EnsureRange(Value, 0, FParent.FItems.Count-1);
  FParent.FItems.Move(FTabIndex, Value);
  FTabIndex := Value;

  // UI
  StandardUpdateLayout;
end;

procedure FXStripTab.SetText(const Value: string);
begin
  if FText = Value then
    Exit;

  // UI
  StandardUpdateLayout;
end;

procedure FXStripTab.SetSize(const Value: integer);
begin
  if FSize = Value then
    Exit;

  // UI
  StandardUpdateLayout;
end;

procedure FXStripTab.StandardUpdateLayout;
begin
  if Parent = nil then
    Exit;

  Parent.StandardUpdateLayout;
end;

end.
