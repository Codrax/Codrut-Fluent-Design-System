unit CFX.PopupMenu;

interface

uses
  SysUtils,
  Winapi.Windows,
  Classes,
  Types,
  Math,
  CFX.Components,
  UITypes,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.ExtCtrls,
  Vcl.Dialogs,
  Threading,
  System.Generics.Collections,
  CFX.Animation.Main,
  CFX.Animation.Component,
  Vcl.Menus,
  CFX.Graphics,
  CFX.VarHelpers,
  CFX.ArrayHelpers,
  Vcl.Forms,
  DateUtils,
  IOUtils,
  CFX.Utilities,
  CFX.ThemeManager,
  CFX.Classes,
  CFX.ComponentClasses,
  CFX.Constants,
  CFX.Colors,
  CFX.Math,
  CFX.GDI,
  CFX.Linker,
  CFX.Types;

type
  // Class
  FXPopupComponent = class;
  FXPopupItem = class;
  FXPopupMenu = class;

  // Types
  TOnItemClick = procedure(Sender: TObject; Item: FXPopupComponent; Index: integer) of object;
  TOnBeforePopup = procedure(Sender: TObject; var CanPopup: boolean; Point: TPoint) of object;

  // Menu
  FXPopupItems = class(FXPersistent)
  private
    FItems: TArray<FXPopupItem>;

    function GetItem(AIndex: Integer): FXPopupItem;

  protected
    // Serialization
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);

  public
    // Constructors
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    // Items
    property Item[AIndex: Integer]: FXPopupItem read GetItem; default;

    // Data
    function Count: integer;
    function IndexOf(AText: string): integer;
    procedure Add(AItem: FXPopupItem);
    function AddNew: FXPopupItem;
    procedure Delete(Index: integer; AndFree: boolean = true);

    procedure Clear; overload;
    procedure Clear(AndFree: boolean); overload;
  end;

  // Popup Container
  FXPopupContainer = class({TPopupMenu}FXComponent)
  private
    // Properties
    FText: string;
    FHint: string;

    FAutoCheck: Boolean;
    FChecked: Boolean;
    FEnabled: Boolean;
    FDefault: Boolean;
    FRadioItem: Boolean;
    FVisible: Boolean;

    FImage: FXIconSelect;
    FItems: FXPopupItems;

    FShortCut: string;

    // Notify Events
    FOnClick: TNotifyEvent;
    FOnHover: TNotifyEvent;
    FOnCheck: TNotifyEvent;

    // Internal for Drawing
    FBounds: TRect;

    function HasSubItems: boolean;
    procedure SetChecked(const Value: boolean);
    function GetMenuItem(Index: Integer): FXPopupContainer;
    function GetIndex: integer;
    function GetSeparator: boolean;
    procedure SetSeparator(const Value: boolean);

  protected
    // Notify Event
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnHover: TNotifyEvent read FOnHover write FOnHover;
    property OnCheck: TNotifyEvent read FOnCheck write FOnCheck;

    // Data
    property Text: string read FText write FText;
    property Hint: string read FHint write FHint;

    property Enabled: boolean read FEnabled write FEnabled default True;
    property Checked: boolean read FChecked write SetChecked default False;
    property AutoCheck: boolean read FAutoCheck write FAutoCheck default False;
    property RadioItem: boolean read FRadioItem write FRadioItem default False;

    property IsDefault: boolean read FDefault write FDefault default False;
    property Visible: boolean read FVisible write FVisible default True;

    // Items
    property Items: FXPopupItems read FItems write FItems;

    // Data
    property Image: FXIconSelect read FImage write FImage;
    property Shortcut: string read FShortcut write FShortcut;

    // Separator
    property IsSeparator: boolean read GetSeparator write SetSeparator;

    property MenuIndex: integer read GetIndex;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Items
    property MenuItems[Index: Integer]: FXPopupContainer read GetMenuItem;

    function GetMenuItemCount: integer;

    // Stream Conversion
    procedure SaveToStream(AStream: TStream);
    procedure LoadFromStream(AStream: TStream);
  end;

  // General Component
  FXPopupComponent = class(FXPopupContainer)
  private
    // Animation
    FAnim: FXIntAnim;
    FAnimType: FXAnimateSelection;
    FAnimGoesUpwards: boolean;
    FMinimumWidth: integer;

    FCanCloseWhenLoseFocus: boolean;

    // Parent
    FParentPopup: FXPopupComponent;
    FParentMenu: FXPopupMenu;

    // Size and Position
    NormalHeight,
    NormalWidth: integer;

    FDropPoint: TPoint;

    // Form & Controls
    FForm: TForm;
    FGlassBlur: TControl;

    // Colors
    FCustomColors: FXColorSets;
    FDrawColors: FXColorSet;

    // Drawing internal
    FHoverOver: integer;
    FScrollOffset: integer;
    FScrollVisible: boolean;
    FMaxVisibleItems: integer;

    // Settings
    FItemPressed: boolean;
    FFlatMenu: boolean;
    FEnableRadius: boolean;
    FEnableBorder: boolean;

    // Animate
    procedure Animation;

    procedure DoAnimationStep(Sender: TObject; Step, TotalSteps: integer);

    // Items
    procedure SetHover(Index: integer);
    function IndexIsValid(Index: integer): boolean;

    procedure OpenItem(MenuIndex: integer);
    procedure ExecuteItem(AMenuIndex: integer);
    function GetOpenChildIndex: integer;
    function HasChildOpen: boolean;

    // Menu
    function IsOpen: boolean;

    // Scrolling
    procedure UpdateScrolling;
    procedure ScrollUp;
    procedure ScrollDown;

    // Glass interaction
    procedure GlassUp(Sender: TObject; Button: TMouseButton;
                      Shift: TShiftState; X, Y: Integer);
    procedure GlassDown(Sender: TObject; Button: TMouseButton;
                        Shift: TShiftState; X, Y: Integer);
    procedure GlassEnter(Sender: TObject);
    procedure GlassMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure GlassMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);

    // FXPopupMenu Only
    procedure CheckFocusLoss;

    // Form
    procedure FormPosition;

    procedure FormLoseFocus(Sender: TObject);
    procedure FormGainFocus(Sender: TObject);
    procedure FormKeyPress(ender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormOnShow(Sender: TObject);

    // Paint Glass
    procedure OnPaintControl(Sender: TObject);

    // Popup Internal
    procedure PopupAtPointS(Point: TPoint);

  protected
    property CustomColors: FXColorSets read FCustomColors write FCustomColors;

    property AnimationType: FXAnimateSelection read FAnimType write FAnimType default FXAnimateSelection.Linear;
    property MinimumWidth: integer read FMinimumWidth write FMinimumWidth;

    property FlatMenu: boolean read FFlatMenu write FFlatMenu default false;
    property EnableBorder: boolean read FEnableBorder write FEnableBorder default true;
    property EnableRadius: boolean read FEnableRadius write FEnableRadius default true;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Close
    procedure CloseMenu(FreeMem: boolean = false);
    procedure CloseWindowsBackwards;
    procedure CloseWindowsForward(CloseSelf: boolean = false);

    // Interface
    procedure UpdateTheme(const UpdateChildren: Boolean); override;
  end;

  // Popup Item
  FXPopupItem = class(FXPopupComponent)
  published
    // Notify Event
    property OnClick;
    property OnHover;
    property OnCheck;

    // Data
    property Text;
    property Hint;

    property Image;
    property Shortcut;

    // Status
    property Enabled;
    property Checked;
    property AutoCheck;
    property RadioItem;

    property IsDefault;
    property Visible;

    // Items
    property Items;

    // Separator
    property IsSeparator;

    property MenuIndex;
  end;

  // Popup Menu
  FXPopupMenu = class(FXPopupComponent)
  private
    FOnPopup: TNotifyEvent;
    FOnBeforePopup: TOnBeforePopup;
    FCloseOnCheck,
    FCloseOnExecute,
    FCloseOnNoExecuteClick: boolean;
    FOnItemClick: TOnItemClick;

  published
    property CustomColors;
    property FlatMenu;
    property AnimationType;
    property MinimumWidth;
    property EnableBorder;
    property EnableRadius;
    property Items;

    property OnPopup: TNotifyEvent read FOnPopup write FOnPopup;
    property OnBeforePopup: TOnBeforePopup read FOnBeforePopup write FOnBeforePopup;
    property OnItemClick: TOnItemClick read FOnItemClick write FOnItemClick;

    property CloseOnCheck: boolean read FCloseOnCheck write FCloseOnCheck default false;
    property CloseOnExecute: boolean read FCloseOnExecute write FCloseOnExecute default true;
    property CloseOnNoExecuteClick: boolean read FCloseOnNoExecuteClick write FCloseOnNoExecuteClick default true;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // TPopupMenu Inherited
    procedure Popup(X, Y: integer); //override;

    // Custom Implementations
    procedure PopupAtCursor;
    procedure PopupAtPoint(Point: TPoint);
  end;

implementation

uses
  CFX.Controls, CFX.BlurMaterial;

const
  POPUP_SCROLL_BUTTON_HEIGHT = 20;

{ FXPopupComponent }

procedure FXPopupComponent.ScrollUp;
begin
  if FScrollOffset > 0 then
  begin
    Dec(FScrollOffset);
    FGlassBlur.Repaint;
  end;
end;

procedure FXPopupComponent.ScrollDown;
var
  VisibleCount, I: Integer;
begin
  // Count visible items from scroll offset
  VisibleCount := 0;
  I := FScrollOffset;
  while (I < GetMenuItemCount) and (VisibleCount < FMaxVisibleItems) do
  begin
    if MenuItems[I].Visible then
      Inc(VisibleCount);
    Inc(I);
  end;

  if I < GetMenuItemCount then
  begin
    Inc(FScrollOffset);
    FGlassBlur.Repaint;
  end;
end;

procedure FXPopupComponent.GlassMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  Handled := true;
  if WheelDelta > 0 then
    ScrollUp
  else
    ScrollDown;
end;

procedure FXPopupComponent.UpdateScrolling;
var
  I, VisibleCount, CalcHeight, AvailableRoom: Integer;
begin
  // Calculate visible items
  VisibleCount := 0;
  for I := 0 to GetMenuItemCount - 1 do
    if MenuItems[I].Visible then
      Inc(VisibleCount);

  // Calculate max height
  CalcHeight := VisibleCount * POPUP_ITEM_HEIGHT + POPUP_SPACING_TOPBOTTOM * 2;

  if FAnimGoesUpwards then
    AvailableRoom := FDropPoint.Y - Screen.WorkAreaRect.Top
  else
    AvailableRoom := Screen.WorkAreaRect.Bottom - FDropPoint.Y;

  FScrollVisible := CalcHeight > AvailableRoom;

  if FScrollVisible then
  begin
    FMaxVisibleItems := Trunc((AvailableRoom -
      POPUP_SPACING_TOPBOTTOM * 2) / POPUP_ITEM_HEIGHT);
  end;
end;

{ FXPopupContainer }

constructor FXPopupContainer.Create(AOwner: TComponent);
begin
  inherited;
  FImage := FXIconSelect.Create(Self);
  FItems := FXPopupItems.Create(Self);

  Enabled := true;

  AutoCheck := false;
  Checked := false;
  RadioItem := false;
  Visible := true;

  Text := Name;
end;

destructor FXPopupContainer.Destroy;
var
  I: Integer;
begin
  FreeAndNil(FImage);
  // Free items
  for I := Items.Count-1 downto 0 do
    Items.Delete(I);

  inherited;
end;

function FXPopupContainer.GetIndex: integer;
var
  I: Integer;
begin
  Result := -1;

  if Owner is FXPopupContainer then
    for I := 0 to FXPopupContainer(Owner).GetMenuItemCount - 1 do
      if FXPopupContainer(Owner).MenuItems[I] = Self then
        Exit(I);
end;

function FXPopupContainer.GetMenuItem(Index: Integer): FXPopupContainer;
begin
  Result := Items.Item[Index]
end;

function FXPopupContainer.GetMenuItemCount: integer;
begin
  Result := Items.Count;
end;

function FXPopupContainer.HasSubItems: boolean;
begin
  Result := GetMenuItemCount <> 0;
end;

function FXPopupContainer.GetSeparator: boolean;
begin
  Result := Text = '-';
end;

procedure FXPopupContainer.LoadFromStream(AStream: TStream);
var
  Reader: TReader;
begin
  Reader := TReader.Create(AStream, 4096);
  try
    Reader.ReadSignature;
    // The FLoaded refrence need to be initialised
    Reader.BeginReferences;
    Reader.ReadComponent(Self);
  finally
    Reader.Free;
  end;
end;

procedure FXPopupContainer.SaveToStream(AStream: TStream);
var
  Writer: TWriter;
begin
  Writer := TWriter.Create(AStream, 4096);
  try
    Writer.WriteSignature;
    Writer.WriteComponent(Self);
  finally
    Writer.Free;
  end;
end;

procedure FXPopupContainer.SetChecked(const Value: boolean);
var
  I: integer;
begin
  FChecked := Value;

  if RadioItem and FChecked then
    if Owner is FXPopupContainer then
      begin
        with FXPopupContainer(Owner) do
          for I := Self.MenuIndex - 1 downto 0 do
            begin
              if (not MenuItems[I].RadioItem) then
                Break;

              MenuItems[I].FChecked := false;
            end;

        with FXPopupContainer(Owner) do
          for I := Self.MenuIndex + 1 to GetMenuItemCount - 1 do
            begin
              if (not MenuItems[I].RadioItem) then
                Break;

              MenuItems[I].FChecked := false;
            end;
      end;
end;

procedure FXPopupContainer.SetSeparator(const Value: boolean);
begin
  if Value then
    Text := '-';
end;

{ FXPopupMenu }

constructor FXPopupMenu.Create(AOwner: TComponent);
begin
  inherited;
  // Properties
  FAnimType := MENU_ANIMATION_SELECTION;

  FCloseOnCheck := false;
  CloseOnExecute := true;
  CloseOnNoExecuteClick := true;

  // Is Root Menu
  FParentMenu := Self;

  // Update Children (children not required)
  UpdateTheme(false);
end;

destructor FXPopupMenu.Destroy;
begin
  inherited;
end;

procedure FXPopupMenu.Popup(X, Y: integer);
begin
  inherited;
  PopupAtPoint(Point(X, Y));
end;

procedure FXPopupMenu.PopupAtCursor;
begin
  PopupAtPoint( Mouse.CursorPos );
end;

procedure FXPopupMenu.PopupAtPoint(Point: TPoint);
var
  CanPopup: boolean;
begin
  inherited;
  CanPopup := true;

  // Before
  if Assigned(OnBeforePopup) then
    OnBeforePopup(Self, CanPopup, Point);

  if CanPopup then
    begin
      // Popup
      PopupAtPointS( Point );

      // Notify Event
      if Assigned(OnPopup) then
        OnPopup( Self );
    end;
end;

{ FXPopupComponent }

procedure FXPopupComponent.Animation;
begin
  // Stop previous
  FAnim.Stop;

  // Prepare
  with FForm do
    begin
      // Prepare
      case FAnimType of
        FXAnimateSelection.Instant: begin
          Height := NormalHeight;
          Width := NormalWidth;
        end;
        FXAnimateSelection.Opacity: begin
          Height := NormalHeight;
          Width := NormalWidth;

          FAnim.StartValue := 0;
          FAnim.EndValue := 255;
        end;
        FXAnimateSelection.Linear: begin
          Width := NormalWidth;

          FAnimGoesUpwards := FDropPoint.Y > Top;

          FAnim.StartValue := NormalHeight div 8;
          FAnim.EndValue := NormalHeight;
        end;
        FXAnimateSelection.Square: begin
          FAnimGoesUpwards := FDropPoint.Y > Top;

          FAnim.StartValue := NormalHeight div 8;
          FAnim.EndValue := NormalHeight;
        end;
      end;
    end;

  // Start
  FAnim.Start;
end;

procedure FXPopupComponent.CheckFocusLoss;
var
  Focused: boolean;
  Item: FXPopupComponent;
begin
  if not FCanCloseWhenLoseFocus then
    Exit;

  // Focus
  Item := Self;
  Focused := FForm.Focused;

  while Item.HasChildOpen do
    begin
      Item := Item.Items[Item.GetOpenChildIndex];

      if Item.FForm.Focused then
        begin
          Focused := true;
          Break;
        end;
    end;

  if not Focused then
    Item.CloseWindowsBackwards;
end;

procedure FXPopupComponent.CloseWindowsBackwards;
begin
  CloseMenu(false);

  if FParentPopup is FXPopupComponent then
    FParentPopup.CloseWindowsBackwards;
end;

procedure FXPopupComponent.CloseWindowsForward(CloseSelf: boolean);
begin
  // Close
  if CloseSelf then
    CloseMenu(false);

  // Parent
  if HasChildOpen then
    Items[GetOpenChildIndex].CloseWindowsForward(true);
end;

procedure FXPopupComponent.CloseMenu(FreeMem: boolean);
begin
  // Child
  if FreeMem and (FForm <> nil) then
    begin
      FForm.Free;
      FForm := nil;
    end;

  // Close
  if IsOpen then
    FForm.Close;
end;

constructor FXPopupComponent.Create(AOwner: TComponent);
begin
  inherited;

  FScrollOffset := 0;
  FScrollVisible := false;
  FMaxVisibleItems := 10;

  FAnim := FXIntAnim.Create(nil);
  with FAnim do begin
    OnStep := DoAnimationStep;

    Kind := FXAnimationKind.ReverseExpo;

    LatencyAdjustments := true;
    LatencyCanSkipSteps := true;

    Duration := 0.15;

    StartValue := 0;
    EndValue := 255;
  end;

  FCustomColors := FXColorSets.Create(False);
  FDrawColors := FXColorSet.Create;
  FEnableBorder := MENU_ANIMATION_ENABLE_BORDER;
  FEnableRadius := MENU_ANIMATION_ENABLE_RADIUS;
  FFlatMenu := MENU_ANIMATION_FLAT;

  if (AOwner is FXPopupMenu) then
    begin
      FAnimType := (AOwner as FXPopupMenu).FAnimType;
      FCustomColors.Assign((AOwner as FXPopupMenu).CustomColors);
    end;
end;

destructor FXPopupComponent.Destroy;
begin
  FAnim.Stop;
  FreeAndNil( FAnim );

  inherited;
end;

procedure FXPopupComponent.DoAnimationStep(Sender: TObject; Step,
  TotalSteps: integer);
begin
  // Lazy path fix, too lazy to re-write entire popup menu library atm
  var AddonSize: integer := 0;

  // Update height, just in case
  if not (FAnimType in [FXAnimateSelection.Opacity]) then
    AddonSize := NormalHeight-FAnim.EndValue;

  // Anim
  with FForm do begin
    case FAnimType of
      FXAnimateSelection.Opacity: AlphaBlendValue := FAnim.CurrentValue;
      FXAnimateSelection.Linear: begin
        Height := FAnim.CurrentValue + AddonSize;

        if FAnimGoesUpwards then
          begin
            Top := FDropPoint.Y - FAnim.CurrentValue;
          end;
      end;
      FXAnimateSelection.Square: begin
        Height := FAnim.CurrentValue + AddonSize;
        Width := trunc(FAnim.CurrentValue / FAnim.EndValue * (NormalWidth - POPUP_ANIMATE_X_SIZE)) + POPUP_ANIMATE_X_SIZE;

        if FAnimGoesUpwards then
          begin
            Top := FDropPoint.Y - FAnim.CurrentValue;
          end;
      end;
    end;

    if FAnimType <> FXAnimateSelection.Opacity then
      FForm.AlphaBlendValue := 255;
  end;
end;

procedure FXPopupComponent.ExecuteItem(AMenuIndex: integer);
var
  Item: FXPopupItem;
begin
  Item := FXPopupItem(MenuItems[AMenuIndex]);

  // Execute
  if (not Item.HasSubItems) then
    begin
      // Check
      if Item.AutoCheck then
        if (not Item.RadioItem) xor (Item.RadioItem and not Item.Checked) then
          begin
            Item.Checked := not Item.Checked;

            // Notify
            if Assigned(Item.OnCheck) then
              Item.OnCheck(Item);
          end;

      // Close Menu
      if Assigned( Item.OnClick ) then
        begin
          if (FParentMenu.CloseOnExecute) then
            CloseWindowsBackwards;
        end
      else
        if Item.AutoCheck then
          begin
            if FParentMenu.CloseOnCheck then
              CloseWindowsBackwards;
          end
        else
          if (FParentMenu.CloseOnNoExecuteClick) then
            CloseWindowsBackwards;

      // Select Item
      if Assigned(FParentMenu.OnItemClick) then
        FParentMenu.OnItemClick(FParentMenu, Item, AMenuIndex);

      // Execute
      if Assigned( Item.OnClick ) then
        Item.OnClick(Item);
    end;
end;

procedure FXPopupComponent.FormGainFocus(Sender: TObject);
begin
  if HasChildOpen then
    CloseWindowsForward(false);
end;

procedure FXPopupComponent.FormKeyPress(ender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Direction, NewPos: integer;
  Valid: boolean;
begin
  case Key of
    27: CloseMenu;

    37: if not (Self is FXPopupMenu) then
      CloseMenu();

    39: if IndexIsValid(FHoverOver) then
      OpenItem(FHoverOver);

    13, 32: if IndexIsValid(FHoverOver) then
      ExecuteItem(FHoverOver);

    38, 40: begin
      // Up/Down
      if Key = 38 then
        Direction := -1
      else
        Direction := 1;

      // Add Value
      NewPos := FHoverOver;
      repeat
        Inc(NewPos, Direction);

        Valid := IndexIsValid(NewPos);
      until (not Valid) or (Valid and not MenuItems[NewPos].IsSeparator);

      if Valid then
        FHoverOver := NewPos;

      FGlassBlur.Repaint;
    end;
  end;
end;

procedure FXPopupComponent.FormLoseFocus(Sender: TObject);
begin
  if not HasChildOpen then
    FParentMenu.CheckFocusLoss;
end;

procedure FXPopupComponent.FormOnShow(Sender: TObject);
begin
  // Position
  FForm.Left := FDropPoint.X;
  FForm.Top := FDropPoint.Y;

  FormPosition;

  // Animate
  Animation;
end;

procedure FXPopupComponent.FormPosition;
var
  AHeight, AWidth: integer;
begin
  // Get Supposed values
  AHeight := NormalHeight;
  AWidth := NormalWidth;

  // Left
  with FForm do
    begin
      Left := FDropPoint.X;
      Top := FDropPoint.Y;

      // Set Position
      if Left < Screen.DesktopRect.Left then
        Left := Screen.DesktopRect.Left;
      if Top < Screen.DesktopRect.Top then
        Top := Screen.DesktopRect.Top;

      OutputDebugString( PChar(FForm.Left.ToString) );
      if Left + AWidth > Screen.DesktopRect.Right then
        Left := Mouse.CursorPos.X - AWidth;
      OutputDebugString( PChar(FForm.Left.ToString) );

      if Top + AHeight > Screen.DesktopRect.Bottom then
        Top := Mouse.CursorPos.Y - AHeight;
    end;
end;

function FXPopupComponent.GetOpenChildIndex: integer;
var
  I: Integer;
begin
  Result := -1;

  for I := 0 to GetMenuItemCount - 1 do
    if FXPopupItem(MenuItems[I]).IsOpen then
      Exit(I);
end;

procedure FXPopupComponent.GlassDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FItemPressed := true;
  SetHover(FHoverOver);
end;

procedure FXPopupComponent.GlassEnter(Sender: TObject);
begin
  FItemPressed := false;
  SetHover(FHoverOver);
end;

procedure FXPopupComponent.GlassMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  I, FHoverPrevious, VisibleCount: Integer;
  Hover: boolean;
  Item: FXPopupMenu;
begin
  // Previous
  FHoverPrevious := FHoverOver;

  // Search
  Hover := false;
  VisibleCount := 0;
  for I := 0 to GetMenuItemCount - 1 do
    if MenuItems[I] is FXPopupItem then
      begin
        if MenuItems[I].Visible then
        begin
          if (VisibleCount >= FScrollOffset) and (VisibleCount < FScrollOffset + FMaxVisibleItems) then
          begin
            if MenuItems[I].FBounds.Contains(Point(X,Y)) and MenuItems[I].Enabled then
              begin
                SetHover(I);

                Hover := true;

                Break;
              end;
          end;
          Inc(VisibleCount);
        end;
      end;

  /// None Found
  if not Hover then
    SetHover(-1);

  // Notify
  if (FHoverOver <> -1) and (FHoverOver <> FHoverPrevious) then
    begin
      FCanCloseWhenLoseFocus := false;
      try
        // Focus
        FForm.SetFocus;

        // Get Item
        Item := FXPopupMenu(MenuItems[FHoverOver]);

        // Close windows if exists
        if HasChildOpen then
          Items[GetOpenChildIndex].CloseMenu();

        // Hover
        if Assigned( Item.OnHover ) then
          Item.OnHover(Item);

        // Extend
        if Item.HasSubItems and not Item.IsOpen then
          OpenItem( FHoverOver );

        // Focus - good measure
        if not HasChildOpen then
          FForm.SetFocus;
      finally
        FCanCloseWhenLoseFocus := true;
      end;
    end;

  (FGlassBlur as FXWindowsControl).Redraw(false);
end;

procedure FXPopupComponent.GlassUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Item: FXPopupMenu;
begin
  FItemPressed := false;
  SetHover(FHoverOver);

  // Notify
  if IndexIsValid(FHoverOver) then
    begin
      Item := FXPopupMenu(MenuItems[FHoverOver]);

      // Execute
      ExecuteItem(FHoverOver);

      // Re-Open
      if Item.HasSubItems and not Item.IsOpen then
        OpenItem( FHoverOver );
    end;
end;

function FXPopupComponent.HasChildOpen: boolean;
begin
  Result := GetOpenChildIndex <> -1;
end;

function FXPopupComponent.IndexIsValid(Index: integer): boolean;
begin
  Result := (Index > -1) and (Index < GetMenuItemCount);
end;

function FXPopupComponent.IsOpen: boolean;
begin
  Result := (FForm <> nil) and FForm.Visible;
end;

procedure FXPopupComponent.OnPaintControl(Sender: TObject);
var
  I, X, ActualX, Y, BiggestWidth, VisibleCount: integer;
  R: TRect;
  Text: string;
  XPress: boolean;

  B: TGDIBrush;
  TextDrawFlags: TTextFormat;
  LineColor: TColor;
  LineOpacity: integer;

  RoundR: TRoundRect;

  // Raise
  AnyHasIcon,
  AnyCanBeChecked: boolean;
begin
  with FXBlurMaterial(FGlassBlur).Buffer do
    begin
      FForm.Font.Color := FDrawColors.ForeGround;

      Pen.Color := FDrawColors.ForeGround;
      Brush.Color := FDrawColors.ForeGround;
      Font.Color := FDrawColors.ForeGround;

      // Initiate Variabiles
      Y := POPUP_SPACING_TOPBOTTOM;
      BiggestWidth := 0;

      LineColor := Pen.Color;

      XPress := FItemPressed;

      // Invert for Dark Mode
      if ThemeManager.DarkTheme then
        XPress := not XPress;

      // Selected
      if XPress then
        LineOpacity := 40
      else
        LineOpacity := 20;

      // Brush
      B := GetRGB( Font.Color, LineOpacity ).MakeGDIBrush;

      // Text Output
      TextDrawFlags := [tfSingleLine, tfCenter, tfVerticalCenter];

      // Default Round
      RoundR.SetRoundness( POPUP_SELECTION_ROUND );

      // Get Status
      AnyHasIcon := false;
      AnyCanBeChecked := false;

      for I := 0 to GetMenuItemCount - 1 do
        if MenuItems[I].Image.Enabled then
          begin
            AnyHasIcon := true;
            Break;
          end;

      for I := 0 to GetMenuItemCount - 1 do
        if not MenuItems[I].HasSubItems and (MenuItems[I].AutoCheck or MenuItems[I].RadioItem or MenuItems[I].Checked) then
          begin
            AnyCanBeChecked := true;
            Break;
          end;

      // Draw
      var LastWasSeparator := true; // cannot start with separator
      const ACount = GetMenuItemCount;
      VisibleCount := 0;
      for I := 0 to ACount -1 do
        begin
          // Clear Bound
          MenuItems[I].FBounds := TRect.Empty;

          // Hidden or scrolled out of view
          if not MenuItems[I].Visible then
            Continue;

          if FScrollVisible and ((VisibleCount < FScrollOffset) or (VisibleCount >= FScrollOffset + FMaxVisibleItems)) then
          begin
            Inc(VisibleCount);
            Continue;
          end;

          // Analise
          if not MenuItems[I].IsSeparator then
            begin
              // NORMAL ITEM
              LastWasSeparator := false;
              X := POPUP_ITEM_SPACINT;

              Brush.Style := bsClear;

              // Highlight Item
              if I = FHoverOver then
                begin
                  RoundR.Rect := Rect(POPUP_LINE_SPACING, Y + POPUP_FRACTION_SPACE,
                    FForm.Width - POPUP_LINE_SPACING, Y + POPUP_ITEM_HEIGHT - POPUP_FRACTION_SPACE);
                  GDIRoundRect(RoundR, B, nil);
                end;

              // Checkmark / Radio
              if not MenuItems[I].HasSubItems
                and (MenuItems[I].AutoCheck or MenuItems[I].Checked or MenuItems[I].RadioItem) then
                begin
                  R := Rect( X, Y, X + POPUP_ITEM_SPACINT, Y + POPUP_ITEM_HEIGHT );

                  if MenuItems[I].Checked then
                    begin
                      if MenuItems[I].RadioItem then
                        Text := POPUP_RADIO
                      else
                        Text := POPUP_CHECKMARK;

                      Font.Assign( FForm.Font );
                      Font.Name := ThemeManager.IconFont;
                      if not MenuItems[I].Enabled then
                        Font.Color := POPUP_TEXT_DISABLED;

                      Font.Height :=  GetMaxFontHeight(FXBlurMaterial(FGlassBlur).Buffer, Text, R.Width, R.Height);

                      TextRect(R, Text, TextDrawFlags);
                    end;
                end;


              if AnyCanBeChecked then
                X := X + POPUP_ITEM_HEIGHT;

              // Icon
              with MenuItems[I].Image do
                if Enabled then
                  begin
                    Font.Assign( FForm.Font );
                    if not MenuItems[I].Enabled then
                      Font.Color := POPUP_TEXT_DISABLED;

                    R := Rect( X, Y, X + POPUP_ITEM_SPACINT, Y + POPUP_ITEM_HEIGHT );
                    DrawIcon(FXBlurMaterial(FGlassBlur).Buffer, R);
                  end;

              if MenuItems[I].Image.Enabled or (AnyHasIcon and not AnyCanBeChecked) then
                X := X + POPUP_ITEM_HEIGHT;

              // Text
              Text := MenuItems[I].Text;

              if MenuItems[I].IsDefault then
                Font.Style := [fsBold];

              Font.Assign( FForm.Font );

              if not MenuItems[I].Enabled then
                Font.Color := POPUP_TEXT_DISABLED;

              R := Rect(X, Y, X + TextWidth( Text ), Y + POPUP_ITEM_HEIGHT);

              TextRect( R, Text, TextDrawFlags);

              X := X + TextWidth(Text) + POPUP_ITEM_SPACINT;

              // Shortcut
              if (MenuItems[I].ShortCut <> '') and (not MenuItems[I].HasSubItems) then
                begin
                  ActualX := X;
                  Text := MenuItems[I].Shortcut;

                  Font.Assign( FForm.Font );
                  Font.Size := round( Font.Size * 2.5 / 3 );

                  if not MenuItems[I].Enabled then
                    Font.Color := POPUP_TEXT_DISABLED;

                  X := FForm.Width - POPUP_ITEM_SPACINT - TextWidth(Text);

                  R := Rect(X, Y, X + TextWidth( Text ), Y + POPUP_ITEM_HEIGHT);

                  TextRect( R, Text, TextDrawFlags);

                  X := ActualX;
                  X := X + TextWidth(Text) + POPUP_ITEM_SPACINT;
                end;

              // Sub Items
              if MenuItems[I].HasSubItems then
                begin
                  ActualX := X;
                  Text := #$E76C;

                  Font.Assign( FForm.Font );

                  Font.Name := ThemeManager.IconFont;
                  Font.Size := round( Font.Size * 2.8 / 3 );

                  X := FForm.Width - POPUP_ITEM_SPACINT - TextWidth(Text);

                  R := Rect(X, Y, X + TextWidth( Text ), Y + POPUP_ITEM_HEIGHT);

                  TextRect( R, Text, TextDrawFlags);

                  X := ActualX;
                  X := X + TextWidth(Text) + POPUP_ITEM_SPACINT;
                end;

              // Width
              if X > BiggestWidth then
                NormalWidth := X;

              if NormalWidth < POPUP_MINIMUM_WIDTH then
                NormalWidth := POPUP_MINIMUM_WIDTH;

              // Bounds
              with MenuItems[I].FBounds do
                begin
                  TopLeft.X := POPUP_LINE_SPACING;
                  TopLeft.y := Y;

                  BottomRight.X := NormalWidth - BiggestWidth;
                  BottomRight.Y := Y + POPUP_ITEM_HEIGHT;
                end;

              // Next
              Y := Y + POPUP_ITEM_HEIGHT;
            end
          else
            begin
              // SEPARATOR ITEM
              if LastWasSeparator then continue; // do not draw more that 1 separator in a row
              LastWasSeparator := true;

              X := POPUP_LINE_SPACING;

              Brush.Style := bsSolid;

              R := Rect(X, Y, FForm.Width - X, Y + POPUP_SEPARATOR_HEIGHT);

              //Rectangle( R );
              GDITint( R, LineColor, 100 );

              // Next
              Y := Y + POPUP_SEPARATOR_HEIGHT;
            end;

          Inc(VisibleCount);
        end;

      // End
      Y := Y + POPUP_SPACING_TOPBOTTOM;

      // Resize
      if (FForm.Height <> Y) and (FAnim = nil) then
        FForm.Height := Y;

      // Data
      NormalHeight := Y;

      // Final Border
      Brush.Style := bsClear;
      Pen.Width := 1;
      if FEnableRadius then
        for I := 0 to POPUP_MENU_ROUND - 1 do
          begin
            if I = POPUP_MENU_ROUND - 1 then
              begin
                if FEnableBorder then
                  Pen.Color := FDrawColors.Accent
                else
                  Break;
              end
            else
              Pen.Color := FORM_COMPOSITE_COLOR;
            RoundRect(ClipRect, I, I);
          end;
      if FEnableBorder and not FEnableRadius then begin
        Pen.Color := FDrawColors.Accent;
        Rectangle(ClipRect);
      end;
    end;

  // Upate normal width
  NormalWidth := Max(NormalWidth, Self.FMinimumWidth);
end;

procedure FXPopupComponent.OpenItem(MenuIndex: integer);
var
  Item: FXPopupMenu;
begin
  // Get Item
  Item := FXPopupMenu(MenuItems[MenuIndex]);

  // Clone Settings
  Item.FEnableRadius := FEnableRadius;
  Item.FEnableBorder := FEnableBorder;

  // Can be ran
  if not Item.HasSubItems then
    Exit;

  // Update Theme
  Item.UpdateTheme(false);

  // Parent
  Item.FParentPopup := Self;
  Item.FParentMenu := FParentMenu;

  // Open
  Item.PopupAtPointS(Point(FForm.Left + FForm.Width - POPUP_ITEMS_OVERLAY_DISTANCE,
    FGlassBlur.ClientToScreen(Point(0, Item.FBounds.Top)).Y ));
end;

procedure FXPopupComponent.PopupAtPointS(Point: TPoint);
begin
  FDropPoint := Point;
  FScrollOffset := 0; // Reset scrolling

  // Create
  if FForm = nil then
    begin
      FForm := TForm.Create(Self);

      with FForm do
        begin
          // Prepare Form
          Position := poDesigned;
          AlphaBlend := true;
          Caption := POPUP_CAPTION_DEFAULT;

          DoubleBuffered := true;

          BorderStyle := bsNone;

          TransparentColor := true;
          TransparentColorValue := FORM_COMPOSITE_COLOR;

          FormStyle := fsStayOnTop;

          Font.Name := ThemeManager.FormFont;
          Font.Height := ThemeManager.FormFontHeight;

          OnShow := FormOnShow;
          OnDeactivate := FormLoseFocus;
          OnActivate := FormGainFocus;
          OnKeyDown := FormKeyPress;

          // Math
          NormalHeight := 0;
          NormalWidth := 0;

          Width := POPUP_MINIMUM_WIDTH;

          // Create Blur
          FGlassBlur := FXBlurMaterial.Create( FForm );
          with FXBlurMaterial(FGlassBlur) do
            begin
              Parent := FForm;
              Align := alClient;

              Version := FXBlurVersion.Screenshot;
              if FParentMenu.FFlatMenu then
                Version := FXBlurVersion.None;

              CustomColors.Assign( FParentMenu.CustomColors );
              UpdateTheme(false);

              RefreshMode := FXGlassRefreshMode.Manual;
            end;

          // Form-Create
          with FXBlurMaterial(FGlassBlur) do
            begin
              OnPaintBuffer := OnPaintControl;

              OnMouseUp := GlassUp;
              OnMouseDown := GlassDown;
              OnMouseMove := GlassMove;
              OnMouseEnter := GlassEnter;
              OnMouseWheel := GlassMouseWheel;

              SyncroniseImage;
              OnPaintBuffer(FGlassBlur);
            end;
        end;
    end;

  // Update scrolling calculations
  UpdateScrolling;

  // Clear
  FCanCloseWhenLoseFocus := true;

  // Hide flicker for animation to take over
  FForm.AlphaBlendValue := 0;

  // Show
  FForm.Show;
end;

procedure FXPopupComponent.SetHover(Index: integer);
begin
  FHoverOver := Index;

  FXBlurMaterial(FGlassBlur).Invalidate;
end;

procedure FXPopupComponent.UpdateTheme(const UpdateChildren: Boolean);
var
  I: integer;
begin
  // Inherit from parent
  if Self.Owner is FXPopupComponent  then
    begin
      FAnimType := FXPopupComponent(Self.Owner).FAnimType;
      FDrawColors := FXPopupComponent(Self.Owner).FDrawColors;
    end
      else
        // Color
        if CustomColors.Enabled then
          begin
            FDrawColors := FXColorSet.Create( CustomColors, ThemeManager.DarkTheme );

            // Update Glass Blur
            if FGlassBlur <> nil then
              FXBlurMaterial(FGlassBlur).CustomColors.Assign( CustomColors );
          end
        else
          begin
            FDrawColors.LoadFrom(ThemeManager.SystemColorSet, ThemeManager.DarkTheme);
          end;

  // Update glass
  if Assigned(FGlassBlur) then
    (FGlassBlur as FXWindowsControl).UpdateTheme(true);

  // Redraw
  if (FGlassBlur <> nil) and not (csReading in ComponentState) then
    FGlassBlur.Repaint;

  // Update Children
  if IsOpen then
    if UpdateChildren then begin
      for I := 0 to Items.Count-1 do
        if Supports(Items[I], IFXComponent) then
          (Items[I] as IFXComponent).UpdateTheme(UpdateChildren);
    end;
end;

{ FXPopupItems }

procedure FXPopupItems.Add(AItem: FXPopupItem);
var
  Index: integer;
begin
  Index := Length(FItems);
  SetLength(FItems, Index + 1);

  FItems[Index] := AItem;
  Pointer((@FItems[Index].Owner)^) := Self.Owner;
end;

function FXPopupItems.AddNew: FXPopupItem;
begin
  Result := FXPopupItem.Create(Self.Owner as TComponent);

  Add(Result);
end;

procedure FXPopupItems.Clear(AndFree: boolean);
var
  I: Integer;
begin
  if AndFree then
    for I := High(FItems) downto 0 do
      Delete(I);
end;

procedure FXPopupItems.Clear;
begin
  Clear(true);
end;

function FXPopupItems.Count: integer;
begin
  Result := Length(FItems);
end;

constructor FXPopupItems.Create(AOwner: TPersistent);
begin
  inherited;
  SetLength(FItems, 0);
end;

procedure FXPopupItems.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('Item', ReadData, WriteData, true);
end;

procedure FXPopupItems.Delete(Index: integer; AndFree: boolean = true);
begin
  // Free item
  if AndFree then
    if FItems[Index] <> nil then
      FItems[Index].Free;

  // Remove from index
  TArrayUtils<FXPopupItem>.Delete(Index, FItems);
end;

destructor FXPopupItems.Destroy;
begin

  inherited;
end;

function FXPopupItems.GetItem(AIndex: Integer): FXPopupItem;
begin
  Result := FItems[AIndex];
end;

function FXPopupItems.IndexOf(AText: string): integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(FItems) do
    if Item[I].Text = AText then
      Exit(I);
end;

procedure FXPopupItems.ReadData(Stream: TStream);
var
  Count: Integer;
  I: Integer;
  AItem: FXPopupItem;
begin
  Stream.ReadBuffer(Count, SizeOf(Count));
  SetLength(FItems, Count);
  for I := 0 to Count - 1 do
  begin
    AItem := FXPopupItem.Create(TComponent(Owner));
    AItem.LoadFromStream(Stream);
    FItems[I] := AItem;
  end;
end;

procedure FXPopupItems.WriteData(Stream: TStream);
var
  Count: Integer;
  I: Integer;
begin
  Count := Length(FItems);

  Stream.WriteBuffer(Count, SizeOf(Count));
  for I := 0 to Count - 1 do
    FItems[I].SaveToStream(Stream);
end;

initialization
  RegisterClass(FXPopupContainer);
end.

