unit CFX.Edit;

interface
uses
  Classes,
  Winapi.Messages,
  Winapi.Windows,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.Clipbrd,
  Types,
  Math,
  UITypes,
  CFX.Translations,
  CFX.Colors,
  CFX.ThemeManager,
  CFX.Graphics,
  CFX.Constants,
  SysUtils,
  CFX.Classes,
  CFX.ComponentClasses,
  CFX.Accessibility,
  CFX.GDI,
  CFX.VarHelpers,
  CFX.Types,
  CFX.Linker,
  CFX.PopupMenu,
  CFX.Scrollbar,
  CFX.Controls;

const
  TEXTAREA_DEFAULT_WIDTH = 250;
  TEXTAREA_DEFAULT_HEIGHT = 120;
  TEXTAREA_UNDO_LIMIT = 200;
  TEXTAREA_CARET_WIDTH = 1;
  TEXTAREA_NEWLINE_SELECT_WIDTH = 6;
  TEXTAREA_SCROLL_LINES = 3;

type
  FXCustomEdit = class(FXWindowsControl)
  private
    var DrawRect, MainRect, LineRect, TxtRect: TRect;
    FCustomColors: FXCompleteColorSets;
    FCustomEditColors: FXSingleColorStateSets;
    FEditColors: FXSingleColorStateSet;
    FDrawColors: FXCompleteColorSet;
    FHandleUpDown: boolean;
    FLineSize: integer;
    FAutoSize: boolean;
    FText: string;
    FCutPosition,
    FPosition,
    FDrawPosition: integer;
    FSelLength: integer;
    FIndicatorWidth: integer;
    FReadOnly: boolean;
    FRoundness: integer;
    FLineColor: TColor;
    FLayout: TLayout;
    FLayoutHoriz: TLayout;
    FSelGoesLeft: boolean;
    FDownStart: integer;
    LastShiftState: TShiftState;
    FHistory: TStringList;
    FDefaultMenu: FXPopupMenu;
    FTextMarginX: integer;
    FTextMarginY: integer;
    FPassChar: char;
    FCanUndo: boolean;
    FEnableSelection: boolean;
    FCharCase: FXCharCase;
    FNumbersOnly: boolean;
    FClearSelOnExit: boolean;
    FOnChange: TNotifyEvent;
    FOnChangeValue: TNotifyEvent;
    FOnEnterPressed: TNotifyEvent;
    FTextHint: string;
    FDetail: FXDetailType;

    // Canvas
    function TextW(AText: string): integer;
    function TextH(AText: string): integer;

    // Handle Messages
    procedure WM_LButtonDown(var Msg: TWMMouse); message WM_LBUTTONDOWN;

    // Data
    procedure UpdateAutoSize;
    procedure UpdateLine;
    procedure UpdateDrawPosition;

    // Menu
    procedure PrepDefaultMenu;
    procedure PopupBeforePopup(Sender: TObject; var CanPopup: boolean; Point: TPoint);
    procedure PopupItemClick(Sender: TObject; Item: FXPopupComponent; Index: integer);

    // Text
    // Change text, notify OnChange & OnChangeValue
    procedure ChangeText(AText: string); virtual;
    // Change text, notify OnChangeValue
    procedure ChangeTextValue(AText: string);
    procedure DeleteChar(Index: integer);

    procedure ScrollForCursor; overload;
    procedure ScrollForCursor(ADrawPosition: integer); overload;

    procedure ApplyCharCase;
    function DrawText: string;

    // Getter
    function GetValue: int64;

    // Setters
    procedure SetAutoSizing(const Value: boolean);
    procedure SetLineSize(const Value: integer);
    procedure SetRoundness(const Value: integer);
    procedure SetPosition(const Value: integer);
    procedure SetLayout(const Value: TLayout);
    procedure SetSelLength(const Value: integer);
    procedure SetPasswordChar(const Value: char);
    procedure SetCanUndo(const Value: boolean);
    procedure SetEnableSelection(const Value: boolean);
    procedure SetCharCase(const Value: FXCharCase);
    procedure SetNumbersOnly(const Value: boolean);
    procedure SetValue(const Value: int64);
    procedure SetLayoutHoriz(const Value: TLayout);
    procedure SetTextHint(const Value: string);
    procedure SetDetail(const Value: FXDetailType);
    procedure SetTextMarginX(const Value: integer);
    procedure SetTextMarginY(const Value: integer);

  protected
    procedure PaintBuffer; override;

    // Accesibility
    function AccessibilityGetControlType: Integer; override;
    function AccessibilityGetControlTypeName: string; override;

    function AccessibilityGetName: string; override;

    function AccessibilityGetPattern(PatternId: Integer): IUnknown; override;

    function AccessibilityGetValue: string; override;
    function AccessibilityIsReadOnly: Boolean; override;
    function AccessibilitySetValue(const Value: string): Boolean; override;

    // Text
    procedure SetText(const Value: string); virtual;
    function GetText: string; virtual;

    (* Ensure the caret position and the selection length are always inside
       the bounds of the current text. Must be called after ANY modification
       of FText. *)
    procedure ValidatePositions;

    function SearchPosition(AX: integer): integer;
    function AnalizeCharSolid(C: char): boolean;
    function AnalizeCharSpace(C: char): boolean;
    function AnalizeCurrentCharSolid: boolean;
    function AnalizeCurrentCharSpace: boolean;
    function CurrentCharExists: boolean;
    function FindNext(From: integer; GoesLeft: boolean = false): integer;
    procedure SelectPoints(P1, P2: integer);

    //  Internal
    procedure UpdateColors; override;
    procedure UpdateRects; override;

    // Size
    procedure Sized; override;

    // State
    procedure InteractionStateChanged(AState: FXControlState); override;

    // Font
    procedure FontUpdate; override;

    // Utils
    function CanEdit: boolean;

    // Key Presses
    procedure EnterPressed; virtual;
    procedure HandleKeyDown(var CanHandle: boolean; Key: integer; ShiftState: TShiftState); override;
    procedure KeyPress(var Key: Char); override;

    // Mouse
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure DblClick; override;

    // Inherited
    procedure OpenPopupMenu(X, Y: integer); override;
    procedure DoEnter; override;
    procedure DoExit; override;

    // Text
    property Text: string read GetText write SetText;
    property PasswordChar: char read FPassChar write SetPasswordChar;

    property Value: int64 read GetValue write SetValue;

    property Position: integer read FPosition write SetPosition;
    property SelectionLength: integer read FSelLength write SetSelLength;
    property Layout: TLayout read FLayout write SetLayout default TLayout.Center;
    property LayoutHorizontal: TLayout read FLayoutHoriz write SetLayoutHoriz default TLayout.Beginning;

    // Settings
    property TextHint: string read FTextHint write SetTextHint;
    property ClearSelectionOnExit: boolean read FClearSelOnExit write FClearSelOnExit default true;
    property CanUndo: boolean read FCanUndo write SetCanUndo default true;
    property CharCase: FXCharCase read FCharCase write SetCharCase default FXCharCase.Both;
    property NumbersOnly: boolean read FNumbersOnly write SetNumbersOnly default false;
    property ReadOnly: boolean read FReadOnly write FReadOnly;
    property EnableSelection: boolean read FEnableSelection write SetEnableSelection default true;
    property AutoSize: boolean read FAutoSize write SetAutoSizing default true;
    property LineSize: integer read FLineSize write SetLineSize default EDIT_LINE_SIZE;
    property Roundness: integer read FRoundness write SetRoundness default EDIT_BORDER_ROUND;
    property TextMarginX: integer read FTextMarginX write SetTextMarginX;
    property TextMarginY: integer read FTextMarginY write SetTextMarginY;
    property Detail: FXDetailType read FDetail write SetDetail default FXDetailType.Underline;

  published
    // Text
    function SelectionStart: integer;
    function SelectionEnd: integer;
    function Selection: string;
    function TextLength: integer;
    function HasSelection: boolean;

    function ExtendsBounds: boolean;
    procedure ClearSelection;
    procedure DeleteSelection;
    procedure SelectAll;
    procedure Clear;

    procedure Undo;

    procedure CopyToClipBoard;
    procedure CutToClipBoard;
    procedure PasteFromClipBoard;

    // Custom Colors
    property CustomColors: FXCompleteColorSets read FCustomColors write FCustomColors stored true;
    property CustomEditColors: FXSingleColorStateSets read FCustomEditColors write FCustomEditColors;

    // Other
    property HandleUpDown: boolean read FHandleUpDown write FHandleUpDown default true;

    // Events
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChangeValue: TNotifyEvent read FOnChangeValue write FOnChangeValue;
    property OnEnterPressed: TNotifyEvent read FOnEnterPressed write FOnEnterPressed;

    // Inherited properties
    property Cursor default crIBeam;
    property Align;
    property Font;
    property Transparent;
    property Opacity;
    property PaddingFill;
    property Constraints;
    property Anchors;
    property Hint;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property ControlFlags;
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
    property OnMouseMove;

    //  Modify default props
    property ParentColor default true;

  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    // Interface
    function Background: TColor; override;
  end;

  FXEdit = class(FXCustomEdit)
  published
    property Text;
    property PasswordChar;

    property Value;

    property Position;
    property SelectionLength;
    property Layout default TLayout.Center;
    property LayoutHorizontal default TLayout.Beginning;

    // Settings
    property TextHint;
    property ClearSelectionOnExit;
    property CanUndo;
    property CharCase;
    property NumbersOnly;
    property ReadOnly;
    property EnableSelection;
    property AutoSize;
    property LineSize;
    property Roundness;
    property TextMarginX;
    property TextMarginY;
    property Detail;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

  // Number box
  FXNumberEdit = class(FXCustomEdit)
  private
    FValue: Extended;
    FNumberType: FXNumberType;
    FDecimals: integer;
    FRange: FNumberRange;
    FAllowPartialTyping: boolean;

    FOnNumberChanged: TNotifyEvent;

    // Updated
    procedure RangeUpdated(Sender: TObject);

    // Internal
    procedure UpdateTextValue;
    function TextUpdateTrySetValue: boolean; overload;
    function TextUpdateTrySetValue(AText: string): boolean; overload;

    // Getters
    function GetValueInt: int64;
    function GetValueCurrency: currency;
    function GetValueExtended: extended;

    // Setters
    procedure SetValue(Value: extended);
    procedure SetValueEx(Value: Extended; Update: boolean);
    procedure SetValueInt(Value: int64);
    procedure SetValueCurrency(Value: currency);
    procedure SetValueExtended(Value: extended);
    procedure SetDecimals(const Value: integer);
    procedure SetNumberType(const Value: FXNumberType);

  protected
    // Focus
    procedure DoExit; override;

    // Key
    procedure EnterPressed; override;

    // Text
    procedure SetText(const Value: string); override;
    procedure ChangeText(AText: string); override;

  published
    property Text stored false;

    property OnNumberChanged: TNotifyEvent read FOnNumberChanged write FOnNumberChanged;

    property Range: FNumberRange read FRange write FRange;

    property NumberType: FXNumberType read FNumberType write SetNumberType default FXNumberType.Integer;
    property Decimals: integer read FDecimals write SetDecimals default 2;
    property Value: extended read FValue write SetValue stored true;
    property AllowPartialTyping: boolean read FAllowPartialTyping write FAllowPartialTyping default false;

    property Position;
    property SelectionLength;
    property Layout default TLayout.Center;
    property LayoutHorizontal default TLayout.Beginning;

    // Settings
    property TextHint;
    property ClearSelectionOnExit;
    property CanUndo;
    property CharCase;
    property ReadOnly;
    property EnableSelection;
    property AutoSize;
    property LineSize;
    property Roundness;
    property TextMarginX;
    property TextMarginY;
    property Detail;
  public
    property ValueInt: int64 read GetValueInt write SetValueInt;
    property ValueExtended: extended read GetValueExtended write SetValueExtended;
    property ValueCurrency: currency read GetValueCurrency write SetValueCurrency;

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

  ///  <summary> One VISUAL line of the text area. A hard line (paragraph) may
  ///  be split into several visual lines when word wrapping is enabled. </summary>
  FXTextAreaLine = record
    Text: string;
    StartPos: integer;  // caret position (0 based) of the first character
    Len: integer;       // number of characters, line break NOT included
    BreakLen: integer;  // 0 = soft wrap, 1 = #13 or #10, 2 = #13#10
  end;

  FXCustomTextArea = class(FXWindowsControl)
  private
    var DrawRect, MainRect, LineRect, TxtRect: TRect;

    FCustomColors: FXCompleteColorSets;
    FCustomEditColors: FXSingleColorStateSets;
    FEditColors: FXSingleColorStateSet;
    FDrawColors: FXCompleteColorSet;

    FText: string;
    FLines: TArray<FXTextAreaLine>;
    FStrings: TStringList;
    FUpdatingStrings: boolean;

    // Caret & selection. The selection is the range between FAnchor and FCaret
    FCaret,
    FAnchor: integer;
    FDesiredX: integer; // used for up / down movement

    // Scrolling
    FScrollX,
    FScrollY: integer;
    FContentWidth: integer;

    // Settings
    FReadOnly: boolean;
    FWordWrap: boolean;
    FEnableSelection: boolean;
    FCanUndo: boolean;
    FWantReturns: boolean;
    FWantTabs: boolean;
    FShowScrollbar: boolean;
    FClearSelOnExit: boolean;
    FCharCase: FXCharCase;
    FMaxLength: integer;
    FLineSpacing: integer;
    FRoundness: integer;
    FLineSize: integer;
    FTextMarginX: integer;
    FTextMarginY: integer;
    FDetail: FXDetailType;
    FTextHint: string;

    FLineColor: TColor;

    FHistory: TStringList;
    FDefaultMenu: FXPopupMenu;

    // Real scrollbars (instead of hand-drawn ones)
    FVertScroll,
    FHorzScroll: FXScrollbar;
    FSyncingScrollbars: boolean;

    // Interaction
    FDownStart: integer;
    LastShiftState: TShiftState;

    // Events
    FOnChange: TNotifyEvent;
    FOnChangeValue: TNotifyEvent;

    // Canvas
    function TextW(const AText: string): integer;
    function TextH(const AText: string): integer;

    // Messages
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;

    // Lines
    procedure AddLine(const AText: string; AStart, ALen, ABreak: integer);
    procedure AddParagraph(const APara: string; AStart, ABreak: integer);
    procedure RebuildLines;

    // Menu
    procedure PrepDefaultMenu;
    procedure PopupBeforePopup(Sender: TObject; var CanPopup: boolean; Point: TPoint);
    procedure PopupItemClick(Sender: TObject; Item: FXPopupComponent; Index: integer);

    // Strings
    function GetLines: TStrings;
    procedure SetLines(const Value: TStrings);
    procedure StringsChanged(Sender: TObject);

    // Internal
    procedure UpdateLine;
    procedure PushUndo;

    procedure DoChange;
    procedure DoChangeValue;

    // Scrollbars
    procedure VScrollChange(Sender: TObject);
    procedure HScrollChange(Sender: TObject);
    (* Push FScrollX / FScrollY into the scrollbar controls without
       re-triggering VScrollChange / HScrollChange. *)
    procedure PushScrollToBars;

    // Getters
    function GetSelStart: integer;
    function GetSelLength: integer;
    function GetSelText: string;
    function GetLineCount: integer;
    function GetCaretLine: integer;
    function GetCaretColumn: integer;

    // Setters
    procedure SetTextEx(const Value: string);
    procedure SetWordWrap(const Value: boolean);
    procedure SetLineSpacing(const Value: integer);
    procedure SetRoundness(const Value: integer);
    procedure SetLineSize(const Value: integer);
    procedure SetDetail(const Value: FXDetailType);
    procedure SetTextMarginX(const Value: integer);
    procedure SetTextMarginY(const Value: integer);
    procedure SetTextHint(const Value: string);
    procedure SetCharCase(const Value: FXCharCase);
    procedure SetShowScrollbar(const Value: boolean);
    procedure SetCanUndo(const Value: boolean);
    procedure SetEnableSelection(const Value: boolean);
    procedure SetScrollY(const Value: integer);
    procedure SetScrollX(const Value: integer);
    procedure SetSelStart(const Value: integer);
    procedure SetSelLength(const Value: integer);
    procedure SetMaxLength(const Value: integer);
    procedure SetCaretPosition(const Value: integer);

  protected
    procedure PaintBuffer; override;

    // Accesibility
    function AccessibilityGetControlType: Integer; override;
    function AccessibilityGetControlTypeName: string; override;
    function AccessibilityGetName: string; override;
    function AccessibilityGetPattern(PatternId: Integer): IUnknown; override;
    function AccessibilityGetValue: string; override;
    function AccessibilityIsReadOnly: Boolean; override;
    function AccessibilitySetValue(const Value: string): Boolean; override;

    // Internal
    procedure UpdateColors; override;
    procedure UpdateRects; override;
    procedure UpdateAll;

    procedure FontUpdate; override;
    procedure InteractionStateChanged(AState: FXControlState); override;

    // Text
    function GetText: string; virtual;
    procedure SetText(const Value: string); virtual;
    procedure ApplyCharCase;

    (* Ensure the caret & the anchor are always valid positions in the text *)
    procedure ValidatePositions;

    // Editing
    procedure ApplyTextChange(const ANewText: string; ACaret: integer);
    procedure InsertText(const AText: string);

    // Navigation utils
    function StepLeft(APos: integer): integer;
    function StepRight(APos: integer): integer;
    function IsWordChar(C: char): boolean;
    function WordLeft(APos: integer): integer;
    function WordRight(APos: integer): integer;

    // Geometry
    function LineHeight: integer;
    function ContentHeight: integer;
    function MaxScrollY: integer;
    function MaxScrollX: integer;
    function VisibleLines: integer;
    function LineFromPosition(APos: integer): integer;
    function PositionOfLineStart(ALine: integer): integer;
    function PositionOfLineEnd(ALine: integer): integer;
    function XFromPosition(APos: integer): integer; // relative to text left
    function PositionFromPoint(X, Y: integer): integer;

    procedure EnsureCaretVisible;

    // Caret
    procedure MoveCaret(ANewPos: integer; AExtend: boolean);

    // Keys
    procedure HandleKeyDown(var CanHandle: boolean; Key: integer; ShiftState: TShiftState); override;
    procedure KeyPress(var Key: Char); override;

    // Mouse
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DblClick; override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;

    // Inherited
    procedure OpenPopupMenu(X, Y: integer); override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure Loaded; override;

    // Properties (published by the descendant)
    property Text: string read GetText write SetTextEx;
    property Lines: TStrings read GetLines write SetLines;

    property CaretPosition: integer read FCaret write SetCaretPosition;
    property SelStart: integer read GetSelStart write SetSelStart;
    property SelLength: integer read GetSelLength write SetSelLength;

    property ScrollX: integer read FScrollX write SetScrollX;
    property ScrollY: integer read FScrollY write SetScrollY;

    property TextHint: string read FTextHint write SetTextHint;
    property WordWrap: boolean read FWordWrap write SetWordWrap default true;
    property ReadOnly: boolean read FReadOnly write FReadOnly default false;
    property MaxLength: integer read FMaxLength write SetMaxLength default 0;
    property CanUndo: boolean read FCanUndo write SetCanUndo default true;
    property EnableSelection: boolean read FEnableSelection write SetEnableSelection default true;
    property ClearSelectionOnExit: boolean read FClearSelOnExit write FClearSelOnExit default false;
    property CharCase: FXCharCase read FCharCase write SetCharCase default FXCharCase.Both;
    property WantReturns: boolean read FWantReturns write FWantReturns default true;
    property WantTabs: boolean read FWantTabs write FWantTabs default false;
    property ShowScrollbar: boolean read FShowScrollbar write SetShowScrollbar default true;
    property LineSpacing: integer read FLineSpacing write SetLineSpacing default 2;
    property LineSize: integer read FLineSize write SetLineSize default EDIT_LINE_SIZE;
    property Roundness: integer read FRoundness write SetRoundness default EDIT_BORDER_ROUND;
    property TextMarginX: integer read FTextMarginX write SetTextMarginX;
    property TextMarginY: integer read FTextMarginY write SetTextMarginY;
    property Detail: FXDetailType read FDetail write SetDetail default FXDetailType.Outline;

  published
    // Custom Colors
    property CustomColors: FXCompleteColorSets read FCustomColors write FCustomColors stored true;
    property CustomEditColors: FXSingleColorStateSets read FCustomEditColors write FCustomEditColors;

    // Events
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChangeValue: TNotifyEvent read FOnChangeValue write FOnChangeValue;

  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    // Interface
    function Background: TColor; override;

    // Info
    property LineCount: integer read GetLineCount;
    property CaretLine: integer read GetCaretLine;
    property CaretColumn: integer read GetCaretColumn;
    property SelText: string read GetSelText;

    // Commands
    function HasSelection: boolean;
    function TextLength: integer;
    procedure SelectAll;
    procedure ClearSelection;
    procedure DeleteSelection;
    procedure Clear;
    procedure Undo;

    procedure CopyToClipBoard;
    procedure CutToClipBoard;
    procedure PasteFromClipBoard;

    procedure ScrollToCaret;
    procedure ScrollToTop;
    procedure ScrollToBottom;

    // Utils
    function GetLineText(AIndex: integer): string;
  end;

  FXTextArea = class(FXCustomTextArea)
  published
    property Lines;
    property TextHint;
    property WordWrap;
    property ReadOnly;
    property MaxLength;
    property CanUndo;
    property EnableSelection;
    property ClearSelectionOnExit;
    property CharCase;
    property WantReturns;
    property WantTabs;
    property ShowScrollbar;
    property LineSpacing;
    property LineSize;
    property Roundness;
    property TextMarginX;
    property TextMarginY;
    property Detail;

    // Inherited properties
    property Cursor default crIBeam;
    property Align;
    property Font;
    property Transparent;
    property Opacity;
    property PaddingFill;
    property Constraints;
    property Anchors;
    property Hint;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property ControlFlags;
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
    property OnMouseMove;
    property OnResize;

  public
    // Text is available at runtime (the Lines property is streamed)
    property Text;
    property CaretPosition;
    property SelStart;
    property SelLength;
    property ScrollX;
    property ScrollY;
  end;

implementation

(* Replace every kind of line break with a #13#10 sequence *)
function NormaliseLineBreaks(const AText: string): string;
begin
  Result := AText;
  Result := StringReplace(Result, #13#10, #10, [rfReplaceAll]);
  Result := StringReplace(Result, #13, #10, [rfReplaceAll]);
  Result := StringReplace(Result, #10, #13#10, [rfReplaceAll]);
end;

(* Remove a single trailing line break, as added by TStrings.Text *)
function RemoveTrailingBreak(const AText: string): string;
begin
  Result := AText;
  if Result.EndsWith(#13#10) then
    Result := Copy(Result, 1, Length(Result)-2)
  else
    if Result.EndsWith(#10) or Result.EndsWith(#13) then
      Result := Copy(Result, 1, Length(Result)-1);
end;

{ FXCustomEdit }

procedure FXCustomEdit.KeyPress(var Key: Char);
begin
  inherited;
  if Key = #13 then begin
    EnterPressed;
    Exit;
  end;

  // Invalid
  if CharInSet(Key, [#8, #27, #9, #10]) then
    Exit;

  // Special keys
  if (ssCtrl in LastShiftState) or (ssAlt in LastShiftState) then
    Exit;

  // Numbers Only
  if NumbersOnly and not CharInSet(Key, ['0'..'9']) then
    Exit;

  // Read Only
  if ReadOnly then
    Exit;

  // Safety
  ValidatePositions;

  // Override Selection
  if HasSelection then
    DeleteSelection;

  // Safety (the selection deletion has changed the text)
  ValidatePositions;

  // Add
  ChangeText( Copy(FText, 1, FPosition) + Key + Copy(FText, FPosition+1, Length(FText)) );
  Position := Position + 1;
end;

function FXCustomEdit.TextLength: integer;
begin
  Result := Length(FText);
end;

procedure FXCustomEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  PosX, Value: integer;
begin
  inherited;
  if DoubleClickInProgress then
    Exit;

  if InteractionState = FXControlState.Press then
    begin
      if (FDownStart <> -1) and EnableSelection then
        begin
          PosX := X - TxtRect.Left + FCutPosition;
          if PosX < 0 then
            PosX := 0;

          if PosX >= 0 then
            begin
              Value := SearchPosition(PosX);

              // Set position
              FSelLength := abs(FDownStart-Value);

              if Value < FDownStart then
                FPosition := Value
              else
                FPosition := FDownStart;

              // Safety
              ValidatePositions;

              // Bring cursor to focus
              ScrollForCursor( TextW(Copy(FText, 1, Value))-FCutPosition );

              // Invalidate
              StandardUpdateDraw;
            end;
        end;
    end;
end;

procedure FXCustomEdit.OpenPopupMenu(X, Y: integer);
begin
  if not Assigned(PopupMenu) then
    FDefaultMenu.PopupAtPoint(ClientToScreen(Point(X, Y)))
  else
    inherited;
end;

function FXCustomEdit.GetText: string;
begin
  Result := FText;
end;

function FXCustomEdit.GetValue: int64;
begin
  Result := 0;
  if FText <> '' then
    try
      Result := FText.ToInt64;
    except

    end;
end;

procedure FXCustomEdit.Undo;
var
  Index: integer;
begin
  if not FCanUndo then
    Exit;

  if FHistory.Count > 0 then
    begin
      Index := FHistory.Count-1;
      FText := FHistory[Index];

      FHistory.Delete(Index);

      (* THE TEXT HAS CHANGED, the caret and the selection may now point
         outside of the new text *)
      ValidatePositions;

      // Notify
      if Assigned(OnChange) then
        OnChange(Self);

      // Notify
      if Assigned(OnChangeValue) then
        OnChangeValue(Self);

      // Update
      StandardUpdateLayout;
    end;
end;

procedure FXCustomEdit.UpdateAutoSize;
begin
  if FAutoSize then
    if CanUpdate then
      SetBounds(Left, Top, Width, TextH('Aa.') + LineSize + FTextMarginY * 2)
end;

procedure FXCustomEdit.UpdateColors;
begin
  // Access theme manager
  FDrawColors.Assign( ThemeManager.SystemColor );
  if FCustomColors.Enabled then
    // Custom Colors
    FDrawColors.LoadFrom(FCustomColors, ThemeManager.DarkTheme)
  else begin
    if ThemeManager.DarkTheme then
      FDrawColors.BackGroundInterior := ChangeColorLight(ThemeManager.SystemColor.BackGroundInterior, EDIT_COLOR_CHANGE * 4)
    else
      FDrawColors.BackGroundInterior := ChangeColorLight(ThemeManager.SystemColor.BackGroundInterior, -EDIT_COLOR_CHANGE * 4);
  end;

  // Edit colors
  if FCustomEditColors.Enabled then
    begin
      FEditColors.LoadColors(FCustomEditColors, ThemeManager.DarkTheme);
    end
  else
    begin
      FEditColors.None := ChangeColorLight(ThemeManager.SystemColor.BackGroundInterior, EDIT_COLOR_CHANGE);
      FEditColors.Hover := ChangeColorLight(FEditColors.None, EDIT_COLOR_CHANGE);
      FEditColors.Press := ThemeManager.SystemColor.BackGround;
    end;

  // Update Popup Menu
  FDefaultMenu.UpdateTheme(false);

  // Update
  Font.Color := FDrawColors.ForeGround;
  UpdateLine;
end;

procedure FXCustomEdit.UpdateDrawPosition;
begin
  FDrawPosition := TextW( Copy(DrawText, 1, FPosition) ) - FCutPosition;
end;

procedure FXCustomEdit.UpdateLine;
begin
  if Focused then
    FLineColor := FDrawColors.Accent
  else
    FLineColor := FDrawColors.BackGroundInterior;

  // Line
  StandardUpdateDraw;
end;

procedure FXCustomEdit.UpdateRects;
begin
  // Safety, the text may have been changed from the outside
  ValidatePositions;

  DrawRect := ClientRect;
  MainRect := ContentRect;

  // Rects
  LineRect := MainRect;
  LineRect.Top := Height - LineSize;

  // Pos
  TxtRect := MainRect;
  TxtRect.Inflate(-TextMarginX, -TextMarginY);

  TxtRect.Bottom := TxtRect.Bottom - LineSize;

  // Margin
  case Detail of
    FXDetailType.Underline: TxtRect.Bottom := TxtRect.Bottom - LineSize;
    FXDetailType.Outline: TxtRect.Inflate(-LineSize, -LineSize);
  end;

  // Draw
  UpdateDrawPosition;
  if not HasSelection then
    ScrollForCursor;
end;

procedure FXCustomEdit.ValidatePositions;
var
  ATotal: integer;
begin
  ATotal := Length(FText);

  if FPosition < 0 then
    FPosition := 0;
  if FPosition > ATotal then
    FPosition := ATotal;

  if FSelLength < 0 then
    FSelLength := 0;
  if FPosition + FSelLength > ATotal then
    FSelLength := ATotal - FPosition;
  if FSelLength < 0 then
    FSelLength := 0;

  if FDownStart > ATotal then
    FDownStart := ATotal;
end;

constructor FXCustomEdit.Create(aOwner: TComponent);
begin
  inherited;
  ParentColor := false;
  TabStop := true;
  AutoFocusLine := false;
  BufferedComponent := true;
  FIndicatorWidth := EDIT_INDIC_WIDTH;
  FRoundness := EDIT_BORDER_ROUND;
  FTextMarginX := EDIT_EXTRA_SPACE;
  FTextMarginY := EDIT_EXTRA_SPACE;
  FLayout := TLayout.Center;
  FLayoutHoriz := TLayout.Beginning;
  FCanUndo := true;
  FEnableSelection := true;
  FClearSelOnExit := true;
  FDownStart := -1;

  FAutoSize := true;
  FLineSize := EDIT_LINE_SIZE;

  FHandleUpDown := true;
  FPassChar := #0;

  Cursor := crIBeam;

  FDetail := FXDetailType.Underline;

  // History
  FHistory := TStringList.Create;

  // Menu
  FDefaultMenu := FXPopupMenu.Create(Self);
  FDefaultMenu.OnItemClick := PopupItemClick;
  FDefaultMenu.OnBeforePopup := PopupBeforePopup;
  PrepDefaultMenu;

  // Text
  FText := 'Hello World!';
  FPosition := 0;
  FDrawPosition := 0;
  FCutPosition := 0;

  // Custom Color
  FCustomColors := FXCompleteColorSets.Create(Self);
  FCustomEditColors := FXSingleColorStateSets.Create(Self);

  FDrawColors := FXCompleteColorSet.Create;
  FEditColors := FXSingleColorStateSet.Create;

  // Sizing
  Height := EDIT_DEFAULT_HEIGHT;
  Width := EDIT_DEFAULT_WIDTH;

  Font.Color := 255;

  // Font
  Font.Height := ThemeManager.FormFontHeight;
end;

function FXCustomEdit.CurrentCharExists: boolean;
begin
  Result := (FPosition >= 0) and (FPosition + 1 <= Length(FText));
end;

procedure FXCustomEdit.CutToClipBoard;
begin
  if PasswordChar <> #0 then
    Exit;

  CopyToClipBoard;

  DeleteSelection;
end;

procedure FXCustomEdit.CopyToClipBoard;
begin
  if PasswordChar <> #0 then
    Exit;

  if not HasSelection then
    Exit;

  Clipboard.AsText := Selection;
end;

procedure FXCustomEdit.DblClick;
var
  P1, P2: integer;
  FStart: integer;
begin
  inherited;

  // Safety
  ValidatePositions;

  // Start
  FStart := Position;
  if not AnalizeCurrentCharSpace then
    Inc(FStart);

  // Find margins
  P1 := FindNext(FStart, true);
  P2 := FindNext(Position, false);

  SelectPoints(P1, P2);
end;

procedure FXCustomEdit.DeleteChar(Index: integer);
begin
  // Out of range protection
  if (Index < 1) or (Index > Length(FText)) then
    Exit;

  ChangeText( FText.Remove(Index-1, 1) );
end;

procedure FXCustomEdit.DeleteSelection;
begin
  ValidatePositions;

  if FSelLength > 0 then
    begin
      ChangeText( Copy(FText, 1, FPosition) + Copy(FText, 1+FPosition+FSelLength, Length(FText)) );

      FSelLength := 0;

      ValidatePositions;

      Redraw;
    end;
end;

destructor FXCustomEdit.Destroy;
begin
  FreeAndNil( FCustomColors );
  FreeAndNil( FDrawColors );
  FreeAndNil( FCustomEditColors );
  FreeAndNil( FEditColors );
  FreeAndNil( FHistory );
  inherited;
end;

procedure FXCustomEdit.DoEnter;
begin
  inherited;

  UpdateLine;
end;

procedure FXCustomEdit.DoExit;
begin
  inherited;

  if NumbersOnly and (FText = '') then
    Text := '0';

  if FClearSelOnExit and HasSelection then
    SelectionLength := 0;

  UpdateLine;
end;

function FXCustomEdit.DrawText: string;
var
  I: Integer;
begin
  Result := '';
  if PasswordChar = #0 then
    Result := Text
  else
    for I := 1 to Length(Text) do
      Result := Result + PasswordChar;
end;

procedure FXCustomEdit.EnterPressed;
begin
  //
  if Assigned(FOnEnterPressed) then
    FOnEnterPressed(Self);
end;

function FXCustomEdit.ExtendsBounds: boolean;
begin
  Result := TextW(DrawText) > TxtRect.Width;
end;

function FXCustomEdit.FindNext(From: integer; GoesLeft: boolean): integer;
var
  ATotal: integer;
begin
  ATotal := TextLength;

  // Clamp
  if From < 0 then
    From := 0;
  if From > ATotal then
    From := ATotal;

  // Password Char
  if PasswordChar <> #0 then
    begin
      if GoesLeft then
        Result := 0
      else
        Result := ATotal;

      Exit;
    end;

  // Data
  Result := From;

  // Search characters
  if GoesLeft then
    while Result >= 1 do
      begin
        Dec(Result);

        if (Result = 0) or AnalizeCharSpace(FText[Result]) then
          begin
            Exit(Result);
          end;
      end
    else
      while Result <= ATotal-1 do
      begin
        Inc(Result);

        if (Result >= 1) and (Result <= ATotal) and AnalizeCharSpace(FText[Result]) then
          begin
            Exit(Result);
          end;
      end;
end;

procedure FXCustomEdit.FontUpdate;
begin
  Redraw;

  UpdateAutoSize;
end;

procedure FXCustomEdit.HandleKeyDown(var CanHandle: boolean; Key: integer;
  ShiftState: TShiftState);
procedure IncPosition(By: integer);
begin
  FPosition := FPosition + By;
  ValidatePositions;
  UpdateRects;
  Redraw;
end;
var
  NewPos: integer;
  SelectionDone: boolean;
begin
  inherited;

  // Safety
  ValidatePositions;

  SelectionDone := false;
  LastShiftState := ShiftState;

  if FHandleUpDown then
    case Key of
      VK_UP: Key := VK_LEFT;
      VK_DOWN: Key := VK_RIGHT;
    end;

  case Key of
    // Left & Right
    VK_LEFT: begin
      SelectionDone := true;

      // Select
      if FSelLength = 0 then
        FSelGoesLeft := true;

      if ssCtrl in ShiftState then
        begin
          NewPos := FindNext(SelectionEnd, true);

          if ssShift in ShiftState then
            SelectPoints(NewPos, SelectionStart)
          else
            Position := NewPos;
        end
      else
        if (ssShift in ShiftState) and EnableSelection then
          begin
            if FSelGoesLeft then
              begin
                if FPosition > 0 then
                  begin
                    IncPosition(-1);
                    SelectionLength := SelectionLength+1;
                  end;
              end
            else
              SelectionLength := SelectionLength-1;
          end
        else
          // Left
          begin
            if HasSelection then
              ClearSelection
            else
              Position := Position - 1;
          end;

      CanHandle := false;
    end;

    VK_RIGHT: begin
      SelectionDone := true;

      if FSelLength = 0 then
        FSelGoesLeft := false;

      // Select
      if ssCtrl in ShiftState then
        begin
          NewPos := FindNext(SelectionEnd, false);

          if ssShift in ShiftState then
            SelectPoints(NewPos, SelectionStart)
          else
            Position := NewPos;
        end
      else
        if (ssShift in ShiftState) and EnableSelection then
          begin
            if FSelGoesLeft then
              begin
                if FPosition < TextLength then
                  begin
                    IncPosition(1);
                    SelectionLength := SelectionLength-1;
                  end;
              end
            else
              SelectionLength := SelectionLength+1;
          end
        else
          // Right
          begin
            if HasSelection then
              begin
                Position := Position + SelectionLength;
                ClearSelection;
              end
            else
              Position := Position + 1;
          end;

      CanHandle := false;
    end;

    // Home & End
    VK_HOME: begin
      SelectionDone := true;
      if (ssShift in ShiftState) and EnableSelection then
        SelectPoints(0, SelectionStart)
      else
        Position := 0;

      CanHandle := false;
    end;

    VK_END: begin
      SelectionDone := true;
      if (ssShift in ShiftState) and EnableSelection then
        SelectPoints(TextLength, SelectionStart)
      else
        Position := TextLength;

      CanHandle := false;
    end;

    // Del + Backspace
    VK_BACK: if CanEdit then begin
      if HasSelection then
        DeleteSelection
      else
        // Backspace
        if Position > 0 then
          begin
            if ssCtrl in ShiftState then
              begin
                NewPos := FindNext(SelectionEnd, true);
                SelectPoints(Position, NewPos);
                DeleteSelection;
              end
            else
              begin
                DeleteChar(Position);
                IncPosition(-1);

                Redraw;
                CanHandle := false;
              end;
          end;
    end;

    VK_DELETE: if CanEdit then begin
      if HasSelection then
        Self.DeleteSelection
      else
        // Delete
        if Position < TextLength then
          begin
            DeleteChar(Position+1);

            Redraw;
            CanHandle := false;
          end;
    end;

    // Keyboard shortcuts
    65: if ssCtrl in ShiftState then
      SelectAll;
    67: if ssCtrl in ShiftState then
      try
        CopyToClipBoard;
      except
      end;
    86: if CanEdit and (ssCtrl in ShiftState) then
      try
        PasteFromClipBoard;
      except
      end;
    88: if CanEdit and (ssCtrl in ShiftState) then
        try
          CutToClipBoard;
        except
        end;
    90: if CanEdit and (ssCtrl in ShiftState) then
      Undo;
  end;

  // Safety
  ValidatePositions;

  // Selection common
  if SelectionDone and (SelectionLength > 0) then begin
    var ScrollCursor: integer;
    if FSelGoesLeft then
      ScrollCursor := FPosition
    else
      ScrollCursor := SelectionLength+FPosition;

    // When selecting text with Ctrl+Shift, this code ensures that the END of the selection is visible
    ScrollForCursor( TextW( Copy(DrawText, 1, ScrollCursor) )-FCutPosition );

    // Draw
    StandardUpdateDraw;
  end;
end;

function FXCustomEdit.HasSelection: boolean;
begin
  Result := FSelLength > 0;
end;

procedure FXCustomEdit.InteractionStateChanged(AState: FXControlState);
begin
  inherited;
end;

function FXCustomEdit.AccessibilityGetControlType: Integer;
begin
  Result := UIA_EditControlTypeId;
end;

function FXCustomEdit.AccessibilityGetControlTypeName: string;
begin
  Result := 'edit';
end;

function FXCustomEdit.AccessibilityGetName: string;
begin
  Result := TextHint;
end;

function FXCustomEdit.AccessibilityGetPattern(PatternId: Integer): IUnknown;
begin
  Result := nil;

  case PatternId of
    UIA_ValuePatternId:
      Result := TFXValueProvider.Create(Self);
  else
    Result := inherited AccessibilityGetPattern(PatternId);
  end;
end;

function FXCustomEdit.AccessibilityGetValue: string;
begin
  Result := Text;
end;

function FXCustomEdit.AccessibilityIsReadOnly: Boolean;
begin
  Result := ReadOnly;
end;

function FXCustomEdit.AccessibilitySetValue(const Value: string): Boolean;
begin
  if ReadOnly then
    Exit(False);

  Text := Value;
  Result := True;
end;

function FXCustomEdit.AnalizeCharSolid(C: char): boolean;
begin
  Result := CharInSet(C, ['A'..'Z', 'a'..'z', '0'..'9']);
end;

function FXCustomEdit.AnalizeCharSpace(C: char): boolean;
begin
  Result := CharInSet(C, [' ', '_']);
end;

function FXCustomEdit.AnalizeCurrentCharSolid: boolean;
begin
  Result := false;
  if CurrentCharExists then
    Result := AnalizeCharSolid( FText[Position+1] );
end;

function FXCustomEdit.AnalizeCurrentCharSpace: boolean;
begin
  Result := false;
  if CurrentCharExists then
    Result := AnalizeCharSpace( FText[Position+1] );
end;

procedure FXCustomEdit.ApplyCharCase;
begin
  case CharCase of
    FXCharCase.Uppercase: FText := UpperCase(FText);
    FXCharCase.Lowercase: FText := Lowercase(FText);
  end;

  // The length may have changed with certain locales
  ValidatePositions;
end;

function FXCustomEdit.Background: TColor;
begin
  Result := FDrawColors.Background;
end;

function FXCustomEdit.CanEdit: boolean;
begin
  Result := not FReadOnly;
end;

procedure FXCustomEdit.ChangeText(AText: string);
begin
  ChangeTextValue(AText);

  // Notify
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure FXCustomEdit.ChangeTextValue(AText: string);
begin
  // Undo
  if FCanUndo then
    FHistory.Add(FText);

  // Text
  FText := AText;

  // Char
  ApplyCharCase;

  (* THE TEXT HAS CHANGED *)
  ValidatePositions;

  // Scroll
  ScrollForCursor;

  // Update
  StandardUpdateLayout;

  // Notify
  if Assigned(OnChangeValue) then
    OnChangeValue(Self);
end;

procedure FXCustomEdit.Clear;
begin
  Text := '';
end;

procedure FXCustomEdit.ClearSelection;
begin
  SelectionLength := 0;
end;

procedure FXCustomEdit.PaintBuffer;
var
  ARect: TRect;
  Indic: TBitMap;
  TxtHeight: integer;
  ARound: integer;
  Temp: real;
  ASelect: string;
  FillColor: TColor;
  AText: string;
  FPen: TGDIPen;

  X, Y: integer;
begin
  // Safety
  ValidatePositions;

  // Background
  Color := FDrawColors.BackGround;
  PaintBackground;

  // Draw
  with Buffer do
    begin
      // Fill
      if Focused then
        FillColor := FEditColors.Press
      else
        FillColor := FEditColors.GetColor(InteractionState);

      // Fill
      GDIRoundRect(MakeRoundRect(DrawRect, Roundness),
        TAlphaColor.Create(FillColor).MakeGDIBrush, nil);

      // Outline
      FPen := nil;
      case Detail of
        FXDetailType.None, FXDetailType.Underline: FPen := TAlphaColor.Create(ChangeColorLight(FDrawColors.BackGround, EDIT_BORDER_FADE) ).MakeGDIPen(1);
        FXDetailType.Outline: FPen := TAlphaColor.Create(ChangeColorLight(FLineColor, EDIT_BORDER_FADE) ).MakeGDIPen(LineSize);
      end;

      ARect := DrawRect;
      ARect.Inflate(-trunc(FPen.GetWidth), -trunc(FPen.GetWidth));
      ARound := Roundness-trunc(FPen.GetWidth);
      if ARound < 1 then
        ARound := 1;
      GDIRoundRect(MakeRoundRect(ARect, ARound),
        nil, FPen);

      // Line
      if Detail = FXDetailType.Underline then
        GDIRoundRect(MakeRoundRect(LineRect, LineSize), TAlphaColor.Create(FLineColor).MakeGDIBrush, nil);

      // Text
      AText := DrawText;
      Font.Assign(Self.Font);

      // Color
      Font.Color := Self.Font.Color;

      // Text Hint
      if (AText = '') and (TextHint <> '') then
        begin
          Font.Color := ColorBlend(Font.Color, FillColor, EDIT_TEXT_HINT_FADE);
          AText := TextHint;
        end;

      // Data
      TxtHeight := TextHeight(AText);

      Y := 0;
      case Layout of
        TLayout.Beginning: Y := 0;
        TLayout.Center: Y := (TxtRect.Height - TxtHeight) div 2;
        TLayout.Ending: Y := TxtRect.Height-TxtHeight;
      end;
      X := -FCutPosition + TxtRect.Left;
      Y := Y + TxtRect.Top;

      // Selection
      ASelect := Selection;

      if ASelect <> '' then
        begin
          ARect.Top := TxtRect.Top + (TxtRect.Height - TxtHeight) div 2;
          ARect.Height := TxtHeight;
          ARect.Left := TxtRect.Left + FDrawPosition;
          ARect.Right := ARect.Left + TextWidth(ASelect);

          GDIRectangle(ARect, TAlphaColor.Create(FDrawColors.Accent).MakeGDIBrush, nil);
        end;

      // Text
      Brush.Style := bsClear;
      TextRect(TxtRect, X, Y, AText);

      // Indicator
      if Focused then
        begin
          Temp := FIndicatorWidth / 2;
          ARect := Rect(FDrawPosition - trunc(Temp), 0, FDrawPosition + ceil(Temp), TxtHeight);
          ARect.Height := TxtHeight;
          ARect.Offset(TxtRect.Left, (Height - TxtHeight) div 2);

          Indic := TBitMap.Create;
          try
            Indic.SetSize(Max(ARect.Width, 1), Max(TxtHeight, 1));
            Indic.Canvas.Brush.Color := clWhite;
            Indic.Canvas.FillRect(Rect(0, 0, Indic.Width, Indic.Height));

            StretchInvertedMask(Indic, Buffer, ARect);
          finally
            Indic.Free;
          end;
        end;
    end;

  inherited;
end;

procedure FXCustomEdit.PasteFromClipBoard;
var
  ClipText, NewText: string;
begin
  if not CanEdit then
    Exit;

  ClipText := Clipboard.AsText;

  // Single line only
  ClipText := StringReplace(ClipText, #13#10, ' ', [rfReplaceAll]);
  ClipText := StringReplace(ClipText, #13, ' ', [rfReplaceAll]);
  ClipText := StringReplace(ClipText, #10, ' ', [rfReplaceAll]);

  // Numbers Only
  if NumbersOnly then
    try
      ClipText.ToInt64;
    except
      Exit;
    end;

  // Safety
  ValidatePositions;

  // Selection
  if HasSelection then
    NewText := Copy(FText, 1, FPosition) + ClipText + Copy(FText, 1+FPosition+FSelLength, Length(FText))
  else
    NewText := Copy(FText, 1, FPosition) + ClipText + Copy(FText, FPosition+1, Length(FText));

  FSelLength := 0;

  // Set
  FPosition := FPosition + ClipText.Length;
  ChangeText( NewText );

  // Safety (ChangeText validates, this is only for clarity)
  ValidatePositions;
end;

procedure FXCustomEdit.PopupBeforePopup(Sender: TObject; var CanPopup: boolean;
  Point: TPoint);
var
  HasSelection: boolean;
  IsPassword: boolean;
begin
  ThemeManager.ProcessSystemMenu(FDefaultMenu);

  HasSelection := SelectionLength <> 0;
  IsPassword := (PasswordChar <> #0);

  FDefaultMenu.Items[0].Visible := HasSelection and not IsPassword;
  FDefaultMenu.Items[1].Visible := HasSelection and not IsPassword;
  FDefaultMenu.Items[2].Visible := Clipboard.AsText <> '';
  FDefaultMenu.Items[3].Visible := (FHistory.Count > 0) and FCanUndo;

  FDefaultMenu.Items[0].Enabled := CanEdit;
  FDefaultMenu.Items[2].Enabled := CanEdit;
  FDefaultMenu.Items[3].Enabled := CanEdit;
end;

procedure FXCustomEdit.PopupItemClick(Sender: TObject; Item: FXPopupComponent;
  Index: integer);
begin
  case Index of
    0:
    try
      CutToClipboard;
    except
    end;
    1:
    try
      CopyToClipboard;
    except
    end;
    2:
    try
      PasteFromClipboard;
    except
    end;
    3: Undo;
    4: SelectAll;
  end;
end;

procedure FXCustomEdit.PrepDefaultMenu;
var
  M: FXPopupItem;
begin
  with FDefaultMenu.Items do
    begin
      M := FXPopupItem.Create(FDefaultMenu);
      with M do
        begin
          Image.Enabled := true;
          Image.IconType := FXIconType.SegoeIcon;
          Image.SelectSegoe := #$E8C6;
          Text := CFX_String_Edit_Menu_Cut;
          Shortcut := 'Ctrl+X';
        end;
      Add(M);

      M := FXPopupItem.Create(FDefaultMenu);
      with M do
        begin
          Image.Enabled := true;
          Image.IconType := FXIconType.SegoeIcon;
          Image.SelectSegoe := #$E8C8;
          Text := CFX_String_Edit_Menu_Copy;
          Shortcut := 'Ctrl+C';
        end;
      Add(M);

      M := FXPopupItem.Create(FDefaultMenu);
      with M do
        begin
          Image.Enabled := true;
          Image.IconType := FXIconType.SegoeIcon;
          Image.SelectSegoe := #$E77F;
          Text := CFX_String_Edit_Menu_Paste;
          Shortcut := 'Ctrl+V';
        end;
      Add(M);

      M := FXPopupItem.Create(FDefaultMenu);
      with M do
        begin
          Image.Enabled := true;
          Image.IconType := FXIconType.SegoeIcon;
          Image.SelectSegoe := #$E7A7;
          Text := CFX_String_Edit_Menu_Undo;
          Shortcut := 'Ctrl+Z';
        end;
      Add(M);

      M := FXPopupItem.Create(FDefaultMenu);
      with M do
        begin
          Image.Enabled := true;
          Image.IconType := FXIconType.SegoeIcon;
          Image.SelectSegoe := #$E8B3;
          Text := CFX_String_Edit_Menu_SelectAll;
          Shortcut := 'Ctrl+A';
        end;
      Add(M);
    end;
end;

procedure FXCustomEdit.ScrollForCursor;
begin
  ScrollForCursor(FDrawPosition);
end;

procedure FXCustomEdit.ScrollForCursor(ADrawPosition: integer);
var
  ATextWidth: integer;
begin
  if ADrawPosition < 0 then
    begin
      FCutPosition := FCutPosition + ADrawPosition(*negative*) - LineSize * 2;
    end;

  if ADrawPosition > TxtRect.Width then
    begin
      FCutPosition := FCutPosition + ADrawPosition-TxtRect.Width + LineSize * 2;
    end;

  const ADrawText = DrawText;
  if ADrawText = '' then
    ATextWidth := TextW(FTextHint)
  else
    ATextWidth := TextW(ADrawText);
  if ATextWidth <= TxtRect.Width  then
    case LayoutHorizontal of
      TLayout.Center: FCutPosition := -(TxtRect.Width - ATextWidth) div 2;
      TLayout.Ending: FCutPosition := -(TxtRect.Width - ATextWidth);
    end;

  // Update
  UpdateDrawPosition;
end;

function FXCustomEdit.SearchPosition(AX: integer): integer;
var
  I, X: integer;
begin
  Result := 0;

  if AX >= TextW(Text) then
    Exit(TextLength);

  for I := 1 to TextLength do
    begin
      X := TextW(Copy(Text, 1, I)) - TextW(Text[I]) div 2;

      if AX <= X then
        begin
          Result := I-1;

          Break;
        end;
    end;
end;

procedure FXCustomEdit.SelectAll;
begin
  FPosition := 0;
  SelectionLength := TextLength;
end;

function FXCustomEdit.Selection: string;
begin
  Result := Copy(DrawText, FPosition+1, FSelLength);
end;

function FXCustomEdit.SelectionEnd: integer;
begin
  if FSelGoesLeft then
    Result := Position
  else
    Result := Position + SelectionLength;
end;

function FXCustomEdit.SelectionStart: integer;
begin
  if FSelGoesLeft then
    Result := Position+SelectionLength
  else
    Result := Position;
end;

procedure FXCustomEdit.SelectPoints(P1, P2: integer);
var
  ATotal: integer;
begin
  if not EnableSelection then
    Exit;

  // Clamp
  ATotal := TextLength;
  P1 := EnsureRange(P1, 0, ATotal);
  P2 := EnsureRange(P2, 0, ATotal);

  // Type
  FSelGoesLeft := P1 < P2;

  if FSelGoesLeft then
    begin
      FPosition := P1;
      FSelLength := P2-P1;
    end
  else
    begin
      FPosition := P2;
      FSelLength := P1-P2;
    end;

  // Safety
  ValidatePositions;

  // Update
  StandardUpdateLayout;
end;

procedure FXCustomEdit.SetAutoSizing(const Value: boolean);
begin
  if FAutoSize = Value then
    Exit;

  FAutoSize := Value;

  // Auto Size
  UpdateAutoSize;

  StandardUpdateLayout;
end;

procedure FXCustomEdit.SetCanUndo(const Value: boolean);
begin
  if FCanUndo = Value then
    Exit;

  if not Value then
    FHistory.Clear;

  FCanUndo := Value;
  StandardUpdateLayout;
end;

procedure FXCustomEdit.SetCharCase(const Value: FXCharCase);
begin
  if FCharCase = Value then
    Exit;

  FCharCase := Value;

  ApplyCharCase;
  StandardUpdateLayout;
end;

procedure FXCustomEdit.SetDetail(const Value: FXDetailType);
begin
  if FDetail = Value then
    Exit;

  FDetail := Value;
  StandardUpdateLayout;
end;

procedure FXCustomEdit.SetEnableSelection(const Value: boolean);
begin
  if FEnableSelection = Value then
    Exit;

  FEnableSelection := Value;

  if SelectionLength <> 0 then
    SelectionLength := 0 // this will update the UI instead
  else
    StandardUpdateLayout;
end;

procedure FXCustomEdit.SetLayout(const Value: TLayout);
begin
  if FLayout = Value then
    Exit;

  FLayout := Value;
  StandardUpdateLayout;
end;

procedure FXCustomEdit.SetLayoutHoriz(const Value: TLayout);
begin
  if FLayoutHoriz = Value then
    Exit;

  FLayoutHoriz := Value;
  StandardUpdateLayout;
end;

procedure FXCustomEdit.SetLineSize(const Value: integer);
begin
  if FLineSize = Value then
    Exit;

  FLineSize := Value;
  StandardUpdateLayout;
end;

procedure FXCustomEdit.SetNumbersOnly(const Value: boolean);
begin
  if FNumbersOnly = Value then
    Exit;

  FNumbersOnly := Value;
  if FNumbersOnly then
    try
      FText.ToInt64;
    except
      FText := '0';
    end;

  // The text may have changed
  ValidatePositions;

  StandardUpdateLayout;
end;

procedure FXCustomEdit.SetPasswordChar(const Value: char);
begin
  if FPassChar = Value then
    Exit;

  FPassChar := Value;
  StandardUpdateLayout;
end;

procedure FXCustomEdit.SetPosition(const Value: integer);
begin
  if (FPosition = Value) or (Value < 0) or (Value > Length(FText)) then
    Exit;

  FPosition := Value;

  // Sel
  ClearSelection;

  // Safety
  ValidatePositions;

  // Draw
  StandardUpdateLayout;
end;

procedure FXCustomEdit.SetRoundness(const Value: integer);
begin
  if FRoundness = Value then
    Exit;

  FRoundness := Value;
  StandardUpdateLayout;
end;

procedure FXCustomEdit.SetSelLength(const Value: integer);
var
  ATotal: integer;
begin
  // Set Selection
  if FSelLength = Value then
    Exit;

  FSelLength := Value;

  ATotal := TextLength;

  if FPosition + FSelLength > ATotal then
    FSelLength := ATotal - FPosition;

  if FSelLength < 0 then
    FSelLength := 0;

  // Draw
  StandardUpdateLayout;
end;

procedure FXCustomEdit.SetText(const Value: string);
begin
  FText := Value;
  FHistory.Clear;

  ApplyCharCase;

  (* THE TEXT HAS CHANGED *)
  ValidatePositions;

  // Notify
  if Assigned(OnChangeValue) and not (csReading in ComponentState) then
    OnChangeValue(Self);

  // Update
  StandardUpdateLayout;
end;

procedure FXCustomEdit.SetTextHint(const Value: string);
begin
  if FTextHint = Value then
    Exit;

  FTextHint := Value;
  StandardUpdateDraw;
end;

procedure FXCustomEdit.SetTextMarginX(const Value: integer);
begin
  if FTextMarginX = Value then
    Exit;

  FTextMarginX := Value;
  StandardUpdateLayout;
end;

procedure FXCustomEdit.SetTextMarginY(const Value: integer);
begin
  if FTextMarginY = Value then
    Exit;

  FTextMarginY := Value;
  StandardUpdateLayout;
end;

procedure FXCustomEdit.SetValue(const Value: int64);
begin
  if not IsReading then
    Text := Value.ToString;
end;

procedure FXCustomEdit.Sized;
begin
  inherited;
  UpdateAutoSize;
end;

function FXCustomEdit.TextH(AText: string): integer;
begin
  Result := 0;
  if Parent <> nil then
    begin
      Buffer.Font.Assign( Self.Font );
      Result := Buffer.TextHeight(AText);
    end;
end;

function FXCustomEdit.TextW(AText: string): integer;
begin
  Result := 0;
  if Parent <> nil then
    begin
      Buffer.Font.Assign( Self.Font );
      Result := Buffer.TextWidth(AText);
    end;
end;

procedure FXCustomEdit.WM_LButtonDown(var Msg: TWMMouse);
var
  PosX, Value: integer;
begin
  FSelLength := 0;

  PosX := Msg.XPos - TxtRect.Left;

  FDownStart := -1;
  if (PosX >= 0) then
    begin
      PosX := PosX + FCutPosition;

      Value := SearchPosition(PosX);

      FDownStart := Value;
      Position := Value;
    end;

  inherited;
end;

{ FXEdit }

constructor FXEdit.Create(aOwner: TComponent);
begin
  inherited;
  // inherit
end;

destructor FXEdit.Destroy;
begin
  // inherit
  inherited;
end;

{ FXNumberEdit }

procedure FXNumberEdit.ChangeText(AText: string);
begin
  //inherited;
  // we do not call inheirted and we just do what it would normally do here, because OnChange must be called AFTER TextUpdated;
  ChangeTextValue(AText);

  // Test if valid
  TextUpdateTrySetValue;

  // Notify
  if Assigned(OnChange) then
    OnChange(Self);
end;

constructor FXNumberEdit.Create(aOwner: TComponent);
begin
  inherited;
  FNumberType := FXNumberType.Integer;
  FDecimals := 2;
  FText := '0';
  ValidatePositions;

  FRange := FNumberRange.Create(Self);
  FRange.OnChange := RangeUpdated;
end;

destructor FXNumberEdit.Destroy;
begin
  FreeAndNil( FRange );
  inherited;
end;

procedure FXNumberEdit.DoExit;
begin
  inherited;
  UpdateTextValue;
end;

procedure FXNumberEdit.EnterPressed;
begin
  UpdateTextValue;

  inherited;
end;

function FXNumberEdit.GetValueCurrency: currency;
begin
  Result := FValue;
end;

function FXNumberEdit.GetValueExtended: extended;
begin
  Result := FValue;
end;

function FXNumberEdit.GetValueInt: int64;
begin
  Result := Trunc(FValue);
end;

procedure FXNumberEdit.RangeUpdated(Sender: TObject);
begin
  if FRange.Enabled then
    Value := FRange.EnsureRange(FValue);
end;

procedure FXNumberEdit.SetDecimals(const Value: integer);
begin
  if FDecimals = Value then
    Exit;

  FDecimals := Value;
  if not IsReading then
    UpdateTextValue;
end;

procedure FXNumberEdit.SetNumberType(const Value: FXNumberType);
begin
  if FNumberType = Value then
    Exit;

  FNumberType := Value;
  if not IsReading then
    UpdateTextValue;
end;

procedure FXNumberEdit.SetText(const Value: string);
var
  ShouldUpdate: boolean;
begin
  // Inherit
  inherited;

  // Test if valid
  ShouldUpdate := not TextUpdateTrySetValue;
  if AllowPartialTyping then begin
    // Failed but has potential
    if ShouldUpdate and Text.EndsWith(FormatSettings.DecimalSeparator) and not Text.StartsWith(FormatSettings.DecimalSeparator) then begin
      ShouldUpdate := not TextUpdateTrySetValue(Text+'0'); // attempt to validate "1." as "1.0"
    end;

    // Too large a number of decimals
    if FNumberType <> FXNumberType.Integer then
      if Text.Contains(FormatSettings.DecimalSeparator) and
        (Text.Substring(Text.IndexOf(FormatSettings.DecimalSeparator)+1).Length > FDecimals) then
        ShouldUpdate := true;
  end;

  // Update display
  if not IsReading then
    if ShouldUpdate or not AllowPartialTyping then
      UpdateTextValue;
end;

procedure FXNumberEdit.SetValueInt(Value: int64);
begin
  // Range
  if FRange.Enabled then
    Value := trunc(FRange.EnsureRange(Value));

  // Set
  Self.Value := Value;
end;

function FXNumberEdit.TextUpdateTrySetValue(AText: string): boolean;
var
  I: Int64;
  New: Extended;
begin
  Result := false;
  try
    if AText = '' then
      // 0
      New := 0
    else
      // autp
      case FNumberType of
        FXNumberType.Integer: begin
          if not TryStrToInt64(AText, I) then
            Exit;
          New := I;
        end;
        FXNumberType.Extended: begin
          if not TryStrToFloat(AText, New) then
            Exit
        end;
        FXNumberType.Currency: begin
          if not TryStrToFloat(AText, New) then
            Exit;
        end;
        else
          Exit;
      end;

    //
    SetValueEx(New, false);

    //
    Result := New = Value;
  except
  end;
end;

function FXNumberEdit.TextUpdateTrySetValue: boolean;
begin
  Result := TextUpdateTrySetValue(Text);
end;

procedure FXNumberEdit.UpdateTextValue;
var
  Number: string;
begin
  // Set
  Number := '';
  case FNumberType of
    FXNumberType.Integer: Number := ValueInt.ToString;
    FXNumberType.Extended,
    FXNumberType.Currency:
      if FDecimals > 0 then
        Number := Format('%.' + FDecimals.ToString + 'f', [Value])
      else
        Number := ValueInt.ToString;
  end;

  // Update
  if Number <> Text then
    inherited ChangeTextValue(Number);
end;

procedure FXNumberEdit.SetValue(Value: extended);
begin
  SetValueEx(Value, true);
end;

procedure FXNumberEdit.SetValueCurrency(Value: currency);
begin
  // Range
  if FRange.Enabled then
    Value := FRange.EnsureRange(Value);

  Self.Value := Value;
end;

procedure FXNumberEdit.SetValueEx(Value: Extended; Update: boolean);
begin
  // Range
  if FRange.Enabled then
    Value := FRange.EnsureRange(Value);

  // Same value?
  if (FValue = Value) and not IsReading then
    Exit;

  // Set
  FValue := Value;

  // Update text
  if Update then
    UpdateTextValue;

  // Set
  if not IsReading and Assigned(OnNumberChanged) then
    OnNumberChanged(Self);
end;

procedure FXNumberEdit.SetValueExtended(Value: extended);
begin
  // Range
  if FRange.Enabled then
    Value := FRange.EnsureRange(Value);

  // Set
  Self.Value := Value;
end;

{ FXCustomTextArea }

constructor FXCustomTextArea.Create(aOwner: TComponent);
begin
  inherited;
  ParentColor := false;
  TabStop := true;
  AutoFocusLine := false;
  BufferedComponent := true;
  Transparent := false;

  Cursor := crIBeam;

  // Catch the navigation keys, they are used internally
  FocusFlags := [FXFocusFlag.CatchLeft, FXFocusFlag.CatchUp,
    FXFocusFlag.CatchRight, FXFocusFlag.CatchDown];

  // Defaults
  FWordWrap := true;
  FEnableSelection := true;
  FCanUndo := true;
  FWantReturns := true;
  FWantTabs := false;
  FShowScrollbar := true;
  FClearSelOnExit := false;
  FReadOnly := false;
  FCharCase := FXCharCase.Both;
  FMaxLength := 0;
  FLineSpacing := 2;
  FRoundness := EDIT_BORDER_ROUND;
  FLineSize := EDIT_LINE_SIZE;
  FTextMarginX := EDIT_EXTRA_SPACE;
  FTextMarginY := EDIT_EXTRA_SPACE;
  FDetail := FXDetailType.Outline;

  FCaret := 0;
  FAnchor := 0;
  FDesiredX := -1;
  FDownStart := -1;
  FScrollX := 0;
  FScrollY := 0;

  FText := '';

  // Strings
  FStrings := TStringList.Create;
  FStrings.OnChange := StringsChanged;

  // History
  FHistory := TStringList.Create;

  // Menu
  FDefaultMenu := FXPopupMenu.Create(Self);
  FDefaultMenu.OnItemClick := PopupItemClick;
  FDefaultMenu.OnBeforePopup := PopupBeforePopup;
  PrepDefaultMenu;

  // Scrollbars (real controls, same pattern as FXScrollLayout)
  FVertScroll := FXScrollbar.Create(Self);
  with FVertScroll do begin
    Parent := Self;
    Orientation := FXOrientation.Vertical;
    TabStop := false;
    Visible := false;
    Width := DEFAULT_SCROLLBAR_SIZE;
    Max := 0;
    OnChangeValue := VScrollChange;
  end;

  FHorzScroll := FXScrollbar.Create(Self);
  with FHorzScroll do begin
    Parent := Self;
    Orientation := FXOrientation.Horizontal;
    TabStop := false;
    Visible := false;
    Height := DEFAULT_SCROLLBAR_SIZE;
    Max := 0;
    OnChangeValue := HScrollChange;
  end;

  // Custom Color
  FCustomColors := FXCompleteColorSets.Create(Self);
  FCustomEditColors := FXSingleColorStateSets.Create(Self);

  FDrawColors := FXCompleteColorSet.Create;
  FEditColors := FXSingleColorStateSet.Create;

  // Font
  Font.Height := ThemeManager.FormFontHeight;

  // Sizing
  Width := TEXTAREA_DEFAULT_WIDTH;
  Height := TEXTAREA_DEFAULT_HEIGHT;
end;

destructor FXCustomTextArea.Destroy;
begin
  if FStrings <> nil then
    FStrings.OnChange := nil;

  FreeAndNil( FStrings );
  FreeAndNil( FHistory );

  FreeAndNil( FVertScroll );
  FreeAndNil( FHorzScroll );

  FreeAndNil( FCustomColors );
  FreeAndNil( FCustomEditColors );
  FreeAndNil( FDrawColors );
  FreeAndNil( FEditColors );

  SetLength(FLines, 0);
  inherited;
end;

// Canvas -

function FXCustomTextArea.TextW(const AText: string): integer;
begin
  Result := 0;
  if (Parent <> nil) and (AText <> '') then
    begin
      Buffer.Font.Assign( Self.Font );
      Result := Buffer.TextWidth(AText);
    end;
end;

function FXCustomTextArea.TextH(const AText: string): integer;
begin
  Result := 0;
  if Parent <> nil then
    begin
      Buffer.Font.Assign( Self.Font );
      Result := Buffer.TextHeight(AText);
    end;
end;

// Messages --

procedure FXCustomTextArea.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;

  Message.Result := Message.Result or DLGC_WANTARROWS or DLGC_WANTCHARS;

  if FWantReturns then
    Message.Result := Message.Result or DLGC_WANTALLKEYS;
  if FWantTabs then
    Message.Result := Message.Result or DLGC_WANTTAB;
end;

// Lines

procedure FXCustomTextArea.AddLine(const AText: string; AStart, ALen, ABreak: integer);
var
  Index, AWidth: integer;
begin
  Index := Length(FLines);
  SetLength(FLines, Index+1);

  FLines[Index].Text := AText;
  FLines[Index].StartPos := AStart;
  FLines[Index].Len := ALen;
  FLines[Index].BreakLen := ABreak;

  // Content width
  AWidth := TextW(AText);
  if AWidth > FContentWidth then
    FContentWidth := AWidth;
end;

procedure FXCustomTextArea.AddParagraph(const APara: string; AStart, ABreak: integer);
var
  MaxW, I, LineBegin, LastSpace, BreakAt, ALength: integer;
  Chunk: string;
begin
  ALength := Length(APara);
  MaxW := TxtRect.Width;

  // No wrapping needed
  if (not FWordWrap) or (MaxW <= 0) or (ALength = 0) or (TextW(APara) <= MaxW) then
    begin
      AddLine(APara, AStart, ALength, ABreak);
      Exit;
    end;

  LineBegin := 1;
  LastSpace := 0;
  I := 1;
  while I <= ALength do
    begin
      if APara[I] = ' ' then
        LastSpace := I;

      Chunk := Copy(APara, LineBegin, I-LineBegin+1);

      if (I > LineBegin) and (TextW(Chunk) > MaxW) then
        begin
          if LastSpace >= LineBegin then
            BreakAt := LastSpace
          else
            BreakAt := I-1;

          // Safety, always advance
          if BreakAt < LineBegin then
            BreakAt := LineBegin;

          AddLine(Copy(APara, LineBegin, BreakAt-LineBegin+1),
            AStart + LineBegin-1, BreakAt-LineBegin+1, 0);

          LineBegin := BreakAt+1;
          LastSpace := 0;
          I := LineBegin;

          Continue;
        end;

      Inc(I);
    end;

  // Remainder
  AddLine(Copy(APara, LineBegin, ALength-LineBegin+1),
    AStart + LineBegin-1, ALength-LineBegin+1, ABreak);
end;

procedure FXCustomTextArea.RebuildLines;
var
  I, LineStart, BreakLen, Total: integer;
begin
  SetLength(FLines, 0);
  FContentWidth := 0;

  Total := Length(FText);

  I := 1;
  LineStart := 1;
  while true do
    begin
      if I > Total then
        begin
          AddParagraph(Copy(FText, LineStart, I-LineStart), LineStart-1, 0);
          Break;
        end;

      if CharInSet(FText[I], [#13, #10]) then
        begin
          BreakLen := 1;
          if (FText[I] = #13) and (I < Total) and (FText[I+1] = #10) then
            BreakLen := 2;

          AddParagraph(Copy(FText, LineStart, I-LineStart), LineStart-1, BreakLen);

          Inc(I, BreakLen);
          LineStart := I;
        end
      else
        Inc(I);
    end;
end;

// Menu-

procedure FXCustomTextArea.PrepDefaultMenu;
var
  M: FXPopupItem;
begin
  with FDefaultMenu.Items do
    begin
      M := FXPopupItem.Create(FDefaultMenu);
      with M do
        begin
          Image.Enabled := true;
          Image.IconType := FXIconType.SegoeIcon;
          Image.SelectSegoe := #$E8C6;
          Text := CFX_String_Edit_Menu_Cut;
          Shortcut := 'Ctrl+X';
        end;
      Add(M);

      M := FXPopupItem.Create(FDefaultMenu);
      with M do
        begin
          Image.Enabled := true;
          Image.IconType := FXIconType.SegoeIcon;
          Image.SelectSegoe := #$E8C8;
          Text := CFX_String_Edit_Menu_Copy;
          Shortcut := 'Ctrl+C';
        end;
      Add(M);

      M := FXPopupItem.Create(FDefaultMenu);
      with M do
        begin
          Image.Enabled := true;
          Image.IconType := FXIconType.SegoeIcon;
          Image.SelectSegoe := #$E77F;
          Text := CFX_String_Edit_Menu_Paste;
          Shortcut := 'Ctrl+V';
        end;
      Add(M);

      M := FXPopupItem.Create(FDefaultMenu);
      with M do
        begin
          Image.Enabled := true;
          Image.IconType := FXIconType.SegoeIcon;
          Image.SelectSegoe := #$E7A7;
          Text := CFX_String_Edit_Menu_Undo;
          Shortcut := 'Ctrl+Z';
        end;
      Add(M);

      M := FXPopupItem.Create(FDefaultMenu);
      with M do
        begin
          Image.Enabled := true;
          Image.IconType := FXIconType.SegoeIcon;
          Image.SelectSegoe := #$E8B3;
          Text := CFX_String_Edit_Menu_SelectAll;
          Shortcut := 'Ctrl+A';
        end;
      Add(M);
    end;
end;

procedure FXCustomTextArea.PopupBeforePopup(Sender: TObject;
  var CanPopup: boolean; Point: TPoint);
var
  ASelection: boolean;
begin
  ThemeManager.ProcessSystemMenu(FDefaultMenu);

  ASelection := HasSelection;

  FDefaultMenu.Items[0].Visible := ASelection;
  FDefaultMenu.Items[1].Visible := ASelection;
  FDefaultMenu.Items[2].Visible := Clipboard.AsText <> '';
  FDefaultMenu.Items[3].Visible := (FHistory.Count > 0) and FCanUndo;

  FDefaultMenu.Items[0].Enabled := not FReadOnly;
  FDefaultMenu.Items[2].Enabled := not FReadOnly;
  FDefaultMenu.Items[3].Enabled := not FReadOnly;
end;

procedure FXCustomTextArea.PopupItemClick(Sender: TObject;
  Item: FXPopupComponent; Index: integer);
begin
  case Index of
    0:
    try
      CutToClipBoard;
    except
    end;
    1:
    try
      CopyToClipBoard;
    except
    end;
    2:
    try
      PasteFromClipBoard;
    except
    end;
    3: Undo;
    4: SelectAll;
  end;
end;

procedure FXCustomTextArea.OpenPopupMenu(X, Y: integer);
begin
  if not Assigned(PopupMenu) then
    FDefaultMenu.PopupAtPoint(ClientToScreen(Point(X, Y)))
  else
    inherited;
end;

// Strings

function FXCustomTextArea.GetLines: TStrings;
begin
  // Sync the string list with the internal text
  FUpdatingStrings := true;
  try
    FStrings.Text := FText;
  finally
    FUpdatingStrings := false;
  end;

  Result := FStrings;
end;

procedure FXCustomTextArea.SetLines(const Value: TStrings);
begin
  FUpdatingStrings := true;
  try
    FStrings.Assign(Value);
  finally
    FUpdatingStrings := false;
  end;

  SetText( RemoveTrailingBreak(FStrings.Text) );
end;

procedure FXCustomTextArea.StringsChanged(Sender: TObject);
begin
  if FUpdatingStrings then
    Exit;

  SetText( RemoveTrailingBreak(FStrings.Text) );
end;

// Text-

function FXCustomTextArea.GetText: string;
begin
  Result := FText;
end;

procedure FXCustomTextArea.SetTextEx(const Value: string);
begin
  SetText(Value);
end;

procedure FXCustomTextArea.SetText(const Value: string);
begin
  FText := NormaliseLineBreaks(Value);
  FHistory.Clear;

  ApplyCharCase;

  (* THE TEXT HAS CHANGED *)
  ValidatePositions;

  // Update
  UpdateAll;

  // Notify
  if not IsReading then
    DoChangeValue;
end;

procedure FXCustomTextArea.ApplyCharCase;
begin
  case FCharCase of
    FXCharCase.Uppercase: FText := UpperCase(FText);
    FXCharCase.Lowercase: FText := LowerCase(FText);
  end;

  ValidatePositions;
end;

procedure FXCustomTextArea.ValidatePositions;
var
  ATotal: integer;
begin
  ATotal := Length(FText);

  FCaret := EnsureRange(FCaret, 0, ATotal);
  FAnchor := EnsureRange(FAnchor, 0, ATotal);

  // Never allow the caret to sit in the middle of a #13#10 sequence
  if (FCaret >= 1) and (FCaret < ATotal) then
    if (FText[FCaret] = #13) and (FText[FCaret+1] = #10) then
      Inc(FCaret);

  if (FAnchor >= 1) and (FAnchor < ATotal) then
    if (FText[FAnchor] = #13) and (FText[FAnchor+1] = #10) then
      Inc(FAnchor);

  if FDownStart > ATotal then
    FDownStart := ATotal;

  if not FEnableSelection then
    FAnchor := FCaret;
end;

procedure FXCustomTextArea.ApplyTextChange(const ANewText: string; ACaret: integer);
begin
  if FReadOnly then
    Exit;

  // Undo
  PushUndo;

  FText := NormaliseLineBreaks(ANewText);
  FCaret := ACaret;
  FAnchor := ACaret;

  ApplyCharCase;

  (* THE TEXT HAS CHANGED *)
  ValidatePositions;

  FDesiredX := -1;

  // Update
  UpdateAll;

  // Notify
  DoChangeValue;
  DoChange;
end;

procedure FXCustomTextArea.InsertText(const AText: string);
var
  S: string;
  NewText: string;
  ACaret: integer;
  AStart, AEnd: integer;
begin
  if FReadOnly then
    Exit;

  S := NormaliseLineBreaks(AText);

  ValidatePositions;

  AStart := GetSelStart;
  AEnd := AStart + GetSelLength;

  NewText := Copy(FText, 1, AStart) + S + Copy(FText, AEnd+1, Length(FText));

  // Max length
  if (FMaxLength > 0) and (Length(NewText) > FMaxLength) then
    begin
      NewText := Copy(NewText, 1, FMaxLength);
      if AStart > Length(NewText) then
        AStart := Length(NewText);
    end;

  ACaret := AStart + Length(S);
  if ACaret > Length(NewText) then
    ACaret := Length(NewText);

  ApplyTextChange(NewText, ACaret);
end;

// Navigation

function FXCustomTextArea.StepLeft(APos: integer): integer;
begin
  Result := APos;
  if Result <= 0 then
    Exit(0);

  Dec(Result);

  // Skip the #13#10 pair
  if (Result >= 1) and (Result+1 <= Length(FText)) then
    if (FText[Result+1] = #10) and (FText[Result] = #13) then
      Dec(Result);
end;

function FXCustomTextArea.StepRight(APos: integer): integer;
var
  ATotal: integer;
begin
  ATotal := Length(FText);

  Result := APos;
  if Result >= ATotal then
    Exit(ATotal);

  Inc(Result);

  // Skip the #13#10 pair
  if (Result <= ATotal) and (APos+1 <= ATotal) then
    if (FText[APos+1] = #13) and (Result < ATotal) and (FText[Result+1] = #10) then
      Inc(Result);
end;

function FXCustomTextArea.IsWordChar(C: char): boolean;
begin
  Result := CharInSet(C, ['A'..'Z', 'a'..'z', '0'..'9', '_']);
end;

function FXCustomTextArea.WordLeft(APos: integer): integer;
begin
  Result := EnsureRange(APos, 0, Length(FText));

  // Skip the non word characters
  while (Result > 0) and not IsWordChar(FText[Result]) do
    Dec(Result);

  // Skip the word
  while (Result > 0) and IsWordChar(FText[Result]) do
    Dec(Result);
end;

function FXCustomTextArea.WordRight(APos: integer): integer;
var
  ATotal: integer;
begin
  ATotal := Length(FText);
  Result := EnsureRange(APos, 0, ATotal);

  // Skip the word
  while (Result < ATotal) and IsWordChar(FText[Result+1]) do
    Inc(Result);

  // Skip the non word characters
  while (Result < ATotal) and not IsWordChar(FText[Result+1]) do
    Inc(Result);
end;

// Geometry --

function FXCustomTextArea.LineHeight: integer;
begin
  Result := TextH('Ag');
  if Result <= 0 then
    Result := Abs(Font.Height);
  if Result <= 0 then
    Result := 16;

  Inc(Result, FLineSpacing);
end;

function FXCustomTextArea.ContentHeight: integer;
begin
  Result := Length(FLines) * LineHeight;
end;

function FXCustomTextArea.MaxScrollY: integer;
begin
  Result := Max(0, ContentHeight - TxtRect.Height);
end;

function FXCustomTextArea.MaxScrollX: integer;
begin
  if FWordWrap then
    Exit(0);

  Result := Max(0, FContentWidth - TxtRect.Width + TEXTAREA_NEWLINE_SELECT_WIDTH);
end;

function FXCustomTextArea.VisibleLines: integer;
begin
  Result := Max(1, TxtRect.Height div LineHeight);
end;

function FXCustomTextArea.LineFromPosition(APos: integer): integer;
var
  I: integer;
begin
  Result := 0;
  for I := 0 to High(FLines) do
    if APos >= FLines[I].StartPos then
      Result := I
    else
      Break;
end;

function FXCustomTextArea.PositionOfLineStart(ALine: integer): integer;
begin
  if Length(FLines) = 0 then
    Exit(0);

  ALine := EnsureRange(ALine, 0, High(FLines));
  Result := FLines[ALine].StartPos;
end;

function FXCustomTextArea.PositionOfLineEnd(ALine: integer): integer;
begin
  if Length(FLines) = 0 then
    Exit(0);

  ALine := EnsureRange(ALine, 0, High(FLines));
  Result := FLines[ALine].StartPos + FLines[ALine].Len;
end;

function FXCustomTextArea.XFromPosition(APos: integer): integer;
var
  L, Offset: integer;
begin
  Result := 0;
  if Length(FLines) = 0 then
    Exit;

  L := LineFromPosition(APos);
  Offset := EnsureRange(APos - FLines[L].StartPos, 0, FLines[L].Len);

  Result := TextW( Copy(FLines[L].Text, 1, Offset) );
end;

function FXCustomTextArea.PositionFromPoint(X, Y: integer): integer;
var
  L, I, LX, W, Idx: integer;
  S: string;
begin
  if Length(FLines) = 0 then
    Exit(0);

  L := (Y - TxtRect.Top + FScrollY) div LineHeight;
  L := EnsureRange(L, 0, High(FLines));

  S := FLines[L].Text;
  LX := X - TxtRect.Left + FScrollX;

  Idx := Length(S);
  if LX <= 0 then
    Idx := 0
  else
    for I := 1 to Length(S) do
      begin
        W := TextW(Copy(S, 1, I)) - TextW(S[I]) div 2;

        if LX <= W then
          begin
            Idx := I-1;
            Break;
          end;
      end;

  Result := FLines[L].StartPos + Idx;
end;

procedure FXCustomTextArea.EnsureCaretVisible;
var
  L, ATop, ABottom, AX: integer;
begin
  if Length(FLines) = 0 then
    begin
      FScrollX := 0;
      FScrollY := 0;
      Exit;
    end;

  L := LineFromPosition(FCaret);

  // Vertical
  ATop := L * LineHeight;
  ABottom := ATop + LineHeight;

  if ATop - FScrollY < 0 then
    FScrollY := ATop;
  if ABottom - FScrollY > TxtRect.Height then
    FScrollY := ABottom - TxtRect.Height;

  FScrollY := EnsureRange(FScrollY, 0, MaxScrollY);

  // Horizontal
  if FWordWrap then
    FScrollX := 0
  else
    begin
      AX := XFromPosition(FCaret);

      if AX - FScrollX < 0 then
        FScrollX := AX;
      if AX - FScrollX > TxtRect.Width - TEXTAREA_CARET_WIDTH then
        FScrollX := AX - TxtRect.Width + TEXTAREA_CARET_WIDTH;

      FScrollX := EnsureRange(FScrollX, 0, MaxScrollX);
    end;

  PushScrollToBars;
end;

procedure FXCustomTextArea.MoveCaret(ANewPos: integer; AExtend: boolean);
begin
  FCaret := EnsureRange(ANewPos, 0, Length(FText));

  if not AExtend or not FEnableSelection then
    FAnchor := FCaret;

  ValidatePositions;

  EnsureCaretVisible;
  StandardUpdateDraw;
end;

// Internal --

procedure FXCustomTextArea.UpdateLine;
begin
  if Focused then
    FLineColor := FDrawColors.Accent
  else
    FLineColor := FDrawColors.BackGroundInterior;

  StandardUpdateDraw;
end;

procedure FXCustomTextArea.UpdateColors;
begin
  // Access theme manager
  FDrawColors.Assign( ThemeManager.SystemColor );
  if FCustomColors.Enabled then
    FDrawColors.LoadFrom(FCustomColors, ThemeManager.DarkTheme)
  else begin
    if ThemeManager.DarkTheme then
      FDrawColors.BackGroundInterior := ChangeColorLight(ThemeManager.SystemColor.BackGroundInterior, EDIT_COLOR_CHANGE * 4)
    else
      FDrawColors.BackGroundInterior := ChangeColorLight(ThemeManager.SystemColor.BackGroundInterior, -EDIT_COLOR_CHANGE * 4);
  end;

  // Edit colors
  if FCustomEditColors.Enabled then
    FEditColors.LoadColors(FCustomEditColors, ThemeManager.DarkTheme)
  else
    begin
      FEditColors.None := ChangeColorLight(ThemeManager.SystemColor.BackGroundInterior, EDIT_COLOR_CHANGE);
      FEditColors.Hover := ChangeColorLight(FEditColors.None, EDIT_COLOR_CHANGE);
      FEditColors.Press := ThemeManager.SystemColor.BackGround;
    end;

  // Menu
  FDefaultMenu.UpdateTheme(false);

  // Update
  Font.Color := FDrawColors.ForeGround;
  UpdateLine;
end;

procedure FXCustomTextArea.UpdateRects;
var
  BaseTxt: TRect;
  VertVis, HorzVis: boolean;
  Pass: integer;
begin
  // Safety, the text may have been modified from the outside
  ValidatePositions;

  DrawRect := ClientRect;
  MainRect := ContentRect;

  // Underline
  LineRect := MainRect;
  LineRect.Top := MainRect.Bottom - FLineSize;

  // Text area before any scrollbar gutter is reserved
  BaseTxt := MainRect;
  BaseTxt.Inflate(-FTextMarginX, -FTextMarginY);

  case FDetail of
    FXDetailType.Underline: BaseTxt.Bottom := BaseTxt.Bottom - FLineSize;
    FXDetailType.Outline: BaseTxt.Inflate(-FLineSize, -FLineSize);
  end;

  (* The vertical bar's presence narrows the wrap width, which changes the
     line count / content height, which can change whether the vertical bar
     is needed - the same mutual dependency CalculateRange solves in
     CFX.Layouts. Converge it the same way: a couple of passes, then settle. *)
  VertVis := false;
  HorzVis := false;

  for Pass := 1 to 3 do
    begin
      TxtRect := BaseTxt;
      if VertVis then
        TxtRect.Right := TxtRect.Right - DEFAULT_SCROLLBAR_SIZE;
      if HorzVis then
        TxtRect.Bottom := TxtRect.Bottom - DEFAULT_SCROLLBAR_SIZE;

      if TxtRect.Right < TxtRect.Left then
        TxtRect.Right := TxtRect.Left;
      if TxtRect.Bottom < TxtRect.Top then
        TxtRect.Bottom := TxtRect.Top;

      // Rebuild the visual lines with the current width
      RebuildLines;

      // Vertical: whenever the content is taller than the box
      VertVis := FShowScrollbar and (TxtRect.Height > 0)
        and (ContentHeight > TxtRect.Height);

      // Horizontal: only without word wrap, and only when a line overflows
      HorzVis := FShowScrollbar and not FWordWrap and (TxtRect.Width > 0)
        and (FContentWidth > TxtRect.Width);
    end;

  // Scroll values, clamped to the now-final rect
  FScrollY := EnsureRange(FScrollY, 0, MaxScrollY);
  FScrollX := EnsureRange(FScrollX, 0, MaxScrollX);

  // Position, size and show the scrollbars
  // (values computed up front - "Max" inside a "with FVertScroll do" block
  // would resolve to the scrollbar's own Max property, not Math.Max)
  FVertScroll.Visible := VertVis;
  FHorzScroll.Visible := HorzVis;

  var ScrollbarOffsetAll := Self.LineSize;
  var ScrollbarOffsetBottom := Self.LineSize;
  case Detail of
    FXDetailType.None: begin
      ScrollbarOffsetAll := 0;
      ScrollbarOffsetBottom := 0;
    end;
    FXDetailType.Underline: ScrollbarOffsetAll := 0;
    FXDetailType.Outline: ScrollbarOffsetBottom := 0;
  end;


  FVertScroll.Width := DEFAULT_SCROLLBAR_SIZE;
  FVertScroll.Left := MainRect.Right - DEFAULT_SCROLLBAR_SIZE - ScrollbarOffsetAll;
  FVertScroll.Top := MainRect.Top + ScrollbarOffsetAll;
  if HorzVis then
    FVertScroll.Height := Math.Max(MainRect.Height - DEFAULT_SCROLLBAR_SIZE - 2 * ScrollbarOffsetAll, 0)
  else
    FVertScroll.Height := Math.Max(MainRect.Height - 2 * ScrollbarOffsetAll, 0);
  FVertScroll.Max := MaxScrollY;

  FHorzScroll.Height := DEFAULT_SCROLLBAR_SIZE;
  FHorzScroll.Top := MainRect.Bottom - DEFAULT_SCROLLBAR_SIZE - ScrollbarOffsetAll - ScrollbarOffsetBottom;
  FHorzScroll.Left := MainRect.Left + ScrollbarOffsetAll;
  if VertVis then
    FHorzScroll.Width := Math.Max(MainRect.Width - DEFAULT_SCROLLBAR_SIZE - 2 * ScrollbarOffsetAll, 0)
  else
    FHorzScroll.Width := Math.Max(MainRect.Width - 2 * ScrollbarOffsetAll, 0);
  FHorzScroll.Max := MaxScrollX;

  // Reflect the (possibly just-clamped) scroll position on the bars
  PushScrollToBars;
end;

procedure FXCustomTextArea.PushScrollToBars;
begin
  if FSyncingScrollbars then
    Exit;
  if (FVertScroll = nil) or (FHorzScroll = nil) then
    Exit;

  FSyncingScrollbars := true;
  try
    FVertScroll.Value := EnsureRange(FScrollY, 0, Max(FVertScroll.Max, 0));
    FHorzScroll.Value := EnsureRange(FScrollX, 0, Max(FHorzScroll.Max, 0));
  finally
    FSyncingScrollbars := false;
  end;
end;

procedure FXCustomTextArea.VScrollChange(Sender: TObject);
begin
  if FSyncingScrollbars then
    Exit;

  FScrollY := FVertScroll.Value;
  StandardUpdateDraw;
end;

procedure FXCustomTextArea.HScrollChange(Sender: TObject);
begin
  if FSyncingScrollbars then
    Exit;

  FScrollX := FHorzScroll.Value;
  StandardUpdateDraw;
end;

procedure FXCustomTextArea.UpdateAll;
begin
  if not CanUpdate then
    Exit;

  UpdateRects;
  EnsureCaretVisible;
  Redraw;
end;

procedure FXCustomTextArea.FontUpdate;
begin
  if CanUpdate then
    UpdateRects;

  Redraw;
end;

procedure FXCustomTextArea.InteractionStateChanged(AState: FXControlState);
begin
  inherited;
end;

procedure FXCustomTextArea.Loaded;
begin
  inherited;

  ValidatePositions;
  UpdateAll;
end;

procedure FXCustomTextArea.PushUndo;
begin
  if not FCanUndo then
    Exit;

  if (FHistory.Count > 0) and (FHistory[FHistory.Count-1] = FText) then
    Exit;

  FHistory.AddObject(FText, TObject(NativeInt(FCaret)));

  while FHistory.Count > TEXTAREA_UNDO_LIMIT do
    FHistory.Delete(0);
end;

procedure FXCustomTextArea.DoChange;
begin
  if not IsReading and Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure FXCustomTextArea.DoChangeValue;
begin
  if not IsReading and Assigned(FOnChangeValue) then
    FOnChangeValue(Self);
end;

// Getters

function FXCustomTextArea.GetSelStart: integer;
begin
  Result := Min(FCaret, FAnchor);
end;

function FXCustomTextArea.GetSelLength: integer;
begin
  Result := Abs(FCaret - FAnchor);
end;

function FXCustomTextArea.GetSelText: string;
begin
  Result := Copy(FText, GetSelStart+1, GetSelLength);
end;

function FXCustomTextArea.GetLineCount: integer;
begin
  Result := Length(FLines);
end;

function FXCustomTextArea.GetCaretLine: integer;
begin
  Result := LineFromPosition(FCaret);
end;

function FXCustomTextArea.GetCaretColumn: integer;
begin
  if Length(FLines) = 0 then
    Exit(0);

  Result := FCaret - FLines[GetCaretLine].StartPos;
  if Result < 0 then
    Result := 0;
end;

function FXCustomTextArea.GetLineText(AIndex: integer): string;
begin
  Result := '';
  if (AIndex >= 0) and (AIndex <= High(FLines)) then
    Result := FLines[AIndex].Text;
end;

// Setters

procedure FXCustomTextArea.SetWordWrap(const Value: boolean);
begin
  if FWordWrap = Value then
    Exit;

  FWordWrap := Value;
  FScrollX := 0;
  UpdateAll;
end;

procedure FXCustomTextArea.SetLineSpacing(const Value: integer);
begin
  if FLineSpacing = Value then
    Exit;

  FLineSpacing := Max(0, Value);
  UpdateAll;
end;

procedure FXCustomTextArea.SetRoundness(const Value: integer);
begin
  if FRoundness = Value then
    Exit;

  FRoundness := Value;
  StandardUpdateLayout;
end;

procedure FXCustomTextArea.SetLineSize(const Value: integer);
begin
  if FLineSize = Value then
    Exit;

  FLineSize := Max(0, Value);
  UpdateAll;
end;

procedure FXCustomTextArea.SetDetail(const Value: FXDetailType);
begin
  if FDetail = Value then
    Exit;

  FDetail := Value;
  UpdateAll;
end;

procedure FXCustomTextArea.SetTextMarginX(const Value: integer);
begin
  if FTextMarginX = Value then
    Exit;

  FTextMarginX := Value;
  UpdateAll;
end;

procedure FXCustomTextArea.SetTextMarginY(const Value: integer);
begin
  if FTextMarginY = Value then
    Exit;

  FTextMarginY := Value;
  UpdateAll;
end;

procedure FXCustomTextArea.SetTextHint(const Value: string);
begin
  if FTextHint = Value then
    Exit;

  FTextHint := Value;
  StandardUpdateDraw;
end;

procedure FXCustomTextArea.SetCharCase(const Value: FXCharCase);
begin
  if FCharCase = Value then
    Exit;

  FCharCase := Value;
  ApplyCharCase;
  UpdateAll;
end;

procedure FXCustomTextArea.SetShowScrollbar(const Value: boolean);
begin
  if FShowScrollbar = Value then
    Exit;

  FShowScrollbar := Value;
  UpdateAll;
end;

procedure FXCustomTextArea.SetCanUndo(const Value: boolean);
begin
  if FCanUndo = Value then
    Exit;

  if not Value then
    FHistory.Clear;

  FCanUndo := Value;
end;

procedure FXCustomTextArea.SetEnableSelection(const Value: boolean);
begin
  if FEnableSelection = Value then
    Exit;

  FEnableSelection := Value;

  if not FEnableSelection then
    FAnchor := FCaret;

  StandardUpdateDraw;
end;

procedure FXCustomTextArea.SetScrollY(const Value: integer);
var
  ANew: integer;
begin
  ANew := EnsureRange(Value, 0, MaxScrollY);
  if ANew = FScrollY then
    Exit;

  FScrollY := ANew;
  PushScrollToBars;
  StandardUpdateDraw;
end;

procedure FXCustomTextArea.SetScrollX(const Value: integer);
var
  ANew: integer;
begin
  ANew := EnsureRange(Value, 0, MaxScrollX);
  if ANew = FScrollX then
    Exit;

  FScrollX := ANew;
  PushScrollToBars;
  StandardUpdateDraw;
end;

procedure FXCustomTextArea.SetSelStart(const Value: integer);
var
  ALength: integer;
begin
  ALength := GetSelLength;

  FAnchor := EnsureRange(Value, 0, Length(FText));
  FCaret := EnsureRange(FAnchor + ALength, 0, Length(FText));

  ValidatePositions;
  EnsureCaretVisible;
  StandardUpdateDraw;
end;

procedure FXCustomTextArea.SetSelLength(const Value: integer);
begin
  FAnchor := GetSelStart;
  FCaret := EnsureRange(FAnchor + Max(Value, 0), 0, Length(FText));

  ValidatePositions;
  EnsureCaretVisible;
  StandardUpdateDraw;
end;

procedure FXCustomTextArea.SetMaxLength(const Value: integer);
begin
  if FMaxLength = Value then
    Exit;

  FMaxLength := Max(0, Value);

  if (FMaxLength > 0) and (Length(FText) > FMaxLength) then
    SetText( Copy(FText, 1, FMaxLength) );
end;

procedure FXCustomTextArea.SetCaretPosition(const Value: integer);
begin
  MoveCaret(Value, false);
end;

// Commands --

function FXCustomTextArea.HasSelection: boolean;
begin
  Result := FEnableSelection and (FCaret <> FAnchor);
end;

function FXCustomTextArea.TextLength: integer;
begin
  Result := Length(FText);
end;

procedure FXCustomTextArea.SelectAll;
begin
  if not FEnableSelection then
    Exit;

  FAnchor := 0;
  FCaret := Length(FText);

  ValidatePositions;
  EnsureCaretVisible;
  StandardUpdateDraw;
end;

procedure FXCustomTextArea.ClearSelection;
begin
  FAnchor := FCaret;
  StandardUpdateDraw;
end;

procedure FXCustomTextArea.DeleteSelection;
var
  AStart, AEnd: integer;
begin
  if FReadOnly or not HasSelection then
    Exit;

  AStart := GetSelStart;
  AEnd := AStart + GetSelLength;

  ApplyTextChange(Copy(FText, 1, AStart) + Copy(FText, AEnd+1, Length(FText)), AStart);
end;

procedure FXCustomTextArea.Clear;
begin
  Text := '';
end;

procedure FXCustomTextArea.Undo;
var
  Index: integer;
begin
  if not FCanUndo or (FHistory.Count = 0) then
    Exit;

  Index := FHistory.Count-1;

  FText := FHistory[Index];
  FCaret := integer(NativeInt(FHistory.Objects[Index]));
  FAnchor := FCaret;

  FHistory.Delete(Index);

  (* THE TEXT HAS CHANGED, the caret may now be out of bounds *)
  ValidatePositions;

  UpdateAll;

  DoChangeValue;
  DoChange;
end;

procedure FXCustomTextArea.CopyToClipBoard;
begin
  if not HasSelection then
    Exit;

  Clipboard.AsText := GetSelText;
end;

procedure FXCustomTextArea.CutToClipBoard;
begin
  if not HasSelection then
    Exit;

  CopyToClipBoard;

  if not FReadOnly then
    DeleteSelection;
end;

procedure FXCustomTextArea.PasteFromClipBoard;
var
  S: string;
begin
  if FReadOnly then
    Exit;

  S := Clipboard.AsText;
  if S = '' then
    Exit;

  if not FWantReturns then
    begin
      S := StringReplace(S, #13#10, ' ', [rfReplaceAll]);
      S := StringReplace(S, #13, ' ', [rfReplaceAll]);
      S := StringReplace(S, #10, ' ', [rfReplaceAll]);
    end;

  InsertText(S);
end;

procedure FXCustomTextArea.ScrollToCaret;
begin
  EnsureCaretVisible;
  StandardUpdateDraw;
end;

procedure FXCustomTextArea.ScrollToTop;
begin
  ScrollY := 0;
end;

procedure FXCustomTextArea.ScrollToBottom;
begin
  ScrollY := MaxScrollY;
end;

// Keys-

procedure FXCustomTextArea.HandleKeyDown(var CanHandle: boolean; Key: integer;
  ShiftState: TShiftState);
var
  Extend: boolean;
  NewPos, L, AX, ATarget: integer;
  ATotal: integer;
begin
  inherited;

  ValidatePositions;

  LastShiftState := ShiftState;
  Extend := (ssShift in ShiftState) and FEnableSelection;
  ATotal := Length(FText);

  case Key of
    VK_LEFT: begin
      if (ssCtrl in ShiftState) then
        NewPos := WordLeft(FCaret)
      else
        if HasSelection and not Extend then
          NewPos := GetSelStart
        else
          NewPos := StepLeft(FCaret);

      MoveCaret(NewPos, Extend);
      FDesiredX := -1;
      CanHandle := false;
    end;

    VK_RIGHT: begin
      if (ssCtrl in ShiftState) then
        NewPos := WordRight(FCaret)
      else
        if HasSelection and not Extend then
          NewPos := GetSelStart + GetSelLength
        else
          NewPos := StepRight(FCaret);

      MoveCaret(NewPos, Extend);
      FDesiredX := -1;
      CanHandle := false;
    end;

    VK_UP, VK_DOWN: begin
      if Length(FLines) > 0 then
        begin
          L := LineFromPosition(FCaret);

          if FDesiredX < 0 then
            FDesiredX := XFromPosition(FCaret);
          AX := FDesiredX;

          if Key = VK_UP then
            ATarget := L-1
          else
            ATarget := L+1;

          if (ATarget < 0) then
            MoveCaret(0, Extend)
          else
            if ATarget > High(FLines) then
              MoveCaret(ATotal, Extend)
            else
              MoveCaret(
                PositionFromPoint(TxtRect.Left + AX - FScrollX,
                  TxtRect.Top + ATarget*LineHeight - FScrollY + LineHeight div 2),
                Extend);
        end;

      CanHandle := false;
    end;

    VK_PRIOR, VK_NEXT: begin
      if Length(FLines) > 0 then
        begin
          L := LineFromPosition(FCaret);

          if FDesiredX < 0 then
            FDesiredX := XFromPosition(FCaret);
          AX := FDesiredX;

          if Key = VK_PRIOR then
            ATarget := L - VisibleLines
          else
            ATarget := L + VisibleLines;

          ATarget := EnsureRange(ATarget, 0, High(FLines));

          MoveCaret(
            PositionFromPoint(TxtRect.Left + AX - FScrollX,
              TxtRect.Top + ATarget*LineHeight - FScrollY + LineHeight div 2),
            Extend);
        end;

      CanHandle := false;
    end;

    VK_HOME: begin
      if ssCtrl in ShiftState then
        NewPos := 0
      else
        NewPos := PositionOfLineStart( LineFromPosition(FCaret) );

      MoveCaret(NewPos, Extend);
      FDesiredX := -1;
      CanHandle := false;
    end;

    VK_END: begin
      if ssCtrl in ShiftState then
        NewPos := ATotal
      else
        NewPos := PositionOfLineEnd( LineFromPosition(FCaret) );

      MoveCaret(NewPos, Extend);
      FDesiredX := -1;
      CanHandle := false;
    end;

    VK_BACK: begin
      if not FReadOnly then
        begin
          if HasSelection then
            DeleteSelection
          else
            if FCaret > 0 then
              begin
                if ssCtrl in ShiftState then
                  NewPos := WordLeft(FCaret)
                else
                  NewPos := StepLeft(FCaret);

                ApplyTextChange(
                  Copy(FText, 1, NewPos) + Copy(FText, FCaret+1, Length(FText)),
                  NewPos);
              end;
        end;

      FDesiredX := -1;
      CanHandle := false;
    end;

    VK_DELETE: begin
      if not FReadOnly then
        begin
          if HasSelection then
            DeleteSelection
          else
            if FCaret < ATotal then
              begin
                if ssCtrl in ShiftState then
                  NewPos := WordRight(FCaret)
                else
                  NewPos := StepRight(FCaret);

                ApplyTextChange(
                  Copy(FText, 1, FCaret) + Copy(FText, NewPos+1, Length(FText)),
                  FCaret);
              end;
        end;

      FDesiredX := -1;
      CanHandle := false;
    end;

    // Ctrl + A
    65: if ssCtrl in ShiftState then
      begin
        SelectAll;
        CanHandle := false;
      end;
    // Ctrl + C
    67: if ssCtrl in ShiftState then
      try
        CopyToClipBoard;
        CanHandle := false;
      except
      end;
    // Ctrl + V
    86: if (ssCtrl in ShiftState) and not FReadOnly then
      try
        PasteFromClipBoard;
        CanHandle := false;
      except
      end;
    // Ctrl + X
    88: if (ssCtrl in ShiftState) and not FReadOnly then
      try
        CutToClipBoard;
        CanHandle := false;
      except
      end;
    // Ctrl + Z
    90: if (ssCtrl in ShiftState) and not FReadOnly then
      begin
        Undo;
        CanHandle := false;
      end;
  end;

  ValidatePositions;
end;

procedure FXCustomTextArea.KeyPress(var Key: Char);
begin
  inherited;

  if FReadOnly then
    Exit;

  // Control characters
  if CharInSet(Key, [#8, #27]) then
    Exit;

  if Key = #9 then
    begin
      if FWantTabs then
        InsertText(#9);
      Exit;
    end;

  if CharInSet(Key, [#13, #10]) then
    begin
      if FWantReturns then
        InsertText(#13#10);
      Exit;
    end;

  // Shortcuts are handled in HandleKeyDown
  if (ssCtrl in LastShiftState) or (ssAlt in LastShiftState) then
    Exit;

  InsertText(Key);
end;

// Mouse

procedure FXCustomTextArea.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;

  if Button <> mbLeft then
    Exit;

  (* The scrollbars are real child controls (FVertScroll / FHorzScroll), so a
     click on either one never reaches here - VCL routes it to the scrollbar's
     own window. Nothing to special-case. *)

  // Caret
  FDownStart := PositionFromPoint(X, Y);
  FDesiredX := -1;

  MoveCaret(FDownStart, (ssShift in Shift) and FEnableSelection);
end;

procedure FXCustomTextArea.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if DoubleClickInProgress then
    Exit;

  // Selection
  if (InteractionState = FXControlState.Press) and (FDownStart <> -1)
    and FEnableSelection then
    begin
      // Auto scroll when dragging outside
      if Y < TxtRect.Top then
        FScrollY := EnsureRange(FScrollY - LineHeight, 0, MaxScrollY)
      else
        if Y > TxtRect.Bottom then
          FScrollY := EnsureRange(FScrollY + LineHeight, 0, MaxScrollY);

      FCaret := EnsureRange(PositionFromPoint(X, Y), 0, Length(FText));
      FAnchor := EnsureRange(FDownStart, 0, Length(FText));

      ValidatePositions;
      EnsureCaretVisible;
      StandardUpdateDraw;
    end;
end;

procedure FXCustomTextArea.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  FDownStart := -1;

  inherited;
end;

procedure FXCustomTextArea.DblClick;
var
  P1, P2, ATotal: integer;
begin
  inherited;

  if not FEnableSelection then
    Exit;

  ValidatePositions;
  ATotal := Length(FText);

  // Select the word under the caret
  P1 := FCaret;
  while (P1 > 0) and IsWordChar(FText[P1]) do
    Dec(P1);

  P2 := FCaret;
  while (P2 < ATotal) and IsWordChar(FText[P2+1]) do
    Inc(P2);

  if P1 = P2 then
    Exit;

  FAnchor := P1;
  FCaret := P2;

  ValidatePositions;
  EnsureCaretVisible;
  StandardUpdateDraw;
end;

function FXCustomTextArea.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
var
  ANew, Delta: integer;
begin
  if ssCtrl in Shift then
    Exit( inherited );

  Delta := MulDiv(WheelDelta, LineHeight * TEXTAREA_SCROLL_LINES, WHEEL_DELTA);

  // Shift+wheel scrolls horizontally, but only when that bar is actually in play
  if (ssShift in Shift) and FHorzScroll.Visible then
    begin
      ANew := EnsureRange(FScrollX - Delta, 0, MaxScrollX);
      if ANew <> FScrollX then
        begin
          FScrollX := ANew;
          PushScrollToBars;
          StandardUpdateDraw;
        end;

      Exit(true);
    end;

  if MaxScrollY <= 0 then
    Exit( inherited );

  ANew := EnsureRange(FScrollY - Delta, 0, MaxScrollY);

  if ANew <> FScrollY then
    begin
      FScrollY := ANew;
      PushScrollToBars;
      StandardUpdateDraw;
    end;

  Result := true;
end;

// Focus

procedure FXCustomTextArea.DoEnter;
begin
  inherited;
  UpdateLine;
end;

procedure FXCustomTextArea.DoExit;
begin
  inherited;

  if FClearSelOnExit then
    FAnchor := FCaret;

  UpdateLine;
end;

// Accessibility

function FXCustomTextArea.AccessibilityGetControlType: Integer;
begin
  Result := UIA_EditControlTypeId;
end;

function FXCustomTextArea.AccessibilityGetControlTypeName: string;
begin
  Result := 'edit';
end;

function FXCustomTextArea.AccessibilityGetName: string;
begin
  Result := FTextHint;
end;

function FXCustomTextArea.AccessibilityGetPattern(PatternId: Integer): IUnknown;
begin
  case PatternId of
    UIA_ValuePatternId:
      Result := TFXValueProvider.Create(Self);
  else
    Result := inherited AccessibilityGetPattern(PatternId);
  end;
end;

function FXCustomTextArea.AccessibilityGetValue: string;
begin
  Result := FText;
end;

function FXCustomTextArea.AccessibilityIsReadOnly: Boolean;
begin
  Result := FReadOnly;
end;

function FXCustomTextArea.AccessibilitySetValue(const Value: string): Boolean;
begin
  if FReadOnly then
    Exit(False);

  SetText(Value);
  Result := True;
end;

function FXCustomTextArea.Background: TColor;
begin
  Result := FDrawColors.Background;
end;

// Paint

procedure FXCustomTextArea.PaintBuffer;
var
  ACanvas: TCanvas;
  ARect, SelRect: TRect;
  FillColor: TColor;
  FPen: TGDIPen;
  ARound, LH, TH: integer;
  FirstLine, LastLine, I: integer;
  X, Y, X1, X2: integer;
  SelS, SelE, A, B: integer;
  Ln: FXTextAreaLine;
  Indic: TBitMap;
  AHintText: string;
  ACaretRect: TRect;
begin
  // Safety
  ValidatePositions;

  // Background
  Color := FDrawColors.BackGround;
  PaintBackground;

  ACanvas := Buffer;

  // Fill
  if Focused then
    FillColor := FEditColors.Press
  else
    FillColor := FEditColors.GetColor(InteractionState);

  ACanvas.GDIRoundRect(MakeRoundRect(DrawRect, FRoundness),
    TAlphaColor.Create(FillColor).MakeGDIBrush, nil);

  // Outline
  FPen := nil;
  case FDetail of
    FXDetailType.None, FXDetailType.Underline:
      FPen := TAlphaColor.Create( ChangeColorLight(FDrawColors.BackGround, EDIT_BORDER_FADE) ).MakeGDIPen(1);
    FXDetailType.Outline:
      FPen := TAlphaColor.Create( ChangeColorLight(FLineColor, EDIT_BORDER_FADE) ).MakeGDIPen(Max(FLineSize, 1));
  end;

  if FPen <> nil then
    begin
      ARect := DrawRect;
      ARect.Inflate(-trunc(FPen.GetWidth), -trunc(FPen.GetWidth));
      ARound := FRoundness-trunc(FPen.GetWidth);
      if ARound < 1 then
        ARound := 1;

      ACanvas.GDIRoundRect(MakeRoundRect(ARect, ARound), nil, FPen);
    end;

  // Underline
  if FDetail = FXDetailType.Underline then
    ACanvas.GDIRoundRect(MakeRoundRect(LineRect, Max(FLineSize, 1)),
      TAlphaColor.Create(FLineColor).MakeGDIBrush, nil);

  // Font
  ACanvas.Font.Assign( Self.Font );
  ACanvas.Font.Color := Self.Font.Color;
  ACanvas.Brush.Style := bsClear;

  LH := LineHeight;
  TH := TextH('Ag');
  if TH <= 0 then
    TH := LH;

  // Text hint
  if (FText = '') and (FTextHint <> '') then
    begin
      AHintText := FTextHint;
      ACanvas.Font.Color := ColorBlend(Self.Font.Color, FillColor, EDIT_TEXT_HINT_FADE);
      ACanvas.Brush.Style := bsClear;
      ACanvas.TextRect(TxtRect, TxtRect.Left, TxtRect.Top + (LH-TH) div 2, AHintText);
      ACanvas.Font.Color := Self.Font.Color;
    end;

  // Lines
  if (Length(FLines) > 0) and (TxtRect.Height > 0) and (TxtRect.Width > 0) then
    begin
      FirstLine := EnsureRange(FScrollY div LH, 0, High(FLines));
      LastLine := EnsureRange((FScrollY + TxtRect.Height) div LH, 0, High(FLines));

      SelS := GetSelStart;
      SelE := SelS + GetSelLength;

      for I := FirstLine to LastLine do
        begin
          Ln := FLines[I];

          Y := TxtRect.Top + I*LH - FScrollY;
          X := TxtRect.Left - FScrollX;

          // Selection
          if HasSelection and (SelE > Ln.StartPos)
            and (SelS < Ln.StartPos + Ln.Len + Ln.BreakLen) then
            begin
              A := EnsureRange(SelS, Ln.StartPos, Ln.StartPos + Ln.Len);
              B := EnsureRange(SelE, Ln.StartPos, Ln.StartPos + Ln.Len);

              X1 := X + TextW( Copy(Ln.Text, 1, A - Ln.StartPos) );
              X2 := X + TextW( Copy(Ln.Text, 1, B - Ln.StartPos) );

              // The line break is inside the selection
              if SelE > Ln.StartPos + Ln.Len then
                Inc(X2, TEXTAREA_NEWLINE_SELECT_WIDTH);

              SelRect := Rect(X1, Y, X2, Y + LH);
              SelRect.Intersect(TxtRect);

              if not SelRect.IsEmpty then
                ACanvas.GDIRectangle(SelRect,
                  TAlphaColor.Create(FDrawColors.Accent).MakeGDIBrush, nil);
            end;

          // Text
          ACanvas.Brush.Style := bsClear;
          if Ln.Text <> '' then
            ACanvas.TextRect(TxtRect, X, Y + (LH-TH) div 2, Ln.Text);
        end;

      // Caret
      if Focused and not IsDesigning then
        begin
          I := LineFromPosition(FCaret);

          Y := TxtRect.Top + I*LH - FScrollY;
          X := TxtRect.Left - FScrollX + XFromPosition(FCaret);

          ACaretRect := Rect(X, Y + (LH-TH) div 2,
            X + Max(TEXTAREA_CARET_WIDTH, 1), Y + (LH-TH) div 2 + TH);
          ACaretRect.Intersect(TxtRect);

          if not ACaretRect.IsEmpty then
            begin
              Indic := TBitMap.Create;
              try
                Indic.SetSize(Max(ACaretRect.Width, 1), Max(ACaretRect.Height, 1));
                Indic.Canvas.Brush.Color := clWhite;
                Indic.Canvas.FillRect(Rect(0, 0, Indic.Width, Indic.Height));

                StretchInvertedMask(Indic, ACanvas, ACaretRect);
              finally
                Indic.Free;
              end;
            end;
        end;
    end;

  (* The scrollbars are real FXScrollbar child controls (FVertScroll /
     FHorzScroll) - they paint themselves, nothing to draw here. *)

  inherited;
end;

end.
