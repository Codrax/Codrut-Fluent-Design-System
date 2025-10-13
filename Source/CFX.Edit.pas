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
  CFX.Colors,
  CFX.ThemeManager,
  CFX.Graphics,
  CFX.Constants,
  SysUtils,
  CFX.Classes,
  CFX.GDI,
  CFX.VarHelpers,
  CFX.Types,
  CFX.Linker,
  CFX.PopupMenu,
  CFX.Controls;

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
    procedure ChangeText(AText: string); virtual;
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

    // Text
    procedure SetText(const Value: string); virtual;
    function GetText: string; virtual;

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

    FOnNumberChanged: TNotifyEvent;

    // Updated
    procedure RangeUpdated(Sender: TObject);

    // Internal
    procedure UpdateTextValue;
    procedure TextUpdated;

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

implementation

procedure FXCustomEdit.KeyPress(var Key: Char);
begin
  inherited;
  if Key = #13 then begin
    EnterPressed;
    Exit;
  end;

  // Invalid
  if CharInSet(Key, [#8, #27]) then
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

  // Override Selection
  if (Key <> '') and HasSelection then
    DeleteSelection;

  // Add
  ChangeText( Copy(Text, 1, FPosition) + Key + Copy(Text, + FPosition+1, Length(Text)) );
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

      // Update
      StandardUpdateLayout;
    end;
end;

procedure FXCustomEdit.UpdateAutoSize;
begin
  if FAutoSize then
    if CanUpdate then
      SetBounds(Left, Top, Width, TextH('Aa.') + LineSize + FTextMarginX * 2)
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
  Result := Position+1 <= High(FText);
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

  Clipboard.AsText := Selection;
end;

procedure FXCustomEdit.DblClick;
var
  P1, P2: integer;
  FStart: integer;
begin
  inherited;

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
  ChangeText( Text.Remove(Index-1, 1) );
end;

procedure FXCustomEdit.DeleteSelection;
begin
  if FSelLength > 0 then
    begin
      ChangeText( Copy(FText, 1, FPosition) + Copy(FText, 1+FPosition+FSelLength, Length(FText)) );

      FSelLength := 0;

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
  // Password Char
  if PasswordChar <> #0 then
    begin
      if GoesLeft then
        Result := 0
      else
        Result := TextLength;

      Exit;
    end;

  // Data
  Result := From;
  ATotal := TextLength;

  // Search characters
  if GoesLeft then
    while Result >= 1 do
      begin
        Dec(Result);

        if (Result = 0) or AnalizeCharSpace(FText[Result]){ or not AnalizeCharSolid(FText[Result])} then
          begin
            Exit(Result);
          end;
      end
    else
      while Result <= ATotal-1 do
      begin
        Inc(Result);

        if AnalizeCharSpace(FText[Result]){ or not AnalizeCharSolid(FText[Result])} then
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
  UpdateRects;
  Redraw;
end;
var
  NewPos: integer;
  SelectionDone: boolean;
begin
  inherited;

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
                IncPosition(-1);
                SelectionLength := SelectionLength+1;
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
                IncPosition(1);
                SelectionLength := SelectionLength-1;
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
      CopyToClipBoard;
    86: if CanEdit and (ssCtrl in ShiftState) then
      PasteFromClipBoard;
    88: if CanEdit and (ssCtrl in ShiftState) then
        CutToClipBoard;
    90: if CanEdit and (ssCtrl in ShiftState) then
      Undo;
  end;

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
  // Undp
  if FCanUndo then
    FHistory.Add(FText);

  // Text
  FText := AText;

  // Char
  ApplyCharCase;

  // Notify
  if Assigned(OnChange) then
    OnChange(Self);
  if Assigned(OnChangeValue) then
    OnChangeValue(Self);

  // Scroll
  ScrollForCursor;

  // Update
  StandardUpdateLayout;
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
        GetRGB(FillColor).MakeGDIBrush, nil);

      // Outline
      FPen := nil;
      case Detail of
        FXDetailType.None, FXDetailType.Underline: FPen := GetRGB(ChangeColorLight(FDrawColors.BackGround, EDIT_BORDER_FADE) ).MakeGDIPen(1);
        FXDetailType.Outline: FPen := GetRGB(ChangeColorLight(FLineColor, EDIT_BORDER_FADE) ).MakeGDIPen(LineSize);
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
        GDIRoundRect(MakeRoundRect(LineRect, LineSize), GetRGB(FLineColor).MakeGDIBrush, nil);

      // Selection

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

      ARect.Top := TxtRect.Top + (TxtRect.Height - TxtHeight) div 2;
      ARect.Height := TxtHeight;
      ARect.Left := TxtRect.Left + FDrawPosition;
      ARect.Right := ARect.Left + TextWidth(ASelect);

      GDIRectangle(ARect, GetRGB(FDrawColors.Accent).MakeGDIBrush, nil);

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

          Indic := TBitMap.Create(FIndicatorWidth, TxtHeight);
          try
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
  ClipText := Clipboard.AsText;

  // Numbers Only
  if NumbersOnly then
    try
      ClipText.ToInt64;
    except
      Exit;
    end;

  // Selection
  if HasSelection then
    NewText := Copy(FText, 1, FPosition) + ClipText + Copy(FText, 1+FPosition+FSelLength, Length(FText))
  else
    NewText := Copy(FText, 1, FPosition) + ClipText + Copy(FText, FPosition+1, Length(FText));

  FSelLength := 0;

  // Set
  FPosition := FPosition + ClipText.Length;
  ChangeText( NewText );
end;

procedure FXCustomEdit.PopupBeforePopup(Sender: TObject; var CanPopup: boolean;
  Point: TPoint);
var
  HasSelection: boolean;
  IsPassword: boolean;
begin
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
    0: CutToClipboard;
    1: CopyToClipboard;
    2: PasteFromClipboard;
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
          Text := 'Cut';
          Shortcut := 'Ctrl+X';
        end;
      Add(M);

      M := FXPopupItem.Create(FDefaultMenu);
      with M do
        begin
          Image.Enabled := true;
          Image.IconType := FXIconType.SegoeIcon;
          Image.SelectSegoe := #$E8C8;
          Text := 'Copy';
          Shortcut := 'Ctrl+C';
        end;
      Add(M);

      M := FXPopupItem.Create(FDefaultMenu);
      with M do
        begin
          Image.Enabled := true;
          Image.IconType := FXIconType.SegoeIcon;
          Image.SelectSegoe := #$E77F;
          Text := 'Paste';
          Shortcut := 'Ctrl+V';
        end;
      Add(M);

      M := FXPopupItem.Create(FDefaultMenu);
      with M do
        begin
          Image.Enabled := true;
          Image.IconType := FXIconType.SegoeIcon;
          Image.SelectSegoe := #$E7A7;
          Text := 'Undo';
          Shortcut := 'Ctrl+Z';
        end;
      Add(M);

      M := FXPopupItem.Create(FDefaultMenu);
      with M do
        begin
          Image.Enabled := true;
          Image.IconType := FXIconType.SegoeIcon;
          Image.SelectSegoe := #$E8B3;
          Text := 'Select All';
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

  ATextWidth := TextW(DrawText);
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
begin
  if not EnableSelection then
    Exit;

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
  try
    FText.ToInt64;
  except
    FText := '0';
  end;

  StandardUpdateLayout;;
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
  if (FPosition = Value) or (Value < 0) or (Value > Length(Text)) then
    Exit;

  FPosition := Value;

  // Sel
  ClearSelection;

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
  inherited;

  // Test if valid
  TextUpdated;
end;

constructor FXNumberEdit.Create(aOwner: TComponent);
begin
  inherited;
  FNumberType := FXNumberType.Integer;
  Decimals := 2;
  FText := '0';

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
begin
  // Inherit
  inherited;

  // Test if valid
  TextUpdated;

  if not IsReading then
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

procedure FXNumberEdit.TextUpdated;
var
  Number: string;
  I: Int64;
  New: Extended;
begin
  try
    Number := Text;

    case FNumberType of
      FXNumberType.Integer: begin
        if not TryStrToInt64(Number, I) then
          Exit;
        New := I;
      end;
      FXNumberType.Extended: begin
        if not TryStrToFloat(Number, New) then
          Exit;
      end;
      FXNumberType.Currency: begin
        if not TryStrToFloat(Number, New) then
          Exit;
      end;
    end;

    SetValueEx(New, false);
  except
  end;
end;

procedure FXNumberEdit.UpdateTextValue;
var
  Number: string;
begin
  // Set
  case FNumberType of
    FXNumberType.Integer: Number := ValueInt.ToString;
    FXNumberType.Extended,
    FXNumberType.Currency:
      if Decimals > 0 then
        Number := Format('%.2f', [Value])
      else
        Number := Value.ToString;
  end;

  // Update
  if Number <> Text then
    inherited ChangeText(Number);
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
  if FValue = Value then
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

end.
