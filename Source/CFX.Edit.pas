unit CFX.Edit;

interface
uses
  Classes,
  Messages,
  Windows,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.Clipbrd,
  Types,
  Math,
  CFX.Colors,
  CFX.ThemeManager,
  CFX.Graphics,
  CFX.UIConsts,
  SysUtils,
  CFX.Classes,
  CFX.GDI,
  CFX.VarHelpers,
  CFX.Types,
  CFX.Linker,
  CFX.Animations,
  CFX.PopupMenu,
  CFX.Controls;

type
  FXCustomEdit = class(FXWindowsControl, FXControl)
  private
    var DrawRect, LineRect, TxtRect: TRect;
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
    FLayout: FXLayout;
    FLayoutHoriz: FXLayout;
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
    FTextHint: string;
    FDetail: FXDetailType;

    //  Internal
    procedure UpdateColors;
    procedure UpdateRects;

    // Canvas
    function TextW(AText: string): integer;
    function TextH(AText: string): integer;

    // Handle Messages
    procedure WM_LButtonDown(var Msg: TWMMouse); message WM_LBUTTONDOWN;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;

    // Data
    procedure UpdateAutoSize;
    procedure UpdateLine;
    procedure UpdateDrawPosition;

    // Menu
    procedure PrepDefaultMenu;
    procedure PopupBeforePopup(Sender: TObject; var CanPopup: boolean; Point: TPoint);
    procedure PopupItemClick(Sender: TObject; Item: FXPopupComponent; Index: integer);

    // Text
    procedure ChangeText(AText: string);
    procedure DeleteChar(Index: integer);

    procedure ScrollForCursor; overload;
    procedure ScrollForCursor(ADrawPosition: integer); overload;

    function SearchPosition(AX: integer): integer;
    function FindNext(From: integer; GoesLeft: boolean = false): integer;
    procedure SelectPoints(P1, P2: integer);

    procedure ApplyCharCase;
    function DrawText: string;

    // Getter
    function GetValue: int64;

    // Setters
    procedure SetText(const Value: string);
    procedure SetAutoSizing(const Value: boolean);
    procedure SetLineSize(const Value: integer);
    procedure SetRoundness(const Value: integer);
    procedure SetPosition(const Value: integer);
    procedure SetLayout(const Value: FXLayout);
    procedure SetSelLength(const Value: integer);
    procedure SetPasswordChar(const Value: char);
    procedure SetCanUndo(const Value: boolean);
    procedure SetEnableSelection(const Value: boolean);
    procedure SetCharCase(const Value: FXCharCase);
    procedure SetNumbersOnly(const Value: boolean);
    procedure SetValue(const Value: int64);
    procedure SetLayoutHoriz(const Value: FXLayout);
    procedure SetTextHint(const Value: string);
    procedure SetDetail(const Value: FXDetailType);
    procedure SetTextMarginX(const Value: integer);
    procedure SetTextMarginY(const Value: integer);

  protected
    procedure PaintBuffer; override;
    procedure Resize; override;
    procedure ChangeScale(M, D: Integer{$IF CompilerVersion > 29}; isDpiChange: Boolean{$ENDIF}); override;

    function CanEdit: boolean;

    // State
    procedure InteractionStateChanged(AState: FXControlState); override;

    // Font
    procedure FontUpdate; override;

    // Key Presses
    procedure HandleKeyDown(var CanHandle: boolean; Key: integer; ShiftState: TShiftState); override;
    procedure KeyPress(var Key: Char); override;

    // Mouse
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure DblClick; override;

    // Inherited
    procedure ComponentCreated; override;
    procedure Loaded; override;
    procedure OpenPopupMenu(X, Y: integer); override;
    procedure DoEnter; override;
    procedure DoExit; override;

    // Text
    property Text: string read FText write SetText;
    property PasswordChar: char read FPassChar write SetPasswordChar;

    property Value: int64 read GetValue write SetValue;

    property Position: integer read FPosition write SetPosition;
    property SelectionLength: integer read FSelLength write SetSelLength;
    property Layout: FXLayout read FLayout write SetLayout default FXLayout.Center;
    property LayoutHorizontal: FXLayout read FLayoutHoriz write SetLayoutHoriz default FXLayout.Beginning;

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

    //  Modify default props
    property ParentColor default true;

  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    // Interface
    function IsContainer: Boolean;
    procedure UpdateTheme(const UpdateChildren: Boolean);

    function Background: TColor;
  end;

  FXEdit = class(FXCustomEdit)
  published
    property Text;
    property PasswordChar;

    property Value;

    property Position;
    property SelectionLength;
    property Layout default FXLayout.Center;
    property LayoutHorizontal default FXLayout.Beginning;

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

implementation

procedure FXCustomEdit.InteractionStateChanged(AState: FXControlState);
begin
  inherited;
  Invalidate;
end;

function FXCustomEdit.IsContainer: Boolean;
begin
  Result := false;
end;

procedure FXCustomEdit.KeyPress(var Key: Char);
begin
  inherited;
  // Invalid
  if CharInSet(Key, [#8, #13, #27]) then
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

procedure FXCustomEdit.Loaded;
begin
  inherited;
  Invalidate;
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
              Invalidate;
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

procedure FXCustomEdit.UpdateTheme(const UpdateChildren: Boolean);
begin
  UpdateColors;
  UpdateRects;
  Invalidate;
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
      UpdateRects;
      Invalidate;
    end;
end;

procedure FXCustomEdit.UpdateAutoSize;
var
  AHeight: integer;
begin
  if FAutoSize then
    begin
      AHeight := TextH(Text);

      if AHeight > 0 then
        begin
          AHeight := AHeight + LineSize + FTextMarginX * 2;

          if Height <> AHeight then
            Height := AHeight;
        end;
    end;
end;

procedure FXCustomEdit.UpdateColors;
begin
  FDrawColors.Assign( ThemeManager.SystemColor );

  // Access theme manager
  if FCustomColors.Enabled then
    begin
      // Custom Colors
      FDrawColors.Accent := FCustomColors.Accent;
      FDrawColors.Foreground := ExtractColor(FCustomColors, FXColorType.Foreground);
      FDrawColors.BackGround := ExtractColor(FCustomColors, FXColorType.BackGround);
      FDrawColors.BackGroundInterior := ExtractColor(FCustomColors, FXColorType.Content);
    end
  else
    begin
      // Global Colors
      FDrawColors.Accent := ThemeManager.AccentColor;
      FDrawColors.ForeGround := ThemeManager.SystemColor.ForeGround;

      FDrawColors.BackGround := GetParentBackgroundColor(ThemeManager.SystemColor.Background);

      if ThemeManager.DarkTheme then
        FDrawColors.BackGroundInterior := ChangeColorLight(ThemeManager.SystemColor.BackGroundInterior, EDIT_COLOR_CHANGE * 4)
      else
        FDrawColors.BackGroundInterior := ChangeColorLight(ThemeManager.SystemColor.BackGroundInterior, -EDIT_COLOR_CHANGE * 4);
    end;

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
  Invalidate;
end;

procedure FXCustomEdit.UpdateRects;
begin
  DrawRect := ClientRect;

  // Rects
  LineRect := DrawRect;
  LineRect.Top := Height - LineSize;

  // Pos
  TxtRect := DrawRect;
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
  FLayout := FXLayout.Center;
  FLayoutHoriz := FXLayout.Beginning;
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
  Height := 35;
  Width := 150;

  Font.Color := 255;

  // Font
  Font.Height := ThemeManager.FormFontHeight;

  // Update
  UpdateRects;
  UpdateColors;
end;

procedure FXCustomEdit.CutToClipBoard;
begin
  if PasswordChar <> #0 then
    Exit;

  CopyToClipBoard;

  DeleteSelection;
end;

procedure FXCustomEdit.ComponentCreated;
begin
  inherited;
  Invalidate;
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
begin
  inherited;
  P1 := FindNext(Position, true);
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

      Invalidate;
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

function FXCustomEdit.ExtendsBounds: boolean;
begin
  Result := TextW(DrawText) > TxtRect.Width;
end;

function FXCustomEdit.FindNext(From: integer; GoesLeft: boolean): integer;
function AnaliseChar(C: char): boolean;
begin
  Result := not CharInSet(C, ['A'..'Z', 'a'..'z', '0'..'9']);
end;
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

        if (Result = 0) or AnaliseChar(FText[Result]) then
          begin
            Exit(Result);
          end;
      end
    else
      while Result < ATotal do
      begin
        Inc(Result);

        if AnaliseChar(FText[Result]) then
          begin
            Exit(Result - 1);
          end;
      end;
end;

procedure FXCustomEdit.FontUpdate;
begin
  Invalidate;

  UpdateAutoSize;
end;

procedure FXCustomEdit.HandleKeyDown(var CanHandle: boolean; Key: integer;
  ShiftState: TShiftState);
procedure IncPosition(By: integer);
begin
  FPosition := FPosition + By;
  UpdateRects;
  Invalidate;
end;
var
  NewPos: integer;
begin
  inherited;

  LastShiftState := ShiftState;

  if FHandleUpDown then
    case Key of
      VK_UP: Key := VK_LEFT;
      VK_DOWN: Key := VK_RIGHT;
    end;
  
  case Key of
    // Left & Right
    VK_LEFT: begin
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
      if FSelLength = 0 then
        FSelGoesLeft := false;

      // Select
      if ssCtrl in ShiftState then
        begin
          NewPos := FindNext(SelectionEnd+1, false);

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

                Invalidate;
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

          Invalidate;
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
end;

function FXCustomEdit.HasSelection: boolean;
begin
  Result := FSelLength > 0;
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

procedure FXCustomEdit.ChangeScale(M, D: Integer{$IF CompilerVersion > 29}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  UpdateRects;
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
  UpdateRects;
  Invalidate;
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
  if Creating then
    Exit;

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
        FXLayout.Beginning: Y := 0;
        FXLayout.Center: Y := (TxtRect.Height - TxtHeight) div 2;
        FXLayout.Ending: Y := TxtRect.Height-TxtHeight;
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

procedure FXCustomEdit.Resize;
begin
  inherited;
  UpdateRects;
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
      FXLayout.Center: FCutPosition := -(TxtRect.Width - ATextWidth) div 2;
      FXLayout.Ending: FCutPosition := -(TxtRect.Width - ATextWidth);
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
  UpdateRects;
  Invalidate;
end;

procedure FXCustomEdit.SetAutoSizing(const Value: boolean);
begin
  if FAutoSize <> Value then
    begin
      FAutoSize := Value;

      // Auto Size
      UpdateAutoSize;

      UpdateRects;
      Invalidate;
    end;
end;

procedure FXCustomEdit.SetCanUndo(const Value: boolean);
begin
  if FCanUndo <> Value then
    begin
      FCanUndo := Value;

      if not Value then
        FHistory.Clear;
    end;
end;

procedure FXCustomEdit.SetCharCase(const Value: FXCharCase);
begin
  if FCharCase <> Value then
    begin
      FCharCase := Value;

      ApplyCharCase;

      Invalidate;
    end;
end;

procedure FXCustomEdit.SetDetail(const Value: FXDetailType);
begin
  if FDetail <> Value then
    begin
      FDetail := Value;

      Invalidate;
    end;
end;

procedure FXCustomEdit.SetEnableSelection(const Value: boolean);
begin
  if FEnableSelection <> Value then
    begin
      FEnableSelection := Value;

      if SelectionLength <> 0 then
        SelectionLength := 0;
    end;
end;

procedure FXCustomEdit.SetLayout(const Value: FXLayout);
begin
  if FLayout <> Value then
    begin
      FLayout := Value;

      UpdateRects;
      Invalidate;
    end;
end;

procedure FXCustomEdit.SetLayoutHoriz(const Value: FXLayout);
begin
  if FLayoutHoriz <> Value then
    begin
      FLayoutHoriz := Value;

      UpdateRects;
      Invalidate;
    end;
end;

procedure FXCustomEdit.SetLineSize(const Value: integer);
begin
  if FLineSize <> Value then
    begin
      FLineSize := Value;

      UpdateRects;
      Invalidate;
    end;
end;

procedure FXCustomEdit.SetNumbersOnly(const Value: boolean);
begin
  if FNumbersOnly <> Value then
    begin
      FNumbersOnly := Value;

      try
        FText.ToInt64;
      except
        FText := '0';
      end;

      Invalidate;
    end;
end;

procedure FXCustomEdit.SetPasswordChar(const Value: char);
begin
  if FPassChar <> Value then
    begin
      FPassChar := Value;

      UpdateRects;
      Invalidate;
    end;
end;

procedure FXCustomEdit.SetPosition(const Value: integer);
begin
  if FPosition <> Value then
    begin
      if (Value >=0) and (Value <= Length(Text)) then
        begin
          FPosition := Value;

          ClearSelection;
          UpdateRects;

          Invalidate;  
        end;
    end;
end;

procedure FXCustomEdit.SetRoundness(const Value: integer);
begin
  if FRoundness <> Value then
    begin
      FRoundness := Value;

      Invalidate;
    end;
end;

procedure FXCustomEdit.SetSelLength(const Value: integer);
var
  ATotal: integer;
begin
  // Set Selection
  if FSelLength <> Value then
    begin
      FSelLength := Value;

      ATotal := TextLength;

      if FPosition + FSelLength > ATotal then
        FSelLength := ATotal - FPosition;

      if FSelLength < 0 then
        FSelLength := 0;

      // Update
      UpdateRects;
      Invalidate;
    end;
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
  UpdateRects;
  Invalidate;
end;

procedure FXCustomEdit.SetTextHint(const Value: string);
begin
  if FTextHint <> Value then
    begin
      FTextHint := Value;

      Invalidate;
    end;
end;

procedure FXCustomEdit.SetTextMarginX(const Value: integer);
begin
  if FTextMarginX <> Value then
    begin
      FTextMarginX := Value;

      UpdateRects;
      Invalidate;
    end;
end;

procedure FXCustomEdit.SetTextMarginY(const Value: integer);
begin
    if FTextMarginY <> Value then
    begin
      FTextMarginY := Value;

      UpdateRects;
      Invalidate;
    end;
end;

procedure FXCustomEdit.SetValue(const Value: int64);
begin
  if not IsReading then
    Text := Value.ToString;
end;

function FXCustomEdit.TextH(AText: string): integer;
begin
  Result := 0;
  if Parent <> nil then
    begin
      Canvas.Font.Assign( Self.Font );
      Result := Canvas.TextHeight(AText);
    end;
end;

function FXCustomEdit.TextW(AText: string): integer;
begin
  Result := 0;
  if Parent <> nil then
    begin
      Canvas.Font.Assign( Self.Font );
      Result := Canvas.TextWidth(AText);
    end;
end;

procedure FXCustomEdit.WMSize(var Message: TWMSize);
begin
  UpdateRects;
  Invalidate;
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

end.
