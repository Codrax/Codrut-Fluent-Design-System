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
  CFX.VarHelpers,
  CFX.Types,
  CFX.Linker,
  CFX.Animations,
  CFX.PopupMenu,
  CFX.Controls;

type
  FXEdit = class(FXWindowsControl, FXControl)
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
      FRoundness: integer;
      FLineColor: TColor;
      FLayout: FXLayout;
      FSelGoesLeft: boolean;
      FDownStart: integer;
      LastShiftState: TShiftState;
      FHistory: TStringList;
      FDefaultMenu: FXPopupMenu;
      FTextMargin: integer;
      FPassChar: char;
      FCanUndo: boolean;
      FEnableSelection: boolean;
      FCharCase: FXCharCase;
      FNumbersOnly: boolean;
      FClearSelOnExit: boolean;

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

      // Font
      procedure FontUpdate(Sedner: TObject);

      // Text
      procedure ChangeText(AText: string);
      procedure DeleteChar(Index: integer);

      procedure ApplyCharCase;
      procedure ScrollForCursor; overload;
      procedure ScrollForCursor(ADrawPosition: integer); overload;
      function SearchPosition(AX: integer): integer;
      function FindNext(From: integer; GoesLeft: boolean = false): integer;
      procedure SelectPoints(P1, P2: integer);
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
      procedure SetTextMargin(const Value: integer);
      procedure SetPasswordChar(const Value: char);
      procedure SetCanUndo(const Value: boolean);
      procedure SetEnableSelection(const Value: boolean);
      procedure SetCharCase(const Value: FXCharCase);
      procedure SetNumbersOnly(const Value: boolean);
      procedure SetValue(const Value: int64);

    protected
      procedure PaintBuffer; override;
      procedure Resize; override;
      procedure ChangeScale(M, D: Integer{$IF CompilerVersion > 29}; isDpiChange: Boolean{$ENDIF}); override;

      // State
      procedure InteractionStateChanged(AState: FXControlState); override;

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

    published
      // Text
      function SelectionStart: integer;
      function SelectionEnd: integer;
      function Selection: string;
      function TextLength: integer;
      function HasSelection: boolean;

      procedure ClearSelection;
      procedure DeleteSelection;
      procedure SelectAll;

      procedure Undo;

      procedure CopyToClipBoard;
      procedure CutToClipBoard;
      procedure PasteFromClipBoard;

      // Custom Colors
      property CustomColors: FXCompleteColorSets read FCustomColors write FCustomColors stored true;
      property CustomEditColors: FXSingleColorStateSets read FCustomEditColors write FCustomEditColors;

      // Text
      property Text: string read FText write SetText;
      property PasswordChar: char read FPassChar write SetPasswordChar;

      property Value: int64 read GetValue write SetValue;

      property Position: integer read FPosition write SetPosition;
      property SelectionLength: integer read FSelLength write SetSelLength;
      property Layout: FXLayout read FLayout write SetLayout default FXLayout.Center;

      // Settings
      property ClearSelectionOnExit: boolean read FClearSelOnExit write FClearSelOnExit default true;
      property CanUndo: boolean read FCanUndo write SetCanUndo default true;
      property CharCase: FXCharCase read FCharCase write SetCharCase default FXCharCase.Both;
      property NumbersOnly: boolean read FNumbersOnly write SetNumbersOnly default false;
      property EnableSelection: boolean read FEnableSelection write SetEnableSelection default true;
      property AutoSize: boolean read FAutoSize write SetAutoSizing default true;
      property LineSize: integer read FLineSize write SetLineSize default EDIT_LINE_SIZE;
      property Roundness: integer read FRoundness write SetRoundness default EDIT_BORDER_ROUND;
      property TextMargin: integer read FTextMargin write SetTextMargin default EDIT_EXTRA_SPACE;

      // Other
      property HandleUpDown: boolean read FHandleUpDown write FHandleUpDown default true;
      property Font;

      property Cursor default crIBeam;
      property Align;
      property Constraints;
      property Anchors;
      property Hint;
      property ShowHint;
      property TabStop;
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

implementation

procedure FXEdit.InteractionStateChanged(AState: FXControlState);
begin
  inherited;
  PaintBuffer;
end;

function FXEdit.IsContainer: Boolean;
begin
  Result := false;
end;

procedure FXEdit.KeyPress(var Key: Char);
begin
  inherited;
  // Invalid
  if CharInSet(Key, [#8, #13]) then
    Exit;

  // Special keys
  if (ssCtrl in LastShiftState) or (ssAlt in LastShiftState) then
    Exit;

  // Numbers Only
  if NumbersOnly and not CharInSet(Key, ['0'..'0']) then
    Exit;

  // Override Selection
  if (Key <> '') and HasSelection then
    Self.DeleteSelection;

  // Add
  ChangeText( Copy(Text, 1, FPosition) + Key + Copy(Text, + FPosition+1, Length(Text)) );
  Position := Position + 1;
end;

procedure FXEdit.Loaded;
begin
  inherited;
  Invalidate;
end;

function FXEdit.TextLength: integer;
begin
  Result := Length(FText);
end;

procedure FXEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  PosX, Value: integer;
begin
  inherited;
  if InteractionState = FXControlState.Press then
    begin
      if (FDownStart <> -1) and EnableSelection then
        begin
          PosX := X - TxtRect.Left + FCutPosition;

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

procedure FXEdit.OpenPopupMenu(X, Y: integer);
begin
  if not Assigned(PopupMenu) then
    FDefaultMenu.PopupAtPoint(ClientToScreen(Point(X, Y)))
  else
    inherited;
end;

procedure FXEdit.UpdateTheme(const UpdateChildren: Boolean);
begin
  UpdateColors;
  UpdateRects;
  Invalidate;
end;

function FXEdit.GetValue: int64;
begin
  Result := 0;
  if FText <> '' then
    try
      Result := FText.ToInt64;
    except

    end;
end;

procedure FXEdit.Undo;
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

procedure FXEdit.UpdateAutoSize;
var
  AHeight: integer;
begin
  if FAutoSize then
    begin
      AHeight := TextH(Text);

      if AHeight > 0 then
        begin
          AHeight := AHeight + LineSize + FTextMargin * 2;

          if Height <> AHeight then
            Height := AHeight;
        end;
    end;
end;

procedure FXEdit.UpdateColors;
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

procedure FXEdit.UpdateDrawPosition;
begin
  FDrawPosition := TextW( Copy(DrawText, 1, FPosition) ) - FCutPosition;
end;

procedure FXEdit.UpdateLine;
begin
  if Focused then
    FLineColor := FDrawColors.Accent
  else
    FLineColor := FDrawColors.BackGroundInterior;
  PaintBuffer;
end;

procedure FXEdit.UpdateRects;
begin
  DrawRect := Rect(0, 0, Width, Height);

  // Rects
  LineRect := DrawRect;
  LineRect.Top := Height - LineSize;

  // Pos
  TxtRect := DrawRect;
  TxtRect.Inflate(-TextMargin, -TextMargin);

  TxtRect.Bottom := TxtRect.Bottom - LineSize;

  UpdateDrawPosition;
  if not HasSelection then
    ScrollForCursor;
end;

constructor FXEdit.Create(aOwner: TComponent);
begin
  inherited;
  ParentColor := false;
  TabStop := true;
  AutoFocusLine := false;
  BufferedComponent := true;
  FIndicatorWidth := EDIT_INDIC_WIDTH;
  FRoundness := EDIT_BORDER_ROUND;
  FTextMargin := EDIT_EXTRA_SPACE;
  FLayout := FXLayout.Center;
  FCanUndo := true;
  FEnableSelection := true;
  FClearSelOnExit := true;

  FAutoSize := true;
  FLineSize := EDIT_LINE_SIZE;

  FHandleUpDown := true;
  FPassChar := #0;

  Cursor := crIBeam;

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
  Font.OnChange := FontUpdate;

  // Update
  UpdateRects;
  UpdateColors;
end;

procedure FXEdit.CutToClipBoard;
begin
  if PasswordChar <> #0 then
    Exit;

  CopyToClipBoard;

  DeleteSelection;
end;

procedure FXEdit.ComponentCreated;
begin
  inherited;
  Invalidate;
end;

procedure FXEdit.CopyToClipBoard;
begin
  if PasswordChar <> #0 then
    Exit;

  Clipboard.AsText := Selection;
end;

procedure FXEdit.DblClick;
var
  P1, P2: integer;
begin
  inherited;
  P1 := FindNext(Position, true);
  P2 := FindNext(Position, false);

  SelectPoints(P1, P2);
end;

procedure FXEdit.DeleteChar(Index: integer);
begin
  ChangeText( Text.Remove(Index-1, 1) );
end;

procedure FXEdit.DeleteSelection;
begin
  if FSelLength > 0 then
    begin
      ChangeText( Copy(FText, 1, FPosition) + Copy(FText, 1+FPosition+FSelLength, Length(FText)) );

      FSelLength := 0;

      PaintBuffer;
    end;
end;

destructor FXEdit.Destroy;
begin
  FreeAndNil( FCustomColors );
  FreeAndNil( FDrawColors );
  FreeAndNil( FCustomEditColors );
  FreeAndNil( FEditColors );
  FreeAndNil( FHistory );
  inherited;
end;

procedure FXEdit.DoEnter;
begin
  inherited;

  UpdateLine;
end;

procedure FXEdit.DoExit;
begin
  inherited;

  if NumbersOnly and (FText = '') then
    Text := '0';

  if FClearSelOnExit and HasSelection then
    SelectionLength := 0;

  UpdateLine;
end;

function FXEdit.DrawText: string;
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

function FXEdit.FindNext(From: integer; GoesLeft: boolean): integer;
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
    while Result > 0 do
      begin
        Dec(Result);

        if AnaliseChar(FText[Result]) then
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

procedure FXEdit.FontUpdate(Sedner: TObject);
begin
  Invalidate;
  UpdateAutoSize;
end;

procedure FXEdit.HandleKeyDown(var CanHandle: boolean; Key: integer;
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
    VK_BACK: begin
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

                PaintBuffer;
                CanHandle := false;
              end;
          end;
    end;

    VK_DELETE: begin
      if HasSelection then
        Self.DeleteSelection
      else
        // Delete
        begin
          DeleteChar(Position+1);

          PaintBuffer;
          CanHandle := false;
        end;
    end;

    // Keyboard shortcuts
    65: if ssCtrl in ShiftState then
      SelectAll;
    67: if ssCtrl in ShiftState then
      CopyToClipBoard;
    86: if ssCtrl in ShiftState then
      PasteFromClipBoard;
    88: if ssCtrl in ShiftState then
      CutToClipBoard;
    90: if ssCtrl in ShiftState then
      Undo;
  end;
end;

function FXEdit.HasSelection: boolean;
begin
  Result := FSelLength > 0;
end;

procedure FXEdit.ApplyCharCase;
begin
  case CharCase of
    FXCharCase.Uppercase: FText := UpperCase(FText);
    FXCharCase.Lowercase: FText := Lowercase(FText);
  end;
end;

function FXEdit.Background: TColor;
begin
  Result := FDrawColors.Background;
end;

procedure FXEdit.ChangeScale(M, D: Integer{$IF CompilerVersion > 29}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  UpdateRects;
end;

procedure FXEdit.ChangeText(AText: string);
begin
  // Undp
  if FCanUndo then
    FHistory.Add(FText);

  // Text
  FText := AText;

  // Char
  ApplyCharCase;

  // Update
  UpdateRects;
  PaintBuffer;
end;

procedure FXEdit.ClearSelection;
begin
  SelectionLength := 0;
end;

procedure FXEdit.PaintBuffer;
var
  ARect: TRect;
  Indic: TBitMap;
  TxtHeight: integer;
  Temp: real;
  ASelect: string;
  FillColor: TColor;
  AText: string;

  X, Y: integer;
begin
  if Creating then
    Exit;

  // Draw
  with Buffer do
    begin
      // Paint background
      Pen.Style := psClear;
      Brush.Style := bsSolid;
      Brush.Color := FDrawColors.Background;
      ARect := ClipRect;
      ARect.Inflate(1, 1);
      Rectangle(ARect);

      if Focused then
        FillColor := FEditColors.Press
      else
        FillColor := FEditColors.GetColor(InteractionState);

      // Fill
      GDIRoundRect(MakeRoundRect(DrawRect, Roundness),
        GetRGB(FillColor).MakeGDIBrush, nil);

      // Line
      GDIRoundRect(MakeRoundRect(LineRect, LineSize), GetRGB(FLineColor).MakeGDIBrush, nil);

      // Outline
      GDIRoundRect(MakeRoundRect(DrawRect, Roundness),
        nil, GetRGB(ChangeColorLight(FDrawColors.BackGround, EDIT_BORDER_FADE) ).MakeGDIPen(1));

      // Selection

      // Text
      AText := DrawText;
      Font.Assign(Self.Font);
      Font.Color := Self.Font.Color;

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

procedure FXEdit.PasteFromClipBoard;
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

procedure FXEdit.PopupBeforePopup(Sender: TObject; var CanPopup: boolean;
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
end;

procedure FXEdit.PopupItemClick(Sender: TObject; Item: FXPopupComponent;
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

procedure FXEdit.PrepDefaultMenu;
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

procedure FXEdit.Resize;
begin
  inherited;
  UpdateRects;
end;

procedure FXEdit.ScrollForCursor;
begin
  ScrollForCursor(FDrawPosition);
end;

procedure FXEdit.ScrollForCursor(ADrawPosition: integer);
begin
  if ADrawPosition < 0 then
    begin
      FCutPosition := FCutPosition + ADrawPosition(*negative*) - LineSize * 2;
    end;

  if ADrawPosition > TxtRect.Width then
    begin
      FCutPosition := FCutPosition + ADrawPosition-TxtRect.Width + LineSize * 2;
    end;

  if FCutPosition < 0 then
    FCutPosition := 0;

  // Update
  UpdateDrawPosition;
end;

function FXEdit.SearchPosition(AX: integer): integer;
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

procedure FXEdit.SelectAll;
begin
  FPosition := 0;
  SelectionLength := TextLength;
end;

function FXEdit.Selection: string;
begin
  Result := Copy(DrawText, FPosition+1, FSelLength);
end;

function FXEdit.SelectionEnd: integer;
begin
  if FSelGoesLeft then
    Result := Position
  else
    Result := Position + SelectionLength;
end;

function FXEdit.SelectionStart: integer;
begin
  if FSelGoesLeft then
    Result := Position+SelectionLength
  else
    Result := Position;
end;

procedure FXEdit.SelectPoints(P1, P2: integer);
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

procedure FXEdit.SetAutoSizing(const Value: boolean);
begin
  if FAutoSize <> Value then
    begin
      FAutoSize := Value;

      // Auto Size
      UpdateAutoSize;

      UpdateRects;
      PaintBuffer;
    end;
end;

procedure FXEdit.SetCanUndo(const Value: boolean);
begin
  if FCanUndo <> Value then
    begin
      FCanUndo := Value;

      if not Value then
        FHistory.Clear;
    end;
end;

procedure FXEdit.SetCharCase(const Value: FXCharCase);
begin
  if FCharCase <> Value then
    begin
      FCharCase := Value;

      ApplyCharCase;

      Invalidate;
    end;
end;

procedure FXEdit.SetEnableSelection(const Value: boolean);
begin
  if FEnableSelection <> Value then
    begin
      FEnableSelection := Value;

      if SelectionLength <> 0 then
        SelectionLength := 0;
    end;
end;

procedure FXEdit.SetLayout(const Value: FXLayout);
begin
  if FLayout <> Value then
    begin
      FLayout := Value;

      UpdateRects;
      Invalidate;
    end;
end;

procedure FXEdit.SetLineSize(const Value: integer);
begin
  if FLineSize <> Value then
    begin
      FLineSize := Value;

      UpdateRects;
      PaintBuffer;
    end;
end;

procedure FXEdit.SetNumbersOnly(const Value: boolean);
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

procedure FXEdit.SetPasswordChar(const Value: char);
begin
  if FPassChar <> Value then
    begin
      FPassChar := Value;

      UpdateRects;
      Invalidate;
    end;
end;

procedure FXEdit.SetPosition(const Value: integer);
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

procedure FXEdit.SetRoundness(const Value: integer);
begin
  if FRoundness <> Value then
    begin
      FRoundness := Value;

      PaintBuffer;
    end;
end;

procedure FXEdit.SetSelLength(const Value: integer);
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

procedure FXEdit.SetText(const Value: string);
begin
  FText := Value;
  FHistory.Clear;

  ApplyCharCase;

  // Update
  UpdateRects;
  PaintBuffer;
end;

procedure FXEdit.SetTextMargin(const Value: integer);
begin
  if FTextMargin <> Value then
    begin
      FTextMargin := Value;

      UpdateAutoSize;
      Invalidate;
    end;
end;

procedure FXEdit.SetValue(const Value: int64);
begin
  if not IsReading then
    Text := Value.ToString;
end;

function FXEdit.TextH(AText: string): integer;
begin
  Result := 0;
  if Parent <> nil then
    begin
      Canvas.Font.Assign( Self.Font );
      Result := Canvas.TextHeight(AText);
    end;
end;

function FXEdit.TextW(AText: string): integer;
begin
  Result := 0;
  if Parent <> nil then
    begin
      Canvas.Font.Assign( Self.Font );
      Result := Canvas.TextWidth(AText);
    end;
end;

procedure FXEdit.WMSize(var Message: TWMSize);
begin
  UpdateRects;
  Invalidate;
end;

procedure FXEdit.WM_LButtonDown(var Msg: TWMMouse);
var
  PosX, Value: integer;
begin
  ClearSelection;

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

end.
