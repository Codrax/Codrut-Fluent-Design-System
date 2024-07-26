unit CFX.Controls;

interface

uses
  Winapi.Windows, Vcl.Graphics, Classes, Types, Winapi.Messages, CFX.Types,
  CFX.Constants, SysUtils, CFX.Graphics, CFX.VarHelpers, CFX.ThemeManager,
  Vcl.Controls, CFX.Linker, Vcl.Forms, CFX.PopupMenu,
  Vcl.Dialogs, CFX.Classes, Math, DateUtils, CFX.ArrayHelpers, CFX.Messages;

type
  // Canvas-Based Control
  FXCustomControl = class(TCustomControl)
  protected
    property Canvas;
  end;

  // Custom classes
  FXControlSize = class(FXPointGeneric)
  private
    FParent: TControl;

  protected
    // Getters
    function GetX: integer; override;
    function GetY: integer; override;

    // Setters
    procedure SetX(const Value: integer); override;
    procedure SetY(const Value: integer); override;

  published
    property X;
    property Y;

    constructor Create(Control: TControl);
  end;

  FXControlPosition = class(FXPointGeneric)
  private
    FParent: TControl;

  protected
    // Getters
    function GetX: integer; override;
    function GetY: integer; override;

    // Setters
    procedure SetX(const Value: integer); override;
    procedure SetY(const Value: integer); override;

  published
    property X;
    property Y;

    property Point;

    constructor Create(Control: TControl);
  end;

  // Control
  FXWindowsControl = class(FXCustomControl)
  private
    FPopupMenu: FXPopupMenu;
    FBufferedComponent: boolean;
    FFocusRect: TRect;
    FAutoFocusLine: boolean;
    FHasEnteredTab: boolean;
    FInteraction: FXControlState;
    FPreviousInteraction: FXControlState;
    FCreated: boolean;
    FOpacity: FXPercent;
    FBackground: TBitMap;
    FOnPaint: FXControlOnPaint;
    FOnPaintBuffer: FXControlOnPaint;
    FTextFont: TFont;
    FFocusFlags: FXFocusFlags;
    FHitTest: boolean;
    FTransparent: boolean;

    FPadding: FXPadding;
    FMargins: FXMargins;

    FSize: FXControlSize;
    FPosition: FXControlPosition;

    LastDraw: TTime;

    FBuffer1,
    FBuffer2: TBitMap;
    BufferSecondary: boolean;

    // Events
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;

    // Data
    procedure ResizeBuffer;
    function GetBuffer: TCanvas;
    function CanDrawFocusLine: boolean;

    // Draw
    procedure SolidifyBuffer;

    procedure SwitchBuffer;
    function GetActiveBuffer: TBitMap;
    function GetCacheBuffer: TBitMap;

    // Object Notify Events
    procedure FontNotifyUpdate(Sender: TObject);

    // Set
    procedure SetState(const Value: FXControlState);
    procedure SetTransparent(const Value: boolean);
    procedure SetOpacity(const Value: FXPercent);
    procedure SetPosition(const Value: FXControlPosition);
    procedure SetSize(const Value: FXControlSize);

  protected
    // Messages
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMMove(var Message: TWMMove); message WM_MOVE;

    procedure QuickBroadcast(MessageID: integer);

    // Handle messages
    procedure WndProc(var Message: TMessage); override;

    // Size
    procedure Resize; override;
    procedure ApplyPadding; virtual;

    // Paint
    procedure Paint; override;
    procedure DoPaint; virtual;
    procedure PaintBuffer; virtual;

    function GetInheritedOpacity: FXPercent;

    // Props
    property HitTest: boolean read FHitTest write FHitTest default true;

    property BufferedComponent: boolean read FBufferedComponent write FBufferedComponent;

    property OnPaint: FXControlOnPaint read FOnPaint write FOnPaint;
    property OnPaintBuffer: FXControlOnPaint read FOnPaintBuffer write FOnPaintBuffer;

    // Background
    procedure DrawBackground(var Background: TBitMap; OnlyFill: boolean); virtual;
    procedure PaintBackground(OnlyFill: boolean=false);
    function GetBackground: TCanvas;

    // Virtual Events
    procedure ComponentCreated; virtual;
    procedure UpdateFocusRect; virtual;
    procedure FontUpdate; virtual;
    procedure OpenPopupMenu(X, Y: integer); virtual;
    procedure ScaleChanged(Scaler: single); virtual;
    procedure HandleKeyDown(var CanHandle: boolean; Key: integer; ShiftState: TShiftState); virtual;

    // Focus Line and Events
    procedure DoEnter; override;
    procedure DoExit; override;

    // Size
    function GetClientRect: TRect; override;

    // Created
    procedure CreateWnd; override;
    procedure Loaded; override;

    // Visible Change
    procedure OnVisibleChange(var Message : TMessage); message CM_VISIBLECHANGED;

    // Events
    procedure CMMouseEnter(var Message : TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CNKeyDown(var Message: TWMKeyDown); message CN_KEYDOWN;

    procedure MouseUp(Button : TMouseButton; Shift: TShiftState; X, Y : integer); override;
    procedure MouseDown(Button : TMouseButton; Shift: TShiftState; X, Y : integer); override;

    procedure PaddingUpdated(Sender: TObject);
    procedure MarginsUpdated(Sender: TObject);

    // Interaction
    procedure InteractionStateChanged(AState: FXControlState); virtual;
    procedure SetNewInteractionState(AState: FXControlState; ForceUpdate: boolean = false; UpdatePrevious: boolean = true);

    // Utilities
    function IsReading: boolean;
    function IsDesigning: boolean;
    function IsDestroying: boolean;
    //function Creating: boolean;
    function Destroyed: boolean;

    // Component redraw utilities
    procedure RedrawAndSortControls(AControls: TArray<TControl>);
    procedure SortControlsByIndex(var AControls: TArray<TControl>);

    // Padding
    property PaddingFill: FXPadding read FPadding write FPadding;

    // Catch Events
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;

    // Scaling
    procedure ChangeScale(M, D: Integer{$IF CompilerVersion > 29}; isDpiChange: Boolean{$ENDIF}); override;

    // Properties
    property Color;

    property FocusRect: TRect read FFocusRect write FFocusRect;
    property AutoFocusLine: boolean read FAutoFocusLine write FAutoFocusLine;

    property FocusFlags: FXFocusFlags read FFocusFlags write FFocusFlags default [];

    property PreviousInteractionState: FXControlState read FPreviousInteraction write FPreviousInteraction;

    property Font: TFont read FTextFont write FTextFont;

  published
    property Transparent: boolean read FTransparent write SetTransparent default true;
    property Opacity: FXPercent read FOpacity write SetOpacity;

    // Popup Menu
    property PopupMenu: FXPopupMenu read FPopupMenu write FPopupMenu;

    // Client
    property MarginsFill: FXMargins read FMargins write FMargins;

    // Defaults
    property Hint;

    property TabStop default true;

    property Size: FXControlSize read FSize write SetSize;
    property Position: FXControlPosition read FPosition write SetPosition;

    property AlignWithMargins default true;
    property Enabled;
    property Visible;
    property Tag;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Override
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

    // Drawing
    procedure DrawTo(ACanvas: TCanvas; Destination: TRect); overload;
    procedure DrawTo(Client: TRect; ACanvas: TCanvas; Destination: TRect); overload;

    // State
    property InteractionState: FXControlState read FInteraction write SetState;

    // Buffer
    property Buffer: TCanvas read GetBuffer;

    // Parent Utilities
    function GetParentBackgroundColor(Default: TColor): TColor;

    // Invalidate
    procedure Invalidate; override;

    // Controls
    /// <summary> Return all controls with the same parent above this one. </summary>
    function GetControlsAbove: TArray<TControl>;
    function GetChildControls: TArray<TControl>;

    // Redraw
    procedure Redraw(RedrawAbove: boolean=true);
    procedure RedrawChildren;
    procedure RedrawControlsAbove;
  end;

// Utilities
function GetParentBackgroundColorEx(Control: TControl; Default: TColor): TColor;

implementation

function GetParentBackgroundColorEx(Control: TControl; Default: TColor): TColor;
begin
  if (Control.Parent <> nil) and Supports(Control.Parent, FXControl) then
    Result := (Control.Parent as FXControl).Background
      else
        Result := Default;
end;

procedure FXWindowsControl.ApplyPadding;
begin
  Resize;
end;

{ FXTransparentControl }

function FXWindowsControl.CanDrawFocusLine: boolean;
begin
  Result := AutoFocusLine and Focused and FHasEnteredTab and not IsDesigning;
end;

procedure FXWindowsControl.ChangeScale(M, D: Integer{$IF CompilerVersion > 29}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  ScaleChanged( M / D );
  Invalidate;
end;

procedure FXWindowsControl.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  SetState( FXControlState.None );
  if BufferedComponent and Supports(Self, FXControl) then
    begin
      (Self as FXControl).UpdateTheme(false);
      Invalidate;
    end;
end;

procedure FXWindowsControl.CMMouseEnter(var Message: TMessage);
begin
  SetState( FXControlState.Hover );

  if Assigned(OnMouseEnter) then
    OnMouseenter(Self);
end;

procedure FXWindowsControl.CMMouseLeave(var Message: TMessage);
begin
  SetState( FXControlState.None );

  if Assigned(OnMouseLeave) then
    OnMouseLeave(Self);
end;

procedure FXWindowsControl.CNKeyDown(var Message: TWMKeyDown);
var
  CanContinue: boolean;
begin
  CanContinue := true;

  HandleKeyDown(CanContinue, Message.CharCode, KeyDataToShiftState(Message.KeyData));

  if CanContinue then
    inherited;
end;

procedure FXWindowsControl.ComponentCreated;
begin
  // nothing
end;

constructor FXWindowsControl.Create(AOwner: TComponent);
begin
  inherited;
  // Component Defaults
  FCreated := false;
  FBufferedComponent := true;
  FAutoFocusLine := false;
  FOpacity := 100;
  FTransparent := true;
  FHitTest := true;

  AlignWithMargins := true;
  Margins.Left := 0;
  Margins.Top := 0;
  Margins.Right := 0;
  Margins.Bottom := 0;

  FPadding := FXPadding.Create(Self);
  FPadding.OnChange := PaddingUpdated;
  FMargins := FXMargins.Create(Self);
  FMargins.OnChange := MarginsUpdated;

  // Font
  FTextFont := TFont.Create;
  FTextFont.Name := FORM_FONT_NAME;
  FTextFont.Height := ThemeManager.FormFontHeight;
  FTextFont.OnChange := FontNotifyUpdate;

  // Navigation
  TabStop := true;
  ParentColor := false;

  // Style
  ControlStyle := ControlStyle + [csOpaque, csCaptureMouse];
  Brush.Style := bsClear;

  FSize := FXControlSize.Create(Self);
  FPosition := FXControlPosition.Create(Self);

  // Initialise Buffer
  FBuffer1 := TBitMap.Create;
  FBuffer2 := TBitMap.Create;
  FBackground := TBitMap.Create;

  SwitchBuffer;
  ResizeBuffer;
  SwitchBuffer;
  ResizeBuffer;
end;

procedure FXWindowsControl.CreateWnd;
begin
  FCreated := true;
  inherited;

  // Notify
  ComponentCreated;
end;

destructor FXWindowsControl.Destroy;
begin
  FreeAndNil(FTextFont);

  FreeAndNil(FPadding);
  FreeAndNil(FMargins);

  FreeAndNil(FSize);
  FreeAndNil(FPosition);

  FreeAndNil(FBuffer1);
  FreeAndNil(FBuffer2);
  FreeAndNil(FBackground);
  inherited;
end;

function FXWindowsControl.Destroyed: boolean;
begin
  Result := IsDestroying or (Self = nil) or (Self.Parent = nil);
end;

procedure FXWindowsControl.DoEnter;
begin
  inherited;
  if AutoFocusLine and (InteractionState <> FXControlState.Press) then
    begin
      FHasEnteredTab := true;
      Redraw;
    end;
end;

procedure FXWindowsControl.DoExit;
begin
  inherited;
  if AutoFocusLine then
    begin
      FHasEnteredTab := false;
      Redraw;
    end;
end;

procedure FXWindowsControl.DoPaint;
var
  Composite: TBitMap;
  ARect: TRect;
begin
  if BufferedComponent then
    begin
      // Background
      const AOpacity = GetInheritedOpacity.ToByte;
      if AOpacity <> 255 then
        begin
          Composite := TBitMap.Create(Width, Height);
          with Composite.Canvas do
            try
              Draw(0, 0, FBackground);
              Draw(0, 0, GetCacheBuffer, AOpacity);

              // Copy to screen
              Canvas.Draw(0, 0, Composite);
            finally
              Composite.Free;
            end;
        end
      else
        // Draw Buffer
        Canvas.Draw(0, 0, GetCacheBuffer, AOpacity);
    end;

  // Focus Line
  if CanDrawFocusLine then
    begin
      UpdateFocusRect;

      // Offset by width
      ARect := FocusRect;
      ARect.Width := ARect.Width+FOCUS_LINE_SIZE div 2;
      ARect.Height := ARect.Height+FOCUS_LINE_SIZE div 2;

      Canvas.GDIRoundRect(MakeRoundRect(ARect, FOCUS_LINE_ROUND, FOCUS_LINE_ROUND),
        nil,
        GetRGB(ThemeManager.SystemColor.ForeGround).MakeGDIPen(FOCUS_LINE_SIZE))
    end;

  // Notify
  if Assigned(FOnPaint) then
    FOnPaint(Self);
end;

procedure FXWindowsControl.DrawBackground(var Background: TBitMap; OnlyFill: boolean);
var
  FControl: FXWindowsControl;
  I: integer;
  Intersection: TRect;
  ControlBounds: TRect;
  Local,
  Host: TRect;
begin
  if IsReading then
    Exit;

  // Draw Background
  with Background.Canvas do
    begin
      // Solid color background
      Brush.Color := Color;
      FillRect(ClipRect);

      // Opaque
      if OnlyFill or not Transparent then
        Exit;

      // Copy parent data
      if Parent is FXWindowsControl then begin
        FControl := Parent as FXWindowsControl;

        // Bounds
        ControlBounds := FControl.ClientRect; // It's the client!

        // Intersects 100%
        Intersection := TRect.Intersect(ControlBounds, BoundsRect);

        Local := Intersection;
        Local.Offset(-BoundsRect.Left, -BoundsRect.Top);

        Host := Intersection;
        Host.Offset(-ControlBounds.Left, -ControlBounds.Top);

        // Copy colliding
        CopyRect(Local, FControl.Buffer, Host);
      end;

      // Copy child controls
      for I := 0 to Parent.ControlCount-1 do
        if (Parent.Controls[I] <> Self) and (Parent.Controls[I] is FXWindowsControl) then begin
          FControl := FXWindowsControl(Parent.Controls[I]);

          // Hidden or is Self
          if not FControl.Visible or (FControl = Self) then
            Continue;

          // Bounds
          ControlBounds := FControl.BoundsRect;

          // Intersect
          if not ControlBounds.IntersectsWith(BoundsRect) then
            Continue;
        
          // Check behind
          if (FControl.ComponentIndex >= ComponentIndex) then
            Continue;

          // Intersect
          Intersection := TRect.Intersect(ControlBounds, BoundsRect);

          Local := Intersection;
          Local.Offset(-BoundsRect.Left, -BoundsRect.Top);

          Host := Intersection;
          Host.Offset(-ControlBounds.Left, -ControlBounds.Top);

          // Copy colliding
          FControl.DrawTo(Host, Background.Canvas, Local);
        end;

      // Opacity support
      { this will ensure that the background is already drawn when the
        composite background is drawn on top }
      if Opacity <> 255 then
        Self.Canvas.Draw(0, 0, GetCacheBuffer, 255);
    end;
end;

procedure FXWindowsControl.DrawTo(Client: TRect; ACanvas: TCanvas;
  Destination: TRect);
var
  FControl: FXWindowsControl;

  Local: TRect;
  TranslateDest: TRect;
begin
  const SelfClient = ClientRect;

  // Draw Self
  const AOpacity = GetInheritedOpacity.ToByte;
  CFX.Graphics.CopyRectWithOpacity(ACanvas, Destination, GetCacheBuffer.Canvas, Client, AOpacity);

  // Draw children
  for var I := 0 to ControlCount-1 do begin
    if not (Controls[I] is FXWindowsControl) then
      continue;

    // Get
    FControl := Controls[I] as FXWindowsControl;
    Local := FControl.BoundsRect;

    // Hidden
    if not FControl.Visible then
      continue;

    // Out of bounds
    if not Local.IntersectsWith(Client) then
      Continue;

    // Intersect
    //Local.Intersect(Client);
    if Local.Left < SelfClient.Left then
      Local.Left := SelfClient.Left;
    if Local.Top < SelfClient.Top then
      Local.Top := SelfClient.Top;
    if Local.Right > SelfClient.Right then
      Local.Right := SelfClient.Right;
    if Local.Bottom > SelfClient.Bottom then
      Local.Bottom := SelfClient.Bottom;

    // Translate dest
    TranslateDest := TranslateRect(Local, Client, Destination);

    // Translate local
    Local.Offset(-Local.Left, -Local.Top);

    // Draw
    FControl.DrawTo( Local, ACanvas, TranslateDest );
  end;
end;

procedure FXWindowsControl.DrawTo(ACanvas: TCanvas; Destination: TRect);
begin
  DrawTo(ClientRect, ACanvas, Destination);
end;

procedure FXWindowsControl.FontNotifyUpdate(Sender: TObject);
begin
  FontUpdate;
end;

procedure FXWindowsControl.FontUpdate;
begin
  // Update
  Redraw;
end;

function FXWindowsControl.GetActiveBuffer: TBitMap;
begin
  if BufferSecondary then
    Result := FBuffer2
  else
    Result := FBuffer1;
end;

function FXWindowsControl.GetBackground: TCanvas;
begin
  Result := FBackground.Canvas;
end;

function FXWindowsControl.GetBuffer: TCanvas;
begin
  Result := GetActiveBuffer.Canvas;
end;

function FXWindowsControl.GetCacheBuffer: TBitMap;
begin
  if BufferSecondary then
    Result := FBuffer1
  else
    Result := FBuffer2;
end;

function FXWindowsControl.GetChildControls: TArray<TControl>;
begin
  SetLength(Result, ControlCount);
  for var I := 0 to ControlCount-1 do
    Result[I] := Controls[I];
end;

function FXWindowsControl.GetClientRect: TRect;
begin
  // Apply padding
  Result := FPadding.RectangleInflate( Rect(0, 0, Width, Height) );
end;

function FXWindowsControl.GetControlsAbove: TArray<TControl>;
var
  I: Integer;
  FControl: TControl;
begin
  Result := [];

  if Parent = nil then
    Exit;

  for I := 0 to Parent.ControlCount-1 do
    if (Parent.Controls[I] <> Self) and (Parent.Controls[I] is FXWindowsControl) then begin
      FControl := FXWindowsControl(Parent.Controls[I]);

      if FControl.ComponentIndex > ComponentIndex then
        if FControl.BoundsRect.IntersectsWith(BoundsRect) then
          TArrayUtils<TControl>.AddValue(FControl, Result);
    end;
end;

function FXWindowsControl.GetInheritedOpacity: FXPercent;
begin
  Result := FOpacity;
  if not (Parent is FXWindowsControl)  then
    Exit;

  const AParent = (Parent as FXWindowsControl);

  if AParent.Transparent then
    // Use It's inherited transparent
    Result := (Parent as FXWindowsControl).GetInheritedOpacity.Percentage * FOpacity
  else
    // Ute It's static transparency for controls
    Result := (Parent as FXWindowsControl).Opacity.Percentage * FOpacity
end;

function FXWindowsControl.GetParentBackgroundColor(Default: TColor): TColor;
begin
  Result := GetParentBackgroundColorEx(Self, Default);
end;

procedure FXWindowsControl.HandleKeyDown(var CanHandle: boolean; Key: integer;
  ShiftState: TShiftState);
begin
  // Handle options // or nothing
  if FFocusFlags <> [] then
    case Key of
      VK_TAB: if FXFocusFlag.CatchTab in FFocusFlags then
        CanHandle := false;
      VK_LEFT: if FXFocusFlag.CatchLeft in FFocusFlags then
        CanHandle := false;
      VK_UP: if FXFocusFlag.CatchUp in FFocusFlags then
        CanHandle := false;
      VK_RIGHT: if FXFocusFlag.CatchRight in FFocusFlags then
        CanHandle := false;
      VK_DOWN: if FXFocusFlag.CatchDown in FFocusFlags then
        CanHandle := false;
    end;
end;

procedure FXWindowsControl.InteractionStateChanged(AState: FXControlState);
begin
  Redraw;
end;

procedure FXWindowsControl.Invalidate;
begin
  if IsReading then
    Exit;

  Redraw;

  inherited;
end;

function FXWindowsControl.IsDesigning: boolean;
begin
  Result := csDesigning in ComponentState;
end;

function FXWindowsControl.IsDestroying: boolean;
begin
  Result := csDestroying in ComponentState;
end;

function FXWindowsControl.IsReading: boolean;
begin
  Result := csReading in ComponentState;
end;

procedure FXWindowsControl.Loaded;
begin
  inherited;
  FCreated := true;
  Redraw;
end;

procedure FXWindowsControl.MarginsUpdated(Sender: TObject);
begin
  Margins.SetBounds(FMargins.AbsoluteLeft, FMargins.AbsoluteTop, FMargins.AbsoluteRight, FMargins.AbsoluteBottom);
end;

procedure FXWindowsControl.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  inherited;
  // State
  if (InteractionState = FXControlState.Hover) and (Button = mbLeft) then
    SetState( FXControlState.Press );

  // Focus
  if (InteractionState = FXControlState.Press) and CanFocus and not Focused then
    SetFocus;

  // Entered
  if FHasEnteredTab then
    begin
      FHasEnteredTab := false;
      if AutoFocusLine then
        Redraw;
    end;
end;

procedure FXWindowsControl.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  inherited;
  if InteractionState = FXControlState.Press then
    SetState( FXControlState.Hover );

  // Popup Menu
  if (Button = mbRight) then
    OpenPopupMenu(X, Y);
end;

procedure FXWindowsControl.OnVisibleChange(var Message: TMessage);
begin
  inherited;

  if Visible and not IsDesigning then
    Redraw;
end;

procedure FXWindowsControl.OpenPopupMenu(X, Y: integer);
begin
  if Assigned(PopupMenu) then
    FPopupMenu.PopupAtPoint( ClientToScreen(Point(X,Y)) );
end;

procedure FXWindowsControl.PaddingUpdated(Sender: TObject);
begin
  Realign;

  ApplyPadding;

  Redraw;
end;

procedure FXWindowsControl.Paint;
begin
  inherited;
  DoPaint;
end;

procedure FXWindowsControl.PaintBackground(OnlyFill: boolean);
begin
  DrawBackground(FBackground, OnlyFill);
  GetActiveBuffer.Canvas.Draw(0, 0, FBackground);
end;

procedure FXWindowsControl.PaintBuffer;
begin
  // Paint
  if Assigned(FOnPaintBuffer) then
    FOnPaintBuffer(Self);
end;

procedure FXWindowsControl.QuickBroadcast(MessageID: integer);
var
  AMsg: TMessage;
begin
  AMsg.Msg := MessageID;
  AMsg.WParam := 0;
  AMsg.LParam := LongInt(Self);
  AMsg.Result := 0;

  Broadcast(AMsg);
end;

procedure FXWindowsControl.Redraw(RedrawAbove: boolean);
begin
  if BufferedComponent and (Parent <> nil) and not IsReading and FCreated then begin
    // Draw
    ResizeBuffer;
    SolidifyBuffer;
    PaintBuffer;

    // Switch to new buffer
    SwitchBuffer;

    // Draw new buffer
    DoPaint;

    // Draw children (always need to be redraw)
    RedrawChildren;

    // Draw controls above
    if RedrawAbove then
      RedrawControlsAbove;

    LastDraw := Now;
  end;
end;

procedure FXWindowsControl.RedrawAndSortControls(AControls: TArray<TControl>);
var
  I: integer;
begin
  // Sort by index, so all control are updated by Z-Index
  SortControlsByIndex(AControls);

  // Redraw all
  for I := 0 to High(AControls) do
    if (AControls[I] is FXWindowsControl) then
      with AControls[I] as FXWindowsControl do
        if Transparent and (Visible or IsDesigning) then
          Redraw(false); // do not start another redrawing session
          ///  Why is another redrawing session started even if
          ///  control bounds can affect out of region controls?
          ///
          ///  While that is true, currently NO controls except maybe the FXEffect
          ///  control does that, and the performance benefits exceed the
          ///  utility for such a rare use case. In the future, a flag to test
          ///  this may be added.
end;

procedure FXWindowsControl.RedrawChildren;
begin
  RedrawAndSortControls( GetChildControls );
end;

procedure FXWindowsControl.RedrawControlsAbove;
begin
  // Notify neighbours
  RedrawAndSortControls( GetControlsAbove );
end;

procedure FXWindowsControl.Resize;
begin
  inherited;
end;

procedure FXWindowsControl.ResizeBuffer;
begin
  if BufferedComponent then
    begin
      const AWidth = Max(Width, 0);
      const AHeight = Max(Height, 0);

      if (GetActiveBuffer.Width <> AWidth) or (GetActiveBuffer.Height <> AHeight) then
        begin
          GetActiveBuffer.SetSize(AWidth, AHeight);
          FBackground.SetSize(AWidth, AHeight);
        end;
    end;
end;

procedure FXWindowsControl.ScaleChanged(Scaler: single);
begin
  FTextFont.Height := round(FTextFont.Height * Scaler);

  FPadding.ScaleChanged(Scaler);
  FMargins.ScaleChanged(Scaler);
end;

procedure FXWindowsControl.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if not IsReading then begin
    // Get controls before bounds change
    const AControls = GetControlsAbove;

    // Execute bounds change
    inherited;

    // Notify previous neighbours
    RedrawAndSortControls(AControls);
  end else
    inherited;
end;

procedure FXWindowsControl.SetNewInteractionState(AState: FXControlState;
  ForceUpdate, UpdatePrevious: boolean);
begin
  if (AState <> FInteraction) or ForceUpdate then
    begin
      if UpdatePrevious then
        FPreviousInteraction := FInteraction;
      FInteraction := AState;

      InteractionStateChanged(AState);
    end;
end;

procedure FXWindowsControl.SetOpacity(const Value: FXPercent);
begin
  if (FOpacity <> Value) and InRange(Value, 0, 100) then
    begin
      FOpacity := Value;

      // Draw
      Invalidate;
    end;
end;

procedure FXWindowsControl.SetPosition(const Value: FXControlPosition);
begin
  FPosition.Point := Value.Point;
end;

procedure FXWindowsControl.SetSize(const Value: FXControlSize);
begin
  FSize.Point := Value.Point;
end;

procedure FXWindowsControl.SetState(const Value: FXControlState);
begin
  SetNewInteractionState(Value);
end;

procedure FXWindowsControl.SetTransparent(const Value: boolean);
begin
  if Value = FTransparent then
    Exit;

  FTransparent := Value;
  Redraw;
end;

procedure FXWindowsControl.SolidifyBuffer;
begin
  with GetActiveBuffer.Canvas do
    begin
      // Reset Color
      Brush.Color := Color;

      // Clear
      FillRect(ClipRect);
    end;
end;

procedure FXWindowsControl.SortControlsByIndex(var AControls: TArray<TControl>);
begin
  // Sort
  TArrayUtils<TControl>.Sort(AControls, function(A, B: TControl): boolean
    begin
      Result := A.ComponentIndex > B.ComponentIndex;
    end);
end;

procedure FXWindowsControl.SwitchBuffer;
begin
  BufferSecondary := not BufferSecondary;
end;

procedure FXWindowsControl.UpdateFocusRect;
begin
  FocusRect := Self.ClientRect;

  FFocusRect.Right := FocusRect.Right - FOCUS_LINE_SIZE;
  FFocusRect.Bottom := FocusRect.Bottom - FOCUS_LINE_SIZE;
end;

procedure FXWindowsControl.WMMove(var Message: TWMMove);
begin
  inherited;

  // Redraw at current position
  Redraw;
end;

procedure FXWindowsControl.WMNCHitTest(var Message: TWMNCHitTest);
begin
  if FHitTest or (csDesigning in ComponentState) then
    inherited
  else
    Message.Result := HTTRANSPARENT;
end;

procedure FXWindowsControl.WMSize(var Message: TWMSize);
begin
  inherited;

  // Redraw at current position
  Redraw;
end;

procedure FXWindowsControl.WndProc(var Message: TMessage);
begin
  inherited;
  if InRange(Message.Msg, WM_CFX_MESSAGES, WM_CFX_MESSAGES_END) then
    Broadcast( Message );
end;

{ FXControlSize }

constructor FXControlSize.Create(Control: TControl);
begin
  FParent := Control;
end;

function FXControlSize.GetX: integer;
begin
  Result := FParent.Width;
end;

function FXControlSize.GetY: integer;
begin
  Result := FParent.Height;
end;

procedure FXControlSize.SetX(const Value: integer);
begin
  inherited;
  FParent.Width := Value;
end;

procedure FXControlSize.SetY(const Value: integer);
begin
  inherited;
  FParent.Height := Value;
end;

{ FXControlPosition }

constructor FXControlPosition.Create(Control: TControl);
begin
  FParent := Control;
end;

function FXControlPosition.GetX: integer;
begin
  Result := FParent.Left;
end;

function FXControlPosition.GetY: integer;
begin
  Result := FParent.Top;
end;

procedure FXControlPosition.SetX(const Value: integer);
begin
  FParent.Left := Value;
end;

procedure FXControlPosition.SetY(const Value: integer);
begin
  FParent.Top := Value;
end;

end.
