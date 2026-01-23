unit CFX.Controls;

interface

uses
  Winapi.Windows,
  Vcl.Graphics,
  Classes,
  Types,
  Winapi.Messages,
  CFX.Types,
  CFX.Constants,
  SysUtils,
  CFX.Graphics,
  CFX.VarHelpers,
  CFX.ThemeManager,
  Vcl.Controls,
  CFX.Linker,
  Vcl.Forms,
  CFX.PopupMenu,
  Vcl.Dialogs,
  CFX.Classes,
  CFX.ComponentClasses,
  Math,
  DateUtils,
  CFX.ArrayHelpers,
  CFX.Messages;

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
  FXWindowsControl = class(FXCustomControl, IFXComponent, IFXControl)
  private
    FPopupMenu: FXPopupMenu;
    FBufferedComponent: boolean;
    FFocusRect: TRect;
    FAutoFocusLine: boolean;
    FHasEnteredTab: boolean;
    FInteraction: FXControlState;
    FPreviousInteraction: FXControlState;
    FCreated: boolean; // Handle (WND) created
    FOpacity: FXPercent;
    FBackground: TBitMap;
    FOnPaint: FXControlOnPaint;
    FOnPaintBuffer: FXControlOnPaint;
    FTextFont: TFont;
    FFocusFlags: FXFocusFlags;
    FHitTest: boolean;
    FTransparent: boolean;
    FDoubleClickInProgress: boolean;

    //FPadding: FXPadding;
    FInnerMargins: FXPadding;
    FMargins: FXMargins;
    FPadding: FXPadding;

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

    // Stored
    function IsOpacityStored: Boolean;

    // Get
    function GetLeft: integer;
    function GetTop: integer;
    function GetHeight: integer;
    function GetWidth: integer;
    function GetInvisibile: boolean;

    // Set
    procedure SetState(const Value: FXControlState);
    procedure SetTransparent(const Value: boolean);
    procedure SetOpacity(const Value: FXPercent);
    procedure SetPosition(const Value: FXControlPosition);
    procedure SetSize(const Value: FXControlSize);
    procedure SetLeft(const Value: integer);
    procedure SetTop(const Value: integer);
    procedure SetHeight(const Value: integer);
    procedure SetWidth(const Value: integer);
    procedure SetInvisibile(const Value: boolean);

  protected
    // Paint
    procedure Paint; override;
    procedure DoPaint; virtual;
    procedure PaintBuffer; virtual;

    // Internal
    function CanUpdate: boolean;
    procedure UpdateRects; virtual;
    procedure UpdateColors; virtual;

    // General standard updates to make code shorter (for properties)
    procedure StandardUpdateLayout;
    procedure StandardUpdateColor;
    procedure StandardUpdateDraw;
    procedure StandardUpdateComplete;

    // Align
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    function GetControlExtents: TRect; override;

    // Messages
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMMove(var Message: TWMMove); message WM_MOVE;

    procedure QuickBroadcast(MessageID: integer);

    // Handle messages
    procedure WndProc(var Message: TMessage); override;

    // Size
    procedure Resize; override;
    procedure Sized; virtual;
    procedure Moved; virtual;
    procedure ApplyInnerMargins; virtual;
    procedure ApplyPadding; virtual;
    procedure BoundsUpdated; virtual; // move, size or either one

    // Utils
    function GetInheritedOpacity: FXPercent;

    // Props
    property HitTest: boolean read FHitTest write FHitTest default true;

    property BufferedComponent: boolean read FBufferedComponent write FBufferedComponent;

    property OnPaint: FXControlOnPaint read FOnPaint write FOnPaint;
    property OnPaintBuffer: FXControlOnPaint read FOnPaintBuffer write FOnPaintBuffer;

    // Background
    procedure ClearBufferRegion(ARect: TRect);
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
    procedure HandleKeyUp(var CanHandle: boolean; Key: integer; ShiftState: TShiftState); virtual;

    // Focus Line and Events
    procedure FocusChanged(Focused: boolean); virtual;
    procedure DoEnter; override;
    procedure DoExit; override;

    // Size
    function GetAbsoluteRect: TRect;
    function GetClientRect: TRect; override;
    function GetContentRect: TRect; virtual;

    // Created
    procedure CreateWnd; override;
    procedure Loaded; override;

    // Visible Change
    procedure OnVisibleChange(var Message : TMessage); message CM_VISIBLECHANGED;

    // Events
    procedure CMMouseEnter(var Message : TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CNKeyDown(var Message: TWMKeyDown); message CN_KEYDOWN;
    procedure CNKeyUp(var Message: TWMKeyUp); message CN_KEYUP;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;

    procedure MouseUp(Button : TMouseButton; Shift: TShiftState; X, Y : integer); override;
    procedure MouseDown(Button : TMouseButton; Shift: TShiftState; X, Y : integer); override;
    procedure DblClick; override;

    procedure InnerMarginsUpdated(Sender: TObject);
    procedure MarginsUpdated(Sender: TObject);
    procedure PaddingUpdated(Sender: TObject);

    // Interaction
    procedure InteractionStateChanged(AState: FXControlState); virtual;
    procedure SetNewInteractionState(AState: FXControlState; ForceUpdate: boolean = false; UpdatePrevious: boolean = true);

    // Utilities
    function IsReading: boolean;
    function IsDesigning: boolean;
    function IsDestroying: boolean;
    function IsCreating: boolean;
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

    property DoubleClickInProgress: boolean read FDoubleClickInProgress;

    property FocusRect: TRect read FFocusRect write FFocusRect;
    property AutoFocusLine: boolean read FAutoFocusLine write FAutoFocusLine;

    property FocusFlags: FXFocusFlags read FFocusFlags write FFocusFlags default [];

    property PreviousInteractionState: FXControlState read FPreviousInteraction write FPreviousInteraction;

    property Font: TFont read FTextFont write FTextFont;

  published
    property Transparent: boolean read FTransparent write SetTransparent default true;
    property Invisible: boolean read GetInvisibile write SetInvisibile stored true default false; // helps FOpacity
    property Opacity: FXPercent read FOpacity write SetOpacity stored IsOpacityStored;

    // System
    property AbsoluteRect: TRect read GetAbsoluteRect;
    property ClientRect;
    property ContentRect: TRect read GetContentRect;

    // Popup Menu
    property PopupMenu: FXPopupMenu read FPopupMenu write FPopupMenu;

    // Client
    property InnerMarginsFill: FXPadding read FInnerMargins write FInnerMargins;
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

    property Left: integer read GetLeft write SetLeft;
    property Top: integer read GetTop write SetTop;
    property Width: integer read GetWidth write SetWidth;
    property Height: integer read GetHeight write SetHeight;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Override
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure Inflate(ALeft, ATop, AWidth, AHeight: Integer);

    // Drawing
    procedure DrawTo(ACanvas: TCanvas; Destination: TRect); overload;
    procedure DrawTo(Client: TRect; ACanvas: TCanvas; Destination: TRect); overload;

    // State
    property InteractionState: FXControlState read FInteraction write SetState;

    // Buffer
    property Buffer: TCanvas read GetBuffer;

    // Utilities
    procedure CenterInParent;

    // Parent Utilities
    function GetParentBackgroundColor(Default: TColor): TColor;

    // Invalidate
    procedure Invalidate; override;

    // Utility
    procedure DisableAllAnimations; virtual;

    // Controls
    /// <summary> Return all controls with the same parent above this one. </summary>
    function GetControlsAbove: TArray<TControl>;
    function GetChildControls: TArray<TControl>;
    function GetChildControlsRecursively: TArray<TControl>;

    // Components
    function GetChildComponents: TArray<TComponent>;

    // Handles
    procedure AllocateHandles;

    // Redraw
    procedure Redraw; overload;
    procedure Redraw(RedrawAbove: boolean); overload; virtual;

    procedure Redraw(Flags: FXRedrawFlags); overload; virtual;
    procedure RedrawChildren; virtual;
    procedure RedrawControlsAbove; virtual;

    // Interface
    function IsContainer: Boolean; virtual;
    function Background: TColor; virtual;

    procedure UpdateTheme(const UpdateChildren: Boolean); virtual;
  end;

  /// CONTAINER
  FXContainerWindowsControl = class(FXWindowsControl)
  published
    property PaddingFill;
    property TabStop default false;

  public
    // Draw
    procedure Redraw(RedrawAbove: boolean); overload; override;

    // Constructors
    constructor Create(aOwner: TComponent); override;
  end;

// Utilities
function GetParentBackgroundColorEx(Control: TControl; Default: TColor): TColor;

// Const
const
  DEFAULT_CONTROL_REDRAW_FLAGS = [FXRedrawFlag.RedrawBuffer, FXRedrawFlag.Paint,
    FXRedrawFlag.UpdateChildren, FXRedrawFlag.UpdateAbove];

implementation

function GetParentBackgroundColorEx(Control: TControl; Default: TColor): TColor;
begin
  if (Control.Parent <> nil) and Supports(Control.Parent, IFXControl) then
    Result := (Control.Parent as IFXControl).Background
      else
        Result := Default;
end;

procedure FXWindowsControl.AlignControls(AControl: TControl; var Rect: TRect);
begin
  // Adjust align rectange
  Rect := FPadding.RectangleInflate(Rect);

  // Inherit
  inherited;
end;

procedure FXWindowsControl.AllocateHandles;
begin
  HandleNeeded; // allocate self

  // Allocate children
  const AControls = GetChildControls;
  for var I := 0 to High(AControls) do
    if (AControls[I] is FXWindowsControl) then
      (AControls[I] as FXWindowsControl).AllocateHandles;
end;

procedure FXWindowsControl.ApplyInnerMargins;
begin
  Resize;
  UpdateRects;

  // Draw
  StandardUpdateDraw;
end;

procedure FXWindowsControl.ApplyPadding;
begin
  Resize;
  UpdateRects;

  // Draw
  StandardUpdateDraw;
end;

function FXWindowsControl.Background: TColor;
begin
  Result := Color;
end;

procedure FXWindowsControl.BoundsUpdated;
begin
  Redraw;
end;

{ FXTransparentControl }

function FXWindowsControl.CanDrawFocusLine: boolean;
begin
  Result := AutoFocusLine and Focused and FHasEnteredTab and not IsDesigning;
end;

function FXWindowsControl.CanUpdate: boolean;
begin
  Result := not IsReading and (Parent <> nil) and HandleAllocated;
end;

procedure FXWindowsControl.CenterInParent;
begin
  if Parent = nil then
    Exit;

  SetBounds((Parent.Width - Self.Width) div 2, (Parent.Height - Self.Height) div 2, Self.Width, Self.Height);
end;

procedure FXWindowsControl.ChangeScale(M, D: Integer{$IF CompilerVersion > 29}; isDpiChange: Boolean{$ENDIF});
begin
  inherited;
  ScaleChanged( M / D );
  if isDpiChange then begin
    UpdateRects;
    Redraw;
  end;
end;

procedure FXWindowsControl.ClearBufferRegion(ARect: TRect);
begin
  ARect.Intersect(Buffer.ClipRect);
  if ARect.IsEmpty then
    Exit;

  with Buffer do
  if Transparent then begin
    // TRANSPARENT
    Buffer.CopyRect(ARect, FBackground.Canvas, ARect);
  end else begin
    // SOLID
    Brush.Color := Background;

    // Fill
    FillRect(ARect);
  end;
end;

procedure FXWindowsControl.CMColorChanged(var Message: TMessage);
begin
  // no NOT invalidate
end;

procedure FXWindowsControl.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  SetState( FXControlState.None );
  if BufferedComponent and Supports(Self, IFXComponent) then
    begin
      (Self as IFXComponent).UpdateTheme(false);
      Redraw;
    end;
end;

procedure FXWindowsControl.CMFontChanged(var Message: TMessage);
begin
  // no NOT invalidate
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

procedure FXWindowsControl.CNKeyUp(var Message: TWMKeyUp);
var
  CanContinue: boolean;
begin
  CanContinue := true;

  HandleKeyUp(CanContinue, Message.CharCode, KeyDataToShiftState(Message.KeyData));

  if CanContinue then
    inherited;
end;

procedure FXWindowsControl.ComponentCreated;
begin
  // Update colors when first created
  UpdateColors;

  // Update rects
  UpdateRects;

  // First draw
  Redraw;
end;

constructor FXWindowsControl.Create(AOwner: TComponent);
begin
  inherited;
  // Component Defaults
  FCreated := false;
  FBufferedComponent := true;
  FAutoFocusLine := false;
  FOpacity := DEFAULT_OPACITY;
  FTransparent := true;
  FHitTest := true;

  AlignWithMargins := true;
  Margins.Left := 0;
  Margins.Top := 0;
  Margins.Right := 0;
  Margins.Bottom := 0;

  FInnerMargins := FXPadding.Create(Self);
  FInnerMargins.OnChange := InnerMarginsUpdated;
  FMargins := FXMargins.Create(Self);
  FMargins.OnChange := MarginsUpdated;
  FPadding := FXPadding.Create(Self);
  FPadding.OnChange := PaddingUpdated;

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
  inherited;

  // Was created
  FCreated := true;
  ComponentCreated;
end;

procedure FXWindowsControl.DblClick;
begin
  FDoubleClickInProgress := true;

  inherited;
end;

destructor FXWindowsControl.Destroy;
begin
  FreeAndNil(FTextFont);

  FreeAndNil(FInnerMargins);
  FreeAndNil(FMargins);
  FreeAndNil(FPadding);

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

procedure FXWindowsControl.DisableAllAnimations;
begin
  //
end;

procedure FXWindowsControl.DoEnter;
begin
  inherited;
  FocusChanged(true);
end;

procedure FXWindowsControl.DoExit;
begin
  inherited;
  FocusChanged(false);
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

        // Copy colliding (draw from system cache)
        CopyRect(Local, FControl.GetCacheBuffer.Canvas, Host);
      end else
      // Copy background color
      if Supports(Parent, IFXControl) then begin
        Brush.Color := (Parent as IFXControl).Background;
        FillRect(ClipRect);
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
begin    // Draw absolute rect, as we need to capture control clipping of the "inner margin"
  DrawTo(AbsoluteRect, ACanvas, Destination);
end;

procedure FXWindowsControl.FocusChanged(Focused: boolean);
begin
  if AutoFocusLine then begin
    FHasEnteredTab := Focused;
    Invalidate; // do not redraw
  end;
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

function FXWindowsControl.GetAbsoluteRect: TRect;
begin
  Result := Rect(0, 0, Width, Height);
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

function FXWindowsControl.GetChildComponents: TArray<TComponent>;
begin
  SetLength(Result, ComponentCount);
  for var I := 0 to ComponentCount-1 do
    Result[I] := Components[I];
end;

function FXWindowsControl.GetChildControls: TArray<TControl>;
begin
  SetLength(Result, ControlCount);
  for var I := 0 to ControlCount-1 do
    Result[I] := Controls[I];
end;

function FXWindowsControl.GetChildControlsRecursively: TArray<TControl>;
begin
  SetLength(Result, ControlCount);
  for var I := 0 to ControlCount-1 do begin
    Result[I] := Controls[I];
    if Controls[I] is FXWindowsControl then
      Result := Result + FXWindowsControl(Controls[I]).GetChildControlsRecursively;
  end;
end;

function FXWindowsControl.GetClientRect: TRect;
begin
  // Apply padding
  Result := FInnerMargins.RectangleInflate( GetAbsoluteRect );
  if Result.Left > Result.Right then
    Result.Left := Result.Right;
  if Result.Top > Result.Bottom then
    Result.Top := Result.Bottom;
end;

function FXWindowsControl.GetContentRect: TRect;
begin
  Result := FPadding.RectangleInflate( GetClientRect );
end;

function FXWindowsControl.GetControlExtents: TRect;
begin
  Result := inherited;

  // Add padding
  Result := FPadding.RectangleExpand(Result);
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

function FXWindowsControl.GetHeight: integer;
begin
  Result := inherited Height;
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

function FXWindowsControl.GetInvisibile: boolean;
begin
  Result := FOpacity = 0;
end;

function FXWindowsControl.GetLeft: integer;
begin
  Result := inherited Left;
end;

function FXWindowsControl.GetParentBackgroundColor(Default: TColor): TColor;
begin
  Result := GetParentBackgroundColorEx(Self, Default);
end;

function FXWindowsControl.GetTop: integer;
begin
  Result := inherited Top;
end;

function FXWindowsControl.GetWidth: integer;
begin
  Result := inherited Width;
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

procedure FXWindowsControl.HandleKeyUp(var CanHandle: boolean; Key: integer;
  ShiftState: TShiftState);
begin
  //
end;

procedure FXWindowsControl.Inflate(ALeft, ATop, AWidth, AHeight: Integer);
begin
  SetBounds(Left-ALeft, Top-ATop, Width+AWidth, Height+AHeight);
end;

procedure FXWindowsControl.InteractionStateChanged(AState: FXControlState);
begin
  Redraw;
end;

procedure FXWindowsControl.Invalidate;
begin
  if IsReading or not Visible then
    Exit;

  // Draw the buffer
  DoPaint;

  inherited;
end;

function FXWindowsControl.IsContainer: Boolean;
begin
  Result := false;
end;

function FXWindowsControl.IsCreating: boolean;
begin
  Result := not Self.FCreated;
end;

function FXWindowsControl.IsDesigning: boolean;
begin
  Result := csDesigning in ComponentState;
end;

function FXWindowsControl.IsDestroying: boolean;
begin
  Result := csDestroying in ComponentState;
end;

function FXWindowsControl.IsOpacityStored: Boolean;
begin
  Result := FOpacity <> 100;
end;

function FXWindowsControl.IsReading: boolean;
begin
  Result := csReading in ComponentState;
end;

procedure FXWindowsControl.Loaded;
begin
  inherited;
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
  FDoubleClickInProgress := false;

  if InteractionState = FXControlState.Press then
    SetState( FXControlState.Hover );

  // Popup Menu
  if (Button = mbRight) then
    OpenPopupMenu(X, Y);
end;

procedure FXWindowsControl.Moved;
begin
  // no need to invoke Redraw() here since BoundsUpdated() allready calls It
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

procedure FXWindowsControl.InnerMarginsUpdated(Sender: TObject);
begin
  Realign;

  // Apply
  ApplyInnerMargins;
end;

procedure FXWindowsControl.PaddingUpdated(Sender: TObject);
begin
  Realign;

  // Rects
  ApplyPadding;
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

procedure FXWindowsControl.Redraw;
begin
  Redraw(true);
end;

procedure FXWindowsControl.Redraw(Flags: FXRedrawFlags);
begin
  // Quit conditions
  if not (FXRedrawFlag.Force in Flags) then
    if not BufferedComponent or (Parent = nil) or IsReading or not HandleAllocated then
      Exit;

  GetInvisibile;

  // Redraw buffer
  if FXRedrawFlag.RedrawBuffer in Flags then begin
    // Draw
    ResizeBuffer;
    SolidifyBuffer;
    PaintBuffer;

    // Switch to new buffer
    SwitchBuffer;
  end;

  // Paint buffer
  if FXRedrawFlag.Paint in Flags then
    DoPaint;

  // Invalidate buffer
  if FXRedrawFlag.Invalidate in Flags then
    Invalidate;

  // Draw children
  if FXRedrawFlag.UpdateChildren in Flags then
    RedrawChildren;

  // Draw controls above
  if FXRedrawFlag.UpdateAbove in Flags then
    RedrawControlsAbove;

  // Last draw (unused)
  LastDraw := Now;
end;

procedure FXWindowsControl.Redraw(RedrawAbove: boolean);
var
  Flags: FXRedrawFlags;
begin
  Flags := DEFAULT_CONTROL_REDRAW_FLAGS;

  if not RedrawAbove then
    Flags := Flags - [FXRedrawFlag.UpdateAbove];

  Redraw(Flags);
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
        if Transparent and (Visible or IsDesigning) and (Width > 0) and (Height > 0) then
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

  FInnerMargins.ScaleChanged(Scaler);
  FMargins.ScaleChanged(Scaler);
  FPadding.ScaleChanged(Scaler);

  // Rects
  UpdateRects;
end;

procedure FXWindowsControl.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  // Check for changes
  const BoundsChanged = (ALeft <> Left) or (ATop <> Top) or (AWidth <> Width) or (AHeight <> Height);

  if not IsReading and BoundsChanged then begin
    // Get controls before bounds change
    const AControls = GetControlsAbove;

    // Execute bounds change
    inherited;

    // Notify previous neighbours
    RedrawAndSortControls(AControls);
  end else
    inherited;

  // Notify
  if BoundsChanged then
    BoundsUpdated;
end;

procedure FXWindowsControl.SetHeight(const Value: integer);
begin
  if Height = Value then
    Exit;
  inherited Height := Value;
end;

procedure FXWindowsControl.SetInvisibile(const Value: boolean);
begin
  if Invisible = Value then
    Exit;

  if Value then
    FOpacity := 0
  else
    FOpacity := 100;

  // Draw
  StandardUpdateDraw;
end;

procedure FXWindowsControl.SetLeft(const Value: integer);
begin
  inherited Left := Value;
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
  if (FOpacity = Value) or not InRange(Value, 0, 100) then
    Exit;

  FOpacity := Value;

  // Draw
  StandardUpdateDraw;
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

procedure FXWindowsControl.SetTop(const Value: integer);
begin
  if Top = Value then
    Exit;

  inherited Top := Value;
end;

procedure FXWindowsControl.SetTransparent(const Value: boolean);
begin
  if Value = FTransparent then
    Exit;

  FTransparent := Value;
  Redraw;
end;

procedure FXWindowsControl.SetWidth(const Value: integer);
begin
  if Width = Value then
    Exit;
  inherited Width := Value;
end;

procedure FXWindowsControl.Sized;
begin
  if not IsReading then
    // When sized, update the Rects
    UpdateRects;
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
  TArrayUtils<TControl>.Sort(AControls, function(A, B: TControl): TValueRelationship
    begin
      Result := TType<integer>.Compare(A.ComponentIndex, B.ComponentIndex);
    end);
end;

procedure FXWindowsControl.StandardUpdateColor;
begin
  if not CanUpdate then
    Exit;

  UpdateColors;
end;

procedure FXWindowsControl.StandardUpdateComplete;
begin
  if not CanUpdate then
    Exit;

  UpdateColors;
  UpdateRects;
  Redraw;
end;

procedure FXWindowsControl.StandardUpdateDraw;
begin
  if not CanUpdate then
    Exit;

  Redraw;
end;

procedure FXWindowsControl.StandardUpdateLayout;
begin
  if not CanUpdate then
    Exit;

  UpdateRects;
  Redraw;
end;

procedure FXWindowsControl.SwitchBuffer;
begin
  BufferSecondary := not BufferSecondary;
end;

procedure FXWindowsControl.UpdateColors;
begin
  //
end;

procedure FXWindowsControl.UpdateFocusRect;
begin
  FocusRect := Self.ClientRect;

  FFocusRect.Right := FocusRect.Right - FOCUS_LINE_SIZE;
  FFocusRect.Bottom := FocusRect.Bottom - FOCUS_LINE_SIZE;
end;

procedure FXWindowsControl.UpdateRects;
begin
  //
end;

procedure FXWindowsControl.UpdateTheme(const UpdateChildren: Boolean);
begin
  UpdateColors;
  UpdateRects;

  // Re-paint, do not redraw children
  Redraw(false);

  // Children
  if UpdateChildren then begin
    const Children = GetChildControls;
    for var I := 0 to High(Children) do
    if Supports(Children[I], IFXComponent) then
      (Children[I] as IFXComponent).UpdateTheme(UpdateChildren);

    const ChildrenC = GetChildComponents;
    for var I := 0 to High(ChildrenC) do
    if Supports(ChildrenC[I], IFXComponent) then
      (ChildrenC[I] as IFXComponent).UpdateTheme(UpdateChildren);
  end;
end;

procedure FXWindowsControl.WMMove(var Message: TWMMove);
begin
  inherited;

  Moved;
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

  Sized;
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

{ FXContainerWindowsControl }

constructor FXContainerWindowsControl.Create(aOwner: TComponent);
begin
  inherited;
  TabStop := false;
  ControlStyle := ControlStyle + [csAcceptsControls];
  Transparent := false;
end;

procedure FXContainerWindowsControl.Redraw(RedrawAbove: boolean);
begin
  inherited;
end;

end.
