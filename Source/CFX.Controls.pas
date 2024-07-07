unit CFX.Controls;

interface
  uses
    Winapi.Windows, Vcl.Graphics, Classes, Types, Winapi.Messages, CFX.Types,
    CFX.UIConsts, SysUtils, CFX.Graphics, CFX.VarHelpers, CFX.ThemeManager,
    Vcl.Controls, CFX.PopupMenu, CFX.Linker, Vcl.Forms, CFX.Forms,
    Vcl.Dialogs, CFX.Classes, Math;

  type
    // Canvas-Based Control
    FXCustomControl = class(TCustomControl)
    protected
      property Canvas;
    end;

    // Control
    FXWindowsControl = class(FXCustomControl)
    private
      FPopupMenu: FXPopupMenu;
      FBuffer: TBitMap;
      FBufferedComponent: boolean;
      FFocusRect: TRect;
      FAutoFocusLine: boolean;
      FHasEnteredTab: boolean;
      FInteraction: FXControlState;
      FPreviousInteraction: FXControlState;
      FCreated: boolean;
      FTransparent: boolean;
      FOpacity: byte;
      FBackground: TBitMap;
      FOnPaint: FXControlOnPaint;
      FOnPaintBuffer: FXControlOnPaint;
      FPadding: FXPadding;
      FTextFont: TFont;
      FFocusFlags: FXFocusFlags;
      FHitTest: boolean;

      // Events
      procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;

      // Data
      procedure ResizeBuffer;
      function GetBuffer: TCanvas;
      function CanDrawFocusLine: boolean;

      // Draw
      procedure SolidifyBuffer;

      // Object Notify Events
      procedure FontNotifyUpdate(Sender: TObject);

      // Set
      procedure SetState(const Value: FXControlState);
      procedure SetTransparent(const Value: boolean);
      procedure SetOpacity(const Value: byte);

    protected
      // Paint
      procedure WMSize(var Message: TWMSize); message WM_SIZE;

      procedure Resize; override;

      procedure Paint; override;
      procedure PaintBuffer; virtual;

      property HitTest: boolean read FHitTest write FHitTest default true;

      property BufferedComponent: boolean read FBufferedComponent write FBufferedComponent;

      property OnPaint: FXControlOnPaint read FOnPaint write FOnPaint;
      property OnPaintBuffer: FXControlOnPaint read FOnPaintBuffer write FOnPaintBuffer;

      // Background
      procedure DrawBackground(var Background: TBitMap); virtual;
      procedure PaintBackground;
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

      // Padding
      property PaddingFill: FXPadding read FPadding write FPadding;
      function GetClientRect: TRect; override;

      // Created
      procedure CreateWnd; override;

      // Visible Change
      procedure OnVisibleChange(var Message : TMessage); message CM_VISIBLECHANGED;

      // Mouse Events
      procedure CMMouseEnter(var Message : TMessage); message CM_MOUSEENTER;
      procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;

      procedure MouseUp(Button : TMouseButton; Shift: TShiftState; X, Y : integer); override;
      procedure MouseDown(Button : TMouseButton; Shift: TShiftState; X, Y : integer); override;

      // Key Events
      procedure CNKeyDown(var Message: TWMKeyDown); message CN_KEYDOWN;

      // Interaction
      procedure InteractionStateChanged(AState: FXControlState); virtual;
      procedure SetNewInteractionState(AState: FXControlState; ForceUpdate: boolean = false; UpdatePrevious: boolean = true);

      // Utilities
      function IsReading: boolean;
      function IsDesigning: boolean;
      function IsDestroying: boolean;
      function Creating: boolean;

      function Destroyed: boolean;

      // Catch Events
      procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;

      // Scaling
      procedure ChangeScale(M, D: Integer{$IF CompilerVersion > 29}; isDpiChange: Boolean{$ENDIF}); override;

      // Properties
      property Color;

      property FocusRect: TRect read FFocusRect write FFocusRect;
      property AutoFocusLine: boolean read FAutoFocusLine write FAutoFocusLine;

      property FocusFlags: FXFocusFlags read FFocusFlags write FFocusFlags default [];

      property InteractionState: FXControlState read FInteraction write SetState;
      property PreviousInteractionState: FXControlState read FPreviousInteraction write FPreviousInteraction;

      property Font: TFont read FTextFont write FTextFont;

      property Transparent: boolean read FTransparent write SetTransparent default true;
      property Opacity: byte read FOpacity write SetOpacity default 255;

    published
      // Buffer
      property Buffer: TCanvas read GetBuffer;

      // Popup Menu
      property PopupMenu: FXPopupMenu read FPopupMenu write FPopupMenu;

      property Hint;

      property TabStop default true;

      property Enabled;
      property Visible;
      property Tag;

    public
      // Constructors
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;

      // Parent Utilities
      function GetParentBackgroundColor(Default: TColor): TColor;

      // Invalidate
      procedure Invalidate; override;
      procedure InvalidateControlsAbove;
    end;

    FXBufferGraphicControl = class(TGraphicControl)
    private
      FPopupMenu: FXPopupMenu;
      FInteraction: FXControlState;
      FPreviousInteraction: FXControlState;
      FBuffer: TBitMap;
      FOnPaint: FXControlOnPaint;
      FOnPaintBuffer: FXControlOnPaint;

      procedure SetState(const Value: FXControlState);
      function GetBuffer: TCanvas;

    protected
      // Mouse Events
      procedure CMMouseEnter(var Message : TMessage); message CM_MOUSEENTER;
      procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;

      procedure MouseUp(Button : TMouseButton; Shift: TShiftState; X, Y : integer); override;
      procedure MouseDown(Button : TMouseButton; Shift: TShiftState; X, Y : integer); override;

      // Detect creation
      procedure Loaded; override;

      // Paint
      procedure Paint; override;
      procedure PaintBuffer; virtual;

      property OnPaint: FXControlOnPaint read FOnPaint write FOnPaint;
      property OnPaintBuffer: FXControlOnPaint read FOnPaintBuffer write FOnPaintBuffer;

      // Buffer
      procedure ResizeBuffer;
      procedure Resize; override;

      // Created
      procedure ComponentCreated; virtual;

      // Visible Change
      procedure OnVisibleChange(var Message : TMessage); message CM_VISIBLECHANGED;

      // Interaction
      procedure InteractionStateChanged(AState: FXControlState); virtual;

      // Utilities
      function IsReading: boolean;

      // Interact State
      property InteractionState: FXControlState read FInteraction write SetState;

    published
      // Draw Buffer
      property Buffer: TCanvas read GetBuffer;

      // Popup Menu
      property PopupMenu: FXPopupMenu read FPopupMenu write FPopupMenu;

      // Canvas
      function GetCanvas: TCanvas;

    public
      // Constructors
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;

      // Parent Utilities
      function GetParentBackgroundColor(Default: TColor): TColor;

      // Invalidate
      procedure Invalidate; override;
      procedure InvalidateControlsAbove;
    end;

    FXGraphicControl = class(TGraphicControl)
    private
      FPopupMenu: FXPopupMenu;
      FInteraction: FXControlState;
      FPreviousInteraction: FXControlState;
      FTransparent: boolean;

      procedure SetState(const Value: FXControlState);
      procedure SetTransparent(const Value: boolean);

    protected
      // Mouse Events
      procedure CMMouseEnter(var Message : TMessage); message CM_MOUSEENTER;
      procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;

      procedure MouseUp(Button : TMouseButton; Shift: TShiftState; X, Y : integer); override;
      procedure MouseDown(Button : TMouseButton; Shift: TShiftState; X, Y : integer); override;

      // Detect creation
      procedure Loaded; override;

      // Paint
      procedure Paint; override;

      // Created
      procedure ComponentCreated; virtual;

      // Interaction
      procedure InteractionStateChanged(AState: FXControlState); virtual;

      // Utilities
      function IsReading: boolean;

      // Interact State
      property InteractionState: FXControlState read FInteraction write SetState;

      // Transparent
      property Transparent: boolean read FTransparent write SetTransparent default true;

    published
      // Popup Menu
      property PopupMenu: FXPopupMenu read FPopupMenu write FPopupMenu;

    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;

      // Parent Utilities
      function GetParentBackgroundColor(Default: TColor): TColor;
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
  InteractionState := FXControlState.Hover;

  if Assigned(OnMouseEnter) then
    OnMouseenter(Self);
end;

procedure FXWindowsControl.CMMouseLeave(var Message: TMessage);
begin
  InteractionState := FXControlState.None;

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
  FTransparent := true;
  FOpacity := 255;
  FPadding := FXPadding.Create(Self);
  FHitTest := true;

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

  // Initialise Buffer
  FBuffer := TBitMap.Create;
  FBackground := TBitMap.Create;
  ResizeBuffer;
end;

procedure FXWindowsControl.CreateWnd;
begin
  FCreated := true;
  inherited;

  // Notify
  ComponentCreated;
end;

function FXWindowsControl.Creating: boolean;
begin
  Result := not FCreated;
end;

destructor FXWindowsControl.Destroy;
begin
  FreeAndNil(FTextFont);
  FreeAndNil(FPadding);
  FreeAndNil(FBuffer);
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
      Paint;
    end;
end;

procedure FXWindowsControl.DoExit;
begin
  inherited;
  if AutoFocusLine then
    begin
      FHasEnteredTab := false;
      Paint;
    end;
end;

procedure FXWindowsControl.DrawBackground(var Background: TBitMap);
var
  FControl: TControl;
  FWinControl: FXWindowsControl;
  FGraphic: FXBufferGraphicControl;
  I: integer;
  Intersection: TRect;
  HostBounds: TRect;
  Local,
  Host: TRect;
begin
  // Draw Background
  with Background.Canvas do
    begin
      // Solid color background
      Brush.Color := Color;
      FillRect(ClipRect);

      // Opaque
      if not (Transparent and (Parent is TWinControl)) then
        Exit;

      for I := 0 to Parent.ControlCount-1 do
        begin
          FControl := Parent.Controls[I];

          // Hidden
          if not FControl.Visible then
            Continue;

          // Bounds
          HostBounds := FControl.BoundsRect;

          // Intersect
          if not HostBounds.IntersectsWith(BoundsRect) then
            Continue;

          if Parent.Controls[I] is FXWindowsControl then
            begin
              FWinControl := FXWindowsControl(Parent.Controls[I]);

              // Check behind
              if (FWinControl.ComponentIndex < ComponentIndex) then
                begin
                  Intersection := TRect.Intersect(HostBounds, BoundsRect);

                  Local := Intersection;
                  Local.Offset(-BoundsRect.Left, -BoundsRect.Top);

                  Host := Intersection;
                  Host.Offset(-HostBounds.Left, -HostBounds.Top);

                  // Copy colliding
                  CopyRect(Local, FWinControl.Buffer, Host);
                end;
            end
          else
            if Parent.Controls[I] is FXBufferGraphicControl then
              begin
                FGraphic := FXBufferGraphicControl(Parent.Controls[I]);
                HostBounds := FGraphic.BoundsRect;

                // Check behind
                if (FGraphic.ComponentIndex < ComponentIndex) then
                  begin
                    Intersection := TRect.Intersect(HostBounds, BoundsRect);

                    Local := Intersection;
                    Local.Offset(-BoundsRect.Left, -BoundsRect.Top);

                    Host := Intersection;
                    Host.Offset(-HostBounds.Left, -HostBounds.Top);

                    // Copy colliding
                    CopyRect(Local, FGraphic.Buffer, Host);
                  end;
              end;
        end;

      // Opacity support
      { this will ensure that the background is already drawn when the
        composite background is drawn on top }
      if Opacity <> 255 then
        Self.Canvas.Draw(0, 0, FBuffer, 255);
    end;
end;

procedure FXWindowsControl.FontNotifyUpdate(Sender: TObject);
begin
  FontUpdate;
end;

procedure FXWindowsControl.FontUpdate;
begin
  // Update
  Invalidate;
end;

function FXWindowsControl.GetBackground: TCanvas;
begin
  Result := FBackground.Canvas;
end;

function FXWindowsControl.GetBuffer: TCanvas;
begin
  Result := FBuffer.Canvas;
end;

function FXWindowsControl.GetClientRect: TRect;
begin
  // Apply padding
  Result := FPadding.ApplyTo( Rect(0, 0, Width, Height) );
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
  Paint;
end;

procedure FXWindowsControl.Invalidate;
begin
  if BufferedComponent and (Parent <> nil) then
    with Buffer do
      begin
        ResizeBuffer;
        SolidifyBuffer;
        PaintBuffer;
      end;
  inherited;
end;

procedure FXWindowsControl.InvalidateControlsAbove;
var
  I: Integer;
  FControl: FXWindowsControl;
begin
  for I := 0 to Parent.ControlCount-1 do
    if Parent.Controls[I] is TWinControl then
      if Parent.Controls[I] <> Self then
        begin
          FControl := FXWindowsControl(Parent.Controls[I]);

          if (FControl.ComponentIndex > ComponentIndex) and FControl.Transparent then
            if FControl.BoundsRect.IntersectsWith(BoundsRect) and FControl.Transparent then
              FControl.Invalidate;
        end;
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

procedure FXWindowsControl.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  inherited;
  // State
  if (InteractionState = FXControlState.Hover) and (Button = mbLeft) then
    InteractionState := FXControlState.Press;

  // Focus
  if (InteractionState = FXControlState.Press) and CanFocus and not Focused then
    SetFocus;

  // Entered
  if FHasEnteredTab then
    begin
      FHasEnteredTab := false;
      if AutoFocusLine then
        Paint;
    end;
end;

procedure FXWindowsControl.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  inherited;
  if InteractionState = FXControlState.Press then
    InteractionState := FXControlState.Hover;

  // Popup Menu
  if (Button = mbRight) then
    OpenPopupMenu(X, Y);
end;

procedure FXWindowsControl.OnVisibleChange(var Message: TMessage);
begin
  inherited;
  InvalidateControlsAbove;
end;

procedure FXWindowsControl.OpenPopupMenu(X, Y: integer);
begin
  if Assigned(PopupMenu) then
    FPopupMenu.PopupAtPoint( ClientToScreen(Point(X,Y)) );
end;

procedure FXWindowsControl.Paint;
var
  Composite: TBitMap;
  ARect: TRect;
begin
  inherited;
  if BufferedComponent then
    begin
      // Reset Color
      Buffer.Brush.Color := Color;

      // Background
      if FOpacity <> 255 then
        begin
          Composite := TBitMap.Create(Width, Height);
          with Composite.Canvas do
            try
              Draw(0, 0, FBackground);
              Draw(0, 0, FBuffer, FOpacity);

              // Copy to screen
              Canvas.Draw(0, 0, Composite);
            finally
              Composite.Free;
            end;
        end
      else
        // Draw Buffer
        Canvas.Draw(0, 0, FBuffer, FOpacity);
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

  // Transparency
  if not IsReading then
    InvalidateControlsAbove;

  // Notify
  if Assigned(FOnPaint) then
    FOnPaint(Self);
end;

procedure FXWindowsControl.PaintBackground;
begin
  DrawBackground(FBackground);
  FBuffer.Canvas.Draw(0, 0, FBackground);
end;

procedure FXWindowsControl.PaintBuffer;
begin
  // Paint
  if Assigned(FOnPaintBuffer) then
    FOnPaintBuffer(Self);
end;

procedure FXWindowsControl.Resize;
begin
  inherited;
end;

procedure FXWindowsControl.ResizeBuffer;
begin
  if BufferedComponent then
    begin
      Width := Max(Width, 0);
      Height := Max(Height, 0);

      if (FBuffer.Width <> Width) or (FBuffer.Height <> Height) then
        begin
          FBuffer.SetSize(Width, Height);
          FBackground.SetSize(Width, Height);
        end;
    end;
end;

procedure FXWindowsControl.ScaleChanged(Scaler: single);
begin
  FTextFont.Height := round(FTextFont.Height * Scaler);

  FPadding.ScaleChanged(Scaler);
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

procedure FXWindowsControl.SetOpacity(const Value: byte);
begin
  if FOpacity <> Value then
    begin
      FOpacity := Value;

      // Draw
      Invalidate;
    end;
end;

procedure FXWindowsControl.SetState(const Value: FXControlState);
begin
  SetNewInteractionState(Value);
end;

procedure FXWindowsControl.SetTransparent(const Value: boolean);
begin
  if FTransparent <> Value then
    begin
      FTransparent := Value;

      if not IsDesigning then
        Invalidate;
    end;
end;

procedure FXWindowsControl.SolidifyBuffer;
begin
  with Buffer do
    begin
      // Reset Color
      Brush.Color := Color;

      // Clear
      FillRect(ClipRect);
    end;
end;

procedure FXWindowsControl.UpdateFocusRect;
begin
  FocusRect := Self.ClientRect;

  FFocusRect.Right := FocusRect.Right - FOCUS_LINE_SIZE;
  FFocusRect.Bottom := FocusRect.Bottom - FOCUS_LINE_SIZE;
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
  Invalidate;
end;

{ FXGraphicControl }

procedure FXGraphicControl.CMMouseEnter(var Message: TMessage);
begin
  InteractionState := FXControlState.Hover;

  if Assigned(OnMouseEnter) then
    OnMouseenter(Self);
end;

procedure FXGraphicControl.CMMouseLeave(var Message: TMessage);
begin
  InteractionState := FXControlState.None;

  if Assigned(OnMouseLeave) then
    OnMouseLeave(Self);
end;

procedure FXGraphicControl.ComponentCreated;
begin
  // nothing
end;

constructor FXGraphicControl.Create(AOwner: TComponent);
begin
  inherited;
  FTransparent := true;
end;

destructor FXGraphicControl.Destroy;
begin
  inherited;
end;

function FXGraphicControl.GetParentBackgroundColor(Default: TColor): TColor;
begin
  if (Parent <> nil) and Supports(Parent, FXControl) then
    Result := (Parent as FXControl).Background
      else
        Result := Default;
end;

procedure FXGraphicControl.InteractionStateChanged(AState: FXControlState);
begin
  Paint;
end;

function FXGraphicControl.IsReading: boolean;
begin
  Result := csReading in ComponentState;
end;

procedure FXGraphicControl.Loaded;
begin
  inherited;
  ComponentCreated;
end;

procedure FXGraphicControl.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: integer);
begin
  inherited;
  // State
  if (InteractionState = FXControlState.Hover) and (Button = mbLeft) then
    InteractionState := FXControlState.Press;
end;

procedure FXGraphicControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: integer);
begin
  inherited;
  InteractionState := FXControlState.Hover;

  // Popup Menu
  if (Button = mbRight) and Assigned(PopupMenu) then
    FPopupMenu.PopupAtPoint( ClientToScreen(Point(X,Y)) );
end;

procedure FXGraphicControl.Paint;
begin
  inherited;
  // nothing
end;

procedure FXGraphicControl.SetState(const Value: FXControlState);
begin
  if Value <> FInteraction then
    begin
      FPreviousInteraction := FInteraction;
      FInteraction := Value;

      InteractionStateChanged(Value);
    end;
end;

procedure FXGraphicControl.SetTransparent(const Value: boolean);
begin
  if FTransparent <> Value then
    begin
      FTransparent := Value;

      if not IsReading then
        RePaint;
    end;
end;

{ FXBufferGraphicControl }

procedure FXBufferGraphicControl.CMMouseEnter(var Message: TMessage);
begin
  InteractionState := FXControlState.Hover;

  if Assigned(OnMouseEnter) then
    OnMouseenter(Self);
end;

procedure FXBufferGraphicControl.CMMouseLeave(var Message: TMessage);
begin
  InteractionState := FXControlState.None;

  if Assigned(OnMouseEnter) then
    OnMouseenter(Self);
end;

procedure FXBufferGraphicControl.ComponentCreated;
begin

end;

constructor FXBufferGraphicControl.Create(AOwner: TComponent);
begin
  inherited;
  FBuffer := TBitMap.Create;
  ResizeBuffer;
end;

destructor FXBufferGraphicControl.Destroy;
begin
  FreeAndNil( FBuffer );
  inherited;
end;

function FXBufferGraphicControl.GetBuffer: TCanvas;
begin
  Result := FBuffer.Canvas;
end;

function FXBufferGraphicControl.GetCanvas: TCanvas;
begin
  Result := Canvas;
end;

function FXBufferGraphicControl.GetParentBackgroundColor(
  Default: TColor): TColor;
begin
  Result := GetParentBackgroundColorEx(Self, Default);
end;

procedure FXBufferGraphicControl.InteractionStateChanged(
  AState: FXControlState);
begin
  // none
end;

procedure FXBufferGraphicControl.Invalidate;
begin
  if Parent = nil then
    Exit;

  ResizeBuffer;
  PaintBuffer;

  inherited;
end;

procedure FXBufferGraphicControl.InvalidateControlsAbove;
var
  I: Integer;
  FControl: FXWindowsControl;
begin
  for I := 0 to Parent.ControlCount-1 do
    if Parent.Controls[I] is TWinControl then
      begin
        FControl := FXWindowsControl(Parent.Controls[I]);

        if FControl.BoundsRect.IntersectsWith(BoundsRect) and FControl.Transparent then
          FControl.Invalidate;
      end;
end;

function FXBufferGraphicControl.IsReading: boolean;
begin
  Result := csReading in ComponentState;
end;

procedure FXBufferGraphicControl.Loaded;
begin
  inherited;
  ComponentCreated;
end;

procedure FXBufferGraphicControl.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  inherited;
  // State
  if (InteractionState = FXControlState.Hover) and (Button = mbLeft) then
    InteractionState := FXControlState.Press;
end;

procedure FXBufferGraphicControl.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  inherited;
  InteractionState := FXControlState.Hover;

  // Popup Menu
  if (Button = mbRight) and Assigned(PopupMenu) then
    FPopupMenu.PopupAtPoint( ClientToScreen(Point(X,Y)) );
end;

procedure FXBufferGraphicControl.OnVisibleChange(var Message: TMessage);
begin
  InvalidateControlsAbove;
end;

procedure FXBufferGraphicControl.Paint;
begin
  inherited;
  // Draw Buffer
  Buffer.Brush.Color := Color;

  with inherited Canvas do
    Draw(0, 0, FBuffer);

  if not IsReading then
    InvalidateControlsAbove;

  // On Paint
  if Assigned(FOnPaint) then
    FOnPaint(Self);
end;

procedure FXBufferGraphicControl.PaintBuffer;
begin
  // Assign
  if Assigned(FOnPaintBuffer) then
    FOnPaintBuffer(Self);
end;

procedure FXBufferGraphicControl.Resize;
begin
  inherited;
  Invalidate;
end;

procedure FXBufferGraphicControl.ResizeBuffer;
begin
  if (FBuffer.Width <> Width) or (FBuffer.Height <> Height) then
    FBuffer.SetSize(Width, Height);
end;

procedure FXBufferGraphicControl.SetState(const Value: FXControlState);
begin
  if Value <> FInteraction then
    begin
      FPreviousInteraction := FInteraction;
      FInteraction := Value;

      InteractionStateChanged(Value);
    end;
end;

end.
