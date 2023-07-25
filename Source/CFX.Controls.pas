unit CFX.Controls;

interface
  uses
    Winapi.Windows, Vcl.Graphics, Classes, Types, Winapi.Messages, CFX.Types,
    CFX.UIConsts, SysUtils, CFX.Graphics, CFX.VarHelpers, CFX.ThemeManager,
    Vcl.Controls, CFX.PopupMenu, CFX.Linker, Vcl.Forms, CFX.Forms;

  type
    // Canvas-Based Control
    FXCustomControl = class(TCustomControl)
    published
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

      // Events
      procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;

      // Data
      procedure ResizeBuffer;
      function GetBuffer: TCanvas;
      function CanDrawFocusLine: boolean;

      // Set
      procedure SetState(const Value: FXControlState);
      procedure SetTransparent(const Value: boolean);

    protected
      // Paint
      procedure WMSize(var Message: TWMSize); message WM_SIZE;

      procedure Resize; override;

      procedure SolidifyBuffer;

      procedure Paint; override;
      procedure PaintBuffer; virtual;

      // Background
      procedure PaintBackground;

      // Virtual Events
      procedure ComponentCreated; virtual;
      procedure UpdateFocusRect; virtual;
      procedure OpenPopupMenu(X, Y: integer); virtual;
      procedure HandleKeyDown(var CanHandle: boolean; Key: integer; ShiftState: TShiftState); virtual;

      // Focus Line and Events
      procedure DoEnter; override;
      procedure DoExit; override;

      property FocusRect: TRect read FFocusRect write FFocusRect;
      property AutoFocusLine: boolean read FAutoFocusLine write FAutoFocusLine;

      // Created
      procedure CreateWnd; override;

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

      // Color
      property Color;
      property Transparent: boolean read FTransparent write SetTransparent;

      // Interact State
      property InteractionState: FXControlState read FInteraction write SetState;
      property PreviousInteractionState: FXControlState read FPreviousInteraction write FPreviousInteraction;

      // Draw Buffer
      property Buffer: TCanvas read GetBuffer;
      property BufferedComponent: boolean read FBufferedComponent write FBufferedComponent;

    published
      // Popup Menu
      property PopupMenu: FXPopupMenu read FPopupMenu write FPopupMenu;

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
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;

      // Popup Menu
      property PopupMenu: FXPopupMenu read FPopupMenu write FPopupMenu;

    public
      // Parent Utilities
      function GetParentBackgroundColor(Default: TColor): TColor;

    end;

implementation

{ FXTransparentControl }

function FXWindowsControl.CanDrawFocusLine: boolean;
begin
  Result := AutoFocusLine and Focused and FHasEnteredTab and not IsDesigning;
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
  //InterceptMouse := true;
  FCreated := false;
  FBufferedComponent := true;
  FAutoFocusLine := false;
  FTransparent := false;

  TabStop := true;
  ParentColor := false;

  ControlStyle := ControlStyle + [csOpaque, csCaptureMouse];
  Brush.Style := bsClear;

  FBuffer := TBitMap.Create;
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
  FreeAndNil(FBuffer);
  inherited;
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

function FXWindowsControl.GetBuffer: TCanvas;
begin
  Result := FBuffer.Canvas;
end;

function FXWindowsControl.GetParentBackgroundColor(Default: TColor): TColor;
begin
  if (Parent <> nil) and Supports(Parent, FXControl) then
    Result := (Parent as FXControl).Background
      else
        Result := Default;
end;

procedure FXWindowsControl.HandleKeyDown(var CanHandle: boolean; Key: integer;
  ShiftState: TShiftState);
begin
  // nothing
end;

procedure FXWindowsControl.InteractionStateChanged(AState: FXControlState);
begin
  Paint;
end;

procedure FXWindowsControl.Invalidate;
begin
  inherited;
  if BufferedComponent and (Parent <> nil) then
    with Buffer do
      begin
        ResizeBuffer;
        SolidifyBuffer;
        PaintBuffer;
      end;
end;

function FXWindowsControl.IsDesigning: boolean;
begin
  Result := csDesigning in ComponentState;
end;

function FXWindowsControl.IsDestroying: boolean;
begin
  Result := csDesigning in ComponentState;
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
  if (InteractionState = FXControlState.Press) and not Focused then
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
  InteractionState := FXControlState.Hover;

  // Popup Menu
  if (Button = mbRight) then
    OpenPopupMenu(X, Y);
end;

procedure FXWindowsControl.OpenPopupMenu(X, Y: integer);
begin
  if Assigned(PopupMenu) then
    FPopupMenu.PopupAtPoint( ClientToScreen(Point(X,Y)) );
end;

procedure FXWindowsControl.Paint;
begin
  inherited;
  if BufferedComponent then
    begin
      // Reset Color
      Buffer.Brush.Color := Color;

      // Draw Buffer
      Canvas.Draw(0, 0, FBuffer);
    end;

  // Focus Line
  if CanDrawFocusLine then
    begin
      UpdateFocusRect;

      Canvas.GDIRoundRect(MakeRoundRect(FocusRect, FOCUS_LINE_ROUND, FOCUS_LINE_ROUND),
        nil,
        GetRGB(ThemeManager.SystemColor.ForeGround).MakeGDIPen(FOCUS_LINE_SIZE))
    end;
end;

procedure FXWindowsControl.PaintBackground;
var
  FControl: FXCustomControl;
  FForm: FXForm;
begin
  // Draw Background
  with Buffer do
    begin
      if Transparent and false { WORK IN PROGRESS} then
        begin
          // FX Control
          if (Parent is FXCustomControl) then
            begin
              FControl := FXCustomControl(Parent);

              Canvas.CopyRect(ClientRect, FControl.Canvas, BoundsRect);
            end
          else
            // Form
            if (Parent is FXForm) then
              begin
                FForm := FXForm(Parent);

                Canvas.CopyRect(ClientRect, FForm.Canvas, BoundsRect);
              end;
        end
      else
        begin
          Brush.Color := Color;
          FillRect(ClipRect);
        end;
    end;
end;

procedure FXWindowsControl.PaintBuffer;
begin
  // Paint
  Paint;
end;

procedure FXWindowsControl.Resize;
begin
  inherited;
end;

procedure FXWindowsControl.ResizeBuffer;
begin
  if BufferedComponent then
    begin
      if (FBuffer.Width <> Width) or (FBuffer.Height <> Height) then
        FBuffer.SetSize(Width, Height);
    end;
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
  Message.Result := 0;
  inherited;
end;

procedure FXWindowsControl.WMSize(var Message: TWMSize);
begin
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

end.
