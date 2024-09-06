unit CFX.Progress;

interface

uses
  Classes,
  Messages,
  Windows,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.ExtCtrls,
  Types,
  SyncObjs,
  Math,
  Threading,
  CFX.Colors,
  CFX.ThemeManager,
  CFX.Graphics,
  CFX.Constants,
  SysUtils,
  CFX.Classes,
  CFX.Threading,
  CFX.Types,
  CFX.VarHelpers,
  CFX.Linker,
  CFX.Controls;

type
  FXProgress = class(FXWindowsControl)
  private
    const
      MAX_ANIM = 25;

    var DrawRect, FilledRect, LineRect: TRect;
    FDrawColors: FXCompleteColorSet;
    FCustomColors: FXCompleteColorSets;
    FOtherColors: FXColorSet;
    FCustomOtherColors: FXColorSets;

    FAnimations: boolean;

    FOffset: integer; // the offset position for the marquee animation
    FInterSize: integer; // the internal size of the drawing width
    FLengthState: integer;

    FProgressThread: TThread;
    FProgressPos: integer;

    FAnimateThread: TThread;
    FThreadFinishedEvent: TEvent;
    FThreadActive: boolean;

    FValue: FXPercent;
    FValueAdd: single;
    FValueAddMax: single;

    FProgressKind: FXProgressKind;

    FProgressHeight: integer;
    FProgressLineHeight: integer;

    // Thread
    procedure SetAnimationThread(Enabled: boolean);
    procedure AnimateValue(From: single);

    // Setters
    procedure SetProgressHeight(const Value: integer);
    procedure SetProgressLineHeight(const Value: integer);
    procedure SetProgressKind(const Value: FXProgressKind);
    procedure SetValue(const Value: FXPercent);

    // Colors
    function GetFrontColor: TColor;

  protected
    procedure PaintBuffer; override;

    // Internal
    procedure UpdateColors; override;
    procedure UpdateRects; override;

    // Scaler
    procedure ScaleChanged(Scaler: single); override;

    // State
    procedure InteractionStateChanged(AState: FXControlState); override;

  published
    // Custom Colors
    property CustomColors: FXCompleteColorSets read FCustomColors write FCustomColors stored true;
    property CustomOtherColors: FXColorSets read FCustomOtherColors write FCustomOtherColors stored true;

    // Props
    property Value: FXPercent read FValue write SetValue;

    property Animations: boolean read FAnimations write FAnimations;

    property ProgressKind: FXProgressKind read FProgressKind write SetProgressKind default FXProgressKind.Normal;
    property ProgressHeight: integer read FProgressHeight write SetProgressHeight default PROGRESS_HEIGHT;
    property ProgressLineHeight: integer read FProgressLineHeight write SetProgressLineHeight default PROGRESS_LINE_HEIGHT;

    // Utils
    procedure Reset; (* reset to 0 without animation *)

    // Default props
    property Align;
    property Font;
    property Transparent;
    property Opacity;
    property PaddingFill;
    property Constraints;
    property Anchors;
    property Hint;
    property ShowHint;
    property ParentShowHint;
    property TabStop default false;
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

  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    // Interface
    function Background: TColor; override;
  end;

implementation

procedure FXProgress.AnimateValue(From: single);
var
  Position: single;
begin
  // Prep
  Position := Value; // this is what the thread will animate TO

  // Started
  if (FProgressThread <> nil) and FProgressThread.Running then
    begin
      // The thread will automatically configure any changes
      Exit;
    end;

  // Values
  FValueAdd := From - Position;
  FValueAddMax := FValueAdd;
  FProgressPos := 0;

  // Thread
  FProgressThread := TThread.CreateAnonymousThread(procedure
    begin
      repeat
        // Set
        FValueAdd := (MAX_ANIM-FProgressPos) / MAX_ANIM * FValueAddMax;

        // Draw
        TThread.Synchronize(nil, procedure
          begin
            UpdateRects;
            Redraw;
          end);

        // Inc
        Inc(FProgressPos);

        // Sleep
        Sleep(10);

        // Check for sys modifications
        if Position <> Value then
          begin
            From := Position;
            Position := Value;

            FValueAdd := From - Position;
            FValueAddMax := FValueAdd;
            FProgressPos := 0;
          end;
      until (FProgressPos >= MAX_ANIM);

      // Reset
      FValueAdd := 0;

      // Draw
      TThread.Synchronize(nil, procedure
        begin
          UpdateRects;
          Redraw;
        end);
    end);
  with FProgressThread do
    begin
      Priority := tpLowest;

      FreeOnTerminate := true;
      Start;
    end;
end;

function FXProgress.Background: TColor;
begin
  Result := FDrawColors.BackGround;
end;

constructor FXProgress.Create(aOwner: TComponent);
begin
  inherited;
  AutoFocusLine := true;
  BufferedComponent := true;

  TabStop := false;

  // Custom Color
  Value := 0;
  FAnimations := true;

  FProgressHeight := PROGRESS_HEIGHT;
  FProgressLineHeight := PROGRESS_LINE_HEIGHT;

  FCustomColors := FXCompleteColorSets.Create(Self);
  FCustomOtherColors := FXColorSets.Create(Self);

  FDrawColors := FXCompleteColorSet.Create;
  FOtherColors := FXColorSet.Create;

  // Threading
  FThreadFinishedEvent := TEvent.Create(nil, True, False, '');

  // Sizing
  Height := 20;
  Width := 200;
end;

destructor FXProgress.Destroy;
begin
  // Stop Threads
  FProgressPos := MAX_ANIM;

  if FThreadActive and (FAnimateThread <> nil) then
    begin
      FAnimateThread.Terminate;
      FThreadFinishedEvent.ResetEvent;

      FThreadFinishedEvent.WaitFor(FIFTH_SECOND){ = wrTimeout};
        // FAnimateThread.Free; // The thread WILL free Itself, since FreeOnTerminate is TRUE
    end;

  // Set refrerence to nil
  FAnimateThread := nil;

  // Free event
  FThreadFinishedEvent.Free;

  // Destroy
  FreeAndNil( FCustomColors );
  FreeAndNil( FDrawColors );
  FreeAndNil( FCustomOtherColors );
  FreeAndNil( FOtherColors );
  inherited;
end;

function FXProgress.GetFrontColor: TColor;
begin
  case FProgressKind of
    FXProgressKind.Paused: Result := FOtherColors.Foreground;
    FXProgressKind.Error: Result := FOtherColors.Background;
    else Result := FDrawColors.Accent;
  end;
end;

procedure FXProgress.InteractionStateChanged(AState: FXControlState);
begin
  inherited;
  Redraw;
end;

procedure FXProgress.PaintBuffer;
var
  ARectRound: TRoundRect;
  AColor: TColor;
begin
  // Background
  Color := FDrawColors.BackGround;
  PaintBackground;

  // Draw
  with Buffer do
    begin
      // Draw Line
      if ProgressKind <> FXProgressKind.Intermediate then
        begin
          ARectRound.Rect := LineRect;
          ARectRound.SetRoundness(LineRect.Height);

          AColor := FDrawColors.BackGroundInterior;

          Buffer.GDIRoundRect(ARectRound, GetRGB(AColor).MakeGDIBrush, nil);
        end;

      // Draw Main
      ARectRound.Rect := FilledRect;
      ARectRound.SetRoundness(FilledRect.Height);

      AColor := GetFrontColor;

      Buffer.GDIRoundRect(ARectRound, GetRGB(AColor).MakeGDIBrush, nil);
    end;

  // Inherit
  inherited;
end;

procedure FXProgress.Reset;
begin
  SetAnimationThread(false);

  FValue := 0;
  FValueAdd := 0;
  FOffset := 0;
  FInterSize := 0;

  UpdateRects;
  Redraw;
end;

procedure FXProgress.UpdateColors;
begin
  // Access theme manager
  FDrawColors.Assign( ThemeManager.SystemColor );
  if not Enabled then
    begin
      FDrawColors.Accent := GetColorGrayScale(FDrawColors.Accent);
    end
  else
    begin
      if FCustomColors.Enabled then
        // Load custom
        FDrawColors.LoadFrom( FCustomColors, ThemeManager.DarkTheme )
      else
        begin
          // Build color palette
          FDrawColors.LoadFrom( ThemeManager.SystemColorSet, ThemeManager.DarkTheme );

          if ThemeManager.DarkTheme then
            FDrawColors.BackGroundInterior := ColorRepository.LightBackgroundControl
          else
            FDrawColors.BackGroundInterior := ColorRepository.DarkBackgroundControl;
        end;

      if FCustomOtherColors.Enabled then
        FOtherColors.LoadFrom( FCustomOtherColors, ThemeManager.DarkTheme )
      else
        begin
          if ThemeManager.darkTheme then
            begin
              FOtherColors.Background := ColorRepository.DarkErrorColor;
              FOtherColors.Foreground := ColorRepository.DarkPausedColor;
            end
          else
            begin
              FOtherColors.Background := ColorRepository.LightErrorColor;
              FOtherColors.Foreground := ColorRepository.LightPausedColor;
            end;
        end;
    end;
end;

procedure FXProgress.UpdateRects;
begin
  // Rect
  DrawRect := GetClientRect;

  // Fill
  FilledRect := DrawRect;
  FilledRect.Height := ProgressHeight;
  FilledRect.Width := FilledRect.Width - 1;
  FilledRect.Offset(0, (DrawRect.Height-ProgressHeight) div 2);

  FilledRect.Width := round((Value++FValueAdd) / 100 * FilledRect.Width);

  // Line
  LineRect := DrawRect;
  LineRect.Height := ProgressLineHeight;
  LineRect.Width := LineRect.Width - 1;
  LineRect.Offset(0, (DrawRect.Height-ProgressLineHeight) div 2);

  // State
  if ProgressKind = FXProgressKind.Intermediate then
    begin
      FilledRect.OffSet(FOffset, 0);

      FilledRect.Width := FInterSize;
    end;
end;

procedure FXProgress.ScaleChanged(Scaler: single);
begin
  inherited;
  // update scale
end;

procedure FXProgress.SetAnimationThread(Enabled: boolean);
begin
  if Enabled = FThreadActive then
    Exit;

  FThreadActive := Enabled;

  if Enabled then
    begin
      FOffset := 0;

      if IsDesigning then
        begin
          FInterSize := DrawRect.Width;
          FOffset := 0;

          Exit;
        end;

      FAnimateThread := TThread.CreateAnonymousThread(procedure
        begin
          while FThreadActive do
            begin
              var Passed: boolean;
              Passed := false;

              // Increase
              case FLengthState of
                0: Inc(FOffset, ceil(DrawRect.Width div 30));
                1: Inc(FOffset, ceil(DrawRect.Width div 20));
                else Inc(FOffset, ceil(DrawRect.Width div 25));
              end;

              if FOffset >= DrawRect.Width then
                begin
                  Passed := true;

                  inc(FLengthState);
                  if FLengthState > 1 then
                    FLengthState := 0;
                end;

              // Size
              case FLengthState of
                0: FInterSize := ceil(DrawRect.Width / 3);
                1: FInterSize := ceil(DrawRect.Width / 2.2);
                else
                  FInterSize := DrawRect.Width;
              end;

              // Offset
              if Passed then
                FOffset := -FInterSize;

              // Update
              TThread.Synchronize(nil, procedure
                begin
                  /// Now that the thread has access to the main thread,
                  ///  perform a check that the main thread did not free the object
                  ///  in the meanwhile.
                  if FAnimateThread = nil then
                    Exit;

                  UpdateRects;
                  Redraw;
                end);

              // Sleep
              if (FAnimateThread = nil) or FAnimateThread.CheckTerminated then
                  Exit;
              Sleep(10);
              if (FAnimateThread = nil) or FAnimateThread.CheckTerminated then
                  Exit;
            end;

          // Done
          if FThreadFinishedEvent <> nil then
            FThreadFinishedEvent.SetEvent;
        end);
      with FAnimateThread do
        begin
          Priority := tpLowest;

          FValueAdd := 0;

          FreeOnTerminate := true;
          Start;
        end;
    end
  else
    if (FAnimateThread <> nil) and FAnimateThread.Running then
      begin
        // Wait for thread to finalise work
        FAnimateThread.Terminate;
        FThreadFinishedEvent.WaitFor(FIFTH_SECOND)
      end;
end;

procedure FXProgress.SetProgressHeight(const Value: integer);
begin
  if FProgressHeight = Value then
    Exit;

  FProgressHeight := Value;
  StandardUpdateLayout;
end;

procedure FXProgress.SetProgressKind(const Value: FXProgressKind);
begin
  if FProgressKind = Value then
    Exit;

  SetAnimationThread(Value = FXProgressKind.Intermediate);

  FProgressKind := Value;

  StandardUpdateLayout;
end;

procedure FXProgress.SetProgressLineHeight(const Value: integer);
begin
  if FProgressLineHeight = Value then
    Exit;

  FProgressLineHeight := Value;
  StandardUpdateLayout;
end;

procedure FXProgress.SetValue(const Value: FXPercent);
begin
  if (FValue = Value) or not InRange(Value, 0, 100) then
    Exit;

  const Previous = FValue;
  FValue := Value;

  if Animations and not IsReading then
    AnimateValue(Previous);

  StandardUpdateLayout;
end;

end.
