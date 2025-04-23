(**************************************************************)
(*                 Codrut's Animation Library                 *)
(*                                                            *)
(*                                                            *)
(*                    Copyright (c) 2024                      *)
(*             Petculescu Codrut. Codrut Software             *)
(*                                                            *)
(*                https://www.codrutsoft.com/                 *)
(*       https://github.com/Codrax/Codrut-Animation-Lib/      *)
(*                                                            *)
(**************************************************************)

unit CFX.Animation.Component;

interface

uses
  Winapi.Windows, Winapi.Messages, SysUtils, System.Variants, System.Classes,
  DateUtils, Vcl.Controls, Vcl.Dialogs, System.Math, TypInfo,
  CFX.Types, CFX.Animation.Main, CFX.Animation.Utils;

type
  // Notify Event
  TAniStepEvent = procedure(Sender: TObject; Step, TotalSteps: integer) of object;

  // Classes
  FXAnimationController = class;

  TAnimationThread = class(TThread)
    private
      FAnimation: FXAnimationController;
    public
      procedure Execute; override;
      constructor Create(AAnim: FXAnimationController);
  end;

  FXAnimationController = class(TComponent)
  private
    // Anim Const
    ValueKinds: set of TTypeKind;

    // Data
    FKind: FXAnimationKind;
    FDelay: single;
    { The maximum exponent of 10 to use as a sleep interval, eg: 1 = 10ms }
    FDelayMaxSegment: integer;
    FDuration: Single;
    { Duration values are stored as single and are
      noted in seconds, they will be multiplied by 10^3
      to be used as miliseconds. }
    FInverse: boolean;

    // Runtime
    FSteps: integer;
    FStatus: FXTaskStatus;
    ComponentBased: boolean;

    // Latency
    FLatencyAdjust: boolean;
    FLatencyCanSkipSteps: boolean;

    // Loop
    FLoop: boolean;
    FLoopInverse: boolean;
    FLoopDelay: boolean;

    // Property
    FStartFromCurrent: boolean;
    FPropertyName: string;

    // Notify Event
    FOnStart,
    FOnFinish,
    FOnLoop: TNotifyEvent;
    FOnStep: TAniStepEvent;

    // Thread
    FThread: TAnimationThread;
    FComponent: TComponent;

    // Animation Tick
    FStepValue: integer;
    FTotalStep: integer;
    FSleepStep: integer;

    // Thread & System
    procedure CreateThread;
    function PropertyValid: boolean;

    procedure WaitDelay;
    procedure ExecuteAnimation; virtual;
    procedure DoStepValue; virtual;
    function CalculatePercent: single;

    // Getters
    function GetPaused: boolean;
    function GetRunning: boolean;

    // Setters
    procedure SetPaused(const Value: boolean);
    procedure SetSteps(const Value: integer);
    procedure SetDuration(const Value: single);
    procedure SetRunning(const Value: boolean);
    procedure SetDelayMaxSegment(const Value: integer);

  published
    // Start
    procedure Start;
    procedure StartInverse;

    // Task
    procedure Stop;
    procedure Restart;

    // Properties
    property Delay: single read FDelay write FDelay;
    property DelayMaxSegment: integer read FDelayMaxSegment write SetDelayMaxSegment default 2;
    property Duration: single read FDuration write SetDuration;

    property Kind: FXAnimationKind read FKind write FKind;
    property Inverse: boolean read FInverse write FInverse default false;

    property Steps: integer read FSteps write SetSteps default 0;

    { compensate for the time needed to execute the code }
    property LatencyAdjustments: boolean read FLatencyAdjust write FLatencyAdjust default false;
    { determine wheather in order to compensate, skipping steps is permitted }
    property LatencyCanSkipSteps: boolean read FLatencyCanSkipSteps write FLatencyCanSkipSteps default true;

    property StartFromCurrent: boolean read FStartFromCurrent write FStartFromCurrent default false;
    property Loop: boolean read FLoop write FLoop default false;
    property LoopInverse: boolean read FLoopInverse write FLoopInverse default false;
    property LoopDelay: boolean read FLoopDelay write FLoopDelay default false;

    property PropertyName: string read FPropertyName write FPropertyName;

    property Component: TComponent read FComponent write FComponent;

    // Status
    property Percent: single read CalculatePercent;

    // Notify
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
    property OnLoop: TNotifyEvent read FOnLoop write FOnLoop;
    property OnStep: TAniStepEvent read FOnStep write FOnStep;

  public
    // Code Properties
    property Running: boolean read GetRunning write SetRunning;
    property Paused: boolean read GetPaused write SetPaused;
    property Status: FXTaskStatus read FStatus;

    // Constructors
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  FXIntAnim = class(FXAnimationController)
  private
    // Values
    FStartValue: integer;
    FEndValue: integer;

    FDelta: integer;
    FCurrentValue: integer;

    procedure ExecuteAnimation; override;
    procedure DoStepValue; override;

  published
    // Properties
    property StartValue: integer read FStartValue write FStartValue;
    property EndValue: integer read FEndValue write FEndValue;

    // Status
    property CurrentValue: integer read FCurrentValue;

  public
    // Constructors
    constructor Create(AOwner: TComponent); override;
  end;

  FXFloatAnim = class(FXAnimationController)
  private
    // Values
    FStartValue: real;
    FEndValue: real;

    FDelta: real;
    FCurrentValue: real;

    procedure ExecuteAnimation; override;
    procedure DoStepValue; override;

  published
    // Properties
    property StartValue: real read FStartValue write FStartValue;
    property EndValue: real read FEndValue write FEndValue;

    // Status
    property CurrentValue: real read FCurrentValue;

  public
    // Constructors
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{ FXAnimationController }

function FXAnimationController.CalculatePercent: single;
begin
  if not Running then
    Exit(1);

  Result := FStepValue / (FTotalStep-1);
end;

constructor FXAnimationController.Create(AOwner: TComponent);
begin
  // Inherit
  inherited;

  // Animation Kind
  ValueKinds := [];

  // Defaults
  FLoop := false;
  FLoopInverse := false;
  FStartFromCurrent := false;
  FStatus := FXTaskStatus.Stopped;

  FKind := FXAnimationKind.Linear;
  FSteps := 0;
  FDuration := 2;
  FDelay := 0;
  FDelayMaxSegment := 2;
  FInverse := false;

  FLatencyAdjust := false;
  FLatencyCanSkipSteps := true;
end;

procedure FXAnimationController.CreateThread;
begin
  if FThread = nil then
    FThread := TAnimationThread.Create(Self)
  else
    begin
      if FThread.Finished then
        FThread.Free;

      FThread := TAnimationThread.Create(Self);
    end;
end;

destructor FXAnimationController.Destroy;
begin
  // Free
  if (FThread <> nil) and FThread.Started and not FThread.Finished then
    begin
      FThread.Terminate;

      FThread.Free;
    end;

  // Inherit
  inherited;
end;

procedure FXAnimationController.DoStepValue;
begin
  // nothing
end;

procedure FXAnimationController.ExecuteAnimation;
label StartLoop;
var
  StartTime: TDateTime;
  SleepTime: cardinal;
begin
  // Sleep
  WaitDelay;

  // Terminated
  if FThread.CheckTerminated then
    Exit;

  // Notify
  if Assigned(FOnStart) then
    TThread.Synchronize(FThread, procedure
      begin
        FOnStart(Self);
      end);

  // Begin work
  StartLoop:
  StartTime := 0;
  FStepValue := 0;
  while FStepValue < FTotalStep do
    begin
      // Terminate
      if FThread.CheckTerminated then
        Exit;

      // Do step
      DoStepValue;

      // Compensate
      if FLatencyAdjust then
        StartTime := Now;

      // Notify
      if Assigned(FOnStep) then
        TThread.Synchronize(FThread, procedure
          begin
            FOnStep(Self, FStepValue, FTotalStep);
          end);

      // Stopped
      if FThread.CheckTerminated then
        Exit;

      // Sleep
      if (FStepValue < FTotalStep-1) and (FSleepStep > 0) then begin
        SleepTime := FSleepStep;
        if FLatencyAdjust then begin
          const CodeLatency = MillisecondsBetween(Now, StartTime);
          SleepTime := Max(0, SleepTime-CodeLatency);

          if FLatencyCanSkipSteps and (CodeLatency >= FSleepStep*2) then begin
            FStepValue := FStepValue + (CodeLatency div FSleepStep - 1);

            // Ensure the final step will execute
            if FStepValue >= FTotalStep-1 then
              FStepValue := FTotalStep-2;
          end;
        end;

        // Sleep
        Sleep(SleepTime);
      end;

      // Increase
      Inc(FStepValue);
    end;

  // Stopped
  if FThread.CheckTerminated then
    Exit;

  // Loop
  if Loop then
    begin
      if LoopDelay then
        WaitDelay;

      // Stopped
      if FThread.CheckTerminated then
        Exit;

      // Reset
      if FLoopInverse then
        Inverse := not Inverse;

      // Notify
      if Assigned(FOnLoop) then
        TThread.Synchronize(FThread, procedure
          begin
            FOnLoop(Self);
          end);

      goto StartLoop;
    end;

  // Done
  FStatus := FXTaskStatus.Stopped;

  // Notify
  if Assigned(FOnFinish) then
    TThread.Synchronize(FThread, procedure
      begin
        FOnFinish(Self);
      end);
end;

function FXAnimationController.GetPaused: boolean;
begin
  Result := FStatus = FXTaskStatus.Paused;
end;

function FXAnimationController.GetRunning: boolean;
begin
  Result := FStatus = FXTaskStatus.Running;
end;

function FXAnimationController.PropertyValid: boolean;
var
  O: TObject;
  N: string;
begin
  // Valid
  if (PropertyName = '') or (FComponent = nil) then
    Exit(false);

  O := FComponent;
  N := PropertyName;
  try
    GetRootInstance(O, N);
  except
    Exit(false);
  end;

  Result := (GetPropertyType(FComponent, PropertyName) in ValueKinds);
end;

procedure FXAnimationController.Restart;
begin
  if FStatus in [FXTaskStatus.Running, FXTaskStatus.Paused] then
    begin
      Stop;
      Start;
    end;
end;

procedure FXAnimationController.SetDelayMaxSegment(const Value: integer);
begin
  if Value >= 0 then
    FDelayMaxSegment := Value;
end;

procedure FXAnimationController.SetDuration(const Value: single);
begin
  if Value >= 0 then
    FDuration := Value;
end;

procedure FXAnimationController.SetPaused(const Value: boolean);
begin
  if FStatus in [FXTaskStatus.Running, FXTaskStatus.Paused] then
    begin
      if GetPaused <> Value then
        // Suspend thread
        FThread.Suspended := Value;

      if Value then
        FStatus := FXTaskStatus.Paused
      else
        FStatus := FXTaskStatus.Running;
    end;
end;

procedure FXAnimationController.SetRunning(const Value: boolean);
begin
  if Value <> Running then
    begin
      if Value then
        Start
      else
        Stop;
    end;
end;

procedure FXAnimationController.SetSteps(const Value: integer);
begin
  if Value >= 0 then
    FSteps := Value;
end;

procedure FXAnimationController.Start;
begin
  if FStatus = FXTaskStatus.Stopped then
    begin
      // Component Based
      ComponentBased := (Component <> nil);
      if not ComponentBased and StartFromCurrent then
        Exit;

      // Check valid
      if ComponentBased and not PropertyValid then
        Exit;

      // Build values
      FStepValue := 0;
      if Steps > 0 then
        begin
          FTotalStep := Max(Steps, 2); // Step 0 is considered
          FSleepStep := Max(trunc(Duration*1000 / FTotalStep), 1);
        end
      else
      if Duration = 0 then
        begin
          FTotalStep := 2;
          FSleepStep := 1;
        end
      else
        begin
          FTotalStep := Max(round(FDuration * 100), 2);
          FSleepStep := 10;
        end;

      // Thread
      CreateThread;

      FStatus := FXTaskStatus.Running;
      FThread.Start;
    end;
end;

procedure FXAnimationController.StartInverse;
begin
  if FStatus = FXTaskStatus.Stopped then
    begin
      FInverse := not FInverse;
      Start;
    end;
end;

procedure FXAnimationController.Stop;
begin
  if FStatus in [FXTaskStatus.Running, FXTaskStatus.Paused] then
    begin
      FThread.Terminate;
      //FThread.WaitFor;

      FStatus := FXTaskStatus.Stopped;
    end;
end;

procedure FXAnimationController.WaitDelay;
var
  I: integer;
  Time: integer;
  Segment: integer;
  Count: integer;
begin
  // Calculate segment
  Time := round(Delay * 1000);
  for I := 2 downto 0 do begin
    Segment := trunc(Power(10, I));
    if Time mod Segment = 0 then
      break;
  end;

  // Calculate repeatcount
  Count := Time div Segment;

  // Sleep for interval
  for I := 1 to Count do begin
    Sleep(Segment);

    if FThread.CheckTerminated then
        Exit;
  end;
end;

{ TAnimationThread }

constructor TAnimationThread.Create(AAnim: FXAnimationController);
begin
  inherited Create(true);
  FAnimation := AAnim;
end;

procedure TAnimationThread.Execute;
begin
  inherited;
  FAnimation.ExecuteAnimation;
end;

{ FXIntAnimation }

constructor FXIntAnim.Create(AOwner: TComponent);
begin
  inherited;
  ValueKinds := [tkInteger, tkInt64];
end;

procedure FXIntAnim.DoStepValue;
var
  AStart, ADelta: integer;
begin
  inherited;
  // Inverse
  if Inverse then
    begin
      AStart := FEndValue;
      ADelta := -FDelta;
    end
  else
    begin
      AStart := FStartValue;
      ADelta := FDelta;
    end;

  // Calc
  FCurrentValue := AStart + trunc(CalculateAnimationValue(FKind, FStepValue, FTotalStep, ADelta));

  // Set
  if ComponentBased then
    TThread.Synchronize(nil, procedure
      begin
        SetPropertyValue(FComponent, PropertyName, FCurrentValue);
      end);
end;

procedure FXIntAnim.ExecuteAnimation;
begin
  if StartFromCurrent then
    FStartValue := GetPropertyValue(FComponent, PropertyName);
  FDelta := FEndValue - FStartValue;

  // Start
  inherited;
end;

{ FXFloatAnim }

constructor FXFloatAnim.Create(AOwner: TComponent);
begin
  inherited;
  ValueKinds := [tkFloat];
end;

procedure FXFloatAnim.DoStepValue;
var
  AStart, ADelta: real;
begin
  inherited;

  // Inverse
  if Inverse then
    begin
      AStart := FEndValue;
      ADelta := -FDelta;
    end
  else
    begin
      AStart := FStartValue;
      ADelta := FDelta;
    end;

  // Calc
  FCurrentValue := AStart + CalculateAnimationValue(FKind, FStepValue, FTotalStep, ADelta);

  // Set
  if ComponentBased then
    TThread.Synchronize(nil, procedure
      begin
        SetPropertyValue(FComponent, PropertyName, FCurrentValue);
      end);
end;

procedure FXFloatAnim.ExecuteAnimation;
begin
  if StartFromCurrent then
    FStartValue := GetPropertyValue(FComponent, PropertyName);
  FDelta := FEndValue - FStartValue;

  // Start
  inherited;
end;

end.
