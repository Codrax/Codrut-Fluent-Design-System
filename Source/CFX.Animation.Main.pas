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

unit CFX.Animation.Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Math, CFX.Types, DateUtils;

type
  // Async animations
  FXAsyncAnim = class
  private
    FFreeOnFinish: boolean;

    // Data
    FKind: FXAnimationKind;
    FDelay: single;
    { The maximum exponent of 10 to use as a sleep interval, eg: 1 = 10ms }
    FDelayMaxSegment: integer;
    FDuration: Single;
    FInverse: boolean;

    // Runtime
    FSteps: integer;
    FStatus: FXTaskStatus;
    FCanceled: boolean;

    // Latency
    FLatencyAdjust: boolean;
    FLatencyCanSkipSteps: boolean;

    // Notify events
    FOnStart,
    FOnStep,
    FOnFinish: TProc;

    // Animation Tick
    FStepValue: integer;
    FTotalStep: integer;
    FSleepStep: integer;

    // System
    procedure WaitDelay;
    procedure ExecuteAnimation; virtual;
    procedure DoStepValue; virtual;
    function CalculatePercent: single;

    // Getters
    function GetPaused: boolean;
    function GetRunning: boolean;

    // Setters
    procedure SetDuration(const Value: single);
    procedure SetSteps(const Value: integer);
    procedure SetRunning(const Value: boolean);
    procedure SetPaused(const Value: boolean);
    procedure SetDelayMaxSegment(const Value: integer);

  public
    // Start
    procedure Start;

    // Task
    procedure Stop;

    // Properties
    property FreeOnFinish: boolean read FFreeOnFinish write FFreeOnFinish;
    property Delay: single read FDelay write FDelay;
    property DelayMaxSegment: integer read FDelayMaxSegment write SetDelayMaxSegment default 2;
    property Duration: single read FDuration write SetDuration;

    property Kind: FXAnimationKind read FKind write FKind;
    property Inverse: boolean read FInverse write FInverse default false;

    property Steps: integer read FSteps write SetSteps;

    { compensate for the time needed to execute the code }
    property LatencyAdjustments: boolean read FLatencyAdjust write FLatencyAdjust default false;
    { determine wheather in order to compensate, skipping steps is permitted }
    property LatencyCanSkipSteps: boolean read FLatencyCanSkipSteps write FLatencyCanSkipSteps default true;

    // Status
    property Percent: single read CalculatePercent;

    // Code Properties
    property Running: boolean read GetRunning write SetRunning;
    property Paused: boolean read GetPaused write SetPaused;
    property Status: FXTaskStatus read FStatus;

    // Notify
    property OnStart: TProc read FOnStart write FOnStart;
    property OnFinish: TProc read FOnFinish write FOnFinish;
    property OnStep: TProc read FOnStep write FOnStep;

    // Constructors
    constructor Create;
    destructor Destroy; override;
  end;

  FXAsyncIntAnim = class(FXAsyncAnim)
  type TValueCall = reference to procedure(Value: integer);
  private
    // Values
    FStartValue: integer;
    FEndValue: integer;

    FDelta: integer;
    FCurrentValue: integer;

    FOnValue: TValueCall;

    procedure ExecuteAnimation; override;
    procedure DoStepValue; override;

  public
    // Properties
    property StartValue: integer read FStartValue write FStartValue;
    property EndValue: integer read FEndValue write FEndValue;

    // Status
    property CurrentValue: integer read FCurrentValue;

    // Special event
    property OnValue: TValueCall read FOnValue write FOnValue;
  end;

  FXAsyncFloatAnim = class(FXAsyncAnim)
  type TValueCall = reference to procedure(Value: real);
  private
    // Values
    FStartValue: real;
    FEndValue: real;

    FDelta: real;
    FCurrentValue: real;

    FOnValue: TValueCall;

    procedure ExecuteAnimation; override;
    procedure DoStepValue; override;

  public
    // Properties
    property StartValue: real read FStartValue write FStartValue;
    property EndValue: real read FEndValue write FEndValue;

    // Status
    property CurrentValue: real read FCurrentValue;

    // Special event
    property OnValue: TValueCall read FOnValue write FOnValue;
  end;

// Equations
function CalculateAnimationValue(Kind: FXAnimationKind; Step, StepCount: integer; Delta: real): real;

implementation

function CalculateAnimationValue(Kind: FXAnimationKind; Step, StepCount: integer; Delta: real): real;
begin
  case Kind of
    FXAnimationKind.Linear: Result := Step / (StepCount-1) * Delta;
    FXAnimationKind.Exponential: Result := sign(Delta) * (Power(abs(Delta)+1, Step / (StepCount-1))-1);
    FXAnimationKind.ReverseExpo: Result := Delta - sign(Delta) * (Power(abs(Delta)+1, 1-Step / (StepCount-1))-1);
    FXAnimationKind.Random: Result := RandomRange(0, StepCount) / StepCount * Delta;
    FXAnimationKind.Spring: begin
      const ASign = Sign(Delta);
      const X = (StepCount-1) / 5;
      const D = Delta / 5;
      const T = (D+Delta)*ASign;

      if Step >= X then
        Result := -D + ASign * T- Power(abs(T), 1-(Step-X)/(StepCount-X))
      else
        Result := -D + ASign *  Power(abs(D), 1-Step / X);
    end;
    FXAnimationKind.Sinus: Result := sin(((Step / (StepCount-1))/2)*pi) * Delta;
    FXAnimationKind.SinusArc: begin
      const X = Step / (StepCount-1);
      if X <= 0.5 then
        Result := sin(X*pi)/2 * Delta
      else
        Result := (sin((X+1)*pi)/2+1) * Delta;
    end;
    FXAnimationKind.Wobbly: Result := sin(((Step / (StepCount-1))*2)*pi) * Delta;

    // Non END value animations
    FXAnimationKind.Pulse: Result := sin((Step / (StepCount-1))*pi) * Delta;

    else Result := 0;
  end;
end;

{ FXAsyncAnim }

function FXAsyncAnim.CalculatePercent: single;
begin
  if not Running then
    Exit(1);

  Result := FStepValue / Self.FTotalStep;
end;

constructor FXAsyncAnim.Create;
begin
  // Defaults
  FFreeOnFinish := false;

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

destructor FXAsyncAnim.Destroy;
begin

  inherited;
end;

procedure FXAsyncAnim.DoStepValue;
begin
  // nothing
end;

procedure FXAsyncAnim.ExecuteAnimation;
var
  StartTime: TDateTime;
  SleepTime: cardinal;
begin
  // Sleep
  WaitDelay;

  // Terminate
  if FCanceled then
    Exit;

  // Notify
  if Assigned(FOnStart) then
    FOnStart();

  // Begin work
  StartTime := 0;
  FStepValue := 0;
  while FStepValue < FTotalStep do
    begin
      // Terminate
      if FCanceled then
        Exit;

      // Paused
      if FStatus = FXTaskStatus.Paused then
        continue;

      // Do step
      DoStepValue;

      // Compensate
      if FLatencyAdjust then
        StartTime := Now;

      // Notify
      if Assigned(FOnStep) then
        FOnStep();

      // Stopped
      if FCanceled then
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
        Sleep(SleepTime);
      end;

      // Increase
      Inc(FStepValue);
    end;

  // Stopped
  if FCanceled then
    Exit;

  // Done
  FStatus := FXTaskStatus.Stopped;

  // Notify
  if Assigned(FOnFinish) then
    FOnFinish();

  // Free?
  if FFreeOnFinish  then
    Self.Free;
end;

function FXAsyncAnim.GetPaused: boolean;
begin
  Result := FStatus = FXTaskStatus.Paused;
end;

function FXAsyncAnim.GetRunning: boolean;
begin
  Result := FStatus = FXTaskStatus.Running;
end;

procedure FXAsyncAnim.SetDelayMaxSegment(const Value: integer);
begin
  if Value >= 0 then
    FDelayMaxSegment := Value;
end;

procedure FXAsyncAnim.SetDuration(const Value: single);
begin
  if Value >= 0 then
    FDuration := Value;
end;

procedure FXAsyncAnim.SetPaused(const Value: boolean);
begin
  if FStatus in [FXTaskStatus.Running, FXTaskStatus.Paused] then
    begin
      if Value then
        FStatus := FXTaskStatus.Paused
      else
        FStatus := FXTaskStatus.Running;
    end;
end;

procedure FXAsyncAnim.SetRunning(const Value: boolean);
begin
  if Value <> Running then
    begin
      if Value then
        Start
      else
        Stop;
    end;
end;

procedure FXAsyncAnim.SetSteps(const Value: integer);
begin
  if Value > 0 then
    FSteps := Value;
end;

procedure FXAsyncAnim.Start;
begin
  if FStatus = FXTaskStatus.Stopped then
    begin
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

      // Status
      FStatus := FXTaskStatus.Running;

      // Thread
      ExecuteAnimation;
    end;
end;

procedure FXAsyncAnim.Stop;
begin
  FCanceled := true;
end;

procedure FXAsyncAnim.WaitDelay;
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

    if FCanceled then
        Exit;
  end;
end;

{ FXAsyncIntAnim }

procedure FXAsyncIntAnim.DoStepValue;
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

  // Event
  if Assigned(FOnValue) then
    FOnValue(FCurrentValue);
end;

procedure FXAsyncIntAnim.ExecuteAnimation;
begin
  FDelta := FEndValue - FStartValue;

  // Start
  inherited;
end;

{ FXAsyncFloatAnim }

procedure FXAsyncFloatAnim.DoStepValue;
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

  // Event
  if Assigned(FOnValue) then
    FOnValue(FCurrentValue);
end;

procedure FXAsyncFloatAnim.ExecuteAnimation;
begin
  FDelta := FEndValue - FStartValue;

  // Start
  inherited;
end;

end.
