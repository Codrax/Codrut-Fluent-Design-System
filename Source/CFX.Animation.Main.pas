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
    Windows, Messages, SysUtils, Variants, Classes, Math, CFX.Types;

  type
    // Async animations
    FXAsyncAnim = class
    private
      FFreeOnFinish: boolean;

      // Data
      FDelay: single;
      FDuration: Single;

      // Properties
      FSteps: integer;
      FStatus: FXTaskStatus;

      FKind: FXAnimationKind;

      FInverse: boolean;

      FOnStart,
      FOnStep,
      FOnFinish: TProc;

      // Animation Tick
      FStepValue: integer;
      FTotalStep: integer;
      FSleepStep: integer;

      FCanceled: boolean;

      // Getters
      function GetPaused: boolean;
      function GetRunning: boolean;

      // Setters
      procedure SetDuration(const Value: single);
      procedure SetSteps(const Value: integer);
      procedure SetRunning(const Value: boolean);
      procedure SetPaused(const Value: boolean);

      procedure WaitDelay;
      procedure ExecuteAnimation; virtual;
      procedure DoStepValue; virtual;
      function CalculatePercent: single;

    public
      // Start
      procedure Start;

      // Task
      procedure Stop;

      // Properties
      property FreeOnFinish: boolean read FFreeOnFinish write FFreeOnFinish;
      property Delay: single read FDelay write FDelay;
      property Duration: single read FDuration write SetDuration;

      property Kind: FXAnimationKind read FKind write FKind;
      property Inverse: boolean read FInverse write FInverse default false;

      property Steps: integer read FSteps write SetSteps;

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
    FXAnimationKind.Linear: Result := Step / StepCount * Delta;
    FXAnimationKind.Exponential: Result := sign(Delta) * (Power(abs(Delta)+1, Step / StepCount)-1);
    FXAnimationKind.ReverseExpo: Result := Delta - sign(Delta) * (Power(abs(Delta)+1, 1-Step / StepCount)-1);
    FXAnimationKind.Random: Result := RandomRange(0, StepCount+1) / StepCount * Delta;
    FXAnimationKind.Spring: begin
      const ASign = Sign(Delta);
      const X = StepCount / 5;
      const D = Delta / 5;
      const T = (D+Delta)*ASign;

      if Step >= X then
        Result := -D + ASign * T- Power(abs(T), 1-(Step-X)/(StepCount-X))
      else
        Result := -D + ASign *  Power(abs(D), 1-Step / X);
    end;
    FXAnimationKind.Sinus: Result := sin(((Step / StepCount)/2)*pi) * Delta;
    FXAnimationKind.SinusArc: begin
      const X = Step / StepCount;
      if X <= 0.5 then
        Result := sin(X*pi)/2 * Delta
      else
        Result := (sin((X+1)*pi)/2+1) * Delta;
    end;
    FXAnimationKind.Wobbly: Result := sin(((Step / StepCount)*2)*pi) * Delta;

    // Non END value animations
    FXAnimationKind.Pulse: Result := sin((Step / StepCount)*pi) * Delta;

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

  FInverse := false;
  FStatus := FXTaskStatus.Stopped;
  FKind := FXAnimationKind.Linear;

  FSteps := 0;

  FDuration := 2;
  FDelay := 0;
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
begin
  // Sleep
  WaitDelay;

  // Notify
  if Assigned(FOnStart) then
    FOnStart();

  // Begin work
  while FStepValue <= FTotalStep do
    begin
      // Terminate
      if FCanceled then
        Exit;

      // Paused
      if FStatus = FXTaskStatus.Paused then
        continue;

      // Draw
      DoStepValue;

      // Notify
      if Assigned(FOnStep) then
        FOnStep();

      // Sleep
      Sleep(FSleepStep);

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

procedure FXAsyncAnim.SetDuration(const Value: single);
begin
  if Value*1000 > 1 then
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
          FTotalStep := Max(Steps-1, 1); // Step 0 is considered
          FSleepStep := Max(trunc(Duration*1000 / FTotalStep), 1);
        end
      else
        begin
          FTotalStep := round(FDuration * 100);
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
begin
  Sleep( trunc(FDelay * 1000) );
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
