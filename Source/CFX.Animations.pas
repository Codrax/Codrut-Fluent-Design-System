{***********************************************************}
{                 Codruts Animation Library                 }
{                                                           }
{                         version 1                         }
{                                                           }
{ !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! }
{                                                           }
{                        !DISCLAIMER!                       }
{                                                           }
{           <!> This is a fork of AniVCL by Vuio <!>        }
{             https://github.com/VuioVuio/AniVCL            }
{                                                           }
{ !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! }
{                                                           }
{                                                           }
{                                                           }
{                   -- WORK IN PROGRESS --                  }
{***********************************************************}

unit CFX.Animations;


interface

uses
  System.Classes, System.SysUtils, System.Threading;

type
  TAniSyncProc = reference to procedure (V: Integer);
  TAniDoneProc = reference to procedure;  //  TProc

  TAniFloatSyncProc = reference to procedure (V: Single);

  TAniFunction = reference to function (P: Single): Single;

  TAniKind = (akIn, akOut, akInOut);
  TAniFunctionKind = (
    afkLinear, afkQuadratic, afkCubic, afkQuartic, afkQuintic,
    afkBack, afkBounce, afkExpo, afkSine, afkCircle
    );

  TIntAniSet = class(TPersistent)
    private
      FAniKind: TAniKind;
      FAniFunctionKind: TAniFunctionKind;
      FDelayStartTime: Cardinal;
      FDuration: Cardinal;
      FStep: Cardinal;
    public
      constructor Create;
      procedure Assign(Source: TPersistent); override;
      procedure QuickAssign(AniKind: TAniKind; AniFunctionKind: TAniFunctionKind;
        Delay, Duration, Step: Cardinal);
    published
      property AniKind: TAniKind read FAniKind write FAniKind;
      property AniFunctionKind: TAniFunctionKind read FAniFunctionKind write FAniFunctionKind;
      property DelayStartTime: Cardinal read FDelayStartTime write FDelayStartTime;
      property Duration: Cardinal read FDuration write FDuration;
      property Step: Cardinal read FStep write FStep;
  end;

  TIntAni = class(TThread)
    var CurrentValue: Integer;

    private
      var FStep: Byte;
      function GetEndValue: Integer;
      function GetRunning: boolean;
      procedure SetEndValue(const Value: Integer);
      var AniFunction: TAniFunction;

      FOnSync: TAniSyncProc;
      FOnDone: TAniDoneProc;

      FAniKind: TAniKind;
      FAniFunctionKind: TAniFunctionKind;

      FDelayStartTime: Cardinal;
      FDuration: Cardinal;
      FStartValue: Integer;
      FDeltaValue: Integer;
      FPercent: real;

      function UpdateFunction: Boolean;
      procedure UpdateControl;
      procedure DoneControl;

    protected
      procedure Execute; override;

    public
      constructor Create; overload;
      constructor Create(
        FreeOnFinish: Boolean;
        aAniKind: TAniKind; aAniFunctionKind: TAniFunctionKind;
        aStartValue, aDeltaValue: Integer;
        aSyncProc: TAniSyncProc; aDoneProc: TAniDoneProc); overload;

      //  Events
      property OnSync: TAniSyncProc read FOnSync write FOnSync;
      property OnDone: TAniDoneProc read FOnDone write FOnDone;

      //  Props
      property Running: boolean read GetRunning;

      property Step: Byte read FStep write FStep default 25;
      property AniKind: TAniKind read FAniKind write FAniKind default akIn;
      property AniFunctionKind: TAniFunctionKind read FAniFunctionKind write FAniFunctionKind default afkLinear;

      property DelayStartTime: Cardinal read FDelayStartTime write FDelayStartTime default 0;
      property Duration: Cardinal read FDuration write FDuration default 400;
      property StartValue: Integer read FStartValue write FStartValue default 0;
      property EndValue: Integer read GetEndValue write SetEndValue;
      property DeltaValue: Integer read FDeltaValue write FDeltaValue default 0;

      // Read Only
      property Percent: real read FPercent;
  end;

  TFloatAni = class(TThread)
    var CurrentValue: Single;

    private
      var FStep: Byte;
      var AniFunction: TAniFunction;

      FOnSync: TAniFloatSyncProc;
      FOnDone: TAniDoneProc;

      FAniKind: TAniKind;
      FAniFunctionKind: TAniFunctionKind;

      FDelayStartTime: Cardinal;
      FDuration: Cardinal;
      FStartValue: Single;
      FDeltaValue: Integer;

      FPercent: real;

      function UpdateFunction: Boolean;
      procedure UpdateControl;
      procedure DoneControl;

    protected
      procedure Execute; override;

    public
      constructor Create(
        FreeOnFinish: Boolean;
        aAniKind: TAniKind; aAniFunctionKind: TAniFunctionKind;
        aStartValue, aDeltaValue: Integer;
        aSyncProc: TAniFloatSyncProc);

      //  Events
      property OnSync: TAniFloatSyncProc read FOnSync write FOnSync;
      property OnDone: TAniDoneProc read FOnDone write FOnDone;

      //  Props
      property Step: Byte read FStep write FStep default 25;
      property AniKind: TAniKind read FAniKind write FAniKind default akIn;
      property AniFunctionKind: TAniFunctionKind read FAniFunctionKind write FAniFunctionKind default afkLinear;

      property DelayStartTime: Cardinal read FDelayStartTime write FDelayStartTime default 0;
      property Duration: Cardinal read FDuration write FDuration default 400;
      property StartValue: Single read FStartValue write FStartValue;
      property DeltaValue: Integer read FDeltaValue write FDeltaValue default 0;

      // Read Only
      property Percent: real read FPercent;
  end;

implementation

uses
  System.Math,
  CFX.AnimColection;

{ SPECIAL }

function TIntAni.UpdateFunction: Boolean;
begin
  Result := true;
  case AniKind of
    akIn:
      case AniFunctionKind of
        afkLinear:
          AniFunction := TIntAniCollection.Linear;
        afkQuadratic:
          AniFunction := TIntAniCollection.Quadratic_In;
        afkCubic:
          AniFunction := TIntAniCollection.Cubic_In;
        afkQuartic:
          AniFunction := TIntAniCollection.Quartic_In;
        afkQuintic:
          AniFunction := TIntAniCollection.Quintic_In;
        afkBack:
          AniFunction := TIntAniCollection.Back_In;
        afkBounce:
          AniFunction := TIntAniCollection.Bounce_In;
        afkExpo:
          AniFunction := TIntAniCollection.Expo_In;
        afkSine:
          AniFunction := TIntAniCollection.Sine_In;
        afkCircle:
          AniFunction := TIntAniCollection.Circle_In;
        else
          Result := false;
      end;

    akOut:
      case AniFunctionKind of
        afkLinear:
          AniFunction := TIntAniCollection.Linear;
        afkQuadratic:
          AniFunction := TIntAniCollection.Quadratic_Out;
        afkCubic:
          AniFunction := TIntAniCollection.Cubic_Out;
        afkQuartic:
          AniFunction := TIntAniCollection.Quartic_Out;
        afkQuintic:
          AniFunction := TIntAniCollection.Quintic_Out;
        afkBack:
          AniFunction := TIntAniCollection.Back_Out;
        afkBounce:
          AniFunction := TIntAniCollection.Bounce_Out;
        afkExpo:
          AniFunction := TIntAniCollection.Expo_Out;
        afkSine:
          AniFunction := TIntAniCollection.Sine_Out;
        afkCircle:
          AniFunction := TIntAniCollection.Circle_Out;
        else
          Result := false;
      end;

    akInOut:
      case AniFunctionKind of
        afkLinear:
          AniFunction := TIntAniCollection.Linear;
        afkQuadratic:
          AniFunction := TIntAniCollection.Quadratic_InOut;
        afkCubic:
          AniFunction := TIntAniCollection.Cubic_InOut;
        afkQuartic:
          AniFunction := TIntAniCollection.Quartic_InOut;
        afkQuintic:
          AniFunction := TIntAniCollection.Quintic_InOut;
        afkBack:
          AniFunction := TIntAniCollection.Back_InOut;
        afkBounce:
          AniFunction := TIntAniCollection.Bounce_InOut;
        afkExpo:
          AniFunction := TIntAniCollection.Expo_InOut;
        afkSine:
          AniFunction := TIntAniCollection.Sine_InOut;
        afkCircle:
          AniFunction := TIntAniCollection.Circle_InOut;
        else
          Result := false;
      end;

    else
      Result := false;
  end;
end;

{ MAIN CLASS }

constructor TIntAni.Create(
  FreeOnFinish: Boolean;
  aAniKind: TAniKind; aAniFunctionKind: TAniFunctionKind;
  aStartValue, aDeltaValue: Integer;
  aSyncProc: TAniSyncProc; aDoneProc: TAniDoneProc);
begin
  inherited Create(True);

  //  Internal
  CurrentValue := 0;
  AniFunction := nil;

  //  New props
  FStep := 25;
  FAniKind := aAniKind;
  FAniFunctionKind := aAniFunctionKind;

  FDelayStartTime := 0;
  FDuration := 400;
  FStartValue := aStartValue;
  FDeltaValue := aDeltaValue;

  FOnDone := aDoneProc;
  FOnSync := aSyncProc;

  //  Old props
  FreeOnTerminate := FreeOnFinish;

  UpdateFunction;
end;

procedure TIntAni.Execute;
var
  i: Cardinal;
  t, d, TimePerStep: Cardinal;
  b, c: Integer;
begin
  if UpdateFunction = false then exit;
    ///  Update easing function
    ///  Depend on AniKind (In, Out,...) and AniFunctionKind (Linear,...)
    ///  If Result = false (error found), then exit

  d := Duration;
  b := StartValue;
  c := DeltaValue;

  //  Delay start
  Sleep(DelayStartTime);

  //  Calc step by FPS
  TimePerStep := Round(d / Step);

  //  Run
  for i := 1 to Step do
    begin
      t := i * TimePerStep;
      FPercent := t / d;
      CurrentValue := b + Round(c * AniFunction(t / d));
      Synchronize(UpdateControl);
      Sleep(TimePerStep);
    end;

  //  Finish
  Synchronize(UpdateControl);
  Synchronize(DoneControl);
end;

function TIntAni.GetEndValue: Integer;
begin
  Result := FStartValue + FDeltaValue;
end;

function TIntAni.GetRunning: boolean;
begin
  Result := Started and not Finished;
end;

procedure TIntAni.SetEndValue(const Value: Integer);
begin
  DeltaValue := Value - StartValue;
end;

{ PROCS }

constructor TIntAni.Create;
begin
  inherited Create(true);

  //  Internal
  CurrentValue := 0;
  AniFunction := nil;

  //  New props
  FStep := 25;
  FAniKind := akIn;
  FAniFunctionKind := afkLinear;

  FDelayStartTime := 0;
  FDuration := 400;
  FStartValue := 0;
  FDeltaValue := 0;

  //  Old props
  FreeOnTerminate := False;

  UpdateFunction;
end;

procedure TIntAni.DoneControl;
begin
  if Assigned(FOnDone) then
    FOnDone();
end;

procedure TIntAni.UpdateControl;
begin
  if Assigned(FOnSync) then
    FOnSync(CurrentValue);
end;

{ TFloatAni }

function TFloatAni.UpdateFunction: Boolean;
begin
  Result := true;
  case AniKind of
    akIn:
      case AniFunctionKind of
        afkLinear:
          AniFunction := TIntAniCollection.Linear;
        afkQuadratic:
          AniFunction := TIntAniCollection.Quadratic_In;
        afkCubic:
          AniFunction := TIntAniCollection.Cubic_In;
        afkQuartic:
          AniFunction := TIntAniCollection.Quartic_In;
        afkQuintic:
          AniFunction := TIntAniCollection.Quintic_In;
        afkBack:
          AniFunction := TIntAniCollection.Back_In;
        afkBounce:
          AniFunction := TIntAniCollection.Bounce_In;
        afkExpo:
          AniFunction := TIntAniCollection.Expo_In;
        afkSine:
          AniFunction := TIntAniCollection.Sine_In;
        afkCircle:
          AniFunction := TIntAniCollection.Circle_In;
        else
          Result := false;
      end;

    akOut:
      case AniFunctionKind of
        afkLinear:
          AniFunction := TIntAniCollection.Linear;
        afkQuadratic:
          AniFunction := TIntAniCollection.Quadratic_Out;
        afkCubic:
          AniFunction := TIntAniCollection.Cubic_Out;
        afkQuartic:
          AniFunction := TIntAniCollection.Quartic_Out;
        afkQuintic:
          AniFunction := TIntAniCollection.Quintic_Out;
        afkBack:
          AniFunction := TIntAniCollection.Back_Out;
        afkBounce:
          AniFunction := TIntAniCollection.Bounce_Out;
        afkExpo:
          AniFunction := TIntAniCollection.Expo_Out;
        afkSine:
          AniFunction := TIntAniCollection.Sine_Out;
        afkCircle:
          AniFunction := TIntAniCollection.Circle_Out;
        else
          Result := false;
      end;

    akInOut:
      case AniFunctionKind of
        afkLinear:
          AniFunction := TIntAniCollection.Linear;
        afkQuadratic:
          AniFunction := TIntAniCollection.Quadratic_InOut;
        afkCubic:
          AniFunction := TIntAniCollection.Cubic_InOut;
        afkQuartic:
          AniFunction := TIntAniCollection.Quartic_InOut;
        afkQuintic:
          AniFunction := TIntAniCollection.Quintic_InOut;
        afkBack:
          AniFunction := TIntAniCollection.Back_InOut;
        afkBounce:
          AniFunction := TIntAniCollection.Bounce_InOut;
        afkExpo:
          AniFunction := TIntAniCollection.Expo_InOut;
        afkSine:
          AniFunction := TIntAniCollection.Sine_InOut;
        afkCircle:
          AniFunction := TIntAniCollection.Circle_InOut;
        else
          Result := false;
      end;

    else
      Result := false;
  end;
end;

{ MAIN CLASS }

constructor TFloatAni.Create(
  FreeOnFinish: Boolean;
  aAniKind: TAniKind; aAniFunctionKind: TAniFunctionKind;
  aStartValue, aDeltaValue: Integer;
  aSyncProc: TAniFloatSyncProc);
begin
  inherited Create(True);

  //  Internal
  CurrentValue := 0;
  AniFunction := nil;

  //  New props
  FStep := 25;
  FAniKind := aAniKind;
  FAniFunctionKind := aAniFunctionKind;

  FDelayStartTime := 0;
  FDuration := 400;
  FStartValue := aStartValue;
  FDeltaValue := aDeltaValue;

  FOnDone := nil;
  FOnSync := aSyncProc;

  //  Old props
  FreeOnTerminate := FreeOnFinish;

  UpdateFunction;
end;

procedure TFloatAni.Execute;
var
  i: Cardinal;
  t, d, TimePerStep: Cardinal;
  c: Integer;
  b: Single;
begin
  if UpdateFunction = false then exit;
    ///  Update easing function
    ///  Depend on AniKind (In, Out,...) and AniFunctionKind (Linear,...)
    ///  If Result = false (error found), then exit

  d := Duration;
  b := StartValue;
  c := DeltaValue;

  //  Delay start
  Sleep(DelayStartTime);

  //  Calc step by FPS
  TimePerStep := Round(d / Step);

  //  Run
  for i := 1 to Step do
    begin
      t := i * TimePerStep;
      FPercent := t / d;
      CurrentValue := b + Round(c * AniFunction(t / d));
      Synchronize(UpdateControl);
      Sleep(TimePerStep);
    end;

  //  Finish
  Synchronize(UpdateControl);
  Synchronize(DoneControl);
end;

{ PROCS }

procedure TFloatAni.DoneControl;
begin
  if Assigned(FOnDone) then
    FOnDone();
end;

procedure TFloatAni.UpdateControl;
begin
  if Assigned(FOnSync) then
    FOnSync(CurrentValue);
end;

{ TIntAniSet }

procedure TIntAniSet.Assign(Source: TPersistent);
begin
    if Source is TIntAniSet then
    begin
      FAniKind := (Source as TIntAniSet).AniKind;
      FAniFunctionKind := (Source as TIntAniSet).AniFunctionKind;
      FDelayStartTime := (Source as TIntAniSet).DelayStartTime;
      FDuration := (Source as TIntAniSet).Duration;
      FStep := (Source as TIntAniSet).Step;
    end
  else
    inherited;
end;

constructor TIntAniSet.Create;
begin
  inherited Create;

  FAniKind := akOut;
  FAniFunctionKind := afkLinear;
  FDelayStartTime := 0;
  FDuration := 200;
  FStep := 20;
end;

procedure TIntAniSet.QuickAssign(AniKind: TAniKind;
  AniFunctionKind: TAniFunctionKind; Delay, Duration, Step: Cardinal);
begin
  FAniKind := AniKind;
  FAniFunctionKind := AniFunctionKind;
  FDelayStartTime := Delay;
  FDuration := Duration;
  FStep := Step;
end;

end.
