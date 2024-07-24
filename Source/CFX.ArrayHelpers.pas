{***********************************************************}
{                  Codruts Variabile Helpers                }
{                                                           }
{                        version 1.0                        }
{                           ALPHA                           }
{                                                           }
{              https://www.codrutsoft.com/                  }
{             Copyright 2024 Codrut Software                }
{    This unit is licensed for usage under a MIT license    }
{                                                           }
{***********************************************************}

{$SCOPEDENUMS ON}

unit CFX.ArrayHelpers;

interface
uses
  System.SysUtils, System.Classes,
  System.Generics.Collections, System.Generics.Defaults;

type
  TMultiSwitch<T> = class
    type
    TCase = record
      Values: TArray<T>;
      CallBack: TProc;

      procedure Execute;
    end;

    // Make
    class function Option(Value: T; Call: TProc): TCase; overload;
    class function Option(Values: TArray<T>; Call: TProc): TCase; overload;

    // Switch
    class procedure Switch(Value: T; Cases: TArray<TCase>); overload;
    class procedure Switch(Value: T; Cases: TArray<TCase>; Default: TProc); overload;
  end;

  /// Note about internal errors
  ///  This class uses lComparer to compare values because some value types,
  ///  such as record cannot be directly compared and would give the
  ///  "Invalid operand type" error, but since this class is type based,
  ///  a internal error would appear instead.

  // TArray colection
  TArrayUtils<T> = class
  public
    // Callback types
    type
    TArrayEachCallback = reference to procedure(var Element: T);
    TArrayEachCallbackConst = reference to procedure(Element: T);
    TArrayDualCallback = reference to function(A, B: T): boolean;
    TArrayIndexCallback = reference to function(Index: integer): T;

    /// <summary> Verify if the array contains element x. </summary>
    class function Build(const Length: integer; Callback: TArrayIndexCallback): TArray<T>;

    /// <summary> Verify if the array contains element x. </summary>
    class function Contains(const x: T; const Values: TArray<T>): boolean;
    /// <summary> Compares is two arrays are equal. </summary>
    class function CheckEquality(const First, Second: TArray<T>) : boolean;

    /// <summary> Get the index if element x. </summary>
    class function GetIndex(const x: T; const Values: TArray<T>): integer;
    /// <summary> Go trough all elements of an array and get their value. </summary>
    class procedure ForEach(const Values: TArray<T>; Callback: TArrayEachCallbackConst); overload;
    /// <summary> Go trough all elements of an array and modify their value. </summary>
    class procedure ForEach(var Values: TArray<T>; Callback: TArrayEachCallback); overload;
    /// <summary> Sort the elements of an array using the provided callback for comparison. </summary>
    class procedure Sort(var Values: TArray<T>; Callback: TArrayDualCallback); overload;

    /// <summary> Move one item from It's index to another item's index and moving that one uppwards. </summary>
    class procedure Move(var Values: TArray<T>; const Source, Destination: integer); overload;
    /// <summary> Switch places for two items. </summary>
    class procedure Switch(var Values: TArray<T>; const Source, Destination: integer); overload;

    /// <summary> Add blank value to the end of the array. </summary>
    class function AddValue(var Values: TArray<T>) : integer; overload;
    /// <summary> Add value to the end of the array. </summary>
    class function AddValue(const x: T; var Values: TArray<T>) : integer; overload;
    /// <summary> Add value to the end of the array. </summary>
    class procedure AddValues(const Values: TArray<T>; var Destination: TArray<T>);
    /// <summary> Concat secondary array to primary array </summary>
    class function Concat(const Primary, Secondary: TArray<T>) : TArray<T>;
    /// <summary> Insert empty value at the specified index into the array. </summary>
    class procedure Insert(const Index: integer; var Values: TArray<T>); overload;
    /// <summary> Insert value at the specified index into the array. </summary>
    class procedure Insert(const Index: integer; const x: T; var Values: TArray<T>); overload;

    /// <summary> Delete element by index from array. </summary>
    class procedure Delete(const Index: integer; var Values: TArray<T>);
    /// <summary> Delete element by type T from array. </summary>
    class procedure DeleteElement(const Element: T; var Values: TArray<T>);
    /// <summary> Set length to specifieed value. </summary>
    ///
    class procedure SetLength(const Length: integer; var Values: TArray<T>);
    /// <summary> Get array length. </summary>
    class function Count(const Values: TArray<T>) : integer;
  end;


implementation

{ TArrayUtils<T> }

class function TArrayUtils<T>.AddValue(const x: T;
  var Values: TArray<T>): integer;
begin
  Result := AddValue(Values);
  Values[Result] := x;
end;

class function TArrayUtils<T>.AddValue(var Values: TArray<T>): integer;
begin
  System.SetLength(Values, length(Values)+1);

  Result := High(Values);
end;

class procedure TArrayUtils<T>.AddValues(const Values: TArray<T>;
  var Destination: TArray<T>);
begin
  const StartIndex = High(Destination)+1;
  System.SetLength(Destination, length(Destination)+length(Values));

  const LowPoint = Low(Values);
  for var I := LowPoint to High(Values) do
    Destination[StartIndex+I-LowPoint] := Values[I];
end;

class function TArrayUtils<T>.Build(const Length: integer;
  Callback: TArrayIndexCallback): TArray<T>;
begin
  System.SetLength(Result, Length);
  for var I := 0 to Length-1 do
    Result[I] := Callback(I);
end;

class function TArrayUtils<T>.Concat(const Primary,
  Secondary: TArray<T>): TArray<T>;
begin
  Result := Primary;

  AddValues(Secondary, Result);
end;

class function TArrayUtils<T>.Contains(const x: T; const Values: TArray<T>): boolean;
var
  y : T;
  lComparer: IEqualityComparer<T>;
begin
  lComparer := TEqualityComparer<T>.Default;
  for y in Values do
  begin
    if lComparer.Equals(x, y) then
      Exit(True);
  end;
  Exit(False);
end;

class function TArrayUtils<T>.Count(const Values: TArray<T>): integer;
begin
  Result := Length(Values);
end;

class procedure TArrayUtils<T>.Delete(const Index: integer;
  var Values: TArray<T>);
begin
  if Index = -1 then
    Exit;

  for var I := Index to High(Values)-1 do
    Values[I] := Values[I+1];

  System.SetLength(Values, Length(Values)-1);
end;

class procedure TArrayUtils<T>.DeleteElement(const Element: T;
  var Values: TArray<T>);
begin
  const Index = GetIndex(Element, Values);
  if Index <> -1 then
    Delete(Index, Values);
end;

class function TArrayUtils<T>.CheckEquality(const First, Second: TArray<T>): boolean;
var
  lComparer: IEqualityComparer<T>;
begin
  Result := true;
  lComparer := TEqualityComparer<T>.Default;

  if Length(First) <> Length(Second) then
    Exit(false);
  const Count = Length(First);
  for var I := 0 to Count-1 do
    if not lComparer.Equals(First[I], Second[I]) then
      Exit(false);
end;

class procedure TArrayUtils<T>.ForEach(var Values: TArray<T>;
  Callback: TArrayEachCallback);
begin
  for var I := Low(Values) to High(Values) do
    Callback( Values[I] );
end;

class procedure TArrayUtils<T>.ForEach(const Values: TArray<T>;
  Callback: TArrayEachCallbackConst);
var
  y : T;
begin
  for y in Values do
    Callback(y);
end;

class function TArrayUtils<T>.GetIndex(const x: T; const Values: TArray<T>): integer;
var
  I: Integer;
  y: T;
  lComparer: IEqualityComparer<T>;
begin
  lComparer := TEqualityComparer<T>.Default;
  for I := Low(Values) to High(Values) do
    begin
      y := Values[I];

      if lComparer.Equals(x, y) then
        Exit(I);
    end;
    Exit(-1);
end;

class procedure TArrayUtils<T>.Insert(const Index: integer;
  var Values: TArray<T>);
var
  Size: integer;
  I: Integer;
begin
  System.SetLength(Values, Length(Values)+1);
  Size := High(Values);

  for I := Size downto Index+1 do
    Values[I] := Values[I-1];
end;

class procedure TArrayUtils<T>.Insert(const Index: integer; const x: T;
  var Values: TArray<T>);
begin
  Insert(Index, Values);

  // Set
  Values[Index] := x;
end;

class procedure TArrayUtils<T>.Move(var Values: TArray<T>; const Source,
  Destination: integer);
var
  I: integer;
begin
  const OriginalItem = Values[Source];

  // Move all items
  if Source < Destination then begin
    for I := Source to Destination-1 do
      Values[I] := Values[I+1];
  end else begin
    for I := Source downto Destination+1 do
      Values[I] := Values[I-1];
  end;

  // Item
  Values[Destination] := OriginalItem;
end;

class procedure TArrayUtils<T>.SetLength(const Length: integer;
  var Values: TArray<T>);
begin
  System.SetLength(Values, Length);
end;

class procedure TArrayUtils<T>.Sort(var Values: TArray<T>;
  Callback: TArrayDualCallback);
var
  Stack: TArray<Integer >;
  ALow, AHigh, i, j, PivotIndex: Integer;
  Pivot, Temp: T;
begin
  if Length(Values) <= 1 then
    Exit;

  // Initialize the stack for iterative QuickSort
  System.SetLength(Stack, Length(Values) * 2);
  ALow := 0;
  AHigh := High(Values);

  Stack[0] := ALow;
  Stack[1] := AHigh;
  PivotIndex := 2;

  while PivotIndex > 0 do
  begin
    // Pop Low and High from stack
    Dec(PivotIndex);
    AHigh := Stack[PivotIndex];
    Dec(PivotIndex);
    ALow := Stack[PivotIndex];

    // Partition the array
    Pivot := Values[(ALow + AHigh) div 2];
    i := ALow;
    j := AHigh;
    while i <= j do
    begin
      while Callback(Pivot, Values[i]) do
        Inc(i);
      while Callback(Values[j], Pivot) do
        Dec(j);
      if i <= j then
      begin
        Temp := Values[i];
        Values[i] := Values[j];
        Values[j] := Temp;
        Inc(i);
        Dec(j);
      end;
    end;

    // Push sub-arrays onto stack
    if ALow < j then
    begin
      Stack[PivotIndex] := ALow;
      Inc(PivotIndex);
      Stack[PivotIndex] := j;
      Inc(PivotIndex);
    end;
    if i < AHigh then
    begin
      Stack[PivotIndex] := i;
      Inc(PivotIndex);
      Stack[PivotIndex] := AHigh;
      Inc(PivotIndex);
    end;
  end;
end;

class procedure TArrayUtils<T>.Switch(var Values: TArray<T>; const Source,
  Destination: integer);
begin
  const OriginalItem = Values[Source];
  Values[Source] := Values[Destination];
  Values[Destination] := OriginalItem;
end;

{ TMultiSwitch<T> }

class function TMultiSwitch<T>.Option(Value: T; Call: TProc): TCase;
begin
  Result := Option([Value], Call);
end;

class function TMultiSwitch<T>.Option(Values: TArray<T>; Call: TProc): TCase;
begin
  Result.Values := Values;
  Result.CallBack := Call;
end;

class procedure TMultiSwitch<T>.Switch(Value: T; Cases: TArray<TCase>; Default: TProc);
begin
  for var I := 0 to High(Cases) do
    if TArrayUtils<T>.Contains(Value, Cases[I].Values) then begin
      Cases[I].Execute;
      Exit;
    end;

  // Default
  if Assigned(Default) then
    Default;
end;

class procedure TMultiSwitch<T>.Switch(Value: T; Cases: TArray<TCase>);
begin
  Switch(Value, Cases, nil);
end;

{ TMultiSwitch<T>.TCase }

procedure TMultiSwitch<T>.TCase.Execute;
begin
  Callback;
end;

end.
