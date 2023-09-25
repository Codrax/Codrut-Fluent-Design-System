unit CFX.ArrayHelpers;

interface

uses
  Types, SysUtils, System.Generics.Collections;

type
    // TArray generic types
    TArrayArrayHelper = record helper for TArray<TArray>
    public
      function Count: integer; overload; inline;
      procedure SetToLength(ALength: integer);
    end;

    TStringArrayHelper = record helper for TArray<string>
    public
      function AddValue(Value: string): integer;
      procedure Insert(Index: integer; Value: string);
      procedure Delete(Index: integer);
      function Count: integer; overload; inline;
      function Find(Value: string): integer;
      procedure SetToLength(ALength: integer);
    end;

    TIntegerArrayHelper = record helper for TArray<integer>
    public
      function AddValue(Value: integer): integer;
      procedure Insert(Index: integer; Value: integer);
      procedure Delete(Index: integer);
      function Count: integer; overload; inline;
      function Find(Value: integer): integer;
      procedure SetToLength(ALength: integer);
    end;

    TRealArrayHelper = record helper for TArray<real>
    public
      function AddValue(Value: real): integer;
      procedure Insert(Index: integer; Value: real);
      procedure Delete(Index: integer);
      function Count: integer; overload; inline;
      function Find(Value: real): integer;
      procedure SetToLength(ALength: integer);
    end;

implementation

function TArrayArrayHelper.Count: integer;
begin
  Result := length(Self);
end;

function TStringArrayHelper.Count: integer;
begin
  Result := length(Self);
end;

function TIntegerArrayHelper.Count: integer;
begin
  Result := length(Self);
end;

function TRealArrayHelper.Count: integer;
begin
  Result := length(Self);
end;

procedure TArrayArrayHelper.SetToLength(ALength: integer);
begin
  SetLength(Self, ALength);
end;

procedure TStringArrayHelper.SetToLength(ALength: integer);
begin
  SetLength(Self, ALength);
end;

procedure TIntegerArrayHelper.SetToLength(ALength: integer);
begin
  SetLength(Self, ALength);
end;

procedure TRealArrayHelper.SetToLength(ALength: integer);
begin
  SetLength(Self, ALength);
end;

function TStringArrayHelper.AddValue(Value: string): integer;
var
  AIndex: integer;
begin
  AIndex := Length(Self);
  SetLength(Self, AIndex + 1);
  Self[AIndex] := Value;
  Result := AIndex;
end;

function TIntegerArrayHelper.AddValue(Value: integer): integer;
var
  AIndex: integer;
begin
  AIndex := Length(Self);
  SetLength(Self, AIndex + 1);
  Self[AIndex] := Value;
  Result := AIndex;
end;

function TRealArrayHelper.AddValue(Value: real): integer;
var
  AIndex: integer;
begin
  AIndex := Length(Self);
  SetLength(Self, AIndex + 1);
  Self[AIndex] := Value;
  Result := AIndex;
end;

procedure TStringArrayHelper.Insert(Index: integer; Value: string);
var
  Size: integer;
  I: Integer;
begin
  Size := Length(Self);
  SetLength(Self, Size+1);

  for I := Size downto Index+1 do
    Self[I] := Self[I-1];
  Self[Index] := Value;
end;

procedure TIntegerArrayHelper.Insert(Index: integer; Value: integer);
var
  Size: integer;
  I: Integer;
begin
  Size := Length(Self);
  SetLength(Self, Size+1);

  for I := Size downto Index+1 do
    Self[I] := Self[I-1];
  Self[Index] := Value;
end;

procedure TRealArrayHelper.Insert(Index: integer; Value: real);
var
  Size: integer;
  I: Integer;
begin
  Size := Length(Self);
  SetLength(Self, Size+1);

  for I := Size downto Index+1 do
    Self[I] := Self[I-1];
  Self[Index] := Value;
end;

procedure TStringArrayHelper.Delete(Index: integer);
var
  I: Integer;
begin
  if Index <> -1 then
    begin
      for I := Index to High(Self)-1 do
        Self[I] := Self[I+1];

      SetToLength(Length(Self)-1);
    end;
end;

procedure TIntegerArrayHelper.Delete(Index: integer);
var
  I: Integer;
begin
  if Index <> -1 then
    begin
      for I := Index to High(Self)-1 do
        Self[I] := Self[I+1];

      SetToLength(Length(Self)-1);
    end;
end;

procedure TRealArrayHelper.Delete(Index: integer);
var
  I: Integer;
begin
  if Index <> -1 then
    begin
      for I := Index to High(Self)-1 do
        Self[I] := Self[I+1];

      SetToLength(Length(Self)-1);
    end;
end;

function TStringArrayHelper.Find(Value: string): integer;
var
  I: integer;
begin
  Result := -1;
  for I := Low(Self) to High(Self) do
    if Self[I] = Value then
      Exit(I);
end;

function TIntegerArrayHelper.Find(Value: integer): integer;
var
  I: integer;
begin
  Result := -1;
  for I := Low(Self) to High(Self) do
    if Self[I] = Value then
      Exit(I);
end;

function TRealArrayHelper.Find(Value: real): integer;
var
  I: integer;
begin
  Result := -1;
  for I := Low(Self) to High(Self) do
    if Self[I] = Value then
      Exit(I);
end;

end.

