unit CFX.Math;

interface

  function PercOf(number: integer; percentage: integer): integer;
  function PercOfR(number: Real; percentage: integer): real;

  function EqualApprox(number1, number2: integer; span: real = 1): boolean; overload;
  function EqualApprox(number1, number2: real; span: real = 1): boolean; overload;

implementation

function PercOf(number: integer; percentage: integer): integer;
begin
  Result := trunc(percentage / 100 * number);
end;

function PercOfR(number: Real; percentage: integer): real;
begin
  Result := percentage / 100 * number;
end;

function EqualApprox(number1, number2: integer; span: real): boolean;
begin
  if (number1 <= number2 + span) and (number1 >= number2 - span) then
    Result := true
  else
    Result := false;
end;

function EqualApprox(number1, number2: real; span: real): boolean;
begin
  if (number1 <= number2 + span) and (number1 >= number2 - span) then
    Result := true
  else
    Result := false;
end;

end.
