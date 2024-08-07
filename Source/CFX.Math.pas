unit CFX.Math;

interface

uses
  SysUtils;

function PercOf(number: integer; percentage: integer): integer;
function PercOfR(number: Real; percentage: integer): real;

function EqualApprox(number1, number2: integer; span: real = 1): boolean; overload;
function EqualApprox(number1, number2: real; span: real = 1): boolean; overload;

function IntToStrIncludePrefixZeros(Value: integer; NumbersCount: integer): string;

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

function IntToStrIncludePrefixZeros(Value: integer; NumbersCount: integer): string;
var
  ResLength: integer;
  I: Integer;
begin
  Result := IntToStr( abs(Value) );

  ResLength := Length( Result );
  if ResLength < NumbersCount then
    begin
      for I := 1 to NumbersCount - ResLength do
        Result := '0' + Result;

      if Value < 0 then
        Result := '-' + Result;
    end;
end;

end.
