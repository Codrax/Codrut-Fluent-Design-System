unit CFX.StringUtils;

interface
  uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Math;

  type
    TStringFindFlag = (sffIgnoreCase, sffFoundOnce, sffFoundMultiple);
    TStringFindFlags = set of TStringFindFlag;

  // Upper String, Lower string
  function SuperStr(nr: string): string;
  function SubStr(nr: string): string;

  // String Func
  function GetAllSeparatorItems(str: string; separators: TArray<string>): TArray<string>; overload;
  function GetAllSeparatorItems(str: string; separator: string = ','): TArray<string>; overload;
  function GenerateString(strlength: integer; letters: boolean = true;
                          capitalization: boolean = true; numbers: boolean = true;
                          symbols: boolean = true): string;


  // String Alterations
  function StrCopy(MainString: string; frompos, topos: integer; justcontent: boolean = false): string;
  function StrRemove(MainString: string; frompos, topos: integer): string;
  function StrReplZone(MainString: string; frompos, topos: integer; ReplaceWith: string): string;
  function StrInsert(MainString: string; AtPos: integer; InsertText: string): string;
  function StrFirst(MainString: string; Count: integer = 1): string;
  function StrClearUntilDifferent(MainString: string; ReplaceChar: char): string;
  function StringClearLineBreaks(MainString: string): string;

  // String Search
  function StrCount(SubString: string; MainString: string; Flags: TStringFindFlags = []): integer;
  function StrPos(SubString: string; MainString: string; index: integer = 1; offset: integer = 0; Flags: TStringFindFlags = []): integer;
  function InString(SubString, MainString: string; Flags: TStringFindFlags = []): boolean;

  // String List
  procedure InsertStListInStList(insertindex: integer; SubStrList: TStringList; var ParentStringList: TStringList);
  function StringToStringList(str: string; Separator: string = #13): TStringList;
  function StringToArray(str: string; Separator: string = #13): TArray<string>;
  function StringListToString(stringlist: TStringList; Separator: string = #13): string;
  procedure ArrayToStringList(AArray: TArray<string>; var StringList: TStringList);
  function ArrayToString(AArray: TArray<string>; Separator: string = ', '): string;

const
  allchars = ['0'..'9', 'a'..'z', 'A'..'Z', '~', '!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '_', '-', '=', '+', '[', ']', '{', '}', ';', ':', '"', '\', '|', '<', '>', ',', '.', '/', '?', ' '];
  symbolchars : TArray<String> = ['~', '!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '_', '-', '=', '+', '[', ']', '{', '}', ';', ':', '"', '\', '|', '<', '>', ',', '.', '/', '?'];
  superspr : TArray<String> = ['⁰','¹','²','³','⁴','⁵','⁶','⁷','⁸','⁹','⁺','⁻','⁼','⁽','⁾', '⁄','ᵃ', 'ᵇ', 'ᶜ', 'ᵈ', 'ᵉ', 'ᶠ', 'ᵍ', 'ʰ', 'ⁱ', 'ʲ', 'ᵏ', 'ˡ', 'ᵐ', 'ⁿ', 'ᵒ', 'ᵖ', 'q', 'ʳ', 'ˢ', 'ᵗ', 'ᵘ', 'ᵛ', 'ʷ', 'ˣ', 'ʸ', 'ᶻ', 'ᴬ', 'ᴮ', 'C', 'ᴰ', 'ᴱ', 'F', 'ᴳ', 'ᴴ', 'ᴵ', 'ᴶ', 'ᴷ', 'ᴸ', 'ᴹ', 'ᴺ', 'ᴼ', 'ᴾ', 'Q', 'ᴿ', 'S', 'ᵀ', 'ᵁ', 'ⱽ', 'ᵂ', 'X', 'Y', 'Z'];
  subspr : TArray<String> = ['₀','₁','₂','₃','₄','₅','₆','₇','₈','₉','+','-','=','(',')', '⁄', 'ₐ', 'b', 'c', 'd', 'ₑ', 'f', 'g', 'ₕ', 'ᵢ', 'j', 'ₖ', 'ₗ', 'ₘ', 'ₙ', 'ₒ', 'ₚ', 'q', 'ᵣ', 'ₛ', 'ₜ', 'ᵤ', 'ᵥ', 'w', 'ₓ', 'y', 'z', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'];

implementation

function GenerateString(strlength: integer; letters, capitalization,
  numbers, symbols: boolean): string;
var
  I, A: Integer;
  F: string;
begin
  for I := 1 to strlength do
  begin
    Randomize();

    F := '';

    if (NOT letters) and (NOT numbers) and (NOT symbols) then
      Exit;


    repeat
      A := Random(3);

      case A of
        0: if letters then
            F := Chr(ord('a') + Random(26));
        1: if numbers then
            F := inttostr(randomrange(0,9));
        2: if symbols then
            F := symbolchars[Random(length(symbolchars))];
      end;

    until (F <> '');

    if (Random(2) = 1) and capitalization then
      F := ANSIUpperCase(F);

    Result := Result + F;
  end;
end;

function SuperStr(nr: string): string;
var
  I: Integer;
begin
  for I := 1 to Length ( nr ) do
    case nr[I] of
      ' ': Result := Result + ' ';
      '0': Result := Result + superspr[0];
      '1': Result := Result + superspr[1];
      '2': Result := Result + superspr[2];
      '3': Result := Result + superspr[3];
      '4': Result := Result + superspr[4];
      '5': Result := Result + superspr[5];
      '6': Result := Result + superspr[6];
      '7': Result := Result + superspr[7];
      '8': Result := Result + superspr[8];
      '9': Result := Result + superspr[9];
      '+': Result := Result + superspr[10];
      '-': Result := Result + superspr[11];
      '=': Result := Result + superspr[12];
      '(': Result := Result + superspr[13];
      ')': Result := Result + superspr[14];
      '/': Result := Result + superspr[15];
      'a': Result := Result + superspr[16];
      'b': Result := Result + superspr[17];
      'c': Result := Result + superspr[18];
      'd': Result := Result + superspr[19];
      'e': Result := Result + superspr[20];
      'f': Result := Result + superspr[21];
      'g': Result := Result + superspr[22];
      'h': Result := Result + superspr[23];
      'i': Result := Result + superspr[24];
      'j': Result := Result + superspr[25];
      'k': Result := Result + superspr[26];
      'l': Result := Result + superspr[27];
      'm': Result := Result + superspr[28];
      'n': Result := Result + superspr[29];
      'o': Result := Result + superspr[30];
      'p': Result := Result + superspr[31];
      'q': Result := Result + superspr[32];
      'r': Result := Result + superspr[33];
      's': Result := Result + superspr[34];
      't': Result := Result + superspr[35];
      'u': Result := Result + superspr[36];
      'v': Result := Result + superspr[37];
      'w': Result := Result + superspr[38];
      'x': Result := Result + superspr[39];
      'y': Result := Result + superspr[40];
      'z': Result := Result + superspr[41];
      'A': Result := Result + superspr[42];
      'B': Result := Result + superspr[43];
      'C': Result := Result + superspr[44];
      'D': Result := Result + superspr[45];
      'E': Result := Result + superspr[46];
      'F': Result := Result + superspr[47];
      'G': Result := Result + superspr[48];
      'H': Result := Result + superspr[49];
      'I': Result := Result + superspr[50];
      'J': Result := Result + superspr[51];
      'K': Result := Result + superspr[52];
      'L': Result := Result + superspr[53];
      'M': Result := Result + superspr[54];
      'N': Result := Result + superspr[55];
      'O': Result := Result + superspr[56];
      'P': Result := Result + superspr[57];
      'Q': Result := Result + superspr[58];
      'R': Result := Result + superspr[59];
      'S': Result := Result + superspr[60];
      'T': Result := Result + superspr[61];
      'U': Result := Result + superspr[62];
      'V': Result := Result + superspr[63];
      'W': Result := Result + superspr[64];
      'X': Result := Result + superspr[65];
      'Y': Result := Result + superspr[66];
      'Z': Result := Result + superspr[67];
    end;
end;

function SubStr(nr: string): string;
var
  I: Integer;
begin
  for I := 1 to Length ( nr ) do
    case nr[I] of
      ' ': Result := Result + ' ';
      '0': Result := Result + subspr[0];
      '1': Result := Result + subspr[1];
      '2': Result := Result + subspr[2];
      '3': Result := Result + subspr[3];
      '4': Result := Result + subspr[4];
      '5': Result := Result + subspr[5];
      '6': Result := Result + subspr[6];
      '7': Result := Result + subspr[7];
      '8': Result := Result + subspr[8];
      '9': Result := Result + subspr[9];
      '+': Result := Result + subspr[10];
      '-': Result := Result + subspr[11];
      '=': Result := Result + subspr[12];
      '(': Result := Result + subspr[13];
      ')': Result := Result + subspr[14];
      '/': Result := Result + subspr[15];
      'a': Result := Result + subspr[16];
      'b': Result := Result + subspr[17];
      'c': Result := Result + subspr[18];
      'd': Result := Result + subspr[19];
      'e': Result := Result + subspr[20];
      'f': Result := Result + subspr[21];
      'g': Result := Result + subspr[22];
      'h': Result := Result + subspr[23];
      'i': Result := Result + subspr[24];
      'j': Result := Result + subspr[25];
      'k': Result := Result + subspr[26];
      'l': Result := Result + subspr[27];
      'm': Result := Result + subspr[28];
      'n': Result := Result + subspr[29];
      'o': Result := Result + subspr[30];
      'p': Result := Result + subspr[31];
      'q': Result := Result + subspr[32];
      'r': Result := Result + subspr[33];
      's': Result := Result + subspr[34];
      't': Result := Result + subspr[35];
      'u': Result := Result + subspr[36];
      'v': Result := Result + subspr[37];
      'w': Result := Result + subspr[38];
      'x': Result := Result + subspr[39];
      'y': Result := Result + subspr[40];
      'z': Result := Result + subspr[41];
      'A': Result := Result + subspr[42];
      'B': Result := Result + subspr[43];
      'C': Result := Result + subspr[44];
      'D': Result := Result + subspr[45];
      'E': Result := Result + subspr[46];
      'F': Result := Result + subspr[47];
      'G': Result := Result + subspr[48];
      'H': Result := Result + subspr[49];
      'I': Result := Result + subspr[50];
      'J': Result := Result + subspr[51];
      'K': Result := Result + subspr[52];
      'L': Result := Result + subspr[53];
      'M': Result := Result + subspr[54];
      'N': Result := Result + subspr[55];
      'O': Result := Result + subspr[56];
      'P': Result := Result + subspr[57];
      'Q': Result := Result + subspr[58];
      'R': Result := Result + subspr[59];
      'S': Result := Result + subspr[60];
      'T': Result := Result + subspr[61];
      'U': Result := Result + subspr[62];
      'V': Result := Result + subspr[63];
      'W': Result := Result + subspr[64];
      'X': Result := Result + subspr[65];
      'Y': Result := Result + subspr[66];
      'Z': Result := Result + subspr[67];
    end;
end;

function GetAllSeparatorItems(str: string; separators: TArray<string>): TArray<string>;
var
  P, N, I, SeparLength: integer;
begin
  SetLength(Result, 0);

  N := -1;
  repeat
    // Item 0
    P := Pos(separators[0], str);
    SeparLength := length( separators[0] );

    for I := 1 to High( separators ) do
      begin
        N := Pos(separators[I], str);
        if (N > 0) and (N < P) then
          begin
            P := N;
            SeparLength := length( separators[I] );
          end;
      end;

    // Exit
    if P = 0 then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[N + 1] := str;

        Break;
      end;

    // Add
    N := Length( Result );
    SetLength(Result, N + 1);

    Result[N] := StrCopy(str, 0, P - 1);

    // Delete
    str := StrRemove(str, 0, P + SeparLength - 1);
  until P = 0;
end;

function GetAllSeparatorItems(str: string; separator: string = ','): TArray<string>; overload;
begin
  Result := GetAllSeparatorItems( str, [separator]);
end;


function StrRemove(MainString: string; frompos, topos: integer): string;
begin
  Result := Copy(mainstring, 1, frompos - 1)
    + Copy(mainstring, topos + 1, length(mainstring));
end;

function StrReplZone(MainString: string; frompos, topos: integer; ReplaceWith: string): string;
begin
    Result := Copy(mainstring, 1, frompos - 1) + ReplaceWith
    + Copy(mainstring, topos + 1, length(mainstring));
end;

function StrInsert(MainString: string; AtPos: integer; InsertText: string): string;
begin
  Result := Copy(mainstring, 1, AtPos - 1) + InsertText
    + Copy(mainstring, AtPos, length(mainstring));
end;

function StrFirst(MainString: string; Count: integer): string;
begin
  Result := Copy( MainString, 1, Count );
end;

function StrClearUntilDifferent(MainString: string; ReplaceChar: char): string;
var
  I: integer;
  Total: integer;
begin
  Result := MainString;

  Total := Length(Result);
  for I := 1 to Total do
    if Result[1] = ReplaceChar then
      Result := Copy(Result, 2, Length(Result)-1)
    else
      Break;
end;

function StringClearLineBreaks(MainString: string): string;
begin
  Result := MainString.Replace(#13, '').Replace(#$D, '').Replace(#$A, '');
end;

function StrCount(SubString: string; MainString: string; Flags: TStringFindFlags): integer;
var
  P: integer;
begin
  // Flags
  if sffIgnoreCase in Flags then
    begin
      MainString := AnsiLowerCase( MainString );
      SubString := AnsiLowerCase( SubString );
    end;

  // Find
  Result := 0;
  while Pos(substring, mainstring) <> 0 do
    begin
      P := Pos(substring, mainstring);
      mainstring := Copy(mainstring, P + 1, length(mainstring) );

      inc(Result);
    end;
end;

function StrPos(SubString: string; MainString: string; index: integer; offset: integer; Flags: TStringFindFlags): integer;
var
  I, L, offs: Integer;
begin
  // Flags
  if sffIgnoreCase in Flags then
    begin
      MainString := AnsiLowerCase( MainString );
      SubString := AnsiLowerCase( SubString );
    end;

  // Prepare
  Result := 0;
  L := 0;

  // Find by index
  for I := 1 to index do
  begin
    offs := Result + 1;
    if I = 1 then
      offs := offs + offset;

    Result := Pos(substring, mainstring, offs);

    if Result < L then
    begin
      Break;
      Result := 0;
    end;

    L := Result;
  end;
end;

function InString(SubString, MainString: string; Flags: TStringFindFlags = []): boolean;
var
  Found, CPos: integer;
begin
  if sffIgnoreCase in Flags then
    begin
      substring := AnsiLowerCase(substring);
      substring := AnsiLowerCase(substring);
    end;

  // Get Count
  Found := 0;
  CPos := 0;
  repeat
    CPos := Pos(SubString, MainString, CPos + 1);

    if CPos <> 0 then
      Inc(Found)

  until (CPos = 0) or not ((sffFoundOnce in Flags) or (sffFoundMultiple in Flags));

  // Flags Search
  if not ((sffFoundOnce in Flags) or (sffFoundMultiple in Flags)) then
    Result := Found <> 0
  else
    begin
      if sffFoundOnce in Flags then
        Result := Found = 1
      else
        Result := Found > 1;
    end;
end;

function StrCopy(MainString: string; frompos, topos: integer; justcontent: boolean): string;
begin
  if justcontent then
    begin
      frompos := frompos + 1;
      topos := topos - 1;
    end;
  if frompos < 1 then
    frompos := 1;
  Result := Copy(mainstring, frompos, topos - frompos + 1);
end;

procedure InsertStListInStList(insertindex: integer; SubStrList: TStringList; var ParentStringList: TStringList);
var
  I: Integer;
begin
  for I := 0 to SubStrList.Count - 1 do
    ParentStringList.Insert(insertindex + I, SubStrList[I]);
end;

function StringToStringList(str: string; Separator: string): TStringList;
var
  P: integer;
begin
  Result := TStringList.Create;

  // Empty
  if str = '' then
    Exit;

  // Get each item
  repeat
    P := Pos(Separator, str);

    if P = 0 then
      begin
        Result.Add(str);

        Break;
      end;

    Result.Add( StrCopy(str, 0, P - 1) );

    str := StrRemove(str, 0, P);
  until P = 0;
end;

function StringToArray(str: string; Separator: string = #13): TArray<string>;
procedure AddItem(AItem: string);
var
  AIndex: integer;
begin
  AIndex := Length(Result);
  SetLength(Result, AIndex + 1);

  Result[AIndex] := AItem;
end;
var
  P: integer;
begin
  SetLength(Result, 0);

  // Empty
  if str = '' then
    Exit;

  // Get each item
  repeat
    P := Pos(Separator, str);

    if P = 0 then
      begin
        AddItem(str);

        Break;
      end;

    AddItem( StrCopy(str, 0, P - 1) );

    str := StrRemove(str, 0, P);
  until P = 0;

end;

function StringListToString(stringlist: TStringList; Separator: string): string;
var
  I: Integer;
begin
  for I := 0 to stringlist.Count - 1 do
    begin
      Result := Result + stringlist[I];

      if I < stringlist.Count - 1 then
        Result := Result + Separator;
    end;
end;

procedure ArrayToStringList(AArray: TArray<string>; var StringList: TStringList);
var
  I: Integer;
begin
  StringList.Clear;
  for I := 0 to length( AArray ) - 1 do
    StringList.Add( AArray[I] );
end;

function ArrayToString(AArray: TArray<string>; Separator: string): string;
var
  I: Integer;
begin
  if Length(AArray) = 0 then
    Exit('');

  for I := 0 to High( AArray ) - 1 do
    Result := Result + AArray[I] + Separator;

  Result := Result + AArray[High( AArray )];
end;

end.
