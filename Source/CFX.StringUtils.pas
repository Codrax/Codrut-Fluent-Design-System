unit CFX.StringUtils;

interface
uses
  System.SysUtils, System.Classes, Math;

type
  TStringFindFlag = (IgnoreCase, FoundOnce, FoundMultiple);
  TStringFindFlags = set of TStringFindFlag;

  TStrGenFlag = (UppercaseLetters, LowercaseLetters, Numbers, Symbols);
  TStrGenFlags = set of TStrGenFlag;

  TIDGenerator = class
  public
    class function GenerateSequence(Sequence: string; Length: integer): string;

    // Basic
    class function GenerateLetterNumber(Length: integer): string;
    class function GenerateHex(Length: integer): string;

    // Known
    class function GenerateUUID: string;
    class function GenerateGUID: string; // microsoft language for UUID
    class function GenerateCLSID: string; // microsoft language for UUID
  end;

// Upper String, Lower string
function SuperStr(nr: string): string;
function SubStr(nr: string): string;

// String Func
function GetAllSeparatorItems(str: string; separators: TArray<string>): TArray<string>; overload;
function GetAllSeparatorItems(str: string; separator: string = ','): TArray<string>; overload;
function GenerateStringSequence(Length: integer; Characters: string): string;
function GenerateString(Length: integer; Flags: TStrGenFlags): string;
function GenerateStringEx(strlength: integer; letters: boolean = true;
                        capitalization: boolean = true; numbers: boolean = true;
                        symbols: boolean = true): string;
function StringBuild(Length: integer; Character: char): string;

// String Alterations
function StrCopy(MainString: string; frompos, topos: integer; justcontent: boolean = false): string;
function StrRemove(MainString: string; frompos, topos: integer): string;
function StrReplZone(MainString: string; frompos, topos: integer; ReplaceWith: string): string;
function StrInsert(MainString: string; AtPos: integer; InsertText: string): string;
function StrFirst(MainString: string; Count: integer = 1): string;
function StrClearUntilDifferent(MainString: string; ReplaceChar: char): string;
function StringClearLineBreaks(MainString: string; ReplaceWith: string = ''): string;

// String Search
function StrCount(SubString: string; MainString: string; Flags: TStringFindFlags = []): integer;
function StrPos(SubString: string; MainString: string; index: integer = 1; offset: integer = 0; Flags: TStringFindFlags = []): integer;
function InString(SubString, MainString: string; Flags: TStringFindFlags = []): boolean;

// Search Utilities
/// <summary> Remove all known symbols from string. </summary>
function ClearStringSymbols(MainString: string): string;
/// <summary> Remove provided characters from string. </summary>
function StringRemoveCharacters(MainString: string; Characters: TArray<char>): string;
/// <summary> Return the first string which is not Empty. </summary>
function StringNullLess(Strings: TArray<string>): string; overload;
/// <summary> Return the first string which is not Empty. </summary>
function StringNullLess(First, Second: string): string; overload;

// String comparison
function DamerauLevenshteinDistance(const Str1, Str2: String): Integer;
function StringSimilarityRatio(const Str1, Str2: String; IgnoreCase: Boolean): Double; // 0-1

// String List
procedure InsertStListInStList(insertindex: integer; SubStrList: TStringList; var ParentStringList: TStringList);
function StringToStringList(str: string; Separator: string = #13): TStringList;
function StringToArray(str: string; Separator: string = #13): TArray<string>;
function StringListToString(stringlist: TStringList; Separator: string = #13): string;
function StringListToArray(stringlist: TStrings): TArray<string>;
procedure ArrayToStringList(AArray: TArray<string>; StringList: TStringList);
function ArrayToString(AArray: TArray<string>; Separator: string = #13): string;

const
  allchars = ['0'..'9', 'a'..'z', 'A'..'Z', '~', '!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '_', '-', '=', '+', '[', ']', '{', '}', ';', ':', '"', '\', '|', '<', '>', ',', '.', '/', '?', #39, '`', ' '];
  nrchars = ['0'..'9'];
  letterchars = ['a'..'z', 'A'..'Z'];
  symbolchars : TArray<String> = ['~', '!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '_', '-', '=', '+', '[', ']', '{', '}', ';', ':', '"', '\', '|', '<', '>', ',', '.', '/', '?', #39, '`'];
  superspr : TArray<String> = ['⁰','¹','²','³','⁴','⁵','⁶','⁷','⁸','⁹','⁺','⁻','⁼','⁽','⁾', '⁄','ᵃ', 'ᵇ', 'ᶜ', 'ᵈ', 'ᵉ', 'ᶠ', 'ᵍ', 'ʰ', 'ⁱ', 'ʲ', 'ᵏ', 'ˡ', 'ᵐ', 'ⁿ', 'ᵒ', 'ᵖ', 'q', 'ʳ', 'ˢ', 'ᵗ', 'ᵘ', 'ᵛ', 'ʷ', 'ˣ', 'ʸ', 'ᶻ', 'ᴬ', 'ᴮ', 'C', 'ᴰ', 'ᴱ', 'F', 'ᴳ', 'ᴴ', 'ᴵ', 'ᴶ', 'ᴷ', 'ᴸ', 'ᴹ', 'ᴺ', 'ᴼ', 'ᴾ', 'Q', 'ᴿ', 'S', 'ᵀ', 'ᵁ', 'ⱽ', 'ᵂ', 'X', 'Y', 'Z'];
  subspr : TArray<String> = ['₀','₁','₂','₃','₄','₅','₆','₇','₈','₉','+','-','=','(',')', '⁄', 'ₐ', 'b', 'c', 'd', 'ₑ', 'f', 'g', 'ₕ', 'ᵢ', 'j', 'ₖ', 'ₗ', 'ₘ', 'ₙ', 'ₒ', 'ₚ', 'q', 'ᵣ', 'ₛ', 'ₜ', 'ᵤ', 'ᵥ', 'w', 'ₓ', 'y', 'z', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z'];

implementation

function GenerateStringSequence(Length: integer; Characters: string): string;
var
  CharactersLength: integer;
begin
  CharactersLength := Characters.Length;

  // Generate
  SetLength(Result, Length);
  for var I := 1 to Length do begin
    Randomize;
    Result[I] := Characters[Random(CharactersLength)+1];
  end;
end;

function GenerateString(Length: integer; Flags: TStrGenFlags): string;
var
  Chars: string;
begin
  Chars := '';
  if TStrGenFlag.UppercaseLetters in Flags then
    Chars := Chars + 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  if TStrGenFlag.LowercaseLetters in Flags then
    Chars := Chars + 'abcdefghijklmnopqrstuvwxyz';
  if TStrGenFlag.Numbers in Flags then
    Chars := Chars + '0123456789';
  if TStrGenFlag.Symbols in Flags then
    Chars := Chars + '+-_!@#$%^&*/';

  Result := GenerateStringSequence(Length, Chars);
end;

function GenerateStringEx(strlength: integer; letters, capitalization,
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

function StringBuild(Length: integer; Character: char): string;
var
  I: integer;
begin
  Result := '';
  for I := 1 to Length do
    Result := Result + Character;
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

function StringClearLineBreaks(MainString, ReplaceWith: string): string;
begin
  Result := StringReplace( MainString.Replace(#13, '').Replace(#$D, ''), #$A, ReplaceWith, [rfReplaceAll] );
end;

function StrCount(SubString: string; MainString: string; Flags: TStringFindFlags): integer;
var
  P: integer;
begin
  // Flags
  if TStringFindFlag.IgnoreCase in Flags then
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
  if TStringFindFlag.IgnoreCase in Flags then
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
  if TStringFindFlag.IgnoreCase in Flags then
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

  until (CPos = 0) or not ((TStringFindFlag.FoundOnce in Flags) or (TStringFindFlag.FoundMultiple in Flags));

  // Flags Search
  if not ((TStringFindFlag.FoundOnce in Flags) or (TStringFindFlag.FoundMultiple in Flags)) then
    Result := Found <> 0
  else
    begin
      if TStringFindFlag.FoundOnce in Flags then
        Result := Found = 1
      else
        Result := Found > 1;
    end;
end;

function ClearStringSymbols(MainString: string): string;
var
  I: Integer;
begin
  Result := MainString;
  for I := 0 to High(SymbolChars) do
    Result := Result.Replace(SymbolChars[I], '')
end;

function StringRemoveCharacters(MainString: string; Characters: TArray<char>): string;
var
  I: Integer;
begin
  Result := MainString;
  for I := 0 to High(Characters) do
    Result := Result.Replace(Characters[I], '')
end;

function StringNullLess(Strings: TArray<string>): string;
begin
  for var I := 0 to High(Strings) do
    if not Strings[I].IsEmpty then
      Exit(Strings[I]);
  Exit('');
end;

function StringNullLess(First, Second: string): string; overload;
begin
  Result := StringNullLess([First, Second]);
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

function DamerauLevenshteinDistance(const Str1, Str2: String): Integer;

  function Min(const A, B, C: Integer): Integer;
  begin
    Result := A;
    if B < Result then
      Result := B;
    if C < Result then
      Result := C;
  end;

var
  LenStr1, LenStr2: Integer;
  I, J, T, Cost, PrevCost: Integer;
  pStr1, pStr2, S1, S2: PChar;
  D: PIntegerArray;
begin
  LenStr1 := Length(Str1);
  LenStr2 := Length(Str2);

  // save a bit memory by making the second index points to the shorter string
  if LenStr1 < LenStr2 then
  begin
    T := LenStr1;
    LenStr1 := LenStr2;
    LenStr2 := T;
    pStr1 := PChar(Str2);
    pStr2 := PChar(Str1);
  end
  else
  begin
    pStr1 := PChar(Str1);
    pStr2 := PChar(Str2);
  end;

  // bypass leading identical characters
  while (LenStr2 <> 0) and (pStr1^ = pStr2^) do
  begin
    Inc(pStr1);
    Inc(pStr2);
    Dec(LenStr1);
    Dec(LenStr2);
  end;

  // bypass trailing identical characters
  while (LenStr2 <> 0) and ((pStr1 + LenStr1 - 1)^ = (pStr2 + LenStr2 - 1)^) do
  begin
    Dec(LenStr1);
    Dec(LenStr2);
  end;

  // is the shorter string empty? so, the edit distance is length of the longer one
  if LenStr2 = 0 then
  begin
    Result := LenStr1;
    Exit;
  end;

  // calculate the edit distance
  GetMem(D, (LenStr2 + 1) * SizeOf(Integer));

  for I := 0 to LenStr2 do
    D[I] := I;

  S1 := pStr1;
  for I := 1 to LenStr1 do
  begin
    PrevCost := I - 1;
    Cost := I;
    S2 := pStr2;
    for J := 1 to LenStr2 do
    begin
      if (S1^ = S2^) or ((I > 1) and (J > 1) and (S1^ = (S2 - 1)^) and (S2^ = (S1 - 1)^)) then
        Cost := PrevCost
      else
        Cost := 1 + Min(Cost, PrevCost, D[J]);
      PrevCost := D[J];
      D[J] := Cost;
      Inc(S2);
    end;
    Inc(S1);
  end;
  Result := D[LenStr2];
  FreeMem(D);
end;

function StringSimilarityRatio(const Str1, Str2: String; IgnoreCase: Boolean): Double;
var
  MaxLen: Integer;
  Distance: Integer;
begin
  Result := 1.0;
  if Length(Str1) > Length(Str2) then
    MaxLen := Length(Str1)
  else
    MaxLen := Length(Str2);
  if MaxLen <> 0 then
  begin
    if IgnoreCase then
      Distance := DamerauLevenshteinDistance(LowerCase(Str1), LowerCase(Str2))
    else
      Distance := DamerauLevenshteinDistance(Str1, Str2);
    Result := Result - (Distance / MaxLen);
  end;
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

function StringListToArray(stringlist: TStrings): TArray<string>;
var
  I: Integer;
begin
  SetLength(Result, StringList.Count);
  for I := 0 to StringList.Count-1 do
    Result[I] := StringList[I];
end;

procedure ArrayToStringList(AArray: TArray<string>; StringList: TStringList);
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

{ TIDGenerator }

class function TIDGenerator.GenerateCLSID: string;
begin
  Result := GenerateGUID;
end;

class function TIDGenerator.GenerateGUID: string;
begin
  Result := TGUID.NewGuid.ToString;
end;

class function TIDGenerator.GenerateHex(Length: integer): string;
begin
  Result := GenerateStringSequence(Length, '0123456789abcdef');
end;

class function TIDGenerator.GenerateLetterNumber(Length: integer): string;
begin
  Result := GenerateStringSequence(Length, 'abcdefghijklmnopqrstuvwxyz0123456789');
end;

class function TIDGenerator.GenerateSequence(Sequence: string; Length: integer): string;
begin
  Result := GenerateStringSequence(Length, Sequence);
end;

class function TIDGenerator.GenerateUUID: string;
begin
  Result := TGUID.NewGuid.ToString;
  Result := Result.Substring(1, Result.Length-2);
end;

end.
