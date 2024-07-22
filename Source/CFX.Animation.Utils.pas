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

unit CFX.Animation.Utils;

{$SCOPEDENUMS ON}

interface
  uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, System.Types, TypInfo;

  function PropertyExists(Instance: TObject; PropName: string): boolean; overload;

  function GetPropertyType(Instance: TObject; PropName: string): TTypeKind; overload;

  // Root
  function GetRootInstance(Instance: TObject; var PropName: string): TObject;

  // Get
  function GetPropertyValue(Instance: TObject; PropName: string): Variant;

  // Set
  procedure SetPropertyValue(Instance: TObject; PropName: string; Value: Variant); overload;

implementation

function GetRootInstance(Instance: TObject; var PropName: string): TObject;
var
  Tree: TArray<string>;
begin
  Tree := PropName.Split(['.']);

  // Check root
  if Length(Tree) <= 1 then
    Exit(Instance);

  // Parse
  PropName := PropName.Remove(0, Length(Tree[0])+1);

  // Get upper level object
  var V: Variant;
  var EObject: TObject;

  V := GetPropValue(Instance, Tree[0]);
  EObject := TObject(int64(V));

  Result := GetRootInstance(EObject, PropName);
end;

function PropertyExists(Instance: TObject; PropName: string): boolean; overload;
var
  AProp: PPropInfo;
begin
  AProp := GetPropInfo(PTypeInfo(Instance.ClassInfo), PropName);

  Result := AProp <> nil;
end;

function GetPropertyType(Instance: TObject; PropName: string): TTypeKind; overload;
var
  AProp: PPropInfo;
  Info: PTypeInfo;
begin
  // Root
  Instance := GetRootInstance(Instance, PropName);

  // Work
  AProp := GetPropInfo(PTypeInfo(Instance.ClassInfo), PropName);

  Info := AProp^.PropType^;
  Result := Info.Kind;
end;

function GetPropertyValue(Instance: TObject; PropName: string): Variant;
begin
  // Root
  Instance := GetRootInstance(Instance, PropName);

  // Work
  Result := GetPropValue(Instance, PropName, false);
end;

procedure SetPropertyValue(Instance: TObject; PropName: string; Value: Variant); overload;
var
  AProp: PPropInfo;
begin
  // Root
  Instance := GetRootInstance(Instance, PropName);

  // Work
  AProp := GetPropInfo(PTypeInfo(Instance.ClassInfo), PropName);

  SetPropValue(Instance, AProp, Value);
end;

end.
