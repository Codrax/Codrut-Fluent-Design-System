unit CFX.TypeInfo;

interface

uses
  Winapi.Windows, Vcl.Graphics, Types, UITypes, Classes, Vcl.Forms, Math,
  CFX.Types, TypInfo;

function PropertyExists(Instance: TObject; const PropName: string): boolean;

implementation

function PropertyExists(Instance: TObject; const PropName: string): boolean; overload;
var
  AProp: PPropInfo;
begin
  AProp := GetPropInfo(PTypeInfo(Instance.ClassInfo), PropName);

  Result := AProp <> nil;
end;

end.
