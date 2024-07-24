{***********************************************************}
{                  Codruts Property Editors                 }
{                                                           }
{                        version 0.2                        }
{                           ALPHA                           }
{                                                           }
{                                                           }
{                                                           }
{                                                           }
{                                                           }
{                   -- WORK IN PROGRESS --                  }
{***********************************************************}

{$SCOPEDENUMS ON}

unit CFX.DesignEditors;

interface
uses
  System.Types, System.SysUtils, System.Classes, Winapi.ActiveX, System.TypInfo, System.Variants, DesignIntf, DesignMenus,
  ToolsAPI, DesignEditors;

type
  FXPropertyEditor = class(TBasePropertyEditor, IProperty, IPropertyKind,
    IProperty70, IProperty80)
  private
    FDesigner: IDesigner;
    FPropList: PInstPropList;
    FPropCount: Integer;
    FAncestorList: TList;
    FRoot: TComponent;
    FAncestor: TPersistent;
    FRootAncestor: TComponent;
    FLookingFor: TComponent;
    FDoneLooking: Boolean;
    procedure AddAncestor(Component: TComponent);
    procedure GetLookupInfo(var Ancestor: TPersistent;
      var Root, LookupRoot, RootAncestor: TComponent);
    function GetPrivateDirectory: string;
    procedure WriteComponentSimulation(Component: TComponent);
  protected
    procedure SetPropEntry(Index: Integer; AInstance: TPersistent;
      APropInfo: PPropInfo); override;
  protected
    function GetFloatValue: Extended;
    function GetFloatValueAt(Index: Integer): Extended;
    function GetInt64Value: Int64;
    function GetInt64ValueAt(Index: Integer): Int64;
    function GetMethodValue: TMethod;
    function GetMethodValueAt(Index: Integer): TMethod;
    function GetOrdValue: Longint;
    function GetOrdValueAt(Index: Integer): Longint;
    function GetStrValue: string;
    function GetStrValueAt(Index: Integer): string;
    function GetWideStrValue: WideString;
    function GetWideStrValueAt(Index: Integer): WideString;
    function GetVarValue: Variant;
    function GetVarValueAt(Index: Integer): Variant;
    function GetIntfValue: IInterface;
    function GetIntfValueAt(Index: Integer): IInterface;
    procedure Modified;
    procedure SetFloatValue(Value: Extended);
    procedure SetMethodValue(const Value: TMethod);
    procedure SetInt64Value(Value: Int64);
    procedure SetOrdValue(Value: Longint);
    procedure SetStrValue(const Value: string);
    procedure SetWideStrValue(const Value: WideString);
    procedure SetVarValue(const Value: Variant);
    procedure SetIntfValue(const Value: IInterface);
  protected
    { IProperty }
    function GetEditValue(out Value: string): Boolean; overload;
    function HasInstance(Instance: TPersistent): Boolean;
    { IProperty70 }
    function GetIsDefault: Boolean; virtual;
    { For WProperty }
    function GetEditValue(out Value: WideString): Boolean; overload;
  public
    // Constructors
    constructor Create(const ADesigner: IDesigner; APropCount: Integer); override;
    destructor Destroy; override;

    // Parent
    property Root: TComponent read FRoot write FRoot; // Ususally the parent form
    property Component: TComponent read FLookingFor write FLookingFor; // The parent component of the property
    property PropListData: PInstPropList read FPropList write FPropList;

    // Data
    procedure Activate; virtual;
    function AllEqual: Boolean; virtual;
    function AutoFill: Boolean; virtual;
    procedure Edit; overload; virtual;
    procedure Edit(const Host: IPropertyHost; DblClick: Boolean); overload; virtual;
    function GetAttributes: TPropertyAttributes; virtual;
    function GetComponent(Index: Integer): TPersistent;
    function GetEditLimit: Integer; virtual;
    function GetName: string; virtual;
    procedure GetProperties(Proc: TGetPropProc); virtual;
    function GetPropInfo: PPropInfo; virtual;
    function GetPropType: PTypeInfo;
    function GetValue: string; virtual;
    function GetValueW: WideString; virtual;
    function GetVisualValue: string;
    procedure GetValues(Proc: TGetStrProc); virtual;
    procedure Initialize; override;
    procedure Revert;
    procedure SetValue(const Value: string); overload; virtual;
    procedure SetValue(const Value: WideString); overload; virtual;
    function ValueAvailable: Boolean;
    function GetKind: TTypeKind;
    property Kind: TTypeKind read GetKind;
    property Designer: IDesigner read FDesigner;
    property PrivateDirectory: string read GetPrivateDirectory;
    property PropCount: Integer read FPropCount;
    property Value: string read GetValue write SetValue;
  end;


implementation

uses DesignConst, Vcl.Consts, System.RTLConsts, System.SysConst, System.Contnrs, Proxies, Vcl.ActnList;

{ TPropertyEditor }

function PossibleStream(const S: string): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 1 to Length(S) - 6 do
  begin
    if ((CharInSet(S[I], ['O','o'])) and (CompareText(Copy(S, I, 6), 'OBJECT') = 0)) or
       ((CharInSet(S[I], ['I','i'])) and (CompareText(Copy(S, I, 6), 'INLINE') = 0)) then
      Exit;
    if not (CharInSet(S[I], [' ',#9, #13, #10])) then Break;
  end;
  Result := False;
end;

{ TPropertyEditor }

constructor FXPropertyEditor.Create(const ADesigner: IDesigner;
  APropCount: Integer);
begin
  inherited Create(ADesigner, APropCount);
  FDesigner := ADesigner;
  GetMem(FPropList, APropCount * SizeOf(TInstProp));
  FPropCount := APropCount;
end;

destructor FXPropertyEditor.Destroy;
begin
  if FPropList <> nil then
    FreeMem(FPropList, FPropCount * SizeOf(TInstProp));
  inherited Destroy;
end;

procedure FXPropertyEditor.Edit(const Host: IPropertyHost; DblClick: Boolean);
begin

end;

procedure FXPropertyEditor.Activate;
begin
end;

function FXPropertyEditor.AllEqual: Boolean;
begin
  Result := FPropCount = 1;
end;

procedure FXPropertyEditor.Edit;
type
  TGetStrFunc = function(const Value: string): Integer of object;
var
  I: Integer;
  Values: TStringList;
  AddValue: TGetStrFunc;
begin
  if not AutoFill then Exit;
  Values := TStringList.Create;
  Values.Sorted := paSortList in GetAttributes;
  try
    AddValue := Values.Add;
    GetValues(TGetStrProc(AddValue));
    if Values.Count > 0 then
    begin
      I := Values.IndexOf(Value) + 1;
      if I = Values.Count then I := 0;
      Value := Values[I];
    end;
  finally
    Values.Free;
  end;
end;

function FXPropertyEditor.AutoFill: Boolean;
begin
  Result := Assigned(GetPropInfo^.SetProc);
end;

function FXPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paRevertable];
  if FPropList[0].PropInfo.SetProc = nil then
    Result := Result + [paReadOnly, paDisplayReadOnly];
end;

function FXPropertyEditor.GetComponent(Index: Integer): TPersistent;
begin
  Result := FPropList^[Index].Instance;
end;

function FXPropertyEditor.GetFloatValue: Extended;
begin
  Result := GetFloatValueAt(0);
end;

function FXPropertyEditor.GetFloatValueAt(Index: Integer): Extended;
begin
  with FPropList^[Index] do Result := GetFloatProp(Instance, PropInfo);
end;

function FXPropertyEditor.GetMethodValue: TMethod;
begin
  Result := GetMethodValueAt(0);
end;

function FXPropertyEditor.GetMethodValueAt(Index: Integer): TMethod;
begin
  with FPropList^[Index] do Result := GetMethodProp(Instance, PropInfo);
end;

function FXPropertyEditor.GetEditLimit: Integer;
begin
  Result := 2047;
end;

function FXPropertyEditor.GetName: string;
begin
  Result := GetPropName(FPropList^[0].PropInfo);
end;

function FXPropertyEditor.GetOrdValue: Longint;
begin
  Result := GetOrdValueAt(0);
end;

function FXPropertyEditor.GetOrdValueAt(Index: Integer): Longint;
begin
  with FPropList^[Index] do Result := GetOrdProp(Instance, PropInfo);
end;

function FXPropertyEditor.GetPrivateDirectory: string;
begin
  Result := '';
  if Designer <> nil then
    Result := Designer.GetPrivateDirectory;
end;

procedure FXPropertyEditor.GetProperties(Proc: TGetPropProc);
begin
end;

function FXPropertyEditor.GetPropInfo: PPropInfo;
begin
  Result := FPropList^[0].PropInfo;
end;

function FXPropertyEditor.GetPropType: PTypeInfo;
begin
  Result := FPropList^[0].PropInfo^.PropType^;
end;

function FXPropertyEditor.GetStrValue: string;
begin
  Result := GetStrValueAt(0);
end;

function FXPropertyEditor.GetStrValueAt(Index: Integer): string;
begin
  with FPropList^[Index] do Result := GetStrProp(Instance, PropInfo);
end;

function FXPropertyEditor.GetWideStrValue: WideString;
begin
  Result := GetWideStrValueAt(0);
end;

function FXPropertyEditor.GetWideStrValueAt(Index: Integer): WideString;
begin
  with FPropList^[Index] do Result := GetWideStrProp(Instance, PropInfo);
end;

function FXPropertyEditor.GetVarValue: Variant;
begin
  Result := GetVarValueAt(0);
end;

function FXPropertyEditor.GetVarValueAt(Index: Integer): Variant;
begin
  with FPropList^[Index] do Result := GetVariantProp(Instance, PropInfo);
end;

function FXPropertyEditor.GetValue: string;
begin
  Result := srUnknown;
end;

function FXPropertyEditor.GetValueW: WideString;
begin
  Result := srUnknown;
end;

function FXPropertyEditor.GetVisualValue: string;
begin
  if AllEqual then
    Result := GetValue
  else
    Result := '';
end;

procedure FXPropertyEditor.GetValues(Proc: TGetStrProc);
begin
end;

procedure FXPropertyEditor.Initialize;
begin
end;

procedure FXPropertyEditor.Modified;
begin
  if Designer <> nil then
    Designer.Modified;
end;

procedure FXPropertyEditor.SetFloatValue(Value: Extended);
var
  I: Integer;
begin
  for I := 0 to FPropCount - 1 do
    with FPropList^[I] do SetFloatProp(Instance, PropInfo, Value);
  Modified;
end;

procedure FXPropertyEditor.SetMethodValue(const Value: TMethod);
var
  I: Integer;
begin
  for I := 0 to FPropCount - 1 do
    with FPropList^[I] do SetMethodProp(Instance, PropInfo, Value);
  Modified;
end;

procedure FXPropertyEditor.SetOrdValue(Value: Longint);
var
  I: Integer;
begin
  for I := 0 to FPropCount - 1 do
    with FPropList^[I] do SetOrdProp(Instance, PropInfo, Value);
  Modified;
end;

procedure FXPropertyEditor.SetPropEntry(Index: Integer;
  AInstance: TPersistent; APropInfo: PPropInfo);
begin
  with FPropList^[Index] do
  begin
    Instance := AInstance;
    PropInfo := APropInfo;
  end;
end;

procedure FXPropertyEditor.SetStrValue(const Value: string);
var
  I: Integer;
begin
  for I := 0 to FPropCount - 1 do
    with FPropList^[I] do
      if PropInfo.SetProc <> nil then
        SetStrProp(Instance, PropInfo, Value);
  Modified;
end;

procedure FXPropertyEditor.SetWideStrValue(const Value: WideString);
var
  I: Integer;
begin
  for I := 0 to FPropCount - 1 do
    with FPropList^[I] do
      if PropInfo.SetProc <> nil then
        SetWideStrProp(Instance, PropInfo, Value);
  Modified;
end;

procedure FXPropertyEditor.SetVarValue(const Value: Variant);
var
  I: Integer;
begin
  for I := 0 to FPropCount - 1 do
    with FPropList^[I] do SetVariantProp(Instance, PropInfo, Value);
  Modified;
end;

procedure FXPropertyEditor.Revert;
var
  I: Integer;
begin
  if Designer <> nil then
    for I := 0 to FPropCount - 1 do
      with FPropList^[I] do Designer.Revert(Instance, PropInfo);
end;

procedure FXPropertyEditor.SetValue(const Value: string);
begin
end;

procedure FXPropertyEditor.SetValue(const Value: WideString);
begin
end;

function FXPropertyEditor.ValueAvailable: Boolean;
var
  I: Integer;
  S: string;
begin
  Result := True;
  for I := 0 to FPropCount - 1 do
  begin
    if (FPropList^[I].Instance is TComponent) and
      (csCheckPropAvail in TComponent(FPropList^[I].Instance).ComponentStyle) then
    begin
      try
        S := GetValue;
        AllEqual;
      except
        Result := False;
      end;
      Exit;
    end;
  end;
end;

function FXPropertyEditor.GetInt64Value: Int64;
begin
  Result := GetInt64ValueAt(0);
end;

function FXPropertyEditor.GetInt64ValueAt(Index: Integer): Int64;
begin
  with FPropList^[Index] do Result := GetInt64Prop(Instance, PropInfo);
end;

procedure FXPropertyEditor.SetInt64Value(Value: Int64);
var
  I: Integer;
begin
  for I := 0 to FPropCount - 1 do
    with FPropList^[I] do SetInt64Prop(Instance, PropInfo, Value);
  Modified;
end;

function FXPropertyEditor.GetIntfValue: IInterface;
begin
  Result := GetIntfValueAt(0);
end;

function FXPropertyEditor.GetIntfValueAt(Index: Integer): IInterface;
begin
  with FPropList^[Index] do Result := GetInterfaceProp(Instance, PropInfo);
end;

procedure FXPropertyEditor.SetIntfValue(const Value: IInterface);
var
  I: Integer;
begin
  for I := 0 to FPropCount - 1 do
    with FPropList^[I] do SetInterfaceProp(Instance, PropInfo, Value);
  Modified;
end;

function FXPropertyEditor.GetEditValue(out Value: string): Boolean;
var
  PropInfo: PPropInfo;
begin
  Result := False;
  try
    Value := GetValue;
    PropInfo := GetPropInfo;
    Result := (PropInfo <> nil) and Assigned(PropInfo^.SetProc);
  except
    on E: EPropWriteOnly do Value := sNotAvailable;
    on E: Exception do Value := Format('(%s)', [E.Message]);
  end;
end;

function FXPropertyEditor.GetEditValue(out Value: WideString): Boolean;
var
  PropInfo: PPropInfo;
begin
  Result := False;
  try
    Value := GetValueW;
    PropInfo := GetPropInfo;
    Result := (PropInfo <> nil) and Assigned(PropInfo^.SetProc);
  except
    on E: EPropWriteOnly do Value := sNotAvailable;
    on E: Exception do Value := Format('(%s)', [E.Message]);
  end;
end;

function FXPropertyEditor.HasInstance(Instance: TPersistent): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to FPropCount - 1 do
    if FPropList^[I].Instance = Instance then Exit;
  Result := False;
end;

type
  TComponentHack = class(TComponent);

procedure FXPropertyEditor.WriteComponentSimulation(Component: TComponent);
  function FindAncestor(const Name: string): TComponent;
  var
    I: Integer;
  begin
    for I := 0 to FAncestorList.Count - 1 do
    begin
      Result := FAncestorList[I];
      if SameText(Result.Name, Name) then Exit;
    end;
    Result := nil;
  end;
var
  OldAncestor: TPersistent;
  OldRoot, OldRootAncestor: TComponent;
  OldAncestorList: TList;
  TempAncestor: TPersistent;
begin
  if FDoneLooking then
    Exit;

  OldAncestor := FAncestor;
  OldRootAncestor := FRootAncestor;
  try
    if Assigned(FAncestorList) then
      FAncestor := FindAncestor(Component.Name);

    // If we are at the component we were looking for, then we
    // can stop at this point
    if FLookingFor = Component then
    begin
      FDoneLooking := True
    end
    else if SameText(FLookingFor.Name, Component.Name) then
    begin
      FDoneLooking := True;
    end
    else
    begin

      if (FAncestor = nil) and (Component <> Designer.Root)
        and IsProxyClass(Component.ClassType) then
      begin
        TempAncestor := Designer.FindRootAncestor(Component.ClassName);
        if TempAncestor <> nil then
        begin
          FAncestor := TempAncestor;
          FRootAncestor := TComponent(FAncestor);
        end;

{        InlineRoot := ActiveDesigner.OpenRootClass(Component.ClassName);
        if InlineRoot <> nil then
        begin
          FAncestor := InlineRoot.GetRoot;
          FRootAncestor := TComponent(FAncestor);
        end;
        }
      // use IDesigner.FindRootAncestor
      end;

      // Component.WriteState(Self); // This is simulated below, inline
      OldAncestorList := FAncestorList;
      OldRoot := FRoot;
      OldRootAncestor := FRootAncestor;
      try
        FAncestorList := nil;
  //        if the instance isn't a TActiveXControl...
  //        if not IgnoreChildren then
          try
            if (FAncestor <> nil) and (FAncestor is TComponent) then
            begin
              if csInline in TComponent(FAncestor).ComponentState then
                FRootAncestor := TComponent(FAncestor);
              FAncestorList := TList.Create;
              TComponentHack(FAncestor).GetChildren(AddAncestor, FRootAncestor);
            end;
            if csInline in Component.ComponentState then
              FRoot := Component;
            TComponentHack(Component).GetChildren(WriteComponentSimulation, FRoot);
          finally
            FAncestorList.Free;
          end;
      finally
        FAncestorList := OldAncestorList;
        if not FDoneLooking then
        begin
          FRoot := OldRoot;
          FRootAncestor := OldRootAncestor;
        end;
      end;
    end;
  finally
    if not FDoneLooking then
    begin
      // Only restore the ancestor if we were not done looking.
      // This way, we can continue up the chaing looking for the
      // component
      FAncestor := OldAncestor;
      FRootAncestor := OldRootAncestor;
    end
  end;
end;

function FXPropertyEditor.GetIsDefault: Boolean;
  function CheckProperties(AnObject: TObject): Boolean;
  var
    PropList: PPropList;
    PropInfo: PPropInfo;
    I, Count: Integer;
  begin
    Result := True;
    // Go through each of the properties on the object
    Count := GetTypeData(AnObject.ClassInfo)^.PropCount;
    if Count > 0 then
    begin
      GetMem(PropList, Count * SizeOf(Pointer));
      try
        GetPropInfos(AnObject.ClassInfo, PropList);
        for I := 0 to Count - 1 do
        begin
          PropInfo := PropList^[I];
          if PropInfo = nil then
            Break;
          if not IsDefaultPropertyValue(AnObject, PropInfo, GetLookupInfo) then
          begin
            Result := False;
            Break;
          end;
        end;
      finally
        FreeMem(PropList, Count * SizeOf(Pointer));
      end;
    end;
  end;

  function FindChildComponent(Component: TComponent; const Name: string): TComponent;
  var
    I: Integer;
  begin
    Result := Component.FindComponent(Name);
    if Result = nil then
      for I := 0 to Component.ComponentCount - 1 do
      begin
        Result := FindChildComponent(Component.Components[I], Name);
        if Result <> nil then
          Exit;
      end;
  end;

var
  FirstInstance: TObject;
  FirstPropInfo: PPropInfo;
  ChildName: string;

  SubObject: TObject;
  OldAncestor: TPersistent;

begin
  Result := True;
  if (Designer <> nil) and (PropCount > 0) then
  begin
    // if they are not all equal, then they aren't all the default (at least one..)
    if not AllEqual then
    begin
      Result := False;
      Exit;
    end;

    FirstInstance := FPropList^[0].Instance;
    FirstPropInfo := FPropList^[0].PropInfo;
    if IsStoredProp(FirstInstance, FirstPropInfo) then
    begin
      // TWriter.WriteDescendent simulation
      if Designer.AncestorDesigner <> nil then
      begin
        FRootAncestor := Designer.AncestorDesigner.Root;
        FAncestor := FRootAncestor;
      end
      else
      begin
        FRootAncestor := nil;
        FAncestor := nil;
      end;
      FRoot := Designer.Root;

      if FirstInstance is TComponent then
      begin
        FLookingFor := TComponent(FirstInstance);
        // Only lookup the component if it was introduced in an ancestor form/frame
        if csAncestor in FLookingFor.ComponentState then
        begin
          FDoneLooking := False;
          if csSubComponent in FLookingFor.ComponentStyle then
          begin
            ChildName := FLookingFor.Name;
            repeat
              FLookingFor := FLookingFor.Owner;
            until not (csSubComponent in FLookingFor.ComponentStyle);
            WriteComponentSimulation(FRoot);
            FAncestor := FindChildComponent(TComponent(FAncestor), ChildName);
          end
          else
            WriteComponentSimulation(FRoot);
        end
        else
        begin
          FRootAncestor := nil;
          FAncestor := nil;
        end;
      end
      else
      begin
        // In this case, we will not look up the ancestor (there really
        // isn't one - take columns on tlistview as an example)
        FRootAncestor := nil;
        FAncestor := nil;
      end;

      Result := IsDefaultPropertyValue(FirstInstance, FirstPropInfo, GetLookupInfo);
      if not Result then
      begin
        if FirstPropInfo^.PropType^.Kind = tkClass then
        begin
          // If it was a class/object then we need to recursivly check that
          // object to see if it has all default properties.
          SubObject := GetObjectProp(FirstInstance, FirstPropInfo);

          OldAncestor := FAncestor;
          try
            if AncestorIsValid(FAncestor, FRoot, FRootAncestor) then
              FAncestor := TPersistent(GetOrdProp(FAncestor, FirstPropInfo));
            Result := CheckProperties(SubObject);
          finally
            FAncestor := OldAncestor;
          end;

          if SubObject is TCollection then
          begin
            if not AncestorIsValid(FAncestor, FRoot, FRootAncestor) or
              not CollectionsEqual(TCollection(SubObject),
                TCollection(GetOrdProp(FAncestor, FirstPropInfo)),
                  FRoot, FRootAncestor) then
                  Result := False;
          end;
        end;
      end;
    end;
  end;
end;

procedure FXPropertyEditor.AddAncestor(Component: TComponent);
begin
  FAncestorList.Add(Component);
end;

procedure FXPropertyEditor.GetLookupInfo(var Ancestor: TPersistent;
  var Root, LookupRoot, RootAncestor: TComponent);
begin
  Ancestor := FAncestor;
  Root := FRoot;
  LookupRoot := FRoot; // Same in this case
  RootAncestor := FRootAncestor;
end;

function FXPropertyEditor.GetKind: TTypeKind;
begin
  Result := GetPropType.Kind;
end;
end.
