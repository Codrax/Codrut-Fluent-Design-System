unit CFX.Components;

interface

uses
  Winapi.Windows, Vcl.Graphics, Classes, Types, Winapi.Messages, CFX.Types,
  CFX.Constants, SysUtils, CFX.Graphics, CFX.VarHelpers, CFX.ThemeManager,
  Vcl.Controls, CFX.Linker, Vcl.Forms, CFX.PopupMenu,
  Vcl.Dialogs, CFX.Classes, Math, DateUtils, CFX.ArrayHelpers, CFX.Messages;

type
  // Component
  FXComponent = class(TComponent, IFXComponent)
  protected
    // Utilities
    function IsReading: boolean;
    function IsDesigning: boolean;
    function IsDestroying: boolean;
    function Destroyed: boolean;

    // Internal
    function CanUpdate: boolean;
    procedure UpdateColors; virtual;
  public
    // Components
    function GetChildComponents: TArray<TComponent>;

    // Interface
    procedure UpdateTheme(const UpdateChildren: Boolean); virtual;

    // Constructors
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ FXComponent }

function FXComponent.CanUpdate: boolean;
begin
  Result := not IsReading and (Self.Owner <> nil);
end;

constructor FXComponent.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor FXComponent.Destroy;
begin
  inherited;
end;

function FXComponent.Destroyed: boolean;
begin
  Result := IsDestroying or (Self = nil) or (Self.Owner = nil);
end;

function FXComponent.GetChildComponents: TArray<TComponent>;
begin
  SetLength(Result, ComponentCount);
  for var I := 0 to ComponentCount-1 do
    Result[I] := Components[I];
end;

function FXComponent.IsDesigning: boolean;
begin
  Result := csDesigning in ComponentState;
end;

function FXComponent.IsDestroying: boolean;
begin
  Result := csDestroying in ComponentState;
end;

function FXComponent.IsReading: boolean;
begin
  Result := csReading in ComponentState;
end;

procedure FXComponent.UpdateColors;
begin
  //
end;

procedure FXComponent.UpdateTheme(const UpdateChildren: Boolean);
begin
  UpdateColors;

  if UpdateChildren then begin
    const ChildrenC = GetChildComponents;
    for var I := 0 to High(ChildrenC) do
    if Supports(ChildrenC[I], IFXComponent) then
      (ChildrenC[I] as IFXComponent).UpdateTheme(UpdateChildren);
  end;
end;

end.
