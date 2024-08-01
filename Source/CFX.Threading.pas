unit CFX.Threading;

interface

uses
  Classes,
  Messages,
  Windows,
  Types,
  Math,
  SysUtils,
  Threading;

type
  FXThreadHelper = class helper for TThread
  public
    function Running: boolean;

    procedure Halt;
  end;

implementation

{ FXThreadHelper }

procedure FXThreadHelper.Halt;
begin
  TerminateThread(Handle, 0);
end;

function FXThreadHelper.Running: boolean;
begin
  Result := Started and not Finished;
end;

end.
