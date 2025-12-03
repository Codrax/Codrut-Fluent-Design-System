unit CFX.Test;

interface
uses
  Vcl.Controls,
  Classes,
  Types,
  CFX.Classes,
  CFX.ComponentClasses;

type
  TTest = class(TControl)
  private
    FTest: FXSideValues;

  published
    property Test: FXSideValues read FTest write FTest;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  procedure Register;

implementation

procedure Register;
begin
  //RegisterComponents('Standard', [TTest]);
end;

{ TTest }

constructor TTest.Create(AOwner: TComponent);
begin
  inherited;
  FTest := FXSideValues.Create(Self);
end;

destructor TTest.Destroy;
begin
  FTest.Free;
  inherited;
end;

end.
