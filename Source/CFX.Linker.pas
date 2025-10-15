unit CFX.Linker;

interface
uses
  Vcl.Graphics;

type
  // Define Control type to identify on update
  IFXComponent = interface
    ['{A3FFB2B1-05D3-4758-80A6-8BC97C0D9392}']
    procedure UpdateTheme(const UpdateChildren: Boolean);
  end;
  IFXControl = interface
    ['{5098EF5C-0451-490D-A0B2-24C414F21A24}']
    function IsContainer: Boolean;
    function Background: TColor;

    procedure Redraw;
  end;

implementation

end.
