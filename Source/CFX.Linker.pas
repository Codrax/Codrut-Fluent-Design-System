unit CFX.Linker;

interface
uses
  Vcl.Graphics;

type
  // Define Control type to identify on update
  FXControl = interface
    ['{5098EF5C-0451-490D-A0B2-24C414F21A24}']
    function IsContainer: Boolean;
    procedure UpdateTheme(const UpdateChidlren: Boolean);

    function Background: TColor;
  end;

implementation

end.
