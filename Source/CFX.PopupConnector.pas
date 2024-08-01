unit CFX.PopupConnector;

interface

uses
  Vcl.Menus, CFX.PopupMenu;

type
  FXPopupConnector = class(TPopupMenu)
  private
    FPopupMenu: FXPopupMenu;

  published
    property PopupMenu: FXPopupMenu read FPopupMenu write FPopupMenu;

  public
    procedure Popup(X, Y: Integer); override;

  end;

implementation

{ FXPopupConnector }

procedure FXPopupConnector.Popup(X, Y: Integer);
begin
  //inherited;
  if Assigned(PopupMenu) then
    PopupMenu.Popup(X, Y);
end;

end.
