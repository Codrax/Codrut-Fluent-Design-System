unit CFX.ImageList;

interface

uses
  SysUtils,
  Winapi.Windows,
  Classes,
  Types,
  UITypes,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.ExtCtrls,
  Vcl.Dialogs,
  Threading,
  System.Generics.Collections,
  Vcl.Menus,
  CFX.Components,
  CFX.Graphics,
  CFX.VarHelpers,
  Vcl.Forms,
  DateUtils,
  IOUtils,
  CFX.Utilities,
  CFX.ThemeManager,
  CFX.Classes,
  CFX.Constants,
  CFX.Colors,
  CFX.Math,
  CFX.GDI,
  CFX.Types;

type
  // Image List
  FXImageList = class(FXComponent)
  private
    FPictures: FXPictureList;

    procedure AssignPic(const Value: FXPictureList);

  published
    property Pictures: FXPictureList read FPictures write AssignPic;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ FXImageList }

procedure FXImageList.AssignPic(const Value: FXPictureList);
begin
  FPictures.Assign(Value);
end;

constructor FXImageList.Create(AOwner: TComponent);
begin
  inherited;
  FPictures := FXPictureList.Create(Self);
end;

destructor FXImageList.Destroy;
begin
  FreeAndNil(FPictures);
  inherited;
end;

end.
