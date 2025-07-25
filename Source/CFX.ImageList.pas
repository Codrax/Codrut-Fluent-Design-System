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
  CFX.BlurMaterial,
  CFX.Classes,
  CFX.Constants,
  CFX.Colors,
  CFX.Math,
  CFX.GDI,
  CFX.Animations,
  CFX.Types;

  type

    // Image Array
    FXPictureImages = class(FXPersistent)
    private
      FPictures: TArray<TPicture>;

      function CreateNewSpot: integer;

      function GetPicture(AIndex: Integer): TPicture;
      procedure SetPicture(AIndex: Integer; const Value: TPicture);

    protected
      // Serialization
      procedure DefineProperties(Filer: TFiler); override;
      procedure ReadData(Stream: TStream);
      procedure WriteData(Stream: TStream);

    public
      constructor Create(AOwner : TPersistent); override;
      destructor Destroy; override;

      // Images
      property Pictures[AIndex: Integer]: TPicture read GetPicture write SetPicture;

      function Count: integer;

      // Load and Delete
      procedure AddNew(Picture: TPicture);
      procedure Delete(AIndex: integer);
      procedure AddNewFromFile(FileName: string);
    end;

    // Image List
    FXImageList = class(FXComponent)
    private
      FInternalImages: FXPictureImages;

      procedure AssignPic(const Value: FXPictureImages);

    published
      property InternalImages: FXPictureImages read FInternalImages write AssignPic;

    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
    end;

implementation

{ FXImageList }

procedure FXImageList.AssignPic(const Value: FXPictureImages);
begin
  FInternalImages.Assign(Value) ;
end;

constructor FXImageList.Create(AOwner: TComponent);
begin
  inherited;
  FInternalImages := FXPictureImages.Create(Self);
end;

destructor FXImageList.Destroy;
begin
  FreeAndNil(FInternalImages);
  inherited;
end;

{ FXPictureImages }

procedure FXPictureImages.AddNew(Picture: TPicture);
var
  NewIndex: integer;
begin
  NewIndex := CreateNewSpot;

  FPictures[NewIndex] := TPicture.Create;
  FPictures[NewIndex].Assign(Picture);
end;

procedure FXPictureImages.AddNewFromFile(FileName: string);
var
  NewIndex: integer;
begin
  NewIndex := CreateNewSpot;

  FPictures[NewIndex] := TPicture.Create;
  FPictures[NewIndex].LoadFromFile(FileName);
end;

function FXPictureImages.Count: integer;
begin
  Result := Length(FPictures);
end;

constructor FXPictureImages.Create(AOwner: TPersistent);
begin
  inherited;
  SetLength(FPictures, 0);
end;

function FXPictureImages.CreateNewSpot: integer;
begin
  Result := Length(FPictures);
  SetLength(FPictures, Result + 1);
end;

procedure FXPictureImages.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('Pictures', ReadData, WriteData, true);
end;

procedure FXPictureImages.Delete(AIndex: integer);
var
  I: Integer;
begin
  if FPictures[AIndex] <> nil then
    FPictures[AIndex].Free;

  for I := AIndex to High(FPictures) - 1 do
    FPictures[I] := FPictures[I + 1];
end;

destructor FXPictureImages.Destroy;
var
  I: Integer;
begin
  // Free Pictures
  for I := 0 to High(FPictures) do
    if FPictures[I] <> nil then
      FPictures[I].Free;

  SetLength(FPictures, 0);
  inherited;
end;

function FXPictureImages.GetPicture(AIndex: Integer): TPicture;
begin
  Result := FPictures[AIndex];
end;

procedure FXPictureImages.ReadData(Stream: TStream);
var
  Count: Integer;
  I: Integer;
  Picture: TPicture;
begin
  Stream.ReadBuffer(Count, SizeOf(Count));
  SetLength(FPictures, Count);
  for I := 0 to Count - 1 do
  begin
    Picture := TPicture.Create;
    Picture.LoadFromStream(Stream);
    FPictures[I] := Picture;
  end;
end;

procedure FXPictureImages.SetPicture(AIndex: Integer; const Value: TPicture);
begin
  Pictures[AIndex].Assign(Value);
end;

procedure FXPictureImages.WriteData(Stream: TStream);
var
  Count: Integer;
  I: Integer;
begin
  Count := Length(FPictures);

  Stream.WriteBuffer(Count, SizeOf(Count));
  for I := 0 to Count - 1 do
    FPictures[I].SaveToStream(Stream);
end;

end.
