unit CFX.Classes;

interface
uses
  Vcl.Graphics, Classes, Types, CFX.Types, CFX.Constants, SysUtils,
  CFX.Graphics, CFX.VarHelpers, CFX.ThemeManager, Vcl.Controls,
  TypInfo, CFX.Linker, CFX.Colors, Math, Vcl.Dialogs;

type
  // Persistent
  FXPersistent = class(TPersistent)
    Owner : TPersistent;
    constructor Create(AOwner : TPersistent); overload; virtual;
  end;

  FXAssignPersistent = class(FXPersistent)
  public
    procedure Assign(Source: TPersistent); override;
  end;

  // Picture List
  FXPictureList = class(FXPersistent)
  private
    FPictures: TArray<TPicture>;

    FOnChange: TNotifyEvent;

    function CreateNewSpot: integer;

    function GetPicture(AIndex: Integer): TPicture;
    procedure SetPicture(AIndex: Integer; const Value: TPicture);

  protected
    procedure Updated; virtual;

    // Serialization
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);

  public
    constructor Create(AOwner : TPersistent); override;
    destructor Destroy; override;

    // Images
    property Pictures[AIndex: Integer]: TPicture read GetPicture write SetPicture; default;
    function Count: integer;

    // Events
    property OnChange: TNotifyEvent read FOnChange write FOnChange;

    // Assign
    procedure AssignTo(Dest: TPersistent); override;

    // Load and Delete
    procedure Clear;
    procedure AddNew(Picture: TPicture);
    procedure Delete(AIndex: integer);
    procedure AddNewFromFile(FileName: string);
  end;

implementation

{ FXPersistent }

constructor FXPersistent.Create(AOwner: TPersistent);
begin
  inherited Create;
  Owner := AOwner;
end;


{ FXAssignPersistent }

procedure FXAssignPersistent.Assign(Source: TPersistent);
var
  PropList: PPropList;
  PropCount, i: Integer;
begin
  if Source is FXAssignPersistent then
  begin
    PropCount := GetPropList(Source.ClassInfo, tkProperties, nil);
    if PropCount > 0 then
    begin
      GetMem(PropList, PropCount * SizeOf(PPropInfo));
      try
        GetPropList(Source.ClassInfo, tkProperties, PropList);
        for i := 0 to PropCount - 1 do
          SetPropValue(Self, string(PropList^[i]^.Name), GetPropValue(Source, string(PropList^[i]^.Name)));
      finally
        FreeMem(PropList);
      end;
    end;
  end
  else
    inherited Assign(Source);
end;

{ FXPictureList }

procedure FXPictureList.AddNew(Picture: TPicture);
var
  NewIndex: integer;
begin
  NewIndex := CreateNewSpot;

  FPictures[NewIndex] := TPicture.Create;
  FPictures[NewIndex].Assign(Picture);
end;

procedure FXPictureList.AddNewFromFile(FileName: string);
var
  Pic: TPicture;
begin
  Pic := TPicture.Create;
  try
    Pic.LoadFromFile(FileName);

    FPictures[CreateNewSpot] := Pic;
  except
    Pic.Free;
  end;
end;

procedure FXPictureList.AssignTo(Dest: TPersistent);
begin
  if Dest is FXPictureList then begin
    const Destination = FXPictureList(Dest);

    Destination.Clear;

    for var I := 0 to Count-1 do
      Destination.AddNew( Pictures[I] );

    // Notify
    Destination.Updated;
  end
  else
    inherited AssignTo(Dest);
end;

procedure FXPictureList.Clear;
begin
  for var I := 0 to Count-1 do
    Delete(0);
end;

function FXPictureList.Count: integer;
begin
  Result := Length(FPictures);
end;

constructor FXPictureList.Create(AOwner: TPersistent);
begin
  inherited;
  SetLength(FPictures, 0);
end;

function FXPictureList.CreateNewSpot: integer;
begin
  Result := Length(FPictures);
  SetLength(FPictures, Result + 1);
end;

procedure FXPictureList.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('Pictures', ReadData, WriteData, true);
end;

procedure FXPictureList.Delete(AIndex: integer);
var
  I: Integer;
begin
  if FPictures[AIndex] <> nil then
    FPictures[AIndex].Free;

  // Offset
  for I := AIndex to High(FPictures) - 1 do
    FPictures[I] := FPictures[I + 1];

  // Len
  SetLength(FPictures, Length(FPictures)-1);
end;

destructor FXPictureList.Destroy;
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

function FXPictureList.GetPicture(AIndex: Integer): TPicture;
begin
  Result := FPictures[AIndex];
end;

procedure FXPictureList.SetPicture(AIndex: Integer; const Value: TPicture);
begin
  Pictures[AIndex].Assign(Value);
end;

procedure FXPictureList.Updated;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure FXPictureList.WriteData(Stream: TStream);
var
  Count: Integer;
  I: Integer;
  MS: TMemoryStream;
  BlobSize: Int64;
begin
  Count := Length(FPictures);
  Stream.WriteBuffer(Count, SizeOf(Count));

  for I := 0 to Count - 1 do
  begin
    MS := TMemoryStream.Create;
    try
      if (FPictures[I] <> nil) and (FPictures[I].Graphic <> nil) then
        FPictures[I].SaveToStream(MS);

      BlobSize := MS.Size;
      // write the blob size first
      Stream.WriteBuffer(BlobSize, SizeOf(BlobSize));

      if BlobSize > 0 then
      begin
        MS.Position := 0;
        Stream.CopyFrom(MS, BlobSize);
      end;
    finally
      MS.Free;
    end;
  end;
end;

procedure FXPictureList.ReadData(Stream: TStream);
var
  Count: Integer;
  I: Integer;
  MS: TMemoryStream;
  BlobSize: Int64;
begin
  Stream.ReadBuffer(Count, SizeOf(Count));
  SetLength(FPictures, Count);

  for I := 0 to Count - 1 do
  begin
    FPictures[I] := TPicture.Create;

    // read blob size
    Stream.ReadBuffer(BlobSize, SizeOf(BlobSize));

    if BlobSize > 0 then
    begin
      MS := TMemoryStream.Create;
      try
        MS.SetSize(BlobSize);
        MS.Position := 0;
        Stream.ReadBuffer(MS.Memory^, BlobSize);
        MS.Position := 0;
        FPictures[I].LoadFromStream(MS);
      finally
        MS.Free;
      end;
    end
    else
    begin
      // no image stored -> leave TPicture empty (0x0)
    end;
  end;
end;

end.
