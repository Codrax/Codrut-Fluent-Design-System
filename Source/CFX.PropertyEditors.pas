unit CFX.PropertyEditors;

interface

uses
  SysUtils,
  Windows,
  Classes,
  Types,
  Math,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.ExtCtrls,
  Threading,
  System.Generics.Collections,
  Vcl.Menus,
  CFX.Graphics,
  CFX.VarHelpers,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtDlgs,
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
  CFX.Types,
  CFX.Forms,
  CFX.FontIcons,
  CFX.Version,
  CFX.ButtonDesign,
  CFX.PopupMenu,
  CFX.ImageList,

  // Property Editor
  DesignEditors,
  DesignIntf,
  RTTI,
  TypInfo,
  VCLEditors,
  CFX.DesignEditors;

type
  // Default Edit Form Template
  FXEditForm = class(TForm)
  private
    FMainTitle,
    FSubTitle: string;

    FTitle1,
    FTitle2: TLabel;

    FButtonSave,
    FButtonClose: FXButtonDesign;

    FAllowCancel: boolean;
    FStyled: boolean;

    const
      ZONE_MARGIN = 20;

    procedure SetSubTitle(const Value: string);
    procedure SetTitle(const Value: string);
    procedure SetAllowCancel(const Value: boolean);

  public
    property Title: string read FMainTitle write SetTitle;
    property SubTitle: string read FSubTitle write SetSubTitle;

    property AllowCancel: boolean read FAllowCancel write SetAllowCancel;
    property Styled: boolean read FStyled write FStyled;

    function ComponentsZone: TRect;
    function Margin: integer;
    function MarginTiny: integer;

    procedure UpdateUI;

    constructor CreateNew(AOwner: TComponent; Dummy: Integer  = 0); override;
    destructor Destroy; override;
  end;

  // Popup Menu Items
  TFXPopupItemsProperty = class(TPropertyEditor)
  private
    Form: FXEditForm;
    Item: FXPopupItems;

  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  // Percent Property
  TFXPercentProperty = class(TPropertyEditor)
  private
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  // Angle Property
  TFXAngleProperty = class(TPropertyEditor)
  private
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  // Color Property
  TFXColorProperty = class(FXPropertyEditor, ICustomPropertyDrawing,
    ICustomPropertyListDrawing, ICustomPropertyDrawing80)
  private
    const
      COLBOX_WIDTH = 25;
      COLBOX_SPACING = 5;

    procedure DrawColorBox(ACanvas: TCanvas; ARect: TRect; AColor: FXColor);
    function OpenEditor: FXColor;

  public
    procedure Edit(const Host: IPropertyHost; DblClick: Boolean); override;

    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;

    procedure GetValues(Proc: TGetStrProc); override;

    { ICustomPropertyListDrawing }
    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas;
      var AHeight: Integer);
    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas;
      var AWidth: Integer);
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean);

    { ICustomPropertyDrawing }
    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect;
      ASelected: Boolean);
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
      ASelected: Boolean);
    { ICustomPropertyDrawing80 }
    function PropDrawNameRect(const ARect: TRect): TRect;
    function PropDrawValueRect(const ARect: TRect): TRect;
  end;

  // FXPictureImages Items
  TFXPictureImagesProperty = class(TPropertyEditor)
  private
    Form: FXEditForm;
    Item: FXPictureImages;

  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;
  end;

  // Icon Selector
  TFXIconSelectProperty = class(TPropertyEditor)
  private
    Item: FXIconSelect;
    Form: FXEditForm;

    LB1,
    LB2: TLabel;

    ImagePicture,
    ImageBitmap: TImage;
    FontIcon: TEdit;

    const
      IMAGEBOX_SIZE = 150;

  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure SetValue(const Value: string); override;

    procedure ButtonSelect(Sender: TObject);
    procedure ButtonImageAction(Sender: TObject);
    procedure EditInteract(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure ShowPanel(Index: integer);
  end;


const
  SET_CUSTOM = 'Custom...';

implementation

{ TFXPopupItemsProperty }

procedure TFXPopupItemsProperty.Edit;
var
  Menu: FXPopupItem;
begin
  inherited;
  // Item
  Item := FXPopupItems(Self.GetOrdValue);

  Modified;
  Form := FXEditForm.CreateNew(Application);
  try
    // Edit
    Menu := FXPopupItem.Create(TComponent(Item.Owner));
    with Menu do
      begin
        Text := 'Copy';
        ShortCut := 'Ctrl+C';

        Image.Enabled := true;
        Image.IconType := FXIconType.SegoeIcon;
        Image.SelectSegoe := #$E709;
      end;

    Item.Add(Menu);

    // Data
    Form.SubTitle := 'SubControl count: ' + Item.Count.ToString;

    // Form
    Form.ShowModal;
  finally
    Form.Free;
  end;
end;

function TFXPopupItemsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

function TFXPopupItemsProperty.GetValue: string;
begin
  // Return the current value of the property as a string
  Result := '(FXPopupItems)';
end;

procedure TFXPopupItemsProperty.SetValue(const Value: string);
begin
  inherited;
end;

{ TFXIconSelectProperty }

procedure TFXIconSelectProperty.ButtonImageAction(Sender: TObject);
begin
  case FXButtonDesign(Sender).Tag of
    (* TPicture *)
    1: begin
      with TOpenPictureDialog.Create(nil) do
        if Execute then
          if TFile.Exists(FileName) then
            begin
              Item.SelectPicture.LoadFromFile(FileName);

              ImagePicture.Picture.Assign( Item.SelectPicture );

              LB1.Hide;
            end;
    end;
    2: begin
      with TSavePictureDialog.Create(nil) do
        begin
          FileName := 'Image.png';
          if Execute then
            if not Item.SelectPicture.Graphic.Empty then
              Item.SelectPicture.SaveToFile(FileName);
        end;
    end;
    3: begin
      Item.SelectPicture.Free;
      Item.SelectPicture := TPicture.Create;

      ImagePicture.Picture.Assign( Item.SelectPicture );

      LB1.Show;
    end;

    (* TBitMap *)
    4: begin
      with TOpenPictureDialog.Create(nil) do
        begin
          Filter := 'Bitmaps (*.bmp)|*.bmp|All Files|*';
          if Execute then
            if TFile.Exists(FileName) then
              begin
                Item.SelectBitmap.LoadFromFile(FileName);

                ImageBitMap.Picture.Assign( Item.SelectBitmap );

                LB2.Hide;
              end;
        end;
    end;
    5: begin
      with TSavePictureDialog.Create(nil) do
        begin
          FileName := 'Bitmap.bmp';
          Filter := 'Bitmaps (*.bmp)|*.bmp|All Files|*';

          if Execute then
            if not Item.SelectBitmap.Empty then
              Item.SelectBitmap.SaveToFile(FileName);
        end;
    end;
    6: begin
      Item.SelectBitmap.Free;
      Item.SelectBitmap := TBitMap.Create;

      ImageBitMap.Picture.Assign( Item.SelectBitmap );

      LB2.Show;
    end;
  end;
end;

procedure TFXIconSelectProperty.ButtonSelect(Sender: TObject);
var
  I: integer;
begin
  Item.IconType := FXIconType(FXButtonDesign(Sender).Tag);

  for I := 0 to FXButtonDesign(Sender).Parent.ControlCount - 1 do
    if FXButtonDesign(Sender).Parent.Controls[I] is FXButtonDesign then
      FXButtonDesign(FXButtonDesign(Sender).Parent.Controls[I]).FlatButton := false;

  FXButtonDesign(Sender).FlatButton := true;

  ShowPanel( FXButtonDesign(Sender).Tag );
end;

procedure TFXIconSelectProperty.Edit;
var
  ListPanel, Panel: TPanel;
  I: integer;
  ItemOrig: FXIconSelect;
begin
  inherited;
  // Item
  ItemOrig := FXIconSelect(Self.GetOrdValue);
  Item := FXIconSelect.Create(nil);
  Item.Assign(ItemOrig);

  // Form
  Form := FXEditForm.CreateNew(Application);
  try
    with Form do
      begin
        Caption := 'Editing Icon Item';

        Title := 'Edit Selected Image';
        SubTitle := 'Choose a icon';

        Form.Height := Form.Height + 100;

        // Create Components
        ListPanel := TPanel.Create(Form);
        with ListPanel do
          begin
            Parent := Form;

            Top := Form.ComponentsZone.Top;
            Left := Form.ComponentsZone.Left;
            Width := Form.ComponentsZone.Width;
            Height := 100;

            BevelOuter := bvNone;
            ParentColor := true;
          end;

        with FXButtonDesign.Create(ListPanel) do
          begin
            Parent := ListPanel;
            Left := 0;

            Image.SelectSegoe := #$E711;
            Text := 'None';
            Tag := 0;
          end;

        with FXButtonDesign.Create(ListPanel) do
          begin
            Parent := ListPanel;
            Left := 0;

            Image.SelectSegoe := #$EB9F;
            Text := 'Picture';
            Tag := 1;
          end;

        with FXButtonDesign.Create(ListPanel) do
          begin
            Parent := ListPanel;
            Left := 0;

            Image.SelectSegoe := #$E8BA;
            Text := 'Bitmap';
            Tag := 2;
          end;

        with FXButtonDesign.Create(ListPanel) do
          begin
            Parent := ListPanel;
            Left := 0;

            Image.SelectSegoe := #$E8B9;
            Text := 'Image List';
            Tag := 3;
          end;

        with FXButtonDesign.Create(ListPanel) do
          begin
            Parent := ListPanel;
            Left := 0;

            Image.SelectSegoe := #$F714;
            Text := 'Font Icon';
            Tag := 4;
          end;

          // Create Panels
          (* TPicture *)
          Panel := TPanel.Create(Form);
          with Panel do
            begin
              Parent := Form;
              Tag := 1;

              BevelOuter := bvNone;
              ParentBackground := false;
              Color := ThemeManager.SystemColor.BackGroundInterior;

              Top := ListPanel.BoundsRect.Bottom + Form.MarginTiny;
              Left := Form.ComponentsZone.Left;
              Width := Form.ComponentsZone.Width;
              Height := Form.ComponentsZone.Bottom - Top;

              LB1 := TLabel.Create(Panel);
              with LB1 do
                begin
                  Parent := Panel;

                  AutoSize := false;
                  Transparent := false;
                  Color := ThemeManager.SystemColor.Accent;

                  Layout := tlCenter;
                  Alignment := taCenter;


                  //Font.Name := ThemeManager.IconFont;
                  Caption := 'No image loaded';

                  Top := Form.MarginTiny;
                  Left := Form.MarginTiny;

                  Width := IMAGEBOX_SIZE;
                  Height := IMAGEBOX_SIZE;
                end;

              ImagePicture := TImage.Create(Panel);
              with ImagePicture do
                begin
                  Parent := Panel;

                  Proportional := true;
                  Center := true;

                  Top := Form.MarginTiny;
                  Left := Form.MarginTiny;

                  Width := IMAGEBOX_SIZE;
                  Height := IMAGEBOX_SIZE;

                  // Update Image
                  Picture.Assign( Item.SelectPicture );

                  LB1.Visible := (Item.SelectPicture.Graphic = nil) or Item.SelectPicture.Graphic.Empty;
                end;

              with FXButtonDesign.Create(Panel) do
                begin
                  Parent := Panel;

                  Top := Form.MarginTiny;
                  Left := Form.MarginTiny + IMAGEBOX_SIZE + Form.Margin;

                  Width := IMAGEBOX_SIZE;

                  Text := 'Browse';

                  Image.Enabled := true;
                  Image.IconType := FXIconType.SegoeIcon;
                  Image.SelectSegoe := #$E7C5;

                  ParentColor := true;

                  Tag := 1;
                  OnClick := ButtonImageAction;
                end;

              with FXButtonDesign.Create(Panel) do
                begin
                  Parent := Panel;

                  Top := Form.MarginTiny + Height + Form.MarginTiny;
                  Left := Form.MarginTiny + IMAGEBOX_SIZE + Form.Margin;

                  Width := IMAGEBOX_SIZE;

                  Text := 'Save';

                  Image.Enabled := true;
                  Image.IconType := FXIconType.SegoeIcon;
                  Image.SelectSegoe:= #$EA35;

                  ParentColor := true;

                  Tag := 2;
                  OnClick := ButtonImageAction;
                end;

              with FXButtonDesign.Create(Panel) do
                begin
                  Parent := Panel;

                  Top := Form.MarginTiny + (Height + Form.MarginTiny) * 2;
                  Left := Form.MarginTiny + IMAGEBOX_SIZE + Form.Margin;

                  Width := IMAGEBOX_SIZE;

                  Text := 'Clear';

                  Image.Enabled := true;
                  Image.IconType := FXIconType.SegoeIcon;
                  Image.SelectSegoe := #$ED62;

                  ParentColor := true;

                  Tag := 3;
                  OnClick := ButtonImageAction;
                end;
            end;

          (* TBitMap *)
          Panel := TPanel.Create(Form);
          with Panel do
            begin
              Parent := Form;
              Tag := 2;

              BevelOuter := bvNone;
              ParentBackground := false;
              Color := ThemeManager.SystemColor.BackGroundInterior;

              Top := ListPanel.BoundsRect.Bottom + Form.MarginTiny;
              Left := Form.ComponentsZone.Left;
              Width := Form.ComponentsZone.Width;
              Height := Form.ComponentsZone.Bottom - Top;

              LB2 := TLabel.Create(Panel);
              with LB2 do
                begin
                  Parent := Panel;

                  AutoSize := false;
                  Transparent := false;
                  Color := ThemeManager.SystemColor.Accent;

                  Layout := tlCenter;
                  Alignment := taCenter;


                  //Font.Name := ThemeManager.IconFont;
                  Caption := 'No bitmap loaded';

                  Top := Form.MarginTiny;
                  Left := Form.MarginTiny;

                  Width := IMAGEBOX_SIZE;
                  Height := IMAGEBOX_SIZE;
                end;

              ImageBitmap := TImage.Create(Panel);
              with ImageBitmap do
                begin
                  Parent := Panel;

                  Proportional := true;
                  Center := true;

                  Top := Form.MarginTiny;
                  Left := Form.MarginTiny;

                  Width := IMAGEBOX_SIZE;
                  Height := IMAGEBOX_SIZE;

                  // Update Image
                  Picture.Assign( Item.SelectBitmap );

                  LB2.Visible := Item.SelectBitmap.Empty;
                end;

              with FXButtonDesign.Create(Panel) do
                begin
                  Parent := Panel;

                  Top := Form.MarginTiny;
                  Left := Form.MarginTiny + IMAGEBOX_SIZE + Form.Margin;

                  Width := IMAGEBOX_SIZE;

                  Text := 'Browse';

                  Image.Enabled := true;
                  Image.IconType := FXIconType.SegoeIcon;
                  Image.SelectSegoe := #$E7C5;

                  ParentColor := true;

                  Tag := 4;
                  OnClick := ButtonImageAction;
                end;

              with FXButtonDesign.Create(Panel) do
                begin
                  Parent := Panel;

                  Top := Form.MarginTiny + Height + Form.MarginTiny;
                  Left := Form.MarginTiny + IMAGEBOX_SIZE + Form.Margin;

                  Width := IMAGEBOX_SIZE;

                  Text := 'Save';

                  Image.Enabled := true;
                  Image.IconType := FXIconType.SegoeIcon;
                  Image.SelectSegoe := #$EA35;

                  ParentColor := true;

                  Tag := 5;
                  OnClick := ButtonImageAction;
                end;

              with FXButtonDesign.Create(Panel) do
                begin
                  Parent := Panel;

                  Top := Form.MarginTiny + (Height + Form.MarginTiny) * 2;
                  Left := Form.MarginTiny + IMAGEBOX_SIZE + Form.Margin;

                  Width := IMAGEBOX_SIZE;

                  Text := 'Clear';

                  Image.Enabled := true;
                  Image.IconType := FXIconType.SegoeIcon;
                  Image.SelectSegoe := #$ED62;

                  ParentColor := true;

                  Tag := 6;
                  OnClick := ButtonImageAction;
                end;
            end;

        (* Image List *)
        Panel := TPanel.Create(Form);
          with Panel do
            begin
              Parent := Form;
              Tag := 3;

              BevelOuter := bvNone;
              ParentBackground := false;
              Color := ThemeManager.SystemColor.BackGroundInterior;

              Top := ListPanel.BoundsRect.Bottom + Form.MarginTiny;
              Left := Form.ComponentsZone.Left;
              Width := Form.ComponentsZone.Width;
              Height := Form.ComponentsZone.Bottom - Top;

              Caption := 'Work in progress...';
            end;

        (* Font Icon *)
        Panel := TPanel.Create(Form);
          with Panel do
            begin
              Parent := Form;
              Tag := 4;

              BevelOuter := bvNone;
              ParentBackground := false;
              Color := ThemeManager.SystemColor.BackGroundInterior;

              Top := ListPanel.BoundsRect.Bottom + Form.MarginTiny;
              Left := Form.ComponentsZone.Left;
              Width := Form.ComponentsZone.Width;
              Height := Form.ComponentsZone.Bottom - Top;

              Caption := '';

              with TLabel.Create(Panel) do
                begin
                  Parent := Panel;

                  Left := Form.MarginTiny;
                  Top := Form.MarginTiny;

                  Width := Panel.Width - Left * 2;

                  AutoSize := false;
                  WordWrap := true;
                  Caption := 'Paste the Font Unicode Character below, or enter the Unicode Point and press enter. You may paste multiple characters to overlay them';

                  Height := Form.Margin * 4 - Top;

                  OnKeyUp := EditInteract;
                end;

              FontIcon := TEdit.Create(Panel);
              with FontIcon do
                begin
                  Parent := Panel;

                  Left := Form.MarginTiny;
                  Top := Form.Margin * 4;

                  Color := ThemeManager.SystemColor.BackGround;

                  Width := Panel.Width - Left * 2;
                  Alignment := taCenter;

                  Font.Size := 50;
                  Font.Name := ThemeManager.IconFont;

                  OnKeyUp := EditInteract;

                  // Load Icon
                  Text := Item.SelectSegoe;
                end;
            end;

        // Prepare Buttons
        for I := 0 to ListPanel.ControlCount - 1 do
          if ListPanel.Controls[I] is FXButtonDesign then
            with FXButtonDesign(ListPanel.Controls[I]) do
              begin
                Align := alLeft;
                AlignWithMargins := true;
                Image.Enabled := true;
                Image.IconType := FXIconType.SegoeIcon;

                ParentColor := true;

                ImageLayout := cpTop;

                Width := ListPanel.Width div 5 - Margins.Left * 2;

                OnClick := ButtonSelect;

                FlatButton := (Tag = integer(Item.IconType));

                if FlatButton then
                  ShowPanel( Tag );
              end;

      end;

      // Save?
      if Form.ShowModal = mrOk then
        Self.SetOrdValue(Integer(Item));
  finally
    Form.Free;
  end;
end;

procedure TFXIconSelectProperty.EditInteract(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  Icon: string;
  StrLen: integer;
  ValidContender: boolean;
begin
  try
    // Obtain length
    StrLen := Length(FontIcon.Text);

    // Validate for unicode conversion
    ValidContender := false;
    if (not (ssAlt in Shift)) and (Key <> 18) then
      if (StrLen >= 4) and (StrLen <= 5) then
        begin
          if StrLen = 4 then
            ValidContender := true
          else
            if (StrLen = 5) and (FontIcon.Text[1] = '#') then
              begin
                FontIcon.Text := Copy(FontIcon.Text, 2, 4);

                ValidContender := true;
              end;

          // Alpha Numeric Unicode Point
          if not IsStringAlphaNumeric(FontIcon.Text) then
            ValidContender := false;
        end;

    // Convert to character
    if ValidContender then
      begin
        Icon := UnicodeToString(FontIcon.Text);

        FontIcon.Text := Icon;
      end;
  except

  end;

  // Update Icon
  Item.SelectSegoe := FontIcon.Text;
end;

function TFXIconSelectProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

function TFXIconSelectProperty.GetValue: string;
begin
  Result := '(FXIconSelect)';
end;

procedure TFXIconSelectProperty.SetValue(const Value: string);
begin
  inherited;
end;

procedure TFXIconSelectProperty.ShowPanel(Index: integer);
var
  I: Integer;
begin
  with Form do
    for I := 0 to ControlCount - 1 do
      if Controls[I] is TPanel then
        if (Controls[I] as TPanel).Tag > 0 then
          TPanel(Controls[I]).Visible := TPanel(Controls[I]).Tag = Index;
end;

{ FXEditForm }

function FXEditForm.ComponentsZone: TRect;
begin
  Result := Rect(ZONE_MARGIN, FTitle2.BoundsRect.Bottom + ZONE_MARGIN div 2, ClientWidth - ZONE_MARGIN, FButtonSave.Top - ZONE_MARGIN div 2);
end;

constructor FXEditForm.CreateNew(AOwner: TComponent; Dummy: Integer);
begin
  inherited;

  // Update Theme
  ThemeManager.DarkThemeMode := FXDarkSetting.ForceLight;

  // Defaults
  FMainTitle := 'Title';
  FSubTitle := 'Sub title';

  FAllowCancel := true;
  FStyled := true;

  // Form
  BorderIcons := [biSystemMenu];
  BorderStyle := bsSingle;
  Position := poMainFormCenter;

  Font.Name := 'Segoe UI';
  Font.Size := 12;

  Caption := 'Class Editor';

  // Theme
  if Styled then
    if ThemeManager.DarkTheme then
      begin
        Color := ThemeManager.SystemColor.BackGround;
        Font.Color := ThemeManager.SystemColor.ForeGround;
      end;

  // Labels
  FTitle1 := TLabel.Create(Self);
  with FTitle1 do
    begin
      Parent  := Self;

      Left := ZONE_MARGIN;
      Top := ZONE_MARGIN div 2;

      Font.Size := 18;
      Font.Name := 'Segoe UI Bold';
    end;

  FTitle2 := TLabel.Create(Self);
  with FTitle2 do
    begin
      Parent  := Self;

      Left := ZONE_MARGIN;
      Top := FTitle1.Top + FTitle1.Height;

      Font.Size := 14;
      Font.Name := 'Segoe UI SemiBold';
    end;

  // Buttons
  FButtonSave := FXButtonDesign.Create(Self);
  with FButtonSave do
    begin
      Parent  := Self;

      Left := Self.ClientWidth - Width - ZONE_MARGIN;
      Top := Self.ClientHeight - Height - ZONE_MARGIN div 2;

      Text := 'Save';
      Image.Enabled := true;
      Image.IconType := FXIconType.SegoeIcon;
      Image.SelectSegoe := #$E73E;

      ParentColor := true;

      Default := true;

      ModalResult := mrOk;

      Anchors := [akBottom, akRight];
    end;

  FButtonClose := FXButtonDesign.Create(Self);
  with FButtonClose do
    begin
      Parent  := Self;

      Left := Self.ClientWidth - Width - ZONE_MARGIN - FButtonSave.Width - 10;
      Top := Self.ClientHeight - Height - ZONE_MARGIN div 2;

      Text := 'Close';
      Image.Enabled := true;
      Image.IconType := FXIconType.SegoeIcon;
      Image.SelectSegoe := #$E8BB;

      ParentColor := true;

      Cancel := true;

      FlatButton := true;

      ModalResult := mrCancel;

      Anchors := [akBottom, akRight];
    end;

  // Size
  Width := 500;
  Height := 350;

  // Update
  UpdateUI;
end;

destructor FXEditForm.Destroy;
begin

  inherited;
end;

function FXEditForm.Margin: integer;
begin
  Result := ZONE_MARGIN;
end;

function FXEditForm.MarginTiny: integer;
begin
  Result := ZONE_MARGIN div 2;
end;

procedure FXEditForm.SetAllowCancel(const Value: boolean);
begin
  FAllowCancel := Value;
  UpdateUI;
end;

procedure FXEditForm.SetSubTitle(const Value: string);
begin
  FSubTitle := Value;
  UpdateUI;
end;

procedure FXEditForm.SetTitle(const Value: string);
begin
  FMainTitle := Value;
  UpdateUI;
end;

procedure FXEditForm.UpdateUI;
begin
  // New Values
  FTitle1.Caption := Title;
  FTitle2.Caption := SubTitle;

  // Cancel
  FButtonClose.Visible := FAllowCancel;
end;

{ TFXPictureImagesProperty }

procedure TFXPictureImagesProperty.Edit;
begin
  inherited;
  // Item
  Item := FXPictureImages(Self.GetOrdValue);

  Modified;
  Form := FXEditForm.CreateNew(Application);
  try
    // Edit
    Item.AddNewFromFile('F:\Assets\By Me\Arrow Left.png');

    // Data
    Form.Title := 'Edit Image list';
    Form.SubTitle := 'Pictures Count: ' + Item.Count.ToString;

    // Form
    Form.ShowModal;
  finally
    Form.Free;
  end;
end;

function TFXPictureImagesProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog];
end;

function TFXPictureImagesProperty.GetValue: string;
begin
  // Return the current value of the property as a string
  Result := '(FXPictureImages)';
end;

procedure TFXPictureImagesProperty.SetValue(const Value: string);
begin
  inherited;
end;

{ TFXPercentProperty }

function TFXPercentProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paValueEditable];
end;

function TFXPercentProperty.GetValue: string;
begin
  Result := Format('%.2f', [GetFloatValue]) + '%';
end;

procedure TFXPercentProperty.SetValue(const Value: string);
begin
  inherited;
  SetFloatValue(Value.Replace('%', '').ToExtended);
end;

{ TFXColorProperty }

procedure TFXColorProperty.DrawColorBox(ACanvas: TCanvas; ARect: TRect;
  AColor: FXColor);
const
  PEN_W = 1;
begin
  // Check Board
  DrawCheckedboard(ACanvas, ARect, 4, 4, clWhite, clGray);

  // Color
  ACanvas.GDIRectangle(ARect, AColor.MakeGDIBrush, FXColors.Black.MakeGDIPen(PEN_W));

  // Reset Style
  ACanvas.Brush.Style := bsClear;
end;

procedure TFXColorProperty.Edit(const Host: IPropertyHost; DblClick: Boolean);
begin
  inherited;
  // Open dialog editor
  if DblClick then
    begin

    end;
end;

function TFXColorProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paMultiSelect, paDialog, paValueList, paRevertable];
end;

function TFXColorProperty.GetValue: string;
begin
  Result := ColorToStringN( GetOrdValue );
end;

procedure TFXColorProperty.GetValues(Proc: TGetStrProc);
var
  I: FXColorID;
begin
  inherited;
  Proc(SET_CUSTOM);

  // Add Colors
  for I := Low(FXColorID) to High(FXColorID) do
    Proc( TFXColorNames[I] );
end;

procedure TFXColorProperty.ListDrawValue(const Value: string; ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean);
var
  R: TRect;
  S: string;
begin
  R := ARect;
  R.Left := COLBOX_WIDTH + COLBOX_SPACING;

  S := Value;
  with ACanvas do
    begin
      // Selected
      if ASelected then
        Brush.Color := clHighlight;
      FillRect(ARect);

      // Text
      TextRect(R, S, [tfSingleLine, tfVerticalCenter]);

      // Rect
      R := ARect;
      R.Width := COLBOX_WIDTH;

      R.Inflate(-2, -2);

      // Color
      try
        DrawColorBox(ACanvas, R, StringNToColor(Value));
      except
        DrawColorBox(ACanvas, R, FXColors.None);
      end;
    end;
end;

procedure TFXColorProperty.ListMeasureHeight(const Value: string;
  ACanvas: TCanvas; var AHeight: Integer);
begin
  inherited;
end;

procedure TFXColorProperty.ListMeasureWidth(const Value: string;
  ACanvas: TCanvas; var AWidth: Integer);
begin
  AWidth := ACanvas.TextWidth(Value) + COLBOX_WIDTH + COLBOX_SPACING;
end;

function TFXColorProperty.OpenEditor: FXColor;
begin
  Result := 0;
end;

procedure TFXColorProperty.PropDrawName(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
begin
  DefaultPropertyDrawName(TPropertyEditor(Self), ACanvas, ARect);
end;

function TFXColorProperty.PropDrawNameRect(const ARect: TRect): TRect;
begin
  inherited;
end;

procedure TFXColorProperty.PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
begin
  ARect.Inflate(0, -1);
  ARect.Inflate(-3, 0, 0, 0);

  DrawColorBox(ACanvas, ARect, FXColor(GetOrdValue));
end;

function TFXColorProperty.PropDrawValueRect(const ARect: TRect): TRect;
begin
  Result := Rect(ARect.Left-COLBOX_SPACING, ARect.Top, ARect.Left + COLBOX_WIDTH-COLBOX_SPACING*2, ARect.Bottom);
end;

procedure TFXColorProperty.SetValue(const Value: string);
var
  CID: FXColorID;
  Color: FXColor;
begin
  // Empty
  if Value = '' then
    Exit;

  // Analise
  { HEX }
  if Value[1] = '#' then
    begin
      Color := StrToIntDef('$' + Value.Remove(0, 1), 0);

      if Length(Value) = 6 + 1{#} then
        Color.SetAlpha(255);
    end
  else
  { Name }
  if FindColorName(Value, CID) then
    Color := GetColor(CID)
  else
  { Custom }
  if Value = SET_CUSTOM then
    Color := OpenEditor
  else
  { Numeric }
  try
    Color := Value.ToInteger;
  except
    raise Exception.Create('Invalid Color Type:'#13'Color types can be:'#13+
      '-HEX (#AARRGGBB)'#13'-Identifiers (Color Name)'#13'-Decimal');
  end;

  // Set
  SetOrdValue( Color );
end;

{ TFXAngleProperty }

function TFXAngleProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paValueEditable];
end;

function TFXAngleProperty.GetValue: string;
begin
  Result := Format('%.2f', [GetFloatValue]) + '°';
end;

procedure TFXAngleProperty.SetValue(const Value: string);
var
  S: string;
begin
  inherited;
  S := Value.Replace(' ', '').Replace('°', '').Replace('deg', '').ToLower;

  if S.EndsWith('pi') then begin
    S := S.Replace('pi', '');
    if S = '' then
      SetFloatValue( 180 )
    else
      SetFloatValue( S.Replace('pi', '').ToExtended*180 )
  end
  else
  if S.EndsWith('rad') then
    SetFloatValue( RadToDeg(S.Replace('rad', '').ToExtended) )
  else
    SetFloatValue(Value.Replace('°', '').ToExtended);
end;

initialization
  { Initialize }
  RegisterPropertyEditor(TypeInfo(FXIconSelect), nil, '', TFXIconSelectProperty);

  RegisterPropertyEditor(TypeInfo(FXPopupItems), nil, '', TFXPopupItemsProperty);
  RegisterPropertyEditor(TypeInfo(FXPictureImages), nil, '', TFXPictureImagesProperty);

  RegisterPropertyEditor(TypeInfo(FXPercent), nil, '', TFXPercentProperty);
  RegisterPropertyEditor(TypeInfo(FXAngle), nil, '', TFXAngleProperty);
  RegisterPropertyEditor(TypeInfo(FXColor), nil, '', TFXColorProperty);
  (*
  Parameter 1: Edited Class for Property Edit
  Parameter 2: Compoent to work with. Enter nil for it to work with all
  Parameter 3: Property Name, leave blank to work with any name
  Parameter 4: Property Edit Class
  *)

end.
