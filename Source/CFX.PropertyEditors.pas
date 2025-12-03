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
  Vcl.Imaging.pngimage,
  Vcl.Imaging.GIFImg,
  Vcl.Imaging.jpeg,
  CFX.QuickDialogs,
  CFX.Utilities,
  CFX.ThemeManager,
  CFX.BlurMaterial,
  CFX.Classes,
  CFX.ComponentClasses,
  CFX.Constants,
  CFX.Colors,
  CFX.Math,
  CFX.GDI,
  CFX.Types,
  CFX.Forms,
  CFX.FontIcons,
  CFX.Version,
  CFX.ButtonDesign,
  CFX.Lists,
  CFX.Button,
  CFX.Edit,
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
    FButtonClose: FXButton;

    FAllowCancel: boolean;
    FStyled: boolean;

    const
      ZONE_MARGIN = 20;

    procedure SetSubTitle(const Value: string);
    procedure SetTitle(const Value: string);
    procedure SetAllowCancel(const Value: boolean);

  public
    const
      BUTTON_DEFAULT_HEIGHT = 50;

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

  // FXPictureList Items
  TFXPictureListProperty = class(TPropertyEditor)
  private
    Form: FXEditForm;
    Item: FXPictureList;

    FList: FXLinearDrawList;
    FActionButton: array[0..5] of FXButton;

    procedure UpdateUIInfo;

    procedure ShowPreviewFor(Index: integer);

    procedure DoListDrawItem(Sender: TObject; AIndex: integer; ARect: TRect; Canvas: TCanvas);
    procedure DoActionButtonPress(Sender: TObject);
    procedure DoImageActionButtonPress(Sender: TObject);
    procedure DoListItemSelect(Sender: TObject);
    procedure DoListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoListDoubleClick(Sender: TObject);

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
    FontIcon: FXEdit;

    const
      IMAGEBOX_SIZE = 150;

    procedure DoNumberEditChange(Sender: TObject);

  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    procedure GetProperties(Proc: TGetPropProc); override;
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
  case FXButton(Sender).Tag of
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
      if (Item.SelectPicture.Graphic = nil) or Item.SelectPicture.Graphic.Empty then
        Exit;
      const Img = Item.SelectPicture.Graphic;
      with TSavePictureDialog.Create(nil) do begin
        FileName := 'Image';
        if Img is TPNGImage then FileName := FileName + '.png';
        if Img is TJPEGImage then FileName := FileName + '.jpeg';
        if Img is TGifImage then FileName := FileName + '.gif';
        if Img is TBitMap then FileName := FileName + '.bmp';
        if Execute then
          Img.SaveToFile(FileName);
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
      with TOpenPictureDialog.Create(nil) do begin
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
      if (Item.SelectBitmap = nil) or Item.SelectBitmap.Empty then
        Exit;
      with TSavePictureDialog.Create(nil) do begin
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
  Item.IconType := FXIconType(FXButton(Sender).Tag);

  for I := 0 to FXButton(Sender).Parent.ControlCount - 1 do
    if FXButton(Sender).Parent.Controls[I] is FXButton then
      FXButton(FXButton(Sender).Parent.Controls[I]).Checked := false;

  FXButton(Sender).Checked := true;

  ShowPanel( FXButton(Sender).Tag );
end;

procedure TFXIconSelectProperty.DoNumberEditChange(Sender: TObject);
begin
  Item.SelectImageIndex := FXNumberEdit(Sender).ValueInt;
end;

procedure TFXIconSelectProperty.Edit;
var
  ListPanel, Panel: TPanel;
  I: integer;
begin
  inherited;
  // Item
  const Original = FXIconSelect(Self.GetOrdValue);
  Item := FXIconSelect.Create(nil);
  Item.Assign(Original);

  // Form
  Form := FXEditForm.CreateNew(Application);
  try
    with Form do begin
      // Data
      Width := 500;
      Height := 450;
      Caption := 'Editing Icon Item';

      Title := 'Edit Selected Image';
      SubTitle := 'Choose a icon';

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

      with FXButton.Create(ListPanel) do
        begin
          Parent := ListPanel;
          ButtonKind := FXButtonKind.Toggle;
          Left := 0;

          Image.SelectSegoe := #$E711;
          Text := 'None';
          Tag := 0;
        end;

      with FXButton.Create(ListPanel) do
        begin
          Parent := ListPanel;
          ButtonKind := FXButtonKind.Toggle;
          Left := 0;

          Image.SelectSegoe := #$EB9F;
          Text := 'Picture';
          Tag := 1;
        end;

      with FXButton.Create(ListPanel) do
        begin
          Parent := ListPanel;
          ButtonKind := FXButtonKind.Toggle;
          Left := 0;

          Image.SelectSegoe := #$E8BA;
          Text := 'Bitmap';
          Tag := 2;
        end;

      with FXButton.Create(ListPanel) do
        begin
          Parent := ListPanel;
          ButtonKind := FXButtonKind.Toggle;
          Left := 0;

          Image.SelectSegoe := #$E8B9;
          Text := 'Image List';
          Tag := 3;
        end;

      with FXButton.Create(ListPanel) do
        begin
          Parent := ListPanel;
          ButtonKind := FXButtonKind.Toggle;
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

            with FXButton.Create(Panel) do
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

            with FXButton.Create(Panel) do
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

            with FXButton.Create(Panel) do
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

            with FXButton.Create(Panel) do
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

            with FXButton.Create(Panel) do
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

            with FXButton.Create(Panel) do
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

            with TLabel.Create(Panel) do
              begin
                Parent := Panel;

                Left := Form.MarginTiny;
                Top := Form.MarginTiny;

                Width := Panel.Width - Left * 2;

                AutoSize := false;
                WordWrap := true;
                Caption := 'Image index';

                Height := Form.Margin * 4 - Top;
              end;

            with FXNumberEdit.Create(Panel) do begin
              Parent := Panel;

              Left := Form.MarginTiny;
              Top := Form.MarginTiny + 30;

              Width := Panel.Width - Left * 2;

              ValueInt :=  Item.SelectImageIndex;

              Range.Min := -1;
              Range.Max := integer.MaxValue;
              Range.Enabled := true;

              OnChange := DoNumberEditChange;
              //OnExit := DoNumberEditChange;
            end;
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
              end;

            FontIcon := FXEdit.Create(Panel);
            with FontIcon do
              begin
                Parent := Panel;

                Left := Form.MarginTiny;
                Top := Form.Margin * 4;

                Color := ThemeManager.SystemColor.BackGround;

                Width := Panel.Width - Left * 2;
                LayoutHorizontal := TLayout.Center;

                Font.Size := 50;
                Font.Name := ThemeManager.IconFont;

                OnKeyUp := EditInteract;

                // Load Icon
                Text := Item.SelectSegoe;
              end;
          end;

      // Prepare Buttons
      for I := 0 to ListPanel.ControlCount - 1 do
        if ListPanel.Controls[I] is FXButton then
          with FXButton(ListPanel.Controls[I]) do
            begin
              Align := alLeft;
              AlignWithMargins := true;
              Image.Enabled := true;
              Image.IconType := FXIconType.SegoeIcon;

              ParentColor := true;

              ImageLayout := FXDrawLayout.Top;

              Width := ListPanel.Width div 5 - Margins.Left * 2;

              OnClick := ButtonSelect;

              Checked := (Tag = integer(Item.IconType));

              if Checked then
                ShowPanel( Tag );
            end;

    end;

    // Save
    if Form.ShowModal = mrOk then
      Original.Assign(Item);
  finally
    Form.Free;
    Item.Free;
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
  Result := [paDialog, paMultiSelect, paSubProperties, paReadOnly];
end;

procedure TFXIconSelectProperty.GetProperties(Proc: TGetPropProc);
var
  I: Integer;
  J: Integer;
  Components: IDesignerSelections;
begin
  Components := TDesignerSelections.Create;
  for I := 0 to PropCount - 1 do
  begin
    J := GetOrdValueAt(I);
    if J <> 0 then
      Components.Add(TComponent(GetOrdValueAt(I)));
  end;
  if Components.Count > 0 then
    GetComponentProperties(Components, tkProperties, Designer, Proc);
end;

function TFXIconSelectProperty.GetValue: string;
begin
  FmtStr(Result, '(%s)', [GetTypeName(GetPropType)]);
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
  if Styled then begin
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
  FButtonSave := FXButton.Create(Self);
  with FButtonSave do
    begin
      Parent  := Self;

      Height := BUTTON_DEFAULT_HEIGHT;

      Left := Self.ClientWidth - Width - ZONE_MARGIN;
      Top := Self.ClientHeight - Height - ZONE_MARGIN div 2;

      Text := 'Save';
      Image.Enabled := true;
      Image.IconType := FXIconType.SegoeIcon;
      Image.SelectSegoe := #$E73E;

      ParentColor := true;

      Default := true;
      ButtonKind := FXButtonKind.Accent;

      ModalResult := mrOk;

      Anchors := [akBottom, akRight];
    end;

  FButtonClose := FXButton.Create(Self);
  with FButtonClose do
    begin
      Parent  := Self;

      Height := BUTTON_DEFAULT_HEIGHT;

      Left := Self.ClientWidth - Width - ZONE_MARGIN - FButtonSave.Width - 10;
      Top := Self.ClientHeight - Height - ZONE_MARGIN div 2;

      Text := 'Close';
      Image.Enabled := true;
      Image.IconType := FXIconType.SegoeIcon;
      Image.SelectSegoe := #$E711;

      ParentColor := true;

      Cancel := true;

      ModalResult := mrCancel;

      Anchors := [akBottom, akRight];
    end;

  // Size
  Width := 700;
  Height := 500;

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

{ TFXPictureListProperty }

procedure TFXPictureListProperty.DoActionButtonPress(Sender: TObject);
begin
  case TComponent(Sender).Tag of
    1: begin
      with TOpenPictureDialog.Create(nil) do begin
        Options := [TOpenOption.ofAllowMultiSelect];
        if Execute then begin
          for var I := 0 to Files.Count-1 do begin
            try
              Item.AddNewFromFile(Files[I]);
            except
            end;
          end;

          // UI
          UpdateUIInfo;
        end;
      end;
    end;
    2: begin
      Item.Clear;

      // UI
      UpdateUIInfo;
    end;
  end;
end;

procedure TFXPictureListProperty.DoImageActionButtonPress(Sender: TObject);
begin
  case TComponent(Sender).Tag of
    // Delete
    1: begin
      const Selected = FList.GetSelectedItems;
      for var I := High(Selected) downto 0 do
        Item.Delete(Selected[I]);

      // UI
      UpdateUIInfo;
    end;
    // Save
    2: begin
      const Img = Item.Pictures[FList.ItemIndex].Graphic;
      if Img = nil then Exit;
      with TSavePictureDialog.Create(nil) do begin
        FileName := Format('Image %d', [FList.ItemIndex]);
        if Img is TPNGImage then FileName := FileName + '.png';
        if Img is TJPEGImage then FileName := FileName + '.jpeg';
        if Img is TGifImage then FileName := FileName + '.gif';
        if Img is TBitMap then FileName := FileName + '.bmp';
        if Execute then
          Img.SaveToFile(FileName);
      end;
    end;
    // Replace
    3: begin
      with TOpenPictureDialog.Create(nil) do
        if Execute then
          if TFile.Exists(FileName) then begin
            const P = TPicture.Create;
            try
              P.LoadFromFile(FileName);

              Item.Pictures[FList.ItemIndex].Assign(P);
            finally
              P.Free;
            end;
          end;

      // UI
      FList.Redraw;
    end;
    // View
    4: ShowPreviewFor(FList.ItemIndex);
    // Duplicate
    5: begin
      Item.AddNew( Item[FList.ItemIndex] );

      // UI
      UpdateUIInfo;
    end;
    // Scale (down)
    6: begin
      const Selected = FList.GetSelectedItems;
      if Length(Selected) = 0 then Exit;
      var ScaleBy := OpenDialog('Scale by', 'What should the minimum size scale factor be?', ['Cancel', 'Width', 'Height', 'Both', 'Both (fit 1:1)', 'Both (fill 1:1)']);
      if ScaleBy = 0 then Exit;

      var AScaleMin: integer := 256;
      var Img := Item.Pictures[Selected[0]].Graphic;
      if (Img <> nil) and not Img.Empty then
        case ScaleBy of
          1: AScaleMin := Img.Width;
          2: AScaleMin := Img.Height;
          3, 4, 5: AScaleMin := Max(Img.Height, Img.Width);
        end;

      if not OpenInput('Scale down', 'Scale to the following value (px).', AScaleMin) then
        Exit;

      // Scale down
      for var I := 0 to High(Selected) do begin
        Img := Item.Pictures[Selected[I]].Graphic;
        var NewW := Img.Width;
        var NewH := Img.Height;
        var LocalScale := ScaleBy;

        // New graphic
        if LocalScale = 3 then begin
          // NewW > NewH
          if NewW > NewH then begin
            if NewH > AScaleMin then
              LocalScale := 2
            else
              LocalScale := 1;
          end else
          // NewH > NewW or equal
          begin
            if NewW > AScaleMin then
              LocalScale := 1
            else
              LocalScale := 2;
          end;
        end;

        case LocalScale of
          1: if NewW > AScaleMin then begin
            NewH := round(AScaleMin / NewW * NewH);
            NewW := AScaleMin;
          end;
          2: if NewH > AScaleMin then begin
            NewW := round(AScaleMin / NewH * NewW);
            NewH := AScaleMin;
          end;
          4, 5: if (NewW <> NewH) or (NewW > AScaleMin) or (NewH > AScaleMin) then begin
            NewW := AScaleMin;
            NewH := AScaleMin;
          end;
        end;

        // Render
        if (NewW <> Img.Width) or (NewH <> Img.Height) then begin
          // Draw
          var B := TBitmap.Create;
          try
            // Select format
            if ((Img is TBitMap) and (TBitMap(Img).PixelFormat = pf24bit)) or
              (Img is TJPEGImage) then
                B.PixelFormat := pf24bit
                  else
                    B.PixelFormat := pf32bit;
            B.Transparent := Img.Transparent;

            // Generate
            B.SetSize(NewW, NewH);

            // New aspect ratio?
            if LocalScale >= 4 then begin
              if LocalScale = 4 then
                DrawImageInRect(B.Canvas, B.Canvas.ClipRect, Img, TDrawMode.CenterFit)
              else
                DrawImageInRect(B.Canvas, B.Canvas.ClipRect, Img, TDrawMode.CenterFill);
            end else begin
              B.Canvas.StretchDraw(Rect(0,0,NewW,NewH), Img);
            end;

            // Re-assign to preserve correct format
            try
              Img.Assign(B);
              Item.Pictures[Selected[I]].Graphic := Img;
            except
              // Nevermind, keep bitmap! :))
              Item.Pictures[Selected[I]].Graphic := B;
            end;
          finally
            B.Free;
          end;
        end;
      end;

      // UI
      UpdateUIInfo;
    end;
  end;
end;

procedure TFXPictureListProperty.DoListDoubleClick(Sender: TObject);
begin
  ShowPreviewFor( FList.ItemIndex );
end;

procedure TFXPictureListProperty.DoListDrawItem(Sender: TObject;
  AIndex: integer; ARect: TRect; Canvas: TCanvas);
var
  R: TRect;
  Img: TGraphic;
begin
  with Canvas do begin
    R := ARect;
    R.Height := R.Width;

    // Draw
    try
      Img := Item.Pictures[AIndex].Graphic;
      if Img <> nil then
        DrawImageInRect(Canvas, R, Img, TDrawMode.CenterFit);
    except
      Img := nil;
      DrawTextRect(Canvas, R, 'An error occured previewing the image', [
        FXTextFlag.Center, FXTextFlag.VerticalCenter, FXTextFlag.WordWrap]);
    end;

    // Info
    R := ARect;
    R.Top := R.Top + R.Width;
    R.Inflate(-4, -4);
    Canvas.Brush.Color := ThemeManager.SystemColor.BackGround;
    Canvas.FillRect(R);
    R.Inflate(-4, 0);

    if Img = nil then
      DrawTextRect(Canvas, R, 'Information cannot be displayed.', [
        FXTextFlag.Center, FXTextFlag.VerticalCenter, FXTextFlag.WordWrap])
    else
      DrawTextRect(Canvas, R, Format(
        'Width: %d'#13+
        'Height: %d'#13+
        'Type: %s'
        , [Img.Width, Img.Height, Img.ClassName]),
        [FXTextFlag.VerticalCenter, FXTextFlag.WordWrap]);
  end;
end;

procedure TFXPictureListProperty.DoListItemSelect(Sender: TObject);
begin
  UpdateUIInfo;
end;

procedure TFXPictureListProperty.DoListKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  const Selected = FList.GetSelectedItems;
  case Key of
    VK_DELETE: begin
      for var I := High(Selected) downto 0 do
        Item.Delete(Selected[I]);

      // UI
      UpdateUIInfo;
    end;
  end;
end;

procedure TFXPictureListProperty.Edit;
begin
  inherited;
  // Item
  const Original = FXPictureList(Self.GetOrdValue);
  Item := FXPictureList.Create;
  Item.Assign(Original);

  // Form
  Form := FXEditForm.CreateNew(Application);
  try
    // Edit
    //Item.AddNewFromFile('F:\Assets\By Me\Arrow Left.png');
    Form.Height := 500;

    // Data
    Form.Title := 'Edit Image list';

    // Controls
    FList := FXLinearDrawList.Create(Form);
    with FList do begin
      Parent := Form;

      //DragMode := dmAutomatic;
      CanDeselect := true;

      FullLine := true;
      Orientation := FXOrientation.Horizontal;

      Top := Form.ComponentsZone.Top;
      Left := Form.ComponentsZone.Left;
      Width := Form.ComponentsZone.Width;
      Height := 250;

      ScrollExtendX := 0;

      MultiSelect := true;

      OnDrawItem := DoListDrawItem;
      OnItemSelect := DoListItemSelect;
      OnKeyDown := DoListKeyDown;
      OnItemDoubleClick := DoListDoubleClick;

      NoItemsOutputText := 'No images. Add some to begin';
    end;

    // Btns-action
    var CurLeft: integer := Form.ZONE_MARGIN;
    FActionButton[0] := FXButton.Create(Form);
    with FActionButton[0] do begin
      Parent := Form;

      Width := 60; Text := '';
      Top := FList.BoundsRect.Bottom+5;
      Left := CurLeft; Inc(CurLeft, Width+10);

      Image.IconType := FXIconType.SegoeIcon;
      Image.SelectSegoe := #$E74D;

      Tag := 1;
      OnClick := DoImageActionButtonPress;
    end;
    FActionButton[3] := FXButton.Create(Form);
    with FActionButton[3] do begin
      Parent := Form;

      Width := 60; Text := '';
      Top := FList.BoundsRect.Bottom+5;
      Left := CurLeft; Inc(CurLeft, Width+10);

      Image.IconType := FXIconType.SegoeIcon;
      Image.SelectSegoe := #$E7B3;

      Tag := 4;
      OnClick := DoImageActionButtonPress;
    end;
    FActionButton[5] := FXButton.Create(Form);
    with FActionButton[5] do begin
      Parent := Form;

      Width := 110;
      Top := FList.BoundsRect.Bottom+5;
      Left := CurLeft; Inc(CurLeft, Width+10);

      Image.IconType := FXIconType.SegoeIcon;
      Image.SelectSegoe := #$E61F;
      Text := 'Scale';

      Tag := 6;
      OnClick := DoImageActionButtonPress;
    end;
    FActionButton[1] := FXButton.Create(Form);
    with FActionButton[1] do begin
      Parent := Form;

      Width := 110;
      Top := FList.BoundsRect.Bottom+5;
      Left := CurLeft; Inc(CurLeft, Width+10);

      Image.IconType := FXIconType.SegoeIcon;
      Image.SelectSegoe := #$E78C;
      Text := 'Save';

      Tag := 2;
      OnClick := DoImageActionButtonPress;
    end;
    FActionButton[2] := FXButton.Create(Form);
    with FActionButton[2] do begin
      Parent := Form;

      Width := 110;
      Top := FList.BoundsRect.Bottom+5;
      Left := CurLeft; Inc(CurLeft, Width+10);

      Image.IconType := FXIconType.SegoeIcon;
      Image.SelectSegoe := #$E8EE ;
      Text := 'Replace';

      Tag := 3;
      OnClick := DoImageActionButtonPress;
    end;
    FActionButton[4] := FXButton.Create(Form);
    with FActionButton[4] do begin
      Parent := Form;

      Width := 110;
      Top := FList.BoundsRect.Bottom+5;
      Left := CurLeft; Inc(CurLeft, Width+10);

      Image.IconType := FXIconType.SegoeIcon;
      Image.SelectSegoe := #$E8C8;
      Text := 'Clone';

      Tag := 5;
      OnClick := DoImageActionButtonPress;
    end;

    // Btns-bottom
    with FXButton.Create(Form) do begin
      Parent := Form;

      ButtonKind := FXButtonKind.Flat;
      Height := Form.BUTTON_DEFAULT_HEIGHT;

      Top := Form.FButtonClose.Top;
      Left := Form.ZONE_MARGIN;

      Image.IconType := FXIconType.SegoeIcon;
      Image.SelectSegoe := #$E7C5;
      Text := 'Add pictures...';

      Tag := 1;
      OnClick := DoActionButtonPress;
    end;
     with FXButton.Create(Form) do begin
      Parent := Form;

      ButtonKind := FXButtonKind.Flat;
      Height := Form.BUTTON_DEFAULT_HEIGHT;

      Top := Form.FButtonClose.Top;
      Left := Form.ZONE_MARGIN + (10+Width);

      Image.IconType := FXIconType.SegoeIcon;
      Image.SelectSegoe := #$E8E6;
      Text := 'Clear';

      Tag := 2;
      OnClick := DoActionButtonPress;
    end;

    //
    UpdateUIInfo;

    // Save
    if Form.ShowModal = mrOk then
      Original.Assign(Item);
  finally
    Form.Free;
    Item.Free;
  end;
end;

function TFXPictureListProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

function TFXPictureListProperty.GetValue: string;
begin
  FmtStr(Result, '(%s)', [GetTypeName(GetPropType)]);
end;

procedure TFXPictureListProperty.SetValue(const Value: string);
begin
  inherited;
end;

procedure TFXPictureListProperty.ShowPreviewFor(Index: integer);
begin
  const Img = Item.Pictures[Index].Graphic;
  if Img = nil then Exit;
  const F = TForm.Create(nil);
  with F do
    try
      Position := poScreenCenter;
      Color := 0;

      Caption := Format('Image preview - %dx%d (%s)', [Img.Width, Img.Height, Img.ClassName]);
      with TImage.Create(F) do begin
        Parent := F;
        Align := alClient;
        Center := true;

        Picture.Assign(Img);
      end;

      ShowModal;
    finally
      Free;
    end;
end;

procedure TFXPictureListProperty.UpdateUIInfo;
begin
  Form.SubTitle := 'Picture Count: ' + Item.Count.ToString;
  if (FList.ItemIndex = 1) and (FList.SelectedItemCount>0) then
    Form.SubTitle := Form.SubTitle + Format(', image %d', [FList.ItemIndex]);
  FList.ItemCount := Item.Count;

  const HasSel = FList.SelectedItemCount>0;
  const HasOneSel = FList.SelectedItemCount=1;
  FActionButton[0].Enabled := HasSel;
  FActionButton[1].Enabled := HasOneSel;
  FActionButton[2].Enabled := HasOneSel;
  FActionButton[3].Enabled := HasOneSel;
  FActionButton[4].Enabled := HasOneSel;
  FActionButton[5].Enabled := HasSel;
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
  RegisterPropertyEditor(TypeInfo(FXPictureList), nil, '', TFXPictureListProperty);

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
