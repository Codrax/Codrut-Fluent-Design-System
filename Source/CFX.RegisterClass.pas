unit CFX.RegisterClass;

interface
  uses Classes, CFX.Classes, CFX.UIConsts,
       CFX.Button, CFX.ButtonDesign, CFX.Checkbox, CFX.Panels,
       CFX.StandardIcons, CFX.Slider, CFX.BlurMaterial, CFX.PopupMenu,
       CFX.ImageList, CFX.TextBox, CFX.RadioButton, CFX.Scrollbar,
       CFX.ScrollBox, CFX.Selector, CFX.Edit, CFX.PopupConnector,
       CFX.IconView, CFX.ScrollText, CFX.Progress;


procedure Register;

implementation

procedure Register;
begin
  // Visual Components
  RegisterComponents(REGISTER_CLASS_NAME,[FXMinimisePanel, FXPanel]);
  RegisterComponents(REGISTER_CLASS_NAME, [FXButton]);
  RegisterComponents(REGISTER_CLASS_NAME, [FXButtonDesign]);
  RegisterComponents(REGISTER_CLASS_NAME, [FXCheckBox]);
  RegisterComponents(REGISTER_CLASS_NAME, [FXStandardIcon]);
  RegisterComponents(REGISTER_CLASS_NAME, [FXSlider]);
  RegisterComponents(REGISTER_CLASS_NAME, [FXBlurMaterial]);
  RegisterComponents(REGISTER_CLASS_NAME, [FXTextBox]);
  RegisterComponents(REGISTER_CLASS_NAME, [FXAnimatedTextBox]);
  RegisterComponents(REGISTER_CLASS_NAME, [FXRadioButton]);
  RegisterComponents(REGISTER_CLASS_NAME, [FXScrollbar]);
  RegisterComponents(REGISTER_CLASS_NAME, [FXScrollBox]);
  RegisterComponents(REGISTER_CLASS_NAME, [FXSelector]);
  RegisterComponents(REGISTER_CLASS_NAME, [FXEdit]);
  RegisterComponents(REGISTER_CLASS_NAME, [FXIconView]);
  RegisterComponents(REGISTER_CLASS_NAME, [FXScrollText]);
  RegisterComponents(REGISTER_CLASS_NAME, [FXProgress]);

  // Non-Visual Components
  RegisterComponents(REGISTER_CLASS_NAME, [FXPopupMenu]);
  RegisterComponents(REGISTER_CLASS_NAME, [FXImageList]);

  // Utils
  RegisterComponents(REGISTER_CLASS_UTILS_NAME, [FXPopupConnector]);
end;

end.
