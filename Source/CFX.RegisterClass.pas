unit CFX.RegisterClass;

interface
  uses Classes, CFX.Classes, CFX.UIConsts,
       CFX.Button, CFX.Checkbox, CFX.Panels, CFX.StandardIcons,
       CFX.Slider, CFX.BlurMaterial, CFX.PopupMenu;


procedure Register;

implementation

procedure Register;
begin
  // Components
  RegisterComponents(REGISTER_CLASS_NAME,[FXMinimisePanel, FXPanel]);
  RegisterComponents(REGISTER_CLASS_NAME, [FXButton]);
  RegisterComponents(REGISTER_CLASS_NAME, [FXCheckBox]);
  RegisterComponents(REGISTER_CLASS_NAME, [FXStandardIcon]);
  RegisterComponents(REGISTER_CLASS_NAME, [FXSlider]);
  RegisterComponents(REGISTER_CLASS_NAME, [FXBlurMaterial]);
  RegisterComponents(REGISTER_CLASS_NAME, [FXPopupMenu]);
end;

end.
