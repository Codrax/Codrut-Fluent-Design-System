unit CFX.RegisterClass;

interface
  uses Classes, CFX.Classes, CFX.Constants, CFX.Controls,
       CFX.Button, CFX.ButtonDesign, CFX.Checkbox, CFX.Panels,
       CFX.StandardIcons, CFX.Slider, CFX.BlurMaterial, CFX.PopupMenu,
       CFX.ImageList, CFX.TextBox, CFX.RadioButton, CFX.Scrollbar,
       CFX.ScrollBox, CFX.Selector, CFX.Edit, CFX.PopupConnector,
       CFX.IconView, CFX.ScrollText, CFX.Progress, CFX.RatingControl,
       CFX.Effects, CFX.AppManager, CFX.PaintBox, CFX.TabStrip, CFX.Lists,
       CFX.Animation.Component, CFX.Shapes;

procedure Register;

implementation

procedure Register;
begin
  // Visual Components
  RegisterComponents(REGISTER_CLASS_NAME, [FXMinimisePanel, FXPanel]);
  RegisterComponents(REGISTER_CLASS_NAME, [FXButton]);
  RegisterComponents(REGISTER_CLASS_NAME, [FXButtonDesign]);
  RegisterComponents(REGISTER_CLASS_NAME, [FXPaintBox]);
  RegisterComponents(REGISTER_CLASS_NAME, [FXCheckBox]);
  RegisterComponents(REGISTER_CLASS_NAME, [FXStandardIcon]);
  RegisterComponents(REGISTER_CLASS_NAME, [FXSlider]);
  RegisterComponents(REGISTER_CLASS_NAME, [FXTabStrip]);
  RegisterComponents(REGISTER_CLASS_NAME, [FXBlurMaterial]);

  RegisterComponents(REGISTER_CLASS_NAME, [FXLinearDrawList, FXLinearControlList]);

  RegisterComponents(REGISTER_CLASS_NAME, [FXTextBox, FXValueTextBox, FXAnimatedTextBox]);

  RegisterComponents(REGISTER_CLASS_NAME, [FXRadioButton]);
  RegisterComponents(REGISTER_CLASS_NAME, [FXScrollbar]);
  RegisterComponents(REGISTER_CLASS_NAME, [FXScrollBox]);
  RegisterComponents(REGISTER_CLASS_NAME, [FXSelector]);
  RegisterComponents(REGISTER_CLASS_NAME, [FXEdit]);
  RegisterComponents(REGISTER_CLASS_NAME, [FXIconView]);
  RegisterComponents(REGISTER_CLASS_NAME, [FXScrollText]);
  RegisterComponents(REGISTER_CLASS_NAME, [FXProgress]);
  RegisterComponents(REGISTER_CLASS_NAME, [FXRatingControl]);

  // Shapes
  RegisterComponents(REGISTER_CLASS_SHAPES, [FXShapeSquare, FXShapeCircle, FXShapeTriangle, FXShapeTriangleCorner]);

  // Effects
  RegisterComponents(REGISTER_CLASS_EFFECTS_NAME, [FXBlurEffect,
    FXColorEffect, FXZoomEffect, FXGrayscaleEffect, FXInvertEffect,
    FXDeepFryEffect, FXGlowEffect]);

  // Animations
  RegisterComponents(REGISTER_CLASS_ANIM_NAME, [FXIntAnim, FXFloatAnim]);

  // Non-Visual Components
  RegisterComponents(REGISTER_CLASS_NAME, [FXPopupMenu, FXImageList]);

  // Utils
  RegisterComponents(REGISTER_CLASS_UTILS_NAME, [FXPopupConnector]);
  RegisterComponents(REGISTER_CLASS_UTILS_NAME, [FXAppManager]);
end;

end.
