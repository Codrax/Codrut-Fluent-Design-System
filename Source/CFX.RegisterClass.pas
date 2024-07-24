unit CFX.RegisterClass;

interface
  uses Classes, CFX.Classes, CFX.Constants, CFX.Controls,
       CFX.Button, CFX.ButtonDesign, CFX.Checkbox, CFX.Panels,
       CFX.StandardIcons, CFX.Slider, CFX.BlurMaterial, CFX.PopupMenu,
       CFX.ImageList, CFX.TextBox, CFX.RadioButton, CFX.Scrollbar,
       CFX.ScrollBox, CFX.Selector, CFX.Edit, CFX.PopupConnector,
       CFX.IconView, CFX.ScrollText, CFX.Progress, CFX.RatingControl,
       CFX.Effects, CFX.AppManager, CFX.PaintBox, CFX.TabStrip, CFX.Lists,
       CFX.Animation.Component, CFX.Layouts, CFX.Shapes;

procedure Register;

implementation

procedure Register;
begin
  // Visual Components
  RegisterComponents(REGISTER_CLASS_NAME,
    [
    FXMinimisePanel,
    FXPanel,

    FXButton,
    FXButtonDesign,
    FXPaintBox,
    FXCheckBox,
    FXStandardIcon,
    FXSlider,
    FXTabStrip,
    FXBlurMaterial,

    FXLinearDrawList,
    FXLinearControlList,

    FXTextBox,
    FXValueTextBox,
    FXAnimatedTextBox,
    FXScrollText,

    FXRadioButton,
    FXScrollbar,
    FXScrollBox,
    FXSelector,
    FXEdit,
    FXIconView,
    FXProgress,
    FXRatingControl
    ]
  );

  // Layouts
  RegisterComponents(REGISTER_CLASS_LAYOUTS,
    [
    FXLayout
    ]
  );

  // Shapes
  RegisterComponents(REGISTER_CLASS_SHAPES,
    [
    FXShapeSquare,
    FXShapeCircle,
    FXShapeTriangle,
    FXShapeTriangleCorner
    ]
  );

  // Effects
  RegisterComponents(REGISTER_CLASS_EFFECTS_NAME,
    [
    FXBlurEffect,
    FXColorEffect,
    FXZoomEffect,
    FXGrayscaleEffect,
    FXInvertEffect,
    FXDeepFryEffect,
    FXGlowEffect
    ]
  );

  // Animations
  RegisterComponents(REGISTER_CLASS_ANIM_NAME,
    [
    FXIntAnim,
    FXFloatAnim
    ]
  );

  // Non-Visual Components
  RegisterComponents(REGISTER_CLASS_NAME,
    [
    FXPopupMenu,
    FXImageList
    ]
  );

  // Utils
  RegisterComponents(REGISTER_CLASS_UTILS_NAME,
    [
    FXPopupConnector, FXAppManager
    ]
  );
end;

end.
