object Form1: TForm1
  Left = 0
  Top = 0
  AlphaBlend = True
  Caption = 'Codrut Fluent Design'
  ClientHeight = 661
  ClientWidth = 1184
  Color = clBtnFace
  CustomTitleBar.CaptionAlignment = taCenter
  CustomTitleBar.Control = FXTitleBarPanel1
  CustomTitleBar.Height = 30
  CustomTitleBar.SystemHeight = False
  CustomTitleBar.ShowIcon = False
  CustomTitleBar.SystemColors = False
  CustomTitleBar.SystemButtons = False
  CustomTitleBar.BackgroundColor = clWhite
  CustomTitleBar.ForegroundColor = 65793
  CustomTitleBar.InactiveBackgroundColor = clWhite
  CustomTitleBar.InactiveForegroundColor = 10066329
  CustomTitleBar.ButtonForegroundColor = 65793
  CustomTitleBar.ButtonBackgroundColor = clWhite
  CustomTitleBar.ButtonHoverForegroundColor = 65793
  CustomTitleBar.ButtonHoverBackgroundColor = 16053492
  CustomTitleBar.ButtonPressedForegroundColor = 65793
  CustomTitleBar.ButtonPressedBackgroundColor = 15395562
  CustomTitleBar.ButtonInactiveForegroundColor = 10066329
  CustomTitleBar.ButtonInactiveBackgroundColor = clWhite
  TransparentColor = True
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -16
  Font.Name = 'Segoe UI'
  Font.Style = []
  GlassFrame.Top = 30
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    1184
    661)
  TextHeight = 21
  object PaintBox1: TPaintBox
    Left = 1038
    Top = 441
    Width = 138
    Height = 128
    OnPaint = PaintBox1Paint
  end
  object FXStandardIcon1: FXStandardIcon
    Left = 15
    Top = 33
    Width = 37
    Height = 37
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Size.X = 37
    Size.Y = 37
    Position.X = 15
    Position.Y = 33
    CustomColors.Enabled = False
    CustomColors.Accent = 13924352
    CustomColors.LightBackGround = 15987699
    CustomColors.LightForeGround = 1776410
    CustomColors.DarkBackGround = 2105376
    CustomColors.DarkForeGround = clWhite
    Color = 15987699
    SelectedIcon = Checkmark
    PenWidth = 10
  end
  object FXEdit1: FXEdit
    Left = 237
    Top = 392
    Width = 125
    Height = 33
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Size.X = 125
    Size.Y = 33
    Position = 0
    CustomColors.Enabled = False
    CustomColors.Accent = 13924352
    CustomColors.LightBackGround = 15987699
    CustomColors.LightForeGround = 1776410
    CustomColors.DarkBackGround = 2105376
    CustomColors.DarkForeGround = clWhite
    CustomColors.LightBackGroundInterior = 16514043
    CustomColors.DarkBackGroundInterior = 2829099
    CustomEditColors.Enabled = False
    CustomEditColors.Accent = 13924352
    CustomEditColors.LightNone = 9145227
    CustomEditColors.LightHover = 10461087
    CustomEditColors.LightPress = 7697781
    CustomEditColors.DarkNone = 10657693
    CustomEditColors.DarkHover = 12039603
    CustomEditColors.DarkPress = 9275783
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 1776410
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = []
    TabOrder = 0
    ParentColor = False
    Text = ''
    PasswordChar = #0
    Value = 0
    SelectionLength = 0
    TextHint = 'Username'
    ReadOnly = False
    TextMarginX = 5
    TextMarginY = 5
  end
  object FXEdit2: FXEdit
    Left = 237
    Top = 463
    Width = 125
    Height = 33
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Size.X = 125
    Size.Y = 33
    Position = 0
    CustomColors.Enabled = False
    CustomColors.Accent = 13924352
    CustomColors.LightBackGround = 15987699
    CustomColors.LightForeGround = 1776410
    CustomColors.DarkBackGround = 2105376
    CustomColors.DarkForeGround = clWhite
    CustomColors.LightBackGroundInterior = 16514043
    CustomColors.DarkBackGroundInterior = 2829099
    CustomEditColors.Enabled = False
    CustomEditColors.Accent = 13924352
    CustomEditColors.LightNone = 9145227
    CustomEditColors.LightHover = 10461087
    CustomEditColors.LightPress = 7697781
    CustomEditColors.DarkNone = 10657693
    CustomEditColors.DarkHover = 12039603
    CustomEditColors.DarkPress = 9275783
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 1776410
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = []
    TabOrder = 1
    ParentColor = False
    Text = ''
    PasswordChar = '*'
    Value = 0
    SelectionLength = 0
    TextHint = 'Password'
    ReadOnly = False
    TextMarginX = 5
    TextMarginY = 5
  end
  object FXButton1: FXButton
    Left = 237
    Top = 504
    Width = 125
    Height = 76
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Size.X = 125
    Size.Y = 76
    Position.X = 237
    Position.Y = 504
    CustomColors.Enabled = False
    CustomColors.Accent = 13924352
    CustomColors.LightBackGround = 15987699
    CustomColors.LightForeGround = 1776410
    CustomColors.DarkBackGround = 2105376
    CustomColors.DarkForeGround = clWhite
    CustomColors.LightBackGroundInterior = 16514043
    CustomColors.DarkBackGroundInterior = 2829099
    CustomButtonColors.Enabled = False
    CustomButtonColors.Accent = clBlack
    CustomButtonColors.LightBackgroundNone = clBlack
    CustomButtonColors.LightBackgroundHover = clBlack
    CustomButtonColors.LightBackgroundPress = clBlack
    CustomButtonColors.LightForeGroundNone = clBlack
    CustomButtonColors.LightForeGroundHover = clBlack
    CustomButtonColors.LightForeGroundPress = clBlack
    CustomButtonColors.DarkBackGroundNone = clBlack
    CustomButtonColors.DarkBackGroundHover = clBlack
    CustomButtonColors.DarkBackGroundPress = clBlack
    CustomButtonColors.DarkForeGroundNone = clBlack
    CustomButtonColors.DarkForeGroundHover = clBlack
    CustomButtonColors.DarkForeGroundPress = clBlack
    Text = 'Login'
    LineWidth = 3.000000000000000000
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 21
    Font.Name = 'Segoe UI'
    Font.Style = []
    TabOrder = 2
    Image.Enabled = True
    Image.SelectSegoe = #59182
    Image.SelectImageIndex = 0
    ImageScale = 1.250000000000000000
    LayoutHorizontal = Beginning
    Margin = 10
    StateText = 'Login success'
    StateImage.Enabled = True
    StateImage.SelectSegoe = #59269
    StateImage.SelectImageIndex = 0
    AutoStateToggle = True
  end
  object FXButtonDesign3: FXButtonDesign
    Left = 1038
    Top = 192
    Width = 140
    Height = 40
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Size.X = 140
    Size.Y = 40
    Position.X = 1038
    Position.Y = 192
    Allignment = taCenter
    Default = False
    Cancel = False
    TabOrder = 3
    ParentColor = False
    UseManualColor = True
    Image.Enabled = True
    Image.SelectSegoe = #59188
    Image.SelectImageIndex = 0
    ImageLayout = cpLeft
    ActionText = 'Working'
    ActionImage.Enabled = True
    ActionImage.SelectSegoe = #59180
    ActionImage.SelectImageIndex = 0
    ActionToggle = False
    ShowCaption = True
    CustomColors.Enabled = False
    CustomColors.Accent = 13924352
    CustomColors.LightBackGround = 15987699
    CustomColors.LightForeGround = 1776410
    CustomColors.DarkBackGround = 2105376
    CustomColors.DarkForeGround = clWhite
    GradientOptions.Enabled = True
    GradientOptions.Enter = clTeal
    GradientOptions.Leave = clFuchsia
    GradientOptions.Down = clBlue
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 14123546
    Font.Height = -16
    Font.Name = 'Segoe UI Semibold'
    Font.Style = []
    FontAutoSize.Enabled = False
    FontAutoSize.Max = -1
    FontAutoSize.Min = -1
    SubTextFont.Charset = DEFAULT_CHARSET
    SubTextFont.Color = 14123546
    SubTextFont.Height = -13
    SubTextFont.Name = 'Segoe UI'
    SubTextFont.Style = []
    Text = 'Gradient'
    SubText = 'Hello World!'
    RoundTransparent = True
    RoundAmount = 10
    State = mbsLeave
    FlatButton = False
    FlatComplete = False
    Colors.Enter = 16711808
    Colors.Leave = 2383600
    Colors.Down = clLime
    Colors.BLine = 14024
    UnderLine.Enable = True
    UnderLine.UnderLineRound = True
    UnderLine.UnderLineThicknes = 6
    TextColors.Enter = clWhite
    TextColors.Leave = clWhite
    TextColors.Down = clWhite
    TextColors.BLine = clBlack
    Pen.Color = clWindow
    Pen.Width = 0
    Pen.AltHoverColor = clBlack
    Pen.AltPressColor = clBlack
    Animations.PressAnimation = False
    Animations.PADelay = 2
    Animations.PAShrinkAmount = 6
    Animations.PAAnimateEngine = cbneAtDraw
    Animations.FadeAnimation = True
    Animations.FASpeed = 10
  end
  object FXButtonDesign1: FXButtonDesign
    Left = 886
    Top = 192
    Width = 140
    Height = 40
    Hint = 'Hello World!'
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Size.X = 140
    Size.Y = 40
    Position.X = 886
    Position.Y = 192
    Allignment = taCenter
    Default = False
    Cancel = False
    ShowHint = True
    TabOrder = 4
    ParentColor = False
    UseManualColor = False
    Image.IconType = None
    Image.SelectSegoe = #59188
    Image.SelectImageIndex = 0
    ImageLayout = cpLeft
    ActionText = 'Working'
    ActionImage.IconType = None
    ActionImage.SelectSegoe = #59188
    ActionImage.SelectImageIndex = 0
    ActionToggle = False
    ShowCaption = True
    CustomColors.Enabled = False
    CustomColors.Accent = 13924352
    CustomColors.LightBackGround = 15987699
    CustomColors.LightForeGround = 1776410
    CustomColors.DarkBackGround = 2105376
    CustomColors.DarkForeGround = clWhite
    GradientOptions.Enabled = False
    GradientOptions.Enter = clFuchsia
    GradientOptions.Leave = clRed
    GradientOptions.Down = clMaroon
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 14123546
    Font.Height = -16
    Font.Name = 'Segoe UI Semibold'
    Font.Style = []
    FontAutoSize.Enabled = False
    FontAutoSize.Max = -1
    FontAutoSize.Min = -1
    SubTextFont.Charset = DEFAULT_CHARSET
    SubTextFont.Color = 14123546
    SubTextFont.Height = -13
    SubTextFont.Name = 'Segoe UI'
    SubTextFont.Style = []
    Text = 'Advanced Button'
    SubText = 'Hello World!'
    RoundTransparent = True
    RoundAmount = 10
    State = mbsLeave
    FlatButton = False
    FlatComplete = False
    Colors.Enter = 16414947
    Colors.Leave = 15428052
    Colors.Down = 13783227
    Colors.BLine = 12796332
    UnderLine.Enable = True
    UnderLine.UnderLineRound = True
    UnderLine.UnderLineThicknes = 6
    TextColors.Enter = clWhite
    TextColors.Leave = clWhite
    TextColors.Down = clWhite
    TextColors.BLine = clBlack
    Pen.Color = clWindow
    Pen.Width = 0
    Pen.AltHoverColor = clBlack
    Pen.AltPressColor = clBlack
    Animations.PressAnimation = True
    Animations.PADelay = 2
    Animations.PAShrinkAmount = 6
    Animations.PAAnimateEngine = cbneAtDraw
    Animations.FadeAnimation = True
    Animations.FASpeed = 10
  end
  object FXButton3: FXButton
    Left = 885
    Top = 53
    Width = 147
    Height = 35
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Size.X = 147
    Size.Y = 35
    Position.X = 885
    Position.Y = 53
    CustomColors.Enabled = False
    CustomColors.Accent = 13924352
    CustomColors.LightBackGround = 15987699
    CustomColors.LightForeGround = 1776410
    CustomColors.DarkBackGround = 2105376
    CustomColors.DarkForeGround = clWhite
    CustomColors.LightBackGroundInterior = 16514043
    CustomColors.DarkBackGroundInterior = 2829099
    CustomButtonColors.Enabled = True
    CustomButtonColors.Accent = clWhite
    CustomButtonColors.LightBackgroundNone = clPurple
    CustomButtonColors.LightBackgroundHover = clRed
    CustomButtonColors.LightBackgroundPress = clYellow
    CustomButtonColors.LightForeGroundNone = clAqua
    CustomButtonColors.LightForeGroundHover = clLime
    CustomButtonColors.LightForeGroundPress = clRed
    CustomButtonColors.DarkBackGroundNone = clPurple
    CustomButtonColors.DarkBackGroundHover = clRed
    CustomButtonColors.DarkBackGroundPress = clYellow
    CustomButtonColors.DarkForeGroundNone = clAqua
    CustomButtonColors.DarkForeGroundHover = clLime
    CustomButtonColors.DarkForeGroundPress = clRed
    Text = 'Rainbow Button'
    LineWidth = 3.000000000000000000
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 21
    Font.Name = 'Segoe UI'
    Font.Style = []
    TabOrder = 5
    Image.IconType = None
    Image.SelectSegoe = #59188
    Image.SelectImageIndex = 0
    ImageScale = 1.250000000000000000
    StateText = 'Wooo!'
    StateImage.IconType = None
    StateImage.SelectSegoe = #59188
    StateImage.SelectImageIndex = 0
    AutoStateToggle = True
  end
  object FXButton7: FXButton
    Left = 1038
    Top = 53
    Width = 140
    Height = 35
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Size.X = 140
    Size.Y = 35
    Position.X = 1038
    Position.Y = 53
    CustomColors.Enabled = False
    CustomColors.Accent = 13924352
    CustomColors.LightBackGround = 15987699
    CustomColors.LightForeGround = 1776410
    CustomColors.DarkBackGround = 2105376
    CustomColors.DarkForeGround = clWhite
    CustomColors.LightBackGroundInterior = 16514043
    CustomColors.DarkBackGroundInterior = 2829099
    CustomButtonColors.Enabled = False
    CustomButtonColors.Accent = clBlack
    CustomButtonColors.LightBackgroundNone = clBlack
    CustomButtonColors.LightBackgroundHover = clBlack
    CustomButtonColors.LightBackgroundPress = clBlack
    CustomButtonColors.LightForeGroundNone = clBlack
    CustomButtonColors.LightForeGroundHover = clBlack
    CustomButtonColors.LightForeGroundPress = clBlack
    CustomButtonColors.DarkBackGroundNone = clBlack
    CustomButtonColors.DarkBackGroundHover = clBlack
    CustomButtonColors.DarkBackGroundPress = clBlack
    CustomButtonColors.DarkForeGroundNone = clBlack
    CustomButtonColors.DarkForeGroundHover = clBlack
    CustomButtonColors.DarkForeGroundPress = clBlack
    Text = 'Toggle me'
    LineWidth = 3.000000000000000000
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 21
    Font.Name = 'Segoe UI'
    Font.Style = []
    TabOrder = 6
    Image.IconType = None
    Image.SelectSegoe = #59188
    Image.SelectImageIndex = 0
    ImageScale = 1.250000000000000000
    ButtonKind = Toggle
    StateText = 'You toggled me'
    StateImage.IconType = None
    StateImage.SelectSegoe = #59188
    StateImage.SelectImageIndex = 0
    StateDuration = 0
    AutoStateToggle = True
  end
  object FXButton8: FXButton
    Left = 886
    Top = 94
    Width = 139
    Height = 35
    Cursor = crHandPoint
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Size.X = 139
    Size.Y = 35
    Position.X = 886
    Position.Y = 94
    CustomColors.Enabled = False
    CustomColors.Accent = 13924352
    CustomColors.LightBackGround = 15987699
    CustomColors.LightForeGround = 1776410
    CustomColors.DarkBackGround = 2105376
    CustomColors.DarkForeGround = clWhite
    CustomColors.LightBackGroundInterior = 16514043
    CustomColors.DarkBackGroundInterior = 2829099
    CustomButtonColors.Enabled = False
    CustomButtonColors.Accent = clBlack
    CustomButtonColors.LightBackgroundNone = clBlack
    CustomButtonColors.LightBackgroundHover = clBlack
    CustomButtonColors.LightBackgroundPress = clBlack
    CustomButtonColors.LightForeGroundNone = clBlack
    CustomButtonColors.LightForeGroundHover = clBlack
    CustomButtonColors.LightForeGroundPress = clBlack
    CustomButtonColors.DarkBackGroundNone = clBlack
    CustomButtonColors.DarkBackGroundHover = clBlack
    CustomButtonColors.DarkBackGroundPress = clBlack
    CustomButtonColors.DarkForeGroundNone = clBlack
    CustomButtonColors.DarkForeGroundHover = clBlack
    CustomButtonColors.DarkForeGroundPress = clBlack
    Text = 'Online topics'
    LineWidth = 3.000000000000000000
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 21
    Font.Name = 'Segoe UI'
    Font.Style = []
    TabOrder = 7
    Image.IconType = None
    Image.SelectSegoe = #59188
    Image.SelectImageIndex = 0
    ImageScale = 1.250000000000000000
    ButtonKind = Link
    HyperLinkURL = 'https://docs.codrutsoft.com'
    StateText = 'Success'
    StateImage.IconType = None
    StateImage.SelectSegoe = #59188
    StateImage.SelectImageIndex = 0
  end
  object FXButtonDesign2: FXButtonDesign
    Left = 1038
    Top = 239
    Width = 140
    Height = 75
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Size.X = 140
    Size.Y = 75
    Position.X = 1038
    Position.Y = 239
    Allignment = taCenter
    Default = False
    Cancel = False
    TabOrder = 8
    Color = clWhite
    ParentColor = False
    UseManualColor = True
    Image.Enabled = True
    Image.SelectSegoe = #63226
    Image.SelectImageIndex = 0
    ImageLayout = cpLeft
    ActionText = 'Working'
    ActionImage.Enabled = True
    ActionImage.SelectSegoe = #59180
    ActionImage.SelectImageIndex = 0
    ActionToggle = False
    ShowCaption = True
    CustomColors.Enabled = False
    CustomColors.Accent = 13924352
    CustomColors.LightBackGround = 15987699
    CustomColors.LightForeGround = 1776410
    CustomColors.DarkBackGround = 2105376
    CustomColors.DarkForeGround = clWhite
    GradientOptions.Enabled = False
    GradientOptions.Enter = clTeal
    GradientOptions.Leave = clFuchsia
    GradientOptions.Down = clBlue
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 14123546
    Font.Height = -16
    Font.Name = 'Segoe UI Semibold'
    Font.Style = []
    FontAutoSize.Enabled = False
    FontAutoSize.Max = -1
    FontAutoSize.Min = -1
    SubTextFont.Charset = DEFAULT_CHARSET
    SubTextFont.Color = 14123546
    SubTextFont.Height = -13
    SubTextFont.Name = 'Segoe UI'
    SubTextFont.Style = []
    Text = 'Network'
    SubText = 'Hello World!'
    RoundTransparent = True
    RoundAmount = 10
    State = mbsLeave
    FlatButton = False
    FlatComplete = False
    Colors.Enter = 16744448
    Colors.Leave = 12615680
    Colors.Down = clWhite
    Colors.BLine = 14024
    UnderLine.Enable = False
    UnderLine.UnderLineRound = True
    UnderLine.UnderLineThicknes = 6
    TextColors.Enter = clWhite
    TextColors.Leave = clWhite
    TextColors.Down = clBlack
    TextColors.BLine = clBlack
    Pen.Color = clWhite
    Pen.Width = 3
    Pen.ManualColor = True
    Pen.EnableAlternativeColors = True
    Pen.AltHoverColor = clAqua
    Pen.AltPressColor = 16711808
    Animations.PressAnimation = False
    Animations.PADelay = 2
    Animations.PAShrinkAmount = 6
    Animations.PAAnimateEngine = cbneAtDraw
    Animations.FadeAnimation = True
    Animations.FASpeed = 10
  end
  object FXButtonDesign4: FXButtonDesign
    Left = 885
    Top = 238
    Width = 147
    Height = 75
    Hint = 'Hello World!'
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Size.X = 147
    Size.Y = 75
    Position.X = 885
    Position.Y = 238
    OnClick = FXButtonDesign4Click
    Allignment = taLeftJustify
    Default = False
    Cancel = False
    ShowHint = True
    TabOrder = 9
    ParentColor = False
    UseManualColor = False
    Image.IconType = None
    Image.SelectSegoe = #59188
    Image.SelectImageIndex = 0
    ImageLayout = cpLeft
    ActionText = 'Working'
    ActionImage.IconType = None
    ActionImage.SelectSegoe = #59188
    ActionImage.SelectImageIndex = 0
    ActionToggle = False
    ShowCaption = True
    CustomColors.Enabled = False
    CustomColors.Accent = 13924352
    CustomColors.LightBackGround = 15987699
    CustomColors.LightForeGround = 1776410
    CustomColors.DarkBackGround = 2105376
    CustomColors.DarkForeGround = clWhite
    GradientOptions.Enabled = False
    GradientOptions.Enter = clFuchsia
    GradientOptions.Leave = clRed
    GradientOptions.Down = clMaroon
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 14123546
    Font.Height = -16
    Font.Name = 'Segoe UI Semibold'
    Font.Style = []
    FontAutoSize.Enabled = False
    FontAutoSize.Max = -1
    FontAutoSize.Min = -1
    SubTextFont.Charset = DEFAULT_CHARSET
    SubTextFont.Color = clWhite
    SubTextFont.Height = -13
    SubTextFont.Name = 'Segoe UI'
    SubTextFont.Style = []
    Text = 'Search'
    SubText = 'Search topics online'
    SubTextEnabled = True
    RoundTransparent = True
    RoundAmount = 10
    State = mbsLeave
    FlatButton = False
    FlatComplete = False
    Colors.Enter = 16414947
    Colors.Leave = 15428052
    Colors.Down = 13783227
    Colors.BLine = 12796332
    UnderLine.Enable = True
    UnderLine.UnderLineRound = True
    UnderLine.UnderLineThicknes = 6
    TextColors.Enter = clWhite
    TextColors.Leave = clWhite
    TextColors.Down = clWhite
    TextColors.BLine = clBlack
    Pen.Color = clWindow
    Pen.Width = 0
    Pen.AltHoverColor = clBlack
    Pen.AltPressColor = clBlack
    Animations.PressAnimation = False
    Animations.PADelay = 2
    Animations.PAShrinkAmount = 6
    Animations.PAAnimateEngine = cbneAtDraw
    Animations.FadeAnimation = True
    Animations.FASpeed = 10
  end
  object FXButton11: FXButton
    Left = 1038
    Top = 135
    Width = 140
    Height = 35
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Size.X = 140
    Size.Y = 35
    Position.X = 1038
    Position.Y = 135
    CustomColors.Enabled = False
    CustomColors.Accent = 13924352
    CustomColors.LightBackGround = 15987699
    CustomColors.LightForeGround = 1776410
    CustomColors.DarkBackGround = 2105376
    CustomColors.DarkForeGround = clWhite
    CustomColors.LightBackGroundInterior = 16514043
    CustomColors.DarkBackGroundInterior = 2829099
    CustomButtonColors.Enabled = False
    CustomButtonColors.Accent = clBlack
    CustomButtonColors.LightBackgroundNone = clBlack
    CustomButtonColors.LightBackgroundHover = clBlack
    CustomButtonColors.LightBackgroundPress = clBlack
    CustomButtonColors.LightForeGroundNone = clBlack
    CustomButtonColors.LightForeGroundHover = clBlack
    CustomButtonColors.LightForeGroundPress = clBlack
    CustomButtonColors.DarkBackGroundNone = clBlack
    CustomButtonColors.DarkBackGroundHover = clBlack
    CustomButtonColors.DarkBackGroundPress = clBlack
    CustomButtonColors.DarkForeGroundNone = clBlack
    CustomButtonColors.DarkForeGroundHover = clBlack
    CustomButtonColors.DarkForeGroundPress = clBlack
    Text = 'Round'
    Roundness = 35
    LineWidth = 3.000000000000000000
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 21
    Font.Name = 'Segoe UI'
    Font.Style = []
    TabOrder = 10
    Image.IconType = None
    Image.SelectSegoe = #59188
    Image.SelectImageIndex = 0
    ImageScale = 1.250000000000000000
    ButtonKind = Accent
    StateText = 'Success'
    StateImage.IconType = None
    StateImage.SelectSegoe = #59188
    StateImage.SelectImageIndex = 0
  end
  object FXButton12: FXButton
    Left = 885
    Top = 135
    Width = 140
    Height = 35
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Size.X = 140
    Size.Y = 35
    Position.X = 885
    Position.Y = 135
    CustomColors.Enabled = False
    CustomColors.Accent = 13924352
    CustomColors.LightBackGround = 15987699
    CustomColors.LightForeGround = 1776410
    CustomColors.DarkBackGround = 2105376
    CustomColors.DarkForeGround = clWhite
    CustomColors.LightBackGroundInterior = 16514043
    CustomColors.DarkBackGroundInterior = 2829099
    CustomButtonColors.Enabled = False
    CustomButtonColors.Accent = clBlack
    CustomButtonColors.LightBackgroundNone = clBlack
    CustomButtonColors.LightBackgroundHover = clBlack
    CustomButtonColors.LightBackgroundPress = clBlack
    CustomButtonColors.LightForeGroundNone = clBlack
    CustomButtonColors.LightForeGroundHover = clBlack
    CustomButtonColors.LightForeGroundPress = clBlack
    CustomButtonColors.DarkBackGroundNone = clBlack
    CustomButtonColors.DarkBackGroundHover = clBlack
    CustomButtonColors.DarkBackGroundPress = clBlack
    CustomButtonColors.DarkForeGroundNone = clBlack
    CustomButtonColors.DarkForeGroundHover = clBlack
    CustomButtonColors.DarkForeGroundPress = clBlack
    Text = 'Press & hold'
    LineWidth = 3.000000000000000000
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 21
    Font.Name = 'Segoe UI'
    Font.Style = []
    TabOrder = 11
    OnClick = FXButton12Click
    Image.IconType = None
    Image.SelectSegoe = #59188
    Image.SelectImageIndex = 0
    ImageScale = 1.250000000000000000
    RepeatWhenPressed = True
    StateText = 'Success'
    StateImage.IconType = None
    StateImage.SelectSegoe = #59188
    StateImage.SelectImageIndex = 0
    AutoStateToggle = True
  end
  object FXScrollText2: FXScrollText
    Left = 15
    Top = 598
    Width = 186
    Height = 50
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Size.X = 186
    Size.Y = 50
    Position.X = 15
    Position.Y = 598
    CustomColors.Enabled = False
    CustomColors.Accent = 13924352
    CustomColors.LightBackGround = 15987699
    CustomColors.LightForeGround = 1776410
    CustomColors.DarkBackGround = 2105376
    CustomColors.DarkForeGround = clWhite
    Text = 'Codrut Fluent Design System'
    SpacePercent = 75.000000000000000000
    Image.Enabled = True
    Image.SelectSegoe = #59718
    Image.SelectImageIndex = 0
    ImageScale = 1.000000000000000000
    AnimationDelay = 200
    AnimationSpeed = 1
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 21
    Font.Name = 'Segoe UI'
    Font.Style = []
    Anchors = [akLeft, akBottom]
    TabOrder = 12
  end
  object FXButton4: FXButton
    Left = 235
    Top = 80
    Width = 130
    Height = 35
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    PopupMenu = FXPopupMenu1
    Size.X = 130
    Size.Y = 35
    Position.X = 235
    Position.Y = 80
    CustomColors.Enabled = False
    CustomColors.Accent = 13924352
    CustomColors.LightBackGround = 15987699
    CustomColors.LightForeGround = 1776410
    CustomColors.DarkBackGround = 2105376
    CustomColors.DarkForeGround = clWhite
    CustomColors.LightBackGroundInterior = 16514043
    CustomColors.DarkBackGroundInterior = 2829099
    CustomButtonColors.Enabled = False
    CustomButtonColors.Accent = clBlack
    CustomButtonColors.LightBackgroundNone = clBlack
    CustomButtonColors.LightBackgroundHover = clBlack
    CustomButtonColors.LightBackgroundPress = clBlack
    CustomButtonColors.LightForeGroundNone = clBlack
    CustomButtonColors.LightForeGroundHover = clBlack
    CustomButtonColors.LightForeGroundPress = clBlack
    CustomButtonColors.DarkBackGroundNone = clBlack
    CustomButtonColors.DarkBackGroundHover = clBlack
    CustomButtonColors.DarkBackGroundPress = clBlack
    CustomButtonColors.DarkForeGroundNone = clBlack
    CustomButtonColors.DarkForeGroundHover = clBlack
    CustomButtonColors.DarkForeGroundPress = clBlack
    Text = 'Open dialog'
    LineWidth = 3.000000000000000000
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 21
    Font.Name = 'Segoe UI'
    Font.Style = []
    TabOrder = 15
    OnClick = FXButton4Click
    Image.IconType = None
    Image.SelectSegoe = #59188
    Image.SelectImageIndex = 0
    ImageScale = 1.250000000000000000
    ButtonKind = Accent
    StateText = 'Success'
    StateImage.IconType = None
    StateImage.SelectSegoe = #59188
    StateImage.SelectImageIndex = 0
  end
  object FXButton5: FXButton
    Left = 368
    Top = 80
    Width = 130
    Height = 35
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Size.X = 130
    Size.Y = 35
    Position.X = 368
    Position.Y = 80
    CustomColors.Enabled = False
    CustomColors.Accent = 13924352
    CustomColors.LightBackGround = 15987699
    CustomColors.LightForeGround = 1776410
    CustomColors.DarkBackGround = 2105376
    CustomColors.DarkForeGround = clWhite
    CustomColors.LightBackGroundInterior = 16514043
    CustomColors.DarkBackGroundInterior = 2829099
    CustomButtonColors.Enabled = False
    CustomButtonColors.Accent = clBlack
    CustomButtonColors.LightBackgroundNone = clBlack
    CustomButtonColors.LightBackgroundHover = clBlack
    CustomButtonColors.LightBackgroundPress = clBlack
    CustomButtonColors.LightForeGroundNone = clBlack
    CustomButtonColors.LightForeGroundHover = clBlack
    CustomButtonColors.LightForeGroundPress = clBlack
    CustomButtonColors.DarkBackGroundNone = clBlack
    CustomButtonColors.DarkBackGroundHover = clBlack
    CustomButtonColors.DarkBackGroundPress = clBlack
    CustomButtonColors.DarkForeGroundNone = clBlack
    CustomButtonColors.DarkForeGroundHover = clBlack
    CustomButtonColors.DarkForeGroundPress = clBlack
    Text = 'Change theme'
    LineWidth = 3.000000000000000000
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 21
    Font.Name = 'Segoe UI'
    Font.Style = []
    TabOrder = 16
    OnClick = FXButton5Click
    Image.IconType = None
    Image.SelectSegoe = #59188
    Image.SelectImageIndex = 0
    ImageScale = 1.250000000000000000
    StateText = 'Success'
    StateImage.IconType = None
    StateImage.SelectSegoe = #59188
    StateImage.SelectImageIndex = 0
  end
  object FXTextBox2: FXTextBox
    Left = 237
    Top = 436
    Width = 67
    Height = 21
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    TabStop = True
    Size.X = 67
    Size.Y = 21
    Position.X = 237
    Position.Y = 436
    CustomColors.Enabled = False
    CustomColors.Accent = 13924352
    CustomColors.LightBackGround = 15987699
    CustomColors.LightForeGround = 1776410
    CustomColors.DarkBackGround = 2105376
    CustomColors.DarkForeGround = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = []
    TabOrder = 17
    Text = 'Password'
    AutoSize = True
    Elipsis = False
    InnerMargin = 0
  end
  object FXTextBox3: FXTextBox
    Left = 237
    Top = 365
    Width = 71
    Height = 21
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    TabStop = True
    Size.X = 71
    Size.Y = 21
    Position.X = 237
    Position.Y = 365
    CustomColors.Enabled = False
    CustomColors.Accent = 13924352
    CustomColors.LightBackGround = 15987699
    CustomColors.LightForeGround = 1776410
    CustomColors.DarkBackGround = 2105376
    CustomColors.DarkForeGround = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = []
    TabOrder = 18
    Text = 'Username'
    AutoSize = True
    Elipsis = False
    InnerMargin = 0
  end
  object FXTextBox4: FXTextBox
    Left = 237
    Top = 44
    Width = 52
    Height = 42
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    TabStop = True
    Size.X = 52
    Size.Y = 42
    Position.X = 237
    Position.Y = 44
    CustomColors.Enabled = False
    CustomColors.Accent = 13924352
    CustomColors.LightBackGround = 15987699
    CustomColors.LightForeGround = 1776410
    CustomColors.DarkBackGround = 2105376
    CustomColors.DarkForeGround = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = []
    TabOrder = 19
    Text = 'Dialogs'
    AutoSize = False
    Elipsis = False
    InnerMargin = 0
  end
  object FXTextBox5: FXTextBox
    Left = 64
    Top = 30
    Width = 101
    Height = 37
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    TabStop = True
    Size.X = 101
    Size.Y = 37
    Position.X = 64
    Position.Y = 30
    CustomColors.Enabled = False
    CustomColors.Accent = 13924352
    CustomColors.LightBackGround = 15987699
    CustomColors.LightForeGround = 1776410
    CustomColors.DarkBackGround = 2105376
    CustomColors.DarkForeGround = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -27
    Font.Name = 'Segoe UI'
    Font.Style = []
    TabOrder = 20
    Text = 'Test'
    AutoSize = False
    Elipsis = False
    InnerMargin = 0
  end
  object FXButton14: FXButton
    Left = 886
    Top = 319
    Width = 146
    Height = 35
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Size.X = 146
    Size.Y = 35
    Position.X = 886
    Position.Y = 319
    CustomColors.Enabled = False
    CustomColors.Accent = 13924352
    CustomColors.LightBackGround = 15987699
    CustomColors.LightForeGround = 1776410
    CustomColors.DarkBackGround = 2105376
    CustomColors.DarkForeGround = clWhite
    CustomColors.LightBackGroundInterior = 16514043
    CustomColors.DarkBackGroundInterior = 2829099
    CustomButtonColors.Enabled = False
    CustomButtonColors.Accent = clBlack
    CustomButtonColors.LightBackgroundNone = clBlack
    CustomButtonColors.LightBackgroundHover = clBlack
    CustomButtonColors.LightBackgroundPress = clBlack
    CustomButtonColors.LightForeGroundNone = clBlack
    CustomButtonColors.LightForeGroundHover = clBlack
    CustomButtonColors.LightForeGroundPress = clBlack
    CustomButtonColors.DarkBackGroundNone = clBlack
    CustomButtonColors.DarkBackGroundHover = clBlack
    CustomButtonColors.DarkBackGroundPress = clBlack
    CustomButtonColors.DarkForeGroundNone = clBlack
    CustomButtonColors.DarkForeGroundHover = clBlack
    CustomButtonColors.DarkForeGroundPress = clBlack
    Text = 'Update'
    LineWidth = 3.000000000000000000
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = []
    TabOrder = 21
    OnClick = FXButton14Click
    Image.Enabled = True
    Image.SelectSegoe = #60371
    Image.SelectImageIndex = 0
    ImageScale = 1.250000000000000000
    StateText = 'Success'
    StateImage.IconType = None
    StateImage.SelectSegoe = #59188
    StateImage.SelectImageIndex = 0
  end
  object FXButton16: FXButton
    Left = 1038
    Top = 320
    Width = 140
    Height = 35
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Size.X = 140
    Size.Y = 35
    Position.X = 1038
    Position.Y = 320
    CustomColors.Enabled = False
    CustomColors.Accent = 13924352
    CustomColors.LightBackGround = 15987699
    CustomColors.LightForeGround = 1776410
    CustomColors.DarkBackGround = 2105376
    CustomColors.DarkForeGround = clWhite
    CustomColors.LightBackGroundInterior = 16514043
    CustomColors.DarkBackGroundInterior = 2829099
    CustomButtonColors.Enabled = False
    CustomButtonColors.Accent = clBlack
    CustomButtonColors.LightBackgroundNone = clBlack
    CustomButtonColors.LightBackgroundHover = clBlack
    CustomButtonColors.LightBackgroundPress = clBlack
    CustomButtonColors.LightForeGroundNone = clBlack
    CustomButtonColors.LightForeGroundHover = clBlack
    CustomButtonColors.LightForeGroundPress = clBlack
    CustomButtonColors.DarkBackGroundNone = clBlack
    CustomButtonColors.DarkBackGroundHover = clBlack
    CustomButtonColors.DarkBackGroundPress = clBlack
    CustomButtonColors.DarkForeGroundNone = clBlack
    CustomButtonColors.DarkForeGroundHover = clBlack
    CustomButtonColors.DarkForeGroundPress = clBlack
    Text = 'Full-Dialog'
    LineWidth = 3.000000000000000000
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = []
    TabOrder = 22
    OnClick = FXButton16Click
    Image.Enabled = True
    Image.SelectSegoe = #60412
    Image.SelectImageIndex = 0
    ImageScale = 1.250000000000000000
    StateText = 'Success'
    StateImage.IconType = None
    StateImage.SelectSegoe = #59188
    StateImage.SelectImageIndex = 0
  end
  object FXTextBox8: FXTextBox
    Left = 1038
    Top = 365
    Width = 138
    Height = 70
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    TabStop = True
    Size.X = 138
    Size.Y = 70
    Position.X = 1038
    Position.Y = 365
    CustomColors.Enabled = False
    CustomColors.Accent = 13924352
    CustomColors.LightBackGround = 15987699
    CustomColors.LightForeGround = 1776410
    CustomColors.DarkBackGround = 2105376
    CustomColors.DarkForeGround = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = []
    TabOrder = 23
    Text = 'Wow cool very col indeed wow 23'
    AutoSize = False
    WordWrap = True
    Elipsis = False
    InnerMargin = 0
  end
  object FXIconView1: FXIconView
    Left = 49
    Top = 110
    Width = 116
    Height = 60
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    TabStop = True
    Size.X = 116
    Size.Y = 60
    Position.X = 49
    Position.Y = 110
    CustomColors.Enabled = False
    CustomColors.Accent = 13924352
    CustomColors.LightBackGround = 15987699
    CustomColors.LightForeGround = 1776410
    CustomColors.DarkBackGround = 2105376
    CustomColors.DarkForeGround = clWhite
    Image.Enabled = True
    Image.SelectSegoe = #59188
    Image.SelectImageIndex = 0
    Scale = 1.000000000000000000
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 22
    Font.Name = 'Segoe UI'
    Font.Style = []
    TabOrder = 24
  end
  object FXSelector1: FXSelector
    Left = 231
    Top = 253
    Width = 232
    Height = 30
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Size.X = 232
    Size.Y = 30
    Position.X = 231
    Position.Y = 253
    CustomColors.Enabled = False
    CustomColors.Accent = 13924352
    CustomColors.LightBackGround = 15987699
    CustomColors.LightForeGround = 1776410
    CustomColors.DarkBackGround = 2105376
    CustomColors.DarkForeGround = clWhite
    CustomColors.LightBackGroundInterior = 16514043
    CustomColors.DarkBackGroundInterior = 2829099
    SelectedItem = -1
    Items.Strings = (
      'Item1'
      'Item2'
      'Item3')
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Segoe UI'
    Font.Style = []
    TabOrder = 14
  end
  object FXTabStrip1: FXTabStrip
    Left = 725
    Top = 604
    Width = 300
    Height = 40
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Size.X = 300
    Size.Y = 40
    Position.X = 725
    Position.Y = 604
    CustomColors.Enabled = False
    CustomColors.Accent = 13924352
    CustomColors.LightBackGround = 15987699
    CustomColors.LightForeGround = 1776410
    CustomColors.DarkBackGround = 2105376
    CustomColors.DarkForeGround = clWhite
    TabOrder = 25
  end
  object FXTitleBarPanel1: FXTitleBarPanel
    Left = 0
    Top = 0
    Width = 1184
    Height = 30
    CustomColors.Enabled = False
    CustomColors.Accent = 13924352
    CustomColors.LightBackGround = 15987699
    CustomColors.LightForeGround = 1776410
    CustomColors.DarkBackGround = 2105376
    CustomColors.DarkForeGround = clWhite
    CustomButtons = <>
    object FXBlurMaterial2: FXBlurMaterial
      Left = 0
      Top = 0
      Width = 215
      Height = 30
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 0
      Margins.Bottom = 0
      TabStop = True
      Size.X = 215
      Size.Y = 30
      Position.X = 0
      Position.Y = 0
      Align = alLeft
      CustomColors.Enabled = False
      CustomColors.Accent = 5789570
      CustomColors.LightBackGround = clWhite
      CustomColors.LightForeGround = 1776410
      CustomColors.DarkBackGround = clBlack
      CustomColors.DarkForeGround = clWhite
    end
  end
  object FXButton9: FXButton
    Left = 1038
    Top = 94
    Width = 140
    Height = 35
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Size.X = 140
    Size.Y = 35
    Position.X = 1038
    Position.Y = 94
    CustomColors.Enabled = False
    CustomColors.Accent = 13924352
    CustomColors.LightBackGround = 15987699
    CustomColors.LightForeGround = 1776410
    CustomColors.DarkBackGround = 2105376
    CustomColors.DarkForeGround = clWhite
    CustomColors.LightBackGroundInterior = 16514043
    CustomColors.DarkBackGroundInterior = 2829099
    CustomButtonColors.Enabled = False
    CustomButtonColors.Accent = clBlack
    CustomButtonColors.LightBackgroundNone = clBlack
    CustomButtonColors.LightBackgroundHover = clBlack
    CustomButtonColors.LightBackgroundPress = clBlack
    CustomButtonColors.LightForeGroundNone = clBlack
    CustomButtonColors.LightForeGroundHover = clBlack
    CustomButtonColors.LightForeGroundPress = clBlack
    CustomButtonColors.DarkBackGroundNone = clBlack
    CustomButtonColors.DarkBackGroundHover = clBlack
    CustomButtonColors.DarkBackGroundPress = clBlack
    CustomButtonColors.DarkForeGroundNone = clBlack
    CustomButtonColors.DarkForeGroundHover = clBlack
    CustomButtonColors.DarkForeGroundPress = clBlack
    Text = 'Options'
    LineWidth = 3.000000000000000000
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 21
    Font.Name = 'Segoe UI'
    Font.Style = []
    TabOrder = 27
    Image.IconType = None
    Image.SelectSegoe = #59188
    Image.SelectImageIndex = 0
    ImageScale = 1.250000000000000000
    ButtonKind = Dropdown
    DropDown = FXPopupMenu1
    StateText = 'Success'
    StateImage.IconType = None
    StateImage.SelectSegoe = #59188
    StateImage.SelectImageIndex = 0
  end
  object FXScrollText1: FXScrollText
    Left = 237
    Top = 576
    Width = 125
    Height = 68
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Size.X = 125
    Size.Y = 68
    Position.X = 237
    Position.Y = 576
    CustomColors.Enabled = False
    CustomColors.Accent = 13924352
    CustomColors.LightBackGround = 15987699
    CustomColors.LightForeGround = 1776410
    CustomColors.DarkBackGround = 2105376
    CustomColors.DarkForeGround = clWhite
    Text = 
      'This text is way too long to fit here, and now has to scroll jus' +
      't for It to be visible'
    SpacePercent = 100.000000000000000000
    Image.Enabled = True
    Image.SelectSegoe = #59543
    Image.SelectImageIndex = 0
    ImageScale = 1.000000000000000000
    AnimationDelay = 200
    AnimationSpeed = 1
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 21
    Font.Name = 'Segoe UI'
    Font.Style = []
    TabOrder = 28
  end
  object FXScrollbar1: FXScrollbar
    Left = 230
    Top = 223
    Width = 233
    Height = 13
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Size.X = 233
    Size.Y = 13
    Position = 0
    CustomColors.Enabled = False
    CustomColors.Accent = 13924352
    CustomColors.LightBackGround = 15987699
    CustomColors.LightForeGround = 1776410
    CustomColors.DarkBackGround = 2105376
    CustomColors.DarkForeGround = clWhite
    CustomColors.LightBackGroundInterior = 16514043
    CustomColors.DarkBackGroundInterior = 2829099
    Orientation = Horizontal
    Animation = True
    CustomScrollbarSize = 0
    TabOrder = 29
  end
  object FXButton2: FXButton
    Left = 512
    Top = 80
    Width = 129
    Height = 35
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Size.X = 129
    Size.Y = 35
    Position.X = 512
    Position.Y = 80
    CustomColors.Enabled = False
    CustomColors.Accent = 13924352
    CustomColors.LightBackGround = 15987699
    CustomColors.LightForeGround = 1776410
    CustomColors.DarkBackGround = 2105376
    CustomColors.DarkForeGround = clWhite
    CustomColors.LightBackGroundInterior = 16514043
    CustomColors.DarkBackGroundInterior = 2829099
    CustomButtonColors.Enabled = False
    CustomButtonColors.Accent = clBlack
    CustomButtonColors.LightBackgroundNone = clBlack
    CustomButtonColors.LightBackgroundHover = clBlack
    CustomButtonColors.LightBackgroundPress = clBlack
    CustomButtonColors.LightForeGroundNone = clBlack
    CustomButtonColors.LightForeGroundHover = clBlack
    CustomButtonColors.LightForeGroundPress = clBlack
    CustomButtonColors.DarkBackGroundNone = clBlack
    CustomButtonColors.DarkBackGroundHover = clBlack
    CustomButtonColors.DarkBackGroundPress = clBlack
    CustomButtonColors.DarkForeGroundNone = clBlack
    CustomButtonColors.DarkForeGroundHover = clBlack
    CustomButtonColors.DarkForeGroundPress = clBlack
    Text = 'Icon dialog'
    LineWidth = 3.000000000000000000
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 22
    Font.Name = 'Segoe UI'
    Font.Style = []
    TabOrder = 30
    OnClick = FXButton2Click
    Image.IconType = None
    Image.SelectSegoe = #59188
    Image.SelectImageIndex = 0
    ImageScale = 1.250000000000000000
    StateImage.IconType = None
    StateImage.SelectSegoe = #59188
    StateImage.SelectImageIndex = 0
  end
  object FXButton15: FXButton
    Left = 733
    Top = 94
    Width = 140
    Height = 35
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Size.X = 140
    Size.Y = 35
    Position.X = 733
    Position.Y = 94
    CustomColors.Enabled = False
    CustomColors.Accent = 13924352
    CustomColors.LightBackGround = 15987699
    CustomColors.LightForeGround = 1776410
    CustomColors.DarkBackGround = 2105376
    CustomColors.DarkForeGround = clWhite
    CustomColors.LightBackGroundInterior = 16514043
    CustomColors.DarkBackGroundInterior = 2829099
    CustomButtonColors.Enabled = False
    CustomButtonColors.Accent = clBlack
    CustomButtonColors.LightBackgroundNone = clBlack
    CustomButtonColors.LightBackgroundHover = clBlack
    CustomButtonColors.LightBackgroundPress = clBlack
    CustomButtonColors.LightForeGroundNone = clBlack
    CustomButtonColors.LightForeGroundHover = clBlack
    CustomButtonColors.LightForeGroundPress = clBlack
    CustomButtonColors.DarkBackGroundNone = clBlack
    CustomButtonColors.DarkBackGroundHover = clBlack
    CustomButtonColors.DarkBackGroundPress = clBlack
    CustomButtonColors.DarkForeGroundNone = clBlack
    CustomButtonColors.DarkForeGroundHover = clBlack
    CustomButtonColors.DarkForeGroundPress = clBlack
    Text = 'Button'
    Detail = Underline
    LineWidth = 3.000000000000000000
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 22
    Font.Name = 'Segoe UI'
    Font.Style = []
    TabOrder = 31
    Image.IconType = None
    Image.SelectSegoe = #59188
    Image.SelectImageIndex = 0
    ImageScale = 1.250000000000000000
    StateImage.IconType = None
    StateImage.SelectSegoe = #59188
    StateImage.SelectImageIndex = 0
  end
  object FXButton17: FXButton
    Left = 733
    Top = 135
    Width = 140
    Height = 35
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Size.X = 140
    Size.Y = 35
    Position.X = 733
    Position.Y = 135
    CustomColors.Enabled = False
    CustomColors.Accent = 13924352
    CustomColors.LightBackGround = 15987699
    CustomColors.LightForeGround = 1776410
    CustomColors.DarkBackGround = 2105376
    CustomColors.DarkForeGround = clWhite
    CustomColors.LightBackGroundInterior = 16514043
    CustomColors.DarkBackGroundInterior = 2829099
    CustomButtonColors.Enabled = False
    CustomButtonColors.Accent = clBlack
    CustomButtonColors.LightBackgroundNone = clBlack
    CustomButtonColors.LightBackgroundHover = clBlack
    CustomButtonColors.LightBackgroundPress = clBlack
    CustomButtonColors.LightForeGroundNone = clBlack
    CustomButtonColors.LightForeGroundHover = clBlack
    CustomButtonColors.LightForeGroundPress = clBlack
    CustomButtonColors.DarkBackGroundNone = clBlack
    CustomButtonColors.DarkBackGroundHover = clBlack
    CustomButtonColors.DarkBackGroundPress = clBlack
    CustomButtonColors.DarkForeGroundNone = clBlack
    CustomButtonColors.DarkForeGroundHover = clBlack
    CustomButtonColors.DarkForeGroundPress = clBlack
    Text = 'Button'
    LeftAccentPill = True
    LineWidth = 3.000000000000000000
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 22
    Font.Name = 'Segoe UI'
    Font.Style = []
    TabOrder = 32
    Image.IconType = None
    Image.SelectSegoe = #59188
    Image.SelectImageIndex = 0
    ImageScale = 1.250000000000000000
    StateImage.IconType = None
    StateImage.SelectSegoe = #59188
    StateImage.SelectImageIndex = 0
  end
  object FXPicture1: FXPicture
    Left = 1144
    Top = 624
    Width = 32
    Height = 32
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    TabStop = True
    Size.X = 32
    Size.Y = 32
    Position.X = 1144
    Position.Y = 624
    CustomColors.Enabled = False
    CustomColors.Accent = 13924352
    CustomColors.LightBackGround = 15987699
    CustomColors.LightForeGround = 1776410
    CustomColors.DarkBackGround = 2105376
    CustomColors.DarkForeGround = clWhite
    Picture.Data = {
      0954506E67496D61676589504E470D0A1A0A0000000D49484452000000640000
      0064080600000070E29554000000017352474200AECE1CE90000000467414D41
      0000B18F0BFC6105000000097048597300000EC300000EC301C76FA864000016
      B34944415478DAED5D095853571A3D8484842540D877C11550DC1571A356ADE2
      86BB1DAD56AD4E6D6B753AD5B633D54E1DA7D6D6A5B5DA45A75AB5DA4EAB7577
      EA828A222A20888AB288ECFB92B024248124CCBD69B18001B2BC10743CDFF7BE
      24F7BD77F3DE3DEFDE7FB9FF7F9F199EA143C1CCD417F00C4DF18C900E866784
      7430747842EAEBEBD9E4C3837C7A914F77B239934D5028143B1596570B245299
      B5999919A79EDC0BD994F52A95DC816F5DE9E1682374B4B32E25C79693AD986C
      F9E4B81CB2894D7D4FADA1C3114209205B77F23550A152F9C725E705452765BB
      DE7A50E0985124B22D28ABE69756886D6A154AB6AABEE5EB3737838A6BC156B8
      39F0ABBD5DECAB7CDDECAB06FB7B958504FAA4F4EDEA9E480E4926E42491ADD2
      D4F7DC18AD13722D39081CD64750A904C6BC088E190BC36DB818CBB766394864
      9E49B73278F1B169AC7B6905FCAA1A398FE91B76B6B716F70AF0AA1914DC43E5
      D6CB273F51A592FE5A2545894261CCDBA4FFAE04CBEC3006F5D8D1DAF5B58CD8
      D47B60B1028D7579AE6C738CE25BA2BF9919EEDD48C5F5A87B484FCB27FCD71B
      B961FE009B638EA03E7E0809ED0576A037A26475B85D2387CA987F5AA71C8110
      FFAB9A76B541488A0C2C732ED3D7D399CBC1445B2BD8170A71F97C2262A29321
      97D719B309B4029F5C53E8E8DEF01FD90BF196168812CBA0A837C2C3515BB710
      4303F769DAD5AE84785BB031C3DE1AFCBC721C3F1C8DBB891930C6FD1A0A7336
      0BC3082943A70CC635AE05AE8AA5CCF618531362C36261A6C01ABEE5D538F2C3
      E50E4B4473989B1362C85036687A084E930B4E9131D48B4D49C8506B1EA6909E
      71F1C8755C389B00A5D2A8A3B3516069C545F8AC61E00D0BC0A16A2924ADC838
      7BB90C628E0514E4216C11A620844F2E68A1231FE62979D8B7EB2C44C26A53B4
      25A3E8E4E78A19AF8EC7391B1E92A4B54DF659289518529487DE65C5F83A6820
      941D8990EE44682F2602F2ECC1484446243E11C393B6A0F265D6BCE7A0181E88
      231512B56C091496E2F9DC4CD8D5CA9167638B7D017D5AAFA43D09194DD4D8D1
      7205767E7E1CD999C5A66E3FA3614070770C9C3608350F52E053FD876D19EFE2
      8E339DBAB67E727B10423BE83C073EBCF2CBB16DE361D4105DFE698535CF1C23
      7B3AA0AF9F2D31D39A36E1AF848C0417F7D62B3036216C62D82D73B2852C360D
      DFED3CF3440A6E6D604E1ABF7F175B8C08748025D75CE3317BC970954F86AD56
      614C4228192B9CED5015938A3DDF9C2156F6D3494667372B8CE9E304673B8B16
      8FA19AD5B63EC190B1D9AD57662C42CCC9D96F3ADB43FC149321B0E1603421A2
      87A7759BC7B2ACADB1BFEF6064D7B6E113330621F4C4A5649852C53D782AC9E0
      725808F11720B8BB3DD8E6DA39C533CB6AE1F4DC70FC876F851285B2E5038D41
      C8747B6B78A51560C796634F15195446F7F4E1E3B92047D85AB1753AF7725239
      12B26AB0E4C3B9D84DAC7C494BEDC2342183ADB998249163E33F7E78AAB4290F
      071EC6F675849793A55EE71FBA5A88B40209DCDC1D306DED1CEC94C834FBC098
      24C49D638EB74997DCBCE6008A0A85A66E43466043D458DA23827CF9A487E837
      67478DDFAFFE9B8D0AC96FFEAEA0BE9DD1EDF50938522979FC60A608A11AD55A
      577B9CDC710A3763D24CDD8E0683CA86015DEC309CD8143C0ECBA0BAA472253E
      3F9189C66EAE692F8E40CAB04024CB9ABA5918236496C00682C44CEC22843CC9
      A037DDC5DD4AAD3D39D95A185C1F454EA914DF5FCA6F5246BDC56FFE6B3E765A
      9843DA98292608F1B560E34D9E053E787B3724629929DBD32038120246F77644
      570F6B46030AE21E54E0DCADB2C7CAA94372F87B3371B0AAE68F424309A107AD
      7517E0F417A7101FFB640E55548D1D1E28C0C0AEDAABB1BAE0745C091233AB34
      EE9B4E86AEDB4303F0B06156D4504246DAF03030B70C5B371C6AE766341C5446
      0775E26314E915363CDDD4585DF05D442E0A849A354E9EA5051691A1EB1BA206
      AB072E4308B1306773377A3860DBFBDF3F71DE5B2F471EC6F47582A723A3812B
      8F4149E4C3D66399A855B46C8F8D7CBE37E433872186A8C2061132DE9ECFEDF6
      A000DB371F6D87266406D4A07BAE97237A76E283D50E91676555B5D87926A7D5
      63D402FED385F88290A7D297104E5CAAEC532F67EE677FDF8F9CAC12A3DE149B
      5CB00D511A7C885ADD99C82B2F673BFC74E92E4A35E9F12DD66186C1DDED31D4
      5FA09619ED85FBB9621CBD5ED4E67121C303A19A1B8A1851B57E8484DECF91F5
      CE2AE16EDF74C4E08BA66339876D0E673B6B742643601762CDFA9186EF42BE77
      26DF2911AE44ADA6C43420A7B8020B3F398C4B44D56E0BD4F947D558EA0C6C6F
      5CBA5B8E6BC9A2368FA3BDE4D54F17E12B995C3F423ECA2B93FFB8F9A845D2ED
      B61B84825AB9965C0E7CDDECD58DDCD0D89D3D04A4F11DE04D9E7ABE956E33C2
      0AA50A1B0E4662FDF797D4DF9BC399A8B1544E50F7B8A9F0535401D20B6BB43A
      F6850903E1163E78F1FEEE5EDF69DADF2221F5F5F59D37DC7A98B666D51EF3C6
      73E2F4497713F0D54F391D5A7E6BF0DF1ADECF4D007747FE63B3684C20EA4E16
      5E225A5E4E4985FAB72531B6A81A3BA0AB9D7AE2C854A04DB3E35416AA6AB40B
      43B5B2E6E1C75DAF6F0CF770FA9BA6FD2DDE894AA59A3FE58B93DF9D3A1EA39E
      1A7B6160576C5A16A67EEAAD79CC58B7BA42582DC5AB5B8EE2617E2E427B3AAA
      A7524D0DEA32D97A5CBB11A4016B168DBEFCCF97468D63B1588FE9C91A0921BD
      83A552D57FEE3C7BE372A150FCE89849437AE0DBD5D3D563BDA940AE0D71F753
      F1EBF538D4193D38BA6D6495487130325FA7738203BC72AEEF58368B1012DB7C
      5F4B84F89FB87A7F73F807072736DFE7E9648BBDEFCEC098016D4456181945E5
      421CBA7005C5C2B685A931119B5681F389653A9DC366B194F7F7AE78BBBBB7CB
      B6E6FB34124286ABD94B371F7D6FF7AFF1FD345648B485D57346E01F2F3F4FD4
      4BE359BF6DA1B64E8133D76311979CA6EE39A6C0C9D862DCC9D23D0870CBB2B0
      537F9D3D3CDCCCCCAC89A6D2520FF9D0FFE5CF5E4BCD2D7369ADD2A13D7DB0FF
      6FB3D472C594487A98851351D750236BFFC9B26FCFE5A2B842F7FF0D1F1A907C
      74FDBCC964D87AD8B8FC314248EFB014564977BACDF878AE42A56A536A3AF02D
      F1E5CA299833AA37F49CDB6104A2AA6AFC72290A5985EDE7DEA12E934D4732D4
      9FBA8218BEA29CFFAC9E4708F9B571B926427AFE78F1EEE6791FFD345EDBCAA9
      FDB1386C00B6BE3E41673B83D106522A1199701B576EDD258D64FC79FE92CA5A
      FCFB6C8E5EE7124DBDFECEEE15AB7BF9B96D695CAE8990092B779CFE70FBD1EB
      8374FD939EBE2EF89E0C61FDBA7918BD315A43467EA1BAB7548AB577BBE883A4
      EC6A1C8FD1BF47EE5839F987E55343E6352ED344C89271EFEC7DF77C7CBA5E6A
      94158F838D4BC7E18DF021463110B585442AC3B1CBD148CED2EF09D606176E97
      E1466A85DEE7AF9A3DECC6A7AF863DD7D81ED144C89A9E8B3E5F9E9C53E66AC8
      C54E1D16805D6F4FA30996466B90B64035AF987BC94413BB098552697885CDF0
      C3E50264166BE732D184E9237ADEFF65DDDC309AAEDD50A689906D8229EB9756
      4AE4FAC5C23402F55DEDFBDB4C8CEADB99F1C6D00585C466F9392212A5226633
      A069508344A63FD1FDBABAE7C7EF7C630AE921090D654D08214F94B94C5EB79B
      3F71DD7C85AA9E11FF35B559FE3E2F141FCC7F5EEDED3415A8CD723A3A06F129
      CC4C41574B15D87E2ACBA0DC97CEEE82F2F4036FCF26845C6C286B4208E91DB6
      19F9C25D5D176C9DC35443B809B818DBD70923827C3173F44808F8A673BB50DC
      49CFC0C9A8EB90CA6B0DAA27A3A8063F5E2930A80E01DFB2A6FCD8FB730921C7
      1BCA9AF710B72B7732BF0EFDCBB7530DBD71EAF80BEDE9803E8D72282CB95C84
      8F1C8A5E5D7C0DADDE200889CD72F8E215E414E93FE9762345840B77CA0DBA0E
      0B364B213BFBCFF98490FF34943527C48710B2CD1042A82B9CBAC4A96B9CBAC8
      9B8308300C0CE88EB090C1B030A1DB850AF94B371311753B49E7D8642A377EB9
      5688DC32C3C2A17E27643121E4FB47EDD3F80042882F21E4334308991EE28600
      EFB68725570701669121CCCDD1B46E97F4DC7C1C8D8C46A5A46D9B855AE437D3
      2B71F5BE10B25AC30DCFDF0959420879347BC8780F5939C557EB701BDA435E08
      1E88C13DFDF58EA96502E21AA9DA6649C9CED5B89FCAED4C2233A85797063430
      85DF09798510B2BFA1ACB950778FBA9BF595BE84D858B2F1E6A44E3A352E1D2E
      BCEDF9F8D3C471B0B7B363EC6675858AA84BD7EFDE47446C3CEA1AE57608AB6B
      D5B2222D9F79ABFF77421610427E6C286BDE4304B7D30B76F5FDF39733F5F903
      3F572BCC0DD5DD6D522B9743525A84C533A76140DF3E3A9FCF24F24BCB70F8C2
      15E4958A109D2C5287882A94C671EDF32DB9B2CA536BA996F528C6AA39219C92
      0AC9776E3336CCADAFD73DF475480F7B75E4873E50D4D5A1343F17C38202307F
      F62C7038ED1F3DD200596D1D567F731A5F1EBF69D479167707DBCAFC43EFD099
      C3F30D659A2CF51D36133E5C5A2357E83C713E25D8551DB6A92FEAC9F0555156
      068BDA1AAC7AFD55787A98D649793022116F6C3B4904BE7182CB7BFABA14DDDD
      BD22BCF154AE2642D675FAD3A637734B2A755EB46CE90BDE70B137D0FD4E9EC8
      1AA2F1E4A6DCC3923933316EEC18A33486B648CF2FC7BC8F7E466C4A1EE3758F
      1DD035FDECA70BC7379EA4D244C8EB21CBBF79372639CF4797CA396C16DE221A
      16FD6402B532195212E38961E983F7DF7D073C9E71E3735BBD963A25D6EC398F
      2D87AE32BAB8DA9209036FFD7BD5B4918DD7817C8C1032664E9DFFF1A17507CE
      27F6D6A57257D23396901EC224A85CC94E4B41557E36366FF80801FEFE8CD6AF
      2BCEC6A561D1274750C8D0423AEB178D39B176C1F3E18DCB34F590FE9B7F8ADA
      FACEAEB3A1BA54DEDB978FC9830DF2D86B04952B457939488C8AC48A3F2FC182
      975E525BFBA6426179355ED97404BF3290277372C3FC7F4E0E09F847E3324D3D
      C4FE6E66D1AEDEAF6C9FA54BE5349C93E6741B0354D311959620EE5204FA05F4
      C027A4B708EC8DF35FDA802E1DF2D9E168ACFD2E826864FAC586D95A7165E5C7
      D6CEE570584DD20A5A0A03FAC46DC6C7AF1115586B95691EB13F7C5D8D175F4B
      49A9120991407A4A6D85105F6CDD82909010A3FD9F368849CEC5828F0F232D4F
      B7B82C8A21015E39D7762C9B4004FABDC6E52D11F272D87BFBDE3F1BF7A09B36
      955367EE8AC97EC60FED24A488ABAB90702512C9F13158F5D65BF82BD94C69B3
      5488A558B1FD140E44DCD6C966797BF6F0A8CDCBC2C690E1B7892FA62542FA6F
      F9396AEBEA9DDAC9119A20F3C644DD5C2686402A1123292E0677AE5D45BFA05E
      D8F9CDD7F0F66656A1D0059488FDE76E61E58ED35ADB2CC7D6CF5B3F7578CF0F
      9A97B74408B74824FEDAE7C54D8B148AB6A728698AF18B23DAD788A31A584156
      262E9F38823A493576EEDC894993269954E0A7E4949221EC10E2525B8FF5F572
      B2ADC8F9E99DE964B8BAD47C5F6BD1EFAF0D7C77EFF6849BE96D8E4334638926
      55B637A8065698938DB33F1D4085448965EF6DC4BFDE98A9CE513115A8DBE5FD
      6FCF63DB916B2D06D02D0AEB9FB07BD5F4518490C7D2765B23A4CFCAFFDE8CDF
      BEE5589B844C1DE2AA5EB0C5142816C97026260779E564A8605B62400F4F756C
      58402717C32B3700A76FA4AAD5E36251D3B5FF690FFEF7C70BBE5C12DC63B9A6
      F35A4BD8616DCE2D956D78EB5B8E48D8FA0B055E1DEFC3D88A08DAA246AE44D4
      3D216E65543DF624D2E8C96D6F4CC4C2F1034C1ADE5A505E85851B7FC1F9F8F4
      4765BDFAF861C65FC297AEF375F956D339AD5E6E786A9EACFE7C22F7C42FD75A
      3C862657AE9CE2078E1192F13581BA2E28095708199494D63077741F7CF59729
      B0B36E5FB70B4DBDA3F9911985423CC82FC7470722915FF6DBE834EFAD70FCEC
      CA5F5817AC478EA1CDCD07B2750E7CEE9AE53B21956A8EF0761770B1786CFB68
      38D92552F5AC9D2ED1E63432FFE0FBB3111CC0EC35D26CAEEC2291BAD1330AC9
      6781100F0B84EADF3925954D26B91AE0E3EB02FFF766E25C7995FE79EA7F72B4
      E38AFF7B13278F5CD778085D9973E220E38ED774C9A34B77CA919C2B863EAE3D
      0BB639D62F1E43757F98B3B4737E5287623E1972B28A7E6B6CF556F83B01E4BB
      482CD339A07BC62B63718A3C187279ADFE84D87138DC0F6CADB066C52E48352C
      56F6423F270CEA661C37065D1981C6CEDE48159127CE702FEBB881DDB0E79DE9
      F070B255DB0EC22A29F2CA2A1F3DE10D8D4D7FE795564256A760CCBBEB4D948C
      1EABA723828E3486AE7532CDDE1ACA3309D0244BE68FF2848FB3C151A74D400D
      DEE4DC6A5CBC5B4E0C2D66F308697E24CD16CE244F7E0579CAE575ED93A7A896
      1D9E8EA8A3376728215CA2AA7C489EAA4DABF64058F687EA4C63B0DE9CEC0B6B
      2E732E9322915C2D27E8FA534F0B82FAFA81B7741CE21A461826D6CB1A4054C9
      11B965D8F6C9E147F1AC76D6D465E2CBC8BA5312A2315D49122231A312EDF882
      1DA383C7B3C0EC7573B1BF716A06532BCA2D77B643C29E085CBB92A4FEDDCDC3
      1AB387B7B1AC761BA03644C2C34A62538820AD653E65C0D498FCE248DC08EE8E
      D2C65A175384F0CD59788F6F89CDEFEE53BF7E82868B86F6D2DF6542039669D2
      0B4D0D7B1AD18368546E2B26E172F315F8985C95B497A505C28806B265FDCF98
      3AC405015EBA47B30BC5756A221E14489EAA57593486B50D0FE1EBE6E1074DAA
      31D3EBF64E255A17F7721204E51970E46BEF32A16A2C0D3EA3C9F6C60A3EEB08
      60115B670ED1AA8E79089A2E7EF9A8211826849EF49A9D25EC23CE93EF6D372C
      0DD3BC9F23561B775552D32F87616CCC583406D7FBF8A1A02595DA184B8DFB48
      AA31FF7E629B175720942182A8B18686EE3F29183D71200AC6F547726BAF0134
      0621FD4B0A11969DDEE2A9629902917785B89B55ADEE21FF0F184BC8A89830F0
      0F7BA325188390F1848C018494E6A06A2C0D508EBE2F82ACEEE959A4BF2D5032
      2464BB26D1C2F1690C425E4EBE0D2F71D309AF3299190E5FCA4279B5E9DFDAD9
      9E18438810939E7143DB1713304D8879BD0A2B6FC5C052F99BD02AE359E2AA4F
      6784F9FAE0DAFE8BC470BC87FF07506DEA85E943901F1A84BB321D6C29A60911
      C8A578FDCE4DC8CCD98876F7C64D570FF5EB7E38666678D9C106C273B770ECE7
      E8A7EABD22CD6165C5C5E457C6E27A5777E4E81A2CC73421DD45E5E85E518E48
      4F5F882D1EB743C6F2ADE09F59843D5F9E7EA2D7896F09EE9E0E085D1686D3D6
      3C54EBF30234630C594AB3D6277AFCB81CCC21BDE6DCBE0B48887B60CAF6630C
      74881A35BE3FD81306E0BCB456FF17169BEA5DB8D46D3F9358F5968999F8716F
      C413DD5B5CDD0518BB782CAEBAD8214BCF78DE4730F5DBA2BB90DE32DD828DDB
      276271E15C02948A2747B6D017133F3F79106A47F6C245A91C8C787C4C4D48C3
      1F0DB7E161985C81E8E3316A177E477E01A5A5252122AC3FAC9E0BC279851215
      4C5E6B4720A401541353BFFE82A889374EC521363AA5C5881653806F6B851163
      FAC0961071919050A24528ADCED09F905409916446C931A0615C83AC7808269F
      C53752111D9984DC6CE32EF8DF12E85A2C5DBB7BA2DFA8DE900575C215F2B088
      8CD97BEB94F311E27F40D3AEB67AC85ED2435E3676837870D8184A54484F6135
      72E2D3D52F1CCBCF2D65349FAF39D81C7374E9EA81A0C1DD60D1C70F2944CEDD
      664A46B486FA7A21A0E8854181859A769BB571B2196293274251AF7346AEBE08
      77B4C502275B4773913820F57666F7F8A46CE7DB19C5CE39C515F6D25ADD53B5
      1B6ED2C6922BEBE6E950DEBB8B7B69600FCF077EFD3BDF4F02B2B6158B941575
      ED3475CC66913FE25D44B05F8BEFB63061E46BDBA8AFAF6793CDDBCCCCAC53AD
      52E97DE35E6E97B4DC52BFE20A8967A9486C5B22125BD62A548F1944963C8EC2
      4D605D6367C52DF170B2CF0EF4757D181CE049538FE9527A99C49E60266BD308
      E8D084B4064214956D56E493983B669CDFCBE8A34E9D4A52522621DB131735F1
      C412F2B4E219211D0CCF08E96078464807C3FF004A3715FB5B6C4C6900000000
      49454E44AE426082}
    PictureOpacity = 100.000000000000000000
    TileFlags = [ExtendX, ExtendY]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 22
    Font.Name = 'Segoe UI'
    Font.Style = []
    Anchors = [akRight, akBottom]
    TabOrder = 33
  end
  object FXTextBox1: FXTextBox
    Left = 984
    Top = 623
    Width = 150
    Height = 34
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Size.X = 150
    Size.Y = 34
    Position.X = 984
    Position.Y = 623
    CustomColors.Enabled = False
    CustomColors.Accent = 13924352
    CustomColors.LightBackGround = 15987699
    CustomColors.LightForeGround = 1776410
    CustomColors.DarkBackGround = 2105376
    CustomColors.DarkForeGround = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 18
    Font.Name = 'Segoe UI'
    Font.Style = []
    Anchors = [akRight, akBottom]
    TabOrder = 34
    Text = 'Codrut Software.  Empowering technology '
    LayoutHorizontal = Ending
    AutoSize = True
    WordWrap = True
    Elipsis = False
    InnerMargin = 0
  end
  object FXButton6: FXButton
    Left = 733
    Top = 53
    Width = 140
    Height = 35
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Size.X = 140
    Size.Y = 35
    Position.X = 733
    Position.Y = 53
    CustomColors.Enabled = False
    CustomColors.Accent = 13924352
    CustomColors.LightBackGround = 15987699
    CustomColors.LightForeGround = 1776410
    CustomColors.DarkBackGround = 2105376
    CustomColors.DarkForeGround = clWhite
    CustomColors.LightBackGroundInterior = 16514043
    CustomColors.DarkBackGroundInterior = 2829099
    CustomButtonColors.Enabled = False
    CustomButtonColors.Accent = clBlack
    CustomButtonColors.LightBackgroundNone = clBlack
    CustomButtonColors.LightBackgroundHover = clBlack
    CustomButtonColors.LightBackgroundPress = clBlack
    CustomButtonColors.LightForeGroundNone = clBlack
    CustomButtonColors.LightForeGroundHover = clBlack
    CustomButtonColors.LightForeGroundPress = clBlack
    CustomButtonColors.DarkBackGroundNone = clBlack
    CustomButtonColors.DarkBackGroundHover = clBlack
    CustomButtonColors.DarkBackGroundPress = clBlack
    CustomButtonColors.DarkForeGroundNone = clBlack
    CustomButtonColors.DarkForeGroundHover = clBlack
    CustomButtonColors.DarkForeGroundPress = clBlack
    Text = 'Button'
    LineWidth = 3.000000000000000000
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 22
    Font.Name = 'Segoe UI'
    Font.Style = []
    TabOrder = 36
    Image.Enabled = True
    Image.IconType = Image
    Image.SelectPicture.Data = {
      0954506E67496D61676589504E470D0A1A0A0000000D49484452000001F40000
      01F40806000000CBD6DF8A000000017352474200AECE1CE90000000467414D41
      0000B18F0BFC6105000000097048597300000EC300000EC301C76FA864000000
      1874455874536F667477617265005061696E742E4E455420352E312E381B69EA
      A8000000B66558496649492A000800000005001A010500010000004A0000001B
      010500010000005200000028010300010000000200000031010200100000005A
      00000069870400010000006A0000000000000060000000010000006000000001
      0000005061696E742E4E455420352E312E380003000090070004000000303233
      3001A00300010000000100000005A00400010000009400000000000000020001
      000200040000005239380002000700040000003031303000000000AB80211353
      9E382800007B6D4944415478DAEC9D079C54D5F5C7EF22A274761716DC5D1010
      61E9A8A0A008586389BD1BBBC64AB06134C59EC418AF46B1241A358926B1C59A
      A8894605354AFE56409A22A8ECD0DBB2CB52E5FECF6FDE5C7D8CBB53DFCCB96F
      E67C3F9F7367CBECCC796F66DF6FEEB9E79E53A20441100441083D25DC0E0882
      10431BFC3F96915590758D7DDD29661D7DB71DC8DA92B5216B1DBBB5DFB78C3D
      DA0ECAFBFF6E41D62AF6B34D645BC90CD9C6D8CF3693AD275B47D618FBBA31F6
      FD5AB23AB235BE5BD82AB2A564CBA25F4F2C31DCA74E1004117441C80FDAEC48
      633559F798F588DDE267DD9427E010F2EDB95D4D137C2080B043E09790D5922D
      24FB2A76BB30FAB389251BB81D15844247045D1082421BCC907725EBD38455A9
      E2FD7FC30C3E4236AF09FB8CC4BE91DB414128048AF502230899A30DC2DBFDC9
      06900DF47DDD4B79216E2175B004B0806C16D96CB299B1AF6789D00B427A88A0
      0B4222B4C13AF66E311B16BBED47B61DB76B05CED76473C93E22FB3876FB1189
      FC2A6EC704C15544D005C1A20D12C920D87B918D8CDDF6E2764BD806CCE6FF47
      3635760B91DF98DD430A426120822E142FDA94D3B82FD958B251CA13F356593D
      A6906F90B98FD9FBBB6453C8DE22815FC9ED94207020822E140FDA208B7C1CD9
      18E58938D6BFE57FA0B040021ED6E121EE6F924D26815FC6ED9420E403B99809
      858B36D8A33D9AECA0980D55F29E2F3620F0D3C85E8DD9DB24F0EBB99D12845C
      201737A1B0D0A686C6C3C9BEA7BC70FA8EDC2E094E81FDF06F91FD9BEC4512F7
      39DC0E09425088A00BE1461BAC79237C7E78CCFA70BB24840AEC857F31665348
      E037713B24089922822E840F6DDAD37828D971B1DBF6DC2E0905413DD9CB644F
      476F2796D4733B2408E920822E84036D50C7FC08E589F8C1CAAB612E08B902EB
      ECAF284FDCFF41E2BE86DB2141488608BAE02E5E295588F8A9648728D95226F0
      80303CD6DCFF46F68254B0135C45045D700B6DD02DEC40E589F8D14AC2E9825B
      34903DA73C71FF0F89FB666E8704C122822EB8813683683C9BEC34E5751D1304
      D7C1FEF6BF92FD89847D3AB7338220822EF0A14D298DA7284FC88773BB230859
      F08182B063E62EF5E6052644D085FCA20DDE73D81F7E21D9314AF6890B8505EA
      CA3F4BF67B854A75134B0CB74342F120822EE4076D3AD27806D905CA2BB92A08
      850EDAC042D81F952C79211F88A00BB9451B945B1DAFBCD07A5B6E7704810164
      C53F46763709FB346E6784C245045D081E6D5AD0F87DB2CBC8F6E37647101CE2
      0DB23BC9FE49E2BE95DB19A1B010411782439B76CA4B709BA0A404AB20240225
      67EF26FBA354A4138242045DC81E6DBA2A4FC42F222BE57647104204D6D6B1CE
      7E1709FB126E67847023822E648E3698855F497696926C7541C8067481FB3399
      26619FC7ED8C104E44D085F4D166371AAF515E5DF5EDB8DD118402E26BE5D58F
      FF3509FB47DCCE08E142045D481D4FC8AF233B4AC97B47107209F6AF3F4F7693
      08BB902A7251169223422E085C88B00B29231767A179BCFAEA372B117241E0C6
      0AFBB524EC9F703B23B8895CA485EFA24D6F1A6F20FB01590B6E770441F806EC
      5D4743981B48D8E7733B23B88508BAF02DDA74A3F1E7643F54D27B5C105C063D
      DAFF40F60BD9EE265844D00508394AB24E24BB4A497956410813EB14B6BA2975
      1B09FB3A6E67045E44D08B19AF44EB99CA5B27AFE2764710848C59A4B0BE8EBD
      EC134BBEE67646E04104BD58D1667F1A6F271BC6ED8A20088181E62F5791A8BF
      CAED88907F44D08B0D6D7AD27887F27A910B825098BC4076B924CE151722E8C5
      8236AD69FC31D9D564ADB9DD110421E7A09CEC6FC86E25616FE47646C83D22E8
      C58036472B6F56DE8BDB154110F2CE97645790A83FC3ED88905B44D00B192FBC
      7E2FD961DCAE0882C0CEBFC82E91307CE122825E8868D35261FD4CA9EB956C43
      1304E15B107ABF49216237B16433B73342B088A0171ADAEC45E3FD6443B95D11
      04C159A6935D48A2FE2EB723427088A0170A5E71985B14426A52AE551084E4A0
      8CECEF145A214F2C69E07646C81E11F442C0DB53FEA092A4B7420117DAE564CB
      62B6946C25D96AB235BEDBB5CA0BA15A5B1F33FCFD86D8637DFD4D68559BEDD5
      B7FDEB7754DE07BFD6316BE3B30E64A5649D7CB7E5645DC92A62B79D957C702C
      14BE5028F73CB1E43FDC8E08D921821E66B4C185F736E5D55E97D7323C408CE7
      917D4EB630CE6A15C4DCF56A5FDAE0834117B26AB2EE71B60B591FE57D1010C2
      01BAB961523091DE7B6BB99D1132434420AC68F33DE53567E8CEED8AD0241064
      08F64CB25964739427E2F3E882B982DBB9BCA00D66F17D62564336806CA0F204
      7FBB2C1E59C81DF85089D9FABFB91D11D247043D6C6883902866E5172979FD5C
      0121F08FC93E8A19128E3EA58BE286AC1EB550D106E1FEBE6443C8768B194A10
      9772BB2644C16C1D6BEB5749419A7021821026BC0CF6479477311478C01AF587
      6453C9FE47F6BEC21AE4C412C3ED58A8D106D7A29E64C3C9F03E1F49B6BB92AA
      869C7C4A7606BDB7FFC7ED88901A22E861C04B66429FF29F92B5E476A7C84052
      DA5B646F92618BCF34BAC06DE276AA28D0A695F2B65F8E221B43B6AFF292F284
      FCB185EC57CAEBBB2EFBD61D4704DD75B441E6FADF94376311720F04FC35B2C9
      CA13F1B932FB76046F16DF4F79E23E8EEC0025029F2F10913A95FE1716703B22
      348F08BACB687392F28AC474E476A580C13A3766E0AFC66C3A5DB4B6723B25A4
      8036D8368775F883628619FC8EDC6E1530756417D0FFC713DC8E084D2382EE22
      5E9198BBC8CEE576A5405944F622D93FC85EA70BD43A6E878400F01246316BFF
      3ED9E16455DC2E15280F915D2AFF37EE2182EE1ADA0CA6F149E56DF31182035B
      C7FEAEBC3ED11F4A18BDC0F1C2F3C89E3F8AEC58B241DC2E1518D8867922FD1F
      CDE07644F816117497D0E60CE56D1769C3ED4A8180AD644F476D62C96C6E6704
      46B4C107E4E39527EEBB71BB5320604BDB45F4BFF508B723828708BA0B78FB72
      2729AFE29B901D9F292F89F06F74A1F994DB19C141B4C1B6CF5363B62BB73B05
      000A5C4D90BA0BFC88A073A34D6F1A9F52DE9E5B213390996E45FC3D6E678410
      A1CD08E509FB29CAAB512F64066A339C20BDD6791141E7449B4395274452F33A
      7DB027F665B287C95E923DB2425678B51E0E233B3B76BB3DB74B210415137F40
      FF8B2F733B52AC88A073E025EC5C4DF64B251DABD205617484F81EA50BC7526E
      678402441BEC6D3F8DEC7CE5ED7B1752075B3E7F4676AB249EE61F11F47CE36D
      49C3B68F93B85D0911987D3F4FF67BE56D33930B85907BBC0FDEFB915D4876B4
      92597B3A60AFFAB9B2B52DBF88A0E713AFEADBB3CA2B67292467B1F2B2FE1FA4
      0BC3626E678422469B6E349E477631D94EDCEE84846964C74875B9FC21829E2F
      B4D9477962DE85DB951080041B14D679822E061BB99D11846FF0EACB23BA7699
      9244D65458AE3C51FF2FB723C580087A3ED006EB710F92EDC0ED8AC3208C8ECA
      6DB7D33FFF9BDCCE084252B4414DF92BC98E50722D4D043E949F47FFD77FE176
      A4D09137612EF1D6E06E545EA73439D74D83F57164FAFF86FEE167713B230869
      A3CD001A7FACBCED6FB2CEDE34F8C0FE0BB2EB2507267788C8E40AAF58CC9F94
      24BF3507AA4CA1F1CC6FE91F7C21B733829035DA74A7F10AE565C74BB5C7A641
      B2DC595284263788A0E7026DCA687C4E79DD9F846D819023D10D33F265DCCE08
      42E078DBDE3063BF4889B03705BA1B1E4DFFFFABB81D293444D083469B9D150A
      9D283580DB15C7C0F6156C3B1321178A036D90008B35F64BC8DA71BBE318585E
      3B8CAE055F723B524888A0078936C394D796B392DB1587D8A4BCD0FA2FA5108C
      5094783376145BB9404962AC1FB4313E9CAE0B1F733B522888A0078536E8C3FC
      0C59076E571CE16B3264B5DE40FFB05F703B2308EC78D1BB1BC84E27DB8EDB1D
      4758ABD0016F62C96BDC8E140222E841A00D5A3222535B3E7D7B60C9E1C7F44F
      3A93DB1141700E6D062A2C3D7935E3056F5BDBA974BD7886DB91B023829E2DDA
      A099C303642DB95D7180E96413E91FF3556E4704C179B4390823D9106E571C60
      8BC2EE8089257FE47624CC88A0678336972B144291F3B844797BEDFF44FF905F
      733B2308A1411B84DECF52DE1EED6EDCEE3083FDE957D235E4B7C9EEB87AF5EA
      CA8D9B371FD8A2A4C56B155D3A47B81D77856217A2CCD1E6261AAFE57683197C
      AABE4779EBE475DCCE084268D1A6A3F2D6D7C72B89F6DD4CD793EB9AFA05847C
      CE9C399775DB69A78B3B75ECD876E6ECD9F78DDE7BEF4BB81D760511F44CD0E6
      3685D07271F306D98F649D5C1002C45B5FBF5B795DDE8A192C45FCD856958390
      CF9D3B777C4545C5F8D2D2D2F6F64E3367CE5C347AF4E82A6E675D41043D1DBC
      52AE77924DE076851184D72FA77FB4C7B91D118482459B9369BC43157767B77B
      169DB9E6D7AD5BAA7D16D6468EEF5E5D7542FC1D3EF9E49385FBEEBB6F0F6E47
      5D41043D55B469A1BCF0F245DCAE30B15579FBC97F4A62BE86DB19412878BC30
      FCAF94D78FBD05B73BF9A65B9BADEA96415FCE3B7A70699FE6EE337DC68C0563
      C78CE9CDEDAB2B88A0A78227E6A872F6436E5798405FE30B49C8A7723B220845
      87367B29EFFA338CDB957C00213FAECB4275D988ED54E78E890BECCD9E33F7B7
      7B8F1A7905B7CFAE20829E8CE21673547943F6EDAF49CC37733B2308458B3648
      94437D78248B1564BD0B08F9515D226AC26E465596A7569FEBEBAD5B4FED5C5E
      FE18B7EFAE20829E086FCDFC5E559C61F6F7C9CE21219FC1ED88200831BCA4B9
      87C9F6E476252820E423BB6E51A33B2C51E70E4FBDD0E6BAC6C64D6DDBB4E955
      5A5ABA88FB185C41043D11DADCA58A2F010E559BB0250F4D54B6703B2308421C
      DE6C1DBB6C6E50219FAD43CC27F459A42E1A917EEF9A99336746468F1E5DCD7D
      0C2E2182DE1CC5B9350D95DECE20219FC6ED88200849D066308D8F920DE57625
      5D20E4C77489A8CBF728515D4A336B4437E3934FBE1AB3EFBE3B731F8B4B88A0
      3745F1158D41063BF67D5E4762BE91DB19411052441BCCD06F24BB4A852013DE
      AE935F3CE46BD5A3A253568F357BCE9C497B8F1A7529F731B984087A3C5E39D7
      3BB8DDC8235F286F56FE16B7238220648836FBD2F808594F6E579A03627EE92E
      1175E19EEDB37F30E2AE69DB3F76C3B876A7721F974B88A0FBF11AAD3CA48AE7
      BC3CA9D0A359F6950B42F8D106535ED48A3891DB153F4184D79BE2ECD7DB98E7
      16B43A8FAE5F0F731FA32B148B7025C76B81FA842A8E3ACA8D6497D23FC283DC
      8E08821030DA9C4723127ADB70BAF14D787DF016D5A36B69A08FBD6AED3AB5CF
      8B3BA9258D2DD00CEA4469BDEA21820EB43980C61755C833465304DBD04EA27F
      80D9DC8E08829023B4E9AFBC09CAE07C3FB515F2F3066C567D2ACB72F21C6F7C
      F4993AF6C311F65BE4FD1C4ED7B4D7F27DACAE2182AE0DAA2F4D214B7D036478
      C11ADB45F4C66FE4764410841CA30D66E8BF53C891C91310F3F1BD17AB4BF66A
      9BD3E7F9F7FB9FAA93A76DB3157F2DD958BAB67D9CAF637591E216746DB0E5E1
      1DB24A6E57720C3EC14EA037FB03DC8E08829067B4399FC6492A8711C820B3D7
      53E1FAD7EAD4A42FBEB3630D0566F6A6EBDC973977C0518A57D0B5412C0899DD
      03B85DC93178731F476FF20FB81D110481096DF6A0F169B240F76DE723BCDE14
      E7BCDE463DBBA05553BF9A45B62F5DEF56E5CD1987284E41D766471A5F5178E1
      0B9BC96427D09B7B05B723822030A34D67E5ED6C09A4D77ABEC2EB4D713609FA
      734D0B3AC044ED60BAEE6DC8BB63CC149FA07BF5D951CCFF246E5772CCDD6457
      4A53154110BEC12B1B8B2252191764B1B3F20B066E51BD760A367B3D1556D635
      A8D12F5522C33DD1DD9010780A5DFF4CDE1D64A41805BDD0ABC0A1431A5A9DFE
      91DB1141101C451B24CA21A726E575752BE4E70ED8AC76CD63783D9ED73EF84C
      1DFFF18854EE7A335D07AF63739481E212746D4E535EA677A11E37D68D8EA537
      F1146E470441701C6D46D3F81C5979B2BB42CC2FD965B11ABF67FEC3EBF1DCF8
      FA1A75E7829EA9DC15B37354C1FC0BB7CFF9A25085EDBB68B30F8DD8A758A87B
      CDE7931D466FDEB9DC8E08821012B4E9ABBC1A1C7D9AFAB5ADF276619EB2D753
      E1DCD75BAB6716A47C19C70E9F03E8BAF85F6EBFF3417108BA36BD68FC1F5917
      6E5772C4BB6447D19B7639B7238220840C6D705DC44C7D6FFB232BE4E70FDAA2
      7A76CBFF3A79229224C43505AE8B7BD1F57101B7EFB9A6F0055D1BC488F0E92C
      742D0653E4EFCA0B2BADE77644108490E2EDFC798484FC04AC935FE8A0908335
      F58D6AD43FBB254B886B0AB484DE87AE93EBB88F219714B6A0177E463B7AB65F
      436FD2ADDC8E0882106E3E5FBCA6FA9D792B5FF9FEA0B2FEDCBE34C7E48F3E53
      C77C9852425C53147CE67BA10BFA3534DEC2ED460ED842369EDE98F7733B2208
      42B859BD7A75E59CB973275456564EE8D0BE7D6B6E7F1271F3EBABD41D0B7A67
      F3103FA1EBE6AFB98F235714AEA06B73288DFF244B3B36E33808ADA358CC8BDC
      8E0882105E20E473E7CEBDB85BB76E9777ECD891B5335BAA9CFFC68EEAA9F93B
      66F31088667E9FAE9F2F731F4B2E284C41D7061FE150EAD48DB4CCE068203B92
      DE8C6F703B220842388190D3CDE885B591E3BB57579DC0ED4F3A649010D7E429
      201B4ED7D1F9DCC713348527E85E720792E076E7762560D6286F5BDABBDC8E08
      82104E20E6B591C8ADD55555A771FB920901093AF85079497205551EB610051D
      D58F7EC8ED46C0A0163B6A137FC4ED882008E123165E1F5F55557545BB76ED42
      598B63D5DA756A9F1777CA24C3BD39FE40D7D4F3B98F2B480A4BD0BD72867FE6
      762360D01210623E93DB114110C2855D27EFDAB5EB659D3A75E22FF396056994
      7C4D8733E9DAFA08F7B10545E108BA3683699C4A168AE48E1441EBD303E90D37
      8FDB114110C2835D27AFAD8D1C5D5D5D750AB73F419046C9D77468241B49D7D8
      19DCC717048521E85EF198F7C96AB85D0990CF9427E65F713B22084278809847
      22915F5655559DC5ED4B90A459F2351DE6282F492EF445670A45D01FA4F15C6E
      370204623E8EDE608BB81D1104211C84711B5A3A049810D7140FD1F5F63CEE63
      CC96F00BBA36A802F738B71B018230FB1899990B82900A56C83B77EE7C497979
      79A16DD5FD861C0B3A3899AEBB4F701F6736845BD0BDA62BC8FCEEC8ED4A4060
      463E56D6CC05414885E836B4DAC82DD5D5556770FB924B96AD6E5063FF551964
      867B53D491ED16E6262EE115746DB6A7F14D858486C2005BD3C64936BB2008C9
      B0B3F2CACACA2BDA3B5EAE35080228F99A2A48AC46847433F73167429805FD46
      1AAFE37623205034667FD9672E0842227CE1F5F1E5E5E58512994CCA796FB456
      4FCFCFDBF6F99BE95A1C4A6D09A7A06BB3178D6F93B5E476250050CEF560A900
      2708427358212F2B2FBFA84BE7CE65DCFEE49B3CAC9FFB41F3AB7DE99A3C95FB
      B8D3257C82AE0DB2373193EDCBED4A00A0D1CAE1529B5D1084E6886D43BBA9AA
      AAAA9076F2A4459E051D60A7D16E61DBCA164641BF97C68BB9DD08007C0A3C5A
      BAA60982D014C51A5E8F2707255F53E5F7747DBE88FBF8D3215C82AECDF7687C
      39747E37CD85D2CF5C1084788A3DBC1E4F8E4ABEA682515EABD597B8CF41AA84
      4718B5E940E32764DDB95D0980DBE84DF2636E270441700B09AF7F97EB5FAB53
      93BED899EBE9236483E87ABD86FB3CA44298041DB3D942E88CF377B293E80DB2
      95DB114110DC40C2EBCDC3B07E1ECFC374BD0EC507AC7008BA36FBD3F89FD0F8
      DB3CC8643F80DE1CEBB91D1104811FDBD69484FCA242AEF2960D0E083A42EF87
      D075FB15EE73910CF705D26BBC824E38BDB85DC992F9CAEBEAB39CDB114110F8
      895579D385D20D2D573820E80025B907D3F5BB9EDB91448441D027D1F8236E37
      B26415D9DEF46698CBED882008BC40C8E7CC9D7B596565E5F80E4550E52D1BF2
      54F23555EEA36BF825DC4E24C26D41F70AC8BC43E6C4AB99219B945738660AB7
      238220F061C3EB5DBB759BD0A963C7B6DCFE84819B5E5FAD7EBBC099E02CF29E
      5070E61D6E479AC35D41D70655E0D0E37C28B72B59720EBD01FEC8ED8420083C
      40C8E966F4C2DAC8F1DDABAB4EE0F6274CE4B9E46B2A60F9770F576BBDBB2CE8
      57D1F81B6E37B2E46E7AE127703B2108020FB24E9E1D8EAC9FC7730D5DD76FE5
      76A229DC14746D7A2A6FCF7998C3529395176A77F2939C2008B9C386D7ABABAB
      AF68DBB6AD5353CC30E1A8A037920DA46BFB17DC8EC4E3AAA0A31CEA61DC6E64
      01322287D30BBE82DB114110F287DD4FDEB55BB7CB649D3C3B184BBEA6C24B74
      7D3F9CDB8978DC13746D8EA6F1596E37B26023D93EF4627FC0ED882008F9E1DB
      75F2DA63BA57579FCCED4F21C058F235558EA1EBFC73DC4EF8714BD0B5C1168E
      992ADC7BCE2FA017F9016E270441C80FB175F25BAAABABCEE0F6A590B8EEB53A
      75375FC9D75458A0BCD0BB3385C25C13F4EB69BC81DB8D2C78845EDC33B99D10
      0421F7D8F07A6565E515ED653F79E038BA7E1ECF0D74CDBF91DB098B3B82EE25
      C2CD220BEB3F06B633A0125C23B7238220E40E2BE45DBA74195F56562675D773
      4448041DB3F301AE24C8B924E8CF28AC498413883892E066733B220842EE90F0
      7AFE0889A08367E9DA7F2CB713C00D41F79AAFBCC6ED4616FC905ED007B99D10
      042137D85979F7EEDDAF6AD3A64D285426CC2C5FDDA0C6B853F23515D074EB75
      6E27F8055D1BBC62C8081FC6ED4A863C492FE449DC4E0882103CBEF0FA84B2B2
      B2F6DCFE140B37BEBE46DDB9A027B71BE9F0B1F22AC8B1B6C57641D0CF56E837
      1B4EBE20DB8D5EC435DC8E0882101C56C8CBCBCB2FEADCB97319B73FC5C639AF
      B751CF8623DCBE8DDBDC65BE7905DD6B8D8A0E6455AC7E64063E898DA317F02D
      6E470441080E88792412B9A9AAAAEA5C6E5F8A9510AD9FFB8990F5234D58C7E5
      00B7A087799BDA6FE885BB9ADB09411082C1CECA2B2A2A269496964A789D9190
      0A3A60DDC6C627E8DA74A3719E0A67BDF6E9647BD20BB791DB114110B243C2EB
      6ED1D0D0B071EF172BE72D6C683190DB970CC0ECBC0F69C3128E27E714F47B68
      74BA597C3340C4F7A2176C1AB7238220648784D7DD63C68C190BC6FCDFBE472A
      AF7D76181BDBDC4BFA309EE38979045D9BDE3462CF7618632A3FA317EB57DC4E
      08829039D244C55DE875B965E4C8913F259DF8317DEB649BD2246C22EB4F3A31
      3FDF4FCC25E88FD0783ACB7367073E318EA2176A0BB7238220A48F84D743C149
      A5A5A54F924EB4A4AFFFABB0BC193E1E259DC87BF1A1FC0BBA36836844B83A34
      150362E05317AAC1CDE076441084F4B0425E56567661972E5DCAB9FD1112E209
      3AF0F4228CA177EC821A4A7AF1493E9F9443D0D11AF5E8BC3F6FF65C472FCECD
      DC4E0882901E52AE35747C2BE8409B6B69BC89DBA90C788E3423AFE5CCF32BE8
      DAECA6BCAA70FC056DD203118511F4E26CE676441084D48090CF993B7742B7AE
      5DC777EAD449D6C943C0DAFAFAF51DDAB7EF4382BEE89B1F6AB33D8DEF29CC78
      C385515EF5B88FF2F584F916F430CECE113AD9875E94A9DC8E0882901C2BE45D
      2B2A2E2261E8C0ED8F903AB367CFB963EFBD475DF99D5F68B3178DEFA8F02DD5
      E675969E3F410FEFECFC77F4825CCCED842008C959B67C45D592258B75F7EAEA
      93B97D113262DB70BB9F706E75CEEB2C3D9F821EC6D9398A03F4975AED82E036
      9895D3ECEECA1E3BF718DF56BAA1859944828EDEF3B3C82AB99D4C93BCCDD2F3
      23E8E19D9D9F422FC4E3DC4E0882D03436BC5E595939A143FBF6ADB9FD11B2A6
      794107DA1C4FE353DC4EA649DE66E9F912F427683C312FCF151C6FD00BB03FB7
      1382207C1708F95663C6442291E3BB57571FC7ED8F903D28F9DAAE5DBBDEDB24
      C4358536AFD27820B7BF69929736DBB917746DFAD038876CBB9C3F5770A070CC
      307A0166723B2208C2B6C4B6A1E9EAEAAA53B87D118263FA8C19F3C78E19B34B
      D23B6A334079FDC7B7E7F6390DBE26AB214D9997CB27C987A0FF8EC60B73FE3C
      C172279DF8CBB99D1004E15B6C789D66E497B56DDB366C85468424D06B7BEBA8
      9123AF49E9CEDADC4EE315DC3EA7C9EF49572ECAE513E456D0B5E94AE317643B
      E6F47982058970F82455C7ED882008DF54791B5F5151315EDA9A163489D7CFFD
      6883ED8873C9BA713B9D061BC87A92B62CCDD513E45AD07F49E34F73FA1CC173
      1E9DF087B89D10846247EAAE171DA90B3AD0061DF21EE4763A4D7E45FAF2B35C
      3D78EE045D9B76347E45569AB3E7081EF439DF9D4EF8D7DC8E08423103315FB4
      68D12F2A2B2BCFE6F645C81BE90A3AF2B23E241BC2ED781AAC26EB411AD3908B
      07CFA5A0FF88C649397BFCDC70309DE857B99D108462C5CECA2B2A2A264878BD
      78585357B7AE53C78E7D9366B8C7A3CD4134BEC2ED7F9A4C209DB93B170F9C1B
      41D706E5F9B0BED127872725685EA2937C38B71382508C4878BDB8993367CE6D
      A3468DFA71467FACCD8B341EC67D0C69804CF77EA4375B837EE05C09FA91343E
      9FE393122408B10F956D6A82907F20E69148E4A6AAAAAA73B97D11D8482FDCEE
      479B81CA6BA015A6ADD14791DEBC10F483E64AD05FA771BF5C9F9100F9339DDC
      B3B89D107830261A51522525C17F62169AC7CECABB76EB7659A78E1DA51B5A71
      93B9A0036DFE44E399DC07910639295C16BCA06B8316771FE7E38C04C426E585
      3FBEE07644C80D24D8F8E48EAD936D9A300809F6342F239B41A29E936415E15B
      AC9077EEDC1921F63025CD0AB9235B41EFA9BC65DE30D5F147F1B269413E602E
      04FD0F0A5BBFC2C3DD745227703B21640E0936DEC7F847462DEF36B15BFFD7D8
      B35AA1BC3DAB30D447D829665D627FFBBE42673DFAE44CA2BE98FB980A112BE4
      74E13EBFA2A2A20BB73F821BD4D7D7AF6F1FDF033D13B44112F68FB88F270D1E
      24EDF961900F18ACA07BDD7022CA9BF584817564BBE472A3BF100C24DA2D9537
      936EDD84E1FD86442A2BD256B8EDF79805262B1389060AD866F9273234E4F99C
      847D33F771170AB24E2E34C7B469D33E1B376E5CDFAC1FC82B64365F791FE4C3
      00F4A72AC82266410B7AD8B6AADD4E277322B71382078936041BA1F1D6B15BFB
      350C1F16F10F8BD689DD62B730083666DF41FD13AF25FB8F42E446A98F48D4A5
      626016F8C2EB9794979777E2F647700F7A7FFC72E4C8913F0FE4C1B4D1345EC9
      7D4C6910E816B6E0045D47C39E33C806329C944C6824EB45277319B723C5826F
      2DDB0AB7FF6B08B20D8543A8AB7C5FC310366F992757D19C070D85EE24FB17D9
      221276C37DFEC28484D78534C86EFDDC8F36F870BF408567968E9D5583498702
      B9BE0429E863689CC27452324166E73980441BE28C3569FFED0E3143E8DB0AB4
      156CDC562B4FCC5D4B685941F637B2FB951782DFC8ED501890F0BA9026C1093A
      08DF2C7D2C69D19B413C5090828E0B5F58DA19CAEC3C437C6BD9AD7C66051BA1
      71883384BA7BEC16DF57C66E510EB805F731A4C97AB2A964BF26FB3F12F535DC
      0EB98AAF89CA257481EEC0ED8FE03EEB1A1B37B56DD3A657D609717EC2374B7F
      8CB4E8D4201E281841D706332FBC2061E9AAF65B3A81616BBD97177C19E34D19
      44BB5C79625DA9BE15ED1EB1AF31CBCE7D4BDEFC83FDE90B9517827F826CA9EC
      59FF162BE49D3B77BE48D6C98574983163C6823163C6F40EFC81B5B983C6B0B4
      C04617B64AD2A4D5D93E5050827E318DF7329F945441E63232DB17723BC2456C
      960D81DEDE6756B491310E814628DC0AB7FD1EB3ECB07CEACD05F5CAAB8088D9
      3A42F01BB81DE266D9F215554B972EB9ADBAAA2A2CD139C121024D88F3A30DAE
      559F2BF796F19AE312D2A4FBB27D90A004FD3D1A87739F911429F8AA70B1CA67
      10E9964DDC222C8E9014441A6FFAEEB1DB1E31EBCCEDBFE3E003E12CB2EBC9A6
      146B081EB3F2D973E64EECD1BDFAE2B66DDBEEC0ED8F105A825D3FF7A3CDC334
      86A55BDFFBA44B23B27D90EC055D9B41CACB6E0F03C8241C44276E16B723591D
      84171647C678CB38B3B36D2C81748F99156E1B1647883C2C4B23AE82F7D14AE5
      15A2C1A7EAE524EC45D17217423E67EEDCCB2A2B2BC77768DFBE35B73F42E8C9
      A5A0D7282F8B3C2C793BC876FF249B070842D06FA7312CEBD12FD0093B8ADB89
      5488CDB2AD686FA7B6157084C5EDDAB5358875CFD8ADAC63E60784DC2793FD84
      6C762167C1C7847C42D78A8A8BA5ADA91020B91374A0CD33341EC37D90297207
      E95356D9F9D909BA8EAEC5A2325C05F7994891C0B607644B6C966D453BDE30CB
      46F2D9CE31B3A28DAF7B2AEF7C87656DA8D041725C2DD94FC99E2FC45AF010F3
      85B5B57776AFAE3E81DB17A170A8ABAB6BECD8B1E3AE8166B8C7A3CD3E34BECD
      7DAC29828AA5D5A4515B327D806C05FD101A5FE63E0B29F2219DA83DF2F984B1
      5976BC6D17BBC5B61E249AED1C673D63B7EDB84F989016D8DEF610D92F558184
      E0ED3AF9CE3DBA5FD2A64D1BF9002904CAB4E9D3E78E1B3BB626E74F14AE1CAF
      4349A7FE95E91F672BE88FD0783AF719489133E9443D12F483C6897689EF6B44
      2F1016EFA5B615EA9E31C30C3C597D71215CE093353A0D8E27FB30ACB5E06D78
      BDAAB272427B5927177244CE32DCE3D1E6341A1FE53EDE14799474EA8C4CFF38
      7341D706DB97969085613D0DDDB3504826ED35CE5868BC39434310AC59631FE5
      CEEA5BF1B6DF4BF66FF1818439D483BF81ECDE3089BADD4FDEA54B97F1656565
      61F8BF16C24D6ED7CF2DDA20BAF485F22658AE83ADB1DD48AB1A33F9E36C04FD
      24E575A50A03D7D109BA39D11D62C2ED3F27B8C50C1AC2BC8BDA56A8EDF7C826
      0F4B06A5905F10727F91EC7CB265AED7828798D7D6467475B5EC2717F2467E04
      1D68732D8D37711F708A9C4C7AF544267F988DA0A3C0C691DC479E029821ED4C
      27E83B3DAE63E172348EE8A93C91C66DEFB8EF05215320E22860849AE6AFB928
      EA36BC5E5D557559BB76ED24A224E4858686868DF47EEB9DD384383FDA6076FE
      A50AC73267C6BBB13213746DB02D0A1979CE27CA7468A59EADFB91C23A8D7F96
      DD33768BCC715B73DA7F2E4A9AF899206402447C9342B961A56E70656B9BED86
      D6A54B9709125E17F2CDF4E9D3E78D1D3B76D7BC3EA9364FD1783CF7B1A700AE
      175D49D4D32E5A95A9A023112EF004B36CE8481F2DF6A4CF6023C97A7524E526
      99EE4DB7DD3BA84DDB95442FAA89D6C2052197E0FD8710FCBB64E7917DC6355B
      F7F527BFB8BCBCBC94FBC408C5C9C7D3A6CDDA6FDCB8FCB6DAD6E6001AFFC37D
      EC297206097ADA897C990AFA73343A55A0050732A05CA96B472A752C7DEE6B41
      3F2829F16E05C11120EA886C5D4DF62489FAA67C3E796C9DFC96EAEAAA8CB368
      052108F296E1EE4747F3A4E690F5E53EFE14789E04FDE874FF287DB9D306E139
      5C949CDBCE8205F12AF2EEF4014A4D1CAE54A9143815DC038568B067FD31B29F
      296FCF7A4E67EB125E171C247F09717EB49948E36DDC079F02B84620EC5E9FCE
      1F6522E8272AAF85A493E080DAB7526A6C77F5F55347A84F76D84E0DE5F64910
      9A006563D1536082F2FAAC07BEBDCD0A795979F9455D3A772EE33E6041F0C125
      E868F18C44D53024C79D44829ED639CA44D021E627721F693276DC4EFD73FD65
      D11ADB97919DA5BC0A6D82E0122844B3824C93DD1F64D95888F9A2458B7E5159
      5919966E53429140EFCD7A12F39ABC65B8C7139E1D5A4F92A09F94CE1FA427E8
      DE067D5C80C210B63BDA5CA9FEA910B6F0D6FBB15E53C9ED9420C481707B1DD9
      ABCA7B8FCE2361DF9AE983F9C2EB280ED391FBE004211E9684383FDA400F9EE3
      3E0F2980707B97740AA2A52BE807D1F80AF751A6C032E515B9DF1C2B1883BAE8
      E8358B35CBFDB99D138426C09AD9A7CAAB30F76F12F5F5E9FCB115729AF59C5F
      5151D185FB6004A1395812E2FC6883703B1A2A85A1A9D8C1A463AFA67AE77405
      FD4E1A2FE53EC214B8934EC2E5FE1F90B023BA80BDE767C68EC1B9A43EA1E841
      087EB9F2B684DE49A2BE24D91F88900B218467FDDC8F36A80B7119F7894881BB
      49CB26A47AE77405FD331AFB701F610AEC4927E1BDF81FFA2AC361967E830AC7
      F605A1F8584DF68EF23AB77DD0DCF63688792412B9A9AAAAEA5C6E8705210D5C
      1074446CFF8FFB44A4C07CD2B25D52BD73EA82AE0DDADCCDE63EBA14F88C4E40
      B3421D0BC1A3B10CB2DF3153773EC14F284A10729FA7F0095DA9A748D4BFA91A
      25B37221ACACADAF5FDFA17DFB3E6C09717EB4C112577EABD565C620D2B499A9
      DC311D41BF5279D9B8AE73231DFC0DC9EE44C28EF6A6E88E865280D89B28DB7A
      04D740211AF4207881ECAEBABABAC63973E65C5856567661972E5DCAB99D1384
      74C95B0FF454D0E6061AAFE7762305AE214DBB35953BA623E848863B88FBC852
      A01F1DFCA7A9DC31365BEF4CB68FF212E686733B2F084DB09ADEAB33162D5AB4
      B96DDBB607703B230899C29E10E7471B4472E772BB91026F90A6A594CC9D9AA0
      6B8304B25564AED75EFB980E7CB774FF882E9638AE41643F54DE9E75E79BCE08
      C505BD47D5D6AD5BD5A64D9BA286EF052184F0AF9FFBD1E6231A8771BB91046C
      5B2B4BA5477AAA821E96ED6AD7D241FF22933F8C25CCA11BDBA10A210EA5BA73
      1F8C20F88188C3B66CD9A2366EDCA8BEFEFA6B6E9704215D5C1374440B6EE676
      23050E216DFB77B23BA52AE8BFA1F12AEE234A810174D05925EED10513C538F6
      565EC2DCF7B80F4810E281A843CC21EA107799AD0B61A0B1B171539B366D7A39
      911067D1A6BFF24A30BBCEEDA46D1393DD2955410F435862161D7020D587E802
      89C20378AC1F905D48D656499B55C1216C087EF3E6CDD1103CBE160497993E63
      C682B163C6F4E6F6E33B68830CF201DC6E24611AE95B520D4E2E52DAA09ACE92
      94EECBCB4D74C081662CD245B34AA1528F523F56DE9EF516DC0729087E20E498
      A543D4712B08AEF2F1C71FCFDC6FBFFD0671FBF11DB4B991C6EBB8DD4802C270
      DD48E39625BA532A82EE7477351FC3E9603F08FA4149D4B1671D2178CCD48F50
      5E971ED73FDC0845840DC143D431639710BCE0224E65B8FBD1660F1ADFE77623
      0592765F4B45D0EFA1F112EE234902D66450BB3D2757B2D8F6B6C16468380F61
      47C31799AD0BCEE00FC1C324614E7010B712E22C3A7A7D476D77D79B77DD4B1A
      373ED11D5211F419CADBD2E5327FA0033D3FD74F42174D2C3F20047F91F29ABD
      84A1A7AE504440D4FDB37541700837051D68F380F2B62DBBCC27A4738313DD21
      B1A06B836A54CB93DE8F9F23E940FF918F272251DF816E46929D4E7682F25AC9
      BA7E7E8422C286E0ED6C5D12E6046ED6D4D5ADEBD4B1635FA732DCFD6883E5D4
      17B8DD480222D068A7BAB2B93B24137484989FE53E8A246C50A8F636B1645D3E
      9F942E9A285F88F373B6F2BAB8B5E43E118260B121789B30272178819369D3A6
      CD1E376E9CBB99E4DA6027D30AE57EF1B46348EB9AEDE59E4CD0C3D062EE553A
      C083399E982E9AA8FF8EA23B68C93A4E494B56C131FC2178D9B32E70E16C86BB
      9F709437FF4E6B703FC9047D2A8D7B711F41127E4C07781BD793C79ABCEC4976
      B2F23AB7A1FB9524CC09CEE02F1B2B217881036733DCFD6883E269BFE1762309
      FF23BD1BD9DC2F9B17741D5D2B5EABDCAF6BBE1B1DE0C7DC4ED045139DDB8E54
      DE6C1D45695C3F6F4291E1CF82973DEB429E713721CEA20D0AB77CC4ED461236
      917520CDDBD8D42F13093A3E05BCCBED7D12B0C97E273A3827A61C24EA588741
      F8FF54E5856E3A284998131CC2D682B7C22E217821D73434346C6CD7AE5D6F67
      13E22C3ADACF03ED8A2BB85D49C228D2BCA94DFD2291A0A396F99DDC9E27E131
      3AB053B99DF013DBB3BEBBF232E011824793174998139C41F6AC0BF964FAF4E9
      9F8E1D3BB61FB71F29A1CDDF683C85DB8D245C46BA775753BF4824E88F296F5D
      D8652EA0037B80DB89A6A08B6637E565C1A31E3C04BE0DB74F82E0273E0B5E66
      EB422E0845429C451BD433B99FDB8D243C4EBAD7E4878E44823E9FC65EDC9E27
      A13F1DD81C6E279A832E9058473F50791F8C0E51D85E272178C121FC656321EE
      923027044D2812E22CDA2092E0ACA6C45840BAD764939BA6C54547B763AD546E
      83F5F36EB92AF71A2474D14492DC4964C792F521DB81DB2741B0C89E7521C7B8
      9F1067F1CAC062ADBF1BB72B492827ED5B15FFC3E604FD001AFFC3ED71129EA6
      033A9EDB895489ED59473522844A46292F614E109C41F6AC0B39223C820EB441
      33B213B9DD48C281A47FAFC5FFB03941472375B6BDDD2972291DD0246E27D281
      2E90DBD1CD68E565C11FAABC6600DB71FB250816D9B32E04C9CA952BEBCACBCB
      07389FE1EE479B8B151AA1B8CD55A47F3AFE87CD09FA5F9497CCE5327BD201BD
      C7ED4426D04513EB1FF804884CF8FE4A2ACC098E217BD685200855429C459BDD
      68FC90DB8D24FC95F4EFB4F81F3627E8336974B7EEAE52EBC93AD1016DE27624
      5348D4DBD1CDE1CA0BC163D68E90BC24CC09CE60F7AC4B085EC8945025C45974
      B4FA679D727B67D22CD2BF81F13FFCAE807845EA71302E8782FF4B07339ADB89
      6C89ED591FAEBCCE6D1077EC599796AC8233F8F7AC43D825042FA449B8D6CF2D
      DABC49E3BEDC6E240099ABA818D7E8FF6153820E81713D947D3B1DC8446E2782
      822E9A3B292FFC8E4CF82164EDB87D12043FB2675DC890B00A3A72C85CD79811
      A483EFFB7FD094A09F41E39FB93D4DC28974204F713B1124BE3DEB38FFE39434
      79111C43F6AC0BE9505757D7D8B163C75D43951067D1063BA85CD7983349071F
      F1FFA02941BF55A18399DBF4A60359C0ED442EA08B26721710823F4A797DD665
      CFBAE00C107598CD82973DEB427338DF033D113ADA6C6B3EB71B49F80DE9E0D5
      FE1F3425E82F286FBFB4ABAC56DEA6FA828DF9D105B354A191BD52C86244D9D8
      8EDC3E09821F9B30B771E34609C10B4D12CA0C778B576006C5D54AB95D49C03F
      48078FF4FFA029419F47E32EDC9E26E00D3A88FDB99DC835B13DEB7B939DABBC
      CE6D5D95DB898A42916113E620EA128217E2096586BB1F6D5EA7713F6E3712F0
      3969611FFF0FB615746D90A65FAFDC5EBBBD830EE24A6E27F2055D3477565E08
      1E0973F8A0257BD605A7F067C14B085EF011CE84388B36B7D37805B71B09C027
      E8F6FE4CF778411F4AE3C7DC5E26E1743A80BF703B914F627DD60F233B876C2F
      853DF8B2675D70089B306767EB12822F6ED635366E6ADBA64DAF5026C459B4C1
      92E7A3DC6E246118E9E134FB4DBCA01F47E3DFB93D4CC2503A80E9DC4E704017
      C9617483F67EDF27C35637E9B32E38834D9883A84BD9D8E266FAF4E9F3C68E1D
      BB2BB71F59A10DB6104FCBFA7172CBF1A4874FDB6FE2051D1973BFE6F6300188
      E7B5A303D8C0ED081774C1440B56B463C5DA7A5FE5763523A10881A843D06DC2
      9C507C843A21CEA2CD8E343628B77397AE213DBCD57E132FE87FA0F13C6E0F13
      F02939DF8FDB096EE88289993992352E515E352384E05DCE7B108A0C9B30B761
      C30609C11721A14F88B368335779132757799034F187F69B78417F4379454D5C
      E55972FE586E275C812E92C870C40730CCD8D1B94DCAC60ACEE0DFB32E65638B
      8E7027C459B47946795B885D653269E23799F8F182BE90C66A6E0F13F04B723E
      FC9FFA02842E98EDE9E668B21F29AFA14E5B6E9F04C18F4D985BBF7EBD84E08B
      874211F45FD0F8336E3712504B9AD8DD7EF3ADA07BEB058DCAEDECE9A2CB704F
      855893973DC82E575E367C07252178C121EC6C1D2178ACAF4B08BE7059B56A55
      7D5959594DA833DC2DEE67BAE31FA98DCD2BF30B3AC2B79F717B978451E4F854
      6E275C25D6E405B5E02F20AB226BC5ED9320F8B1217824CC4908BE30F978DAB4
      59FB8D1B3730FB4772006D46D2F82EB71B49D895741105E1B61174C4E15FE7F6
      2C095DC8F115DC4EB80C5D301169394079F5F84790E17B97A32E42916113E610
      8247C29C5058144486BB454777152DE7762309FB932E22FF6D1B4177BDCBDA1A
      72DAE5BABA4E41174D646622047FAAF2DAB14A085E70061B7247081EB375A170
      28980C778B36E81FD289DB8D047CD375CD2FE878016EE6F62C01EF93D323B89D
      081374D1C49B107DD6515F002564A5108DE014B6C90B66EB12822F180A2321CE
      A2CD7B340EE7762301D7923622796F1B41BF5F7955C85CE50972FA646E27C246
      ACC9CB9E64D72B6FEF3AB6B649085E700A88796363A384E043CEDAFAFAF51DDA
      B7EF53100971166D1E575E2F0D577980B4117953DB08FA8BCACB9076154D4E5F
      C5ED44582161C73EF50BC92E551282171C05217898104E0A2A21CEA2CD6D344E
      E47623012F91361E8E2FFC82FE81F27A6FBBCAA5E4F4246E27C20C893A3AB5A1
      152B4A05A2E29ECCD405E7B02178D9B31E3E0A2A21CEA2CD041AEFE27623011F
      923662DBF236825EABBCAD4EAE722C39FD2CB7136127B6671D0973D7292F614E
      109CC226CC41D4B1C54D080F05971007B441A5B867B8DD484084B4315A10AE24
      E6306E916AEA72E9D011E4F4FBDC4E140A74D12C579EA0636DBD9CDB1F41F063
      451D456820EC528826341456421CD0060971EF71BB9180CD643B903E1A2BE8B8
      A0BBBEBF7B27727809B71385446CCF3AFAABDF1ABB1504A7903DEBA1A31005BD
      1B8D8BB9DD484267D2C79556D0FBD3388BDBA304603F4B2B725816D502269605
      8F5AC097292F696E076E9F04C18F7FCF3A42F0325B7793952B57AE2E2F2F1F54
      5019EE4047AF9158FB7139917800E9E36C2BE8E3687C83DBA3042C2567BB713B
      51A8C4D6D551B4E750850638DE9E754170062BE2B267DD5D0A3221CEA2CD521A
      2BB8DD48C07EA49193ADA0A325E9D3DC1E256006393B84DB8942271682C73F24
      12E68EE0F64710E2F1F759C7FABAE00E052EE8D3691CCCED46028E238D7CC60A
      FA39343EC4ED51025E23670FE476A2188885E0B1DB01A580510FBE3DB74F82E0
      C7766E83A043D82504EF060599E16ED1E63FCAEB91E12AE792463E6C05FD0A1A
      6FE7F628017F23677FC0ED44B1100BC1A36CEC3E64B7286FD62E084E217DD69D
      A3F012E22CDAFC55B9BDCDF74AD2C83BACA0DFA8BC30ABABDC4DCE4EE076A2D8
      A00B2612E4B0671D15FA4E51520B5E700C1B8247B29C24CCF1B166CD9A759D3A
      75EA5B700971166D50D4EC47DC6E24E026D2C8EBADA0A30A8ECB82197596DB89
      62842E90C8EC44D958ACA9DFA0DC4E0C118A101B8247C21C42F09230977F0AB2
      E4AB1F6D6E526882E22E9348232FB582FE2785166CEE720539FB5B6E278A9558
      08BE0319CA0BE283D5186E9F04211E1B82473B564998CB2F059D1007B4412BEA
      3BB8DD48C09F4923CFB2828EB276C7707B9480B3C9D93F713B51ECD4D5D5F588
      44223FA9AEAE3EAF5DBB762D4B4AA414BCE01636040F4187B04B083E3F147442
      1CD0E66C1A1FE6762301CF92461E6B05FDDF341ECCED51028E21679FE376A258
      59BD7A7525FDC35EDCA54B97F16565651D21E42D5BB6543BEEB8A3DA6EBBEDB8
      DD13846DF087E021EA923097170A37210E6873B48268BACB2BA491DFB382FE16
      8DA3B93D4AC001E4ECEBDC4E142310F3DADAC82DD5D55567C4FF0E62BEC30E3B
      A8EDB7DF5EC96C5D700D1B8247B21C66EC325BCF0D0D0D0D1BDBB56BD7BB6013
      E28036FB2B6C9F7697B74923F7B5828EA6277B707B948051E4EC546E278A093B
      2BEFDEBDFB556DDAB469D5DCFD5AB468111574CCD68108BBE012FE103C845D12
      E68267DAF4E973C78D1D5BC3ED474ED166248DEF72BB91800F4823875B41471D
      F7FEDC1E256028393B9DDB8962C0175E9F505656965251191B82C76C1DB37611
      75C13520E436614E9ABC044BC127C4016D50A9741AB71B09984D1A39C00AFA97
      34F6E0F62801BB92B3F3B89D2864AC909797975FD4B973E7B24C1E0362DEAA55
      ABA8011176C1256C08DECED625041F0C4522E87D68FC8CDB8D047C451AB9B315
      F4E50AEDD7DCA59A9C8D703B51A840CC2391C84D555555E766FB5836048FD93A
      045D445D70097FD95888BA24CC654FC167B8036D500EBB96DB8D042C278DACB0
      82BE5A79A53E5DA59C9C5DC5ED44A16167E5151515134A4B4B03ABD90E11B709
      7308C58BA80BAE6143F0107584E065B69E15859DE10EB441D47225B71B095843
      1A591A16412F2567D7703B512804115E4F051B82B759F022EC824BF8CBC662C6
      2E0973E9B372E5CA35741D1958D019EE401BE8E36A6E3712B08DA037D2D89ADB
      A304B4266737703B510804195E4F0584E06DC21CBE1651175CC2BF67DDCED685
      D4298AF573A0A3ADA5D773BB9180F5A4916DACA0C3D11DB93D4A80087A96D859
      79D76EDD2EEBD4B163DB7C3F3F44DD3F5B170497B0A28E99BAEC594F1D117467
      D8401AD9DA0AFA461A5B65F77839A515392BC59933205FE1F55440081E820E61
      9710BCE01AFE3DEB3049984B4E5124C4016DB6A77113B71B09D8441AB9831574
      B73F8E4E942B7FBA58212F2D2DBDA0A2A2C2991D0C10712BEAB2675D701188BA
      84E053A6F013E22C21D04911F4022451B95657B021789B052FC22EB8847FCFBA
      24CC35CD9ABABA759D3A762CDC1EE8F184402725E45E4040C8E7CC9D3BA15BD7
      AEE33B75EA94F775F274B17BD621EC923027B8860DC1DBD9BA84E0B7A568D6CF
      41C842EE92141762AC9077ADA8B8883E2D77E0F6275D20EA30CCD621EC82E012
      B267BD698A4CD043951427DBD642CAB2E52BAA962C59ACBB57579FCCED4B26D8
      8BA37F7B9BACAD0BAE2121F8EF52340971C07D41DF66DB9A149609199895CF9E
      3DE7CA9D77EE313E513734D7B1828E0B240CC2DEBA75EB6F845D105CC15F3616
      56CC0973B5B591C7AAABAB2616D1FA79A80ACBB82EE852FA35860DAF5756564E
      E8D0BEBDCB519594F00B3A2E92E886859F41D43B76ECF84D5B5641708562DFB3
      1E89441EAAAAAABAAE68C41C84ACF4AB34670901B1F0FA5DDDABAB8FE3F62548
      FCD5BA20E86BD7AE8DDE42CC77DA6927D5A1430799AD0B4E518C7DD6972D5BB6
      9C26140FF4EBD7EFBEA2127310B2E62CD23E3504FCF79D77EF1ED0BF663CB71F
      41E39FA5E3E2B86EDD3AB566CD1AD4898E86DEBB77EFAE684620B375C1398A61
      CF3AFD1FAE5EB162C57D4529E49690B54F9D45637F6E8F1230949C9DCEED0427
      08B5373434CC6FD7AEDD0EDCBE048D15747F5BCBFAFA7AB57CF972B578F1E2E8
      CF7BF6ECA96A6A6A54A74E9D24614E700A9B306745BD5066EBD867BE74C9923B
      8B5AC82DDA0CA1711AB71B09984D1A39C00AFAFB34EEC1ED51024691B353B99D
      E064E5AA5527B72829798CDB8F5C61C3EEFEB57484DE972C59A2BEFAEA2BB57E
      FD7AD5A3470F356CD83055595919DDE62608AE60DFBFB6735BD8F7AC2F5AB4E8
      8FF47FF6F3A217728B3623697C97DB8D047C401A39DC0AFA5B348EE6F6280107
      90B3AF733BC1C9D2E5CB4F6BD5B2E5A3DC7EE4127B51B4DB83366CD8100DBD43
      D4172E5C88353CD5AD5B37357CF870B5EBAEBBAA76EDDA71BB2C08DB10DFB92D
      6C0973125E6F066DF6A7F1356E3712F03669E4BE56D0FF4DE3C1DC1E25E01872
      F6396E273829D4F5F378EC2C1DA28E0B6263636354D421E69148242AEC6DDBB6
      55BBEDB65B74B6DEB97367294623384518FBACC7C2EB9348C8EF11216F026D8E
      A6F1596E3712F00A69E4F7ACA03FA3209AEE723639FB276E273879F3ADB7BE1C
      3C6890CB898B8111DFA31A337584DFB1A6BE68D1A268081E174ABAF8A8912347
      4643F1489E130457F0EF5977B96C2C7D60DEF4D5C285BFADE9D76F92087902B4
      399BC687B9DD48C0B3A491C75A41FF138D67727B94802BC8D9DF723BC1C9DB6F
      BF5D3B70E0C02A6E3FF2855FD4ED9A7A5D5D5D34F37DE9D2A5AAB6B6362AF0D5
      D5D56AAFBDF652FDFBF78F26CC09824BB81C822FBAE230D9A0CDE534DEC1ED46
      02FE4C1A799615F4BB689CC0ED51026E2267AFE776820B64B8AF5BB76E7EDBB6
      6D8B6E1A6AB705D9994E4343835AB56A5554CC1182C78C1DB3F3112346A8DD77
      DF5D75EDDA355A4256105CC1B5103C5D4FEA972D5B768F84D7D3409B9B68BC96
      DB8D044C228DBCD40AFA8D345EC7ED5102EE26675DFEC0915396AF58F18396DB
      6DF7176E3FB8B08972B6F00C441DDBDA56AC5811157418F6AE23516EF4E8D16A
      E79D778EAEB30B824BF843F01C7BD6D7D6D7AF5F1489DC53535373A708799A68
      3389C61F71BB9180E8A4D70AFA1534DECEED5102FE46CEFE80DB092EDE79E7DD
      DFF6EF5F7319B71F9C58518741D4B1AE0E6187A8DB843984E2CBCBCBD53EFBEC
      A3060F1EACCACACA64CFBAE01476B68EF76FBE42F05827FFF2AB85F7F6AFE9A7
      45C833449BBFD2782AB71B09B89234F20E2BE8E7D0F810B74709788D9C3D90DB
      092EA6BCF9E6FC218307F7E2F6839BF8E6181076CCCC57AF5E1D1575BBBD0DF7
      D9638F3DA2C28E103CFAAD0B822BF8F7ACE7BA6C6C6D24F244D7AEDDAEACE8D2
      B9E84B67678536FF51D83EED2EE792463E6C05FD581A9FE6F6280133C8D921DC
      4E70F1D65B6F7D39A84832DC53C17666B3A28EAD6D08C143D451590EA28EAD6E
      BD7AF552FBEFBFBFEAD3A78FEC59179CC3DFBF20E810FCAA55ABEA972D5F7E9F
      64AF078436A8543A98DB8D041C471AF98C15F47134BEC1ED51029692B3DDB89D
      E00009712456F3DA174067B52081A0C76F6D83A863B66EF7AB636D1D42BEDF7E
      FB4567EC08C7CB9E75C1256C081EA21E44E736EC275FB264C93D22E401A3CD52
      1A2BB8DD48C07EA49193ADA0A38EFB2C6E8F128098542B72D8CDCD9C39A4D04B
      BE66833F04EF4F98B3D5E5B0B5EDCB2FBF8CFE1C19F0071D7450B46CACEC5917
      5CC386E0F15ECD34045F5B1B79A4BABAEA2722E401A30D5A3D6E227379363080
      F471B615F4721A57707B94849DC8E125DC4EE49B77A74EFD0D7DDABE8ADB0F57
      F1978BB5B3755B5D0E5BDB20EA98AD23148FAE6D471C7144B4C90B5AB20A824B
      D8F731A24DB84D75B6BE6CD9B215AB57AFBE5FCAB5E6086D101D5ECCED46123A
      933EAEB4828EDB8D642E77BC18410EBFCFED44BE993265CA6743860CE9C3ED87
      EBD8D0A59DA9E3A2884234B6639B9DADA3A90B66EA63C78E8D66C14B9F75C125
      EC0754BC87F1E13491A8AF5CB972CD8A152BEE1521CF31DA0CA7F13D6E3712B0
      996C07D247F3ED9E1E6DD0BCDDE54A64C792C32ED7D2CD096FBEF9E6FCC192E1
      9E12FEEA7230BBAE8EEA7256D4BFF8E28BA8D0A316FCD1471F1D9DB54B9F75C1
      35EC5212DEC34D85E02391C843555555D78990E7016D5016FD196E371210216D
      ACC6177E41FF80C6DDB93D4BC0A5E4F4246E27F28D64B8A78FCD82B7B375D481
      4708DE968C85A843E02B2A2AD489279E186DF282E439D9B32EB844537BD625BC
      CE8036286A7617B71B09F890B431DAFEDC2FE82FD27818B76709D0E47451AD25
      23C3BDA1A1E173121B9942A6891575DB8AD526CB616B1BB2DFE7CF9F1F6DF282
      8BE421871CA20E3BECB06816BC948D155CC286DCE9BDBB61E1C285F7EFBCF3CE
      B777EAD46921B75F458536B7D13891DB8D04BC44DA7838BEF00BFAFD349ECFED
      59029E20A74FE676229F147BC9D76C89CF8247B21C66EB56D4B1A6BE60C18268
      6DF8418306A933CF3C53F5EEDD5B42F082AB4C569EB0CC2A292959CFED4CD1A0
      CDE3349EC4ED46021E206DBC005FF805FDE734DECCED5902DE27A747703B914F
      DE7977EA1DFD6BFA5DCEED47D889EFAFEE2F4283593A841D7BD7D1ADEDB4D34E
      8BEE5B87A8CB9E75C1415691E19AF00289FA1A6E678A026D9010379CDB8D045C
      4BDAF80B7CE117F433145AB0B9CB1A72BA94DB897C3265CA947943860CD985DB
      8F42C016A2896FC56AFBAB5B83E81F78E081EA8C33CE9010BCE032E8CD8D865A
      4B48D88BAE3E475ED166358D2EF7663E93B4F1117CE117F4FD687C9DDBB32474
      21C75DDF2F1F1892E11E2CFEAD6DB6B90BC2ED28420331C7BA3A66EAF8193AB7
      4D9830410D1830202AEA92302738C84CB20BC93E90107C8ED0A6338DCBB9DD48
      C2FEA48BD14AAF7E41C75EE7CFB83D4BC228727C2AB713F900097175756B3FED
      D8B183F4010D105BBCC3660F23048F8E6DD8AF8E0234C88087B8231CDFA64D1B
      75CE39E744B7B721042FA22E38C83AB21BC81E24ABA3F768EEDBB71513DA8CA4
      F15D6E3792B02BE9E23C7CE117746402356EF333F7389D1C2F8A243129F99A3B
      EC4CDDDF8A15E177083B441D6BEA765D1DBFDB77DF7DD555575D15DDE626EBEA
      82A3FC43796BEB0B48D473D7BEADD8D0E6341A1FE5762301F800D786747103BE
      D956BCB5C176886A6E0F13F04B72FCE7DC4EE40329F99A5BFC256351916BFDFA
      F5D1757584DF313B47F6BB1575347CA9AAAA52D75C734D54DC05C151501C6C3C
      D9BF49D437703B5310688364B39F71BB91805AD2C4EEF69B7841471C7E1CB787
      0978969C3F96DB897C3079F2E44F870E1DBA2BB71F858C9DA9DB529B988D635B
      1BC2EF36590E217814A341511AACA59F7EFAE9EAA28B2E92062F828B60B68632
      A028C0A549D497723B147AB44185B863B8DD48C064D2C4FDEC37F182FE071ACF
      E3F630019F92F3FDB89DC80792E19E3FFCDBDA6C65395B2ED686DF118AC7EC1D
      BF472BD6EBAEBB4EEDB28BBC3C827340D491F5FE0ED9D5CA4B98DBCCED5468D1
      662E8D7DB9DD48C083A4893FB4DFC40B3ADE00BFE6F6300178A3B6B3EB05858C
      64B8E797F872B14896B3A21E9F2C07C1EFD2A58BBAE28A2BA2097382E020B856
      2E535EC2DC1324EA75DC0E850E2FAFAC81CCE50E4ED7901EDE6ABF8917F4E368
      FC3BB78749184A07309DDB895C820CF7FAFAFA79EDDBB76FCDED4B31E1DFD686
      FDEAEBD6AD8BAEAB23DC0E51B7EBEA08C723810E21F8238F3C32BAB62EED5805
      0741721C263FA874760BD9E792059F06DA0CA1711AB71B49389EF4F069FB4DBC
      A00FA5F1636E0F9350F099EE2B56AE3C65BB162DFEC6ED4731E2EFD80651B795
      E5305B8798FBF7AB63AD1DF7E9DBB7AFBAF9E69BA34D5E04C14120EA33C8AE27
      9B2C7BD653C4FD0C77308CF4F09B0F1DF182DE86C67A3297F7E6DC41077025B7
      13B9E4DD77DFD5353535057D8C2E6345DDCED691016FD7D5313B47F81D66D7D5
      51A006BDD52FB8E00275D65967497539C145B6282F048F9E1D0F90A82FE176C8
      79B4B99DC62BB8DD48002230ED490F1BED0FBEBBE75C1B6C507739DBE70D3A80
      FDB99DC82592E1EE06FEE62E107584E06D6539CCD63153C72DBE472737149F19
      3D7AB4BAE9A69B54D7AE5DB9DD178478200098B0BDA6BC10FCC724EC5BB89D72
      166D503975BFAC1F27777C4E5AD8C7FF83A604FD051A8FE0F63401A8AB5B4E07
      52B06B4153A64CF96CC890217DB27F24215B6CFB4A5B03DEAEAB43C4ED4C1D6B
      EBF81E628F6CF95EBD7AA99FFFFCE76ADCB871DCEE0B425320E4FEA942B4539A
      BC348D36D0C695642EF70FF907E9E091FE1F3425E8C898FB31B7A749E84D07B2
      80DB895C808438B2D9A5A5A59265E510B6080D66EA36040F11B719F036590E61
      79883E42F0A79C724AB41EBCEC59171CC486E09130F73B250973DBA20D7618CD
      E7762309BF211DBCDAFF83A604DDF5AE6BE0443A90A7B89DC80552F2D56D20EA
      2840632BCBD972B13659CE66C0635DBD75EBD66AF7DD7757D75E7B6DB4D98BD4
      82171C03028EED6CA8558ED9FADB52612E8636C7D3E8BAC67CD365CDD294A0A3
      EFEB7BDC9E26E1763A9089DC4EE48277A74EFD754DBF7E5767FF4842AEF0978B
      8570A3B73A84DC66C1A3B21C32E0318BC7CCBEBABA5A8D1F3F3EBA677DBBED5C
      DED22A142908C17F4E86C2624F488539051DBC8D46D7356604E9E0FBFE1F3425
      E8E8EE854F6D2E5F79FE4B07329ADB895C3079CA943943870C298A6A7861C66E
      6BC3BA3A441DB3726C6583985B51C7CF308B87F0A310CDA1871EAA2EB9E49268
      9F7599AD0B8E81423410F297C8EE239B56D44D5EB4799346971B37E0F5EAE0CF
      70074D5F55B4419FDD01DC1E27009F283BD1C16CE2762468264F9E3C77E8D0A1
      2E971A1462605B9BCD80C75E7564BA63A66EEBC0FB67EAD8CF8E2C787A6DA3EB
      EAB895ED6D82836032F721D9EFC95E2651AFE77628EF68D332761EDA70BB9280
      59A47F03E37FD89CA0A370CB0FB83D4EC29E7440AE2F0DA48D64B8870BBB571D
      EBEA765B1B2ACB41CC3163C7FA3A66EA107B083FEEDFB3674F75D24927A9E38E
      3B2EDA735D66EB82636C2443D23172791EA5F767412620378B36BB29EF438DCB
      FC95F4EFB4F81F3627E8583BB88DDBE3245C4A073489DB89209192AFE1C45F03
      1EA28D4C77CCCC21EA56D8F13D441DA28FF57784E0C78C19A3CE3BEF3CD5BD7B
      77595B175C03E17684E0D18113C568DE2D9A262FDA5C4CE3BDDC6E24E12AD23F
      1DFFC3E604FD001AFFC3ED71129EA6033A9EDB8920910CF7F0E21775BB57DD8A
      3A041D6178CCDC21EA587387A823043F70E04075CE39E7A8BDF7DE5BB6B7092E
      82903B7A67FC91EC3912F595DC0EE51C6D9E50D849E5360792FEBD16FFC3E604
      BD4C799BEA5D067B28BB15528119C9700F37B60D2BC41A6BE6106F64C0DBE62E
      10777C8FB03C441F617A64C1A310CDE1871FAE4E3CF1C468C25C8B162E575E16
      8A10CCCCBF207B9E0CDBA43E29D83DEB5E4199450ADAE23628AEB62AFE87CD2F
      DE69834DF5AEB7EFEC4F073587DB89A0983C79F2ACA14387F6E7F643C81C7FB7
      360836441D3375B45DB559F0107884E53153C77D70FFCE9D3BAB912347466BC1
      D7D4D4A856AD5A711F8A20F881802F277B9BEC4F64AF93A8AFE3762A70B4C10E
      23D7356501E95EEFA67E9148D011FA3D99DBF3245C4007F600B7134141823E9B
      04BD86DB0F213BFC337508B6DDAB6E2BCBC1F03D441D19F0B80F3E0040C4070F
      1EAC8E3FFE78F5BDEF7D4FB56FDF9EFB5004211E88F82CE525CCFD9D447D21B7
      4381A2CDF9CACB197099C749F74E69EA178904FD521AEFE4F63C098FD1819DCA
      ED441048C9D7C202A21EBFAD0D33759B010FC3AC1D19F0F81DEE830F00B616FC
      41071D142D1D2B09738283600FF457CADBB38EF6A21F144C93176DD0B6FA94AC
      1F27B75C46BA775753BF4824E823955712D065B08EBE131D5CE80B2090982309
      E3096E3F84E0F08B3A66E1B6AFBA1575EC55B7A26EF7AADBD97AC78E1DD5A851
      A3D4E9A79FAE468C1811DDDE26088E813C2B680444F05F24EAABB91DCA0A6D90
      BCB298AC82DB95248C22CD9BDAD42F12093A526ED792B9BE98B71B1DDCC7DC4E
      64CBD4A9537FD1AF5FBF9F71FB21048B1575DBD805A2EEDFAB8EF03B441DDF23
      33DEAEAB235B1EC97128408310FC51471D155D67973DEB8263A0F63BD69C9F26
      7B92DE9F9F723B9431DA0CA3F1236E379280626AA810B7B1A95F26BE3A68834F
      017B711F41127E4C07E7FA9EF9A4BC3179F2CC614387BA5C9D4FC89078518768
      43BC6DA21C66EAB8B5EBEA36040F51C7DFF4EEDD3BBAA67EDA69A7A9BE7DFB4A
      C29CE01A88902233FC558582275E93978DD93D2403DA5CA5D0C1CC6DFE477A37
      B2B95F2613F4DF2AC4EBDDE6553AC083B99DC81611F4C2C62FEA988123BC8E99
      3A441C628EC62E98B163BF3A7E0EC1C7FDB0AE8EAD6D1D3A7450A3478F8E8A3A
      6E11921704C7404417D53BD192F59F24EA4BB81D4A0B6D5EA1F1206E37927027
      E9DDE5CDFD3299A01F4DE3B3DC479004847CCAE38BD4870D69CA52F84098E30B
      D0D8BDEA1075F455B7B375FF4C1DF7C7DF22DC8E76AC08BFA3731BBAB849C29C
      E0180809A373DB73644F2AAFC98BFB7BD6B541920A720276E4762509C790D63D
      D7DC2F93097AB9F2F61EBABE7077241DE43FB89DC81464B8373434CC6FD7AE9D
      940A2B7020CC302BEA98A963366E8BCF40D0172C58F08DA8FBF7AAE3C300FE16
      99EF071F7CB03AF9E493D5902143A27DD705C12120E048587E5D79DBDBDCDFB3
      AECD1134BEC0ED4612705EBB90D6355BF42DB9506B3383C641DC47928407E820
      2FE07622532421AEF8F0AFA943D83153B7897210F4F8993AEE83EC772BEAC87A
      DF679F7DA2D5E50E38E08068853941700C444D91B00C51FF0789FA97DC0E358B
      36D87B7E3EB71B49F884746E70A23BA422E8F7D07809F791242142D63DAC6560
      DF78E38D4F860D1B3630FB4712C284BF000D841D337584DFED963684E0B1A6EE
      0FBF43D4ED6C1D0C1A34281A823FF2C823D52EBBECA2B6DF7E7BEEC312043FD8
      B30E21B72178F7F6AC7BE55E5120A78ADB9524DC4B1A373ED11D5211F4B0EC8F
      DE830ED6F596774D22825EBCF84BC562168EFDE898A9C320EA76A66EF7AADB6E
      6DB83F3E0C60B68ECE6D284473C20927A8E1C3874713E804C131B0477D8AF2F6
      ACBF4AA2BE86DBA16FD066771A3FE07623054E228D7B32D11D5211746CB25F92
      D27D79B9890EF67A6E27324104BDB8B133752BEA76AF3A66E730DBAD0DB3F7D5
      AB576FB3571D1F06F0B79899A3000D44FD90430E51959595B2675D700D24CCA1
      6C2C44FD7967F6AC6B73238DD771BB9104449FD18C6C59A23BA5F61FAF0D36DB
      0FE33EA224245D5F701124C4D1C57B56595999EC432A62ACA8C36C063C441DE1
      76EC5787A0E316869FDBDEEA360C6F67EC7DFAF48966C01F7BECB16AC08001D2
      9255700D08132688E8DC86ED6DE8B3BE89D5A370E4894D237D4BAAC1A90A3A36
      DB5FC57D442910BAEE6B52F255B0F8B7B5D91938C2EC766B9B1575DB5B1D628F
      757708BB4D9A83A823E48E2C78D48247E21C7D58E43E344188A7810C85CBFE4C
      F60A89FAB22C1F2F33B44133ACD9DC2723056E276D9B98EC4EA90A3A36DBBFC2
      7D4429702D1DF42FB89D4807C97017FC5851B72178AC9723C40E5147B81D1DDB
      20EA2844634BC662C66EBBB6D98439948D45081EA28E5EEB3D7AF4903DEB826B
      20390E6DBAFFA2BC7A2733F3BE675D9B9FD37833F78948814348DBFE9DEC4EA9
      0A3A36BAA299BAEB9BEE3FA283DE9DDB897490F573A129EC5E756B360BDE5F88
      06A20EB36563EDF6369B2C0783901F73CC31EAA4934E8AEE5997262F82832061
      0E754420ECEFE475CFBA3648A4DE8DFB0424016574CB52299E967AD64C38CAE2
      817E74E06E245BA48008BAD01C569421D0FEEA729891A3152B32E03FFFFC7335
      7FFEFCE8D798ADE3F7FEFDEA30149EC15E75746E1B33668CEADAB52BF7A10942
      3C102DEC597F50799DDB6A73FE8CDAF4A5712EF781A7C01BA469FBA772C77404
      FD4A8CDC47960237D2C1DFC0ED442A20216EF98A1533BA74EE2C8B9C42935851
      B66BEB08C1A3BA1C42EC08B9A3BADCDCB973D59C3973A2A28EED6DB65CAC1575
      0B66E8A79E7A6A74DF3A7AAECB9E75C131505C01ED4BD163FD29E5958DFD3A67
      CFA6CD0D34866167D435A469B7A672C774043D2CC9039FD1C1F7E57622152421
      4E4815FFDA3AC4DA5F071E05683EF9E413357BF6EC68511ACCE021EAB8AF5FD4
      B18DAD5BB76E5141479397C18307AB76EDDA711F9A20C48390FB7FC850BD0D21
      F8BA9C3C8B3688E4EECA7DB0293088346D662A774C6FA3AA369FD1D887FBE852
      604F3A01EF713B910C498813D2C13F5B47581DA28E993AB2DEE7CD9B179DA5C3
      20EA98A9DB2439BFA0B76CD9F29B2CF833CE3843EDB9E79ED1CE6DB2675D700C
      24CC4170EF237B89DE9F0B027D746D46D0F87FDC079902F349CB7649F5CEE90A
      FA9D345ECA7D842990B0C59C2BC8FAB9900956D4117E87A823590E228EB57408
      3A42F0585BC70CDE5F510EA28D5EEA10703478193B766CB4642C42F198A92333
      5E101C029F44313B47081EDBDB6604B6673D1CADC1C1DDA4651352BD73BA821E
      96ED6B4B9557DB7D33B7238910411732C5CED6ADA8634B1BD6D4117E9F356B96
      7AFFFDF7A35FDBC233B82FB6AD41B851456EE0C08151ABA9A989D683472B5624
      CF89A80B0E8284B977C9EE227B8B447D65568FA60D924750BB3D0CD9A107938E
      BD9AEA9DD315F45634AE206BCF7D942970349D88E7B99D48C4E4C993670D1D3A
      B43FB71F4238B10D5AFC3375ACA92339EEBDF7DE53FFFBDFFFA2FBD6911D0FDA
      B66D1B15EE7EFDFA45C51C8971555555AAB4B45475EEDC391A8AC70C1EA22E21
      78C131F0664785B93B94D7E6745EC67BD6B5394A79CD625CA75E79ED5237A6FA
      07E9FFD76A8324AE13B98F34059EA7137134B713CD810CF7B5F5F5F33AB46F2F
      CDAC858CF187DFB1666E67EB5853FFE0830FD4CC9933A3497228010B31EFDBB7
      AFEADDBB77747F3A5AAE42C4B1371DBF47D63B6E21EA98CD8BA80B0E0271FB3B
      D924E585E0D7A7FD08DA40CC8FE23E9014789234ECA474FE2013410F4B6636C2
      EDD5C98AD9732119EE4250D8F0BBAD2C0761477539149CC16C1D020FA1DE69A7
      9DA233F14E9D3A4567EB56C891280731B7026E85DD8ABA08BBE018D8CA861D57
      37282F049FFA35DE6B36863DEE61D8B399B4BB5A3C99083AC2ED58A30EC3CCF2
      4A3A2177703BD11492E12E04497C111A5B88066BE810798832847AC71D77FC66
      060E21B7B7162BDE08BBE37EF81B09C10B0E82703B42D248D44685B9CFE93DBA
      35E95F697385425D74F741E4A12BE9577D3A7F94D97F69784216A80284862DF9
      AD0F9C02921027048DDD9E16BF67DDFE0CA20C0187405B91B649704D09B6FD10
      202178C16120E248D4BE81EC93846563B5C11B1833FB7EDC4EA740464BC6990A
      FAE9343EC27DC42972009D98D7B99D8847045DC805FECA70F67B6BC01F424F35
      A31D626E43F31282171C046F6E5498FB89F2C47D69930973DAA07CEA6BDCCEA6
      C819A45B8FA6FB47990A7A27E585DD5B711F750A3C4527C6A9243E24C491CD2E
      2D2DEDC0ED8B50B8C48B3BC8548CED1E76C982171C06E1A8FB62B6E03B7BD6B5
      C17AF409DC4EA600FC46B87D4DBA7F98F97FA536D83A7004F791A70092E37AD0
      C959C2ED884512E284B0B2AEB1F1CDCA9D766A43174B948296BAB1828BA0CF3A
      F293DEA7F7E9DAE84FB4E946E3572A1CC970FF20BD3A32933FCC46D04FA6F131
      EE234F11A7FAA44B429C103616D6D63EDDBDBAFA49BA40BED311A5E694427211
      F2684AC9A41A8DE01A68C98AC62BD8E2B6B4E476F553158EBEE7E014D2ABC733
      F9C36C041D8D9511760FC3A774ACAFF4A493144CD9C02C91F573212CACADAF5F
      BF78D1A23BE903E83DA5A5A58BECCF8D31E57473AAF284BD528563F94D283EFE
      525BAF749F87D4CB1BBF563B713B93020DCA0BB727ED7DDE14D92D84698345FB
      D3B8CF408A649464900B44D005D7696868D8581B894CAAE9D7EF4EBF90FB2151
      DF816EF6525E86F19E64F8902F8BEB8253ACD9A0169DF1B2AA7C2BA254DD462F
      83CE61FE423A757AA67F9CADA01F4AE34BDC6720453EA413B507B71360F2E4C9
      73870E1D1A8A16AF42F1515B1B79A2BABAEA8AE6843C1E12F6EE748366486793
      21D15342F08253346E56EA8E0F94FAFD34A596AC53EA6B7755FD30D2A99733FD
      E36C051D0906A8BA53C17D1652642C9DAC37391D4086FBBA75EBE6B76DDB7607
      EE9321087E56AD5A55BF7CF9F27BE2C3EBA940A2DE966E0E27FB9542126A3892
      8F8422E33F5F2A75CD5B4A7DB242A98D5F737BF31D50F1AE3A9BA662D987C7B4
      412536E75B95C678814E166B411CC970175C83DE93F54B972DBBAFA65FBF49E9
      0AB91F1275CCCC91FDFE1B32746684A84B085E7082AD06EF519A813690A8D3B4
      EE1F9F2BB56E0BB757DBF05BD2A72BB2798020047D088DD3B8CF448A20D03288
      4EDA2C2E0724C35D7085BABAB5EB162F597C5FA275F24C2061479D8A8B9557E8
      03EBEA128217F281F11B047CCB5615AD6B0C215FBC4EA905754A7DBE866ED72A
      F5E7994A2D6A706A4D7D2869D3F46C1E20984FCFDABC4FA313EBD329F0673A69
      67713DB924C4092E501B89FCA5BAAAEAEA2085DC0F893AB2DE4793DDA3BC59BB
      CCD485203009BE46D9579A77AB2F70ABDF53A3DF58A8467E0111AF7332C4EEE7
      03D2A5E1D93E4850823E9EC6BBB9CF488A607D62173A790B399E5C045DE064F9
      F2E52B57AD5AF5FB7EFDFADD972B31B790A8E3FAD25579EBEA67731FBB107A20
      C9C8D99AA73CE1FE52F904BCA4A464C537F7D4A63AF6BBB06CA7FC1169D23DD9
      3E4850825E46232E0E6149F4CA7AAD221362255FE7D085B43DF709108A8B65CB
      96ADA0F7DEFDF910F278620973E8EB8CB5F572EE7321380B041BE54EE72B4FA4
      718BEA6E56BCF133AC7ADB99B90DAFABEFD46E0F576E177ABC579226ADCAF681
      820B836983AA7127339E9474C0A6FD5EF9EE952E097102079148E4A1AAAAAAEB
      F22DE47E48D4912087C8D4BD647B739F13810DB4058DA86F05FB0BDFD7106D08
      BA15EAADBEAF4D930D579AC2EB79BE4079F91B61E071D2A2538278A020057D2C
      8D93994E4826DC4E2771623E9F5012E2847C42B3F2E5F421F2018E597953C4B2
      E03B935DA9BC0A732DB37B44C141B0A4B9527D3BA38EBF454F0D54ECDC1A6F29
      F5334F056DB4F2DE6361611C69D194201E284841C7637D423680E9A4A40B66E9
      2807BB3C5F4F28EBE7423EE00CAFA702093BCA451FA0BCBC9BEEDCFE08698386
      27986543A417C46EAD4562BF47F8DC8AF5375F0726DACDA14D17E57D7008CBEC
      1C3BAEB0F32A9064FB60334FB5F9118D93184E4AA6DC4A27F29A7C3D9908BA90
      6B5C08AFA7422C0BBEA742A44CA9EF73FB236C03D674B11C0981FECA67F67BCC
      C031CB86506F89DD5ADB9A72683C1768F36B1AAF663C77E932813428B084F2A0
      051DFB4FF1092D2C9F8E50087F977CACA523216EE5CA9533CBCBCB3B711FB450
      78B83E2B6F8A58163C126A91018FCE58489E93ED6DF9010958C81887407F11FB
      1A828DDD3F6866852D607EC1DE62BFCEF92C3B53BCB57324D085A161184094B8
      2A93BEE7CD11FC3F8F360FD2786E1E4F4AB64CA2137A69AE9F4412E2845C401F
      1257AF58B1E2BE3009793CB110FC08B23B15C28F52882608907C06618660D7C6
      DD42B4EB9437CBB642BD596D2BDA0ED55B49116DEE5298F1868787487BCE0BF2
      017321E84369FC385F672400105EEA4727F6CB5C3E8924C409418272AD342BCF
      A8EEBA8BC4B2E051031EE1D2B394D4824F05B4AF4644D48AB415EC48EC771BD4
      B742BDD9675B9C9D65678A363BD338578567EB341846BA136895D5DC84B7B479
      9DC6FDF2714602E24F7462735AF842D6CF85A0A8AD8D3C525D5DF5934210723F
      B1103C929A8E24BB91AC9B2ADED93A66C8986C40A017A96F453BE2BBC5922166
      D99BE36F49B0DDAA529E6BB4F9A3F23E088685374873F60FFA417325E8F8877C
      3ED7672440B046843ABA3373F50422E842B6AC5CB972CD8A152BEE0D73783D15
      48D851786937B2EBC8F655E1A9F6952EB8EE60970D041AAF67ADEF6BDCA2F2D9
      A6986DF47D1DB55086C5738136B8AE62A6BB1DB72B697014E9CD0B413F68AE04
      1D9FAA11FEE893E39312242FD1093E3C570F3E79CA943943870CE9C77D9042F8
      585357B76EE99225930A25BC9E0AB1107C4FE5E5E35CA8BC3EEB614B9843581B
      B36808355EB788FA56B817C7BE46F29915EB8DFEAF8B6E969D29DABCA8D0473C
      3CA0742D9679035FF6C8DD3F8836484EB82B872725171C4C27F9D5A01F1419EE
      0D0D0D9FB76BD76E47EE0314C2436363E3A6AFBEFAEAAE9A9A9A40BBA1850912
      76D4821FA7BCD97A5FE55E311ABBC50B62BD38766B851BB65A7D2BD41B7C5F47
      4D66D959A20DDAF4BEC2ED469A5C4A3A9393EDDDB9147484CDB0EE13A66D5A68
      5DB73B9DEC40FBF24886BB902E0B6B6B1FEF5E5D7D65B10AB91F12756C831DAC
      BCEA5F9889B5CDE3D3DBFAE2781D16C7DDC21032C72CDB8AF5069FC92C3B9768
      8310FB876443B85D4903BC977A90C6D4E7E2C1731BC2D2E6161AF356B82520CE
      A393FD50900F2819EE42AA145AF67A50C4CAC6EE42763CD925643BA9E012E620
      C898652F56DB8A360CA54A51F90CDBC036F86EEDD7B296CD8536588E7990DB8D
      34F935E9CB4F72F5E0B9167464A9A234609842CDF807AEA1935E17D4034A429C
      908C8686860DB5B5B5771773783D1548D89105BF0FD955647BA8D4B62961968C
      0433FC6F43A497AA6FC51AB728B202515FDF846D20C176BB937631A20D722A90
      A7D58DDB9534C0874034055B92AB27C87D928936BFA7F1829C3F4FB0DC49273D
      B0D67B22E842226A2391C7AAABAA268A90A706893A260808C1A328C70964A5CA
      9B452F8D9915EA253143581C21CEC698AD8FBBDD2CB3EC90A10D4A06E7BD0576
      96DC4FBA72612E9F201F828E4CF7392A5C5B0AF0897E5810DBD89010B766CD9A
      4F3B75EA94CF753F21042CAC8D3CD5BDBAEAEFF4E5DB22E6E943C28E10FC30E5
      85DE1B62D61867D1F5ED822BA452CC68830660285E16A6E24388F220F23B2F97
      4F929F6D20DA2021ECC4BC3C577004B2F15F12E28478B00D6DC99225F7D4F4EB
      3749843C3B624D5EBE96B07811A10D76221DC8ED469A3C497A7252AE9F245F82
      8E22111FE4EDF982E3147A111ECFE60124214EB0D4D7D7AF8F4422F7D5D4D4DC
      21422E0819A00D92229FE276234DB09CB30769C947B97EA2FC09AC36CFD27874
      DE9E2F18B00ED73F9B0439593F17406D6DE4B1EA6A592717848CF112E1669355
      72BB9226CF91861C938F27CAA7A08775967E1FBD189764FAC722E8C54DAC5CEB
      EF641B9A20648936E81B3E9EDB8D34C9DBEC1CE4575CC3394B4732CDDEF482FC
      2F933F9E3265CADC214386F4E53E0821BF88900B42806883F6BA5355F89AF5E4
      6D760EF22DE8619DA523A37204BD3069557D42867B6363E382366DDA146A7309
      210E290C230801A30DCAFDFE9FF21AF68489BCCECE41FE85359CB374F0337A61
      7E95CE1F48867B711189441EAAAAAABA4E845C1002449B9FD2F84B6E373220AF
      B373C021E88394D7EA2E6CA113D469DE239DBDE992E15E1CD08C7C057D78BBBF
      D0DB9A0A42DEF1F69CA35E7B2A15015D024BB568C9FD493E9F9427F4ADCD2334
      9ECEF2DCD981B0CF3EA986DE2521AEB059B56A55DDF2E5CBEF112117841CE035
      5F799B6C24B72B19F028E9C419F97E522E41EFADBCED07615C5BBE865EA85B53
      B9E31B9327CF1C3674E8006E8785E091F0BA20E4186DD05D4F73BB9101E8678F
      EDCEF3F3FDC47CC969DADCA3BCAE496103A17724C8CD48742724C4D5D7D7CF6B
      DFBE7D6B6E8785E090F0BA20E4016D10D97C5F85ABB197E55ED20796ED759C82
      8E2E39A86B1BC61AE7C801D88B5EB48DCDDD4112E20A8BD836B47B45C80521C7
      E868395F6C510B5B563B40EF803EB9ECA89608DEED63DADC40E3F5AC3E64CEAD
      F4A235DBEB5D12E20A0709AF0B421ED1E61685A5CD707223E9C20D5C4FCE2DE8
      989D7FAAC257CA0F208B711CBD786F35F54B49880B3F125E17843CA3CD681A27
      AB7075E7B4E01AD19734611D9703FC055EB43987C687B8DDC8902F14C242134B
      D6C4FF4212E2C2CBCA952B57AF58B1E23E117241C823DA74A21145587A72BB92
      21E792163CCCE9800B828E4F62A81E3794DB950CF94E5B3C24C491CD2631E8C0
      ED9C903AE886B668D1A23B44C805818170B6D9B620AF0A754A58DBF8F20B3AD0
      E6201A5FE176230B7E482FE483F61B49880B1F24E47FACACACFCB908B92030A0
      CD7934FE81DB8D2C389834E0556E27DC1074A0CDF3341EC9ED468634920DA717
      147BEB45D0438464AF0B0233DAF457DE16B536DCAE64C80B74ED3F8ADB09E092
      A0A3D80CCAAA8671DF21C0BEF491F4C2364A86BBFBD4D5AD5DB764C9E249D244
      451018D106228E2D6A83B95DC9900D6403398AC834853B820EB4B991C6EBB8DD
      C88247E8853D5332DCDDA6B636F2487575D54F44C80581196DFE4C63DE4BA406
      C84D74CD7766EBB56B828E4F6BB3C876E676250B2E9832E2CD2BA507BA7B2CAC
      AD7DBC7B7535BAFDBD2D622E08CC68733E8DF773BB91055F920D405496DB118B
      5B820EB43996C6A7B9DDC8946E6DB66E7AEFC82525EDDAB6D99EDB17C1437A94
      0B826368B3078DFF55E1EBA2E6E73812F367B89DF0E39EA0036D5EA6F1106E37
      32E1A85E9BD4F30B5A7DB9FADC35618E321404D886168944EEA9A9A9B953845C
      101C419BCECA4B820BF335F25F24E687723B118FAB828E04392499852EEBF1BC
      1EB5EAC1AFAAF1E51B24EAFB71FB53ACD4D6461EABAEAE9A28422E080EA14D4B
      1AFF4DB63FB72B59B09E6C902B89707EDC1474A0CDD534FE9ADB8D6C2041E776
      A1E858585BFB545555D5332D4A4ADE14311704C7D0E60E1A2FE776234B7E4A62
      7E0BB7134DE1B2A0630D1A619921DCAE640389BA512E9FE70261D5AA55F5CB96
      2FBFAFA65FBF4922E482E020DAFC80C6BF70BB9125D85ABD3B09FA266E479AC2
      6DA1D166148D6F93B5E0762515DAB434AA71CB774EE94612757C382951AE9FEF
      10525757D7B878F1E27B659D5C101C469BBD697C4D85B7CE08C0E46C0C89F9DB
      DC8E3487FB02A3CD3D345EC2ED462A74D8DEA8B59B9B3CA52B579DB3A65349C9
      37A2EEFE790F019148E491AA2AD94F2E084EA34D1F1ADF25EBCCED4A96FC9EC4
      FC226E2712E1BEB068D34E7909723DB95D4946CB92AD6A8B693698308F44BD07
      893A9AD188B067C1F2E5CB57AE5AB5EAF752AE55101C479B721ADF51682B1A6E
      BE52A86637B1642DB723890887A06873A0F29AB784C3DFE67967F5B96B901380
      BD972D6216F663CA1B52775D1042843608AFA361C9686E5702E01012F37F733B
      918CF08889360F2874350B3F4F91A87F4F79A28E2D1C22EA291089441EAAAAAA
      BA4E845C1042808E862AFF467652B60FE5000F93989FCBED442A844748B4416F
      F14FC8BA73BB922D9FFFA0AEAE6C47B395BE6C4D86843911F56658B66CD98AD5
      AB57DF2FB372410811DA60CBF1D5DC6E044044797BCE43B107395C22A20D66B6
      2F87CEEF26F8E2F435CB3BB68AAEA7A3788E887A1C125E17849012FE1AED1664
      B57F9FC4FC256E4752257C02A2CDBD345ECCED46006CA93D63CD8AB6DB4743EF
      32538FB1B6BE7EFDE2458BEE1021178410A2CDE1343EA7BCE5C4B0E37C567B3C
      E1130FAF23DB472AFC5993E0FFDB3B17F028AB338FBF1320401008552ECB6545
      D406B172E9E591E25ADB5AADB73EB56ED7D64B11907AD9BAB69651DBD56AADB6
      5A7B6A15DD6A57AD41AD78A954AD4AEBA588AC36AE8F45BC5F58111362209964
      4248B82539FBBE736624600233C9CCBCDF77BEFFEF79FEDF04D03CFF24F37DFF
      9C73DEF39ECDB567249B07F74FEDCD94508FF49AFABA75EB2AC78D1B7709821C
      801062ACB4BA7E8CDCB32CECBCCB9AC181DEAA6D2417C2191CC61E4AAEE18C0F
      BF056EAA9B936C1BD82F75130C4A7F4DE1FCB9F4924422D1D4D0D0F05B8CCA01
      0829C6CE24B71369A8B6953CD0CE3A9CC3BC4ADB48AE8437388CBD82AF9769DB
      C813C90D73939D034A52819ED9D216DE9F4DB65F747373EBFABABAEB11E40084
      1863A7F1F51956B9B6953C7125877928B325BCA1E17ABD3FCB9AA96D254FD437
      CC4D0EEA57B2D376366FC13634003CC0D8C9E4C27CB4B6953C21A37269EFBA5D
      DB486F086FA00BC6EE476E3D7DB8B6953C51DB382F3922164B15C8653ACA7945
      434343632291B819A37200428E7BFECAA06ABCB6953CD14C6EDD7C8DB691DE12
      FEC030561A17DCAB6D238FACE1501FCFA1EED55A7A535353CB860D1B1622C801
      F00063C7F275056B92B6953CF26D0EF3FBB44DF4053F02C3D8DBF81A8A4E3E59
      F276D399C9FDC983A2BFD6D6D6AD35353506410E8027182BD3EBCB5807695BC9
      23B77398CFD736D1577C09F421E4CE4E9FAC6D258F48A8CB294599B5F4D0FDAC
      701A1A009EE146E64F915F61FE16EBB361DBA2D61DA10B891E31F61072050D65
      DA56F2884CBF4F481FBB1A9A606F6C6C6CAEAFAFBF09A372003CC2AD994B98FB
      34CDDE4652581D8FBDAA6D241F043E1C72C2D8D97C5DA46D23CF48A1DCDEE963
      5733DBD902F9736B6969D95C8B2E6F00F887AB669793D37C2980CB700687F99D
      DA26F2452083A14FF8732A5B57EA13F39265253BAADF03B54FBDADAD6DDB07D5
      D5BF995C51B110410E8067B87DE67274A82F5BD332DCCA617E96B6897C129850
      C81BEE0CDEE7589FD6B6926792F57393FDFA97A40AE502B3ADADBAA6E6DE09E3
      C72F409003E021AE039C1C88E54BD3980CFF601DC681BE45DB483E510F848260
      ACACF1BC44FEBD09376D98936C1FD08F4A89761AAD179DF436345927BF09610E
      8087B8DEEC0F931FED5CBBD244AE08EE3D6D23F9C6CF40178C3D96AF8F927F1D
      D736D7CD496E1DD82F15E812EC450DF58D6E9D7C21A6D701F018776ADA03E4C7
      412B5DE9247724EA526D2385C0DF40178CFD115FAFD6B65100E4E8D5D6210352
      412E4B0C050FF54D9B36C97EF29B264F9E7C1D821C008F71E799CB31D5A1EF83
      D10D3FE630BF46DB44A1F03DD0E5EB5BCCFA96B69542B0E6F4E696F281D6D28E
      A3570BF2F3ACA959B778F49831178E1AB9CF3AEDAF190050208C9541C12F5817
      6B5B2910D205EE140E74AB6DA450F81DE8826B3A234572D3B4AD1488071AE725
      8F8EC55287BAC8347CDE2AE013894473FA5853AC9303E033AE98B8923C1DFC30
      ABC815C185BE79CCEEF03FD005D710E105D6486D2B05E2F98679C903FBC55223
      7509F63E4DC1373636B6D4D7D7A3EF3A0051C0D8BDF9FA10EB5FB4AD14887AD6
      A1613E74255BA211E882B187F1F5697281E723ABD7CF490E2EED975A5317C968
      3D330D9FF5CFB9B6B6F68EB163C75E8A20072002182BEDA51F637D52DB4A81D8
      CA3A92C3FC396D23C5203A812E187B3A5FEFF4F8EB4EACFD4EB2715869AAFA5D
      B69A94D2CE95F03D7EDD38D614808861EC2C72DBD2F6D1B6522064AD7C3687F9
      DDDA468A85AFC1D633C6FE8CAF3FD1B65140B6AC3C79E3DB13877676F0C79F60
      0D23573427A1FEB1113BA6D7018820C69EC65739A57290B69502722587F965DA
      268A491403DDEBCAF70C4B8ED9F4DA1163DB6B4A623486FF38825CB0CB72838C
      D853A3F5DADADA4A4CAF0310218C955FEAAF655DA06DA5C0785FD1DE1DD10B74
      C155743EC13A5CDB4A81F95BF5ECE4877B0DA051E4FA30CBD4DA90868686B644
      22713B8FCA6F46980310118C95FB5F82EECBDA560ACC0AD6D1BEB575CD866806
      BA60AC4C47CB0F7E8AB69502B3B6EAA48D4F548CE82C6B6C6C1C595F5FFF170E
      F2C51CE475DAC6000045C2D8CFF0F541D6BEDA560ACC1B2403B578AC51DB8806
      D10D74C15879733FCF1AAB6DA5C06C3D69CCFAEB6F3F7EE0D51CE4CDDA660000
      45C4757E5B48FEEEF0C920B38DB338CCD76A1BD122DA812E183B9DAFCBC9AD31
      7B47694927B5DB1875A64A075215FEE7F21BBE4DDB1700A0C0185BC6D79B492A
      BDFD6723EB087EB6BDAC6D441304BA60EC91E4F6627AF61BACD4837CEC47FC2A
      4941603CF6A6B63B00408130F62072EBE587685B2902B2D7FC787EA63DAD6D44
      1B047A06634F227703F87820C1AEC808FDFB7C03DCA66D040090678C9DCFD71B
      5865DA568A806CCF3D999F654BB48D0401047A578C9DCBD7DB293ADF97FB5967
      F3CD90D4360200E823C696F3F5772401170D640A723E3FBF7EAF6D24284425B8
      B2C758D99F799DB68D22F23EB96E4A2BB48D00007A89B1D287FD2ED6446D2B45
      E4427E6E196D13410281DE1DFE7793DB1599B6921BE332BE41B6699B01006489
      B1D228EA0A927073DD20A3C22FF8597589B689A08140EF09637FC5D7B8B68D22
      2315A2DFE11BE5356D2300803D60ECC1E446E533B4AD14991BF819F5036D1341
      0481DE13AE45ACECDD3C6FE77FE8B672DC27A46254FA1F5FC7374DBBB61900C0
      2E182B237109B4ABC8EF5EECDD21DBF0BE17B596AED9E27532F51917EAB7B0CE
      CAFC5569AC93B6D95E1F351E26FE97752646EB00040863A5B3A514EECED4B6A2
      80ECCA9122DE4E6D23410581BE274C2ABDE58D3457DB8A02325AFF39EB1ABE89
      B66B9B0120B2B843552E22377BE659BF8CAC90A658F3F839D4A16D24C820D0B3
      C14D71DD56D6DFCE696B8FE4B76C15B9DF8C5FD0360240E430F673E4B6A3456D
      AD3C03C23C4B22994EBD8247EA27ECBB6DC9A36B4BBFAE6D450999E692F5ABFF
      E41B6BA3B61900BCC75869472D3364FF4EEEC8E32822B3A3E720CCB303819E03
      F589A6636A5B638BA6DD3F7C94B61745E40004E932F7476D2300788BB1DF24D7
      EDCDF783A376870C20CEC39A79F620D073A0AAAAEA928A8A8AF39BB752D9C4BB
      CBF7D2F6A3CC53E482FD0D6D230078832B7A9320FF8AB61565E47B7001AAD973
      03819E03CB962D7B79FAF4E993F9C3CE2DED54F24F8BCAA3589CD2152994BB91
      A4B105A6E101E83D6E7AFD72D67FB00668DB51064D637A09023D07962F5FFEEE
      D4A953F727B719BD637B27758EBAA35C3A3545FDFB58C7BA945589B52E0072C0
      15DCCE21B7A77C8CB61D65E4B97A11DAB9F69EA80751D63435358D6D6B6B5B53
      5656569AFE2B59D7E9D8DE41EDA32A53A11EA5B68B3DF10A4977BD78EC496D23
      00041E638F22D77279AAB69500200381B370D04ADF40A0670907BA9C6074DF2E
      7F2DA1DEBEAD83B68FAE2C977DA2519F82CFF038B9DFB45FD7360240E0702D5B
      AF651DA76D252048BF8B5371046ADF41A067490F812EC86F96DBB776D0E63195
      E5324A1FA6ED3520C8F7E56ED64FF9467D5FDB0C00EA18BB2FC9FD20E7256046
      2F83D4DE9CC4CF88A7B58DF800023D4BAAAAAAAEAAA8A8E8A950437A9E6FE591
      7A0B8FD465D4FED1569312B23C8C8FF4B7594E6F93A6183FE79B76BDB619008A
      8EB1B2CD559E1D671366F1BA225B608FE7E7C2CBDA467C21D249930BCF2C5FFE
      D6B4A9532B7AF86729E690206FE5504F70A86F9EB2D7A629EFB40EA1768B6F71
      9A56727DF1AFE51B7883B619000A8EB123F9BA80E43011A2A86F73DD15D9EE7A
      1C3F0BD66A1BF109A44D1648415C4B4BCBEAA143870EDECD7F26A1BEBDA666DD
      EF96344EBAF1F21707CB010A876B7B0F206DE41A4620D8819FB811B9F45D3F97
      55A66D2780AC609DC8F77FA3B611DF40A067C16ED6CF3F229148241B1A1A6EAA
      A8A8F8ED8811233EE49B5A8E35AC647D4BDB7F40916097A97839A6B546DB0C00
      7DC6D8F17CFD21B9A9750479F7C873740EDFF35BB48DF808023D0B7617E8C9E6
      E6D6F57575D7A783BC76A77F74C7AF5E416E8F36BED7DD236BEC7F2037627F4B
      DB0C003963AC349B9211F969ACD23E7E365F91194CD96B7F39BABF150E844C16
      7417E86D6D6DDB3EA8AEFECDE48A8A851F0BF25D31F67472870CA020A667A406
      E121D6AFF9867F5EDB0C007BC4D859247D1788E4C0A6A81E9E920DB22D6D3EDF
      D7776B1BF11D047A16241A1BBF5D128B2DCEFCB9BAA6E6DE09E3C72FD8639077
      C5D8C3F8FA27D648EDAF2704BC48AE97F303FC10D8A66D06808F305646E0FF46
      728E01D1E7B4ED84807AD637F83E7E4EDB481440A06741F5BADA096BD7BE7F45
      6747C791A50306DCD5EDF47A3618BB1FB9509FA6FD3585840FC915D0DDC60F84
      0FB5CD800863ACB4659D4FAED02DCA27A0E5C22A7261BE46DB485440A0171B63
      87F0552AE0512C973D7208CCC3E4B6BDFD0D6B70A028B81A982F919CC72D55D9
      383425176489F24CBE575BB58D440904BA06EE41713149B315ACBDE5CA3BAC5B
      5977A1510D28086EDB99D4BD9CC5AAE8E3678B1A520B234D747E895FBC8B0F02
      5D13638FE5EB3DAC726D2B214446ED4B597298C3E3FCF0D8AE6D0884186365F4
      2DBDD5E7A65F311ACF9D26924AFF786CA9B691A88240D7C6D84924C55F449FD6
      B61262A441CD3D29C5632F6A9B0121C258296C3B95750A6BB4B69D10F30F9262
      C178EC3D6D235106811E045C139A85ACEF6A5BF180776947B8BFA36D06041063
      3F492EC445076ADBF10059023B1FCD62F441A00709636793ABEA4697A9FC2087
      3E3C98523CF6A6B619A0886BFEF24D9293BD886668DBF104E9F6782EDF5B776A
      1B010E047AD030F610BEDECF9AAC6DC533E430883FB21E21991E44C18EDFB8C2
      53096E69FA2221FE296D4B9E215D1D4FE6FBE8556D23600708F420E2B6B64963
      9533B5AD788AF410788CF56772DBE0B0B5C6078C9599AD235927901CCB49344E
      DB92A7C8B6DBEFE3BE091E08F42063ACEC5597034C866B5BF11859F793D39F9E
      4CEB157E50756A9B025960AC6CF99CCA3A2A2D39DD7090B62D8F69263978261E
      BBAFCF9F091404047AD071DDE5A4C86BA6B695882015F34FB39E612D27D9F78E
      E9F960E0A6D1A5A0ED08D617C98DC64769DB8A0855244584E8FA166810E861C0
      ED91FD09EBC7ACFEDA7622461DEB5972E1FE77D6ABFC506BD73615098C95F7BA
      D4947C9E5C887F813546DB56C490F7FAD5AC2BD1EB21F820D0C384B1324A978A
      526CB5D1432A7B5F62BD90967CFC3E46F17DC48DBE27B23EC33A342DF9183B3E
      F4902DA0B3F9BD5DA56D046407023D6CB8823943B296859F5F50900E59B2456E
      655AAF909BAAC7BEDCEE707D1764EA5CD6BF67A4359D3542DB1A4821BF9C4AED
      4E1C856FE1028110568C95F694FF4DA8E40D2A1DACFF63BD4E6ECB9C6CF3599D
      523CD6A06DAE2818BB0F5F0F484BB6614E611DCCDA9FD54FDB1EE89675243DEC
      E3B1C7B58D80DC41A0871963A507FCAFC9F59FC6CF323C24C985BB047EF52EAA
      2139433A1EEBD036B95B8C95401EC91ACF9AB08B24B025C4714641B890731116
      F07B2FA96D04F40E84800F187B34B9D1FABEDA56405E906D73F5E42AEE4572AA
      5C82DCD47EB2CBEB46726BFA196D4E4BFEFFCC747FC747C54CAEB83233329669
      6FD9F63538ADB22E1A466EFABBBCCBEBDEE47A9D8F4ABFEE433829D0173E2037
      2AFFABB611D03710E8BE60EC50BE5E43EEEC663C6801007B22B3567E3187F946
      6D33A0EF20D07DC3D8597CBD85DC761F0000E80EA9ED388783FC7FB48D80FC81
      40F71137B5FA43D665846D3F00801DC892CC9524B537F1D8366D3320BF20D07D
      C6D8897CFD2FD671DA560000EAFC85F53D9C59EE2F08F42860EC897CBD8EB59F
      B6150040D1594B3263178F2DD136020A0B023D2A182B95CC179114C0B8AA6600
      80DFC84E876B59BFE4306FD336030A0F023D6AB8697819AD7F43DB0A00A0603C
      C2BA00D3EBD102811E558CFD32B9A634D3B5AD0000F2C62AD6851CE44F6A1B01
      C507811E65DC79D26790AB7A450B5900C24B2DB913191705BECB202818087490
      39F0254EF29B3DD1106D3B0080AC91C353E4B0A65FE120158040073B3056CE9A
      BE94F55D56A9B61D00408FC81EF25B59577190D7699B01C100810E3E8EB193F8
      FA53D6698436B2000409E9D3FF0792FB13056F601710E8A0678CFD14B9F5F5AF
      13DE2B0068227DD71F2659278FC75ED3360382091ED260CF183B835C1B59043B
      00C52513E43FE3205FA96D06041B3C9C41F620D801281608729033782883DC71
      C1FE23D6BFD28EF3B501007D47B69C3D48721432821CE408021DF41E630FE0EB
      02D61CD6206D3B00841869D3BA88640B5A3CB65ADB0C08270874D0778C1DCDD7
      F359E7B24668DB0120442459B7B06EC0F633D05710E8207F18BB175FE7920BF7
      03B4ED00106064147E23EB0E0EF2166D33C00F10E820FFB896B227B07EC0FA92
      B61D0002C432D6F5AC4739C83BB5CD00BF40A083C262EC34BE9EC73A85D05616
      441339BA7431C9883C1E5BA56D06F80B021D14076387F37536EB6CD6C1DA7600
      28026F905B1FBF8B833CA96D06F80F021D141763E53D7738EB1C7267B2A33A1E
      F8C456D69FC805F9B31CE456DB10880E0874A087B152112F53F15248F7596D3B
      00F481975895AC7B38C41BB5CD8068824007C1C0F58D97603F9D354ADB0E0059
      B081DC4129951CE2AF689B0100810E8285B1FDF9FA15D6A9AC135943B52D01D0
      854DAC874846E2444F71906FD736044006043A082EC696F1F56BE4C2FD18C219
      ED4007397BFCAFE442FC110EF1366D43007407021D840363CBC985BBF48F3F9A
      3558DB12F09ACDAC27C8F555FF33AAD4411840A083F061AC4CC31F4B2EDCE515
      D3F2201F48C7B6A5E4427C293AB881B0814007E1C6D8817CFD02B9D1FBF1AC49
      DA9640A8788FF518C928DC6D33DBAA6D0880DE8240077E61AC34AD91B6B35F65
      CD620DD4B604028504F6F3E4D6C4A5FDEAEBDA8600C8170874E02FAEA84E9AD8
      1C45AE727E2AE13D1F35A4B18B6C297B8AF5246B058ADA80AFE0E106A283B1B2
      BFFD8BE4A6E88F20D78216F7805F4880CBA87B39C9143AD1331CE01BB44D0150
      0CF03003D1C5D8BDC98DE025DC3FCF9A41D81A1736644BD94AD6DFC985B88CC0
      13DAA600D000810E4006576027A17E286B66FA753F6D5B6027D6B05E6055A55F
      57A2900D0007021D80DD61EC27C885BC687AFAB582D54FDB9AE774B0DE2637FA
      7E39FDBA127DD201E819043A00B962AC9CEB7E106B0AB975F8CCC7329A2FD1B6
      17323AC98DBAE5A8D137C9AD7FBF91128AD700C809043A00F9C255D51FC83AA0
      1B8DA3E8DE6F52A8B68EB5BA1BBD8BE006203F44F5010340713156CE7D1FCF9A
      90D63FA75FE5EFC6B046933B656E80B6D51C91C349A48A7C3DAB8E55C3AA667D
      907EAD4EFD5D3CB645DB2800BE8340072028182BF7A3ACD94BB08F4E7F5C9ED6
      F02EAFC35832ED2F330283D3AF993FF74F7F3629F093CF274B0099CA7DA90897
      296E1931670AC92490A56F792BAB2DFD715BFACF1B59CDAC64975791AC634B80
      6F487D1C8F59ED6F1D0000810E00000078C1FF03C09321D42080C66400000000
      49454E44AE426082}
    Image.SelectSegoe = #59188
    Image.SelectImageIndex = 0
    ImageScale = 1.250000000000000000
    StateImage.IconType = None
    StateImage.SelectSegoe = #59188
    StateImage.SelectImageIndex = 0
  end
  object FXBlurMaterial1: FXBlurMaterial
    Left = 0
    Top = 30
    Width = 215
    Height = 631
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    TabStop = True
    Size.X = 215
    Size.Y = 631
    Position.X = 0
    Position.Y = 30
    Align = alLeft
    Version = Wallpaper
    CustomColors.Enabled = False
    CustomColors.Accent = 5789570
    CustomColors.LightBackGround = clWhite
    CustomColors.LightForeGround = 1776410
    CustomColors.DarkBackGround = clBlack
    CustomColors.DarkForeGround = clWhite
  end
  object FXButton13: FXButton
    Left = 512
    Top = 125
    Width = 140
    Height = 60
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Size.X = 140
    Size.Y = 60
    Position.X = 512
    Position.Y = 125
    CustomColors.Enabled = False
    CustomColors.Accent = 13924352
    CustomColors.LightBackGround = 15987699
    CustomColors.LightForeGround = 1776410
    CustomColors.DarkBackGround = 2105376
    CustomColors.DarkForeGround = clWhite
    CustomColors.LightBackGroundInterior = 16514043
    CustomColors.DarkBackGroundInterior = 2829099
    CustomButtonColors.Enabled = False
    CustomButtonColors.Accent = clBlack
    CustomButtonColors.LightBackgroundNone = clBlack
    CustomButtonColors.LightBackgroundHover = clBlack
    CustomButtonColors.LightBackgroundPress = clBlack
    CustomButtonColors.LightForeGroundNone = clBlack
    CustomButtonColors.LightForeGroundHover = clBlack
    CustomButtonColors.LightForeGroundPress = clBlack
    CustomButtonColors.DarkBackGroundNone = clBlack
    CustomButtonColors.DarkBackGroundHover = clBlack
    CustomButtonColors.DarkBackGroundPress = clBlack
    CustomButtonColors.DarkForeGroundNone = clBlack
    CustomButtonColors.DarkForeGroundHover = clBlack
    CustomButtonColors.DarkForeGroundPress = clBlack
    Text = 'Launch popup form'
    LineWidth = 3.000000000000000000
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 22
    Font.Name = 'Segoe UI'
    Font.Style = []
    TabOrder = 37
    OnClick = FXButton13Click
    Image.IconType = None
    Image.SelectSegoe = #59188
    Image.SelectImageIndex = 0
    ImageScale = 1.250000000000000000
    StateImage.IconType = None
    StateImage.SelectSegoe = #59188
    StateImage.SelectImageIndex = 0
  end
  object FXRadioButton1: FXRadioButton
    Left = 236
    Top = 292
    Width = 180
    Height = 30
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Size.X = 180
    Size.Y = 30
    Position.X = 236
    Position.Y = 292
    CustomColors.Enabled = False
    CustomColors.Accent = 13924352
    CustomColors.LightBackGround = 15987699
    CustomColors.LightForeGround = 1776410
    CustomColors.DarkBackGround = 2105376
    CustomColors.DarkForeGround = clWhite
    IconScale = 0.500000000000000000
    Checked = True
    Text = 'Fluent Radio Button'
    Image.IconType = None
    Image.SelectSegoe = #59188
    Image.SelectImageIndex = 0
    ImageScale = 1.500000000000000000
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 22
    Font.Name = 'Segoe UI'
    Font.Style = []
    TabOrder = 38
  end
  object FXRadioButton2: FXRadioButton
    Left = 236
    Top = 322
    Width = 180
    Height = 30
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Size.X = 180
    Size.Y = 30
    Position.X = 236
    Position.Y = 322
    CustomColors.Enabled = False
    CustomColors.Accent = 13924352
    CustomColors.LightBackGround = 15987699
    CustomColors.LightForeGround = 1776410
    CustomColors.DarkBackGround = 2105376
    CustomColors.DarkForeGround = clWhite
    IconScale = 0.500000000000000000
    Text = 'Fluent Radio Button'
    Image.IconType = None
    Image.SelectSegoe = #59188
    Image.SelectImageIndex = 0
    ImageScale = 1.500000000000000000
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 22
    Font.Name = 'Segoe UI'
    Font.Style = []
    TabOrder = 39
  end
  object FXSlider1: FXSlider
    Left = 237
    Top = 125
    Width = 261
    Height = 60
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Size.X = 261
    Size.Y = 60
    Position.X = 237
    Position.Y = 125
    CustomColors.Enabled = False
    CustomColors.Accent = 13924352
    CustomColors.LightBackGround = 15987699
    CustomColors.LightForeGround = 1776410
    CustomColors.DarkBackGround = 2105376
    CustomColors.DarkForeGround = clWhite
    CustomColors.LightBackGroundInterior = 16514043
    CustomColors.DarkBackGroundInterior = 2829099
    AutomaticCursorPointer = False
    Value = 0
    ParentColor = False
    ShowHint = True
    ParentShowHint = False
    TabOrder = 40
  end
  object FXRatingControl3: FXRatingControl
    Left = 733
    Top = 192
    Width = 140
    Height = 30
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Size.X = 140
    Size.Y = 30
    Position.X = 733
    Position.Y = 192
    CustomColors.Enabled = False
    CustomColors.Accent = 13924352
    CustomColors.LightBackGround = 15987699
    CustomColors.LightForeGround = 1776410
    CustomColors.DarkBackGround = 2105376
    CustomColors.DarkForeGround = clWhite
    ValueIncrement = 2.000000000000000000
    ClearEnabled = True
    TabOrder = 41
  end
  object FXDropdownButton1: FXDropdownButton
    Left = 520
    Top = 278
    Width = 140
    Height = 35
    Margins.Left = 0
    Margins.Top = 0
    Margins.Right = 0
    Margins.Bottom = 0
    Size.X = 140
    Size.Y = 35
    Position.X = 520
    Position.Y = 278
    CustomColors.Enabled = False
    CustomColors.Accent = 13924352
    CustomColors.LightBackGround = 15987699
    CustomColors.LightForeGround = 1776410
    CustomColors.DarkBackGround = 2105376
    CustomColors.DarkForeGround = clWhite
    CustomColors.LightBackGroundInterior = 16514043
    CustomColors.DarkBackGroundInterior = 2829099
    CustomButtonColors.Enabled = False
    CustomButtonColors.Accent = clBlack
    CustomButtonColors.LightBackgroundNone = clBlack
    CustomButtonColors.LightBackgroundHover = clBlack
    CustomButtonColors.LightBackgroundPress = clBlack
    CustomButtonColors.LightForeGroundNone = clBlack
    CustomButtonColors.LightForeGroundHover = clBlack
    CustomButtonColors.LightForeGroundPress = clBlack
    CustomButtonColors.DarkBackGroundNone = clBlack
    CustomButtonColors.DarkBackGroundHover = clBlack
    CustomButtonColors.DarkBackGroundPress = clBlack
    CustomButtonColors.DarkForeGroundNone = clBlack
    CustomButtonColors.DarkForeGroundHover = clBlack
    CustomButtonColors.DarkForeGroundPress = clBlack
    Text = 'Button'
    LineWidth = 3.000000000000000000
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 22
    Font.Name = 'Segoe UI'
    Font.Style = []
    TabOrder = 42
    SelectedItem = -1
    Items.Strings = (
      'Item1'
      'Item2'
      'Item3')
    Image.IconType = None
    Image.SelectSegoe = #59188
    Image.SelectImageIndex = 0
    ImageScale = 1.250000000000000000
    StateImage.IconType = None
    StateImage.SelectSegoe = #59188
    StateImage.SelectImageIndex = 0
  end
  object FXPopupMenu1: FXPopupMenu
    CustomColors.Enabled = False
    CustomColors.Accent = 13924352
    CustomColors.LightBackGround = 15987699
    CustomColors.LightForeGround = 1776410
    CustomColors.DarkBackGround = 2105376
    CustomColors.DarkForeGround = clWhite
    AnimationType = Square
    MinimumWidth = 300
    Items.Item = {
      05000000545046300B4658506F7075704974656D0004546578740604436F7079
      0D496D6167652E456E61626C65640911496D6167652E53656C6563745365676F
      65120100000009E716496D6167652E53656C656374496D616765496E64657802
      000853686F727463757406064374726C2B430A4974656D732E4974656D0A0400
      0000000000000B4973536570617261746F72080000545046300B4658506F7075
      704974656D0004546578740604436F70790D496D6167652E456E61626C656409
      11496D6167652E53656C6563745365676F65120100000009E716496D6167652E
      53656C656374496D616765496E64657802000853686F72746375740606437472
      6C2B430A4974656D732E4974656D0A04000000000000000B4973536570617261
      746F72080000545046300B4658506F7075704974656D0004546578740604436F
      70790D496D6167652E456E61626C65640911496D6167652E53656C6563745365
      676F65120100000009E716496D6167652E53656C656374496D616765496E6465
      7802000853686F727463757406064374726C2B430A4974656D732E4974656D0A
      04000000000000000B4973536570617261746F72080000545046300B4658506F
      7075704974656D0004546578740604436F70790D496D6167652E456E61626C65
      640911496D6167652E53656C6563745365676F65120100000009E716496D6167
      652E53656C656374496D616765496E64657802000853686F7274637574060643
      74726C2B430A4974656D732E4974656D0A04000000000000000B497353657061
      7261746F72080000545046300B4658506F7075704974656D0004546578740604
      436F70790D496D6167652E456E61626C65640911496D6167652E53656C656374
      5365676F65120100000009E716496D6167652E53656C656374496D616765496E
      64657802000853686F727463757406064374726C2B430A4974656D732E497465
      6D0A04000000000000000B4973536570617261746F72080000}
    Left = 816
    Top = 393
  end
  object FXAppManager1: FXAppManager
    ApplicationIdentifier = 'fluent-design-test'
    ApplicationName = 'Codrut Fluent Design Test'
    PublisherName = 'Codrut Software'
    HasAppData = True
    AppVersion = '1.1.0.0'
    APIName = 'timely'
    AutomaticTasks = [UpdatePrompt, WindowSaveForm, WindowLoadForm]
    UserUpdateWaitDelay = 2000
    Left = 808
    Top = 328
  end
end
