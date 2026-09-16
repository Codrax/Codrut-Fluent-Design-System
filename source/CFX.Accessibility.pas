unit CFX.Accessibility;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.ActiveX,
  System.Types,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Threading;

type
{ ---------------------------------------------------------------------------
Microsoft UI Automation
--------------------------------------------------------------------------- }

ProviderOptions = type Integer;

const
ProviderOptions_ClientSideProvider = 0;
ProviderOptions_ServerSideProvider = 1;
ProviderOptions_NonClientAreaProvider = 2;
ProviderOptions_OverrideProvider = 4;
ProviderOptions_ProviderOwnsSetFocus = 16;
ProviderOptions_UseComThreading = 32;

type
IRawElementProviderSimple = interface(IUnknown)
['{D6DD68D1-86FD-4332-8666-9ABEDEA2D24C}']


function Get_ProviderOptions(
  out pRetVal: ProviderOptions
): HRESULT; stdcall;

function GetPatternProvider(
  patternId: Integer;
  out pRetVal: IUnknown
): HRESULT; stdcall;

function GetPropertyValue(
  propertyId: Integer;
  out pRetVal: OleVariant
): HRESULT; stdcall;

function Get_HostRawElementProvider(
  out pRetVal: IRawElementProviderSimple
): HRESULT; stdcall;


end;

{ ---------------------------------------------------------------------------
CFX accessibility control interface

CFX.Controls implements this interface.

Keeping the accessibility provider dependent on this interface rather than
FXWindowsControl itself prevents CFX.Accessability from depending on
CFX.Controls.
--------------------------------------------------------------------------- }

IFXAccessibilityControl = interface
['{6C7E7A6A-8B4D-4E9A-9B44-7A1D9B5E4E21}']


function AccessibilityGetName: string;
function AccessibilityGetDescription: string;
function AccessibilityGetHelp: string;
function AccessibilityGetAutomationId: string;

function AccessibilityGetControlType: Integer;
function AccessibilityGetControlTypeName: string;

function AccessibilityIsEnabled: Boolean;
function AccessibilityIsKeyboardFocusable: Boolean;
function AccessibilityHasKeyboardFocus: Boolean;
function AccessibilityIsOffscreen: Boolean;

function AccessibilityIsControlElement: Boolean;
function AccessibilityIsContentElement: Boolean;

function AccessibilityGetBoundingRectangle: TRect;
function AccessibilityGetClassName: string;


function AccessibilityGetHandle: HWND;

// Patterns
function AccessibilityGetPattern(
  PatternId: Integer
): IUnknown;

// IValueProvider + IRangeValueProvider
function AccessibilityIsReadOnly: Boolean;

// IInvokeProvider
function AccessibilityInvoke: boolean;

// IToggleProvider
function AccessibilityGetToggleState: Integer;
function AccessibilityToggle: Boolean;

// IValueProvider
function AccessibilityGetValue: string;
function AccessibilitySetValue(const Value: string): Boolean;

// ISelectionProvider
function AccessibilityGetSelectionCount: Integer;
function AccessibilityGetSelectedIndex: Integer;
function AccessibilitySelectIndex(Index: Integer): Boolean;
function AccessibilityGetSelectionItem(Index: Integer): IRawElementProviderSimple;
function AccessibilityGetSelectionContainer: IRawElementProviderSimple;
function AccessibilityIsSelectionRequired: Boolean;

// IRangeValueProvider
function AccessibilityGetRangeValue: Double;
function AccessibilityGetRangeMinimum: Double;
function AccessibilityGetRangeMaximum: Double;
function AccessibilityGetRangeSmallChange: Double;
function AccessibilityGetRangeLargeChange: Double;
function AccessibilitySetRangeValue(Value: Double): Boolean;


end;

{ ---------------------------------------------------------------------------
TFXAccessibilityProvider
--------------------------------------------------------------------------- }

TFXAccessibilityProvider = class(
TInterfacedObject,
IRawElementProviderSimple
)
private
FControl: IFXAccessibilityControl;

protected
{ IRawElementProviderSimple }


function Get_ProviderOptions(
  out pRetVal: ProviderOptions
): HRESULT; stdcall;

function GetPatternProvider(
  patternId: Integer;
  out pRetVal: IUnknown
): HRESULT; stdcall;

function GetPropertyValue(
  propertyId: Integer;
  out pRetVal: OleVariant
): HRESULT; stdcall;

function Get_HostRawElementProvider(
  out pRetVal: IRawElementProviderSimple
): HRESULT; stdcall;


public
constructor Create(
const AControl: IFXAccessibilityControl
);


procedure Detach;


end;

{ ---------------------------------------------------------------------------
Providers
--------------------------------------------------------------------------- }

IValueProvider = interface(IUnknown)
  ['{C7935180-6FB3-4201-B174-7DF73ADBF64A}']

  function SetValue(
    const value: PWideChar
  ): HRESULT; stdcall;

  function Get_Value(
    out pRetVal: WideString
  ): HRESULT; stdcall;

  function Get_IsReadOnly(
    out pRetVal: BOOL
  ): HRESULT; stdcall;
end;

TFXValueProvider = class(TInterfacedObject, IValueProvider)
private
  FControl: IFXAccessibilityControl;

protected
  function SetValue(const Value: PWideChar): HRESULT; stdcall;
  function Get_Value(out pRetVal: WideString): HRESULT; stdcall;
  function Get_IsReadOnly(out pRetVal: BOOL): HRESULT; stdcall;

public
  constructor Create(const AControl: IFXAccessibilityControl);
end;


IToggleProvider = interface(IUnknown)
  ['{56D00BD0-C4F4-433C-A836-1A52A57E0892}']

  function Toggle: HRESULT; stdcall;

  function Get_ToggleState(
    out pRetVal: Integer
  ): HRESULT; stdcall;
end;

TFXToggleProvider = class(TInterfacedObject, IToggleProvider)
private
  FControl: IFXAccessibilityControl;
public
  constructor Create(const AControl: IFXAccessibilityControl);

  function Toggle: HRESULT; stdcall;
  function Get_ToggleState(out pRetVal: Integer): HRESULT; stdcall;
end;


IInvokeProvider = interface(IUnknown)
  ['{54FCB24B-E18E-47A2-B4D3-ECCBE77599A2}']
  function Invoke: HRESULT; stdcall;
end;

TFXInvokeProvider = class(TInterfacedObject, IInvokeProvider)
private
  FControl: IFXAccessibilityControl;
public
  constructor Create(const AControl: IFXAccessibilityControl);
  function Invoke: HRESULT; stdcall;
end;


ISelectionProvider = interface(IUnknown)
  ['{FB8B03AF-3BDF-48D4-BD36-1A657D9D5D41}']
  function Get_Selection(out pRetVal: IUnknown): HRESULT; stdcall;
  function Get_CanSelectMultiple(out pRetVal: BOOL): HRESULT; stdcall;
  function Get_IsSelectionRequired(out pRetVal: BOOL): HRESULT; stdcall;
end;

ISelectionItemProvider = interface(IUnknown)
  ['{2ACAD808-B2D4-452D-A407-91FF1AD167B2}']
  function Select: HRESULT; stdcall;
  function AddToSelection: HRESULT; stdcall;
  function RemoveFromSelection: HRESULT; stdcall;
  function Get_IsSelected(out pRetVal: BOOL): HRESULT; stdcall;
  function Get_SelectionContainer(
    out pRetVal: IRawElementProviderSimple
  ): HRESULT; stdcall;
end;

TFXSelectionProvider = class(TInterfacedObject, ISelectionProvider)
private
  FControl: IFXAccessibilityControl;
protected
  function Get_Selection(out pRetVal: IUnknown): HRESULT; stdcall;
  function Get_CanSelectMultiple(out pRetVal: BOOL): HRESULT; stdcall;
  function Get_IsSelectionRequired(out pRetVal: BOOL): HRESULT; stdcall;
public
  constructor Create(const AControl: IFXAccessibilityControl);
end;

TFXSelectionItemProvider = class(TInterfacedObject, ISelectionItemProvider)
private
  FControl: IFXAccessibilityControl;
  FIndex: Integer;
protected
  function Select: HRESULT; stdcall;
  function AddToSelection: HRESULT; stdcall;
  function RemoveFromSelection: HRESULT; stdcall;
  function Get_IsSelected(out pRetVal: BOOL): HRESULT; stdcall;
  function Get_SelectionContainer(
    out pRetVal: IRawElementProviderSimple
  ): HRESULT; stdcall;
public
  constructor Create(
    const AControl: IFXAccessibilityControl;
    AIndex: Integer
  );
end;


IRangeValueProvider = interface(IUnknown)
  ['{36DC7AEF-33E6-4691-AFE1-2BE7274B3D33}']

  function SetValue(Value: Double): HRESULT; stdcall;
  function Get_Value(out pRetVal: Double): HRESULT; stdcall;
  function Get_IsReadOnly(out pRetVal: BOOL): HRESULT; stdcall;
  function Get_Maximum(out pRetVal: Double): HRESULT; stdcall;
  function Get_Minimum(out pRetVal: Double): HRESULT; stdcall;
  function Get_LargeChange(out pRetVal: Double): HRESULT; stdcall;
  function Get_SmallChange(out pRetVal: Double): HRESULT; stdcall;
end;

TFXRangeValueProvider = class(TInterfacedObject, IRangeValueProvider)
private
  FControl: IFXAccessibilityControl;
public
  constructor Create(const AControl: IFXAccessibilityControl);

  function SetValue(Value: Double): HRESULT; stdcall;

  function Get_IsReadOnly(
    out pRetVal: BOOL
  ): HRESULT; stdcall;

  function Get_LargeChange(
    out pRetVal: Double
  ): HRESULT; stdcall;

  function Get_Maximum(
    out pRetVal: Double
  ): HRESULT; stdcall;

  function Get_Minimum(
    out pRetVal: Double
  ): HRESULT; stdcall;

  function Get_SmallChange(
    out pRetVal: Double
  ): HRESULT; stdcall;

  function Get_Value(
    out pRetVal: Double
  ): HRESULT; stdcall;
end;

{ ---------------------------------------------------------------------------
Windows UI Automation functions
--------------------------------------------------------------------------- }

function UiaHostProviderFromHwnd(
  hwnd: HWND;
  out provider: IRawElementProviderSimple
): HRESULT; stdcall;

function UiaReturnRawElementProvider(
  hwnd: HWND;
  wParam: WPARAM;
  lParam: LPARAM;
  element: IRawElementProviderSimple
): LRESULT; stdcall;

type
  TUiaHostProviderFromHwnd = function(
    hwnd: HWND;
    out provider: IRawElementProviderSimple
  ): HRESULT; stdcall;

  TUiaReturnRawElementProvider = function(
    hwnd: HWND;
    wParam: WPARAM;
    lParam: LPARAM;
    element: IRawElementProviderSimple
  ): LRESULT; stdcall;

var
  _UiaModule: HMODULE = 0;

  _UiaHostProviderFromHwnd: TUiaHostProviderFromHwnd;
  _UiaReturnRawElementProvider: TUiaReturnRawElementProvider;

{ ---------------------------------------------------------------------------
UI Automation constants
--------------------------------------------------------------------------- }

const
UiaRootObjectId = -25;

{ Import }
UIA_E_ELEMENTNOTAVAILABLE = HRESULT($80040201);
UIA_E_INVALIDOPERATION = HRESULT($80131509);

{ Toggle State }
ToggleState_Off      = 0;
ToggleState_On       = 1;
ToggleState_Indeterminate = 2;

{ UIA properties }

UIA_BoundingRectanglePropertyId       = 30001;
UIA_ProcessIdPropertyId               = 30002;
UIA_ControlTypePropertyId             = 30003;
UIA_LocalizedControlTypePropertyId    = 30004;
UIA_NamePropertyId                    = 30005;
UIA_AcceleratorKeyPropertyId          = 30006;
UIA_AccessKeyPropertyId               = 30007;
UIA_HasKeyboardFocusPropertyId        = 30008;
UIA_IsKeyboardFocusablePropertyId     = 30009;
UIA_IsEnabledPropertyId               = 30010;
UIA_AutomationIdPropertyId            = 30011;
UIA_ClassNamePropertyId               = 30012;
UIA_HelpTextPropertyId                = 30013;
UIA_IsControlElementPropertyId        = 30016;
UIA_IsContentElementPropertyId        = 30017;
UIA_LabeledByPropertyId               = 30018;
UIA_NativeWindowHandlePropertyId      = 30020;
UIA_ItemTypePropertyId                = 30021;
UIA_IsOffscreenPropertyId             = 30022;
UIA_OrientationPropertyId             = 30023;
UIA_FrameworkIdPropertyId             = 30024;
UIA_ItemStatusPropertyId              = 30026;

{ UIA patterns }

UIA_InvokePatternId                   = 10000;
UIA_SelectionPatternId                = 10001;
UIA_ValuePatternId                    = 10002;
UIA_RangeValuePatternId               = 10003;
UIA_ScrollPatternId                   = 10004;
UIA_ExpandCollapsePatternId           = 10005;
UIA_GridPatternId                     = 10006;
UIA_GridItemPatternId                 = 10007;
UIA_MultipleViewPatternId             = 10008;
UIA_WindowPatternId                   = 10009;
UIA_SelectionItemPatternId            = 10010;
UIA_DockPatternId                     = 10011;
UIA_TablePatternId                    = 10012;
UIA_TransformPatternId                = 10016;
UIA_TextPatternId                     = 10014;
UIA_TogglePatternId                   = 10015;
UIA_ScrollItemPatternId               = 10013;
UIA_GridPattern2Id                    = 10018;
UIA_StylesPatternId                   = 10025;
UIA_TextPattern2Id                    = 10024;

{ Standard control types }

UIA_ButtonControlTypeId               = 50000;
UIA_CalendarControlTypeId             = 50001;
UIA_CheckBoxControlTypeId             = 50002;
UIA_ComboBoxControlTypeId             = 50003;
UIA_EditControlTypeId                 = 50004;
UIA_HyperlinkControlTypeId            = 50005;
UIA_ImageControlTypeId                = 50006;
UIA_ListItemControlTypeId             = 50007;
UIA_ListControlTypeId                 = 50008;
UIA_MenuControlTypeId                 = 50009;
UIA_MenuBarControlTypeId              = 50010;
UIA_MenuItemControlTypeId             = 50011;
UIA_ProgressBarControlTypeId          = 50012;
UIA_RadioButtonControlTypeId           = 50013;
UIA_ScrollBarControlTypeId            = 50014;
UIA_SliderControlTypeId               = 50015;
UIA_SpinnerControlTypeId              = 50016;
UIA_StatusBarControlTypeId            = 50017;
UIA_TabControlTypeId                  = 50018;
UIA_TabItemControlTypeId              = 50019;
UIA_TextControlTypeId                 = 50020;
UIA_ToolBarControlTypeId              = 50021;
UIA_ToolTipControlTypeId              = 50022;
UIA_TreeControlTypeId                 = 50023;
UIA_TreeItemControlTypeId             = 50024;
UIA_CustomControlTypeId               = 50025;
UIA_GroupControlTypeId                = 50026;
UIA_ThumbControlTypeId                = 50027;
UIA_DataGridControlTypeId             = 50028;
UIA_DataItemControlTypeId             = 50029;
UIA_DocumentControlTypeId             = 50030;
UIA_SplitButtonControlTypeId          = 50031;
UIA_WindowControlTypeId               = 50032;

implementation

function UiaHostProviderFromHwnd(
  hwnd: HWND;
  out provider: IRawElementProviderSimple
): HRESULT;
begin
  if not Assigned(_UiaHostProviderFromHwnd) then
    Exit(E_POINTER);
  provider := nil;

  Result := _UiaHostProviderFromHwnd(hwnd, provider);
end;

function UiaReturnRawElementProvider(
  hwnd: HWND;
  wParam: WPARAM;
  lParam: LPARAM;
  element: IRawElementProviderSimple
): LRESULT;
begin
  if not Assigned(_UiaReturnRawElementProvider) then
    Exit(E_POINTER);
  Result := _UiaReturnRawElementProvider(
    hwnd,
    wParam,
    lParam,
    element
  );
end;

{ ---------------------------------------------------------------------------
TFXAccessibilityProvider
--------------------------------------------------------------------------- }

constructor TFXAccessibilityProvider.Create(
const AControl: IFXAccessibilityControl
);
begin
  inherited Create;

  FControl := AControl;
end;

procedure TFXAccessibilityProvider.Detach;
begin
  FControl := nil;
end;

function TFXAccessibilityProvider.Get_ProviderOptions(
out pRetVal: ProviderOptions
): HRESULT;
begin
  pRetVal := ProviderOptions_ServerSideProvider;

  Result := S_OK;
end;

function TFXAccessibilityProvider.GetPatternProvider(
patternId: Integer;
out pRetVal: IUnknown
): HRESULT;
begin
  pRetVal := nil;

  if not Assigned(FControl) then begin
    Result := UIA_E_ELEMENTNOTAVAILABLE;
    Exit;
  end;

  pRetVal := FControl.AccessibilityGetPattern(patternId);

  Result := S_OK;
end;

function TFXAccessibilityProvider.GetPropertyValue(
propertyId: Integer;
out pRetVal: OleVariant
): HRESULT;
var
R: TRect;
A: OleVariant;
begin
  pRetVal := Unassigned;

  if not Assigned(FControl) then begin
    Result := UIA_E_ELEMENTNOTAVAILABLE;
    Exit;
  end;

  case propertyId of
    UIA_NamePropertyId: pRetVal := FControl.AccessibilityGetName;
    UIA_ControlTypePropertyId: pRetVal := FControl.AccessibilityGetControlType;
    UIA_AutomationIdPropertyId: pRetVal := FControl.AccessibilityGetAutomationId;
    UIA_ClassNamePropertyId: pRetVal := FControl.AccessibilityGetClassName;
    UIA_HelpTextPropertyId: pRetVal := FControl.AccessibilityGetHelp;
    UIA_IsEnabledPropertyId: pRetVal := FControl.AccessibilityIsEnabled;
    UIA_IsKeyboardFocusablePropertyId: pRetVal := FControl.AccessibilityIsKeyboardFocusable;
    UIA_HasKeyboardFocusPropertyId: pRetVal := FControl.AccessibilityHasKeyboardFocus;
    UIA_IsOffscreenPropertyId: pRetVal := FControl.AccessibilityIsOffscreen;
    UIA_IsControlElementPropertyId: pRetVal := FControl.AccessibilityIsControlElement;
    UIA_IsContentElementPropertyId: pRetVal := FControl.AccessibilityIsContentElement;
    UIA_NativeWindowHandlePropertyId: pRetVal := Integer(FControl.AccessibilityGetHandle);
    UIA_ProcessIdPropertyId: pRetVal := GetCurrentProcessId;
    UIA_FrameworkIdPropertyId: pRetVal := 'Win32';
    UIA_LocalizedControlTypePropertyId: pRetVal := FControl.AccessibilityGetControlTypeName;
    UIA_BoundingRectanglePropertyId: begin
      R := FControl.AccessibilityGetBoundingRectangle;

      A := VarArrayCreate([0, 3], varDouble);

      A[0] := R.Left;
      A[1] := R.Top;
      A[2] := R.Right - R.Left;
      A[3] := R.Bottom - R.Top;

      pRetVal := A;
    end;
  end;

  Result := S_OK;
end;

function TFXAccessibilityProvider.Get_HostRawElementProvider(
out pRetVal: IRawElementProviderSimple
): HRESULT;
begin
  pRetVal := nil;

  if not Assigned(FControl) then
  begin
  Result := UIA_E_ELEMENTNOTAVAILABLE;
  Exit;
  end;

  Result := UiaHostProviderFromHwnd(
  FControl.AccessibilityGetHandle,
  pRetVal
  );
end;

{ TFXValueProvider }

constructor TFXValueProvider.Create(
  const AControl: IFXAccessibilityControl
);
begin
  inherited Create;

  FControl := AControl;
end;

function TFXValueProvider.SetValue(
  const Value: PWideChar
): HRESULT;
var
  R: HRESULT;
begin
  if not Assigned(FControl) then
    Exit(UIA_E_ELEMENTNOTAVAILABLE);

  TThread.Synchronize(TThread.Current, procedure begin
    if FControl.AccessibilitySetValue(string(Value)) then
      R := S_OK
    else
      R := UIA_E_INVALIDOPERATION;
  end);
  Result := R;
end;

function TFXValueProvider.Get_Value(
  out pRetVal: WideString
): HRESULT;
begin
  if not Assigned(FControl) then
    Exit(UIA_E_ELEMENTNOTAVAILABLE);

  pRetVal := FControl.AccessibilityGetValue;

  Result := S_OK;
end;

function TFXValueProvider.Get_IsReadOnly(
  out pRetVal: BOOL
): HRESULT;
begin
  if not Assigned(FControl) then
    Exit(UIA_E_ELEMENTNOTAVAILABLE);

  pRetVal := FControl.AccessibilityIsReadOnly;

  Result := S_OK;
end;

{ TFXToggleProvider }

constructor TFXToggleProvider.Create(const AControl: IFXAccessibilityControl);
begin
  inherited Create;

  FControl := AControl;
end;

function TFXToggleProvider.Get_ToggleState(out pRetVal: Integer): HRESULT;
begin
  if not Assigned(FControl) then
    Exit(UIA_E_ELEMENTNOTAVAILABLE);

  pRetVal := FControl.AccessibilityGetToggleState;
  Result := S_OK;
end;

function TFXToggleProvider.Toggle: HRESULT;
var
  R: HRESULT;
begin
  if not Assigned(FControl) then
    Exit(UIA_E_ELEMENTNOTAVAILABLE);

    TThread.Synchronize(TThread.Current, procedure begin
    if FControl.AccessibilityToggle then
      R := S_OK
    else
      R := UIA_E_INVALIDOPERATION;
  end);
  Result := R;
end;




function LoadUIAutomation: Boolean;
begin
  if _UiaModule <> 0 then
    Exit(
      Assigned(_UiaHostProviderFromHwnd) and
      Assigned(_UiaReturnRawElementProvider)
    );

  _UiaModule := LoadLibrary('UIAutomationCore.dll');

  if _UiaModule = 0 then
    Exit(False);

  Pointer(@_UiaHostProviderFromHwnd) :=
    GetProcAddress(_UiaModule, 'UiaHostProviderFromHwnd');

  Pointer(@_UiaReturnRawElementProvider) :=
    GetProcAddress(_UiaModule, 'UiaReturnRawElementProvider');

  if not Assigned(_UiaHostProviderFromHwnd) or
     not Assigned(_UiaReturnRawElementProvider) then
  begin
    FreeLibrary(_UiaModule);
    _UiaModule := 0;

    _UiaHostProviderFromHwnd := nil;
    _UiaReturnRawElementProvider := nil;

    Exit(False);
  end;

  Result := True;
end;

procedure UnloadUIAutomation;
begin
  _UiaHostProviderFromHwnd := nil;
  _UiaReturnRawElementProvider := nil;

  if _UiaModule <> 0 then
  begin
    FreeLibrary(_UiaModule);
    _UiaModule := 0;
  end;
end;


{ TFXInvokeProvider }

constructor TFXInvokeProvider.Create(const AControl: IFXAccessibilityControl);
begin
  inherited Create;
  FControl := AControl;
end;

function TFXInvokeProvider.Invoke: HRESULT;
var
  R: Boolean;
begin
  if not Assigned(FControl) then
    Exit(UIA_E_ELEMENTNOTAVAILABLE);

  TThread.Synchronize(TThread.Current,
    procedure
    begin
      R := FControl.AccessibilityInvoke;
    end);

  if R then
    Result := S_OK
  else
    Result := UIA_E_INVALIDOPERATION;
end;

{ TFXSelectionProvider }

constructor TFXSelectionProvider.Create(
  const AControl: IFXAccessibilityControl);
begin
  inherited Create;
  FControl := AControl;
end;

function TFXSelectionProvider.Get_CanSelectMultiple(
  out pRetVal: BOOL): HRESULT;
begin
  if not Assigned(FControl) then
    Exit(UIA_E_ELEMENTNOTAVAILABLE);

  pRetVal := False;
  Result := S_OK;
end;

function TFXSelectionProvider.Get_IsSelectionRequired(
  out pRetVal: BOOL): HRESULT;
begin
  if not Assigned(FControl) then
    Exit(UIA_E_ELEMENTNOTAVAILABLE);

  pRetVal := FControl.AccessibilityIsSelectionRequired;
  Result := S_OK;
end;

function TFXSelectionProvider.Get_Selection(
  out pRetVal: IInterface): HRESULT;
var
  Index: Integer;
begin
  pRetVal := nil;

  if not Assigned(FControl) then
    Exit(UIA_E_ELEMENTNOTAVAILABLE);

  Index := FControl.AccessibilityGetSelectedIndex;

  if Index >= 0 then
    pRetVal := FControl.AccessibilityGetSelectionItem(Index);

  Result := S_OK;
end;

{ TFXSelectionItemProvider }

constructor TFXSelectionItemProvider.Create(
  const AControl: IFXAccessibilityControl;
  AIndex: Integer);
begin
  inherited Create;
  FControl := AControl;
  FIndex := AIndex;
end;

function TFXSelectionItemProvider.Get_IsSelected(
  out pRetVal: BOOL): HRESULT;
begin
  if not Assigned(FControl) then
    Exit(UIA_E_ELEMENTNOTAVAILABLE);

  pRetVal := FControl.AccessibilityGetSelectedIndex = FIndex;
  Result := S_OK;
end;

function TFXSelectionItemProvider.Select: HRESULT;
var
  R: Boolean;
begin
  if not Assigned(FControl) then
    Exit(UIA_E_ELEMENTNOTAVAILABLE);

  TThread.Synchronize(TThread.Current,
    procedure
    begin
      R := FControl.AccessibilitySelectIndex(FIndex);
    end);

  if R then
    Result := S_OK
  else
    Result := UIA_E_INVALIDOPERATION;
end;

function TFXSelectionItemProvider.AddToSelection: HRESULT;
begin
  // This selector only supports a single selection.
  Result := UIA_E_INVALIDOPERATION;
end;

function TFXSelectionItemProvider.RemoveFromSelection: HRESULT;
begin
  // A single-selection selector cannot remove its only selection
  // when selection is required.
  Result := UIA_E_INVALIDOPERATION;
end;

function TFXSelectionItemProvider.Get_SelectionContainer(
  out pRetVal: IRawElementProviderSimple): HRESULT;
begin
  pRetVal := nil;

  if not Assigned(FControl) then
    Exit(UIA_E_ELEMENTNOTAVAILABLE);

  pRetVal := FControl.AccessibilityGetSelectionContainer;

  if Assigned(pRetVal) then
    Result := S_OK
  else
    Result := UIA_E_ELEMENTNOTAVAILABLE;
end;

{ TFXRangeValueProvider }

constructor TFXRangeValueProvider.Create(
  const AControl: IFXAccessibilityControl);
begin
  inherited Create;
  FControl := AControl;
end;

function TFXRangeValueProvider.SetValue(Value: Double): HRESULT;
var
  R: Boolean;
begin
  if not Assigned(FControl) then
    Exit(UIA_E_ELEMENTNOTAVAILABLE);

  TThread.Synchronize(TThread.Current,
    procedure
    begin
      R := FControl.AccessibilitySetRangeValue(Value);
    end);

  if R then
    Result := S_OK
  else
    Result := UIA_E_INVALIDOPERATION;
end;

function TFXRangeValueProvider.Get_IsReadOnly(
  out pRetVal: BOOL): HRESULT;
begin
  if not Assigned(FControl) then
    Exit(UIA_E_ELEMENTNOTAVAILABLE);

  pRetVal := FControl.AccessibilityIsReadOnly;
  Result := S_OK;
end;

function TFXRangeValueProvider.Get_Maximum(
  out pRetVal: Double): HRESULT;
begin
  if not Assigned(FControl) then
    Exit(UIA_E_ELEMENTNOTAVAILABLE);

  pRetVal := FControl.AccessibilityGetRangeMaximum;
  Result := S_OK;
end;

function TFXRangeValueProvider.Get_Minimum(
  out pRetVal: Double): HRESULT;
begin
  if not Assigned(FControl) then
    Exit(UIA_E_ELEMENTNOTAVAILABLE);

  pRetVal := FControl.AccessibilityGetRangeMinimum;
  Result := S_OK;
end;

function TFXRangeValueProvider.Get_LargeChange(
  out pRetVal: Double): HRESULT;
begin
  if not Assigned(FControl) then
    Exit(UIA_E_ELEMENTNOTAVAILABLE);

  pRetVal := FControl.AccessibilityGetRangeLargeChange;
  Result := S_OK;
end;

function TFXRangeValueProvider.Get_SmallChange(
  out pRetVal: Double): HRESULT;
begin
  if not Assigned(FControl) then
    Exit(UIA_E_ELEMENTNOTAVAILABLE);

  pRetVal := FControl.AccessibilityGetRangeSmallChange;
  Result := S_OK;
end;

function TFXRangeValueProvider.Get_Value(
  out pRetVal: Double): HRESULT;
begin
  if not Assigned(FControl) then
    Exit(UIA_E_ELEMENTNOTAVAILABLE);

  pRetVal := FControl.AccessibilityGetRangeValue;
  Result := S_OK;
end;

initialization
  LoadUIAutomation;
finalization
  UnloadUIAutomation;
end.

