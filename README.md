# Codrut-Fluent-Design-System
Codrut Fluent Design System is a suite of components made to mimic the Interface of Windows 11, the Fluent Design System, This is not to be confused with CodrutsVisualLibrary, as that is ajust a suite of independent components

## Notice: Under Construction!!
The component package is not at a state where it can be considered finished without encountering bugs. This package has advanced to Its **Alpha** Stage.

## Components
- Minimise Panel, a Panel that is able to minimise itself with a optional animation, works best with DoubleBuffered and for more panels, use Align.alTop
- Panel, a TPanel component with inproved features and the ability to sync to the System Color Theme
- Button, a powerfull button in the Fluent Design style, customizable with custom colors, shapes, button types, long presses, 2-states, checkable, dropdown, and more. It can also be aligned Vertically and Horizontalle, and the Icon as well
- ButtonDesign, a very customizable and advanced button from CodrutsVisualLibrary with inproved features, can be styled multiple ways, icon support, gradient, accent color sync, align, subtext, automatic font sizing and more
- Radio Button, a radio button component with multiple aligmnets
- Scrollbar, a simple scrollbar that minimised itself to a line when not in use
- Scrollbox, a modern scrollbox that uses the modern scrollbars
- Selector, a component which allows to select between multiple options with a animation when switching
- Checkbox, a animated checkbox that supports 3 states
- Edit Box, a flue edit box with vertical alignment support
- Standard icon, a drawable TGraphicControl icon component that has multiple icons
- IconV iew, a simple icon view control that relies on the FXIcon class for drawing & storing. Very lightweight
- Slider, a slider with tick support, hint previews and more features
- Blur Material, a acrilic blur box that can be drawn from the wallaper or a screenshot of the screen. Great for Windows Fluent Design System like apps
- Popup Menu, it supports (FXIconSelect) icons and can have multiple sub-menus, It used a acrylic effect for the background with a accent colored border. It also has Checkable items, Radio Items, separators and as mentioned before, It can go infinite layers deep
- Text Box, a simple Label-Like component with more features. It also has a few components dependent on the base class
- Animated Text Box, a label box with the ability to animate from a TStringList of values
- Scroll Text, a box with scrolling text. The fade size, fade width and animation speed can be adjusted
- Progress Bar, a simple animated progress bar with 4 different styles
- ImageList, a work-in-progress component that can hold images
- Simple shapes, such as Square, Circle, Triangle and more planned in the future
- Linear Draw List, a list with a number of elements that are all drawn in a Notify Event given, the layouts are calculated automatically and you can provide a custom Content Justification and orientation.
- Linear Control List, a linear draw list that accepts CFX controls, similar to a TControlList but more customizable in terms of layout and custom drawing.
- Effects, you can overlay effects onto controls. Effects can be Blur, Monocrome, Invert, Deepfry, Color, Zoom and more!
- Layout, which is a component container for storing other controls inside
- Scrollable Layout, is another layout-based control but with two scrollbars for scrolling the control.

## Apps made with CodrutFluentDesignSystem
 - Codrut Printing - https://github.com/Codrax/Codrut-Printing
 - File Sync Manager - (work in progress)

## Creating a CFX Visual Application
1) Create a new VCL Application in Delphi
<img src="https://github.com/user-attachments/assets/56ffe547-242d-49c9-a9b0-daa59dff726d" width="300">

2) Include all required Unit Files
The required unit files are:
`CFX.Forms`, `CFX.Types`, `CFX.ThemeManager`, `CFX.Colors` and `CFX.Classes`

3) Change Main Form class to FXForm
<img src="https://github.com/user-attachments/assets/f5e8e3a3-f6dc-4beb-91ca-7bfb9b9decc4" width="300">

4) Place some components
You can add any components from the Palette. They are grouped under `CFX Main`, `CFX.Shapes`, `CFX Animations` and `CFX Utils`.

5) Adding the Application Manager (`FXAppManager`) from `CFX Utils`
The App Manager is an optional feature for your application that automatically creates a AppData directory for your application and saves the location on screen where the form was closed, and re-loads that on startup. It can also start an automatic update check for you sending a POST request to the APIEndpoint provided provided under the following format:
```
{
 "mode":"getversion",
 "app":"app-api-name"
}
```
And It expects a result in the format of
```
{
 "version":"1.0.0",
 "updateurl":"https://server.com/download-file.exe" // optional
{
```

7) Include additional units (optional)
Some of the most usefull units to use are:
- `CFX.Dialogs`, For dialogs and the classes for each type
- `CFX.QuickDialogs`, For executing a quick dialog, with procedures as `OpenMessage()` or `OpenDialog()` or `OpenInput()`
- `CFX.FormClasses`, This unit contains all classes for Full-Screen dialogs. Such as confirmations, status

## Image Gallery
> The following images are ALPHA versions of the component suite, the components are expected to change in the future.

![Screenshot 2023-09-25 210750](https://github.com/Codrax/CodrutFluentDesignSystem/assets/68193064/7df7f666-a793-4b96-bb77-b3ab9a0fe7c0)
![Screenshot 2023-07-25 103550](https://github.com/Codrax/CodrutFluentDesignSystem/assets/68193064/d5245fdc-d226-40df-8d70-424012c3326c)
![ezgif com-video-to-gif(1)](https://github.com/Codrax/CodrutFluentDesignSystem/assets/68193064/8a3b3378-2c76-4baf-a1c2-84fa1748dc93)
![Screenshot 2023-06-29 185725](https://github.com/Codrax/CodrutFluentDesignSystem/assets/68193064/24959e8c-b207-4d24-9bc2-3a46a6e8708b)
![ezgif com-optimize](https://github.com/Codrax/CodrutFluentDesignSystem/assets/68193064/43419ec6-e583-455d-b113-34f49d9137d9)
![Screenshot_7](https://user-images.githubusercontent.com/68193064/215814322-41a0e245-af55-4e97-aaf2-75e81d25dd17.png)
![Screenshot 2023-04-07 200816](https://user-images.githubusercontent.com/68193064/230649040-7c1ccc50-8d72-46b7-afca-d07b734f2112.png)
![Screenshot 2023-06-27 213121](https://github.com/Codrax/CodrutFluentDesignSystem/assets/68193064/786e1e3f-8c57-405a-8abd-173887aa9b06)
![Screenshot 2023-10-04 201114](https://github.com/Codrax/Codrut-Fluent-Design-System/assets/68193064/c05d8eb2-ba8e-4070-ab2f-6aeadd558079)
![Screenshot 2023-06-27 212832](https://github.com/Codrax/CodrutFluentDesignSystem/assets/68193064/e54efb73-4f1f-4236-a632-6cbc1fd07664)

