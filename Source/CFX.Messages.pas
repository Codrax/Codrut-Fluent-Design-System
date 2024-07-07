unit CFX.Messages;

interface
  uses
    Winapi.Windows, System.Classes, System.Types, Winapi.Messages;

  const
    // Separetr from User Messages
    CFX_MESSAGE_OFFSET = $200; // these are sent in controls AND sub-controls
    WM_CFX_MESSAGES = WM_USER + CFX_MESSAGE_OFFSET;
    WM_CFX_MESSAGES_END = WM_CFX_MESSAGES+500;

    WM_WINDOW_MOVE = WM_CFX_MESSAGES + 1;
    WM_WINDOW_RESIZE = WM_CFX_MESSAGES + 2;
    WM_WINDOW_DEF = WM_CFX_MESSAGES + 3;

implementation

end.
