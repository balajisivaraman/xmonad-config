import qualified Codec.Binary.UTF8.String      as UTF8
import qualified DBus                          as D
import qualified DBus.Client                   as D
import           Data.List                      ( isInfixOf )
import           Data.Map                       ( Map
                                                , union
                                                )
import           System.Exit                    ( exitSuccess )
import           XMonad
import           XMonad.Config.Desktop
import qualified XMonad.Core                   as XMonad
import           XMonad.Hooks.EwmhDesktops      ( ewmh
                                                , ewmhFullscreen
                                                )
import           XMonad.Hooks.ManageDocks       ( avoidStruts
                                                , docks
                                                )
import           XMonad.Hooks.StatusBar.PP
import           XMonad.Layout.NoBorders        ( noBorders )
import           XMonad.Layout.NoFrillsDecoration
import           XMonad.Layout.Spacing
import           XMonad.Prompt
import           XMonad.Prompt.XMonad
import qualified XMonad.StackSet               as W
import           XMonad.Util.EZConfig           ( mkKeymap )
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Run                ( hPutStrLn
                                                , spawnPipe
                                                )
import           XMonad.Util.SpawnOnce

myTerminal = "alacritty"
myBrowser = "firefox"
myAppLauncher =
  "rofi -matching fuzzy -modi combi -show combi -combi-modi drun,window"
myDocumentLauncher = "document-launcher"
myClipboardManager = "clipcat-menu"

-- Gruvbox Dark Hard Theme
bg = "#282828"
bg0H = "#1d2021"
bg0S = "#32302f"
bg0 = "#282828"
bg1 = "#3c3836"
bg2 = "#504945"
bg3 = "#665c54"
bg4 = "#7c6f64"
red = "#cc241d"
redAlt = "#fb4934"
green = "#98971a"
greenAlt = "#b8bb26"
yellow = "#d79921"
yellowAlt = "#fabd2f"
blue = "#458588"
blueAlt = "#83a598"
purple = "#b16286"
purpleAlt = "#d3869b"
orange = "#689d6a"
orangeAlt = "#8ec07c"
gray = "#a89984"
grayAlt = "#928374"
fg = "#ebdbb2"
fg0 = "#fbf1c7"
fg1 = "#ebdbb2"
fg2 = "#d5c4a1"
fg3 = "#bdae93"
fg4 = "#a89984"

myFont = "xft:TerminessTTF NerdFont:pixelsize=14"

topBar = def { fontName            = myFont
             , activeColor         = blue
             , activeBorderColor   = blue
             , activeTextColor     = blue
             , inactiveColor       = bg1
             , inactiveBorderColor = bg1
             , inactiveTextColor   = bg1
             , urgentBorderColor   = red
             , urgentTextColor     = red
             , decoHeight          = 10
             }

myPromptTheme = def { font                 = myFont
                    , bgColor              = bg
                    , fgColor              = fg
                    , fgHLight             = fg
                    , bgHLight             = bg
                    , borderColor          = bg
                    , promptBorderWidth    = 0
                    , height               = 20
                    , position             = Top
                    , autoComplete         = Just 1
                    , complCaseSensitivity = CaseInSensitive
                    }

warmPromptTheme =
  myPromptTheme { bgColor = yellow, fgColor = bg0H, position = Top }

addTopBar = noFrillsDeco shrinkText topBar

mySpacing = spacingRaw True (Border 0 10 10 10) True (Border 10 10 10 10) True

myLayoutHook =
  addTopBar
    $ avoidStruts
    -- Needed to ensure XMobar stays on top
    $ mySpacing
     -- Setup 10 spacing as above
    $ noBorders
    -- Don't want borders
    $ layoutHook desktopConfig

myLogHook :: D.Client -> PP
myLogHook dbus = filterOutWsPP [scratchpadWorkspaceTag] $ def
  { ppOutput  = dbusOutput dbus
  , ppCurrent = wrap ("%{B" ++ bg2 ++ "} ") " %{B-}"
  , ppVisible = wrap ("%{B" ++ bg1 ++ "} ") " %{B-}"
  , ppUrgent  = wrap ("%{F" ++ red ++ "} ") " %{F-}"
  , ppHidden  = wrap " " " "
  , ppWsSep   = ""
  , ppSep     = " : "
  , ppTitle   = const ""
  }

myStartupHook = do
  spawnOnce "launch_polybar.sh"
  spawnOnce
    "xss-lock --transfer-sleep-lock -- i3lockr --brighten 30 --blur 25 -- --nofork --ignore-empty-password"
  spawnOnce "xsetroot -cursor_name left_ptr"

-- Emit a DBus signal on log updates
dbusOutput :: D.Client -> String -> IO ()
dbusOutput dbus str = do
  let signal = (D.signal objectPath interfaceName memberName)
        { D.signalBody = [D.toVariant $ UTF8.decodeString str]
        }
  D.emit dbus signal
 where
  objectPath    = D.objectPath_ "/org/xmonad/Log"
  interfaceName = D.interfaceName_ "org.xmonad.Log"
  memberName    = D.memberName_ "Update"

systemPromptCmds =
  [ ("Shutdown", spawn "shutdown -h now")
  , ("Reboot"  , spawn "systemctl reboot")
  , ("Exit"    , io exitSuccess)
  , ( "Lock"
    , spawn
      "i3lockr --brighten 30 --blur 25 -- --nofork --ignore-empty-password ; mode \"default\""
    )
  ]

-- RationalRect computations based on screen resolution of 3440x1440
myScratchpads =
  [ NS "keepassxc"
       "keepassxc"
       (className =? "KeePassXC")
       (customFloating (W.RationalRect 0.17 0.14 0.29 0.45))
  -- , NS "zotero"
  --      "zotero"
  --      (className =? "Zotero")
  --      (customFloating (W.RationalRect 0.17 0.10 0.52 0.69))
  , NS "yubikey"
       "yubioath-desktop"
       (className =? "Yubico Authenticator")
       (customFloating (W.RationalRect 0.38 0.28 0.24 0.42))
  ]

myManageHook =
  composeAll
      [ className =? "pinentry" --> doFloat
      , (className =? "Nightly" <&&> title =? "Library") --> doFloat
      , (className =? "Emacs" <&&> title =? "Ediff") --> doFloat
      , (className =? "KeePassXC") --> doFloat
      ]
    <+> namedScratchpadManageHook myScratchpads
    <+> manageHook desktopConfig

myWorkspaces = ["WRK", "GEN"]

myKeys :: XConfig l -> Map (KeyMask, KeySym) (X ())
myKeys c = mkKeymap
  c
  [   -----------------------------------------------------------
      -- Launchers
      -----------------------------------------------------------
    ("M-b"          , spawn myBrowser)
  , ("M-d"          , spawn myDocumentLauncher)
  , ("M-y"          , spawn myClipboardManager)
  , ("M-<Space>"    , spawn myAppLauncher)
  , ("M-<Return>"   , spawn myTerminal)
  ,
      -----------------------------------------------------------
      -- Layouts
      -----------------------------------------------------------
    ("M-<Tab>"      , sendMessage NextLayout)
  ,
      -----------------------------------------------------------
      -- Navigation
      -----------------------------------------------------------
    ("M-j"          , windows W.focusDown)
  , ("M-k"          , windows W.focusUp)
  ,
      -----------------------------------------------------------
      -- Scratchpads
      -----------------------------------------------------------
    ("M-p", namedScratchpadAction myScratchpads "keepassxc")
  , ("M-z", namedScratchpadAction myScratchpads "zotero")
  , ("M-o", namedScratchpadAction myScratchpads "yubikey")
  ,
      -----------------------------------------------------------
      -- System/Utilities
      -----------------------------------------------------------
    ("M-<Backspace>", kill)
  , ("M-S-r", spawn "xmonad --recompile && xmonad --restart")
  , ( "M-S-o"
    , spawn
      "i3lockr --brighten 30 --blur 25 -- --nofork --ignore-empty-password ; mode \"default\""
    )
  , ("M-x"  , xmonadPromptC systemPromptCmds warmPromptTheme)
  ,
      -----------------------------------------------------------
      -- Volume Control
      -----------------------------------------------------------
    ("M-S-.", spawn "volume-control inc 5")
  , ("M-S-,", spawn "volume-control dec 5")
  , ("M-S-m", spawn "volume-control mute")
  ,
      -----------------------------------------------------------
      -- Window Operations
      -----------------------------------------------------------
    ("M-h"  , sendMessage Shrink)
  , ("M-l"  , sendMessage Expand)
  , ("M-S-j", windows W.swapDown)
  , ("M-S-k", windows W.swapUp)
  ]

main = do
  dbus <- D.connectSession
  D.requestName
    dbus
    (D.busName_ "org.xmonad.Log")
    [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
  xmonad
    -- Needed to ensure stuff like XMobar stays on top
    $ docks
    -- Advertises EWMH support
    $ ewmh
    -- Enables fullscreen EWMH support
    $ ewmhFullscreen
    $ myConfig dbus

myConfig p = desktopConfig { borderWidth       = 0
                           , focusFollowsMouse = False
                           , keys = \c -> myKeys c `union` keys XMonad.def c
                           , layoutHook        = myLayoutHook
                           , logHook           = dynamicLogWithPP (myLogHook p)
                           , manageHook        = myManageHook
                           , modMask           = mod4Mask
                           , workspaces        = myWorkspaces
                           , startupHook       = myStartupHook
                           }
