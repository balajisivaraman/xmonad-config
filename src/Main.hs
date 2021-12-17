import qualified Codec.Binary.UTF8.String      as UTF8
import           Data.List                      ( isInfixOf )
import           Data.Map                       ( Map
                                                , union
                                                )
import           System.Exit                    ( exitSuccess )
import           XMonad
import           XMonad.Actions.WindowNavigation
                                                ( withWindowNavigation )
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
import           XMonad.Layout.Renamed
import           XMonad.Layout.Spacing
import           XMonad.Layout.ThreeColumns
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

-- Modus Colors
bg = "#000000"
fg = "#ffffff"
bgWarm = "#382f27"
fgWarm = "#f8dec0"
red = "#ff8059"
blue = "#2fafff"
bgInactive = "#1e1e1e"
fgInactive = "#bfc0c4"

myFont = "xft:TerminessTTF NerdFont:pixelsize=14"

topBar = def { fontName            = myFont
             , activeColor         = blue
             , activeBorderColor   = blue
             , activeTextColor     = blue
             , inactiveColor       = bgInactive
             , inactiveBorderColor = bgInactive
             , inactiveTextColor   = bgInactive
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
  myPromptTheme { bgColor = bgWarm, fgColor = fgWarm, position = Top }

addTopBar = noFrillsDeco shrinkText topBar

mySpacing = spacingRaw True (Border 0 10 10 10) True (Border 10 10 10 10) True

myLayoutHook =
  named "Standard"
    $ addTopBar
    -- Needed to ensure XMobar stays on top
    $ avoidStruts
    -- Setup 10 spacing as above
    $ mySpacing
    -- Don't want borders
    $ noBorders
    $ ThreeColMid 1 (1 / 10) 0.45
  where named n = renamed [XMonad.Layout.Renamed.Replace n]

myStatusBar = "xmobar -x1"
myLogHook h = do
  filterOutWsPP [scratchpadWorkspaceTag] $ def { ppOutput = hPutStrLn h }

myStartupHook = do
  spawnOnce "stalonetray"
  spawnOnce
    "xss-lock --transfer-sleep-lock -- i3lockr --brighten 30 --blur 25 -- --nofork --ignore-empty-password"
  spawnOnce "xsetroot -cursor_name left_ptr"

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
    ("M-["  , sendMessage Shrink)
  , ("M-]"  , sendMessage Expand)
  ]

main = do
  xmproc <- spawnPipe myStatusBar
  config <-
    withWindowNavigation (xK_l, xK_j, xK_k, xK_semicolon)
    -- Needed to ensure stuff like XMobar stays on top
    $ docks
    -- Advertises EWMH support
    $ ewmh
    -- Enables fullscreen EWMH support
    $ ewmhFullscreen
    $ myConfig xmproc
  xmonad config

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
