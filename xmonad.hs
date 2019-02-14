-- imports {{{1
import XMonad
import XMonad.Hooks.DynamicLog 
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Named
import XMonad.Layout.Maximize
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Layout.TwoPane
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run(spawnPipe)

import Control.Monad (liftM2)
import Data.List(elemIndex)
import System.IO

import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8
import qualified XMonad.StackSet as W

--- colors {{{1
black   = "#282828"
red     = "#cc241d"
green   = "#98971a"
yellow  = "#d79921"
blue    = "#458588"
magenta = "#b16286"
cyan    = "#689d6a"
white   = "#a89984"

bg      = "#262626"
bg2     = "#4e4e4e"

-- bright colors
lblack   = "#928374"
lred     = "#fb4934"
lgreen   = "#b8bb26"
lyellow  = "#fabd2f"
lblue    = "#83a598"
lmagenta = "#d3869b"
lcyan    = "#8ec07c"
lwhite   = "#ebdbb2"

--- layout {{{1
renameLayout name = ("%{A1:xdotool key super+space:}%{u"++yellow++"} " ++ name ++ " %{-u}%{A-}")
nameClick name = named $ renameLayout name

myLayout = avoidStruts $
    tiled ||| three ||| two ||| full 
    where 
        three = nameClick "|||" $ maximizeWithPadding 2 $ gaps $ ThreeColMid nmaster delta (5/12)
        two   = nameClick "[]|" $ maximizeWithPadding 2 $ gaps $ TwoPane delta ratio
        tiled = nameClick "[]=" $ maximizeWithPadding 2 $ gaps $ Tall nmaster delta ratio
        full  = nameClick "[ ]" $ Full
        nmaster = 1
        ratio = 2/3
        delta = 3/100
        gaps = spacingRaw True (Border 0 0 0 0) False (Border 5 5 5 5) True

myScratchpads = 
    [ NS "spotify" "spotify" (className =? "Spotify") defaultFloating
    , NS "htop" "st -e htop" (title =? "htop") defaultFloating
    , NS "arandr" "arandr" (className =? "Arandr") defaultFloating
    ]

myManageHook = composeAll
   [ className =? "Xmessage"        --> doFloat
   , className =? "Blueman-manager" --> doFloat
   , className =? "Arandr"          --> doFloat
   , className =? "Spotify"         --> doFloat  -- TODO this doesn't work
   , className =? "Slack"           --> viewShift "7"
   , className =? "TelegramDesktop" --> viewShift "7"
   , manageDocks
   ]
   where 
     viewShift = doF . liftM2 (.) W.greedyView W.shift
          
--- statusbar {{{1
icon "1" = "\xf120"
icon "2" = "\xf268"
icon "3" = "\xf16c"
icon "4" = "\xf121"
icon "5" = "\xf1fe"
icon "6" = "\xf085"
icon "7" = "\xf075"
icon "8" = "\xf17c"
icon "9" = "\xf02d"
icon n = n

format foreground background line "NSP" = ""
format foreground background line ws = wrap (click ++ ln ++ bg ++ fg ++ padding) (padding ++ close) $ icon ws
    where
        click   = "%{A1:xdotool key super+" ++ ws ++ ":}"
        ln      = "%{u" ++ line ++ "}"
        bg      = "%{B" ++ background ++ "}"
        fg      = "%{F" ++ foreground ++ "}"
        close   = "%{-u}%{B- F- A-}"
        padding = "    "

--- shortcuts {{{1
dmenu_settings = " -nb '" ++ bg ++ "' -nf '" ++ white ++ "' -sb '" ++ blue ++ "' -sf '" ++ lwhite ++ "' -fn terminus-20:normal"

myKeys = 
    [ ("M4-M1-l"                    , spawn "slock") -- lock screen
    , ("M-d"                        , spawn ("dmenu_run" ++ dmenu_settings))
    , ("M-S-d"                      , spawn ("j4-dmenu-desktop --term=/usr/local/bin/st --dmenu=\"dmenu -i " ++ dmenu_settings ++ "\""))
    , ("M-g"                        , spawn "chromium --profile-directory=Default")
    , ("M-y"                        , spawn "chromium --profile-directory=Default --app-id=adnlfjpnmidfimlkaohpidplnoimahfh") -- youtube
    , ("M-p"                        , spawn "chromium --profile-directory=Default --app-id=amfkemaodmghlnknncknfhcmmiclmbpa") -- plex
    , ("M-S-m"                      , spawn ("$HOME/.xmonad/chscreen.sh " ++ dmenu_settings))

    , ("<XF86AudioPlay>"            , spawn "playerctl play-pause")
    , ("<XF86AudioStop>"            , spawn "playerctl stop")
    , ("<XF86AudioNext>"            , spawn "playerctl next")
    , ("<XF86AudioPrev>"            , spawn "playerctl previous")
    , ("<XF86MonBrightnessUp>"      , spawn "light -A 5")
    , ("<XF86MonBrightnessDown>"    , spawn "light -U 5")
    , ("M-b"                        , sendMessage $ ToggleStrut U) --- show/hide polybar
    , ("M-<Return>"                 , spawn "/usr/local/bin/st")
    , ("M-S-<Return>"               , windows W.swapMaster)

    , ("M-S-a"                      , namedScratchpadAction myScratchpads "arandr")
    , ("M-s"                        , namedScratchpadAction myScratchpads "spotify")
    , ("M-f"                        , withFocused (sendMessage . maximizeRestore))
    ]

--- logging {{{1
myLogHook dbus = def 
    { ppOutput  = dbusOutput dbus
    , ppCurrent = format lwhite bg2 blue
    , ppVisible = format lwhite bg2 white
    , ppUrgent  = format red white red
    , ppHidden  = format white bg bg2
    , ppHiddenNoWindows = format white bg bg
    , ppWsSep   = " "
    , ppSep     = "  "
    , ppTitle   = take 50
    }
  

-- Emit a DBus signal on log updates
dbusOutput dbus str = do
    let signal = (D.signal objectPath interfaceName memberName) {
            D.signalBody = [D.toVariant $ UTF8.decodeString str]
        }
    D.emit dbus signal
  where
    objectPath = D.objectPath_ "/org/xmonad/Log"
    interfaceName = D.interfaceName_ "org.xmonad.Log"
    memberName = D.memberName_ "Update"

--- main {{{1
modm = mod4Mask

main = do
    dbus <- D.connectSession

    D.requestName dbus (D.busName_ "org.xmonad.Log")
        [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]


    xmonad $ docks $ def
        { manageHook = myManageHook <+> manageHook def
        , layoutHook = myLayout
        , logHook = dynamicLogWithPP (myLogHook dbus)
        , modMask = modm     -- Rebind Mod to the Windows key
        , terminal = "st"
        , normalBorderColor = bg2
        , focusedBorderColor = blue
        } `additionalKeysP` myKeys

-- vim: fdm=marker fdc=2 fcs=fold\:\ :
