import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Named
import XMonad.Layout.Spacing
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)
import System.IO
import XMonad.Layout.ThreeColumns
import Data.List(elemIndex)

import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8
import qualified XMonad.StackSet as W

modm = mod4Mask


--- colors
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

--- bright colors
lblack   = "#928374"
lred     = "#fb4934"
lgreen   = "#b8bb26"
lyellow  = "#fabd2f"
lblue    = "#83a598"
lmagenta = "#d3869b"
lcyan    = "#8ec07c"
lwhite   = "#ebdbb2"


main = do
    dbus <- D.connectSession

    D.requestName dbus (D.busName_ "org.xmonad.Log")
        [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]


    xmonad $ docks $ def
        { manageHook = manageDocks <+> manageHook def
        , layoutHook = myLayout
        , logHook = dynamicLogWithPP (myLogHook dbus)
        , modMask = modm     -- Rebind Mod to the Windows key
        , terminal = "st"
        , normalBorderColor = bg2
        , focusedBorderColor = blue
        } `additionalKeysP` myKeys

nameClick name = named ("%{A1:xdotool key super+space:}%{u"++yellow++"} " ++ name ++ " %{-u}%{A-}")

myLayout = avoidStruts $
    tiled ||| three ||| full 
    where 
        three = nameClick "|||" $ gaps $ ThreeColMid nmaster delta (5/12)
        tiled = nameClick "[]=" $ gaps $ Tall nmaster delta ratio
        full  = nameClick "[ ]" $ Full
        nmaster = 1
        ratio = 2/3
        delta = 3/100
        gaps = spacingRaw True (Border 0 0 0 0) False (Border 5 5 5 5) True
          
icon "1" = "\xf120"
icon "2" = "\xf268"
icon "3" = "\xf16c"
icon "4" = "\xf121"
icon "5" = "\xf1fe"
icon "6" = "\xf085"
icon "7" = "\xf075"
icon "8" = "\xf17c"
icon "9" = "\xf02d"

myKeys = 
    [ ("M4-M1-l",         spawn "slock") -- lock screen
    , ("M-d",             spawn "dmenu_run")
    , ("M-S-d",           spawn "j4-dmenu-desktop --term=/usr/local/bin/st")
    , ("M-g",             spawn "chromium --profile-directory=Default")
    , ("M-y",             spawn "chromium --profile-directory=Default --app-id=adnlfjpnmidfimlkaohpidplnoimahfh") -- youtube
    , ("M-p",             spawn "chromium --profile-directory=Default --app-id=amfkemaodmghlnknncknfhcmmiclmbpa") -- plex
    , ("M-S-m",           spawn "$HOME/.xmonad/chscreen.sh") -- plex
    , ("M-S-a",           spawn "arandr")
    , ("<XF86AudioPlay>", spawn "playerctl play-pause")
    , ("<XF86AudioStop>", spawn "playerctl stop")
    , ("<XF86AudioNext>", spawn "playerctl next")
    , ("<XF86AudioPrev>", spawn "playerctl previous")
    , ("M-b",             sendMessage $ ToggleStrut U)
    , ("M-<Return>",        spawn "/usr/local/bin/st")
    , ("M-S-<Return>",      windows W.swapMaster)
    ]

myLogHook dbus = def 
    { ppOutput  = dbusOutput dbus
    , ppCurrent = format lwhite bg2 blue
    , ppVisible = format lwhite bg2 white
    , ppUrgent  = format red white red
    , ppHidden  = format white bg bg2
    , ppHiddenNoWindows = format white bg bg
    , ppWsSep   = " "
    , ppSep     = "  "
    }

   
format foreground background line ws = wrap (click ++ ln ++ bg ++ fg ++ padding) (padding ++ close) $ icon ws
    where
        click   = "%{A1:xdotool key super+" ++ ws ++ ":}"
        ln      = "%{u" ++ line ++ "}"
        bg      = "%{B" ++ background ++ "}"
        fg      = "%{F" ++ foreground ++ "}"
        close   = "%{-u}%{B- F- A-}"
        padding = "    "

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
