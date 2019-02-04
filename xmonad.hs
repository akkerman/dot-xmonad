import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
import XMonad.Layout.ThreeColumns

import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

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


    xmonad $ docks $ defaultConfig
        { manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = myLayout
        , logHook = dynamicLogWithPP (myLogHook dbus)
        , modMask = modm     -- Rebind Mod to the Windows key
        , terminal = "st"
        } `additionalKeys` myKeys

myLayout =
    avoidStruts $ tiled ||| three ||| Full 
    where 
        three = ThreeColMid nmaster delta (5/12)
        tiled = Tall nmaster delta ratio
        nmaster = 1
        ratio = 2/3
        delta = 3/100
          
myKeys = 
    [ ((mod4Mask .|. mod1Mask, xK_l), spawn "slock") -- lock screen
    , ((modm,               xK_d), spawn "dmenu_run")
    , ((modm .|. shiftMask, xK_d), spawn "j4-dmenu-desktop --term=/usr/local/bin/st")

    , ((modm,               xK_g), spawn "chromium --profile-directory=Default")
    , ((modm,               xK_y), spawn "chromium --profile-directory=Default --app-id=adnlfjpnmidfimlkaohpidplnoimahfh") -- youtube
    , ((modm,               xK_p), spawn "chromium --profile-directory=Default --app-id=amfkemaodmghlnknncknfhcmmiclmbpa") -- plex
    , ((modm .|. shiftMask, xK_m), spawn "$HOME/.xmonad/chscreen.sh") -- plex


    , ((modm .|. shiftMask, xK_a), spawn "arandr")
    ]

myLogHook dbus = def 
    { ppOutput  = dbusOutput dbus
    , ppCurrent = format lwhite bg2 blue . icon
    , ppVisible = format white bg2 green . icon
    , ppUrgent  = format red white red . icon
    , ppHidden  = format white bg bg2 . icon
    , ppHiddenNoWindows = format white bg bg . icon
    , ppWsSep   = " "
    , ppSep     = " · "
    }

icon :: WorkspaceId -> String
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
   
format foreground background line ws = wrap (ln ++ bg ++ fg ++ padding) (padding ++ close) ws
    where
        ln      = "%{u" ++ line ++ "}"
        bg      = "%{B" ++ background ++ "}"
        fg      = "%{F" ++ foreground ++ "}"
        close   = "%{-u}%{B- F-}"
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
