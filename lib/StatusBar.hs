module StatusBar (
    StatusBar.myLogHook
) where

import XMonad
import XMonad.Hooks.DynamicLog (ppCurrent, ppHidden, ppHiddenNoWindows, ppOutput, ppSep, ppTitle, ppUrgent, ppVisible, ppWsSep, shorten, wrap, dynamicLogWithPP)
import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8

import Colors

-- fontawesome icons
icon "1" = "\xf120" -- terminal
icon "2" = "\xf268" -- chrome
icon "3" = "\xf16c" -- stack overflow
icon "4" = "\xf121" -- code
icon "5" = "\xf1fe" -- area chart
icon "6" = "\xf085" -- cogs
icon "7" = "\xf075" -- comment
icon "8" = "\xf17c" -- linux
icon "9" = "\xf02d" -- book
icon "0" = "\xf1de" -- sliders
icon "-" = "\xf26c" -- television
icon "=" = "\xf0f4" -- coffee
icon n = n

xdo "=" = "equal"
xdo "-" = "minus"
xdo x = x

format foreground background line "NSP" = ""
format foreground background line ws = wrap (click ++ ln ++ bg ++ fg ++ padding) (padding ++ close) $ icon ws
    where
        click   = "%{A1:xdotool key super+" ++ (xdo ws) ++ ":}"
        ln      = "%{u" ++ line ++ "}"
        bg      = "%{B" ++ background ++ "}"
        fg      = "%{F" ++ foreground ++ "}"
        close   = "%{u#00000000}%{B- F- A-}"
        padding = "  "

myLogHook dbus = 
    dynamicLogWithPP $ def { ppOutput  = dbusOutput dbus
    , ppCurrent = format fg bg1 orange
    , ppVisible = format fg bg1 blue
    , ppUrgent  = format red fg red
    , ppHidden  = format fg bg1 bg1
    -- , ppHiddenNoWindows = format bg3 bg1 bg1
    , ppWsSep   = " "
    , ppSep     = "   "
    , ppTitle   = shorten 50
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
