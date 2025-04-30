module StatusBar (
    StatusBar.myLogHook
) where

import XMonad
import XMonad.Hooks.DynamicLog (ppCurrent, ppHidden, ppHiddenNoWindows, ppOutput, ppSep, ppTitle, ppUrgent, ppVisible, ppWsSep, shorten, wrap, dynamicLogWithPP)
import qualified XMonad.DBus as D
import qualified DBus.Client as DC
import qualified Codec.Binary.UTF8.String as UTF8

import XMonad.Actions.UpdatePointer

import Colors

-- fontawesome icons -- https://fontawesome.com/search?ic=free
-- the values are 'code points' for the icons
-- if the icon is displayed in a browser the codepoint can be 
-- viewed in the inspect under styles
icon "1" = "\xf120" -- terminal
icon "2" = "\xf268" -- chrome
icon "3" = "\xf16c" -- stack overflow
icon "4" = "\xf121" -- code
icon "5" = "\xf1fe" -- area chart
icon "6" = "\xf085" -- cogs
icon "7" = "\xf075" -- comment
icon "8" = "\xf17c" -- linux
icon "9" = "\xf02d" -- book
-- icon "0" = "\xf1de" -- sliders
icon "0" = "\xf249" -- note-sticky
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
        close   = "%{u#00000000}%{B- F- A}"
        padding = "  "

myLogHook :: DC.Client -> X()
myLogHook dbus = 
    (dynamicLogWithPP $ def { ppOutput = D.send dbus
    , ppCurrent = format fg bg2 blue
    , ppVisible = format fg bg2 bg1
    , ppUrgent  = format red fg red
    , ppHidden  = format bg3 bg2 bg1
    , ppHiddenNoWindows = format bg2 bg1 bg
    , ppWsSep   = ""
    , ppSep     = "   "
    , ppTitle   = shorten 50
    } ) >> updatePointer (0.5, 0.5) (0, 0)
