import XMonad

import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks)
import XMonad.Hooks.WindowSwallowing

import qualified XMonad.DBus as D

-- Custom modules
import ShortCuts  (modify)
import Colors
import StatusBar (myLogHook)
import Layout (modify)

main = do
    dbus <- D.connect

    D.requestAccess dbus

    xmonad $ docks $ ShortCuts.modify $ Layout.modify $ def
        { logHook = StatusBar.myLogHook dbus
        , handleEventHook = swallowEventHook (className =? "st-256color") (return True)
        , terminal = "/usr/local/bin/st"
        , normalBorderColor = bg1
        , focusedBorderColor = gray
        } 

-- vim: fdm=marker fdc=2 fcs=fold\:\ :
