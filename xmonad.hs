import XMonad

import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks)

import qualified DBus as D
import qualified DBus.Client as D

-- Custom modules
import ShortCuts  (modify)
import Colors
import StatusBar (myLogHook)
import Layout (modify)

main = do
    dbus <- D.connectSession

    D.requestName dbus (D.busName_ "org.xmonad.Log")
        [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]

    xmonad $ docks $ ShortCuts.modify $ Layout.modify $ def
        { logHook = StatusBar.myLogHook dbus 
        , terminal = "st"
        , normalBorderColor = blue
        , focusedBorderColor = orange
        } 

-- vim: fdm=marker fdc=2 fcs=fold\:\ :
