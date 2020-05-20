import XMonad

import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks)
import XMonad.Hooks.DynamicLog (dynamicLogWithPP)

import XMonad.Layout.MultiToggle (mkToggle, EOT(EOT), Toggle(..), (??))
import XMonad.Layout.MultiToggle.Instances ( StdTransformers( NBFULL, MIRROR, NOBORDERS ))
import XMonad.Layout.Named (named)
import XMonad.Layout.Spacing (Border(..), spacingRaw)
import XMonad.Layout.ThreeColumns (ThreeCol(..))

import XMonad.Util.Run(spawnPipe)

import Control.Monad (liftM2)
import Data.List(elemIndex)
import System.IO

import qualified DBus as D
import qualified DBus.Client as D
import qualified XMonad.StackSet as W

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
        { logHook = dynamicLogWithPP (StatusBar.myLogHook dbus) 
        , terminal = "st"
        , normalBorderColor = blue
        , focusedBorderColor = orange
        } 

-- vim: fdm=marker fdc=2 fcs=fold\:\ :
