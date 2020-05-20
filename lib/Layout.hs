module Layout ( Layout.modify ) where

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

import Colors


renameLayout name = ("%{A1:xdotool key super+space:}%{B"++bg1++"}%{u"++bg1++"}  " ++ name ++ "  %{-u}%{B- A-}")
nameClick name = named $ renameLayout name

myLayout = avoidStruts $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) $
    three ||| tiled
    where 
        three = nameClick "|||" $ gaps $ ThreeCol nmaster delta (5/12)
        tiled = nameClick "[]=" $ gaps $ Tall nmaster delta ratio
        nmaster = 1
        ratio = 2/3
        delta = 3/100
        gaps = spacingRaw True (Border 0 0 0 0) False (Border 5 5 5 5) True


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


modify conf = conf
    { manageHook = myManageHook <+> manageHook def
    , layoutHook = myLayout
    }
