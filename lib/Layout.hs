module Layout ( Layout.modify, Layout.myScratchpads ) where

import XMonad

import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks)
import XMonad.Hooks.DynamicLog (dynamicLogWithPP)

import XMonad.Layout.MultiToggle (mkToggle, EOT(EOT), Toggle(..), (??))
import XMonad.Layout.MultiToggle.Instances ( StdTransformers( NBFULL, MIRROR, NOBORDERS ))
import XMonad.Layout.Named (named)
import XMonad.Layout.Spacing (Border(..), spacingRaw)
import XMonad.Layout.ThreeColumns (ThreeCol(..))

import XMonad.Util.NamedScratchpad(namedScratchpadManageHook, defaultFloating, nonFloating, customFloating, namedScratchpadAction, NamedScratchpad(NS))
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


myScratchpads = 
    [ NS "spotify" "spotify" (className =? "Spotify") (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
    , NS "arandr" "arandr" (className =? "Arandr") (customFloating $ W.RationalRect (1/8) (1/8) (1/3) (1/3))
    , NS "htop" "st -t htop -e htop" (title =? "htop") (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3))
    , NS "memento" "st -t memento -e nvim '+:cd ~/git/memento' -S Session.vim" (title =? "memento") nonFloating
    ]

myManageHook = composeAll
   [ className =? "Xmessage"        --> doFloat
   , className =? "Blueman-manager" --> doFloat
   , className =? "Slack"           --> viewShift "7"
   , className =? "TelegramDesktop" --> viewShift "7"

   , title =? "mux xmonad"          --> viewShift "="
   , title =? "mux rules"           --> viewShift "4"
   , title =? "mux analytics"       --> viewShift "4"
   , title =? "mux capo"            --> viewShift "4"
   , title =? "mux memento"         --> viewShift "9"

   , manageDocks
   ]
   where 
     viewShift = doF . liftM2 (.) W.greedyView W.shift


modify conf = conf
    { manageHook = namedScratchpadManageHook myScratchpads <+> myManageHook <+> manageHook def
    , layoutHook = myLayout
    }
