module Layout ( Layout.modify, Layout.myScratchpads ) where

import XMonad 

import XMonad.Hooks.ManageDocks (avoidStruts, docks, manageDocks)
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap)

import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.MultiToggle (mkToggle, EOT(EOT), Toggle(..), (??))
import XMonad.Layout.MultiToggle.Instances ( StdTransformers( NBFULL, MIRROR, NOBORDERS ))
import XMonad.Layout.Named (named)
import XMonad.Layout.Renamed (Rename)
import XMonad.Layout.Spacing (Border(..), spacingRaw)
import XMonad.Layout.ThreeColumns (ThreeCol(..))
import XMonad.Layout.Tabbed

import XMonad.Util.NamedScratchpad(namedScratchpadManageHook, defaultFloating, nonFloating, customFloating, namedScratchpadAction, NamedScratchpad(NS))
import XMonad.Util.Run(spawnPipe)

import Control.Monad (liftM2)
import Data.List(elemIndex)
import System.IO

import qualified DBus as D
import qualified DBus.Client as D
import qualified XMonad.StackSet as W

import Colors


renameLayout :: String -> String
renameLayout name = click ++ bg ++ ln ++ padding ++ name ++ padding ++ close
    where
      click   = "%{A1:xdotool key super+space:}"
      bg      = "%{B" ++ bg1 ++ "}"
      ln      = "%{u" ++ bg1 ++ "}"
      close   = "%{u#00000000}%{B- A}"
      padding = "  "

nameClick :: String -> l a -> ModifiedLayout Rename l a
nameClick name = named $ renameLayout name


myTabConfig = def { activeColor = bg
                  , activeBorderColor = bg
                  , activeTextColor = yellow

                  , inactiveColor = bg
                  , inactiveBorderColor = bg
                  , inactiveTextColor = gray

                  , urgentColor = bg
                  , urgentBorderColor = bg
                  , urgentTextColor = red
                  }

myLayout = avoidStruts $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) $
    tiled ||| three ||| mthree ||| tab
    where 
        three = nameClick "[]··" $ ThreeCol nmaster delta (5/12)
        mthree = nameClick "·[]·" $ ThreeColMid nmaster delta (1/2)
        tiled = nameClick "[]·" $ Tall nmaster delta (2/3)
        tab = nameClick "T" $ tabbedBottom shrinkText myTabConfig
        nmaster = 1
        delta = 3/100
        gaps = spacingRaw True (Border 1 0 0 0) False (Border 5 5 5 5) True

floatMiddle :: ManageHook
floatMiddle = customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)

floatMiddleLarge :: ManageHook
floatMiddleLarge = customFloating $ W.RationalRect (1/20) (1/20) (9/10) (9/10)

floatMiddleSmall :: ManageHook
floatMiddleSmall = customFloating $ W.RationalRect (1/3) (1/3) (1/3) (1/3)

floatTopSmall :: ManageHook
floatTopSmall = customFloating $ W.RationalRect (2/3) 0 (1/3) (1/3)

myScratchpads :: [NamedScratchpad]
myScratchpads = 
    [ NS "spotify" "spotify" (className =? "Spotify") floatMiddleLarge
    , NS "keepass" "keepassxc" (className =? "KeePassXC") floatMiddle
    , NS "arandr" "arandr" (className =? "Arandr") (customFloating $ W.RationalRect (1/8) (1/8) (1/3) (1/3))
    , NS "memento" "st -t memento -e nvim '+:cd ~/git/memento' -S Session.vim" (title =? "memento") nonFloating
    ]

myManageHook = composeAll
   [ className =? "Xmessage"          --> doFloat
   , className =? "Blueman-manager"   --> doFloat
   , className =? "Slack"             --> viewShift "7"
   , className =? "TelegramDesktop"   --> viewShift "7"
   , title =? "mux memento"           --> viewShift "9"
   , className =? "plexmediaplayer"   --> viewShift "-"

   , (className =? "Chromium" <&&> title =? "Open File") --> floatMiddle
   , (className =? "Chromium" <&&> title =? "Open Files") --> floatMiddle
   , (className =? "Chromium" <&&> title =? "Save File") --> floatMiddle

   , (className =? "Google-chrome" <&&> title =? "Open File") --> floatMiddle
   , (className =? "Google-chrome" <&&> title =? "Open Files") --> floatMiddle
   , (className =? "Google-chrome" <&&> title =? "Save File") --> floatMiddle

   , (className =? "Synergy" <&&> title =? "Synergy 1 Pro" )       --> floatMiddle
   , (className =? "Synergy" <&&> title =? "Server Configuration") --> floatMiddleSmall

   , className =? "GoldenDict" --> floatMiddle

   , className =? "Spotify" --> floatMiddle
   , className =? "Pavucontrol" --> floatTopSmall
   , className =? "Pavumeter" --> floatTopSmall
   , className =? "flameshot" --> floatMiddleSmall
   , className =? "Gmrun" --> doFloat
   , (className =? "Gimp" <&&> title =? "Quit GIMP") --> defaultFloating
   , role =? "GtkFileChooserDialog" --> floatMiddle
   , className =? "SimpleScreenRecorder" --> doFloat


   , manageDocks
   ]
   where 
     viewShift = doF . liftM2 (.) W.greedyView W.shift
     role = stringProperty "WM_WINDOW_ROLE"


-- modify :: XConfig l -> XConfig ...
modify conf = conf
    { manageHook = namedScratchpadManageHook myScratchpads <+> myManageHook <+> manageHook def
    , layoutHook = myLayout
    }
