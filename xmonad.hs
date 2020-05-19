--- imports {{{1
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.DwmPromote

import XMonad.Hooks.DynamicLog (ppCurrent, ppHidden, ppHiddenNoWindows, ppOutput, ppSep, ppTitle, ppUrgent, ppVisible, ppWsSep, shorten, wrap, dynamicLogWithPP)
import XMonad.Hooks.ManageDocks (ToggleStruts(..), avoidStruts, docks, manageDocks, Direction2D(U))

import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), Toggle(..), (??))
import XMonad.Layout.MultiToggle.Instances ( StdTransformers( NBFULL, MIRROR, NOBORDERS ))
import XMonad.Layout.Named (named)
import XMonad.Layout.Spacing (Border(..), spacingRaw)
import XMonad.Layout.ThreeColumns (ThreeCol(..))

import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Util.Run(spawnPipe)

import Control.Monad (liftM2)
import Data.List(elemIndex)
import System.IO

import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8
import qualified XMonad.StackSet as W

import ShortCuts  (modify)
import Colors


--- layout {{{1
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
          
--- statusbar {{{1
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
        close   = "%{B- F- A-}"
        padding = "   "


--- logging {{{1
myLogHook dbus = def 
    { ppOutput  = dbusOutput dbus
    , ppCurrent = format fg bg1 orange
    , ppVisible = format fg bg1 blue
    , ppUrgent  = format red fg red
    , ppHidden  = format fg bg1 bg1
    , ppHiddenNoWindows = format bg3 bg1 bg1
    , ppWsSep   = ""
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

--- main {{{1

main = do
    dbus <- D.connectSession

    D.requestName dbus (D.busName_ "org.xmonad.Log")
        [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]


    xmonad $ docks $ ShortCuts.modify $ def
        { manageHook = myManageHook <+> manageHook def
        , layoutHook = myLayout
        , logHook = dynamicLogWithPP (myLogHook dbus) 
        -- , workspaces = myWorkspaces
        , terminal = "st"
        , normalBorderColor = blue
        , focusedBorderColor = orange
        } 

-- vim: fdm=marker fdc=2 fcs=fold\:\ :
