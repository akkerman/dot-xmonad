module ShortCuts (
    ShortCuts.modify
) where

import XMonad

import XMonad.Hooks.ManageDocks (ToggleStruts(..), Direction2D(U))

import XMonad.Actions.CycleWS
import XMonad.Actions.DwmPromote
import XMonad.Actions.DynamicProjects
import XMonad.Actions.WithAll (killAll)

import XMonad.Layout.MultiToggle (Toggle(..), (??))
import XMonad.Layout.MultiToggle.Instances ( StdTransformers( NBFULL, MIRROR, NOBORDERS ))

import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Util.NamedScratchpad(namedScratchpadAction)
import XMonad.Util.NamedActions(addName)
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt.Theme

import Data.Maybe

import qualified XMonad.StackSet as W

import Colors

import XMonad.Prompt.Ssh (sshPrompt)
import XMonad.Prompt.Shell (shellPrompt)
import Prompt (promptCfg, promptDangerCfg)
import Layout (myScratchpads)
import Projects (findProject)


myWorkspaces = map show ([1..9 :: Int])-- ++ ["0", "-", "="]


dmenu_settings = " -nb '" ++ bg2 ++ "' -nf '" ++ fg ++ "' -sb '" ++ orange ++ "' -sf '" ++ bg1 ++ "' -fn terminus-12:normal -h 26"

modify :: XConfig l -> XConfig l
modify conf = conf 
  { modMask = mod4Mask
  , workspaces = myWorkspaces
  }

  `additionalKeysP` 
    (
    [ ("M4-M1-l"                    , spawn "slock") -- lock screen

    -- dmenu
    , ("M-d c"                      , spawn ("dmenu_run" ++ dmenu_settings))
    , ("M-d d"                      , spawn ("j4-dmenu-desktop --term=/usr/local/bin/st --dmenu=\"dmenu -i " ++ dmenu_settings ++ "\""))
    , ("M-d r"                      , spawn ("$HOME/.config/xmonad/dmenu_restart.sh" ++ dmenu_settings))
    , ("M-d t"                      , spawn ("$HOME/.config/tmuxinator/dmenu_mux.sh" ++ dmenu_settings))
    , ("M-d p"                      , spawn ("$HOME/.config/xmonad/dmenu_pdf.sh" ++ dmenu_settings))
    , ("M-d m"                      , spawn ("$HOME/.xmonad/chscreen.sh " ++ dmenu_settings))
    , ("M-d w"                      , spawn ("$HOME/.xmonad/change-wallpaper.sh " ++ dmenu_settings))

    -- prompt
    , ("M-d s"                      , sshPrompt promptCfg)
    , ("M-d <Return>"               , shellPrompt promptCfg)
    , ("M-p"                        , switchProjectPrompt promptCfg)
    , ("M-S-p"                      , shiftToProjectPrompt promptCfg)

    -- programs
    , ("M-g"                        , spawn "chromium --profile-directory=Default")
    , ("M-S-f"                      , spawn "firefox")
    , ("M-S-g"                      , spawn "chromium --incognito")
    , ("M-y"                        , spawn "chromium --profile-directory=Default --app-id=adnlfjpnmidfimlkaohpidplnoimahfh") -- youtube

    , ("M-S-<Return>"               , spawn "/usr/local/bin/st")
    , ("M-<Backspace>"              , kill)
    , ("M-S-<Backspace>"            , confirmPrompt promptDangerCfg "close all windows on this workspace" $ killAll)
    , ("<Print>"                    , spawn "flameshot gui")
    , ("S-<Print>"                  , spawn "flameshot full -p $HOME/Pictures/scrot")

    -- modify screen/layout
    , ("M-<Return>"                 , dwmpromote)
    , ("M-b"                        , sendMessage $ ToggleStrut U)    --- overlap window over polybar
    , ("M-n"                        , sendMessage $ Toggle NOBORDERS) --- show/hide borders
    , ("M-f"                        , sendMessage (Toggle NBFULL) >> sendMessage ToggleStruts)

    , ("M-S-a"                      , namedScratchpadAction myScratchpads "arandr")
    , ("M-s"                        , namedScratchpadAction myScratchpads "spotify")
    , ("M-m"                        , namedScratchpadAction myScratchpads "memento")

    , ("M-="                        , switchProject $ findProject "xmonad")

    -- , ("M-S-0"                      , windows $ W.shift "0") -- workspace 10
    -- , ("M-S--"                      , windows $ W.shift "-") -- workspace 11
    -- , ("M-S-="                      , windows $ W.shift "=") -- workspace 12


    -- media
    , ("<XF86AudioPlay>"            , spawn "playerctl play-pause")
    , ("<XF86AudioStop>"            , spawn "playerctl stop")
    , ("<XF86AudioNext>"            , spawn "playerctl next")
    , ("<XF86AudioPrev>"            , spawn "playerctl previous")
    , ("<XF86MonBrightnessUp>"      , spawn "light -A 5")
    , ("<XF86MonBrightnessDown>"    , spawn "light -U 5")
    ] 
    ++ 
    [("M-" ++ id, toggleOrView id) | id <- myWorkspaces] -- auto back and forth
    -- ++
    -- [ (mask ++ "M-" ++ [key], screenWorkspace scr >>= flip whenJust (windows . action))
    --      | (key, scr)  <- zip "wer" [1,0,2] -- was [0..] *** change to match your screen order ***
    --      , (action, mask) <- [ (W.view, "") , (W.shift, "S-")]
    -- ]
    )
