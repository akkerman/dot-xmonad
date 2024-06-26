module ShortCuts (
    ShortCuts.modify
) where

import XMonad

import XMonad.Hooks.ManageDocks (ToggleStruts(..), Direction2D(U))

import XMonad.Actions.CycleWS
import XMonad.Actions.DwmPromote
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
import Prompt (promptCfg, promptWarnCfg, promptDangerCfg)
import Layout (myScratchpads)
import System.Exit


myWorkspaces = map show ([1..9 :: Int]) ++ ["0", "-", "="]


dmenu_settings = " -nb '" ++ bg2 ++ "' -nf '" ++ fg ++ "' -sb '" ++ orange ++ "' -sf '" ++ bg1 ++ "' -fn terminus-12:normal -h 26"

xmonadHome = "$HOME/.config/xmonad"

modify :: XConfig l -> XConfig l
modify conf = conf 
  { modMask = mod4Mask
  , workspaces = myWorkspaces
  }

  `additionalKeysP` 
    (
    [ ("M4-M1-l"                    , spawn "playerctl stop; slock") -- lock screen
    , ("M4-M1-S-l"                  , spawn "playerctl stop; slock systemctl suspend -i") -- lock screen and suspend

    -- dmenu
    , ("M-d c"                      , spawn ("dmenu_run" ++ dmenu_settings))
    , ("M-d d"                      , spawn ("j4-dmenu-desktop --term=/usr/local/bin/st --dmenu=\"dmenu -i " ++ dmenu_settings ++ "\""))
    , ("M-d t"                      , spawn ("$HOME/.config/tmuxinator/dmenu_mux.sh start" ++ dmenu_settings))
    , ("M-d S-t"                    , spawn ("$HOME/.config/tmuxinator/dmenu_mux.sh stop" ++ dmenu_settings))
    , ("M-d r"                      , spawn (xmonadHome ++ "/dmenu_restart.sh" ++ dmenu_settings))
    , ("M-d p"                      , spawn (xmonadHome ++ "/dmenu_pdf.sh" ++ dmenu_settings))
    , ("M-p"                        , spawn (xmonadHome ++ "/dmenu_pdf.sh" ++ dmenu_settings))
    , ("M-d m"                      , spawn (xmonadHome ++ "/chscreen.sh " ++ dmenu_settings))
    , ("M-d w"                      , spawn (xmonadHome ++ "/change-wallpaper.sh " ++ dmenu_settings))
    , ("M-d i"                      , spawn (xmonadHome ++ "/insert-bookmark.sh " ++ dmenu_settings))
    , ("M-d b"                      , spawn (xmonadHome ++ "/create-bookmark.sh " ++ dmenu_settings))
    , ("M-S-o"                      , spawn (xmonadHome ++ "/launch-bookmark.sh " ++ dmenu_settings))
    , ("M1-<Tab>"                   , spawn ("/usr/bin/rofi -show window"))

    -- , ("M-r"                        , spawn ("$HOME/Pictures/wallpapers/one_random_background.sh"))

    -- prompt
    , ("M-d s"                      , sshPrompt promptCfg)
    , ("M-d <Return>"               , shellPrompt promptCfg)
    , ("M-S-q"                      , confirmPrompt promptDangerCfg "Quit XMonad" $ io (exitWith ExitSuccess))

    -- programs
    , ("M-g"                        , spawn "chromium --profile-directory=Default")
    , ("M-o"                        , spawn "qutebrowser")

    , ("M-S-f"                      , spawn "firefox")
    , ("M-S-g"                      , spawn "chromium --incognito")

    , ("M-S-<Return>"               , spawn "/usr/local/bin/st")
    , ("M-<Backspace>"              , kill)
    , ("M-S-<Backspace>"            , confirmPrompt promptWarnCfg "close all windows on this workspace" $ killAll)
    , ("<Print>"                    , spawn "flameshot gui")
    , ("S-<Print>"                  , spawn "flameshot full -p $HOME/Pictures/scrot")

    -- emacs add todo etc.
    , ("M-n t"                      , spawn "emacsclient -c -a '' --eval '(org-capture)'")
    , ("M-S-m"                      , spawn "emacsclient -c")
    , ("M-m"                        , spawn "emacsclient -c")

    -- modify screen/layout
    , ("M-<Return>"                 , dwmpromote)
    -- , ("M-b"                        , sendMessage $ ToggleStrut U)    --- overlap window over polybar
    , ("M-b"                        , sendMessage $ Toggle NOBORDERS) --- show/hide borders
    , ("M-f"                        , sendMessage (Toggle NBFULL) >> sendMessage ToggleStruts)

    , ("M-S-a"                      , namedScratchpadAction myScratchpads "arandr")
    , ("M-s"                        , namedScratchpadAction myScratchpads "spotify")
    -- , ("M-m"                        , namedScratchpadAction myScratchpads "memento")
    , ("M-x"                        , namedScratchpadAction myScratchpads "keepass")
    , ("M-z"                        , spawn "pcmanfm")


    , ("M-S-0"                      , windows $ W.shift "0") -- workspace 10
    , ("M-S--"                      , windows $ W.shift "-") -- workspace 11
    , ("M-S-="                      , windows $ W.shift "=") -- workspace 12


    , ("C-<Space>"                  , spawn "dunstctl close")
    , ("C-S-<Space>"                , spawn "dunstctl close-all")
    , ("C-S-."                      , spawn "dunstctl context")



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
