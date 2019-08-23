--- imports {{{1
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.DynamicLog 
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Named
import XMonad.Layout.Maximize (maximizeWithPadding, maximizeRestore)
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Layout.TwoPane
import XMonad.Layout.NoBorders
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run(spawnPipe)

import Control.Monad (liftM2)
import Data.List(elemIndex)
import System.IO

import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8
import qualified XMonad.StackSet as W

--- colors {{{1
bg      = "#282828"
red     = "#cc241d"
green   = "#98971a"
yellow  = "#d79921"
blue    = "#458588"
lblue   = "83a598"
purple  = "#b16286"
aqua    = "#689d6a"
gray    = "#a89984"
orange  = "#d65d0e"

fg      = "#ebdbb2"

bg1     = "#3c3836"
bg2     = "#504945"
bg3     = "#665c54"

--- layout {{{1
renameLayout name = ("%{A1:xdotool key super+space:}%{B"++bg1++"}%{u"++bg1++"}  " ++ name ++ "  %{-u}%{B- A-}")
nameClick name = named $ renameLayout name

myLayout = avoidStruts $ smartBorders $
    tiled ||| three ||| two
    where 
        three = nameClick "|||" $ maximizeWithPadding 20 $ gaps $ ThreeColMid nmaster delta (5/12)
        two   = nameClick "[]|" $ maximizeWithPadding 20 $ gaps $ TwoPane delta ratio
        tiled = nameClick "[]=" $ maximizeWithPadding 20 $ gaps $ Tall nmaster delta ratio
        nmaster = 1
        ratio = 2/3
        delta = 3/100
        gaps = spacingRaw True (Border 0 0 0 0) False (Border 5 5 5 5) True

myScratchpads = 
    [ NS "spotify" "spotify" (className =? "Spotify") defaultFloating
    , NS "arandr" "arandr" (className =? "Arandr") defaultFloating
    ]

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
icon "1" = "\xf120"
icon "2" = "\xf268"
icon "3" = "\xf16c"
icon "4" = "\xf121"
icon "5" = "\xf1fe"
icon "6" = "\xf085"
icon "7" = "\xf075"
icon "8" = "\xf17c"
icon "9" = "\xf02d"
icon "0" = "\xf1de"
icon "-" = "\xf120"
icon "=" = "\xf0f4"
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
        close   = "%{-u}%{B- F- A-}"
        padding = "   "

--- shortcuts {{{1
dmenu_settings = " -nb '" ++ bg2 ++ "' -nf '" ++ fg ++ "' -sb '" ++ orange ++ "' -sf '" ++ bg1 ++ "' -fn terminus-12:normal -h 26"

myKeys = 
    [ ("M4-M1-l"                    , spawn "slock") -- lock screen
    , ("M-d"                        , spawn ("dmenu_run" ++ dmenu_settings))
    , ("M-S-t"                      , spawn ("$HOME/.config/tmuxinator/dmenu_mux.sh" ++ dmenu_settings))
    , ("M-S-p"                      , spawn ("$HOME/.xmonad/dmenu_pdf.sh" ++ dmenu_settings))
    , ("M-S-d"                      , spawn ("j4-dmenu-desktop --term=/usr/local/bin/st --dmenu=\"dmenu -i " ++ dmenu_settings ++ "\""))
    , ("M-g"                        , spawn "chromium --profile-directory=Default")
    , ("M-S-g"                      , spawn "chromium --incognito")
    , ("M-y"                        , spawn "chromium --profile-directory=Default --app-id=adnlfjpnmidfimlkaohpidplnoimahfh") -- youtube
    , ("M-p"                        , spawn "chromium --profile-directory=Default --app-id=amfkemaodmghlnknncknfhcmmiclmbpa") -- plex
    , ("M-S-m"                      , spawn ("$HOME/.xmonad/chscreen.sh " ++ dmenu_settings))

    , ("<XF86AudioPlay>"            , spawn "playerctl play-pause")
    , ("<XF86AudioStop>"            , spawn "playerctl stop")
    , ("<XF86AudioNext>"            , spawn "playerctl next")
    , ("<XF86AudioPrev>"            , spawn "playerctl previous")
    , ("<XF86MonBrightnessUp>"      , spawn "light -A 5")
    , ("<XF86MonBrightnessDown>"    , spawn "light -U 5")
    , ("M-b"                        , sendMessage $ ToggleStrut U) --- show/hide polybar
    , ("M-<Return>"                 , spawn "/usr/local/bin/st")
    , ("M-S-<Return>"               , windows W.swapMaster)

    , ("M-S-a"                      , namedScratchpadAction myScratchpads "arandr")
    , ("M-s"                        , namedScratchpadAction myScratchpads "spotify")
    , ("M-f"                        , withFocused (sendMessage . maximizeRestore))

    , ("M-<Backspace>"              , kill)
    , ("M-S-0"                      , windows $ W.shift "0")
    , ("M-S--"                      , windows $ W.shift "-")
    , ("M-S-="                      , windows $ W.shift "=")
    ] 
    ++ 
    [("M-" ++ id, toggleOrView id) | id <- myWorkspaces] -- auto back and forth
    -- ++
    -- [ (mask ++ "M-" ++ [key], screenWorkspace scr >>= flip whenJust (windows . action))
    --      | (key, scr)  <- zip "wer" [1,0,2] -- was [0..] *** change to match your screen order ***
    --      , (action, mask) <- [ (W.view, "") , (W.shift, "S-")]
    -- ]

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
modm = mod4Mask

myWorkspaces = map show ([1..9 :: Int]) ++ ["0", "-", "="]

main = do
    dbus <- D.connectSession

    D.requestName dbus (D.busName_ "org.xmonad.Log")
        [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]


    xmonad $ docks $ def
        { manageHook = myManageHook <+> manageHook def
        , layoutHook = myLayout
        , logHook = dynamicLogWithPP (myLogHook dbus) >> updatePointer (0.5,0) (0,0)
        , modMask = modm     -- Rebind Mod to the Windows key
        , workspaces = myWorkspaces
        , terminal = "st"
        , normalBorderColor = blue
        , focusedBorderColor = orange
        } `additionalKeysP` myKeys

-- vim: fdm=marker fdc=2 fcs=fold\:\ :
