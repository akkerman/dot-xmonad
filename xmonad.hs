import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
import XMonad.Layout.ThreeColumns


main = do
    xmproc <- spawnPipe "/usr/sbin/xmobar /home/akkerman/.xmobarrc"

    xmonad $ docks $ defaultConfig
        { manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = myLayout
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        , terminal = "st"
        [ ((mod4Mask .|. mod1Mask, xK_l), spawn "slock")
        ]

myLayout =
    avoidStruts $ tiled ||| three ||| Full 
    where 
        three = ThreeColMid nmaster delta (5/12)
        tiled = Tall nmaster delta ratio
        nmaster = 1
        ratio = 2/3
        delta = 3/100
