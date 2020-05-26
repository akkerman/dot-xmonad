module Projects ( Projects.modify ) where

import XMonad (XConfig, spawn)
import XMonad.Actions.DynamicProjects

projects :: [Project]
projects =
  [ Project { projectName      = "rules"
            , projectDirectory = "~/git/dsplatform/dsp-rules-manager"
            , projectStartHook = Just $ do spawn "st -e tmuxinator rules"
                                           spawn "st -e nvim -S Session.vim"
                                           spawn "chromium --new-window"
            }
  , Project { projectName      = "analytics"
            , projectDirectory = "~/git/dsplatform/dsp-analytics-frontend"
            , projectStartHook = Just $ do spawn "st -e tmuxinator analytics"
                                           spawn "st -e nvim -S Session.vim"
                                           spawn "chromium --new-window"
            }
  , Project { projectName      = "capo"
            , projectDirectory = "~/git/dsplatform/capo-frontend"
            , projectStartHook = Just $ do spawn "st -e tmuxinator capo"
                                           spawn "st -e nvim -S Session.vim"
                                           spawn "chromium --new-window"
            }
  , Project { projectName      = "xmonad"
            , projectDirectory = "~/.config/xmonad"
            , projectStartHook = Just $ do spawn "chromium --new-window https://hackage.haskell.org/package/xmonad-contrib-0.16"
                                           spawn "st -e tmuxinator xmonad"
            }

  ]

modify:: XConfig a -> XConfig a
modify conf = dynamicProjects projects conf
