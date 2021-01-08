module Projects ( Projects.modify, findProject ) where

import XMonad (XConfig, spawn)
import XMonad.Actions.DynamicProjects
import Data.List (find)
import Data.Maybe (fromMaybe)

projects :: [Project]
projects =
  [ Project { projectName      = "1"
            , projectDirectory = "~/"
            , projectStartHook = Just $ do spawn "st"
            }
  , Project { projectName      = "rules"
            , projectDirectory = "~/git/dsplatform/dsp-rules-manager"
            , projectStartHook = Just $ do spawn "st -e tmuxinator rules"
                                           spawn "st -e nvim -S Session.vim"
                                           spawn "chromium --new-window"
            }
  , Project { projectName      = "EnergieOnderbrekingen"
            , projectDirectory = "~/git/energieonderbrekingen/development-vm/energieonderbrekingen.nl"
            , projectStartHook = Just $ do spawn "st -e tmuxinator energieonderbrekingen"
                                           spawn "st -e nvim -S Session.vim"
                                           spawn "chromium --new-window"
            }
  , Project { projectName      = "Haskell"
            , projectDirectory = "~/git/learn/haskell"
            , projectStartHook = Just $ do spawn "st -e tpwd"
            }
  , Project { projectName      = "Plex"
            , projectDirectory = "~"
            , projectStartHook = Just $ do 
                spawn "chromium --app=https://app.plex.tv/desktop"
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
  , Project { projectName      = "services"
            , projectDirectory = "~/git/dsplatform/dsp-analytics-services"
            , projectStartHook = Just $ do spawn "st -e tmuxinator services"
                                           spawn "st -e nvim -S Session.vim"
                                           spawn "chromium --new-window"
            }
  , Project { projectName      = "xmonad"
            , projectDirectory = "~/.config/xmonad"
            , projectStartHook = Just $ do spawn "chromium --new-window https://hackage.haskell.org/package/xmonad-contrib-0.16"
                                           spawn "st -e tmuxinator xmonad"
            }
  , Project { projectName      = "Running"
            , projectDirectory = "~"
            , projectStartHook = Just $ do 
                 spawn "chromium --new-window https://connect.garmin.com/modern/ https://www.strava.com/dashboard https://www.endomondo.com/home chrome-extension://dhiaggccakkgdfcadnklkbljcgicpckn/app/index.html#/fitnessTrend"
            }

  ]

modify:: XConfig a -> XConfig a
modify conf = dynamicProjects projects conf

firstProject :: Project
firstProject = head projects

findProject :: ProjectName -> Project
findProject name = fromMaybe firstProject (maybeFindProject name)

maybeFindProject :: ProjectName -> (Maybe Project)
maybeFindProject name = find (matchProject name) projects

matchProject :: ProjectName -> Project -> Bool
matchProject name (Project projectName _ _)   = projectName == name
