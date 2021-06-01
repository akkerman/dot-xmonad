module Projects ( Projects.modify, findProject ) where

import XMonad (XConfig, spawn)
import XMonad.Actions.DynamicProjects
import XMonad.Util.Run (runInTerm)
import Data.List (find)
import Data.Maybe (fromMaybe)


termOption prog session = "-n "++prog++"_"++session++" -t '"++prog++" '"++session
tmux session        = runInTerm (termOption "tmux" session) ("tmuxinator "++session)
nvimSession session = runInTerm (termOption "nvim" session) ("nvim -S Session.vim")

myProject session dir = Project 
   { projectName      = session
   , projectDirectory = dir
   , projectStartHook = Just $ do tmux session
                                  nvimSession session
                                  spawn "chromium --new-window"
   }

projects :: [Project]
projects =
  [ Project { projectName      = "1"
            , projectDirectory = "~/"
            , projectStartHook = Just $ do spawn "st"
            }

  , myProject "analytics"              "~/git/dsplatform/dsp-analytics-frontend"
  , myProject "services"               "~/git/dsplatform/dsp-analytics-services"
  , myProject "rules"                  "~/git/dsplatform/dsp-rules-manager"
  , myProject "capo"                   "~/git/dsplatform/capo-frontend"
  , myProject "energieonderbrekingen"  "~/git/energieonderbrekingen/development-vm/energieonderbrekingen.nl"

  , Project { projectName      = "notes"
            , projectDirectory = "~/org/slip-box"
            , projectStartHook = Just $ do spawn "chromium --app=http://localhost:5678"
                                           spawn "emacs"
                                           spawn "chromium --app=https://read.amazon.com/notebook"
                                           spawn "npx browser-sync start -s -f . --directory --host 0.0.0.0 --port 9000 --browser surf"
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
  , Project { projectName      = "xmonad"
            , projectDirectory = "~/.config/xmonad"
            , projectStartHook = Just $ do tmux "xmonad"
                                           spawn "chromium --new-window https://hackage.haskell.org/package/xmonad-contrib-0.16"
            }
  , Project { projectName      = "demo"
            , projectDirectory = "~"
            , projectStartHook  = Just $ do 
                                            spawn "/usr/lib/xscreensaver/spheremonics"
                                            runInTerm "-n demo_top" "top"
                                            runInTerm "-n demo_htop" "htop"
                                            runInTerm "-n glances" "glances"
                                            spawn "/usr/lib/xscreensaver/cubicgrid"
                                            spawn "/usr/lib/xscreensaver/surfaces"
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
