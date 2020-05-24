module Prompt ( promptCfg ) where

import XMonad
import XMonad.Prompt
import XMonad.Prompt.FuzzyMatch (fuzzyMatch, fuzzySort)

import Data.Maybe (isJust)

import Colors

promptCfg :: XPConfig
promptCfg = def
      { font                = "xft:Source Code Pro:size=10"
      , bgColor             = bg2
      , fgColor             = fg
      , bgHLight            = orange
      , fgHLight            = bg2
      , borderColor         = bg3
      , promptBorderWidth   = 1
      , position            = Top
      , height              = 28
      , historySize         = 256
      , historyFilter       = id
      , defaultText         = []
      , autoComplete        = Nothing -- Just 1    -- set Just 100000 for .1 sec
      , showCompletionOnTab = False
      , searchPredicate     = fuzzyMatch
      , sorter              = fuzzySort
      , alwaysHighlight     = True
      , maxComplRows        = Nothing        -- set to Just 5 for 5 rows
      }
