-- |
-- Module      : Sahasrara.Plugins.Netrunner.Plugin
-- Description : A plugin for finding Netrunner cards from Discord.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- Commands for interfacing with NetrunnerDB.
module Sahasrara.Plugins.Netrunner.Plugin (netrunnerPlugin) where

import Control.Monad.IO.Class
import Sahasrara.Internal.Handler.Command ()
import Sahasrara.Plugins.Netrunner.Command.Find (nrInline, nrInlineBanHistory, nrInlineFlavour, nrInlineImg)
import Sahasrara.Plugins.Netrunner.Command.Glossary (nrGlossary)
import Sahasrara.Plugins.Netrunner.Command.Help (helpPageRoots)
import Sahasrara.Plugins.Netrunner.Command.Horoscope (nrHoroscope)
import Sahasrara.Plugins.Netrunner.Command.Legality
import Sahasrara.Plugins.Netrunner.Command.Ram (nrRam)
import Sahasrara.Plugins.Netrunner.Command.Search (nrRandom, nrSearch)
import Sahasrara.Plugins.Netrunner.Command.Sets (nrCycles, nrSets)
import Sahasrara.Plugins.Netrunner.Type.NrApi (NrApi (..))
import Sahasrara.Plugins.Netrunner.Utility.NrApi (getNrApi)
import Sahasrara.Utility
import Sahasrara.Utility.Types ()

-- | @netrunnerStartUp@ loads the NetrunnerDB api once at start up
netrunnerStartUp :: StartUp NrApi
netrunnerStartUp = StartUp $ liftIO getNrApi

-- | @welcomePlugin@ assembles these commands into a plugin.
netrunnerPlugin :: EnvPlugin NrApi
netrunnerPlugin =
  (envPlug "netrunner" netrunnerStartUp)
    { commands =
        [ nrSearch,
          nrRandom,
          nrSets,
          nrCycles,
          nrRestriction,
          commandAlias "bl" nrRestriction,
          commandAlias "mwl" nrRestriction,
          nrGlossary,
          commandAlias "g" nrGlossary,
          nrRam,
          nrHoroscope
        ],
      inlineCommands = [nrInline, nrInlineImg, nrInlineFlavour, nrInlineBanHistory],
      helpPages = helpPageRoots
    }
