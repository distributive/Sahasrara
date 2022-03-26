-- |
-- Module      : Tablebot.Plugins
-- Description : Available plugins for Tablebot.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Here is a collection of existing plugins for Tablebot. If you add new plugins
-- to the Plugins directory, include an import here. This means that users only
-- need to import @Tablebot.Plugins@ to import individual plugins.
module Tablebot.Plugins
  ( plugins,
  )
where

import Control.Concurrent.MVar (MVar)
import Tablebot.Internal.Administration (ShutdownReason)
import Tablebot.Internal.Plugins (compilePlugin)
import Tablebot.Internal.Types (CompiledPlugin)
import Tablebot.Plugins.Administration (administrationPlugin)
import Tablebot.Plugins.Basic (basicPlugin)
import Tablebot.Plugins.Flip (flipPlugin)
import Tablebot.Plugins.Netrunner (netrunnerPlugin)

-- Use long list format to make additions and removals non-conflicting on git PRs
plugins :: MVar ShutdownReason -> [CompiledPlugin]
plugins rFlag =
  addAdministrationPlugin
    rFlag
    [ compilePlugin basicPlugin,
      compilePlugin flipPlugin,
      compilePlugin netrunnerPlugin
    ]

-- | @addAdministrationPlugin@ is needed to allow the administration plugin to be aware of the list of current plugins
addAdministrationPlugin :: MVar ShutdownReason -> [CompiledPlugin] -> [CompiledPlugin]
addAdministrationPlugin rFlag cps = compilePlugin (administrationPlugin rFlag cps) : cps
