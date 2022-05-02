-- |
-- Module      : Sahasrara.Plugins
-- Description : Available plugins for Sahasrara.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- Here is a collection of existing plugins for Sahasrara. If you add new plugins
-- to the Plugins directory, include an import here. This means that users only
-- need to import @Sahasrara.Plugins@ to import individual plugins.
module Sahasrara.Plugins
  ( plugins,
  )
where

import Control.Concurrent.MVar (MVar)
import Sahasrara.Internal.Administration (ShutdownReason)
import Sahasrara.Internal.Plugins (compilePlugin)
import Sahasrara.Internal.Types (CompiledPlugin)
import Sahasrara.Plugins.Administration (administrationPlugin)
import Sahasrara.Plugins.Basic (basicPlugin)
import Sahasrara.Plugins.Flip (flipPlugin)
import Sahasrara.Plugins.Netrunner (netrunnerPlugin)
import Sahasrara.Plugins.ONR (onrPlugin)

-- Use long list format to make additions and removals non-conflicting on git PRs
plugins :: MVar ShutdownReason -> [CompiledPlugin]
plugins rFlag =
  addAdministrationPlugin
    rFlag
    [ compilePlugin basicPlugin,
      compilePlugin flipPlugin,
      compilePlugin netrunnerPlugin,
      compilePlugin onrPlugin
    ]

-- | @addAdministrationPlugin@ is needed to allow the administration plugin to be aware of the list of current plugins
addAdministrationPlugin :: MVar ShutdownReason -> [CompiledPlugin] -> [CompiledPlugin]
addAdministrationPlugin rFlag cps = compilePlugin (administrationPlugin rFlag cps) : cps
