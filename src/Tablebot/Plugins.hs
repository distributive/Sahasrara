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

import Tablebot.Handler.Plugins (compilePlugin)
import Tablebot.Handler.Types (CompiledPlugin)
import Tablebot.Plugins.Administration (administrationPlugin)
import Tablebot.Plugins.Basic (basicPlugin)
import Tablebot.Plugins.Cats (catPlugin)
import Tablebot.Plugins.Flip (flipPlugin)
import Tablebot.Plugins.Ping (pingPlugin)
import Tablebot.Plugins.Quote (quotePlugin)
import Tablebot.Plugins.Reminder (reminderPlugin)
import Tablebot.Plugins.RollDice (rollPlugin)
import Tablebot.Plugins.Say (sayPlugin)
import Tablebot.Plugins.Shibe (shibePlugin)
import Tablebot.Plugins.Welcome (welcomePlugin)

-- Use long list format to make additions and removals non-conflicting on git PRs
plugins :: [CompiledPlugin]
plugins =
  addAdministrationPlugin
    [ compilePlugin pingPlugin,
      compilePlugin basicPlugin,
      compilePlugin catPlugin,
      compilePlugin shibePlugin,
      compilePlugin flipPlugin,
      compilePlugin quotePlugin,
      compilePlugin reminderPlugin,
      compilePlugin sayPlugin,
      compilePlugin rollPlugin,
      compilePlugin welcomePlugin
    ]

-- | @addAdministrationPlugin@ is needed to allow the administration plugin to be aware of the list of current plugins
addAdministrationPlugin :: [CompiledPlugin] -> [CompiledPlugin]
addAdministrationPlugin cps = compilePlugin (administrationPlugin cps) : cps
