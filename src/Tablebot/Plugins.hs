-- |
-- Module      : Tablebot.Plugins
-- Description : Available plugins for Tablebot.
-- Copyright   : (c) Finnbar Keating 2021
-- License     : MIT
-- Maintainer  : finnjkeating@gmail.com
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

import Tablebot.Plugin (Plugin)
import Tablebot.Plugins.Basic (basicPlugin)
import Tablebot.Plugins.Cats (catPlugin)
import Tablebot.Plugins.Ping (pingPlugin)
import Tablebot.Plugins.Quote (quotePlugin)
import Tablebot.Plugins.Reminder (reminderPlugin)

plugins :: [Plugin]
plugins =
  [ pingPlugin,
    quotePlugin,
    reminderPlugin,
    basicPlugin,
    catPlugin
  ]
