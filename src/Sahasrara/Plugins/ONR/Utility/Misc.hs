-- |
-- Module      : Sahasrara.Plugins.Netrunner.Plugin
-- Description : A plugin for finding Netrunner cards from Discord.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- General purpose Netrunner functions.
module Sahasrara.Plugins.ONR.Utility.Misc where

import Data.Text (Text, replace)
import Sahasrara.Plugins.ONR.Type.OnrApi (OnrApi)
import Sahasrara.Utility
import Sahasrara.Utility.Discord (formatFromEmojiName)
import Sahasrara.Utility.Types ()

-- | @formatNr@ takes a card's raw description and replaces the html formatting
-- tags with Discord formatting.
formatNr :: Text -> EnvDatabaseDiscord OnrApi Text
formatNr raw = do
  action <- formatFromEmojiName "onr_action"
  bit <- formatFromEmojiName "onr_bit"
  subroutine <- formatFromEmojiName "onr_subroutine"
  trash <- formatFromEmojiName "onr_trash"
  return $
    foldr
      (uncurry replace)
      raw
      [ ("<strong>", "**"),
        ("</strong>", "**"),
        ("<em>", "*"),
        ("</em>", "*"),
        ("<trace>", "**"),
        ("</trace>", "**"),
        ("[action]", action),
        ("[bit]", bit),
        ("[subroutine]", subroutine),
        ("[trash]", trash),
        ("*", "\\*"),
        ("_", "\\_")
      ]
