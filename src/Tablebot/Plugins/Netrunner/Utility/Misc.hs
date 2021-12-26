-- |
-- Module      : Tablebot.Plugins.Netrunner.Plugin
-- Description : A plugin for finding Netrunner cards from Discord.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- General purpose Netrunner functions.
module Tablebot.Plugins.Netrunner.Utility.Misc where

import Data.Text (Text, replace)
import Tablebot.Plugins.Netrunner.Type.NrApi (NrApi)
import Tablebot.Utility
import Tablebot.Utility.Discord (formatFromEmojiName)
import Tablebot.Utility.Types ()

-- | @formatNr@ takes a card's raw description and replaces the html formatting
-- tags with Discord formatting.
formatNr :: Text -> EnvDatabaseDiscord NrApi Text
formatNr raw = do
  credit <- formatFromEmojiName "credit"
  click <- formatFromEmojiName "click"
  interrupt <- formatFromEmojiName "interrupt"
  link <- formatFromEmojiName "link"
  mu <- formatFromEmojiName "mu"
  recurringCredit <- formatFromEmojiName "recurring_credit"
  subroutine <- formatFromEmojiName "subroutine"
  trash <- formatFromEmojiName "trash_ability"
  hb <- formatFromEmojiName "hb"
  jinteki <- formatFromEmojiName "jinteki"
  nbn <- formatFromEmojiName "nbn"
  weyland <- formatFromEmojiName "weyland"
  anarch <- formatFromEmojiName "anarch"
  criminal <- formatFromEmojiName "criminal"
  shaper <- formatFromEmojiName "shaper"
  apex <- formatFromEmojiName "apex"
  adam <- formatFromEmojiName "adam"
  sunny <- formatFromEmojiName "sunny"
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
        ("<errata>", "_**Errata:** "),
        ("</errata>", "_"),
        ("<champion>", "**"),
        ("</champion>", "**"),
        ("<ul>", "\n"),
        ("</ul>", ""),
        ("<li>", "• "),
        ("</li>", "\n"),
        ("[credit]", credit),
        ("[click]", click),
        ("[interrupt]", interrupt),
        ("[link]", link),
        ("[mu]", mu),
        ("[recurring-credit]", recurringCredit),
        ("[subroutine]", subroutine),
        ("[trash]", trash),
        ("[haas-bioroid]", hb),
        ("[jinteki]", jinteki),
        ("[nbn]", nbn),
        ("[weyland-consortium]", weyland),
        ("[anarch]", anarch),
        ("[criminal]", criminal),
        ("[shaper]", shaper),
        ("[apex]", apex),
        ("[adam]", adam),
        ("[sunny-lebeau]", sunny)
      ]
