-- |
-- Module      : Sahasrara.Plugins.Netrunner.Plugin
-- Description : A plugin for finding Netrunner cards from Discord.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- General purpose Netrunner functions.
module Sahasrara.Plugins.Netrunner.Utility.Formatting where

import Data.Text (Text, replace)
import Sahasrara.Plugins.Netrunner.Type.NrApi (NrApi)
import Sahasrara.Utility
import Sahasrara.Utility.Discord (formatFromEmojiName)
import Sahasrara.Utility.Types ()

-- | @formatNr@ replaces the Netrunner symbols in a given text with Discord
-- emoji.
formatNr :: Text -> EnvDatabaseDiscord NrApi Text
formatNr raw = do
  credit <- formatFromEmojiName "s_credit"
  click <- formatFromEmojiName "s_click"
  interrupt <- formatFromEmojiName "s_interrupt"
  link <- formatFromEmojiName "s_link"
  mu <- formatFromEmojiName "s_mu"
  recurringCredit <- formatFromEmojiName "s_recurring_credit"
  subroutine <- formatFromEmojiName "s_subroutine"
  trash <- formatFromEmojiName "s_trash_ability"
  hb <- formatFromEmojiName "s_hb"
  jinteki <- formatFromEmojiName "s_jinteki"
  nbn <- formatFromEmojiName "s_nbn"
  weyland <- formatFromEmojiName "s_weyland"
  anarch <- formatFromEmojiName "s_anarch"
  criminal <- formatFromEmojiName "s_criminal"
  shaper <- formatFromEmojiName "s_shaper"
  apex <- formatFromEmojiName "s_apex"
  adam <- formatFromEmojiName "s_adam"
  sunny <- formatFromEmojiName "s_sunny"
  return $
    foldr
      (uncurry replace)
      raw
      [ ("[credit]", credit),
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

-- | @formatDiscord@ replaces the html formatting in a given text with Discord
-- formatting.
formatDiscord :: Text -> Text
formatDiscord raw =
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
      ("<br>", "\n"),
      ("*", "\\*"),
      ("_", "\\_")
    ]

-- | @formatDiscord'@ replaces the html formatting in a given text with Discord
-- formatting. but does not escape existing Discord formatting.
formatDiscord' :: Text -> Text
formatDiscord' raw =
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
      ("<br>", "\n")
    ]

-- | @formatText@ reformats Netrunner symbols and html tags.
formatText :: Text -> EnvDatabaseDiscord NrApi Text
formatText raw = formatNr $ formatDiscord raw

-- | @formatText@ reformats Netrunner symbols and html tags without escaping
-- existing Discord formatting.
formatText' :: Text -> EnvDatabaseDiscord NrApi Text
formatText' raw = formatNr $ formatDiscord' raw
