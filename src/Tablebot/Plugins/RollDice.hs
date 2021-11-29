-- |
-- Module      : Tablebot.Plugins.RollDice
-- Description : A command that outputs the result of rolling dice.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- A command that outputs the result of rolling the input dice.
module Tablebot.Plugins.RollDice (rollPlugin) where

import Control.Monad.Writer (MonadIO (liftIO))
import Data.Text (pack)
import Discord.Types (Message)
import Tablebot.Plugin
import Tablebot.Plugin.Dice (Expr, PrettyShow (prettyShow), evalExpr)
import Tablebot.Plugin.Discord (sendMessage)
import Tablebot.Plugin.SmartCommand (PComm (parseComm))

rollDice :: Command
rollDice = Command "roll" diceComm []
  where
    rollDice' :: Expr -> Message -> DatabaseDiscord ()
    rollDice' e m = do
      (v, s) <- liftIO $ evalExpr e
      sendMessage m $ pack $ "You rolled " ++ s ++ ".\nOutput: " ++ show v
    diceComm = parseComm rollDice'

-- TODO: help page

-- sayHelp :: HelpPage
-- sayHelp =
--   HelpPage
--     "say"
--     "make the bot speak"
--     [r|**Say**
-- Repeat the input.

-- * Usage:* `say This text will be repeated by the bot!`|]

--     []
--     None

-- | @sayPlugin@ assembles the command into a plugin.
rollPlugin :: Plugin
rollPlugin = (plug "roll") {commands = [rollDice], helpPages = []}
