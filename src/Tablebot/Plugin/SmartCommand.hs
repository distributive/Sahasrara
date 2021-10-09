{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Tablebot.Plugin.SmartCommand
-- Description : Automatic parser generation from function types.
-- Copyright   : (c) Finnbar Keating 2021
-- License     : MIT
-- Maintainer  : finnjkeating@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Generates a parser based on the shape of the command function.
-- For example, if you have a command that takes in an Int as argument, we
-- build a parser that reads in that Int and then runs the command.

module Tablebot.Plugin.SmartCommand where

import Data.Proxy
import Data.Text
import Discord.Types
import GHC.TypeLits
import Tablebot.Plugin.Parser (digit, quoted, space, word)
import Tablebot.Plugin.Types (DatabaseDiscord, Parser)
import Text.Megaparsec

class PComm commandty where
  parseComm :: commandty -> Parser (Message -> DatabaseDiscord ())

instance {-# OVERLAPPING #-} PComm (Message -> DatabaseDiscord ()) where
  parseComm comm = eof >> return comm

instance {-# OVERLAPPABLE #-}(CanParse a, PComm as) => PComm (a -> as) where
  parseComm comm = do
    this <- pars @a
    space
    parseComm (comm this)

class CanParse a where
  pars :: Parser a

instance CanParse Text where
  pars = pack <$> word

instance CanParse String where
  pars = word

newtype Quoted = Qu Text

instance CanParse Quoted where
  pars = Qu . pack <$> quoted

instance CanParse Int where
  pars = read <$> many digit

-- Try to read the value, or skip if you fail.
instance CanParse a => CanParse (Maybe a) where
  pars = optional $ try (pars @a)

-- Parse any number of a type (can include zero)
instance CanParse a => CanParse [a] where
  pars = many pars

data Exactly (s :: Symbol) = Ex

instance KnownSymbol s => CanParse (Exactly s) where
  pars = chunk (pack $ symbolVal (Proxy :: Proxy s)) >> return Ex

instance (CanParse a, CanParse b) => CanParse (Either a b) where
  pars = (Left <$> pars @a) <|> (Right <$> pars @b)

instance (CanParse a, CanParse b) => CanParse (a, b) where
  pars = do
    x <- pars @a
    space
    y <- pars @b
    return (x, y)

instance (CanParse a, CanParse b, CanParse c) => CanParse (a, b, c) where
  pars = do
    x <- pars @a
    space
    y <- pars @b
    space
    z <- pars @c
    return (x, y, z)

newtype WithError (err :: Symbol) x = WErr x

instance (KnownSymbol err, CanParse x) => CanParse (WithError err x) where
  pars = (WErr <$> pars @x) <?> symbolVal (Proxy :: Proxy err)