{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Tablebot.TypeLevel where

import Data.Proxy
import Data.Text
import Discord.Types
import GHC.TypeLits
import Tablebot.Plugin.Parser (digit, quoted, space, word)
import Tablebot.Plugin.Types (DatabaseDiscord, Parser)
import Text.Megaparsec

-- TODO: generally put this in the correct place and integrate properly.
-- This is currently just a scratchpad.

data Permission = Admin | Moderator | None

data ParsedCommand (perm :: Permission) ty = PC
  { name :: Text,
    command :: ty -> Message -> DatabaseDiscord ()
  }

parseCommand ::
  (PComm (ty -> Message -> DatabaseDiscord ())) =>
  ParsedCommand perm ty ->
  Parser (Message -> DatabaseDiscord ())
parseCommand comm = parseComm (command comm)

class PComm commandty where
  parseComm :: commandty -> Parser (Message -> DatabaseDiscord ())

instance PComm (Message -> DatabaseDiscord ()) where
  parseComm comm = eof >> return comm

instance (CanParse a, PComm as) => PComm (a -> as) where
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
  pars = (Qu . pack) <$> quoted

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
