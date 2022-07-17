{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module      : Sahasrara.Utility.SmartParser.SmartParser
-- Description : Automatic parser generation from function types.
-- License     : MIT
-- Maintainer  : tagarople@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Generates a parser based on the shape of the command function.
-- For example, if you have a command that takes in an Int as argument, we
-- build a parser that reads in that Int and then runs the command.
module Sahasrara.Utility.SmartParser.SmartParser where

import Data.Proxy (Proxy (..))
import Data.Scientific ()
import Data.String (IsString (fromString))
import Data.Text (Text, pack)
import Discord.Interactions ()
import Discord.Types (Message, Snowflake (Snowflake))
import GHC.TypeLits (KnownSymbol, symbolVal)
import Sahasrara.Utility.Discord (sendCustomMessage)
import Sahasrara.Utility.Parser
import Sahasrara.Utility.SmartParser.Types
import Sahasrara.Utility.Types (EnvDatabaseDiscord, MessageDetails, Parser)
import Text.Megaparsec (MonadParsec (eof, try), chunk, many, optional, (<?>), (<|>))

-- | @PComm@ defines function types that we can automatically turn into parsers
-- by composing a parser per input of the function provided.
-- For example, @Int -> Maybe Text -> Message -> DatabaseDiscord s ()@ builds a
-- parser that reads in an @Int@, then some optional @Text@, and then uses
-- those to run the provided function with the arguments parsed and the message
-- itself.
--
-- The arguments to this class are the type of the function, the type of the
-- environment, the type of the context (either Message or Interaction), and the
-- type of the result of the function (which is either () or MessageDetails
-- usually).
class PComm commandty s context returns where
  parseComm :: (Context context) => commandty -> Parser (context -> EnvDatabaseDiscord s returns)

-- TODO: verify that all the parsers for PComm actually work

-- If there is the general case where we have just what we want to parse, then
-- return it
-- (1)
instance {-# OVERLAPPING #-} PComm (t -> EnvDatabaseDiscord s r) s t r where
  parseComm comm = skipSpace >> return comm

-- If we have the specific case where we are returning `()`, parse eof as well.
-- This should cover the base case for the rest of the program that doesn't use
-- more complex stuff.
-- (2)
instance {-# OVERLAPPING #-} PComm (t -> EnvDatabaseDiscord s ()) s t () where
  parseComm comm = skipSpace >> eof >> return comm

-- If an action takes a message and returns a message details and we want it to
-- return unit, assume that it wants to be sent, and send it. eof this as well
-- (3)
instance {-# OVERLAPPING #-} PComm (Message -> EnvDatabaseDiscord s MessageDetails) s Message () where
  parseComm comm = skipSpace >> eof >> return (\m -> comm m >>= sendCustomMessage m)

-- When there is no context to the function (eg no Message or Interaction),
-- just run the action. don't parse eof cause we may wanna return.
-- similar to (1)
-- (4)
instance PComm (EnvDatabaseDiscord s r) s t r where
  parseComm comm = skipSpace >> return (const comm)

-- When there is no context to the function (eg no Message or Interaction),
-- just run the action. effectively the function hasn't interacted with the `t`.
-- parse eof because we have unit here. similar to (2)
-- (5)
instance {-# OVERLAPPING #-} PComm (EnvDatabaseDiscord s ()) s t () where
  parseComm comm = skipSpace >> eof >> return (const comm)

-- if we're in a message context and have a message details but want to return
-- unit, assume that we want to send it, and send it. similar to (3)
-- (6)
instance {-# OVERLAPPING #-} PComm (EnvDatabaseDiscord s MessageDetails) s Message () where
  parseComm comm = skipSpace >> eof >> return (\m -> comm >>= sendCustomMessage m)

-- Recursive case is to parse the domain of the function type, then the rest.
-- (7)
instance {-# OVERLAPPABLE #-} (CanParse a, PComm as s t r) => PComm (a -> as) s t r where
  parseComm comm = do
    this <- parsThenMoveToNext @a
    parseComm (comm this)

-- if we have two contexts for some reason, collapse them if the resultant can
-- be parsed
-- (8)
instance {-# OVERLAPPABLE #-} (PComm (t -> as) s t r) => PComm (t -> t -> as) s t r where
  parseComm comm = parseComm (\m -> comm m m)

-- if we have a context and then some parseable value, effectively juggle the
-- context so that parsing continues (and the context is passed on)
-- (9)
instance {-# OVERLAPPABLE #-} (Context t, CanParse a, PComm (t -> as) s t r) => PComm (t -> a -> as) s t r where
  parseComm comm = do
    this <- parsThenMoveToNext @a
    parseComm (`comm` this)

-- special value case - if we get SenderUserId, we need to get the value from
-- the context. so, get the value from the context, and then continue parsing.
-- (10)
instance {-# OVERLAPPABLE #-} (PComm (t -> as) s t r) => PComm (SenderUserId -> as) s t r where
  parseComm comm = parseComm $ \(m :: t) -> comm (SenderUserId $ contextUserId m)

-- | @CanParse@ defines types from which we can generate parsers.
class CanParse a where
  pars :: Parser a
  parsThenMoveToNext :: Parser a
  parsThenMoveToNext = pars <* (eof <|> skipSpace1)

-- Note: since FromString and (Read, Integral) can overlap, we cannot specify
-- this instance as FromString a => CanParse a.
instance CanParse Text where
  pars = pack <$> word

-- This overlaps CanParse [a], since String = [Char].
instance {-# OVERLAPPING #-} CanParse String where
  pars = word

instance IsString a => CanParse (Quoted a) where
  pars = Qu . fromString <$> quoted

instance (ParseShow a) => ParseShow (Quoted a) where
  parseShow (Qu a) = "\"" <> parseShow a <> "\""

-- A parser for @Maybe a@ attempts to parse @a@, returning @Just x@ if
-- correctly parsed, else @Nothing@.
instance CanParse a => CanParse (Maybe a) where
  pars = optional $ try (pars @a)

  -- Note: we override @parsThenMoveToNext@:
  -- there will be no spaces to parse if the argument isn't present.
  parsThenMoveToNext =
    pars >>= \case
      Nothing -> return Nothing
      Just val -> Just val <$ (eof <|> skipSpace1)

-- A parser for @[a]@ parses any number of @a@s.
instance {-# OVERLAPPABLE #-} CanParse a => CanParse [a] where
  pars = many pars

-- A parser for @Either a b@ attempts to parse @a@, and if that fails then
-- attempts to parse @b@.
instance (CanParse a, CanParse b) => CanParse (Either a b) where
  pars = (Left <$> try (pars @a)) <|> (Right <$> pars @b)

-- TODO: automate creation of tuple instances using TemplateHaskell
instance (CanParse a, CanParse b) => CanParse (a, b) where
  pars = do
    x <- parsThenMoveToNext @a
    y <- pars @b
    return (x, y)

instance (CanParse a, CanParse b, CanParse c) => CanParse (a, b, c) where
  pars = do
    x <- parsThenMoveToNext @a
    y <- parsThenMoveToNext @b
    z <- pars @c
    return (x, y, z)

instance (CanParse a, CanParse b, CanParse c, CanParse d) => CanParse (a, b, c, d) where
  pars = do
    x <- parsThenMoveToNext @a
    y <- parsThenMoveToNext @b
    z <- parsThenMoveToNext @c
    w <- pars @d
    return (x, y, z, w)

instance (CanParse a, CanParse b, CanParse c, CanParse d, CanParse e) => CanParse (a, b, c, d, e) where
  pars = do
    x <- parsThenMoveToNext @a
    y <- parsThenMoveToNext @b
    z <- parsThenMoveToNext @c
    w <- parsThenMoveToNext @d
    v <- pars @e
    return (x, y, z, w, v)

instance KnownSymbol s => CanParse (Exactly s) where
  pars = chunk (pack $ symbolVal (Proxy :: Proxy s)) >> return Ex

instance (KnownSymbol err, CanParse x) => CanParse (WithError err x) where
  pars = (WErr <$> try (pars @x)) <?> symbolVal (Proxy :: Proxy err)

-- | Parsing implementation for all integral types
-- Overlappable due to the really flexible head state
instance {-# OVERLAPPABLE #-} (Integral a, Read a) => CanParse a where
  pars = integer

instance CanParse Double where
  pars = double

instance CanParse () where
  pars = eof

instance CanParse Snowflake where
  pars = Snowflake . fromInteger <$> posInteger

instance IsString a => CanParse (RestOfInput a) where
  pars = ROI . fromString <$> untilEnd

instance IsString a => CanParse (RestOfInput1 a) where
  pars = ROI1 . fromString <$> untilEnd1

-- | Parse a labelled value, by parsing the base value and adding the label
-- values.
instance (CanParse a) => CanParse (Labelled n d a) where
  pars = labelValue <$> pars

-- | @PosInteger a@ parses a sequence of digits with no preceeding "-" as an int.
newtype PosInt = PosInt Int

instance CanParse PosInt where
  pars = PosInt <$> posInteger

-- | @NonNegativeInt a@ parses a non-zero sequence of digits with no preceeding
-- "-" as an int.
newtype NonNegativeInt = NonNegativeInt Int

instance CanParse NonNegativeInt where
  pars = NonNegativeInt <$> nonNegativeInteger

-- | @noArguments@ is a type-specific alias for @parseComm@ for commands that
-- have no arguments (thus making it extremely clear).
noArguments :: (Message -> EnvDatabaseDiscord d ()) -> Parser (Message -> EnvDatabaseDiscord d ())
noArguments = parseComm
