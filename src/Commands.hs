module Commands (
    Command
) where

import Data.Text

-- TODO: latter should be an actual function type.
-- This might also extend into a typeclass of runnable things with different arguments.
data Command = Command Text ()