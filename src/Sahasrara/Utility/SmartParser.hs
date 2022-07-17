{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Sahasrara.Utility.SmartParser
-- Description : Automatic parser generation from function types.
-- License     : MIT
-- Maintainer  : github.com/distributive
-- Stability   : experimental
-- Portability : POSIX
--
-- Generates a parser based on the shape of the command function.
-- For example, if you have a command that takes in an Int as argument, we
-- build a parser that reads in that Int and then runs the command.
module Sahasrara.Utility.SmartParser
  ( module Sahasrara.Utility.SmartParser.SmartParser,
    module Sahasrara.Utility.SmartParser.Interactions,
    module Sahasrara.Utility.SmartParser.Types,
  )
where

import Sahasrara.Utility.SmartParser.Interactions
import Sahasrara.Utility.SmartParser.SmartParser
import Sahasrara.Utility.SmartParser.Types
