{-|
Module      : Tablebot.Plugin
Description : Helpful imports for building plugins.
Copyright   : (c) Finnbar Keating 2021
License     : MIT
Maintainer  : finnjkeating@gmail.com
Stability   : experimental
Portability : POSIX

Imports for when you develop your own plugins. This deliberately hides some
functionality as to avoid plugin creation from breaking if the underlying types
are ever updated. You should always import this over "Tablebot.Plugin.Types".
-}
module Tablebot.Plugin (
    module Types
) where

import Tablebot.Plugin.Types as Types hiding (Pl)