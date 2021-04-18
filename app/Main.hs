module Main where

import Tablebot
import Commands

import LoadEnv
import System.Environment (getEnv)
import Data.Text

main :: IO ()
main = do
    loadEnv
    dtoken <- getEnv "DISCORD_TOKEN"
    let cfg = Cfg (pack dtoken) []
    runTablebot cfg