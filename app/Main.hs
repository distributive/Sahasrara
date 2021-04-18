{-# LANGUAGE NamedFieldPuns, ImportQualifiedPost #-}

module Main where

import Tablebot
import Commands

import LoadEnv (loadEnv)
import System.Environment (getEnv, lookupEnv)
import Data.Text (pack)
import Data.ByteString.Char8 qualified as BS
import Data.Maybe (fromMaybe)
import Database.Redis

main :: IO ()
main = do
    loadEnv
    discordToken <- pack <$> getEnv "DISCORD_TOKEN"
    prefix <- pack . fromMaybe "!" <$> lookupEnv "PREFIX"
    conninfo <- redisConnectInfo
    rconn <- checkedConnect conninfo
    let commands = []
    runTablebot Cfg {discordToken, prefix, rconn, commands}

redisConnectInfo :: IO ConnectInfo
redisConnectInfo = do
    connectHost <- fromMaybe "localhost" <$> lookupEnv "REDIS_HOST"
    port <- lookupEnv "REDIS_PORT"
    let connectPort = PortNumber $ maybe 6379 read port
    auth <- lookupEnv "REDIS_AUTH"
    let connectAuth = BS.pack <$> auth
    return $ defaultConnectInfo {
        connectHost, connectPort, connectAuth
    }
