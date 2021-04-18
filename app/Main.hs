{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Tablebot
import Features.Ping (pingPlugin)

import LoadEnv (loadEnv)
import System.Environment (getEnv, lookupEnv)
import Data.Text (pack)
import qualified Data.ByteString.Char8 as BS (pack)
import Data.Maybe (fromMaybe)
import Database.Redis

main :: IO ()
main = do
    loadEnv
    discordToken <- pack <$> getEnv "DISCORD_TOKEN"
    prefix <- pack . fromMaybe "!" <$> lookupEnv "PREFIX"
    conninfo <- redisConnectInfo
    rconn <- checkedConnect conninfo
    let plugins = [pingPlugin]
    runTablebot Cfg {discordToken, prefix, rconn, plugins}

redisConnectInfo :: IO ConnectInfo
redisConnectInfo = do
    connectHost <- fromMaybe "localhost" <$> lookupEnv "REDIS_HOST"
    port <- lookupEnv "REDIS_PORT"
    let connectPort = PortNumber $ maybe 6379 read port
    auth <- lookupEnv "REDIS_AUTH"
    let connectAuth = BS.pack <$> auth
    return $ defaultConnectInfo {connectHost, connectPort, connectAuth}
