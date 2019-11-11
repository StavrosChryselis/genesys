module Genesys.Backend.Logger where

import Genesys

type Logger = LogString -> IO ()

silentLogger :: Logger
silentLogger _ = pure ()

verbalLogger :: Logger
verbalLogger = putStrLn

fileLogger :: Logger
fileLogger = undefined
