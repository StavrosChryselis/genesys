module Main where

import Genesys
import Genesys.Backend.Threaded

testCommando :: Command String () ()
testCommando = do
  pid <- spawn $ do
    s <- receive
    say $ "got " ++ s
    pure ()
  send pid "fromMaster"
  liftIO $ putStrLn "LIFTED FROM MASTER"

main :: IO ()
main = mkSystem () testCommando verbalLogger
