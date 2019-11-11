import System.Environment (getArgs)
import Control.Monad (replicateM_)

import Genesys
import Genesys.Backend.Threaded

data Message = Inc Pid
             | Report Int
             | Done
             deriving (Eq, Show)

type Process a = Command Message () a

master :: Int -> Int -> Process ()
master n m = do
    myPid <- getSelfPid
    counterPid <- spawn $ counter 0
    replicateM_ n $ spawn $ consumer m counterPid myPid
    replicateM_ n $ do
        msg <- receive
        case msg of
            Done -> say' "Received 'Done'"
            _    -> say' "WRONG MESSAGE"
    say' "Terminating counter"
    send counterPid Done
    say' "Exiting"
  where
    say' = taggedSay "Master"

consumer :: Int -> Pid -> Pid -> Process ()
consumer m counterPid masterPid = do
    myPid <- getSelfPid
    replicateM_ m $ do
        say' $ "Sending 'Inc' Message to counter " ++ show counterPid
        send counterPid $ Inc myPid
        msg <- receive
        case msg of
            Report x -> say' $ "Got 'Report " ++ show x ++ "' Message from counter"
    say' "Sending 'Done' Message to master"
    send masterPid Done
    say' "Exiting"
  where
    say' = taggedSay "Consumer"

counter :: Int -> Process ()
counter n = do
    msg <- receive
    case msg of
        Done -> say' "Received termination signal, exiting"
        Inc pid -> do
            say' $ "Got 'Inc' Message from " ++ show pid
            send pid $ Report (n + 1)
            say' $ "Sending current state (" ++ show (n + 1) ++ ") to " ++ show pid
            counter (n + 1)
  where
    say' = taggedSay "Counter"

main :: IO ()
main = do
    [n,m,verbal] <- getArgs
    mkSystem () (master (read n) (read m)) (customLogger' verbal)
  where
    customLogger' verbal = case verbal of
      "silent" -> silentLogger
      "verbal" -> verbalLogger
