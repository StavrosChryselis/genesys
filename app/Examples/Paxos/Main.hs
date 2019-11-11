import Control.Monad (replicateM, forM_)

import Genesys
import Genesys.Backend.Threaded

import Examples.Paxos.Message

type Process = Command (PaxosMessage Int) ()

master :: Int -> Int -> Process ()
master a p = do
  masterPid <- getSelfPid
  acceptorPids <- replicateM a $ spawn $ undefined
  forM_ [1..p] $ \cmd -> spawn $ proposer acceptorPids (a`div`2+1) cmd

acceptor :: Pid -> Ticket -> Maybe Int -> Process ()
acceptor masterPid highestTicket (Just cmd) = undefined
  where
    say' = taggedSay "Acceptor"

proposer :: [Pid] -> Int -> Int -> Process ()
proposer acceptorPids majority cmd = undefined
  where
    say' = taggedSay "Proposer"

main :: IO ()
main = pure ()
