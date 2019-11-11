module Examples.Paxos.Message where

import Genesys
import Genesys.Backend.Threaded

type Ticket = Int

data ProposerMessage cmd = Prepare Ticket
                         | AcceptRequest Ticket cmd
  deriving Show

data AcceptorMessage cmd = Promise Ticket
                         | Ignore
                         | Accept Ticket cmd
                         | Executed cmd
  deriving Show

data PaxosMessage cmd = Either (ProposerMessage cmd) (AcceptorMessage cmd)
  deriving Show
