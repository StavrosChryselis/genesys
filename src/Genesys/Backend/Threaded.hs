module Genesys.Backend.Threaded  ( silentLogger
                                 , verbalLogger
                                 , fileLogger
                                 , mkSystem ) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad (forever)
import Control.Monad.Free (Free(..))
import Control.Monad.IO.Class (MonadIO)
import Data.IntMap (IntMap)
import qualified Data.IntMap as M

import Genesys
import Genesys.Backend.Logger

data ThreadedEnv msg = ThreadedEnv {
    pidCounter :: TVar Pid
  , mailboxes :: TVar (IntMap (TChan msg))
  , asyncs :: TChan (Async ())
}

data ProcessEnv msg st = ProcessEnv {
    pid :: Pid
  , mailbox :: TChan msg
  , localState :: st
  , cachedMailboxes :: IntMap (TChan msg)
  , logger :: Logger
}

mkThreadedEnv :: IO (ThreadedEnv msg)
mkThreadedEnv = do
   pidCounter' <- newTVarIO 0
   mailboxes' <- newTVarIO M.empty
   asyncs' <- newTChanIO
   pure $ ThreadedEnv {
      pidCounter = pidCounter'
    , mailboxes = mailboxes'
    , asyncs = asyncs'
   }

mkProcessEnv :: ThreadedEnv msg -> st -> Logger -> IO (ProcessEnv msg st)
mkProcessEnv ThreadedEnv{..} st logger' = do
    pid' <- atomically $ stateTVar pidCounter $ \counter -> (counter,counter+1)
    broadcastMailbox <- newBroadcastTChanIO
    mailbox' <- atomically $ dupTChan broadcastMailbox
    atomically $ modifyTVar mailboxes $ M.insert pid' broadcastMailbox
    pure $ ProcessEnv {
        pid = pid'
      , mailbox = mailbox'
      , localState = st
      , cachedMailboxes = M.empty
      , logger = logger'
    }

mkProcess :: ThreadedEnv msg -> ProcessEnv msg st -> Command msg st () -> IO ()
mkProcess _ _ (Pure ()) = pure ()
mkProcess env penv@ProcessEnv{..} (Free cmd) = case cmd of
  Self cont -> mkProcess env penv $ cont pid
  Send pid msg cont -> do
    cachedMailboxes' <- dispatch pid msg env penv
    mkProcess env penv{ cachedMailboxes = cachedMailboxes' } cont
  Receive cont -> do
    msg <- liftIO $ atomically $ readTChan mailbox
    mkProcess env penv $ cont msg
  Lift action -> action >>= mkProcess env penv
  Spawn child cont -> do
    pid' <- spawner env penv child
    mkProcess env penv $ cont pid'
  Get cont -> mkProcess env penv $ cont localState
  Put st cont -> mkProcess env penv{ localState = st } cont
  Say logString cont -> logger ("[" ++ show pid ++ "]" ++ logString) >> mkProcess env penv cont


dispatch :: Pid -> msg -> ThreadedEnv msg -> ProcessEnv msg st -> IO (IntMap (TChan msg))
dispatch pid' msg env@ThreadedEnv{..} penv@ProcessEnv{..} = case M.lookup pid' cachedMailboxes of
    Just mailbox' -> do
        atomically $ writeTChan mailbox' msg
        pure cachedMailboxes
    Nothing -> do
        mailbox' <- liftIO $ atomically $ stateTVar mailboxes $ \memo -> case M.lookup pid' memo of
          Just mailbox'' -> (mailbox'',memo)
          Nothing -> error "Uknown Pid"
        dispatch pid' msg env penv { cachedMailboxes = M.insert pid' mailbox' cachedMailboxes }

spawner :: ThreadedEnv msg -> ProcessEnv msg st -> Command msg st () -> IO Pid
spawner env@ThreadedEnv{..} penv cmd = do
  penv' <- mkProcessEnv env (localState penv) (logger penv)
  asyncProcess <- async $ mkProcess env penv' cmd
  atomically $ writeTChan asyncs asyncProcess
  pure $ pid penv'

mkSystem :: st -> Command msg st () -> Logger -> IO ()
mkSystem st cmd logger' = do
  env <- mkThreadedEnv
  penv <- mkProcessEnv env st logger'
  spawner env penv cmd
  waitAsyncs $ asyncs env

waitAsyncs :: TChan (Async ()) -> IO ()
waitAsyncs asyncs = do
  a <- liftIO $ atomically $ tryReadTChan asyncs
  case a of
    Nothing -> pure ()
    Just asyncProcess -> wait asyncProcess >> waitAsyncs asyncs
