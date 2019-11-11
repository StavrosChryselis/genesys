{-# LANGUAGE FlexibleInstances #-}

module Genesys ( Command(..)
               , CommandOp(..)
               , Pid
               , LogString
               , Free(..)
               , getSelfPid
               , send
               , receive
               , liftIO
               , spawn
               , getState
               , putState
               , say
               , taggedSay ) where

import Control.Monad.Free (Free, liftF)
import Control.Monad.IO.Class

type Pid = Int
type LogString = String

data CommandOp msg st a =
    Self (Pid -> a)
  | Send Pid msg a
  | Receive (msg -> a)
  | Lift (IO a)
  | Spawn (Command msg st ()) (Pid -> a)
  | Get (st -> a)
  | Put st a
  | Say LogString a

instance Functor (CommandOp msg st) where
    fmap f (Self g) = Self $ f . g
    fmap f (Send pid msg a) = Send pid msg (f a)
    fmap f (Receive g) = Receive $ f . g
    fmap f (Lift action) = Lift $ fmap f action
    fmap f (Spawn cmd g) = Spawn cmd (f . g)
    fmap f (Get g) = Get $ f . g
    fmap f (Put st a) = Put st (f a)
    fmap f (Say clog a) = Say clog (f a)

type Command msg st = Free (CommandOp msg st)

instance MonadIO (Free (CommandOp msg st)) where
  liftIO action = liftF $ Lift action

getSelfPid :: Command msg st Pid
getSelfPid = liftF $ Self id

send :: Pid -> msg -> Command msg st ()
send pid msg = liftF $ Send pid msg ()

receive :: Command msg st msg
receive = liftF $ Receive id

spawn :: Command msg st () -> Command msg st Pid
spawn childCommand = liftF $ Spawn childCommand id

getState :: Command msg st st
getState = liftF $ Get id

putState :: st -> Command msg st ()
putState st = liftF $ Put st ()

say :: LogString -> Command msg st ()
say clog = liftF $ Say clog ()

taggedSay :: LogString -> LogString -> Command msg st ()
taggedSay tag clog = liftF $ Say ("[" ++ tag ++ "] " ++ clog) ()
