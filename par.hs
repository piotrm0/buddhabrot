module Par where

import GHC.Conc (numCapabilities)
import Control.Concurrent
import qualified Control.Concurrent.Thread.Group as Thread
import Control.Monad
import Data.IORef

cores = numCapabilities

--doSome :: Int -> Int -> Int -> Thread.ThreadGroup -> (Int -> IO a) -> IO ()
--doSome ci cmax cinc tg f =
--    if ci < cmax then do
--        Thread.forkIO tg $ do
--          doSome (ci + cinc) cmax cinc tg f
--          forM_ [ci..ci+cinc-1] f
--        return ()
--    else return ()

doFromQueue :: (Queue (IO ())) -> IO ()
doFromQueue q = do
    me <- dequeue q 
    case me of
          Just e -> do 
                  e
                  doFromQueue q
          Nothing -> return ()

forMrange_ :: Int -> Int -> (Int -> IO a) -> IO ()
forMrange_ i j f = do
  tg <- forMrangeAsync_ i j f
  Thread.wait tg

forMrangeAsync_ :: Int -> Int -> (Int -> IO a) -> IO (Thread.ThreadGroup)
forMrangeAsync_ i j f =
    let jobs = (j - i + 1) :: Int in
    let each = max 1 (quot jobs (4 * cores)) in

    do
      q <- newQueue
      addQueue f q i j each

      tg <- Thread.new      
      spawnThreads tg cores $ do doFromQueue q
      return tg

spawnThreads :: Thread.ThreadGroup -> Int -> (IO a) -> IO ()
spawnThreads tg num f = do
  if num > 0 then do
               tid <- Thread.forkIO tg f
               spawnThreads tg (num - 1) f
  else return ()

addQueue :: (Int -> IO a) -> (Queue (IO ())) -> Int -> Int -> Int -> IO ()
addQueue f q ci maxi inci = do
  if ci + inci - 1 <= maxi then do
      enqueue q $ do forM_ [ci..ci+inci-1] f
      addQueue f q (ci + inci) maxi inci
  else do return ()

data Queue a = Queue (IORef [a]) (MVar ())

newQueue :: IO (Queue a)
newQueue = do
  aref <- newIORef []
  amvar <- newMVar () 
  return $ Queue aref amvar

enqueue :: Queue a -> a -> IO ()
enqueue (Queue aref amvar) e =
    withMVar amvar $ \_ -> do
      modifyIORef aref $ \t -> e : t

dequeue :: Queue a -> IO (Maybe a)
dequeue (Queue aref amvar) = do
    ret <- withMVar amvar $ \_ -> do
      ret <- readIORef aref
      modifyIORef aref $ \t ->
          case t of 
            h : tl -> tl
            _ -> t
      return ret
    case ret of
      h : t -> return (Just h)
      [] -> return (Nothing)
