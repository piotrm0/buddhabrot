module Par where

import GHC.Conc (numCapabilities)
import Control.Concurrent
import qualified Control.Concurrent.Thread.Group as Thread
import Control.Monad

cores = numCapabilities

doSome :: Int -> Int -> Int -> Thread.ThreadGroup -> (Int -> IO a) -> IO ()
doSome ci cmax cinc tg f =
    if ci < cmax then do
        Thread.forkIO tg $ do
          doSome (ci + cinc) cmax cinc tg f
          forM_ [ci..ci+cinc-1] f
        return ()
    else return ()

forMrange_ :: Int -> Int -> (Int -> IO a) -> IO ()
forMrange_ i j f =
    let jobs = (j - i + 1) :: Int in
    let each = max 1 (quot jobs cores) in
    do
      putStrLn $ (show jobs) ++ " " ++ (show each)
      tg <- Thread.new
      doSome i j each tg f
      Thread.wait tg

forMrangeAsync_ :: Int -> Int -> (Int -> IO a) -> IO (Thread.ThreadGroup)
forMrangeAsync_ i j f =
    let jobs = (j - i + 1) :: Int in
    let each = max 1 (quot jobs cores) in
    do
      putStrLn $ (show jobs) ++ " " ++ (show each)
      tg <- Thread.new
      doSome i j each tg f
      return tg

    
            
          


