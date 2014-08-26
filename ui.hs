{-# LANGUAGE ImplicitParams, RecordWildCards, NoMonomorphismRestriction #-}
module Ui 
    (startLoop, GfxHandlers(..)) where

--import Control.Monad
--import Control.Applicative 
--import Data.Function
--import Data.Array.Storable
--import Foreign (with, nullPtr, toBool)
import Graphics.UI.SDL as SDL
import Graphics.Rendering.OpenGL.Raw.Types
--import Graphics.Rendering.OpenGL
import System.Environment

import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Storable

import Data.Bits ((.|.))

data UiState =
    UiState {mainWindow :: Window,
             mainWidth :: CInt,
             mainHeight :: CInt}

data GfxHandlers s =
    GfxHandlers {handleInit :: CInt -> CInt -> IO (s),
                handleMark :: s -> GLfloat -> s,
                handleDraw :: s -> IO ()}

loop :: GfxHandlers s -> s -> UiState -> IO (Bool)
loop glHandlers glState uiState = do
  event <- pollAnEvent
  case event of
    Just (QuitEvent _ _) -> 
        do 
          putStrLn "quitting from window"
          return True
    Just (KeyboardEvent _ _ _ _ _ (Keysym 41 27 _)) ->
        do
          putStrLn "quitting from keyboard"
          return True
    Just (MouseMotionEvent _ _ _ _ _ _ _ _ _) -> do again
    Just (TouchFingerEvent _ _ _ _ _ _ _ _ _) -> do again
    Just (MultiGestureEvent _ _ _ _ _ _ _ _) -> do again
    Just e ->
        do
          putStrLn $ "event: " ++ (show e)
          again
    Nothing -> do redraw
  where redraw = do
          (handleDraw glHandlers) glState 
          glSwapWindow (mainWindow uiState)
          delay (fromIntegral 50)
          loop glHandlers ((handleMark glHandlers) glState 1.0) uiState
        again = do
          loop glHandlers glState uiState

initUI =
    let initWidth = 640 in
    let initHeight = 480 in

    do
      ?log "initing SDL"
      SDL.init initFlagVideo

      progname <- getProgName
      prognamecstring <- newCString progname
      window <- createWindow prognamecstring
                windowPosUndefined windowPosUndefined
                initWidth initHeight
               $ windowFlagOpenGL .|. windowFlagResizable

      ?log "creating GL context"
      glCreateContext window

      return $ UiState {mainWindow = window,
                        mainWidth = initWidth,
                        mainHeight = initHeight}

pollAnEvent :: IO (Maybe Event)
pollAnEvent = alloca doPoll
    where doPoll event = do
            retVal <- pollEvent event
            case retVal of
              1 -> peek event >>= (\e -> return $ Just e)
              _ -> return Nothing

startLoop glhandlers = do
  ?log "starting loop"
  uistate <- initUI
  glstate <- (handleInit glhandlers) 640 480
  loop glhandlers glstate uistate
  where ?log = putStrLn
