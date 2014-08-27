{-# LANGUAGE ImplicitParams, RecordWildCards, NoMonomorphismRestriction #-}
module Ui 
    (startLoop, GfxHandlers(..)) where

import Graphics.UI.SDL as SDL
import Graphics.Rendering.OpenGL.Raw.Types
import System.Environment

import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr

import Data.Bits ((.|.))

data UiState =
    UiState {mainWindow :: Window,
             mainWidth :: CInt,
             mainHeight :: CInt}

data GfxHandlers s =
    GfxHandlers {handleInit :: CInt -> CInt -> IO (s),
                 handleResize :: s -> GLint -> GLint -> IO (s),
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
    Just (WindowEvent 512 _ _ 6 w h) ->
        do
          putStrLn $ "resizing to " ++ (show w) ++ "," ++ (show h)
          newstate <- (handleResize glHandlers) glState (fromIntegral w) (fromIntegral h)
          loop glHandlers newstate uiState
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

      SDL.glSetAttribute glAttrContextMajorVersion 3 -- 4
      SDL.glSetAttribute glAttrContextMinorVersion 2 -- 1
      SDL.glSetAttribute glAttrContextProfileMask glProfileCore

      SDL.glSetAttribute glAttrDoubleBuffer 1
      SDL.glSetAttribute glAttrDepthSize 24

      context <- glCreateContext window

      if nullPtr == context then
          error "could not create gl context"
      else do
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
