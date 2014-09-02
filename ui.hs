{-# LANGUAGE ImplicitParams, RecordWildCards, NoMonomorphismRestriction #-}
module Ui 
    (UiOps(..), startLoop, GfxHandlers(..)) where

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
    UiState {glContext :: SDL.GLContext,
             mainWindow :: Window,
             mainWidth :: CInt,
             mainHeight :: CInt}

data UiOps =
    UiOps {takeContext :: IO (),
           swapBuffers :: IO ()}

data GfxHandlers s =
    GfxHandlers {handleInit :: CInt -> CInt -> UiOps -> IO (s),
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
          delay (fromIntegral 50)
          loop glHandlers ((handleMark glHandlers) glState 1.0) uiState
        again = do
          loop glHandlers glState uiState

createGLContext :: SDL.Window -> IO (SDL.GLContext)
createGLContext w = do
    SDL.glSetAttribute glAttrContextMajorVersion 3 -- 4
    SDL.glSetAttribute glAttrContextMinorVersion 2 -- 1
    SDL.glSetAttribute glAttrContextProfileMask glProfileCore
    SDL.glSetAttribute glAttrDoubleBuffer 1
    SDL.glSetAttribute glAttrDepthSize 24
    SDL.glSetAttribute glAttrShareWithCurrentContext 1
    context <- glCreateContext w
    if nullPtr == context then
        error "could not create gl context"
    else do return context

takeGLContext uistate = do
  SDL.glMakeCurrent (mainWindow uistate) (glContext uistate)
  return ()

initUI =
    let initWidth = 1200 :: CInt in
    let initHeight = 1024 :: CInt in

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

      context <- createGLContext window

      let uistate = UiState {glContext = context,
                             mainWindow = window,
                             mainWidth = initWidth,
                             mainHeight = initHeight}

      return uistate

swapGLBuffers uistate =
    do glSwapWindow (mainWindow uistate)

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
  glstate <- (handleInit glhandlers)
             (mainWidth uistate)
             (mainHeight uistate)
             (UiOps {takeContext = takeGLContext uistate,
                     swapBuffers = swapGLBuffers uistate})
  loop glhandlers glstate uistate
  where ?log = putStrLn
