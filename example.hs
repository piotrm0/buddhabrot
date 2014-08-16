-- adapted from https://gist.github.com/uhef/be090579be5cc5fb0db1

module Main where
 
import Graphics.Rendering.OpenGL
import Graphics.UI.SDL as SDL
import System.Environment
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Storable
import Data.Bits
 
main = do
  SDL.init initFlagVideo

  progname <- getProgName
  prognamecstring <- newCString progname
  window <- createWindow prognamecstring
            windowPosUndefined windowPosUndefined
            width height
           $ windowFlagOpenGL .|. windowFlagResizable

  glCreateContext window

  depthFunc $= Just Less

  clearColor $= Color4 0 0 0 1

  let (w,h) = (fromIntegral width,fromIntegral height)
  viewport $= (Position 0 0,Size w h)

  matrixMode $= Projection
  loadIdentity

  perspective 45 (fromIntegral width / fromIntegral height) 0.1 100

  matrixMode $= Modelview 0

  mainLoop window

  where width = 640
        height = 480
 
mainLoop :: Window -> IO (Bool)
mainLoop window = do
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
          drawGLScreen window
          delay (fromIntegral 50)
          mainLoop window
        again = do
          mainLoop window

pollAnEvent :: IO (Maybe Event)
pollAnEvent = alloca doPoll
    where doPoll event = do
            retVal <- pollEvent event
            case retVal of
              1 -> peek event >>= (\e -> return $ Just e)
              _ -> return Nothing

drawGLScreen window = do
  clear [ColorBuffer,DepthBuffer]
  loadIdentity
  color $ Color3 0.8 0.4 (0.9 :: GLfloat)
  scale 0.7 0.7 (0.7 :: GLfloat)
  cube 0.3
  glSwapWindow window
 
cube :: GLfloat -> IO ()
cube w = renderPrimitive Quads $ mapM_ vertex3f
  [ ( w, w, w), ( w, w,-w), ( w,-w,-w), ( w,-w, w),
    ( w, w, w), ( w, w,-w), (-w, w,-w), (-w, w, w),
    ( w, w, w), ( w,-w, w), (-w,-w, w), (-w, w, w),
    (-w, w, w), (-w, w,-w), (-w,-w,-w), (-w,-w, w),
    ( w,-w, w), ( w,-w,-w), (-w,-w,-w), (-w,-w, w),
    ( w, w,-w), ( w,-w,-w), (-w,-w,-w), (-w, w,-w) ]
 
vertex3f :: (GLfloat, GLfloat, GLfloat) -> IO ()
vertex3f (x, y, z) = vertex $ Vertex3 x y z
