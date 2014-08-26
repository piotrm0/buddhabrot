{-# LANGUAGE ImplicitParams, RecordWildCards, NoMonomorphismRestriction #-}
-- adapted from https://gist.github.com/uhef/be090579be5cc5fb0db1

module Main where

--import Control.Monad
--import Control.Applicative 
--import Data.Function
--import Data.Array.Storable
--import Foreign (with, nullPtr, toBool)
--import Foreign.C.String
--
import Graphics.Rendering.OpenGL
--import Graphics.Rendering.OpenGL.GL.Shaders.ProgramObjects
--import Graphics.Rendering.OpenGL.GL.VertexArrays
--import System.Environment
--import Foreign.C.Types
--import Foreign.Marshal.Alloc
--import Foreign.Storable
--import Data.Bits ((.|.))

import Ui

data GlState =
    GlState {rot1 :: GLfloat,
             rot2 :: GLfloat}

handlerMark :: GlState -> GLfloat -> GlState
handlerMark state dt =
    state {rot1 = (rot1 state) + 1.0 * dt,
           rot2 = (rot2 state) + 1.1 * dt}

handlerDraw state = do
  clear [ColorBuffer,DepthBuffer]
  loadIdentity
  color $ Color3 0.8 0.4 (0.9 :: GLfloat)
  scale 0.7 0.7 (0.7 :: GLfloat)
  translate $ Vector3 0.0 0.0 (-10.0 :: GLfloat)
  rotate (rot1 state) $ Vector3 0 0 1
  rotate (rot2 state) $ Vector3 0 1 0
  cube 1.0
  return ()

handlerInit w h = do
  viewport $= (Position 0 0, Size w h)
  matrixMode $= Projection
  loadIdentity
  perspective 45 ((fromIntegral w) / (fromIntegral h)) 0.1 100
  matrixMode $= Modelview 0
  return $ GlState {rot1 = 0.0,
                    rot2 = 0.0}
 
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

main = do
      startLoop
       (GfxHandlers {handleInit = handlerInit,
                     handleMark = handlerMark,
                     handleDraw = handlerDraw})
    where ?log = putStrLn
