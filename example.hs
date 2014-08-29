{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
-- adapted from https://gist.github.com/uhef/be090579be5cc5fb0db1

module Main where

--import Data.ObjectName (genObjectNames)
--import Graphics.Rendering.OpenGL.GL.Texturing.Objects (textureBinding)
--import Graphics.Rendering.OpenGL.GL.Texturing.Specification (TextureTarget2D(..))

import qualified Codec.Picture.Types as JuicyTypes
import qualified Data.Vector.Storable.Mutable as ST
import Data.Int
import Foreign
import Control.Monad.Primitive
import Control.Monad
import Data.Complex
import Graphics.Rendering.OpenGL
import Graphics.GLUtil (makeBuffer,
                        getAttrib,setAttrib,enableAttrib,
                        ShaderProgram,simpleShaderProgram,program,
                        makeVAO,withVAO,
                        setUniform)

import Foreign.Storable
import Foreign.C.Types

import Par

import Ui
import Gl

data GfxState =
    GfxState {shaders :: ShaderProgram,
              tex :: TextureObject,
--              counts :: ST.STVector Int32 (ForeignPtr Int32),
              counts :: ST.IOVector CInt,
              vao_quad :: VertexArrayObject}

handlerMark :: GfxState -> GLfloat -> GfxState
handlerMark state dt = state

handlerDraw state = do
  clear [ColorBuffer,DepthBuffer]
  checkErrors "clearing buffers"

  let prog = (shaders state)
  let p = program prog 

  withVAO (vao_quad state) $ do
    currentProgram $= Just p
    checkErrors "setting currentProgram"

    --textureBinding Texture2D $= Just (tex state)
    --checkErrors "binding texture"

    --let TextureObject texId = (tex state)
    setUniform prog "tex" (0 :: Level)
    checkErrors $ "setting uniform"

    drawArrays TriangleStrip 0 4
    checkErrors "drawing array"

  return ()

handlerResize state w h = do
  viewport $= (Position 0 0, Size w h)
  checkErrors "seting up view"
  return state

width = 640
height = 480

handlerInit w h = do
  myVBO <- makeBuffer ArrayBuffer
           [-1.0, -1.0 :: GLfloat, --, 0, 0,  :: GLfloat,
            -1.0,  1.0, -- 0, 1, 
             1.0, -1.0, --1, 0,
             1.0,  1.0] --,1, 1]

  checkErrors "loading arraybuffer"

  prog <- simpleShaderProgram "simple.vert" "simple.frag"
  checkErrors "loading shaders"

  myVAO <- makeVAO $ do
    enableAttrib prog "position"
    checkErrors "enablingAttrib position"

    bindBuffer ArrayBuffer $= Just (myVBO)
    checkErrors "binding ArrayBuffer"

    let stride = fromIntegral $ sizeOf (undefined::GLfloat) * 2
    setAttrib prog "position" ToFloat $ VertexArrayDescriptor 2 Float stride offset0

  clearColor $= Color4 0.0 0.0 0.5 1.0

  TextureObject texid <- genObjectName
  checkErrors "creating texture objectName"

  let tex = TextureObject texid

  textureBinding Texture2D $= Just tex
  checkErrors "binding texture"
  
  textureFilter Texture2D $= ((Nearest, Just Nearest), Nearest)

--  counts <- JuicyTypes.createMutableImage 640 480 (JuicyTypes.PixelRGBA8 0 0 0 255) 

  counts <- ST.new (width * height)
  forM_ [0..width * height - 1] $ \i -> ST.write counts i 0
  fillMandel counts
--  error "done"

  ST.unsafeWith counts $ \ptr -> do
      texImage2D Texture2D NoProxy 0 RGBA'
                     (TextureSize2D width height) 0
                     (PixelData RGBA UnsignedByte ptr)

      generateMipmap' Texture2D

      let state = GfxState {shaders = prog,
                            counts = counts,
                            tex = tex,
                            vao_quad = myVAO}

      handlerResize state w h

--fillMandel :: ST.MVector (PrimState IO) Int32 -> ST.MVector (PrimState IO) Int32

doCol dat x = 
    forM_ [0..height-1] $ \y -> do
      let vecPos = width * x + y
      let xf = ((fromIntegral x) / (width / 2)) - 1.0
      let yf = ((fromIntegral y) / (height / 2)) - 1.0
      let m = mandel (Vertex2 xf yf)
      ST.write dat vecPos (fromIntegral m)

fillMandel dat = do
  forMrange_ 0 (width-1) $ \x -> doCol dat x
              
iterations = 10
mandel (Vertex2 r i) = length . takeWhile (\z -> magnitude z <= 2) .
                       take iterations $ iterate (\z -> z^2 + (r :+ i)) 0
 
main = do
      startLoop
       (GfxHandlers {handleInit   = handlerInit,
                     handleResize = handlerResize,
                     handleMark   = handlerMark,
                     handleDraw   = handlerDraw})
    where ?log = putStrLn
