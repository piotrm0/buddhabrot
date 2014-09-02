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

import Control.Concurrent.MVar
import Control.Concurrent (threadDelay, forkIO)

import Par

import Ui
import Gl

data GfxState =
    GfxState {
      ui :: UiOps,
      width :: CInt,
      height :: CInt,
      shaders :: ShaderProgram,
      tex :: TextureObject,
      buff :: BufferObject,
      texAccess :: MVar (),
      counts :: ST.IOVector CInt,
      vao_quad :: VertexArrayObject}

withGL :: GfxState -> IO (a) -> IO (a)
withGL state f = do
  canGo <- takeMVar (texAccess state)
  
  takeContext (ui state)

  ret <- f

  putMVar (texAccess state) ()
  return ret

handlerMark :: GfxState -> GLfloat -> GfxState
handlerMark state dt = state

syncTex state = 
  withGL state $
         do bindBuffer PixelUnpackBuffer $= Just (buff state)
            texImage2D Texture2D NoProxy 0 RGBA'
                           (TextureSize2D (width state) (height state)) 0
                           (PixelData RGBA UnsignedByte nullPtr)
            --bindBuffer PixelUnpackBuffer $= Nothing

asyncTex state = do
  threadDelay 50000
  syncTex state
  asyncTex state

handlerDraw state = do
  withGL state $ do
    clear [ColorBuffer,DepthBuffer]
    checkErrors "clearing buffers"

    let prog = (shaders state)
    let p = program prog 

    withVAO (vao_quad state) $
            do drawArrays TriangleStrip 0 4
               checkErrors "drawing array"

    (swapBuffers (ui state))

    return ()

handlerResize :: GfxState -> CInt -> CInt -> IO (GfxState)
handlerResize state w h = do
  withGL state $ do
    viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
    checkErrors "seting up view"
    return state

handlerInit :: CInt -> CInt -> UiOps -> IO (GfxState)
handlerInit w h uiops = do
  myVBO <- makeBuffer ArrayBuffer
           [-1.0, -1.0 :: GLfloat, --, 0, 0,  :: GLfloat,
            -1.0,  1.0, -- 0, 1, 
             1.0, -1.0, --1, 0,
             1.0,  1.0] --,1, 1]

  checkErrors "loading arraybuffer"

  prog <- simpleShaderProgram "simple.vert" "simple.frag"
  checkErrors "loading shaders"

  let p = program prog 
  currentProgram $= Just p
  checkErrors "setting currentProgram"

  myVAO <- makeVAO $ do
    enableAttrib prog "position"
    checkErrors "enablingAttrib position"

    bindBuffer ArrayBuffer $= Just (myVBO)
    checkErrors "binding ArrayBuffer"

    let stride = fromIntegral $ sizeOf (undefined::GLfloat) * 2
    setAttrib prog "position" ToFloat $ VertexArrayDescriptor 2 Float stride offset0

    setUniform prog "tex" (0 :: Level)
    checkErrors $ "setting uniform"

  clearColor $= Color4 0.0 0.0 0.5 1.0

  pxBuff <- genObjectName
  bindBuffer PixelUnpackBuffer $= Just pxBuff
  bufferData PixelUnpackBuffer $= (fromIntegral (w * h * 4), nullPtr, StreamCopy)
  checkErrors "creating pixel buffer name"
 
  Just ptr <- mapBufferRange PixelUnpackBuffer 0 (fromIntegral (w * h * 4)) [Write, Unsychronized]
  checkErrors "mapping buffer"

  unmapBuffer PixelUnpackBuffer
  checkErrors "unmapping buffer"

  fPtr <- newForeignPtr_ ptr
  --counts <- ST.new (width * height)
  let counts = ST.unsafeFromForeignPtr0 fPtr (fromIntegral (w * h)) 

  TextureObject texid <- genObjectName
  checkErrors "creating texture objectName"
  let tex = TextureObject texid
  textureBinding Texture2D $= Just tex
  textureFilter Texture2D $= ((Linear', Nothing), Linear')
  checkErrors "binding texture"
  
  --ST.unsafeWith counts $ \ptr -> do
  --texImage2D Texture2D NoProxy 0 RGBA'
  --                 (TextureSize2D width height) 0
  --                 (PixelData RGBA UnsignedByte ptr)

  -- texImage2D Texture2D NoProxy 0 RGBA'
  --               (TextureSize2D w h) 0
  --               (PixelData RGBA UnsignedByte nullPtr)
  -- textureBinding Texture2D $= Nothing
  checkErrors "texImage2d"

  bindBuffer PixelUnpackBuffer $= Nothing
  checkErrors "unbindBuffer"

  fillMandel (fromIntegral w) (fromIntegral h) counts
  -- generateMipmap' Texture2D

  texAccess <- newMVar ()

  let state = GfxState {ui = uiops,
                        width = w,
                        height = h,
                        shaders = prog,
                        counts = counts,
                        buff = pxBuff,
                        tex = tex,
                        texAccess = texAccess,
                        vao_quad = myVAO}

  fid <- forkIO $ do
           --ignore <- createGLContext uistate
           --copyGLContext (mainWindow uistate) (glContext uistate)
           asyncTex state

  handlerResize state (fromIntegral w) (fromIntegral h)

doCol w h wi hi dat y yi = 
    let vecPos = fromIntegral (wi * yi) in
    let yf = 2 * y / h - 1.0 in
    forM_ [0..(fromIntegral wi)-1] $ \xi -> do
      let xf = 2 * (fromIntegral xi) / w - 1.5
      let m = mandel (Vertex2 xf yf)
      ST.write dat (vecPos+xi) (fromIntegral m)

fillMandel w h dat = do
  forMrangeAsync_ 0 (h-1) $ \y -> doCol
                                  (fromIntegral w) (fromIntegral h)
                                  w h
                                  dat
                                  (fromIntegral y) (fromIntegral y)
              
iterations = 20
mandel (Vertex2 r i) = length . takeWhile (\z -> magnitude z <= 2) .
                       take iterations $ iterate (\z -> z^2 + (r :+ i)) 0
 
main = do
      startLoop
       (GfxHandlers {handleInit   = handlerInit,
                     handleResize = handlerResize,
                     handleMark   = handlerMark,
                     handleDraw   = handlerDraw})
    where ?log = putStrLn
