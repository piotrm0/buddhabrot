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
import System.IO
--import Data.Complex
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
import System.Random

import Data.IORef

import Text.Printf

import Par

import Ui
import Gl

import qualified Complex as C

import qualified Control.Concurrent.Thread.Group as Thread

import Graphics.Rendering.OpenGL.GL.VertexArrays (DataType (Short))

data GfxState =
    GfxState {
      ui :: UiOps,
      width :: CInt,
      height :: CInt,
      shaders :: ShaderProgram,
      tex :: TextureObject,
      buff :: BufferObject,
      texAccess :: MVar (),
      counts :: ST.IOVector Word16,
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

layers = 4 -- must be 4 right now
channelsize = 2
maxint = (fromIntegral (maxBound :: Word16)) :: GLfloat

syncTex state = do
    max0 <- newIORef 0
    max1 <- newIORef 0
    max2 <- newIORef 0
--   max3 <- newIORef 0
    printf "syncing texture\n"
    forM_ [0..(width state) * (height state) - 1] $ \i -> do
      v0 <- ST.read (counts state) ((fromIntegral i) * layers)
      v1 <- ST.read (counts state) ((fromIntegral i) * layers + 1)
      v2 <- ST.read (counts state) ((fromIntegral i) * layers + 2)
--      ST.write (counts state) ((fromIntegral i) * layers + 3) (maxBound :: Word16)
--      v3 <- ST.read (counts state) ((fromIntegral i) * layers + 3)
      modifyIORef' max0 $ \current -> if v0 > current then v0 else current
      modifyIORef' max1 $ \current -> if v1 > current then v1 else current
      modifyIORef' max2 $ \current -> if v2 > current then v2 else current
--      modifyIORef' max3 $ \current -> if v3 > current then v3 else current
    max0v <- readIORef max0
    max1v <- readIORef max1
    max2v <- readIORef max2
--    max3v <- readIORef max3
    printf "max0v = %s\n" (show max0v)
    printf "max1v = %s\n" (show max1v)
    printf "max2v = %s\n" (show max2v)
--    printf "max3v = %s\n" (show max3v)
    withGL state $
           do bindBuffer PixelUnpackBuffer $= Just (buff state)
              texImage2D Texture2D NoProxy 0 RGBA'
                           (TextureSize2D (width state) (height state)) 0
                           (PixelData RGBA Short nullPtr)
              setUniform (shaders state) "scale0" (((fromIntegral (max0v) :: GLfloat) / maxint))
              setUniform (shaders state) "scale1" (((fromIntegral (max1v) :: GLfloat) / maxint))
              setUniform (shaders state) "scale2" (((fromIntegral (max2v) :: GLfloat) / maxint))
--              setUniform (shaders state) "scale3" (((fromIntegral max3v) :: GLfloat) / maxint)
    juicySaveImage "01.png" (fromIntegral (width state)) (fromIntegral (height state)) (counts state)
    --    threadDelay 1000000
    --    error "hellos"
    --bindBuffer PixelUnpackBuffer $= Nothing

asyncTex state = do
  threadDelay 5000000
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
           [-1.0, -1.0, 1.0, 0.0 :: GLfloat,
            -1.0,  1.0, 0.0, 0.0,
             1.0, -1.0, 1.0, 1.0,
             1.0,  1.0, 0.0, 1.0]

  checkErrors "loading arraybuffer"

  prog <- simpleShaderProgram "buddha.vert" "buddha.frag"
  checkErrors "loading shaders"

  let p = program prog 
  currentProgram $= Just p
  checkErrors "setting currentProgram"

  myVAO <- makeVAO $ do
    enableAttrib prog "position"
    enableAttrib prog "texCoordIn"
    checkErrors "enablingAttrib position"

    bindBuffer ArrayBuffer $= Just (myVBO)
    checkErrors "binding ArrayBuffer"

    let stride = fromIntegral $ sizeOf (undefined::GLfloat) * 4
    setAttrib prog "position" ToFloat $ VertexArrayDescriptor 2 Float stride offset0
    setAttrib prog "texCoordIn" ToFloat $ VertexArrayDescriptor 2 Float stride (offset (2 * 4))
    checkErrors $ "setting attribute parameters"

    setUniform prog "tex" (0 :: Level)
    checkErrors $ "setting uniform"

  clearColor $= Color4 0.0 0.0 0.5 1.0

  pxBuff <- genObjectName
  bindBuffer PixelUnpackBuffer $= Just pxBuff
  bufferData PixelUnpackBuffer $= (fromIntegral (channelsize * w * h * layers), nullPtr, StreamCopy)
  checkErrors "creating pixel buffer name"
 
  Just ptr <- mapBufferRange PixelUnpackBuffer 0 (fromIntegral (channelsize * w * h * layers)) [Write, Unsychronized]
  checkErrors "mapping buffer"

  unmapBuffer PixelUnpackBuffer
  checkErrors "unmapping buffer"

  fPtr <- newForeignPtr_ ptr
  --counts <- ST.new (width * height)
  let counts = ST.unsafeFromForeignPtr0 fPtr (fromIntegral (w * h * layers)) 

  TextureObject texid <- genObjectName
  checkErrors "creating texture objectName"
  let tex = TextureObject texid
  textureBinding Texture2D $= Just tex
  textureFilter Texture2D $= ((Linear', Nothing), Linear')
  checkErrors "binding texture"
  
  checkErrors "texImage2d"

  bindBuffer PixelUnpackBuffer $= Nothing
  checkErrors "unbindBuffer"

  async $ do
    --startFillMandelDet (fromIntegral w) (fromIntegral h) 1.0 counts
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

  fid <- forkIO $ do asyncTex state

  handlerResize state (fromIntegral w) (fromIntegral h)

doPoint itermin itermax layer dat (screenToView, viewToData) (x, y) = do
  mandelMap itermin itermax (x, y) $ \(C.C x y) ->
      let dataPos = viewToData (x, y) in
      case dataPos of
        Just p -> do
            old_temp <- ST.read dat (p+layer)
            ST.write dat (p+layer) (old_temp + 1)
        Nothing ->
            return ()

fillMandel w h dat =
    let vieww = 2.5 * 1.0 in
    let viewh = 2.2 * 1.0 in
    let centerx = -0.4 in
    let centery = 0.0 in
    let maps = makeMaps w h centerx centery vieww viewh in

    let randStart (xa : ya : rnd) = do
          let x = (xa - 0.5) * vieww * 1.0 + centerx
          let y = (ya - 0.5) * viewh * 1.0 + centery
          let q = (x - 1/4) ** 2 + y ** 2
          let cond1 = q * (q + (x-1/4)) < (y ** 2) / 4
          let cond2 = ((x+1) ** 2) + (y ** 2) < (1/16)
          if not (cond1 || cond2) then return (rnd, (x,y)) else randStart rnd in

    let enqueueSome q itermin itermax layer howmany = do
          rndInt <- randomIO :: IO (Int)
          let rnd = (randoms (mkStdGen rndInt)) :: [GLfloat]
          enqueue q $ do
            foldM_ (\rnd _ -> do
                        (rnd, (x,y)) <- randStart rnd
                        doPoint itermin itermax layer dat maps (x,y)
                        return rnd)
                   rnd [1..howmany] in

    do q <- newQueue
       forM_ [1..cores] $ \_ -> do enqueueSome q 100 100 0 (1024 * 1024 * 1024)
       tg <- runQueueAsync 1 q
       return ()

startFillMandelDet w h delta dat = do
    tg <- fillMandelDet w h delta dat 
    Thread.wait tg
    printf "delta %s done\n" (show delta)
    startFillMandelDet w h (delta * 0.5) dat

fillMandelDet w h delta dat =
    let vieww = 2.5 * 1.0 in
    let viewh = 2.2 * 1.0 in
    let centerx = -0.4 in
    let centery = 0.0 in
    let minx = centerx - 0.5 * vieww in
    let maxx = centerx + 0.5 * vieww in
    let miny = centery - 0.5 * viewh in
    let maxy = centery + 0.5 * viewh in

    let maps = makeMaps w h centerx centery vieww viewh in
    let (screenToView, viewToData) = maps in

    let in_bounds (x,y) =
            let q = (x - 1/4) ** 2 + y ** 2 in
            let cond1 = q * (q + (x-1/4)) < (y ** 2) / 4 in
            let cond2 = ((x+1) ** 2) + (y ** 2) < (1/16) in
            not (cond1 || cond2) in

    let enqueueSome q itermin itermax layer aminy amaxy = do
          enqueue q $ do
            forM_ [aminy, aminy+delta .. amaxy] $
                      \y -> do forM_ [minx, minx+delta .. maxx] $
                                          \x -> if in_bounds (x,y)
                                                then doPoint itermin itermax layer dat maps (x,y)
                                                else return ()
            --printf "finished yrange [%s,%s] for delta %s\n" (show aminy) (show amaxy) (show delta)
    in
  
    let rows_per_job = (viewh / ((fromIntegral cores) * 4)) :: CFloat in

    do q <- newQueue
       forM_ [miny,miny+rows_per_job .. maxy] $ \y -> do enqueueSome q 20 20 0 y (y + rows_per_job)
       tg <- runQueueAsync 1 q
       return tg

makeMaps:: CInt -> CInt -> GLfloat -> GLfloat -> GLfloat -> GLfloat ->
           ((GLfloat, GLfloat) -> (GLfloat, GLfloat),
            (GLfloat, GLfloat) -> Maybe Int)
makeMaps screenw screenh viewx viewy vieww viewh =
    let screenwf = (fromIntegral screenw) :: GLfloat in
    let screenhf = (fromIntegral screenh) :: GLfloat in
    let screenToView (xa, ya) =
            let x = (vieww * ((xa / screenwf) - 0.5)) + viewx in
            let y = (viewh * ((ya / screenhf) - 0.5)) + viewy in
            (x, y) in
    let viewToData (x, y) =
            let xi = (floor (((x - viewx) / vieww + 0.5) * screenwf)) :: CInt in
            let yi = (floor (((y - viewy) / viewh + 0.5) * screenhf)) :: CInt in
            let dataPos = (fromIntegral $ screenw * yi + xi) * layers in
            if (xi >= 0) && (xi < screenw) && (yi >= 0) && (yi < screenh)
            then Just dataPos
            else Nothing in
    (screenToView, viewToData)

--https://github.com/yogsototh/mandelbrot/blob/master/mandelbrot.hs
escapeOn c z 0 = 0
escapeOn c z n = if (C.magnitude z > 2)
                 then n
                 else escapeOn c ((z*z)+c) (n-1)

mapPoints c z 0 _ = return ()
mapPoints c z n f = do f z
                       mapPoints c ((z*z)+c) (n-1) f

mandelMap itermin itermax (r, i) f = do
--    putStrLn $ "mapping " ++ (show (r,i))
    let c = C.C r i
    let z = C.C 0 0
    let e = (itermax+1) - (escapeOn c z (itermax+1))
    if (e >= itermin) && (e <= itermax) then
        mapPoints c c (e-1) f
    else
        return ()
 
main = do
      startLoop
       (GfxHandlers {handleInit   = handlerInit,
                     handleResize = handlerResize,
                     handleMark   = handlerMark,
                     handleDraw   = handlerDraw})
    where ?log = putStrLn
