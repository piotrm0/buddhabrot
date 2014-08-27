{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
-- adapted from https://gist.github.com/uhef/be090579be5cc5fb0db1

module Main where

import Graphics.Rendering.OpenGL
import Graphics.GLUtil (makeBuffer,
                        getAttrib,setAttrib,enableAttrib,
                        ShaderProgram,simpleShaderProgram,program,
                        makeVAO,withVAO,
                        setUniform)

import Foreign.Storable

import Ui
import Gl

data GfxState =
    GfxState {shaders :: ShaderProgram,
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

    drawArrays TriangleStrip 0 4
    checkErrors "drawing array"

  return ()

handlerResize state w h = do
  viewport $= (Position 0 0, Size w h)
  checkErrors "seting up view"
  return state

handlerInit w h = do
  myVBO <- makeBuffer ArrayBuffer
           [-1.0, -1.0 :: GLfloat, --, 0, 0,  :: GLfloat,
            -1.0,  1.0, -- 0, 1, 
             1.0, -1.0, --1, 0,
             1.0,  1.0] --,1, 1]

  checkErrors "loading arraybuffer"

  prog <- simpleShaderProgram "simple.vert" "mandel.frag"
  checkErrors "loading shaders"

  myVAO <- makeVAO $ do
    enableAttrib prog "position"
    checkErrors "enablingAttrib position"

    bindBuffer ArrayBuffer $= Just (myVBO)
    checkErrors "binding ArrayBuffer"

    let stride = fromIntegral $ sizeOf (undefined::GLfloat) * 2
    setAttrib prog "position" ToFloat $ VertexArrayDescriptor 2 Float stride offset0

  clearColor $= Color4 0.0 0.0 0.5 1.0

  let state = GfxState {shaders = prog,
                        vao_quad = myVAO}

  handlerResize state w h
 
main = do
      startLoop
       (GfxHandlers {handleInit   = handlerInit,
                     handleResize = handlerResize,
                     handleMark   = handlerMark,
                     handleDraw   = handlerDraw})
    where ?log = putStrLn
