{-# LANGUAGE ImplicitParams, RecordWildCards, NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Gl where

import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.GL.Shaders.ProgramObjects
import Graphics.Rendering.OpenGL.GL.VertexArrays
--import Graphics.GLUtil
import Data.Array.MArray
import Data.Array.Storable
import Foreign.Ptr
import Foreign.Storable
import qualified Data.ByteString as B
import System.IO (hPutStrLn, stderr)

offset = plusPtr nullPtr
offset0 = nullPtr

checkErrors :: String -> IO ()
checkErrors msg = do
  errs <- get errors
  if (not (null errs)) then
      do putStrLn msg >> mapM_ printErr errs
         error "had GL error, exiting"
  else return ()
  where printErr = hPutStrLn stderr . ("  GL: "++) . show

checkAttribs p = do
  as <- get (activeAttribs p)
  putStrLn "active attributes:"
  mapM (\s -> putStrLn (show s)) as
