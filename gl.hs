{-# LANGUAGE ImplicitParams, RecordWildCards, NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Gl where

import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.GL.Shaders.ProgramObjects
import Graphics.Rendering.OpenGL.GL.VertexArrays
import Graphics.Rendering.OpenGL.GL.Texturing.Specification
--import Graphics.GLUtil
import Data.Array.MArray
import Data.Array.Storable
import Foreign.Ptr
import Foreign.Storable
import qualified Data.ByteString as B
import System.IO (hPutStrLn, stderr)

import Graphics.Rendering.OpenGL.GL.PixelRectangles

import Foreign (Ptr, Word8, alloca, peek)
import qualified Data.Vector.Storable as V
import qualified Codec.Picture as Juicy
import qualified Codec.Picture.Types as JTypes

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

-- image loading from:
-- https://github.com/fiendfan1/Haskell-OpenGL/blob/master/src/Engine/Graphics/Textures.hs
data Image = Image (GLint, GLint) (Ptr Word8)
    deriving (Show)
juicyLoadTexture :: FilePath -> IO TextureObject
juicyLoadTexture file = do
  (Image (w, h) pd) <- juicyLoadImageRaw file

  TextureObject texid <- genObjectName
  let tex :: TextureObject = TextureObject texid    

  textureBinding Texture2D $= Just tex
  textureFilter Texture2D $= ((Nearest, Just Nearest), Nearest)
  texImage2D Texture2D NoProxy 0 RGB' (TextureSize2D w h) 0 (PixelData RGB UnsignedByte pd)

  return tex

juicyLoadImageRaw :: FilePath -> IO Image
juicyLoadImageRaw file = do
    image <- Juicy.readImage file

    case image of
        Left err -> error err

        Right (Juicy.ImageRGB8 (Juicy.Image w h dat)) ->
            V.unsafeWith dat $ \ptr ->
            return $ Image (fromIntegral w, fromIntegral h) ptr
        Right (Juicy.ImageYCbCr8 img) ->
            let (Juicy.Image w h dat) =
                    JTypes.convertImage img :: Juicy.Image Juicy.PixelRGB8
            in V.unsafeWith dat $ \ptr ->
                return $ Image (fromIntegral w, fromIntegral h) ptr
        Right (Juicy.ImageCMYK8 img) ->
            let (Juicy.Image w h dat) =
                    JTypes.convertImage img :: Juicy.Image Juicy.PixelRGB8
            in V.unsafeWith dat $ \ptr ->
                return $ Image (fromIntegral w, fromIntegral h) ptr 
        _ -> error "bad image colorspace or format"