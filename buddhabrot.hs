import Graphics.UI.GLUT
import System.Exit
import Data.Char

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "buddhabrot"
  displayCallback $= display
  reshapeCallback $= Just reshape
  keyboardCallback $= Just handleKeyboard
  specialCallback $= Just handleSpecial
  mainLoop

handleKeyboard :: KeyboardCallback
handleKeyboard char pos = do
  case char of
    '\27' -> exitWith ExitSuccess
    _ -> putStrLn $ "handleKeyboard: " ++ (show (ord char))

handleSpecial :: SpecialCallback
handleSpecial k pos = do
  putStrLn $ "handleSpecial: " ++ (show k)

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)
  postRedisplay Nothing

display :: DisplayCallback
display = do
  let color3f r g b = color $ Color3 r g (b :: GLfloat)
      vertex3f x y z = vertex $ Vertex3 x y (z :: GLfloat)
  clear [ColorBuffer]
  renderPrimitive Quads $ do
    color3f 1 0 0
    vertex3f 0 0 0
    vertex3f 0 0.2 0
    vertex3f 0.2 0.2 0
    vertex3f 0.2 0 0

    color3f 0 1 0
    vertex3f 0 0 0
    vertex3f 0 (-0.2) 0
    vertex3f 0.2 (-0.2) 0
    vertex3f 0.2 0 0

    color3f 0 0 1
    vertex3f 0 0 0
    vertex3f 0 (-0.2) 0
    vertex3f (-0.2) (-0.2) 0
    vertex3f (-0.2) 0 0

    color3f 1 0 1
    vertex3f 0 0 0
    vertex3f 0 0.2 0
    vertex3f (-0.2) 0.2 0
    vertex3f (-0.2) 0 0
  flush
