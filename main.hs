import Gragh
import Data.IORef
import Graphics.UI.GLUT

test1 :: GraghInfo
test1 = GraghInfo {width = 600,height = 400,
                   func = sin ,fcolor = Color3 0 1.0 0.2,
                   x_axis = 200,y_axis = 300,
                   px = 50,py = 400}
test2 :: GraghInfo
test2 = GraghInfo {width = 600,height = 400,
                   func = (^2) ,fcolor = Color3 0 1.0 0.2,
                   x_axis = 200,y_axis = 300,
                   px = 100,py = 400}
test3 :: GraghInfo
test3 = GraghInfo {width = 600,height = 400,
                   func =id ,fcolor = Color3 0 1.0 0.2,
                   x_axis = 200,y_axis = 300,
                   px = 100,py = 400}



main :: IO()
main
 = do gragh <- newIORef test1
      gragh' <- readIORef gragh
      getArgsAndInitialize
      initialWindowSize $= Size (floor $ width gragh') (floor $height gragh')
      initialWindowPosition $= Position 100 100
      initialDisplayMode $= [DoubleBuffered , RGBMode]

      createWindow "Gragh with Haskell"

      displayCallback $= drawGragh gragh
      keyboardMouseCallback $= Just (moveGragh gragh)
      mainLoop
