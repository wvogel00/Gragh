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
                   func= \x->x*(x-3) ,fcolor = Color3 0 1.0 0.2,
                   x_axis = 200,y_axis = 100,
                   px = 50,py = 600}

main :: IO()
main
 = do getArgsAndInitialize
      initialWindowSize $= Size (floor $ width test1) (floor $height test1)
      initialWindowPosition $= Position 100 100
      initialDisplayMode $= [DoubleBuffered , RGBMode]

      createWindow "Gragh with Haskell"

      displayCallback $= drawGragh test1
      mainLoop
