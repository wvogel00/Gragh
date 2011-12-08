import Gragh
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


main :: IO()
main
 = do getArgsAndInitialize
      initialWindowSize $= Size (floor $ width test2) (floor $height test2)
      initialWindowPosition $= Position 100 100
      initialDisplayMode $= [DoubleBuffered , RGBMode]

      createWindow "gragh wigh Haskell"

      displayCallback $= drawGragh test2
      mainLoop
