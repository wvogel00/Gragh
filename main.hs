import Gragh
import Graphics.UI.GLUT

test :: GraghInfo
test = GraghInfo {width = 600,height = 400,
                  func = sin ,fcolor = Color3 0 1.0 0.2,
                  x_axis = 200,y_axis = 300 , px = 50}

main :: IO()
main
 = do getArgsAndInitialize
      initialWindowSize $= Size (floor $ width test) (floor $height test)
      initialWindowPosition $= Position 100 100
      initialDisplayMode $= [DoubleBuffered , RGBMode]

      createWindow "gragh wigh Haskell"

      displayCallback $= drawGragh test
      mainLoop
