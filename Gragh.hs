module Gragh where

import Graphics.UI.GLUT
import Data.IORef

data GraghInfo
  = GraghInfo {
        width :: Float,
        height:: Float,
        func  :: Float -> Float,
        fcolor:: Color3 GLdouble,
        x_axis::Float,
        y_axis::Float,
        px    ::Float
    }

drawAxis :: Float -> Float -> Float -> Float -> IO()
drawAxis w h x y
 = do
    preservingMatrix $ do	-- x軸描画
      color (Color3 1.0 1.0 1.0 :: Color3 GLdouble)
      renderPrimitive LineStrip $ mapM_ vertex
       [ Vertex2 ((w-y)/w-0.5)  1.0,
         Vertex2 ((w-y)/w-0.5) (-1.0) :: Vertex2 GLfloat]
      renderPrimitive LineStrip $ mapM_ vertex
       [ Vertex2  1.0   ((h-x)/h-0.5),
         Vertex2 (-1.0) ((h-x)/h-0.5) :: Vertex2 GLfloat]


drawGragh :: GraghInfo -> IO()
drawGragh info
 = do
      clear [ColorBuffer]

      drawAxis (width info) (height info) (x_axis info) (y_axis info)
      preservingMatrix $ do
        color (fcolor info)
        renderPrimitive Points $ mapM_ vertex
          $ map (\(x,y) -> Vertex2 x y) $ map (\x -> ((x,(func info) x))) [0,0.1..1.0]
          --[Vertex2 0.8 0.8 :: Vertex2 GLfloat]
     
      swapBuffers
