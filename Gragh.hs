{-# LANGUAGE ViewPatterns  #-}

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
        px    ::Float,
        py    ::Float
    }

drawAxis :: Float -> Float -> Float -> Float -> IO()
drawAxis w h x y
 = do
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

      preservingMatrix $ do
        drawAxis (width info) (height info) (x_axis info) (y_axis info)
      preservingMatrix $ do
        color (fcolor info)
        renderPrimitive Points $ mapM_ vertex
            $ map (calcPos info) [(-1.0),(-0.999) .. 1.0]

      swapBuffers

calcPos :: GraghInfo -> Float -> Vertex2 GLfloat
calcPos info = \x -> Vertex2 x (graghFunc info x)

transX :: GraghInfo -> Float -> Float
transX info x = (x * width info) / 2 / px info

graghFunc :: GraghInfo -> Float -> GLfloat
graghFunc info (transX info -> x')
 = let y' = (func info) x'
   in  (y' * height info) / 2 / py info
