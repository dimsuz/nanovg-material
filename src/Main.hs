module Main where
import           Data.Bits hiding (rotate)
import qualified Data.Set as S
import           NanoVG as NVG
import           Prelude hiding (init)
import           Graphics.UI.GLFW hiding (Image)
import           Control.Monad
import           Control.Monad.Loops
import           Graphics.GL.Core32

import           Foreign.C.Types
import           Foreign.Ptr

foreign import ccall unsafe "initGlew"
  glewInit :: IO CInt

main :: IO ()
main = do
  e <- init
  when (not e) $ putStrLn "Failed to init GLFW"
  windowHint $ WindowHint'ContextVersionMajor 2
  windowHint $ WindowHint'ContextVersionMinor 1
  win <- createWindow 800 600 "NanoVG" Nothing Nothing
  case win of
    Nothing -> putStrLn "Failed to create window" >> terminate
    Just w ->
      do makeContextCurrent win
         glewInit
         glGetError
         c@(Context c') <- createGL2 (S.fromList [Antialias,StencilStrokes,Debug])
         -- error handling? who needs that anyway
         swapInterval 0
         setTime 0
         whileM_ (not <$> windowShouldClose w) $
           do Just t <- getTime
              (mx,my) <- getCursorPos w
              (width,height) <- getWindowSize w
              (fbWidth,fbHeight) <- getFramebufferSize w
              let pxRatio = fromIntegral fbWidth / fromIntegral width
              glViewport 0 0 (fromIntegral fbWidth) (fromIntegral fbHeight)
              glClearColor 0.3 0.3 0.32 1.0
              glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT .|. GL_STENCIL_BUFFER_BIT)
              beginFrame c (fromIntegral width) (fromIntegral height) pxRatio
              drawSpinner c (fromIntegral fbWidth / 2) (fromIntegral fbHeight / 2) 20 (realToFrac t)
              endFrame c
              swapBuffers w
              pollEvents

drawSpinner :: Context -> CFloat -> CFloat -> CFloat -> CFloat -> IO ()
drawSpinner vg cx cy r t =
  do let a0 = 0+t*6
         a1 = pi + t*6
         r0 = r
         r1 = r*0.75
     save vg

     beginPath vg
     arc vg cx cy r0 a0 a1 CW
     arc vg cx cy r1 a1 a0 CCW
     closePath vg
     let ax = cx+cos a0 * (r0+r1)*0.5
         ay = cy+sin a0 * (r0+r1)*0.5
         bx = cx+cos a1 * (r0+r1)*0.5
         by = cy+sin a1 * (r0+r1)*0.5
     paint <- linearGradient vg ax ay bx by (rgba 0 0 0 0) (rgba 0 0 0 128)
     fillPaint vg paint
     fill vg

     restore vg
