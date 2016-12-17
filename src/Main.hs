{-# LANGUAGE OverloadedStrings #-}
module Main where
import           Data.Bits hiding (rotate)
import qualified Data.Set as S
import           NanoVG as NVG
import           Prelude hiding (init)
import           Graphics.UI.GLFW as GLFW hiding (Image)
import           Control.Monad
import           Control.Monad.Loops
import           Control.Monad.Trans.Maybe
import           Graphics.GL.Core32
import           System.Exit ( exitWith, ExitCode(..) )
import           Reactive.Banana as R
import           Reactive.Banana.Frameworks
import           Reactive.Banana.GLFW

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
         -- createGL2 is a function from a local nanovg fork (also on my github) modified
         -- to run on GL2 instead of upstream version which runs on GL3
         c@(Context c') <- createGL2 (S.fromList [Antialias,StencilStrokes,Debug])
         -- error handling? who needs that anyway
         Just staticData <- runMaybeT $ loadStaticData c
         swapInterval 0
         setTime 0
         setKeyCallback w (Just keyPressed)
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
              drawRaisedButton c (fromIntegral fbWidth / 4) (fromIntegral fbHeight / 4) 88 36
              endFrame c
              swapBuffers w
              pollEvents

keyPressed :: KeyCallback
keyPressed win GLFW.Key'Escape _ GLFW.KeyState'Pressed _ = shutdown win
keyPressed _   _               _ _                     _ = return ()

data StaticData = StaticData { fontMedium :: Font }

loadStaticData :: Context -> MaybeT IO StaticData
loadStaticData c = do
  medium <- MaybeT $ createFont c "medium" (FileName "fonts/Roboto-Medium.ttf")
  pure (StaticData medium)

shutdown :: WindowCloseCallback
shutdown win = do
  GLFW.destroyWindow win
  GLFW.terminate
  _ <- exitWith ExitSuccess
  return ()

drawRaisedButton :: Context -> CFloat -> CFloat -> CFloat -> CFloat -> IO ()
drawRaisedButton c x y w h = do
  save c
  beginPath c
  roundedRect c x y w h 2
  fillColor c (rgba 128 0 0 255)
  fill c
  -- button text
  fontSize c 18
  fontFace c "medium"
  textAlign c (S.fromList [AlignCenter,AlignMiddle])
  fillColor c (rgba 255 255 255 255)
  NVG.text  c (x + w / 2) (y + h / 2) "BUTTON"
  restore c

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
