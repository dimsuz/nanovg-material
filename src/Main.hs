{-# LANGUAGE OverloadedStrings, RecursiveDo #-}
module Main where
import           Data.Bits hiding (rotate)
import qualified Data.Set as S
import           NanoVG as NVG
import           Prelude hiding (init)
import           Graphics.UI.GLFW as GLFW hiding (Image)
import           Control.Monad
import           Control.Monad.Loops
import           Control.Monad.Trans.Maybe
import           Control.Monad.Fix
import           Graphics.GL.Core32
import           System.Exit ( exitWith, ExitCode(..) )
import           Reactive.Banana as R
import           Reactive.Banana.Frameworks
import           Reactive.Banana.GLFW as RGLFW
import           Debug.Trace

import           Foreign.C.Types
import           Foreign.Ptr

foreign import ccall unsafe "initGlew"
  glewInit :: IO CInt

type Point = (CFloat, CFloat)
type PointB = Behavior Point
type BoolB = Behavior Bool
type RealB = Behavior CFloat

type CPoint = (PointB, BoolB)
type ImageB = Behavior (IO ())

toPoint :: (Double, Double) -> Point
toPoint (x,y) = (realToFrac x, realToFrac y)

isPress :: MouseButton -> MouseEvent -> Bool
isPress mb e = press e && (mouseButton e == mb)

isRelease :: MouseButton -> MouseEvent -> Bool
isRelease mb e = release e && (mouseButton e == mb)

isLeftPress :: MouseEvent -> Bool
isLeftPress = isPress MouseButton'1

isLeftRelease :: MouseEvent -> Bool
isLeftRelease = isRelease MouseButton'1

leftMouseReleasePos :: RGLFW.Cursor -> Event MouseEvent -> Event Point
leftMouseReleasePos c e = mouseEventPos c e isLeftRelease

leftMousePressPos :: RGLFW.Cursor -> Event MouseEvent -> Event Point
leftMousePressPos c e = mouseEventPos c e isLeftPress

mouseEventPos :: RGLFW.Cursor -> Event MouseEvent -> (MouseEvent -> Bool) -> Event Point
mouseEventPos cursor mouseE filterPred = toPoint <$> mouseEDouble
  where mouseEDouble = cursorPos cursor <@ mouseClickE
        mouseClickE = filterE filterPred mouseE

cursorPos' :: RGLFW.Cursor -> PointB
cursorPos' c = toPoint <$> cursorPos c

traceShowP :: Show a => String -> a -> a
traceShowP prefix v = trace (prefix ++ " " ++ show v) v

editCPoint' :: (MonadMoment m, MonadFix m) => Point -> RGLFW.Cursor -> Event MouseEvent -> m CPoint
editCPoint' p0 cursor mouseE = mdo
  pointPos <- ifB (mouseMove cursor) grabB (cursorPos' cursor) lastRelease
  grabB <- stepper False grabbingE
  lastRelease <- stepper p0 (pointPos <@ releaseE)
  let closeEnough = (<) <$> distance2 pointPos (cursorPos' cursor) <*> grabDistance
      grabE = const True <$> whenE closeEnough (filterE isLeftPress mouseE)
      releaseE = const False <$> whenE grabB (filterE isLeftRelease mouseE)
      grabbingE = unionWith (\v1 v2 -> v1) grabE releaseE
  return (pointPos, closeEnough)

ifB :: MonadMoment m => Event b -> BoolB -> Behavior a -> Behavior a -> m (Behavior a)
ifB trigger condB b1 b2 = switchB b2 (switcherB <@ trigger)
  where switcherB = (\x -> if x then b1 else b2) <$> condB

grabDistance :: RealB
grabDistance = (2 *) <$> pointSize

distance2 :: PointB -> PointB -> RealB
distance2 = liftA2 distance
  where distance (x1, y1) (x2, y2) = sqrt ((x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2))

pointSize :: RealB
pointSize = pure 8

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

         (displayHandler, runDisplay) <- newAddHandler

         h <- windowHandler w
         network <- compile $ do
           displayE <- fromAddHandler displayHandler
           keyE <- keyEvent h
           mouseE <- mouseEvent h
           closeE <- close h
           cursor <- cursor h TopLeft
           cpoint1 <- editCPoint' (50.0, 50.0) cursor mouseE
           cpoint2 <- editCPoint' (150.0, 250.0) cursor mouseE
           reactimate $ renderCPoint c cpoint1 <@ displayE
           reactimate $ renderCPoint c cpoint2 <@ displayE
           reactimate $ shutdown w <$ filterE (match Key'Escape) keyE
           reactimate $ shutdown w <$ closeE
         actuate network

         forever $
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
              runDisplay t
              endFrame c
              swapBuffers w
              pollEvents

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

renderCPoint:: Context -> CPoint -> ImageB
renderCPoint c (posB, excitedB) = liftA2 (renderPoint c) posB excitedB
  where renderPoint c (x, y) excited = drawControlPoint c x y pointSize excited
        pointSize = 8

drawControlPoint :: Context -> CFloat -> CFloat -> CFloat -> Bool -> IO ()
drawControlPoint c x y r excited = do
  let color = if excited then (rgba 255 0 0 255) else (rgba 128 0 0 255)
  beginPath c
  circle c x y (r - 3)
  fillColor c color
  fill c
  beginPath c
  circle c x y r
  strokeColor c color
  strokeWidth c 2
  stroke c

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
