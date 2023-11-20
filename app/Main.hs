{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Control.Monad (forM_)
import GHC.Float (double2Float, float2Double, int2Double)
import Graphics.Gloss
import Graphics.Gloss.Interface.Environment
import MyLib
import System.Environment (getArgs)
import System.Exit
import Text.Read (readMaybe)

type WithScreenSize = (?sz :: (Double, Double))

type State = (Integer, [Model])

makeBox :: Float -> Picture
makeBox a = Polygon [(0, 0), (a, 0), (a, a), (0, a)]

screenSize :: (WithScreenSize) => (Double, Double)
screenSize = ?sz

withScreenSize :: (Double, Double) -> ((WithScreenSize) => a) -> a
withScreenSize sz act = let ?sz = sz in act

drawModel :: (WithScreenSize) => Integer -> Model -> Picture
drawModel ctr model =
  Translate (-0.9 * width / 2) (-height / 4) $
    Pictures
      [ Translate (double2Float model.x * 0.9 * width) 0 smallBox,
        Translate (double2Float model.bigX * 0.9 * width) 0 bigBox,
        wall,
        Line [(0, 0), (width, 0)],
        Translate (0.1 * width) (0.75 * height / 2) $ Text (show ctr)
      ]
  where
    smallBoxSize = 50
    smallBoxColor = makeColor 0.196 0.5529 0.6588 1
    smallBox = Color smallBoxColor $ makeBox smallBoxSize

    bigBoxSize = 1.5 * smallBoxSize
    bigBoxColor = makeColor 0.8588 0.396 0.367 1
    bigBox = Translate smallBoxSize 0 $ Color bigBoxColor $ makeBox bigBoxSize
    wall = Line [(0, 0), (0, width / 2)]
    (width, height) = let (w, h) = screenSize in (double2Float w, double2Float h)

calcInsideDeltaTime :: Double -> Model -> Model
calcInsideDeltaTime dt model = model {x = x', bigX = bigX'}
  where
    x' = model.x + dt * model.config.v
    bigX' = model.bigX + dt * model.config.u

updateModel :: Double -> State -> State
updateModel _ (_, []) = error "error"
updateModel dt (a, [x]) = (a, [calcInsideDeltaTime dt $ x {deltaTime = dt}])
updateModel dt (ctr, x : y : xs)
  | y.deltaTime >= dt = (ctr, calcInsideDeltaTime dt x : y {deltaTime = y.deltaTime - dt} : xs)
  | otherwise = updateModel (dt - y.deltaTime) (ctr + 1, y : xs)

updateModel' :: Float -> State -> State
updateModel' a = updateModel (float2Double a)

getInitialModel :: IO Model
getInitialModel = do
  getArgs >>= \case
    [velocity, e] -> do
      case (,) <$> readMaybe @Double velocity <*> readMaybe @Int e of
        Just (v', e') | 0.01 <= v' && v' <= 1 && 0 <= e' && e' <= 10 -> pure $ initializeModel (-v') e'
        _ -> do
          putStrLn "error: incorrect params\n\t<velocity> should be in range [0.01; 1]\n\t<exp> should be a positive number and no more than 10"
          exitWith (ExitFailure 1)
    ["model"] -> do
      printHitsCount
      exitSuccess
    _ -> do
      putStrLn "usage: ./program model | ./program <velocity> <exp>"
      exitWith (ExitFailure 1)
  where
    initializeModel u bigMExp = Model {x = 0.5, bigX = 1, deltaTime = 0, config = Config {m = 1, bigM = 10 ^ bigMExp, v = 0, u = u}}

printHitsCount :: IO ()
printHitsCount = forM_ (take 15 getHitsCount) $ \(m, cnt) -> do
  putStrLn $ "M = " <> show m <> "; count = " <> show cnt

main :: IO ()
main = do
  initialModel <- getInitialModel
  (xWidth, yWidth) <- getScreenSize
  let env = withScreenSize (int2Double xWidth, int2Double yWidth)
  simulate
    FullScreen
    white
    60
    (0, generateModel initialModel)
    (env (\(ctr, xs) -> drawModel ctr (head xs)))
    (const updateModel')
