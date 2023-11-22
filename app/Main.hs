{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Control.Monad (forM_)
import GHC.Float (double2Float, float2Double, int2Double)
import Graphics.Gloss
import Graphics.Gloss.Interface.Environment
import SimOptimized qualified as SO
import System.Environment (getArgs)
import System.Exit
import Text.Read (readMaybe)

type WithScreenSize = (?sz :: (Double, Double))

makeBox :: Float -> Picture
makeBox a = Polygon [(0, 0), (a, 0), (a, a), (0, a)]

screenSize :: (WithScreenSize) => (Double, Double)
screenSize = ?sz

withScreenSize :: (Double, Double) -> ((WithScreenSize) => a) -> a
withScreenSize sz act = let ?sz = sz in act

drawWorld :: (WithScreenSize) => SO.World -> Picture
drawWorld w =
  Translate (-0.9 * width / 2) (-height / 4) $
    Pictures
      [ Translate (double2Float w.currentState.smallObject.position * 0.9 * width) 0 smallBox,
        Translate (double2Float w.currentState.bigObject.position * 0.9 * width) 0 bigBox,
        wall,
        Line [(0, 0), (width, 0)],
        Translate (0.1 * width) (0.75 * height / 2) $ Text (show w.hitsCount)
      ]
  where
    smallBoxSize = 50
    smallBoxColor = makeColor 0.196 0.5529 0.6588 1
    smallBox = Color smallBoxColor $ makeBox smallBoxSize

    bigBoxSize = 1.5 * smallBoxSize
    bigBoxColor = makeColor 0.8588 0.396 0.367 1
    bigBox = Translate smallBoxSize 0 $ Color bigBoxColor $ makeBox bigBoxSize
    wall = Line [(0, 0), (0, width / 2)]
    (width, height) = let (w', h') = screenSize in (double2Float w', double2Float h')

updateWorld' :: SO.Config -> Float -> SO.World -> SO.World
updateWorld' cfg dt = SO.step cfg (float2Double dt)

parseConfig :: String -> String -> IO SO.Config
parseConfig velocity e = do
  case (,) <$> readMaybe @Double velocity <*> readMaybe @Int e of
    Just (v', e')
      | 0.001 <= v' && v' <= 1 && 0 <= e' && e' <= 13 ->
          pure $
            SO.Config
              { SO.smallMass = 1,
                SO.bigMass = 10 ^ e',
                SO.smallPosition = 0.5,
                SO.bigPosition = 1,
                SO.bigVelocity = -v'
              }
    _ -> do
      putStrLn "error: incorrect params\n\t<velocity> should be in range [0.01; 1]\n\t<exp> should be a positive number and no more than 13"
      exitWith (ExitFailure 1)

printHitsCount :: IO ()
printHitsCount = forM_ (take 15 getHitsCount) $ \(m, cnt) -> do
  putStrLn $ "M = " <> show m <> "; count = " <> show cnt
  where
    bases :: [Double]
    bases = map (10.0 ^) [(0 :: Int) ..]

    getHitsCount :: [(Double, Integer)]
    getHitsCount = map (\m -> (m, go m)) bases
      where
        go m =
          let cfg =
                SO.Config
                  { SO.smallMass = 1,
                    SO.bigMass = m,
                    SO.smallPosition = 0.5,
                    SO.bigPosition = 1,
                    SO.bigVelocity = -1
                  }
              w = SO.initialWorld cfg
           in SO.countHits cfg w.nextHitState

main :: IO ()
main = do
  getArgs >>= \case
    ["hits"] -> do
      printHitsCount
    [velocity, e] -> do
      cfg <- parseConfig velocity e
      let world0 = SO.initialWorld cfg
      (xWidth, yWidth) <- getScreenSize
      let env = withScreenSize (int2Double xWidth, int2Double yWidth)
      simulate
        FullScreen
        white
        60
        world0
        (env drawWorld)
        (const (updateWorld' cfg))
    _ -> putStrLn "usage: ./program hits | ./program <velocity> <exp>"
