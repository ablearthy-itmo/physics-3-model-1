{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -O2 #-}

module MyLib (someFunc) where

import Control.Monad (forM_)

data Config = Config
  { m :: {-# UNPACK #-} !Double,
    bigM :: {-# UNPACK #-} !Double,
    v :: {-# UNPACK #-} !Double,
    u :: {-# UNPACK #-} !Double
  }
  deriving (Show, Eq)

calcBump :: Config -> Config
calcBump Config {m, bigM, v, u} = Config {m, bigM, v = v1, u = u1}
  where
    !v1 = (m * v + 2 * bigM * u - bigM * v) / (m + bigM)
    !u1 = m * (v - v1) / bigM + u

calcBumpWithWall :: Config -> Config
calcBumpWithWall Config {m, bigM, v, u} = Config {m, bigM, v = -v, u}

countHits :: Config -> Integer
countHits = go 1 . calcBump
  where
    go :: Integer -> Config -> Integer
    go !cnt !cfg
      | cfg.u >= cfg.v && cfg.v >= 0 = cnt
      | even cnt = go (cnt + 1) (calcBump cfg)
      | otherwise = go (cnt + 1) (calcBumpWithWall cfg)

someFunc :: IO ()
someFunc = do
  let powers = map (10 ^) [0 ..]
  forM_ (take 18 powers) $ \bigM -> do
    let !cfg = Config {m = 1, bigM = bigM, v = 0, u = -1}
    putStrLn $ "M = " <> show bigM <> ", hits = " <> show (countHits cfg)
