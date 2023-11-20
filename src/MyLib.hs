{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -O2 #-}

module MyLib (getHitsCount, generateModel, Model (..), Config (..)) where

import Data.List (unfoldr)
import Data.List.NonEmpty (NonEmpty (..))

data Config = Config
  { m :: !Double,
    bigM :: !Double,
    v :: !Double,
    u :: !Double
  }
  deriving (Show, Eq)

data Model = Model
  { x :: !Double,
    bigX :: !Double,
    deltaTime :: !Double,
    config :: !Config
  }
  deriving (Show, Eq)

calcBump :: Config -> Config
calcBump Config {m, bigM, v, u} = Config {m, bigM, v = v1, u = u1}
  where
    !v1 = (m * v + 2 * bigM * u - bigM * v) / (m + bigM)
    !u1 = m * (v - v1) / bigM + u

calcBumpWithWall :: Config -> Config
calcBumpWithWall Config {m, bigM, v, u} = Config {m, bigM, v = -v, u}

generateSequence :: Config -> [Config]
generateSequence = unfoldr go . (False,)
  where
    go :: (Bool, Config) -> Maybe (Config, (Bool, Config))
    go (b, cfg)
      | cfg.u >= cfg.v && cfg.v >= 0 = Nothing
      | b = let cfg' = calcBumpWithWall cfg in Just (cfg', (not b, cfg'))
      | otherwise = let cfg' = calcBump cfg in Just (cfg', (not b, cfg'))

generateSequence0 :: Config -> NonEmpty Config
generateSequence0 cfg = cfg :| generateSequence cfg

generateModel :: Model -> [Model]
generateModel initialModel = scanl f initialModel (generateSequence initialModel.config)
  where
    f :: Model -> Config -> Model
    f prev cur
      | prev.config.v < 0 =
          let dt = prev.x / (-prev.config.v)
           in Model {x = 0, bigX = prev.bigX - dt * (-prev.config.u), deltaTime = dt, config = cur}
      | otherwise =
          let dt = (prev.bigX - prev.x) / abs (prev.config.u - prev.config.v)
           in Model {x = prev.x + dt * prev.config.v, bigX = prev.bigX + dt * prev.config.u, deltaTime = dt, config = cur}

countHits :: Config -> Integer
countHits = foldr (const (+ 1)) 0 . generateSequence

getHitsCount :: [(Double, Integer)]
getHitsCount = map (\x -> (x, countHits (initialCfg x))) powers
  where
    powers = map (10 ^) [0 ..]
    initialCfg x = Config {m = 1, bigM = x, v = 0, u = -0.01}
