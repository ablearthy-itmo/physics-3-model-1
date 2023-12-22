{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module SimOptimized (Config (..), World (..), WorldState (..), ObjectState (..), initialWorld, step, countHits, getNextState) where

infinity :: Double
infinity = 1 / 0

type Vector = Double

data Config = Config
  { smallMass :: !Double,
    bigMass :: !Double,
    smallPosition :: !Vector,
    bigPosition :: !Vector,
    bigVelocity :: !Vector
  }

data ObjectState = ObjectState
  { velocity :: !Vector,
    position :: !Vector
  }
  deriving (Show)

data WorldState = WorldState
  { smallObject :: !ObjectState,
    bigObject :: !ObjectState
  }
  deriving (Show)

data World = World
  { currentState :: !WorldState,
    nextHitState :: !(Maybe WorldState),
    deltaTime :: !Double,
    hitsCount :: !Integer
  }
  deriving (Show)

adjustObjectState :: Double -> ObjectState -> ObjectState
adjustObjectState dt s = s {position = s.position + dt * s.velocity}

adjustWorldState :: Double -> WorldState -> WorldState
adjustWorldState dt s =
  s
    { smallObject = adjustObjectState dt s.smallObject,
      bigObject = adjustObjectState dt s.bigObject
    }

calcHitBetweenObjects :: Config -> WorldState -> WorldState
calcHitBetweenObjects cfg WorldState {smallObject = s, bigObject = b} =
  WorldState
    { smallObject = ObjectState {velocity = smallVelocity', position = pos},
      bigObject = ObjectState {velocity = bigVelocity', position = pos}
    }
  where
    dt = abs $ (b.position - s.position) / (s.velocity - b.velocity)
    pos = s.position + s.velocity * dt
    m = cfg.smallMass
    bM = cfg.bigMass
    !smallVelocity' = (m * s.velocity + 2 * bM * b.velocity - bM * s.velocity) / (bM + m)
    !bigVelocity' = m * (s.velocity - smallVelocity') / bM + b.velocity

calcHitWithWall :: WorldState -> WorldState
calcHitWithWall state =
  state
    { smallObject = state.smallObject {velocity = -state.smallObject.velocity, position = 0},
      bigObject = adjustObjectState dt state.bigObject
    }
  where
    !dt = state.smallObject.position / (-state.smallObject.velocity)

initialWorld :: Config -> World
initialWorld cfg = World cur (Just (calcHitBetweenObjects cfg cur)) dt 0
  where
    dt = (cfg.bigPosition - cfg.smallPosition) / (-cfg.bigVelocity)
    !cur = WorldState {smallObject = ObjectState {velocity = 0, position = cfg.smallPosition}, bigObject = ObjectState {velocity = cfg.bigVelocity, position = cfg.bigPosition}}

getNextState :: Config -> WorldState -> Maybe WorldState
getNextState cfg nt
  | nt.bigObject.velocity >= nt.smallObject.velocity && nt.smallObject.velocity >= 0 = Nothing
  | nt.smallObject.velocity < 0 = Just $ calcHitWithWall nt
  | otherwise = Just $ calcHitBetweenObjects cfg nt

step :: Config -> Double -> World -> World
step cfg dt w
  | w.deltaTime >= dt =
      w {currentState = adjustWorldState dt w.currentState, deltaTime = w.deltaTime - dt}
  | otherwise = case w.nextHitState of
      Nothing -> w {currentState = adjustWorldState dt w.currentState}
      Just nt -> step cfg (dt - w.deltaTime) $ w {currentState = nt, nextHitState = nextState', deltaTime = dt', hitsCount = w.hitsCount + 1}
        where
          !nextState' = getNextState cfg nt

          !dt' = case nextState' of
            Nothing -> infinity
            Just nt' -> abs ((nt.smallObject.position - nt'.smallObject.position) / nt.smallObject.velocity)

countHits :: Config -> Maybe WorldState -> Integer
countHits cfg = go 0
  where
    go !cnt Nothing = cnt
    go !cnt (Just state) = go (cnt + 1) (getNextState cfg state)
