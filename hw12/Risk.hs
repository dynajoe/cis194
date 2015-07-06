{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Control.Monad
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

rollTimes :: Int -> Rand StdGen [DieValue]
rollTimes x = replicateM x die

getSizedArmies :: Battlefield -> (Army, Army)
getSizedArmies (Battlefield a d) = (att, def)
   where available = if a > 0 then a - 1 else a -- must always leave 1 unit behind
         att = min 3 available
         def = min 2 d

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
   deriving Show

executeBattle :: Battlefield -> Rand StdGen (Army, Army)
executeBattle b = x
   where (a, d) = getSizedArmies b
         aRolls  = rollTimes a >>= (pure . sort)
         dRolls  = rollTimes d >>= (pure . sort)
         pairs   = zip <$> aRolls <*> dRolls
         results = fmap (map (uncurry (>))) pairs
         x = fmap (foldr reduceArmies (a, d)) results

reduceArmies :: Bool -> (Army, Army) -> (Army, Army)
reduceArmies True (a, d) = (a, d - 1)
reduceArmies _ (a, d) = (a - 1, d)

-- Determine count in battle
-- role that number of die for each team
-- sort
-- compare
-- reduce army count for loser of each sub battle
battle :: Battlefield -> Rand StdGen Battlefield
battle b = fmap (uncurry Battlefield) (executeBattle b)

executeBattle' :: Battlefield -> ([DieValue], [DieValue]) -> Battlefield
executeBattle' (Battlefield a d) rolls = uncurry Battlefield newArmies
   where pairs = uncurry zip rolls
         results = map (uncurry (>)) pairs
         newArmies = foldr reduceArmies (a, d) results

battle' :: Battlefield -> Rand StdGen Battlefield
battle' b = liftM (executeBattle' b) rolls
   where (a, d) = getSizedArmies b
         rolls = (,) <$> (rollTimes a >>= (pure . sort)) <*> (rollTimes d >>= (pure . sort))

invade :: Battlefield -> Rand StdGen Battlefield
invade b@(Battlefield a d)
   | a < 2 || d < 1 = return b
   | otherwise = battle' b >>= invade

successProb :: Battlefield -> Rand StdGen Double
successProb b = replicateM 1000 (invade b) >>= pure . wins

wins :: [Battlefield] -> Double
wins bs = n / 1000
   where n = (fromIntegral . length . filter ((== 0) . defenders)) bs
