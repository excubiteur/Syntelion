{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}

-- | Discrete line with ticks, unit continuities, and combinators for consecutive intervals
class DiscreteLine d where
    type Tick d
    type UnitContinuity d
    type ContinuityIndex d

    -- | Ordered list of ticks (boundaries)
    ticks :: d -> [Tick d]

    -- | Unit continuity between consecutive ticks
    unitContinuity :: Tick d -> Tick d -> UnitContinuity d

    -- | List of all consecutive unit continuities
    consecutives :: d -> [UnitContinuity d]
    consecutives d = zipWith unitContinuity ts (tail ts)
      where ts = ticks d

    -- | Continuity by index
    continuityByIndex :: d -> ContinuityIndex d -> UnitContinuity d
    continuityByIndex d i = consecutives d !! i

    -- | Optional wrap-around continuity
    wrapAround :: d -> Maybe UnitContinuity d
    wrapAround d = let ts = ticks d
                   in if null ts then Nothing else Just (last ts, head ts)

    -- | Consecutive unit continuities from start index to end index (inclusive)
    consecutive :: d -> ContinuityIndex d -> ContinuityIndex d -> [UnitContinuity d]
    consecutive d start end
        | start <= end = map (continuityByIndex d) [start .. end]
        | otherwise    = error "Start index must be <= end index"

    -- | All possible consecutive subsequences (like a restricted power set)
    allConsecutives :: d -> [[UnitContinuity d]]
    allConsecutives d = [ consecutive d i j | i <- [0..n-1], j <- [i..n-1] ]
      where n = length (consecutives d)

    -- | All consecutive subsequences of exactly length k
    consecutivesOfLength :: d -> Int -> [[UnitContinuity d]]
    consecutivesOfLength d k = [ consecutive d i (i+k-1) | i <- [0..n-k] ]
      where n = length (consecutives d)

data Week = Week
    { weekTicks :: [String]  -- ["Mon","Tue","Wed","Thu","Fri","Sat","Sun","MonEnd"]
    }

instance DiscreteLine Week where
    type Tick Week = String
    type UnitContinuity Week = (String,String)
    type ContinuityIndex Week = Int

    ticks = weekTicks

    unitContinuity a b = (a,b)

let week = Week ["Mon","Tue","Wed","Thu","Fri","Sat","Sun","MonEnd"]

-- Monday as a single unit continuity
consecutive week 0 0
-- [("Mon","Tue")]

-- Monday through Wednesday
consecutive week 0 2
-- [("Mon","Tue"),("Tue","Wed"),("Wed","Thu")]

-- All possible consecutive combos
allConsecutives week
-- [[("Mon","Tue")], [("Mon","Tue"),("Tue","Wed")], ..., [("Mon","Tue"),...,("Sun","MonEnd")]]

-- All sequences of exactly 3 consecutive days
consecutivesOfLength week 3
-- [[("Mon","Tue"),("Tue","Wed"),("Wed","Thu")], ..., [("Fri","Sat"),("Sat","Sun"),("Sun","MonEnd")]]
