module Space where

import Data.List

class Monoid v => Averageable v where

  intDiv   :: Integral i => v -> i -> Maybe v
  intDiv v 0 = Nothing
  intDiv v i = meanPoint $ v : genericReplicate i mempty 

  meanPoint :: [v] -> Maybe v
  meanPoint vs = intDiv (mconcat vs) (length vs)

class Distanceable v where

  compd :: v -> v -> v -> Ordering
  
  closest :: v -> [v] -> v
  closest = minimumBy . compd
  
