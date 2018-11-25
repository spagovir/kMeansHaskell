import Space
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Arrow

type KMeansState v = State (Maybe [v])

kMeansStep :: (Distanceable v, Averageable v, Eq v) => [v] -> KMeansState v ()
kMeansStep vs = do
  maybeMs <- get
  put $ do
    ms <- maybeMs
    sequence $ map meanPoint $ cluster vs ms -- map (flip filter vs . (>>>) (flip closest ms) . (==)) $ ms

cluster :: (Distanceable v, Eq v) => [v] -> [v] -> [[v]]
cluster vs ms = map (flip filter vs . (>>>) (flip closest ms) . (==)) $ ms

kMeansF :: (Distanceable v, Averageable v, Eq v) => [v] -> KMeansState v ()
kMeansF vs = do
  ms <- get
  kMeansStep vs
  ms' <- get
  if ms == ms' then return () else kMeansF vs

kMeans :: (Distanceable v, Averageable v, Eq v) => Int -> [v] -> Maybe [v] 
kMeans n vs =
  execState (kMeansF vs) $ Just $ take n vs
