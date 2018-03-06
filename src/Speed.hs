
import Control.Monad
import Control.Monad.Random
import Control.Exception

import Histogram (histogramCorrect, histogramFast)
import BigO (profile)

randomHistogram :: (RandomGen g, Monad m) => Int -> RandT g m [Int]
randomHistogram n = replicateM n (getRandomR (0, 100))

type Implementation = [Int] -> [(Int, Int)]

computeHistogram :: RandomGen g => Implementation -> Int -> g -> IO ()
computeHistogram fn n g = do
    h1 <- evalRandT (randomHistogram n) g
    evaluate $ fn h1
    return ()

main :: IO ()
main = do
    profile "correct" (computeHistogram histogramCorrect)
    profile "fast" (computeHistogram histogramFast)
    return ()

