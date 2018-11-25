
import Control.Monad
import Control.Monad.Random
import Control.Exception
import Control.DeepSeq

import Histogram (histogramCorrect, histogramFast)
import BigO (profile)

type Implementation = [Int] -> [(Int, Int)]

randomHistogram :: (RandomGen g, Monad m) => Int -> RandT g m [Int]
randomHistogram n = replicateM n (getRandomR (0, 100))

computeHistogram :: RandomGen g => Implementation -> Int -> g -> IO ()
computeHistogram fn n g = do
    h1 <- evalRandT (randomHistogram n) g
    evaluate $ force $ fn h1
    return ()

main :: IO ()
main = do
    profile "correct" (computeHistogram histogramCorrect)
    profile "fast" (computeHistogram histogramFast)
    return ()

