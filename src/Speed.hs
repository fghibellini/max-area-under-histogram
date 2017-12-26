
import Data.List
import Control.Monad
import Control.Monad.Random
import Control.Exception
import Data.Time

import Histogram (histogramCorrect, histogramFast)

measure :: IO () -> IO Int
measure op = do
    start <- getCurrentTime
    op
    end <- getCurrentTime
    return $ floor $ 1000000 * (diffUTCTime end start)

randomHistogram :: (RandomGen g, Monad m) => Int -> RandT g m [Int]
randomHistogram n = replicateM n (getRandomR (0, 100))

type Implementation = [Int] -> [(Int, Int)]

measureHistogram :: RandomGen g => Int -> Implementation -> g -> IO Int
measureHistogram n fn g = do
    measure $ do
        h1 <- evalRandT (randomHistogram n) g
        evaluate $ fn h1
        return ()
        
genSequence n0 max stride = unfoldr (\n -> if n > max then Nothing else Just (n, n + stride)) n0

profileImpl (name, fn) sizes g = do
    mapM_ (\n -> do
        dur <- measureHistogram n fn g
        putStrLn $ name ++ "," ++ (show n) ++ "," ++ (show $ dur)) sizes


main :: IO ()
main = do
    let g0 = mkStdGen 42
    profileImpl ("correct", histogramCorrect) (genSequence 500 3000 200) g0
    --profileImpl ("fast", histogramFast) (genSequence 2000 4000 20) g0
    return ()

