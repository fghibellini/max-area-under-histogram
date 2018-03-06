
-- don't do this. see https://chrisdone.com/posts/measuring-duration-in-haskell

import Data.List
import Control.Monad
import Control.Monad.Random
import Control.Exception
import Data.Time
import System.Timeout

import Histogram (histogramCorrect, histogramFast)

-- returns duration in μs
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
        
profile :: String -> Implementation -> IO ()
profile name f = do
  Just (minO, maxO) <- logProfile f (1000, 1000000)
  let n0 = (5 * (pow 10 minO ))
      nMax = (5 * (pow 10 maxO ))
      nStep = (nMax - n0) `div` 100
  let ns = unfoldr (\n -> let nnext = (n + nStep) in if nnext > nMax then Nothing else Just (nnext, nnext)) n0
  let g0 = mkStdGen 42
  xs <- mapM (\n -> measureHistogram n f g0 >>= (\t -> return (n, t))) ns
  mapM_ (\(n, t) -> putStrLn (name ++ "," ++ show n ++ "," ++ show t)) xs

pow :: Float -> Float -> Int
pow x e = floor (x ** e)

step = 0.25

findMinLog :: Implementation -> (Int, Int) -> Float -> IO (Maybe Float)
findMinLog f ts@(tmin, tmax) p = do
  let g0 = mkStdGen 42
  let n = 5 * (pow 10 p)
  m <- timeout tmax (measureHistogram n f g0)
  case m of
    Nothing -> putStrLn $ "MIN for n=" ++ (show n) ++ " TIMEOUT"
    Just t -> putStrLn $ "MIN for n=" ++ (show n) ++ " t=" ++ (show t)
  case m of
    Nothing -> return Nothing
    Just t -> if t < tmin
              then findMinLog f ts (p + step)
              else return (Just p)

findMaxLog :: Implementation -> (Int, Int) -> Float -> IO Float
findMaxLog f ts@(tmin, tmax) p = do
  let g0 = mkStdGen 42
  let n = 5 * (pow 10 p)
  m <- timeout tmax (measureHistogram n f g0)
  case m of
    Nothing -> putStrLn $ "MAX for n=" ++ (show n) ++ " TIMEOUT"
    Just t -> putStrLn $ "MAX for n=" ++ (show n) ++ " t=" ++ (show t)
  case m of
    Nothing -> return (p - step)
    Just t -> findMaxLog f ts (p + step)

logProfile :: Implementation -> (Int, Int) -> IO (Maybe (Float, Float))
logProfile f ts@(tmin, tmax) = do
  minOrder <- findMinLog f ts 0
  case minOrder of
    Nothing -> error "Could not find any input size that would compute in the specified time span"
    Just minO -> do
      maxO <- findMaxLog f ts (minO + step)
      return $ Just (minO, maxO)


main :: IO ()
main = do
    --ts <- logProfile histogramFast (1000, 1000000) 
    --case ts of
    --  Nothing -> print "nope"
    --  Just t -> putStrLn $ "Times: " ++ (show t)
    profile "correct" histogramCorrect
    profile "fast" histogramFast
    return ()

