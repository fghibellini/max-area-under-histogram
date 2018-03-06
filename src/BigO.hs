
-- don't do this. see https://chrisdone.com/posts/measuring-duration-in-haskell

module BigO (
  profile
) where

import Control.Monad.Random
import Data.Time
import Data.List
import System.Timeout
import System.IO

type Implementation g = Int -> g -> IO ()

type ImplementationT g = Int -> g -> IO Int

debug = hPutStrLn stderr

-- returns duration in μs
measure :: IO () -> IO Int
measure op = do
    start <- getCurrentTime
    op
    end <- getCurrentTime
    return $ floor $ 1000000 * (diffUTCTime end start)

profile :: String -> Implementation StdGen -> IO ()
profile name f' = do
  let f = \n g -> measure $ f' n g
  let g = mkStdGen 42
  Just (minO, maxO) <- logProfile g f (1000, 1000000)
  let n0 = (5 * (pow 10 minO ))
      nMax = (5 * (pow 10 maxO ))
      nStep = (nMax - n0) `div` 100
  let ns = unfoldr (\n -> let nnext = (n + nStep) in if nnext > nMax then Nothing else Just (nnext, nnext)) n0
  xs <- mapM (\n -> f n g >>= (\t -> return (n, t))) ns
  mapM_ (\(n, t) -> putStrLn (name ++ "," ++ show n ++ "," ++ show t)) xs

pow :: Float -> Float -> Int
pow x e = floor (x ** e)

step = 0.25

findMinLog :: RandomGen g => g -> ImplementationT g -> (Int, Int) -> Float -> IO (Maybe Float)
findMinLog g f ts@(tmin, tmax) p = do
  let n = 5 * (pow 10 p)
  m <- timeout tmax (f n g)
  case m of
    Nothing -> debug $ "MIN for n=" ++ (show n) ++ " TIMEOUT"
    Just t -> debug $ "MIN for n=" ++ (show n) ++ " t=" ++ (show t)
  case m of
    Nothing -> return Nothing
    Just t -> if t < tmin
              then findMinLog g f ts (p + step)
              else return (Just p)

findMaxLog :: RandomGen g => g -> ImplementationT g -> (Int, Int) -> Float -> IO Float
findMaxLog g f ts@(tmin, tmax) p = do
  let n = 5 * (pow 10 p)
  m <- timeout tmax (f n g)
  case m of
    Nothing -> debug $ "MAX for n=" ++ (show n) ++ " TIMEOUT"
    Just t -> debug $ "MAX for n=" ++ (show n) ++ " t=" ++ (show t)
  case m of
    Nothing -> return (p - step)
    Just t -> findMaxLog g f ts (p + step)

logProfile :: RandomGen g => g -> ImplementationT g -> (Int, Int) -> IO (Maybe (Float, Float))
logProfile g f ts@(tmin, tmax) = do
  minOrder <- findMinLog g f ts 0
  case minOrder of
    Nothing -> error "Could not find any input size that would compute in the specified time span"
    Just minO -> do
      maxO <- findMaxLog g f ts (minO + step)
      return $ Just (minO, maxO)

