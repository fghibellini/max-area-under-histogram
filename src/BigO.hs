
-- don't do this. see https://chrisdone.com/posts/measuring-duration-in-haskell

module BigO (
  profile
) where

import Control.Monad.Random
import Data.Time
import Data.List
import System.Timeout
import System.IO
import Data.Maybe

type Implementation g = Int -> g -> IO ()

type ImplementationT g = Int -> g -> IO Int

data LOG_LEVEL = NONE | BASIC | ALL deriving (Eq, Ord)

log_level = NONE

debug :: String -> LOG_LEVEL -> String -> IO ()
debug ctx lvl s = if lvl <= log_level 
                  then hPutStrLn stderr (ctx ++ ":" ++ s)
                  else return ()

average xs = sum xs `div` (length xs)

-- returns duration in μs
measure :: IO () -> IO Int
measure op = sequence (replicate 5 (measure' op)) >>= (return . average)

-- returns duration in μs
measure' :: IO () -> IO Int
measure' op = do
    start <- getCurrentTime
    op
    end <- getCurrentTime
    return $ floor $ 1000000 * (diffUTCTime end start)

seqWithStep :: Int -> Int -> Int -> [Int]
seqWithStep n0 nMax step = unfoldr f n0 
  where f n = let nnext = (n + step)
              in if nnext > nMax
                 then Nothing
                 else Just (nnext, nnext)

profile :: String -> Implementation StdGen -> IO ()
profile name f = do
  let g = mkStdGen 42
      timedF = \n g -> measure $ f n g
  Right (minOrder, maxOrder) <- logProfile g timedF (1000, 1000000) -- (1ms, 1s)
  let n0 = computeN minOrder
      nMax = computeN maxOrder
      nStep = (nMax - n0) `div` 100
  let ns = seqWithStep n0 nMax nStep
  xs <- mapM (\n -> timedF n g >>= (\t -> return (n, t))) ns
  mapM_ (\(n, t) -> putStrLn (name ++ "," ++ show n ++ "," ++ show t)) xs

step0 = 0.5
minStep = 0.005

pow :: Float -> Float -> Int
pow x e = floor (x ** e)

-- computes the input size from the order
-- on the order of units it makes sense to have at
-- least a few elements so we multiply by 5 (on bigger
-- numbers multiplying by 5 doesn't really change the order)
computeN p = 5 * (pow 10 p)

findMinLog :: RandomGen g => g -> ImplementationT g -> (Int, Int) -> Float -> IO (Maybe Float)
findMinLog g f ts@(tmin, tmax) p = do
  let n = computeN p
  m <- timeout tmax (f n g)
  debug "FIND_MIN_LOG" ALL $ "for n=" ++ (show n) ++ " t=" ++ (fromMaybe "TIMEOUT" (fmap show m))
  case m of
    Nothing -> return Nothing
    Just t -> if t < tmin
              then findMinLog g f ts (p + step0)
              else return (Just p)

findMaxLog :: RandomGen g => g -> ImplementationT g -> (Int, Int) -> Float -> Float -> IO Float
findMaxLog g f ts@(tmin, tmax) p step = do
  let n = computeN p
  m <- timeout tmax (f n g)
  debug "FIND_MAX_LOG" ALL $ "for n=" ++ (show n) ++ " t=" ++ (fromMaybe "TIMEOUT" (fmap show m))
  case m of
    Nothing | step >= minStep -> findMaxLog g f ts (p - step + step') step'
    Nothing | otherwise -> return (p - step)
    Just t -> findMaxLog g f ts (p + step) step
  where step' = step / 2.0

-- for given miminal and maximal durations in μs it returns
-- the log10 order span of the input size that the function can process
-- i.e. it answers the question "if I want the function to run between 0.5s and 2s how big the input should be?"
logProfile :: RandomGen g => g -> ImplementationT g -> (Int, Int) -> IO (Either String (Float, Float))
logProfile g f ts@(tmin, tmax) = do
  resMin <- findMinLog g f ts 0
  case resMin of
    Nothing -> return $ Left "Could not find any input size that would compute in the specified time span"
    Just minOrder -> do
      maxOrder <- findMaxLog g f ts (minOrder + step0) step0
      let n0 = computeN minOrder
          nMax = computeN maxOrder
      debug "LOG_PROFILE" BASIC $ "minN = " ++ (show n0)
      debug "LOG_PROFILE" BASIC $ "maxN = " ++ (show nMax)
      return $ Right (minOrder, maxOrder)

