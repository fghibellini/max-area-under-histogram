
module Histogram (
    histogramCorrect
  , histogramFast
) where

-- CORRECT

histogramCorrect :: [Int] -> [(Int, Int)]
histogramCorrect [] = []
histogramCorrect h =
    let n = length h
        allRectangles = [ (x,y) | x <- [1..n], y <- [1..n], x <= y ]
        allAreas = [ getArea r | r <- allRectangles ]
        maxArea = maximum allAreas
        maxRects = [ r | r <- allRectangles, getArea r == maxArea ]

        getArea :: (Int, Int) -> Int
        getArea (start, end) = let width = end - start + 1
                                   subhistogram = (take width (drop (start - 1) h))
                                   height = minimum subhistogram
                               in height * width
    in maxRects


-- FAST

type Res = (Int, [(Int, Int)])

histogramFast :: [Int] -> [(Int, Int)]
histogramFast h =
    let n = length h
        res0 = (0, [])
        (res1, stack') = foldl stepFn (res0, []) (zip [1..] h)
        (maxArea, maxRects) = close (n + 1) res1 stack'

        stepFn :: (Res, [(Int, Int)]) -> (Int, Int) -> (Res, [(Int, Int)])
        stepFn (res, stack@[]) (i, x) = (res, [(i, x)])
        stepFn (res, stack@((topIndex, topHeight):rest)) (i, x) | x > topHeight = (res, (i, x):stack)
        stepFn (res, stack@((topIndex, topHeight):rest)) (i, x) | x == topHeight = (res, stack)
        stepFn (res, stack@((topIndex, topHeight):rest)) (i, x) | x < topHeight =
            let biggerElements = takeWhile (\(_, y) -> y > x) stack
                rest' = drop (length biggerElements) stack
                res' = close i res biggerElements
                (_, topHeight'):_ = rest'
                rest'' = if not (null rest') && (x == topHeight') then rest' else (fst (last biggerElements), x):rest'
            in (res', rest'') :: (Res, [(Int, Int)])

        close :: Int -> Res -> [(Int, Int)] -> Res         
        close i res0 popped = foldl (\(cmax, rects) (j, y) ->
                            let area = ((i - j) * y)
                                rect = (j, i - 1)
                            in  if cmax > area
                                then (cmax, rects)
                                else if cmax == area
                                then (cmax, rect:rects)
                                else (area, [rect])) res0 popped

    in maxRects

