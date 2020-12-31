{-# LANGUAGE OverloadedStrings #-}

import Data.List (sort)
import AOC

mainA :: IO ()
mainA = do
  ls <- readFileIntLines "Day10Input"
  print . (\(a,b)->a*b) . countDiffs $ sort ls

mainB :: IO ()
mainB = do
  ls <- readFileIntLines "Day10Input"
  let max = maximum ls
  print . (countCombinations2 []) . reverse . countVertices $ sort (0:max+3:ls)

countDiffs :: [Int] -> (Int, Int)
countDiffs (x:[]) = (1,1)
countDiffs (x:y:ls) =
  if y - x == 1
    then (\ (a,b)->(a+1,b)) $ countDiffs (y:ls)
    else (\ (a,b)->(a,b+1)) $ countDiffs (y:ls)

countCombinations :: [Int] -> Int
countCombinations (a:b:c:d:ls) = do
  let 
    dCount = if d-a <= 3 then countCombinations (d:ls) else 0
    cCount = if c-a <= 3 then  countCombinations (c:d:ls) else 0
  (+cCount) . (+dCount) $ countCombinations (b:c:d:ls)
countCombinations _ = 1

countVertices :: [Int] -> [Int]
countVertices (x:ls) = do
  let val = length . (filter (==True)) $ (`elem` [x+1 .. x+3]) <$> ls
  val:countVertices ls
countVertices [] = []

countCombinations2 :: [Int] -> [Int] -> Int
countCombinations2 [] (_:_:_:ls) = countCombinations2 [1,1,1] ls
countCombinations2 (a:b:c:[]) (x:ls) = 
  case x of
    3 -> countCombinations2 ((a+b+c):[a,b]) ls
    2 -> countCombinations2 ((a+b):[a,b]) ls
    1 -> countCombinations2 (a:[a,b]) ls
countCombinations2 a [] = head a



--[0, 1, 4, 5, 6, 7, 10, 11, 12, 15, 16, 19, 22]
--[1, 1, 3, 2, 1, 1, 2,  1,  1,  1,  1,  1,  0]
-- 8  8  8  4  2  2  2   1   1   1   1   1


--                                        0
--                                        |
--                                        1
--                                        |
--                      _________________ 4 _____________
--                     /                  |              \
--                    5                   6               7
--                   / \                 /               /
--                  /   \               7               10
--                 6     \             /               /  \
--                /       \           10              11  12
--               7         7         /  \            /    /
--              /         /         11  12          12   15
--             10        10        /    /          /    /
--            /  \      /  \      12   15         15   16
--           11  12    11  12    /    /          /    /
--          /    /    /    /    15   16         16   19
--         12   15   12   15   /    /          /    /
--        /    /    /    /    16   19         19   22
--       15   16   15   16   /    /          /
--      /    /    /    /    19   22         22
--     16   19   16   19   /
--    /    /    /    /    22
--   19   22   19   22
--  /         /
-- 22        22


--[    3,    3,   2,   1,   1,   3,   3,  2,  1,  1,  1,  3,  2, 1, 1, 2, 1, 1, 1, 3, 3, 2,1,1,1,1,1,3,3,2,1,1,0]
-- 19208,10976,5488,2744,2744,2744,1568,784,392,392,392,392,196,98,98,98,49,49,49,49,28,14,7,7,7,7,7,7,4,2,1,1



