import Data.List(reverse, sort)

import qualified Data.Char    as Char
import qualified Data.Text    as Text
import qualified Data.Text.IO as TextIO

import Debug.Trace(trace)

main :: IO ()
main = do
   chemicalInput <- TextIO.readFile "day5solution"
   -- print $ findReaction $ Text.unpack chemicalInput
   print $ puzzle2 $ Text.unpack chemicalInput

puzzle2 :: String -> Int
puzzle2 reactions =
   let
      abcLower = "abcdefghijklmnopqrstuvwxyz"
      abcUpper = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
      filterReactions :: (Char, Char) -> String
      filterReactions (lower, upper) =
         let
            lowerFiltered = (flip filter) reactions <$> (flip notElem) <$> [[lower]]
            bothFiltered = (flip filter) (concat lowerFiltered) <$> (flip notElem) <$> [[upper]]
         in (concat bothFiltered)
      filteredReactions = filterReactions <$> (zip abcLower abcUpper)
      parsedFilteredReactions = findReaction <$> filteredReactions
   in
      length . head . reverse $ sort parsedFilteredReactions
findReaction :: String -> String
findReaction reactions =
   let
      (popped, newReactions) = findReaction' reactions
   in
      if popped then
         findReaction newReactions
      else
         newReactions

findReaction' :: String -> (Bool, String)
findReaction' [] = (False, [])
findReaction' (a:[]) = (False, [a])
findReaction' (a:b:chain) =
   if Char.toLower a == Char.toLower b then
      case (Char.isUpper a, Char.isUpper b) of
         (False, True) -> (True, chain)
         (True, False) -> (True, chain)
         _             -> 
            let (popped, newReactions) = findReaction' ([b] ++ chain)
            in (False || popped, [a] ++ newReactions)
   else
      let (popped, newReactions) = findReaction' ([b] ++ chain)
      in (False || popped, [a] ++ newReactions)
