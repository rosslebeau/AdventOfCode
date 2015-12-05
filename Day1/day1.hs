import System.IO
import Data.List

valueForChar :: Char -> Int
valueForChar '(' = 1
valueForChar ')' = -1
valueForChar _ = 0

firstNegativeFloor :: [Char] -> Maybe Int
firstNegativeFloor directions = case (elemIndex (-1) $ takeWhile (>=0) (scanl (+) 0 (map valueForChar directions))) of
                                    Just i -> Just (i + 1)
                                    Nothing -> Nothing

main :: IO ()
main = do
    inputHandle <- openFile "input" ReadMode
    input <- hGetContents inputHandle

    print $ ("Part 1: " ++) . show $ sum $ map valueForChar input
    print $ ("Part 2: " ++) . show $ elemIndex (-1) $ (scanl (+) 0 (map valueForChar input))
