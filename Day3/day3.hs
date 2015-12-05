import System.IO
import Data.List
import Data.Char (isSpace)

type House = (Int, Int)

everyOther :: [a] -> [a]
everyOther [] = []
everyOther (x:xs) = x : (everyOther (drop 1 xs))

nextHouse :: House -> Char -> House
nextHouse (startX, startY) direction
    | direction == '^' = (startX, startY + 1)
    | direction == '>' = (startX + 1, startY)
    | direction == 'v' = (startX, startY - 1)
    | direction == '<' = (startX - 1, startY)
    | otherwise = error $ "Unexpected input: " ++ [direction]

housesFromDirections :: [Char] -> [House]
housesFromDirections directions = scanl nextHouse (0,0) directions

housesFromAlternatingDirections :: [Char] -> [House]
housesFromAlternatingDirections directions = concat [(housesFromDirections (everyOther directions)), (housesFromDirections (everyOther (drop 1 directions)))]

main :: IO ()
main = do
    inputHandle <- openFile "input" ReadMode
    input <- hGetContents inputHandle

    let inputNoWhitespace = filter (not . isSpace) input

    print $ ("Part 1: " ++) . show $ length $ nub $ housesFromDirections (inputNoWhitespace)
    print $ ("Part 2: " ++) . show $ length $ nub $ housesFromAlternatingDirections (inputNoWhitespace)
