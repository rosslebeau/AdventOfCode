import System.IO
import Data.List

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
    | otherwise = (0,0) -- It's specified that this won't happen

housesFromDirections :: [Char] -> [House]
housesFromDirections directions = scanl nextHouse (0,0) directions

housesFromAlternatingDirections :: [Char] -> [House]
housesFromAlternatingDirections directions = concat [(housesFromDirections (everyOther directions)), (housesFromDirections (everyOther (drop 1 directions)))]

main :: IO ()
main = do
    inputHandle <- openFile "input" ReadMode
    input <- hGetContents inputHandle

    print $ ("Part 1: " ++) . show $ length $ nub $ housesFromDirections (input)
    print $ ("Part 2: " ++) . show $ length $ nub $ housesFromAlternatingDirections (input)