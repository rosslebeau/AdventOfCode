import System.IO
import Data.List

type House = (Int, Int)

nextHouse :: House -> Char -> House
nextHouse (startX, startY) direction
    | direction == '^' = (startX, startY + 1)
    | direction == '>' = (startX + 1, startY)
    | direction == 'v' = (startX, startY - 1)
    | direction == '<' = (startX - 1, startY)
    | otherwise = (0,0) -- It's specified that this won't happen

housesFromDirections :: [Char] -> [House]
housesFromDirections directions = scanl nextHouse (0,0) directions

main :: IO ()
main = do
    inputHandle <- openFile "pt1_input" ReadMode
    input <- hGetContents inputHandle

    print $ length $ nub $ housesFromDirections (input)
