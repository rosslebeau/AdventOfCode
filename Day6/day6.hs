import System.IO
import Data.List
import Data.Bits
import qualified Data.HashMap.Strict as HashMap
import Text.Regex.PCRE as R

type Coordinate = (Int, Int)
type Lights = HashMap.HashMap Coordinate Int

rectCornersFromString :: String -> (Coordinate, Coordinate)
rectCornersFromString string = case getAllTextSubmatches (string =~ "([0-9]+),([0-9]+) through ([0-9]+),([0-9]+)" :: AllTextSubmatches [] String) of
                                [_, lx, ly, ux, uy] -> ((read lx :: Int, read ly :: Int), (read ux :: Int, read uy :: Int))
                                otherwise -> error ("Coordinates not found in instruction: " ++ string)

coordinatesFromRectCorners :: Coordinate -> Coordinate -> [Coordinate]
coordinatesFromRectCorners (lx, ly) (ux, uy) = concat $ map ((zip [lx..ux]) . repeat) [ly..uy]

initialLights :: Lights
initialLights = turnOff1 HashMap.empty (coordinatesFromRectCorners (0, 0) (999, 999))

-- Part 1

turnOn1 :: Lights -> [Coordinate] -> Lights
turnOn1 lights coords = foldl' (\lights coord -> HashMap.insert coord 1 lights) lights coords

turnOff1 :: Lights -> [Coordinate] -> Lights
turnOff1 lights coords = foldl' (\lights coord -> HashMap.insert coord 0 lights) lights coords

toggle1 :: Lights -> [Coordinate] -> Lights
toggle1 lights coords = foldl' (\lights coord -> HashMap.insert coord (oppositeVal coord lights) lights) lights coords
    where oppositeVal c l = case HashMap.lookup c l of Just x -> x `xor` 1
                                                       Nothing -> error ("Coordinate out of bounds: " ++ (show c))

performOperations1 :: Lights -> [String] -> Lights
performOperations1 lights [] = lights
performOperations1 lights (o:os) = performOperations1 (performOperation1 lights o) os

performOperation1 :: Lights -> String -> Lights
performOperation1 lights operation
    | isPrefixOf "turn on" operation = turnOn1 lights (coordinatesFromRectCorners upperLeft lowerRight)
    | isPrefixOf "turn off" operation = turnOff1 lights (coordinatesFromRectCorners upperLeft lowerRight)
    | isPrefixOf "toggle" operation = toggle1 lights (coordinatesFromRectCorners upperLeft lowerRight)
    | otherwise = error ("Unsupported operation in instruction: " ++ operation)
        where (upperLeft, lowerRight) = (rectCornersFromString operation)

-- Part 2

turnOn2 :: Lights -> [Coordinate] -> Lights
turnOn2 lights coords = foldl' (\lights coord -> HashMap.insert coord (newVal coord lights) lights) lights coords
    where newVal c l = case HashMap.lookup c l of Just x -> x + 1
                                                  Nothing -> error ("Coordinate out of bounds: " ++ (show c))

turnOff2 :: Lights -> [Coordinate] -> Lights
turnOff2 lights coords = foldl' (\lights coord -> HashMap.insert coord (newVal coord lights) lights) lights coords
    where newVal c l = case HashMap.lookup c l of Just x -> max (x - 1) 0
                                                  Nothing -> error ("Coordinate out of bounds: " ++ (show c))

toggle2 :: Lights -> [Coordinate] -> Lights
toggle2 lights coords = foldl' (\lights coord -> HashMap.insert coord (newVal coord lights) lights) lights coords
    where newVal c l = case HashMap.lookup c l of Just x -> x + 2
                                                  Nothing -> error ("Coordinate out of bounds: " ++ (show c))

performOperations2 :: Lights -> [String] -> Lights
performOperations2 lights [] = lights
performOperations2 lights (o:os) = performOperations2 (performOperation2 lights o) os

performOperation2 :: Lights -> String -> Lights
performOperation2 lights operation
    | isPrefixOf "turn on" operation = turnOn2 lights (coordinatesFromRectCorners upperLeft lowerRight)
    | isPrefixOf "turn off" operation = turnOff2 lights (coordinatesFromRectCorners upperLeft lowerRight)
    | isPrefixOf "toggle" operation = toggle2 lights (coordinatesFromRectCorners upperLeft lowerRight)
    | otherwise = error ("Unsupported operation in instruction: " ++ operation)
        where (upperLeft, lowerRight) = (rectCornersFromString operation)

main :: IO ()
main = do
    inputHandle <- openFile "input" ReadMode
    input <- hGetContents inputHandle

    let lights1 = performOperations1 initialLights (lines input)
    print $ ("Part 1: " ++) . show $ HashMap.foldl' (\acc light -> acc + light) 0 lights1

    let lights2 = performOperations2 initialLights (lines input)
    print $ ("Part 2: " ++) . show $ HashMap.foldl' (\acc light -> acc + light) 0 lights2
