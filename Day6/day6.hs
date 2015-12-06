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
                                otherwise -> error "no"

coordinatesFromRectCorners :: Coordinate -> Coordinate -> [Coordinate]
coordinatesFromRectCorners (lx, ly) (ux, uy) = concat $ map ((zip [lx..ux]) . repeat) [ly..uy]

initialHashMap :: Lights
initialHashMap = turnOff HashMap.empty (coordinatesFromRectCorners (0, 0) (999, 999))

turnOn :: Lights -> [Coordinate] -> Lights
turnOn lights coords = foldl' (\lights coord -> HashMap.insert coord 1 lights) lights coords

turnOff :: Lights -> [Coordinate] -> Lights
turnOff lights coords = foldl' (\lights coord -> HashMap.insert coord 0 lights) lights coords

toggle :: Lights -> [Coordinate] -> Lights
toggle lights coords = foldl' (\lights coord -> HashMap.insert coord (oppositeVal coord lights) lights) lights coords
    where oppositeVal c l = case HashMap.lookup c l of Just x -> x `xor` 1
                                                       Nothing -> error "Nope"

performOperations :: Lights -> [String] -> Lights
performOperations lights [] = lights
performOperations lights (o:os) = performOperations (performOperation lights o) os

performOperation :: Lights -> String -> Lights
performOperation lights operation
    | isPrefixOf "turn on" operation = let (upperLeft, lowerRight) = (rectCornersFromString operation)
                                       in turnOn lights (coordinatesFromRectCorners upperLeft lowerRight)
    | isPrefixOf "turn off" operation = let (upperLeft, lowerRight) = (rectCornersFromString operation)
                                       in turnOff lights (coordinatesFromRectCorners upperLeft lowerRight)
    | isPrefixOf "toggle" operation = let (upperLeft, lowerRight) = (rectCornersFromString operation)
                                       in toggle lights (coordinatesFromRectCorners upperLeft lowerRight)
    | otherwise = error "hi"

main :: IO ()
main = do
    inputHandle <- openFile "input" ReadMode
    input <- hGetContents inputHandle

    let lights = performOperations initialHashMap (lines input)
    
    print $ HashMap.foldl' (\acc light -> acc + light) 0 lights
