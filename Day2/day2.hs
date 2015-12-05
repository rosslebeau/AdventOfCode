import System.IO
import Data.List
import Data.List.Split (splitOn)

type Gift = (Int, Int, Int)

-- Part 1
area :: Gift -> Int
area (l, w, h) = (2 * l * w) + (2 * w * h) + (2 * h * l)

areaOfSmallestSide :: Gift -> Int
areaOfSmallestSide (l, w, h) = s1 * s2
    where [s1, s2, _] = sort [l, w, h]

wrappingPaperAreaForGift :: Gift -> Int
wrappingPaperAreaForGift gift = (area gift) + (areaOfSmallestSide gift)
--wrappingPaperAreaForGift gift = foldl (\acc x -> acc + (x gift)) 0 [area, areaOfSmallestSide]

--  Part 2
bowLengthForGift :: Gift -> Int
bowLengthForGift (l, w, h) = l * w * h

perimeterOfSmallestSide :: Gift -> Int
perimeterOfSmallestSide (l, w, h) = (2 * s1) + (2 * s2)
    where [s1, s2, _] = sort [l, w, h]

ribbonlengthForGift :: Gift -> Int
ribbonlengthForGift gift = (perimeterOfSmallestSide gift) + (bowLengthForGift gift)

-- Shared
giftFromString :: String -> Gift
giftFromString s = (read l :: Int, read w :: Int, read h :: Int)
    where [l, w, h] = splitOn "x" s

-- Problem states gifts are one-per-line, in the format of "lxwxh", e.g. "6x2x3"
giftListFromInput :: String -> [Gift]
giftListFromInput input = map giftFromString (words input)

main :: IO ()
main = do
    inputHandle <- openFile "input" ReadMode
    input <- hGetContents inputHandle

    print $ ("Part 1: " ++) . show $ sum $ map wrappingPaperAreaForGift (giftListFromInput input)
    print $ ("Part 2: " ++) . show $ sum $ map ribbonlengthForGift (giftListFromInput input)
