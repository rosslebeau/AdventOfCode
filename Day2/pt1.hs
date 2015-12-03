import System.IO
import Data.List
import Data.List.Split (splitOn)

type Gift = (Int, Int, Int)

area :: Gift -> Int
area (l, w, h) = (2 * l * w) + (2 * w * h) + (2 * h * l)

areaOfSmallestSide :: Gift -> Int
areaOfSmallestSide (l, w, h) = s1 * s2
    where [s1, s2, _] = sort [l, w, h]

wrappingPaperAreaForGift :: Gift -> Int
wrappingPaperAreaForGift gift = (area gift) + (areaOfSmallestSide gift)
--wrappingPaperAreaForGift gift = foldl (\acc x -> acc + (x gift)) 0 [area, areaOfSmallestSide]

giftFromString :: String -> Gift
giftFromString s = (read l :: Int, read w :: Int, read h :: Int)
    where [l, w, h] = splitOn "x" s

-- Problem states gifts are one-per-line, in the format of "lxwxh", e.g. "6x2x3"
giftListFromInput :: String -> [Gift]
giftListFromInput input = map giftFromString (words input)

main :: IO ()
main = do
    inputHandle <- openFile "pt1_input" ReadMode
    input <- hGetContents inputHandle

    print $ sum $ map wrappingPaperAreaForGift (giftListFromInput input)
