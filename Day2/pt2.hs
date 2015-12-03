import System.IO
import Data.List
import Data.List.Split (splitOn)

type Gift = (Int, Int, Int)

bowLengthForGift :: Gift -> Int
bowLengthForGift (l, w, h) = l * w * h

perimeterOfSmallestSide :: Gift -> Int
perimeterOfSmallestSide (l, w, h) = (2 * s1) + (2 * s2)
    where [s1, s2, _] = sort [l, w, h]

ribbonlengthForGift :: Gift -> Int
ribbonlengthForGift gift = (perimeterOfSmallestSide gift) + (bowLengthForGift gift)

giftFromString :: String -> Gift
giftFromString s = (read l :: Int, read w :: Int, read h :: Int)
    where [l, w, h] = splitOn "x" s

-- Problem states gifts are one-per-line, in the format of "lxwxh", e.g. "6x2x3"
giftListFromInput :: String -> [Gift]
giftListFromInput input = map giftFromString (words input)

main :: IO ()
main = do
    inputHandle <- openFile "pt2_input" ReadMode
    input <- hGetContents inputHandle

    print $ sum $ map ribbonlengthForGift (giftListFromInput input)
