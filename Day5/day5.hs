import System.IO
import Text.Regex.PCRE

isNice1 :: String -> Bool
isNice1 string = (string =~ "(a.*|e.*|i.*|o.*|u.*){3,}" :: Bool) && (string =~ "(.)\\1" :: Bool) && (not (string =~ "ab|cd|pq|xy"))

isNice2 :: String -> Bool
isNice2 string = (string =~ "(..).*\\1" :: Bool) && (string =~ "(.).\\1" :: Bool)

main :: IO ()
main = do
    inputHandle <- openFile "input" ReadMode
    input <- hGetContents inputHandle

    print $ ("Part 1: " ++) . show $ length $ filter (== True) $ map isNice1 (words input)
    print $ ("Part 2: " ++) . show $ length $ filter (== True) $ map isNice2 (words input)
