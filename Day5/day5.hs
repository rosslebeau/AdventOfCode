import System.IO
import Text.Regex.PCRE

isNice :: String -> Bool
isNice string = (string =~ "(a.*|e.*|i.*|o.*|u.*){3,}" :: Bool) && (string =~ "(.)\\1" :: Bool) && (not (string =~ "ab|cd|pq|xy"))

main :: IO ()
main = do
    inputHandle <- openFile "input" ReadMode
    input <- hGetContents inputHandle

    print $ length $ filter (== True) $ map isNice (words input)
    
    --print $ matchRegex (mkRegex "(.)\\1{2,}") "aa"
