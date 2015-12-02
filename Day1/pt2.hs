import System.IO 

valueForChar :: Char -> Int
valueForChar '(' = 1
valueForChar ')' = -1
valueForChar _ = 0

main :: IO ()
main = do
    inputHandle <- openFile "pt2_input" ReadMode
    input <- hGetContents inputHandle

    print $ length $ takeWhile (>=0) (scanl (+) 0 (map valueForChar input))
