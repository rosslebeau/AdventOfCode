import System.IO 

valueForChar :: Char -> Int
valueForChar '(' = 1
valueForChar ')' = -1
valueForChar _ = 0

main :: IO ()
main = do
    inputHandle <- openFile "input" ReadMode
    input <- hGetContents inputHandle

    print $ sum $ map valueForChar input
