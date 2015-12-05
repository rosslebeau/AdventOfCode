import System.IO
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.ByteString.Builder as ByteString.Builder
import Crypto.Hash.MD5
import Text.Printf

incrementalHashes :: String -> [ByteString.ByteString]
incrementalHashes base = incrementalHashesFrom base 1
    where incrementalHashesFrom base start = (hash $ ByteString.Char8.pack (base ++ (show start))):(incrementalHashesFrom base (start + 1))

anyIsPrefix :: [ByteString.ByteString] -> ByteString.ByteString -> Bool
anyIsPrefix [] _ = False
anyIsPrefix [x] string = ByteString.isPrefixOf x string
anyIsPrefix (x:xs) string = (anyIsPrefix [x] string) || (anyIsPrefix xs string)

main :: IO ()
main = do
    -- Since the ByteStrings are backed by Word8 arrays, each element must be 2 characters in hex.
    -- For hex "00000", we need to create the ByteStrings for "000000", "000001"..."00000f" and check them all
    let requiredPrefixesPart1 = [(ByteString.pack [0, 0, 0]),
                                 (ByteString.pack [0, 0, 1]),
                                 (ByteString.pack [0, 0, 2]),
                                 (ByteString.pack [0, 0, 3]),
                                 (ByteString.pack [0, 0, 4]),
                                 (ByteString.pack [0, 0, 5]),
                                 (ByteString.pack [0, 0, 6]),
                                 (ByteString.pack [0, 0, 7]),
                                 (ByteString.pack [0, 0, 8]),
                                 (ByteString.pack [0, 0, 9]),
                                 (ByteString.pack [0, 0, 10]),
                                 (ByteString.pack [0, 0, 11]),
                                 (ByteString.pack [0, 0, 12]),
                                 (ByteString.pack [0, 0, 13]),
                                 (ByteString.pack [0, 0, 14]),
                                 (ByteString.pack [0, 0, 15])]

    let requiredPrefixPart2 = ByteString.pack [0, 0, 0]

    print $ ("Part 1: " ++) . show $ (1 +) $ length $ takeWhile (not . (anyIsPrefix requiredPrefixesPart1)) (incrementalHashes "yzbqklnj")
    print $ ("Part 2: " ++) . show $ (1 +) $ length $ takeWhile (not . (ByteString.isPrefixOf requiredPrefixPart2)) (incrementalHashes "yzbqklnj")
    --print $ ByteString.isPrefixOf requiredPrefix (hash (ByteString.Char8.pack "abcdef609043"))
    --print $ hex (hash (ByteString.Char8.pack "abcdef609043"))
    --print $ hex $ ByteString.pack [0, 0, 300]
