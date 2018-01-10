import Data.List
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Digest.Pure.MD5       as MD5

genHash :: String -> String
genHash secret = show hash
    where hash  = MD5.md5 bytes
          bytes = C.pack secret

genFullSecrets :: String -> [String]
genFullSecrets secret = [ secret ++ show number | number <- [1..] ]

hashStartsWithZeros :: String -> Bool
hashStartsWithZeros hash = isPrefixOf "000000" hash -- Number of zeroes...

main = do
    print $ (\(Just i) -> i + 1) $ findIndex hashStartsWithZeros $ map genHash $ genFullSecrets secret
        where secret  = "iwrupvqb"
