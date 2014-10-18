{-# LANGUAGE BangPatterns #-}
import Data.Attoparsec
import qualified Data.ByteString as B
import Data.Time.Clock
import Butter.Core.Bencode

main :: IO ()
main = do
    start <- getCurrentTime
    f <- B.readFile "benchmark/test.torrent"
    let !parsed = parseOnly parseBEValues f
    end <- getCurrentTime
    putStrLn $ "`parseOnly` took " ++ show (diffUTCTime end start)
