module HashSplit
    ( Config(..)
    , validConfig
    , RollingHash
    , split
    , rrs
    , rrs1
    ) where

import Data.Word

type RollingHash = [Word8] -> Word32

data Config = Config
    { splitMin  :: !Int
    , splitMax  :: !Int
    , hash      :: RollingHash
    , winSize   :: !Int
    , threshold :: !Int
    }

validConfig :: Config -> Bool
validConfig cfg =
    splitMax cfg >= splitMin cfg
    && splitMin cfg >= winSize cfg
    && winSize cfg > 0

splitIndex :: Config -> [Word8] -> Int
splitIndex cfg bytes = go 0 bytes where
    go i [] = i
    go i xs
        | i == splitMax cfg = i
        | i < splitMin cfg = next
        | hash cfg (take (winSize cfg) xs) `mod` (2 ^ threshold cfg) == 0 = i
        | otherwise = next
      where
        next = go (i+1) (drop 1 xs)

split :: Config -> [Word8] -> [[Word8]]
split _cfg [] = []
split cfg bytes =
    let
        i = splitIndex cfg bytes
        prefix = take i bytes
        remainder = drop i bytes
    in
    [prefix] ++ split cfg remainder

rrs :: Word32 -> Word32 -> RollingHash
rrs m c xs = skl
  where
    k = 0
    l = k + fromIntegral (length xs) - 1
    xsI = zip xs [k..l]

    akl = sum [ fromIntegral x + c | x <- xs ] `mod` m
    bkl = sum [ (l - i + 1) * (fromIntegral x + c) | (x, i) <- xsI ] `mod` m
    skl = bkl + (2^16) * akl

rrs1 :: RollingHash
rrs1 = rrs (2^16) 31
