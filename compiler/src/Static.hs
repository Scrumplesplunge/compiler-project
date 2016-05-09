module Static where

import Data.Binary.Put
import qualified Data.ByteString.Lazy as BL
import Data.Char
import Data.Int
import Data.List
import Data.Ord
import Data.Word

-- Compile-time constant arrays.
data Static = WordArray [Int32]  -- Constant array of words.
            | ByteArray String   -- Constant array of bytes.
  deriving (Eq, Show)

-- Encode basic types as bytes.
encode_word :: Int32 -> Put
encode_word = putWord32le . fromIntegral

encode_byte :: Char -> Put
encode_byte = putWord8 . fromIntegral . ord

-- Encode array types as bytes.
encode :: Static -> Put
encode (WordArray ws) = sequence_ $ map encode_word ws
encode (ByteArray bs) = sequence_ $ map encode_byte bs

mapSnd :: (b -> c) -> [(a, b)] -> [(a, c)]
mapSnd f = foldr (\(a, b) ys -> (a, f b) : ys) []

-- Encode all static data as bytes.
encode_all :: [(Int32, Static)] -> [(Int32, BL.ByteString)]
encode_all = mapSnd (runPut . encode)

-- Compact consecutive byte sequences into single instances.
compact :: [(Int32, BL.ByteString)] -> [(Int32, BL.ByteString)]
compact ss = group ss'
  where ss' = sortBy (comparing fst) ss
        group [] = []
        group [s] = [s]
        group ((loc_1, val_1) : (loc_2, val_2) : ss) =
          if loc_1 + fromIntegral (BL.length val_1) == loc_2 then
            -- Combine the data from the two entries together.
            (loc_1, BL.append val_1 val_2) : group ss
          else
            -- Leave the first entry as distinct.
            (loc_1, val_1) : group ((loc_2, val_2) : ss)

-- Store a binary blob with a prefix which locates it and states its length.
encode_located :: (Int32, BL.ByteString) -> BL.ByteString
encode_located = runPut . encode_located'

encode_located' (location, value) = do
  encode_word location
  encode_word . fromIntegral . BL.length $ value
  putLazyByteString value

-- Construct a binary blob which stores all the provided static data.
make_blob :: [(Int32, Static)] -> (Int32, [Word8])
make_blob bs = (location, BL.unpack value)
  where xs = compact (encode_all bs)
        (location, value) =
          if length xs == 1 then
            head xs
          else
            error "Static data should be contiguous."
