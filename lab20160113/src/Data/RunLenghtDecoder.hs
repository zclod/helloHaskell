module Data.RunLenghtDecoder (decode, decodeStream, writeRunLengh)where

import           Data.Monoid
import           Data.Word
import           Data.Binary.Get (runGet)
import           Data.Binary.Bits.Get
import           Data.Binary.Bits.Put
import           Data.RunLenghtEncoder (RunLenght(..))
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B

toInt :: Word8 -> Int
toInt = fromInteger . toInteger

serialize :: RunLenght -> B.ByteString
serialize (Run n w) = B.pack $ replicate n w
serialize (Stream s) = s

parseRun :: BitGet RunLenght
parseRun = do
  rlenght <- getWord8 6
  if rlenght == 0
    then return $ Run 0 0
    else do
         w <- getWord8 8
         return $ Run (toInt $ rlenght + 1) w

parseStream :: BitGet RunLenght
parseStream = do
  slenght <- getWord8 6
  stream <- getByteString (toInt $ slenght + 1)
  return $ Stream stream

parseToken :: BitGet RunLenght
parseToken = do
           b <- getBool
           if b
           then parseRun
           else parseStream

decode :: BitGet [RunLenght]
decode = do
       token <- parseToken
       case token of
            Run 0 _ -> return mempty
            _ -> do
              rest <- decode
              return $ token : rest

writeRunLengh :: RunLenght -> BitPut ()
writeRunLengh x = putByteString $ serialize x

decodeStream :: [RunLenght] -> BitPut ()
decodeStream xs = sequence_ $ fmap writeRunLengh xs
