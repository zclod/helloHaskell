module Data.RunLenghtEncoder where
  -- ( RunLenght(..)
  -- , finalParser
  -- , encodeStream
  -- ) where

import Control.Monad (when)
import Data.Word
import Data.Monoid
import Data.Binary.Bits.Put
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary.Parser as P
import Control.Applicative hiding (empty)

data RunLenght
  = Run Int
        Word8
  | Stream B.ByteString
  deriving (Show)

toWord8 :: Int -> Word8
toWord8 = fromInteger . toInteger

-- sameParserH :: Word8 -> P.Parser B.ByteString
-- sameParserH n prev = do
--   next <- P.peekMaybe
--   case next of
--     Just x -> do
sameParserH :: Word8 -> P.Parser B.ByteString
sameParserH prev = P.takeWhile1 (== prev)

get2Bytes :: P.Parser (Word8, Word8)
get2Bytes = do
  i1 <- P.anyWord8
  i2 <- P.anyWord8
  return (i1, i2)

get2Bytes' :: P.Parser (Word8, Maybe Word8)
get2Bytes' = do
  i1 <- P.anyWord8
  i2 <- P.peekMaybe
  return (i1, i2)

sameParser :: P.Parser B.ByteString
sameParser = do
  (start, next) <- P.lookAhead get2Bytes'
  case next of
    Just x ->
      if start == x
        then P.scan (start, 0) sameparse
        else fail "sameparser"
    Nothing -> return $ B.singleton start

  where sameparse (prev, len) current =
          if prev == current && len < 64
          then Just (current, len + 1)
          else Nothing

diffParser :: B.ByteString -> P.Parser B.ByteString
diffParser accumulator = diffParserH accumulator <|> diffParserLast accumulator

diffParserLast :: B.ByteString -> P.Parser B.ByteString
diffParserLast accumulator = do
  b <- P.anyWord8
  return $ accumulator <> B.singleton b

diffParserH :: B.ByteString -> P.Parser B.ByteString
diffParserH accumulator =
  if B.length accumulator == 64
    then return accumulator
    else
      do
        (b1,b2) <- P.lookAhead get2Bytes
        if b1 /= b2
          then do
            P.anyWord8
            diffParser (accumulator <> B.singleton b1)
        else if B.length accumulator > 0
               then return accumulator
               else fail "no match"

runLenghtParser :: P.Parser B.ByteString
runLenghtParser = sameParser <|> diffParser mempty

parseRunLenght :: P.Parser [B.ByteString]
parseRunLenght = P.many' runLenghtParser

encodeByteString :: B.ByteString -> RunLenght
encodeByteString b =
  case B.unpack b of
    x1:x2:_ ->
      if x1 == x2
        then Run (B.length b) x1
        else Stream b
    _ -> Stream b

encodeRunLenght :: RunLenght -> BitPut ()
encodeRunLenght (Run 0 _) = do
  putBool True
  putWord8 6 0
encodeRunLenght (Run size byte) = do
  putBool True
  putWord8 6 (toWord8 size - 1)
  putWord8 8 byte
encodeRunLenght (Stream bs) =
  -- when (B.length bs > 0) $
  do
    putBool False
    putWord8 6 (toWord8 (B.length bs) - 1)
    putByteString bs

endStream :: BitPut ()
endStream = do
  putBool True
  putWord8 6 0

encodeStream :: [RunLenght] -> BitPut ()
encodeStream xs = do
  sequence_ $ fmap encodeRunLenght xs
  putBool True
  putWord8 6 0
