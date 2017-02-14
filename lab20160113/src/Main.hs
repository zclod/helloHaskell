module Main where

import System.Environment (getArgs)
import System.IO
import Options.Applicative
import Data.RunLenghtEncoder
import qualified Data.RunLenghtDecoder as D
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Pipes.Binary as P hiding (ByteString)
import Pipes
import qualified Pipes.ByteString as PB hiding (ByteString)
import Data.Binary.Bits.Put
import Data.Binary.Bits.Get
import Pipes.Parse (evalStateT, runStateT, parsed, parsed)


data Opts = Opts {
                   decode       :: Bool -- decode = True, encode = False
                 , input        :: String
                 , output       :: String
                 } deriving (Show)

opts :: Parser Opts
opts = Opts <$>
  switch (short 'd'
         <> long "decode"
         <> help "decode the file"
         )
  <*> argument str (metavar "SOURCE")
  <*> argument str (metavar "TARGET")

cmdparser = info (helper <*> opts) mempty

----------------------------------------------------------------------------
--encoding
parser :: (Monad m) => PB.Parser
          B.ByteString m (Either P.DecodingError B.ByteString)
parser = P.decodeGet runLenghtParser

runLenghtEncoder :: (Monad m) => Producer B.ByteString m r -> Producer RunLenght m ()
runLenghtEncoder p = do
  (result, leftovers) <- lift $ runStateT parser p
  case result of
    Right runlenght -> do
      yield $ encodeByteString runlenght
      runLenghtEncoder leftovers
    Left _ -> yield $ Run 0 0 -- end of stream character

encodeEffect :: Producer B.ByteString BitPut r -> Effect BitPut ()
encodeEffect source = for (runLenghtEncoder source) (lift . encodeRunLenght)

bitPutStream :: Producer B.ByteString BitPut r -> BitPut ()
bitPutStream source = runEffect (encodeEffect source)

runLenghtWriter :: (Monad m) => Producer B.ByteString BitPut r -> Producer B.ByteString m ()
runLenghtWriter source = P.encodePut $ runBitPut $ bitPutStream source

----------------------------------------------------------------------------
--decoding

decoder :: (Monad m) => PB.Parser
          B.ByteString m (Either P.DecodingError [RunLenght])
decoder = P.decodeGet (runBitGet D.decode)

runLenghtDecoder p = do
  xs <- lift (evalStateT decoder p)
  case xs of
    Right as -> each as
    _ -> each []

decodeEffect p = for (runLenghtDecoder p) (lift . D.writeRunLengh)

-- bitPutStream' :: Producer B.ByteString BitPut r -> BitPut ()
bitPutStream' source = runEffect (decodeEffect source)

-- runLenghtWriter' :: (Monad m) => Producer B.ByteString BitPut r -> Producer B.ByteString m ()
runLenghtWriter' source = P.encodePut $ runBitPut $ bitPutStream' source
----------------------------------------------------------------------------

main :: IO ()
main = do
  options <- execParser cmdparser
  let writer = if decode options
        then runLenghtWriter'
        else runLenghtWriter
  content <- BL.readFile $ input options
  let fileReader = PB.fromLazy content
  withFile (output options) WriteMode $ \hout ->
    runEffect $ writer fileReader >-> PB.toHandle hout
