module Maelstrom.IO (
    logNB,
    logN,
    receiveMessage,
    sendMessage,
    sendEncoded,
) where

import Data.Aeson (ToJSON, encode)
import Data.ByteString.Lazy.Char8 qualified as C8
import Data.String (fromString)
import GHC.IO.Handle (hFlush)
import System.IO (hPutStrLn, stderr, stdout)

receiveMessage :: IO C8.ByteString
receiveMessage = fromString <$> getLine

sendMessage :: C8.ByteString -> IO ()
sendMessage msg = do
    let msg' = C8.unpack msg
    putStrLn msg' >> hFlush stdout

sendEncoded :: ToJSON a => a -> IO ()
sendEncoded = sendMessage . encode

logN :: String -> IO ()
logN msg = hPutStrLn stderr msg >> hFlush stderr

logNB :: C8.ByteString -> IO ()
logNB = logN . C8.unpack
