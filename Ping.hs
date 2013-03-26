
module Ping (readPing) where

import System.Environment (getArgs)
import System.Process (readProcessWithExitCode)

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text.Lazy as T
import Data.Aeson (encode)

strtokenize = T.split (==' ')
hosttokenize = T.split (=='.')
procline = head . hosttokenize . (!!4) . strtokenize

readPing :: String -> IO [String]
readPing interface = do
    (rc, out, err) <- readProcessWithExitCode "./ping6x" ["-Qwc1", "ff02::1%" ++ interface] [] -- TODO: make configurable
    let lines = T.lines $ T.pack out
    return $ map (T.unpack . procline) lines

main' interface = LBS.putStrLn =<< return . encode =<< readPing interface
main = main' =<< interface
    where
        interface = do
            args <- getArgs
            return $ if not $ null args then head args else "en1"
