
module Ping (readPing) where

import System.Process (readProcessWithExitCode)

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T
import Data.Aeson (encode)

strtokenize = T.split (==' ')
hosttokenize = T.split (=='.')
procline = (T.unpack . head . hosttokenize . (!!4) . strtokenize)

readPing =  do
    (rc, out, err) <- readProcessWithExitCode "./ping6x" ["-Qwc1", "ff02::1%en1"] [] -- TODO: make configurable
    let lines = T.lines $ T.pack out
    let hostnames = map procline lines
    return hostnames

main = do
    hostnames <- readPing
    LBS.putStrLn $ encode hostnames
