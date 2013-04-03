{-# LANGUAGE OverloadedStrings #-}

module Ping (readPing) where

import System.Environment (getArgs)

import System.Process.Text.Lazy (readProcessWithExitCode)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text.Lazy as T
import Data.Aeson (encode)

hosttokenize = T.split (=='.')
procline = head . hosttokenize . (!!4) . T.words

readPing :: String -> IO [String]
readPing interface = do
    (rc, out, err) <- readProcessWithExitCode "./ping6x" ["-Qwc1", "ff02::1%" ++ interface] "" -- TODO: make configurable
    return $ map (T.unpack . procline) $ T.lines out

{-
main' interface = LBS.putStrLn =<< return . encode =<< readPing interface
main = main' =<< interface
    where
        interface = do
            args <- getArgs
            return $ if not $ null args then head args else "en1"
-}
