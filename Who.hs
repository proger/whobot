{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import GHC.Generics

import System.Environment (getArgs)

import Control.Applicative
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Char8 as BS
import Text.Regex (splitRegex, mkRegex)

import qualified Data.Map as Map

import Data.Yaml
import qualified Data.Aeson as Aeson

import Ping
import LeaseParse

data Node = Node { name :: String
                 , type_ :: Maybe String
                 , mac :: Maybe String
                 } deriving (Show, Generic)

cleanName = head . splitRegex (mkRegex "\\.") . name

instance FromJSON Node where
    parseJSON (Object v) = Node <$>
                    v .: "name" <*>
                    v .:? "type_" <*>
                    v .:? "mac"


people = do
    p <- readFile "./people.yaml"
    return $ BS.pack p

peopleReverse = do
    p <- people
    let nodemap = (decode p :: Maybe (Map.Map String [Node]))
        Just nodemapl = fmap Map.toList nodemap in
        return [(cleanName node, ("", name')) | (name', nodelist) <- nodemapl, node <- nodelist]

main' interface = do
    availNow <- readPing interface
    leases <- readDHCPLeases
    pr <- peopleReverse 

    let availNodes = Map.fromList [(node, ("now", "")) | node <- availNow]
        lm = leaseMap leases
        allNodes = Map.unionWith const availNodes lm
        merged = Map.unionWith (\(_, personName) (ts, _) -> (ts, personName)) (Map.fromList pr) allNodes
        list = [ (name, ts, node)  | (node, (ts, name)) <- Map.toList merged, name /= "", ts /= "" ]
        in
        LBS.putStrLn $ Aeson.encode list

main = main' =<< interface
    where
        interface = do
            args <- getArgs
            return $ if not $ null args then head args else "en1"
