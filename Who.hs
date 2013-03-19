{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Who where

import GHC.Generics

import Control.Applicative
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Char8 as BS
import Text.Regex (splitRegex, mkRegex)

import Data.Map as Map

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
    let nodemap = (decode p :: Maybe (Map String [Node]))
        Just nodemapl = fmap Map.toList nodemap in
        return [(cleanName node, ("", name')) | (name', nodelist) <- nodemapl, node <- nodelist]


main = do
    availNow <- readPing
    let availNodes = Map.fromList [(node, ("now", "")) | node <- availNow]
    leases <- readDHCPLeases
    let lm = leaseMap leases
    let allNodes = Map.unionWith const availNodes lm
    pr <- peopleReverse 
    let merged = Map.unionWith (\(_, personName) (ts, _) -> (ts, personName)) (Map.fromList pr) allNodes

    let list = [ (name, ts, node)  | (node, (ts, name)) <- Map.toList merged, name /= "", ts /= "" ]
    LBS.putStrLn $ Aeson.encode list
