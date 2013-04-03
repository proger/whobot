{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics

import System.Environment (getArgs)
import System.Posix.Env (getEnvDefault)

import Control.Applicative
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Char8 as BS
import Text.Regex (splitRegex, mkRegex)

import qualified Data.Map as Map

import Data.Yaml
import qualified Data.Aeson as Aeson

import Ping
import LeaseParse

type UserName = String
type NodeName = String
type TimeStamp = String

data Node = Node { name :: Maybe NodeName
                 , type_ :: Maybe String
                 , mac :: Maybe String
                 } deriving (Show, Generic)

cleanName :: Node -> NodeName
cleanName (Node Nothing _ (Just mac)) = mac
cleanName (Node Nothing _ Nothing) = "(unknown)"
cleanName (Node (Just name') _ _) = head . splitRegex (mkRegex "\\.") $ name'

instance FromJSON Node where
    parseJSON (Object v) = Node <$>
                    v .:? "name" <*>
                    v .:? "type_" <*>
                    v .:? "mac"

peoplemap :: IO (Maybe (Map.Map UserName [Node]))
peoplemap = people >>= return . decode

peopleReverse :: IO [(NodeName, (TimeStamp, UserName))]
peopleReverse = do
    p <- peoplemap
    let Just nodemapl = fmap Map.toList p
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


people = getEnvDefault "PEOPLE_YAML" "./people.yaml" >>= BS.readFile 
interface = getEnvDefault "PING_INTERFACE" "en1"

main = main' =<< interface
