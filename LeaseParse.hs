--
-- processes `ip dhcp-server lease print terse' output from MikroTik router
--

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module LeaseParse (Lease, emptyLease, readLease, readDHCPLeases, leaseMap) where

import GHC.Generics
import GHC.IO.Handle

import System.Process (runInteractiveProcess, waitForProcess)
import Data.List (foldl')
import qualified Data.Map as M
import Data.Maybe (mapMaybe, isJust)

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as E
import Data.Aeson (ToJSON, encode)
import qualified Data.Text.Lazy.IO as TIO

data Lease = Lease { leaseHostName :: T.Text
                   , leaseMacAddress :: T.Text
                   , leaseAddress :: T.Text
                   , leaseLastSeen :: T.Text
                   , leaseStatus :: T.Text
                   , leaseOpts :: [(T.Text, T.Text)]
                   } deriving (Show, Eq, Generic)

instance ToJSON Lease

emptyLease = Lease "(unknown)" "" "" "" "" []

-- long-key=string-value
splitKV :: T.Text -> Maybe (T.Text, T.Text)
splitKV tokens = case T.split (=='=') tokens of
            a:b:[] -> Just (a, b)
            _ -> Nothing

readIntSafe t = case reads $ T.unpack t :: [(Int, String)] of
                    [(i, _)] -> Just i
                    _ -> Nothing

type Proplist = [(T.Text, T.Text)]
proc :: T.Text -> Proplist
proc = proctokens . T.split (==' ')
    where
        proctokens :: [T.Text] -> Proplist
        proctokens (t:"D":pairs)
            | isJust $ readIntSafe t
            = mapMaybe splitKV pairs
        proctokens _ = []

-- [("host-name","Ingvars-iPhone"),("active-mac-address","50:EA:D6:92:B5:3F"),("active-address","192.168.60.94"),("last-seen","28m45s"),
--  ,("status","bound"),("dhcp-option","\"\""),("server","default"),("client-id","1:50:ea:d6:92:b5:3f"),("mac-address","50:EA:D6:92:B5:3F"),("address","192.168.60.94")]

readLease proplist = readLease' emptyLease proplist
        where
            readLease' l (("host-name", a):xs) = readLease' l {leaseHostName=a} xs
            readLease' l (("active-mac-address", a):xs) = readLease' l {leaseMacAddress=a} xs
            readLease' l (("active-address", a):xs) = readLease' l {leaseAddress=a} xs
            readLease' l (("last-seen", a):xs) = readLease' l {leaseLastSeen=a} xs
            readLease' l (("status", a):xs) = readLease' l {leaseStatus=a} xs
            readLease' l (_x:xs) = readLease' l xs
            -- readLease' l (x:xs) = readLease' l {leaseOpts=x:(leaseOpts l)} xs
            readLease' l [] = l

readDHCPLeases' :: [T.Text] -> [Lease]
readDHCPLeases' lines = map readLease $ filter (/=[]) $ map proc lines

readDHCPLeases = do
    -- we need 'interact' command in expect to work this way
    (_in, out, _err, pid) <- runInteractiveProcess "./dhcpwho" [] Nothing Nothing
    hSetBinaryMode out False
    _ <- waitForProcess pid
    TIO.hGetContents out >>= return . T.lines >>= return . readDHCPLeases'

leaseMap leases = M.fromList $ map (\l -> (
    T.unpack (case leaseHostName l of
        "(unknown)" -> leaseMacAddress l
        x -> x)
    , (T.unpack $ leaseLastSeen l, ""))) leases

main = BS.interact $ encode . readDHCPLeases' . T.lines . E.decodeUtf8

-- tl1 = T.pack "5 D address=192.168.60.66 mac-address=3C:07:54:5B:B3:BD client-id=1:3c:7:54:5b:b3:bd server=default dhcp-option=\"\" status=bound expires-after=1d19h44m10s last-seen=1d3h25m53s active-address=192.168.60.66 active-mac-address=3C:07:54:5B:B3:BD active-client-id=1:3c:7:54:5b:b3:bd active-server=default host-name=Hells-MacBook"
