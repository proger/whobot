--
-- processes `ip dhcp-server lease print terse' output from MikroTik router
--

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T
import Data.List (foldl')
import Data.Aeson

strtokenize = T.split (==' ')

-- NN D a=b c=d e=f
getPairs (t:"D":pairs) = case readIntSafe t of
                            Nothing -> []
                            Just _ -> pairs
getPairs _ = []

-- long-key=string-value
splitKV tokens = case T.split (=='=') tokens of
            a:b:[] -> Just (a, b)
            _ -> Nothing

proc x = case getPairs $ strtokenize x of
             [] -> Nothing
             v -> Just $ mapClean splitKV v

cleanMaybe results (Just v) = v:results
cleanMaybe results Nothing = results

readIntSafe :: T.Text -> Maybe Int
readIntSafe t = case reads $ T.unpack t :: [(Int, String)] of
                    [] -> Nothing
                    [(i, _)] -> Just i

mapClean f x = foldl' cleanMaybe [] $ map f x

data Lease = Lease { leaseHostName :: T.Text
                   , leaseMacAddress :: T.Text
                   , leaseAddress :: T.Text
                   , leaseLastSeen :: T.Text
                   , leaseStatus :: T.Text
                   , leaseOpts :: [(T.Text, T.Text)]
                   } deriving (Show, Eq, Generic)

instance ToJSON Lease

emptyLease = Lease "" "" "" "" "" []

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


main = do
    inp <- getContents
    LBS.putStrLn $ encode $ map readLease $ (mapClean proc . T.lines . T.pack) inp
