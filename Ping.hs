
import System.Process (readProcessWithExitCode)

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T
import Data.Aeson (encode)

strtokenize = T.split (==' ')
hosttokenize = T.split (=='.')
procline = (head . hosttokenize . (!!4) . strtokenize)

main = do
    (rc, out, err) <- readProcessWithExitCode "./ping6x" ["-Qwc1", "ff02::1%en0"] []
    let lines = T.lines $ T.pack out
    let hostnames = map procline lines
    LBS.putStrLn $ encode hostnames
