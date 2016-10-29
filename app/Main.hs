module Main where

import qualified Data.ByteString.Char8 as B8
import           Network.HTTP.Base (urlEncode)
import           Network.Socket hiding (send, recv)
import           Network.Socket.ByteString
import           Options.Applicative

data Options = Options { host :: String
                       , port :: Int
                       , message :: String
                       }

getConnection :: HostName -> Int -> IO Socket
getConnection host port = do
  addrInfo : _ <- getAddrInfo Nothing (Just host) (Just $ show port)
  sock <- socket (addrFamily addrInfo) Stream defaultProtocol
  connect sock (addrAddress addrInfo)
  return sock

httpString :: String -> String
httpString message = let encoded = urlEncode message in
                       "GET /echo.php?message=" ++ encoded ++ " HTTP/1.0\r\n\r\n"

sendMessage :: Socket -> String -> IO B8.ByteString
sendMessage sock message = do
  send sock (B8.pack $ httpString message)
  recv sock 4096

runWithOptions :: Options -> IO ()
runWithOptions opts = do
  sock <- getConnection (host opts) (port opts)
  response <- sendMessage sock (message opts)
  B8.putStrLn response
  close sock

main :: IO ()
main = execParser opts >>= runWithOptions
  where
    parser = Options <$> strOption (long "host" <> short 'h' <> metavar "HOST")
                     <*> option auto (long "port" <> short 'p' <> metavar "PORT")
                     <*> strOption (long "message" <> short 'm' <> metavar "MESSAGE")
    opts = info parser mempty
