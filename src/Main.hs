module Main where

import API (OfficeAPI)
import Data.Proxy (Proxy (..))
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant.Server (serve)
import Server (officeHandler)

api :: Proxy OfficeAPI
api = Proxy

app :: Application
app = serve api officeHandler

port :: Int
port = 3000

main :: IO ()
main = do
  putStrLn $ "Running server on port " <> show port
  run port app
