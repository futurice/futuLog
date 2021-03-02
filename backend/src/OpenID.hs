module OpenID (openidHandler, OpenIDAPI) where

import Control.Monad.Except (throwError)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LChar8
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client (Manager)
import Servant.API
import Servant.Server (err400, errBody)
import Types (Server)
import Web.Cookie (parseCookies)

type OpenIDAPI =
  Login :<|> Success :<|> Failure

type Login = "login" :> Get '[PlainText] NoContent

type Success =
  "return"
    :> QueryParam "code" Text
    :> QueryParam "state" Text
    :> Header "cookie" SessionCookie
    :> Get '[PlainText] NoContent

type Failure =
  "return"
    :> QueryParam "error" Text
    :> QueryParam "state" Text
    :> Header "cookie" SessionCookie
    :> Get '[PlainText] NoContent

newtype SessionCookie = MkSessionCookie ByteString

instance FromHttpApiData SessionCookie where
  parseUrlPiece = parseHeader . encodeUtf8
  parseHeader bs = case lookup "session" (parseCookies bs) of
    Nothing -> Left "session cookie missing"
    Just val -> Right (MkSessionCookie val)

openidHandler :: Manager -> Server OpenIDAPI
openidHandler m = login :<|> success m :<|> failed

login :: Server Login
login = undefined

success :: Manager -> Server Success
success = undefined

failed :: Server Failure
failed err _ _ = throwError $ err400 {errBody = maybe "authentication failure" (LChar8.fromStrict . encodeUtf8) err}
