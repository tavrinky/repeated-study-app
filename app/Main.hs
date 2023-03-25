{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
import Data.Proxy (Proxy(..))
import Servant.API ( Get, JSON, ReqBody, (:<|>)(..), (:>)(..))
import Servant.Swagger.UI (SwaggerSchemaUI, swaggerSchemaUIServerT)
import Data.Text (Text)
import GHC.Generics (Generic)
import Control.Monad.Trans.State.Strict (StateT, evalStateT)
import System.Random (StdGen, initStdGen, genByteString)
import Servant.Server (Handler, Application, HasServer (..), layout, serve, hoistServer)
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Strict (MonadState, MonadTrans)
import Network.Wai.Handler.Warp (runSettings, defaultSettings)
import Servant.OpenApi (HasOpenApi(..))
import Data.OpenApi (OpenApi)
import Data.ByteString (ByteString)
import Crypto.Hash (SHA512(..), hashWith)
import Data.ByteArray (convert)
import Database.PostgreSQL.Simple (ConnectInfo (..), Connection, connect)

import Types.DB 
import Types.Domain
import Types.App
import Data.Text.Encoding (encodeUtf8)

type Api = MetaApi :<|> PostingApi 
api :: Proxy Api 
api = Proxy 

server :: ServerT Api AppT 
server = metaServer :<|> signupH 

postingApi = signupH 

type MetaApi = "swagger" :> Get '[JSON] OpenApi :<|> SwaggerSchemaUI "swagger-ui" "swagger.json"  :<|> "layout" :> Get '[JSON] Text 

metaServer :: ServerT MetaApi AppT 
metaServer = swaggerH :<|> swaggerServerH :<|> layoutH 

type PostingApi = "signup" :> ReqBody '[JSON] OrgUser :> Get '[JSON] Bool 

echoH :: Text -> AppT Text
echoH = return

layoutH :: AppT Text
layoutH = return $ layout api 

swaggerH :: AppT OpenApi
swaggerH = return $ toOpenApi (Proxy @PostingApi) 

swaggerDoc :: OpenApi
swaggerDoc = toOpenApi (Proxy @PostingApi)

signupH :: OrgUser -> AppT Bool 
signupH (OrgUser username pw) = do 
    (hash, salt) <- pwHash pw 
    orgSignup username hash salt 

swaggerServerH = swaggerSchemaUIServerT swaggerDoc 



readerNt :: Config -> AppT a -> RandomT Handler a
readerNt config (AppT app) = runReaderT app config

stateNt :: Monad m => StdGen -> RandomT m a -> m a
stateNt gen (RandomT app) = evalStateT app gen  

appNt :: Config -> StdGen -> AppT a -> Handler a
appNt config gen = stateNt gen . readerNt config 

mkApp :: Config -> StdGen -> Application 
mkApp config gen = serve api $ hoistServer api (appNt config gen) server 

main :: IO ()
main = do 
    pgPass <- readFile "./config/pgpass"
    gen <- initStdGen 
    let connInfo = ConnectInfo { connectHost = "127.0.0.1", connectPort = 5432, connectUser = "ankidb", connectPassword = pgPass, connectDatabase = "ankidb" }
    conn <- connect connInfo 
    let config = Config conn 
    print "starting"
    runSettings defaultSettings $ mkApp config gen 



class PasswordHash m where 
    pwHash :: Text -> m (PWHash, PWSalt) 

instance PasswordHash AppT where 
    pwHash pw = do 
        let pwBS = encodeUtf8 pw 
        gen <- initStdGen
        let (salt, _) = genByteString 64 gen 
        let saltless = convert $ hashWith SHA512 pwBS 
        let salted = convert $ hashWith SHA512 (saltless <> salt) 
        return (PWHash salted, PWSalt salt)   