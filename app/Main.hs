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
type Api = MetaApi :<|> PostingApi 
api :: Proxy Api 
api = Proxy 

server :: ServerT Api AppT 
server = metaServer :<|> echoH 

type MetaApi = "swagger" :> Get '[JSON] OpenApi :<|> SwaggerSchemaUI "swagger-ui" "swagger.json"  :<|> "layout" :> Get '[JSON] Text 

metaServer :: ServerT MetaApi AppT 
metaServer = swaggerH :<|> swaggerServerH :<|> layoutH 

type PostingApi = "echo" :> ReqBody '[JSON] Text :> Get '[JSON] Text 

echoH :: Text -> AppT Text
echoH = return

layoutH :: AppT Text
layoutH = return $ layout api 

swaggerH :: AppT OpenApi
swaggerH = return $ toOpenApi (Proxy @PostingApi) 

swaggerDoc :: OpenApi
swaggerDoc = toOpenApi (Proxy @PostingApi)


swaggerServerH = swaggerSchemaUIServerT swaggerDoc 

data Config = Config { connStr :: String } deriving (Generic, Show)

newtype AppT a = AppT { runAppT :: ReaderT Config (RandomT Handler) a } deriving (Functor, Applicative, Monad, MonadReader Config, MonadIO, MonadState StdGen  ) 

newtype RandomT m a = RandomT { runRandomT :: StateT StdGen m a } deriving (Functor, Applicative, Monad, MonadState StdGen, MonadTrans, MonadIO  )

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
    gen <- initStdGen 
    let config = Config "test.db"
    print "starting"
    runSettings defaultSettings $ mkApp config gen 


newtype PWHash = PWHash { getHash :: ByteString } 
newtype PWSalt = PWSalt { getSalt :: ByteString } 

class PasswordHash m where 
    pwHash :: ByteString -> m (PWHash, PWSalt) 

instance PasswordHash AppT where 
    pwHash pw = do 
        gen <- initStdGen
        let (salt, _) = genByteString 64 gen 
        let saltless = convert $ hashWith SHA512 pw
        let salted = convert $ hashWith SHA512 (saltless <> salt) 
        return (PWHash salted, PWSalt salt)   