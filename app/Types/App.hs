{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
module Types.App where
import Database.PostgreSQL.Simple (Connection, execute, execute_)
import GHC.Generics (Generic)
import Servant (Handler)
import Control.Monad.Reader (ReaderT, MonadReader, MonadIO (liftIO), MonadTrans, asks)
import Control.Monad.State.Strict (StateT, MonadState)
import System.Random (StdGen)
import Types.DB (DBOrganizationUser)
import Types.Domain
import Database.PostgreSQL.Simple.SqlQQ (sql)

data Config = Config { conn :: Connection } deriving (Generic)

newtype AppT a = AppT { runAppT :: ReaderT Config (RandomT Handler) a } deriving (Functor, Applicative, Monad, MonadReader Config, MonadIO, MonadState StdGen  ) 

newtype RandomT m a = RandomT { runRandomT :: StateT StdGen m a } deriving (Functor, Applicative, Monad, MonadState StdGen, MonadTrans, MonadIO  )
 
class MonadDB m where 
    orgSignup :: Username -> PWHash -> PWSalt -> m Bool 

instance MonadDB AppT where 
    orgSignup (Username username) (PWHash password) (PWSalt salt) = do 
        conn' <- asks conn  
        rows <- liftIO $ execute conn' [sql| INSERT INTO organization_user
                                                (id, username, password, salt, created_at) VALUES (DEFAULT, ?, ?, ?, DEFAULT)   |] (username, password, salt)
        return (rows > 0) 
    

