{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Types.App where
import Database.PostgreSQL.Simple (Connection, execute, execute_, query, Only (..))
import GHC.Generics (Generic)
import Servant (Handler, ServerError)
import Control.Monad.Reader (ReaderT, MonadReader, MonadIO (liftIO), MonadTrans, asks)
import Control.Monad.State.Strict (StateT, MonadState)
import System.Random (StdGen)
import Types.DB (DBOrganizationUser, DBOrganizationPk, DBOrganization, DBOrganizationUserPk (..))
import Types.Domain
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Control.Monad (void)
import Data.Maybe (listToMaybe)
import Data.Text.Encoding (decodeUtf8Lenient)
import Control.Monad.Error (MonadError)


data Config = Config { conn :: Connection } deriving (Generic)

newtype AppT a = AppT { runAppT :: ReaderT Config (RandomT Handler) a } deriving (Functor, Applicative, Monad, MonadReader Config, MonadIO, MonadState StdGen, MonadError ServerError  ) 

newtype RandomT m a = RandomT { runRandomT :: StateT StdGen m a } deriving (Functor, Applicative, Monad, MonadState StdGen, MonadTrans, MonadIO  )
deriving instance MonadError e m => MonadError e (RandomT m)     
class MonadDB m where 
    orgSignup :: Username -> PWHash -> PWSalt -> m Bool 
    teacherSignup :: Username -> PWHash -> PWSalt -> DBOrganizationPk -> m () 
    getOrganization :: Organization -> m (Maybe DBOrganization) 
    createOrg :: Organization -> DBOrganizationUserPk -> m Bool 
    getOrgUser :: OrgUser -> m (Maybe DBOrganizationUser)

instance MonadDB AppT where 
    orgSignup (Username username) (PWHash password) (PWSalt salt) = do 
        let pwText = decodeUtf8Lenient password 
        let saltText = decodeUtf8Lenient password 
        conn' <- asks conn  
        rows <- liftIO $ execute conn' [sql| INSERT INTO organization_user
                                                (id, username, password, salt, created_at) VALUES (DEFAULT, ?, ?, ?, DEFAULT)   |] (username, pwText, saltText)
        return (rows > 0) 

    teacherSignup (Username username) (PWHash password) (PWSalt salt) orgKey = do 
        conn' <- asks conn 
        void $ liftIO $ execute conn' [sql| INSERT INTO teacher_user 
                                                (id, username, password, salt, created_at, organization) VALUES (DEFAULT, ?, ?, ?, ?, DEFAULT) |] (username, password, salt, orgKey)
    getOrganization (Organization orgName) = do 
        conn' <- asks conn 
        results <- liftIO $ query conn' [sql| SELECT * FROM organization WHERE organization.org_name = ? |] $ Only orgName 
        return $ listToMaybe results 

    createOrg (Organization orgName) (DBOrganizationUserPk pk) = do 
        conn' <- asks conn 
        rows <- liftIO $ execute conn' [sql| INSERT INTO organiztaion (org_name, created_by) VALUES (DEFAULT, ?, ?, DEFAULT) |] (orgName, pk) 
        return (rows > 0)

    getOrgUser (OrgUser username _) = do 
        conn' <- asks conn 
        results <- liftIO $ query conn' [sql| SELECT * FROM organization_user WHERE organization_username = ? |] $ Only username 
        return $ listToMaybe results 

{-instance MonadFail AppT where 
    fail = error . ("whoopsie\n" <>)  -} 