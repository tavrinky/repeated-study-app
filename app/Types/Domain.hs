{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types.Domain where 
import Data.Aeson (FromJSON, ToJSON, Value)
import Data.ByteString (ByteString)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.OpenApi (ToSchema, ToParamSchema)
import Servant (FromHttpApiData, ToHttpApiData)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)

data Student = Student { username :: Username, password :: Text } deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)
data Teacher = Teacher { username :: Username, password :: Text } deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)
data OrgUser = OrgUser { username :: Username, password :: Text } deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema) 

newtype Organization = Organization { orgName :: Text } deriving newtype (Eq, Show, ToJSON, FromJSON, ToSchema, FromHttpApiData, ToHttpApiData, ToParamSchema)

data Card = Card {cardData :: Value } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Deck = Deck { deckData :: Value } deriving (Eq, Show, Generic, ToJSON, FromJSON) 

newtype PWHash = PWHash { getHash :: ByteString } 
newtype PWSalt = PWSalt { getSalt :: ByteString } 
newtype Username = Username { getUsername :: Text } deriving newtype (Eq, Show, ToJSON, FromJSON, ToSchema, ToField, FromField)