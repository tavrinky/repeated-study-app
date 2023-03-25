{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Types.Domain where 
import Data.Aeson (FromJSON, ToJSON, Value)
import Data.ByteString (ByteString)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.OpenApi (ToSchema)

data Student = Student { username :: Username, password :: Text } deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)
data Teacher = Teacher { username :: Username, password :: Text } deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)
data OrgUser = OrgUser { username :: Username, password :: Text } deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema) 

data Card = Card {cardData :: Value } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Deck = Deck { deckData :: Value } deriving (Eq, Show, Generic, ToJSON, FromJSON) 

newtype PWHash = PWHash { getHash :: ByteString } 
newtype PWSalt = PWSalt { getSalt :: ByteString } 
newtype Username = Username { getUsername :: Text } deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)