{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types.DB where 
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Aeson (Value)
import Data.Time (ZonedTime)
import Database.PostgreSQL.Simple (ToRow)
import Database.PostgreSQL.Simple.FromRow (FromRow)

newtype DBStudentUserPk = DBStudentUserPk { getStudentUserPk :: Int } deriving newtype (FromField, ToField, Show, Eq)
data DBStudentUser = DBStudentUser { id :: DBStudentUserPk, username :: Text, password :: ByteString, created_at :: ZonedTime } deriving (Show, Generic, ToRow, FromRow) 

newtype DBOrganizationUserPk = DBOrganizationUserPk { getOrganizationUserPk :: Int } deriving newtype (FromField, ToField, Show, Eq) 
data DBOrganizationUser = DBOrganizationUser { userId :: DBOrganizationUserPk, username :: Text, password :: ByteString , created_at :: ZonedTime } deriving (Show, Generic, ToRow, FromRow) 

newtype DBOrganizationPk = DBOrganizationPk { getOrganizationPk :: Int } deriving newtype ( ToField, FromField, Eq, Show)
data DBOrganization = DBOrganization { id :: DBOrganizationPk, org_name :: Text, created_by :: DBOrganizationUserPk, created_at :: ZonedTime } deriving (Generic, Show, ToRow, FromRow) 

data DBTeacherUserPk = DBTeacherUserPk { getTeacherUserPk :: Int } deriving (Generic, ToField, FromField, Eq, Show)
data DBTeacherUser = DBTeacherUser { id :: DBTeacherUserPk, username :: Text, password :: ByteString, created_at :: ZonedTime } deriving (ToRow, FromRow, Show, Generic)

data DBCardPk = DBCardPk { getCardPk :: Int } deriving (Eq, Show, Generic, FromField, ToField) 
data DBCard = DBCard { id :: DBCardPk, cardData :: Value, created_by :: DBTeacherUserPk, created_at :: ZonedTime} deriving (ToRow, FromRow, Show, Generic )

data DBDeckPk = DBDeckPk { getDeckPk :: Int } deriving (Eq, Show, Generic, FromField, ToField) 
data DBDeck = DBDeck { id :: DBDeckPk, deckData :: Value, created_by :: DBTeacherUserPk, created_at :: ZonedTime } deriving (ToRow, FromRow, Generic, Show) 

data DBDeckCardRelationship = DBDeckCardRelationship { id :: Int, card_id :: DBCardPk, deck_id :: DBDeckPk } 

data DBCardStudy = DBCardStudy { id :: Int, card_id :: DBCardPk, student :: DBStudentUserPk, succeeds :: Bool, created_at :: ZonedTime } deriving (ToRow, FromRow, Generic, Show)