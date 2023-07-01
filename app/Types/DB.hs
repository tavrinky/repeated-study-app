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
data DBStudentUser = DBStudentUser { studentId :: DBStudentUserPk, username :: Text, password :: ByteString, created_at :: ZonedTime } deriving (Show, Generic, ToRow, FromRow) 

newtype DBOrganizationUserPk = DBOrganizationUserPk { getOrganizationUserPk :: Int } deriving newtype (FromField, ToField, Show, Eq) 
data DBOrganizationUser = DBOrganizationUser { userId :: DBOrganizationUserPk, username :: Text, password :: ByteString , created_at :: ZonedTime } deriving (Show, Generic, ToRow, FromRow) 

newtype DBOrganizationPk = DBOrganizationPk { getOrganizationPk :: Int } deriving newtype ( ToField, FromField, Eq, Show)
data DBOrganization = DBOrganization { organizationId :: DBOrganizationPk, org_name :: Text, created_by :: DBOrganizationUserPk, created_at :: ZonedTime } deriving (Generic, Show, ToRow, FromRow) 

newtype DBTeacherUserPk = DBTeacherUserPk { getTeacherUserPk :: Int } deriving newtype (ToField, FromField, Eq, Show)
data DBTeacherUser = DBTeacherUser { teacherId :: DBTeacherUserPk, username :: Text, password :: ByteString, created_at :: ZonedTime } deriving (ToRow, FromRow, Show, Generic)

newtype DBCardPk = DBCardPk { getCardPk :: Int } deriving newtype (Eq, Show, FromField, ToField) 
data DBCard = DBCard { cardId :: DBCardPk, cardData :: Value, created_by :: DBTeacherUserPk, created_at :: ZonedTime} deriving (ToRow, FromRow, Show, Generic )

newtype DBDeckPk = DBDeckPk { getDeckPk :: Int } deriving newtype (Eq, Show, FromField, ToField) 
data DBDeck = DBDeck { deckId :: DBDeckPk, deckData :: Value, created_by :: DBTeacherUserPk, created_at :: ZonedTime } deriving (ToRow, FromRow, Generic, Show) 

data DBDeckCardRelationship = DBDeckCardRelationship { deckCardRelationshipId :: Int, card_id :: DBCardPk, deck_id :: DBDeckPk } 

data DBCardStudy = DBCardStudy { cardStudyId :: Int, card_id :: DBCardPk, student :: DBStudentUserPk, succeeds :: Bool, created_at :: ZonedTime } deriving (ToRow, FromRow, Generic, Show)