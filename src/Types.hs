{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Data.Aeson   (ToJSON(..), object, (.=))
import Data.Int     (Int64)
import Data.Text    (Text)
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.Internal (Field(Field))
import Database.SQLite.Simple.Ok
import Database.SQLite.Simple.Types
import Snap.Snaplet.Auth (UserId)

-- | Type for name of the database. 
type DBName = String

type PersonId = Int

-- | A user is a person with credentials for authentication.
data User = User
  { usrId      :: Int     -- ^ Id of the user in DB
  , usrAuthId  :: UserId  -- ^ Id of the user within authentication
  , usrFName   :: Text    -- ^ first name of the user
  , usrLName   :: Text    -- ^ last name of the user
  , usrEmail   :: Text    -- ^ email adress of the user
  , usrLogin   :: Text    -- ^ login user
  , usrRole    :: Role    -- ^ role of user
  } deriving Show

-- | The person is responsible for saving a reservation.
-- For single games there will be two persons and on a double game
-- there will be four. If the game is a training or a championship the
-- person is the contact person of the game.
data Person = Person
  { perId    :: PersonId  -- ^ Id of the person
  , perFName :: Text      -- ^ first name of the person
  , perLName :: Text      -- ^ last name of the person
  } deriving (Show)

-- | Get a person from a list of persons if the given person-id is the same.
getPerson :: [Person] -> PersonId -> Maybe Person
getPerson ps n = case filter (\p -> perId p == n) ps of
  []  -> Nothing
  x:_ -> Just x             

-- | Defines on which court the user will play.
data Court = CourtOne     -- ^ court #1
           | CourtTwo     -- ^ court #2
           | CourtThree   -- ^ court #3
           | CourtFour    -- ^ court #4

instance Show Court where
  show CourtOne   = "1"
  show CourtTwo   = "2"
  show CourtThree = "3"
  show CourtFour  = "4"

courtToInt :: Court -> Int
courtToInt CourtOne   = 1
courtToInt CourtTwo   = 2
courtToInt CourtThree = 3
courtToInt CourtFour  = 4

intToCourt :: Int -> Court
intToCourt 1 = CourtOne
intToCourt 2 = CourtTwo
intToCourt 3 = CourtThree
intToCourt 4 = CourtFour

data Role = SuperUser
          | Admin
          | Member
          | Trainer
          | Youth

instance Show Role where
  show SuperUser = "0"
  show Admin     = "1"
  show Member    = "2"
  show Trainer   = "3"
  show Youth     = "4"

roleToInt :: Role -> Int
roleToInt SuperUser = 0
roleToInt Admin     = 1
roleToInt Member    = 2
roleToInt Trainer   = 3
roleToInt Youth     = 4

roleFromInt :: Int -> Role
roleFromInt 0 = SuperUser
roleFromInt 1 = Admin
roleFromInt 2 = Member
roleFromInt 3 = Trainer
roleFromInt 4 = Youth

instance FromField Role where
  fromField (Field (SQLInteger n) _) = case n of
    0 -> Ok $ SuperUser
    1 -> Ok $ Admin
    2 -> Ok $ Member
    3 -> Ok $ Trainer
    4 -> Ok $ Youth

data FreeCourts = FreeCourts
  { fcMsg    :: Text
  , fcStatus :: Int
  , fcCourts :: [Int]
  } deriving Show

-- | Reservation main datastructure.
data Reservation = Reservation
  { resId         :: Integer  -- ^ ID of the reservation in DB
  , resDate       :: String   -- TODO: use datetime
  , resStartTime  :: String   -- TODO: use datetime
  , resStopTime   :: String   -- TODO: use datetime
  , resUser       :: UserId
  , resPersons    :: [Person] -- ^ all players w/ this reservation
  , resCourt      :: Court    -- ^ court where players are playing
  , resType       :: Int      -- 0 (single), 1 (double), 2 (training), 4 (championship)
  , resComment    :: String
  } deriving Show

instance ToJSON Person where
    toJSON (Person{..}) =
        object [ "fname" .= perFName
               , "lname" .= perLName
               ]

instance ToJSON User where
  toJSON (User{..}) =
    object [ "id"     .= usrId
           , "authId" .= usrAuthId
           , "fname"  .= usrFName
           , "lname"  .= usrLName
           , "email"  .= usrEmail
           , "login"  .= usrLogin
           , "role"   .= case usrRole of
             SuperUser -> 0 :: Int
             Admin     -> 1 :: Int
             Member    -> 2 :: Int
             Trainer   -> 3 :: Int
             Youth     -> 4 :: Int
           ]

instance ToJSON Reservation where
    toJSON (Reservation{..}) =
        object [ "id"        .= resId
               , "persons"   .= resPersons
               , "date"      .= resDate
               , "startTime" .= resStartTime
               , "stopTime"  .= resStopTime
               , "court"     .= case resCourt of
                 CourtOne   -> 1 :: Int
                 CourtTwo   -> 2 :: Int
                 CourtThree -> 3 :: Int
                 CourtFour  -> 4 :: Int
               , "type"      .= resType
               , "comment"   .= resComment
               ]

instance ToJSON FreeCourts where
  toJSON (FreeCourts{..}) =
    object [ "msg"    .= fcMsg
           , "status" .= fcStatus
           , "courts" .= fcCourts
           ]
