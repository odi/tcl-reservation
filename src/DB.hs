{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module DB where

import Control.Applicative
import Data.Maybe (catMaybes)
import Data.Int
import Data.Lists
import Data.Maybe
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.Internal (Field(Field))
import Database.SQLite.Simple.Ok
import Database.SQLite.Simple.Types
import qualified Data.Text as T
import Snap.Snaplet.Auth (UserId, UserId(..))

-- own modules
import Types
import Utils
import Player

newtype ReservationId = ReservationId Int64

instance Show ReservationId where
  show (ReservationId n) = show n

instance FromField ReservationId where
  fromField (Field (SQLInteger n) _) = Ok $ ReservationId n

instance FromField UserId where
  fromField (Field (SQLText t) _) = Ok $ UserId t

instance ToField UserId where
  toField (UserId t) = SQLText t

data DBReservation = DBReservation
  { dbrId        :: Integer
  , dbrDate      :: String
  , dbrStartTime :: String
  , dbrStopTime  :: String
  , dbrCourt     :: Int
  , dbrUser      :: UserId
  , dbrPerson1   :: Int
  , dbrPerson2   :: Int
  , dbrPerson3   :: Int
  , dbrPerson4   :: Int
  , dbrType      :: Int
  , dbrComment   :: String
  } deriving (Show)

instance FromRow Person where
  fromRow = Person <$> field <*> field <*> field

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field <*> field <*> field <*> field

instance FromRow DBReservation where
  fromRow = DBReservation <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

-- TODO: reformat Query
saveReservation :: String -> Reservation -> IO ()
saveReservation db r@Reservation{..} = do
  con <- open db
  ps  <- mapM (\p -> savePerson db p) resPersons
  logString $ "DB: creating new reservation: " ++ show r -- TODO: better logging for utf-8
  execute con "insert into reservations (date,starttime,stoptime,court,user,player1,player2,player3,player4,type,comment) values (?,?,?,?,?,?,?,?,?,?,?)"
    ([ resDate, resStartTime, resStopTime, show resCourt, T.unpack (unUid resUser) ]
    ++ fillList [ show $ perId x | x <- ps ] ++ [show resType, resComment])

savePerson :: DBName -> Person -> IO Person
savePerson db p@Person{..} = do
  con <- open db
  dbu <- query con "select * from persons where fname=(?) and lname=(?)" [perFName,perLName]
  case dbu of
    []  -> do
      logString $ "DB: person [" ++ T.unpack perFName ++ " " ++ T.unpack perLName
        ++ "] not found, creating new one"
      execute con "insert into persons (fname,lname) values (?,?)" [perFName,perLName]
      u <- query con "select * from persons where fname=(?) and lname=(?)" [perFName, perLName]
      logString $ "DB: created person -> " ++ T.unpack perFName ++ " " ++ T.unpack perLName      
      return $ head u
    x:_ -> do
      logString $ "DB: using person [" ++ show perId ++ "] from database"
      return x

-- | Get all persons from the database.
getAllPersons :: DBName -> IO [Person]
getAllPersons db = do
  con <- open db
  query_ con "select distinct * from persons"

-- | Get one user by id or 'Nothing' if the user is not found.
getPersonById :: DBName -> PersonId -> IO (Maybe Person)
getPersonById db n = do
  con <- open db
  ps <- query con "select * from persons where id=(?)" [n]
  case ps of
    []  -> return Nothing
    x:_ -> return $ Just x

-- | Converts a internal 'DBReservation' to a 'Reservation'.
-- All players will be appended to the new reservation datatype.
convertReservation :: DBReservation -> [Person] -> Reservation
convertReservation dbr@DBReservation{..} ps =
  Reservation dbrId dbrDate dbrStartTime dbrStopTime dbrUser ps (intToCourt dbrCourt) dbrType dbrComment

-- | Get all reservations in the DB.
-- This function uses the internal data structure 'DBReservation' for
-- representing the table 'reservations' in the database.
-- The users will be append afterwards to the 'Reservation' datatype.
getAllReservations :: DBName -> IO [Reservation]
getAllReservations db = do
  con <- open db
  rs  <- query_ con "select * from reservations" :: IO [DBReservation]
  ps  <- getAllPersons db
  return $ for rs (\r -> do
                      convertReservation r
                        (catMaybes [ getPerson ps (dbrPerson1 r)
                                   , getPerson ps (dbrPerson2 r)
                                   , getPerson ps (dbrPerson3 r)
                                   , getPerson ps (dbrPerson4 r)
                                   ]))

-- | Save user for registration to the database if it is not already there.
saveUser :: DBName -> User -> IO (Maybe User)
saveUser db u@User{..} = do
  con <- open db
  dbu <- query con "select * from users where authId=(?)" [usrAuthId]
  case dbu of
    [] -> do
      logString $ "DB: user [" ++ T.unpack usrFName ++ " " ++ T.unpack usrLName
        ++ "] not found, creating new one"
      execute con "insert into users (authId,fname,lname,email,login,role) values (?,?,?,?,?,?)"
        [ T.unpack $ unUid usrAuthId, T.unpack usrFName
        , T.unpack usrLName, T.unpack usrEmail, T.unpack usrLogin, show usrRole]
      u <- query con "select * from users where authId=(?)" [usrAuthId]
      logString $ "DB: created user -> " ++ T.unpack usrFName ++ " " ++ T.unpack usrLName
      return $ Just $ head u
    x:_ -> do
      logString $ "DB: using user [" ++ show usrId ++ "] from database"
      return $ Just x

-- | Get User from database for given 'UserId'.
getUserByAuthId :: DBName -> UserId -> IO (Maybe User)
getUserByAuthId db uid = do
  con <- open db
  dbu <- query con "select * from users where authId=(?)" [T.unpack $ unUid uid]
  case dbu of
    []  -> return Nothing
    x:_ -> return $ Just x

dropReservation :: DBName -> Integer -> UserId -> IO ()
dropReservation db resId usrId = do
  con <- open db
  logString $ "DB: try to delete reservation [" ++ show resId ++ "] for user " ++ show usrId
  execute con "delete from reservations where id = ? and user = ?"
    [show resId, T.unpack $ unUid usrId]
  close con

getAllUsers :: DBName -> IO [User]
getAllUsers db = do
  con   <- open db
  query_ con "select * from users"

updateUser :: DBName -> User -> IO ()
updateUser db u = do
  con <- open db
  print $ u
  execute con "update users set role = ? where id = ?" [show $ usrRole u, show $ usrId u]
  close con
