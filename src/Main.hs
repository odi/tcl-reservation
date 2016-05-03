{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative  ((<|>))
import           Control.Error.Safe
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Either
import           Data.Aeson           (encode)
import           Data.ByteString      (ByteString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BSC
import           Data.Maybe           (fromMaybe, catMaybes, fromJust)
import qualified Data.Text            as T
import qualified Data.Text.Read       as T
import qualified Data.Text.Encoding   as T
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime
import           Snap.Core            (Snap, Method(..), route, writeBS, dir,
                                       getParam, getPostParam, method, redirect, logError)
import           Snap.Http.Server     (quickHttpServe)
import           Snap.Util.FileServe  (serveDirectory)

import           Auth                 (withAuth)
import           DB
-- own modules
import           App
import           Types
import           Filter

import Snap.Snaplet
import Snap.Snaplet.Auth hiding (saveUser)
import Snap.Snaplet.Session.SessionManager
import Snap.Snaplet.Session.Backends.CookieSession
import Snap.Snaplet.Auth.Backends.JsonFile
import Snap.Http.Server.Config
import Snap.Snaplet.SqliteSimple

import qualified Data.ByteString.UTF8 as BU

--import System.Time

-- | Split reservations to 1h time slots.
-- If a reservation has a length of more than 1 hour, this reservation will be split
-- into multiple reservations. The id of the splitted reservations are the same.
-- e.g.
-- Reservation (start: 18:00, end: 20:00)
-- becomes:
-- Reservation (start: 18:00, end: 19:00)
-- Reservation (start: 19:00, end: 20:00)
--
-- TODO: refactor to Reservation.hs
splitReservation :: Reservation -> [Reservation]
splitReservation r = case (endHour - startHour) of
  1 -> [r]
  x -> map (\(s,e) -> r{resStartTime = s, resStopTime = e}) $ hours startHour x
  where
    startHour = read (T.unpack . head $ T.splitOn ":" $ T.pack $ resStartTime r) :: Int
    endHour   = read (T.unpack . head $ T.splitOn ":" $ T.pack $ resStopTime r) :: Int
    hours s l = [((show b) ++ ":00",(show $ b+1) ++ ":00") | b <- [s..(s+(l-1))]]

-- | Handler for getting all reservations.
handleAllReservations :: String -> Snap ()
handleAllReservations db = do
  res <- liftIO $ getAllReservations db
  writeBS $ BSL.toStrict $ encode res

-- | Handler for reservations on a specific date or today.
-- This will be used on the entry page to show all reservations on a specific date.
handleReservationsByDate :: DBName -> Handler App (AuthManager App) ()
handleReservationsByDate db = do
  pDate <- getParam "date"
  res   <- liftIO $ getAllReservations db
  date  <- liftIO $ getCurrentTime
  let (y,m,d) = toGregorian $ utctDay date
      today   = BSC.pack $ show y ++ "-" ++ show m ++ "-" ++ show d
      res'    = filterReservationsByDate res $ BSC.unpack (fromMaybe today pDate)
  writeBS $ BSL.toStrict $ encode (concat $ map splitReservation res')

-- | Handler for reservations on a specific date and time.
-- This handler will be used for the date and time overview on the new reservation page.
handleReservationsByDateTime :: DBName -> Handler App (AuthManager App) ()
handleReservationsByDateTime db = do
  pDate   <- getParam "date"
  pTime   <- getParam "btime"
  pLength <- getParam "length"
  res     <- liftIO $ getAllReservations db
  date    <- liftIO $ getCurrentTime
  let (y,m,d) = toGregorian $ utctDay date
      today   = BSC.pack $ show y ++ "-" ++ show m ++ "-" ++ show d
      res'    = filterReservationsByDateTime res (BSC.unpack (fromMaybe today pDate))
                (BSC.unpack (fromMaybe "00:00" pTime))
                (read (BSC.unpack $ fromMaybe "1" pLength) :: Int)
  writeBS $ BSL.toStrict $ encode (filterReservationsByStartLength (concat $ map splitReservation res')
                                   (BSC.unpack (fromMaybe today pDate))
                                   (BSC.unpack (fromMaybe "00:00" pTime))
                                   (read (BSC.unpack $ fromMaybe "1" pLength) :: Int))

createPerson :: Maybe ByteString -> Maybe Person
createPerson Nothing = Nothing
createPerson (Just "") = Nothing
createPerson (Just s ) = Just $ Person 0 (head names) (last names)
  where
    names = T.splitOn " " $ T.pack (BSC.unpack s)

cP :: Maybe T.Text -> Maybe Person
cP Nothing = Nothing
cP (Just "") = Nothing
cP (Just t) = Just $ Person 0 (head names) (last names)
  where
    names = T.splitOn " " t

createDate :: Maybe ByteString -> String
createDate Nothing  = error "date not defined"
createDate (Just d) = BSC.unpack d

createTime :: Maybe ByteString -> String
createTime Nothing  = error "time not defined"
createTime (Just t) = BSC.unpack t

createLength :: Maybe ByteString -> String
createLength Nothing  = error "length not defined"
createLength (Just l) = BSC.unpack l

createPlayer :: Maybe ByteString -> Maybe String
createPlayer Nothing  = Nothing
createPlayer (Just p) = Just $ BSC.unpack p

-- | Handler for creating a reservation and saving it to the database.
-- TODO: redirect does not work
handleSaveReservation :: DBName -> Handler App (AuthManager App) ()
handleSaveReservation db = do
  pPl1    <- fmap T.decodeUtf8 <$> getPostParam "name1"
  pPl2    <- fmap T.decodeUtf8 <$> getPostParam "name2"
  pPl3    <- fmap T.decodeUtf8 <$> getPostParam "name3"
  pPl4    <- fmap T.decodeUtf8 <$> getPostParam "name4"
  pDate   <- getPostParam "date"
  pTime   <- getPostParam "time"
  pLength <- getPostParam "length"
  pCourt  <- getPostParam "court"
  pType   <- getPostParam "type"
  pCom    <- fmap T.decodeUtf8 <$> getPostParam "comment"
  cu      <- currentUser
  let r = Reservation 0 (createDate pDate) (createTime pTime)
          (stopTime (createTime pTime) (createLength pLength))
          (fromMaybe (UserId "") (userId $ fromMaybe defAuthUser cu))
          (catMaybes $ [] ++ [cP pPl1] ++ [cP pPl2]
           ++ [cP pPl3] ++ [cP pPl4])
          (intToCourt (read (BSC.unpack $ fromMaybe "0" pCourt) :: Int))
          (read (BSC.unpack $ fromMaybe "0" pType) :: Int)
          (T.unpack $ fromMaybe "" pCom)
  liftIO $ saveReservation db r
  redirect "/" -- TODO: redirect does not work

  where
    stopTime time length = do
      let ts = T.splitOn ":" $ T.pack time
          nt = (read (T.unpack $ head ts) :: Int) + (read length :: Int)
      (show nt) ++ ":" ++ (T.unpack $ last ts)

-- | Handler for all players.
-- This handler returns all players in the database.
handleAllPersons :: DBName -> Handler App (AuthManager App) ()
handleAllPersons db = do
  ps <- liftIO $ getAllPersons db
  writeBS $ BSL.toStrict $ encode ps

handleLogin :: Handler App (AuthManager App) ()
handleLogin = do
  loginUser "username" "password" (Just "1") onFailure onSuccess
  where
    onFailure f = case f of
      AuthError s       -> redirect $
                           BSC.pack $ "/login.html?failure=" ++ s
      UserNotFound      -> redirect $
                           BSC.pack $ "/login.html?failure=Benutzer nicht gefunden"
      IncorrectPassword -> redirect $
                           BSC.pack $ "/login.html?failure=Falsches Passwort"
      _                 -> redirect $ "/login.html?failure=Undefined error"
    onSuccess = do
      mu <- currentUser
      case mu of
        Just _ -> redirect "/"
        Nothing -> writeBS "Can't happen"

logoutHandler :: Handler App (AuthManager App) ()
logoutHandler = logoutUser $ redirect "/"

-- | Handler for registering a new user in the database.
handleRegisterNewUser :: DBName -> Handler App (AuthManager App) ()
handleRegisterNewUser db = do
  pFName <- getParam "fname"
  pLName <- getParam "lname"
  pEmail <- getParam "email"
  user   <- registerUser "username" "password"
  case user of
    Left failure -> writeBS $ BSC.pack $ show failure
    Right u      -> do
      liftIO $ saveUser db  -- TODO: get dbname from dbr
        (User 0 (fromMaybe (UserId "") (userId u))
         (T.pack . BSC.unpack $ fromMaybe "" pFName)
         (T.pack . BSC.unpack $ fromMaybe "" pLName)
         (T.pack . BSC.unpack $ fromMaybe "" pEmail)
         (userLogin u) Member)
      redirect "/"

handleCurrentUser :: DBName -> Handler App (AuthManager App) ()
handleCurrentUser db = currentUser >>= go
  where
    go Nothing  = do writeBS $ ""
    go (Just u) = do
      case userId u of
        Nothing -> redirect "/auth/login"
        Just u' -> do
          u'' <- liftIO $ getUserByAuthId db u'
          writeBS $ BSL.toStrict $ encode u''

handleReservationsByAuthId :: DBName -> Handler App (AuthManager App) ()
handleReservationsByAuthId db = do
  res  <- liftIO $ getAllReservations db
  user <- currentUser
  date <- liftIO $ getCurrentTime
  tz   <- liftIO $ getCurrentTimeZone
  let (TimeOfDay hour _ _) = localTimeOfDay $ utcToLocalTime tz date
  case user of
    Nothing -> writeBS ""
    Just u  -> do
      let res' = filterReservationsByUser res (fromMaybe (UserId "") $ userId u)
      writeBS $ BSL.toStrict $ encode res'

handleReservations :: DBName -> Handler App (AuthManager App) ()
handleReservations db = do
  res <- liftIO $ getAllReservations db
  writeBS $ BSL.toStrict $ encode res

handleDeleteReservation :: DBName -> Handler App (AuthManager App) ()
handleDeleteReservation db = do
  pResId <- getParam "resId"
  user   <- currentUser
  case pResId of
    Nothing  -> writeBS "error"
    Just rid -> do
      case user of
        Nothing -> writeBS "error"
        Just u  -> do
          liftIO $ dropReservation db (read (BSC.unpack rid) :: Integer)
            (fromMaybe (UserId "") $ userId u)
          redirect "/"  -- TODO: redirect does not work!

handleTest :: Handler App (AuthManager App) ()
handleTest = do
  t <- getPostParam "test"
  liftIO $ print (T.unpack . T.decodeUtf8 $ fromJust t)
  liftIO $ putStrLn (T.unpack . T.decodeUtf8 $ fromJust t)
  writeBS $ fromJust t

handleAllUsers :: DBName -> Handler App (AuthManager App) ()
handleAllUsers db = do
  users <- liftIO $ getAllUsers db
  writeBS $ BSL.toStrict $ encode users

handleUpdateUser :: DBName -> Handler App (AuthManager App) ()
handleUpdateUser db = do
  pId   <- getPostParam "id"
  pRole <- getPostParam "role"
  users <- liftIO $ getAllUsers db
  let user = head $ filter (\u -> usrId u == (read (BSC.unpack $ fromJust pId) :: Int)) users
  liftIO $ updateUser db (user {usrRole = roleFromInt $ (read (BSC.unpack $ fromJust pRole) :: Int)})
  writeBS ""

handleFreeCourts :: DBName -> Handler App (AuthManager App) ()
handleFreeCourts db = do
  pDate   <- getParam "date"
  pTime   <- getParam "time"
  pLength <- getParam "length"
  cu      <- currentUser
  res     <- liftIO $ getAllReservations db
  date    <- liftIO $ getCurrentTime
  let (y,m,d) = toGregorian $ utctDay date
      today   = BSC.pack $ show y ++ "-" ++ show m ++ "-" ++ show d
      res'    = filterReservationsByDateTime res (BSC.unpack (fromMaybe today pDate))
                (BSC.unpack (fromMaybe "00:00" pTime))
                (read (BSC.unpack $ fromMaybe "1" pLength) :: Int)
  redirect "/"
  -- case cu of
  --   Nothing -> redirect "/auth/login"
  --   Just u  -> do
  --     u' <- liftIO $ getUserByAuthId db (fromJust $ userId u)
  --     case usrRole u' of
  --       3 -> do
          
  --     redirect "/"
  
-- | Configuration of the main app.
app :: DBName -> SnapletInit App App
app db = makeSnaplet "app" "reservation system" Nothing $ do
  s <- nestSnaplet "session" sess $
       initCookieSessionManager "site_key.txt" "cookie" Nothing
  a <- nestSnaplet "auth" auth $
       initJsonFileAuthManager defAuthSettings sess "users.json"
  d <- nestSnaplet "db" dbr sqliteInit
  addRoutes [ ("/auth/login", with auth $ handleLogin)
            , ("/auth/logout", with auth $ logoutHandler)
            , ("/auth/register", with auth $ handleRegisterNewUser db)
            , ("/auth/current", with auth $ handleCurrentUser db)
            , ("/get/persons", with auth $ handleAllPersons db)
            , ("/get/users", with auth $ handleAllUsers db)
            , ("/get/resbydatetime", with auth $ handleReservationsByDateTime db)
            , ("/get/resbydate", with auth $ handleReservationsByDate db)
            , ("/get/resbyuser", with auth $ handleReservationsByAuthId db)
            , ("/get/reservations", with auth $ handleReservations db)
            , ("/get/freecourts", with auth $ handleFreeCourts db)
            , ("/del/reservation", with auth $ handleDeleteReservation db)
            , ("/put/reservation", with auth $ method POST (handleSaveReservation db))
            , ("/put/user", with auth $ method POST (handleUpdateUser db))
            , ("/test", with auth $ handleTest)
            , ("/", serveDirectory "static")
            ]
  return $ App a s d

main :: IO ()
main = do
  let db = "reservation.db"
  serveSnaplet defaultConfig $ app db
--  let db = "reservation.db"
--  quickHttpServe (site db)
