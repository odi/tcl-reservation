module Reservation where

import Data.Char (toUpper)

data ReservationType = ResSingle  -- ^ reservation for single
                     | ResDouble  -- ^ reservation for double

data Reservation = Reservation
  { resUsers   :: [User]
  , resID      :: Integer
  , resFromTS  :: String   -- ^ TODO: change to Timestamp
  , resToTS    :: String   -- ^ TODO: change to Timestamp
  , resCourt   :: Int
  , resCreated :: String   -- ^ TODO: change to Timestamp
  , resUser    :: User
  , resType    :: ReservationType
  }

data User = User
  { userFName :: String
  , userLName :: String
  , userID    :: Integer
  }

-- i would like to use:
--   - cmdcfg (parsing config files)
--   - cmdargs
--   - snap
--   - json-rpc


-- convert to ical

data ICal = ICal
  { icalSummary     :: String
  , icalDescription :: String
  }

{-

SUMMARY: Müller W., Dunkl O., Krendl R., Untermayer M.

-}

reservation2ical :: Reservation -> ICal
reservation2ical r =
    ICal summary "" -- e.g. Dunkl O. Krendl R. ...
    where
        summary = unwords $ map (name) $ resUsers r
        name n = userLName n ++ " " ++ ([toUpper . head $ userFName n]) ++ "."

ical2str :: ICal -> String
ical2str c = unwords
             [ "BEGIN:VCALENDAR\n"
             , "BEGIN:VEVENT\n"
             , "SUMMARY: " ++ icalSummary c
             , "END:VECENT\n"
             , "END:VCALENDAR\n"
             ]
