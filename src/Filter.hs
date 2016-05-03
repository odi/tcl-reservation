{-# LANGUAGE OverloadedStrings #-}

module Filter where

import           Data.Maybe         (catMaybes)
import qualified Data.Text          as T
import           Snap.Snaplet.Auth  (UserId)

-- own modules
import Types

import Debug.Trace

-- | Filter reservations by date.
-- This filter returns all reservations on a specific date.
filterReservationsByDate :: [Reservation] -> String -> [Reservation]
filterReservationsByDate rs date = filter (\r -> resDate r == date) rs

-- | Filter reservations by date and time.
-- This filter returns all reservations on a specific date and between a start time
-- and an end time. The end time will be calculated by adding the length to the start time.
filterReservationsByDateTime :: [Reservation] -> String -> String -> Int -> [Reservation]
filterReservationsByDateTime rs date time length = filterTime time $ filterDate rs date
  where
    filterDate rs d = filter (\r -> resDate r == d) rs
    filterTime t rs = catMaybes
                      [ if or [between x (resStartTime r) (resStopTime r) | x <- times t] then Just r else Nothing
                      | r <- rs ]
    times t = do
      let x = read (T.unpack . head $ T.splitOn ":" $ T.pack t) :: Int
      map (\y -> show y ++ ":00") ( zipWith (+) [0..(length - 1)] [x,x..])
    between t st et = do
      let x = read (T.unpack . head $ T.splitOn ":" $ T.pack t) :: Int
          y = read (T.unpack . head $ T.splitOn ":" $ T.pack st) :: Int
          z = read (T.unpack . head $ T.splitOn ":" $ T.pack et) :: Int
      if (x >= y) && (x < z) then True else False  

-- | Filter for free courts on a specific date and time.
-- This filter returns all courts which are not reserved for a specific date and time.
filterFreeCourts :: [Reservation] -> String -> String -> Int -> [Court]
filterFreeCourts rs date time length =
  map resCourt $ filterReservationsByDateTime rs date time length

filterReservationsByUser :: [Reservation] -> UserId -> [Reservation]
filterReservationsByUser rs uid = filter (\r -> resUser r == uid) rs

filterReservationsByStartLength :: [Reservation] -> String -> String ->Int -> [Reservation]
filterReservationsByStartLength rs date time length = filterTime time $ filterDate rs date
  where
    filterDate rs d = filter (\r -> resDate r == d) rs
    filterTime t rs = catMaybes
              [ if resStartTime r `elem` times t then Just r else Nothing
              | r <- rs ]
             
    times t = do
      let x = read (T.unpack . head $ T.splitOn ":" $ T.pack t) :: Int
      map (\y -> show y ++ ":00") ( zipWith (+) [0..(length-1)] [x,x..])
