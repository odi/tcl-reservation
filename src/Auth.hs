{-# LANGUAGE OverloadedStrings #-}

module Auth where

import           Data.ByteString           (ByteString)
import qualified Data.ByteString           as BS
import           Data.ByteString.Base64    (decode, encode)
import           Data.Maybe                (isJust)
import qualified Data.Text                 as T
import           Snap.Core                 (Snap, writeBS, getRequest, getHeader,
                                            modifyResponse, setResponseStatus, getResponse,
                                            finishWith, writeText, logError)

type Username = ByteString
type Password = ByteString

type Credentials = (Username, Password)

-- TODO: comes from a DB
validCredentials = [ "b2xpdmVyLmR1bmtsQGdtYWlsLmNvbTpvZGk=" ]

-- 32 is the numeric literal for ' ' (space)
-- :m +Data.Char
-- fromIntegral (ord ' ') ⇒ 32
-- to pase: Basic lkasjdfla832jlkfzuo=
extractCredentials ∷ ByteString → Maybe ByteString
extractCredentials bs = case BS.split 32 bs of
    [] → Nothing
    hs → chkH hs
    where
        chkH h
            | length h < 2     = Nothing -- header is corrupt or not available  
            | head h /= "Basic" = Nothing -- header does not start with 'Basic'
            | otherwise        = (Just $ head . tail $ h)

-- | Check if the credentials are valid or not.
-- TODO: get credentials from DB?
credentialsValid ∷ ByteString → Bool
credentialsValid bs = bs `elem` validCredentials

throwDenied ∷ T.Text → ByteString → Snap ()
throwDenied msg bs = do
    modifyResponse $ setResponseStatus 403 "Access Denied"
    writeText $ T.append "Access Denied: " msg
    logError $ BS.append "Access Denied from: " bs
    getResponse >>= finishWith

-- | Basic authorization for requests.
-- TODO: logging ...
withAuth :: Snap () -> Snap ()
withAuth succ = do
  rq <- getRequest
  let mh = getHeader "Authorization" rq
  case mh of
    Just header -> case extractCredentials header of
        Just cred → if credentialsValid cred
                    then logError (BS.append "Request from: " cred) >> succ
                    else throwDenied "wrong username/password" cred
        Nothing   → throwDenied "error in parsing request-header" "error in parsing request-header"
    Nothing   -> throwDenied "could not find matching header for authentication" "could not find matching header for authentication"
