{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}

module App where

import Control.Lens                        (makeLenses)

import Snap.Snaplet                        (Snaplet, Handler, SnapletInit, makeSnaplet,
                                            nestSnaplet)
import Snap.Snaplet.Auth                   (AuthManager, defAuthSettings)
import Snap.Snaplet.Session.SessionManager (SessionManager)
import Snap.Snaplet.Session.Backends.CookieSession
import Snap.Snaplet.Auth.Backends.SqliteSimple
import Snap.Snaplet.Auth.Backends.JsonFile
import Snap.Snaplet.SqliteSimple

data App = App
  { _auth :: Snaplet (AuthManager App)
  , _sess :: Snaplet SessionManager
  , _dbr  :: Snaplet Sqlite
  }

makeLenses ''App

type AppHandler = Handler App App
