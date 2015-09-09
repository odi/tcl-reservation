module App where

-- import Snap.Snaplet                        (Snaplet, SnapletInit, makeSnaplet)
-- import Snap.Snaplet.Auth                   (AuthManager)
-- import Snap.Snaplet.Session.SessionManager (SessionManager)

-- data App = App
--     { _auth :: Snaplet (AuthManager App)
--     , _sess :: Snaplet SessionManager
--     }

-- app :: SnapletInit App App
-- app = makeSnaplet "app" "snaplet example" Nothing $ do
--   s <- nestSnaplet "sess" sess $
--       initCookieSessionManager "site_key.txt" "sess" (Just 3600)
--   a <- nestSnaplet "auth" auth $
--       initJsonFileAuthManager defaultAuthSettings sess "users.json"
--   return $ App s s
