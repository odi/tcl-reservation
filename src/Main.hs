{-# LANGUAGE OverloadedStrings #-}

module Main where

import Snap.Core        (Snap, route, writeBS)
import Snap.Http.Server (quickHttpServe)

import Auth

site :: Snap ()
site = route [("foo", writeBS "bar")
             ,("/login", withAuth $ writeBS "hello user")
             ]

main :: IO ()
main = quickHttpServe site
