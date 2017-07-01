{-# LANGUAGE OverloadedStrings #-}

module Lib (someFunc) where

import Control.Applicative ((<$>), optional)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Lazy (unpack)
import Happstack.Lite
import System.RaspberryPi.GPIO
import Control.Monad.IO.Class

someFunc :: IO ()
someFunc =  serve Nothing router

hello :: ServerPart Response
hello =  ok $  toResponse hh

hh = "hello blarp1" :: String


router :: ServerPart Response
router = msum 
  [ dir "echo" echo
  , dir "pinRead" pinRead
  , dir "pinWrite" pinWrite
  , ok (toResponse hh)
  ]


pinRead :: ServerPart Response
pinRead =  liftIO $ withGPIO $ do 
  x <- readPin Pin05 
  return (toResponse (show x))

pinWrite :: ServerPart Response
pinWrite = path pinWrite'
pinWrite' :: String -> ServerPart Response
pinWrite' (b:_)  = let isTrue = b == 't' in liftIO $ withGPIO $ do
  setPinFunction Pin11 Output
  writePin Pin11 isTrue
  return (toResponse ("Set to " ++ (show isTrue) ))



--ok $ toResponse (readPin Pin05)

echo :: ServerPart Response
echo = path blarp

blarp :: String -> ServerPart Response
blarp s = ok (toResponse s)

--myApp :: ServerPart Response
--myApp = msum
--  [ dir "echo"    $ echo
--  , homePage
--  ]

