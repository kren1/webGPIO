--{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
--module Main where
--
--import Control.Applicative ((<$>), optional)
--import Data.Maybe (fromMaybe)
--import Data.Text (Text)
--import Data.Text.Lazy (unpack)
--import Happstack.Lite
--
--main :: IO ()
--main = serve Nothing myApp
--
--hello :: ServerPart Response
--hello :: toResponse "hello blarp"
--
--myApp :: ServerPart Response
--myApp = msum
--  [ dir "echo"    $ echo
--  , homePage
--  ]


