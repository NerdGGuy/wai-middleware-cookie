{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

import Data.Unique (newUnique, hashUnique)
import Data.Ratio (numerator, denominator)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.String (fromString)
import Network.HTTP.Types (ResponseHeaders)
import Network.Wai (Application, Middleware, Request(..), Response(..))
import Web.Cookie (parseCookies, renderSetCookie, SetCookie(..), Cookies(..))

import Data.ByteString (ByteString)
import qualified Blaze.ByteString.Builder as Builder

import Data.CaseInsensitive (mk)
import Network.Wai.Middleware.Cookie

import Control.Monad (unless)
import Data.List (stripPrefix)
import System.Exit (exitFailure)
import Test.QuickCheck.All (quickCheckAll)
import Network.Wai.Test (defaultRequest)

noCookie   = defaultRequest
oneCookie  = noCookie { requestHeaders = (mk "Cookie", "one=test;"):(requestHeaders noCookie) }
twoCookies = noCookie { requestHeaders = (mk "Cookie", "one=test; two=test2;"):(requestHeaders noCookie) }

prop_noCookies  = Nothing == cookies noCookie
prop_oneCookies = Just [("one","test")] == cookies oneCookie
prop_twoCookies = Just [("one","test"),("two","test2")] == cookies twoCookies

main = do
    allPass <- $quickCheckAll -- Run QuickCheck on all prop_ functions
    unless allPass exitFailure
