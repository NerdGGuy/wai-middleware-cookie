module Network.Wai.Middleware.Cookie (cookies, setCookie) where

import Data.Unique (newUnique, hashUnique)
import Data.Ratio (numerator, denominator)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.String (fromString)
import Network.HTTP.Types (ResponseHeaders)
import Network.Wai (Application, Middleware, Request(..), Response(..))
import Web.Cookie (parseCookies, renderSetCookie, SetCookie(..), Cookies(..))

import Data.ByteString (ByteString)
import qualified Blaze.ByteString.Builder as Builder

import Test.QuickCheck.All (quickCheckAll)

cookies :: Request -> Maybe Cookies
cookies req = lookupCookies where
	lookupCookies = fmap parseCookies $ lookup cookieString $ requestHeaders req
	cookieString = fromString "Cookie"
	
setCookie :: SetCookie -> Middleware
setCookie x app req = do
        resp <- app req
        return $ mapHeader (\hs -> (setCookieString, newCookie x):hs) resp where
        newCookie v = Builder.toByteString $ renderSetCookie x
        setCookieString = fromString "Set-Cookie"
        
mapHeader :: (ResponseHeaders -> ResponseHeaders) -> Response -> Response
mapHeader f (ResponseFile s h b1 b2) = ResponseFile s (f h) b1 b2
mapHeader f (ResponseBuilder s h b) = ResponseBuilder s (f h) b
mapHeader f (ResponseSource s h b) = ResponseSource s (f h) b
