{-# LANGUAGE OverloadedStrings #-}
module WebRepl where

import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString
import Network.HTTP.Req
import Data.String (IsString(..))
import StringMap

eg :: String -> IO ByteString
-- You can either make your monad an instance of 'MonadHttp', or use
-- 'runReq' in any IO-enabled monad without defining new instances.
eg message = runReq defaultHttpConfig $ do
  let payload = fromString message

  -- One function—full power and flexibility, automatic retrying on timeouts
  -- and such, automatic connection sharing.
  r <-
    req
      POST -- method
      (https "boundvariable.space" /: "communicate") -- safe by construction URL
      (ReqBodyBs payload) -- use built-in options or add your own
      bsResponse -- specify how to interpret response
      (header "Authorization" "Bearer 3e6b791a-a998-49db-9ec6-ddc017323fc0") -- query params, headers, explicit port number, etc.
  pure (responseBody r)
