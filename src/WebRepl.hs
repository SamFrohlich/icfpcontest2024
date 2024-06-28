{-# LANGUAGE OverloadedStrings #-}
module WebRepl where

import Control.Monad.IO.Class
-- import Data.Aeson
import Data.ByteString.UTF8 (toString, fromString)
import Flow
import Network.HTTP.Req
import Network.URI
import StringMap
import System.Console.Isocline (readline)

repl :: IO ()
repl = do
  msg <- readline "icfp"
  putStrLn ""
  putStrLn "Response:"
  sendMessage msg
  repl

sendMessage :: String -> IO ()
-- You can either make your monad an instance of 'MonadHttp', or use
-- 'runReq' in any IO-enabled monad without defining new instances.
sendMessage message = runReq defaultHttpConfig $ do
  let payload = fromString ('S' : (toIcfpString message))

  -- One function—full power and flexibility, automatic retrying on timeouts
  -- and such, automatic connection sharing.
  r <-
    req
      POST -- method
      (https "boundvariable.space" /: "communicate") -- safe by construction URL
      (ReqBodyBs payload) -- use built-in options or add your own
      bsResponse -- specify how to interpret response
      (header "Authorization" "Bearer 3e6b791a-a998-49db-9ec6-ddc017323fc0") -- query params, headers, explicit port number, etc.
  let responseMessage
        = r |> responseBody
            |> toString
            |> Prelude.tail
            |> fromIcfpString
  liftIO $ putStrLn responseMessage
