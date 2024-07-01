{-# LANGUAGE OverloadedStrings #-}
module WebRepl where

import Control.Monad.IO.Class
-- import Data.Aeson
import Data.ByteString.UTF8 (toString, fromString)
import Flow
import Network.HTTP.Req
import StringMap
import System.Console.Isocline (readline)
import Eval
import HOAS
import Data.ByteString (ByteString)

repl :: IO ()
repl = do
  msg <- readline "icfp"
  case msg of
    ":q" -> pure ()
    _    -> do
      putStrLn ""
      putStrLn "Response:"
      res <- sendMessage msg
      putStrLn res
      repl

sendMessage :: String -> IO String
sendMessage message = runReq defaultHttpConfig $ do
  let payload = fromString ('S' : (toIcfpString message))

  r <-
    req
      POST
      (https "boundvariable.space" /: "communicate")
      (ReqBodyBs payload)
      bsResponse
      (header "Authorization" "Bearer 3e6b791a-a998-49db-9ec6-ddc017323fc0")

  r |> responseBody
    |> toString
    |> evalStr @String
    |> liftIO

replDebug :: IO ()
replDebug = do
  msg <- readline "icfp"
  case msg of
    ":q" -> pure ()
    _    -> do
      putStrLn ""
      putStrLn "Response:"
      res <- sendMessageDebug msg
      putStrLn res
      replDebug

sendMessageDebug :: String -> IO String
sendMessageDebug message = sendDebug (fromString ('S' : (toIcfpString message)))

sendHOASe :: HOASe String -> IO String
sendHOASe hoas = sendDebug (fromString $ hoasToStr hoas)

sendDebug :: ByteString -> IO String
sendDebug payload = runReq defaultHttpConfig $ do
  r <-
    req
      POST -- method
      (https "boundvariable.space" /: "communicate")
      (ReqBodyBs payload)
      bsResponse
      (header "Authorization" "Bearer 3e6b791a-a998-49db-9ec6-ddc017323fc0")

  let res = r |> responseBody
              |> toString
  liftIO $ print res
  res
    |> evalStr @String
    |> liftIO
