{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class
import Data.ByteString.Lazy.Char8 hiding (putStrLn,head)
import Data.Aeson
import Network.HTTP.Conduit
import Snap.Core hiding (Response, method)
import Snap.Http.Server

import LoadBalancer

-- Handler for register new service endpoint.
handlerReg :: LBState -> Snap ()
handlerReg meps = do
  reqB <- readRequestBody 1024
  case (decode reqB) :: Maybe Endpoint of
   Just ep -> do
     liftIO $ addEndpoint meps ep
   Nothing -> writeBS "ERROR: error in parsing"

-- Handler for retrieving informations about service endpoints.
handlerInfo :: LBState -> Snap ()
handlerInfo meps = do
  eps <- liftIO $ endpoints meps
  writeLBS $ (encode . toJSON) eps

-- Handler for sending abritary requests to balanced services.
handlerSend :: LBState -> Snap ()
handlerSend meps = do
  reqB <- readRequestBody 1024
  nep  <- liftIO $ nextEndpoint meps nextRR
  liftIO $ putStrLn ("send request to: " ++ (show nep))
  res <- liftIO $ sendReq reqB nep
  writeLBS $ responseBody res

-- Send request to given endpoint.
sendReq :: ByteString -> Endpoint -> IO (Response ByteString)
sendReq rqB ep = do
  req' <- parseUrl (location ep)
  let req = req' { method         = "GET"
                 , requestHeaders = [("Content-Type","application/json")]
                 , requestBody    = RequestBodyLBS rqB
                 }
  res <- withManager $ httpLbs req
  return res

main :: IO ()
main = do
  lbState <- newEPState
  let routes = route [ ("/register", handlerReg lbState)
                     , ("/info"    , handlerInfo lbState)
                     , ("/send"    , handlerSend lbState)
                     ]
  httpServe defaultConfig routes
