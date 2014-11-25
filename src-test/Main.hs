{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class
import Snap.Core
import Snap.Http.Server
import System.Environment (getArgs)
import Data.ByteString.Char8 as BC (pack)

handler_status :: String -> Snap ()
handler_status tag = method HEAD $ chkTag
  where failed = modifyResponse $ setResponseCode 412  -- precondition failed
                                . setHeader "Connection" "close"
        chkTag = do
          req <- getRequest
          case (getHeader "x-lb-match" req) of
           Nothing -> failed
           Just t  -> if (BC.pack tag == t) then
                        modifyResponse $ setHeader "x-lb-match" t
                                       . setHeader "Connection" "close"
                      else failed

handler_req :: Snap ()
handler_req = do
  reqB <- readRequestBody 1024
  liftIO $ putStrLn (show reqB)
  writeBS (BC.pack $ show reqB)

main :: IO ()
main = do
  args <- getArgs
  let (tag,port) = (args !! 0, args !! 1)
  let routes = route [ ("/status", handler_status $ tag)
                     , ("/req",    handler_req)
                     ]
  httpServe (setPort (read port :: Int) defaultConfig) routes
