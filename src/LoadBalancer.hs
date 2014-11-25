{-# LANGUAGE OverloadedStrings #-}
module LoadBalancer where

import Control.Applicative
import Control.Concurrent
import Data.Aeson

-- | Datastructure for single endpoint.
data Endpoint = Endpoint
  { location :: String  -- ^ location to service
  , name     :: String  -- ^ name of the service
  , tag      :: String  -- ^ tag of the loaded data of the service
  } deriving Show

instance FromJSON Endpoint where
  parseJSON (Object o) =
    Endpoint <$> o .: "location"
             <*> o .:? "name" .!= defaultName
             <*> o .:? "tag"  .!= defaultTag

instance ToJSON Endpoint where
  toJSON (Endpoint location name tag) =
    object [ "location" .= location
           , "name"     .= name
           , "tag"      .= tag
           ]

-- | Datatype for multiple endpoints.
-- TODO: maybe better a map?
type Endpoints = [Endpoint]

-- | Datatype for the state of the load balancer.
newtype LBState = LBState (MVar (Endpoints,Endpoints))

defaultTag :: String
defaultTag = "#tag#"

defaultName :: String
defaultName = "#name#"

-- create shared memory
newEPState :: IO LBState
newEPState = do
  m <- newMVar ([],[])
  return (LBState m)

-- insert an endpoint to the shared memory
-- see: http://chimera.labs.oreilly.com/books/1230000000929/ch07.html#sec_conc-phonebook
addEndpoint :: LBState -> Endpoint -> IO ()
addEndpoint (LBState m) ep = do
  (neps,peps) <- takeMVar m
  let neps' = neps ++ [ep]
  putMVar m (neps',peps)
  seq neps' (return ())

-- retrieves all endpoints from the shared memory
endpoints :: LBState -> IO Endpoints
endpoints (LBState m) = do
  (neps,peps) <- readMVar m
  return $ neps ++ peps

nextEndpoint :: LBState
             -> ((Endpoints,Endpoints) -> (Endpoint,(Endpoints,Endpoints)))
             -> IO Endpoint
nextEndpoint (LBState m) f = do
  (neps,peps) <- takeMVar m
  let (ep,nstate) = f (neps,peps)
  putMVar m nstate
  seq nstate (return ep)

-- Algorithm:      round robin
-- Datastructure:  (next-items, previous-items)
-- current-item:   the actual item
-- next-itmes:     all the next items
-- previous-items: all the past itmes
--
-- if the next-items is empty flip previous-items with next-items
-- else add current-itme to the last element of the previous-items
--      take first element from next-items and make it current-item
--      remove first element from next-items
nextRR :: ([a],[a]) -> (a,([a],[a]))
nextRR ([],ys)   = nextRR (ys,[])
nextRR (x:xs,ys) = (x, (xs, ys ++ [x]))
