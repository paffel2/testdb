{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric  #-}
module Server where
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import GHC.Generics
import Data.Aeson
import Testdatabase

{-simpleApp :: Application 
simpleApp _ respond = respond $
    responseLBS status200 
                [(hContentType, "text/plain")]
                (encode tst)

simpleServer :: IO ()
simpleServer = do
    putStrLn "Serving..."
    run 8000 simpleApp-}

{-simpleApp :: ToJSON a => a -> Application 
simpleApp jsonInfo _ respond = respond $
    responseLBS status200
     []
     (encode jsonInfo)

simpleServer :: IO ()
simpleServer = do
    putStrLn "Serving..."
    run 8000 (simpleApp tst)-}


simpleApp :: ToJSON a => a -> Application 
simpleApp jsonInfo _ respond = respond $
    responseLBS status200
     []
     (encode jsonInfo)

simpleServer :: IO ()
simpleServer = do
    putStrLn "Serving..."
    news <- getNews
    run 8000 (simpleApp news)


data SimpleJson = SimpleJson { name :: String,
                               age :: Int 
                               } deriving (Show, Generic)
instance ToJSON SimpleJson where
    toJSON = genericToJSON defaultOptions


tst :: SimpleJson
tst = SimpleJson "Paul" 26