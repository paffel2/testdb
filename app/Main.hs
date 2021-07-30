module Main where

import Lib 


main :: IO ()
main = someFunc


{-main :: IO ()
main = do
    conf <- newConfigHandle
    --print "1"
    n <- getDbConfig conf
    print n-}
