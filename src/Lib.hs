module Lib
    ( someFunc
    ) where
        
import Testdatabase ()

someFunc :: IO ()
someFunc = putStrLn "someFunc"
