module Main (main) where

import Lib

main :: IO ()
main = mapM_ putStrLn $ map show $ take 40 factorbuzz
