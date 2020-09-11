module Main (main) where

import RowComponents (projectName)


main :: IO ()
main = putStrLn ("Tests for " ++ projectName)
