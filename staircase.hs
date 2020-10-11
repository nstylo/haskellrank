{-# LANGUAGE FlexibleInstances, UndecidableInstances, DuplicateRecordFields #-}

module Main where

import Control.Monad
import Data.Array
import Data.Bits
import Data.List
import Data.List.Split
import Data.Set
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

-- Complete the staircase function below.
staircase _ 0 = ""
staircase n i =
    (staircase n (i - 1)) ++ (if (i /= 1) then "\n" else "") ++ replicate (n - i) ' ' ++ replicate i '#'
                             -- hacky inline if then else ftw

main :: IO()
main = do
    n <- readLn :: IO Int

    putStrLn $ staircase n n
