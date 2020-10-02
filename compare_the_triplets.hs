{-# LANGUAGE DuplicateRecordFields, FlexibleInstances, UndecidableInstances #-}

module Main where


import Data.Array
import Data.Bits
import Data.List
import Data.List.Split
import Data.Set hiding (foldr)
import Data.Text hiding (foldr, zipWith)
import Debug.Trace
import System.Environment
import System.IO
import System.IO.Unsafe

calcScore :: Int -> Int -> [Int]
calcScore a b
    | a > b = [1, 0]
    | b > a = [0, 1]
    | otherwise = [0, 0]

-- Complete the compareTriplets function below.
compareTriplets :: [Int] -> [Int] -> [Int]
compareTriplets a b = foldr (zipWith (+)) [0, 0] $ zipWith calcScore a b

lstrip = Data.Text.unpack . Data.Text.stripStart . Data.Text.pack
rstrip = Data.Text.unpack . Data.Text.stripEnd . Data.Text.pack

main :: IO()
main = do
    stdout <- getEnv "OUTPUT_PATH"
    fptr <- openFile stdout WriteMode

    aTemp <- getLine

    let a = Data.List.map (read :: String -> Int) . Data.List.words $ rstrip aTemp

    bTemp <- getLine

    let b = Data.List.map (read :: String -> Int) . Data.List.words $ rstrip bTemp

    let result = compareTriplets a b

    hPutStrLn fptr $ Data.List.intercalate " " $ Data.List.map (\x -> show x) $ result

    hFlush fptr
    hClose fptr
