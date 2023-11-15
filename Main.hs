module Main (main) where

import System.Environment (getArgs)
import Prelude hiding (Maybe, Nothing, Just)

data FizzBuzzNum = Num Int | Fizz | Buzz | FizzBuzz
data Maybe a = Nothing | Just a

instance Show FizzBuzzNum where
    show (Num a) = show a
    show Fizz = "Fizz"
    show Buzz = "Buzz"
    show FizzBuzz = "FizzBuzz"

instance Show (Maybe FizzBuzzNum) where
    show (Just a) = show a
    show Nothing = ""

main :: IO ()
main = do
    args <- getArgs
    fizzBuzz ( read $ head  args :: Int )

fizzBuzz :: Int -> IO ()
fizzBuzz x
    | x <= 0 = return ()
    | otherwise = fizzBuzz ( x - 1 ) >> ( putStrLn $ show $ isfizzBuzz x )

isfizzBuzz :: Int -> Maybe FizzBuzzNum
isfizzBuzz x
    | x <= 0 = Nothing
    | x `mod` 15 == 0 = Just FizzBuzz
    | x `mod` 5 == 0 = Just Buzz
    | x `mod` 3 == 0 = Just Fizz
    | otherwise = Just $ Num x
